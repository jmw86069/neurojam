
#' SQLite table description
#'
#' SQLite table description
#'
#' This function returns a description of a SQLite table
#' using data stored in the `sqlite_master` reference database
#' table. When the table is actually a database view, it
#' calls `sqlite_view()` and returns the corresponding
#' information based upon the view, which includes traversing
#' columns to the source table to find out if each column
#' is indexed.
#'
#' @family jam database functions
#'
#' @return when method is `"df"` the output is a `data.frame` with
#'    `"column"`, `"type"`, and `"index"` columns. The `"index"`
#'    column is logical, indicating whether each column has
#'    a corresponding index.
#'
#' @param con valid SQLite database connection
#' @param x character vector
#' @param cat_text logical indicating whether to cat the text
#'   output, or when `cat_text=FALSE` the SQL lines are returned as
#'   a character vector.
#'  @param method character value, where `"df"` returns `data.frame`,
#'     along with column types derived from content in each field,
#'     and column index information; or
#'     and `"sql"` which returns the SQL definition used to create
#'     the table or view, but does not include more information
#'     about whether each column is indexed.
#' @param ... additional arguments are ignored.
#'
#' @importFrom DBI dbGetQuery dbListTables
#' @importFrom jamba nameVector rbindList vigrep renameColumn
#'
#' @examples
#' db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:");
#' DBI::dbCreateTable(db, "forecasts", data.frame(id=1:3, temperature=20:22));
#' DBI::dbGetQuery(db, "CREATE INDEX id_forecast_idx on forecasts(id)");
#' sqlite_desc(db, "forecasts")
#'
#' # create second table
#' DBI::dbCreateTable(db, "feels", data.frame(temperature=20:22, feels=c("cold","warm","hot")));
#'
#' # create a view that joins these two tables
#' DBI::dbGetQuery(db, "CREATE VIEW ff_view as SELECT f.id, f.temperature, fe.feels FROM forecasts f, feels fe WHERE f.temperature = fe.temperature");
#' sqlite_desc(db, "ff_view")
#'
#' @export
sqlite_desc <- function
(con,
 x,
 cat_text=TRUE,
 method=c("df", "sql"),
 ...)
{
   method <- match.arg(method);
   x_type <- DBI::dbGetQuery(con,
      "SELECT type FROM sqlite_master WHERE name = ? LIMIT 1",
      params=list(x))[,1];
   if ("df" %in% method) {
      ## sqlite view gets special treatment
      if ("view" %in% x_type) {
         desc_df <- sqlite_view(con, x, type="desc_df");
      } else {
         df <- DBI::dbGetQuery(con,
            paste0("
               SELECT
               *
               FROM
               ", x, "
               LIMIT 1"));
         df_types <- jamba::cPaste(lapply(nameVector(colnames(df)), function(i){
            class(df[[i]])
         }));
         desc_df <- data.frame(table=x,
            column=colnames(df),
            type=df_types);
         ## Get fields indexed
         index_df <- sqlite_indices(con, tbl_name=x)
         desc_df$index <- desc_df$column %in% index_df$idx_fields;
      }
      desc_df$db_class <- x_type;
      return(desc_df);
   } else {
      sql_txt <- DBI::dbGetQuery(con,
         paste0("
            SELECT
            sql
            FROM
            sqlite_master
            WHERE
            (type = 'table' and
            tbl_name = ?) or
            (type = 'view' and
            name = ?)"),
         param=list(c(x), c(x)))[,1];
      if (cat_text) {
         cat(sql_txt, "\n");
         invisible(sql_txt);
      } else {
         return(unlist(strsplit(sql_txt, "\n")));
      }
   }
}

#' Summarize a SQLite database view
#'
#' Summarize a SQLite database view
#'
#' This function is specific to SQLite database views, and
#' is intended to return deeper information about the view and
#' the underlying tables refereced. It will indicate the tables
#' or views referenced for each field, and will indicate whether
#' each field is associated with a database index.
#'
#' There are numerous allowed methods to create a view, and
#' this function tolerates many but definitely not all possible
#' styles.
#'
#' This function parses the SQL used to create the database view,
#' which is expected to have the format:
#'
#' `"SELECT ta.field1, tb.field2 FROM table_a ta, table_b tb WHERE ta.field1 = tb.field1"`
#'
#' The SELECT columns can be aliased, and can contain some math or
#' other syntax that combines multiple fields, within some limits
#' (see examples below).
#'
#' `"SELECT ta.field1 as alias1, tb.field2 as alias2 FROM table_a ta, table_b tb WHERE ta.field1 = tb.field1"`
#'
#' Some comments about recognized examples:
#'
#' * All database tables in the FROM clause must be aliased,
#' for example:
#' `"SELECT fc.* FROM file_channel fc"` uses the alias `"fc"`
#' to refer to the table `"file_channel"`.
#' * A database view in the FROM clause must also be aliases,
#' for example:
#' `"SELECT fv.* FROM fc_view fv"` uses the alias `"fv"`
#' to refer to the database view `"fc_view"`.
#' * All fields in the SELECT clause must use the table alias prefix,
#' for example:
#' `"SELECT fc.filename, fc.channel FROM file_channel fc"` uses the
#' table alias prefix `"fc."`, which refers to table `"file_channel"`.
#' * When a SELECT field references more than one table field,
#' each table field is parsed, but only the first field is
#' tested for corresponding column index. For example:
#' `"SELECT fc.time * fc.time_step as minutes FROM file_channel fc"`
#' refers to two fields `"fc.time"` and `"fc.time_step"`, and
#' assigns an alias `"minutes"`. In the output, the field name
#' is `"minutes"` and the first referenced column is `"fc.time"`.
#' Only `"fc.time"` will be tested for existence of a column index,
#' in the table `"file_channel"`.
#' * Within some reason, math is parsed to remove non-database
#' columns, for example `"fc.time_step*fc.nrow/60 as minutes"`
#' is recognized with fields `"fc.time", "fc.nrow"` and the alias
#' name `"minutes"`.
#' * A column index is only tested in the table referenced in the
#' table alias of the SELECT clause, even when the field is involved
#' in a JOIN with another table. The second table is not tested
#' for an index. (This might need to change in future.)
#' * Note that column index is represented from the source table,
#' even when the current database view references another
#' database view.
#'
#' Output is one of three formats:
#'
#' 1. When `type = "full"` a wide `data.frame` is returned, which
#' includes all associated fields in the SELECT clause, all
#' database tables and columns referenced in each field, the
#' name of the referenced table or view, and indication whether
#' the source table contains an index.
#' 2. When `type = "desc_df"` the output is equivalent to that
#' returned by `sqlite_desc()`, which only indicates the actual
#' field names in the database view, and whether the source table
#' column is indexed. It therefore does not indicate the source
#' table column name, which is otherwise hidden.
#' 3. When `type = "desc_df_tbl"` the output is similar to `"desc_df"`
#' except that the source table and table column values are returned,
#' in order to assist finding the location of the source table for
#' each field.
#'
#' @family jam database functions
#'
#' @return `data.frame` with content dependent upon `type` argument.
#'
#' @param con SQLite database connection, compatible with `DBI`.
#' @param x character name of a SQLite database view. For a list
#'    of database views, query the `sqlite_master` table, for
#'    example:
#'    `DBI::dbGetQuery(con, "SELECT name, type FROM sqlite_master WHERE type='view'")`
#' @param type character value indicating the type of output to
#'    return, as described above.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:");
#' DBI::dbCreateTable(db, "forecasts", data.frame(id=1:3, temperature=20:22));
#' DBI::dbGetQuery(db, "CREATE INDEX id_forecast_idx on forecasts(id)");
#'
#' # create second table
#' DBI::dbCreateTable(db, "feels", data.frame(temperature=20:22, feels=c("cold","warm","hot")));
#'
#' # create a view that joins these two tables
#' DBI::dbGetQuery(db, "CREATE VIEW ff_view as SELECT f.id, f.temperature, fe.feels FROM forecasts f, feels fe WHERE f.temperature = fe.temperature");
#' sqlite_view(db, "ff_view")
#'
#' @export
sqlite_view <- function
(con,
 x,
 type=c("full", "desc_df", "desc_df_tbl"),
 ...)
{
   ## Purpose is to interpret the SQL used to create a database view
   ## in order to parse information about each field
   type <- match.arg(type);
   sql_txt <- DBI::dbGetQuery(con,
      paste0("
         SELECT
         sql
         FROM
         sqlite_master
         WHERE
         type = 'view' and
         name = ?"),
      param=list(x))[,1];
   sql_txt <- gsub("[ \n]+", " ", sql_txt);
   if (length(sql_txt) == 0 || nchar(sql_txt) == 0) {
      return(NULL);
   }
   viewname <- gsub("^.*create view (.+) as select .+$", "\\1",
      ignore.case=TRUE,
      sql_txt);
   viewname;
   fields <- gsub("^.*create view (.+) as select (.+) from .+$", "\\2",
      ignore.case=TRUE,
      sql_txt);
   fields <- unlist(strsplit(fields, "[ ]*,[ ]*"));
   fields <- gsub(" as as[.]", " as.",
      sub("([a-zA-Z][a-zA-Z0-9_]*|[^a-zA-Z0-9_][0-9]+|[)])[ ]+([a-zA-Z][a-zA-Z0-9_]*)$",
         "\\1 as.\\2", fields));
   fields;
   #fields <- gsub(" as ", " ", fields);
   #fields <- sub("([a-zA-Z][a-zA-Z0-9_]*)[.]", "\\1!", fields);
   fields <- gsub("([a-zA-Z][a-zA-Z0-9_]*)[.]", "\\1!", fields);
   fields <- gsub("([^!])[*]", "\\1 * ", fields);
   fields <- gsub("[ ]+", " ",
      gsub(" [0-9*]+ ", " ",
         gsub("[^a-zA-Z0-9!*_]+", " ", fields)));
   fields_l <- lapply(strsplit(fields, "[ ]"), function(i){
      im <- matrix(nrow=1,
         t(jamba::rbindList(
            strsplit(gsub("^([^!]+)$", "!\\1", i), "!"),
            fixBlanks=FALSE)));
      colnames(im) <- jamba::makeNames(renameFirst=FALSE,
         rep(c("table_alias", "column"), length.out=ncol(im)))
      if (ncol(im) >=4 && im[,ncol(im)-1] %in% "as") {
         colnames(im)[ncol(im)] <- "column_alias";
         im <- im[,-(ncol(im)-1),drop=FALSE];
      }
      im;
   });
   fields_l;
   fields_cols <- unique(unlist(lapply(fields_l, colnames)));
   fields_df <- data.frame(check.names=FALSE,
      stringsAsFactors=FALSE,
      rbindList(lapply(fields_l, function(im){
         im <- im[,match(fields_cols, colnames(im)),drop=FALSE];
         colnames(im) <- fields_cols;
         im;
      })));
   #fields_df <- data.frame(rbindList(strsplit(fields, "[^a-zA-Z0-9_*]+")));
   #fields_df <- data.frame(rbindList(strsplit(fields, "[! .]")));
   #colnames(fields_df) <- jamba::makeNames(
   #   rep(c("table_alias", "column"), length.out=ncol(fields_df)),
   #   renameFirst=FALSE);
   ## Note we lose complex columns like "fc.time_step*fc.nrow/60"
   #fields_df <- fields_df[,1:2];
   sql_tables <- gsub("^.*create view (.+) as select (.+) from (.+) where .+$", "\\3",
      ignore.case=TRUE,
      sql_txt);
   sql_tables <- gsub(" as ", " ",
      unlist(strsplit(sql_tables, "[ ]*,[ ]*")));
   table_df <- data.frame(rbindList(strsplit(sql_tables, "[ ]+")));
   colnames(table_df) <- jamba::makeNames(
      rep(c("table", "table_alias"), length.out=ncol(table_df)),
      renameFirst=FALSE);
   table_df;
   fields_df$table <- table_df$table[match(fields_df$table_alias, table_df$table_alias)];
   if (any("*" %in% fields_df$column)) {
      fstars <- which(fields_df$column %in% c("*"));
      for (fstar in fstars) {
         fstar <- head(which(fields_df$column %in% c("*")), 1);
         ftable <- unique(fields_df$table[fstar]);
         fdesc <- sqlite_desc(con, ftable);
         fadd <- data.frame(table_alias=fields_df$table_alias[fstar],
            column=fdesc$column,
            table=fields_df$table[fstar]);
         frows <- seq_len(nrow(fields_df));
         f1 <- frows[frows < fstar];
         f2 <- frows[frows > fstar];
         fields_df <- rbind(fields_df[f1,,drop=FALSE],
            fadd,
            fields_df[f2,,drop=FALSE]);
      }
   }
   ## Fill blank column_alias with column values
   if (!"column_alias" %in% colnames(fields_df)) {
      fields_df$column_alias <- NA;
   }
   if (any(is.na(fields_df$column_alias))) {
      na_ca <- is.na(fields_df$column_alias);
      fields_df$column_alias[na_ca] <- fields_df$column[na_ca];
   }
   ## Make sure columns are uniquely named
   fields_df$column_alias <- jamba::makeNames(fields_df$column_alias,
      renameFirst=FALSE,
      suffix=".");
   ## Last, check which columns are indexed
   fields_df$row_order <- seq_len(nrow(fields_df));
   utables <- unique(fields_df$table);
   idx_df <- rbindList(lapply(utables, function(utable){
      #sqlite_indices(con, utable)
      sqlite_desc(con, utable, method="df")
   }));
   fields_df2 <- jamba::mixedSortDF(byCols="row_order",
      merge(fields_df,
         idx_df,
         all.x=TRUE,
         all.y=FALSE));
   fields_df2$viewname <- viewname;
   fcols <- setdiff(
      unique(c(
         "viewname", "column_alias", "type", "index",
         colnames(fields_df),
         colnames(idx_df))),
      "row_order");
   fields_df2 <- fields_df2[,fcols,drop=FALSE];
   tc_rename <- vigrep("^column($|_v[0-9]+$)", colnames(fields_df2));
   tc_renameto <- paste0("table_", tc_rename);
   fields_df2 <- renameColumn(fields_df2,
      from=c("column_alias", tc_rename),
      to=c("column", tc_renameto));
   rownames(fields_df2) <- fields_df2$column;
   if ("desc_df" %in% type) {
      return(renameColumn(
         fields_df2[,c("viewname", "column", "type", "index"),drop=FALSE],
         from=c("viewname"),
         to=c("table")));
   } else if ("desc_df_tbl" %in% type) {
      return(renameColumn(
         fields_df2[,c("table", "table_column", "type", "index"),drop=FALSE],
         from=c("table_column"),
         to=c("column")));
   }
   return(fields_df2);
}

#' Get SQLite table index information
#'
#' Get SQLite table index information
#'
#' This function returns a summary of table index data, derived
#' from the SQL used to create the index. As such, it recognizes
#' most but not all possible examples of valid syntax. It currently
#' does not support compound column indexes, but this limitation
#' may change in future, or upon request.
#'
#' Note that this function strictly queries the `sqlite_master` table
#' and parses the SQL for `type = 'index'`, and therefore will not
#' return information about database views. For that information,
#' try either `sqlite_view()` or `sqlite_desc()` both of which
#' indicate whether each field is associated with a column index.
#'
#' @family jam database functions
#'
#' @return `data.frame` with columns `"idx_tbls"` with database tables,
#'    and `"idx_fields"` containing the field indexed for each table.
#'
#' @param con valid SQLite database connection
#' @param tbl_name character vector of tables to return, or when
#'    `NULL` all tables are returned.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:");
#' DBI::dbCreateTable(db, "forecasts", data.frame(id=1:3, temperature=20:22));
#' DBI::dbGetQuery(db, "CREATE INDEX id_forecast_idx on forecasts(id)");
#' sqlite_indices(db, "forecasts")
#'
#' @export
sqlite_indices <- function
(con,
 tbl_name=NULL,
 ...)
{
   ## Check for view, in which case we will call sqlite_desc
   x_type <- DBI::dbGetQuery(con,
      "SELECT type FROM sqlite_master WHERE name = ? LIMIT 1",
      params=list(tbl_name))[,1];
   if ("view" %in% x_type) {
      idx_df1 <- sqlite_view(con, tbl_name);
      idx_df <- data.frame(idx_tbls=idx_df1$viewname,
         idx_fields=idx_df1$column);
      return(idx_df);
   }
   if (length(tbl_name) == 0) {
      tbl_name <- dbListTables(con);
   }
   idx_df <- DBI::dbGetQuery(con,
      "SELECT
      sql
      FROM
      sqlite_master
      WHERE
      type = 'index' AND
      tbl_name = ?",
      params=list(tbl_name))
   idx_txt <- gsub("^.+ ON ", "", ignore.case=TRUE, idx_df[,1]);
   idx_tbls <- gsub("[(].+", "", idx_txt);
   idx_fields <- gsub("^[^(]+[(](.+)[)][ ]*$", "\\1", idx_txt);
   data.frame(idx_tbls, idx_fields)
}

