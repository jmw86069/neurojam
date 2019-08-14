
#' Save raw signal data per animal
#'
#' Save raw signal data per animal
#'
#' This function stores the output from `import_ephys_mat_1()`
#' as an `RData` file, with annotation stored
#' in a relational database table that points to the
#' `RData` file.
#'
#' The `RData` file name is derived from the arguments:
#' `animal`, `project`, `phase`, `source_filename`, and
#' `source_dirname`. Each value is sent to `ephys_short_name()`
#' which creates a shorter abbrevation, then non-blank
#' values are concatenated using `"_"` underscore delimiter.
#' If this output does not produce a unique name for a
#' large set of files, there are two options:
#'
#' 1. Make the `source_filename` input value unique before
#' calling `save_animal_raw_data()`, or
#' 2. Use `...` to send arguments to `ephys_short_name()`,
#' specifically `max_nchar_word=4` which by default keeps
#' the first 4 characters of each detected word. Increasing
#' this number may result in more unique filenames, at the
#' expense of longer filenames.
#'
#' In general two fields should enforce uniqueness:
#' `"animal"` and `"source_filename"`. An exception to
#' this rule is when an experiment run is split across
#' two files, which may occur if the instrument is
#' interrupted in the middle of an experiment run.
#'
#' @family jam database functions
#'
#' @param mat_l `list` object output from `import_ephys_mat_1()`
#'    which should contain all relevant channel data, along with
#'    time step, index, and event tables, as available and relevant.
#' @param con database connection with class `DBIConnection`,
#'    for example produced from `DBI::dbConnect()`. The database
#'    can be of any type compatible with `DBI`, for example
#'    `dbConnect(RMySQL::MySQL())` produces an object of
#'    class `MySQLConnection` which extends `DBIConnection`.
#' @param animal character string indicating the animal associated
#'    with the `mat_l` data.
#' @param project character string with the project name, used to
#'    differentiate raw data between projects.
#' @param phase character string indicating the project phase,
#'    used to differentiate raw data between different phases
#'    of the same project.
#' @param source_filename character string with the filename
#'    used to produce `mat_l`
#' @param source_dirname character string with the file path to
#'    the `source_filename`.
#' @param rdata_filename character string of the `RData` file
#'    to create, by default when `NULL` a filename is created
#'    based upon the animal, project, and phase.
#' @param rdata_dirname character string with the file path
#'    location to save the output `RData` file.
#' @param animal_raw_data_table character string with the
#'    database table name to store data, inside the connection
#'    `con`.
#' @param overwrite logical indicating whether to overwrite
#'    any existing `RData` file. When `overwrite=FALSE` and
#'    the `RData` file exists, no save will be performed
#'    to the database connection `con`, nor to the file system.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional arguments are passed to `ephys_short_name()`,
#'    specifically `max_nchar_word` controls the length of
#'    words retained in output filenames.
#'
#' @examples
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:");
#'
#' @export
save_animal_raw_data <- function
(mat_l,
 con,
 animal,
 project,
 phase="",
 source_filename="",
 source_dirname="",
 rdata_filename=NULL,
 rdata_dirname="animal_rdata",
 animal_raw_data_table="animal_raw_data",
 overwrite=FALSE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to store output from import_ephys_mat_1()
   if (!extends(class(con), "DBIConnection")) {
      stop("Connection 'con' must extend class 'DBIConnection'.");
   }
   if (!is.list(mat_l)) {
      stop("The 'mat_l' argument is expected to be a list.");
   }
   if (length(animal) == 0) {
      stop("The 'animal' argument must be supplied.");
   }
   if (length(project) == 0) {
      stop("The 'project' argument must be supplied.");
   }
   ## Check that rdata_dirname exists
   if (length(rdata_dirname) == 0 || nchar(rdata_dirname) == 0) {
      rdata_dirname <- ".";
   }
   if (!dir.exists(rdata_dirname)) {
      if (verbose) {
         printDebug("save_animal_raw_data(): ",
            "Creating rdata_dirname:",
            rdata_dirname);
      }
      isDir <- dir.create(rdata_dirname,
         mode="0775");
      if (!isDir) {
         stop("dir.create() failed to create rdata_dirname");
      }
   }

   ## Generate an RData filename
   if (length(rdata_filename) == 0 || nchar(rdata_filename) == 0) {
      rdata_list <- jamba::rmNULL(list(animal,
         ephys_short_name(project, ...),
         ephys_short_name(phase, ...),
         ephys_short_name(source_filename, ...),
         ephys_short_name(source_dirname, ...)));
      rdata_list <- lapply(rdata_list, function(i){
         if (nchar(i) == 0) {
            NULL;
         } else {
            i;
         }
      });
      rdata_filename <- paste0(
         paste(rdata_list, collapse="_"),
         ".RData");
      if (verbose) {
         printDebug("save_animal_raw_data(): ",
            rdata_list);
         printDebug("save_animal_raw_data(): ",
            "Writing to rdata_filename:",
            rdata_filename);
         printDebug("save_animal_raw_data(): ",
            "Writing to rdata_dirname:",
            rdata_dirname);
      }
   }
   ## Save to RData file
   rdata_path <- file.path(rdata_dirname,
      rdata_filename);
   ## Check for pre-existing RData file
   if (file.exists(rdata_path)) {
      if (!overwrite) {
         stop(paste0("Output RData file exists, ",
            "use overwrite=TRUE to force writing to this file.",
            rdata_path));
      }
      if (verbose) {
         printDebug("save_animal_raw_data(): ",
            "Overwriting existing RData file:",
            rdata_path);
      }
   }
   base::save(list=c("mat_l"),
      file=rdata_path);

   ## Write to table
   raw_df <- data.frame(
      animal=animal,
      project=project,
      phase=phase,
      source_filename=source_filename,
      source_dirname=source_dirname,
      rdata_filename=rdata_filename,
      rdata_dirname=rdata_dirname);

   ## Check that the database table exists
   if (!DBI::dbExistsTable(con, animal_raw_data_table)) {
      ## TODO: create table here
      ## dbCreateTable(con, "iris", iris)
      if (verbose) {
         printDebug("save_animal_raw_data(): ",
            "Creating database table:",
            animal_raw_data_table);
      }
      DBI::dbCreateTable(con=con,
         name=animal_raw_data_table,
         fields=raw_df[0,,drop=FALSE]);
   }

   ## Check if rows already exist for this animal and filename
   sql_row_ct <-paste0("
       SELECT
         count(*)
       FROM
         ", animal_raw_data_table, "
       WHERE
         animal = ? and
         rdata_filename = ?");
   animal_file_ct <- DBI::dbGetQuery(con,
      sql_row_ct,
      param=list(animal,
         rdata_filename))[,1];
   if (any(animal_file_ct > 0)) {
      ## Rows exist for this RData file
      if (verbose) {
         printDebug("save_animal_raw_data(): ",
            "Updating existing rows for animal:",
            animal,
            ", rdata_filename:",
            rdata_filename);
      }
      sql_update <- paste0("
          UPDATE
            ", animal_raw_data_table, "
          SET
            animal = ?,
            project = ?,
            phase = ?,
            source_filename = ?,
            source_dirname = ?,
            rdata_filename = ?,
            rdata_dirname = ?
          WHERE
            animal = ? and
            rdata_filename = ?");
      sent_sql <- DBI::dbSendQuery(con=con,
         sql_update,
         param=list(animal,
            project,
            phase,
            source_filename,
            source_dirname,
            rdata_filename,
            rdata_dirname,
            animal,
            rdata_filename));
   } else {
      ## Insert rows into this table
      if (verbose) {
         printDebug("save_animal_raw_data(): ",
            "Inserting rows.");
      }
      sql_insert <- DBI::sqlAppendTable(con=con,
         table=animal_raw_data_table,
         values=raw_df);
      sent_sql <- DBI::dbSendQuery(con=con,
         statement=sql_insert);
   }
   invisible(sent_sql);
}

#' Save raw signal data per animal
#'
#' Save raw signal data per animal
#'
#' @family jam database functions
#'
#' @export
get_animal_raw_data <- function
(con,
 animal=NULL,
 project=NULL,
 phase=NULL,
 source_filename=NULL,
 source_dirname=NULL,
 rdata_filename=NULL,
 rdata_dirname=NULL,
 animal_raw_data_table="animal_raw_data",
 return_rdata=FALSE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to retrieve output from import_ephys_mat_1()
   ## Full table of raw data available
   if (!DBI::dbExistsTable(con, animal_raw_data_table)) {
      stop(
         paste0("Database table does not exist:",
            animal_raw_data_table));
   }
   rdata_df <- DBI::dbGetQuery(con, paste0("
      SELECT
        *
      FROM
        ", animal_raw_data_table));
   rownames(rdata_df) <- jamba::makeNames(rdata_df$animal);
   param_names <- c("animal", "project", "phase",
      "source_filename", "source_dirname",
      "rdata_filename", "rdata_dirname");
   ## Subset by each provided parameter
   for (param_name in param_names) {
      val <- get(param_name)
      if (length(val) > 0 && nchar(val) > 0) {
         if (verbose) {
            printDebug("get_animal_raw_data(): ",
               "filtering on ",
               param_name,
               ", for values:",
               val);
         }
         rdata_df <- subset(rdata_df, rdata_df[[param_name]] %in% val);
      }
   }
   if (!return_rdata) {
      return(rdata_df);
   }

   ## Iterate each row and retrieve the RData

   rdata_paths <- pasteByRow(
      rdata_df[,c("rdata_dirname", "rdata_filename"), drop=FALSE],
      sep="/");
   names(rdata_paths) <- rownames(rdata_df);
   mat_ll <- lapply(nameVector(rownames(rdata_df)), function(irow){
      irdata <- pasteByRow(
         rdata_df[irow,c("rdata_dirname", "rdata_filename"), drop=FALSE],
         sep="/");
      rdata_name <- load(irdata);
      if (length(rdata_name) > 1) {
         stop(paste0(
            "RData '",
            irdata,
            "' contained multiple objects, only one is allowed: ",
            paste(rdata_name, collapse=", ")));
      }
      if (!"mat_l" %in% rdata_name) {
         stop(paste0(
            "RData '",
            irdata,
            "' must contain one object named 'mat_l' and does not:",
            paste(rdata_name, collapse=", ")));
      }
      channels <- vigrep("^AI[0-9]+$", names(mat_l));
      ## Filter for channels having 10000 or more rows
      sdim_channels <- sdim(mat_l)[channels,,drop=FALSE];
      channels <- rownames(subset(sdim_channels, rows > 10000));
      ts_step <- mat_l[[paste0(head(channels, 1), ".ts.step")]][1,1];
      animal <- unname(attr(mat_l[[head(channels, 1)]], "animal"));
      attr(mat_l, "animal") <- rdata_df[irow,"animal"];
      attr(mat_l, "project") <- rdata_df[irow,"project"];
      attr(mat_l, "phase") <- rdata_df[irow,"phase"];
      attr(mat_l, "channels") <- channels;
      attr(mat_l, "step") <- ts_step;
      mat_l;
   });
   mat_ll;
}

#' Extract animal event data from raw data
#'
#' Extract animal event data from raw data
#'
#' This function uses `get_animal_raw_data()` to
#' query the database, retrieve the associated
#' animal raw data, then extracts the available
#' event data.
#'
#' In cases where event data for one animal is split across
#' two files, the event names are re-numbered in order
#' that the events appear across files for the same
#' animal. For example, for two files that contain two
#' events each for the same animal, events for the
#' first file will be labeled `c("1", "2")` and the second
#' file with be labeled `c("3", "4")`. The output is
#' therefore suitable to send to `save_animal_event_data()`
#' so the events can be stored without conflict.
#'
#' @family jam database functions
#'
#'
#' @export
extract_event_data <- function
(con,
 animal=NULL,
 project=NULL,
 phase=NULL,
 source_filename=NULL,
 source_dirname=NULL,
 rdata_filename=NULL,
 rdata_dirname=NULL,
 animal_raw_data_table="animal_raw_data",
 event_prestart=4000,
 event_poststop=4000,
 verbose=FALSE,
 ...)
{
   ## Purpose is to retrieve output from import_ephys_mat_1()
   ## Todo: add event_prestart, event_poststop to the table schema

   mat_ll <- get_animal_raw_data(con=con,
      animal=animal,
      project=project,
      phase=phase,
      source_filename=source_filename,
      source_dirname=source_dirname,
      rdata_filename=rdata_filename,
      rdata_dirname=rdata_dirname,
      animal_raw_data_table=animal_raw_data_table,
      return_rdata=TRUE,
      verbose=verbose,
      ...);
   eventsml <- lapply(jamba::nameVectorN(mat_ll), function(mat_name){
      mat_l <- mat_ll[[mat_name]];
      animal <- attr(mat_l, "animal");
      project <- attr(mat_l, "project");
      phase <- attr(mat_l, "phase");
      channels <- attr(mat_l, "channels");
      step <- attr(mat_l, "step");

      ## Retrieve event data with some error-checking
      events_m <- tryCatch({
         event_data_l1 <- get_ephys_event_data(mat_l,
            channels=channels,
            event_prestart=event_prestart,
            event_poststop=event_poststop,
            ...);
         if (length(event_data_l1) == 0) {
            return(NULL);
         }
         if (!"animal" %in% names(attributes(event_data_l1$events_m))) {
            attr(event_data_l1$events_m, "animal") <- animal;
         }
         if (!"project" %in% names(attributes(event_data_l1$events_m))) {
            attr(event_data_l1$events_m, "project") <- project;
         }
         if (!"phase" %in% names(attributes(event_data_l1$events_m))) {
            attr(event_data_l1$events_m, "phase") <- phase;
         }
         if (!"step" %in% names(attributes(event_data_l1$events_m))) {
            attr(event_data_l1$events_m, "step") <- step;
         }
         if (!"event_prestart" %in% names(attributes(event_data_l1$events_m))) {
            attr(event_data_l1$events_m, "event_prestart") <- event_prestart;
         }
         if (!"event_poststop" %in% names(attributes(event_data_l1$events_m))) {
            attr(event_data_l1$events_m, "event_poststop") <- event_poststop;
         }
         event_data_l1$events_m;
      }, error=function(e){
         printDebug("Error:");
         print(e);
         NULL;
      });
      data.frame(animal=animal,
         project=project,
         phase=phase,
         step=step,
         event_prestart=event_prestart,
         event_poststop=event_poststop,
         mat_name=mat_name,
         events_m=I(list(events_m))
      )
   });
   ## Final pass to renumber events when animal data is split across files
   if (length(eventsml) == 0 || all(sdim(eventsml)$rows == 0)) {
      return(NULL);
   }
   events_df <- jamba::rbindList(eventsml);

   ## Re-number events when animal event data is split across multiple files
   dupe_animals <- names(jamba::tcount(events_df$animal, minCount=2));
   if (length(dupe_animals) > 0) {
      for (dupe_animal in dupe_animals) {
         animal_rows <- which(events_df$animal %in% dupe_animal);
         event_num <- 0;
         for (animal_row in animal_rows) {
            if (event_num > 0) {
               ievents_m <- events_df[animal_row,"events_m"][[1]];
               inrow <- nrow(ievents_m);
               irownames <- seq_len(inrow) + event_num;
               if (verbose) {
                  printDebug("extract_event_data(): ",
                     "Adjusting rownames for dupe_animal:",
                     dupe_animal,
                     ", new rownames:",
                     irownames);
               }
               rownames(ievents_m) <- as.character(irownames);
               events_df[["events_m"]][animal_row] <- list(ievents_m);
               rm(ievents_m);
            }
            event_num <- event_num + inrow;
         }
      }
   }
   return(events_df);
}

#' Make Ephys short name
#'
#' Make Ephys short name
#'
#' This function simply creates a short name given
#' a label, using some logic specific to Ephys naming
#' strategies.
#'
#' @family jam database functions
#'
#' @param x character vector to be modified to a short name.
#' @param max_nchar_word integer indicating the maximum
#'    characters retained per word
#' @param word_pattern character pattern used to define characters
#'    which are considered "word" characters.
#' @param remove_extension logical indicating whether to remove
#'    file extensions, defined by "." followed by alphanumeric
#'    characters.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' x <- c("FearConditioning",
#'    "Fear Conditioning",
#'    "Fear_Conditioning",
#'    "Fear-Conditioning");
#' ephys_short_name(x);
#'
#' @export
ephys_short_name <- function
(x,
 max_nchar_word=4,
 word_pattern="[a-zA-Z0-9]",
 remove_extension=TRUE,
 ...)
{
   if (nchar(x) == 0) {
      return(NULL);
   }
   if (remove_extension) {
      x <- gsub("[.][a-zA-Z0-9]+$", "", x);
   }
   max_nchar_word <- max(c(1, round(max_nchar_word)));
   pattern1 <- paste0("\\b(",
      word_pattern,
      "{1,",
      max_nchar_word,
      "})",
      word_pattern,
      "*\\b");
   gsub("[- ]+", "",
      gsub(pattern1, "\\1",
         gsub("([a-z])([A-Z])", "\\1 \\2",
            gsub("_", " ", x))))
}

#' Save event signal data per animal
#'
#' Save event signal data per animal
#'
#' This function saves the subset of raw signal per
#' per animal, per channel, and per annotated event.
#'
#' @family jam database functions
#'
#' @param con database connection with class `DBIConnection`,
#'    for example produced from `DBI::dbConnect()`. The database
#'    can be of any type compatible with `DBI`, for example
#'    `dbConnect(RMySQL::MySQL())` produces an object of
#'    class `MySQLConnection` which extends `DBIConnection`.
#' @param events_m matrix containing lists of numeric vectors,
#'    where each cell contains a numeric vector of raw signal.
#'    The `colnames(events_m)` are channel names, and
#'    `rownames(events_m)` are event names. Alternatively,
#'    `events_m` can be a list of matrices, in which case
#'    each element in the list is handled independently.
#'    When `events_m` is supplied as a list, other
#'    arguments are taken from the attributes including
#'    `animal`, `step`, `event_prestart`,
#'    `event_poststop`. The values for `project`, `phase`
#'    are passed on, but if they are empty, the corresponding
#'    value in attributes is used if possible.
#' @param animal character string with the animal identifier.
#' @param project,phase character string with project name,
#'    and the project phase, respectively.
#' @param step numeric value indicating the time step between
#'    each numeric measurement, assumed to be in units of seconds.
#'    When `step` is `NULL` the attribute `"step"` is retrieved
#'    if possible, otherwise an error is thrown.
#' @param event_prestart,event_poststop integer values indicating
#'    the number of time steps before and after the event start
#'    and event stop, which are included in the signal being stored.
#'    If either value is `NULL` the attribute of the same name
#'    is retrieved if possible, otherwise an error is thrown.
#' @param animal_event_data_table character string with the name
#'    of the database table in which to store event data.
#' @param dryrun logical indicating whether to perform database
#'    operations. Otherwise when `dryrun` is `FALSE`, no database
#'    operations are performed, including not creating the table
#'    if it does not exist.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional arguments are passed to `get_animal_event_data()`,
#'    typically used for optional arguments relevant to event data.
#'
#' @export
save_animal_event_data <- function
(con,
 events_m,
 animal=NULL,
 project=NULL,
 phase=NULL,
 step=NULL,
 event_prestart=NULL,
 event_poststop=NULL,
 animal_event_data_table="animal_event_data",
 dryrun=FALSE,
 verbose=FALSE,
 ...)
{
   #
   ## Purpose is to store output from import_ephys_mat_1()
   if (!extends(class(con), "DBIConnection")) {
      stop("Connection 'con' must extend class 'DBIConnection'.");
   }
   if (!is.matrix(events_m) && is.list(events_m)) {
      if (!all(sapply(events_m, is.matrix))) {
         stop("When events_m is a list, it must be a list containing only matrix entries.");
      }
      ## Iterate each entry one by one
      event_res <- lapply(seq_along(events_m), function(in1){
         ievents_m <- events_m[[in1]];
         save_animal_event_data(con=con,
            events_m=ievents_m,
            project=project,
            phase=phase,
            dryrun=dryrun,
            verbose=verbose,
            ...);
      })
      return(event_res);
   }
   if (!is.matrix(events_m)) {
      stop("The 'events_m' argument is expected to be a matrix.");
   }
   if (length(animal) == 0) {
      animal <- attr(events_m, "animal");
      if (length(animal) == 0) {
         stop("The 'animal' argument must be supplied or be present in attributes(events_m).");
      }
   }
   if (length(project) == 0 || nchar(project) == 0) {
      project <- attr(events_m, "project");
      if (length(project) == 0 || nchar(project) == 0) {
         stop(paste0("The 'project' argument must be supplied and must be non-empty, ",
            "or be present in attributes(events_m)"));
      }
   }
   if (length(phase) == 0 || nchar(phase) == 0) {
      phase <- attr(events_m, "phase");
      if (length(phase) == 0) {
         phase <- "";
      }
   }
   if (length(step) == 0) {
      step <- attr(events_m, "step");
      if (length(step) == 0) {
         stop("The 'step' value must be provided, or present in attr(events_m, 'step')");
      }
   }
   if (length(event_prestart) == 0) {
      event_prestart <- attr(events_m, "event_prestart");
      if (length(event_prestart) == 0) {
         stop("event_prestart must be supplied or must be in attributes(events_m).");
      }
   }
   if (length(event_poststop) == 0) {
      event_poststop <- attr(events_m, "event_poststop");
      if (length(event_poststop) == 0) {
         stop("event_poststop must be supplied or must be in attributes(events_m).");
      }
   }

   ## Check if data is already present in the database
   if (verbose) {
      printDebug("save_animal_event_data(): ",
         "animal:", animal,
         ", project:", project,
         ", phase:", phase,
         ", step:", step,
         ", event_prestart:", event_prestart,
         ", event_poststop:", event_poststop);
   }
   animevdf <- get_animal_event_data(con,
      animal=animal,
      project=project,
      phase=phase,
      channel=colnames(events_m),
      event=rownames(events_m),
      event_prestart=event_prestart,
      event_poststop=event_poststop,
      return_signal=FALSE,
      ...);
   nrow_expected <- prod(dim(events_m));
   if (length(animevdf) > 0 && nrow(animevdf) > 0) {
      nrow_observed <- nrow(animevdf);
      if (nrow_observed == nrow_expected) {
         if (verbose) {
            printDebug("save_animal_event_data(): ",
               "No data was inserted, data for animal '",
               animal,
               "' already exists in table '",
               animal_event_data_table,
               "' with ",
               nrow_expected,
               " rows as expected.",
               fgText=c("orange", "lightgreen"));
         }
      } else {
         printDebug("save_animal_event_data(): ",
            "No data was inserted, data for animal '",
            animal,
            "' already exists in table '",
            animal_event_data_table,
            "' with ",
            nrow_observed,
            " rows, but ",
            nrow_expected,
            " rows were expected.",
            fgText=c("orange", "red"));
      }
      return(NULL);
   }

   ## Convert event signal to serialized "blob"
   event_signal <- lapply(events_m, function(i){
      serialize(i,
         connection=NULL,
         ascii=TRUE);
   });

   ## Write to table
   ncell <- prod(dim(events_m));
   event_df <- data.frame(
      animal=rep(animal, ncell),
      project=rep(project, ncell),
      phase=rep(phase, ncell),
      step=rep(step, ncell),
      channel=rep(colnames(events_m), each=nrow(events_m)),
      event=rep(rownames(events_m), ncol(events_m)),
      event_prestart=rep(event_prestart, ncell),
      event_poststop=rep(event_poststop, ncell)
   );
   event_df$event_signal <- I(event_signal);

   ## Check that the database table exists
   if (!DBI::dbExistsTable(con, animal_event_data_table)) {
      ## TODO: create table here
      ## dbCreateTable(con, "iris", iris)
      if (verbose) {
         printDebug("save_animal_event_data(): ",
            "Creating database table:",
            animal_event_data_table);
      }
      if (!dryrun) {
         DBI::dbCreateTable(con=con,
            name=animal_event_data_table,
            fields=event_df[0,,drop=FALSE]);
      } else if (verbose) {
         printDebug("save_animal_event_data(): ",
            "Skipping table creation due to ",
            "dryrun=",
            dryrun,
            fgText=c("orange", "red"));
      }
   }

   ## Insert rows into this table
   sql_insert <- paste0("
   INSERT INTO
     ", animal_event_data_table, "
   VALUES
     (:animal, :project, :phase, :step, :channel, :event, :event_prestart, :event_poststop, :event_signal)");
   if (!dryrun) {
      if (verbose) {
         printDebug("save_animal_event_data(): ",
            "Inserting rows.");
      }
      res2 <- DBI::dbExecute(con,
         sql_insert,
         param=event_df);
   } else {
      printDebug("save_animal_event_data(): ",
         "Skipping insert due to ",
         "dryrun=",
         dryrun,
         fgText=c("orange", "red"));
   }
}


#' Get animal event data
#'
#' @family jam database functions
#'
#' @export
get_animal_event_data <- function
(con,
 animal=NULL,
 project=NULL,
 phase=NULL,
 channel=NULL,
 event=NULL,
 event_prestart=NULL,
 event_poststop=NULL,
 animal_event_data_table="animal_event_data",
 return_signal=FALSE,
 ...)
{
   ## Full data.frame of available animal event data, without event_signal
   if (!DBI::dbExistsTable(con=con, animal_event_data_table)) {
      printDebug("get_animal_event_data(): ",
         "Table does not exist:",
         animal_event_data_table);
      return(NULL);
   }
   animevdf <- DBI::dbGetQuery(con=con, paste0("
      SELECT
        animal,
        project,
        phase,
        channel,
        event,
        event_prestart,
        event_poststop,
        step
      FROM
        ", animal_event_data_table));

   ## Subset by each provided parameter
   param_names <- c("animal",
      "project",
      "phase",
      "channel",
      "event",
      "event_prestart",
      "event_poststop");
   for (param_name in param_names) {
      val <- get(param_name)
      if (length(val) > 0 && nchar(val) > 0) {
         if (verbose) {
            printDebug("get_animal_event_data(): ",
               "filtering on ",
               param_name,
               ", for values:",
               val);
         }
         animevdf <- subset(animevdf, animevdf[[param_name]] %in% val);
      }
   }

   if (!return_signal) {
      return(animevdf);
   }

   animevdata <- DBI::dbGetQuery(con=con, "
      SELECT
        event_signal
      FROM
        animal_event_data
      WHERE
        animal = ? and
        project = ? and
        phase = ? and
        channel = ? and
        event = ? and
        event_prestart = ? and
        event_poststop = ?",
      param=as.list(
         unname(
            animevdf[,c("animal",
               "project",
               "phase",
               "channel",
               "event",
               "event_prestart",
               "event_poststop"),drop=FALSE]
         )
      ));
   animevdata1 <- lapply(animevdata[,1], unserialize);
   animevdf$event_signal <- I(animevdata1);
   return(animevdf);
}

