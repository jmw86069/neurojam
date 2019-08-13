
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
   animal_file_ct <- dbGetQuery(con,
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
   if (!dbExistsTable(con, animal_raw_data_table)) {
      stop(
         paste0("Database table does not exist:",
            animal_raw_data_table));
   }
   rdata_df <- dbGetQuery(con, paste0("
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
         mat_name=mat_name,
         events_m=I(list(events_m))
      )
   });
   return(jamba::rbindList(eventsml));
}

#' Make Ephys short name
#'
#' Make Ephys short name
#'
#' This function simply creates a short name given
#' a label, using some logic specific to Ephys naming
#' strategies.
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
#' @param con database connection with class `DBIConnection`,
#'    for example produced from `DBI::dbConnect()`. The database
#'    can be of any type compatible with `DBI`, for example
#'    `dbConnect(RMySQL::MySQL())` produces an object of
#'    class `MySQLConnection` which extends `DBIConnection`.
#' @param events_m matrix containing lists of numeric vectors,
#'    where each cell contains a numeric vector of raw signal.
#'    The `colnames(events_m)` are channel names, and
#'    `rownames(events_m)` are event names.
#'
#' @export
save_animal_event_data <- function
(con,
 events_m,
 animal,
 project,
 phase="",
 step=NULL,
 animal_event_data_table="animal_event_data",
 ...)
{
   #
   ## Purpose is to store output from import_ephys_mat_1()
   if (!extends(class(con), "DBIConnection")) {
      stop("Connection 'con' must extend class 'DBIConnection'.");
   }
   if (!is.matrix(events_m)) {
      stop("The 'events_m' argument is expected to be a matrix.");
   }
   if (length(animal) == 0) {
      stop("The 'animal' argument must be supplied.");
   }
   if (length(project) == 0) {
      stop("The 'project' argument must be supplied.");
   }
   if (length(phase) == 0 || nchar(phase) == 0) {
      if ("phase" %in% names(attributes(events_m))) {
         phase <- attr(events_m, "phase");
      }
   }
   if (length(step) == 0 || nchar(step) == 0) {
      if ("step" %in% names(attributes(events_m))) {
         step <- attr(events_m, "step");
      }
   }
   if (length(step) == 0) {
      stop("The 'step' value must be provided, or present in attr(events_m, 'step')");
   }

   ## Write to table
   event_signal <- lapply(events_m, function(i){
      serialize(i,
         connection=NULL,
         ascii=TRUE);
   });

   ncell <- prod(dim(events_m));
   event_df <- data.frame(
      animal=rep(animal, ncell),
      project=rep(project, ncell),
      phase=rep(phase, ncell),
      step=rep(step, ncell),
      channel=rep(colnames(events_m), each=nrow(events_m)),
      event=rep(rownames(events_m), ncol(events_m)));
   event_df$event_signal <- I(event_signal);
   # animal_event_data_table <- "animal_event_data"

   ## Check that the database table exists
   if (!DBI::dbExistsTable(con, animal_event_data_table)) {
      ## TODO: create table here
      ## dbCreateTable(con, "iris", iris)
      if (verbose) {
         printDebug("save_animal_event_data(): ",
            "Creating database table:",
            animal_event_data_table);
      }
      DBI::dbCreateTable(con=con,
         name=animal_event_data_table,
         fields=event_df[0,,drop=FALSE]);
   }

   ## Insert rows into this table
   if (verbose) {
      printDebug("save_animal_event_data(): ",
         "Inserting rows.");
   }
   k <- 1;
   sql_insert <- paste0("
   INSERT INTO
     ", animal_event_data_table, "
   VALUES
     (:animal, :project, :phase, :step, :channel, :event, :event_signal)");
   res2 <- DBI::dbExecute(con,
      sql_insert,
      param=event_df);
   #for (k in seq_len(nrow(event_df))) {
      #sql_insert <- DBI::sqlAppendTable(con=con,
      #   table=animal_event_data_table,
      #   values=event_df[k,,drop=FALSE]);
      #sent_sql <- DBI::dbSendQuery(con=con,
      #   statement=sql_insert);
   #}
   invisible(res2);
}


#' Get animal event data
#'
#' @export
get_animal_event_data <- function
(con,
 animal=NULL,
 project=NULL,
 phase=NULL,
 channel=NULL,
 event=NULL,
 return_signal=FALSE,
 ...)
{
   ## Full data.frame of available animal event data, without event_signal
   animevdf <- dbGetQuery(con=con, "
      SELECT
        animal,
        project,
        phase,
        channel,
        event,
        step
      FROM
        animal_event_data");
   if (length(animal) > 0 && nchar(animal) > 0) {
      animevdf <- subset(animevdf,
         animevdf$animal %in% animal);
   }
   if (length(project) > 0 && nchar(project) > 0) {
      animevdf <- subset(animevdf,
         animevdf$project %in% project);
   }
   if (length(phase) > 0 && nchar(phase) > 0) {
      animevdf <- subset(animevdf,
         animevdf$phase %in% phase);
   }
   if (length(channel) > 0 && nchar(channel) > 0) {
      animevdf <- subset(animevdf,
         animevdf$channel %in% channel);
   }
   if (length(event) > 0 && nchar(event) > 0) {
      animevdf <- subset(animevdf,
         animevdf$event %in% event);
   }
   if (!return_signal) {
      return(animevdf);
   }

   animevdata <- dbGetQuery(con=con, "
      SELECT
        event_signal
      FROM
        animal_event_data
      WHERE
        animal = ? and
        project = ? and
        phase = ? and
        channel = ? and
        event = ?",
      param=as.list(
         unname(
            animevdf[,c("animal", "project", "phase", "channel", "event"),drop=FALSE]
         )
      ));
   animevdata1 <- lapply(animevdata[,1], unserialize);
   animevdf$event_signal <- I(animevdata1);
   return(animevdf);
}

