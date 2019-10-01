
#' Import Ephys raw data file into relational database
#'
#' @param filename character string or vector of file names to import.
#'    Duplicate `filename` values are not permitted. If a duplicate
#'    `filename` is intended to be loaded as a separate entity,
#'    include all or path of the file path in order to make the
#'    `filename` character string unique in the database.
#' @param con database connection as produced by `DBI::dbConnect()`
#'    or compatible database-specific package `RSQLite::dbConnect()`.
#' @param mat_l optional R object containing data imported from
#'    `filename`, used to save time re-importing the same file if
#'    the data had already previously been loaded. Note that
#'    `filename` is always required, since the `filename` is
#'    the primary key for stored data. As such, duplicate `filename`
#'    values are not permitted. Note that `mat_l` may also be
#'    an environment containing the relevant data.
#' @param file_table character name of the database table to store
#'    Ephys file data.
#' @param file_event_table character name of the database table to store
#'    the file-to-event relationship. Events are stored for each
#'    file, and are separately applied to each channel in the file.
#' @param file_channel_table character name of the database table to store
#'    the signal for each channel that meets criteria by `channel_grep`.
#' @param channel_grep regular expression pattern used to identify
#'    measurement channels in the file. Any event names matched
#'    by `event_grep` are removed from the channel list.
#' @param time_step_grep regular expression pattern used to recognize
#'    the name of the time step, expected to be a suffix of
#'    the channel name. For example channel `"AI01"` would imply
#'    a time step name `"AI01.ts.step"`, from which the time step
#'    is obtained.
#' @param index_offset_grep regular expression pattern used to recognize
#'    the name of the index offset, expected to be a suffix of
#'    the channel name. For example, channel `"AI01"` would imply
#'    an index offset name `"AI01.ind"`.
#' @param event_grep regular expression pattern used to identify
#'    names of events in the file. Note that event names take
#'    priority over channel names.
#' @param drop_grep regular expression pattern used to exclude
#'    certain channel names from being stored.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#'
#' @export
import_ephys_file <- function
(filename,
 con,
 mat_l=NULL,
 project="",
 phase="",
 file_table="ephys_file",
 file_event_table="file_event",
 file_channel_table="file_channel",
 #channel_grep="^[A-Z]+[0-9]+$",
 channel_grep="^(AI|FP)(0[1-4]|1[7-9]|2[0])$",
 time_step_grep="[.]ts[.]step$",
 index_offset_grep="[.]ind$",
 event_grep="^EVT[0-9]+$",
 drop_grep="^(WB|SP|AI|FP).*$",
 start_grep="^Start$",
 stop_grep="^Stop$",
 duration_grep="^AllFile$",
 valid=TRUE,
 override=FALSE,
 override_method=c("invalidate", "delete"),
 verbose=TRUE,
 ...)
{
   ## Function imports a single Ephys data file
   ## - parses channel names using channel_grep
   ## - gets time_step per channel using channel_name and time_step_grep
   ## - gets index_offset per channel using channel_name and index_offset_grep
   ## - gets extended channel names using channel names and any suffix
   ## - gets event names using event_grep
   ## - gets drop list using drop_grep, except channel_names and event_names
   if (length(filename) == 0) {
      stop("filename must be provided.");
   }
   override_method <- match.arg(override_method);
   if (length(valid) == 0) {
      valid <- TRUE;
   }
   file_tables <- c(file_table,
      file_event_table,
      file_channel_table);

   ## First check if data is already loaded for this file
   isLoadedL <- sapply(file_tables, function(itable){
      if (itable %in% DBI::dbListTables(con)) {
         isLoaded <- DBI::dbGetQuery(con,
            paste0(
               "SELECT count(*) FROM ",
               itable,
               " WHERE filename = ?"),
            param=list(basename(filename))
         )[,1];
      } else {
         0;
      }
   });
   if (any(isLoadedL > 0)) {
      if (!override) {
         printDebug("filename '",
            basename(filename),
            "' already exists in the database, use ",
            "override=TRUE",
            " to force update.");
         return(NULL);
      }
      for (itable in file_tables[isLoadedL > 0]) {
         if (override_method %in% "invalidate") {
            if (verbose) {
               printDebug("import_ephys_file(): ",
                  "Updating previous entries in ",
                  itable,
                  " with ",
                  "valid = 0");
            }
            setInvalid <- DBI::dbGetQuery(con,
               paste0("UPDATE ",
                  itable,
                  " SET valid = 0 WHERE filename = ?"),
               param=list(basename(filename)));
         } else if (override_method %in% "delete") {
            if (verbose) {
               printDebug("import_ephys_file(): ",
                  "Deleting previous entries in ",
                  itable);
            }
            setInvalid <- DBI::dbGetQuery(con,
               paste0("DELETE FROM ",
                  itable,
                  " WHERE filename = ?"),
               param=list(basename(filename)));
         }
      }
   }

   ## Load data if mat_l is not supplied
   if (length(mat_l) == 0) {
      mat_l <- new.env();
      mat_loaded <- load(filename, envir=mat_l);
      get_mat_l <- function(name, default=NULL) {
         if (name %in% ls(mat_l)) {
            get(name, envir=mat_l);
         } else {
            default;
         }
      }
   } else if (is.environment(mat_l)) {
      mat_loaded <- ls(envir=mat_l);
      get_mat_l <- function(name, default=NULL) {
         if (name %in% ls(mat_l)) {
            get(name, envir=mat_l);
         } else {
            default;
         }
      }
   } else {
      mat_loaded <- names(mat_l);
      get_mat_l <- function(name, default=NULL) {
         if (name %in% names(mat_l)) {
            mat_l[[name]];
         } else {
            default;
         }
      }
   }

   ## sdim - dimensions of the file
   sdim_file <- sdim(mat_l);

   ## channel names
   channel_names <- jamba::nameVector(jamba::provigrep(channel_grep, mat_loaded));

   ## time_step_names
   time_step_names <- jamba::provigrep(
      jamba::nameVector(
         paste0(channel_names, ".*", time_step_grep),
         channel_names),
      returnType="list",
      mat_loaded);
   if (!all(lengths(time_step_names) == 1)) {
      printDebug("import_ephys_file(): ",
         "time_step_grep did not find time_step names for each channel:",
         jamba::cPaste(channel_names));
      printDebug(unname(
         provigrep(channel_names,
            returnType="list",
            mat_loaded)),
         sep="",
         collapse=",");
      stop("Could not determine time_step names for each channel.");
   }
   time_step_names <- unlist(time_step_names);

   ## index_offset_names
   index_offset_names <- jamba::provigrep(
      jamba::nameVector(
         paste0(channel_names, ".*", index_offset_grep),
         channel_names),
      returnType="list",
      mat_loaded);
   if (!all(lengths(index_offset_names) == 1)) {
      printDebug("import_ephys_file(): ",
         "index_offset_grep did not find index_offset names for each channel:",
         jamba::cPaste(channel_names));
      printDebug(unname(
         provigrep(channel_names,
            returnType="list",
            mat_loaded)),
         sep="",
         collapse=",");
      stop("Could not determine index_offset names for each channel.");
   }
   index_offset_names <- unlist(index_offset_names);

   ## event names
   event_names <- jamba::nameVector(jamba::provigrep(event_grep, mat_loaded));

   ## Start,Stop names
   start_name <- head(jamba::provigrep(start_grep, mat_loaded), 1);
   stop_name <- head(jamba::provigrep(stop_grep, mat_loaded), 1);
   duration_name <- head(jamba::provigrep(duration_grep, mat_loaded), 1);
   start_val <- get_mat_l(start_name);
   stop_val <- get_mat_l(stop_name);
   duration_val <- get_mat_l(duration_name);
   if (length(duration_val) > 0) {
      duration <- diff(range(duration_val));
   } else {
      duration <- 0;
   }

   ## drop names
   keep_names <- unname(
      c(channel_names,
         time_step_names,
         index_offset_names,
         event_names,
         start_name,
         stop_name));
   drop_names_all <- jamba::provigrep(drop_grep, mat_loaded);
   drop_names <- setdiff(drop_names_all,
      keep_names);
   keep_names_all <- setdiff(mat_loaded, drop_names);

   names_df <- data.frame(name=mat_loaded,
      keep_names_all=ifelse(mat_loaded %in% keep_names_all, "X", ""),
      channel=ifelse(mat_loaded %in% channel_names, "X", ""),
      time_step=ifelse(mat_loaded %in% time_step_names, "X", ""),
      index_offset=ifelse(mat_loaded %in% index_offset_names, "X", ""),
      event=ifelse(mat_loaded %in% event_names, "X", ""),
      drop=ifelse(mat_loaded %in% drop_names, "X", "")
   );
   sdim_df <- data.frame(sdim_file,
      names_df[match(rownames(sdim_file), names_df$name),,drop=FALSE]);

   ## Populate file_table with new entry
   file_df <- data.frame(
      filename=basename(filename),
      filepath=dirname(filename),
      project=project,
      phase=phase,
      valid=head(valid, 1),
      start=tail(start_val, 1),
      stop=tail(stop_val, 1),
      duration=duration,
      sdim=I(list(emblob(sdim_file)))
   )

   ## - create table if necessary
   ## - check if entry already exists
   ## - add new entry
   if (!file_table %in% DBI::dbListTables(con)) {
      if (verbose) {
         printDebug("import_ephys_file(): ",
            "Calling DBI::dbCreateTable() ",
            file_table);
      }
      DBI::dbCreateTable(con=con,
         name=file_table,
         fields=file_df[0,,drop=FALSE]);
   }

   ## Populate table with new file data
   if (verbose) {
      printDebug("import_ephys_file(): ",
         "Calling DBI::dbAppendTable() ",
         file_table);
   }
   DBI::dbAppendTable(con,
      name=file_table,
      value=file_df);

   ## Populate the event table
   event_df <- rbindList(lapply(event_names, function(event_name) {
      event_m <- get_mat_l(event_name);
      data.frame(filename=basename(filename),
         event_name=event_name,
         event_num=seq_len(nrow(event_m)),
         event_step=event_m[,1],
         valid=head(valid, 1)
      )
   }));
   if (!file_event_table %in% DBI::dbListTables(con)) {
      if (verbose) {
         printDebug("import_ephys_file(): ",
            "Calling DBI::dbCreateTable() ",
            file_event_table);
      }
      DBI::dbCreateTable(con=con,
         name=file_event_table,
         fields=event_df[0,,drop=FALSE]);
   }
   ## Populate table with new file data
   if (verbose) {
      printDebug("import_ephys_file(): ",
         "Calling DBI::dbAppendTable() ",
         file_event_table);
   }
   DBI::dbAppendTable(con,
      name=file_event_table,
      value=event_df);
   ## tidyr::spread(event_df, event_name, event_step
   if (verbose) {
      printDebug("",
         "Populated ",
         nrow(event_df),
         " rows into the ",
         file_event_table,
         " table.")
   }

   ## Iterate each channel
   ## - populate database
   ## Populate the signal for each channel
   signal_df <- rbindList(lapply(channel_names, function(channel_name) {
      signal_m <- get_mat_l(channel_name);
      signal_blob <- emblob(signal_m);
      channel_prefix <- gsub("[0-9]+.*$", "", channel_name);
      time_step <- get_mat_l(time_step_names[channel_name])[1,1];
      index_offset <- tail(get_mat_l(index_offset_names[channel_name])[,1], 1);
      signal_mean <- mean(signal_m[,1], na.rm=TRUE);
      signal_range <- range(signal_m[,1], na.rm=TRUE);
      data.frame(filename=basename(filename),
         channel=channel_name,
         channel_prefix=channel_prefix,
         animal="",
         time_step=time_step,
         index_offset=index_offset,
         nrow=nrow(signal_m),
         ncol=ncol(signal_m),
         mean=signal_mean,
         min=signal_range[1],
         max=signal_range[2],
         dynamic_range=diff(signal_range),
         valid=head(valid, 1),
         signal=I(list(signal_blob))
      )
   }));

   if (!file_channel_table %in% DBI::dbListTables(con)) {
      if (verbose) {
         printDebug("import_ephys_file(): ",
            "Calling DBI::dbCreateTable() ",
            file_channel_table);
      }
      DBI::dbCreateTable(con=con,
         name=file_channel_table,
         fields=signal_df[0,,drop=FALSE]);
   }

   ## Populate table with new file data
   if (verbose) {
      printDebug("import_ephys_file(): ",
         "Calling DBI::dbAppendTable() ",
         file_channel_table);
   }
   DBI::dbAppendTable(con,
      name=file_channel_table,
      value=signal_df);
   if (verbose) {
      printDebug("",
         "Populated ",
         nrow(signal_df),
         " rows into the ",
         file_channel_table,
         " table.")
   }

   invisible(mat_loaded);
}

#' Convert R object to serialized blob
#'
#' Convert R object to serialized blob
#'
#' @return serialized blob objects sufficient to store in a relational
#'    database. If input `x` was a list, the output will be a list
#'    of serialized blob objects.
#'
#' @param x R object
#' @param connection,ascii arguments passed to `base::serialize()`.
#' @param do_list logical indicating whether to process `x` as a list.
#'
#' @export
emblob <- function
(x,
 connection=NULL,
 ascii=TRUE,
 do_list=FALSE)
{
   if (do_list) {
      lapply(x, function(i){
         serialize(i,
            connection=connection,
            ascii=ascii)
      });
   } else {
      serialize(x,
         connection=connection,
         ascii=ascii)
   }
}

#' Convert serialized blob to R object
#'
#' Convert serialized blob to R object
#'
#' @return R object stored in a serialized blob.
#'
#' @param x serialized blob object.
#' @param connection,ascii arguments passed to `base::serialize()`.
#' @param do_list logical indicating whether to process `x` as a list.
#'
#' @export
deblob <- function
(x,
 do_list=is.list(x))
{
   if (do_list) {
      lapply(x, function(i){
         unserialize(i)
      });
   } else {
      unserialize(x)
   }
}

