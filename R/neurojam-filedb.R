
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

#' Extract animal event data from database
#'
#' Extract animal event data from database
#'
#' @export
extract_event_db <- function
(con,
 filename=NULL,
 file_table="ephys_file",
 file_event_table="file_event",
 file_channel_table="file_channel",
 file_channel_event_table="file_channel_event",
 event_prestart=4,
 event_poststop=4,
 event_type="event",
 default_event_diff=40,
 load_db=TRUE,
 reuse_db=TRUE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to re-work extract_event_data() to use the filename db
   ##
   ## Todo: make this function capable of handling a mix of
   ## pre-existing and new data. Currently if any data exists,
   ## it returns it rather than processing anything potentially new.
   ## This function also processes all channels per file, and
   ## not a subset of channels.
   if (length(event_type) != 1) {
      stop(paste0("event_type is expected to have one value,",
         " but length(event_type)=",
         length(event_type)));
   }

   ## First check if data already exists, then return as-is
   ## filename data
   if (reuse_db && file_channel_event_table %in% DBI::dbListTables(con)) {
      fce_pre_df <- DBI::dbGetQuery(con, paste0("
         SELECT
         *
         FROM
         ", file_channel_event_table, "
         WHERE
         valid = 1 and
         filename = ? and
         event_type = ? and
         event_prestart = ? and
         event_poststop = ?"),
         param=list(filename,
            event_type,
            event_prestart,
            event_poststop));
      if (nrow(fce_pre_df) > 0) {
         if (verbose) {
            printDebug("extract_event_db(): ",
               "Returning previously stored data for filename:",
               filename);
         }
         return(fce_pre_df);
      }
   } else {
      fce_pre_df <- NULL;
   }


   ## event data
   event_wide <- get_event_table_db(con=con,
      file_event_table=file_event_table,
      base_max_duration=base_max_duration,
      filename=filename);
   ## Purpose is to retrieve output from import_ephys_mat_1()
   ## Todo: add event_prestart, event_poststop to the table schema

   ## New: verify filename exists

   ## New: load raw signal per channel for the given filename
   ## - convert event_wide to index positions using time_step
   ##
   ## channel data
   channel_df <- DBI::dbGetQuery(con, paste0("
      SELECT
      *
      FROM
      ", file_channel_table, "
      WHERE
      valid = 1 and
      filename = ?"),
      param=list(filename));
   ## filename data
   filename_df <- DBI::dbGetQuery(con, paste0("
      SELECT
      *
      FROM
      ", file_table, "
      WHERE
      valid = 1 and
      filename = ?"),
      param=list(filename));

   #channel_event_df <- mergeAllXY(
   #   dplyr::select(channel_df, -signal),
   #   dplyr::select(event_wide[,c("filename", "event_num", "event_start", "event_end")], -EVT01));

   ## iterate each channel
   if (verbose) {
      printDebug("extract_event_db(): ",
         "Iterating each of ",
         nrow(channel_df),
         " channels for filename:",
         filename);
   }
   file_channel_event_df <- rbindList(lapply(seq_len(nrow(channel_df)), function(channel_row){
      channel <- channel_df$channel[channel_row];
      time_step <- channel_df$time_step[channel_row];
      index_offset <- channel_df$index_offset[channel_row];
      signal <- deblob(channel_df$signal[[channel_row]]);
      ifilename <- channel_df$filename[channel_row];
      ## calculate appropriate start and end
      event_prestart_index <- event_prestart / time_step;
      event_poststop_index <- event_poststop / time_step;
      all_start <- subset(filename_df, filename %in% ifilename)$start;
      all_start_index <- all_start / time_step;
      event_nums <- subset(event_wide, filename %in% ifilename)$event_num;
      event_starts <- subset(event_wide, filename %in% ifilename)$event_start / time_step;
      event_stops <- subset(event_wide, filename %in% ifilename)$event_stop / time_step;
      event_diffs <- event_stops - event_starts;
      index_starts <- (event_starts - event_prestart_index - all_start_index);
      index_stops <- (event_stops + event_poststop_index - all_start_index);
      event_seq <- jamba::nameVector(seq_along(event_nums), event_nums);
      iuse <- which(index_starts >= 1 & index_stops < nrow(signal));
      ## Note:
      ## In future this step would be where to apply time "masks"
      ## per channel, filename, for specific time ranges. This mask
      ## could be used to convert numeric values to NA.
      if (verbose) {
         printDebug("extract_event_db(): ",
            "   Iterating ",
            length(iuse),
            " valid events of ",
            length(index_stops),
            " total events for channel:",
            channel);
      }
      event_signals <- lapply(event_seq[iuse], function(iseq){
         i_range <- seq(from=ceiling(index_starts[iseq]),
            to=ceiling(index_stops[iseq]));
         i_m <- signal[i_range,,drop=FALSE];
         attr(i_m, "filename") <- ifilename;
         attr(i_m, "time_step") <- time_step;
         attr(i_m, "label_start") <- event_prestart_index + 1;
         attr(i_m, "label_stop") <- event_prestart_index + 1 + event_diffs[iseq];
         attr(i_m, "i_range") <- range(i_range);
         attr(i_m, "event_num") <- event_nums[iseq];
         attr(i_m, "event_prestart") <- event_prestart;
         attr(i_m, "event_poststop") <- event_poststop;
         i_m;
      });
      if (length(event_signals) == 0) {
         return(NULL);
      }
      event_blobs <- emblob(event_signals, do_list=TRUE);
      ## - filename
      ## - channel
      ## - event
      ## - event_prestart
      ## - event_poststop
      ## - time_step
      ## - signal
      ## - event_type
      fce_df <- data.frame(filename=ifilename,
         channel=channel,
         event_num=event_nums[iuse],
         event_prestart=event_prestart,
         event_poststop=event_poststop,
         time_step=time_step,
         event_nrow=sdim(event_signals)$rows,
         valid=1
      );
      fce_df$event_signal <- I(event_blobs);
      fce_df[,"event_type"] <- head(event_type, 1);
      fce_df;
   }));

   if (load_db) {
      ## - create table if necessary
      ## - check if entry already exists
      ## - add new entry
      if (!file_channel_event_table %in% DBI::dbListTables(con)) {
         if (verbose) {
            printDebug("extract_event_db(): ",
               "Calling DBI::dbCreateTable() ",
               file_channel_event_table);
         }
         DBI::dbCreateTable(con=con,
            name=file_channel_event_table,
            fields=file_channel_event_df[0,,drop=FALSE]);
      }

      ## Populate table with new file data
      if (verbose) {
         printDebug("extract_event_db(): ",
            "Calling DBI::dbAppendTable() ",
            file_channel_event_table);
      }
      DBI::dbAppendTable(con,
         name=file_channel_event_table,
         value=file_channel_event_df);
   } else if (verbose) {
      printDebug("extract_event_db(): ",
         "Skipping db loading.");
   }
   file_channel_event_df;
}

#' Retrieve event data by filename from the database
#'
#' Retrieve event data by filename from the database
#'
#' @export
get_event_table_db <- function
(con,
 filename,
 default_event_diff=40,
 event_start_name="EVT02",
 event_stop_name="EVT03",
 file_event_table="file_event",
 rename_columns=TRUE,
 ...)
{
   ##
   event_start_name <- head(event_start_name, 1);
   event_stop_name <- head(event_stop_name, 1);
   ## tidyr::spread(event_df, event_name, event_step
   event_df <- DBI::dbGetQuery(con,
      paste0("SELECT *
         FROM ", file_event_table, "
         WHERE filename = ?"),
      param=list(filename));

   event_wide <- tidyr::spread(event_df, event_name, event_step);
   event_wide;
   if (!all(c(event_start_name, event_stop_name) %in% colnames(event_wide))) {
      printDebug("The event start and stop names are not in the event_wide colnames:");
      print(head(event_wide, 30));
      stop("event_start_name, event_stop_name not found in event data.");
   }
   na_start <- is.na(event_wide[,event_start_name]);
   na_stop <- is.na(event_wide[,event_stop_name]) |
      (!is.na(event_wide[,event_start_name]) & !is.na(event_wide[,event_stop_name]) &
            event_wide[,event_start_name] > event_wide[,event_stop_name]);
   if (any(na_start & !na_stop)) {
      event_wide[(na_start & !na_stop),event_start_name] <-
         (event_wide[(na_start & !na_stop),event_stop_name] - default_event_diff);
   }
   if (any(!na_start & na_stop)) {
      event_wide[(!na_start & na_stop),event_stop_name] <-
         (event_wide[(!na_start & na_stop),event_start_name] + default_event_diff);
   }
   if (rename_columns) {
      event_wide <- renameColumn(event_wide,
         from=c(event_start_name, event_stop_name),
         to=c("event_start", "event_stop"));
   }
   return(event_wide);
}


#' Extract animal baseline signal data from raw db
#'
#' Extract animal baseline signal data from raw db
#'
#' @export
extract_baseline_db <- function
(con,
   filename=NULL,
   project=NULL,
   phase=NULL,
   file_table="ephys_file",
   file_event_table="file_event",
   file_channel_table="file_channel",
   file_channel_event_table="file_channel_event",
   before_event_only=TRUE,
   event_prestart=4,
   baseline_step=60,
   baseline_max_duration=480,
   all_start_fixed=NULL,
   event_type="baseline",
   load_db=TRUE,
   verbose=FALSE,
   ...)
{
   ## Purpose is to emulate extract_event_data() specifically
   ## for non-event signal, generally before the first event.
   ## Also this function uses the file database schema
   if (length(event_type) != 1) {
      stop(paste0("event_type is expected to have one value,",
         " but length(event_type)=",
         length(event_type)));
   }

   ## First check if data already exists, then return as-is
   ## filename data
   if (file_channel_event_table %in% DBI::dbListTables(con)) {
      fce_pre_df <- DBI::dbGetQuery(con, paste0("
         SELECT
         *
         FROM
         ", file_channel_event_table, "
         WHERE
         valid = 1 and
         event_prestart = 0 and
         event_poststop = 0 and
         filename = ? and
         event_type = ?"),
         param=list(filename,
            event_type));
      if (nrow(fce_pre_df) > 0) {
         if (verbose) {
            printDebug("extract_baseline_db(): ",
               "Returning previously stored data for filename:",
               filename);
         }
         return(fce_pre_df);
      }
   } else {
      fce_pre_df <- NULL;
   }

   ## event data
   event_wide <- get_event_table_db(con=con,
      file_event_table=file_event_table,
      base_max_duration=base_max_duration,
      filename=filename);

   ## channel data
   channel_df <- DBI::dbGetQuery(con, paste0("
      SELECT
      *
      FROM
      ", file_channel_table, "
      WHERE
      valid = 1 and
      filename = ?"),
      param=list(filename));

   ## filename data
   filename_df <- DBI::dbGetQuery(con, paste0("
      SELECT
      *
      FROM
      ", file_table, "
      WHERE
      valid = 1 and
      filename = ?"),
      param=list(filename));

   ## Extract baseline ranges for each channel
   if (verbose) {
      printDebug("extract_baseline_db(): ",
         "Iterating each of ",
         nrow(channel_df),
         " channels for filename:",
         filename);
   }
   file_channel_event_df <- rbindList(lapply(seq_len(nrow(channel_df)), function(channel_row){
      channel <- channel_df$channel[channel_row];
      time_step <- channel_df$time_step[channel_row];
      index_offset <- channel_df$index_offset[channel_row];
      signal <- deblob(channel_df$signal[[channel_row]]);
      ifilename <- channel_df$filename[channel_row];

      ## calculate appropriate start and end
      all_start <- subset(filename_df, filename %in% ifilename)$start;
      all_start_index <- all_start / time_step;

      index_start1 <- NULL;
      if (before_event_only) {
         event_prestart_index <- event_prestart / time_step;
         event_starts <- subset(event_wide, filename %in% ifilename)$event_start / time_step;
         event_start1 <- min(event_starts);
         index_start1 <- (event_start1 - event_prestart_index - all_start_index);
      }
      if (length(baseline_max_duration) > 0) {
         baseline_max <- baseline_max_duration / time_step;
      } else {
         baseline_max <- Inf;
      }
      baseline_max_index <- min(c(nrow(signal),
         baseline_max,
         index_start1 - 1));
      if (baseline_max_index < 0) {
         if (verbose) {
            printDebug("extract_baseline_db(): ",
               "First event start is less than 1. Skipping.");
         }
         return(NULL);
      }

      ## Define baseline ranges
      baseline_step_index <- baseline_step/time_step;
      baseline_steps <- seq(from=0,
         to=baseline_max_index,
         by=baseline_step_index);
      baseline_start_index <- baseline_steps + 1;
      baseline_stop_index <- noiseFloor(baseline_steps + baseline_step_index,
         ceiling=baseline_max_index);
      baseline_diff_index <- baseline_stop_index - baseline_start_index + 1;
      iuse <- (baseline_diff_index >= baseline_step_index * 0.2);
      baseline_start_index <- baseline_start_index[iuse];
      baseline_stop_index <- baseline_stop_index[iuse];
      event_nums <- seq_along(baseline_start_index);
      names(baseline_start_index) <- makeNames(
         renameFirst=FALSE,
         rep("baseline", length.out=length(baseline_start_index)),
         suffix="_");
      if (verbose) {
         printDebug("extract_baseline_db(): ",
            "   Iterating ",
            length(event_nums),
            " baseline ranges for channel:",
            channel);
      }
      event_signals <- lapply(nameVector(event_nums), function(iseq){
         i_range <- seq(from=ceiling(baseline_start_index[iseq]),
            to=ceiling(baseline_stop_index[iseq]));
         i_m <- signal[i_range,,drop=FALSE];
         attr(i_m, "filename") <- ifilename;
         attr(i_m, "time_step") <- time_step;
         attr(i_m, "label_start") <- 1;
         attr(i_m, "label_stop") <- event_prestart_index + 1 + event_diffs[iseq];
         attr(i_m, "i_range") <- length(i_range);
         attr(i_m, "event_num") <- iseq;
         attr(i_m, "event_prestart") <- 0;
         attr(i_m, "event_poststop") <- 0;
         i_m;
      });
      if (length(event_signals) == 0) {
         return(NULL);
      }
      event_blobs <- emblob(event_signals, do_list=TRUE);
      fce_df <- data.frame(filename=ifilename,
         channel=channel,
         event_num=event_nums[iuse],
         event_prestart=0,
         event_poststop=0,
         time_step=time_step,
         event_nrow=sdim(event_signals)$rows,
         valid=1
      );
      fce_df$event_signal <- I(event_blobs);
      fce_df[,"event_type"] <- head(event_type, 1);
      fce_df;
   }));

   if (load_db) {
      ## - create table if necessary
      ## - check if entry already exists
      ## - add new entry
      if (!file_channel_event_table %in% DBI::dbListTables(con)) {
         if (verbose) {
            printDebug("extract_baseline_db(): ",
               "Calling DBI::dbCreateTable() ",
               file_channel_event_table);
         }
         DBI::dbCreateTable(con=con,
            name=file_channel_event_table,
            fields=file_channel_event_df[0,,drop=FALSE]);
      }

      ## Populate table with new file data
      if (verbose) {
         printDebug("extract_baseline_db(): ",
            "Calling DBI::dbAppendTable() ",
            file_channel_event_table);
      }
      DBI::dbAppendTable(con,
         name=file_channel_event_table,
         value=file_channel_event_df);
   } else if (verbose) {
      printDebug("extract_baseline_db(): ",
         "Skipping db loading.");
   }
   file_channel_event_df;
}

#' Calculate biwavelet frequency-time matrix for an event
#'
#' Calculate biwavelet frequency-time matrix for an event
#'
#' @export
calculate_wavelet_matrix <- function
(signal,
 new_step=0.1,
 dj=1/16,
 s0_factor=5,
 s0=NULL,
 mother="morlet",
 do.sig=FALSE,
 type="power",
 freq_range=c(1, 20),
 freq_step=0.1,
 column_pad=c(0, 0),
 row_pad=c(0, 0),
 verbose=FALSE,
 ...)
{
   ## Todo:
   ## - when data is extremely large, subdivide by time bins
   ## - bins should be 2-minutes plus a buffer size on each side,
   ## - then pieces of the result should be "stitched" together.
   ##
   ##
   ## create second column using time stamps
   time_step <- attr(signal, "time_step");
   label_start <- attr(signal, "label_start");
   if (ncol(signal) == 1) {
      #xtime <- seq(from=time_step - (label_start * time_step),
      #   by=time_step,
      #   length.out=nrow(signal));
      xtime <- seq(from=label_start,
         by=time_step,
         length.out=nrow(signal));
      signal <- cbind(x=signal[,1], time=xtime);
   }
   ## biwavelet on signal
   x_condense_factor <- new_step / time_step;
   if (length(s0) == 0) {
      s0 <- s0_factor * time_step;
   }

   t1 <- Sys.time();
   i_m <- calc_ephys_wavelet(x=signal,
      return_type="m",
      type=type,
      x_condense_factor=x_condense_factor,
      step=time_step,
      dj=dj,
      s0=s0*1,
      mother=mother,
      do.sig=do.sig,
      column_pad=column_pad,
      row_pad=row_pad,
      verbose=verbose);
   ## Convert matrix with period units to frequency in hertz
   iM2normal <- matrix_period2hz(i_m,
      freq_method="2",
      freq_range=freq_range,
      freq_step=freq_step,
      verbose=verbose);
   t2 <- Sys.time();
   if (verbose) {
      printDebug("calculate_wavelet_matrix(): ",
         "Calculation took:",
         format(t2 - t1));
   }
   attr(iM2normal, "s0") <- s0;
   attr(iM2normal, "dj") <- dj;
   attr(iM2normal, "mother") <- mother;
   attr(iM2normal, "type") <- type;
   iM2normal;
}

