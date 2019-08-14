#' Get Ephys event data
#'
#' @family jam event functions
#'
#' @param mat_l `list` containing raw signal output from
#'    `import_ephys_mat_1()` when importing a Matlab `.mat` file,
#'    or from `get_animal_raw_data()` when retrieving raw signal
#'    output from a database. It should contain `matrix` entries
#'    whose names match argument `event_grep`, typically one
#'    matrix with start values, and one matrix with stop values.
#' @param channels character vector of channel names with raw signal
#'    data in `mat_l`.
#' @param events integer vector of events to retain, in order they
#'    appear in the event start table. When `events` is `NULL`, then
#'    all events are returned.
#' @param event_offset integer index to the list of event matrices
#'    whose named matched `event_grep`, which contains event time
#'    offset values, or NULL to use no offset values. Sometimes the
#'    first event table contains an offset value to apply before
#'    taking the event start and stop times.
#' @param event_start,event_stop integer index to the list of event
#'    matrices whose named matched `event_grep`, which contains event
#'    start and stop times. By default the second (2) and third (3)
#'    matrices are used for start and stop times, respectively.
#' @param default_event_diff integer value with the default
#'    time in seconds between event_start and event_stop values,
#'    intended when data in event_stop is truncated. For example
#'    when the event_start matrix has 3 rows, and the event_stop matrix
#'    has 2 rows, the third row of event_stop is calculated by adding
#'    `default_event_diff` to the third row of event_start.
#' @param event_prestart,event_poststop integer number of steps to
#'    include before each event start, and after each event stop,
#'    respectively.
#' @param event_label character value used to create a text label
#'    for each event, where event is numbered starting from 1.
#' @param event_grep character pattern used to identify the names in
#'    `mat_l` which represent numeric event matrices. The default is
#'    expected to match `"EVT01"`, `"EVT02"`, `"EVT03"`, where the
#'    `"EVT02"` contains event start values, and `"EVT03"` contains
#'    event stop values.
#' @param attr_names character vector of attributes to include
#'    within each event data matrix.
#' @param all_start_grep character pattern of the name in `mat_l`
#'    containing the overall start offset position. The default
#'    uses `mat_l$Start`.
#' @param all_start_row integer row number of the matrix defined
#'    by `all_start_grep` to use as the overall start offset. The
#'    start offset can be verified by using argument `verbose=TRUE`.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#'
#' @export
get_ephys_event_data <- function
(mat_l,
 channels,
 events=NULL,
 event_offset=NULL,
 event_start=2,
 event_stop=3,
 default_event_diff=40,
 event_prestart=4000,
 event_poststop=4000,
 event_label="Tone",
 event_grep="^EV",
 attr_names=c("animal", "channel", "ts.step", "ts"),
 all_start_grep="^Start",
 all_start_row=2,
 verbose=FALSE,
 ...)
{
   ##########################################
   ##
   ## mat_l is the matlab list object exported from .pl2 to .mat
   ## channel is the name of the measured channel for which data
   ##    will be extracted
   ## events is an integer vector of event data to keep. When NULL,
   ##    all event data is returned
   ## event_offset is the index of event name to use for event index offset
   ##    if NULL then no offset is used
   ## event_start is the index of event name that contains event start indices
   ## event_stop is the index of event name that contains event stop indices
   ## event_grep is the pattern to use when identifying names(mat_l) which
   ##    are the elements containing event data
   ##
   ## attr_names are the names of attributes to include
   ##    within each event data matrix
   ##
   if (!all(channels %in% names(mat_l))) {
      stop(paste0("channels not found in names(mat_l):",
         cPaste(setdiff(channels, names(mat_l))))
      );
   }
   ## Event data
   event_names <- provigrep(event_grep, names(mat_l));
   event_data <- mat_l[event_names];

   if (nrow(event_data[[event_start]]) > nrow(event_data[[event_stop]])) {
      if (length(default_event_diff) > 0) {
         if (verbose) {
            printDebug("get_ephys_event_data(): ",
               "Repairing incomplete event stop, from:");
            print(event_data[[event_stop]]);
         }
         new_event_rows <- event_data[[event_start]] + default_event_diff;
         diff_nrow <- nrow(event_data[[event_start]]) - nrow(event_data[[event_stop]]);
         event_data[[event_stop]] <- rbind(event_data[[event_stop]],
            tail(new_event_rows, diff_nrow));
         if (verbose) {
            printDebug("get_ephys_event_data(): ",
               "Repairing incomplete event stop, to:");
            print(event_data[[event_stop]]);
         }
      }
   }
   ## Check that start and stop dimensions are identical
   if (nrow(event_data[[event_start]]) != nrow(event_data[[event_stop]])) {
      stop(paste0("The nrow of event_start '",
         event_names[event_start],
         "', and nrow of event_stop '",
         event_names[event_stop],
         "' are not equal. event_names:",
         cPaste(event_names, doSort=FALSE))
      );
   }

   ## Transform eventData into milliseconds
   if ("ts.step" %in% names(attributes(mat_l[[head(channels, 1)]]))) {
      i_step <- attr(mat_l[[head(channels, 1)]], "ts.step")[1,1];
   } else {
      ts_channel <- paste0(channels[1], ".ts.step");
      if (ts_channel %in% names(mat_l)) {
         i_step <- mat_l[[ts_channel]][1,1];
      } else {
         stop("Could not find 'ts.step' in attributes(mat_l[[channels[1]]]) nor names(mat_l)");
      }
   }
   if ("ind" %in% names(attributes(mat_l[[head(channels, 1)]]))) {
      i_ind <- tail(attr(mat_l[[head(channels, 1)]], "ind")[,1], 1);
   } else {
      ind_channel <- paste0(channels[1], ".ind");
      if (ind_channel %in% names(mat_l)) {
         i_ind <- tail(mat_l[[ind_channel]][,1], 1);
      } else {
         stop("Could not find 'ind' in attributes(mat_l[[channels[1]]]) nor names(mat_l)");
      }
   }
   if (verbose) {
      printDebug("get_ephys_event_data(): ",
         "i_step:", i_step);
      printDebug("get_ephys_event_data(): ",
         "i_ind:", i_ind);
   }

   ## Overall Start
   all_start <- 0;
   if (length(all_start_grep) > 0 && nchar(all_start_grep) > 0) {
      all_start_name <- head(vigrep(all_start_grep, names(mat_l)), 1);
      if (verbose) {
         printDebug("get_ephys_event_data(): ",
            "all_start_name:",
            all_start_name);
      }
      if (length(all_start_name) > 0) {
         all_start_m <- mat_l[[all_start_name]];
         if (nrow(all_start_m) < all_start_row) {
            printDebug("get_ephys_event_data(): ",
               "mat_l[['",
               all_start_name,
               "']] did not have nrow > all_start_row (",
               all_start_row,
               ")");
            printDebug("get_ephys_event_data(): ",
               "Returning NULL event data.");
            return(NULL);
         }
         all_start <- all_start_m[all_start_row,1];
      }
   }
   if (verbose) {
      printDebug("get_ephys_event_data(): ",
         "all_start:",
         all_start);
   }

   ## Determine event start and stop indices
   if (length(event_offset) > 0) {
      i_starts <- 0 +
         -event_data[[event_offset]][1,1] +
         event_data[[event_start]][,1] * (1/i_step) +
         -all_start * (1/i_step) +
         i_ind;
      i_stops <- 0 +
         -event_data[[event_offset]][,1] +
         event_data[[event_stop]][,1] * (1/i_step) +
         -all_start * (1/i_step) +
         i_ind;
   } else {
      i_starts <- 0 +
         event_data[[event_start]][,1] * (1/i_step) +
         -all_start * (1/i_step) +
         i_ind;
      i_stops <- 0 +
         event_data[[event_stop]][,1] * (1/i_step) +
         -all_start * (1/i_step) +
         i_ind;
   }

   ## Define names
   i_events <- seq_along(i_starts);
   names(i_starts) <- i_events;
   names(i_stops) <- i_events;

   ## Get specific data for each event
   if (length(events) > 0) {
      if (!any(events %in% i_events)) {
         stop(paste0("No requested events (",
            cPaste(events, doSort=FALSE),
            " are present in observed i_events:",
            cPaste(i_events, doSort=FALSE))
         );
      }
      i_events <- i_events[i_events %in% events];
      i_starts <- i_starts[as.character(i_events)];
      i_stops <- i_stops[as.character(i_events)];
   }

   i_labels <- makeNames(suffix=" ",
      rep(
         c("Signal start",
            paste0(event_label, " start"),
            paste0(event_label, " end")),
         lengths(
            list(i_ind,
               i_starts,
               i_stops))
      ));

   ret_l <- list();
   ret_l$event_data <- event_data;
   ret_l$step <- i_step;
   ret_l$ind <- i_ind;
   ret_l$events <- i_events;
   ret_l$starts <- i_starts;
   ret_l$stops <- i_stops;
   ret_l$labels <- i_labels;

   if (length(names(i_events)) == 0) {
      names(i_events) <- makeNames(i_events);
   }
   if (length(names(channels)) == 0) {
      names(channels) <- makeNames(channels);
   }
   ## Subset i_events to those with data available
   in_range_l <- sapply(i_events, function(k){
      i_channel <- head(channels, 1);
      i_event <- as.character(k);
      i_range <- (c(
         i_starts[i_event] - event_prestart,
         i_stops[i_event] + event_poststop
      ));
      (i_range[1] <= length(mat_l[[i_channel]]) &
         i_range[2] <= length(mat_l[[i_channel]]))
   });
   if (any(!in_range_l)) {
      if (verbose) {
         printDebug("get_ephys_event_data(): ",
            "in_range_l:", in_range_l);
      }
      i_events <- i_events[in_range_l];
      if (verbose) {
         printDebug("get_ephys_event_data(): ",
            "new i_events:", i_events);
      }
   }

   ## iterate each channel, each event
   events_l <- lapply(channels, function(i_channel){
      i_v_l <- lapply(i_events, function(k){
         i_event <- as.character(k);
         i_range <- (c(
            i_starts[i_event] - event_prestart,
            i_stops[i_event] + event_poststop
         ));
         if (verbose) {
            printDebug("get_ephys_event_data(): ",
               "i_range:",
               i_range);
            printDebug("get_ephys_event_data(): ",
               "dim(mat_l[[i_channel]]):",
               dim(mat_l[[i_channel]]));
         }
         j_v <- mat_l[[i_channel]][i_range[1]:i_range[2],1];
         #j_t <- seq(from=i_step,
         #   by=i_step,
         #   length.out=length(j_v));
         #, time=j_t);
         i_m <- cbind(value=j_v);
         attr_names_use <- intersect(attr_names,
            names(attributes(mat_l[[i_channel]])));
         for (i_attr_name in attr_names_use) {
            attr(i_m, i_attr_name) <- attr(mat_l[[i_channel]], i_attr_name);
         }
         if ("channel" %in% attr_names) {
            attr(i_m, "channel") <- i_channel;
         }
         attr(i_m, "label_start") <- event_prestart + 1;
         i_diff <- diff(c(i_starts[i_event], i_stops[i_event]));
         attr(i_m, "label_stop") <- event_prestart + i_diff + 1;
         attr(i_m, "i_range") <- i_range;
         attr(i_m, "event_prestart") <- event_prestart;
         attr(i_m, "event_poststop") <- event_poststop;
         i_m;
      });
   });
   events_m <- do.call(cbind, events_l);
   dimnames(events_m) <- list(
      event=rownames(events_m),
      channel=colnames(events_m));
   #ret_l$events_l <- events_l;
   ret_l$events_m <- events_m;

   return(ret_l);
}
