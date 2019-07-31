#' Get Ephys event data
#'
#' @family jam event functions
#'
#' @export
get_ephys_event_data <- function
(mat_l,
 channel,
 events=NULL,
 event_offset=NULL,
 event_start=2,
 event_stop=3,
 event_prestart=4000,
 event_poststop=4000,
 event_label="Tone",
 event_grep="^EV",
 attr_names=c("animal", "channel", "ts.step", "ts"),
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
   i_step <- attr(mat_l[[head(channels, 1)]], "ts.step")[1,1];
   i_ind <- tail(attr(mat_l[[head(channels, 1)]], "ind")[,1], 1);

   ## Overall Start
   all_start <- mat_l$Start[2,1];

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
         i_m;
      });
   });
   events_m <- do.call(cbind, events_l);
   #ret_l$events_l <- events_l;
   ret_l$events_m <- events_m;

   return(ret_l);
}
