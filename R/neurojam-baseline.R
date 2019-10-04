#' Extract animal baseline signal data from raw data
#'
#' Extract animal baseline signal data from raw data
#'
#' This function uses `get_animal_raw_data()` to query the
#' database, retrieve the associated animal raw data, then
#' extracts baseline signal typically before the first
#' annotated event. This baseline signal can be useful
#' for calculations that measure the change in frequency
#' across events.
#'
#' @param before_event_only logical indicating whether to
#'    require all baseline data to occur before the first
#'    event time stamp.
#' @param baseline_step numeric value indicating the time bins
#'    to return for the baseline. The total baseline signal
#'    will be split into parts of this duration. The time
#'    is assumed to have units of `"seconds"`, consistent
#'    with units recorded in the time `"step"` field.
#' @param baseline_max_duration numeric value indicating
#'    the maximum duration of signal to be obtained for
#'    baseline signal. This argument is useful when the
#'    baseline may be recorded for an extended length of
#'    time, but only a certain duration is sufficient for
#'    analysis. Time units are assumed to be `"seconds"`,
#'    consistent with the time `"step"` field.
#' @param all_start_grep character string used to identify
#'    the element name containing a matrix of start times.
#'    When supplied, `all_start_row` is used to determine
#'    the row indicating the start of the baseline signal.
#' @param all_start_row integer value indicating the row
#'    containing the experiment start time index.
#' @param all_start_fixed numeric value indicating the
#'    start time index, used as an optional replacement
#'    of the `all_start_grep`, and `all_start_row` arguments.
#' @param event_grep character pattern used to identify
#'    element names that contain event time stamps.
#' @param event_start integer value indicating the element
#'    among results matched by `event_grep` to use for the
#'    event start times. For example, when event names
#'    are `c("EVT01", "EVT02", "EVT03")` and `"EVT02"`
#'    contains start times, use `event_start=2`.
#'    This value is relevant only when `before_event_only=TRUE`.
#' @param attr_names character vector of attribute names which
#'    are present in the raw data for each channel, which will
#'    be retained in the resulting baseline signal data.
#'
#' @export
extract_baseline_data <- function
(con,
 animal=NULL,
 project=NULL,
 phase=NULL,
 source_filename=NULL,
 source_dirname=NULL,
 rdata_filename=NULL,
 rdata_dirname=NULL,
 animal_raw_data_table="animal_raw_data",
 before_event_only=TRUE,
 baseline_step=60,
 baseline_max_duration=480,
 all_start_grep="^Start",
 all_start_row=2,
 all_start_fixed=NULL,
 event_grep="^EV",
 event_start=2,
 attr_names=c("animal", "channel", "ts.step", "ts"),
 verbose=FALSE,
 ...)
{
   ## Purpose is to emulate extract_event_data() specifically
   ## for non-event signal, generally before the first event

   ## verify animal exists in the database
   animal_raw <- get_animal_raw_data(con=con,
      animal=animal,
      project=project,
      phase=phase,
      source_filename=source_filename,
      source_dirname=source_dirname,
      rdata_filename=rdata_filename,
      rdata_dirname=rdata_dirname,
      animal_raw_data_table=animal_raw_data_table,
      return_rdata=FALSE,
      verbose=verbose,
      ...);
   if (length(animal) > 0 &&
         (length(animal_raw) == 0 || !all(animal %in% animal_raw$animal))) {
      missing_animals <- setdiff(animal, animal_raw$animal);
      stop(paste0(
         "Data is not available for:",
         jamba::cPaste(missing_animals)));
   }

   ## load RData containing raw data per animal
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

   baselinesml <- lapply(jamba::nameVectorN(mat_ll), function(mat_name){
      if (verbose) {
         printDebug("extract_baseline_data(): ",
            "mat_name:",
            mat_name);
      }
      mat_l <- mat_ll[[mat_name]];
      animal <- attr(mat_l, "animal");
      project <- attr(mat_l, "project");
      phase <- attr(mat_l, "phase");
      channels <- attr(mat_l, "channels");

      ## Overall Start
      all_start <- 0;
      if (length(all_start_fixed) > 0) {
         if (verbose) {
            printDebug("get_ephys_event_data(): ",
               "Using all_start_fixed:",
               all_start_fixed);
         }
         all_start <- all_start_fixed;
      } else if (length(all_start_grep) > 0 && nchar(all_start_grep) > 0) {
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

      ## Obtain first event start time, if before_event_only=TRUE
      if (length(before_event_only) == 1 && before_event_only) {
         event_names <- provigrep(event_grep, names(mat_l));
         event_data <- mat_l[event_names];
      }

      ret_l <- list();
      ret_l$event_data <- event_data;

      ## iterate each channel, each event
      baselines_l <- lapply(nameVector(channels), function(i_channel){
         ## Get time step
         ch_step <- paste0(i_channel, ".ts.step");
         i_step <- mat_l[[ch_step]][,1];
         ## Get event index offsets
         ch_ind <- paste0(i_channel, ".ind");
         if (ch_ind %in% names(mat_l)) {
            i_ind <- tail(mat_l[[ch_ind]][,1], 1);
         } else {
            i_ind <- 0;
         }
         ## Get first event start if needed
         if (length(before_event_only) == 1 && before_event_only) {
            index_nrow <- 0 +
               event_data[[event_start]][1,1] * (1/i_step) +
               -all_start * (1/i_step) +
               i_ind;
            if (verbose) {
               printDebug("extract_baseline_data(): ",
                  "first event index_nrow:",
                  index_nrow);
            }
         } else {
            ## here just use the max available index
            index_nrow <- nrow(mat_l[[i_channel]]);
            if (verbose) {
               printDebug("extract_baseline_data(): ",
                  "total data index_nrow:",
                  index_nrow);
            }
         }
         ## Enforce event_max_duration if supplied
         if (length(event_max_duration) > 0 && event_max_duration > 0) {
            event_max_index <- event_max_duration / i_step;
            index_max <- min(c(index_nrow, event_max_index));
         }
         ## Break into chunks using baseline_step
         if (length(baseline_step) == 0) {
            baseline_step <- 60;
         }
         if (verbose) {
            printDebug("extract_baseline_data(): ",
               "index_max:",
               index_max,
               ", index_nrow:",
               index_nrow,
               ", event_max_index:",
               event_max_index,
               ", event_max_duration:",
               event_max_duration);
         }
         ## Define baseline index positions using baseline_step
         ## TODO: optionally define baseline_offset
         baseline_steps <- seq(from=0,
            to=index_max,
            by=baseline_step/i_step);
         ## If the last step is more than 10% from index_max, include index_max
         if ((index_max - baseline_steps) > baseline_step/i_step/10) {
            baseline_steps <- sort(unique(c(baseline_steps, index_max)));
         }
         names(baseline_steps) <- makeNames(
            renameFirst=FALSE,
            rep("baseline", length.out=length(baseline_steps)),
            suffix="_");

         i_v_l <- lapply(head(seq_along(baseline_steps), -1), function(k){
            i_event <- as.character(names(baseline_steps[k+1]));
            i_range <- c(baseline_steps[k]+1,
               baseline_steps[k+1]);
            if (verbose) {
               printDebug("extract_baseline_data(): ",
                  "i_range:",
                  i_range);
            }
            j_v <- mat_l[[i_channel]][i_range[1]:i_range[2],1];
            i_m <- cbind(value=j_v);
            attr_names_use <- intersect(attr_names,
               names(attributes(mat_l[[i_channel]])));
            for (i_attr_name in attr_names_use) {
               attr(i_m, i_attr_name) <- attr(mat_l[[i_channel]], i_attr_name);
            }
            if ("channel" %in% attr_names) {
               attr(i_m, "channel") <- i_channel;
            }
            attr(i_m, "label_start") <- 1;
            attr(i_m, "label_stop") <- diff(i_range) + 1;
            attr(i_m, "i_range") <- i_range;
            attr(i_m, "event_prestart") <- 0;
            attr(i_m, "event_poststop") <- 0;
            attr(i_m, "event") <- i_event;
            attr(i_m, "project") <- project;
            attr(i_m, "phase") <- phase;
            attr(i_m, "step") <- i_step;
            i_df <- data.frame(animal=animal,
               project=project,
               phase=phase,
               channel=i_channel,
               event=i_event,
               event_prestart=0,
               event_poststop=0,
               step=i_step,
               event_signal=I(list(i_m)));
            i_df;
         });
         names(i_v_l) <- tail(names(baseline_steps), -1);
         i_v_l;
      });
   });
   return(rbindList(
      jamba::unnestList(baselinesml)));
}



