
#' Binned biwavelet signal processing
#'
#' Binned biwavelet signal processing
#'
#' This function attempts to sidestep memory limitations when
#' processing very large time series data using a biwavelet
#' transform. Typically the biwavelet transform `biwavelet::wt()`
#' returns the same number of columns as the rows of input
#' signal, which can be orders of magnitude larger than is
#' practically useful, while requiring huge amounts of memory.
#'
#' This function is also just a wrapper to `calculate_wavelet_matrix()`,
#' only processing subsets of input signal when necessary.
#' It may in future become an optional component of
#' `calculate_wavelet_matrix()`, used when the input signal
#' exceeds the `max_duration` threshold.
#'
#' The function implements the following workflow:
#'
#' * Define partially overlapping time bins for the input signal.
#' * Perform biwavelet transform on each bin.
#' * Trim the extended region from the frequency-time matrix.
#' * Condense the frequency-time matrix to save memory.
#' * Combine the list of frequency-time matrices to create one
#' continuous frequency-time matrix.
#'
#' The signal binning strategy is intended to avoid edge artifacts,
#' by extending signal for each bin into the next region.
#' Each time bin should end slightly beyond the start of the next bin,
#' and the start of each next bin should extend slightly before
#' the end of the previous bin. The resulting frequency-time
#' matrix is trimmed to remove the extended time, in order
#' to produce a continuous matrix with no edge artifacts.
#'
#' @param x numeric vector or matrix representing the source signal.
#' @param dt numeric value indicating the delta-time, or the time step
#'    between each signal measurement, typically in units of seconds.
#'    When `dt` is `NULL`, the attribute `"time_step"` is used, if
#'    present.
#' @param max_duration_sec the maximum time duration in seconds for
#'    each time bin. Note that the number of values in `x` used is
#'    dependent upon the `dt` time step, however since the purpose
#'    is to produce a frequency-time matrix, the time duration should
#'    be appropriate for the intended output frequency range.
#' @param buffer_sec the time buffer ussed to extend each time bin,
#'    in units of seconds. This buffer should exceed the uncertainty
#'    region for the range of output frequencies.
#' @param dj,s0,J1,max.scale numeric arguments passed to
#'    `biwavelet::wt()`.
#' @param new_step numeric value indicating the output time step,
#'    in units of seconds. The default `0.1` will output one column
#'    per 0.1 second. Note: the `new_step` value should ideally be
#'    in multiples of `dt` time step, in order to produce integer
#'    boundaries.
#' @param min_fraction numeric value between 0 and 1, indicating
#'    the required fraction of `new_step` required for the last
#'    bin of processing, below which those rows are dropped. For
#'    example, when `new_step=1` for 1 second, but there is less
#'    than 0.4 seconds data in the last 1-second interval.
#' @param return_type character value indicating the output type:
#'    `"matrix"` returns one matrix after individual matrices have
#'    been trimmed and combined; `"trimmed_list"` returns the list
#'    of matrices after the extended time bins were trimmed;
#'    `"untrimmed_list"` returns the list of matrices before trimming
#'    has occurred, along with `attr(x, "trim")` which contains the
#'    range to use for trimming.
#' @param dryrun logical indicating whether to run in "dryrun" mode,
#'    where `dryrun=TRUE` will print debugging info but not run the
#'    biwavelet processing.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional arguments are passed to
#'    `calculate_wavelet_matrix()`
#'
#'
#' @export
bin_biwavelets <- function
(x,
 dt=NULL,
 max_duration_sec=30,
 buffer_sec=2,
 dj=1/16,
 s0=0.005,
 J1=240,
 max.scale=163,
 new_step=0.1,
 min_fraction=0.4,
 return_type=c("matrix", "trimmed_list", "untrimmed_list"),
 dryrun=FALSE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to subdivide long signal into smaller signals
   ## in order to keep memory usage low enough to be practical.
   ## signals will be subdivided slightly overlapping each edge,
   ## then the exact seams will be put together.
   return_type <- match.arg(return_type);
   if (length(dt) == 0) {
      dt <- attr(x, "time_step");
   }
   if (length(dt) == 0) {
      stop("The argument dt, or attr(x, 'time_step') must be supplied.");
   }
   x_condense_factor <- new_step / dt;

   if (is.atomic(x)) {
      x <- matrix(x, ncol=1);
   }
   xn <- nrow(x);

   ## Check what fraction of time step is represented in the last
   ## time step, if less than min_fraction, drop these rows
   x_last_fraction <- (xn %% x_condense_factor) / x_condense_factor;
   if (x_last_fraction <= min_fraction) {
      if (verbose) {
         printDebug("bin_biwavelets(): ",
            "Dropping last ",
            (xn %% x_condense_factor),
            " rows which are ",
            format(digits=2, x_last_fraction),
            " fraction of new_step ",
            new_step,
            ", and are less than the ",
            min_fraction,
            " threshold.");
      }
      xn_new <- xn - (xn %% x_condense_factor);
      x <- head(x, xn_new);
      xn <- xn_new;
   }

   xends <- unique(c(
      tail(
         seq(from=1,
            to=xn-((2*buffer_sec)/dt),
            by=max_duration_sec/dt), -1) + buffer_sec/dt,
      xn));
   xstarts <- unique(c(1,
      noiseFloor(head(xends, -1) - (2*buffer_sec)/dt,
         minimum=1)));
   keepends <- c((head(xends, -1) + tail(xstarts, -1)) / 2 - 1,
      tail(xends, 1));
   keepstarts <- c(1, head(keepends, -1)+1);
   #label_start <- attr(x, "label_start");
   label_start <- 0;
   label_starts <- (xstarts) * dt + label_start;
   #label_starts <- xstarts * dt;
   actual_start <- keepstarts - xstarts + 1;
   #actual_end <- keepends - keepstarts + 1 + start_offset;
   actual_end <- keepends - keepstarts + actual_start;

   ## Determine potential column padding
   column_pads <- (x_condense_factor -
      ((actual_end) - (actual_start) + 1) %% x_condense_factor) %% x_condense_factor;
   ## values here will be 0.0002, 2.0002, 2.0002
   ## and should become   1     , 3     , 3
   ## (assuming label_start is zero and not time_step)
   adj_start <- ceiling(actual_start / x_condense_factor);
   ## values here will be 10.0000, 12.0000, 12.0000
   ## should become       10     , 12     , 12
   ## (assuming label_start is zero and not time_step)
   adj_end <- ceiling(actual_end / x_condense_factor);
   xse <- data.frame(xstarts,
      xends,
      keepstarts,
      keepends,
      label_starts,
      actual_start,
      actual_end,
      adj_start,
      adj_end,
      column_pads);
   xse$diff <- xse$xends - xse$xstarts;
   ## data.frame describing the rows to send and keep
   if (verbose) {
      printDebug("bin_biwavelets(): ",
         "Signal binning table:");
      print(xse);
   }

   ## Iterate each row
   wt_list <- lapply(seq_len(nrow(xse)), function(inum){
      xsub <- x[xse$xstarts[inum]:xse$xends[inum],,drop=FALSE];
      label_start <- xse$label_starts[inum];
      attr(xsub, "label_start") <- label_start;
      attr(xsub, "time_step") <- dt;
      if (verbose) {
         printDebug("bin_biwavelets(): ",
            "bin number:", inum, " of ", nrow(xse),
            ", row range: ", c(xse$xstarts[inum], xse$xends[inum]),
            ", dim(xsub): ", dim(xsub),
            ", label_start: ", label_start);
      }
      t1 <- Sys.time();
      if (dryrun) {
         return(NULL);
      }
      column_pad <- c(0, xse$column_pads[inum]);
      iwt <- calculate_wavelet_matrix(xsub,
         new_step=new_step,
         dj=dj,
         J1=J1,
         max.scale=max.scale,
         s0=s0,
         column_pad=column_pad,
         x_condense_factor=x_condense_factor,
         verbose=verbose,
         ...);
      t2 <- Sys.time();
      if (!igrepHas("untrimmed_list", return_type)) {
         iwt <- iwt[,xse$adj_start[inum]:xse$adj_end[inum],drop=FALSE];
      } else {
         attr(iwt, "trim") <- c(xse$adj_start[inum], xse$adj_end[inum]);
      }
      iwt;
   });
   if (dryrun) {
      return(wt_list);
   }
   if (igrepHas("matrix", return_type)) {
      wt_new <- do.call(cbind, wt_list);
      return(wt_new);
   }
   return(wt_list);
}


