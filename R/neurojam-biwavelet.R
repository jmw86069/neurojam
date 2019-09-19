#' Calculate wavelet transformation
#'
#' This function takes numeric signal data, and performs
#' biwavelet transformation by calling `biwavelet::wt()`.
#' It optionally returns a specific numeric matrix,
#' for example when `type="power.corr.norm"` it returns
#' the normalized power correlation matrix (see docs for
#' `biwavelet::wt()` for more details).
#'
#' This function attempts to return a numeric spectral
#' frequency matrix, either by calling `biwavelet::wt()`
#' then extracting the appropriate numeric matrix from
#' the resulting list object; or by using the `iWt` data
#' from a previous call to `biwavelet::wt()` to
#' extract the appropriate numeric matrix; or by using
#' a numeric matrix `m` supplied directly. In the latter
#' case, `biwavelet::wt()` is not necessary and may
#' be substantially faster.
#'
#' @return When `return_type` is `"wt"` the output of
#' `biwavelet::wt()` is returned; when `return_type` is
#' `"m"` a numeric matrix is returned, whose rownames
#' are the `"period"` and colnames are the timestamp,
#' this matrix may also be condensed when `x_condense_factor`
#' is greater than 1; when `return_type` is `"list"` then
#' a list is returned with elements `"wt"` and `"m"`
#' containing the two previous return types. The numeric
#' matrix `"m"` has usseful attributes: `"lab_col"` is
#' a character vector of column labels at user-friendly
#' positions; `"lab_row"` is a character vector of row
#' labels at user-friendly positions, after converting
#' `"period"` to frequency in Hertz (per second) units.
#'
#' Since the data matrix is very large, and it is often
#' not necessary to retain the full resolution of this data,
#' it can be condensed by setting `x_condense_factor` to any
#' value above 1. In that case `condense_freq_matrix()` is
#' called to condense the numeric matrix to a smaller size
#' before returning results. This process reduces data size
#' substantially, while avoiding memory consumption of returning
#' each numeric matrix produced by `biwavelet::wt()`.
#'
#' @param x numeric matrix with two columns, where the first column
#'    contains neural signal data, and the second column contains
#'    timestamps in units of seconds.
#'    When `iWt` is supplied, `x` is not used.
#' @param iWt list output from `biwavelet::wt()`, or NULL.
#'    When `iWt` is supplied, `x` is not used. When `iWt` is not
#'    supplied, it is only calculated when `return_type` is
#'    either `"wt"` or `"list"`, or when `"m"` is not supplied.
#' @param return_type character vector indicating the type of
#'    data to return: `"wt"` returns the full output of `biwavelet::wt()`;
#'    `"m"` returns the numeric frequency matrix; `"list"` returns
#'    a list including elements `c("wt", "m")` which include the
#'    previous two formats.
#' @param type character indicating which frequency matrix to
#'    extract from the output of `biwavelet::wt()`. Note that
#'    when `iWt` is supplied, the matrix is taken directly from it.
#'    When `m` is supplied as a numeric matrix, it is used
#'    directly, and `type` is not used.
#' @param x_condense_factor integer value indicating the level
#'    of compression to condense the numeric matrix, when `return_type`
#'    is either `"m"` or `"list"`. When `x_condense_factor=1` there
#'    is no compression, otherwise it calls `condense_freq_matrix()`.
#' @param x_label_multiple integer value indicating the multiple
#'    to use when defining labels to display. For example
#'    when `x_label_multiple=5` labels are displayed at multiples
#'    of 5: 0, 5, 10, 15, 20, 25.
#' @param m numeric matrix, optionally supplied as an alternative
#'    to extracting a numeric matrix from either `iWt` or
#'    the output of `biwavelet::wt(x)`.
#' @param do.sig logical indicating whether to calculate
#'    statistical significance. This argument is passed to
#'    `biwavelet::wt()`.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional arguments are passed to `biwavelet::wt()`
#'    when `iWt` is not supplied.
#'
#' @family jam wavelet functions
#'
#' @export
calc_ephys_wavelet <- function
(x,
 iWt=NULL,
 return_type=c("wt", "m", "list"),
 type=c("power.corr.norm", "power",
    "power.corr", "power.norm", "wavelet", "phase"),
 x_condense_factor=10,
 x_label_multiple=5,
 step=0.001,
 m=NULL,
 dj=1/6,
 s0=i_step*5,
 mother="morlet",
 do.sig=FALSE,
 verbose=FALSE,
 ...)
{
   ## Perform biwavelet::wt() and prepare results for downstream analysis
   if (!suppressPackageStartupMessages(require(biwavelet))) {
      stop("The biwavelet package is required.");
   }
   type <- match.arg(type);
   return_type <- match.arg(return_type);

   ## wavelet transform
   if (length(iWt) == 0 && (length(m) == 0 || any(c("list","wt") %in% return_type))) {
      if (verbose) {
         printDebug("calc_ephys_wavelet(): ",
            "Running biwavelet::wt()");
      }
      if (length(dim(x)) == 0 || ncol(x) == 1) {
         if ("label_start" %in% names(attributes(x))) {
            label_start <- attr(x, "label_start") * step;
         } else {
            label_start <- 0;
         }
         x <- cbind(x,
            seq(from=step - label_start,
               by=step,
               length.out=length(x)));
      }
      if (1 == 2 && verbose) {
         printDebug("calc_ephys_wavelet(): ",
            "head(cbind(x[,2], x[,1])):");
         print(head(cbind(x[,2], x[,1])));
      }
      iWt <- wt(cbind(x[,2], x[,1]),
         dj=dj,
         dt=step,
         s0=s0,
         mother=mother,
         do.sig=do.sig,
         ...);
   }
   if ("wt" %in% return_type) {
      return(iWt);
   }

   ## Extract the data matrix to use
   if (length(m) > 0) {
      iM <- m;
   } else {
      iM <- iWt$power;
      rownames(iM) <- iWt$period;
      colnames(iM) <- iWt$xaxis;
      attr(iM, "period") <- iWt$period;
      attr(iM, "xaxis") <- iWt$xaxis;
   }

   ## Optionally compress the matrix
   if (x_condense_factor > 1) {
      column_n <- round(ncol(iM)/x_condense_factor);
      if (verbose) {
         printDebug("calc_ephys_wavelet(): ",
            "compressing matrix from ",
            formatInt(ncol(iM)),
            " to ",
            formatInt(column_n),
            " columns");
      }
      iM2 <- condense_freq_matrix(iM,
         column_n=column_n,
         verbose=verbose);
   }

   ## Determine x-axis positions to label
   ## TODO: change to code that "finds" the nearest colname to each
   ## requested label, with a maximum distance cutoff. Allows for
   ## colnames to be an approximate match, and the most suitable place
   ## for each desired label.
   ## Example: colnames(iM) includes c(4.977, 4.997, 5.017, 5.037)
   ## requested label "5.0" -- closest matching position is 4.997.
   x_label_multiple <- 5;
   xaxis <- as.numeric(colnames(iM));
   iCol <- (xaxis == as.integer(xaxis/x_label_multiple)*x_label_multiple);
   ## Where to draw separating lines to align with axis labels
   ## on a heatmap image
   colsep <- which(iCol) - 0.5;
   lab_col <- ifelse(iCol,
      xaxis,
      "");
   attr(iM, "colsep") <- colsep;
   attr(iM, "lab_col") <- lab_col;

   ## Determine y-axis positions to label
   ## attempt to use minor log tick positions
   i_row_values <- 1/as.numeric(rownames(iM));
   mlt <- minorLogTicks(lims=log2(range(i_row_values)),
      minorWhich=c(2,3,6));
   mlt_df <- subset(mlt$allLabelsDF, !is.na(text));
   mlt_df$tick_val <- 2^(mlt_df$tick);
   mlt_df$closest_diff <- sapply(mlt_df$tick_val, function(k){
      k_diff <- abs(log2(i_row_values) - log2(k));
      k_diff[which.min(k_diff)];
   });
   mlt_df$closest <- sapply(mlt_df$tick_val, function(k){
      k_diff <- abs(log2(i_row_values) - log2(k));
      i_row_values[which.min(k_diff)];
   });
   mlt_df <- mixedSortDF(mlt_df,
      byCols=c("closest_diff", "type", "-tick_val"));
   mlt_df2 <- mixedSortDF(mlt_df[match(unique(mlt_df$closest), mlt_df$closest),],
      byCols="closest");
   lab_row <- rep("", length(i_row_values));
   lab_row[match(mlt_df2$closest, i_row_values)] <- sapply(mlt_df2$text, format, digits=2);
   attr(iM, "lab_row") <- lab_row;

   if ("m" %in% return_type) {
      if (x_condense_factor > 1) {
         return(iM2);
      }
      return(iM);
   }
   iWt$lab_row <- lab_row;
   iWt$i_row_values <- i_row_values;
   ret_l <- list();
   ret_l$iWt <- iWt;
   ret_l$iM <- iM;
   if (x_condense_factor > 1) {
      ret_l$iM2 <- iM2;
   }
   ret_l$lab_col <- lab_col;
   ret_l$colsep <- colsep;
   return(ret_l);
}

#' Plot Ephys wavelet data
#'
#' @family jam wavelet functions
#' @family jam plot functions
#'
#' @export
plot_ephys_wavelet <- function
(x,
iWt=NULL,
style=c("default", "jam", "CH"),
type=c("power.corr.norm", "power", "power.corr", "power.norm", "wavelet", "phase"),
plot.coi=FALSE,
plot.cb=FALSE,
plot.phase=FALSE,
plot.sig=FALSE,
col.coi="grey30",
fill.cols=warpRamp(getColorRamp("RdBu_r"), lens=2),
ylim=rev(2^c(0,-6)),
arrow.col="green4",
col="RdBu_r",
freq_lim=NULL,
 i_step=0.001,
 x_compress_factor=10,
do_plot=TRUE,
m=NULL,
...)
{
   ##
   if (!suppressPackageStartupMessages(require(biwavelet))) {
      stop("The biwavelet package is required.");
   }
   style <- match.arg(style);
   type <- match.arg(type);

   ## wavelet transform
   if (length(iWt) == 0) {
      if (length(dim(x)) == 0 || ncol(x) == 1) {
         if ("label_start" %in% names(attributes(x))) {
            label_start <- attr(x, "label_start") * i_step;
         } else {
            label_start <- 0;
         }
         x <- cbind(x,
            seq(from=i_step - label_start,
               by=i_step,
               length.out=length(x)));
      }
      iWt <- wt(cbind(x[,2], x[,1]));
   }
   #printDebug("object.size(iWt):", object.size(iWt));

   ## biwavelet spectrogram-like heatmap plot
   if ("default" %in% style) {
      if (par("mar")[4] < 6) {
         opar <- par();
         on.exit(par(opar));
         par("mar"=c(5,4,4,7),
            "oma"=c(0,0,0,4));
      }
      plot.biwavelet(iWt,
         type=type,
         main=type,
         useRaster=TRUE,
         plot.coi=plot.coi,
         col.coi=col.coi,
         xlab="Time (seconds after event)",
         yaxt="n",
         ylab="",
         ylim=ylim,
         fill.cols=fill.cols,
         plot.cb=plot.cb,
         plot.phase=plot.phase,
         plot.sig=plot.sig,
         lwd.sig=2,
         col.sig="#00000077",
         arrow.col=arrow.col);

      yAt <- pretty(par("usr")[3:4]);
      yAt <- yAt[yAt <= max(par("usr")[3:4]) & yAt >= min(par("usr")[3:4])];
      ## Hertz
      yAtLabels2 <- sapply(1/(2^yAt), format, digits=1, big.mark=",");
      axis(2, las=2, at=yAt, labels=yAtLabels2, line=0);
      title(ylab="Hertz");
   } else if ("CH" %in% style) {
      printDebug("plot style:", style);
      ## ComplexHeatmap
      if (!suppressPackageStartupMessages(require(ComplexHeatmap))) {
         stop("The ComplexHeatmap package is required.");
      }
      ## Get data matrix
      iM <- log2(1+iWt[[type]]);
      rownames(iM) <- iWt$period;
      colnames(iM) <- iWt$xaxis;

      ## Decide which rows to use as labels on the heatmap
      i_row <- unique(round(seq(from=1,
         to=nrow(iM),
         length.out=18)));
      i_row_values <- 1/as.numeric(rownames(iM));
      lab_row <- ifelse(seq_along(i_row_values) %in% i_row,
         sapply(i_row_values, format, digits=2),
         "");

      ## attempt to use minor log tick positions
      i_row_values <- 1/as.numeric(rownames(iM));
      mlt <- minorLogTicks(lims=log2(range(i_row_values)),
         minorWhich=c(2,3,6));
      mlt_df <- subset(mlt$allLabelsDF, !is.na(text));
      mlt_df$tick_val <- 2^(mlt_df$tick);
      mlt_df$closest_diff <- sapply(mlt_df$tick_val, function(k){
         k_diff <- abs(log2(i_row_values) - log2(k));
         k_diff[which.min(k_diff)];
      });
      mlt_df$closest <- sapply(mlt_df$tick_val, function(k){
         k_diff <- abs(log2(i_row_values) - log2(k));
         i_row_values[which.min(k_diff)];
      });
      mlt_df <- mixedSortDF(mlt_df,
         byCols=c("closest_diff", "type", "-tick_val"));
      mlt_df2 <- mixedSortDF(mlt_df[match(unique(mlt_df$closest), mlt_df$closest),],
         byCols="closest");
      lab_row <- rep("", length(i_row_values));
      lab_row[match(mlt_df2$closest, i_row_values)] <- sapply(mlt_df2$text, format, digits=2);

      #labRow <- ifelse(seq_along(rownames(iM)) %in% i_row,
      #   num2hz(1/(rev(as.numeric(rownames(iM)))), digits=2), "");

      col_multiple <- 5;
      i_col <- (iWt$xaxis == as.integer(iWt$xaxis/col_multiple)*col_multiple);
      colsep <- which(i_col) - 0.5;
      lab_col <- ifelse(i_col,
         sapply(iWt$xaxis, format, digits=2),
         "");

      ## Color ramp
      pheat_breaks <- warpAroundZero(
         seq(from=-1, to=1, length.out=51),
         lens=1);
      cBR <- circlize::colorRamp2(breaks=pheat_breaks,
         col=getColorRamp("RdBu_r", n=51));


      ## per-row bar chart
      ha2 <- rowAnnotation(
         dist2=anno_barplot(
            rowMeans(2^iM-1),
            bar_width=1,
            gp=gpar(col="white", fill="#FF4400"),
            border=FALSE,
            #axis_param=list(at = c(0, 5e5, 1e6, 1.5e6),
            #   labels=c("0", "500k", "1m", "1.5m")),
            width=unit(2, "cm")
         ),
         show_annotation_name=FALSE
      )

      ## Draw heatmap
      iHM <- Heatmap(iM,
         name=type,
         border=TRUE,
         cluster_rows=FALSE,
         cluster_columns=FALSE,
         row_labels=lab_row,
         row_names_side="left",
         column_labels=lab_col,
         use_raster=TRUE,
         #raster_resize=FALSE,
         #row_split=colData(affy_se[,cor_samples])$Cohort,
         #column_split=colData(affy_se[,cor_samples])$Cohort,
         #top_annotation=colHA,
         #heatmap_legend_param=list(
         #   grid_width=unit(8, "mm"),
         #   border="black",
         #   title_gp=gpar(fontsize=12, fontface="bold"),
         #   legend_height=unit(30, "mm")),
         #row_dend_side="left",
         #col=cBR
         ...
      ) + ha2;
      ComplexHeatmap::draw(iHM,
         merge_legend=TRUE);
      ## iWt <- plot_ephys_wavelet(x, style="CH", iWt=iWt, type="power.corr");
   } else {
      ############################################################
      ## heatmap.3()
      ## Function to convert Hz into a user-friendly label
      num2hz <- function(x, digits=1, big.mark=",", unit="s",...){
         ifelse(x < 1,
            paste0("1/", sapply(1/x, format, digits=digits, big.mark=big.mark), unit),
            paste0(sapply(x, format, digits=digits, big.mark=big.mark), "/", unit));
      }

      ## heatmap.3() version of the heatmap
      if (length(m) > 0) {
         iM <- m;
      } else {
         iM <- iWt$power;
         rownames(iM) <- iWt$period;
         colnames(iM) <- iWt$xaxis;
      }

      ## Optional ylim subset
      if (length(freq_lim) > 0) {
         row_vals <- 1/as.numeric(rownames(iM));
         which_rows <- which(row_vals >= min(freq_lim) &
               row_vals <= max(freq_lim));
         iM <- iM[which_rows,,drop=FALSE];
      }

      ## Decide which rows to use as labels on the heatmap
      iRow <- unique(round(seq(from=1, to=nrow(iM), length.out=18)));
      labRow <- ifelse(seq_along(rownames(iM)) %in% iRow,
         num2hz(1/(rev(as.numeric(rownames(iM)))), digits=2), "");

      colMultiple <- 5;
      #xaxis <- iWt$xaxis;
      xaxis <- as.numeric(colnames(iM));
      iCol <- (xaxis == as.integer(xaxis/colMultiple)*colMultiple);
      colsep <- which(iCol) - 0.5;
      labCol <- ifelse(iCol,
         xaxis,
         "");

      ## attempt to use minor log tick positions
      i_row_values <- 1/as.numeric(rownames(iM));
      mlt <- minorLogTicks(lims=log2(range(i_row_values)),
         minorWhich=c(2,3,6));
      mlt_df <- subset(mlt$allLabelsDF, !is.na(text));
      mlt_df$tick_val <- 2^(mlt_df$tick);
      mlt_df$closest_diff <- sapply(mlt_df$tick_val, function(k){
         k_diff <- abs(log2(i_row_values) - log2(k));
         k_diff[which.min(k_diff)];
      });
      mlt_df$closest <- sapply(mlt_df$tick_val, function(k){
         k_diff <- abs(log2(i_row_values) - log2(k));
         i_row_values[which.min(k_diff)];
      });
      mlt_df <- mixedSortDF(mlt_df,
         byCols=c("closest_diff", "type", "-tick_val"));
      mlt_df2 <- mixedSortDF(mlt_df[match(unique(mlt_df$closest), mlt_df$closest),],
         byCols="closest");
      lab_row <- rep("", length(i_row_values));
      lab_row[match(mlt_df2$closest, i_row_values)] <- sapply(mlt_df2$text, format, digits=2);
      iWt$lab_row <- lab_row;
      iWt$i_row_values <- i_row_values;

      ## heatmap.3()
      if (1 == 1) {
         heatmap.3(log2(1+iM)[nrow(iM):1,],
            Rowv=FALSE,
            Colv=FALSE,
            rowsep=iRow,
            colsep=colsep,
            sepwidth=c(0.001*nrow(iM),0.0015*ncol(iM)),
            sepcolor="#FFFFFFAA",
            labRow=labRow,
            labCol=labCol,
            useRaster=TRUE,
            fixRasterRatio=FALSE,
            #symbreaks=TRUE,
            symkey=FALSE,
            col=col,
            add.expr=box(),
            colLensFactor=-2,
            ...);
         ## Add axis labels
         i_row <- which(!lab_row %in% c(NA, ""));
         axis(2,
            las=2,
            at=rev(i_row),
            label=sapply(lab_row[i_row], format, digits=2)
         );
      }

   }

   invisible(iWt);
}

#' Calculate animal event frequency profile
#'
#' Calculate animal event frequency profile
#'
#' This function takes event signal data, and calculates
#' a frequency matrix, with frequency period rows, and time
#' columns, the result of `calc_ephys_wavelet()` which in turn
#' calls `biwavelet::wt()` wavelet transform. By default it
#' returns the power-corrected frequency `"power.corr"`.
#'
#' When `con` is provided as a `DBI` database connection,
#' data is stored in a relational database table using
#' `save_animal_event_derived()`. It will first check if the
#' data already exists in the database, and if so it
#' will return the database result.
#'
#' @param eventsm numeric matrix whose first column contains
#'    raw signal data for the event to be analyzed. It may optionally
#'    contain a second column with time stamp, assumed to be
#'    in units of seconds, or consistent units with the `"step"`
#'    value.
#'    Some attributes are required to be present in `eventsm`:
#'    `"event_prestart"`, `"event_poststop"` contain the number
#'    of time steps before the event start, and after the event
#'    stop, respectively. Practically, this adjustment allows
#'    calculating a proper `zero` time point, and to indicate
#'    when the official event duration ends.
#'    `eventsm` can also be a list, in which case all arguments
#'    `animal,step,channel,project,phase,event_prestart,event_poststop`
#'    are required to be present as attributes of each item in the
#'    list.
#' @param step,animal,channel,event,project,phase,event_prestart,event_poststop
#'    arguments that describe the source of the event data.
#' @param pre_bin_n,time_bin_n,post_bin_n integer values indicating
#'    the number of bins to sub-divide each time window:
#'    `pre_bin_n` divides the pre-event signal into this many bins;
#'    `time_bin_n` divides the event signal into this many bins;
#'    `post_bin_n` divides the post-event signal into this many bins.
#' @param pre_bin_labels,time_bin_labels,post_bin_labels character
#'    string or vector indicating how to many the bins for each
#'    time window, when any of `pre_bin_n,time_bin_n,post_bin_n`
#'    are greater than 1. The character string is extended to the
#'    appropriate length, then make unique with
#'    `jamba::makeNames(..., suffix="_")`.
#' @param new_step numeric value indicating the new time step
#'    size to store, which effectively reduces the data to this
#'    time unit per output column. For example, `new_step=0.1`
#'    will store data in increments of 0.1 seconds. Data is
#'    condensed using `condense_freq_matrix()`.
#' @param x_condense_factor numeric value calculated using
#'    `new_step/step`. Using `new_step` allows condensing
#'    data to a fixed time step, instead of condensing data
#'    by a fixed factor which is applied regardless the time step.
#' @param dj,s0,mother,do.sig arguments passed to `calc_ephys_wavelet()`
#'    which ultimately passes those arguments to `biwavelet::wt()`.
#' @param freq_range numeric vector indicating the range of frequencies
#'    to include in the output data.
#' @param freq_step numeric value indicating the frequency step, used
#'    to create a sequence of frequencies across the range defined
#'    by `freq_range`.
#' @param freq_method string indicating which frequency method to use
#'    when determining intermediate frequency values: `"1"` chooses the
#'    closest available frequency returned by `biwavelet::wt()`;
#'    `"2"` performs linear interpolation and returns the value
#'    returned from `stats::approx()`.
#' @param adj_freq_range numeric range for which adjusted frequency
#'    values should be calculated. The method subtracts the mean
#'    frequency across this range from the total frequency profile.
#' @param return_type character string indicating the output format:
#'    `"profile"` returns only the frequency profile for each time
#'    bin; `"list"` returns a list containing the frequency profile,
#'    and the full frequency-time matrix returned from `biwavelet::wt()`
#'    after also calling `condense_freq_matrix()`.
#' @param animal_event_derived_table character string indicating the
#'    name of the database table that contains derived event data,
#'    where the results of this function will store data. If the
#'    requested data is already present in the database, it will be
#'    retrieved directly instead of re-calculating results.
#' @param useMedian logical indicating whether to use median instead
#'    of mean value when calculating the adjusted frequency.
#' @param dryrun logical indicating whether to perform a dry-run
#'    without performing calculations, and without storing results
#'    in the datbase.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional arguments are passed to downstream functions.
#'
#' @export
event_freq_profile <- function
(eventsm,
 step=NULL,
 animal=NULL,
 channel=NULL,
 event=NULL,
 project=NULL,
 phase=NULL,
 event_prestart=NULL,
 event_poststop=NULL,
 pre_bin_n=1,
 time_bin_n=2,
 post_bin_n=1,
 pre_bin_labels="pre",
 time_bin_labels=c("tone", "trace"),
 post_bin_labels="post",
 new_step=0.1,
 x_condense_factor=new_step/step,
 dj=1/6,
 s0_factor=5,
 s0=step*s0_factor,
 mother="morlet",
 do.sig=FALSE,
 freq_range=c(1, 20),
 freq_step=0.2,
 freq_method=c("2"),
 adj_freq_range=c(3, 5),
 return_type=c("profile", "list"),
 con=NULL,
 animal_event_derived_table="animal_event_derived",
 useMedian=FALSE,
 dryrun=FALSE,
 verbose=FALSE,
 ...)
{
   return_type <- match.arg(return_type);

   ## Check if input eventsm is a list
   if (is.list(eventsm)) {
      attr_names <- c("animal", "channel", "event", "project", "phase",
         "event_prestart", "event_poststop", "step");
      if (!all(attr_names %in% names(attributes(eventsm[[1]])))) {
         stop(paste0("When eventsm is a list, each element must contain all attributes: ",
            jamba::cPaste(attr_names)));
      }
      if (verbose) {
         jamba::printDebug("event_freq_profile(): ",
            "Processing ", "eventsm", " as a list");
      }
      freq_l <- lapply(eventsm, function(i_eventsm){
         printDebug("event_freq_profile(): ",
            "calling event_freq_profile for animal:",
            attr(i_eventsm, "animal"),
            ", channel:",
            attr(i_eventsm, "channel"),
            ", event:",
            attr(i_eventsm, "event"));
         event_freq_profile(i_eventsm,
            pre_bin_n=pre_bin_n,
            time_bin_n=time_bin_n,
            post_bin_n=post_bin_n,
            pre_bin_labels=pre_bin_labels,
            time_bin_labels=time_bin_labels,
            post_bin_labels=post_bin_labels,
            new_step=new_step,
            dj=dj,
            s0_factor=s0_factor,
            s0=NULL,
            mother=mother,
            do.sig=do.sig,
            freq_range=freq_range,
            freq_method=freq_method,
            adj_freq_range=adj_freq_range,
            return_type=return_type,
            con=con,
            animal_event_derived_table=animal_event_derived_table,
            useMedian=useMedian,
            dryrun=dryrun,
            verbose=verbose)
      });
      return(freq_l);
   }

   ## Determine if arguments are supplied, or found in attributes(eventsm)
   if (length(animal) == 0 && "animal" %in% names(attributes(eventsm))) {
      animal <- attr(eventsm, "animal");
   }
   if (length(channel) == 0 && "channel" %in% names(attributes(eventsm))) {
      channel <- attr(eventsm, "channel");
   }
   if (length(event) == 0 && "event" %in% names(attributes(eventsm))) {
      event <- attr(eventsm, "event");
   }
   if (length(project) == 0 && "project" %in% names(attributes(eventsm))) {
      project <- attr(eventsm, "project");
   }
   if (length(phase) == 0 && "phase" %in% names(attributes(eventsm))) {
      phase <- attr(eventsm, "phase");
   }
   if (length(event_prestart) == 0 && "event_prestart" %in% names(attributes(eventsm))) {
      event_prestart <- attr(eventsm, "event_prestart");
   }
   if (length(event_poststop) == 0 && "event_poststop" %in% names(attributes(eventsm))) {
      event_poststop <- attr(eventsm, "event_poststop");
   }
   if (length(step) == 0 && "step" %in% names(attributes(eventsm))) {
      step <- attr(eventsm, "step");
   }
   if (verbose) {
      printDebug("event_freq_profile(): ",
         "animal:", animal,
         ", channel:", channel,
         ", event:", event,
         ", project:", project,
         ", phase:", phase,
         ", event_prestart:", event_prestart,
         ", event_poststop:", event_poststop,
         ", step:", step);
   }
   if (length(c(animal, channel, event, project, phase, event_prestart, event_poststop, step)) < 8) {
      stop(paste0(
         "All values must be supplied or present in attributes(eventsm):\n",
         "animal, channel, event, project, phase, event_prestart, event_poststop, step"));
   }

   if (length(s0) == 0) {
      s0 <- s0_factor * step;
   }
   derived_extra <- paste0(
      "step=", step,
      ", dj=", dj,
      ", s0=", s0,
      ", mother=", mother,
      ", new_step=", new_step);
   derived_type <- "freq_profile";

   if (length(con) > 0) {
      ## When given a database connection, store results in the
      ## database when the data does not already exist
      if (!extends(class(con), "DBIConnection")) {
         stop("Connection 'con' must extend class 'DBIConnection'.");
      }
      ## Check for pre-existing data
      if (verbose) {
         printDebug("event_freq_profile(): ",
            "Checking for previously stored data.");
      }
      animevdf <- get_animal_event_derived(con=con,
         animal=animal,
         event=event,
         channel=channel,
         step=step,
         project=project,
         phase=phase,
         event_prestart=event_prestart,
         event_poststop=event_poststop,
         derived_type=derived_type,
         derived_extra=derived_extra,
         return_data=FALSE);
      if (length(animevdf) > 0 && nrow(animevdf) > 0) {
         ## Data already exists, retrieve and return
         if (verbose) {
            printDebug("event_freq_profile(): ",
               "Returning previously stored data. nrow(animevdf):",
               nrow(animevdf));
         }
         animevdf <- get_animal_event_derived(con=con,
            animal=animal,
            event=event,
            channel=channel,
            step=step,
            project=project,
            phase=phase,
            event_prestart=event_prestart,
            event_poststop=event_poststop,
            derived_type=derived_type,
            derived_extra=derived_extra,
            return_data=TRUE);
         return(animevdf$derived_data[[1]]);
      }
   }

   ## Calculate post_time
   post_time <- event_poststop * step;
   pre_time <- event_prestart * step;

   if (verbose) {
      printDebug("event_freq_profile: ",
         "calc_ephys_wavelet() started");
   }
   i_m <- calc_ephys_wavelet(x=eventsm,
      return_type="m",
      type="power.corr",
      x_condense_factor=x_condense_factor,
      step=step,
      dj=dj,
      s0=s0,
      mother=mother,
      do.sig=do.sig,
      verbose=verbose);
   if (verbose) {
      printDebug("event_freq_profile: ",
         "calc_ephys_wavelet() complete");
   }

   ## Convert matrix with period units to frequency in hertz
   iM2normal <- matrix_period2hz(i_m,
      freq_method=freq_method,
      freq_range=freq_range,
      freq_step=freq_step,
      verbose=verbose);

   ## Summarize data by time bins
   cut_m <- summarize_event_bins(log2(1+iM2normal),
      pre_time=pre_time,
      post_time=post_time,
      pre_bin_n=pre_bin_n,
      time_bin_n=time_bin_n,
      post_bin_n=post_bin_n,
      pre_bin_labels=pre_bin_labels,
      time_bin_labels=time_bin_labels,
      post_bin_labels=post_bin_labels,
      useMedian=useMedian,
      verbose=verbose,
      ...);

   ## Create time bins using colnames as time
   i_time <- as.numeric(colnames(i_m));
   cut_time_breaks <- sort(
      unique(
         c(-pre_time,
            0,
            max(i_time)-post_time,
            max(i_time))));
   #cut(i_time,
   #   breaks=cut_time_breaks,
   #   include.lowest=TRUE);
   max_time <- max(i_time, na.rm=TRUE);
   if (pre_time == 0 && min(i_time, na.rm=TRUE) < 0) {
      pre_time <- min(i_time, na.rm=TRUE);
   }
   if (pre_time == 0) {
      pre_bin_n <- 0;
   }
   if (post_time == 0) {
      post_bin_n <- 0;
   } else if (post_bin_n == 0) {
      post_bin_n <- 1;
   }
   pre_bin_cuts <- head(unique(seq(from=-pre_time, to=0, length.out=pre_bin_n+1)), -1);
   time_bin_cuts <- seq(from=0, to=max_time-post_time, length.out=time_bin_n + 1);
   post_bin_cuts <- tail(seq(from=max_time-post_time, to=max_time, length.out=post_bin_n + 1), -1);
   time_breaks <- sort(unique(c(pre_bin_cuts, time_bin_cuts, post_bin_cuts)));
   if (length(time_bin_labels) == 0) {
      time_bin_labels <- "event";
   }
   time_labels <- makeNames(c(rep("pre", length(pre_bin_cuts)),
      rep(time_bin_labels, length.out=time_bin_n),
      rep("post", length(post_bin_cuts))));
   time_cuts <- cut(i_time,
      breaks=time_breaks,
      labels=time_labels,
      include.lowest=TRUE);
   col_sets <- split(colnames(iM2normal),
      time_cuts);
   col_groups <- nameVector(
      rep(names(col_sets), lengths(col_sets)),
      unlist(col_sets)
   );
   cut_m <- jamba::rowGroupMeans(log2(1+iM2normal),
      groups=col_groups,
      useMedian=FALSE,
      ...);

   cut_df <- data.frame(freq=as.numeric(rownames(cut_m)), cut_m);
   cut_tall <- tidyr::gather(cut_df,
      "time_bin", "value",
      -freq,
      factor_key=TRUE);
   if (length(adj_freq_range) > 0) {
      value_baselines <- subset(cut_tall,
            freq >= min(adj_freq_range) & freq <= max(adj_freq_range))$value;
      if (useMedian) {
         value_baseline <- median(value_baselines);
      } else {
         value_baseline <- mean(value_baselines);
      }
      cut_tall$value_baseline <- value_baseline;
      cut_tall$adj_value <- cut_tall$value - cut_tall$value_baseline;
   }
   cut_tall$animal <- animal;
   cut_tall$event <- event;
   cut_tall$channel <- channel;
   ret_vals <- list();
   ret_vals$i_m <- i_m;
   ret_vals$cut_tall <- cut_tall;

   ## Optionally store results in database
   if (length(con) > 0) {
      if (length(ret_vals) > 0) {
         if (verbose) {
            printDebug("event_freq_profile(): ",
               "Saving into database via save_animal_event_derived()");
         }
         save_animal_event_derived(con=con,
            animal=animal,
            project=project,
            phase=phase,
            step=step,
            channel=channel,
            event=event,
            event_prestart=event_prestart,
            event_poststop=event_poststop,
            derived_data=ret_vals,
            derived_type=derived_type,
            derived_extra=derived_extra,
            animal_event_derived_table=animal_event_derived_table,
            dryrun=dryrun,
            verbose=verbose);
      }
   }
   if ("list" %in% return_type) {
      return(ret_vals);
   }

   return(cut_tall);
}

#' Convert matrix scale from period to Hertz units
#'
#' Convert matrix scale from period to Hertz units
#'
#' This function converts a numeric matrix whose rows are
#' in units of period, number of seconds per observation,
#' into units of Hertz, number of observations per second.
#' It also makes the Hertz scale linear by using linear
#' interpolation between actual period measurement values,
#' via the `stats::approx()` function.
#'
#' The period values are expected to be `rownames(x)`,
#' which are converted to numeric with `as.numeric(rownames(x))`.
#' Otherwise the argument `row_values` can be used to supply
#' a numeric vector.
#'
#' @return numeric matrix whose rows are in units of frequency
#'    in Hertz, observations per second. An attribute `"row_values"`
#'    contains a numeric vector for each row.
#'
#' @export
matrix_period2hz <- function
(x,
 row_values=NULL,
 freq_range=c(1, 20),
 freq_step=0.2,
 freq_method=c("2"),
 do_rev=FALSE,
 verbose=FALSE,
 ...)
{
   #
   if (length(x) == 0) {
      return(x);
   }
   iM2 <- x;
   if (length(row_values) > 0) {
      if (length(row_values) != nrow(x)) {
         stop("The length(row_values) must equal nrow(x).");
      }
      if (!is.numeric(row_values)) {
         stop("row_values must be a numeric vector.");
      }
   } else {
      if (length(rownames(iM2)) == 0) {
         stop("When row_values is not supplied, there must be rownames(x).");
      }
      row_values <- as.numeric(rownames(iM2));
   }
   ## Convert to Hertz
   iM2hz <- 1/row_values;
   if (do_rev) {
      if (verbose) {
         printDebug("matrix_period2hz(): ",
            "do_rev:",
            do_rev);
      }
      iM2hz <- rev(iM2hz);
   }
   #rownames(iM2) <- rev(iM2hz);
   #ComplexHeatmap::Heatmap(log2(iM2), cluster_columns=FALSE, cluster_rows=FALSE, use_raster=TRUE, column_labels=rep("", ncol(iM2)));

   ## Convert to linear scale
   freq_seq <- seq(from=min(freq_range),
      to=max(freq_range),
      by=freq_step);

   ## Custom function
   closest_value <- function
   (x,
    y,
    value=TRUE,
    ...)
   {
      x_y <- sapply(x, function(i){
         idiff <- abs(i - y)
         if (value) {
            y[which.min(idiff)];
         } else {
            which.min(idiff);
         }
      });
      x_y;
   }

   ## Two approaches
   if (length(freq_method) > 0 && "1" %in% freq_method) {
      if (verbose) {
         printDebug("matrix_period2hz(): ",
            "closest value method");
      }
      ## Method: find closest row to target frequency, use values in that row
      freq_seq <- seq(from=min(freq_range) - freq_step/2,
         to=max(freq_range) + freq_step/2,
         by=freq_step);
      x2i <- closest_value(log2(freq_seq), log2(iM2hz), value=FALSE);
      iM2normal <- rbindList(lapply(head(seq_along(x2i), -1), function(i){
         irows <- seq(from=x2i[i], to=x2i[i+1]);
         colMeans(iM2[irows,,drop=FALSE]);
      }));
      freq_bins <- head(freq_seq, -1) + diff(freq_seq)/2;
      rownames(iM2normal) <- freq_bins;
      attr(iM2normal, "row_values") <- freq_bins;
   } else {
      if (verbose) {
         printDebug("matrix_period2hz(): ",
            "linear interpolation method");
      }
      ## Alternative method:
      ## For each data column use approx() to define the actual
      ## frequency/value relationship, then use linear
      ## interpolation to obtain intermediate frequencies.
      iM2normal <- apply(iM2, 2, function(i){
         nameVector(
            approx(x=log2(iM2hz),
               y=i,
               xout=log2(freq_seq))$y,
            freq_seq);
      });
      rownames(iM2normal) <- freq_seq;
      attr(iM2normal, "row_values") <- freq_seq;
   }
   return(iM2normal);
}

#' Summarize event bins for a numeric matrix
#'
#' Summarize event bins for a numeric matrix
#'
#' This function is intended to summarize event data stored
#' in a numeric matrix with time columns, based upon
#' pre-event, event, and post-event sections. Each section
#' can be subdivided into `n` parts, using arguments
#' `pre_bin_n`, `time_bin_n`, and `post_bin_n`, respectively.
#'
#' Note that the input numeric matrix `x` should already
#' be transformed appropriate for summarization. For example,
#' if `log2` transformation is required, it should be done
#' before calling this function.
#'
#' @return numeric `matrix` whose colnames are the event bins
#'    after being sub-divided, when `make_tall=FALSE`. When
#'    `make_tall=TRUE` the data is returned as a `data.frame`
#'    with each row containing one frequency in column
#'    `"value"`.
#'
#' @param x numeric matrix whose `colnames(x)` represent time.
#' @param pre_time numeric value indicating the time before the
#'    event occurs. By default, the event is assumed to occur
#'    at time=0, so the lowest negative value is used to
#'    derive `pre_time`.
#' @param post_time numeric value indicating the time added
#'    to the end of the event, used as a post-event duration.
#' @param pre_bin_n,time_bin_n,post_bin_n integer number of bins
#'    used to sub-divide each section, for pre-event, event,
#'    and post-event, respectively.
#' @param pre_bin_labels character vector used to label the
#'    pre-event bin sections, expanded to the number of pre-event bins.
#' @param time_bin_labels character vector used to label the
#'    event bin sections, expanded to the number of event bins.
#' @param time_bin_labels character vector used to label the
#'    post-event bin sections, expanded to the number of post-event bins.
#' @param useMedian logical passed to `jamba::rowGroupMeans()`
#'    indicating whether to use the row median value to summarize
#'    each bin.
#' @param make_tall logical indicating whether to convert data
#'    to tall `data.frame` format. When `make_tall=FALSE` the
#'    data is returned as a numeric matrix.
#' @param adj_freq_range numeric range of frequencies to use
#'    as a baseline, which is subtracted from the resulting
#'    profile for each time bin. This subtraction is only
#'    applied when `make_tall=TRUE`, and
#'    `length(adj_freq_range) > 0`.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#'
#' @export
summarize_event_bins <- function
(x,
 col_values=NULL,
 pre_time=0,
 post_time=0,
 pre_bin_n=1,
 time_bin_n=2,
 post_bin_n=1,
 pre_bin_labels="pre",
 time_bin_labels=c("tone", "trace"),
 post_bin_labels="post",
 useMedian=FALSE,
 make_tall=FALSE,
 adj_freq_range=c(3, 5),
 verbose=FALSE,
 ...)
{
   #
   ## Create time bins using colnames as time
   if (length(col_values) == 0) {
      i_time <- as.numeric(colnames(x));
   } else {
      i_time <- as.numeric(col_values);
      if (length(colnames(x)) == 0) {
         colnames(x) <- as.character(col_values);
      }
   }

   max_time <- max(i_time, na.rm=TRUE);
   min_time <- min(i_time, na.rm=TRUE);
   if (pre_time == 0 && min_time < 0) {
      pre_time <- abs(min_time);
   }
   if (pre_time == 0) {
      pre_bin_n <- 0;
   }
   if (post_time == 0) {
      post_bin_n <- 0;
   } else if (post_bin_n == 0) {
      post_bin_n <- 1;
   }
   pre_bin_cuts <- head(
      unique(
         seq(from=min_time,
            to=min_time + pre_time,
            length.out=pre_bin_n+1)
      ), -1);
   time_bin_cuts <- unique(
      seq(from=min_time + pre_time,
         to=max_time - post_time,
         length.out=time_bin_n + 1));
   post_bin_cuts <- tail(
      unique(
         seq(from=max_time - post_time,
            to=max_time,
            length.out=post_bin_n + 1)
      ), -1);
   time_breaks <- sort(unique(
      c(pre_bin_cuts,
         time_bin_cuts,
         post_bin_cuts)));
   if (length(time_bin_labels) == 0) {
      time_bin_labels <- "event";
   }
   time_labels <- makeNames(
      c(
         rep(pre_bin_labels,
            length.out=length(pre_bin_cuts)),
         rep(time_bin_labels,
            length.out=length(time_bin_cuts)-1),
         rep(post_bin_labels,
            length.out=length(post_bin_cuts))
      ));
   if (verbose) {
      printDebug("summarize_event_bins(): ",
         "time_breaks:",
         time_breaks);
      printDebug("summarize_event_bins(): ",
         "time_labels:",
         time_labels);
      time_bins <- paste0(format(digits=3, trim=TRUE, head(time_breaks, -1)),
         " .. ",
         format(digits=3, trim=TRUE, tail(time_breaks, -1)));
      print(data.frame(time_bins, time_labels));
   }
   time_cuts <- cut(i_time,
      breaks=time_breaks,
      labels=time_labels,
      include.lowest=TRUE);
   col_sets <- split(colnames(x),
      time_cuts);
   col_groups <- nameVector(
      rep(names(col_sets), lengths(col_sets)),
      unlist(col_sets)
   );
   if (verbose) {
      printDebug("summarize_event_bins(): ",
         "sdim(col_sets):");
      print(jamba::sdim(col_sets));
      printDebug("summarize_event_bins(): ",
         "observed time_cuts:");
      print(splicejam::shrinkMatrix(matrix(i_time, ncol=1),
         groupBy=time_cuts,
         shrinkFunc=function(x){
            cPaste(format(digits=3, trim=TRUE, range(x)), sep=" .. ")
         }));
   }
   cut_m <- jamba::rowGroupMeans(x,
      groups=col_groups,
      useMedian=useMedian,
      ...);
   if (make_tall) {
      cut_df <- data.frame(freq=as.numeric(rownames(cut_m)), cut_m);
      cut_tall <- tidyr::gather(cut_df,
         "time_bin", "value",
         -freq,
         factor_key=TRUE);
      if (length(adj_freq_range) > 0) {
         value_baselines <- subset(cut_tall,
            freq >= min(adj_freq_range) & freq <= max(adj_freq_range))$value;
         if (useMedian) {
            value_baseline <- median(value_baselines);
         } else {
            value_baseline <- mean(value_baselines);
         }
         cut_tall$value_baseline <- value_baseline;
         cut_tall$adj_value <- cut_tall$value - cut_tall$value_baseline;
      }
      return(cut_tall);
   }
   return(cut_m);
}

#' Choose exemplar channel
#'
#' Choose exemplar channel
#'
#' This function takes tall output from `summarize_event_bins()`,
#' with multiple channels and events per animal, and choose one
#' channel to serve as the exemplar for analysis for each animal,
#' across all events.
#'
#' The basic assumption is that each channel is representing the
#' same signal, but with different level of specificity or
#' signal-to-noise, dependent upon the sensor associated with
#' each channel. This function attempts to choose the one channel
#' with highest signal, and best signal-to-noise.
#'
#' Alternatively, this function can be used to calculate the
#' mean or median signal across all channels.
#'
#' @export
choose_exemplar_channel <- function
(x,
 exemplar_method=c("highest", "median", "mean"),
 group_colnames=c("time_bin"),
 animal_colname="animal",
 event_colname="event",
 channel_colname="channel",
 value_colname="value",
 freq_colname="freq",
 freq_range=c(7, 15),
 ...)
{
   ##
   exemplar_method <- match.arg(exemplar_method);

   keep_colnames <- c(animal_colname,
      event_colname,
      channel_colname,
      value_colname,
      freq_colname,
      group_colnames);
   channel_values <- unique(x[[channel_colname]]);
   x3 <- x[,keep_colnames,drop=FALSE] %>%
      tidyr::spread(key=channel_colname,
         value=value_colname);
   channel_wide <- intersect(channel_values,
      colnames(x3));
   if ("median" %in% exemplar_method) {
      x3[,"value"] <- matrixStats::rowMedians(
         as.matrix(x3[,channel_wide,drop=FALSE]),
         na.rm=TRUE);
      x3[,"channel"] <- "median_signal";
      x_use <- x3[,setdiff(colnames(x3), channel_wide),drop=FALSE];
   } else if ("mean" %in% exemplar_method) {
      x3[,"value"] <- rowMeans(
         as.matrix(x3[,channel_wide,drop=FALSE]),
         na.rm=TRUE);
      x3[,"channel"] <- "mean_signal";
      x_use <- x3[,setdiff(colnames(x3), channel_wide),drop=FALSE];
   } else {
      ## Define "best" as the highest difference in freq_range
      xsub <- subset(x,
         time_bin %in% c("tone", "pre", "trace") &
         freq >= min(freq_range) & freq <= max(freq_range));
      xsub_diff <- splicejam::shrinkMatrix(
         xsub$value,
         groupBy=pasteByRow(xsub[,c("animal","event","channel","time_bin")], sep=":"),
         shrinkFunc=function(x){diff(range(x))});
      xsub_diff[,c("animal","event","channel","time_bin")] <- rbindList(
         strsplit(xsub_diff$groupBy, ":"));
      xsub_diff_best <- splicejam::shrinkMatrix(xsub_diff[,"x"],
         groupBy=pasteByRow(xsub_diff[,c("animal","time_bin")], sep=":"),
         shrinkFunc=max);
      xsub_diff_worst <- splicejam::shrinkMatrix(xsub_diff[,"x"],
         groupBy=pasteByRow(xsub_diff[,c("animal","time_bin")], sep=":"),
         shrinkFunc=min);

      xsub_max <- splicejam::shrinkMatrix(
         xsub$value,
         groupBy=pasteByRow(xsub[,c("animal","event","channel","time_bin")], sep=":"),
         shrinkFunc=function(x){max(x)});
      xsub_diff[,"max_signal"] <- xsub_max[,"x"];

      xsub_diff_best_v <- nameVector(xsub_diff_best[,2:1]);
      xsub_diff_worst_v <- nameVector(xsub_diff_worst[,2:1]);
      xsub_diff[,"animal:time_bin"] <- pasteByRow(xsub_diff[,c("animal","time_bin")],
         sep=":");
      xsub_diff2 <- subset(xsub_diff,
         xsub_diff[,"x"] == xsub_diff_best_v[xsub_diff[,"animal:time_bin"]])
      xsub_diff3 <- subset(xsub_diff,
         xsub_diff[,"x"] == xsub_diff_worst_v[xsub_diff[,"animal:time_bin"]])

      ## Subset xsub_diff2
      xsub_diff2bad <- subset(xsub_diff2, pasteByRow(xsub_diff2[,c("animal","channel")]) %in%
            pasteByRow(xsub_diff3[,c("animal","channel")]));
      ## Conclusion: even the bad ones aren't obviously bad in profile plots

      ## count unique channels per animal
      xsub_diff2_ct <- splicejam::shrinkMatrix(xsub_diff2[,"channel"],
         groupBy=xsub_diff2[,"animal"],
         shrinkFunc=function(x){length(unique(x))});

      ## Choose best by highest delta
      xsub_diff2_maxdelta <- splicejam::shrinkMatrix(xsub_diff2[,"x"],
         groupBy=xsub_diff2[,"animal"],
         shrinkFunc=max);
      xsub_diff2_ismaxdelta <- subset(xsub_diff2,
         x == xsub_diff2_maxdelta[match(xsub_diff2[,"animal"], xsub_diff2_maxdelta[,"groupBy"]),"x"]);

      ## Choose best by highest signal
      xsub_diff2_maxsignal <- splicejam::shrinkMatrix(xsub_diff2[,"max_signal"],
         groupBy=xsub_diff2[,"animal"],
         shrinkFunc=max);
      xsub_diff2_ismaxsignal <- subset(xsub_diff2,
         max_signal == xsub_diff2_maxsignal[match(xsub_diff2[,"animal"], xsub_diff2_maxsignal[,"groupBy"]),"x"]);
      table(xsub_diff2_ismaxsignal$channel == xsub_diff2_ismaxdelta$channel)
      dis_rows <- which(xsub_diff2_ismaxsignal$channel != xsub_diff2_ismaxdelta$channel);
      #data.frame(xsub_diff2_ismaxdelta[dis_rows,], xsub_diff2_ismaxsignal[dis_rows,c("x","channel","max_signal")])
      ## Conclusion: max signal tiebreaker seems most effective
      channel_per_animal <- nameVector(xsub_diff2_ismaxsignal[,c("channel", "animal")]);
      x_use <- subset(x,
         channel == channel_per_animal[x$animal]);
   }
   return(x_use);
}

