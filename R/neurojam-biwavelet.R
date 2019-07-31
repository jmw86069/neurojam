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
#' @export
calc_ephys_wavelet <- function
(x,
 iWt=NULL,
 return_type=c("wt", "m", "list"),
 type=c("power.corr.norm", "power",
    "power.corr", "power.norm", "wavelet", "phase"),
 x_condense_factor=10,
 x_label_multiple=5,
 i_step=0.001,
 m=NULL,
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
      if (length(dim(x)) == 0) {
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
      iWt <- wt(cbind(x[,2], x[,1]),
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
      iM2 <- condense_matrix(iM,
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
      return(iM);
   }
   iWt$lab_row <- lab_row;
   iWt$i_row_values <- i_row_values;
   ret_l <- list();
   ret_l$iWt <- iWt;
   ret_l$iM <- iM;
   ret_l$lab_col <- lab_col;
   ret_l$colsep <- colsep;
   return(ret_l);
}

#' Plot Ephys wavelet data
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

