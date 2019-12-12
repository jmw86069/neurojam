#' Condense a spectral frequence data matrix
#'
#' Condense a spectral frequence data matrix
#'
#' This function takes a numeric matrix, whose columns are
#' time, and rows are frequency, and bins columns and rows
#' to produce a smaller numeric matrix.
#'
#' It imposes some rules to bin labels:
#'
#' * Columns are considered time, the label assigned to the
#' new time bins are typically the "max" value from each bin,
#' such that time "0.01" through "1.00" will be labeled "1.00",
#' to represent "the first second bin".
#' * Rows are considered frequency, where each bin will be
#' labeled by the mean frequency. For example if frequencies
#' "6.75" through "7.25" are binned, the output should be "7.0"
#' which represents the central frequency of the binned values.
#'
#' Also, bins at the edges have two options:
#'
#' 1. "full" indicates the first bin will be full size, and all
#' bins will be equal to that size. This strategy makes sense for
#' time, so each time bin is the same duration.
#' 2. "half" indicates the first bin will be half the width of
#' subsequent bins. This strategy makes sense for frequency, to
#' help preserve the frequency nearest the edge of the matrix.
#' The last bin will also be half the width of intermediate bins.
#' In this case the first bin is labeled by the first value, and
#' the last bin is labeled by the last value.
#'
#'
#' @family jam matrix functions
#'
#' @export
condense_freq_matrix <- function
(x,
 column_n=ncol(x),
 row_n=nrow(x),
 column_method=c("max", "mean", "min"),
 row_method=c("mean", "max", "min"),
 column_edge=c("full"),
 row_edge=c("half", "full"),
 column_offset=0,
 row_offset=0,
 column_pad=c(0, 0),
 row_pad=c(0, 0),
 verbose=TRUE,
 ...)
{
   ## Purpose is to condense a large data matrix by summarizing groups
   ## or row or column values
   column_method <- match.arg(column_method);
   row_method <- match.arg(row_method);
   column_edge <- match.arg(column_edge);
   row_edge <- match.arg(row_edge);

   ## Process column and row padding
   if (length(column_pad) < 2) {
      column_pad <- c(0, column_pad);
   }
   column_pad <- rep(column_pad, length.out=2);
   if (any(column_pad) > 0) {
      if (verbose) {
         printDebug("condense_freq_matrix(): ",
            "Processing column_pad:", column_pad);
      }
      xpaddedcols <- unlist(rep(list(1, seq_len(ncol(x)), ncol(x)),
         c(column_pad[1], 1, column_pad[2])));
      x <- x[,xpaddedcols,drop=FALSE];
   }
   if (length(row_pad) < 2) {
      row_pad <- c(0, row_pad);
   }
   row_pad <- rep(row_pad, length.out=2);
   if (any(row_pad) > 0) {
      if (verbose) {
         printDebug("condense_freq_matrix(): ",
            "Processing row_pad:", row_pad);
      }
      xpaddedrows <- unlist(rep(list(1, seq_len(nrow(x)), nrow(x)),
         c(row_pad[1], 1, row_pad[2])));
      x <- x[xpaddedrows,,drop=FALSE];
   }

   if (column_n < ncol(x)) {
      ## Condense by column
      if (verbose) {
         printDebug("condense_matrix(): ",
            "Condensing from ",
            ncol(x),
            " columns to ",
            column_n);
      }
      if (length(colnames(x)) == 0) {
         colnames(x) <- seq_len(ncol(x));
      }
      if ("half" %in% column_edge) {
         col_l <- cutIntoChunks(nameVector(colnames(x)), column_n*2);
      } else {
         col_l <- cutIntoChunks(nameVector(colnames(x)), column_n);
      }
      col_f <- factor(list2groups(col_l),
         levels=names(col_l));
      col_names <- rev(colnames(x))[match(unique(col_f), rev(col_f))];
      col_values <- as.numeric(col_names) + column_offset;
      x <- t(splicejam::shrinkMatrix(t(x),
         returnClass="matrix",
         groupBy=col_f));
      colnames(x) <- as.character(col_values);
   }
   if (row_n < nrow(x)) {
      ## Condense by row
      if (verbose) {
         printDebug("condense_matrix(): ",
            "Condensing from ",
            nrow(x),
            " rows to ",
            row_n);
      }
      if (length(rownames(x)) == 0) {
         rownames(x) <- seq_len(nrow(x));
      }
      if ("half" %in% row_edge) {
         row_l <- cutIntoChunks(nameVector(rownames(x)), row_n*2);
         row_first <- head(row_l, 1);
         row_last <- tail(row_l, 1);
         row_new <- unlist(tail(head(row_l, -1), -1));
         row_l2 <- cutIntoChunks(nameVector(row_new), row_n-1);
         row_l2_names <- sapply(row_l2, function(i){
            mean(as.numeric(i))
         });
         row_names <- c(as.numeric(head(row_first[[1]], 1)),
            row_l2_names,
            as.numeric(tail(row_last[[1]], 1)));
         row_l <- c(row_first, row_l2, row_last);
         names(row_l) <- row_names;
         row_f <- factor(list2groups(row_l),
            levels=names(row_l));
      } else {
         row_l <- cutIntoChunks(nameVector(rownames(x)), row_n);
         row_f <- factor(list2groups(row_l),
            levels=names(row_l));
         row_names <- rev(rownames(x))[match(unique(row_f), rev(row_f))];
         row_values <- as.numeric(row_names) + row_offset;
         row_names <- as.character(row_values);
      }
      x <- splicejam::shrinkMatrix(x,
         returnClass="matrix",
         groupBy=row_f);
      rownames(x) <- row_names;
   }
   return(x);
}

#' Discretize labels
#'
#' Discretize labels
#'
#' This function takes a vector of numeric values, a vector of
#' target values intended to be used as labels, and returns
#' a vector of labels where all unmatched target values are
#' replaced with `""` blank, and matched target values use
#' the target label.
#'
#' @family jam matrix functions
#' @family jam heatmap functions
#'
#' @examples
#' x <- seq(from=-2, to=2, by=0.2);
#' x;
#' xd <- discretize_labels(x, target=pretty(x));
#' data.frame(original_value=x, discretized_label=xd);
#'
#' @export
discretize_labels <- function
(x,
 target=NULL,
 pretty.n=15,
 ...)
{
   ##
   if (!is.numeric(x)) {
      x <- as.numeric(x);
      if (all(is.na(x))) {
         stop("x was not numeric, nor could be coerced to numeric.");
      }
   }
   if (length(target) == 0) {
      target <- pretty(x, n=pretty.n);
   }
   ## Find closest values
   itarget <- t(sapply(target, function(i){
      j <- which.min(abs(i-x));
      c(label=i, which=j, row_value=x[j]);
   }));
   ## Append fraction difference
   itarget <- cbind(itarget,
      diff=rmNA(infiniteValue=0,
         abs(itarget[,"row_value"] - itarget[,"label"])/itarget[,"label"])
   );
   ## Filter by fraction difference from target
   itarget <- subset(itarget, itarget[,"diff"] <= 0.1);
   ## Filter for only one match per label
   itarget <- itarget[match(unique(itarget[,"row_value"]), itarget[,"row_value"]),,drop=FALSE];
   itarget <- itarget[match(unique(itarget[,"label"]), itarget[,"label"]),,drop=FALSE];
   #printDebug("itarget:");print(itarget);

   irowlabels <- rep("", length(x));
   irowlabels[itarget[,"which"]] <- itarget[,"label"];
   irowlabels;
}

#' Cut vector into a list of fixed size
#'
#' Cut vector into a list of fixed size
#'
#' This function provides a basic method to split a vector
#' into a list of vectors of a known length, with each list element
#' being roughly the same size.
#'
#' @return `list` of vectors with length `n`.
#'
#' @param x `vector`, or any R object compatible with `base::split()`.
#' @param n integer number of list elements to create.
#' @param ... additional arguments are ignored.
#'
#' @family jam matrix functions
#'
#' @examples
#' x <- 1:170;
#' cutIntoChunks(x, n=4);
#' lengths(cutIntoChunks(x, n=8))
#'
#' @export
cutIntoChunks <- function
(x,
 n=1,
 ...)
{
   ## purpose is to split a vector into a list with length n
   ## by evenly distributing x across each group.
   cut_breaks <- c(0,cumsum(rep(length(x)/n, n))[-n], length(x));
   x_group <- factor(as.numeric(cut(seq_along(x), breaks=cut_breaks)),
      levels=seq_len(n));
   split(x, x_group);
}

#' Convert list to groups
#'
#' Convert list to groups
#'
#' This function simply expands the `names(x)` to `lengths(x)`
#' which assigns group labels to each element in `x`.
#'
#' @export
list2groups <- function
(x,
 ...)
{
   ## Purpose is to expand a list
   if (length(x) == 0) {
      return(NULL);
   }
   if (length(names(x)) == 0) {
      names(x) <- seq_along(x);
   }
   rep(names(x), lengths(x))
}

#' Heatmap of frequency-time (psd) matrix
#'
#' Heatmap of frequency-time (psd) matrix
#'
#' This function takes a numeric matrix of frequency rows,
#' and time columns, and produces a heatmap showing the
#' power spectral density (psd), using `ComplexHeatmap::Heatmap()`.
#'
#' All customizations from `ComplexHeatmap::Heatmap()`
#' are available by passing arguments to `...`.
#'
#' This function has some arguments intended to make it easier
#' to split rows and columns, as needed.
#'
#' @family jam matrix functions
#' @family jam heatmap functions
#'
#' @return `Heatmap` object returned by `ComplexHeatmap::Heatmap()`.
#'
#' @param x numeric matrix containing frequency rows, and time
#'    columns, where the `rownames(x)` and `colnames(x)` are character
#'    representations of the numeric values. In future this function
#'    may use `attributes(x)` or separate function arguments.
#' @param quantile_max numeric value usually between 0.8 and 0.99,
#'    representing the quantile used for the color ramp maximum. This
#'    setting `0.99` is useful when data may have extremely large
#'    outlier values, that could otherwise obscure the range of
#'    numeric values.
#' @param col argument passed to `ComplexHeatmap::Heatmap()` to
#'    define the color ramp. When `col` is a function, it is assumed
#'    to conform to expectations of `ComplexHeatmap::Heatmap()`,
#'    specifically that output should mimic the output from
#'    `circlize::colorRamp2()`.
#' @param row_label_n,column_label_n numeric value passed to
#'    `discretize_labels()` indicating the target number of labels
#'    to display for rows and columns, respectively.
#' @param row_range optional numeric range used to subset the
#'    heatmap rows where the numeric range is matched with the numeric
#'    values in `rownames(x)`. The heatmap is built, and `row_split`
#'    is defined as necessary, before the data is subsetted, in
#'    order to make sure the row split is applied relative to the
#'    input data.
#' @param column_range optional numeric range used to subset the
#'    heatmap rows, where the numeric range is matched with the numeric
#'    values in `colnames(x)`. The heatmap is built, and `column_split`
#'    is defined as necessary, before the data is subsetted, in
#'    order to make sure the column split is applied relative to the
#'    input data.
#' @param flip character value indicating whether to flip the y-axis,
#'    when `flip="y"`.
#' @param row_split vector of length `ncol(x)` whose values are
#'    used to split the heatmap by row When supplied, all
#'    other `row_split_` arguments are ignored.
#' @param row_split_width numeric value used to define `row_split`
#'    with a fixed number of rows per split.
#' @param row_split_at numeric vector indicating where to split
#'    the heatmap by row, based upon the numeric values of the rownames.
#' @param row_split_names optional vector of names used to label
#'    each heatmap row group. The names are assigned in order, and
#'    are not recycled. Therefore, any additional row splits will
#'    use the original integer row split number.
#' @param column_split vector of length `ncol(x)` whose values are
#'    used to split the heatmap by column. When supplied, all
#'    other `column_split_` arguments are ignored.
#' @param column_split_width numeric value used to define `column_split`
#'    with a fixed number of columns per split.
#' @param column_split_at numeric vector indicating where to split
#'    the heatmap by column, based upon the numeric values of the colnames.
#' @param column_split_names optional vector of names used to label
#'    each heatmap column group. The names are assigned in order, and
#'    are not recycled. Therefore, any additional column splits will
#'    use the original integer column split number.
#' @param border,row_title_rot,name arguments passed to
#'    `ComplexHeatmap::Heatmap()` defined here to provide
#'    suitable recommended default values
#'    which differ from the Heatmap default.
#' @param ... additional arguments are passed to
#'    `ComplexHeatmap::Heatmap()`
#'
#' @examples
#'
#' #freq_heatmap(im,
#' #   row_split_at=c(6.1, 7.4, 8.7),
#' #   row_split_names=c("low", "low theta", "high theta", "high"))
#'
#' @export
freq_heatmap <- function
(x,
 quantile_max=0.99,
 col=getColorRamp("Reds", lens=2, n=25),
 row_label_n=25,
 column_label_n=25,
 row_range=NULL,
 column_range=NULL,
 flip=c("y"),
 row_split=NULL,
 row_split_width=NULL,
 row_split_at=NULL,
 row_split_names=NULL,
 column_split=NULL,
 column_split_width=NULL,
 column_split_at=NULL,
 column_split_names=NULL,
 border=TRUE,
 row_title_rot=0,
 name="psd",
 ...)
{
   #
   if ("y" %in% flip) {
      x <- x[nrow(x):1,,drop=FALSE];
   }
   xcolnames <- colnames(x);
   xrownames <- rownames(x);

   ## Apply color ramp to the numeric range
   if (!is.function(col) && length(quantile_max) > 0) {
      num_range <- quantile(unlist(abs(x)),
         na.rm=TRUE,
         probs=c(1-quantile_max, quantile_max));
      col <- circlize::colorRamp2(
         breaks=seq(from=num_range[1],
            to=num_range[2],
            length.out=25),
         colors=getColorRamp(col, n=25)
      )
   }

   ## Process optional column breaks
   if (length(column_split) == 0) {
      if (length(column_split_width) > 0) {
         column_split_at <- seq(from=column_split_width,
            to=ceiling(max(as.numeric(xcolnames))/column_split_width)*column_split_width,
            by=column_split_width);
      }
      if (length(column_split_at) > 0) {
         column_split <- as.numeric(
            cut(
               as.numeric(xcolnames),
               breaks=unique(c(0, column_split_at, Inf))));
      }
      if (length(column_split_names) > 0) {
         column_split <- ifelse(
            column_split <= length(column_split_names),
            column_split_names[column_split],
            column_split);
      }
      if (length(column_split) > 0) {
         column_split <- factor(column_split,
            levels=unique(column_split));
         names(column_split) <- xcolnames;
      }
   }

   ## Process optional row breaks
   if (length(row_split) == 0) {
      if (length(row_split_width) > 0) {
         row_split_at <- seq(from=row_split_width,
            to=ceiling(max(as.numeric(xrownames))/row_split_width)*row_split_width,
            by=row_split_width);
      }
      if (length(row_split_at) > 0) {
         row_split <- as.numeric(
            cut(
               as.numeric(xrownames),
               breaks=unique(sort(c(0, row_split_at, Inf)))));
      }
      if (length(row_split_names) > 0) {
         row_split <- ifelse(
            row_split <= length(row_split_names),
            row_split_names[row_split],
            row_split);
      }
      if (length(row_split) > 0) {
         row_split <- factor(row_split,
            levels=unique(row_split));
         names(row_split) <- xrownames;
      }
   }
   ## Optional subset of the heatmap
   if (length(column_range) > 0) {
      column_range <- range(column_range);
      column_keep <- (as.numeric(xcolnames) >= min(column_range) &
            as.numeric(xcolnames) <= max(column_range));
      x <- x[,column_keep,drop=FALSE];
      xcolnames <- xcolnames[column_keep]
      column_split <- column_split[column_keep];
   }
   if (length(row_range) > 0) {
      row_range <- range(row_range);
      row_keep <- (as.numeric(xrownames) >= min(row_range) &
         as.numeric(xrownames) <= max(row_range));
      x <- x[row_keep,,drop=FALSE];
      xrownames <- xrownames[row_keep]
      row_split <- row_split[row_keep];
   }

   HM <- ComplexHeatmap::Heatmap(
      x,
      name=name,
      cluster_rows=FALSE,
      cluster_columns=FALSE,
      row_labels=discretize_labels(xrownames,
         pretty.n=row_label_n),
      column_labels=discretize_labels(xcolnames,
         pretty.n=column_label_n),
      column_split=column_split,
      row_split=row_split,
      row_title_rot=row_title_rot,
      col=col,
      border=border,
      ...
   );
   return(HM);
}
