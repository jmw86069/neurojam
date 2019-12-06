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
 verbose=TRUE,
 ...)
{
   ## Purpose is to condense a large data matrix by summarizing groups
   ## or row or column values
   column_method <- match.arg(column_method);
   row_method <- match.arg(row_method);
   column_edge <- match.arg(column_edge);
   row_edge <- match.arg(row_edge);

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
