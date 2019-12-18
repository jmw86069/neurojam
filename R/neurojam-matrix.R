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
#' @examples
#' freq_m <- matrix(ncol=200,
#'    nrow=96,
#'    data=seq_len(96*200),
#'    dimnames=list(frequency=seq(from=1, by=0.2, length.out=96),
#'       time=seq(from=0, by=0.01, length.out=200)));
#'
#' condense_freq_matrix(freq_m, column_fixed_size=10, row_fixed_size=5)[1:20,1:20]
#'
#' @export
condense_freq_matrix <- function
(x,
 column_n=ncol(x),
 column_fixed_size=1,
 row_n=nrow(x),
 row_fixed_size=1,
 column_method=c("min", "max", "mean"),
 row_method=c("min", "mean", "max"),
 column_edge=c("full","half"),
 row_edge=c("full", "half"),
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
         jamba::printDebug("condense_freq_matrix(): ",
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
         jamba::printDebug("condense_freq_matrix(): ",
            "Processing row_pad:", row_pad);
      }
      xpaddedrows <- unlist(rep(list(1, seq_len(nrow(x)), nrow(x)),
         c(row_pad[1], 1, row_pad[2])));
      x <- x[xpaddedrows,,drop=FALSE];
   }

   if (column_n < ncol(x) || column_fixed_size > 1) {
      ## Condense by column
      if (verbose) {
         jamba::printDebug("condense_matrix(): ",
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
         col_l <- cutIntoChunks(nameVector(colnames(x)),
            n=column_n,
            fixed_size=column_fixed_size);
      }
      col_values_l <- lapply(col_l, function(col_i){
         as.numeric(col_i);
      });
      if ("min" %in% column_method) {
         col_values <- sapply(col_values_l, min);
      } else if ("max" %in% column_method) {
         col_values <- sapply(col_values_l, max);
      } else if ("mean" %in% column_method) {
         col_values <- sapply(col_values_l, mean);
      }
      names(col_values_l) <- col_values;
      col_f <- factor(list2groups(col_l),
         levels=names(col_l));
      #col_names <- rev(colnames(x))[match(unique(col_f), rev(col_f))];
      #col_values <- as.numeric(col_names) + column_offset;
      x <- t(splicejam::shrinkMatrix(t(x),
         returnClass="matrix",
         groupBy=col_f));
      colnames(x) <- as.character(col_values);
   } else {
      col_values_l <- NULL;
   }
   if (row_n < nrow(x) || row_fixed_size > 1) {
      ## Condense by row
      if (verbose) {
         jamba::printDebug("condense_matrix(): ",
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
         row_l <- cutIntoChunks(nameVector(rownames(x)),
            n=row_n,
            fixed_size=row_fixed_size);
         row_values_l <- lapply(row_l, function(row_i){
            as.numeric(row_i);
         });
         if ("min" %in% row_method) {
            row_values <- sapply(row_values_l, min);
         } else if ("max" %in% row_method) {
            row_values <- sapply(row_values_l, max);
         } else if ("mean" %in% row_method) {
            row_values <- sapply(row_values_l, mean);
         }
         names(row_values_l) <- row_values;
         row_f <- factor(list2groups(row_l),
            levels=names(row_l));
         #row_names <- rev(rownames(x))[match(unique(row_f), rev(row_f))];
         #row_values <- as.numeric(row_names) + row_offset;
         row_names <- as.character(row_values);
      }
      x <- splicejam::shrinkMatrix(x,
         returnClass="matrix",
         groupBy=row_f);
      rownames(x) <- row_names;
   }
   attr(x, "col_values_l") <- col_values_l;
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
      diff=jamba::rmNA(infiniteValue=0,
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
#' into a list of vectors of a known length, using one of
#' two approaches:
#'
#' * when `n` is supplied, the vector is split into `n` bins
#' with roughly equal number of elements in each bin.
#' * when `fixed_size` is supplied, the vector is split into
#' bins with size `fixed_size`, with optional `offset` used to control
#' the position of the first bin.
#' * when `n` is supplied and `edge_rule="half"` then the
#' first and last bin are half-size, with intermediate bins full-size.
#'
#' Use `fixed_size` when each bin should be defined by strict unit
#' values. Use `offset` to control the break points, when the first
#' element might not fall on an appropriate break.
#'
#' Use `n` as a rapid way to bin a vector into `n` roughly equal
#' pieces.
#'
#' Use `n` and `edge_rule="half"` when you want to bin a vector
#' and want to maintain bins central to unit measures. For example
#' for the vector `c(1, 1.5, 2, 2.5, 3, 3.5, 4)` if you want 4
#' bins, centered at each integer value. Note that `fixed_size`
#' with `offset` might be a better alternative.
#'
#' @return `list` of vectors, where each vector is a subset of the
#' input `x`.
#'
#' @param x `vector`, or any R object compatible with `base::split()`.
#' @param n integer number of list elements to create.
#' @param fixed_size integer number of values to include in each bin.
#'    When `fixed_size` is provided, `n` is ignored.
#' @param edge_rule character value to define how to handle the edges,
#'    where `"full"` treats every bin the same, and `"half"` makes
#'    the first bin and last bin half-size, with full-size bins
#'    in between. The `"half"` approach is recommended in cases
#'    where you are trying to maintain integer values to represent
#'    the mean signal around each integer value.
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
 fixed_size=1,
 offset=0,
 edge_rule=c("full", "half"),
 ...)
{
   ## purpose is to split a vector into a list with length n
   ## by evenly distributing x across each group.
   edge_rule <- match.arg(edge_rule);

   if (length(fixed_size) > 0 && fixed_size > 1) {
      offset <- offset %% fixed_size;
      n_chunks <- ceiling((length(x) + offset) / fixed_size);
      x_group <- tail(head(
         rep(seq_len(n_chunks), each=fixed_size),
         length(x) + offset), length(x));
      return(split(x, x_group))
   }

   if ("full" %in% edge_rule || n < 3) {
      cut_breaks <- round(c(0,cumsum(rep(length(x)/n, n))[-n], length(x)));
      x_group <- factor(as.numeric(cut(seq_along(x), breaks=cut_breaks)),
         levels=seq_len(n));
      split(x, x_group);
   } else if ("half" %in% edge_rule) {
      x_l <- cutIntoChunks(x, n=n * 2 - 1);
      x_first <- head(x_l, 1);
      x_last <- tail(x_l, 1);
      x_new <- unlist(tail(head(x_l, -1), -1));
      x_l2 <- cutIntoChunks(nameVector(x_new), n-2);
      x_l2_names <- sapply(x_l2, function(i){
         mean(as.numeric(i))
      });
      x_names <- c(as.numeric(head(x_first[[1]], 1)),
         x_l2_names,
         as.numeric(tail(x_last[[1]], 1)));
      x_l <- c(x_first, x_l2, x_last);
      names(x_l) <- x_names;
      x_f <- factor(list2groups(x_l),
         levels=names(x_l));
      return(x_l);
   }
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
 col=jamba::getColorRamp("Reds", lens=2, n=25),
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
         colors=jamba::getColorRamp(col, n=25)
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

#' Heatmap of frequency-time matrix for animal signal data
#'
#' Heatmap of frequency-time matrix for animal signal data
#'
#' This function queries the database to return data, then
#' calls `freq_heatmap()` to create the heatmap.
#'
#' @family jam matrix functions
#' @family jam heatmap functions
#'
#' @param dbxcon DBI database connection
#' @param animal,project,phase,filename,channel,time_step_sec,freq_step_hz
#'    arguments used to filter the database table `fca_freq_matrix` in
#'    order to specify only one row in the results, therefore only one
#'    frequency-time matrix.
#' @param plot logical indicating whether to plot the output. When
#'    `plot=FALSE` the `data.frame` of query results is returned, and
#'    no heatmap is created. This option can be helpful when querying
#'    the database.
#' @param type character value indicating the type of output, where
#'    `"Heatmap"` will return the Heatmap object; and `"matrix"` will
#'    return the numeric matrix itself, and not the heatmap.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional arguments are passed to `freq_heatmap()`,
#'    see that function documentation for info on customizing the
#'    heatmap.
#'
#' @examples
#' # db_path <- "ephys_db.sqlite";
#' # dbxcon <- dbx::dbxConnect(adapter="sqlite", dbname=db_path);
#'
#' ## Basic query of available data
#' # head(signal_freq_heatmap(dbxcon), 20)
#'
#' ## Tabulate data by phase
#' # table(signal_freq_heatmap(dbxcon)[,c("project","phase")])
#' # table(signal_freq_heatmap(dbxcon)[,c("animal","phase")])
#'
#' ## Provide animal and phase, create heatmap
#' # signal_freq_heatmap(dbxcon, animal="AF11-4", phase="Acquisition", plot=TRUE)
#'
#' ## Another example
#' # signal_freq_heatmap(dbxcon, animal="SA88-1", phase="Acquisition", plot=TRUE)
#'
#' ## Zoom into a specific time range
#' # signal_freq_heatmap(dbxcon, animal="SA88-1", phase="Acquisition", plot=TRUE, column_range=c(400,700))
#'
#' @export
signal_freq_heatmap <- function
(dbxcon,
 animal=NULL,
 project=NULL,
 phase=NULL,
 filename=NULL,
 channel=NULL,
 time_step_sec=NULL,
 freq_step_hz=NULL,
 plot=FALSE,
 type=c("df", "Heatmap", "matrix"),
 verbose=FALSE,
 ...)
{
   ##
   type <- match.arg(type);
   freq_df <- dbGetQuery(dbxcon,
      "SELECT
      ffm.animal,
      ffm.channel,
      ffm.filename,
      ffm.time_step_sec,
      ffm.freq_step_hz,
      ef.project,
      ef.phase
      FROM
      fca_freq_matrix ffm,
      ephys_file ef
      WHERE
      ffm.filename = ef.filename
      ");
   paramnames <- c("animal", "project", "phase", "channel", "filename",
      "time_step_sec", "freq_step_hz");
   for (paramname in paramnames) {
      val <- get(paramname);
      if (length(val) > 0) {
         if (verbose) {
            jamba::printDebug("signal_freq_heatmap(): ",
               "Subsetting data for ", paramname, ": ",
               val);
         }
         freq_df <- subset(freq_df, freq_df[[paramname]] %in% val);
      }
   }
   if (nrow(freq_df) == 0) {
      jamba::printDebug("No rows were returned from the query. Use argument '",
         "verbose=TRUE",
         "' to debug.");
   }
   if ("df" %in% type) {
      return(freq_df);
   }
   if (nrow(freq_df) > 1) {
      jamba::printDebug("signal_freq_heatmap(): ",
         "Data contains ",
         nrow(freq_df),
         " rows. Please reduce query to one row.");
   }
   i <- 1;

   ## Get im_json
   im_json <- dbGetQuery(dbxcon,
      "SELECT
      im_json
      FROM
      fca_freq_matrix
      WHERE
      filename = ? and
      channel = ? and
      animal = ? and
      time_step_sec = ? and
      freq_step_hz = ?",
      params=list(
         freq_df[["filename"]][i],
         freq_df[["channel"]][i],
         freq_df[["animal"]][i],
         freq_df[["time_step_sec"]][i],
         freq_df[["freq_step_hz"]][i]
         ))[[1]];
   if (verbose) {
      jamba::printDebug("signal_freq_heatmap(): ",
         "Converting im_json with jsonlite::fromJSON().");
   }
   im <- jsonlite::fromJSON(im_json);
   ## Add attributes to help describe the data
   attr(im, "df") <- freq_df[i,,drop=FALSE];

   if ("matrix" %in% type) {
      return(im);
   }
   HM <- freq_heatmap(im,
      ...);
   return(HM);
}
