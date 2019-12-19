
## Beta effort to create frequency-time numeric matrix class
## based upon data.table.
## Also based loosely on:
## https://github.com/QuantGen/BEDMatrix/blob/master/R/BEDMatrix.R

#' Create FTDT frequency-time data.table
#'
#' Create FTDT frequency-time data.table
#'
#' @export
as.FTDT <- function
(x,
 ...)
{
   if (!"FreqTimeMatrix" %in% class(x)) {
      x <- FreqTimeMatrix(x, ...);
   }
   ftdt <- x@x;

   ## Add attributes
   attr(ftdt, "dimstarts") <- x@dimstarts;
   attr(ftdt, "dimends") <- x@dimends;
   attr(ftdt, "dimunits") <- x@dimunits;
   #attr(ftdt, "dimnames") <- x@dimnames;
   attr(ftdt, "row.names") <- as.character(x@dimnames[[1]]);
   attr(ftdt, "names") <- as.character(x@dimnames[[2]]);
   return(ftdt);
}

FreqTimeMatrix <- setClass("FreqTimeMatrix",
   slots=c(
      x="data.table",
      dims="integer",
      dimstarts="list",
      dimends="list",
      dimunits="character",
      dimnames="list"
))

#' Create FreqTimeMatrix numeric matrix with frequency rows and time columns
#'
#' Create FreqTimeMatrix numeric matrix with frequency rows and time columns
#'
#' This function creates an object class `FreqTimeMatrix` which extends
#' `data.table` and represents frequency in rows, and time in columns.
#' It was created to maintain proper numeric values associated with
#' rows and columns, and specifically the range of values represented.
#'
#' For example, consider columns whose values are `c(0.000, 0.001, 0.002)`.
#' The first column represents a time range from `0.000` until `0.001`,
#' the second column represents a time range from `0.001` until `0.002`,
#' and so on. The duration represented by each column is inferred only
#' by the presence of additional column with consistent step differences.
#' If we extracted only the first column of data, there would be no
#' way of knowing the range of time represented.
#'
#' Further, if we want to condense all points into 1-second intervals,
#' we would want all time ranges from `0.000` until `1.000`, but not
#' including the time range from `1.000` until `1.001`. Extracting exactly
#' the appropriate columns is straightforward. But the output matrix will
#' have columns that represent 1000-fold longer time range, so the output
#' matrix will also need to represent the new time range.
#'
#' To summarize:
#'
#' * FreqTimeMatrix represents row and column numeric values as
#' `row_starts`, `column_starts`, which indicate the starting values;
#' similarly `row_ends`, `column_ends` indicate the ending values.
#' * FreqTimeMatrix represents row and column units, simply as a text
#' label. No further unit-based operations are supported yet.
#' * FreqTimeMatrix data can be summarized by groups of columns or
#' rows, and the output FreqTimeMatrix should indicate the new
#' range of numeric values represented, as relevant.
#'
#'
#' @export
FreqTimeMatrix <- function
(x,
 row_units="Hz",
 row_title="frequency",
 row_starts=as.numeric(rownames(x)),
 row_ends=NULL,
 row_step=NULL,
 column_title="time",
 column_units="sec",
 column_starts=as.numeric(colnames(x)),
 column_ends=NULL,
 column_step=NULL,
 verbose=FALSE,
 ...)
{
   ## Create instance of the class
   if (length(row_starts) == 0) {
      stop("row_starts must be defined.");
   }
   if (length(column_starts) == 0) {
      stop("column_starts must be defined.");
   }
   if (any(is.na(row_starts))) {
      stop("row_starts must all be non-NA.");
   }
   if (any(is.na(column_starts))) {
      stop("column_starts must all be non-NA.");
   }
   if (!"data.table" %in% class(x)) {
      x <- as.data.table(x);
   }
   ## Determine row_ends if needed
   if (length(row_ends) == 0) {
      ## By default, determine median difference in row_starts
      if (length(row_step) == 0) {
         row_step <- median(diff(row_starts));
         if (verbose) {
            printDebug("FreqTimeMatrix(): ",
               "row_ends are calculated by inferring row_step=",
               row_step);
         }
      }
      row_ends <- row_starts + row_step;
   }
   if (length(column_ends) == 0) {
      ## By default, determine median difference in row_starts
      if (length(column_step) == 0) {
         column_step <- median(diff(column_starts));
         if (verbose) {
            printDebug("FreqTimeMatrix(): ",
               "column_ends are calculated by inferring column_step=",
               column_step);
         }
      }
      column_ends <- column_starts + column_step;
   }
   if (length(row_title) == 0 || nchar(row_title) == 0) {
      row_title <- "frequency";
      if (verbose) {
         printDebug("FreqTimeMatrix(): ",
            "defined default row_title=",
            row_title);
      }
   }
   if (length(column_title) == 0 || nchar(column_title) == 0) {
      column_title <- "frequency";
      if (verbose) {
         printDebug("FreqTimeMatrix(): ",
            "defined default column_title=",
            column_title);
      }
   }
   row_title <- head(row_title, 1);
   column_title <- head(column_title, 1);
   xdims <- c(nrow(x), ncol(x));
   xdimstarts <- list(row_starts, column_starts);
   xdimends <- list(row_ends, column_ends);
   xdimunits <- c(row_units, column_units);
   xdimnames <- list(row_starts, column_starts);
   names(xdims) <- c(row_title, column_title);
   names(xdimstarts) <- c(row_title, column_title);
   names(xdimends) <- c(row_title, column_title);
   names(xdimunits) <- c(row_title, column_title);
   names(xdimnames) <- c(row_title, column_title);

   obj <- new(
      "FreqTimeMatrix",
      x=x,
      dims=xdims,
      dimstarts=xdimstarts,
      dimends=xdimends,
      dimunits=xdimunits,
      dimnames=xdimnames
   )
   return(obj)
}

setMethod("show", "FreqTimeMatrix", function(object) {
   dims <- object@dims;
   n <- dims[1L]
   p <- dims[2L]
   cat("FreqTimeMatrix: ",
      (object@dims)[1],
      " x ", (object@dims)[2],
      "\n",
      sep="");
   row_range <- range(c(object@dimstarts[[1]], object@dimends[[1]]));
   column_range <- range(c(object@dimstarts[[2]], object@dimends[[2]]));

   row_dimname <- paste0(
      head(c(names(object@dimnames)[1], "time"), 1),
      "(", object@dimunits[[1]], ")");
   column_dimname <- paste0(
      head(c(names(object@dimnames)[2], "frequency"), 1),
      "(", object@dimunits[[2]], ")");
   cat(paste0(row_dimname, " range:",
      paste(row_range, collapse=" - ")), "\n");
   cat(paste0(column_dimname, " range:",
      paste(column_range, collapse=" - ")), "\n");
   ncolshow <- min(c(20, object@dims[2]));
   nrowshow <- min(c(20, object@dims[1]));
   xsub <- object@x[seq_len(nrowshow),seq_len(ncolshow)];
   print(xsub);
   invisible(object);
});

dim.FreqTimeMatrix <- function(x) {
   x@dims
}

dimnames.FreqTimeMatrix <- function(x) {
   x@dimnames
}

dimunits.FreqTimeMatrix <- function(x) {
   x@dimunits
}

sdim.FreqTimeMatrix <- function(x) {
   jamba::sdim(
      lapply(nameVector(slotNames(x)), function(i){
         slot(x, i);
      }));
}

summary.FreqTimeMatrix <- function(x) {
   jamba::sdim(
      lapply(nameVector(slotNames(x)), function(i){
         slot(x, i);
      }));
}

as.matrix.BEDMatrix <- function(x, ...) {
   as.matrix(x@x[, , drop=FALSE])
}

as.ddata.table.BEDMatrix <- function(x, ...) {
   x@x[, , drop=FALSE]
}

