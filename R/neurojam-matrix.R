#' Condense a spectral frequence data matrix
#'
#' @family jam matrix functions
#'
#' @export
condense_freq_matrix <- function
(x,
   column_n=ncol(x),
   row_n=nrow(x),
   verbose=TRUE,
   ...)
{
   ## Purpose is to condense a large data matrix by summarizing groups
   ## or row or column values
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
      col_l <- cutIntoChunks(colnames(x), column_n);
      col_f <- factor(list2groups(col_l),
         levels=names(col_l));
      col_names <- colnames(x)[match(unique(col_f), col_f)];
      x <- t(shrinkMatrix(t(x),
         returnClass="matrix",
         groupBy=col_f));
      colnames(x) <- col_names;
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
      row_l <- cutIntoChunks(rownames(x), row_n);
      row_f <- factor(list2groups(row_l),
         levels=names(row_l));
      row_names <- rownames(x)[match(unique(row_f), row_f)];
      x <- shrinkMatrix(x,
         returnClass="matrix",
         groupBy=row_f);
      rownames(x) <- row_names;
   }
   return(x);
}
