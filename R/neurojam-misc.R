
#' Convert list to igraph
#'
#' This function takes a list of character vectors, and converts
#' them to an igraph object. It is intended to help recognize when
#' multiple list elements share the same items.
#'
#' When `type` is `"full"` each list element is converted to
#' a fully connected network (all entries connected to all other
#' entries.)
#'
#' When `type` is `"star"` each list element is converted to
#' a star network, with each element connected to one central
#' node names(x).
#'
#' @param x list object, or character vector which will be split
#'    into a list using `base::strsplit()` in the form `strsplit(x, delim)`.
#' @param delim character value used when `x` is not a list, passed
#'    to `base::strsplit()`.
#' @param type character string indicating the type of network defined
#'    for each list element: `"star"` creates a star network, each
#'    node is connected to a central node; `"full"` creates a fully
#'    connected network with each node connected to all other nodes.
#' @param color_sub named vector of colors, either containing names
#'    `"names"` and `"values"` for default colors for the `names(x)`
#'     and the values in `x`, respectively; or a named vector whose
#'     names are `names(x)` and/or values in `x`.
#' @param ... additional arguments are passed to
#'    `jamba::setTextContrastColor()` to determine a contrasting
#'    label color.
#'
#' @family jam misc functions
#'
#' @examples
#' x <- list(
#'    A=c("one", "two", "three"),
#'    B=c("one", "four", "five"),
#'    C=c("six", "seven", "eight")
#' );
#' plot(list2igraph(x))
#'
#' @export
list2igraph <- function
(x,
 delim="[, \t]+",
 type=c("star", "full"),
 color_sub=c(names="dodgerblue", values="gold"),
 useGrey=15,
 ...)
{
   ## Purpose is to convert a list of accession numbers to an igraph,
   ## where genelist is a vector of comma-delimited genes.
   if (!suppressPackageStartupMessages(require(igraph))) {
      stop("The igraph package is required.");
   }
   if (!is.list(x)) {
      x <- strsplit(x, delim);
   }
   if (length(names(x)) == 0) {
      names(x) <- paste0("set_", jamba::padInteger(seq_along(x)));
   }
   if (length(color_sub) == 0) {
      color_sub <- c(names="dodgerblue",
         values="gold");
   }
   if (!"names" %in% names(color_sub)) {
      color_sub["names"] <- "dodgerblue";
   }
   if (!"values" %in% names(color_sub)) {
      color_sub["values"] <- "goldenrod";
   }
   g_l <- lapply(jamba::nameVectorN(x), function(i){
      genes <- x[[i]];
      if ("star" %in% type) {
         g <- make_star(length(genes)+1);
         V(g)$name <- c(i, genes);
         V(g)$type <- rep(c("names", "values"),
            lengths(list(i, genes)));
         #V(g)$color <- rep(color_sub[c("names", "values")],
         #   lengths(list(i, genes)));
      } else if ("full" %in% type) {
         g <- make_full_graph(length(genes));
         V(g)$name <- c(genes);
         V(g)$type <- "values";
         #V(g)$color <- rep(color_sub["values"],
         #   length(genes));
      }
      g;
   });

   ig <- graph.union(g_l);
   V(ig)$color <- color_sub["values"];
   V(ig)$color[V(ig)$name %in% names(x)] <- color_sub["names"];

   type_attrs <- jamba::vigrep("type_[0-9]+$", list.vertex.attributes(ig));
   type_v <- jamba::pasteByRow(do.call(cbind,
      lapply(type_attrs, function(i){
         get.vertex.attribute(ig, i);
      })), condenseBlanks=TRUE);
   i_value <- grepl("values", type_v);
   i_names <- grepl("names", type_v);
   i_both <- (i_value & i_names);
   V(ig)$type <- ifelse(i_both, "both",
      ifelse(i_value, "values", "names"));
   for (i in type_attrs) {
      ig <- remove.vertex.attribute(ig, i);
   }

   if (any(V(ig)$name %in% names(color_sub))) {
      i_names <- (V(ig)$name %in% names(color_sub));
      V(ig)[i_names]$color <- color_sub[V(ig)[i_names]$name];
   }
   V(ig)$label.color <- jamba::setTextContrastColor(V(ig)$color,
      useGrey=useGrey,
      ...);
   return(ig);
}
