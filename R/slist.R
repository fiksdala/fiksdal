#' slist
#'
#' This function splits a singular character vector separated by spaces into distinct elements. i.e. slist('a b c d e') will output c('a', 'b', 'c', 'd', 'e')
#' @param x A character vector
#' @keywords character vector, split
#' @export
#' @examples
#' slist()

slist <- function(x) {
  strsplit(x, "\\s+")[[1]]
}
