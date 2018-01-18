#' allexcept
#'
#' This function produces the n of a sample or subgroup. Similar to length(), but optimized for tabstat. It can be used outside of tabstat as well.
#' @param x Character vector of all variables (use ls() or colnames() etc)
#' @param except Character vector of variables to exclude
#' @keywords select variables
#' @export
#' @examples
#' allexcept()

allexcept <- function(x, except) {
  omit <- x %in% except
  x[!omit]
}
