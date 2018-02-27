#' lastnumbers
#'
#' This function returns the last numbers of a string (that ends in numbers) in numerical form.
#' @param x A string that ends in numbers
#' @keywords strsplit, numbers, regexp
#' @export
#' @examples
#' > lastnumbers('2343lkasdALKENC____33,,,srle567')
#' [1] 567

lastnumbers <- function(x) {
  output <- strsplit(x,'[^0-9]')
  as.numeric(output[[1]][length(output[[1]])])
}
