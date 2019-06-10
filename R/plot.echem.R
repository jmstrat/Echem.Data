#' Plot echem data
#'
#' This function is used to plot echem data.
#' @param x An echem.data.object
#' @param ... Additional parameters passed to \code{\link[jms.classes]{plot.jms.data.object}}
#' @export
plot.echem.data.object <- function(x, ..., frac=c(F, T), div=c(5, 2)) {
  NextMethod(x, frac=frac, div=div, ...)
}
