#' @author Josh Stratford, \email{}
#' ...
NULL

#' @importFrom utils capture.output
#' @importFrom utils read.table

.onLoad <- function(libname, pkgname) {
  jms.classes::create_data_type("echem", expression("Time / s"), "Voltage / V", envir=asNamespace("Echem.Data"))
}

#' @export
echem.data.object <- function(...) echem.data.object.super(...)

#' @export
is.echem.data.object <- function(...) is.echem.data.object.super(...)
