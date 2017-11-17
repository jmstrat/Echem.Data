#' @author Josh Stratford, \email{}
#' ...
NULL

.onLoad <- function(libname, pkgname){
  jms.classes::create_data_type('echem',expression('Time / s'),'Voltage / V',envir=asNamespace('Echem.Data'))
}

#' @export
echem.data.object <- function(...) echem.data.object.super(...)

#' @export
is.echem.data.object <- function(...) is.echem.data.object.super(...)
