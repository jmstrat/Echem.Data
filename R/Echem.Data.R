#' @author Josh Stratford, \email{}
#' ...
#' @import Plotting.Utils
NULL

.onLoad <- function(libname, pkgname){
  Plotting.Utils::create_data_type('echem',expression('Capacity / mAhg'^-1),'Voltage / V',envir=asNamespace('Echem.Data'))
}

