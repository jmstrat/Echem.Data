#' @author Josh Stratford, \email{}
#' ...
NULL

#' @importFrom utils capture.output
#' @importFrom utils read.table

.onLoad <- function(libname, pkgname) {
  jms.classes::create_data_type("echem", expression("Time / s"), "Voltage / V", envir=asNamespace("Echem.Data"))
}

# NOTE These functions are created in the .onLoad function and added to the namespace,
# we need to export and document them here, if overrides to the default behaviour are
# required, then a new function should be created under the same name, the default
# function can be accessed as <function>.super(...)

#' @title Echem data objects
#' @param x The object
#' @rdname data_objects_null_exports

#' @name echem.data.object
#' @usage echem.data.object(...)
#' @export
#' @rdname data_objects_null_exports
#' @keywords internal
NULL

#' @name is.echem.data.object
#' @usage is.echem.data.object(x)
#' @export
#' @rdname data_objects_null_exports
#' @keywords internal
NULL

#' @name as.echem.data.object
#' @usage as.echem.data.object(...)
#' @export
#' @rdname data_objects_null_exports
#' @keywords internal
NULL
