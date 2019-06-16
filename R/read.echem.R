#' Read Echem Data
#'
#' This function is used to import echem data.
#' @param echem_file Path to data file
#' @return An echem.data.object containing the echem data
#' @export
#' @examples
#' read.echem("/path/to/file.mpr")
#' read.echem("/path/to/file.mpt")
#' read.echem("/path/to/file.xlsx")
#' read.echem("/path/to/file.txt")
#' read.echem("/path/to/file.idf")
read.echem <- function(path, ...) {
  jms.classes::load.jms(path, .read.echem, ...)
}

.read.echem <- function(echem_file, cycler_type=NA, ...) {
  if (is.null(echem_file) || is.na(echem_file) || echem_file == "") {
    jms.classes::log.warn("File not specified")
    return(NA)
  }

  jms.classes::log.info('Reading echem data from "%s"', echem_file)

  if (!file.exists(echem_file)) {
    # file doesn't exist
    stop("Unable to find file to load -- is the path typed correctly?", call.=FALSE)
  }
  jms.classes::log.debug('"%s" exists.', echem_file)
  if (is.na(cycler_type)) {
    cycler_type <- guess_cycler(echem_file)
  }
  cycler_types <- names(cycler_types())
  cycler_type <- cycler_normalise(cycler_type)

  if (is.na(cycler_type)) {
    stop(paste0(
      "Unknown cycler type! Known cyclers are: ",
      paste0(cycler_types, collapse=", ")
    ), call.=FALSE)
  }

  jms.classes::log.info('Treating "%s" as a %s file', echem_file, cycler_type)

  echem <- loader_for_cycler(cycler_type, echem_file)
  echem <- load.addMissingColumns(echem)

  if (all(is.null(echem))) {
    return(NA)
  }

  jms.classes::log.info('Successfully loaded data for "%s"', echem_file)

  echem <- as.echem.data.object(echem)

  jms.classes::xcol(echem) <- "Test_Time.s."
  jms.classes::xscale(echem) <- 1 / 3600
  jms.classes::xlab(echem) <- expression("Time / h")
  jms.classes::ycol(echem) <- "Voltage.V."

  # Check that columns were found
  xc <- jms.classes::xcol(echem)
  yc <- jms.classes::ycol(echem)
  yc2 <- jms.classes::y2col(echem)
  if (length(xc) != 1 || is.na(xc)) {
    jms.classes::log.warn(
      "xcol is not correct: xcol = [%s]",
      paste0(xc, collapse=",")
    )
    warning("Unable to find time column")
  }
  if (length(yc) != 1 || is.na(yc)) {
    jms.classes::log.warn(
      "ycol is not correct: ycol = [%s]",
      paste0(yc, collapse=",")
    )
    warning("Unable to find voltage column")
  }
  if (length(yc2) != 1 || !is.na(yc2)) {
    jms.classes::log.warn(
      "y2col is not correct: y2col = [%s]",
      paste0(yc2, collapse=",")
    )
    warning("A second y axis has been specified")
  }

  jms.classes::log.debug("Set xcol = %s; ycol = %s; y2col = %s", xc, yc, yc2)

  if (cycler_type == "internal") {
    # Attributes already added
    return(echem)
  }
  # Add attributes:
  attr(echem, "Cycler") <- cycler_type

  echem
}
