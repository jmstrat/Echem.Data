#' Read echem data that has previously been exported to a csv file
#'
#' This function is used to import echem data from a csv file that was generated using \code{\link{export}}.
#' @param path Path to data file
#' @return A data frame containing the echem data
#' @examples
#' load.internal("/path/to/file.csv")
#' @keywords internal
load.internal <- function(path) {
  # Open the file for reading
  conn <- file(path, open="rt")
  # Read until we find a line starting with #S
  header <- c()
  x <- TRUE
  while (x) {
    line <- readLines(conn, n=1)
    header <- append(header, line)
    x <- !grepl("^==== Data ====", line)
  }

  atts <- .get_internal_attributes(header[-c(1, length(header))])
  data <- read.table.echem(conn, sep=",", header=TRUE)

  close(conn)

  for (n in names(atts)) {
    attr(data, n) <- atts[n][[1]]
  }
  return(data)
}


#' Get additional attributes from an exported csv file
#'
#' This function finds the attributes for an dataset
#'
#' @param header_text The header of an exported csv file
#' @return A list of attributes that were found
#' @keywords internal
.get_internal_attributes <- function(header_text) {
  atts <- list()
  header_text <- strsplit(header_text, ",")
  for (line in header_text) {
    if (!length(line) == 2) {
      next
    }
    attName <- line[[1]]
    attValue <- line[[2]]
    if (attName == "date") attValue <- as.Date(attValue)
    if (attName == "Characteristic Mass") attValue <- as.numeric(attValue)
    atts[[attName]] <- attValue
  }
  atts
}
