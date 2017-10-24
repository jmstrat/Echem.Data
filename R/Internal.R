#' Read echem data that has previously been exported to a csv file
#'
#' This function is used to import echem data from a csv file that was generated using \code{\link{export_echem}}.
#' @param file Path to data file
#' @return A data frame containing the echem data
#' @examples
#' load.internal("/path/to/file.csv")
#' @keywords internal
load.internal <- function(file) {
  data <- readLines(file, n=20) #Assume no more than 19 attributes
  headerRow <- grep("Voltage.V.",data)
  atts=.get_internal_attributes(data[c(1:(headerRow-1))])
  data=read.table(file, sep=",", skip = headerRow-1, header=TRUE)
  for(n in names(atts)) {
    attr(data,n)<-atts[n][[1]]
  }
  return(data)
}


#' Get additional attributes from an exported csv file
#'
#' This function finds the attributes for an dataset
#'
#' @param header_lines The header of an exported csv file
#' @return A list of attributes that were found
#' @keywords internal
.get_internal_attributes <- function(header_text) {
  atts=list()
  header_text=strsplit(header_text,',')
  for(line in header_text) {
    attName=line[[1]]
    attValue=line[[2]]
    if(attName=='date') attValue=as.Date(attValue)
    if(attName=='mass') attValue=as.numeric(attValue)
    atts[[attName]]=attValue
  }
  atts
}
