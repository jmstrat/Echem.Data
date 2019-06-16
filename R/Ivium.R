#' Read Echem Data for ivium Cycler
#'
#' This function is used to import echem data from an ivium idf or txt file.
#' @param file Path to data file
#' @return A data frame containing the echem data
#' @examples
#' load.ivium("/path/to/file.txt")
#' @keywords internal
load.ivium <- function(file) {
  ext <- regexpr("\\.([[:alnum:]]+)$", file)
  ext <- ifelse(ext > -1L, substring(file, ext + 1L), "")

  if (ext == "idf") {
    load.ivium.data.file(file)
  } else if (ext == "txt") {
    load.ivium.txt(file)
  }
}

load.ivium.data.file <- function(file) {
  jms.classes::log.info('Reading ivium .idf data from "%s"', file)
  lines <- readLines(file, skipNul=TRUE)
  pd <- grep("primary_data", perl=T, lines)

  if (!length(pd)) {
    stop("No data found in idf file!", call.=FALSE)
  }
  pd <- pd[[1]]

  # n_cols=lines[[pd+1]]
  # We assume 3 columns here but it's actually as stated above...
  n_lines <- as.numeric(lines[[pd + 2]])

  data <- read.table(text=lines[(pd + 3):(pd + 3 + n_lines)])
  names(data) <- c("Test_Time.s.", "Current.A.", "Voltage.V.")

  st <- grep("starttime=", perl=T, lines)
  if (length(st)) {
    st <- st[[1]]
    attr(data, "date") <- as.Date(lines[[st]], format="starttime=%d/%m/%Y %T")
  }
  data
}

load.ivium.txt <- function(file) {
  jms.classes::log.info('Reading ivium .txt data from "%s"', file)
  data <- read.table(file, sep="\t", header=TRUE)
  if (ncol(data) != 3) {
    data <- read.table(file, header=FALSE)
    if (ncol(data) != 3) {
      stop("Expected 3 column data format: Time (s), Current (A), Voltage (V)", call.=FALSE)
    }
  }
  names(data) <- c("Test_Time.s.", "Current.A.", "Voltage.V.")
  data$Current.A. <- data$Current.A. / 1000
  data
}
