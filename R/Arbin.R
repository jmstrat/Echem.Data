#' Read Echem Data for Arbin Cycler
#'
#' This function is used to import echem data from an Arbin xls file.
#' @param file Path to data file
#' @return A data frame containing the echem data
#' @examples
#' load.arbin("/path/to/file.xls")
#' @keywords internal
load.arbin <- function(file) {
  # Get sheet names
  sn <- .xlsheets(file)
  if (is.null(sn)) {
    return (NULL)
  }

  # Find sheet(s) containing data

  snums <- grep("^Channel_[[:digit:]]", sn)

  if (length(snums) == 0) {
    warning("Found no data worksheets in arbin file [", file, "]")
    return (NULL)
  }

  tryCatch({
      # Read sheets one by one
      for (i in 1:length(snums)) {
        s <- snums[[i]]
        # Read
        ws <- tryCatch(.xldata(file, s), error = function (e) {
          jms.classes::log.debug("Got error %s, trying workaround.", e)
          ### WARNING: THE FOLLOWING IS A WORKAROUND FOR A BUG IN readxl ###
          ### THIS MAY BREAK ###

          chart_sheet <- grep("^Channel_Chart", sn)
          snums[snums > chart_sheet] <- snums[snums > chart_sheet] - 1
          s <- snums[[i]]
          .xldata(file, s)

          ### END WORKAROUND ###
        })

        # Add to data frame
        if (i == 1) {
          dat <- ws
        } else {
          dat <- rbind(dat, ws)
        }
      }
      n <- names(dat)
      n <- gsub("[()]", ".", n)
      names(dat) <- n
      atts <- .get_arbin_attributes(file)
      dat <- as.data.frame(dat)
      for (n in names(atts)) {
        attr(dat, n) <- atts[n][[1]]
      }
      return (dat)
    }, error=function(e) {
      jms.classes::log.error(e)
      warning(sprintf("Error loading [%s]. Failed to read Arbin data.", file))
      NULL
    })
}

#' Get additional attributes from an arbin excel file
#'
#' This function finds the date, channel and schedule name for an arbin dataset
#'
#' @param file Path to the arbin file
#' @return A list of attributes that were found
#' @keywords internal
.get_arbin_attributes <- function(file) {
  atts <- list()
  try({
    info <- .xldata(file, "Info")
    index <- which(info == "Start_DateTime", arr.ind=TRUE)
    #This will give the wrong answer for dates between 1/1/1900 and 1/3/1900 :)
    atts$date <- as.Date("1899-12-30") + as.numeric(info[index[1, "row"] + 1, index[1, "col"]])

    index <- which(info == "Channel", arr.ind=TRUE)
    atts$channel <- info[index[1, "row"] + 1, index[1, "col"]]

    index <- which(info == "Schedule_File_Name", arr.ind=TRUE)
    atts$schedule <- info[index[1, "row"] + 1, index[1, "col"]]
  }, silent=TRUE)
  atts
}
