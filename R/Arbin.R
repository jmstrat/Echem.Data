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
  jms.classes::log.debug("Found [%s] worksheets in arbin excel file", paste0(sn, collapse=", "))
  if (is.null(sn)) {
    return(NULL)
  }

  # Find sheet(s) containing data

  snums <- grep("^Channel_[[:digit:]]", sn)

  if (length(snums) == 0) {
    warning("Found no data worksheets in arbin file [", file, "]")
    return(NULL)
  }

  jms.classes::log.debug("Worksheet(s) [%s] appear to contain data", paste0(snums, collapse=", "))

  tryCatch({
    # Read sheets one by one
    for (i in 1:length(snums)) {
      s <- snums[[i]]
      jms.classes::log.debug("Reading data from worksheet [%s]", s)
      ws <- tryCatch(.xldata(file, s), error=function(e) {
        jms.classes::log.debug("Got error %s, trying workaround.", e)
        ### WARNING: THE FOLLOWING IS A WORKAROUND FOR A BUG IN readxl ###
        ### THIS MAY BREAK ###

        chart_sheet <- grep("^Channel_Chart", sn)
        snums[snums > chart_sheet] <- snums[snums > chart_sheet] - 1
        jms.classes::log.debug("Adjusted data worksheet number(s) to [%s]", paste0(snums, collapse=", "))
        s <- snums[[i]]
        jms.classes::log.debug("Reading data from worksheet [%s]", s)
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

    # Arbin data defines capacity differently to other cyclers, in that it is cumulative with
    # discharge adding to the value and charge subtracting.
    # We store that seperately, the "normal" definition will then be calculated by load.addMissingColumns
    dc_col <- match("Discharge_Capacity.Ah.", names(dat))
    if (!is.na(dc_col)) {
      names(dat)[[dc_col]] <- "Arbin_Discharge_Capacity.Ah."
    }

    c_col <- match("Charge_Capacity.Ah.", names(dat))
    if (!is.na(c_col)) {
      names(dat)[[c_col]] <- "Arbin_Charge_Capacity.Ah."
    }

    for (n in names(atts)) {
      attr(dat, n) <- atts[n][[1]]
    }
    return(dat)
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
  jms.classes::log.debug("Getting additional attributes from arbin data file")
  atts <- list()
  try({
    info <- .xldata(file, "Info", .name_repair="minimal")
    index <- which(info == "Start_DateTime", arr.ind=TRUE)
    # This will give the wrong answer for dates between 1/1/1900 and 1/3/1900 :)
    atts$date <- as.Date("1899-12-30") + as.numeric(info[index[1, "row"] + 1, index[1, "col"]])

    index <- which(info == "Channel", arr.ind=TRUE)
    atts$channel <- info[index[1, "row"] + 1, index[1, "col"]]

    index <- which(info == "Schedule_File_Name", arr.ind=TRUE)
    atts$program_name <- info[index[1, "row"] + 1, index[1, "col"]]
  }, silent=TRUE)
  atts
}
