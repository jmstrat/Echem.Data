#' List of supported cyclers and their file extensions
#'
#' @export
#' @rdname cycler_types
cycler_types <- function() {
  list(
    arbin=c("xlsx", "xls"),
    biologic=c("mpr", "mpt"),
    land=c("cex", "xlsx", "xls", "txt"),
    ivium=c("idf", "txt"),
    maccor=c("txt"),
    internal=c("csv")
  )
}
#' @export
#' @rdname cycler_types
supported_file_extensions <- function() unique(unlist(cycler_types()))

.cycler_loader_functions <- c(
  arbin="load.arbin",
  biologic="load.biologic",
  land="load.land",
  ivium="load.ivium",
  maccor="load.maccor",
  internal="load.internal"
)

loader_for_cycler <- function(cycler, echem_file) {
  loader <- .cycler_loader_functions[[cycler]]
  if (is.null(loader)) stop('Cannot find loader for "', cycler, '"')
  # This preserves the stack trace as loader is a string, not an
  # anonymous function
  return(do.call(loader, list(echem_file)))
}

#' Guess cycler for file
#'
#' A very basic function to choose a cycler based on the file extension, implementation may change if more cyclers are added
#'
#' @param file Path to data file
#' @return The cycler type
guess_cycler <- function(file) {
  ext <- regexpr("\\.([[:alnum:]]+)$", file)
  ext <- ifelse(ext > -1L, substring(file, ext + 1L), "")

  jms.classes::log.debug('Attempting to guess the cycler for "%s" using its extension ("%s")', file, ext)

  if (ext == "mpt" || ext == "mpr") {
    return("biologic")
  } else if (ext == "xlsx" || ext == "xls") {
    jms.classes::log.debug("Found an excel workbook, attempting to read the sheet names to guess the cycler type")
    # Could be arbin or land, so read the 1st line
    # Get sheet names
    sn <- .xlsheets(file)
    if (is.null(sn)) {
      jms.classes::log.error('Could not find any worksheets, unable to determine cycler type for "%s"', file)
      return(NA)
    }
    # Find sheet(s) containing data
    arbin_sheets <- grepl("^Channel_[[:digit:]]", sn)
    if (any(arbin_sheets)) {
      return("arbin")
    }

    land_sheets <- grepl("^Record", sn)
    if (any(land_sheets)) {
      return("land")
    }

    jms.classes::log.error("Unable to map sheet names to a cycler type")
    return(NA)
  } else if (ext == "idf") {
    return("ivium")
  } else if (ext == "cex") {
    return("land")
  } else if (ext == "csv") {
    return("internal")
  } else if (ext == "txt") {
    jms.classes::log.debug("Found a plain text .txt file, attempting to read the table header to guess the cycler type")
    # Could be land or ivium, so read the 1st line
    header_text <- readLines(file, n=1)
    header <- read.table(sep="\t", header=T, text=header_text, quote="")
    if (identical(names(header), c("time..s", "I..mA", "E..V"))) {
      return("ivium")
      return("Today.s.Date.")
    } else if (names(header)[[1]] == "Today.s.Date.") {
    }
    return("land")
  }
  jms.classes::log.error('Could not determine cycler type for "%s"', file)
  return(NA)
}

#' Normalise a string to represent a cycler name
#'
#' @param cycler_type The string to normaliase
#' @return The normalised string
#' @export
cycler_normalise <- function(cycler_type) {
  if (is.na(cycler_type) || length(cycler_type) == 0) {
    return(NA)
  }
  cycler_type <- tolower(cycler_type)
  cycler_type <- gsub("[[:space:]]", "", cycler_type)
  cycler_types <- names(cycler_types())
  if (!cycler_type %in% cycler_types) {
    return(NA)
  }
  return(cycler_type)
}
