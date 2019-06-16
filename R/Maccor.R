#' Read Echem Data for maccor Cycler
#'
#' This function is used to import echem data from a maccor txt file.
#' @param file Path to data file
#' @return A data frame containing the echem data
#' @examples
#' load.maccor("/path/to/file.txt")
#' @keywords internal
load.maccor <- function(file) {
  # Read data
  data <- read.table(file, skip=3, header=T, sep="\t", comment.char="", stringsAsFactors=F)

  # Adjust names
  for (i in 1:length(names(data))) {
    n <- names(data)[[i]]
    names(data)[[i]] <- vswitch(n,
      Rec="Rec",
      Cycle="Cycle_Index",
      Step="Step_Index",
      Test.Time="Test_Time.string.",
      Test.Time..sec.="Test_Time.s.",
      Step.Time..sec.="Step_Time.s.",
      Capacity..mAHr.="Capacity.Ah.",
      Energy="Energy.Wh,",
      Current..uA.="Current.A.",
      Voltage="Voltage.V.",
      MD="MD",
      ES="ES",
      DPT.Time="DPT.Time",
      ACImp.Ohms="AC_Impedance.Ohm.",
      DCIR.Ohms="Internal_Resistance.Ohm.",
      X="x",
      Capacity="Capacity",
      Aux..1="Aux..1",
      Units="Units",
      Aux..2="Aux..2",
      Units.1="Units.1",
      Aux..3="Aux..3",
      Units.2="Units.2",
      Aux..4="Aux..4",
      Units.3="Units.3"
    )
  }

  # Check for Capacity.Ah. column
  if (!"Capacity.Ah." %in% names(data)) {
    if ("Capacity" %in% names(data)) {
      names(data)[names(data) == "Capacity"] <- "Capacity.Ah."
    }
  }
  if ("Capacity.Ah." %in% names(data)) {
    # Check for data
    if (all(data$Capacity.Ah. == 0)) {
      # If no data, delete column
      data <- data[, !(names(data) == "Capacity.Ah.")]
    } else {
      # Adjust units
      data$Capacity.Ah. <- data$Capacity.Ah. / 1000
    }
  }

  # Remove thousands separators
  if ("Current.A." %in% names(data)) {
    data$Current.A. <- sapply(data$Current.A., function(x) {
      as.numeric(gsub(",", "", x))
    })
  }
  if ("Test_Time.s." %in% names(data)) {
    data$Test_Time.s. <- sapply(data$Test_Time.s., function(x) {
      as.numeric(gsub(",", "", x))
    })
  }
  if ("Step_Time.s." %in% names(data)) {
    data$Step_Time.s. <- sapply(data$Step_Time.s., function(x) {
      as.numeric(gsub(",", "", x))
    })
  }
  if ("Voltage.V." %in% names(data)) {
    data$Voltage.V. <- sapply(data$Voltage.V., function(x) {
      as.numeric(gsub(",", "", x))
    })
  }
  # Adjust current units
  data$Current.A. <- data$Current.A. / 1e6

  # Ensure current is signed
  if ("MD" %in% names(data)) {
    data$Current.A.[data$MD == "D"] <- -1 * abs(data$Current.A.[data$MD == "D"])
  }

  # Convert time string to seconds
  if ("Test_Time.string." %in% names(data) && (!"Test_Time.s." %in% names(data))) {
    mat <- do.call(rbind, strsplit(as.character(data$Test_Time.string.), "[d:]"))
    storage.mode(mat) <- "numeric"
    data$Test_Time.string. <- mat %*% c(86400, 3600, 60, 1)
    names(data)[names(data) == "Test_Time.string."] <- "Test_Time.s."
  }

  # Adjust step indexes
  data$Step_Index <- data$Step_Index - 2 # TODO check if this is universal...

  # Adjust cycle indexes (for rest at start)
  data$Cycle_Index[data$Cycle_Index < 1] <- 1 # TODO check if this is universal...

  if ("MD" %in% names(data)) {
    # Not sure what s means, but the few rows with this flag don't seem to contain data
    data <- data[data$MD != "S", ]
  }

  # Add attributes
  try({
    header_text <- readLines(file, n=3)
    header <- strsplit(header_text, "\t")

    attr(data, "date") <- as.Date(header[[1]][[4]], format="%d %B %Y, %I:%M:%S %p")
    attr(data, "channel") <- header[[2]][[4]]
    attr(data, "schedule") <- header[[3]][[2]]
  }, silent=TRUE)

  # Return
  return(data)
}
