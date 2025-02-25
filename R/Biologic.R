#' Read Echem Data for Biologic Cycler
#'
#' This function is used to import echem data from a biologic mpt or mpr file.
#' @param file Path to data file
#' @return A data frame containing the echem data
#' @examples
#' load.biologic("/path/to/file.mpt")
#' load.biologic("/path/to/file.mpr")
#' @keywords internal
load.biologic <- function(file) {
  ext <- regexpr("\\.([[:alnum:]]+)$", file)
  ext <- ifelse(ext > -1L, substring(file, ext + 1L), "")

  if (ext == "mpr") {
    data <- load.biologic.mpr(file)
  } else if (ext == "mpt") {
    data <- load.biologic.mpt(file)
  } else {
    stop(ext, " is not a known extension for a biologic file", call.=FALSE)
  }

  jms.classes::log.debug("Normalising biologic data")

  # Rename columns for consistency between cyclers
  cnames <- names(data)
  for (i in 1:length(cnames)) {
    n <- cnames[[i]]
    #### Column Name Map ####
    newname <- vswitch(n,
      mode="mode",
      ox.red="ox_red",
      error="error",
      control.changes="Control_changes",
      Ns.changes="Ns_changes",
      counter.inc.="counter_inc",
      Ns="Step_Index",
      # If time is a date, we also add "Date_Time"
      time.s="Test_Time.s.",
      control.V.mA="control_v_ma",
      Ewe.V="Voltage.V.",
      X.Ecv..V="Control.Voltage.V.",
      X.Q.Qo..mA.h="q-q0_mah",
      Analog.IN.1.V="analog_in_1_v",
      P.W="p_w",
      Q.charge.discharge.mA.h="q_charge_discharge_mah",
      half.cycle="half_cycle",
      Capacitance.charge..b5.F="capacitance_charge_uf",
      Capacitance.discharge..b5.F="capacitance_discharge_uf",
      X.I..mA="Current.A.",
      dq.mA.h="dq_mah",
      dQ.mA.h="dQ_mah",
      Q.discharge.mA.h="Discharge_Capacity.Ah.",
      Q.charge.mA.h="Charge_Capacity.Ah.",
      Capacity.mA.h="Capacity.Ah.",
      Efficiency..="efficiency.percent",
      Energy.charge.W.h="Charge_Energy.Wh.",
      Energy.discharge.W.h="Discharge_Energy.Wh.",
      control.V="control_v",
      control.mA="control_ma",

      # Biologic seem to class a new cycle as beginning only during charging
      # Whereas it seems more logical (and consistent with other cyclers)
      # that a new cycle begins whenever the loop counter increments.
      # So we store its cycle index separately and calculate the "normal"
      # Cycle_Index later
      cycle.number="Biologic_Cycle_Index",
      x="x",
      I.Range="current_range"
    )
    if (is.na(newname)) {
      jms.classes::log.warn("Unmapped column type: %s", n)
    } else {
      names(data)[[i]] <- newname
    }
  }

  # Adjust units etc.
  if ("Test_Time.s." %in% names(data)) {
    if (is.character(data$Test_Time.s.)) {
      # Absolute time can be specified in the mpt export format options
      jms.classes::log.debug("Converting absolute times to relative times")
      date_time <- strptime(data$Test_Time.s., format="%m/%d/%y %H:%M:%OS")
      data$Date_Time <- date_time
      data$Test_Time.s. <- as.numeric(date_time - date_time[1])
    }
  }

  if ("Step_Index" %in% names(data)) {
    data$Step_Index <- data$Step_Index + 1
  }

  if ("Current.A." %in% names(data)) {
    data$Current.A. <- data$Current.A. / 1000
  }

  if ("Capacity.Ah." %in% names(data)) {
    data$Capacity.Ah. <- data$Capacity.Ah. / 1000
  }

  if ("Discharge_Capacity.Ah." %in% names(data)) {
    data$Discharge_Capacity.Ah. <- abs(data$Discharge_Capacity.Ah.) / 1000
  }
  if ("Charge_Capacity.Ah." %in% names(data)) {
    data$Charge_Capacity.Ah. <- abs(data$Charge_Capacity.Ah.) / 1000
  }

  data
}

# ========= #
#### MPT ####
# ========= #

#' Read Echem Data for Biologic Cycler
#'
#' This function is used to import echem data from a biologic mpt file.
#' @param file Path to data file
#' @return A data frame containing the echem data
#' @examples
#' load.biologic.mpt("/path/to/file.mpt")
#' @keywords internal
load.biologic.mpt <- function(file) {
  jms.classes::log.debug("Reading biologic text file (.mpt)")
  file_header <- readLines(file, n=2)
  if (file_header[[1]] != "EC-Lab ASCII FILE") {
    # e.g. If "Save Headline" is not selected
    jms.classes::log.warn(".mpt file does not appear to have a header")
    warning("No header found for .mpt file")

    # We assume the file just contains the data table, with the column headings
    # as the first row
    skip <- 1
  } else {
    # Nb header lines : ...
    skip <- as.integer(substr(file_header[[2]], 18, 21))
  }

  header_text <- readLines(file, n=skip + 1)
  atts <- .get_biologic_attributes(header_text)
  header <- scan(what=character(), sep="\t", text=header_text[skip], nlines=1, na.strings="", quiet=TRUE)
  data <- read.table(file, sep="\t", skip=skip, header=FALSE, na.strings="XXX", stringsAsFactors=FALSE)

  # Recent ec-lab versions seem to add an extra \t at the end of the header. This gives an NA value (missing name)
  # We drop such names as there are no corresponding data
  header <- make.names(header[!is.na(header)], unique=TRUE)

  names(data) <- header

  # Convert flags to logical
  data$ox.red <- as.logical(data$ox.red)
  data$error <- as.logical(data$error)
  data$control.changes <- as.logical(data$control.changes)
  data$Ns.changes <- as.logical(data$Ns.changes)
  data$counter.inc. <- as.logical(data$counter.inc.)

  for (n in names(atts)) {
    attr(data, n) <- atts[n][[1]]
  }

  return(data)
}

#' Get additional attributes from a biologic mpt file
#'
#' This function finds the date and channel for a biologic dataset
#'
#' @param header_lines The header of a biologic mpt file
#' @return A list of attributes that were found
#' @keywords internal
.get_biologic_attributes <- function(header_text) {
  # TODO: there are more details that we could parse here
  atts <- list()
  try(
    {
      date_line <- suppressWarnings(grep("Acquisition started on", header_text))
      if (length(date_line)) {
        atts$date <- as.Date(substr(header_text[[date_line]], 26, 44), format="%m/%d/%Y %T")
      }

      channel_line <- suppressWarnings(grep("Run on channel", header_text))
      if (length(channel_line)) {
        atts$channel <- substr(header_text[[channel_line]], 18, 30)
      }
    },
    silent=TRUE
  )
  atts
}


# ========= #
#### MPR ####
# ========= #


#' Read Echem Data for Biologic Cycler
#'
#' This function is used to import echem data from a biologic mpr file.
#' Originally based on Chris Kerr's python script (https://github.com/chatcannon/galvani) and modified to support additional data types.
#' @param file Path to data file
#' @return A data frame containing the echem data
#' @examples
#' load.biologic.mpr("/path/to/file.mpr")
#' @keywords internal
load.biologic.mpr <- function(mprfile) {
  jms.classes::log.debug("Reading biologic modular file (.mpr)")
  mpr_file <- file(mprfile, "rb")
  mpr_magic <- c(charToRaw("BIO-LOGIC MODULAR FILE\x1a                         "), rep_len(as.raw(0), 4))
  magic <- readBin(mpr_file, what="raw", n=length(mpr_magic))
  if (!all(magic == mpr_magic)) {
    stop("Unknown magic found in .mpr file", call.=F)
  }

  modules <- read_VMP_modules(mpr_file)
  jms.classes::log.debug("Found %s modules", length(modules))
  close(mpr_file)

  settings_mod <- data_module <- maybe_log_module <- NULL
  for (m in 1:length(modules)) {
    if (modules[[m]]["shortname"] == "Set   ") {
      settings_mod <- m # IGNORED
    }
    if (modules[[m]]["shortname"] == "data  ") {
      data_module <- m
    }
    if (modules[[m]]["shortname"] == "LOG   ") {
      maybe_log_module <- m # IGNORED
    }
    # We also have External Device module (for e.g. temperature probes) -- data format currently unknown
  }

  if (is.null(data_module)) {
    available_modules <- sapply(modules, function(x) x[["shortname"]])
    available_modules <- paste0(available_modules, collapse=", ")
    jms.classes::log.error("Data module not found, available modules:\n%s", available_modules)
    stop("Data module not found", call.=F)
  }

  n_data_points <- sum(2^.subset(0:31, as.logical(rawToBits(modules[[data_module]]$data[1:4]))))
  n_columns <- sum(2^.subset(0:7, as.logical(rawToBits(modules[[data_module]]$data[5]))))

  if (!n_data_points > 0) {
    jms.classes::log.error("Invalid number of data points found in mpr file: %s", n_data_points)
    stop("Invalid number of data points found in mpr file", call.=F)
  }
  if (!n_columns > 0) {
    jms.classes::log.error("Invalid number of data columns found in mpr file: %s", n_columns)
    stop("Invalid number of data columns found in mpr file", call.=F)
  }

  jms.classes::log.debug("Data module version = %s", modules[[data_module]]["version"])

  if (modules[[data_module]]["version"] == 0) {
    # 1 byte unsigned integer
    column_types <- as.integer(modules[[data_module]]$data[6:(5 + n_columns)])
    remaining_headers <- modules[[data_module]]$data[(5 + n_columns):100]
    main_data <- modules[[data_module]]$data[101:length(modules[[data_module]]$data)]
  } else if (modules[[data_module]]["version"] %in% c(2, 3)) {
    # 2 byte unsigned integer (little endian)
    column_types <- rep_len(NA, n_columns)
    for (i in 1:n_columns) {
      column_types[[i]] <- sum(2^.subset(0:15, as.logical(rawToBits(modules[[data_module]]$data[(4 + i * 2):(5 + i * 2)]))))
    }
    ## There are 405 bytes of data before the main array starts
    remaining_headers <- modules[[data_module]]$data[(6 + 2 * n_columns):405]
    main_data_start <- ifelse(modules[[data_module]]["version"] == 2, 406, 407)
    # in v3 there is a 01 between the headers and data
    main_data <- modules[[data_module]]$data[main_data_start:length(modules[[data_module]]$data)]
  } else {
    stop(sprintf("Unrecognised version for data module: %s", modules[[data_module]]["version"]), call.=F)
  }

  # Have seen at least one instance where
  if (!all(remaining_headers == 0)) {
    jms.classes::log.warn("Unknown headers: %s", remaining_headers[remaining_headers != 0])
    warning("Unknown headers were found in the mpr file, ignoring")
  }

  col_types <- VMPdata_dtype_from_colIDs(column_types)

  sizes <- col_types$size[col_types$size > 0]
  if (any(col_types$flag)) {
    # Flag information contained in a single byte integer at the start of every row
    sizes <- c(1, sizes)
  }

  if (!length(main_data) / n_data_points == sum(sizes)) {
    jms.classes::log.error(
      "mpr row size (%s / %s = %s) is different to expected (%s == %s)",
      length(main_data), n_data_points, length(main_data) / n_data_points,
      sum(sizes), col_types$row_size
    )
    stop(
      sprintf(
        "mpr row size (%s) is different to expected (%s)",
        length(main_data) / n_data_points, col_types$row_size
      ),
      call.=F
    )
  }

  data <- data.frame(matrix(NA, nrow=n_data_points, ncol=n_columns))
  names(data) <- col_types$name

  # Vectorised reding for each column in turn
  columnised_data <- split(main_data, rep(1:length(sizes), sizes))

  which_column <- 1
  if (any(col_types$flag)) {
    columnised_data[[1]] <- as.integer(columnised_data[[1]])
    which_column <- 2
  }
  for (i in 1:length(col_types$name)) {
    if (col_types$flag[[i]]) {
      if (col_types$type[[i]] == "logical") {
        col_data <- as.logical(bitwAnd(col_types$mask[[i]], columnised_data[[1]]))
      } else if (col_types$type[[i]] == "integer") {
        col_data <- bitwAnd(col_types$mask[[i]], columnised_data[[1]])
      } else {
        jms.classes::log.warn(
          "Unknown column type for flag in mpr file %s. Some data will not be imported.",
          col_types$type[[i]]
        )
        warning(sprintf(
          "Unknown column type for flag in mpr file %s. Some data will not be imported.",
          col_types$type[[i]]
        ), call.=FALSE)
      }
    } else {
      if (col_types$type[[i]] == "integer") {
        col_data <- readBin(columnised_data[[which_column]], "integer", n=n_data_points, size=col_types$size[[i]], endian="little")
      } else if (col_types$type[[i]] == "numeric") {
        col_data <- readBin(columnised_data[[which_column]], "double", n=n_data_points, size=col_types$size[[i]], endian="little")
      } else {
        jms.classes::log.warn(
          "Unknown column type for data in mpr file %s. Some data will not be imported.",
          col_types$type[[i]]
        )
        warning(sprintf(
          "Unknown column type for data in mpr file %s. Some data will not be imported.",
          col_types$type[[i]]
        ), call.=FALSE)
      }
      which_column <- which_column + 1
    }
    data[, col_types$name[[i]]] <- col_data
  }

  attr(data, "date") <- as.Date(modules[[data_module]]$date, format="%m/%d/%y")
  return(data)
}

read_VMP_modules <- function(fileobj, read_module_data=TRUE) {
  mods <- list()
  magic <- charToRaw("MODULEVMP ")
  while (TRUE) {
    module_magic <- readBin(fileobj, what="raw", n=length(magic))
    if (length(module_magic) == 0) {
      # end of file
      return(mods)
    } else if (!all(module_magic == magic)) {
      jms.classes::log.error("Found unknown magic in module: %s", module_magic)
      stop("Unknown magic found in module within .mpr file", call.=F)
    }
    hdr <- list()
    hdr$shortname <- readChar(fileobj, 6, useBytes=T)
    hdr$longname <- readChar(fileobj, 25, useBytes=T)
    hdr$length <- readBin(fileobj, what="raw", n=4, endian="little")
    hdr$length <- sum(2^.subset(0:31, as.logical(rawToBits(hdr$length))))
    hdr$version <- readBin(fileobj, what="raw", n=4, endian="little")
    hdr$version <- sum(2^.subset(0:31, as.logical(rawToBits(hdr$version))))
    hdr$date <- readChar(fileobj, 8, useBytes=T)
    hdr$offset <- seek(fileobj, where=NA)

    if (read_module_data) {
      hdr$data <- readBin(fileobj, what="raw", n=hdr$length, endian="little")
      if (length(hdr$data) != hdr$length) {
        stop(sprintf(
          "Unexpected end of file while reading data\ncurrent module: %s\nlength read: %s\nlength expected: %s",
          hdr$longname,
          length(hdr$data),
          hdr$length
        ),
        call.=F
        )
      }
      mods <- append(mods, list(hdr))
    } else {
      mods <- append(mods, list(hdr))
      seek(fileobj, where=hdr$offset + hdr$length)
    }
  }
}

#### MPR Column definitions ####

# Flags are stored together as a single byte at the start of a row
VMP_colID_flag_map <- list(
  `1`=list(name="mode", mask=3, type="integer"),
  `2`=list(name="ox.red", mask=4, type="logical"),
  `3`=list(name="error", mask=8, type="logical"),
  `21`=list(name="control.changes", mask=16, type="logical"),
  `31`=list(name="Ns.changes", mask=32, type="logical"),
  # Missing one here?
  `65`=list(name="counter.inc.", mask=128, type="logical")
)

# Data makes up the remainder of the row
VMP_colID_data_map <- list(
  `4`=list(name="time.s", size=8, type="numeric"),
  `5`=list(name="control.V.mA", size=4, type="numeric"),

  # 6 is Ewe, 77 and 174 are <Ewe>, I don"t see the difference
  `6`=list(name="Ewe.V", size=4, type="numeric"),
  `77`=list(name="Ewe.V", size=4, type="numeric"),
  `174`=list(name="Ewe.V", size=4, type="numeric"),

  # 7 is relative to current cycle?, 23 to point 0??
  `7`=list(name="dq.mA.h", size=8, type="numeric"),
  `23`=list(name="dQ.mA.h", size=8, type="numeric"),

  # 76 is <I>, 8 is either I or <I> ??
  `8`=list(name="X.I..mA", size=4, type="numeric"),
  `76`=list(name="X.I..mA", size=4, type="numeric"),

  `9`=list(name="Ece.V", size=4, type="numeric"),
  `11`=list(name="X.I..mA", size=8, type="numeric"),
  `13`=list(name="X.Q.Qo..mA.h", size=8, type="numeric"),
  `16`=list(name="Analog.IN.1.V", size=4, type="numeric"),
  `19`=list(name="control.V", size=4, type="numeric"),
  `20`=list(name="control.mA", size=4, type="numeric"),
  # 23 defined above
  `24`=list(name="cycle.number", size=8, type="numeric"),
  `26`=list(name="Rapp.Ohm", size=4, type="numeric"),
  `32`=list(name="freq.Hz", size=4, type="numeric"),
  `33`=list(name="|Ewe|.V", size=4, type="numeric"),
  `34`=list(name="|I|.A", size=4, type="numeric"),
  `35`=list(name="Phase(Z).deg", size=4, type="numeric"),
  `36`=list(name="|Z|.Ohm", size=4, type="numeric"),
  `37`=list(name="Re(Z).Ohm", size=4, type="numeric"),
  `38`=list(name="-Im(Z).Ohm", size=4, type="numeric"),
  `39`=list(name="I.Range", size=2, type="integer"),
  `69`=list(name="R.Ohm", size=4, type="numeric"),
  `70`=list(name="P.W", size=4, type="numeric"),
  `74`=list(name="Energy.W.h", size=8, type="numeric"),
  `75`=list(name="Analog OUT.V", size=4, type="numeric"),
  # 76 defined above
  # 77 defined above
  `78`=list(name="Cs-2.µF-2", size=4, type="numeric"),
  `96`=list(name="|Ece|.V", size=4, type="numeric"),
  `98`=list(name="Phase(Zce).deg", size=4, type="numeric"),
  `99`=list(name="|Zce|.Ohm", size=4, type="numeric"),
  `100`=list(name="Re(Zce).Ohm", size=4, type="numeric"),
  `101`=list(name="-Im(Zce).Ohm", size=4, type="numeric"),
  `123`=list(name="Energy.charge.W.h", size=8, type="numeric"),
  `124`=list(name="Energy.discharge.W.h", size=8, type="numeric"),
  `125`=list(name="Capacitance.charge..b5.F", size=8, type="numeric"),
  `126`=list(name="Capacitance.discharge..b5.F", size=8, type="numeric"),
  `131`=list(name="Ns", size=2, type="integer"),
  `163`=list(name="|Estack|.V", size=4, type="numeric"),
  `168`=list(name="Rcmp.Ohm", size=4, type="numeric"),
  `169`=list(name="Cs.uF", size=4, type="numeric"),
  `172`=list(name="Cp.uF", size=4, type="numeric"),
  `168`=list(name="Cp-2.µF-2", size=4, type="numeric"),
  `169`=list("Cs/µF", size=4, type="numeric"),
  `172`=list("Cp/µF", size=4, type="numeric"),
  `173`=list("Cp-2/µF-2", size=4, type="numeric"),
  # 174 defined above
  `178`=list("(Q-Qo)/C", size=4, type="numeric"),
  `179`=list("dQ/C", size=4, type="numeric"),
  `211`=list("Q charge/discharge/mA.h", size=8, type="numeric"),
  `212`=list("half cycle", size=4, type="integer"),
  `213`=list("z cycle", size=4, type="integer"),
  `217`=list("THD Ewe/%", size=4, type="numeric"),
  `218`=list("THD I/%", size=4, type="numeric"),
  `220`=list("NSD Ewe/%", size=4, type="numeric"),
  `221`=list("NSD I/%", size=4, type="numeric"),
  `223`=list("NSR Ewe/%", size=4, type="numeric"),
  `224`=list("NSR I/%", size=4, type="numeric"),
  `230`=list("|Ewe h2|/V", size=4, type="numeric"),
  `231`=list("|Ewe h3|/V", size=4, type="numeric"),
  `232`=list("|Ewe h4|/V", size=4, type="numeric"),
  `233`=list("|Ewe h5|/V", size=4, type="numeric"),
  `234`=list("|Ewe h6|/V", size=4, type="numeric"),
  `235`=list("|Ewe h7|/V", size=4, type="numeric"),
  `236`=list("|I h2|/A", size=4, type="numeric"),
  `237`=list("|I h3|/A", size=4, type="numeric"),
  `238`=list("|I h4|/A", size=4, type="numeric"),
  `239`=list("|I h5|/A", size=4, type="numeric"),
  `240`=list("|I h6|/A", size=4, type="numeric"),
  `241`=list(name="|E1|.V", size=4, type="numeric"),
  `242`=list(name="|E2|.V", size=4, type="numeric"),
  `271`=list(name="Phase(Z1).deg", size=4, type="numeric"),
  `272`=list(name="Phase(Z2).deg", size=4, type="numeric"),
  `295`=list(name="I Range", size=2, type="integer"),
  `301`=list(name="|Z1|.Ohm", size=4, type="numeric"),
  `302`=list(name="|Z2|.Ohm", size=4, type="numeric"),
  `331`=list(name="Re(Z1).Ohm", size=4, type="numeric"),
  `332`=list(name="Re(Z2).Ohm", size=4, type="numeric"),

  `361`=list(name="-Im(Z1).Ohm", size=4, type="numeric"),
  `362`=list(name="-Im(Z2).Ohm", size=4, type="numeric"),
  `391`=list(name="<E1>.V", size=4, type="numeric"),
  `392`=list(name="<E2>.V", size=4, type="numeric"),
  `422`=list(name="Phase(Zstack).deg", size=4, type="numeric"),
  `423`=list(name="|Zstack|.Ohm", size=4, type="numeric"),
  `424`=list(name="Re(Zstack).Ohm", size=4, type="numeric"),
  `425`=list(name="-Im(Zstack).Ohm", size=4, type="numeric"),
  `426`=list(name="<Estack>.V", size=4, type="numeric"),
  `430`=list(name="Phase(Zwe-ce).deg", size=4, type="numeric"),
  `431`=list(name="|Zwe-ce|.Ohm", size=4, type="numeric"),
  `432`=list(name="Re(Zwe-ce).Ohm", size=4, type="numeric"),
  `433`=list(name="-Im(Zwe-ce).Ohm", size=4, type="numeric"),
  `434`=list(name="(Q-Qo).C", size=4, type="numeric"),
  `435`=list(name="dQ.C", size=4, type="numeric"),
  `438`=list("step time/s", size=8, type="numeric"),
  `441`=list(name="<Ecv>/V", size=4, type="numeric"),
  `462`=list(name="Temperature.degC", size=4, type="numeric"),
  `467`=list(name="Q.charge.discharge.mA.h", size=8, type="numeric"),
  `468`=list(name="half.cycle", size=4, type="integer"),
  `469`=list(name="z.cycle", size=4, type="integer"),
  `471`=list(name="<Ece>.V", size=4, type="numeric"),
  `473`=list(name="THD Ewe.pct", size=4, type="numeric"),
  `474`=list(name="THD I.pct", size=4, type="integer"),
  `476`=list(name="NSD Ewe.pct", size=4, type="integer"),
  `477`=list(name="NSD I.pct", size=4, type="integer"),
  `479`=list(name="NSR Ewe.pct", size=4, type="integer"),
  `480`=list(name="NSR I.pct", size=4, type="integer"),
  `486`=list(name="|Ewe h2|.V", size=4, type="numeric"),
  `487`=list(name="|Ewe h3|.V", size=4, type="numeric"),
  `488`=list(name="|Ewe h4|.V", size=4, type="numeric"),
  `489`=list(name="|Ewe h5|.V", size=4, type="numeric"),
  `490`=list(name="|Ewe h6|.V", size=4, type="numeric"),
  `491`=list(name="|Ewe h7|.V", size=4, type="numeric"),
  `492`=list(name="|I h2|.A", size=4, type="numeric"),
  `493`=list(name="|I h3|.A", size=4, type="numeric"),
  `494`=list(name="|I h4|.A", size=4, type="numeric"),
  `495`=list(name="|I h5|.A", size=4, type="numeric"),
  `496`=list(name="|I h6|.A", size=4, type="numeric"),
  `497`=list(name="|I h7|.A", size=4, type="numeric"),
  `498`=list("Q charge/mA.h", size=8, type="numeric"),
  `499`=list("Q discharge/mA.h", size=8, type="numeric"),
  `500`=list("step time/s", size=8, type="numeric"),
  `501`=list("Efficiency/%", size=8, type="numeric"),
  `502`=list("Capacity/mA.h", size=8, type="numeric"),
  `505`=list("Rdc/Ohm", size=4, type="numeric"),
  `509`=list("Acir/Dcir Control", size=1, type="integer")
)

# Combine the two maps
VMP_colID_map <- do.call(rbind, append(
  lapply(VMP_colID_flag_map, function(x) {
    x$size <- 0
    x$flag <- TRUE
    x[c("name", "size", "type", "mask", "flag")]
  }),
  lapply(VMP_colID_data_map, function(x) {
    x$mask <- NA
    x$flag <- FALSE
    x
  })
))

VMPdata_dtype_from_colIDs <- function(colIDs) {
  colIDs <- as.character(colIDs)

  missing <- !colIDs %in% rownames(VMP_colID_map)
  if (any(missing)) {
    missing <- colIDs[missing]
    s <- ifelse(length(missing) > 1, "s", "")
    missing <- paste0(missing, collapse=", ")
    jms.classes::log.error("mpr column type%s %s not implemented", s, missing)
    stop(sprintf("mpr column type%s %s not implemented", s, missing), call.=F)
  }

  col_types <- VMP_colID_map[colIDs, ]
  # Columns --> list of vectors maintaining mode
  col_types <- lapply(split(col_types, col(col_types)), unlist, F, F)
  # Restore names
  names(col_types) <- c("name", "size", "type", "mask", "flag")

  return(col_types)
}
