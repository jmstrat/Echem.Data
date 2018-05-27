#' Read Echem Data for land (lahne) Cycler
#'
#' This function is used to import echem data from a land txt file.
#' @param file Path to data file
#' @return A data frame containing the echem data
#' @examples
#' load.land("/path/to/file.txt")
#' @keywords internal
load.land <- function(file) {
  ext= regexpr("\\.([[:alnum:]]+)$", file)
  ext=ifelse(ext > -1L, substring(file, ext + 1L), "")
  if(ext=='cex') {
    data = load.land.cex(file)
  } else if(ext=='txt') {
    data = load.land.txt(file)
  } else if(ext %in% c('xls', 'xlsx')) {
    data = load.land.xls(file)
  }

  data$Current.A.=data$Current.A./1000
  data$Capacity.Ah.=data$Capacity.Ah./1000
  data$Energy.Wh.=data$Energy.Wh./1000

  data = load.addMissingColumns(data)

  return(data)
}

load.land.cex <- function(file) {
  warning('Reading binary data for land cyclers is experimental. Please check that the result is valid!')

  #WARNING: I don't know where this comes from, if it changes,
  #nor whether I've assumed the correct format for decimal numbers
  CONVERSION_FACTOR = 16128
  ENDIANNESS='little'

  cex_file = file(file, 'rb')
  #I don't know what this is
  cex_magic = as.raw(c(0x10, 0x11, 0x09, 0x88))
  magic = readBin(cex_file,what='raw',n=length(cex_magic), endian=ENDIANNESS)
  if(!all(magic == cex_magic))
    stop('Unknown magic found in .cex file',call.=F)

  data_version=c()
  for(i in 1:4) {
    data_version[[i]] = readBin(cex_file,what='int',n=1, size=1, endian=ENDIANNESS)
  }
  data_version=paste0(data_version, collapse='.')
  if(data_version != '0.1.1.0') stop("Unknown cex data version.")

  machine_number = readBin(cex_file,what='int',n=1, size=1, endian=ENDIANNESS)+1
  channel_number = readBin(cex_file,what='int',n=1, size=1, endian=ENDIANNESS)+1

  #This seems to change slightly -- I don't know what it is yet
  unknown = readBin(cex_file,what='raw',n=6, endian=ENDIANNESS)

  start_time = as.POSIXct(readBin(cex_file,what='int',n=1, endian=ENDIANNESS), origin="1970-01-01")

  unknown = readBin(cex_file,what='raw',n=4, endian=ENDIANNESS)

  voltage_range = readBin(cex_file,what='int',n=1, size=2, endian=ENDIANNESS)
  current_range = readBin(cex_file,what='int',n=1, size=2, endian=ENDIANNESS)

  convert_voltage <- function(x_int) x_int * voltage_range / CONVERSION_FACTOR
  convert_current <- function(x_int) x_int * current_range / CONVERSION_FACTOR
  convert_capacity <- function(x_int) x_int / CONVERSION_FACTOR / 3600

  active_material_mass = readBin(cex_file,what='numeric',size=4, endian=ENDIANNESS)

  #I don't know what this is yet
  unknown = readBin(cex_file,what='raw',n=4, endian=ENDIANNESS)

  monitor_version = rawToChar(readBin(cex_file, what='raw',n=4, endian=ENDIANNESS))

  #This seems to change slightly -- I don't know what it is yet
  unknown = readBin(cex_file,what='raw',n=8, endian=ENDIANNESS)

  #Seems to be the same as start_time
  start_time2 = as.POSIXct(readBin(cex_file,what='int',n=1, endian=ENDIANNESS), origin="1970-01-01")

  #Sometimes the same as start time, sometimes end time??
  some_other_time = as.POSIXct(readBin(cex_file,what='int',n=1, endian=ENDIANNESS), origin="1970-01-01")

  #This seems to change slightly -- I don't know what it is yet
  unknown = readBin(cex_file,what='raw',n=104, endian=ENDIANNESS)

  program_name = rawToChar(readBin(cex_file, what='raw',n=16, endian=ENDIANNESS))

  # Or something like a comment(?)
  comment = rawToChar(readBin(cex_file, what='raw',n=64, endian=ENDIANNESS))

  #This seems to change slightly -- I don't know what it is yet
  unknown = readBin(cex_file,what='raw',n=8, endian=ENDIANNESS)

  #The nulls are probably a character string or something that isn't set...
  begin_program_magic = as.raw(c(0xff, 0x7f, 0xff, 0x7f, 0xff, 0xff, 0xff, 0x7f, 0xff,
                                 0xff, 0xff, 0x7f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                                 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                                 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00))

  #PROGRAM:

  ###BEGIN-MAGIC###
  #...Begin loop...#
  #[MODE (4)]???? [GOTO (1)][STEP (1)][COND TYPE (1)][COND SIGN (1)][COND TYPE (1)]
  #[COND SIGN (1)][COND VALUE (2)][COND VALUE (2)]????????????????[LOG TIME (2)]
  #????[LOG VOLTAGE (2)]????
  #...end loog...#
  #BBBBFFFF ???????? ???????? ????????

  # [MODE]:     7000 Rest
  #             0200 Discharge CC
  #             0300 Charge CC
  #             7100 IF
  #             F000 OR

  #[COND TYPE]: 1    Time
  #             2    Voltage
  #             6    Cycles

  #[COND SIGN]: 0    <=
  #             1    >=

  magic = readBin(cex_file,what='raw',n=40, endian=ENDIANNESS)
  if(!all(magic == begin_program_magic))
    stop('Unknown program magic found in .cex file',call.=F)

  end_program_magic = as.raw(c(0xbb, 0xbb, 0xff, 0xff))

  mode_rest = as.raw(c(0x70, 0x00, 0x00, 0x00))
  mode_dischargeCC = as.raw(c(0x02, 0x00, 0x00, 0x00))
  mode_chargeCC = as.raw(c(0x03, 0x00, 0x00, 0x00))
  mode_IF = as.raw(c(0x71, 0x00, 0x00, 0x00))
  mode_OR = as.raw(c(0xf0, 0x00, 0x00, 0x00))

  program = list()
  while(TRUE) {
    step=c()
    mode = readBin(cex_file,what='raw',n=4, endian=ENDIANNESS)
    if(all(mode == end_program_magic)) {
      break
    } else if(all(mode == mode_rest)) {
      step[['mode']] = 'rest'
    } else if(all(mode == mode_dischargeCC)) {
      step[['mode']] = 'dischargeCC'
    } else if(all(mode == mode_chargeCC)) {
      step[['mode']] = 'chargeCC'
    } else if(all(mode == mode_IF)) {
      step[['mode']] = 'IF'
    } else if(all(mode == mode_OR)) {
      step[['mode']] = 'OR'
    } else {
      step[['mode']] = 'Unknown'
    }

    unknown = readBin(cex_file,what='raw',n=2, endian=ENDIANNESS)

    goto = readBin(cex_file,what='int',n=1, size=1, signed=FALSE, endian=ENDIANNESS)
    if(goto == 255) {
      goto = 'Next'
    } else if(goto == 254) {
      goto = 'End OK'
    }
    step[['goto']] = goto

    step[['step_number']] = readBin(cex_file,what='int',n=1, size=1, signed=FALSE, endian=ENDIANNESS)+1

    condition_type=c()
    condition_sign=c()
    end_condition=c()
    for(i in 1:2) {
      condition_type[[i]] = readBin(cex_file,what='int',n=1, size=1, signed=FALSE, endian=ENDIANNESS)
      condition_sign[[i]] = readBin(cex_file,what='int',n=1, size=1, signed=FALSE, endian=ENDIANNESS)
    }
    for(i in 1:2) {
    end_condition[[i]] = readBin(cex_file,what='raw',n=2, endian=ENDIANNESS)
    }

    for(i in 1:2) {
      if(condition_type[[i]]==0) {
        step[[paste0('condition_type_',i)]] = NA
        step[[paste0('condition_',i)]] = NA
        condition_sign[[i]] = NA
      } else if(condition_type[[i]]==1) {
        step[[paste0('condition_type_',i)]] = 'time'
        step[[paste0('condition_',i)]] = readBin(end_condition[[i]], what='int', size=2, signed=FALSE, endian=ENDIANNESS)
      } else if(condition_type[[i]]==2) {
        step[[paste0('condition_type_',i)]] = 'voltage'
        step[[paste0('condition_',i)]] = convert_voltage(readBin(end_condition[[i]], what='int', size=2, signed=TRUE, endian=ENDIANNESS))
        # TODO convert voltage
      } else if(condition_type[[i]]==6) {
        step[[paste0('condition_type_',i)]] = 'cycles'
        step[[paste0('condition_',i)]] = readBin(end_condition[[i]], what='int', size=2, endian=ENDIANNESS)
      } else {
        step[[paste0('condition_type_',i)]] = 'unknown'
      }

      if(is.na(condition_sign[[i]])) {
        step[[paste0('condition_sign_',i)]] <- NA
      } else if(condition_sign[[i]] == 0) {
        step[[paste0('condition_sign_',i)]] <- '<='
      } else if(condition_sign[[i]] == 1) {
        step[[paste0('condition_sign_',i)]] <- '>='
      }
    }

    unknown = readBin(cex_file,what='raw',n=8, endian=ENDIANNESS)

    step[['log_time']] = readBin(cex_file,what='int',n=1, size=2, signed = FALSE, endian=ENDIANNESS)

    #Includes log voltage and possibly log current / capacity or similar
    unknown = readBin(cex_file,what='raw',n=6, endian=ENDIANNESS)

    program[[length(program)+1]]=step
  }

  #Possibly related to number of data points
  bbbb_unknown_stuff = readBin(cex_file,what='raw',n=12, endian=ENDIANNESS)

  #Now begins the actual data
  #Data is in 16 byte chunks
  #Decimals are stored as the numerator of a fraction

  #AAAAFFFF ???????? 00000000 00000000 (Start of header??)
  #BBBBFFFF ???????? 00000000 00000000 (Start of data / header??)

  #EVENT:
  #CCCCFFFF 33000000 [DATA (4)] [timestamp (4)]

  #Change step:
  #CCCCFFFF 22000000 [MODE (2 byte)] [??? (2 byte)] [timestamp (4)]
  #CCCCFFFF 44000000 [OX_RED (1)]000000 00000000
  #CCCCFFFF 91000000 00000000 00000000

  #Known unknown:
  #CCCCFFFF 92000000 ???????? ????????

  #Data point:
  #[test time (4)] [VOLTAGE(2)] [CURRENT(2)] [CAPACITY(4)] [ENERGY??(4)]
  #Can't find anything that tells us the data length, so read in largeish chunks
  CHUNK_SIZE = 100000
  remaining_data=c()
  while (length(a <- readBin(cex_file, 'raw', n=CHUNK_SIZE, endian=ENDIANNESS)) > 0) {
    remaining_data = append(remaining_data, a)
  }

  close(cex_file)

  #Split into 4x4 columns
  sizes=c(4,2,2,4,4)
  columnised_data = split(remaining_data,rep(1:length(sizes), sizes))

  n_data_points = length(remaining_data)/sum(sizes)

  columns = c('Test_Time.s.', 'Voltage.V.', 'Current.A.', 'Capacity.Ah.', 'Energy.Wh.', 'mode')
  data=data.frame(matrix(NA, nrow = n_data_points, ncol = length(columns)))
  names(data)<-columns

  data[,1] = readBin(columnised_data[[1]],'integer',size=4, n=n_data_points, endian=ENDIANNESS)
  data[,2] = convert_voltage(readBin(columnised_data[[2]],'int',size=2, n=n_data_points, endian=ENDIANNESS, signed=TRUE))
  data[,3] = convert_current(readBin(columnised_data[[3]],'int',size=2, n=n_data_points, endian=ENDIANNESS, signed=TRUE))
  data[,4] = convert_capacity(readBin(columnised_data[[4]],'int',size=4, n=n_data_points, endian=ENDIANNESS))

  #TODO: This column needs conversion
  warning('Energy column is not currently converted into a useable form.')
  data[,5] = readBin(columnised_data[[5]],'integer',size=4, n=n_data_points, endian=ENDIANNESS)

  #Inspect the control rows (CCCCFFFF)
  control_rows = which(data[,1] == -13108)

  types = readBin(columnised_data[[2]][c(rbind(((control_rows-1)*2+1), ((control_rows-1)*2+2)))],'integer',size=2, n=length(control_rows), endian=ENDIANNESS)
  #IGNORE:
  # 51: 3300
  # This is just an event that occured on the cycler, mostly irrelevant,
  # but may be able to detect end time using this.
  # Ignore for now
  #68: 4400
  # This indicates whether or not there is a current applied I think
  # Ignoring as can be inferred from other data
  #145: 9100
  # Think this just indicates a step change
  #146: 9200
  # I don't know what this is, it seems to occur in clusters and doesn't seem
  # Important to recover the data
  # Silently ignoring for now

  ignore_types = c(51, 68, 145, 146)
  ignore = types %in% ignore_types

  for(i in which(!ignore)) {
    type = types[[i]]
    if(type == 34) { #2200
      control = control_rows[[i]]
      mode = readBin(columnised_data[[4]][((control-1)*4+1):((control-1)*4+2)],'integer',size=2, n=1, endian=ENDIANNESS)
      #Ignore the rest of the data for this control for now
      #(Unknown & timestamp)
      data[control:n_data_points, 'mode'] = mode
    } else {
      warning('Ignoring unknown control sequence type ', type)
      next
    }
  }

  #Remove the control rows
  data = data[-control_rows,]

  #Fix the rownames
  rownames(data) <- NULL

  #Add the discovered attributes to the data
  attr(data,'machine_number')=machine_number
  attr(data,'channel_number')=channel_number
  attr(data,'date')=start_time
  attr(data,'voltage_range')=voltage_range
  attr(data,'current_range')=current_range
  attr(data,'active_material_mass')=active_material_mass
  attr(data,'monitor_version')=monitor_version
  attr(data,'program_name')=program_name
  attr(data,'comment')=comment
  attr(data,'program')=program

  return(data)
}

###NOTE THIS IS UNTESTED!!!###
load.land.xls <- function(file) {
  #Get sheet names
  sn=.xlsheets(file)
  if(is.null(sn)) {
    return(NULL)
  }

  #Find sheet(s) containing data
  snums=grep("^Record",sn)

  if(length(snums)==0) {
    warning("Found no data worksheets in excel file [",file,"]")
    return(NULL)
  }

  tryCatch(
    {
      #Read sheets one by one
      for(i in 1:length(snums)) {
        s=snums[[i]]
        #Read
        ws=.xldata(file,s)
        #Add to data frame
        if(i==1) {
          dat=ws
        } else {
          dat=rbind(dat,ws)
        }
      }
      n=names(dat)
      n=gsub('[()]','.',n)
      names(dat)=n
      dat=as.data.frame(dat)
      return(dat)
    }, error=function(e) {
      jms.classes::log.debug("ERROR: %s",e)
      warning(sprintf('Error loading [%s]. Failed to read Land data.',file))
      NULL
    })
}

load.land.txt <-function(file)
{
  data=read.table(file,sep="\t",header=TRUE)
  names(data) <- c("Data_Point","Test_Time.s.","Step_Time.s.","Voltage.V.","Current.A.","Capacity.Ah.","Energy.Wh.","Date_Time")
  return(data)
}
