#' Read Echem Data for Biologic Cycler
#'
#' This function is used to import echem data from a biologic mpt or mpr file.
#' @param file Path to data file
#' @return A data frame containing the echem data
#' @examples
#' load.biologic("/path/to/file.mpt")
#' load.biologic("/path/to/file.mpr")
#' @keywords internal
load.biologic <-function(file)
{
  ext= regexpr("\\.([[:alnum:]]+)$", file)
  ext=ifelse(ext > -1L, substring(file, ext + 1L), "")

  if(ext=='mpr') {
    data=load.biologic.mpr(file)
  } else if(ext=='mpt') {
    data=load.biologic.mpt(file)
  }

  #Rename columns
  for(i in 1:length(names(data))) {
    n=names(data)[[i]]
    names(data)[[i]]<-vswitch(n,
                              mode="mode",
                              ox.red="ox_red",
                              error="error",
                              control.changes="Control_changes",
                              Ns.changes="Ns_changes",
                              counter.inc.="counter_inc",
                              Ns="Step_Index",
                              time.s="Test_Time.s.",
                              control.V.mA="control_v_ma",
                              Ewe.V="Voltage.V.",
                              X.Q.Qo..mA.h="q-q0_mah",
                              Analog.IN.1.V="analog_in_1_v",
                              P.W="p_w",
                              Q.charge.discharge.mA.h="q_charge_discharge_mah",
                              half.cycle="half_cycle",
                              Capacitance.charge..b5.F="capacitance_charge_uf",
                              Capacitance.discharge..b5.F="capacitance_discharge_uf",
                              X.I..mA="Current.A.",
                              dq.mA.h="dq_mah",
                              Q.discharge.mA.h="Discharge_Capacity.Ah.",
                              Q.charge.mA.h="Charge_Capacity.Ah.",
                              Capacity.mA.h="capacity.mah",
                              Efficiency..="efficiency.percent",
                              control.V="control_v",
                              control.mA="control_ma",
                              cycle.number="Cycle_Index",
                              x="x")
  }

  if(!'Current.A.' %in% names(data)) {
    if('p_w' %in% names(data)) {
      data$Current.A.=data$p_w/data$Voltage.V.*1000  #needs to be in mA here
      if('dq_mah' %in% names(data)) {
        data$Current.A.=data$Current.A.*sign(data$dq_mah)
      } else if('q_charge_discharge_mah' %in% names(data)) {
        data$Current.A.=data$Current.A.*sign(data$q_charge_discharge_mah)
      } else if('ox_red' %in% names(data)) {
        #ox_red is not simply the sign of the current unfortunately...
        #It seems to be the sign of the control
        #(i.e. normally the current, but could be the sign of the held potential e.g. during a voltage hold)
        #(Possibly...)

        #That being said, if all else fails it should be a reasonable guess...
        data$Current.A.[!data$ox_red]=data$Current.A.[!data$ox_red]*-1
        warning("Assuming biologic data is exclusively under current control",call.=F)
      } else {
        warning("Unable to determine sign of current for biologic data",call.=F)
      }
    } else if(all(c('dq_mah','Test_Time.s.') %in% names(data))) {
      data$Current.A.=data$dq_mah*3600/c(1,diff(data$Test_Time.s.)) #Needs to be in mA here
      data$Current.A.[[1]]=0
    } else if('control_v_ma' %in% names(data)) {
      warning('Unable to determine current for biologic data, assuming current control and using control data as the current',call.=FALSE)
      data$Current.A.=data$control_v_ma
    } else {
      warning("Unable to determine current for biologic data",call.=F)
    }
  }

  if(!'Cycle_Index' %in% names(data)) {
    if('counter_inc' %in% names(data)) {
      changes=c(0,which(abs(diff(data$counter_inc))==1),nrow(data))+1
      cycles=c()
      ci=0
      for(i in 1:(length(changes)-1)) {
        cycles=append(cycles,rep.int(ci,changes[[i+1]]-changes[[i]]))
        ci=ci+1
      }
      data$Cycle_Index=cycles
    } else {
      warning("Unable to determine cycle index for biologic data",call.=F)
    }
  }

  if(!'dq_mah' %in% names(data)) {
    if('q_charge_discharge_mah' %in% names(data)) {
      data$dq_mah=c(0,diff(data$q_charge_discharge_mah))
    } else if('capacity.mah' %in% names(data)) {
      data$dq_mah=c(0,diff(data$capacity.mah))
    }
  }

  if(!'capacity.mah' %in% names(data)) {
    if(all(c('dq_mah','Ns_changes') %in% names(data))) {
      changes=c(0,which(abs(diff(data$Ns_changes))==1),nrow(data))
      capacity=c()
      for(i in 1:(length(changes)-1)) {
        capacity=append(capacity,abs(cumsum(data$dq_mah[(changes[[i]]+1):changes[[i+1]]])))
      }
      data$capacity.mah=capacity
    }
  }
  #Use the sign of the current obtained earlier rather than ox_red to separate (dis)charge capacities
  if('Current.A.' %in% names(data)) {
    s=sign(data$Current.A.)
    if(!'Discharge_Capacity.Ah.' %in% names(data)) {
      if(all(c('Current.A.','capacity.mah') %in% names(data))) {
        data$Discharge_Capacity.Ah.=rep_len(0,nrow(data))
        data$Discharge_Capacity.Ah.[s==-1]=data$capacity.mah[s==-1]
      } else {
        warning("Unable to determine discharge capacity for biologic data",call.=F)
      }
    }
    if(!'Charge_Capacity.Ah.' %in% names(data)) {
      if(all(c('Current.A.','capacity.mah') %in% names(data))) {
        data$Charge_Capacity.Ah.=rep_len(0,nrow(data))
        data$Charge_Capacity.Ah.[s==1]=data$capacity.mah[s==1]
      } else {
        warning("Unable to determine charge capacity for biologic data",call.=F)
      }
    }
  }

  #Adjust units etc.
  data$Step_Index=data$Step_Index+1
  data$Cycle_Index=data$Cycle_Index+1
  data$Current.A.=data$Current.A./1000
  if('Discharge_Capacity.Ah.' %in% names(data)) data$Discharge_Capacity.Ah.=abs(data$Discharge_Capacity.Ah.)/1000
  if('Charge_Capacity.Ah.' %in% names(data)) data$Charge_Capacity.Ah.=abs(data$Charge_Capacity.Ah.)/1000

  return(data)
}

#' Read Echem Data for Biologic Cycler
#'
#' This function is used to import echem data from a biologic mpt file.
#' @param file Path to data file
#' @return A data frame containing the echem data
#' @examples
#' load.biologic.mpt("/path/to/file.mpt")
#' @keywords internal
load.biologic.mpt <- function(file) {
  skip=as.integer(substr(readLines(file,n=2)[[2]],18,21))
  header_text=readLines(file,n=skip+1)
  atts=.get_biologic_attributes(header_text)
  header=read.table(sep="\t",header=T,text=header_text[c(skip,skip+1)])
  data=read.table(file,sep="\t",skip=skip,header=FALSE,na.strings="XXX")
  names(data)<-names(header)

  #Convert flags to logical
  data$ox.red=as.logical(data$ox.red)
  data$error=as.logical(data$error)
  data$control.changes=as.logical(data$control.changes)
  data$Ns.changes=as.logical(data$Ns.changes)
  data$counter.inc.=as.logical(data$counter.inc.)

  for(n in names(atts)) {
    attr(data,n)<-atts[n][[1]]
  }

  return(data)
}

#' Read Echem Data for Biologic Cycler
#'
#' This function is used to import echem data from a biologic mpr file.
#' Ported from Chris Kerr's python script (https://github.com/chatcannon/galvani) and modified to support additional data types.
#' @param file Path to data file
#' @return A data frame containing the echem data
#' @examples
#' load.biologic.mpr("/path/to/file.mpr")
#' @keywords internal
load.biologic.mpr <-function(mprfile) {
  mpr_file = file(mprfile, 'rb')
  mpr_magic = c(charToRaw('BIO-LOGIC MODULAR FILE\x1a                         '),rep_len(as.raw(0),4))
  magic = readBin(mpr_file,what='raw',n=length(mpr_magic))
  if(!all(magic == mpr_magic))
    stop('Unknown magic found in .mpr file',call.=F)

  modules = read_VMP_modules(mpr_file)

  close(mpr_file)

  settings_mod=data_module=maybe_log_module=NA
  for(m in 1:length(modules)) {
    if(modules[[m]]['shortname'] == 'Set   ')
      settings_mod = m
    if(modules[[m]]['shortname'] == 'data  ')
      data_module = m
    if(modules[[m]]['shortname'] == 'LOG   ')
      maybe_log_module = m
    #We also have External Device module (for e.g. temperature probes) -- data format currently unknown
  }

  n_data_points= sum(2^.subset(0:31, as.logical(rawToBits(modules[[data_module]]$data[1:4]))))
  n_columns = sum(2^.subset(0:7, as.logical(rawToBits(modules[[data_module]]$data[5]))))

  if(!n_data_points>0) {
    stop("Invalid number of data points found in mpr file",call.=F)
  }
  if(!n_columns>0) {
    stop("Invalid number of data columns found in mpr file",call.=F)
  }

  if(modules[[data_module]]['version'] == 0) {
    #1 byte unsigned integer
    column_types = as.integer(modules[[data_module]]$data[6:(5+n_columns)])
    remaining_headers = modules[[data_module]]$data[(5 + n_columns):100]
    main_data = modules[[data_module]]$data[101:length(modules[[data_module]]$data)]
  } else if(modules[[data_module]]['version'] == 2) {
    #2 byte unsigned integer (little endian)
    modules[[data_module]]$data[6:(5+n_columns*2)]
    column_types = rep_len(NA,n_columns)
    for(i in 1:n_columns) {
      column_types[[i]]=sum(2^.subset(0:15, as.logical(rawToBits(modules[[data_module]]$data[(4+i*2):(5+i*2)]))))
    }
    ## There are 405 bytes of data before the main array starts
    remaining_headers = modules[[data_module]]$data[(5 + 2 * n_columns):405]
    main_data = modules[[data_module]]$data[406:length(modules[[data_module]]$data)]
  } else{
    stop(sprintf("Unrecognised version for data module: %s" ,modules[[data_module]]['version']),call.=F)
  }

  #   We should check that there are no unknown headers. Python code is:
  #   if sys.version_info.major <= 2:
  #     assert(all((b == '\x00' for b in remaining_headers)))
  #   else:
  #     assert(not any(remaining_headers))


  col_types = VMPdata_dtype_from_colIDs(column_types)

  sizes=col_types$size[col_types$size>0]
  if(any(col_types$flag)) {
    #Flag information contained in a single byte integer at the start of every row
    sizes=c(1,sizes)
  }

  if(!length(main_data)/n_data_points==sum(sizes))
    stop(sprintf("mpr row size (%s) is different to expected (%s)",length(main_data)/n_data_points,col_types$row_size),call.=F)

  data=data.frame(matrix(NA, nrow = n_data_points, ncol = n_columns))
  names(data)<-col_types$name

  #Vectorised reding for each column in turn
  columnised_data=split(main_data,rep(1:length(sizes), sizes))

  which_column=1
  if(any(col_types$flag)) {
    columnised_data[[1]]=as.integer(columnised_data[[1]])
    which_column=2
  }
  for(i in 1:length(col_types$name)) {
    if(col_types$flag[[i]]) {
      if(col_types$type[[i]]=='logical') {
        col_data=as.logical(bitwAnd(col_types$mask[[i]],columnised_data[[1]]))
      } else if(col_types$type[[i]]=='integer') {
        col_data=bitwAnd(col_types$mask[[i]],columnised_data[[1]])
      } else {
        stop(sprintf("Unknown column type for flag in mpr file %s",col_types$type[[i]]),call.=FALSE)
      }
    } else {

      if(col_types$type[[i]]=='integer') {
        col_data=readBin(columnised_data[[which_column]], 'integer', n = n_data_points, size = col_types$size[[i]],endian = "little")
      } else if(col_types$type[[i]]=='numeric') {
        col_data=readBin(columnised_data[[which_column]], 'double', n = n_data_points, size = col_types$size[[i]],endian = "little")
      } else {
        stop(sprintf("Unknown column type for data in mpr file %s",col_types$type[[i]]),call.=FALSE)
      }
      which_column=which_column+1
    }
    data[,col_types$name[[i]]]=col_data
  }

  attr(data,'date')=as.Date(modules[[data_module]]$date,format='%m/%d/%y')
  return(data)
}

read_VMP_modules <- function(fileobj, read_module_data=TRUE) {
  mods=list()
  magic=charToRaw('MODULEVMP ')
  while(TRUE) {
    module_magic = readBin(fileobj,what='raw',n=length(magic))
    if(length(module_magic) == 0) {  # end of file
      return(mods)
    } else if(!all(module_magic == magic)) {
      stop('Unknown magic found in module within .mpr file',call.=F)
    }
    hdr=list()
    hdr$shortname=readChar(fileobj, 6, useBytes = T)
    hdr$longname=readChar(fileobj, 25, useBytes = T)
    hdr$length=readBin(fileobj, what='raw',n=4, endian='little')
    hdr$length=sum(2^.subset(0:31, as.logical(rawToBits(hdr$length))))
    hdr$version=readBin(fileobj, what='raw',n=4, endian='little')
    hdr$version=sum(2^.subset(0:31, as.logical(rawToBits(hdr$version))))
    hdr$date=readChar(fileobj, 8, useBytes = T)
    hdr$offset = seek(fileobj,where=NA)

    if(read_module_data){
      hdr$data = readBin(fileobj,what='raw',n=hdr$length, endian='little')
      if(length(hdr$data) != hdr$length)
        stop(sprintf("Unexpected end of file while reading data\ncurrent module: %s\nlength read: %s\nlength expected: %s",
                     hdr$longname,
                     length(hdr$data),
                     hdr$length),
             call.=F)
      mods=append(mods,list(hdr))
    } else {
      mods=append(mods,list(hdr))
      seek(fileobj,where=hdr$offset+hdr$length)
    }
  }
}

VMPdata_dtype_from_colIDs <- function(colIDs){
  col_types=list(name=c(),size=rep_len(0,length(colIDs)),type=c(),flag=rep_len(FALSE,length(colIDs)),mask=rep_len(NA,length(colIDs)))
  for(i in 1:length(colIDs)) {
    colID=colIDs[[i]]
    if(colID == 1) {
      col_types$name[[i]]='mode'
      col_types$mask[[i]]=3
      col_types$type[[i]]='integer'
      col_types$flag[[i]]=TRUE
    } else if(colID == 2)  {
      col_types$name[[i]]='ox.red'
      col_types$mask[[i]]=4
      col_types$type[[i]]='logical'
      col_types$flag[[i]]=TRUE
    } else if(colID == 3) {
      col_types$name[[i]]='error'
      col_types$mask[[i]]=8
      col_types$type[[i]]='logical'
      col_types$flag[[i]]=TRUE
    } else if(colID == 21) {
      col_types$name[[i]]='control.changes'
      col_types$mask[[i]]=16
      col_types$type[[i]]='logical'
      col_types$flag[[i]]=TRUE
    } else if(colID == 31) {
      col_types$name[[i]]='Ns.changes'
      col_types$mask[[i]]=32
      col_types$type[[i]]='logical'
      col_types$flag[[i]]=TRUE
    } else if(colID == 65) {
      col_types$name[[i]]='counter.inc.'
      col_types$mask[[i]]=128
      col_types$type[[i]]='logical'
      col_types$flag[[i]]=TRUE
    } else if(colID ==131 ) {
      col_types$name[[i]]='Ns'
      col_types$size[[i]]=2
      col_types$type[[i]]='integer'
    }  else if(colID == 4) {
      col_types$name[[i]]='time.s'
      col_types$size[[i]]=8
      col_types$type[[i]]='numeric'
    }
    else if(colID == 5) {
      col_types$name[[i]]='control.V.mA'
      col_types$size[[i]]=4
      col_types$type[[i]]='numeric'
    }
    # 6 is Ewe, 77 is <Ewe>, I don't see the difference
    else if(colID %in% c(6, 77)) {
      col_types$name[[i]]='Ewe.V'
      col_types$size[[i]]=4
      col_types$type[[i]]='numeric'
    }
    # Can't see any difference between 7 and 23
    else if(colID %in% c(7, 23)) {
      col_types$name[[i]]='dq.mA.h'
      col_types$size[[i]]=8
      col_types$type[[i]]='numeric'
    }
    # 76 is <I>, 8 is either I or <I> ??
    else if(colID %in% c(8, 76)) {
      col_types$name[[i]]='X.I..mA'
      col_types$size[[i]]=4
      col_types$type[[i]]='numeric'
    }
    else if(colID == 11) {
      col_types$name[[i]]='X.I..mA'
      col_types$size[[i]]=8
      col_types$type[[i]]='numeric'
    }
    else if(colID == 19) {
      col_types$name[[i]]='control.V'
      col_types$size[[i]]=4
      col_types$type[[i]]='numeric'
    }
    else if(colID == 24) {
      col_types$name[[i]]='cycle.number'
      col_types$size[[i]]=8
      col_types$type[[i]]='numeric'
    }
    else if(colID == 32) {
      col_types$name[[i]]='freq.Hz'
      col_types$size[[i]]=4
      col_types$type[[i]]='numeric'
    }
    else if(colID == 33) {
      col_types$name[[i]]='|Ewe|.V'
      col_types$size[[i]]=4
      col_types$type[[i]]='numeric'
    }
    else if(colID == 34) {
      col_types$name[[i]]='|I|.A'
      col_types$size[[i]]=4
      col_types$type[[i]]='numeric'
    }
    else if(colID == 35) {
      col_types$name[[i]]='Phase(Z).deg'
      col_types$size[[i]]=4
      col_types$type[[i]]='numeric'
    }
    else if(colID == 36) {
      col_types$name[[i]]='|Z|.Ohm'
      col_types$size[[i]]=4
      col_types$type[[i]]='numeric'
    }
    else if(colID == 37) {
      col_types$name[[i]]='Re(Z).Ohm'
      col_types$size[[i]]=4
      col_types$type[[i]]='numeric'
    }
    else if(colID == 38) {
      col_types$name[[i]]='-Im(Z).Ohm'
      col_types$size[[i]]=4
      col_types$type[[i]]='numeric'
    }
    else if(colID == 39) {
      col_types$name[[i]]='I.Range'
      col_types$size[[i]]=2
      col_types$type[[i]]='integer'
    }
    else if(colID == 70) {
      col_types$name[[i]]='P.W'
      col_types$size[[i]]=4
      col_types$type[[i]]='numeric'
    }
    else if(colID == 434) {
      col_types$name[[i]]='(Q-Qo).C'
      col_types$size[[i]]=4
      col_types$type[[i]]='numeric'
    }
    else if(colID == 435) {
      col_types$name[[i]]='dQ.C'
      col_types$size[[i]]=4
      col_types$type[[i]]='numeric'
    }
    else if(colID == 13) {
      col_types$name[[i]]='X.Q.Qo..mA.h'
      col_types$size[[i]]=8
      col_types$type[[i]]='numeric'
    }
    else if(colID == 467) {
      col_types$name[[i]]='Q.charge.discharge.mA.h'
      col_types$size[[i]]=8
      col_types$type[[i]]='numeric'
    }
    else if(colID == 468) {
      col_types$name[[i]]='half.cycle'
      col_types$size[[i]]=4
      col_types$type[[i]]='integer'
    }
    else if(colID == 126) {
      col_types$name[[i]]='Capacitance.discharge..b5.F'
      col_types$size[[i]]=8
      col_types$type[[i]]='numeric'
    }
    else if(colID == 125) {
      col_types$name[[i]]='Capacitance.charge..b5.F'
      col_types$size[[i]]=8
      col_types$type[[i]]='numeric'
    }
    else if(colID == 9) {
      col_types$name[[i]]='Ece.V'
      col_types$size[[i]]=4
      col_types$type[[i]]='numeric'
    }
    else if(colID == 16) {
      col_types$name[[i]]='Analog.IN.1.V'
      col_types$size[[i]]=4
      col_types$type[[i]]='numeric'
    }
    else
      stop(sprintf("mpr column type %s not implemented", colID),call.=F)
  }
  return(col_types)
}

#' Get additional attributes from a biologic mpt file
#'
#' This function finds the date and channel for a biologic dataset
#'
#' @param header_lines The header of a biologic mpt file
#' @return A list of attributes that were found
#' @keywords internal
.get_biologic_attributes <- function(header_text) {
  atts=list()
  try({
    date_line=suppressWarnings(grep('Acquisition started on',header_text))
    if(length(date_line))
      atts$date=as.Date(substr(header_text[[date_line]],26,44),format="%m/%d/%Y %T")

    channel_line=suppressWarnings(grep('Run on channel',header_text))
    if(length(channel_line))
      atts$channel=substr(header_text[[channel_line]],18,30)
  },silent=TRUE)
  atts
}
