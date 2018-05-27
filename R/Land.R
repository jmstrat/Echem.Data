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
 stop('Cannot currently read binary data for land cyclers.')
}

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
      for(n in names(atts)) {
        attr(dat,n)<-atts[n][[1]]
      }
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
