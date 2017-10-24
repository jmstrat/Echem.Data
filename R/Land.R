#' Read Echem Data for land (lahne) Cycler
#'
#' This function is used to import echem data from a land txt file.
#' @param file Path to data file
#' @return A data frame containing the echem data
#' @examples
#' load.land("/path/to/file.txt")
#' @keywords internal
load.land <-function(file)
{
  data=read.table(file,sep="\t",header=TRUE)
  names(data) <- c("Data_Point","Test_Time.s.","Step_Time.s.","Voltage.V.","Current.A.","Capacity.Ah.","Energy.Wh.","Date_Time")
  data$Current.A.=data$Current.A./1000
  data$Capacity.Ah.=data$Capacity.Ah./1000
  data$Energy.Wh.=data$Energy.Wh./1000
  data$Step_Index = unlist(lapply(data$Current.A.,function(row) {if(row==0) return(1) else if(row<0) return(2) else return(3)}))
  ci=1
  si=1

  step2=which(data$Step_Index==2)
  step3=which(data$Step_Index==3)
  if(!length(step2)) {
    #No Discharge data
    step2=Inf
  }
  if(!length(step3)) {
    #no Charge data
    step3=Inf
  }

  #charge or discharge first??
  if(step2[[1]]>step3[[1]]) {
    #charge first
    data$Cycle_Index= unlist(lapply(data$Step_Index,function(row) {if((row==1||row==3) && si==2) {ci<<-ci+1} ; si<<-row; return(ci)}))
  } else {
    #discharge first
    data$Cycle_Index= unlist(lapply(data$Step_Index,function(row) {if((row==1||row==2) && si==3) {ci<<-ci+1} ; si<<-row; return(ci)}))
  }
  data$Discharge_Capacity.Ah.= unlist(mapply(function(s,c) {if(s==2) return(c) else return(0)},data$Step_Index,data$Capacity.Ah.))
  data$Charge_Capacity.Ah.= unlist(mapply(function(s,c) {if(s==3) return(c) else return(0)},data$Step_Index,data$Capacity.Ah.))
  data$Discharge_Energy.Wh.= unlist(mapply(function(s,c) {if(s==2) return(c) else return(0)},data$Step_Index,data$Energy.Wh.))
  data$Charge_Energy.Wh.= unlist(mapply(function(s,c) {if(s==3) return(c) else return(0)},data$Step_Index,data$Energy.Wh.))
  return(data)
}
