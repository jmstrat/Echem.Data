load.convert_T_C_V <- function(data) {
  #rest_steps=data$Current.A.[abs(data$Current.A.)<1e-8] #0.01 uA
  discharge_steps=data$Current.A.<(-1e-8)
  charge_steps=data$Current.A.>1e-8

  if(!'Step_Index'%in% names(data)) {
    data$Step_Index<-0
    data$Step_Index[discharge_steps]<-2
    data$Step_Index[charge_steps]<-3
  }

  step2=which(discharge_steps)
  step3=which(charge_steps)
  Ns_changes=c(F,diff(data$Step_Index)!=0)

  if(!'Ns_changes'%in% names(data)) {
    data$Ns_changes=Ns_changes
  }

  if(!'Cycle_Index'%in% names(data)) {
    if(!length(step2)) {
      #No Discharge data
      step2=Inf
    }
    if(!length(step3)) {
      #No Charge data
      step3=Inf
    }
    #charge or discharge first??
    if(step2[[1]]>step3[[1]]) {
      #charge first
      cycle_changes=Ns_changes&data$Step_Index==3
    } else {
      #discharge first
      cycle_changes=Ns_changes&data$Step_Index==2
    }

    cycle_changes=c(1,which(cycle_changes),nrow(data)+1)
    cycles=c()
    ci=1
    for(i in 1:(length(cycle_changes)-1)) {
      cycles=append(cycles,rep.int(ci,cycle_changes[[i+1]]-cycle_changes[[i]]))
      ci=ci+1
    }

    data$Cycle_Index=cycles
  }

  if(!'Capacity.Ah.'%in% names(data)) {
    cap<-abs(data$Current.A.)*c(0,diff(data$Test_Time.s.))*1/3600
    ncn=c(1,which(Ns_changes),nrow(data)+1)
    cap[Ns_changes]<-0
  }
  if(!'Step_Time.s.'%in% names(data)) {
    Step_Time.s.=data$Test_Time.s.
  }

  if(!any(c('Capacity.Ah.','Step_Time.s.')%in% names(data))) {
    pst=0
    for(i in 1:(length(ncn)-1)) {
      if(!'Capacity.Ah.'%in% names(data))
        cap[ncn[[i]]:(ncn[[i+1]]-1)]<-cumsum(cap[ncn[[i]]:(ncn[[i+1]]-1)])
      if(!'Step_Time.s.'%in% names(data)) {
        pst<-Step_Time.s.[[ncn[[i]]]]
        Step_Time.s.[ncn[[i]]:(ncn[[i+1]]-1)]<-Step_Time.s.[ncn[[i]]:(ncn[[i+1]]-1)]-pst
      }
    }
  }
  if(!'Capacity.Ah.'%in% names(data))
    data$Capacity.Ah.<-cap
  if(!'Discharge_Capacity.Ah.'%in% names(data)) {
    data$Discharge_Capacity.Ah.<-0
    data$Charge_Capacity.Ah.<-0
    data$Discharge_Capacity.Ah.[discharge_steps]<-data$Capacity.Ah.[discharge_steps]
    data$Charge_Capacity.Ah.[charge_steps]<-data$Capacity.Ah.[charge_steps]
  }
  if(!'Step_Time.s.'%in% names(data))
    data$Step_Time.s.<-Step_Time.s.

  return(data)
}

#' Implementation of switch function in R
#'
#' This function is used internally to ease the importing of biologic data from different versions of ec-lab.
#' @param EXPR The vector to apply the changes to
#' @param ... The changes to apply
#' @return A vector with the changes applied
#' @examples
#' vswitch(input, changes ...)
#' @keywords internal
vswitch <- function(EXPR, ...) {
  vars <- cbind(...)
  vars[cbind(seq_along(EXPR), match(EXPR, names(list(...))))]
}