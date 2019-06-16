load.addMissingColumns <- function(data) {

  # ===================== #
  #        CURRENT        #
  # ===================== #

  if (!"Current.A." %in% names(data)) {
    if ("p_w" %in% names(data)) {
      jms.classes::log.debug("Current not present in raw data, calculating from power and voltage")
      data$Current.A. <- data$p_w / data$Voltage.V.
      if ("dq_mah" %in% names(data)) {
        data$Current.A. <- data$Current.A. * sign(data$dq_mah)
      } else if ("q_charge_discharge_mah" %in% names(data)) {
        data$Current.A. <- data$Current.A. * sign(data$q_charge_discharge_mah)
      } else if ("ox_red" %in% names(data)) {
        # ox_red is not simply the sign of the current unfortunately...
        # It seems to be the sign of the control
        # (i.e. normally the current, but could be the sign of the held potential e.g. during a voltage hold)
        # (Possibly...)

        # That being said, if all else fails it should be a reasonable guess...
        data$Current.A.[!data$ox_red] <- data$Current.A.[!data$ox_red] * -1
        warning("Assuming data is exclusively under current control", call.=F)
      } else {
        warning("Unable to determine sign of current", call.=F)
      }
    } else if (all(c("dq_mah", "Test_Time.s.") %in% names(data))) {
      jms.classes::log.debug("Current not present in raw data, calculating from capacity and time")
      data$Current.A. <- data$dq_mah * 3.6 / c(1, diff(data$Test_Time.s.))
      data$Current.A.[[1]] <- 0
    } else if ("control_v_ma" %in% names(data)) {
      jms.classes::log.debug("Current not present in raw data, calculating from control")
      jms.classes::log.warn("Assuming current control")
      warning("Unable to determine current, assuming current control and using control data as the current", call.=FALSE)
      data$Current.A. <- data$control_v_ma / 1000
    } else {
      jms.classes::log.warn("Current not present in raw data, unable to calculate from other available data.")
      warning("Unable to determine current", call.=F)

      # Skip all remaining column checks if we don't know the current
      return(data)
    }
  }


  # rest_steps=data$Current.A.[abs(data$Current.A.)<1e-8] #0.01 uA
  discharge_steps <- data$Current.A. < (-1e-8)
  charge_steps <- data$Current.A. > 1e-8


  # ===================== #
  #       STEP INDEX      #
  # ===================== #

  if (!"Step_Index" %in% names(data)) {
    jms.classes::log.debug('Data was missing the "Step_Index" column, using 1 for rest steps, 2 for discharge steps and 3 for charge steps.')
    data$Step_Index <- 1
    data$Step_Index[discharge_steps] <- 2
    data$Step_Index[charge_steps] <- 3
  }

  # ===================== #
  #       Ns_changes      #
  # ===================== #


  if (!"Ns_changes" %in% names(data)) {
    jms.classes::log.debug('Data was missing the "Ns_changes" column, adding it.')
    data$Ns_changes <- c(F, diff(data$Step_Index) != 0)
  }

  # ===================== #
  #      CYCLE INDEX      #
  # ===================== #

  if (!"Cycle_Index" %in% names(data)) {
    jms.classes::log.debug('Data was missing the "Cycle_Index" column, adding it.')
    if ("counter_inc" %in% names(data)) {
      cycle_changes <- which(abs(diff(data$counter_inc)) == 1) + 1
    } else {
      dis_points <- which(discharge_steps)
      ch_points <- which(charge_steps)
      if (!length(step2)) {
        # No Discharge data
        dis_points <- Inf
      }
      if (!length(step3)) {
        # No Charge data
        ch_points <- Inf
      }
      # charge or discharge first??
      if (dis_points[[1]] > ch_points[[1]]) {
        # charge first
        cycle_changes <- data$Ns_changes & charge_steps
      } else {
        # discharge first
        cycle_changes <- data$Ns_changes & discharge_steps
      }
      cycle_changes <- which(cycle_changes)
      # TODO: calculate and store biologic counter_inc ??
    }

    cycle_changes <- c(1, cycle_changes, nrow(data) + 1)
    data$Cycle_Index <- rep(1:(length(cycle_changes) - 1), times=diff(cycle_changes))
  }

  # ===================== #
  #        CAPACITY       #
  # ===================== #

  if (!"dq_mah" %in% names(data)) {
    if ("q_charge_discharge_mah" %in% names(data)) {
      jms.classes::log.debug("dq_mah not present in raw data, calculating from q_charge_discharge_mah")
      data$dq_mah <- c(0, diff(data$q_charge_discharge_mah))
    } else if ("capacity.mah" %in% names(data)) {
      jms.classes::log.debug("dq_mah not present in raw data, calculating from capacity.mah")
      data$dq_mah <- c(0, diff(data$capacity.mah))
    }
  }

  if (!"Capacity.Ah." %in% names(data)) {
    if ("dq_mah" %in% names(data)) {
      jms.classes::log.debug("Capacity.Ah. not present in raw data, calculating from dq_mah and Ns_changes")
      changes <- c(0, which(abs(diff(data$Ns_changes)) == 1), nrow(data))
      capacity <- c()
      for (i in 1:(length(changes) - 1)) {
        capacity <- append(capacity, abs(cumsum(data$dq_mah[(changes[[i]] + 1):changes[[i + 1]]])))
      }
      data$Capacity.Ah. <- capacity / 1000
    } else {
      jms.classes::log.debug("Capacity.Ah. not present in raw data, calculating from Current.A. and Test_Time.s.")
      cap <- abs(data$Current.A.) * c(0, diff(data$Test_Time.s.)) * 1 / 3600
      cap[data$Ns_changes] <- 0

      # Continued...
    }
  }

  if (!"Step_Time.s." %in% names(data)) {
    jms.classes::log.debug("Step_Time.s. not present in raw data, adding it")
    Step_Time.s. <- data$Test_Time.s.
    # Continued...
  }

  # Continued --> split per step
  if (!all(c("Capacity.Ah.", "Step_Time.s.") %in% names(data))) {
    pst <- 0
    ncn <- c(1, which(data$Ns_changes), nrow(data) + 1)
    for (i in 1:(length(ncn) - 1)) {
      if (!"Capacity.Ah." %in% names(data)) {
        cap[ncn[[i]]:(ncn[[i + 1]] - 1)] <- cumsum(cap[ncn[[i]]:(ncn[[i + 1]] - 1)])
      }
      if (!"Step_Time.s." %in% names(data)) {
        pst <- Step_Time.s.[[ncn[[i]]]]
        Step_Time.s.[ncn[[i]]:(ncn[[i + 1]] - 1)] <- Step_Time.s.[ncn[[i]]:(ncn[[i + 1]] - 1)] - pst
      }
    }
  }

  if (!"Capacity.Ah." %in% names(data)) {
    data$Capacity.Ah. <- cap
  }

  if (!"Step_Time.s." %in% names(data)) {
    data$Step_Time.s. <- Step_Time.s.
  }

  if (!"Discharge_Capacity.Ah." %in% names(data)) {
    jms.classes::log.debug('Data was missing the "Discharge_Capacity.Ah." and / or "Charge_Capacity.Ah." columns, adding them.')
    data$Discharge_Capacity.Ah. <- 0
    data$Charge_Capacity.Ah. <- 0
    data$Discharge_Capacity.Ah.[discharge_steps] <- data$Capacity.Ah.[discharge_steps]
    data$Charge_Capacity.Ah.[charge_steps] <- data$Capacity.Ah.[charge_steps]
  }

  return(data)
}

#' Implementation of switch function in R
#'
#' This function is used internally to ease the importing of biologic data from different versions of ec-lab.
#' @param EXPR The vector to apply the changes to
#' @param ... The changes to apply
#' @return A vector with the changes applied
#' @examples
#' vswitch(input, changes1, change2, ...)
#' @keywords internal
vswitch <- function(EXPR, ...) {
  vars <- cbind(...)
  vars[cbind(seq_along(EXPR), match(EXPR, names(list(...))))]
}
