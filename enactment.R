enactment.module <- function(dat, at) {
  # if (at > 25) {browser()}
  # Attributes---------------------------------------------
  active    <- dat$attr$active
  status    <- dat$attr$status
  inf.time  <- dat$attr$infTime
  sevTime   <- dat$attr$sevTime
  icuTime   <- dat$attr$icuTime
  cfrTime   <- dat$attr$cfrTime
  recTime   <- dat$attr$recTime
  # browser()
  #########################################
  # Progression to severity 

  idsEligSev <- intersect(activeInfIdx(dat), which(sevTime == at))
  nEligSev <- length(idsEligSev)
  
  dat$attr$status[idsEligSev] <- "sc"
  
  # Progression to icu 
  idsEligIcu <- intersect(activeIdx(dat), which(icuTime == at))
  nEligIcu <- length(idsEligIcu)
  
  # death from covid 19
  idsEligCfr <- intersect(activeIdx(dat), which(cfrTime == at))
  nEligCfr <- length(idsEligCfr)
  
  # Recovery
  dat$attr$icuTime[which(dat$attr$icuRecTime == at)] <- 0
  
  idsEligRec <- intersect(activeIdx(dat), which(recTime == at))
  if (length(idsEligRec) > 0) {
    vecRecRec <- idsEligRec[which(rbinom(length(idsEligRec), 1, params$immun_ratio) == 1)]
    vecRecSus <- idsEligRec[idsEligRec %!in% vecRecRec]
    
    dat$attr$status[vecRecRec] <- "r"
    dat$attr$infTime[vecRecRec] <- NA
    dat$attr$recTime[vecRecRec] <- at
    nRecRec   <- length(vecRecRec)
    
    dat$attr$status[vecRecSus] <- "s"
    dat$attr$infTime[vecRecSus] <- NA
    dat$attr$recTime[vecRecSus] <- NA
    nRecSus   <- length(vecRecSus)
  }else{
    nRecSus <- 0 
    nRecRec <- 0
  }
  
  # Write out updated status attribute----------------------------------------------
  dat$attr$severe[idsEligSev] <- 1
  dat$attr$icu[idsEligIcu] <- 1
  
  dat <- setInactive(dat, at, idsEligCfr)
  dat$attr$infTime[idsEligCfr] <- NA
  dat$attr$sevTime[idsEligCfr] <- NA
  dat$attr$icuTime[idsEligCfr] <- NA

  if (at == 2) {
    dat$epi$sev.flow               <- c(0, nEligSev)
    dat$epi$sev.num                <- c(params$sc_num_init, nEligSev)
    dat$epi$icu.num                <- c(0, nEligIcu)
    dat$epi$cov.death.flow         <- c(0, nEligCfr)
    dat$epi$cov.death.num          <- c(0, nEligCfr)
    dat$epi$recSus.flow            <- c(0, nRecSus)
    dat$epi$recRec.flow            <- c(0, nRecRec)
    dat$epi$r.num                  <- c(params$r_num_init, length(activeRecIdx(dat)))
  } else {
    dat$epi$sev.flow[at]            <- nEligSev
    dat$epi$sev.num[at]             <- length(activeSevIdx(dat))
    dat$epi$icu.num[at]             <- length(activeIcuIdx(dat))
    dat$epi$cov.death.flow[at]      <- nEligCfr
    dat$epi$cov.death.num[at]       <- sum(dat$epi$cov.death.num[at - 1], nEligCfr)
    dat$epi$recSus.flow[at]         <- nRecSus
    dat$epi$recRec.flow[at]         <- nRecRec
    dat$epi$r.num[at]               <- length(activeRecIdx(dat))
  }
  return(dat)
}