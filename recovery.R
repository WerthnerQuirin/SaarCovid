#Recovery module#
#This new recoverymodule should facilitatete possibility of recovered people becoming susceptible 
#after a certain amount of time as well as a normal dropback of infected people into the susceptible 
#compartment Therefore the new model will look like a variation of the recovereymodules of a SIS 
#and a SIR module, combining both of their functionalitys in a new R Compartment#

recovery.init <- function(dat, at)
{
  # init age of population
  #n <- countActive(dat)
  dat$attr$recTime <- rep(NA, length(dat$attr$status))

  for (i in 1:length(dat$attr$status))
  if (dat$attr$status[i] == "r") {
      dat$attr$recTime[i] <- at
  }
  
  return(dat)  
}

recovery.module <- function(dat,at) {
  if (isInit(at)) {
    dat <- recovery.init(dat,at)
  }
 # if (at > 2) {browser()}
  # Variables ---------------------------------------------------------------
  active    <- dat$attr$active
  status    <- dat$attr$status
  infTime   <- dat$attr$infTime
  recTime   <- dat$attr$recTime
  cfrTime   <- dat$attr$cfrTime
  sevTime   <- dat$attr$sevTime
  icuTime   <- dat$attr$icuTime
  age       <- dat$attr$age

  modes     <- params$modes
  mode      <- idmode(dat$nw)

  type      <- dat$control$type

  rec_van_rate        <- params$rec_van_rate
  immun_ratio         <- params$immun_ratio
  
  rec.rand            <- dat$control$rec.rand
  rec.rate            <- params$rec_prob
  
  # recSus.rate         <- params$recSus.rate           #rates for vaning of imunity and subsequent 
  # #fall back in the sus-Compartment
  

  recInfSus.rate      <- params$recInfSus.rate
  
  idsEligRec <- Reduce(intersect, list(which(active == 1), which(status == 'i'), 
                                       which(is.na(sevTime))))
  
  
  ################################################################################################
  # Recovered people getting suseptible again depending on the duration of their time in the 
  # Recovered Compartiment
  idsEligSusRec <- Reduce(intersect, list(which(active == 1), which(status == 'r')))
    
  if (length(idsEligSusRec) > 0) {
    recProt <- ((at - recTime[idsEligSusRec])*rec_van_rate)
    vecSusRec <- idsEligSusRec[which(rbinom(length(idsEligSusRec), 1, recProt) == 1)]
    if (length(vecSusRec) > 0) {
      dat$attr$status[vecSusRec] <- "s"
      dat$attr$recTime[vecSusRec] <- NA
      nSusRec   <- length(vecSusRec)
    }else{
      nSusRec <- 0
    }
    }else{
      nSusRec <- 0
    }

  # Infected people recovering again depending on the duration of their time in the 
  # infected Compartiment

  if (length(idsEligRec) > 0) {
    infDur <- ((at - infTime[idsEligRec]))
    
    rec_room <- seq(1,400,1)
    nor_dens <- dnorm(rec_room,14,5)
    recProb <- nor_dens[infDur]
    
    recProb_2 <- pnorm(c(1:40), mean = 20, sd = 7)
    recProb_3 <- 0.07 # stay in hospital approximately 20 days, dummmy value
  
    vecRec <- idsEligRec[which(rbinom(length(idsEligRec), 1, recProb_3) == 1)]
    
    NormRec <- recover_this(dat, at, vecRec, params$immun_ratio)
    #nRecRec <- 0
    
  }
  # Recovery for severely ill patients
  idsEligRecSev <- Reduce(intersect, list(which(active == 1), which(status == 'i'), 
                                       which(!is.na(sevTime)), which(is.na(icuTime)), 
                                       which(sevTime <= at)))
  
  if (length(idsEligRecSev) > 0) {
    vecRecSev <- idsEligRecSev[which(rbinom(length(idsEligRecSev), 1, 0.091) == 1)]
  
    SevRec <- recover_this(dat, at, vecRecSev, params$immun_ratio)
    
  }
  # # Recovery for icu patients
   idsEligRecIcu <- Reduce(intersect, list(which(active == 1), which(status == 'i'), 
                                           which(!is.na(sevTime)), which(!is.na(icuTime)), 
                                           which(is.na(cfrTime)), which(icuTime <= at)))
  
  if (length(idsEligRecIcu) > 0) {
    vecRecIcu <- idsEligRecIcu[which(rbinom(length(idsEligRecIcu), 1, 0.091) == 1)]

   icuRec <- recover_this(dat, at, vecRecIcu, params$immun_ratio)
  }


    
  
  
  ################################################################################################
 #   if (at == 20 ) { browser}

  if ("status" %in% dat$temp$fterms) {
    dat$nw <- set.vertex.attribute(dat$nw, "status", dat$attr$status)
  }
  return(dat)
  # Output ------------------------------------------------------------------
  if (at == 2) {
    dat$epi$rs.flow         <- c(0, NormRec$nSusRec)
    dat$epi$ir.flow         <- c(0, NormRec$nRecRec)
    dat$epi$is.flow         <- c(0, NormRec$nRecSus)
  } else {
    dat$epi$rs.flow[at]     <- NormRec$nSusRec
    dat$epi$ir.flow[at]     <- NormRec$nRecRec
    dat$epi$is.flow[at]     <- NormRec$nRecSus
  }

  return(dat)
}

