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
  # Variables ---------------------------------------------------------------
  active    <- dat$attr$active
  status    <- dat$attr$status
  infTime   <- dat$attr$infTime
  recTime   <- dat$attr$recTime
  age       <- dat$attr$age

  modes     <- params$modes
  mode      <- idmode(dat$nw)

  type      <- dat$control$type

  rec_van_rate        <- params$rec_van_rate
  immun_ratio         <- params$immun_ratio
  
  rec.rand            <- dat$control$rec.rand
  rec.rate            <- params$rec_prob
  
  recSus.rate         <- params$recSus.rate           #rates for vaning of imunity and subsequent 
  #fall back in the sus-Compartment
  

  recInfSus.rate      <- params$recInfSus.rate
  
  idsEligRec <- Reduce(intersect, list(which(active == 1), which(status == 'i')))
  
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
    
    rec_room <- seq(1,100,1)
    nor_dens <- dnorm(rec_room,14,5)
    recProb <- nor_dens[infDur]
  
    vecRec <- idsEligRec[which(rbinom(length(idsEligRec), 1, recProb) == 1)]
    vecRecRec <- vecRec[which(rbinom(length(vecRec), 1, immun_ratio) == 1)]
    vecRecSus <- vecRec[vecRec %!in% vecRecRec]
    
    if (length(vecRecRec) > 0) {
      dat$attr$status[vecRecRec] <- "r"
      dat$attr$infTime[vecRecRec] <- NA
      dat$attr$recTime[vecRecRec] <- at
      nRecRec   <- length(vecRecRec)
    }else{
      nRecRec <- 0
    }
    
   
    if (length(vecRecSus) > 0) {
      dat$attr$status[vecRecSus] <- "s"
      dat$attr$infTime[vecRecSus] <- NA
      dat$attr$recTime[vecRecSus] <- NA
      nRecSus   <- length(vecRecSus)
    }else{
      nRecSus <- 0
    }
  }else{
    nRecRec <- 0
    nRecSus <- 0
  }
    
  
  ################################################################################################
 #   if (at == 20 ) { browser}

  if ("status" %in% dat$temp$fterms) {
    dat$nw <- set.vertex.attribute(dat$nw, "status", dat$attr$status)
  }
  
  # Output ------------------------------------------------------------------
  if (at == 2) {
    dat$epi$rs.flow         <- c(0, nSusRec)
    dat$epi$ir.flow         <- c(0, nRecRec)
    dat$epi$is.flow         <- c(0, nRecSus)

    dat$epi$r.num           <- c(0, params$r_num_init) 
    dat$epi$i.num           <- c(0, params$i_num_init) 
    dat$epi$s.num           <- c(0, susIdx(dat)) 
  } else {
    dat$epi$rs.flow[at]     <- nSusRec
    dat$epi$ir.flow[at]     <- nRecRec
    dat$epi$is.flow[at]     <- nRecSus
    
    dat$epi$r.num[at]       <- length(activeRecIdx)
    dat$epi$i.num[at]       <- length(activeInfIdx)
    dat$epi$s.num[at]       <- length(activeSusIdx)
  }

  return(dat)
}

