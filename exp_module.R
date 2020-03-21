exp.module <- function(dat, at) {
  
  # Attributes---------------------------------------------
  active    <- dat$attr$active
  status    <- dat$attr$status
  exp.time  <- dat$attr$exp.time
  
  # rates---------------------------------------------
  exp_rate      <- params$exp_rate
  # inf_rate      <- dat$attr$inf.rate

  #########################################
  # E to I progression 
  nExp <- 0 
  idsEligExp <- activeExpIdx(dat)
  nEligExp <- length(idsEligExp)
  browser()
  if (nEligExp > 0) {
    #as an Option implementation  of draw from normal distribution like with recovery
    expDur <- ((at - exp.time[idsEligExp]))
    
    exp_room <- seq(1,20,1)
    nor_dens_exp <- dnorm(exp_room,5,1.25)
    expProb <- nor_dens_exp[expDur]
    
    vecExp <- nEligExp[which(rbinom(nEligExp, 1, exp_rate) == 1)]
    if (length(vecExp)) {
      idsExp <- idsEligExp[vecExp]
      nExp <- length(idsExp)
      status[idsExp] <- "i"
    }
  }
  
  if (at == 2) {
    dat$epi$ei.flow     <- c(0, nExp)
    dat$epi$e.num       <- c(0, length(activeExpIdx(dat)))
    dat$epi$i.num       <- c(0, length(activeInfIdx(dat)))
  } else {
    dat$epi$ei.flow[at]     <- nExp 
    dat$epi$e.num[at]       <- length(activeExpIdx(dat))
    dat$epi$i.num[at]       <- length(activeInfIdx(dat))
  }
  
  # Write out updated status attribute----------------------------------------------
  dat$attr$status <- status
  
  return(dat)
}