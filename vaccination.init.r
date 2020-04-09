#Vaccination Module#
vaccination.init <- function(dat){
    #if (isInit(at)){
  # init Vaccination status of population
  vac_rate_init <- params$vac_rate_init
        sus <- susIdx(dat)            #Vaccinations are available in Germany since 2007; Hence people up to approximately 30 have the Chance of beeing Vaccinated
        vecVacInit <- sus[which(rbinom(length(sus), 1, vac_rate_init) == 1)]
            if (length(vecVacInit) > 0) {
                nVacInit <- length(vecVacInit)
                dat$attr$vac[vecVacInit]   <- 1
  }
    nVac_init        <- length(vacIdx(dat))
  # init statistics
    dat$epi$v.num        <- c(0, nVac_init)

  return(dat)  
}

#Vaccination Vanning after 10 years#
vaccination.vanning <- function(dat, at) {
  active          <- dat$attr$active
  status          <- dat$attr$status
  vacTime         <- dat$attr$vacTime
  age             <- dat$attr$age
  vaccinated      <- dat$attr$vac

  modes           <- dat$params$modes

  vacVan_rate      <- params$vacVan_rate

  nVecVan         <- 0
  idsEligVacVan   <- which(active == 1 & vaccinated != 0)
  nEligVacVan     <- length(idsEligVacVan)

# Time-Varying Vaccination Vanning Rate ----------------------------------------------
  vacDur <- at - vacTime[active == 1 & vaccinated != 0]
  vacDur[vacDur == 0] <- 1
  lvacVan.rate <- length(vacVan.rate)
  if (lvacVan.rate == 1) {
    mEligVacVan <- mode[idsEligVacVan]
    rates <- c(vacVan.rate, vacVan.rate.m2)
    ratesElig <- rates[mEligVacVan]
  } else {
    mEligVacVan <- mode[idsEligVacVan]
    if (is.null(vacVan.rate.m2)) {
      rates <- ifelse(vacDur <= lvacVan.rate, vacVan.rate[vacDur], vacVan.rate[lvacVan.rate])
    } else {
      rates <- rep(NA, length(vacDur))
      rates[mEligVacVan == 1] <- ifelse(vacDur[mEligVacVan == 1] <= lvacVan.rate,
                                  vacVan.rate[vacDur[mEligVacVan == 1]], vacVan.rate[lvacVan.rate])
      rates[mEligVacVan == 2] <- ifelse(vacDur[mEligVacVan == 2] <= length(vacVan.rate.m2),
                                  vacVan.rate.m2[vacDur[mEligVacVan == 2]], vacVan.rate.m2[length(vacVan.rate.m2)])
    }
    ratesEligVacVan <- rates
  }

  #Process for Regained susceptability from the recovered compartment -----------------------------------------------------------------
    if (nEligVacVan > 0) {

    vecVacVan <- nEligVacVan[which(rbinom(length(nEligVacVan), 1, ratesEligVacVan) == 1)]
    if (length(vecVacVan) > 0) {
      idsVacVan <- idsVacVan[vecVacVan]
      nVacVan <- sum(idsVacVan)
      status[idsVacVan] <- "s"
    }
  }

  if (isInit(at)){

  }
}