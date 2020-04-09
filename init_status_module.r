#Initialize status#
#In this Module statuses are initialzied and especially infection, recovery and vaccination times#
#are setup so that individuals who start out as infected/recovered/vaccinated still get a value #
#infection-, recovery- or vaccination time #

init_status.module <- function(dat) {
  
  #small helper functions
  # find out what values are not in a certain subset
  '%!in%' <- function(sub_set,orig_set)!('%in%'(sub_set,orig_set))

  # Variables ---------------------------------------------------------------
  tea.status    <- dat$control$tea.status
  e.num         <- dat$init$e.num
  i.num         <- dat$init$i.num
  sc.num        <- dat$init$sc.num
  r.num         <- dat$init$r.num

  status.vector <- dat$init$status.vector
  num           <- params$initial_pop
  # mode          <- rep(1, num)

  type          <- dat$control$type

  # Age ------------------------------------------------------------------
    dat <- aging.init(dat)

  # Status ------------------------------------------------------------------
  
  ## Status passed on input network
      start_ids <- c(1:params$initial_pop)
      status <- rep("s", num)
      status[sample(start_ids, size = i.num)] <- "i"
      inf <- which(status == "i")
      status[sample(start_ids[-c(inf)], size = r.num)] <- "r"
      rec <- which(status == "r")
      status[sample(start_ids[-c(inf, rec)], size = e.num)] <- "e"
      exp <- which(status == "e")
      status[sample(start_ids[-c(inf, rec, exp)], size = sc.num)] <- "sc"
      sc <- which(status == "sc")

  dat$attr$status <- status

 # init age of population
  # init statistics
  dat$epi$meanAge <- rep(mean(dat$attr$age, na.rm = TRUE), 2)

  ## Save out other attr
  dat$attr$active         <- rep(1, length(status))
  dat$attr$entrTime       <- rep(1, length(status))
  dat$attr$exitTime       <- rep(NA, length(status))
  dat$attr$recTime        <- rep(NA, length(status))
  dat$attr$expTime        <- rep(NA, length(status))
  dat$attr$screened       <- rep(0, length(status))
  dat$attr$vac            <- rep(0, length(status))
  dat$attr$isolated       <- rep(0, length(status))
  dat$attr$gender         <- sample(c(0,1), size = length(status), 0.5)
  dat$attr$sick           <- rep(0, length(status))
  dat$attr$severe         <- rep(0, length(status))
  dat$attr$ventilation    <- rep(0, length(status))
  dat$attr$hospitalized   <- rep(0, length(status))
  dat$attr$sevTime        <- rep(NA, length(status))
  dat$attr$icuTime        <- rep(NA, length(status))
  dat$attr$cfrTime        <- rep(NA, length(status))
  dat$attr$icuRecTime     <- rep(NA, length(status))

  if (tea.status == TRUE) {
    dat$nw <- activate.vertex.attribute(dat$nw,
                                        prefix = "testatus",
                                        value = status,
                                        onset = 1,
                                        terminus = Inf)
  }
  
  # exposed Time ----------------------------------------------------------
  ## Set up can.time vector
  idsExp <- which(dat$attr$status == "e")
  expTime <- rep(NA, length(status))
  
  if (!is.null(dat$init$expTime.vector)) {
    expTime <- dat$init$expTime.vector
  } else {
    # If vital dynamics, expTime is a geometric draw over the duration of infection
    if (params$de.rate > 0) {
      expTime[idsExp] <- -rgeom(n = length(idsExp),
                                prob = params$de.rate +
                                  (1 - params$de.rate)*mean(params$exp_rate_init)) + 2
    } 
  }
  
  dat$attr$expTime <- expTime
  
  # Infection Time ----------------------------------------------------------
  ## Set up inf.time vector
  idsInf <- which(status == "i")
  infTime <- rep(NA, length(status))

  # Set the starting values for progression
  #hospitalized (severely ill)
    hosp_data <- list(c(0.016, 0.025), c(0.143, 0.208), c(0.212, 0.283), c(0.205, 0.301), 
                    c(0.286, 0.435), c(0.305, 0.587), c(0.313, 0.703))
    hosp_data_mean <- lapply(hosp_data, mean)
    hosp_rate <- unlist(hosp_data_mean)
  
    #admitted to ICU
    ICU_data <- list(c(0), c(0.02, 0.042), c(0.054, 0.104), c(0.047, 0.112), c(0.081, 0.188), 
                   c(0.105, 0.31), c(0.063, 0.29))
    ICU_data_mean <- lapply(ICU_data, mean)
    ICU_rate <- unlist(ICU_data_mean)
  
    # alternative fatalities for the CDC - source
    CFR_data <- list(c(0), c(0.001, 0.002), c(0.005, 0.008), c(0.014, 0.026), c(0.027, 0.049), 
                   c(0.043, 0.105), c(0.104, 0.273))
    CFR_data_mean <- lapply(CFR_data, mean)
    CFR_rate_alt <- unlist(CFR_data_mean) 
    
    if (params$i_num_init) {
      idsProg <- which(dat$attr$status == "i")
      
      #age groups  
      age_groups <- cut(dat$attr$age, breaks = c(0, 20, 45, 55, 65, 75, 85, 100), 
                        ordered_result = FALSE, labels = FALSE, include.lowest = TRUE, right = FALSE)
      
      age_groups_sev <- age_groups[idsProg]
      hosp_rate_pp <- hosp_rate[age_groups_sev]
      
      #evaluate which of the infected Individuals will get severely ill or ICUed and at which time
      vecSev <- idsProg[which(rbinom(length(idsProg), 1, hosp_rate_pp) == 1)]
      nSev <- length(vecSev)
      
      if (nSev > 0) {
        #assign time of severity/hospitalisation Times are taken from Zhou et al.
        sev_time <- draw_from_lognorm(5, 1, length(vecSev))
        
        dat$attr$sevTime[vecSev] <- sev_time
        
        
        # calculate conditional probabilities for ICU admission wenn severely sick
        icu_rate_new <- ICU_rate/hosp_rate
        
        age_groups_icu <- age_groups[vecSev]
        icu_rate_new_pp <- icu_rate_new[age_groups_icu]
        
        vecIcu <- vecSev[which(rbinom(nSev, 1, icu_rate_new_pp) == 1)]
        nIcu <- length(vecIcu)
        
        if (nIcu > 0) {
          #assign Time of ICU 
          icu_time <- draw_from_lognorm(12, 1, length(vecIcu))
          
          dat$attr$icuTime[vecIcu] <- icu_time
          
          #covid related death
          cfr_rate_new <- CFR_rate_alt/icu_rate_new
          cfr_rate_new[which(is.nan(cfr_rate_new))] <- 0
          
          age_groups_cfr <- age_groups[vecIcu]
          cfr_rate_new_pp <- cfr_rate_new[age_groups_cfr]
          
          vecCfr <- vecIcu[which(rbinom(nIcu, 1, cfr_rate_new_pp) == 1)]
          nCfr <- length(vecCfr)
          
          if (nCfr > 0) {
            #assign Time of ICU 
            cfr_time <- draw_from_lognorm(21, 1, length(vecCfr))
            
            dat$attr$cfrTime[vecCfr] <- cfr_time
          }
        }
      }
    }

  if (!is.null(dat$init$infTime.vector)) {
    infTime <- dat$init$infTime.vector
  } else {
    # If vital dynamics, infTime is a geometric draw over the duration of infection
    if (params$di.rate > 0) {
        infTime[idsInf] <- -rgeom(n = length(idsInf),
                                  prob = params$di.rate +
                                    (1 - params$di.rate)*mean(params$rec.rate)) + 2
    } 
  }

  dat$attr$infTime <- infTime

  # Recovery Time ----------------------------------------------------------
  ## Set up rec.time vector
  idsRec <- which(status == "r")
  recTime <- rep(NA, length(status))

  if (!is.null(dat$init$recTime.vector)) {
    recTime <- dat$init$recTime.vector
  } else {
    # If vital dynamics, infTime is a geometric draw over the duration of infection
    if (params$dr.rate > 0) {
        recTime[idsRec] <- -rgeom(n = length(idsRec),
                                  prob = params$dr.rate +
                                    (1 - params$dr.rate)*mean(params$recSus.rate)) + 2
    } 
  }

  dat$attr$recTime <- recTime

  # vaccination Time ----------------------------------------------------------
  ## Set up vac.time vector
  idsInitVac <- which(dat$attr$vaccinated != 2)
  vacTime <- rep(NA, length(status))

  if (!is.null(dat$init$vacTime.vector)) {
    vacTime <- dat$init$vacTime.vector
  } else {
    # If vital dynamics, vacTime is a geometric draw over the duration of vaccination

    if (params$dv.rate > 0) {
        vacTime[idsInitVac] <- -rgeom(n = length(idsInitVac),
                                  prob = params$dv.rate +
                                    (1 - params$dv.rate)*mean(params$vac_rate_init)) + 2
    } 
  }

  dat$attr$vacTime <- vacTime

  return(dat)
}