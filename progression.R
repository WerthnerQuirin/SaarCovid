progression.module <- function(dat, at) {
  # if (at > 20) {browser()}
  # Attributes---------------------------------------------
  active    <- dat$attr$active
  status    <- dat$attr$status
  expTime   <- dat$attr$expTime
  infTime   <- dat$attr$infTime
  sevTime   <- dat$attr$sevTime
  icuTime   <- dat$attr$icuTime
  

  # rates---------------------------------------------
   exp_rate      <- params$exp_rate
  
  # Severe Outcomes Among Patients with Coronavirus Disease 2019 (COVID-19) - United States, February 12-March 16, 2020. MMWR Morb Mortal Wkly Rep. ePub: 18 March 2020 -> ICU and hosp rates per age group
  # 0-19, 20-44, 45-54, 55-64, 65-74, 75-84, 85-100
  
  #Durations are taken from Zhou et al (Lancet)
  
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

  #########################################
  # E to I progression 
  nExp <- 0 
  idsEligExp <- activeExpIdx(dat) # [which())]
  nEligExp <- length(idsEligExp)

  if (nEligExp > 0) {
    #as an option implementation  of draw from normal distribution like with recovery
    # expDur <- ((at - expTime[idsEligExp]))
    # 
    # exp_room <- seq(0,20,1)
    # nor_dens_exp <- dnorm(exp_room,5,1.25)
    # expProb <- nor_dens_exp[expDur + 1]
    # 
    vecExp <- idsEligExp[which(rbinom(nEligExp, 1, exp_rate) == 1)]
    if (length(vecExp)) { 
      nExp <- length(vecExp)
      status[vecExp] <- "i"
      infTime[vecExp] <- at
      expTime[vecExp] <- NA
      
      #age groups  
      age_groups <- cut(dat$attr$age, breaks = c(0, 20, 45, 55, 65, 75, 85, 100), 
                        ordered_result = FALSE, labels = FALSE, include.lowest = TRUE, right = FALSE)
      
      age_groups_sev <- age_groups[vecExp]
      hosp_rate_pp <- hosp_rate[age_groups_sev]
      
      #evaluate which of the infected Individuals will get severely ill or ICUed and at which time
      vecSev <- vecExp[which(rbinom(nExp, 1, hosp_rate_pp) == 1)]
      nSev <- length(vecSev)
      
      # assign recoveries to non-severe infected recoveries
      idsEligRecNorm <- vecExp[which(vecExp %!in% vecSev)]
      nEligRecNorm <- length(idsEligRecNorm)
      
      if (nEligRecNorm > 0) {
        recNorm_dur <- round(runif(nEligRecNorm, min = params$min_norm_duration, 
                                  max = params$max_norm_duration))
        
        recTimeNorm <- at + recNorm_dur
        dat$attr$recTime[idsEligRecNorm] <- recTimeNorm
      }
      
      if (nSev > 0) { 
        #assign time of severity/hospitalisation Times are taken from Zhou et al.
        sev_time <- round(runif(nSev, min = params$min_sev, max = params$max_sev))
          dat$attr$sevTime[vecSev] <- at + sev_time
      
      
        # calculate conditional probabilities for ICU admission wenn severely sick
        icu_rate_new <- ICU_rate/hosp_rate
      
        age_groups_icu <- age_groups[vecSev]
        icu_rate_new_pp <- icu_rate_new[age_groups_icu]
      
        vecIcu <- vecSev[which(rbinom(nSev, 1, icu_rate_new_pp) == 1)]
        nIcu <- length(vecIcu)
        
        # manage recoveries for non ICU-severe patients
        idsEligRecSev <- vecSev[which(vecSev %!in% vecIcu)]
        nEligRecSev <- length(idsEligRecSev)
        if (nEligRecSev > 0) {
          recSev_dur <- round(runif(nEligRecSev, min = params$min_sev_duration, 
                              max = params$max_sev_duration))
        
          recTimeSev <- dat$attr$sevTime[idsEligRecSev] + recSev_dur
          dat$attr$recTime[idsEligRecSev] <- recTimeSev
        }
        if (nIcu > 0) { #browser()
          #assign Time of ICU 
          icu_time <- round(runif(nIcu, min = params$min_icu, max = params$max_icu))
            unfortunate_draw_icu <- which(icu_time < sev_time[which(vecIcu %in% vecSev)])
            icu_time[unfortunate_draw_icu] <- sev_time[unfortunate_draw_icu]
            dat$attr$icuTime[vecIcu] <- at + icu_time
        
          #covid related death
          cfr_rate_new <- CFR_rate_alt/icu_rate_new
          cfr_rate_new[which(is.nan(cfr_rate_new))] <- 0
          
          age_groups_cfr <- age_groups[vecIcu]
          cfr_rate_new_pp <- cfr_rate_new[age_groups_cfr]
          
          vecCfr <- vecIcu[which(rbinom(nIcu, 1, cfr_rate_new_pp) == 1)]
          nCfr <- length(vecCfr)
          
          # manage recoveries for ICU-severe patients
          idsEligRecIcu <- vecIcu[which(vecIcu %!in% vecCfr)]
          nEligRecIcu <- length(idsEligRecIcu)
          recIcu_dur <- round(runif(nEligRecIcu, min = params$min_icu_duration, 
                              max = params$max_icu_duration))
          
          recTimeIcu <- dat$attr$icuTime[idsEligRecIcu] + recIcu_dur
          dat$attr$icuRecTime[idsEligRecIcu] <- recTimeIcu
          
          # final recovery from the virus
          recIcuSev_dur <- round(runif(nEligRecIcu, min = params$min_sev_duration, 
                              max = params$max_sev_duration))
          
          recTimeIcu_fin <- dat$attr$sevTime[idsEligRecIcu] + recIcuSev_dur
          unfortunate_draw_rec <- which(recTimeIcu_fin < recTimeIcu[which(vecIcu %!in% vecCfr)])
          recTimeIcu_fin[unfortunate_draw_rec] <- recTimeIcu[unfortunate_draw_rec] + 1
          
          dat$attr$recTime[idsEligRecIcu] <- recTimeIcu_fin
          
          if (nCfr > 0) {
            #assign Time of ICU 
            cfr_time <- round(runif(nCfr, min = params$min_cfr, max = params$max_cfr))
            dat$attr$cfrTime[vecCfr] <- at + cfr_time
          }
        }
      }
    }
  }
  
  # Write out updated status attribute----------------------------------------------
  dat$attr$status <- status
  dat$attr$infTime <- infTime
  dat$attr$expTime <- expTime
  
  if (at == 2) {
    dat$epi$ei.flow     <- c(0, nExp)
    dat$epi$i.num       <- c(0, nExp)
  } else {
    dat$epi$ei.flow[at]     <- nExp
    dat$epi$i.num[at]       <- length(activeInfIdx(dat))
  }
  
  
  return(dat)
}