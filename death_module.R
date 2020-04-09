  deaths_1956 <- read.csv("deaths_1956.csv", sep = ";", skip = 5)
  deaths_1956 <- deaths_1956[-c(304:307),]
  
  names(deaths_1956)[1:2] <- c("gender", "age")  
  names(deaths_1956) <- gsub("\\.","", names(deaths_1956))
  deaths_1956$`X2018` <- gsub("\\,","", deaths_1956$X2018)
  deaths_1956$gender <- as.character(deaths_1956$gender)
  deaths_1956$age <- as.character(deaths_1956$age)
  deaths_1956$gender[which(deaths_1956$gender == "weiblich")] <- 1
  deaths_1956$gender[which(deaths_1956$gender == "männlich")] <- 2
  deaths_1956$gender[which(deaths_1956$gender == "Insgesamt")] <- 3
  deaths_1956$age[which(deaths_1956$age == "unter 1 Jahr")] <- 0
  deaths_1956$age <- gsub("\\-.*", "", deaths_1956$age)
  deaths_1956 <- data.frame(sapply(deaths_1956, as.numeric), stringsAsFactors = FALSE)
  names(deaths_1956) <- gsub("X","", names(deaths_1956))
  
  deaths_2018 <- deaths_1956[,c("gender","age", "2018")]

  #################################################
  # Deaths of Every Year not only the decades
  
  
  all_deaths_rate_male <- deaths_2018[1:100,-c(1,2)]
  all_deaths_rate_male <- all_deaths_rate_male/age_table_all_men_2018
  all_deaths_rate_female <- deaths_2018[102:201,-c(1,2)]
  all_deaths_rate_female <- all_deaths_rate_female/age_table_all_women_2018
  
 age_table_2017 <- read.table("Age_Dist_Saarland_2017.csv", sep = ";")
 
 ###############################
 #Cancer Deaths
 #Survivability per agegroup
 # Survivability <- read.table("Munich_cancer_registry-Survivability.txt", sep = ",", header = TRUE)
 # Survivability <- Survivability[-c(1),-c(2,4,6,8,10)]
 # names(Survivability) <- c("canTime","1","2","3","4","5")

##################################

death.init <- function(dat, n.dead, 
                       male.dead, 
                       female.dead) {
  dat$epi$death.flow        <- c(0, n.dead)
  dat$epi$male.death.flow   <- c(0, male.dead)
  dat$epi$female.death.flow <- c(0, female.dead)
  
  return(dat)  
}


death.ids <- function(dat, active.idx, props, sex) {
  if (!length(active.idx) || is.na(active.idx)) {
    return(c())
  }

  # clamp age to max.age
  ages <- pmin(agesFromIdx(dat, active.idx), 
               params$max_age)
  props[100,] <- 1
  props
  # death probabilities
  death.p <- props[ages + 1,][[2]]

  return(active.idx[which(rbinom(length(active.idx), 1, death.p) == 1)])
}

death.module <- function(dat, at) {
  if (at == 365) {
    
  nsteps    <- params$nsteps
  # number of new deaths
  female.dead <- 0
  male.dead   <- 0
  n.dead      <- 0
  
  n.active <- countActive(dat)

  if (n.active > 0) {
    active.m.idx <- activeMaleIdx(dat)
    active.f.idx <- activeFemaleIdx(dat)

    dead.m.idx   <- death.ids(dat, active.m.idx, all_deaths_rate_male, "males")
    dead.f.idx   <- death.ids(dat, active.f.idx, all_deaths_rate_female, "females")
    
    female.dead  <- length(dead.f.idx)
    male.dead    <- length(dead.m.idx)
    n.dead       <- female.dead + male.dead

    if (female.dead > 0) {
      dat <- setInactive(dat, at, dead.f.idx)
    }
    if (male.dead > 0) {
      dat <- setInactive(dat, at, dead.m.idx) 
    }
  }
  
  # statistics
  if (isInit(at)) {
    dat <- death.init(dat, n.dead, 
                      male.dead, 
                      female.dead)
    dat$epi$death.flow            <- c(0, n.dead) 
    dat$epi$male.death.flow       <- c(0, male.dead) 
    dat$epi$female.death.flow     <- c(0, female.dead)
  }
  else {
    dat$epi$death.flow[at]        <- n.dead 
    dat$epi$male.death.flow[at]   <- male.dead 
    dat$epi$female.death.flow[at] <- female.dead
    
  }

  if (at == 0) {
    browser()
  }
  
  return(dat)
  }else{
    return(dat)
  }
}

#death through covid
 severeCase.death.module <- function(dat,at){
return(dat)
   active       <- dat$attr$active
   status       <- dat$attr$status
   InfTime      <- dat$attr$infTime
   age          <- dat$attr$age
   gender       <- dat$attr$gender
   severe       <- dat$attr$severe
   
   nCovDep <- 0
   idsEligCov <- activeIcuIdx(dat)
   nEligCov <- length(idsEligCov)
   
   # Severe Outcomes Among Patients with Coronavirus Disease 2019 (COVID-19) - United States, February 12-March 16, 2020. MMWR Morb Mortal Wkly Rep. ePub: 18 March 2020 -> ICU and hosp rates per age group
   # 0-19, 20-44, 45-54, 55-64, 65-74, 75-84, 85-100
   
   #hospitalized (severely ill)
   hosp_data <- list(c(0.016, 0.025), c(0.143, 0.208), c(0.212, 0.283), c(0.205, 0.301), 
                     c(0.286, 0.435), c(0.305, 0.587), c(0.313, 0.703))
   hosp_data_mean <- lapply(hosp_data, mean)
   hosp_rate <- unlist(hosp_data_mean)
   
   # admitted to ICU
   ICU_data <- list(c(0), c(0.02, 0.042), c(0.054, 0.104), c(0.047, 0.112), c(0.081, 0.188), 
                    c(0.105, 0.31), c(0.063, 0.29))
   ICU_data_mean <- lapply(ICU_data, mean)
   ICU_rate <- unlist(ICU_data_mean)
   
   # alternative fatalities for the CDC - source
   CFR_data <- list(c(0), c(0.001, 0.002), c(0.005, 0.008), c(0.014, 0.026), c(0.027, 0.049), 
                    c(0.043, 0.105), c(0.104, 0.273))
   CFR_data_mean <- lapply(CFR_data, mean)
   CFR_rate_alt <- unlist(CFR_data_mean)
   
   # calculate conditional probabilities for ICU admission when severely sick
   icu_rate_new <- ICU_rate/hosp_rate
   
   # calculate conditional probabilities for covid induced death when in ICU
   cfr_rate_new <- CFR_rate_alt/icu_rate_new
   
   age_groups <- cut(dat$attr$age, breaks = c(0, 20, 45, 55, 65, 75, 85, 100), 
                     ordered_result = FALSE, labels = FALSE, include.lowest = TRUE, right = FALSE)
   
   age_groups_icu <- age_groups[idsEligCov]
   death_rate_pp <- cfr_rate_new[age_groups_icu]
   
   # if (length(idsEligCov) > 0) { if (at > 15) {browser()}
   #   # cfr Rabi et al
   #   # 0-9, 10-19, 20-29, 30-39, 40-49, 50-59, 60-69, 70-79,80+
   #   cfr_agegroups <- c(0, 0.002, 0.002, 0.002, 0.004, 0.013, 0.036, 0.08, 0.148)
   #   
   #   age_groups <- cut(dat$attr$age, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 101), 
   #                     ordered_result = FALSE, labels = FALSE, include.lowest = TRUE, right = FALSE)
   #   age_groups_severe <- age_groups[idsEligCov]
   #   cfr_rate <- cfr_agegroups[age_groups_severe]
      
     # Process for specialized death from the cancer compartment -----------------------------------------------------------------
     if (nEligCov > 0) {
        # browser()
       vecCovDep <- idsEligCov[which(rbinom(nEligCov, 1, death_rate_pp) == 1)]
       if (length(vecCovDep) > 0) {
         # idsCanDep <- idsEligCan[vecCanDep]
         nCovDep <- length(vecCovDep)

         dat <- setInactive(dat, at, vecCovDep)
         dat$attr$infTime[vecCovDep] <- NA
         dat$attr$severe[vecCovDep] <- 0
         dat$attr$sevTime[vecCovDep] <- NA
         dat$attr$icuTime[vecCovDep] <- NA
       }
       
     } 
   #}
   if (isInit(at)) {
     dat$epi$cov.death.flow         <- c(0, nCovDep)
     dat$epi$cov.death.num          <- c(0, nCovDep)
   } else{
     dat$epi$cov.death.flow[at]     <- nCovDep
     dat$epi$cov.death.num[at]      <- sum(dat$epi$cov.death.num[at - 1], nCovDep)
   }
   return(dat)
   }

  ##Further effects migration and demographic factors will be explored.

