  german_pop <- read.csv("./german_population_abs.csv", sep = ";", skip = 5)
  names(german_pop) <- c("year", "population")
  german_pop$population <- gsub(",","", german_pop$population)
  german_pop <- german_pop[1:69,]
  german_pop$year <- substring(german_pop$year, 7,10)
  german_pop$population <- as.integer(german_pop$population)
  
  vorausberechnung_14 <- read.table("vorausberechnung_14.txt", header = FALSE, sep = "")
  vorausberechnung_per_anno <- vorausberechnung_14[-c(2,5,7,10,13,16,18,20,23,24,27,30,32,33,35,
                                                      38,41,47,50,53,55,58,63,66,70,73,77,79,82,
                                                      85,91,93,96,99,103,c(105:151)),]
  vorausberechnung_per_anno[,1] <- c(1950:2018)
  vorausberechnung_per_anno[,2] <- vorausberechnung_per_anno[,2]*1000000

  deaths_1956 <- read.csv("./deaths_1956.csv", sep = ";", skip = 5)
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

  deaths_decades <- deaths_1956[,c(7,17,27,37,47,57,65)]
  
  deaths_1950 <- rowMeans(deaths_1956[,c(3:8)]) #before that tried 3:13
  deaths_1950 <- gsub("\\..*", "", deaths_1950)
  deaths_1950 <- as.numeric(deaths_1950)
  
  #################################################
  # Deaths of Every Year not only the decades
  deaths_1950_added <- deaths_1956
  deaths_1950_added <- add_column(deaths_1950_added, "1950" = deaths_1950, "1951" = deaths_1950, 
                                  "1952" = deaths_1950, "1953" = deaths_1950, "1954" = deaths_1950, 
                                  "1955" = deaths_1950, .before = "1956")
  
  all_deaths_rate_male <- deaths_1950_added[1:100,-c(1,2)]
  all_deaths_rate_male <- all_deaths_rate_male/age_table_all_men
  all_deaths_rate_female <- deaths_1950_added[102:201,-c(1,2)]
  all_deaths_rate_female <- all_deaths_rate_female/age_table_all_women
  
  all_deaths_rate_female <- do.call(data.frame ,lapply(all_deaths_rate_female, function(x) 
    replace(x, is.infinite(x), NA)))
  names(all_deaths_rate_female) <- gsub("X","", names(all_deaths_rate_female))
  all_deaths_rate_female <- zoo::na.locf(all_deaths_rate_female)
  all_deaths_rate_female[101,] <- 1
  
  all_deaths_rate_male <- do.call(data.frame ,lapply(all_deaths_rate_male, function(x) 
    replace(x, is.infinite(x), NA)))
  names(all_deaths_rate_male) <- gsub("X","", names(all_deaths_rate_male))
  all_deaths_rate_male <- zoo::na.locf(all_deaths_rate_male)
  all_deaths_rate_male[101,] <- 1
  
  ################################################
  # deaths of people decadewise
  deaths_decades$`1950` <- deaths_1950
  deaths_decades <- deaths_decades[, c(8, 1, 2, 3, 4, 5, 6, 7)]
  
  deaths_decades_female <- deaths_decades[102:201,]
  deaths_decades_male <- deaths_decades[1:100,]  
  
  death_rates_decades_female <- deaths_decades_female/age_pop_decades_women[-c(101),]
  death_rates_decades_male <- deaths_decades_male/age_pop_decades_men[-c(101),]
  
 death_rates_decades_female <- do.call(data.frame ,lapply(death_rates_decades_female, function(x) 
   replace(x, is.infinite(x), NA)))
 names(death_rates_decades_female) <- gsub("X","", names(death_rates_decades_female))
 death_rates_decades_female <- zoo::na.locf(death_rates_decades_female)
 death_rates_decades_female[101,] <- 1
 
 death_rates_decades_male <- do.call(data.frame ,lapply(death_rates_decades_male, function(x) 
   replace(x, is.infinite(x), NA)))
 names(death_rates_decades_male) <- gsub("X","", names(death_rates_decades_male))
 death_rates_decades_male <- zoo::na.locf(death_rates_decades_male)
 death_rates_decades_male[101,] <- 1

 age_table_2017 <- read.table("Age_Dist_Saarland_2017.csv", sep = ";")
 
 ###############################
 #Cancer Deaths
 #Survivability per agegroup
 Survivability <- read.table("Munich_cancer_registry-Survivability.txt", sep = ",", header = TRUE)
 Survivability <- Survivability[-c(1),-c(2,4,6,8,10)]
 names(Survivability) <- c("canTime","1","2","3","4","5")

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
   #browser()
  # clamp age to max.age
  ages <- pmin(agesFromIdx(dat, active.idx), 
               params$max_age)
 
  # death probabilities
  death.p <- props[ages + 1,][[2]]

  return(active.idx[which(rbinom(length(active.idx), 1, death.p) == 1)])
}

death.module <- function(dat, at) {
  nsteps    <- params$nsteps
    
  if (at >= 70) {
    death_props_female <- all_deaths_rate_female[,c(1, 69)]
    death_props_male <- all_deaths_rate_male[, c(1, 69)]
  }
  
  # number of new deaths
  female.dead <- 0
  male.dead   <- 0
  n.dead      <- 0
  
  n.active <- countActive(dat)

  if (n.active > 0) {
    active.m.idx <- activeMaleIdx(dat)
    active.f.idx <- activeFemaleIdx(dat)

    dead.m.idx   <- death.ids(dat, active.m.idx, death_props_male, "males")
    dead.f.idx   <- death.ids(dat, active.f.idx, death_props_female, "females")
    
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
}

#death through cancer#
severeCase.death.module <- function(dat,at){

  active    <- dat$attr$active
  status    <- dat$attr$status
  InfTime   <- dat$attr$canTime
  age       <- dat$attr$age

  modes     <- params$modes
  
  nCanDep <- 0
  idsEligCan <- activeCanIdx(dat)
  
  if (!is.null(idsEligCan)) {
    age_groups <- cut(dat$attr$age, breaks = c(0,35,50,60,70,100), ordered_result = FALSE, 
                      labels = FALSE, include.lowest = TRUE, right = FALSE)
    
    match_can_death_rates <- function(canTime, age_groups, Survivability) {
      # browser()
      idx <- intersect(which(!is.na(canTime)), which(active == 1))
      pairs <- mapply(c, canTime[idx], age_groups[idx], SIMPLIFY = FALSE)
      
      res <- sapply(pairs, function(x){ Survivability[x[1], x[2] + 1]} )
      unlist(res)
    }
    can_death_rates <- 1 - (match_can_death_rates(canTime, age_groups, Survivability)/100)
    
    nEligCan <- length(idsEligCan)
    
    #Process for specialized death from the cancer compartment -----------------------------------------------------------------
    if (nEligCan > 0) {
       # browser()
      
      vecCanDep <- idsEligCan[which(rbinom(nEligCan, 1, can_death_rates) == 1)]
      if (length(vecCanDep) > 0) {
        #idsCanDep <- idsEligCan[vecCanDep]
        nCanDep <- sum(mode[vecCanDep] == 1)
        nCanDepM2 <- sum(mode[vecCanDep] == 2)
        
        dat <- setInactive(dat, at, vecCanDep)
      }
      
    } 
  }
  if (isInit(at)) {
    dat$epi$can.death.flow      <- c(0, nCanDep)
    dat$epi$cd.num              <- c(0, nCanDep)
  } else{
    dat$epi$can.death.flow[at]      <- nCanDep
    dat$epi$cd.num[at]              <- sum(dat$epi$cd.num[at - 1], nCanDep)
  }
  return(dat)
  }

  ##Further effects migration and demographic factors will be explored.

