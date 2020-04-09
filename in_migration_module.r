###################################
# INMIGRATION
###################################

#implement data from DESTATIS on migration inbetween 1964 and 2018; the start from 
#1950 - 1963 potentially has to be estimated. This could pose a problem due to bigger migration
# movements starting in the 50s and leading up to 1973 (mostly Italian and turkish migration)
# Potentially get a mean from the years 1964 - 1973...

suppressWarnings(
  # columns are year,total arrivals, total departures, migratio surpluss. 1950-1957 without the 
  # Saarland and eastern German Population, till 1990 without eastern Germany and afterwards for the unified Germany
  migration_timeline <- suppressMessages(read_tsv("Migration_timeline.txt",  
                                                  skip = 5, n_max = 69, guess_max = 50, 
                                                  col_names = F))
)
suppressWarnings(
  inmigration_1964 <- suppressMessages(read_delim("migration_germany_64-2018.csv", delim = ";", 
                                                  skip = 4, n_max = 14, guess_max = 50))
)
inmigration_1964 <- inmigration_1964[-c(3,5,7,9,11,13,14), -c(1:3)]
inmigration_1964 <- as.data.frame(t(inmigration_1964))
names(inmigration_1964) <- c("Year", "Europe", "Africa", "Asia", "America", "Australia_Ozeania", 
                             "Stateless")
#inmigration_1964$V58 <- gsub(',', '', inmigration_1964$V58)
#strinmigration_1964 <- trans_fac_num(inmigration_1964, 1:7)

vorausberechnung_14 <- read.table("vorausberechnung_14.txt", header = FALSE, sep = "")
vorausberechnung_per_anno <- vorausberechnung_14[-c(2,5,7,10,13,16,18,20,23,24,27,30,32,33,35,
                                                    38,41,47,50,53,55,58,63,66,70,73,77,79,82,
                                                    85,91,93,96,99,103,c(105:151)),]
vorausberechnung_per_anno[,1] <- c(1950:2018)
vorausberechnung_per_anno[,2] <- vorausberechnung_per_anno[,2]*1000000

inmigration_1964$N <- rowSums(inmigration_1964[,c(2:7)])

# table with total population numbers from 1950-2018
suppressWarnings(
  population_over_time <- suppressMessages(read_delim("german_population_over_time.csv", 
                                                      delim = ";", skip = 6,  col_names = FALSE, 
                                                      n_max = 69, guess_max = 50))
)

names(population_over_time) <- c("Year", "Population")
population_over_time$Year <- substrRight(population_over_time$Year, 4)
population_over_time$Year <- as.numeric(population_over_time$Year)

#DESTATIS only provides data on imigration starting in 1964, therefore the mean immigration from
#1964 -1973 is taken as immigration per year. This seems senisble as up until 1973 increased 
#migration especially from Turkey and Italy took place
population_50_63 <- population_over_time[1:14,]
population_over_time_short <- population_over_time[-c(1:14),]

inmigration_1964 <- bind_cols(inmigration_1964, Population = population_over_time_short$Population)
inmigration_1964 <- bind_rows(population_50_63 ,inmigration_1964)

inmigration_1964$N[c(1:14)] <- migration_timeline$X2[c(68:55)]
# inmigration_time_rate <- cbind(inmigration_1964, inmig_rate = 
#                                  inmigration_1964$N/inmigration_1964$Population)
#inmigration_time_rate$inmig_rate[c(1:14)] <- mean(inmigration_time_rate$inmig_rate[c(15:24)])

# inmigration_time_rate <- cbind(inmigration_1964, inmig_rate = 
#                                  inmigration_1964$N/inmigration_1964$Population)

inmigration_time_rate <- cbind(inmigration_1964, inmig_rate = 
                                 inmigration_1964$N/vorausberechnung_per_anno$V2)

##################################################################################################
inmigrate.module <- function(dat, at) {
  # Variables
  nw              <- dat$nw
  n               <- network.size(nw)
  active          <- dat$attr$active
  status          <- dat$attr$status
  infTime         <- dat$attr$infTime
  vacTime         <- dat$attr$vacTime
  hpvType         <- dat$attr$hpvType
  vaccin          <- dat$attr$vaccin
  vaccinated      <- dat$attr$vaccinated
  cancer          <- dat$attr$cancer
  debuted         <- dat$attr$debuted
  underageddeath  <- dat$attr$underageddeath
  sqrt_age        <- dat$attr$sqrt_age
  screened        <- dat$attr$screened
  screenTime      <- dat$attr$screenTime
  screenProb      <- dat$attr$screenProb
  hyst            <- dat$attr$hyst
  hystTime        <- dat$attr$hystTime

  
  inmig_rate            <- param_sum$inmig.rate
  inmig_rate_est        <- param_sum$inmigration.rate.est
  inf.risk.mig          <- param_sum$inf.risk.mig
  vac.rate.mig          <- param_sum$vac.rate.mig
  vac.rate.mig.m2       <- param_sum$vac.rate.mig.m2
  exp_gen_rel_mig       <- param_sum$exp.gen.rel.mig
  
  tea.status            <- dat$control$tea.status
  
  inmig_rate_data <- inmigration_time_rate[which(inmigration_time_rate$Year == (at + 1948)),10]
  #change here as well; rates are now carried forward until 2119, furthermore they all now hinge on
  #the actual populationsize and not on the startingpop which was probably wrong to beginn with
  # this is only true fo the data that we have observations on, the rest should be taken from a 
  #fixed value to allow for a konstant influx
  
  # nNodesActive = active nodes
  nNodesActive <- countActive(dat)
  # number of imigs
  if (at <= 69)
    i.rate <- inmig_rate_data # * 0.9
  else
    i.rate <- inmig_rate_est 
  
  # nImigs <-  nNodesActive * i.rate
   nImigs <-  i.rate * param_sum$initial_pop
  exp_inmig_m1 <- nImigs * exp_gen_rel_mig
  exp_inmig_m2 <- nImigs * (1 - exp_gen_rel_mig)

  # Add Nodes
  nMig.m1 <- nMig.m2 <- 0
  nCurr_M1 <- length(modeids(dat$nw, mode = 1))
  nCurr_M2 <- length(modeids(dat$nw, mode = 2))
  prefixes <- unique(substr(dat$nw %v% "vertex.names", 1, 1))

  # female entries
  idsElig_m1 <- activeFemaleIdx(dat)
  nElig_m1 <- length(idsElig_m1)
  newNodes <- c()
  if (nElig_m1 > 0) {
      nMig_m1 <- rpois(1, exp_inmig_m1)
      #nMig_m1 <- exp_inmig_m1
    if (nMig_m1 > 0) {
      newNodeIds <- (nCurr_M1 + 1):(nCurr_M1 + nMig_m1)
      newPids <- paste0(prefixes[1], newNodeIds)
      dat$nw <- add.vertices(dat$nw, nv = nMig_m1, last.mode = FALSE, vertex.pid = newPids)
      newNodes <- newNodeIds
      dat$nw <- deactivate.vertices(dat$nw, onset = at, terminus = Inf, v = newNodes, 
                                    deactivate.edges = TRUE)
    }else{
      nMig_m1 <- 0
    }
  }

  # male entries
  newNodes_m2 <- c()
  idsElig_m2 <- activeMaleIdx(dat)
  nElig_m2 <- length(idsElig_m2)
  if (nElig_m2 > 0) {
      nMig_m2 <- rpois(1, exp_inmig_m2)
      # nMig_m2 <- exp_inmig_m1
    if (nMig_m2 > 0) {
      newNodeIds <- (nCurr_M2 + 1):(nCurr_M2 + nMig_m2)
      newPids <- paste0(prefixes[2], newNodeIds)
      dat$nw <- add.vertices(dat$nw, nv = nMig_m2, last.mode = TRUE, vertex.pid = newPids)
    newSize <- network.size(dat$nw)
    newNodes_m2 <- (newSize - nMig_m2 + 1):newSize
    dat$nw <- deactivate.vertices(dat$nw, onset = at, terminus = Inf, v = newNodes_m2, 
                                  deactivate.edges = TRUE)
    }else{
      nMig_m2 <- 0
    }
  }
  
  nMig_ges <- (nMig_m1 + nMig_m2)
  # Update Nodal Attributes -------------------------------------------------
  if (length(newNodes) > 0 | length(newNodes_m2) > 0) {
    #Set attributes on nw
    fterms <- dat$temp$fterms
    curr.tab <- get_attr_prop(dat$nw, fterms)
    if (length(curr.tab) > 0) {
      dat$nw <- update_nwattr(dat$nw, newNodes, dat$control$attr.rules,
                              curr.tab, dat$temp$t1.tab)
    }
    
    # Save any val on attr
    dat <- copy_toall_attr(dat, at, fterms)
  if (nMig_ges > 0) {
      dat <- split_bip(dat, "status", "s", nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "active", 1, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "infTime", NA, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "entrTime", at, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "exitTime", NA, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "age", 0, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "recTime", NA, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "vacTime", NA, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "canTime", NA, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "cancer", 0, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "hpvType", NA, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "vaccin", NA, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "vaccinated", 0, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "debuted", 0, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "debut_age", NA, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "underageddeath", 0, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "sqrt_age", 0, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2) 
      dat <- split_bip(dat, "cis", 0, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "screened", 0, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "screenTime", NA, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "screenProb", 0, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "hyst", 0, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "hystTime", NA, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "CIN1", 0, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "CIN2", 0, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
      dat <- split_bip(dat, "CIN3", 0, nCurr_M1, nCurr_M2, nMig_m1, nMig_m2)
  }

      incoming_nodes <- (length(femaleIdx(dat)) - nMig_m1 + 1):length(femaleIdx(dat))
      incoming_nodes_M2 <- (length(dat$attr$status) - nMig_m2 + 1):length(dat$attr$status)

      #Here we set the probability of a node entering the System infected

    i.num.mig <- length(incoming_nodes[rbinom(length(incoming_nodes), 1, inf.risk.mig/2) == 1])
    i.num.mig.m2 <- length(incoming_nodes_M2[rbinom(length(incoming_nodes_M2), 1,
                                                    inf.risk.mig/2) == 1])

    if (i.num.mig > length(incoming_nodes)) {
      i.num.mig <- length(incoming_nodes)
    }
    
    if (i.num.mig.m2 > length(incoming_nodes_M2)) {
      i.num.mig.m2 <- length(incoming_nodes_M2)
    }
    
    # if (i.num.mig > 0) {
    i.ids.inf                     <- sample(incoming_nodes, size = i.num.mig)
    dat$attr$status[i.ids.inf]    <- "i"
    dat$attr$infTime[i.ids.inf]   <- at 
    
    # 16
    expType16 <- param_sum$hpv.prob.16*length(i.ids.inf)  
    num_Type16 <- rpois(1, expType16)
    if (num_Type16 > length(i.ids.inf))
      num_Type16 <- length(i.ids.inf)
    vecType16 <- sample(i.ids.inf, size = num_Type16)
    dat$attr$hpvType[vecType16] <- "16"

    i.ids.inf <- setdiff(i.ids.inf, vecType16)
    
    # 18
    expType18 <- param_sum$hpv.prob.18*length(i.ids.inf)  
    num_Type18 <- rpois(1, expType18)
    if (num_Type18 > length(i.ids.inf))
      num_Type18 <- length(i.ids.inf)
    vecType18 <- sample(i.ids.inf, size = num_Type18)
    dat$attr$hpvType[vecType18] <- "18"
    
    i.ids.inf <- setdiff(i.ids.inf, vecType18)
    
    # HR
    expTypeHRVac <- param_sum$hpv.prob.hrVac*length(i.ids.inf)  
    num_TypeHRVac <- rpois(1, expTypeHRVac)
    if (num_TypeHRVac > length(i.ids.inf))
      num_TypeHRVac <- length(i.ids.inf)
    vecTypeHRVac <- sample(i.ids.inf, size = num_TypeHRVac)
    dat$attr$hpvType[vecTypeHRVac] <- "HRVac"

  i.ids.inf <- setdiff(i.ids.inf, vecTypeHRVac)

    expTypeHRNonVac <- param_sum$hpv.prob.hrNonVac*length(i.ids.inf)  
    num_TypeHRNonVac <- rpois(1, expTypeHRNonVac)
    if (num_TypeHRNonVac > length(i.ids.inf))
      num_TypeHRNonVac <- length(i.ids.inf)
    vecTypeHRNonVac <- sample(i.ids.inf, size = num_TypeHRNonVac)
  dat$attr$hpvType[vecTypeHRNonVac] <- "HRNonVac"

  i.ids.inf <- setdiff(i.ids.inf, vecTypeHRNonVac)


  dat$attr$hpvType[i.ids.inf] <- "LR"
  # }
  
    ###!!! maybe let infectiontime varry and not let it be at, as people are not freshly infected 
    #or vaccinated when entering the population via immigration###
    # if (i.num.mig.m2 > 0) {
    i.ids.inf.m2            <- sample(incoming_nodes_M2, size = i.num.mig.m2)
    dat$attr$status[i.ids.inf.m2]    <- "i"
    dat$attr$infTime[i.ids.inf.m2]   <- at    

    expType16 <- param_sum$hpv.prob.16*length(i.ids.inf.m2) 
    num_Type16 <- rpois(1, expType16)
    if (num_Type16 > length(i.ids.inf.m2))
      num_Type16 <- length(i.ids.inf.m2)
    vecType16 <- sample(i.ids.inf.m2, size = num_Type16)
  dat$attr$hpvType[vecType16] <- "16"

  i.ids.inf.m2 <- setdiff(i.ids.inf.m2, vecType16)
  
  expType18 <- param_sum$hpv.prob.18*length(i.ids.inf.m2) 
  num_Type18 <- rpois(1, expType18)
  if (num_Type18 > length(i.ids.inf.m2))
    num_Type18 <- length(i.ids.inf.m2)
  vecType18 <- sample(i.ids.inf.m2, size = num_Type18)
  dat$attr$hpvType[vecType18] <- "18"
  
  i.ids.inf.m2 <- setdiff(i.ids.inf.m2, vecType18)

    expTypeHRVac <- param_sum$hpv.prob.hrVac*length(i.ids.inf.m2)  
    num_TypeHRVac <- rpois(1, expTypeHRVac)
    if (num_TypeHRVac > length(i.ids.inf.m2))
      num_TypeHRVac <- length(i.ids.inf.m2)
    vecTypeHRVac <- sample(i.ids.inf.m2, size = num_TypeHRVac)
  dat$attr$hpvType[vecTypeHRVac] <- "HRVac"

  i.ids.inf.m2 <- setdiff(i.ids.inf.m2, vecTypeHRVac)

    expTypeHRNonVac <- param_sum$hpv.prob.hrNonVac*length(i.ids.inf.m2)  
    num_TypeHRNonVac <- rpois(1, expTypeHRNonVac)
    if (num_TypeHRNonVac > length(i.ids.inf.m2))
      num_TypeHRNonVac <- length(i.ids.inf.m2)
    vecTypeHRNonVac <- sample(i.ids.inf.m2, size = num_TypeHRNonVac)
  dat$attr$hpvType[vecTypeHRNonVac] <- "HRNonVac"

  i.ids.inf.m2 <- setdiff(i.ids.inf.m2, vecTypeHRNonVac)

  dat$attr$hpvType[i.ids.inf.m2] <- "LR"
  # }
    #resetting the remaining incoming nodes so not to give incoming individuals a vaccination 
    #AND infection
    incom_nodes_rest    <- intersect(incoming_nodes, which(dat$attr$status == "s"))
    incom_nodes_rest_m2 <- intersect(incoming_nodes_M2, which(dat$attr$status == "s"))

    #number of nodes to be vaccinated based on the whole of the incoming nodes
    v.num.mig <- length(incoming_nodes[rbinom(length(incoming_nodes), 1, vac.rate.mig) == 1]) 
    #round(vac.rate.mig*length(incoming_nodes))
    v.num.mig.m2 <- length(incoming_nodes_M2[rbinom(length(incoming_nodes_M2), 1, 
                                                    vac.rate.mig.m2) == 1])    

    v.num.mig.ges = v.num.mig + v.num.mig.m2
    
    if (v.num.mig.ges > 0) {
      
      #women who are getting vaccinated, sampled from the adapted population but the probability 
      #stil based on the whole population
      if (v.num.mig > length(incom_nodes_rest))
        v.num.mig <- length(incom_nodes_rest)
      ids_nom_Vac <- sample(incom_nodes_rest, size = v.num.mig)
      n_nom_Vac <- length(ids_nom_Vac)

      #women who actually aquire immunity
      idsVac <- ids_nom_Vac[which(rbinom(length(ids_nom_Vac), 1, param_sum$vac_eff) == 1)]
      nVac <- length(idsVac)
      
      dat$attr$vaccinated[idsVac] <- 1
      dat$attr$vacTime[idsVac]  <- at

      idsVacNon <- idsVac[which(rbinom(length(idsVac), 1, param_sum$vac.type.rate) == 1)]
      idsVacBi  <- idsVac[which(rbinom(length(idsVac), 1, (1 - param_sum$vac.type.rate)) == 1)]

      dat$attr$vaccin[idsVacNon] <- "nona"
      dat$attr$vaccin[idsVacBi] <- "bi"      

    }else {
        nVac = 0
    }

    if (length(v.num.mig.m2) > 0) {

      #men who are getting vaccinated
      if (v.num.mig.m2 > length(incom_nodes_rest_m2))
        v.num.mig.m2 <- length(incom_nodes_rest_m2)
      ids_nom_Vac_M2 <- sample(incom_nodes_rest_m2, size = v.num.mig.m2)
      n_nom_Vac_M2 <- length(ids_nom_Vac_M2)
      
      #men who actually aquire immunity
      idsVac_M2 <- ids_nom_Vac_M2[which(rbinom(ids_nom_Vac_M2, 1, param_sum$vac_eff) == 1)]
      if (length(idsVac_M2) >  length(ids_nom_Vac_M2)){
        idsVac_M2 <- ids_nom_Vac_M2
      }
      nVac_M2 <- length(idsVac_M2)
      
      dat$attr$vaccinated[idsVac_M2] <- 1
      dat$attr$vacTime[idsVac_M2]  <- at

      idsVacNon_M2 <- idsVac_M2[which(rbinom(length(idsVac_M2), 1, param_sum$vac.type.rate) == 1)]
      idsVacBi_M2  <- idsVac_M2[which(rbinom(length(idsVac_M2), 1, 
                                             (1 - param_sum$vac.type.rate)) == 1)]

      dat$attr$vaccin[idsVacNon_M2] <- "nona"
      dat$attr$vaccin[idsVacBi_M2] <- "bi"

    }else {
        nVac_M2 = 0
    }

    #adding various ages to the inmigrationg nodes, as most individuals will not move to Germany 
    #at the age of 12 according to DESTATIS the average age of people moving to Germany was 35

 #Creating the normal Distribution of 100000 individuals
    # https://en.wikipedia.org/wiki/Log-normal_distribution#Arithmetic_moments
    m_pop <- 30             # aimed for mean age of inmigrants
    s_pop <- 10             # aimed for sd of inmigrants
      location <- log(m_pop^2/sqrt(s_pop^2 + m_pop^2))
      shape <- sqrt(log(1 + (s_pop^2 / m_pop^2)))
    logNorm_pop <- round(rlnorm(param_sum$initial_pop, meanlog = location, sdlog = shape),0)
    
  # Pophist <- round(rnorm(param_sum$initial_pop, mean = 35, sd = 15),0)
  # Pophist[which(Pophist < 0)] <- 0 
  Pophist_2 <- (reshape2::melt(logNorm_pop))

  #Select only values >= min_age and <= max_age
  Pophist_3 <- (filter(Pophist_2, value >= param_sum$min_age))
  Pophist_4 <- (filter(Pophist_3, value <= param_sum$max_age ))

  #get final list of the size of the starting network and change it into a vector for 
  #implementation reasons
  Pophistlist_mig_m1 <- c(sample_n(Pophist_4, length(incoming_nodes), replace = TRUE))
  Pophistlist_mig_m2 <- c(sample_n(Pophist_4, length(incoming_nodes_M2), replace = TRUE))

  dat$attr$age[incoming_nodes]    <- Pophistlist_mig_m1$value
  dat$attr$age[incoming_nodes_M2] <- Pophistlist_mig_m2$value

  #set sqrt_age for in_migrating people to accomodate agemixing
  dat$attr$sqrt_age[incoming_nodes] <- sqrt(dat$attr$age[incoming_nodes]) 
  dat$attr$sqrt_age[incoming_nodes_M2] <- sqrt(dat$attr$age[incoming_nodes_M2])

  #set incomming nodes to have had their sexual debute
  
  dat <- debut_migration_func(incoming_nodes ,dat)
  dat <- debut_migration_func(incoming_nodes_M2, dat)
  
  # sexDeb     <- is.debuted.vec(dat$attr$age[incoming_nodes])
  # sexDeb_M2  <- is.debuted.vec(dat$attr$age[incoming_nodes_M2])
  # browser()
  newNodes <- c(newNodes, newNodes_m2)
  if (!is.null(newNodes) && length(newNodes) > 0) {
    new_adults <- intersect(which(dat$attr$age >= param_sum$sexDeb), newNodes)
    dat$nw <- activate.vertices(dat$nw, onset = at, terminus = Inf, v = new_adults)
    
    }
  }

  # Summary statistics
  if (at == 2) {
    dat$epi$imig.flow       <- c(0, nMig_m1)
    dat$epi$imig.flow.m2    <- c(0, nMig_m2)
  } else {
    dat$epi$imig.flow[at]       <- nMig_m1
    dat$epi$imig.flow.m2[at]    <- nMig_m2
  }
  
  return(dat)
}