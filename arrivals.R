###################################
# ARRIVAL
###################################

#births in Germany from 2008 - 2018 to use the mean birthrate over the course of the last 10 years 

suppressWarnings(
  births_germany <- suppressMessages(read_delim("germany_births_over_time.csv", delim = ";", skip = 6,  
                                                col_names = c("year", "male", "female", "total"), n_max = 69, 
                                                guess_max = 50))
)

# table with total population numbers from 1950-2018
suppressWarnings(
  population_over_time <- suppressMessages(read_delim("german_population_over_time.csv", delim = ";",
                                                      skip = 6,  col_names = FALSE, n_max = 69, 
                                                      guess_max = 50))
)

births_germany <- cbind(births_germany, N_tot = population_over_time$X2)
arrival_rate_yearly <- births_germany$total[69] / births_germany$N_tot[69]
arrival_rate <- arrival_rate_yearly/366

#################################################################################################
arrival.module <- function(dat, at) {
    return(dat)
  a.rate                <- arrival_rate 

  # Control
  a.rand                <- dat$control$a.rand
  delete.nodes          <- dat$control$delete.nodes
  tea.status            <- dat$control$tea.status
  
  status                <- dat$attr$status
  age                   <- dat$attr$age
  recTime               <- dat$attr$recTime
  screened              <- dat$attr$screened
  screenTime            <- dat$attr$screenTime
  vac_rate_arrival      <- params$vac_rate_arrival
  
  alive                 <- activeIdx(dat)
  nOld                  <- dat$epi$num[at - 1]

  nCurr                 <- network.size(dat$nw)  
  nArrivals             <- 0 
  
  newNodes              <- NULL
  
  #################################################################################################  
  # Process
  
  if (nOld > 0) {
    nArrivals <- sum(rbinom(nOld, 1, a.rate))
  }
  
  if ( nArrivals > 0) {
    dat$nw <- add.vertices(dat$nw, nv = nArrivals)
    newNodes <- (nCurr + 1):(nCurr + nArrivals)
    dat$nw <- activate.vertices(dat$nw, onset = at, terminus = Inf, v = newNodes)
  }
  
  
  # Update Nodal Attributes -------------------------------------------------
  if (length(newNodes) > 0) {
    
    #Set attributes on nw
    fterms <- dat$temp$fterms
    curr.tab <- get_attr_prop(dat$nw, fterms)
    
    if (length(curr.tab) > 0) {
      dat$nw <- update_nwattr(dat$nw, newNodes, dat$control$attr.rules,
                              curr.tab, dat$temp$t1.tab)
    }
    
    # Save any val on attr
    dat <- copy_toall_attr(dat, at, fterms)
    
    if (tea.status == TRUE) {
      if ('status' %in% fterms) {
        dat$nw <- activate.vertex.attribute(dat$nw, prefix = 'testatus', 
                                             value = dat$attr$status[newNodes],
                                             onset = at, terminus = Inf,
                                             v = newNodes)
      } else {
        dat$nw <- activate.vertex.attribute(dat$nw, prefix = 'testatus',
                                            value = 's', onset = at, terminus = Inf,
                                            v = newNodes)
      }
    }
    
    if (!('status' %in% fterms)) {
      dat$attr$status <- c(dat$attr$status, rep('s', length(newNodes)))
    }
    dat$attr$active         <- c(dat$attr$active, rep(1, length(newNodes)))
    dat$attr$infTime        <- c(dat$attr$infTime, rep(NA, length(newNodes)))
    dat$attr$expTime        <- c(dat$attr$expTime, rep(NA, length(newNodes)))
    dat$attr$recTime        <- c(dat$attr$recTime, rep(NA, length(newNodes)))
    dat$attr$entrTime       <- c(dat$attr$entrTime, rep(at, length(newNodes)))
    dat$attr$exitTime       <- c(dat$attr$exitTime, rep(NA, length(newNodes)))
    dat$attr$vacTime        <- c(dat$attr$vacTime, rep(NA, length(newNodes)))
    dat$attr$screened       <- c(dat$attr$screened, rep(0, length(newNodes)))
    dat$attr$isolated       <- c(dat$attr$isolated, rep(0, length(newNodes)))
    dat$attr$vac            <- c(dat$attr$vac, rep(0, length(newNodes)))
    dat$attr$age            <- c(dat$attr$age, rep(0, length(newNodes)))
    dat$attr$severe         <- c(dat$attr$severe, rep(0, length(newNodes)))
    dat$attr$occupation     <- c(dat$attr$occupation, rep(0, length(newNodes)))
    dat$attr$sick           <- c(dat$attr$sick, rep(0, length(newNodes)))
    dat$attr$ventilation    <- c(dat$attr$ventilation, rep(0, length(newNodes)))
    dat$attr$hospitalized   <- c(dat$attr$hospitalized, rep(0, length(newNodes)))
    dat$attr$sevTime        <- c(dat$attr$sevTime, rep(NA, length(newNodes)))
    dat$attr$icuTime        <- c(dat$attr$icuTime, rep(NA, length(newNodes)))
    dat$attr$cfrTime        <- c(dat$attr$cfrTime, rep(NA, length(newNodes)))
    dat$attr$icuRecTime     <- c(dat$attr$icuRecTime, rep(NA, length(newNodes)))
    
    males <- sample(c(0,1), size = length(newNodes), 0.5)
    dat$attr$gender     <- c(dat$attr$gender, males)
    
    if (nArrivals > 0) {
      incoming_nodes <- (length(dat$attr$status) - nArrivals + 1):length(dat$attr$status)
    }else{
      incoming_nodes <- 0
    }
    
    # # Entering of exposed and infected people
    # newNodesExp <- intersect(newNodes, which(dat$attr$status == 'e'))
    # dat$attr$expTime[newNodesExp] <- at
    # 
    # newNodesInf <- intersect(newNodes, which(dat$attr$status == 'i'))
    # dat$attr$infTime[newNodesInf] <- at
    # 
    # newNodesRec <- intersect(newNodes, which(dat$attr$status == 'r'))
    # dat$attr$recTime[newNodesRec] <- at
    
    #Here we set the probability of a node entering the System Vaccinated
    if (length(incoming_nodes) > 0) {
      vecVac <- incoming_nodes[which(rbinom(length(incoming_nodes), 1, vac_rate_arrival) == 1)]
      
      if (length(vecVac) > 0) {
        
        #women who are getting vaccinated
        ids_nom_Vac <- intersect(incoming_nodes, vecVac)
        n_nom_Vac <- length(ids_nom_Vac)
        
        #women who actually aquire immunity
        idsVac <- ids_nom_Vac[which(rbinom(length(ids_nom_Vac), 1, params$vac_eff) == 1)]
        
        nVac <- length(idsVac)
        
        dat$attr$vac[idsVac]   <- 1
        dat$attr$vacTime[idsVac]  <- at
        
      }else {
        nVac = 0
      }
    }
    
    
    if (length(unique(sapply(dat$attr, length))) != 1) {
      browser()
      stop("Attribute list of unequal length. Check arrivals.net module.") 
    }
  }  
  
  # Output ------------------------------------------------------------------
  if ((isInit(at))) {
    dat$epi$a.flow            <- c(0, nArrivals)
    dat$epi$actives           <- c(0, length(which(dat$attr$active == 1)))
    # dat$epi$e_entr_flow       <- c(0, length(newNodesExp))
    # dat$epi$i_entr_flow       <- c(0, length(newNodesInf))
    # dat$epi$r_entr_flow       <- c(0, length(newNodesRec))
  } else {
    dat$epi$a.flow[at]        <- nArrivals
    dat$epi$actives[at]       <- length(which(dat$attr$active == 1))
    # dat$epi$e_entr_flow[at]   <- length(newNodesExp)
    # dat$epi$i_entr_flow[at]   <- length(newNodesInf)
    # dat$epi$r_entr_flow[at]   <- length(newNodesRec)
  }
  return(dat)
}
