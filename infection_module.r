## Infection Module##
# Provides the Possibility of people from the Susceptibel compartment to infect themselfs via  
# contacts with infected people. As a addition to the "normal" Module this new one provides 
# functionalities for the usage of vaccinations, which is implemented via a reduced 
# probability to become infected and is somewhat leaky in the sense that it does guarantee  
# total protection when used.

infection.module <- function(dat, at) {
    # Variables ---------------------------------------------------------------
    active <- dat$attr$active
    status <- dat$attr$status
    modes <- dat$param$modes

    inf.prob <- params$inf_prob
    act.rate <- params$act_rate

    nw <- dat$nw
    tea.status <- dat$control$tea.status

    # Vector of infected and susceptible IDs
    idsInf <- activeInfIdx(dat)
    nActive <- countActive(dat)
    nElig <- length(idsInf)

    # Initialize vectors
    nInf <- 0

    # Process -----------------------------------------------------------------
    # If some infected AND some susceptible, then proceed
    if (nElig > 0 && nElig < nActive) {

      # Get discordant edgelist
      del <- discord_edgelist(dat, at)

      # If some discordant edges, then proceed
      if (!(is.null(del))) {

        # Infection duration to at
        del$infDur <- at - dat$attr$infTime[del$inf]
        del$infDur[del$infDur == 0] <- 1
        del$vac <- dat$attr$vac[del$inf]

        # Calculate infection-stage transmission rates
        linf.prob <- length(inf.prob)
        # if (is.null(inf.prob.m2)) {
          del$transProb <- ifelse(del$infDur <= linf.prob,
                                  inf.prob[del$infDur],
                                  inf.prob[linf.prob])
        # } else {
        #   del$transProb <- ifelse(del$sus <= nw %n% "bipartite",
        #                           ifelse(del$infDur <= linf.prob,
        #                                  inf.prob[del$infDur],
        #                                  inf.prob[linf.prob]),
        #                           ifelse(del$infDur <= linf.prob,
        #                                  inf.prob.m2[del$infDur],
        #                                  inf.prob.m2[linf.prob]))
        # }

        # Interventions
        # if (!is.null(dat$param$inter.eff) && at >= dat$param$inter.start) {
        #   del$transProb <- del$transProb * (1 - dat$param$inter.eff)
        # }

        # Calculate infection-stage act/contact rates
        lact.rate <- length(act.rate)
        del$actRate <- ifelse(del$infDur <= lact.rate,
                              act.rate[del$infDur],
                              act.rate[lact.rate])
        
        #check and draw for vaccinations
        del$vac <- dat$attr$vac[del$sus]
        
        vecVac <- which(del$vac == 1)

        if (length(vecVac) != 0) {
        del <- del[-vecVac, ]
        }

        # Calculate final transmission probability per timestep
        del$finalProb <- 1 - (1 - del$transProb) ^ del$actRate

        # Randomize transmissions and subset df
        transmit <- rbinom(nrow(del), 1, del$finalProb)
        del <- del[which(transmit == 1), ]

        # Set new infections vector
        idsNewInf <- unique(del$sus)
        
        if (length(idsNewInf) > 0) {
          nInf <- length(idsNewInf)
        }else{
          nInf <- 0
        }

        
        # Update nw attributes
        if (nInf > 0) {
          if (tea.status == TRUE) {
            nw <- activate.vertex.attribute(nw,
                                            prefix = "testatus",
                                            value = "e",
                                            onset = at,
                                            terminus = Inf,
                                            v = idsNewInf)
          }
          dat$attr$status[idsNewInf] <- "e"
          dat$attr$infTime[idsNewInf] <- at

          nodoublesus <- distinct(del, del$sus, .keep_all = TRUE)
          dat$attr$vac[idsNewInf] <- nodoublesus$vac

          if ("status" %in% dat$temp$fterms) {
            nw <- set.vertex.attribute(nw, "status", dat$attr$status)
          }
        }

        # Substitute PIDs for vital bipartite sims
        if (any(names(nw$gal) %in% "vertex.pid")) {
          del$sus <- get.vertex.pid(nw, del$sus)
          del$inf <- get.vertex.pid(nw, del$inf)
        }

      } # end some discordant edges condition
    } # end some active discordant nodes condition

   # Output ------------------------------------------------------------------

   # Save transmission matrix
   if (nInf > 0) {
     del <- del[!duplicated(del$sus), ]
     if (at == 2) {
       dat$stats$transmat <- del
     } else {
       dat$stats$transmat <- rbind(dat$stats$transmat, del)
     }
   }

   ## Save incidence vector
   if (at == 2) {
    dat$epi$se.flow           <- c(0, nInf)

   } else {
    dat$epi$se.flow[at]     <- nInf
    dat$epi$s.num[at]       <- length(activeSusIdx(dat))
    dat$epi$e.num[at]       <- length(activeExpIdx(dat))

   }

   dat$nw <- nw
   
   # if (at == 50) {browser()}
   
   return(dat)
}