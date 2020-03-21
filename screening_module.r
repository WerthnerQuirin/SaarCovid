screening.module <- function(dat, at) {

    active      <- dat$attr$active
    status      <- dat$attr$status
    age         <- dat$attr$age
    mode        <- idmode(dat$nw)

    screened            <- dat$attr$screened 
    screenTime          <- dat$attr$screenTime
    screenProb          <- dat$attr$screenProb
    screeningprob_20    <- param_sum$screeningrate_20
    screeningprob_30    <- param_sum$screeningrate_30
    pap_sens            <- 0.72      #mean of LSIL+ and ASCUS+ values for CIN3+ fom Cochrane
    pap_spez            <- 0.968     #param_sum$pap_spez

    #browser() 
    
    screened.ids <- function(dat, eligScreen_idx, props) {
        if (!length(eligScreen_idx) || is.na(eligScreen_idx)) {
            return(c())
        }
        
        # list of ages for all people who are alive 
        #ages <- pmin(agesFromIdx(dat, eligScreen_idx))
        ages <- agesFromIdx(dat, eligScreen_idx)
        
        # screening probabilities
        screen.p <- props[ages,][[2]]
        
        return(eligScreen_idx[which(rbinom(length(eligScreen_idx), 1, screen.p) == 1)])
    }
    
    # in effect this takes out the screening module to first focus on the demographics
    if (at == 200) {
    
    # Only women get screened 
    eligScreen_tot <- intersect(activeIdx(dat), which(screened == 0))
    nEligScreen_tot <- length(eligScreen_tot)

    eligScreen <- intersect(eligScreen_tot, modeids(dat$nw, mode = 1))
    nEligScreen <- length(eligScreen)
   
    eligYearlyScreen_1 <- intersect(eligScreen, which(age < 30))
    eligYearlyScreen <- intersect(eligYearlyScreen_1, which(age > 19))
    #eligYearlyScreen_inf <- intersect(eligYearlyScreen, which(status == "i"))
    #eligYearlyScreen_sus <- intersect(eligYearlyScreen, which(status == "s"))

    elig3YearScreen_1 <- intersect(eligScreen, which(age > 30))
    
    # which are eligable but havent been screened in the last 3 years
    elig3YearScreen <- intersect(elig3YearScreen_1, which(at - screenTime > 3 | is.na(screenTime)))
    
    # screening rates 2006 and 2011 from 20 to 100 
    
    if (at > 56 && at < 63) {
        screening_rate_2006 <- read.csv("C:/Users/quiri/Documents/GitHub/PRAEZIS/data/Geyer2015-Cervical_cancer_screening_in_germany_graph_2006.txt", sep = "", skip = 2)
        
        # make sure that you only get screened starting at the age of 20
        names(screening_rate_2006)[1:2] <- c("age", "screenProb")  
        screen_0_20 <- data.frame("age" = c(1:19), "screenProb" = rep(0,19))
        screening_rate_2006 <- rbind(screen_0_20,screening_rate_2006)
        screening_rate_2006$age <- gsub("\\..*","", screening_rate_2006$age)
        
        screened_2006 <- screened.ids(dat, eligScreen, screening_rate_2006)
        
    }
    
    if (at > 62) {
        screening_rate_2011 <- read.csv("C:/Users/quiri/Documents/GitHub/PRAEZIS/data/Geyer2015-Cervical_cancer_screening_in_germany_graph_2011.txt", sep = "", skip = 2)
        
        # make sure that you only get screened starting at the age of 20
        names(screening_rate_2011)[1:2] <- c("age", "screenProb")  
        screening_rate_2011 <- rbind(screen_0_20,screening_rate_2011)
        screening_rate_2011$age <- gsub("\\..*","", screening_rate_2011$age) 
        
    }
    
    
    
    
if (at > 70) {
    if (length(eligYearlyScreen) > 0) {
        vecScreened20 <- eligYearlyScreen[which(rbinom(length(eligYearlyScreen), 1, 
            screeningprob_20) == 1)]
        if (length(vecScreened20) > 0) { 
            # incorporation of sensitivity to the Paptest, infected people are found out with this 
            # part; but it has to be taken into account that test will be retaken; that has to be 
            # implemented, maybe with additional testing, furthermore cancer and Cis have to have 
            # different Probabilities   
            eligScreened20_inf <- intersect(vecScreened20, which(dat$attr$status == "i"))
            eligScreened20_can <- intersect(eligScreened20_inf, which(dat$attr$cancer == 1))
            eligScreened20_canCis <- intersect(vecScreened20_can, which(dat$attr$cis == 1))

            vecScreened20_cisCan <- eligScreened20_canCis[which(rbinom(length(eligScreened20_canCis),
             1, screeningprob_20) == 1)]

            vecScreened20_sens <- vecScreened20_cisCan[which(rbinom(length(vecScreened20_cisCan), 1, 
                pap_sens) == 1)]
            idsScreened20_HR <- intersect(eligYearlyScreen, vecScreened20_sens)
            nScreened20_HR <- length(idsScreened20_HR)
            dat$attr$screened[idsScreened20] <- 1
            dat$attr$screenTime[idsScreened20] <- at
        }else{
            nScreened20 = 0
        }
        }else{
            nScreened20 = 0
        }
    

    if (length(elig3YearScreen) > 0){
        vecScreened30 <- elig3YearScreen[which(rbinom(length(elig3YearScreen), 1, 
            screeningprob_30) == 1)]
        if (length(vecScreened30) > 0) {  
            vecScreened30_true <- vecScreened30[which(rbinom(length(vecScreened30), 1, 
                pap_sens) == 1)]  
            idsScreened30_HR <- intersect(elig3YearScreen, vecScreened30_true)
            nScreened30_HR <- length(idsScreened30_HR)
            dat$attr$screened[vecScreened30] <- 1
            dat$attr$screenTime[vecScreened30] <- at
        }else{
                nScreened30 = 0
       }
            
            }else{
                nScreened30 = 0
        }
    
    
    nScreened_tot <- nScreened20 + nScreened30

    if (isInit(at)) {
        dat$epi$screened20.flow         <- c(0, nScreened20)
        dat$epi$screened30.flow         <- c(0, nScreened30)
        dat$epi$screened.tot.flow       <- c(0, nScreened_tot)
    } else {
        dat$epi$screened20.flow[at]     <- nScreened20
        dat$epi$screened30.flow[at]     <- nScreened30
        dat$epi$screenedtot.flow[at]    <- nScreened_tot
    }

    if (at == 20) {
        browser()
    }
}
} # this is the bracket to take out the screening module -for reference look above
return(dat)
}