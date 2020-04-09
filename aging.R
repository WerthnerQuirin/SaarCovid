suppressPackageStartupMessages(library(dplyr))
###################################
# inc = increment of age per time step

aging.init <- function(dat){
  
  # age <- get.vertex.attribute(dat$nw, "age")
  dat$attr$age <- init_age_dist
  
  # init statistics
  dat$epi$meanAge <- rep(mean(dat$attr$age, na.rm = TRUE), 2)
  dat$epi$medianAge <- rep(median(dat$attr$age, na.rm = TRUE), 2)
  return(dat)
}

aging.module <- function(dat, at) { 
  if (at == 2) { 
   dat$epi$meanAge <- rep(mean(dat$attr$age, na.rm = TRUE), 2)
   dat$epi$medianAge <- c(0, median(dat$attr$age[which(dat$attr$active == 1)]))
  }
  else { 
    if (at == 365) {
      active.idxs <- activeIdx(dat)
      dat$attr$age[active.idxs] <- dat$attr$age[active.idxs] + aging.props$inc
    }
      
    # statistics
    dat$epi$meanAge[at] <- mean(dat$attr$age[which(dat$attr$active == 1)])
    dat$epi$medianAge[at] <- median(dat$attr$age[which(dat$attr$active == 1)])
  }
  
  return(dat)
}
