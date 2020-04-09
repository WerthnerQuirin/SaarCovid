# helper functions
suppressPackageStartupMessages(library(EpiModel))

# returns the number of active nodes
countActive <- function(dat) {
  n <- sum(dat$attr$active == 1)
  return(n)
}

# returns a vector of active node idxs
activeIdx <- function(dat) {
  return(which(dat$attr$active == 1))
}

# returns the ages of nodes with ids in idx
agesFromIdx <- function(dat, idx) {
  return(dat$attr$age[idx])
}

# inactivates nodes with idx for time at 
# and returns updated dat
setInactive <- function(dat, at, idx) {
  if (is.na(idx) || !length(idx) || is.null(idx)) {
    return(dat)
  }
  dat$attr$active[idx] <- 0
  dat$attr$exitTime[idx] <- at
  dat$nw <- deactivate.vertices(dat$nw, onset = at, terminus = Inf, 
                                v = idx, deactivate.edges = TRUE)
  return(dat)
}

# returns idx of male nodes (mode 2)
maleIdx <- function(dat) {
  return(which(dat$attr$gender == 0))
}

# returns idx of active male nodes
activeMaleIdx <- function(dat) {
  return(intersect(maleIdx(dat), activeIdx(dat)))
}

# returns idx of female nodes (mode 1)
femaleIdx <- function(dat) {
  return(which(dat$attr$gender == 1))
}

# returns idx of active male nodes
activeFemaleIdx <- function(dat) {
  return(intersect(femaleIdx(dat), activeIdx(dat)))
}

# returns the number of inactive nodes
countInactive <- function(dat) {
  n <- sum(dat$attr$active != 1)
  return(n)
}

# check if timestep is initializer or not
isInit <- function(at) {
  return(at == 2)
}

# return a vector of statii for selected idx
statusIdx <- function(dat, Idx) {
  return(dat$attr$status[Idx]) 
}

# returns Ids for suseptible Individuals  
susIdx <- function(dat) {
  return(which(dat$attr$status == "s")) 
}

# returns Ids for acrive, suseptible Individuals
activeSusIdx <- function(dat){
  return(intersect(which(dat$attr$active == 1), which(dat$attr$status == "s")))
}

# returns Ids for suseptible Individuals  
expIdx <- function(dat) {
  return(which(dat$attr$status == "e")) 
}

# returns Ids for acrive, suseptible Individuals
activeExpIdx <- function(dat){
  return(intersect(which(dat$attr$active == 1), which(dat$attr$status == "e")))
}

# returns Ids for infected Individuals  
infIdx <- function(dat) {
  return(which(dat$attr$status == "i")) 
}

# returns Ids for active, infected Individuals
activeInfIdx <- function(dat){
  return(intersect(which(dat$attr$active == 1), which(dat$attr$status == "i")))
}

# returns Ids for recovered Individuals  
recIdx <- function(dat) {
  return(which(dat$attr$status == "r"))
}

# returns Ids for active, recovered Individuals
activeRecIdx <- function(dat){
  return(intersect(which(dat$attr$active == 1), which(dat$attr$status == "r")))
}

# returns Ids for vaccinated Individuals
vacIdx <- function(dat) {
  return(which(dat$attr$vac == 1)) 
}

# returns Ids for active, vaccinated Individuals 
activeVacIdx <- function(dat){
  return(intersect(which(dat$attr$active == 1), which(dat$attr$vac == 1)))
}

activeSevIdx <- function(dat) {
  return(intersect(which(dat$attr$active == 1), which(dat$attr$status == "sc")))
}

activeIcuIdx <- function(dat) {
  return(intersect(which(dat$attr$active == 1), which(dat$attr$icu == 1)))
}

activeSickIdx <- function(dat) {
  return(intersect(which(dat$attr$active == 1), which(dat$attr$sick == 1)))
}

is.screened <- function(dat) {
   return(intersect(which(dat$attr$active == 1), which(dat$attr$screened == 1)))
}

# find out what values are not in a certain subset
'%!in%' <- function(sub_set,orig_set)!('%in%'(sub_set,orig_set)) 

substrRight <- function(x, n){
  substr(x, nchar(x) - n + 1, nchar(x))
}

convert_fac_num <- function(dataset, columns) {
  lapply(dataset[columns], function(x) as.numeric(as.character(x)))
}

draw_from_lognorm <- function(mean, sd, length) {
  location <- log(mean^2/sqrt(sd^2 + mean^2))
  shape <- sqrt(log(1 + (sd^2 / mean^2)))
  draw <- round(rlnorm(length, meanlog = location, sdlog = shape), 0)
  return(draw)
}

recover_this <- function(dat, at, vector_recoveries, immun_ratio) { 
  nRecRec <- NA
  if (length(vector_recoveries) > 0) {
    vecRecRec <- vector_recoveries[which(rbinom(length(vector_recoveries), 1, immun_ratio) == 1)]
    vecRecSus <- vector_recoveries[vector_recoveries %!in% vecRecRec]
  
    dat$attr$status[vecRecRec] <- "r"
    dat$attr$infTime[vecRecRec] <- NA
    dat$attr$recTime[vecRecRec] <- at
    nRecRec   <- length(vecRecRec)

    dat$attr$status[vecRecSus] <- "s"
    dat$attr$infTime[vecRecSus] <- NA
    dat$attr$recTime[vecRecSus] <- NA
    nRecSus   <- length(vecRecSus)
  }else{
     nRecSus <- 0 
     nRecRec <- 0
  }

   final_out <- list(dat = dat, nRecRec = nRecRec, nRecSus = nRecSus)
     
return(final_out)
}
