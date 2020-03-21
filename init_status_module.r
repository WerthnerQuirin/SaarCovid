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
  r.num         <- dat$init$r.num

  status.vector <- dat$init$status.vector
  num           <- network.size(dat$nw)
  mode          <- rep(1, num)
  statOnNw      <- "status" %in% dat$temp$fterms

  type          <- dat$control$type

  # Age ------------------------------------------------------------------
    dat <- aging.init(dat)

  # Status ------------------------------------------------------------------
  
  ## Status passed on input network
  if (statOnNw == TRUE) {
    status <- get.vertex.attribute(dat$nw, "status")
  } else {
    if (!is.null(status.vector)) {
      status <- status.vector
    } else {

      start_ids <- c(1:params$initial_pop)
      status <- rep("s", num)
      status[sample(start_ids, size = i.num)] <- "i"
      inf <- which(status == "i")
      status[sample(start_ids[-c(inf)], size = r.num)] <- "r"
      rec <- which(status == "r")
      status[sample(start_ids[-c(inf, rec)], size = e.num)] <- "e"
      exp <- which(status == "e")
    }
  }
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
  

  if (tea.status == TRUE) {
    dat$nw <- activate.vertex.attribute(dat$nw,
                                        prefix = "testatus",
                                        value = status,
                                        onset = 1,
                                        terminus = Inf)
  }
  
  # Infection Time ----------------------------------------------------------
  ## Set up inf.time vector
  idsInf <- which(status == "i")
  infTime <- rep(NA, length(status))

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

    # exposed Time ----------------------------------------------------------
  ## Set up can.time vector
  idsExp <- which(dat$attr$status == "e")
  expTime <- rep(NA, length(status))

  if (!is.null(dat$init$expTime.vector)) {
    expTime <- dat$init$expTime.vector
  } else {
    # If vital dynamics, infTime is a geometric draw over the duration of infection
    if (params$de.rate > 0) {
        expTime[idsExp] <- -rgeom(n = length(idsExp),
                                  prob = params$de.rate +
                                    (1 - params$de.rate)*mean(params$exp_rate_init)) + 2
    } 
  }

  dat$attr$expTime <- expTime

  return(dat)
}