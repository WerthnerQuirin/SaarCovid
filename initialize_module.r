#Initialize.module#
#is needed to add the init_status_module to the initialisation and through that the netsim function#

initialize.module <- function(x, param, init, control, s) {

  if (control$start == 1) {
    # Master Data List --------------------------------------------------------
    dat <- list()
    dat$param <- param
    dat$init <- init
    dat$control <- control

    dat$attr <- list()
    dat$stats <- list()
    dat$temp <- list()


    # Network Simulation ------------------------------------------------------
    nw <- simulate(x$fit, basis = x$fit$newnetwork,
                   control = control$set.control.ergm)
    modes <- ifelse(nw %n% "bipartite", 2, 1)
    if (control$depend == TRUE) {
      if (class(x$fit) == "stergm") {
        nw <- network.collapse(nw, at = 1)
      }
      nw <- sim_nets(x, nw, nsteps = 1, control)
    }
    if (control$depend == FALSE) {
      nw <- sim_nets(x, nw, nsteps = control$nsteps, control)
    }
    nw <- activate.vertices(nw, onset = 1, terminus = Inf)


    # Network Parameters ------------------------------------------------------
    dat$nw <- nw
    dat$nwparam <- list(x[-which(names(x) == "fit")])
    dat$param$modes <- modes

    # Initialization ----------------------------------------------------------

    ## Initialize persistent IDs
    if (control$use.pids == TRUE) {
      dat$nw <- init_pids(dat$nw, dat$control$pid.prefix)
    }

    ## Pull network val to attr
    form <- get_nwparam(dat)$formation
    fterms <- get_formula_term_attr(form, nw)
    dat <- copy_toall_attr(dat, at = 1, fterms)

    ## Store current proportions of attr
    dat$temp$fterms <- fterms
    dat$temp$t1.tab <- get_attr_prop(dat$nw, fterms)

    ## Infection Status and Time Modules
    dat <- init_status.module(dat)

    ## Get initial prevalence
    dat <- get_prev.net(dat, at = 1)
  } else {
    dat <- list()

    dat$nw <- x$network[[s]]
    dat$param <- x$param
    dat$control <- control
    dat$nwparam <- x$nwparam
    dat$epi <- sapply(x$epi, function(var) var[s])
    names(dat$epi) <- names(x$epi)
    dat$attr <- x$attr[[s]]
    dat$stats <- sapply(x$stats, function(var) var[[s]])
    dat$temp <- list()
  }

  return(dat)
}