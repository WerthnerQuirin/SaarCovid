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
    dat$stats$nwstats <- list()
    dat$temp <- list()
    
    
    # Network Simulation ------------------------------------------------------

    dat$nw <- list()
    for (i in 1:length(x)) {
      dat$nw[[i]] <- simulate(x[[i]]$fit, basis = x[[i]]$fit$newnetwork)
    }
    nw <- dat$nw
    # nw <- simulate(x$fit, basis = x$fit$newnetwork,
    #                control = control$set.control.ergm)
    # modes <- ifelse(nw %n% "bipartite", 2, 1)
    # if (control$depend == TRUE) {
    #   if (class(x$fit) == "stergm") {
    #     nw <- network.collapse(nw, at = 1)
    #   }
    #   nw <- sim_nets(x, nw, nsteps = 1, control)
    # }
    # if (control$depend == FALSE) {
    #   nw <- sim_nets(x, nw, nsteps = control$nsteps, control)
    # }
    # nw <- activate.vertices(nw, onset = 1, terminus = Inf)
    
    
    # Network Parameters ------------------------------------------------------
    # dat$nw <- nw
    dat$nwparam <- list()
    for (i in 1:length(x)) {
      dat$nwparam[[i]] <- list(x[[i]][-which(names(x[[i]]) == "fit")])
    }
    # dat$param$modes <- modes
    
    # Initialization ----------------------------------------------------------
    
    ## Initialize persistent IDs
    # if (control$use.pids == TRUE) {
    #   dat$nw <- init_pids(dat$nw)#, dat$control$pid.prefix)
    # }
    dat <- init_status.module(dat)
    
    # Pull in attributes on network
    nwattr.all <- names(nw[[1]][["val"]][[1]])
    nwattr.use <- nwattr.all[!nwattr.all %in% c("na", "vertex.names")]
    for (i in seq_along(nwattr.use)) {
      dat$attr[[nwattr.use[i]]] <- get.vertex.attribute(nw[[1]], nwattr.use[i])
    }
    browser()
    # Convert to tergmLite method
    dat <- init_tergmLite(dat)
    
    # ## Pull network val to attr
    # form <- get_nwparam(dat)$formation
    # fterms <- get_formula_term_attr(form, nw)
    # dat <- copy_toall_attr(dat, at = 1, fterms)
    # 
    # ## Store current proportions of attr
    # dat$temp$fterms <- fterms
    # dat$temp$t1.tab <- get_attr_prop(dat$nw, fterms)
    
    ## Infection Status and Time Modules
    
    ## Get initial prevalence
    dat <- get_prev.net(dat, at = 1)
  # } else {
  #   dat <- list()
  #   browser()
  #   dat$nw <- x$network[[s]]
  #   dat$param <- x$param
  #   dat$control <- control
  #   dat$nwparam <- x$nwparam
  #   dat$epi <- sapply(x$epi, function(var) var[s])
  #   names(dat$epi) <- names(x$epi)
  #   dat$attr <- x$attr[[s]]
  #   #dat$stats <- sapply(x$stats, function(var) var[[s]])
  #   #dat$temp <- list()
  }
  
  return(dat)
}