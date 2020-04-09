resim_nets_saarcovid <- function(dat, at) {
  dat <- edges_correct_saarcovid(dat, at)
  
  # Network Resimulation
  for (i in 1:length(dat$el)) {
    nwparam <- EpiModel::get_nwparam(dat, network = i)
    isTERGM <- ifelse(nwparam$coef.diss$duration > 1, TRUE, FALSE)
    dat <- tergmLite::updateModelTermInputs(dat, network = i)
    if (isTERGM) {
      dat$el[[i]] <- tergmLite::simulate_network(p = dat$p[[i]],
                                                 el = dat$el[[i]],
                                                 coef.form = nwparam$coef.form,
                                                 coef.diss = nwparam$coef.diss$coef.adj,
                                                 save.changes = FALSE)
    } else {
      dat$el[[i]] <- tergmLite::simulate_ergm(p = dat$p[[i]],
                                              el = dat$el[[i]],
                                              coef = nwparam$coef.form)
    }
  }
  
  if (dat$control$save.nwstats == TRUE) {
    dat <- calc_nwstats_covid(dat, at)
  }
  
  return(dat)
}