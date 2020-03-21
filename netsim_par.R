suppressPackageStartupMessages(library(doParallel))

netsimPar <- function(x, param, init, control, sources) 
{ 
  crosscheck.net(x, param, init, control)
  if (!is.null(control[["verbose.FUN"]])) {
    do.call(control[["verbose.FUN"]], list(control, 
                                           type = "startup"))
  }
  nsims <- control$nsims
  ncores <- ifelse(nsims == 1, 1, min(parallel::detectCores(), 
                                      control$ncores))
  control$ncores <- ncores
  if (is.null(control$depend)) {
    control$depend <- FALSE
  }
  if (ncores == 1) {
    for (s in 1:control$nsims) {
      if (!is.null(control[["initialize.FUN"]])) {
        dat <- do.call(control[["initialize.FUN"]], 
                       list(x, param, init, control, s))
      }
      if (control$nsteps > 1) {
        for (at in max(2, control$start):control$nsteps) {
          morder <- control$module.order
          if (is.null(morder)) {
            lim.bi.mods <- control$bi.mods[-which(control$bi.mods %in% 
                                                    c("initialize.FUN", "verbose.FUN"))]
            morder <- c(control$user.mods, lim.bi.mods)
          }
          for (i in seq_along(morder)) {
            dat <- do.call(control[[morder[i]]], list(dat, 
                                                      at))
          }
          if (!is.null(control[["verbose.FUN"]])) {
            do.call(control[["verbose.FUN"]], list(dat, 
                                                   type = "progress", s, at))
          }
        }
      }
      if (s == 1) {
        out <- saveout.net(dat, s)
      }
      else {
        out <- saveout.net(dat, s, out)
      }
      class(out) <- "netsim"
    }
  }
  if (ncores > 1) {
    doParallel::registerDoParallel(ncores)
    sout <- foreach(s = 1:nsims) %dopar% {
      lapply(sources, source)
      control$nsims <- 1
      control$currsim <- s
      if (!is.null(control[["initialize.FUN"]])) {
        dat <- do.call(control[["initialize.FUN"]], 
                       list(x, param, init, control, s))
      }
      if (control$nsteps > 1) {
        for (at in max(2, control$start):control$nsteps) {
          morder <- control$module.order
          if (is.null(morder)) {
            lim.bi.mods <- control$bi.mods[-which(control$bi.mods %in% 
                                                    c("initialize.FUN", "verbose.FUN"))]
            morder <- c(control$user.mods, lim.bi.mods)
          }
          for (i in seq_along(morder)) {
            dat <- do.call(control[[morder[i]]], list(dat, 
                                                      at))
          }
          if (!is.null(control[["verbose.FUN"]])) {
            do.call(control[["verbose.FUN"]], list(dat, 
                                                   type = "progress", s, at))
          }
        }
      }
      out <- saveout.net(dat, s = 1)
      class(out) <- "netsim"
      return(out)
    }
    merged.out <- sout[[1]]
    for (i in 2:length(sout)) {
      merged.out <- merge(merged.out, sout[[i]], param.error = FALSE)
    }
    out <- merged.out
    class(out) <- "netsim"
  }
  return(out)
}