#############################Corona-Runner############################################

###################################################################
# load necessary packages
suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(EpiModel))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(pipeR))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(RColorBrewer))

###################################################################

set.seed(4444)

###################################################################
# load modules
modules <- c(
"./helper.R",
"./params.R",
"./aging_init.R",
"./aging_module.R",
"./init_status_module.R",
"./initialize_module.R",
"./netsim_par.R",
"./recovery_module.R",
"./exp_module.R",
# "./vaccination.init.R",
#"./arrival_module.R",
#"./death_module.R",
#"./in_migration_module.R",
#"./out_migration_module.R",
#"./screening_module.R"
"./infection_module.R"
)

invisible(sapply(modules, source))

###################################################################
# Setup the network and add primary attributes to the network

n = params$initial_pop

nw <- network::network.initialize(n, 
                                  directed = FALSE)

set.network.attribute(nw, "vertex.pid", "vertex.names")
nw <- init_pids(nw, prefixes = c("F", "M"))

network::set.vertex.attribute(x = nw, attr = "age", value = init_age_dist)
network::set.vertex.attribute(x = nw, attr = "gender", value = NA)
network::set.vertex.attribute(x = nw, attr = "expTime", value = NA)
network::set.vertex.attribute(x = nw, attr = "recTime", value = NA)
network::set.vertex.attribute(x = nw, attr = "expTime", value = NA)
network::set.vertex.attribute(x = nw, attr = "vacTime", value = NA)
network::set.vertex.attribute(x = nw, attr = "screened", value = NA)
network::set.vertex.attribute(x = nw, attr = "vac", value = NA)
network::set.vertex.attribute(x = nw, attr = "isolated", value = NA)


#####################################################################
# control
control <- control.net(type = "SEIR",
                       nsims = params$nsims,
                       nsteps = params$nsteps,
                       depend = params$depend,
                       use.pids = params$use_pids,
                       tea.status = params$tea_status,
                       verbose = params$verbose,
                       delete.nodes = params$delete_nodes,
                       save.network = params$save_network,
                       initialize.FUN = initialize.module,
                       # departures.FUN = death.module,
                       aging.FUN = aging.module,
                       # arrivals.FUN = arrival_module,
                       # vaccination_init.FUN = vaccination.init,
                       exp.FUN = exp.module,
                       recovery.FUN = recovery.module,
                       # progression.FUN = progression_module,
                       # screening.FUN = screening_module,

                       infection.FUN = infection.module
)

#####################################################################
# network
mean_degree <- params$mean_degree

edges <- mean_degree*length(nw$val)

formation <- ~edges
target_stats <- c(edges)

coef_diss <- dissolution_coefs(dissolution = ~offset(edges),
                               duration = params$duration #,
                               # d.rate = params$departure_rate
                               )

est <- netest(nw, formation, target_stats, coef_diss, edapprox = TRUE)

dx <- netdx(est, nsims = params$nsims, nsteps = params$nsteps, #keep.tedgelist = TRUE,
             nwstats.formula = ~edges, )

par(mar = c(3,3,1,1), mgp = c(2,1,0))
plot(dx)
dx

source("./initial_status_sizes.R", local = TRUE)

param <- param.net(inf.prob = params$inf.prob, 
                   a.rate = params$a.rate,    
                   rec.rate = params$rec.rate,
                   ds.rate = params$ds.rate,
                   de.rate = params$de.rate,
                   di.rate = params$di.rate,
                   dr.rate = params$dr.rate,
                   dv.rate = params$dv.rate,
)

####################################################

sims <- netsimPar(est, param, init, control, modules)
sim_frame <- as.data.frame(sim)



