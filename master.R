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
suppressPackageStartupMessages(library(tergmLite))


# packageVersion("EpiModel") == "1.8.0"
###################################################################

set.seed(4468)

###################################################################
# load modules
modules <- c(
"./helper.R",
"./params.R",
"./aging_init.R",
"./aging.R",
"./init_status_module.R",
"./initialize_module.R",
"./netsim_par.R",
"./calc_nwstats_saarcovid.R",
"./discord_edgelist_saarcovid.R",
"./edges_correct_saarcovid.R",
"./resim_nets_saarcovid.R",
"./recovery.R",
"./progression.R",
"./vaccination.init.R",
"./enactment.R",
"./arrivals.R",
"./death_module.R",
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

network::set.vertex.attribute(x = nw, attr = "age", value = init_age_dist)
network::set.vertex.attribute(x = nw, attr = "gender", value = NA)
network::set.vertex.attribute(x = nw, attr = "expTime", value = NA)
network::set.vertex.attribute(x = nw, attr = "recTime", value = NA)
network::set.vertex.attribute(x = nw, attr = "expTime", value = NA)
network::set.vertex.attribute(x = nw, attr = "vacTime", value = NA)
network::set.vertex.attribute(x = nw, attr = "screened", value = 0)
network::set.vertex.attribute(x = nw, attr = "vac", value = NA)
network::set.vertex.attribute(x = nw, attr = "isolated", value = NA)
network::set.vertex.attribute(x = nw, attr = "severe", value = 0)
network::set.vertex.attribute(x = nw, attr = "occupation", value = init_age_dist)
network::set.vertex.attribute(x = nw, attr = "sick", value = 0)
network::set.vertex.attribute(x = nw, attr = "ventilation", value = 0)
network::set.vertex.attribute(x = nw, attr = "sevTime", value = NA)
network::set.vertex.attribute(x = nw, attr = "icuTime", value = NA)
network::set.vertex.attribute(x = nw, attr = "cfrTime", value = NA)
network::set.vertex.attribute(x = nw, attr = "icuRecTime", value = NA)

source("./initial_status_sizes.R", local = TRUE)

#####################################################################
# control
control <- control.net(type = "SEIRSSC",
                       nsims = params$nsims,
                       nsteps = params$nsteps,
                       depend = params$depend,
                       use.pids = params$use_pids,
                       tea.status = params$tea_status,
                       verbose = params$verbose,
                       ncores = params$ncores,
                       skip.check = params$skip_check,
                       delete.nodes = params$delete_nodes,
                       save.network = params$save_network,
                       initialize.FUN = initialize.module,
                       aging.FUN = aging.module,
                       departures.FUN = death.module,
                       arrivals.FUN = arrival.module,
                       edges_correct.FUN = NULL,
                       resim_nets.FUN = resim_nets_saarcovid,
                       severeCase.death.FUN = severeCase.death.module,
                       # vaccination_init.FUN = vaccination.init,
                       progression.FUN = progression.module,
                       # recovery.FUN = recovery.module,
                       enactment.FUN = enactment.module,
                       # progression.FUN = progression_module,
                       # screening.FUN = screening_module,
                       infection.FUN = infection.module,
                       module.order = c("aging.FUN", "departures.FUN", "severeCase.death.FUN",
                                        "arrivals.FUN", "resim_nets.FUN", "infection.FUN", 
                                        "severeCase.death.FUN", "progression.FUN", "enactment.FUN")
)

#####################################################################
# networks
# Model 1; model for houshold with high duration and repetitive high risk contacts
mean_degree_1 <- params$mean_degree_1

edges <- mean_degree_1*length(nw$val)
match_1 <- edges*0.7

formation_1 <- ~edges + nodematch("occupation")
target_stats_1 <- c(edges, match_1)

coef_diss_1 <- dissolution_coefs(dissolution = ~offset(edges),
                               duration = params$duration_1
                               )

est_1 <- netest(nw, formation_1, target_stats_1, coef_diss_1,
                set.control.ergm = control.ergm(MCMLE.maxit = 500, MCMC.interval = 3e4,
                                                MCMC.burnin = 2e6))
# 
# dx_1 <- netdx(est_1, nsims = params$nsims, nsteps = params$nsteps, dynamic = FALSE, 
#             keep.tedgelist = TRUE, nwstats.formula = ~edges, )
# 
# par(mar = c(3,3,1,1), mgp = c(2,1,0))
# plot(dx)
# dx

# Model 2; model for society wide occupation-based network no durations and multiple contacts
mean_degree_2 <- params$mean_degree_2
 
edges <- mean_degree_2*length(nw$val)
match_2 <- edges*0.7
 
formation_2 <- ~edges #+ nodematch("occupation")
target_stats_2 <- c(match_2)#, match_2)
 
coef_diss_2 <- dissolution_coefs(dissolution = ~offset(edges),
                                duration = params$duration_2
)
 
est_2 <- netest(nw, formation_2, target_stats_2, coef_diss_2,
                set.control.ergm = control.ergm(MCMLE.maxit = 500, MCMC.interval = 3e4,
                                                MCMC.burnin = 2e6))
# 
# dx_2 <- netdx(est_2, nsims = params$nsims, nsteps = params$nsteps, dynamic = FALSE, 
#            keep.tedgelist = TRUE, nwstats.formula = ~edges, )

init <- init.net(  e.num = params$e_num_init,
                   i.num = params$i_num_init,
                   r.num = params$r_num_init,
                   sc.num = params$sc_num_init
                   #c.num = param_sum$initial_cancer,
                   #c.num.m2 = param_sum$initial_cancer_m2
                   #v.num = 0,      #v.num and v.num.m2 are set to 0 at init, because it is handled by vaccination.init
                   #v.num.m2 = 0
                   
)

param <- param.net(inf.prob = params$inf_prob, 
                   a.rate = params$a.rate,    
                   rec.rate = params$rec.rate,
                   ds.rate = params$ds.rate,
                   de.rate = params$de.rate,
                   di.rate = params$di.rate,
                   dr.rate = params$dr.rate,
                   dv.rate = params$dv.rate
)

est <- list(est_1, est_2)
# if saveout
# saveRDS(est, file = "est.saarcovid.rds"

####################################################

sims <- netsimPar(est, param, init, control, modules)
sim_frame <- as.data.frame(sims)

svg("./testplot.svg", width = 10, height = 7)
  plot(sims)
dev.off()

svg("./testplot_min_Snum.svg", width = 10, height = 7)
plot(sim_frame$time, sim_frame$i.num, type = "l",  xlab = "Time [days]", ylab = "Count", 
     lwd = 2, col = darkred)
lines(sim_frame$time, sim_frame$e.num, type = "l",lwd = 2, col = orange)
lines(sim_frame$time, sim_frame$sev.num, type = "l",lwd = 2, col = purple)
lines(sim_frame$time, sim_frame$icu.num, type = "l",lwd = 2, col = pink)
lines(sim_frame$time, sim_frame$cov.death.num, type = "l",lwd = 2, col = grey)
lines(sim_frame$time, sim_frame$r.num, type = "l",lwd = 2, col = green)

l.text <- c("Infected",
            "Exposed",
            "Hospitalized",
            "ICU",
            "Dead",
            "Recovered"
            )

legend("topleft", legend = l.text,  
       lty = c(1,1,1,1,1,1), col = c(darkred, orange, purple, pink, grey, green),
       border = c(NA,NA,NA,NA), merge = TRUE,
       bty = "n", lwd = 2)
dev.off()
