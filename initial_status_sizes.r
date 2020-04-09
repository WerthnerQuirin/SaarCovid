##Inittial status sizes##
#gives the distribution of individuals throughout the statuses at the start of the simulation#
  #dat$attr$status <- rep(0, length(dat$attr$status))

init <- init.net(  e.num = params$e_num_init,
                   i.num = params$i_num_init,
                   r.num = params$r_num_init,
                   sc.num = params$sc_num_init
                    #c.num = param_sum$initial_cancer,
                    #c.num.m2 = param_sum$initial_cancer_m2
                    #v.num = 0,      #v.num and v.num.m2 are set to 0 at init, because it is handled by vaccination.init
                    #v.num.m2 = 0

)