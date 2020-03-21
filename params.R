#####################################Corona-Parameters###########################################

params <- list(
  ###################Simulation######################
  nsims = 1,
  nsteps = 366,
  ncores = 4,
  ###################Formulas######################
  mean_degree = 1,
  duration = 2,
  concurrency = 1,
  act_rate = 3,
  ###################Population######################
  initial_pop = 1000,
  e_num_init = 3,
  i_num_init = 10,
  r_num_init = 0,
  modes = 1,
  max_age = 100,
  ###################Rates######################
  departure_rate = 0.004,
  
  a.rate = 0.0001,
  ds.rate = 0.0001,
  di.rate = 0.0001,
  de.rate = 0.0001,
  dr.rate = 0.0001,
  dv.rate = 0.0001,
  
  exp_prob = 0.25,
  inf_prob = 0.25,
  rec_prob = 0.1,
  arrival_rate = 0.0067,
  aging_inc = 1,
  dv_rate = 0.0,
  
  vac_rate_init = 0,
  exp_rate_init = 0,
  vac_rate_arrival = 0.0,
  vac_eff = 0.96,
  vacVan_rate = 0,
  
  rec_van_rate = 0,
  mean_inf_dur = 14,
  sd_inf_dur = 4,
  immun_ratio = 0.9,
  
  recSus.rate = 0.1,
  recInfSus.rate = 0.1,
  rec.rate = 0.1,
  
  
  
  ###################Controls######################
  save_network = FALSE,
  delete_nodes = FALSE,
  tea_status = FALSE, 
  verbose = TRUE,
  depend = TRUE,
  use_pids = TRUE
)