#####################################Corona-Parameters###########################################

params <- list(
  ###################Simulation######################
  nsims = 1,
  nsteps = 60,
  ncores = 1,
  ###################Formulas######################
  mean_degree_1 = 0.5,
  duration_1 = 10000,
  concurrency_1 = 0,
  act_rate_1 = 1,
  mean_degree_2 = 8,
  duration_2 = 1,
  concurrency_2 = 0,
  act_rate_2 = 1,
  ###################Population######################
  initial_pop = 2000,
  s_num_init = 1987,
  e_num_init = 4,
  i_num_init = 2,
  r_num_init = 0,
  sc_num_init = 0,
  modes = 1,
  max_age = 100,
  ###################Rates######################
  departure_rate = 0.004,
  
  a.rate = 0.0001,
  ds.rate = 0.0001,
  di.rate = 0.05,
  de.rate = 0.2,
  dr.rate = 0.0001,
  dv.rate = 0.0001,
  
  inf_prob = 0.05,
  rec_prob = 0.001,
  arrival_rate = 0.0067,
  aging_inc = 1,
  dv_rate = 0.0,
  exp_rate = 0.2,
  
  #durations from Zhou et al. lancet (2020)
  
  min_sev = 8,
  max_sev = 14,
  min_icu = 8,
  max_icu = 15,
  min_cfr = 17,
  max_cfr = 25,
  
  # According to https://www.who.int/docs/default-source/coronaviruse/who-china-joint-mission-on-covid-19-final-report#
  # the duration until recovery is about 14 days. Therefore ->
  min_norm_duration = 12,
  max_norm_duration = 16, 
  min_sev_duration = 7,
  max_sev_duration = 14,
  min_icu_duration = 4, 
  max_icu_duration = 12,
  
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
  
  sev_rate = 0.2,
  
  
  
  
  ###################Controls######################
  save_network = FALSE,
  delete_nodes = FALSE,
  tea_status = FALSE, 
  verbose = TRUE,
  depend = TRUE,
  use_pids = TRUE,
  skip_check = TRUE
)
