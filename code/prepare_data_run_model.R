library(tidyverse)
library(rjags)
library(R2jags)
library(boot)


# Helper functions --------------------------------------------------------

source("code/functions/core_data.R")
source("code/functions/model_format_ipums_counts.R")
source("code/functions/model_format_ipums_proportions.R")
source("code/functions/model_format_wpp_constraints.R")
source("code/functions/model_format_wpp.R")
source("code/functions/model_format_index.R")
source("code/functions/model_format_migration.R")
source("code/functions/write_model_M.R")


# General settings --------------------------------------------------------

agemin <- 10
agemax <- 50
yearmin <- 1979
yearmax <- 2019
age_groups <- seq(0, 100, 5)[seq(0, 100, 5) %in% agemin:agemax]
nage_groups <- length(age_groups)
wppyears <- seq(yearmin, yearmax, by = 5) 
nwppyears <- length(wppyears)
years <- seq(yearmin, yearmax, by = 5) 
nyears <- length(years)
lower_bound_constraint <- .9
upper_bound_constraint <- 1.1
runname <- "my_model_run"



# Load in core data -------------------------------------------------------

core_data <- core_data(agemin,
                       agemax,
                       yearmin,
                       yearmax)


# Counts in model format --------------------------------------------------

observed_counts_and_precision <- model_format_ipums_counts(core_data)


# Proportions for priors on first year and age ----------------------------

proportions_list <-
  model_format_ipums_proportions(
    ipums_population = core_data$ipums_population,
    wpp = core_data$wpp,
    years = years,
    nyears = nyears,
    age_groups = age_groups,
    nage_groups = nage_groups,
    counties = core_data$data_settings$counties,
    ncounties = core_data$data_settings$ncounties
  )



# Mortality PCs and national coefficients ---------------------------------


pcs_df <- read_csv("data/pcs_lt.csv")
betas_df <- read_csv("data/betas_nat.csv")
betas_df <- betas_df %>% mutate(year = year - 1) %>% filter(year %in% years)


# Constraints -------------------------------------------------------------

national_constraint_list <- model_format_wpp_constraints(
  wpp = core_data$wpp, 
  wppyears = wppyears,
  nwppyears = nwppyears,
  age_groups = age_groups,
  nage_groups = nage_groups,
  lower_bound = .9,
  upper_bound = 1.1
)

wpp_at <- model_format_wpp(core_data, wppyears)
index_list <- model_format_index(core_data)


# Migration ---------------------------------------------------------------

migration <- model_format_migration(core_data$ipums_migration)



# Age-time multiplier -----------------------------------------------------


D1 <- diff(diag(nage_groups), diff = 1)
Dcomb <- t(D1)%*%solve(D1%*%t(D1))


# Parameters to sample ----------------------------------------------------


parnames <- c(
  "p_atc",
  "qx_atc",
  "phi_atc",
  "psi_atc",
  "nu_atc",
  "Psi_tc",
  "Nu_tc",
  "lu_at",
  "delta",
  "beta",
  "A_in_ac",
  "A_out_ac",
  "eps_atc"
)


# Input data --------------------------------------------------------------


jags_data <- list(
  #data model
  #pop
  logy_i = observed_counts_and_precision$logy_i,
  tau_i =   observed_counts_and_precision$tau_i, 
  npre09 = index_list$npre09,
  n = index_list$n,
  geta_i = index_list$geta_i,
  gett_i = index_list$gett_i,
  getallc_imatrix = index_list$getallc_imatrix,
  getlengthc_i = index_list$getlengthc_i,
  x_at = matrix(1, nage_groups, nwppyears),
  x_mig_at = matrix(1, nage_groups, nwppyears),
  mig_in_j = migration$mig_age_in,
  mig_out_j = migration$mig_age_out,
  tau_mig_in = migration$tau_mig_in,
  tau_mig_out = migration$tau_mig_out,
  mpre09 = index_list$mpre09,
  m = index_list$m,
  geta_j = index_list$geta_j,
  gett_j = index_list$gett_j,
  getallc_jmatrix = index_list$getallc_jmatrix,
  getlengthc_j = index_list$getlengthc_j,
  
  #IPUMS
  nyears = nyears,
  nage_groups = nage_groups,
  ncounties = core_data$data_settings$ncounties,
  prop_atc = proportions_list$prop_atc,
  
  #survival
  Y_ad = pcs_df[,3:4],
  ax = pcs_df %>% select(mean) %>% pull(),
  beta_nat = as.matrix(betas_df %>% select(beta1:beta2)),
  
  #wpp
  tau_wpp_at = 100,
  
  #wpp constraints
  nwppyears = nwppyears,
  log_wpp_atb = log(national_constraint_list$wpp_atb),
  log_wpp_at = log(national_constraint_list$wpp_at[, , 1]),
  Dcomb = Dcomb
)


# Write and save model ----------------------------------------------------


write_model_M()



# Run model ---------------------------------------------------------------


mod <- jags(
  data = jags_data,
  n.iter = 100,
  n.chains = 3,
  n.thin = 5,
  parameters.to.save = parnames,
  model.file = "./code/models/model.txt"
)



# Look at convergence -----------------------------------------------------


mod$BUGSoutput$summary[mod$BUGSoutput$summary[, "Rhat"] > 1.1 ]


# Save --------------------------------------------------------------------

reslist<- list(mcmc.array = mod$BUGSoutput$sims.array,
               mcmclist = mod$BUGSoutput$sims.list,
               core_data = core_data)

saveRDS(reslist, paste0("./results/",runname,".rds"))
