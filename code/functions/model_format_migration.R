model_format_migration <- function(mig){
  
  mig_age_in <- (mig %>% select(n_in_migrants) %>% pull())/1000
  mig_age_out <- (mig %>% select(n_out_migrants) %>% pull())/1000
  
  tau_mig_in <- 1/(0.1 * 0.9 *mig_age_in*5^2*2)
  tau_mig_out <- 1/(0.1 * 0.9 *mig_age_out*5^2*2)
  tau_mig_in[is.infinite(tau_mig_in)]<- 1
  tau_mig_out[is.infinite(tau_mig_out)]<- 1
  tau_mig_in[is.na(tau_mig_in)]<- 1
  tau_mig_out[is.na(tau_mig_out)]<- 1
  
  return(list(
    mig_age_in = mig_age_in*5,
    mig_age_out = mig_age_out*5, #multiplying by 5 because of five year intervals in model
    tau_mig_in = tau_mig_in,
    tau_mig_out = tau_mig_out))
}