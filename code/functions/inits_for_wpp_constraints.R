inits_for_wpp_constraints <- function(chain_number,
                                      age_groups,
                                      nage_groups,
                                      years,
                                      nyears,
                                      counties,
                                      ncounties,
                                      wpp_at,
                                      core_data) {
  # mig <- core_data$ipums_migration #%>%
    #tidyr::complete(year = years, age_group = age_groups, district = districts)
  inits_list <- list()
  inits_list[["log_lu_gt"]] <- array(-10, c(dim(wpp_at)[1], (dim(wpp_at)[2]), 2))
  inits_list$log_lu_gt[,,2] <- 100000
  inits_list$delta <- array(0, c(nyears, ncounties, 1))
  # inits_list[["phi_atc"]] <- array(0, c(nage_groups,nyears, ncounties))
  # for(a in 1:(nage_groups)){
  #   for(t in 1:(nyears)){
  #     for(c in 1:(ncounties)){
  #       inits_list$phi_atc[a,t,c] <- mig %>% 
  #         filter(age_group == age_groups[a]) %>%
  #         filter(year == 2009) %>%
  #         filter(county == counties[c]) %>%
  #         pull()
  #     }
  #   }
  # }
  
  
  all_inits <- list()
  for(i in 1:chain_number){
    all_inits[[i]] <- inits_list
  }
  return(all_inits)
}