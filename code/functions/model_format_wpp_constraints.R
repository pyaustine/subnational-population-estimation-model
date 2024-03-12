
model_format_wpp_constraints <- function(wpp,
                                              wppyears,
                                              nwppyears,
                                              age_groups,
                                              nage_groups,
                                              lower_bound,
                                              upper_bound
                                              )
{
  x_at <- matrix(1, nage_groups, nwppyears)
  wpp_atb <- array(NA, c(nage_groups, nwppyears, 2))
  wpp_at <- array(NA, c(nage_groups, nwppyears, 1))
  for (i in 1:nage_groups) {
    for (j in 1:nwppyears) {
      this_total_pop <- wpp %>%
        filter(age_group == age_groups[i], year == wppyears[j]) %>%
        summarise(sum(population))
      wpp_atb[i, j, 1] <- this_total_pop[[1]] * lower_bound
      wpp_atb[i, j, 2] <- this_total_pop[[1]] * upper_bound
      wpp_at[i, j, 1] <- this_total_pop[[1]]
    }
  }
  return(list(
    x_at = x_at,
    
    wpp_at = wpp_at,
    wpp_atb = wpp_atb
  ))
}
