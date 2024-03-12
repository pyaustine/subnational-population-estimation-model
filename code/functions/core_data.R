core_data <- function(agemin, agemax, yearmin, yearmax, wpp_ref_years) {
  
  # WPP national   
  load("data/wpp_5year.rda")
  wpp <- wpp_5year %>%
    filter(age_group >= !! agemin, age_group <= !! agemax) %>%
    filter(year <= !! yearmax, year >= !! yearmin)
  
  # Census         
  load("data/ipums_cleaned_m.rda")
  ipums_all <- ipums_cleaned_m %>%
    filter(age >= !! agemin, age <= !! agemax) %>%
    filter(year <= !! yearmax, year >= !! yearmin) %>%
    rename(district = geo2, prev_district = prev_geo2)
  
  # Census Population          
  ipums_population <- ipums_all  %>%
    group_by(year, age_group, district, county) %>%
    summarise(population = sum(population)) %>% 
    ungroup()

  # Census Migration         
  mig_status <- c(12, 20)
  in_migrants <- ipums_all %>%
    group_by(year, age_group, district, county) %>%
    summarise(n_in_migrants = sum(population[migrate1 %in% mig_status])) %>%
    collect
  out_migrants <- ipums_all %>%
    group_by(year, age_group, prev_district, prev_county) %>%
    summarise(n_out_migrants = sum(population[migrate1 %in% mig_status])) %>%
    rename(county = prev_county, district = prev_district)
  
  mig <- in_migrants %>%
    dplyr::left_join(out_migrants) %>%
    dplyr::mutate(net_migrants = n_in_migrants - n_out_migrants) %>%
    dplyr::mutate(net_migrants = ifelse(is.na(net_migrants), 0, net_migrants)) %>%# zero imputation here since we are smoothing it anyway
    group_by(year, district, county, age_group) %>%
    mutate(
      net_migrants = sum(net_migrants)
    ) %>%
    left_join(ipums_population %>% ungroup() %>% select(year, age_group, district, county, population)) %>%
    mutate(prop_mig = net_migrants / population) %>%
    ungroup()
  
mig_sum <- mig %>% group_by(year, district, county) %>% 
  summarise(n_in_migrants = sum(n_in_migrants, na.rm = TRUE),
            n_out_migrants = sum(n_out_migrants, na.rm = TRUE))

  
  # Data settings
  counties <- ipums_population %>% drop_na(county) %>% select(county) %>% unique %>% pull
  ncounties <- length(counties)
  districts <- ipums_population %>% drop_na(district) %>% select(district) %>% unique %>% pull
  ndistricts <- length(districts)
  data_settings <- list(
    counties = counties,
    ncounties = ncounties,
    districts = districts,
    ndistricts = ndistricts
  )
  core_data <- list(
    wpp = wpp,
    ipums_population = ipums_population,
    ipums_migration = mig,
    ipums_migration_sum = mig_sum,
    data_settings = data_settings
  )
  return(core_data)
}
