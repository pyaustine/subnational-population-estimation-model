model_format_ipums_proportions <- function(ipums_population, 
                                           wpp, 
                                           years, 
                                           nyears, 
                                           age_groups, 
                                           nage_groups,
                                           counties,
                                           ncounties) {
  
  load("data/wpp_5year.rda")
  
  # proportion of population in each county out of population in respective district for 2009
  county_props <- ipums_population %>%
    filter(year == 2009) %>%
    group_by(age_group, district) %>%
    mutate(prop_1 = population / sum(population)) %>%
    ungroup() %>%
    select(age_group, district, county, prop_1)
  county_props_ac <- county_props %>% 
    select(age_group, county, prop_1) %>%
    spread(county, prop_1) %>%
    select(-age_group) %>%
    as.matrix()
  
  # proportion of population in each (district age) our of national population 
  census_years <- ipums_population$year %>% unique()
  
  prior_census_years <- wpp %>% 
    filter(year %in% census_years) %>% 
    select(year, age_group, population) %>% 
    left_join(
      ipums_population %>% 
        ungroup() %>% 
        group_by(age_group, year) %>% 
        mutate(prop_2 = population/ sum(population)) %>% 
        select(age_group, district, prop_2) 
    ) %>% # proportion of total population by district
    left_join(county_props) %>% 
    mutate(prop_nat_county = prop_1*prop_2) %>% # proportion of total population by county
    mutate(prior_pop = population*prop_nat_county) %>% # implied population
    select(year, age_group, population, county, prop_nat_county, prior_pop) 
  
  prior_2009 <- wpp_5year %>%
    filter(year %in% census_years) %>%
    select(year, age_group, population) %>%
    left_join( ipums_population %>%
                 filter(year==2009) %>%
                group_by(age_group) %>%
                mutate(prop = population/ sum(population)) %>%
                select(age_group, county, prop) ) %>%
    mutate(prior_pop = population*prop)
  
  prior_census_years <- prior_census_years %>%
    filter(year!=2009) %>%
    bind_rows(prior_2009 %>% filter(year==2009))
  
  
  # now we need to interpolate these proportions to fill in the non-census years
  
  priors_df <- c()
  
  for(i in 1:nage_groups){
    for(j in 1:ncounties){
      pop_temp <- prior_census_years %>% filter(county == counties[j], age_group==age_groups[i]) %>% select(prior_pop) %>% pull()
      yrs <- prior_census_years %>% filter(county == counties[j], age_group==age_groups[i]) %>% select(year) %>% pull()
      pops_inter <- approx(pop_temp~yrs, xout=min(yrs):max(yrs))$y
      
      for(k in 1:length((max(yrs)+1):2020)){
        pops_inter <- c(pops_inter, 2*pops_inter[length(pops_inter)] - pops_inter[length(pops_inter)-1] )
      }
      priors_df <- rbind(priors_df, tibble(age_group=age_groups[i], county = counties[j], year = min(years):2020, pop_inter = pops_inter))
    }
  }
  
  county_props_interpolated <- priors_df %>% 
    group_by(age_group, year) %>% 
    mutate(prop_nat_county = pop_inter/sum(pop_inter))
  

  
  prop_atc <- array(NA, dim = c(nage_groups, nyears, ncounties))
  
  for( i in 1:nyears){
    for(j in 1:ncounties){
      prop_atc[,i,j] <- county_props_interpolated %>% 
        ungroup() %>% 
        filter(year==years[i], county == counties[j]) %>% 
        select(prop_nat_county) %>% 
        pull()
    }
  }
  
  prop_atc[prop_atc<=0] <- 0.0001
  
  
  return(list(county_props_ac = county_props_ac,
              county_props_interpolated = county_props_interpolated,
              prop_atc = prop_atc))
}
