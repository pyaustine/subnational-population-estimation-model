model_format_index <- function(core_data) {
  ipums_population <- core_data$ipums_population
  ipums_migration <- core_data$ipums_migration
  ipums_migration_sum <- core_data$ipums_migration_sum
  counties <- core_data$data_settings$counties
  ndistricts <- core_data$data_settings$ndistricts
  districts <- core_data$data_settings$districts
  
  n09 <- ipums_population %>% filter(year == 2009) %>% nrow()
  npre09 <- ipums_population %>% filter(year != 2009) %>% nrow()
  n <- n09 + npre09
  geta_i <- getind(ipums_population, "age_group", age_groups)
  gett_i <- getind(ipums_population, "year", years)
  getd_i <- getind(ipums_population, "district", districts)
  getc_i <- getind(ipums_population, "county", counties)
  district_c <- ipums_population %>%
    filter(!is.na(county)) %>%
    select(district, county) %>%
    distinct() %>%
    mutate(county_index = 1:n())
  getallc_d <- list()
  for (d in 1:ndistricts) {
    getallc_d[[d]] <- district_c %>%
      ungroup() %>%
      filter(district == districts[d]) %>%
      select(county_index) %>%
      pull()
  }

  #getc_d will get all the counties for obs where there is only 1 county but district is still listed
  getallc_i <- getallc_d[getd_i]
  getallc_i[(npre09+1):n] <- getc_i[(npre09+1):n]
  getlengthc_i <- getallc_i %>% lapply(length) %>% unlist()
  maxcounties <- getlengthc_i %>% max
  getallc_imatrix <- matrix(NA, nrow = length(getallc_i), ncol = maxcounties)
  for(i in 1:n) {
    getallc_imatrix[i,1:getlengthc_i[i]] <- getallc_i[[i]]
  }

  

  
  # mig age specific
  
  m09 <- ipums_migration %>% filter(year == 2009) %>% nrow()
  mpre09 <- ipums_migration %>% filter(year != 2009) %>% nrow()
  m <- m09 + mpre09
  geta_j <- getind(ipums_migration, "age_group", age_groups)
  gett_j <- getind(ipums_migration, "year", years)
  getd_j <- getind(ipums_migration, "district", districts)
  getc_j <- getind(ipums_migration, "county", counties)
  district_c <- ipums_migration %>%
    ungroup() %>%
    filter(!is.na(county)) %>%
    select(district, county) %>%
    distinct() %>%
    mutate(county_index = 1:n())
  getallc_d <- list()
  for (d in 1:ndistricts) {
    getallc_d[[d]] <- district_c %>%
      ungroup() %>%
      filter(district == districts[d]) %>%
      select(county_index) %>%
      pull()
  }

  getallc_j <- getallc_d[getd_j]
  getallc_j[(mpre09+1):m] <- getc_j[(mpre09+1):m]
  getlengthc_j <- getallc_j %>% lapply(length) %>% unlist()
  maxcounties <- getlengthc_j %>% max
  getallc_jmatrix <- matrix(NA, nrow = length(getallc_j), ncol = maxcounties)
  for(i in 1:m) {
    getallc_jmatrix[i,1:getlengthc_j[i]] <- getallc_j[[i]]
  }
  
  return(
    list(
      n = n,
      npre09 = npre09,
      geta_i = geta_i,
      getd_i = getd_i,
      gett_i = gett_i,
      getallc_i = getallc_i,
      getlengthc_i = getlengthc_i,
      getallc_imatrix = getallc_imatrix,
      getc_i = getc_i,
      m = m,
      mpre09 = mpre09,
      geta_j = geta_j,
      gett_j = gett_j,
      getallc_j = getallc_j,
      getlengthc_j = getlengthc_j,
      getallc_jmatrix = getallc_jmatrix,
      getc_j = getc_j
    )
  )
}

getind <- function(data, var, values) {
  temp <- data %>% select(var) %>% pull()
  index <- sapply(1:length(temp), function(i) {
    temp2 <- which(values == temp[i])
    if (length(temp2) == 0) {
      temp2 <- NA
    }
    return(temp2)
  })
  return(unlist(index))
}
