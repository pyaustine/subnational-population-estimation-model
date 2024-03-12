model_format_wpp <- function(core_data, wppyears) {
  wpp_at <- core_data$wpp %>%
    dplyr::filter(year %in% wppyears) %>%
    spread(year, population, fill = 0) %>%
    select(-age_group) %>%
    as.matrix()
  return(wpp_at)
}