model_format_ipums_counts <- function(core_data){
  y_i <- core_data$ipums_population %>% select(population) %>% pull() %>% `/`(1000)
  logy_i <- log(y_i)
  tau_i =   1 / (0.1 * 0.9 / y_i)
  
  return(list(logy_i = logy_i, tau_i = tau_i))
}