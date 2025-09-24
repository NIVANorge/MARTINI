
param_unique <- filenames %>% tolower()
for(folder in scenario_folders){
  param_unique <- param_unique %>%
    sapply(stringr::str_remove, tolower(folder)) 
}
for(folder in scenario_ids){
  if(folder!=""){
    param_unique <- param_unique %>%
      sapply(stringr::str_remove, tolower(folder)) 
  }
}

param_unique <- param_unique %>%
  sapply(stringr::str_remove, "\\A_") 


param_unique <- param_unique %>%
  unlist() %>% 
  unique()

param_unique
