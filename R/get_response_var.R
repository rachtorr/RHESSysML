# for creating data set to input to rf_variable_importance.Rmd and shiny 

# factors: 
# scen: irrigation scenario, from baseline, 25%, 50%, 75%, 90% 
# veg: tree type, quag, plra, piun, pica
# clim: historic and 2 degrees warmer 

# read in data 
# carbon csv 0
# water csv 0
# carbon csv 2
# water csv 2 

# rbind 


# summarize by wy 
annual_wy <- allscen %>% 
  group_by(wy, scen, clim, veg) %>% 
  summarize(npp=sum(npp), et=sum(et), trans=sum(trans), mean_dailyCstore = mean(cs.totalc)) %>% 
  mutate(wue_eco=npp/et,
       wue_plant=npp/trans)

### get response variable ###
# var: put in variable you want 
# timespan: year to look at - could be two years for a range, or 1 year to filter - or nothing for averaging over the full data set 
# func: whatever you want summarize with 
get_response_var <- function(df,
                             var, 
                             timespan=NULL,
                             func=NULL){
  if(is.null(timespan)){
    response = df %>% 
      group_by(clim, scen, veg) %>% 
      summarise_at(vars(var), list(func))
  }
  
  if(length(timespan)==1){
    response = df %>% 
      dplyr::filter(wy==timespan) %>%
      group_by(clim, scen, veg) %>% 
      summarise_at(vars(var), list(func))
  }
  
  if(length(timespan)==2){
    response = df %>% 
      dplyr::filter(wy>=timespan[1] &
                      wy<=timespan[2]) %>%
      group_by(clim, scen, veg) %>% 
      summarise_at(vars(var), list(func))
  }
  
  return(response)
}



