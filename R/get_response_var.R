# for creating data set to input to rf_variable_importance.Rmd and shiny 

# factors: 
# scen: irrigation scenario, from baseline, 25%, 50%, 75%, 90% 
# veg: tree type, quag, plra, piun, pica
# clim: historic and 2 degrees warmer 

# read in data 
setwd("~/Documents/patches/scripts")
# carbon csv 0
rz_e0 <- read.csv("../out/rz_irr/evrgrn/all_carbon_hist", sep="") 
rz_d0 <- read.csv("../out/rz_irr/all_carbon_hist", sep="")
# carbon csv 2
rz_e2 <- read.csv("../out/rz_irr/evrgrn/all_carbon_2C", sep="")
rz_d2 <- read.csv("../out/rz_irr/all_carbon_2C", sep="") %>% mutate(clim="2C")

carbon <- rbind(rz_e0, rz_e2, rz_d0, rz_d2) 

# water csv 0
wat_e0 <- read.csv("../out/rz_irr/evrgrn/all_wat_hist", sep="") 
wat_d0 <- read.csv("../out/rz_irr/all_wat_hist", sep="")
# water csv 2
wat_e2 <- read.csv("../out/rz_irr/evrgrn/all_wat_2C", sep="")
wat_d2 <- read.csv("../out/rz_irr/all_wat_2C", sep="") %>%
  mutate(clim="2C")

wat <- rbind(wat_e0, wat_d0, wat_e2, wat_d2)

allscen = left_join(carbon, wat, by=c('basinID','veg','wy','wyd','yd','run','scen','date','year','month','day',"clim")) %>%
  group_by(scen, clim, veg, run) %>% 
  mutate(irr = total_water_in - rain) %>%
  mutate(et = transpiration_sat_zone+transpiration_unsat_zone+evaporation,
         trans = transpiration_sat_zone+transpiration_unsat_zone,
         resp = cdf.total_mr + cdf.total_gr) 

allscen$scen = factor(allscen$scen, levels=c("baseline","25","50","75","90"))
allscen$clim = factor(allscen$clim, levels=c("hist","2C"))
allscen$veg = factor(allscen$veg)


# summarize by wy 
annual_wy <- allscen %>% 
  group_by(wy, scen, clim, veg, run) %>% 
  summarise(npp=sum(npp), 
            et=sum(et), 
            trans=sum(trans), 
            evap=sum(evaporation),
            mean_dailyCstore = mean(cs.totalc),
            mean_cpool = mean(cs.cpool),
            mean_availc = mean(cs.availc),
            psn=sum(cdf.psn_to_cpool),
            lai=mean(lai),
            resp=sum(resp),
            rain = sum(rain),
            psi = mean(epv.psi),
            total_water = sum(total_water_in),
            sat_def = mean(sat_deficit),
            sat_store = mean(sat_zone_storage),
            rz_storage = mean(rz_storage),
            rz_fc = mean(rootzone.field_capacity),
            recharge = sum(recharge)) %>% 
  mutate(wue_eco=npp/et,
       wue_plant=npp/trans) %>%
  group_by(wy, scen, clim, veg) %>%
  summarise(wue_eco = mean(wue_eco),
            wue_plant=mean(wue_plant),
            npp=mean(npp), 
            et=mean(et), 
            trans=mean(trans), 
            evap=mean(evap),
            mean_dailyCstore = mean(mean_dailyCstore),
            mean_cpool = mean(mean_cpool),
            mean_availc = mean(mean_availc),
            psn=mean(psn),
            lai=mean(lai),
            resp=mean(resp),
            rain = mean(rain),
            psi = mean(psi),
            total_water = mean(total_water),
            sat_def = mean(sat_def),
            sat_store = mean(sat_store),
            rz_storage = mean(rz_storage),
            rz_fc = mean(rz_fc),
            recharge = mean(recharge))

varnames <- names(dplyr::select(ungroup(annual_wy), !wy & !where(is.factor)))

### get response variable ###
# var: put in variable you want 
# timespan: year to look at - could be two years for a range, or 1 year to filter - or nothing for averaging over the full data set 
# func: whatever you want summarize with 
get_response_var <- function(df,
                             var, 
                             timespan=NULL,
                             func=NULL,
                             factors=c("clim", "scen", "veg")){
  
  varnames <- names(dplyr::select(ungroup(df), !wy & !where(is.factor)))
  varnames = varnames[varnames!=var]
  
  
  if(is.null(timespan)){
    response = df %>% 
      group_by(across(all_of(factors))) %>% 
      summarise_at(vars(var, all_of(varnames)), list(func)) %>%
      dplyr::select(all_of(factors),
                    response=var,
                    all_of(varnames))
  }
  
  if(length(timespan)==1){
    response = df %>% 
      dplyr::filter(wy==timespan) %>%
      group_by(across(all_of(factors))) %>% 
      summarise_at(vars(var, all_of(varnames)), list(func)) %>%
      dplyr::select(all_of(factors),
                    response=var,
                    all_of(varnames))
  }
  
  if(length(timespan)==2){
    response = df %>% 
      dplyr::filter(wy>=timespan[1] &
                      wy<=timespan[2]) %>%
      group_by(across(all_of(factors))) %>% 
      summarise_at(vars(var, all_of(varnames)), list(func)) %>%
      dplyr::select(all_of(factors),
                    response=var,
                    all_of(varnames))
  }
  
  return(response)
}

# test function 
tmp <- get_response_var(df=annual_wy, 
                        var="npp",
                        timespan = c(2010, 2020),
                        func="mean")

# collect some response vars and save data frames in data/
r_npp_clim <- get_response_var(df=annual_wy,
                                     var="npp",
                                     timespan=c(2012,2016),
func="mean") 

r_wue_eco_0 = r_npp_clim %>% dplyr::filter(clim=="hist")%>% ungroup() %>% 
  dplyr::select(-clim, -wue_eco, -wue_plant)
r_wue_eco_2 = r_npp_clim %>% dplyr::filter(clim=="2C") %>% ungroup() %>% 
  dplyr::select(-clim, -wue_eco, -wue_plant)

# r_wue_plant_2011 <- get_response_var(df=annual_wy,
#                                      var="wue_plant",
#                                      timespan=c(2011),
#                                      func="mean")
# 
# r_wue_plant_2015 <- get_response_var(df=annual_wy,
#                                      var="wue_plant",
#                                      timespan=c(2015),
#                                      func="mean")
# 
# r_wue_plant_2017 <- get_response_var(df=annual_wy,
#                                      var="wue_plant",
#                                      timespan=c(2017),
#                                      func="mean")

# add response for NPP resilience for sum of wy (i.e. single value for each run equals npp17/npp11) 

setwd("~/RHESSysML/data/")
# save(annual_wy, 
#      r_wue_eco_drtyrs,
#      r_wue_plant_2011,
#      r_wue_plant_2015,
#      r_wue_plant_2017,
#      file="rf_wue.RDS")

save(annual_wy,
     r_wue_eco_0,
     r_wue_eco_2,
     file="rf_wue_by_clim.RDS")

# future: add temperature as a variable or specific ecophys parameters instead of veg
