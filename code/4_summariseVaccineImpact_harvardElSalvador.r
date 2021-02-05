library(gtools)
library(data.table)
library(dplyr)
library(prime)
library(countrycode)

load(file = 'data/population/incomegroup.rdata')
load(file = 'output/prime/vaccine_impact_0.rdata')

impact_harvardElSalvador = list(protect_life=list(),
                            protect_20y=list(),
                            protect_30y=list(),
                            protect_life80take = list())


summariseVaccineImpact = function(d,RUN,MODEL='harvardElSalvador')
{
  years = seq(min(d$calendar_year),max(d$calendar_year),1)
  country_list =  as.character(unique(d$iso3c))
  incomegroup_list = incomegroup$incomegroup[match(country_list,incomegroup$iso3c)]
  data_cases_averted = data.frame(data=MODEL,
                                  run = RUN,
                                  scenario = tolower(scenario),
                                  iso3c = rep(country_list,each=length(years)),
                                  incomegroup = rep(incomegroup_list,each=length(years)),
                                  year = rep(years,times = (length(country_list))),
                                  
                                  prevac_cases_all = NA,
                                  prevac_deaths_all = NA,
                                  prevac_dalys_all = NA,
                                  
                                  cases_averted = NA,
                                  deaths_averted = NA,
                                  dalys_averted = NA,
                                  
                                  cases_averted_disc = NA,
                                  deaths_averted_disc = NA,
                                  dalys_averted_disc = NA,
                                  
                                  postvac_cases_all = NA,
                                  postvac_deaths_all = NA,
                                  postvac_dalys_all = NA,
                                  
                                  total_vaccinated = NA)
  
  aggregated_data = list()
  # ASK MARK: calendar_year or VACCINATION YEAR? 
  aggregated_data[[1]] =  aggregate(x = c(d$prevac_cases_all), by = list(d$calendar_year,d$iso3c), FUN = "sum")
  aggregated_data[[2]] =  aggregate(x = c(d$prevac_deaths_all), by = list(d$calendar_year,d$iso3c), FUN = "sum")
  aggregated_data[[3]] =  aggregate(x = c(d$prevac_dalys_all), by = list(d$calendar_year,d$iso3c), FUN = "sum")
  
  aggregated_data[[4]] =  aggregate(x = c(d$cases_averted), by = list(d$calendar_year,d$iso3c), FUN = "sum")
  aggregated_data[[5]] =  aggregate(x = c(d$deaths_averted), by = list(d$calendar_year,d$iso3c), FUN = "sum")
  aggregated_data[[6]] =  aggregate(x = c(d$dalys_averted), by = list(d$calendar_year,d$iso3c), FUN = "sum")
  aggregated_data[[7]] =  aggregate(x = c(d$cases_averted_disc), by = list(d$calendar_year,d$iso3c), FUN = "sum")
  aggregated_data[[8]] =  aggregate(x = c(d$deaths_averted_disc), by = list(d$calendar_year,d$iso3c), FUN = "sum")
  aggregated_data[[9]] =  aggregate(x = c(d$dalys_averted_disc), by = list(d$calendar_year,d$iso3c), FUN = "sum")
  
  aggregated_data[[10]] =  aggregate(x = c(d$postvac_cases_all), by = list(d$calendar_year,d$iso3c), FUN = "sum")
  aggregated_data[[11]] =  aggregate(x = c(d$postvac_deaths_all), by = list(d$calendar_year,d$iso3c), FUN = "sum")
  aggregated_data[[12]] =  aggregate(x = c(d$postvac_dalys_all), by = list(d$calendar_year,d$iso3c), FUN = "sum")
  
  aggregated_data[[13]] =  aggregate(x = c(d$total_vaccinated), by = list(d$calendar_year,d$iso3c), FUN = "sum")
  
  names(aggregated_data) = c('prevac_cases_all','prevac_deaths_all','prevac_dalys_all','cases_averted',
                             'deaths_averted','dalys_averted','cases_averted_disc','deaths_averted_disc',
                             'dalys_averted_disc','postvac_cases_all','postvac_deaths_all','postvac_dalys_all','total_vaccinated')
  
  for(co in country_list)
  {
    # match(aggregated_data$prevac_cases_all$Group.1[aggregated_data$prevac_cases_all$Group.2 %in% co],c(data_cases_averted$year[data_cases_averted$iso3c %in% co]))
    # print(co)
    data_cases_averted$prevac_cases_all[data_cases_averted$iso3c %in% co] =  aggregated_data[[1]]$x[aggregated_data[[1]]$Group.2 %in% co]
    data_cases_averted$prevac_deaths_all[data_cases_averted$iso3c %in% co] =  aggregated_data[[2]]$x[aggregated_data[[2]]$Group.2 %in% co]
    data_cases_averted$prevac_dalys_all[data_cases_averted$iso3c %in% co] =  aggregated_data[[3]]$x[aggregated_data[[3]]$Group.2 %in% co]
    
    data_cases_averted$cases_averted[data_cases_averted$iso3c %in% co] =  aggregated_data[[4]]$x[aggregated_data[[4]]$Group.2 %in% co]
    data_cases_averted$deaths_averted[data_cases_averted$iso3c %in% co] =  aggregated_data[[5]]$x[aggregated_data[[5]]$Group.2 %in% co]
    data_cases_averted$dalys_averted[data_cases_averted$iso3c %in% co] =  aggregated_data[[6]]$x[aggregated_data[[6]]$Group.2 %in% co]
    
    data_cases_averted$cases_averted_disc[data_cases_averted$iso3c %in% co] =  aggregated_data[[7]]$x[aggregated_data[[7]]$Group.2 %in% co]
    data_cases_averted$deaths_averted_disc[data_cases_averted$iso3c %in% co] =  aggregated_data[[8]]$x[aggregated_data[[8]]$Group.2 %in% co]
    data_cases_averted$dalys_averted_disc[data_cases_averted$iso3c %in% co] =  aggregated_data[[9]]$x[aggregated_data[[9]]$Group.2 %in% co]
    
    data_cases_averted$postvac_cases_all[data_cases_averted$iso3c %in% co] =  aggregated_data[[10]]$x[aggregated_data[[10]]$Group.2 %in% co]
    data_cases_averted$postvac_deaths_all[data_cases_averted$iso3c %in% co] =  aggregated_data[[11]]$x[aggregated_data[[11]]$Group.2 %in% co]
    data_cases_averted$postvac_dalys_all[data_cases_averted$iso3c %in% co] =  aggregated_data[[12]]$x[aggregated_data[[12]]$Group.2 %in% co]
    
    data_cases_averted$total_vaccinated[data_cases_averted$iso3c %in% co] =  aggregated_data[[13]]$x[aggregated_data[[13]]$Group.2 %in% co]
  }
  return(data_cases_averted)
  
}



load(file = 'output/prime/vaccine_impact_harvardElSalvador_life.rdata')
vaccine_impact_harvardElSalvador = list(strategy_life = strategy_life)
scenario = 'strategy_life'
for(run in 1:3) 
{
  d = data.frame(vaccine_impact_harvardElSalvador[[scenario]][[run]],total_vaccinated = vaccine_impact_0$total_vaccinated)#cbind(vaccine_impact_0,vaccine_impact_harvardElSalvador[[scenario]][[run]])
  d = d[d$calendar_year > 2019,] #& d$calendar_year < 2081,]
  
  impact_harvardElSalvador$protect_life[[run]] = summariseVaccineImpact(d=d,RUN = run)
  rm(d)
  print(run)
}
rm(vaccine_impact_harvardElSalvador,run,strategy_life)


load(file = 'output/prime/vaccine_impact_harvardElSalvador_20y.rdata')
vaccine_impact_harvardElSalvador = list(strategy_20y = strategy_20y)
scenario = 'strategy_20y'
for(run in 1:3) 
{
  d = data.frame(vaccine_impact_harvardElSalvador[[scenario]][[run]],total_vaccinated = vaccine_impact_0$total_vaccinated)#cbind(vaccine_impact_0,vaccine_impact_harvardElSalvador[[scenario]][[run]])
  d = d[d$calendar_year > 2019,] #& d$calendar_year < 2081,]
  
  impact_harvardElSalvador$protect_20y[[run]] = summariseVaccineImpact(d=d,RUN = run)
  rm(d)
  print(run)
}
rm(vaccine_impact_harvardElSalvador,run,strategy_20y)


load(file = 'output/prime/vaccine_impact_harvardElSalvador_30y.rdata')
vaccine_impact_harvardElSalvador = list(strategy_30y = strategy_30y)
scenario = 'strategy_30y'
for(run in 1:3) 
{
  d = data.frame(vaccine_impact_harvardElSalvador[[scenario]][[run]],total_vaccinated = vaccine_impact_0$total_vaccinated)#cbind(vaccine_impact_0,vaccine_impact_harvardElSalvador[[scenario]][[run]])
  d = d[d$calendar_year > 2019,] #& d$calendar_year < 2081,]
  
  impact_harvardElSalvador$protect_30y[[run]] = summariseVaccineImpact(d=d,RUN = run)
  rm(d)
  print(run)
}
rm(vaccine_impact_harvardElSalvador,run,strategy_30y)


load(file = 'output/prime/vaccine_impact_harvardElSalvador_life80take.rdata')
vaccine_impact_harvardElSalvador = list(strategy_life80take = strategy_life80take)
scenario = 'strategy_life80take'
for(run in 1:3) 
{
  d = data.frame(vaccine_impact_harvardElSalvador[[scenario]][[run]],total_vaccinated = vaccine_impact_0$total_vaccinated)#cbind(vaccine_impact_0,vaccine_impact_harvardElSalvador[[scenario]][[run]])
  d = d[d$calendar_year > 2019,] #& d$calendar_year < 2081,]
  
  impact_harvardElSalvador$protect_life80take[[run]] = summariseVaccineImpact(d=d,RUN = run)
  rm(d)
  print(run)
}
rm(vaccine_impact_harvardElSalvador,run,strategy_life80take)

save(impact_harvardElSalvador,file = 'output/prime/impact_harvardElSalvador.rdata')
