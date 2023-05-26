options(scipen=999)
library(gtools)
library(data.table)
library(dplyr)
library(prime)
library(countrycode)

# load(file = 'data/population/incomegroup.rdata')
# load('output/prime/results_prime_df_1931to2120.rdata')
# 
# 
# source('code/2_dataPrep_hpvadviseUganda_indirect_all.r')

load('../data_hpvadvise.rdata')
run = read.table(file = 'run.txt')[[1]]
cat(run)
impact_hpvadviseUganda = list()


# calculate direct + indrect effects from the vaccine

if(run==1)
{
  # strategy_life = list()
  time_start = Sys.time()
  for(i in 1:length(s_life_hpvadviseUganda))
  {
    print(i)
    index = i
    d  = estimateVaccineImpact_indirecteffects(DATALIST = s_life_hpvadviseUganda[[index]])
    scenario = 'strategy_life'
    d = data.frame(d)#,total_vaccinated = vaccine_impact_0$total_vaccinated)#cbind(vaccine_impact_0,vaccine_impact_hpvadviseUganda[[scenario]][[run]])
    d = d[d$calendar_year > 2019,] #& d$calendar_year < 2081,]
    impact_hpvadviseUganda[[index]] = summariseVaccineImpact(d=d,RUN = index)
    rm(d)
  }
  time_end = Sys.time()
  
}

if(run==2)
{
  time_start = Sys.time()
  for(i in 1:length(s_life_hpvadviseUganda))
  {
    print(i)
    index = i
    d  = estimateVaccineImpact_indirecteffects(DATALIST = s_20y_hpvadviseUganda[[index]])
    scenario = 'strategy_20y'
    d = data.frame(d)#,total_vaccinated = vaccine_impact_0$total_vaccinated)#cbind(vaccine_impact_0,vaccine_impact_hpvadviseUganda[[scenario]][[run]])
    d = d[d$calendar_year > 2019,] #& d$calendar_year < 2081,]
    impact_hpvadviseUganda[[index]] = summariseVaccineImpact(d=d,RUN = index)
    rm(d)
  }
  time_end = Sys.time()
}

if(run==3)
{
  time_start = Sys.time()
  for(i in 1:length(s_life_hpvadviseUganda))
  {
    print(i)
    index = i
    d  = estimateVaccineImpact_indirecteffects(DATALIST = s_30y_hpvadviseUganda[[index]])
    scenario = 'strategy_30y'
    d = data.frame(d)#,total_vaccinated = vaccine_impact_0$total_vaccinated)#cbind(vaccine_impact_0,vaccine_impact_hpvadviseUganda[[scenario]][[run]])
    d = d[d$calendar_year > 2019,] #& d$calendar_year < 2081,]
    impact_hpvadviseUganda[[index]] = summariseVaccineImpact(d=d,RUN = index)
    rm(d)
  }
  time_end = Sys.time()
}


if(run==4)
{
  time_start = Sys.time()
  for(i in 1:length(s_life_hpvadviseUganda))
  {
    print(i)
    index = i
    d  = estimateVaccineImpact_indirecteffects(DATALIST = s_life80take_hpvadviseUganda[[index]])
    scenario = 'strategy_life80take'
    d = data.frame(d)#,total_vaccinated = vaccine_impact_0$total_vaccinated)#cbind(vaccine_impact_0,vaccine_impact_hpvadviseUganda[[scenario]][[run]])
    d = d[d$calendar_year > 2019,] #& d$calendar_year < 2081,]
    impact_hpvadviseUganda[[index]] = summariseVaccineImpact(d=d,RUN = index)
    rm(d)
  }
  time_end = Sys.time()
}


print(time_end - time_start)
save(impact_hpvadviseUganda,file = paste0('impact_hpvadviseUganda',run,'.rdata'))

