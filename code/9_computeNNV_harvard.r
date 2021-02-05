source('code/functions_supporting.r')
load(file = 'data/population/totalvac.rdata')
for(COUNTRY in c('India','Uganda','ElSalvador','Nicaragua','US')) load(paste0('output/prime/impact_harvard',COUNTRY,'.rdata'))

sorter_harvard = function(IMPACT)
{
  ord = order(unlist(lapply(IMPACT$protect_life, function(X){sum(X$cases_averted)})))
  for(i in 1:4) IMPACT[[i]] = (IMPACT[[i]][ord])[c(2,1,3)]
  return(IMPACT)
}
impact_harvardElSalvador = sorter_harvard(impact_harvardElSalvador)
impact_harvardIndia = sorter_harvard(impact_harvardIndia)
impact_harvardNicaragua = sorter_harvard(impact_harvardNicaragua)
impact_harvardUganda = sorter_harvard(impact_harvardUganda)
impact_harvardUS = sorter_harvard(impact_harvardUS)

IMPACTS = list(impact_harvardIndia,impact_harvardUganda,impact_harvardElSalvador,impact_harvardNicaragua,impact_harvardUS)
names(IMPACTS) = c('India','Uganda','ElSalvador','Nicaragua','US')

for(COUNTRY in c('India','Uganda','ElSalvador','Nicaragua','US'))
{
  IMPACT = IMPACTS[[COUNTRY]]
  model = paste0('harvard', COUNTRY)
  countries = as.character(unique(IMPACT[[1]][[1]]$iso3c))
  d = data.frame(year =2018:2209, discount = discount(r = 0.03,STARTYEAR = 2020,LASTYEAR = 2209))
  
  
  no_to_vaccinate = no_to_vaccinate_dish = list()
  
  data = data.frame(matrix(NA, nrow = length(countries),ncol = 4))
  rownames(data) = countries
  colnames(data) = names(IMPACT) #c('iso3c',"zero to one dose (20y)", "zero to one dose (30y)", "zero to one dose (ve80)","one dose (20y) to two", "one dose (30y) to two","one dose (ve80) to two")
  dataall = data.frame(matrix(NA, nrow = length(countries),ncol = 7))
  rownames(dataall) = countries
  colnames(dataall) =  c('iso3c',"zero to one dose (20y)", "zero to one dose (30y)", "zero to one dose (ve80)","one dose (20y) to two", "one dose (30y) to two","one dose (ve80) to two")
  
  
  R = 1
  for(R in 1:length(IMPACT[[1]]))
  {
    total_vaccinated_data = total_cancers_averted = total_cancers_averted_dish = data
    for(SCEN in 1:4)
    {
      IMPACT[[SCEN]][[R]]$discount = d$discount[match(IMPACT[[SCEN]][[R]]$year,d$year)]
      IMPACT[[SCEN]][[R]]$cases_averted_dish = IMPACT[[SCEN]][[R]]$cases_averted*IMPACT[[SCEN]][[R]]$discount 
      
      
      # TEMP = aggregate(IMPACT[[SCEN]][[R]]$total_vaccinated,by = list(IMPACT[[SCEN]][[R]]$iso3c),sum)
      # total_vaccinated_data[,SCEN] = TEMP[match(rownames(total_vaccinated_data),TEMP[,1]),2]
      # rm(TEMP)
      total_vaccinated_data[,SCEN] = rowSums(totalvac[,-1])[match(rownames(total_vaccinated_data),totalvac[,1])]
      
      TEMP = aggregate(IMPACT[[SCEN]][[R]]$cases_averted,by = list(IMPACT[[SCEN]][[R]]$iso3c),sum)
      total_cancers_averted[,SCEN] = TEMP[match(rownames(total_cancers_averted),TEMP[,1]),2]
      rm(TEMP)
      
      TEMP = aggregate(IMPACT[[SCEN]][[R]]$cases_averted_dish,by = list(IMPACT[[SCEN]][[R]]$iso3c),sum)
      total_cancers_averted_dish[,SCEN] = TEMP[match(rownames(total_cancers_averted_dish),TEMP[,1]),2]
      rm(TEMP)
      
    }
    
    
    no_to_vaccinate[[R]] = dataall
    no_to_vaccinate[[R]][,1] = countries
    no_to_vaccinate[[R]][,2] =  total_vaccinated_data[,2]/total_cancers_averted[,2] #sum(country_sum$total_vaccinated_10y) / sum(country_sum$total_cancers_averted_10y)  # Zero to one dose (10y)
    no_to_vaccinate[[R]][,3] =  total_vaccinated_data[,3]/total_cancers_averted[,3] 
    no_to_vaccinate[[R]][,4] =  total_vaccinated_data[,4]/total_cancers_averted[,4] 
    no_to_vaccinate[[R]][,5] =  total_vaccinated_data[,1]/(total_cancers_averted[,1] - total_cancers_averted[,2]) 
    no_to_vaccinate[[R]][,6] =  total_vaccinated_data[,1]/(total_cancers_averted[,1] - total_cancers_averted[,3]) 
    no_to_vaccinate[[R]][,7] =  total_vaccinated_data[,1]/(total_cancers_averted[,1] - total_cancers_averted[,4]) 
    
    no_to_vaccinate_dish[[R]] = dataall
    no_to_vaccinate_dish[[R]][,1] = countries
    no_to_vaccinate_dish[[R]][,2] =  total_vaccinated_data[,2]/total_cancers_averted_dish[,2] #sum(country_sum$total_vaccinated_10y) / sum(country_sum$total_cancers_averted_10y)  # Zero to one dose (10y)
    no_to_vaccinate_dish[[R]][,3] =  total_vaccinated_data[,3]/total_cancers_averted_dish[,3] 
    no_to_vaccinate_dish[[R]][,4] =  total_vaccinated_data[,4]/total_cancers_averted_dish[,4] 
    no_to_vaccinate_dish[[R]][,5] =  total_vaccinated_data[,1]/(total_cancers_averted_dish[,1] - total_cancers_averted_dish[,2]) 
    no_to_vaccinate_dish[[R]][,6] =  total_vaccinated_data[,1]/(total_cancers_averted_dish[,1] - total_cancers_averted_dish[,3]) 
    no_to_vaccinate_dish[[R]][,7] =  total_vaccinated_data[,1]/(total_cancers_averted_dish[,1] - total_cancers_averted_dish[,4]) 
    # (sum(country_sum$total_vaccinated_life) / sum(country_sum$total_cancers_averted_life - country_sum$total_cancers_averted_10y))  # One dose (10y) to two dose
    
    rm(total_vaccinated_data, total_cancers_averted,total_cancers_averted_dish)
  }
  
  # str(no_to_vaccinate)
  no_to_vaccinate = no_to_vaccinate[c(1,2,3)]
  no_to_vaccinate_dish = no_to_vaccinate_dish[c(1,2,3)]
  
  save(no_to_vaccinate,file=paste0('output/nnv_',model,'.rdata'))
  save(no_to_vaccinate_dish,file=paste0('output/nnv_dish_',model,'.rdata'))
}
