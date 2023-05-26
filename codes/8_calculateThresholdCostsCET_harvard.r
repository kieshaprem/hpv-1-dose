source('code/functions_supporting.r')
load('output/costs/costs_2022-05-24.rdata')

# model = 'harvardElSalvador'
# load(paste0('output/prime/impact_',model,'_indirect_batch.rdata'))

for(COUNTRY in c('ElSalvador','Nicaragua','Uganda','US')) load(paste0('output/prime/impact_harvard',COUNTRY,'_indirect_batch.rdata'))
IMPACTS = list(impact_harvardUganda,impact_harvardElSalvador,impact_harvardNicaragua,impact_harvardUS)
names(IMPACTS) = c('Uganda','ElSalvador','Nicaragua','US')



d = data.frame(year =2020:2209, discount = discount(r = 0.03,STARTYEAR = 2020,LASTYEAR = 2209))

countries = as.character(unique(IMPACTS[[1]][[1]][[1]]$iso3c))
term1 = term1_dish = term2 = term2_disc = term2_disc_dish = total_vaccinated = total_vaccinated_disc = list()
dataall = matrix(NA, nrow = length(countries),ncol = length(IMPACTS[[1]]))
rownames(dataall) = countries
colnames(dataall) = names(IMPACTS[[1]])


for(COUNTRY in c('ElSalvador','Nicaragua','Uganda','US')) 
{
  print(COUNTRY)
  IMPACT = IMPACTS[[COUNTRY]]
  for(R in 1:length(IMPACT[[1]]))
  {
    term1_data = term1_dish_data = term2_data = term2_disc_data = term2_disc_dish_data = total_vaccinated_data = total_vaccinated_disc_data = dataall
    for(SCEN in 1:4)
    {
      IMPACT[[SCEN]][[R]]$discount = d$discount[match(IMPACT[[SCEN]][[R]]$year,d$year)]
      IMPACT[[SCEN]][[R]] = cbind(IMPACT[[SCEN]][[R]],costs[match(IMPACT[[SCEN]][[R]]$iso3c,rownames(costs)),])
      IMPACT[[SCEN]][[R]]$cases_averted_disc = IMPACT[[SCEN]][[R]]$cases_averted*IMPACT[[SCEN]][[R]]$discount 
      IMPACT[[SCEN]][[R]]$deaths_averted_disc = IMPACT[[SCEN]][[R]]$deaths_averted*IMPACT[[SCEN]][[R]]$discount 
      IMPACT[[SCEN]][[R]]$dalys_averted_disc = IMPACT[[SCEN]][[R]]$dalys_averted*IMPACT[[SCEN]][[R]]$discount 
      
      IMPACT[[SCEN]][[R]]$dalys_averted_gdp = IMPACT[[SCEN]][[R]]$dalys_averted*IMPACT[[SCEN]][[R]]$cet 
      IMPACT[[SCEN]][[R]]$dalys_averted_gdp_dish = IMPACT[[SCEN]][[R]]$dalys_averted_disc*IMPACT[[SCEN]][[R]]$cet 
      
      IMPACT[[SCEN]][[R]]$cases_averted_trtcost = IMPACT[[SCEN]][[R]]$cases_averted*IMPACT[[SCEN]][[R]]$treatment_cost 
      IMPACT[[SCEN]][[R]]$cases_averted_trtcost_dish = IMPACT[[SCEN]][[R]]$cases_averted_disc*IMPACT[[SCEN]][[R]]$treatment_cost 
      IMPACT[[SCEN]][[R]]$cases_averted_trtcost_dish_disc = IMPACT[[SCEN]][[R]]$cases_averted_disc*IMPACT[[SCEN]][[R]]$treatment_cost*IMPACT[[SCEN]][[R]]$discount 
      IMPACT[[SCEN]][[R]]$cases_averted_trtcost_disc = IMPACT[[SCEN]][[R]]$cases_averted*IMPACT[[SCEN]][[R]]$treatment_cost*IMPACT[[SCEN]][[R]]$discount 
      
      IMPACT[[SCEN]][[R]]$total_vaccinated_disc = IMPACT[[SCEN]][[R]]$total_vaccinated*IMPACT[[SCEN]][[R]]$discount
      
      TEMP = aggregate(IMPACT[[SCEN]][[R]]$dalys_averted_gdp,by = list(IMPACT[[SCEN]][[R]]$iso3c),sum)
      term1_data[,SCEN] = TEMP[match(rownames(term1_data),TEMP[,1]),2]
      rm(TEMP)
      
      TEMP = aggregate(IMPACT[[SCEN]][[R]]$dalys_averted_gdp_dish,by = list(IMPACT[[SCEN]][[R]]$iso3c),sum)
      term1_dish_data[,SCEN] = TEMP[match(rownames(term1_data),TEMP[,1]),2]
      rm(TEMP)
      
      TEMP = aggregate(IMPACT[[SCEN]][[R]]$cases_averted_trtcost,by = list(IMPACT[[SCEN]][[R]]$iso3c),sum)
      term2_data[,SCEN] = TEMP[match(rownames(term1_data),TEMP[,1]),2]
      rm(TEMP)
      
      TEMP = aggregate(IMPACT[[SCEN]][[R]]$cases_averted_trtcost_disc,by = list(IMPACT[[SCEN]][[R]]$iso3c),sum)
      term2_disc_data[,SCEN] = TEMP[match(rownames(term1_data),TEMP[,1]),2]
      rm(TEMP)
      
      TEMP = aggregate(IMPACT[[SCEN]][[R]]$cases_averted_trtcost_dish_disc,by = list(IMPACT[[SCEN]][[R]]$iso3c),sum)
      term2_disc_dish_data[,SCEN] = TEMP[match(rownames(term1_data),TEMP[,1]),2]
      rm(TEMP)
      
      TEMP = aggregate(IMPACT[[SCEN]][[R]]$total_vaccinated,by = list(IMPACT[[SCEN]][[R]]$iso3c),sum)
      total_vaccinated_data[,SCEN] = TEMP[match(rownames(term1_data),TEMP[,1]),2]
      rm(TEMP)
      
      TEMP = aggregate(IMPACT[[SCEN]][[R]]$total_vaccinated_disc,by = list(IMPACT[[SCEN]][[R]]$iso3c),sum)
      total_vaccinated_disc_data[,SCEN] = TEMP[match(rownames(term1_data),TEMP[,1]),2]
      rm(TEMP)
      
    }
    term1[[R]] = term1_data
    term1_dish[[R]] = term1_dish_data
    term2[[R]] = term2_data
    term2_disc[[R]] = term2_disc_data
    term2_disc_dish[[R]] = term2_disc_dish_data
    total_vaccinated[[R]] = total_vaccinated_data
    total_vaccinated_disc[[R]] = total_vaccinated_disc_data
    rm(term1_data, term1_dish_data,term2_data,term2_disc_data,term2_disc_dish_data, total_vaccinated_data,total_vaccinated_disc_data)
  }
  
  # Compute threshold costs
  threshold_costs = threshold_costs_disc = threshold_costs_disc_dish = list()
  
  for(R in 1:length(IMPACT[[1]]))
  {
    threshold_costs[[R]] = threshold_costs_disc[[R]] = threshold_costs_disc_dish[[R]] = data.frame(iso3c = countries,
                                                                                                   threshold_cost_A = NA,
                                                                                                   threshold_cost_B = NA,
                                                                                                   threshold_cost_C = NA,
                                                                                                   threshold_cost_D = NA,
                                                                                                   threshold_cost_E = NA,
                                                                                                   threshold_cost_F = NA)
    
    threshold_costs[[R]]$threshold_cost_A = (term1[[R]][,2] + term2[[R]][,2])/total_vaccinated[[R]][,2]
    threshold_costs[[R]]$threshold_cost_B = (term1[[R]][,3] + term2[[R]][,3])/total_vaccinated[[R]][,3]
    threshold_costs[[R]]$threshold_cost_C = (term1[[R]][,4] + term2[[R]][,4])/total_vaccinated[[R]][,4]
    threshold_costs[[R]]$threshold_cost_D = (term1[[R]][,1] - term1[[R]][,2] + term2[[R]][,1] - term2[[R]][,2])/total_vaccinated[[R]][,1]
    threshold_costs[[R]]$threshold_cost_E = (term1[[R]][,1] - term1[[R]][,3] + term2[[R]][,1] - term2[[R]][,3])/total_vaccinated[[R]][,1]
    threshold_costs[[R]]$threshold_cost_F = (term1[[R]][,1] - term1[[R]][,4] + term2[[R]][,1] - term2[[R]][,4])/total_vaccinated[[R]][,1]
    
    threshold_costs_disc[[R]]$threshold_cost_A = (term1[[R]][,2] + term2_disc[[R]][,2])/total_vaccinated_disc[[R]][,2]
    threshold_costs_disc[[R]]$threshold_cost_B = (term1[[R]][,3] + term2_disc[[R]][,3])/total_vaccinated_disc[[R]][,3]
    threshold_costs_disc[[R]]$threshold_cost_C = (term1[[R]][,4] + term2_disc[[R]][,4])/total_vaccinated_disc[[R]][,4]
    threshold_costs_disc[[R]]$threshold_cost_D = (term1[[R]][,1] - term1[[R]][,2] + term2_disc[[R]][,1] - term2_disc[[R]][,2])/total_vaccinated_disc[[R]][,1]
    threshold_costs_disc[[R]]$threshold_cost_E = (term1[[R]][,1] - term1[[R]][,3] + term2_disc[[R]][,1] - term2_disc[[R]][,3])/total_vaccinated_disc[[R]][,1]
    threshold_costs_disc[[R]]$threshold_cost_F = (term1[[R]][,1] - term1[[R]][,4] + term2_disc[[R]][,1] - term2_disc[[R]][,4])/total_vaccinated_disc[[R]][,1]
    
    threshold_costs_disc_dish[[R]]$threshold_cost_A = (term1_dish[[R]][,2] + term2_disc_dish[[R]][,2])/total_vaccinated_disc[[R]][,2]
    threshold_costs_disc_dish[[R]]$threshold_cost_B = (term1_dish[[R]][,3] + term2_disc_dish[[R]][,3])/total_vaccinated_disc[[R]][,3]
    threshold_costs_disc_dish[[R]]$threshold_cost_C = (term1_dish[[R]][,4] + term2_disc_dish[[R]][,4])/total_vaccinated_disc[[R]][,4]
    threshold_costs_disc_dish[[R]]$threshold_cost_D = (term1_dish[[R]][,1] - term1_dish[[R]][,2] + term2_disc_dish[[R]][,1] - term2_disc_dish[[R]][,2])/total_vaccinated_disc[[R]][,1]
    threshold_costs_disc_dish[[R]]$threshold_cost_E = (term1_dish[[R]][,1] - term1_dish[[R]][,3] + term2_disc_dish[[R]][,1] - term2_disc_dish[[R]][,3])/total_vaccinated_disc[[R]][,1]
    threshold_costs_disc_dish[[R]]$threshold_cost_F = (term1_dish[[R]][,1] - term1_dish[[R]][,4] + term2_disc_dish[[R]][,1] - term2_disc_dish[[R]][,4])/total_vaccinated_disc[[R]][,1]
    
  }
  
  
  threshold = list(threshold_costs = threshold_costs,threshold_costs_disc = threshold_costs_disc,threshold_costs_disc_dish = threshold_costs_disc_dish)
  
  save(threshold,file=paste0('output/costs/threshold_cet_harvard',COUNTRY,'.rdata'))
}
