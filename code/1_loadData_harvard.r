require(tidyverse)
require(readxl)

# •	Scenario 0: status quo
# •	Scenario 1: lifetime protection, vaccine efficacy (VE): 100% , 
# i.e. similar to the current two-dose assumptions
# •	Scenario 2: one-dose offers 20 years protection, VE: 100%
# •	Scenario 3: one-dose offers 30 years protection, VE: 100%
# •	Scenario 4: one-dose offers lifetime protection, VE: 80% 
# protection against persistent infection at 5-year time point. 
# This translates into 80% take (HPV-ADVISE and PHE) or some level of 
# degree of protection per partnership (Harvard).




readHarvard <- function(COUNTRY,SCEN){
   results = list()
   if(SCEN>0) results$median = read.csv(file = paste0('data/harvard/20201127/median_',COUNTRY,'_scen',SCEN,'.csv'))
   if(SCEN>0) results$q10 = read.csv(file = paste0('data/harvard/20201127/lb_',COUNTRY,'_scen',SCEN,'.csv'))
   if(SCEN>0) results$q90 = read.csv(file = paste0('data/harvard/20201127/ub_',COUNTRY,'_scen',SCEN,'.csv'))
   if(SCEN==0) results$median = read.csv(file = paste0('data/harvard/20201127/',COUNTRY,'_scen',SCEN,'.csv'))
   if(SCEN==0) results$q10 = read.csv(file = paste0('data/harvard/20201127/',COUNTRY,'_scen',SCEN,'.csv'))
   if(SCEN==0) results$q90 = read.csv(file = paste0('data/harvard/20201127/',COUNTRY,'_scen',SCEN,'.csv'))
   for(i in 1:length(results))
   {
      results[[i]] = data.frame(t(results[[i]][,-1]))
      results[[i]] = cbind(matrix(0,nrow = nrow(results[[i]]),ncol = 10),results[[i]])
      results[[i]] = rbind(results[[i]],results[[i]][nrow(results[[i]]),])
      colnames(results[[i]]) = paste0('age',0:99)
      row.names(results[[i]]) = paste0('year',2020:2120)
   }
   return(results)
}

harvardUganda = harvardIndia = harvardElSalvador = harvardNicaragua = harvardUS = list()

harvardUganda$strategy_life = readHarvard(COUNTRY = 'UGA',SCEN = '1')
harvardUganda$strategy_20y = readHarvard(COUNTRY = 'UGA',SCEN = '2')
harvardUganda$strategy_30y = readHarvard(COUNTRY = 'UGA',SCEN = '3')
harvardUganda$strategy_life80take = readHarvard(COUNTRY = 'UGA',SCEN = '4')
harvardUganda$strategy_novac = readHarvard(COUNTRY = 'UGA',SCEN = '0')

harvardIndia$strategy_life = readHarvard(COUNTRY = 'IND',SCEN = '1')
harvardIndia$strategy_20y = readHarvard(COUNTRY = 'IND',SCEN = '2')
harvardIndia$strategy_30y = readHarvard(COUNTRY = 'IND',SCEN = '3')
harvardIndia$strategy_life80take = readHarvard(COUNTRY = 'IND',SCEN = '4')
harvardIndia$strategy_novac = readHarvard(COUNTRY = 'IND',SCEN = '0')

harvardElSalvador$strategy_life = readHarvard(COUNTRY = 'ELS',SCEN = '1')
harvardElSalvador$strategy_20y = readHarvard(COUNTRY = 'ELS',SCEN = '2')
harvardElSalvador$strategy_30y = readHarvard(COUNTRY = 'ELS',SCEN = '3')
harvardElSalvador$strategy_life80take = readHarvard(COUNTRY = 'ELS',SCEN = '4')
harvardElSalvador$strategy_novac = readHarvard(COUNTRY = 'ELS',SCEN = '0')

harvardNicaragua$strategy_life = readHarvard(COUNTRY = 'NIC',SCEN = '1')
harvardNicaragua$strategy_20y = readHarvard(COUNTRY = 'NIC',SCEN = '2')
harvardNicaragua$strategy_30y = readHarvard(COUNTRY = 'NIC',SCEN = '3')
harvardNicaragua$strategy_life80take = readHarvard(COUNTRY = 'NIC',SCEN = '4')
harvardNicaragua$strategy_novac = readHarvard(COUNTRY = 'NIC',SCEN = '0')

harvardUS$strategy_life = readHarvard(COUNTRY = 'US',SCEN = '1')
harvardUS$strategy_20y = readHarvard(COUNTRY = 'US',SCEN = '2')
harvardUS$strategy_30y = readHarvard(COUNTRY = 'US',SCEN = '3')
harvardUS$strategy_life80take = readHarvard(COUNTRY = 'US',SCEN = '4')
harvardUS$strategy_novac = readHarvard(COUNTRY = 'US',SCEN = '0')

# readHarvard <- function(SCEN){
#    results = list()
#    
#    incidence_name <- paste0("data/harvard/1d_global_20200427/harvard_1d_SCENARIO",SCEN,"_CASES_RATES.xlsx")
#    results[["Uganda"]] = read_excel(incidence_name,sheet = "Uganda")
#    results[["Nicaragua"]] = read_excel(incidence_name,sheet = "Nicaragua")
#    results[["India"]] = read_excel(incidence_name,sheet = "India")
#    results[["ElSalvador"]] = read_excel(incidence_name,sheet = "El Salvador")
#    
#    
#    for(i in 1:length(results))
#    {
#       results[[i]] = data.frame(t(results[[i]][,-1]))
#       results[[i]] = cbind(matrix(0,nrow = nrow(results[[i]]),ncol = 10),results[[i]])
#       colnames(results[[i]]) = paste0('age',0:99)
#       row.names(results[[i]]) = paste0('year',2020:2120)
#    }
#    return(results)
# }
# s_life = readHarvard(SCEN = 1)
# harvardUganda$strategy_life = s_life$Uganda
# harvardIndia$strategy_life = s_life$India
# harvardElSalvador$strategy_life = s_life$ElSalvador
# harvardNicaragua$strategy_life = s_life$Nicaragua
# 
# s_20y = readHarvard(SCEN = 2)
# harvardUganda$strategy_20y = s_20y$Uganda
# harvardIndia$strategy_20y = s_20y$India
# harvardElSalvador$strategy_20y = s_20y$ElSalvador
# harvardNicaragua$strategy_20y = s_20y$Nicaragua
# 
# s_30y = readHarvard(SCEN = 3)
# harvardUganda$strategy_30y = s_30y$Uganda
# harvardIndia$strategy_30y = s_30y$India
# harvardElSalvador$strategy_30y = s_30y$ElSalvador
# harvardNicaragua$strategy_30y = s_30y$Nicaragua
# 
# s_life80take = readHarvard(SCEN = 4)
# harvardUganda$strategy_life80take = s_life80take$Uganda
# harvardIndia$strategy_life80take = s_life80take$India
# harvardElSalvador$strategy_life80take = s_life80take$ElSalvador
# harvardNicaragua$strategy_life80take = s_life80take$Nicaragua
# 
# s_novac = readHarvard(SCEN = 0)
# harvardUganda$strategy_novac = s_novac$Uganda
# harvardIndia$strategy_novac = s_novac$India
# harvardElSalvador$strategy_novac = s_novac$ElSalvador
# harvardNicaragua$strategy_novac = s_novac$Nicaragua
# 
# rm(readHarvard,s_life,s_life80take,s_novac,s_20y,s_30y)



# harvard_uga_inc = list()
# harvard_uga_mort= list()
# 
# harvard_ind_inc = list()
# harvard_ind_mort= list()
# 
# harvard_els_inc = list()
# harvard_els_mort= list()
# 
# harvard_nic_inc = list()
# harvard_nic_mort= list()
# 
# for(STRATEGY in 0:4)
# {
#   incidence_name <- (paste0("data/harvard/harvard_1D_S",STRATEGY,"_incidence_UGA_NIC_IND_ELS_20200311.xlsx"))
#   harvard_uga_inc[[STRATEGY+1]] = read_excel(incidence_name,sheet = "Uganda")
#   harvard_nic_inc[[STRATEGY+1]] = read_excel(incidence_name,sheet = "Nicaragua")
#   harvard_ind_inc[[STRATEGY+1]] = read_excel(incidence_name,sheet = "India")
#   harvard_els_inc[[STRATEGY+1]] = read_excel(incidence_name,sheet = "El Salvador")
#   
#   mortality_name <- (paste0("data/harvard/harvard_1D_S",STRATEGY,"_mortality_UGA_NIC_IND_ELS_20200311.xlsx"))
#   harvard_uga_mort[[STRATEGY+1]] = read_excel(mortality_name,sheet = "Uganda")
#   harvard_nic_mort[[STRATEGY+1]] = read_excel(mortality_name,sheet = "Nicaragua")
#   harvard_ind_mort[[STRATEGY+1]] = read_excel(mortality_name,sheet = "India")
#   harvard_els_mort[[STRATEGY+1]] = read_excel(mortality_name,sheet = "El Salvador")
# }
#  rm(incidence_name,mortality_name,STRATEGY) 
#   
#  
#  names(harvard_uga_inc) = names(harvard_uga_mort) = 
#    names(harvard_ind_inc) = names(harvard_ind_mort) =
#    names(harvard_els_inc) = names(harvard_els_mort) = 
#    names(harvard_nic_inc) = names(harvard_nic_mort) = c('strategy_statusquo','strategy_life','strategy_20y','strategy_30y','strategy_life80take')
#  
# s_life_harvard = list(UGA = harvard_uga_inc$strategy_life, 
#                       IND = harvard_ind_inc$strategy_life, 
#                       ELS = harvard_els_inc$strategy_life,
#                       NIC = harvard_nic_inc$strategy_life)
# 
# s_life_harvard_mortality = list(UGA = harvard_uga_mort$strategy_life, 
#                                 IND = harvard_ind_mort$strategy_life, 
#                                 ELS = harvard_els_mort$strategy_life,
#                                 NIC = harvard_nic_mort$strategy_life)
#  
# s_20y_harvard = list(UGA = harvard_uga_inc$strategy_20y, 
#                       IND = harvard_ind_inc$strategy_20y, 
#                       ELS = harvard_els_inc$strategy_20y,
#                       NIC = harvard_nic_inc$strategy_20y)
# 
# s_20y_harvard_mortality = list(UGA = harvard_uga_mort$strategy_20y, 
#                                 IND = harvard_ind_mort$strategy_20y, 
#                                 ELS = harvard_els_mort$strategy_20y,
#                                 NIC = harvard_nic_mort$strategy_20y)
# 
# s_30y_harvard = list(UGA = harvard_uga_inc$strategy_30y, 
#                       IND = harvard_ind_inc$strategy_30y, 
#                       ELS = harvard_els_inc$strategy_30y,
#                       NIC = harvard_nic_inc$strategy_30y)
# 
# s_30y_harvard_mortality = list(UGA = harvard_uga_mort$strategy_30y, 
#                                 IND = harvard_ind_mort$strategy_30y, 
#                                 ELS = harvard_els_mort$strategy_30y,
#                                 NIC = harvard_nic_mort$strategy_30y)
# 
# s_life80take_harvard = list(UGA = harvard_uga_inc$strategy_life80take, 
#                      IND = harvard_ind_inc$strategy_life80take, 
#                      ELS = harvard_els_inc$strategy_life80take,
#                      NIC = harvard_nic_inc$strategy_life80take)
# 
# s_life80take_harvard_mortality = list(UGA = harvard_uga_mort$strategy_life80take, 
#                                IND = harvard_ind_mort$strategy_life80take, 
#                                ELS = harvard_els_mort$strategy_life80take,
#                                NIC = harvard_nic_mort$strategy_life80take)
# 
# rm(harvard_els_inc,harvard_els_mort,harvard_ind_inc,harvard_ind_mort,harvard_nic_inc,harvard_nic_mort,harvard_uga_inc,harvard_uga_mort)

 
 