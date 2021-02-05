
readHPVADVISE <- function(COUNTRY,SCEN){
  results = list()
  
  results$median = read_xlsx(paste0('data/hpvadivse/Results/',COUNTRY,'_median.xlsx'),	range = paste0(SCEN,'_CCinc!B4:CX94'))
  results$q25  = read_xlsx(paste0('data/hpvadivse/Results/',COUNTRY,'_25percentile.xlsx'),	range = paste0(SCEN,'_CCinc!B4:CX94'))
  results$q75  = read_xlsx(paste0('data/hpvadivse/Results/',COUNTRY,'_75percentile.xlsx'),	range = paste0(SCEN,'_CCinc!B4:CX94'))
  results$q10  = read_xlsx(paste0('data/hpvadivse/Results/',COUNTRY,'_10percentile.xlsx'), 	range = paste0(SCEN,'_CCinc!B4:CX94'))
  results$q90  = read_xlsx(paste0('data/hpvadivse/Results/',COUNTRY,'_90percentile.xlsx'), 	range = paste0(SCEN,'_CCinc!B4:CX94'))
  
  for(i in 1:length(results))
  {
    results[[i]] = data.frame(t(results[[i]]))
    results[[i]] = cbind(matrix(0,nrow = nrow(results[[i]]),ncol = 10),results[[i]])
    colnames(results[[i]]) = paste0('age',0:99)
    row.names(results[[i]]) = paste0('year',2020:2120)
  }
  return(results)
}

# Outputs sent by JF on 8 Dec 2020
readHPVADVISEcanada <- function(SCEN){
  results = list()
  
  results$median =read_xlsx(paste0('data/hpvadivse/Simulation_Outputs_can1d/Simulation_Outputs_can1d_median_2020-12-07.xlsx'),	range = paste0(SCEN,'_CCinc!B4:CX94'))
  results$q25  =  read_xlsx(paste0('data/hpvadivse/Simulation_Outputs_can1d/Simulation_Outputs_can1d_25perc_2020-12-07.xlsx'),	range = paste0(SCEN,'_CCinc!B4:CX94'))
  results$q75  =  read_xlsx(paste0('data/hpvadivse/Simulation_Outputs_can1d/Simulation_Outputs_can1d_75perc_2020-12-07.xlsx'),	range = paste0(SCEN,'_CCinc!B4:CX94'))
  results$q10  =  read_xlsx(paste0('data/hpvadivse/Simulation_Outputs_can1d/Simulation_Outputs_can1d_10perc_2020-12-07.xlsx'),	range = paste0(SCEN,'_CCinc!B4:CX94'))
  results$q90  =  read_xlsx(paste0('data/hpvadivse/Simulation_Outputs_can1d/Simulation_Outputs_can1d_90perc_2020-12-07.xlsx'),	range = paste0(SCEN,'_CCinc!B4:CX94'))
  
  for(i in 1:length(results))
  {
    results[[i]] = data.frame(t(results[[i]]))
    results[[i]] = cbind(matrix(0,nrow = nrow(results[[i]]),ncol = 10),results[[i]])
    colnames(results[[i]]) = paste0('age',0:99)
    row.names(results[[i]]) = paste0('year',2020:2120)
  }
  return(results)
}

hpvadviseIndia = list()
hpvadviseIndia$strategy_life = readHPVADVISE(COUNTRY = 'India',SCEN = 'S1')
hpvadviseIndia$strategy_20y = readHPVADVISE(COUNTRY = 'India',SCEN = 'S2')
hpvadviseIndia$strategy_30y = readHPVADVISE(COUNTRY = 'India',SCEN = 'S3')
hpvadviseIndia$strategy_life80take = readHPVADVISE(COUNTRY = 'India',SCEN = 'S4')
hpvadviseIndia$strategy_novac = readHPVADVISE(COUNTRY = 'India',SCEN = 'S0')


hpvadviseNigeria = list()
hpvadviseNigeria$strategy_life = readHPVADVISE(COUNTRY = 'Nigeria',SCEN = 'S1')
hpvadviseNigeria$strategy_20y = readHPVADVISE(COUNTRY = 'Nigeria',SCEN = 'S2')
hpvadviseNigeria$strategy_30y = readHPVADVISE(COUNTRY = 'Nigeria',SCEN = 'S3')
hpvadviseNigeria$strategy_life80take = readHPVADVISE(COUNTRY = 'Nigeria',SCEN = 'S4')
hpvadviseNigeria$strategy_novac = readHPVADVISE(COUNTRY = 'Nigeria',SCEN = 'S0')


hpvadviseUganda = list()
hpvadviseUganda$strategy_life = readHPVADVISE(COUNTRY = 'Uganda',SCEN = 'S1')
hpvadviseUganda$strategy_20y = readHPVADVISE(COUNTRY = 'Uganda',SCEN = 'S2')
hpvadviseUganda$strategy_30y = readHPVADVISE(COUNTRY = 'Uganda',SCEN = 'S3')
hpvadviseUganda$strategy_life80take = readHPVADVISE(COUNTRY = 'Uganda',SCEN = 'S4')
hpvadviseUganda$strategy_novac = readHPVADVISE(COUNTRY = 'Uganda',SCEN = 'S0')

hpvadviseVietnam = list()
hpvadviseVietnam$strategy_life = readHPVADVISE(COUNTRY = 'Vietnam',SCEN = 'S1')
hpvadviseVietnam$strategy_20y = readHPVADVISE(COUNTRY = 'Vietnam',SCEN = 'S2')
hpvadviseVietnam$strategy_30y = readHPVADVISE(COUNTRY = 'Vietnam',SCEN = 'S3')
hpvadviseVietnam$strategy_life80take = readHPVADVISE(COUNTRY = 'Vietnam',SCEN = 'S4')
hpvadviseVietnam$strategy_novac = readHPVADVISE(COUNTRY = 'Vietnam',SCEN = 'S0')

hpvadviseCanada = list()
hpvadviseCanada$strategy_life = readHPVADVISEcanada(SCEN = 'S1')
hpvadviseCanada$strategy_20y = readHPVADVISEcanada(SCEN = 'S2')
hpvadviseCanada$strategy_30y = readHPVADVISEcanada(SCEN = 'S3')
hpvadviseCanada$strategy_life80take = readHPVADVISEcanada(SCEN = 'S4')
hpvadviseCanada$strategy_novac = readHPVADVISEcanada(SCEN = 'S0')


rm(readHPVADVISE)