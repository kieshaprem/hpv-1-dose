# source('code/functions_processing.r')

countrycharacteristics = read.csv('input/country_characteristics_and_cost_INPUT.csv',as.is = TRUE)

MODELNAME = c('phe',paste0('hpvadvise',c('India','Nigeria','Uganda','Vietnam','Canada')),paste0('harvard',c('Uganda','ElSalvador','Nicaragua','US')))

# file1 = read.csv(paste0('output/',data,'/',R,'_HPV_Vaccine_Impact_',model,'Life.csv'))
nnv = nnv_dish = list()
for(index in 1:10)
{
  load(paste0('output/nnv/nnv_',MODELNAME[index],'.rdata'))
  nnv[[MODELNAME[index]]]  = no_to_vaccinate 
  rm(no_to_vaccinate)
  load(paste0('output/nnv/nnv_dish_',MODELNAME[index],'.rdata'))
  nnv_dish[[MODELNAME[index]]]  = no_to_vaccinate_dish 
  rm(no_to_vaccinate_dish)
}


incomegroup = countrycharacteristics$Income.Group..World.Bank.July.2016...based.on.2015.data.[match(as.character(nnv$phe[[1]]$iso3c),countrycharacteristics$iso3)]
index_lic = which(incomegroup %in% "Low income")
index_mic = which(incomegroup %in% c("Lower middle income", 
                                     "Upper middle income" ))
index_hic = which(incomegroup %in% "High income")
index_global = 1:length(incomegroup)

index_loc = list(index_lic,index_mic,index_hic,index_global)
rm(index_lic,index_mic,index_hic,index_global)


aggregatennvA = aggregatennvB = aggregatennvC= aggregatennvD = aggregatennvE = aggregatennvF = array(NA,c(5,4))
nnvA = nnvB = nnvC = nnvD = nnvE = nnvF = list()

for(index in 1:10)#  if(index==1)
{
  
  for(loc in 1:4) 
  {
    i = c(index_loc[[loc]])
    if(length(nnv[[index]]) > 1) 
    {for(run in 2:length(nnv[[index]])) 
    {
      j = index_loc[[loc]]+188*run
      i = c(i,j)
    }
    }
    nnvDATA = as.data.frame(rbindlist(nnv[[index]])) #nnv_costs[[index]]$nnv_costs_disc_dish[[1]] #
    nnvDATA[,2] = as.numeric(as.character(nnvDATA[,2]))
    nnvDATA[,3] = as.numeric(as.character(nnvDATA[,3]))
    nnvDATA[,4] = as.numeric(as.character(nnvDATA[,4]))
    nnvDATA[,5] = as.numeric(as.character(nnvDATA[,5]))
    nnvDATA[,6] = as.numeric(as.character(nnvDATA[,6]))    
    nnvDATA[,7] = as.numeric(as.character(nnvDATA[,7]))
    aggregatennvA[1,loc] = median((nnvDATA[i,2]),na.rm = 1)
    aggregatennvB[1,loc] = median((nnvDATA[i,3]),na.rm = 1)
    aggregatennvC[1,loc] = median((nnvDATA[i,4]),na.rm = 1)
    aggregatennvD[1,loc] = median((nnvDATA[i,5]),na.rm = 1)
    aggregatennvE[1,loc] = median((nnvDATA[i,6]),na.rm = 1)
    aggregatennvF[1,loc] = median((nnvDATA[i,7]),na.rm = 1)
    
    aggregatennvA[2:5,loc] = as.numeric(quantile(nnvDATA[i,2],probs = c(0.10,0.25,0.75,0.9),na.rm = 1))
    aggregatennvB[2:5,loc] = as.numeric(quantile(nnvDATA[i,3],probs = c(0.10,0.25,0.75,0.9),na.rm = 1))
    aggregatennvC[2:5,loc] = as.numeric(quantile(nnvDATA[i,4],probs = c(0.10,0.25,0.75,0.9),na.rm = 1))
    aggregatennvD[2:5,loc] = as.numeric(quantile(nnvDATA[i,5],probs = c(0.10,0.25,0.75,0.9),na.rm = 1))
    aggregatennvE[2:5,loc] = as.numeric(quantile(nnvDATA[i,6],probs = c(0.10,0.25,0.75,0.9),na.rm = 1))
    aggregatennvF[2:5,loc] = as.numeric(quantile(nnvDATA[i,7],probs = c(0.10,0.25,0.75,0.9),na.rm = 1))
    
  }
  nnvA[[index]] =  aggregatennvA
  nnvB[[index]] =  aggregatennvB
  nnvC[[index]] =  aggregatennvC
  nnvD[[index]] =  aggregatennvD
  nnvE[[index]] =  aggregatennvE
  nnvF[[index]] =  aggregatennvF
  
}


save(nnvA,file = 'output/nnv/nnvA.rdata')
save(nnvB,file = 'output/nnv/nnvB.rdata')
save(nnvC,file = 'output/nnv/nnvC.rdata')
save(nnvD,file = 'output/nnv/nnvD.rdata')
save(nnvE,file = 'output/nnv/nnvE.rdata')
save(nnvF,file = 'output/nnv/nnvF.rdata')



