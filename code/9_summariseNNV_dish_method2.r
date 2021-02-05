# source('code/functions_processing.r')

countrycharacteristics = read.csv('input/country_characteristics_and_cost_INPUT.csv',as.is = TRUE)

MODELNAME = c('phe',paste0('hpvadvise',c('India','Nigeria','Uganda','Vietnam','Canada')),paste0('harvard',c('India','Uganda','ElSalvador','Nicaragua','US')))

# file1 = read.csv(paste0('output/',data,'/',R,'_HPV_Vaccine_Impact_',model,'Life.csv'))
nnv = nnv_dish = list()
for(index in 1:11)
{
  load(paste0('output/nnv_',MODELNAME[index],'_M2.rdata'))
  nnv[[MODELNAME[index]]]  = no_to_vaccinate 
  rm(no_to_vaccinate)
  load(paste0('output/nnv_dish_',MODELNAME[index],'_M2.rdata'))
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
nnvdishA = nnvdishB = nnvdishC = nnvdishD = nnvdishE = nnvdishF = list()

for(index in 1:11)#  if(index==1)
{
  
  for(loc in 1:4) 
  {
    i = c(index_loc[[loc]])
    if(length(nnv_dish[[index]]) > 1) 
    {for(run in 2:length(nnv_dish[[index]])) 
    {
      j = index_loc[[loc]]+192*run
      i = c(i,j)
    }
    }
    nnvDATA = as.data.frame(rbindlist(nnv_dish[[index]])) #nnv_costs[[index]]$nnv_costs_disc_dish[[1]] #
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
  nnvdishA[[index]] =  aggregatennvA
  nnvdishB[[index]] =  aggregatennvB
  nnvdishC[[index]] =  aggregatennvC
  nnvdishD[[index]] =  aggregatennvD
  nnvdishE[[index]] =  aggregatennvE
  nnvdishF[[index]] =  aggregatennvF
  
}


save(nnvdishA,file = 'output/nnvdishA_M2.rdata')
save(nnvdishB,file = 'output/nnvdishB_M2.rdata')
save(nnvdishC,file = 'output/nnvdishC_M2.rdata')
save(nnvdishD,file = 'output/nnvdishD_M2.rdata')
save(nnvdishE,file = 'output/nnvdishE_M2.rdata')
save(nnvdishF,file = 'output/nnvdishF_M2.rdata')



