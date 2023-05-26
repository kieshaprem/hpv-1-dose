source('code/functions_supporting.r')

load('data/population/incomegroup.rdata')
load('output/costs/threshold_phe.rdata')

MODELNAME = c('phe',paste0('hpvadvise',c('India','Nigeria','Uganda','Vietnam','Canada')),paste0('harvard',c('Uganda','ElSalvador','Nicaragua','US')))

# file1 = read.csv(paste0('output/',data,'/',R,'_HPV_Vaccine_Impact_',model,'Life.csv'))
threshold_costs = list()
for(index in 1:10)
{
  load(paste0('output/costs/threshold_',MODELNAME[index],'.rdata'))
  threshold_costs[[MODELNAME[index]]]  = threshold 
  rm(threshold)
}


incomegroup = incomegroup$incomegroup[match(as.character(threshold_costs$phe$threshold_costs[[1]]$iso3c),incomegroup$iso3c)]
index_lic = which(incomegroup %in% "Low income")
index_mic = which(incomegroup %in% c("Lower middle income", 
                                     "Upper middle income" ))
index_hic = which(incomegroup %in% "High income")
index_global = 1:length(incomegroup)

index_loc = list(index_lic,index_mic,index_hic,index_global)
rm(index_lic,index_mic,index_hic,index_global)


aggregatecostA = aggregatecostB = aggregatecostC= aggregatecostD = aggregatecostE = aggregatecostF = array(NA,c(5,4))
thresholdA = thresholdB = thresholdC = thresholdD = thresholdE = thresholdF = list()

for(index in 1:10)#  if(index==1)
{
  
  for(loc in 1:4) 
  {
    i = c(index_loc[[loc]])
    if(length(threshold_costs[[index]]$threshold_costs_disc_dish) > 1) 
    {for(run in 2:length(threshold_costs[[index]]$threshold_costs_disc_dish)) 
    {
      j = index_loc[[loc]]+188*run
      i = c(i,j)
    }
    }
    thresholdDATA = as.data.frame(rbindlist(threshold_costs[[index]]$threshold_costs_disc)) #threshold_costs[[index]]$threshold_costs_disc_dish[[1]] #
    thresholdDATA[,2] = as.numeric(as.character(thresholdDATA[,2]))
    thresholdDATA[,3] = as.numeric(as.character(thresholdDATA[,3]))
    thresholdDATA[,4] = as.numeric(as.character(thresholdDATA[,4]))
    thresholdDATA[,5] = as.numeric(as.character(thresholdDATA[,5]))
    thresholdDATA[,6] = as.numeric(as.character(thresholdDATA[,6]))    
    thresholdDATA[,7] = as.numeric(as.character(thresholdDATA[,7]))
    aggregatecostA[1,loc] = median((thresholdDATA[i,2]),na.rm = 1)
    aggregatecostB[1,loc] = median((thresholdDATA[i,3]),na.rm = 1)
    aggregatecostC[1,loc] = median((thresholdDATA[i,4]),na.rm = 1)
    aggregatecostD[1,loc] = median((thresholdDATA[i,5]),na.rm = 1)
    aggregatecostE[1,loc] = median((thresholdDATA[i,6]),na.rm = 1)
    aggregatecostF[1,loc] = median((thresholdDATA[i,7]),na.rm = 1)
    
    aggregatecostA[2:5,loc] = as.numeric(quantile(thresholdDATA[i,2],probs = c(0.10,0.25,0.75,0.9),na.rm = 1))
    aggregatecostB[2:5,loc] = as.numeric(quantile(thresholdDATA[i,3],probs = c(0.10,0.25,0.75,0.9),na.rm = 1))
    aggregatecostC[2:5,loc] = as.numeric(quantile(thresholdDATA[i,4],probs = c(0.10,0.25,0.75,0.9),na.rm = 1))
    aggregatecostD[2:5,loc] = as.numeric(quantile(thresholdDATA[i,5],probs = c(0.10,0.25,0.75,0.9),na.rm = 1))
    aggregatecostE[2:5,loc] = as.numeric(quantile(thresholdDATA[i,6],probs = c(0.10,0.25,0.75,0.9),na.rm = 1))
    aggregatecostF[2:5,loc] = as.numeric(quantile(thresholdDATA[i,7],probs = c(0.10,0.25,0.75,0.9),na.rm = 1))
    
  }
  thresholdA[[index]] =  aggregatecostA
  thresholdB[[index]] =  aggregatecostB
  thresholdC[[index]] =  aggregatecostC
  thresholdD[[index]] =  aggregatecostD
  thresholdE[[index]] =  aggregatecostE
  thresholdF[[index]] =  aggregatecostF
  
}


save(thresholdA,file = 'output/costs/thresholdA_disc.rdata')
save(thresholdB,file = 'output/costs/thresholdB_disc.rdata')
save(thresholdC,file = 'output/costs/thresholdC_disc.rdata')
save(thresholdD,file = 'output/costs/thresholdD_disc.rdata')
save(thresholdE,file = 'output/costs/thresholdE_disc.rdata')
save(thresholdF,file = 'output/costs/thresholdF_disc.rdata')



