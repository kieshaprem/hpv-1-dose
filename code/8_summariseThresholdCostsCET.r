# source('code/functions_processing.r')

load('data/population/incomegroup.rdata')
load('output/costs/threshold_phe.rdata')

MODELNAME = c('phe',paste0('hpvadvise',c('India','Nigeria','Uganda','Vietnam','Canada')),paste0('harvard',c('India','Uganda','ElSalvador','Nicaragua','US')))

# file1 = read.csv(paste0('output/',data,'/',R,'_HPV_Vaccine_Impact_',model,'Life.csv'))
threshold_costs = list()
for(index in 1:11)
{
  load(paste0('output/costs/threshold_cet_',MODELNAME[index],'.rdata'))
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

aggregatecostA = aggregatecostB = aggregatecostC= aggregatecostD = array(NA,c(9,4))
thresholdA = thresholdB = thresholdC = thresholdD = list()
# thresholdA[[1]] = thresholdB[[1]] = thresholdC[[1]] = thresholdD[[1]] = c() 
# for(index in 2:5)
# {
#   for(loc in 1:4) 
#     {
#     for(run in 1:5)
#     {
#      aggregatecostA[run,loc] = mean(costs[[index]][[run]][index_loc[[loc]],3],na.rm = 1)
#      aggregatecostB[run,loc] = mean(costs[[index]][[run]][index_loc[[loc]],4],na.rm = 1)
#      aggregatecostC[run,loc] = mean(costs[[index]][[run]][index_loc[[loc]],5],na.rm = 1)
#      aggregatecostD[run,loc] = mean(costs[[index]][[run]][index_loc[[loc]],6],na.rm = 1)
#     }
#   }
#   thresholdA[[index]] =  aggregatecostA
#   thresholdB[[index]] =  aggregatecostB
#   thresholdC[[index]] =  aggregatecostC
#   thresholdD[[index]] =  aggregatecostD
# }
# 
# index=1
# aggregatecostA = aggregatecostB = aggregatecostC= aggregatecostD = array(NA,c(5,4))
# costPHE = as.data.frame(rbindlist(costs$PHE))
# 
# if(index==1)
# {
# 
#   for(loc in 1:4) 
#   {
#     i = c(index_loc[[loc]],
#       index_loc[[loc]]+177,
#       index_loc[[loc]]+177*2,
#       index_loc[[loc]]+177*3,
#       index_loc[[loc]]+177*4)
#     
#       aggregatecostA[1,loc] = median((costPHE[i,3]),na.rm = 1)
#       aggregatecostB[1,loc] = median((costPHE[i,4]),na.rm = 1)
#       aggregatecostC[1,loc] = median((costPHE[i,5]),na.rm = 1)
#       aggregatecostD[1,loc] = median((costPHE[i,6]),na.rm = 1)
#       
#       aggregatecostA[2:5,loc] = as.numeric(quantile(costPHE[i,3],probs = c(0.10,0.25,0.75,0.9),na.rm = 1))
#       aggregatecostB[2:5,loc] = as.numeric(quantile(costPHE[i,4],probs = c(0.10,0.25,0.75,0.9),na.rm = 1))
#       aggregatecostC[2:5,loc] = as.numeric(quantile(costPHE[i,5],probs = c(0.10,0.25,0.75,0.9),na.rm = 1))
#       aggregatecostD[2:5,loc] = as.numeric(quantile(costPHE[i,6],probs = c(0.10,0.25,0.75,0.9),na.rm = 1))
#       
#   }
#   thresholdA[[index]] =  aggregatecostA
#   thresholdB[[index]] =  aggregatecostB
#   thresholdC[[index]] =  aggregatecostC
#   thresholdD[[index]] =  aggregatecostD
#   
# }


aggregatecostA = aggregatecostB = aggregatecostC= aggregatecostD = aggregatecostE = aggregatecostF = array(NA,c(5,4))
thresholdAcet = thresholdBcet = thresholdCcet = thresholdDcet = thresholdEcet = thresholdFcet = list()

for(index in 1:11)#  if(index==1)
{
  
  for(loc in 1:4) 
  {
    i = c(index_loc[[loc]])
    if(length(threshold_costs[[index]]$threshold_costs_disc_dish) > 1) 
    {for(run in 2:length(threshold_costs[[index]]$threshold_costs_disc_dish)) 
    {
      j = index_loc[[loc]]+192*run
      i = c(i,j)
    }
    }
    thresholdDATA = as.data.frame(rbindlist(threshold_costs[[index]]$threshold_costs_disc_dish)) #threshold_costs[[index]]$threshold_costs_disc_dish[[1]] #
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
  thresholdAcet[[index]] =  aggregatecostA
  thresholdBcet[[index]] =  aggregatecostB
  thresholdCcet[[index]] =  aggregatecostC
  thresholdDcet[[index]] =  aggregatecostD
  thresholdEcet[[index]] =  aggregatecostE
  thresholdFcet[[index]] =  aggregatecostF
  
}


save(thresholdAcet,file = 'output/costs/thresholdAcet.rdata')
save(thresholdBcet,file = 'output/costs/thresholdBcet.rdata')
save(thresholdCcet,file = 'output/costs/thresholdCcet.rdata')
save(thresholdDcet,file = 'output/costs/thresholdDcet.rdata')
save(thresholdEcet,file = 'output/costs/thresholdEcet.rdata')
save(thresholdFcet,file = 'output/costs/thresholdFcet.rdata')


