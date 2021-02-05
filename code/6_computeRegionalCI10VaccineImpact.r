options(scipen=999)
source('code/functions_supporting.r')
load('output/prime/protect_life_10prec.rdata')
load('output/prime/protect_20y_10prec.rdata')
load('output/prime/protect_30y_10prec.rdata')
load('output/prime/protect_life80take_10prec.rdata')

cnames = colnames(protect_life_p10$phe)
disc = data.frame(year = 1921:2300, disc = discount(0.03,STARTYEAR = 1921,LASTYEAR = 2300))

protect_life_group_p10 = list()
for(mo in 1:length(protect_life_p10))
{
  
  data_aggregate = aggregate(x = protect_life_p10[[mo]][,7], by = list(protect_life_p10[[mo]]$year,protect_life_p10[[mo]]$incomegroup), FUN = 'sum')
  colnames(data_aggregate) = c('year','incomegroup',cnames[7])
  for(index in 8:18)
  {
    if(is.na(sum(protect_life_p10[[mo]][,index]))) protect_life_p10[[mo]][is.na(protect_life_p10[[mo]][,index]),index] = protect_life_p10[[mo]][is.na(protect_life_p10[[mo]][,index]),index-3]*disc$disc[match(x = protect_life_p10[[mo]]$year[is.na(protect_life_p10[[mo]][,index])],table = disc$year)]
    temp = aggregate(x = protect_life_p10[[mo]][,index], by = list(protect_life_p10[[mo]]$year,protect_life_p10[[mo]]$incomegroup), FUN = 'sum')
    data_aggregate = data.frame(data_aggregate,temp[,3])
    colnames(data_aggregate)[index-4] = cnames[index]
  }
  protect_life_group_p10[[mo]] = data_aggregate
}
rm(data_aggregate)


protect_20y_group_p10 = list()
for(mo in 1:length(protect_life_p10))
{
  data_aggregate = aggregate(x = protect_20y_p10[[mo]][,7], by = list(protect_20y_p10[[mo]]$year,protect_20y_p10[[mo]]$incomegroup), FUN = 'sum')
  colnames(data_aggregate) = c('year','incomegroup',cnames[7])
  for(index in 8:18)
  {
    if(is.na(sum(protect_20y_p10[[mo]][,index]))) protect_20y_p10[[mo]][is.na(protect_20y_p10[[mo]][,index]),index] = protect_20y_p10[[mo]][is.na(protect_20y_p10[[mo]][,index]),index-3]*disc$disc[match(x = protect_20y_p10[[mo]]$year[is.na(protect_20y_p10[[mo]][,index])],table = disc$year)]
    temp = aggregate(x = protect_20y_p10[[mo]][,index], by = list(protect_20y_p10[[mo]]$year,protect_20y_p10[[mo]]$incomegroup), FUN = 'sum')
    data_aggregate = data.frame(data_aggregate,temp[,3])
    colnames(data_aggregate)[index-4] = cnames[index]
  }
  protect_20y_group_p10[[mo]] = data_aggregate
}
rm(data_aggregate)

protect_30y_group_p10 = list()
for(mo in 1:length(protect_life_p10))
{
  data_aggregate = aggregate(x = protect_30y_p10[[mo]][,7], by = list(protect_30y_p10[[mo]]$year,protect_30y_p10[[mo]]$incomegroup), FUN = 'sum')
  colnames(data_aggregate) = c('year','incomegroup',cnames[7])
  for(index in 8:18)
  {
    if(is.na(sum(protect_30y_p10[[mo]][,index]))) protect_30y_p10[[mo]][is.na(protect_30y_p10[[mo]][,index]),index] = protect_30y_p10[[mo]][is.na(protect_30y_p10[[mo]][,index]),index-3]*disc$disc[match(x = protect_30y_p10[[mo]]$year[is.na(protect_30y_p10[[mo]][,index])],table = disc$year)]
    temp = aggregate(x = protect_30y_p10[[mo]][,index], by = list(protect_30y_p10[[mo]]$year,protect_30y_p10[[mo]]$incomegroup), FUN = 'sum')
    data_aggregate = data.frame(data_aggregate,temp[,3])
    colnames(data_aggregate)[index-4] = cnames[index]
  }
  protect_30y_group_p10[[mo]] = data_aggregate
}
rm(data_aggregate)


protect_life80take_group_p10 = list()
for(mo in 1:length(protect_life_p10))
{
  data_aggregate = aggregate(x = protect_life80take_p10[[mo]][,7], by = list(protect_life80take_p10[[mo]]$year,protect_life80take_p10[[mo]]$incomegroup), FUN = 'sum')
  colnames(data_aggregate) = c('year','incomegroup',cnames[7])
  for(index in 8:18)
  {
    if(is.na(sum(protect_life80take_p10[[mo]][,index]))) protect_life80take_p10[[mo]][is.na(protect_life80take_p10[[mo]][,index]),index] = protect_life80take_p10[[mo]][is.na(protect_life80take_p10[[mo]][,index]),index-3]*disc$disc[match(x = protect_life80take_p10[[mo]]$year[is.na(protect_life80take_p10[[mo]][,index])],table = disc$year)]
    temp = aggregate(x = protect_life80take_p10[[mo]][,index], by = list(protect_life80take_p10[[mo]]$year,protect_life80take_p10[[mo]]$incomegroup), FUN = 'sum')
    data_aggregate = data.frame(data_aggregate,temp[,3])
    colnames(data_aggregate)[index-4] = cnames[index]
  }
  protect_life80take_group_p10[[mo]] = data_aggregate
}
rm(data_aggregate)



save(protect_life_group_p10,file = 'output/prime/protect_life_group_10prec.rdata')
save(protect_20y_group_p10,file = 'output/prime/protect_20y_group_10prec.rdata')
save(protect_30y_group_p10,file = 'output/prime/protect_30y_group_10prec.rdata')
save(protect_life80take_group_p10,file = 'output/prime/protect_life80take_group_10prec.rdata')
