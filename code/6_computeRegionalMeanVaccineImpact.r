options(scipen=999)
source('code/functions_supporting.r')
load('output/prime/protect_life.rdata')
load('output/prime/protect_20y.rdata')
load('output/prime/protect_30y.rdata')
load('output/prime/protect_life80take.rdata')

cnames = colnames(protect_life$phe)
disc = data.frame(year = 1921:2300, disc = discount(0.03,STARTYEAR = 1921,LASTYEAR = 2300))

protect_life_group = list()
for(mo in 1:length(protect_life))
{

  data_aggregate = aggregate(x = protect_life[[mo]][,7], by = list(protect_life[[mo]]$year,protect_life[[mo]]$incomegroup), FUN = 'sum')
  colnames(data_aggregate) = c('year','incomegroup',cnames[7])
  for(index in 8:18)
  {
    if(is.na(sum(protect_life[[mo]][,index]))) protect_life[[mo]][is.na(protect_life[[mo]][,index]),index] = protect_life[[mo]][is.na(protect_life[[mo]][,index]),index-3]*disc$disc[match(x = protect_life[[mo]]$year[is.na(protect_life[[mo]][,index])],table = disc$year)]
    temp = aggregate(x = protect_life[[mo]][,index], by = list(protect_life[[mo]]$year,protect_life[[mo]]$incomegroup), FUN = 'sum')
    data_aggregate = data.frame(data_aggregate,temp[,3])
    colnames(data_aggregate)[index-4] = cnames[index]
  }
  protect_life_group[[mo]] = data_aggregate
}
rm(data_aggregate)


protect_20y_group = list()
for(mo in 1:length(protect_life))
{
  data_aggregate = aggregate(x = protect_20y[[mo]][,7], by = list(protect_20y[[mo]]$year,protect_20y[[mo]]$incomegroup), FUN = 'sum')
  colnames(data_aggregate) = c('year','incomegroup',cnames[7])
  for(index in 8:18)
  {
    if(is.na(sum(protect_20y[[mo]][,index]))) protect_20y[[mo]][is.na(protect_20y[[mo]][,index]),index] = protect_20y[[mo]][is.na(protect_20y[[mo]][,index]),index-3]*disc$disc[match(x = protect_20y[[mo]]$year[is.na(protect_20y[[mo]][,index])],table = disc$year)]
    temp = aggregate(x = protect_20y[[mo]][,index], by = list(protect_20y[[mo]]$year,protect_20y[[mo]]$incomegroup), FUN = 'sum')
    data_aggregate = data.frame(data_aggregate,temp[,3])
    colnames(data_aggregate)[index-4] = cnames[index]
  }
  protect_20y_group[[mo]] = data_aggregate
}
rm(data_aggregate)

protect_30y_group = list()
for(mo in 1:length(protect_life))
{
  data_aggregate = aggregate(x = protect_30y[[mo]][,7], by = list(protect_30y[[mo]]$year,protect_30y[[mo]]$incomegroup), FUN = 'sum')
  colnames(data_aggregate) = c('year','incomegroup',cnames[7])
  for(index in 8:18)
  {
    if(is.na(sum(protect_30y[[mo]][,index]))) protect_30y[[mo]][is.na(protect_30y[[mo]][,index]),index] = protect_30y[[mo]][is.na(protect_30y[[mo]][,index]),index-3]*disc$disc[match(x = protect_30y[[mo]]$year[is.na(protect_30y[[mo]][,index])],table = disc$year)]
    temp = aggregate(x = protect_30y[[mo]][,index], by = list(protect_30y[[mo]]$year,protect_30y[[mo]]$incomegroup), FUN = 'sum')
    data_aggregate = data.frame(data_aggregate,temp[,3])
    colnames(data_aggregate)[index-4] = cnames[index]
  }
  protect_30y_group[[mo]] = data_aggregate
}
rm(data_aggregate)


protect_life80take_group = list()
for(mo in 1:length(protect_life))
{
  data_aggregate = aggregate(x = protect_life80take[[mo]][,7], by = list(protect_life80take[[mo]]$year,protect_life80take[[mo]]$incomegroup), FUN = 'sum')
  colnames(data_aggregate) = c('year','incomegroup',cnames[7])
  for(index in 8:18)
  {
    if(is.na(sum(protect_life80take[[mo]][,index]))) protect_life80take[[mo]][is.na(protect_life80take[[mo]][,index]),index] = protect_life80take[[mo]][is.na(protect_life80take[[mo]][,index]),index-3]*disc$disc[match(x = protect_life80take[[mo]]$year[is.na(protect_life80take[[mo]][,index])],table = disc$year)]
    temp = aggregate(x = protect_life80take[[mo]][,index], by = list(protect_life80take[[mo]]$year,protect_life80take[[mo]]$incomegroup), FUN = 'sum')
    data_aggregate = data.frame(data_aggregate,temp[,3])
    colnames(data_aggregate)[index-4] = cnames[index]
  }
  protect_life80take_group[[mo]] = data_aggregate
}
rm(data_aggregate)



save(protect_life_group,file = 'output/prime/protect_life_group.rdata')
save(protect_20y_group,file = 'output/prime/protect_20y_group.rdata')
save(protect_30y_group,file = 'output/prime/protect_30y_group.rdata')
save(protect_life80take_group,file = 'output/prime/protect_life80take_group.rdata')
