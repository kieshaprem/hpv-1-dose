pop_all = data.pop

poplist = list()

countries = as.character(unique(pop_all$country_code))


for(co in 1:length(countries))
{
  poplist[[co]] = matrix(pop_all$value[pop_all$country_code %in% countries[co]& pop_all$year %in% 2000:2100],nrow = 101,ncol = 101,byrow = FALSE,
                         dimnames = list(paste0('age',0:100),paste0('year',2000:2100)))
  print(co)
}
names(poplist) = countries
save(poplist,file = 'data/population/pop.rdata')
## Reconstruct the life table for 2099--2100 for all countries (assume it to be the stationary population) 
# co = 1
lifetable = list()
for(co in 1:length(countries))
{
  year = 2099
  t = year - 1999
  
  lifetable[[co]] = matrix(NA,nrow = 101,ncol = 11,
                           dimnames = list(paste0('age',0:100),c('Nx','Dx','mx','ax','qx','px','lx','dx','Lx','Tx','e0x')))
  lifetable[[co]][,1] = poplist[[co]][,t]
  lifetable[[co]][1:99,2] = c(poplist[[co]][1:99,t] - poplist[[co]][2:100,t+1])
  lifetable[[co]][,3] = lifetable[[co]][,2]/lifetable[[co]][,1]  
  lifetable[[co]][100:101,3] = lifetable[[co]][99,3]
  lifetable[[co]][100:101,2] = round(lifetable[[co]][100:101,3]*lifetable[[co]][100:101,1])
  lifetable[[co]][,4] = 1+1/lifetable[[co]][,3] - 1/(1-exp(-1*lifetable[[co]][,3]))
  lifetable[[co]][1,4] = ifelse(lifetable[[co]][1,3]>0.107,0.35,0.053 + 2.8*lifetable[[co]][1,3])
  lifetable[[co]][,5] = (1*lifetable[[co]][,3])/(1+(1-lifetable[[co]][,4])*lifetable[[co]][,3])
  lifetable[[co]][101,5] = 1
  lifetable[[co]][,6] = 1-lifetable[[co]][,5] 
  lifetable[[co]][1,7] = 100000
  for(a in 2:101) lifetable[[co]][a,7] = lifetable[[co]][a-1,7]*lifetable[[co]][a-1,6]
  for(a in 1:100) lifetable[[co]][a,8] = lifetable[[co]][a,7]-lifetable[[co]][a+1,7]
  lifetable[[co]][101,8] = lifetable[[co]][101,7] 
  lifetable[[co]][1:100,9] = 1*lifetable[[co]][2:101,7]+lifetable[[co]][1:100,4]*lifetable[[co]][1:100,8]
  lifetable[[co]][101,9] = lifetable[[co]][101,7]/lifetable[[co]][101,3]
  lifetable[[co]][,10] = rev(cumsum(rev(lifetable[[co]][,9])))
  lifetable[[co]][,11] = lifetable[[co]][,10]/lifetable[[co]][,7] 
  print(co)
}
names(lifetable) = countries
