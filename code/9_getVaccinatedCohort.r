load(file = 'data/population/pop_projections.rdata')
load(file = 'data/population/pop.rdata')
load('data/population/incomegroup.rdata')
load('output/prime/impact_phe.rdata')
birthcohorts[[1]]

poplist[[1]]
pop[[1]]

countries = names(pop)
girls10y = data.frame(iso3c = countries,matrix(NA, nrow = length(countries),ncol = 100))
colnames(girls10y)[2:101] = paste0('year',2021:2120)


for(co in countries)
{
  girls10y[girls10y$iso3c %in% co ,-1] = as.numeric(c(poplist[[co]][11,22:101],pop[[co]][11,2:21]))
}

save(girls10y,file = 'data/population/girls10y.rdata')     

totalvac = girls10y
totalvac[,-1] =  girls10y[,-1]*0.8

save(totalvac,file = 'data/population/totalvac.rdata') 


incomegroup_some = incomegroup[incomegroup$iso3c %in% as.character(unique(impact_phe$protect_life[[1]]$iso3c)),]
sort(incomegroup_some$iso3c[incomegroup_some$incomegroup %in% "Low income"])
sort(incomegroup_some$iso3c[incomegroup_some$incomegroup %in% c("Lower middle income","Upper middle income")])
sort(incomegroup_some$iso3c[incomegroup_some$incomegroup %in% "High income"])

popsize = list(lic = rep(0,100),mic = rep(0,100), hic = rep(0,100))
for(co in as.character(incomegroup_some$iso3c[incomegroup_some$incomegroup  %in% "Low income"]))
{
  if(co %in% names(pop)) popsize$lic = popsize$lic + as.numeric(c(colSums(poplist[[co]][,22:101]),colSums(pop[[co]][,2:21])))
  if(!(co %in% names(pop))) popsize$lic = popsize$lic 
}

for(co in as.character(incomegroup_some$iso3c[incomegroup_some$incomegroup  %in% c("Lower middle income","Upper middle income")]))
{
  if(co %in% names(pop)) popsize$mic = popsize$mic + as.numeric(c(colSums(poplist[[co]][,22:101]),colSums(pop[[co]][,2:21])))
  if(!(co %in% names(pop))) popsize$mic = popsize$mic 
}

for(co in as.character(incomegroup_some$iso3c[incomegroup_some$incomegroup  %in% "High income"]))
{
  if(co %in% names(pop)) popsize$hic = popsize$hic + as.numeric(c(colSums(poplist[[co]][,22:101]),colSums(pop[[co]][,2:21])))
  if(!(co %in% names(pop))) popsize$hic = popsize$hic 
}
save(popsize,file = 'data/population/popsize.rdata') 
