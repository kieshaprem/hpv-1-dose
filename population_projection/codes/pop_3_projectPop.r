

CONFIG = list(years = 2100:2220)
CONFIG$regions = names(mortality)[names(mortality) %in% names(fertility)]
CONFIG$ages = 0:100
projections = list()

pop= data.frame(matrix(NA,nrow = 101,ncol = length(CONFIG$years),dimnames = list(paste0('age',0:100),paste0('year',2100:2220))))
for(co in 1:length(CONFIG$regions))
{
  iso = CONFIG$regions[co]
  N = as.numeric(poplist[[iso]][,ncol(poplist[[iso]])])
  pop[,1] = N
  for(y in 2:length(CONFIG$years))
  {
    births = sum(fertility[[iso]]*N)
    mbirths = (births*popsrb$srb_male[popsrb$iso3c %in% iso])
    fbirths = births - mbirths
    atrisk = c(fbirths,N[-length(CONFIG$ages)])
    pop[,y] = round(atrisk*(1-mortality[[iso]]$qx))
  }
  projections[[iso]] = pop
}

save(projections,file = 'output/projections.rdata')
# lapply(pop, colSums)

rm(N,y,co,iso,atrisk,births,mbirths,fbirths,pop)

