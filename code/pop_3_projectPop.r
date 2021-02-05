load(file = 'data/population/pop.rdata')
load(file = 'data/population/lifetable.rdata')
load(file = 'data/population/fertility.rdata')
source(file = 'code/pop_2_getSexRatioBirth.r')

CONFIG = list(years = 2100:2220)
CONFIG$regions = names(life)[names(life) %in% names(fertility)]
CONFIG$ages = 0:100
pop = list()

projections = data.frame(matrix(NA,nrow = 101,ncol = length(CONFIG$years),dimnames = list(paste0('age',0:100),paste0('year',2100:2220))))



for(co in 1:length(CONFIG$regions))
{
  iso = CONFIG$regions[co]
  N = as.numeric(poplist[[iso]][,ncol(poplist[[iso]])])
  projections[,1] = N
  for(y in 2:length(CONFIG$years))
  {
    births = sum(fertility[[iso]]*N)
    mbirths = (births*popsrb$srb_male[popsrb$iso3c %in% iso])
    fbirths = births - mbirths#(sum(births*FSRB))
    atrisk = c(fbirths,N[-length(CONFIG$ages)])
    projections[,y] = round(atrisk*(1-life[[iso]]$qx))
  }
  pop[[iso]] = projections
}

save(pop,file = 'data/population/pop_projections.rdata')
# lapply(pop, colSums)

