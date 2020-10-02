birthcohorts = list()
bc = matrix(NA, nrow = 100, ncol = length(2090:2110))
colnames(bc) = paste0('birthcohort',2090:2110)
rownames(bc) = paste0('age',0:99)
for(co in 1:length(CONFIG$regions))
{
  ISO = CONFIG$regions[co]
  pop = cbind(poplist[[ISO]],projections[[ISO]][,-1])
  
  for(birthyear in 2090:2110)
  {
    bc[,birthyear-2089] = diag(data.matrix(pop[,(birthyear-1999):(birthyear-1999+99)]))
  }
  birthcohorts[[ISO]] = bc
  bc = bc*0
}

save(birthcohorts,file = 'output/birthcohorts.rdata')
