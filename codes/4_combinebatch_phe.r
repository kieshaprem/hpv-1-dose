
impact_phe_all = list(protect_life=list(),
                  protect_20y=list(),
                  protect_30y=list(),
                  protect_life80take = list())

for(run in 1:4)
{
  load(file = paste0('1-dose_batch/phe',run,'/impact_phe',run,'.rdata'))
  impact_phe_all[[run]] = impact_phe
  rm(impact_phe)
}

impact_phe = impact_phe_all
rm(impact_phe_all,run)


save(impact_phe,file = 'output/prime/impact_phe_indirect_batch.rdata')
