
impact_hpvadviseIndia_all = list(protect_life=list(),
                                 protect_20y=list(),
                                 protect_30y=list(),
                                 protect_life80take = list())

for(run in 1:4)
{
  load(file = paste0('1-dose_batch/hpvadviseIndia',run,'/impact_hpvadviseIndia',run,'.rdata'))
  impact_hpvadviseIndia_all[[run]] = impact_hpvadviseIndia
  rm(impact_hpvadviseIndia)
}

impact_hpvadviseIndia = impact_hpvadviseIndia_all
rm(impact_hpvadviseIndia_all,run)


save(impact_hpvadviseIndia,file = 'output/prime/impact_hpvadviseIndia_indirect_batch.rdata')
rm(impact_hpvadviseIndia)


impact_hpvadviseNigeria_all = list(protect_life=list(),
                                   protect_20y=list(),
                                   protect_30y=list(),
                                   protect_life80take = list())

for(run in 1:4)
{
  load(file = paste0('1-dose_batch/hpvadviseNigeria',run,'/impact_hpvadviseNigeria',run,'.rdata'))
  impact_hpvadviseNigeria_all[[run]] = impact_hpvadviseNigeria
  rm(impact_hpvadviseNigeria)
}

impact_hpvadviseNigeria = impact_hpvadviseNigeria_all
rm(impact_hpvadviseNigeria_all,run)


save(impact_hpvadviseNigeria,file = 'output/prime/impact_hpvadviseNigeria_indirect_batch.rdata')
rm(impact_hpvadviseNigeria)



impact_hpvadviseUganda_all = list(protect_life=list(),
                                  protect_20y=list(),
                                  protect_30y=list(),
                                  protect_life80take = list())

for(run in 1:4)
{
  load(file = paste0('1-dose_batch/hpvadviseUganda',run,'/impact_hpvadviseUganda',run,'.rdata'))
  impact_hpvadviseUganda_all[[run]] = impact_hpvadviseUganda
  rm(impact_hpvadviseUganda)
}

impact_hpvadviseUganda = impact_hpvadviseUganda_all
rm(impact_hpvadviseUganda_all,run)


save(impact_hpvadviseUganda,file = 'output/prime/impact_hpvadviseUganda_indirect_batch.rdata')
rm(impact_hpvadviseUganda)



impact_hpvadviseVietnam_all = list(protect_life=list(),
                                   protect_20y=list(),
                                   protect_30y=list(),
                                   protect_life80take = list())

for(run in 1:4)
{
  load(file = paste0('1-dose_batch/hpvadviseVietnam',run,'/impact_hpvadviseVietnam',run,'.rdata'))
  impact_hpvadviseVietnam_all[[run]] = impact_hpvadviseVietnam
  rm(impact_hpvadviseVietnam)
}

impact_hpvadviseVietnam = impact_hpvadviseVietnam_all
rm(impact_hpvadviseVietnam_all,run)


save(impact_hpvadviseVietnam,file = 'output/prime/impact_hpvadviseVietnam_indirect_batch.rdata')
rm(impact_hpvadviseVietnam)



impact_hpvadviseCanada_all = list(protect_life=list(),
                                  protect_20y=list(),
                                  protect_30y=list(),
                                  protect_life80take = list())

for(run in 1:4)
{
  load(file = paste0('1-dose_batch/hpvadviseCanada',run,'/impact_hpvadviseCanada',run,'.rdata'))
  impact_hpvadviseCanada_all[[run]] = impact_hpvadviseCanada
  rm(impact_hpvadviseCanada)
}

impact_hpvadviseCanada = impact_hpvadviseCanada_all
rm(impact_hpvadviseCanada_all,run)


save(impact_hpvadviseCanada,file = 'output/prime/impact_hpvadviseCanada_indirect_batch.rdata')
rm(impact_hpvadviseCanada)

