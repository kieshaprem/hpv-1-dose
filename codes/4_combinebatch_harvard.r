# Reading in new Harvard which harmonised the routine age of vaccination (i.e.,  from 9yo to 10yo) "..._new.rdata"

impact_harvardElSalvador_all = list(protect_life=list(),
                                 protect_20y=list(),
                                 protect_30y=list(),
                                 protect_life80take = list())

for(run in 1:4)
{
  load(file = paste0('1-dose_batch/harvardElSalvador',run,'/impact_harvardElSalvador',run,'.rdata'))
  impact_harvardElSalvador_all[[run]] = impact_harvardElSalvador
  rm(impact_harvardElSalvador)
}

impact_harvardElSalvador = impact_harvardElSalvador_all
rm(impact_harvardElSalvador_all,run)


save(impact_harvardElSalvador,file = 'output/prime/impact_harvardElSalvador_indirect_batch_new.rdata')
rm(impact_harvardElSalvador)


impact_harvardNicaragua_all = list(protect_life=list(),
                                   protect_20y=list(),
                                   protect_30y=list(),
                                   protect_life80take = list())

for(run in 1:4)
{
  load(file = paste0('1-dose_batch/harvardNicaragua',run,'/impact_harvardNicaragua',run,'.rdata'))
  impact_harvardNicaragua_all[[run]] = impact_harvardNicaragua
  rm(impact_harvardNicaragua)
}

impact_harvardNicaragua = impact_harvardNicaragua_all
rm(impact_harvardNicaragua_all,run)


save(impact_harvardNicaragua,file = 'output/prime/impact_harvardNicaragua_indirect_batch_new.rdata')
rm(impact_harvardNicaragua)



impact_harvardUganda_all = list(protect_life=list(),
                                  protect_20y=list(),
                                  protect_30y=list(),
                                  protect_life80take = list())

for(run in 1:4)
{
  load(file = paste0('1-dose_batch/harvardUganda',run,'/impact_harvardUganda',run,'.rdata'))
  impact_harvardUganda_all[[run]] = impact_harvardUganda
  rm(impact_harvardUganda)
}

impact_harvardUganda = impact_harvardUganda_all
rm(impact_harvardUganda_all,run)


save(impact_harvardUganda,file = 'output/prime/impact_harvardUganda_indirect_batch_new.rdata')
rm(impact_harvardUganda)



impact_harvardUS_all = list(protect_life=list(),
                                   protect_20y=list(),
                                   protect_30y=list(),
                                   protect_life80take = list())

for(run in 1:4)
{
  load(file = paste0('1-dose_batch/harvardUS',run,'/impact_harvardUS',run,'.rdata'))
  impact_harvardUS_all[[run]] = impact_harvardUS
  rm(impact_harvardUS)
}

impact_harvardUS = impact_harvardUS_all
rm(impact_harvardUS_all,run)


save(impact_harvardUS,file = 'output/prime/impact_harvardUS_indirect_batch_new.rdata')
rm(impact_harvardUS)


