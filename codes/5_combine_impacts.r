# Reading in new Harvard which harmonised the routine age of vaccination (i.e.,  from 9yo to 10yo) "..._new.rdata"
load('output/prime/impact_phe_indirect_batch.rdata')
load('output/prime/impact_hpvadviseIndia_indirect_batch.rdata')
load('output/prime/impact_hpvadviseNigeria_indirect_batch.rdata')
load('output/prime/impact_hpvadviseUganda_indirect_batch.rdata')
load('output/prime/impact_hpvadviseVietnam_indirect_batch.rdata')
load('output/prime/impact_hpvadviseCanada_indirect_batch.rdata')
load('output/prime/impact_harvardElSalvador_indirect_batch_new.rdata')
load('output/prime/impact_harvardNicaragua_indirect_batch_new.rdata')
load('output/prime/impact_harvardUganda_indirect_batch_new.rdata')
load('output/prime/impact_harvardUS_indirect_batch_new.rdata')

impact_all = list(impact_phe = impact_phe,
                  impact_hpvadviseIndia = impact_hpvadviseIndia,
                  impact_hpvadviseNigeria = impact_hpvadviseNigeria,
                  impact_hpvadviseUganda = impact_hpvadviseUganda,
                  impact_hpvadviseVietnam = impact_hpvadviseVietnam,
                  impact_hpvadviseCanada = impact_hpvadviseCanada,
                  impact_harvardUganda = impact_harvardUganda,
                  impact_harvardElSalvador = impact_harvardElSalvador,
                  impact_harvardNicaragua = impact_harvardNicaragua,
                  impact_harvardUS = impact_harvardUS) 

save(impact_all,file = 'output/impact_all.rdata')
# file.info( 'output/impact_all.rdata')$size