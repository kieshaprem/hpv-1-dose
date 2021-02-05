load('output/prime/impact_phe.rdata')

load('output/prime/impact_hpvadviseIndia.rdata')
load('output/prime/impact_hpvadviseNigeria.rdata')
load('output/prime/impact_hpvadviseUganda.rdata')
load('output/prime/impact_hpvadviseVietnam.rdata')
load('output/prime/impact_hpvadviseCanada.rdata')

load('output/prime/impact_harvardIndia.rdata')
load('output/prime/impact_harvardUganda.rdata')
load('output/prime/impact_harvardElSalvador.rdata')
load('output/prime/impact_harvardNicaragua.rdata')
load('output/prime/impact_harvardUS.rdata') 



sorter = function(IMPACT)
{
  ord = order(unlist(lapply(IMPACT$protect_life, function(X){sum(X$cases_averted)})))
  for(i in 1:4) IMPACT[[i]] = (IMPACT[[i]][ord])[c(3,2,4,1,5)]
  return(IMPACT)
}

sorter_harvard = function(IMPACT)
{
  ord = order(unlist(lapply(IMPACT$protect_life, function(X){sum(X$cases_averted)})))
  for(i in 1:4) IMPACT[[i]] = (IMPACT[[i]][ord])[c(2,1,3)]
  return(IMPACT)
}


impact_phe = sorter(impact_phe)
impact_hpvadviseIndia = sorter(impact_hpvadviseIndia)
impact_hpvadviseNigeria = sorter(impact_hpvadviseNigeria)
impact_hpvadviseUganda = sorter(impact_hpvadviseUganda)
impact_hpvadviseVietnam = sorter(impact_hpvadviseVietnam)
impact_hpvadviseCanada = sorter(impact_hpvadviseCanada)
impact_harvardElSalvador = sorter_harvard(impact_harvardElSalvador)
impact_harvardIndia = sorter_harvard(impact_harvardIndia)
impact_harvardNicaragua = sorter_harvard(impact_harvardNicaragua)
impact_harvardUganda = sorter_harvard(impact_harvardUganda)
impact_harvardUS = sorter_harvard(impact_harvardUS)

dim(impact_phe$protect_life[[1]])
dim(impact_hpvadviseIndia$protect_life[[1]])
dim(impact_harvardIndia$protect_life[[1]])
temp = impact_phe$protect_life[[1]]
temp= temp[,which(!(colnames(temp) %in% 'run'))]
rm(temp)
protect_life = list()
protect_life$phe = impact_phe$protect_life[[1]]
protect_life$hpvadvise_ind = impact_hpvadviseIndia$protect_life[[1]]
protect_life$hpvadvise_nga = impact_hpvadviseNigeria$protect_life[[1]]
protect_life$hpvadvise_uga = impact_hpvadviseUganda$protect_life[[1]]
protect_life$hpvadvise_vnm = impact_hpvadviseVietnam$protect_life[[1]]
protect_life$hpvadvise_cad = impact_hpvadviseCanada$protect_life[[1]]
protect_life$harvard_ind = impact_harvardIndia$protect_life[[1]]
protect_life$harvard_uga = impact_harvardUganda$protect_life[[1]]
protect_life$harvard_slv = impact_harvardElSalvador$protect_life[[1]]
protect_life$harvard_nic = impact_harvardNicaragua$protect_life[[1]]
protect_life$harvard_us = impact_harvardUS$protect_life[[1]]

sum(protect_life$hpvadvise_ind$cases_averted)

protect_20y = list()
protect_20y$phe = impact_phe$protect_20y[[1]]
protect_20y$hpvadvise_ind = impact_hpvadviseIndia$protect_20y[[1]]
protect_20y$hpvadvise_nga = impact_hpvadviseNigeria$protect_20y[[1]]
protect_20y$hpvadvise_uga = impact_hpvadviseUganda$protect_20y[[1]]
protect_20y$hpvadvise_vnm = impact_hpvadviseVietnam$protect_20y[[1]]
protect_20y$hpvadvise_cad = impact_hpvadviseCanada$protect_20y[[1]]
protect_20y$harvard_ind = impact_harvardIndia$protect_20y[[1]]
protect_20y$harvard_uga = impact_harvardUganda$protect_20y[[1]]
protect_20y$harvard_slv = impact_harvardElSalvador$protect_20y[[1]]
protect_20y$harvard_nic = impact_harvardNicaragua$protect_20y[[1]]
protect_20y$harvard_us = impact_harvardUS$protect_20y[[1]]

protect_30y = list()
protect_30y$phe = impact_phe$protect_30y[[1]]
protect_30y$hpvadvise_ind = impact_hpvadviseIndia$protect_30y[[1]]
protect_30y$hpvadvise_nga = impact_hpvadviseNigeria$protect_30y[[1]]
protect_30y$hpvadvise_uga = impact_hpvadviseUganda$protect_30y[[1]]
protect_30y$hpvadvise_vnm = impact_hpvadviseVietnam$protect_30y[[1]]
protect_30y$hpvadvise_cad = impact_hpvadviseCanada$protect_30y[[1]]
protect_30y$harvard_ind = impact_harvardIndia$protect_30y[[1]]
protect_30y$harvard_uga = impact_harvardUganda$protect_30y[[1]]
protect_30y$harvard_slv = impact_harvardElSalvador$protect_30y[[1]]
protect_30y$harvard_nic = impact_harvardNicaragua$protect_30y[[1]]
protect_30y$harvard_us = impact_harvardUS$protect_30y[[1]]

protect_life80take = list()
protect_life80take$phe = impact_phe$protect_life80take[[1]]
protect_life80take$hpvadvise_ind = impact_hpvadviseIndia$protect_life80take[[1]]
protect_life80take$hpvadvise_nga = impact_hpvadviseNigeria$protect_life80take[[1]]
protect_life80take$hpvadvise_uga = impact_hpvadviseUganda$protect_life80take[[1]]
protect_life80take$hpvadvise_vnm = impact_hpvadviseVietnam$protect_life80take[[1]]
protect_life80take$hpvadvise_cad = impact_hpvadviseCanada$protect_life80take[[1]]
protect_life80take$harvard_ind = impact_harvardIndia$protect_life80take[[1]]
protect_life80take$harvard_uga = impact_harvardUganda$protect_life80take[[1]]
protect_life80take$harvard_slv = impact_harvardElSalvador$protect_life80take[[1]]
protect_life80take$harvard_nic = impact_harvardNicaragua$protect_life80take[[1]]
protect_life80take$harvard_us = impact_harvardUS$protect_life80take[[1]]


save(protect_life,file = 'output/prime/protect_life.rdata')
save(protect_20y,file = 'output/prime/protect_20y.rdata')
save(protect_30y,file = 'output/prime/protect_30y.rdata')
save(protect_life80take,file = 'output/prime/protect_life80take.rdata')

protect_life_p10 = list()
protect_life_p10$phe = impact_phe$protect_life[[4]]
protect_life_p10$hpvadvise_ind = impact_hpvadviseIndia$protect_life[[4]]
protect_life_p10$hpvadvise_nga = impact_hpvadviseNigeria$protect_life[[4]]
protect_life_p10$hpvadvise_uga = impact_hpvadviseUganda$protect_life[[4]]
protect_life_p10$hpvadvise_vnm = impact_hpvadviseVietnam$protect_life[[4]]
protect_life_p10$hpvadvise_cad = impact_hpvadviseCanada$protect_life[[4]]
protect_life_p10$harvard_ind = impact_harvardIndia$protect_life[[2]]
protect_life_p10$harvard_uga = impact_harvardUganda$protect_life[[2]]
protect_life_p10$harvard_slv = impact_harvardElSalvador$protect_life[[2]]
protect_life_p10$harvard_nic = impact_harvardNicaragua$protect_life[[2]]
protect_life_p10$harvard_us = impact_harvardUS$protect_life[[2]]


protect_20y_p10 = list()
protect_20y_p10$phe = impact_phe$protect_20y[[4]]
protect_20y_p10$hpvadvise_ind = impact_hpvadviseIndia$protect_20y[[4]]
protect_20y_p10$hpvadvise_nga = impact_hpvadviseNigeria$protect_20y[[4]]
protect_20y_p10$hpvadvise_uga = impact_hpvadviseUganda$protect_20y[[4]]
protect_20y_p10$hpvadvise_vnm = impact_hpvadviseVietnam$protect_20y[[4]]
protect_20y_p10$hpvadvise_cad = impact_hpvadviseCanada$protect_20y[[4]]
protect_20y_p10$harvard_ind = impact_harvardIndia$protect_20y[[2]]
protect_20y_p10$harvard_uga = impact_harvardUganda$protect_20y[[2]]
protect_20y_p10$harvard_slv = impact_harvardElSalvador$protect_20y[[2]]
protect_20y_p10$harvard_nic = impact_harvardNicaragua$protect_20y[[2]]
protect_20y_p10$harvard_us = impact_harvardUS$protect_20y[[2]]

protect_30y_p10 = list()
protect_30y_p10$phe = impact_phe$protect_30y[[4]]
protect_30y_p10$hpvadvise_ind = impact_hpvadviseIndia$protect_30y[[4]]
protect_30y_p10$hpvadvise_nga = impact_hpvadviseNigeria$protect_30y[[4]]
protect_30y_p10$hpvadvise_uga = impact_hpvadviseUganda$protect_30y[[4]]
protect_30y_p10$hpvadvise_vnm = impact_hpvadviseVietnam$protect_30y[[4]]
protect_30y_p10$hpvadvise_cad = impact_hpvadviseCanada$protect_30y[[4]]
protect_30y_p10$harvard_ind = impact_harvardIndia$protect_30y[[2]]
protect_30y_p10$harvard_uga = impact_harvardUganda$protect_30y[[2]]
protect_30y_p10$harvard_slv = impact_harvardElSalvador$protect_30y[[2]]
protect_30y_p10$harvard_nic = impact_harvardNicaragua$protect_30y[[2]]
protect_30y_p10$harvard_us = impact_harvardUS$protect_30y[[2]]

protect_life80take_p10 = list()
protect_life80take_p10$phe = impact_phe$protect_life80take[[4]]
protect_life80take_p10$hpvadvise_ind = impact_hpvadviseIndia$protect_life80take[[4]]
protect_life80take_p10$hpvadvise_nga = impact_hpvadviseNigeria$protect_life80take[[4]]
protect_life80take_p10$hpvadvise_uga = impact_hpvadviseUganda$protect_life80take[[4]]
protect_life80take_p10$hpvadvise_vnm = impact_hpvadviseVietnam$protect_life80take[[4]]
protect_life80take_p10$hpvadvise_cad = impact_hpvadviseCanada$protect_life80take[[4]]
protect_life80take_p10$harvard_ind = impact_harvardIndia$protect_life80take[[2]]
protect_life80take_p10$harvard_uga = impact_harvardUganda$protect_life80take[[2]]
protect_life80take_p10$harvard_slv = impact_harvardElSalvador$protect_life80take[[2]]
protect_life80take_p10$harvard_nic = impact_harvardNicaragua$protect_life80take[[2]]
protect_life80take_p10$harvard_us = impact_harvardUS$protect_life80take[[2]]


save(protect_life_p10,file = 'output/prime/protect_life_10prec.rdata')
save(protect_20y_p10,file = 'output/prime/protect_20y_10prec.rdata')
save(protect_30y_p10,file = 'output/prime/protect_30y_10prec.rdata')
save(protect_life80take_p10,file = 'output/prime/protect_life80take_10prec.rdata')

protect_life_p90 = list()
protect_life_p90$phe = impact_phe$protect_life[[5]]
protect_life_p90$hpvadvise_ind = impact_hpvadviseIndia$protect_life[[5]]
protect_life_p90$hpvadvise_nga = impact_hpvadviseNigeria$protect_life[[5]]
protect_life_p90$hpvadvise_uga = impact_hpvadviseUganda$protect_life[[5]]
protect_life_p90$hpvadvise_vnm = impact_hpvadviseVietnam$protect_life[[5]]
protect_life_p90$hpvadvise_cad = impact_hpvadviseCanada$protect_life[[5]]
protect_life_p90$harvard_ind = impact_harvardIndia$protect_life[[3]]
protect_life_p90$harvard_uga = impact_harvardUganda$protect_life[[3]]
protect_life_p90$harvard_slv = impact_harvardElSalvador$protect_life[[3]]
protect_life_p90$harvard_nic = impact_harvardNicaragua$protect_life[[3]]
protect_life_p90$harvard_us = impact_harvardUS$protect_life[[3]]


protect_20y_p90 = list()
protect_20y_p90$phe = impact_phe$protect_20y[[5]]
protect_20y_p90$hpvadvise_ind = impact_hpvadviseIndia$protect_20y[[5]]
protect_20y_p90$hpvadvise_nga = impact_hpvadviseNigeria$protect_20y[[5]]
protect_20y_p90$hpvadvise_uga = impact_hpvadviseUganda$protect_20y[[5]]
protect_20y_p90$hpvadvise_vnm = impact_hpvadviseVietnam$protect_20y[[5]]
protect_20y_p90$hpvadvise_cad = impact_hpvadviseCanada$protect_20y[[5]]
protect_20y_p90$harvard_ind = impact_harvardIndia$protect_20y[[3]]
protect_20y_p90$harvard_uga = impact_harvardUganda$protect_20y[[3]]
protect_20y_p90$harvard_slv = impact_harvardElSalvador$protect_20y[[3]]
protect_20y_p90$harvard_nic = impact_harvardNicaragua$protect_20y[[3]]
protect_20y_p90$harvard_us = impact_harvardUS$protect_20y[[3]]

protect_30y_p90 = list()
protect_30y_p90$phe = impact_phe$protect_30y[[5]]
protect_30y_p90$hpvadvise_ind = impact_hpvadviseIndia$protect_30y[[5]]
protect_30y_p90$hpvadvise_nga = impact_hpvadviseNigeria$protect_30y[[5]]
protect_30y_p90$hpvadvise_uga = impact_hpvadviseUganda$protect_30y[[5]]
protect_30y_p90$hpvadvise_vnm = impact_hpvadviseVietnam$protect_30y[[5]]
protect_30y_p90$hpvadvise_cad = impact_hpvadviseCanada$protect_30y[[5]]
protect_30y_p90$harvard_ind = impact_harvardIndia$protect_30y[[3]]
protect_30y_p90$harvard_uga = impact_harvardUganda$protect_30y[[3]]
protect_30y_p90$harvard_slv = impact_harvardElSalvador$protect_30y[[3]]
protect_30y_p90$harvard_nic = impact_harvardNicaragua$protect_30y[[3]]
protect_30y_p90$harvard_us = impact_harvardUS$protect_30y[[3]]

protect_life80take_p90 = list()
protect_life80take_p90$phe = impact_phe$protect_life80take[[5]]
protect_life80take_p90$hpvadvise_ind = impact_hpvadviseIndia$protect_life80take[[5]]
protect_life80take_p90$hpvadvise_nga = impact_hpvadviseNigeria$protect_life80take[[5]]
protect_life80take_p90$hpvadvise_uga = impact_hpvadviseUganda$protect_life80take[[5]]
protect_life80take_p90$hpvadvise_vnm = impact_hpvadviseVietnam$protect_life80take[[5]]
protect_life80take_p90$hpvadvise_cad = impact_hpvadviseCanada$protect_life80take[[5]]
protect_life80take_p90$harvard_ind = impact_harvardIndia$protect_life80take[[3]]
protect_life80take_p90$harvard_uga = impact_harvardUganda$protect_life80take[[3]]
protect_life80take_p90$harvard_slv = impact_harvardElSalvador$protect_life80take[[3]]
protect_life80take_p90$harvard_nic = impact_harvardNicaragua$protect_life80take[[3]]
protect_life80take_p90$harvard_us = impact_harvardUS$protect_life80take[[3]]


save(protect_life_p90,file = 'output/prime/protect_life_90prec.rdata')
save(protect_20y_p90,file = 'output/prime/protect_20y_90prec.rdata')
save(protect_30y_p90,file = 'output/prime/protect_30y_90prec.rdata')
save(protect_life80take_p90,file = 'output/prime/protect_life80take_90prec.rdata')

