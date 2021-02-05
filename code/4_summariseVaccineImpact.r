load('output/phe/impact_phe.Rdata')
load('output/hpvadvise/impact_hpvadvise_ind.Rdata')
load('output/hpvadvise/impact_hpvadvise_nga.Rdata')
load('output/hpvadvise/impact_hpvadvise_uga.Rdata')
load('output/hpvadvise/impact_hpvadvise_vnm.Rdata')

load('output/prime/vaccine_impact_phe.rdata')

dim(impact_phe$protect_life[[1]])

temp = impact_phe$protect_life[[1]]
temp= temp[,which(!(colnames(temp) %in% 'run'))]

protect_life = list()
protect_life$phe = impact_phe$protect_life[[1]]
protect_life$hpvadvise_ind = impact_hpvadvise_ind$protect_life[[1]]
protect_life$hpvadvise_nga = impact_hpvadvise_nga$protect_life[[1]]
protect_life$hpvadvise_uga = impact_hpvadvise_uga$protect_life[[1]]
protect_life$hpvadvise_vnm = impact_hpvadvise_vnm$protect_life[[1]]

protect_life$phe[,7:16] =  0.2*(impact_phe$protect_life[[1]][,7:16])+
  0.2*(impact_phe$protect_life[[2]][,7:16])+
  0.2*(impact_phe$protect_life[[3]][,7:16])+
  0.2*(impact_phe$protect_life[[4]][,7:16])+
  0.2*(impact_phe$protect_life[[5]][,7:16])
  

# protect_life$hpvadvise_ind[,7:16] =  0.2*(impact_hpvadvise_ind$protect_life[[1]][,7:16])+
#   0.2*(impact_hpvadvise_ind$protect_life[[2]][,7:16])+
#   0.2*(impact_hpvadvise_ind$protect_life[[3]][,7:16])+
#   0.2*(impact_hpvadvise_ind$protect_life[[4]][,7:16])+
#   0.2*(impact_hpvadvise_ind$protect_life[[5]][,7:16])
# 
# 
# protect_life$hpvadvise_nga[,7:16] =  0.2*(impact_hpvadvise_nga$protect_life[[1]][,7:16])+
#   0.2*(impact_hpvadvise_nga$protect_life[[2]][,7:16])+
#   0.2*(impact_hpvadvise_nga$protect_life[[3]][,7:16])+
#   0.2*(impact_hpvadvise_nga$protect_life[[4]][,7:16])+
#   0.2*(impact_hpvadvise_nga$protect_life[[5]][,7:16])
# 
# 
# protect_life$hpvadvise_uga[,7:16] =  0.2*(impact_hpvadvise_uga$protect_life[[1]][,7:16])+
#   0.2*(impact_hpvadvise_uga$protect_life[[2]][,7:16])+
#   0.2*(impact_hpvadvise_uga$protect_life[[3]][,7:16])+
#   0.2*(impact_hpvadvise_uga$protect_life[[4]][,7:16])+
#   0.2*(impact_hpvadvise_uga$protect_life[[5]][,7:16])
# 
# 
# protect_life$hpvadvise_vnm[,7:16] =  0.2*(impact_hpvadvise_vnm$protect_life[[1]][,7:16])+
#   0.2*(impact_hpvadvise_vnm$protect_life[[2]][,7:16])+
#   0.2*(impact_hpvadvise_vnm$protect_life[[3]][,7:16])+
#   0.2*(impact_hpvadvise_vnm$protect_life[[4]][,7:16])+
#   0.2*(impact_hpvadvise_vnm$protect_life[[5]][,7:16])




protect_10ys = list()
protect_10ys$phe = impact_phe$protect_10ys[[1]]
protect_10ys$hpvadvise_ind = impact_hpvadvise_ind$protect_10ys[[1]]
protect_10ys$hpvadvise_nga = impact_hpvadvise_nga$protect_10ys[[1]]
protect_10ys$hpvadvise_uga = impact_hpvadvise_uga$protect_10ys[[1]]
protect_10ys$hpvadvise_vnm = impact_hpvadvise_vnm$protect_10ys[[1]]

protect_10ys$phe[,7:16] =  0.2*(impact_phe$protect_10ys[[1]][,7:16])+
  0.2*(impact_phe$protect_10ys[[2]][,7:16])+
  0.2*(impact_phe$protect_10ys[[3]][,7:16])+
  0.2*(impact_phe$protect_10ys[[4]][,7:16])+
  0.2*(impact_phe$protect_10ys[[5]][,7:16])


# protect_10ys$hpvadvise_ind[,7:16] =  0.2*(impact_hpvadvise_ind$protect_10ys[[1]][,7:16])+
#   0.2*(impact_hpvadvise_ind$protect_10ys[[2]][,7:16])+
#   0.2*(impact_hpvadvise_ind$protect_10ys[[3]][,7:16])+
#   0.2*(impact_hpvadvise_ind$protect_10ys[[4]][,7:16])+
#   0.2*(impact_hpvadvise_ind$protect_10ys[[5]][,7:16])
# 
# 
# protect_10ys$hpvadvise_nga[,7:16] =  0.2*(impact_hpvadvise_nga$protect_10ys[[1]][,7:16])+
#   0.2*(impact_hpvadvise_nga$protect_10ys[[2]][,7:16])+
#   0.2*(impact_hpvadvise_nga$protect_10ys[[3]][,7:16])+
#   0.2*(impact_hpvadvise_nga$protect_10ys[[4]][,7:16])+
#   0.2*(impact_hpvadvise_nga$protect_10ys[[5]][,7:16])
# 
# 
# protect_10ys$hpvadvise_uga[,7:16] =  0.2*(impact_hpvadvise_uga$protect_10ys[[1]][,7:16])+
#   0.2*(impact_hpvadvise_uga$protect_10ys[[2]][,7:16])+
#   0.2*(impact_hpvadvise_uga$protect_10ys[[3]][,7:16])+
#   0.2*(impact_hpvadvise_uga$protect_10ys[[4]][,7:16])+
#   0.2*(impact_hpvadvise_uga$protect_10ys[[5]][,7:16])
# 
# 
# protect_10ys$hpvadvise_vnm[,7:16] =  0.2*(impact_hpvadvise_vnm$protect_10ys[[1]][,7:16])+
#   0.2*(impact_hpvadvise_vnm$protect_10ys[[2]][,7:16])+
#   0.2*(impact_hpvadvise_vnm$protect_10ys[[3]][,7:16])+
#   0.2*(impact_hpvadvise_vnm$protect_10ys[[4]][,7:16])+
#   0.2*(impact_hpvadvise_vnm$protect_10ys[[5]][,7:16])



protect_20ys = list()
protect_20ys$phe = impact_phe$protect_20ys[[1]]
protect_20ys$hpvadvise_ind = impact_hpvadvise_ind$protect_20ys[[1]]
protect_20ys$hpvadvise_nga = impact_hpvadvise_nga$protect_20ys[[1]]
protect_20ys$hpvadvise_uga = impact_hpvadvise_uga$protect_20ys[[1]]
protect_20ys$hpvadvise_vnm = impact_hpvadvise_vnm$protect_20ys[[1]]

protect_20ys$phe[,7:16] =  0.2*(impact_phe$protect_20ys[[1]][,7:16])+
  0.2*(impact_phe$protect_20ys[[2]][,7:16])+
  0.2*(impact_phe$protect_20ys[[3]][,7:16])+
  0.2*(impact_phe$protect_20ys[[4]][,7:16])+
  0.2*(impact_phe$protect_20ys[[5]][,7:16])


# protect_20ys$hpvadvise_ind[,7:16] =  0.2*(impact_hpvadvise_ind$protect_20ys[[1]][,7:16])+
#   0.2*(impact_hpvadvise_ind$protect_20ys[[2]][,7:16])+
#   0.2*(impact_hpvadvise_ind$protect_20ys[[3]][,7:16])+
#   0.2*(impact_hpvadvise_ind$protect_20ys[[4]][,7:16])+
#   0.2*(impact_hpvadvise_ind$protect_20ys[[5]][,7:16])
# 
# 
# protect_20ys$hpvadvise_nga[,7:16] =  0.2*(impact_hpvadvise_nga$protect_20ys[[1]][,7:16])+
#   0.2*(impact_hpvadvise_nga$protect_20ys[[2]][,7:16])+
#   0.2*(impact_hpvadvise_nga$protect_20ys[[3]][,7:16])+
#   0.2*(impact_hpvadvise_nga$protect_20ys[[4]][,7:16])+
#   0.2*(impact_hpvadvise_nga$protect_20ys[[5]][,7:16])
# 
# 
# protect_20ys$hpvadvise_uga[,7:16] =  0.2*(impact_hpvadvise_uga$protect_20ys[[1]][,7:16])+
#   0.2*(impact_hpvadvise_uga$protect_20ys[[2]][,7:16])+
#   0.2*(impact_hpvadvise_uga$protect_20ys[[3]][,7:16])+
#   0.2*(impact_hpvadvise_uga$protect_20ys[[4]][,7:16])+
#   0.2*(impact_hpvadvise_uga$protect_20ys[[5]][,7:16])
# 
# 
# protect_20ys$hpvadvise_vnm[,7:16] =  0.2*(impact_hpvadvise_vnm$protect_20ys[[1]][,7:16])+
#   0.2*(impact_hpvadvise_vnm$protect_20ys[[2]][,7:16])+
#   0.2*(impact_hpvadvise_vnm$protect_20ys[[3]][,7:16])+
#   0.2*(impact_hpvadvise_vnm$protect_20ys[[4]][,7:16])+
#   0.2*(impact_hpvadvise_vnm$protect_20ys[[5]][,7:16])



save(protect_life,file = 'output/protect_life.RData')
save(protect_10ys,file = 'output/protect_10ys.RData')
save(protect_20ys,file = 'output/protect_20ys.RData')
