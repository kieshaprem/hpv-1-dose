


lt = read.csv('data/population/lifetable.csv',as.is = TRUE)
lt$iso3c = countrycode(sourcevar = lt$country,origin = 'country.name',destination = 'iso3c')
lt[is.na(lt$iso3c),]
lt$country[lt$country %in% 'R\x8eunion'] # REU
lt$iso3c[lt$country %in% 'R\x8eunion'] = 'REU'
lt$country[lt$country %in% 'Cura\x8dao'] = 'CUW'

lt$country[lt$country %in% 'Channel Islands'] = 'CHI'
countries

unique(lt$iso3c)[!(unique(lt$iso3c) %in% unique(countries))]
countries[!(unique(countries) %in% unique(lt$iso3c))]

country = sort(unique(lt$iso3c)[(unique(lt$iso3c) %in% unique(countries))])
year = 2100
t = year - 1999
life = list()
lifemat = data.frame(matrix(NA,nrow = 101,
                            ncol = 3,dimnames = list(paste0('age',0:100),c('Nx','mx','qx'))))
                            # ncol = 10,dimnames = list(paste0('age',0:100),c('Nx','mx','ax','qx','px','lx','dx','Lx','Tx','e0x'))))
for(co in country)
{
  lifemat$Nx = poplist[[co]][,t]
  lifemat$mx[1] = lt$nmx[lt$iso3c %in% co][1]/1
  lifemat$mx[2:5] = lt$nmx[lt$iso3c %in% co][2]/1
  for(i in 3:21) lifemat$mx[((i-2)*5+1):((i-1)*5)] = lt$nmx[lt$iso3c %in% co][i]/1
  lifemat$qx[1] = as.numeric(lt$nqx[lt$iso3c %in% co][1])/1
  lifemat$qx[2:5] =  as.numeric(lt$nqx[lt$iso3c %in% co][2])/1
  for(i in 3:21) lifemat$qx[((i-2)*5+1):((i-1)*5)] =  as.numeric(lt$nqx[lt$iso3c %in% co][i])/1
  lifemat$mx[is.na(lifemat$mx)] = 1
  lifemat$qx[is.na(lifemat$qx)] = 1
  # lt$nqx[lt$iso3c %in% co]
  life[[co]] = lifemat
}

save(life,file = 'data/population/lifetable.rdata')

