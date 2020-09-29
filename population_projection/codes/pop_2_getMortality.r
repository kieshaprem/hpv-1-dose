
# Abridged mortality table, for females, by region, 2095-2100

# read the mortalitytable for the period 2095-2100
lt = read.csv('input/lifetable.csv',as.is = TRUE)
lt$iso3c = countrycode(sourcevar = lt$country,origin = 'country.name',destination = 'iso3c')
lt[is.na(lt$iso3c),]
lt$iso3c[lt$country %in% 'R\x8eunion'] = 'REU'
lt$country[lt$country %in% 'Cura\x8dao'] = 'CUW'
lt$country[lt$country %in% 'Channel Islands'] = 'CHI'

unique(lt$iso3c)[!(unique(lt$iso3c) %in% unique(names(poplist)))]
names(poplist)[!(unique(names(poplist)) %in% unique(lt$iso3c))]

country = sort(unique(lt$iso3c)[(unique(lt$iso3c) %in% unique(names(poplist)))])
year = 2100
t = year - 1999
mortality = list()
mortalitymat = data.frame(matrix(NA,nrow = 101,
                            ncol = 3,dimnames = list(paste0('age',0:100),c('Nx','mx','qx'))))
                            # ncol = 10,dimnames = list(paste0('age',0:100),c('Nx','mx','ax','qx','px','lx','dx','Lx','Tx','e0x'))))
for(co in country)
{
  mortalitymat$Nx = poplist[[co]][,t]
  mortalitymat$mx[1] = lt$nmx[lt$iso3c %in% co][1]/1
  mortalitymat$mx[2:5] = lt$nmx[lt$iso3c %in% co][2]/1
  for(i in 3:21) mortalitymat$mx[((i-2)*5+1):((i-1)*5)] = lt$nmx[lt$iso3c %in% co][i]/1
  mortalitymat$qx[1] = as.numeric(lt$nqx[lt$iso3c %in% co][1])/1
  mortalitymat$qx[2:5] =  as.numeric(lt$nqx[lt$iso3c %in% co][2])/1
  for(i in 3:21) mortalitymat$qx[((i-2)*5+1):((i-1)*5)] =  as.numeric(lt$nqx[lt$iso3c %in% co][i])/1
  mortalitymat$mx[is.na(mortalitymat$mx)] = 1
  mortalitymat$qx[is.na(mortalitymat$qx)] = 1
  # lt$nqx[lt$iso3c %in% co]
  mortality[[co]] = mortalitymat
}

save(mortality,file = 'input/mortality.rdata')

rm(co,i,t,year,lt,mortalitymat,country)

