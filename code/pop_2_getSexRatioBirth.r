popsrb = read.csv('data/population/pop2100_sbr.csv',as.is = TRUE)
popsrb$iso3c = countrycode(popsrb$country,origin = 'country.name',destination = 'iso3c')
popsrb$iso3c[is.na(popsrb$iso3c)] = 'CHI'
popsrb = popsrb[order(popsrb$iso3c),]

# first, work out the approximate sex ratio at birth
popsrb$srb_male =  popsrb$males0to4/(popsrb$males0to4+popsrb$females0to4)
