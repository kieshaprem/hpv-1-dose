# To estimate the sex ratio at birth (SRB) using population aged (0,5] for males and females, by region, 2095-2100

# read the population aged (0,5] by sex for the period 2095-2100
popsrb = read.csv('input/pop2100_sbr.csv',as.is = TRUE)
popsrb$iso3c = countrycode(popsrb$country,origin = 'country.name',destination = 'iso3c')
popsrb$iso3c[is.na(popsrb$iso3c)] = 'CHI'
popsrb = popsrb[order(popsrb$iso3c),]

# estimate the approximate sex ratio at birth
popsrb$srb_male =  popsrb$males0to4/(popsrb$males0to4+popsrb$females0to4)
