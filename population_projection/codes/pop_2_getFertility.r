
# Age-specific fertility rates by region 2095-2100 (births per 1,000 women)

# read the annualised ASFR for the period 2095-2100
asfr = read.csv('input/asfr2095.csv',as.is = TRUE)
# head(asfr)
# asfr$country
asfr$iso3c = countrycode(sourcevar = asfr$country,origin = 'country.name',destination = 'iso3c')

asfr[is.na(asfr$iso3c),]
asfr$iso3c[is.na(asfr$iso3c)] = 'CHI'

tfr = apply(asfr[,2:8],1,function(x) 5*sum(x/1000))

asfr = asfr[order(asfr$iso3c),]
fertility = list()

for(i in 1:nrow(asfr))
{
  fertility[[asfr$iso3c[i]]] = c(rep(0,15),rep(asfr[i,2],5),rep(asfr[i,3],5),rep(asfr[i,4],5),rep(asfr[i,5],5),
                                 rep(asfr[i,6],5),rep(asfr[i,7],5),rep(asfr[i,8],5),rep(0,51))/1000
}

save(fertility,file = 'input/fertility.rdata')

rm(i,asfr)
