asfr2020 = read.csv('data/population/asfr2020.csv',as.is = TRUE)
asfr = read.csv('data/population/asfr2095.csv',as.is = TRUE)
head(asfr)
asfr$country
asfr$iso3c = countrycode(sourcevar = asfr$country,origin = 'country.name',destination = 'iso3c')
asfr2020$iso3c = countrycode(sourcevar = asfr2020$country,origin = 'country.name',destination = 'iso3c')
asfr[is.na(asfr$iso3c),]
asfr2020[is.na(asfr2020$iso3c),]
asfr$iso3c[is.na(asfr$iso3c)] = 'CHI'
asfr2020$iso3c[is.na(asfr2020$iso3c)] = 'CHI'

apply(asfr2020[,2:8],1,function(x) 5*sum(x/1000))
apply(asfr[,2:8],1,function(x) sum(x/1000))
sum(asfr2020[asfr2020$iso3c %in% "GBR",2:8]/1000*5)
350/2


asfr = asfr[order(asfr$iso3c),]
fertility = list()

for(i in 1:nrow(asfr))
{
  fertility[[asfr$iso3c[i]]] = c(rep(0,15),rep(asfr[i,2],5),rep(asfr[i,3],5),rep(asfr[i,4],5),rep(asfr[i,5],5),rep(asfr[i,6],5),rep(asfr[i,7],5),rep(asfr[i,8],5),rep(0,51))/1000
}

lapply(fertility, sum)
save(fertility,file = 'data/population/fertility.rdata')
