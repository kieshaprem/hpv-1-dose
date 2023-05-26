library(wbstats)
library(countrycode)
library(prime)
load('output/prime/impact_phe_indirect.rdata')
load('data/population/incomegroup.rdata')

countries = as.character(unique(impact_phe[[1]][[1]]$iso3c))

impact_phe$protect_life

all_gdppc =  wb(indicator = "NY.GDP.PCAP.CD", startdate = 2000, enddate = 2017)

getLatestData_indicator = function(ALLDATA){
  TRIMDATA = list()
  TRIMDATA$iso3c = sort(unique(ALLDATA$iso3c))
  TRIMDATA$year = TRIMDATA$value = TRIMDATA$indicator = TRIMDATA$country =  array(NA, length(TRIMDATA$iso3c))
  
  for(iso in 1:length(TRIMDATA$iso3c))
  {
    COUNTRYDATA= ALLDATA[which(ALLDATA$iso3c %in% TRIMDATA$iso3c[iso]),]
    j = which.max(COUNTRYDATA$date)
    TRIMDATA$year[iso] = as.integer(COUNTRYDATA$date[j])
    TRIMDATA$value[iso] = as.numeric(COUNTRYDATA$value[j])
    TRIMDATA$indicator[iso] = (COUNTRYDATA$indicator[j])
    TRIMDATA$country[iso] = as.character(COUNTRYDATA$country[j])
  }
  TRIMDATA = do.call(cbind.data.frame, TRIMDATA)
  TRIMDATA = TRIMDATA[order(TRIMDATA$iso3c),]
  return(TRIMDATA)
}

cleaned_gdppc = getLatestData_indicator(ALLDATA = all_gdppc)
# cleaned_gdppc$value[match(countries,cleaned_gdppc$iso3c)]

costs = data.frame(matrix(NA,nrow = length(countries),ncol = 4))
colnames(costs) = c("vaccine_cost", "treatment_cost", "treatment_cost_adj", "gdp_per_capita")
rownames(costs) = countries
costs$gdp_per_capita = cleaned_gdppc$value[match(rownames(costs),cleaned_gdppc$iso3c)]
rownames(costs)[which(is.na(costs$gdp_per_capita))]
# costs$gdp_per_capita[which(is.na(costs$gdp_per_capita))] = c(25479,18313,26416,1700,6200,314.54)
rm(cleaned_gdppc,all_gdppc,getLatestData_indicator)

costs$vaccine_cost = data.global$`Vaccine price (USD) [4]`[match(rownames(costs),data.global$iso3)] + data.global$`Vaccine delivery/ operational/ admin costs (USD) [5]`[match(rownames(costs),data.global$iso3)]
costs$treatment_cost = data.costcecx$cancer_cost[match(rownames(costs),data.costcecx$iso3)]
costs$treatment_cost_adj = data.costcecx$cancer_cost_adj[match(rownames(costs),data.costcecx$iso3)]
costs$incomegroup = incomegroup$incomegroup[match(rownames(costs),incomegroup$iso3c)]
costs$incomegroup 
aggregate(costs$gdp_per_capita,by = list(costs$incomegroup),max)
costs$gdp_per_capita[is.na(costs$incomegroup)]
costs$incomegroup[is.na(costs$incomegroup)] = c("Upper middle income","Upper middle income","Upper middle income","Lower middle income")
costs$cet = costs$gdp_per_capita
costs$cet[costs$incomegroup %in% "Low income"] = 0.35*costs$gdp_per_capita[costs$incomegroup %in% "Low income"]
costs$cet[!(costs$incomegroup %in% "Low income")] = 0.625*costs$gdp_per_capita[!(costs$incomegroup %in% "Low income")]
costs = costs[,-5]

countrycode(sourcevar = rownames(costs)[which(is.na(costs$treatment_cost_adj))],origin = 'iso3c',destination = 'country.name')
countrycode(sourcevar = rownames(costs)[which(is.na(costs$treatment_cost))],origin = 'iso3c',destination = 'country.name')
countrycode(sourcevar = rownames(costs)[which(is.na(costs$vaccine_cost))],origin = 'iso3c',destination = 'country.name')


save(costs,file = 'output/costs/costs_2022-05-24.rdata')
