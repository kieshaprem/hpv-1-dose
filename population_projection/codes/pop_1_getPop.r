require(dplyr)
require(readxl)
require(data.table)
require(grid)
require(tidyverse)   # data manipulation, exploration and visualization
require(tictoc)      # run time estimation
require(magrittr)    # pipes
require(prime)       # HPV vaccine impact model
require(countrycode) # country codes (iso3c, etc)
require(gridExtra)
require(ggplot2)
require(ggsci)
library(gtools)

options(scipen=999)


pop_all = data.pop
poplist = list()
countries = as.character(unique(pop_all$country_code))


for(co in 1:length(countries))
{
  poplist[[co]] = matrix(pop_all$value[pop_all$country_code %in% countries[co]& pop_all$year %in% 2000:2100],nrow = 101,ncol = 101,byrow = FALSE,
                         dimnames = list(paste0('age',0:100),paste0('year',2000:2100)))
  # print(co)
}
names(poplist) = countries
save(poplist,file = 'input/pop.rdata')

rm(co,pop_all,countries)
