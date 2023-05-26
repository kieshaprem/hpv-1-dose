require(dplyr)
require(readxl)
require(data.table)
require(grid)
require(tidyverse)   # data manipulation, exploration and visualization
require(tictoc)      # run time estimation
require(magrittr)    # pipes
require(prime)       # HPV vaccine impact model
require(countrycode) # country codes (iso3c, etc)

# options(scipen=999)

# Insert country ISO 3c code here
COUNTRY 


batch_input <- data.table(
  country_code = COUNTRY,
  year = c(seq(1931,2020,1),seq(2021,2120,1)),
  age_first = c(rep(0,90),rep(10,100)),
  age_last = c(rep(0,90),14,rep(0,99)),
  coverage = c(rep(0,90),rep(0.8,100))
)

batch_input

RegisterBatchData(batch_input, force=TRUE)
output <- BatchRun()

# head(output)
output = data.frame(output)

save(output,file = paste0(COUNTRY,'.rdata'))