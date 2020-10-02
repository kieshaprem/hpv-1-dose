# Get 2100 female population for all PRIME countries
source('codes/pop_1_getPop.r') # a little slow, needs to loop across all regions

# Load 2100 age-specific fertility rates
source('codes/pop_2_getFertility.r')

# Load 2100 age-specific mortality rates (from the UN lifetables)
source('codes/pop_2_getMortality.r')

# Estimate the sex ratio at birth for the year 2100
source('codes/pop_2_getSexRatioBirth.r')

# Project the female population for the years 2101 to 2220 
source('codes/pop_3_projectPop.r')

# Project the female birth cohort for the years 2090 to 2120 
source('codes/pop_3_projectPop.r')