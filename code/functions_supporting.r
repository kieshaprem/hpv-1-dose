## Supporting functions 

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



# Country-specific HPV type distribution in HPV-related cancers. 
# We are interested in the 9-valent vaccine-preventable types. 
hpv_distribution = read.csv('data/population/hpv_distribution.csv',as.is = TRUE)

### 1) Prepare the model outputs 
# input:  data by strategy (rows: age, columns: calendar year) 
# output: list with 3 matrices: [[1]] pre-vaccination for ages 0 to 99: 1x100 (column=age) matrix
#                               [[2]] post-vaccination 100 (rows=calendar year) x 100 (column=age) matrix
#                               [[3]] proportion list (ppl) 100 (rows=calendar year) x 100 (column=age) matrix
prepData = function(DATALIST,STRATEGY)
{
  agevac = 10 
  ages = seq(0,99,1)
  years = seq(2021,2121,1)
  
  # Extract the baseline vector (pre-vac incidence/mortality) for ages 0 to 99: 1x100 (column=age) matrix
  prevac = c(rep(0,10),as.matrix(DATALIST[[STRATEGY]])[,2])
  
  # Extract the post-vac incidence/mortality for ages 0 to 99 by calendar year: 100 (rows=calendar year) x 100 (column=age) matrix
  postvac = cbind(matrix(0,nrow = length(years)-1,ncol = 10),
                  t(as.matrix(DATALIST[[STRATEGY]])[,3:ncol(DATALIST[[STRATEGY]])]))
  # Compute lifelong post-vaccination incidence as a % of pre-vac incidence: post-vac proportion list (ppl): 100 (rows=calendar year) x 100 (column=age) matrix
  ppl = matrix(0, nrow = 100, ncol = 100)
  for(y in 1:100)
  {
    ppl[y,] = postvac[y,]/prevac
  }
  ppl[ppl %in% "NaN"] = 1
  
  output = list(prevac = prevac, postvac=postvac,ppl=ppl)
  return(output)
}
# Test function: prepData(DATALIST = harvard_uga_inc, STRATEGY = 'strategy_life')

prepData_PHE = function(DATALIST = phe,STRATEGY)
{
  # agevac = 10 
  # ages = seq(0,99,1)
  years = seq(0,100,1)
  
  output = simultations = list()
  # prevac = postvac = ppl = list()

  for(run in 1:50)
  {
    # Extract the baseline vector (pre-vac incidence/mortality) for ages 0 to 99: 1x100 (column=age) matrix
    simultations$prevac = c(rep(0,10),as.matrix(DATALIST[[STRATEGY]][[run]][1,]),rep(0,length(75:99)))
    
    # Extract the post-vac incidence/mortality for ages 0 to 99 by calendar year: 100 (rows=calendar year) x 100 (column=age) matrix
    simultations$postvac = cbind(matrix(0,nrow = length(years)-1,ncol = 10),
                    (as.matrix(DATALIST[[STRATEGY]][[run]])[2:101,]),
                    matrix(0,nrow = length(years)-1,ncol = length(75:99)))
    
    # Compute lifelong post-vaccination incidence as a % of pre-vac incidence: post-vac proportion list (ppl): 100 (rows=calendar year) x 100 (column=age) matrix
    ppl = matrix(0, nrow = 100, ncol = 100)
    for(y in 1:100)
    {
      ppl[y,] = simultations$postvac[y,]/simultations$prevac
    }
    ppl[ppl %in% "NaN"] = 1
    simultations$ppl = ppl 
    
    output[[run]] = simultations
  }

  # output = list(prevac = prevac, postvac=postvac,ppl=ppl)
 return(output)
}

prepData_HPVADVISE_old = function(DATALIST,STRATEGY)
{
  # agevac = 10 
  # ages = seq(0,99,1)
  years = seq(0,100,1)
  
  output = simultations = list()
  # prevac = postvac = ppl = list()
  
  for(run in 1:5)
  {
    # Extract the baseline vector (pre-vac incidence/mortality) for ages 0 to 99: 1x100 (column=age) matrix
    simultations$prevac = c(rep(0,10),as.matrix(DATALIST[[STRATEGY]][[run]][1,]),rep(0,length(75:99)))
    
    # Extract the post-vac incidence/mortality for ages 0 to 99 by calendar year: 100 (rows=calendar year) x 100 (column=age) matrix
    simultations$postvac = cbind(matrix(0,nrow = length(years)-1,ncol = 10),
                                 (as.matrix(DATALIST[[STRATEGY]][[run]])[2:101,]),
                                 matrix(0,nrow = length(years)-1,ncol = length(75:99)))
    
    # Compute lifelong post-vaccination incidence as a % of pre-vac incidence: post-vac proportion list (ppl): 100 (rows=calendar year) x 100 (column=age) matrix
    ppl = matrix(0, nrow = 100, ncol = 100)
    for(y in 1:100)
    {
      ppl[y,] = simultations$postvac[y,]/simultations$prevac
    }
    ppl[ppl %in% "NaN"] = 1
    simultations$ppl = ppl 
    
    output[[run]] = simultations
  }
  
  # output = list(prevac = prevac, postvac=postvac,ppl=ppl)
  return(output)
}


prepData_HPVADVISE = function(DATALIST,STRATEGY)
{
  # agevac = 10 
  # ages = seq(0,99,1)
  years = seq(0,100,1)
  
  output = simultations = list()
  # prevac = postvac = ppl = list()
  
  for(run in 1:5)
  {
    # Extract the baseline vector (pre-vac incidence/mortality) for )ages 0 to 99: 1x100 (column=age) matrix
    simultations$prevac = as.matrix(DATALIST[['strategy_novac']][[run]])[2:101,] 
    
    # Extract the post-vac incidence/mortality for ages 0 to 99 by calendar year: 100 (rows=calendar year) x 100 (column=age) matrix
    simultations$postvac = as.matrix(DATALIST[[STRATEGY]][[run]])[2:101,]
    
    # Compute lifelong post-vaccination incidence as a % of pre-vac incidence: post-vac proportion list (ppl): 100 (rows=calendar year) x 100 (column=age) matrix
    ppl = matrix(0, nrow = 100, ncol = 100)
    for(y in 1:100)
    {
      ppl[y,] = simultations$postvac[y,]/simultations$prevac[y,]
    }
    ppl[ppl %in% "NaN"] = 1
    ppl[ppl %in% "Inf"] = 1
    simultations$ppl = ppl 
    
    output[[run]] = simultations
  }
  
  # output = list(prevac = prevac, postvac=postvac,ppl=ppl)
  return(output)
}


prepData_HARVARD = function(DATALIST,STRATEGY)
{
  # agevac = 10 
  # ages = seq(0,99,1)
  years = seq(0,100,1)
  
  output = simultations = list()
  # prevac = postvac = ppl = list()
  
  for(run in 1:3)
  {
    # Extract the baseline vector (pre-vac incidence/mortality) for )ages 0 to 99: 1x100 (column=age) matrix
    simultations$prevac = as.matrix(DATALIST[['strategy_novac']][[run]])[2:101,] 
    
    # Extract the post-vac incidence/mortality for ages 0 to 99 by calendar year: 100 (rows=calendar year) x 100 (column=age) matrix
    simultations$postvac = as.matrix(DATALIST[[STRATEGY]][[run]])[2:101,]
    
    # Compute lifelong post-vaccination incidence as a % of pre-vac incidence: post-vac proportion list (ppl): 100 (rows=calendar year) x 100 (column=age) matrix
    ppl = matrix(0, nrow = 100, ncol = 100)
    for(y in 1:100)
    {
      ppl[y,] = simultations$postvac[y,]/simultations$prevac[y,]
    }
    ppl[ppl %in% "NaN"] = 1
    ppl[ppl %in% "Inf"] = 1
    simultations$ppl = ppl 
    
    output[[run]]  = simultations
  }
  # output = list(prevac = prevac, postvac=postvac,ppl=ppl)
  return(output)
}



### 2) Create a matrix for (lifelong) direct reduction. where only the diagonals are vaccinated. 
# 100 (rows=calendar year) x 100 (column=age) matrix. Age is 0 to 99. Calendar Year is 1 to 100; 0 is the baseline year, unvaccinated.  
# Routine programme for girls at 10 year olds and catch-up up to 14 year olds (5 year cohorts: 10, 11, 12, 13, and 14Y)
createDirectReductionMatrix = function(p_vaccinetypecancer,COVERAGE = 0.8,CATCHUP=TRUE,age_vac=10)
{
   # COVERAGE= 0.8 #80%
   # p_vaccinetypecancer: country-specific HPV type distribution of vaccine-prevaentable cancers (9-valent)
  directReduction = matrix(0, nrow = 100, ncol = 100)
  colnames(directReduction) = paste0('age',0:99)
  rownames(directReduction) = paste0('year',1:100)
  for(y in 1:100)
  {
    agevac_min = age_vac
    agevac_max = age_vac+4+(y-1)*1
    directReduction[y,seq(agevac_min+1,min(c(agevac_max+1,100)),1)] = COVERAGE*(p_vaccinetypecancer/100)
  }
  if(!CATCHUP)
  {
    directReduction = matrix(0, nrow = 100, ncol = 100)
    colnames(directReduction) = paste0('age',0:99)
    rownames(directReduction) = paste0('year',1:100)
    for(y in 1:100)
    {
      agevac_min = age_vac
      agevac_max = age_vac+(y-1)*1
      directReduction[y,seq(agevac_min+1,min(c(agevac_max+1,100)),1)] = COVERAGE*(p_vaccinetypecancer/100)
    }
  }
  return(directReduction)
}


### 3) Compute the reduction due to indirect effects (only lifelong protection).
# 100 (rows=calendar year) x 100 (column=age) matrix. Age is 0 to 99. Calendar Year is 1 to 100; 0 is the baseline year, unvaccinated.  
# Routine programme for girls at 10 year olds and catch-up up to 14 year olds (5 year cohorts: 10, 11, 12, 13, and 14Y)
computeIndirectReduction = function(DATALIST)
{
  indirectReduction = (1 - DATALIST$ppl) - DATALIST$direct_reduction
  if(length(DATALIST$prevac)<1000) 
  {
    index = which(DATALIST$prevac <1)
    indirectReduction[,index] = 0
  }
  if(length(DATALIST$prevac)>1000) 
  {
    index = which(colSums(DATALIST$prevac)<1)
    indirectReduction[,index] = 0
  }
  return(indirectReduction)
}

# 4) Calculate the scenario multiplier (only for 20y/30y/80take)
calculateScenarioMultiplier = function(DATALIST,LIFETIME)
{
  scenario_multiplier = DATALIST$direct_reduction*0
  if(length(DATALIST$prevac)<1000) 
  {
    for(y in 1:nrow(scenario_multiplier))
    {
      nominator = (DATALIST$prevac - DATALIST$postvac[y,])/ DATALIST$prevac
      denominator = (LIFETIME$prevac - LIFETIME$postvac[y,])/ LIFETIME$prevac
      scenario_multiplier[y,] = nominator/denominator
      index = unique(c(which(DATALIST$prevac == 0),which(LIFETIME$prevac < 1),which(denominator==0)))
      scenario_multiplier[y,index] = 1 
    }
  }
  if(length(DATALIST$prevac)>1000) 
  {
    for(y in 1:nrow(scenario_multiplier))
    {
      nominator = (DATALIST$prevac[y,] - DATALIST$postvac[y,])/ DATALIST$prevac[y,]
      denominator = (LIFETIME$prevac[y,] - LIFETIME$postvac[y,])/ LIFETIME$prevac[y,]
      scenario_multiplier[y,] = nominator/denominator
      index = unique(c(which(colSums(DATALIST$prevac) == 0),which(colSums(LIFETIME$prevac) == 0),which(denominator==0),which(denominator=='NaN'),which(nominator=='NaN'),which( scenario_multiplier[y,]=='NaN')))
      scenario_multiplier[y,index] = 1 
    }
  }
  return(scenario_multiplier)
}

# 5) Calculate the indirect cohort reduction (only forlifelong protection) --- Extract probabilistic cohorts
calculateIndirectCohortReduction = function(DATALIST)
{
  indirect_cohort_reduction = matrix(nrow = 100, ncol = 100) 
  rownames(indirect_cohort_reduction) <- paste0("cohort", as.character(c(2021:2120)))
  colnames(indirect_cohort_reduction) <- paste0("age", as.character(c(0:99)))
  
  for (a in 1:100) {  ##
    for (co in 1:100) {
      if (a < 10+1) { indirect_cohort_reduction[co, a] <- 0
      } else if (a + co - 10 - 1 < 101) {
        indirect_cohort_reduction[co, a] <- DATALIST$indirect_reduction[a + co - 10 - 1,a]
      } else {
        indirect_cohort_reduction[co, a] <- indirect_cohort_reduction[co,a - 1]
      }
    }
  }
  return(indirect_cohort_reduction)
}



# 6) Calculate multiplier (only for 20y/30y/80take) 
calculateMultiplier = function(DATALIST)
{
  multiplier = matrix(nrow = 100, ncol = 100) 
  rownames(multiplier) <- paste0("cohort", as.character(c(2021:2120)))
  colnames(multiplier) <- paste0("age", as.character(c(0:99)))
  
  for (a in 1:100) {  ##
    for (co in 1:100) {
      if (a < 10+1) { multiplier[co, a] <- 0
      } else if (a + co - 10 - 1 < 101 & a + co - 10 - 1 > 0) {
        multiplier[co, a] <- DATALIST$scenario_multiplier[a + co - 10 - 1,a]
      } else {
        multiplier[co, a] <- multiplier[co,a - 1]
      }
    }
  }
  
  return(multiplier)
}


# 7) Calculate the indirect cohort reduction for catch up (only for lifelong protection) --- Extract catch-up cohorts (2016-2019).
calculateIndirectCohortReductionMAC = function(DATALIST,CATCHUP = TRUE)
{
  indirect_MACcohort_reduction = matrix(0,nrow = 4, ncol = 100) 
  rownames(indirect_MACcohort_reduction) <- paste0("MACcohort", as.character(c(2017:2020)))
  colnames(indirect_MACcohort_reduction) <- paste0("age", as.character(c(0:99)))
  if(CATCHUP)
  {
    for (a in 1:100)
    {  
      for (co in 61:64)
      { 
        if (a + co - 72 > 0) 
        {
          indirect_MACcohort_reduction[co-60, a] <- DATALIST$indirect_reduction[a + co - 72,a]
        } 
        else{indirect_MACcohort_reduction[co-60, a] <- 0}#indirect_cohort_reduction[co,a - 1]}
      }
    }
  }
  return(indirect_MACcohort_reduction)
}


# 8) Calculate multiplier for catch up cohorts (only for 20y/30y/80take) 
calculateMultiplierMAC = function(DATALIST,CATCHUP = TRUE)
{
  multiplier = matrix(0,nrow = 4, ncol = 100) 
  rownames(multiplier) <- paste0("MACcohort", as.character(c(2017:2020)))
  colnames(multiplier) <- paste0("age", as.character(c(0:99)))
  if(CATCHUP)
  {
  for (a in 1:100) 
  {  ##
    for (co in 61:64) 
    {
      if (a + co - 72 > 0) 
      {
        multiplier[co-60, a] <- DATALIST$scenario_multiplier[a + co - 72,a]
      } 
      else{ multiplier[co-60, a] <- 0 }
    }
  }
  }
  return(multiplier)
}


# 9) Calculate the indirect cohort reduction for prevac cohort (only for lifelong protection) --- Extract prevac cohorts (1956-2019).
calculateIndirectCohortReductionPrevac = function(DATALIST,CATCHUP = FALSE)
{
  if(!CATCHUP)
  {
    agevac = 10
    maxage = 100
    years_prevac = maxage - agevac
    start_year = 2021-maxage + agevac
    end_year = 2020
  }
  if(CATCHUP)
  {
    agevac = 10
    maxage = 100
    years_prevac = maxage - agevac
    start_year = 2021-maxage + agevac
    end_year = 2016
    years_prevac = length(start_year:end_year)
  }
  indirect_prevaccohort_reduction = matrix(nrow = years_prevac, ncol = 100) 
  rownames(indirect_prevaccohort_reduction) <- paste0("prevaccohort", as.character(c(start_year:end_year)))
  colnames(indirect_prevaccohort_reduction) <- paste0("age", as.character(c(0:99)))
  
  for (a in 1:100)
  {  
    for (co in 1:years_prevac)
    { 
      if (a + co - 101 > 0) 
      {
        indirect_prevaccohort_reduction[co, a] <- DATALIST$indirect_reduction[a + co - 101,a]
      } 
      else{indirect_prevaccohort_reduction[co, a] <- 0}#indirect_cohort_reduction[co,a - 1]}
    }
  }
  return(indirect_prevaccohort_reduction)
}

# 10) Calculate multiplier for prevac cohorts (only for 20y/30y/80take) 
calculateMultiplierPrevac = function(DATALIST,CATCHUP = FALSE)
{
  if(!CATCHUP)
  {
    agevac = 10
    maxage = 100
    years_prevac = maxage - agevac
    start_year = 2021-maxage + agevac
    end_year = 2020
  }
  if(CATCHUP)
  {
    agevac = 10
    maxage = 100
    years_prevac = maxage - agevac
    start_year = 2021-maxage + agevac
    end_year = 2016
    years_prevac = length(start_year:end_year)
  }
  multiplier = matrix(nrow = years_prevac, ncol = 100) 
  rownames(multiplier) <- paste0("prevaccohort", as.character(c(start_year:end_year)))
  colnames(multiplier) <- paste0("age", as.character(c(0:99)))
  
  for (a in 1:100) 
  {  ##
    for (co in 1:years_prevac)
    {
      if (a + co - 101 > 0) 
      {
        multiplier[co, a] <- DATALIST$scenario_multiplier[a + co - 101,a]
      } 
      else{ multiplier[co, a] <- 0 }
    }
    
  }
  return(multiplier)
}


# 11) Combine the indirect effects and multipliers for prevac, catch up, postvac cohorts
combineIndirectEffects = function(DATALIST,CATCHUP)
{
  if(CATCHUP) combined_indirect_cohort_reduction <- rbind(DATALIST$indirect_cohort_reduction_prevac, DATALIST$indirect_cohort_reduction_mac, DATALIST$indirect_cohort_reduction)
  if(!CATCHUP)combined_indirect_cohort_reduction <- rbind(DATALIST$indirect_cohort_reduction_prevac, DATALIST$indirect_cohort_reduction)
  return(combined_indirect_cohort_reduction)
}
combineMultiplier = function(DATALIST,CATCHUP)
{
  if(CATCHUP) combined_multiplier <- rbind(DATALIST$multiplier_prevac, DATALIST$multiplier_mac, DATALIST$multiplier)
  if(!CATCHUP)combined_multiplier <- rbind(DATALIST$multiplier_prevac, DATALIST$multiplier) 
  return(combined_multiplier)
}

discount = function(r,STARTYEAR,LASTYEAR)
{
  n = length(2021:LASTYEAR)
  d = 1/((1+r)^seq(0,n,1))
  d= c(rep(1,length(STARTYEAR:2019)),d)
  return(d)
}


getYvalues = function(protect = protect,PROTECT,VAR,REGIONS,SMOOTH=FALSE)
{
  # PROTECT = 1 
  # VAR = 'postvac_cases_all'
  # REGIONS = c("Lower middle income","Upper middle income")
  # 
  YVAL = array(NA,c(length(2020:2209),length(protect_life_group)))
  
  if(length(REGIONS)==1)
  {
    for(model in 1:length(protect_life_group)) YVAL[,model] = protect[[PROTECT]][[model]][protect[[PROTECT]][[model]]$incomegroup %in% REGIONS,VAR]
  }
  if(length(REGIONS)>1)
  {
    for(model in 1:length(protect_life_group)) 
    {
      yval = protect[[PROTECT]][[model]][protect[[PROTECT]][[model]]$incomegroup %in% REGIONS[1],VAR]
      for (reg in 2:length(REGIONS)) {
        yval = cbind(yval,protect[[PROTECT]][[model]][protect[[PROTECT]][[model]]$incomegroup %in% REGIONS[reg],VAR]) 
      }
      YVAL[,model] = rowSums(yval)
    }
  }
  
  if(SMOOTH)
  {
    for(model in 1:length(protect_life_group))
    {
      mod = loess(YVAL[,model] ~ c(2020:2209))
      YVAL[,model] = mod$fitted
    }
  }
  YVAL[YVAL < 0] =0
  return(YVAL)
}

