source('code/functions_supporting.r')
source('code/1_loadData_harvard.r')

s_life_harvardIndia = prepData_HARVARD(DATALIST = harvardIndia,STRATEGY = 'strategy_life')
s_20y_harvardIndia = prepData_HARVARD(DATALIST = harvardIndia,STRATEGY = 'strategy_20y')
s_30y_harvardIndia = prepData_HARVARD(DATALIST = harvardIndia,STRATEGY = 'strategy_30y')
s_life80take_harvardIndia = prepData_HARVARD(DATALIST = harvardIndia,STRATEGY = 'strategy_life80take')
s_novac_harvardIndia = prepData_HARVARD(DATALIST = harvardIndia,STRATEGY = 'strategy_novac')

s_life_harvardUganda = prepData_HARVARD(DATALIST = harvardUganda,STRATEGY = 'strategy_life')
s_20y_harvardUganda = prepData_HARVARD(DATALIST = harvardUganda,STRATEGY = 'strategy_20y')
s_30y_harvardUganda = prepData_HARVARD(DATALIST = harvardUganda,STRATEGY = 'strategy_30y')
s_life80take_harvardUganda = prepData_HARVARD(DATALIST = harvardUganda,STRATEGY = 'strategy_life80take')
s_novac_harvardUganda = prepData_HARVARD(DATALIST = harvardUganda,STRATEGY = 'strategy_novac')

s_life_harvardElSalvador = prepData_HARVARD(DATALIST = harvardElSalvador,STRATEGY = 'strategy_life')
s_20y_harvardElSalvador = prepData_HARVARD(DATALIST = harvardElSalvador,STRATEGY = 'strategy_20y')
s_30y_harvardElSalvador = prepData_HARVARD(DATALIST = harvardElSalvador,STRATEGY = 'strategy_30y')
s_life80take_harvardElSalvador = prepData_HARVARD(DATALIST = harvardElSalvador,STRATEGY = 'strategy_life80take')
s_novac_harvardElSalvador = prepData_HARVARD(DATALIST = harvardElSalvador,STRATEGY = 'strategy_novac')

s_life_harvardNicaragua = prepData_HARVARD(DATALIST = harvardNicaragua,STRATEGY = 'strategy_life')
s_20y_harvardNicaragua = prepData_HARVARD(DATALIST = harvardNicaragua,STRATEGY = 'strategy_20y')
s_30y_harvardNicaragua = prepData_HARVARD(DATALIST = harvardNicaragua,STRATEGY = 'strategy_30y')
s_life80take_harvardNicaragua = prepData_HARVARD(DATALIST = harvardNicaragua,STRATEGY = 'strategy_life80take')
s_novac_harvardNicaragua = prepData_HARVARD(DATALIST = harvardNicaragua,STRATEGY = 'strategy_novac')

s_life_harvardUS = prepData_HARVARD(DATALIST = harvardUS,STRATEGY = 'strategy_life')
s_20y_harvardUS = prepData_HARVARD(DATALIST = harvardUS,STRATEGY = 'strategy_20y')
s_30y_harvardUS = prepData_HARVARD(DATALIST = harvardUS,STRATEGY = 'strategy_30y')
s_life80take_harvardUS = prepData_HARVARD(DATALIST = harvardUS,STRATEGY = 'strategy_life80take')
s_novac_harvardUS = prepData_HARVARD(DATALIST = harvardUS,STRATEGY = 'strategy_novac')



rm(harvardElSalvador,harvardIndia,harvardUganda,harvardNicaragua,harvardUS)

# get the direct reduction 
for(i in 1:length(s_life_harvardIndia))
{
  s_life_harvardIndia[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "IND"])
  s_20y_harvardIndia[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "IND"])
  s_30y_harvardIndia[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "IND"])
  s_life80take_harvardIndia[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "IND"])
  # s_novac_harvardIndia[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "IND"])
  
  s_life_harvardUganda[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "UGA"])
  s_20y_harvardUganda[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "UGA"])
  s_30y_harvardUganda[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "UGA"])
  s_life80take_harvardUganda[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "UGA"])
  # s_novac_harvardUganda[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "UGA"])
  
  s_life_harvardElSalvador[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "SLV"])
  s_20y_harvardElSalvador[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "SLV"])
  s_30y_harvardElSalvador[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "SLV"])
  s_life80take_harvardElSalvador[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "SLV"])
  # s_novac_harvardElSalvador[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "SLV"])

  s_life_harvardNicaragua[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "NIC"])
  s_20y_harvardNicaragua[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "NIC"])
  s_30y_harvardNicaragua[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "NIC"])
  s_life80take_harvardNicaragua[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "NIC"])
  # s_novac_harvardNicaragua[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "NIC"])
  
  s_life_harvardUS[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "USA"])
  s_20y_harvardUS[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "USA"])
  s_30y_harvardUS[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "USA"])
  s_life80take_harvardUS[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "USA"])
  # s_novac_harvardUS[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "USA"])
  
}


# get the indirect reduction (only lifelong protection) 
for(i in 1:length(s_life_harvardIndia))
{
  s_life_harvardIndia[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardIndia[[i]])
  s_20y_harvardIndia[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardIndia[[i]])
  s_30y_harvardIndia[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardIndia[[i]])
  s_life80take_harvardIndia[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardIndia[[i]])
  # s_novac_harvardIndia[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardIndia[[i]])
  
  s_life_harvardUganda[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardUganda[[i]])
  s_20y_harvardUganda[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardUganda[[i]])
  s_30y_harvardUganda[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardUganda[[i]])
  s_life80take_harvardUganda[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardUganda[[i]])
  # s_novac_harvardUganda[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardUganda[[i]])
  
  s_life_harvardElSalvador[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardElSalvador[[i]])
  s_20y_harvardElSalvador[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardElSalvador[[i]])
  s_30y_harvardElSalvador[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardElSalvador[[i]])
  s_life80take_harvardElSalvador[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardElSalvador[[i]])
  # s_novac_harvardElSalvador[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardElSalvador[[i]])
  
  s_life_harvardNicaragua[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardNicaragua[[i]])
  s_20y_harvardNicaragua[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardNicaragua[[i]])
  s_30y_harvardNicaragua[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardNicaragua[[i]])
  s_life80take_harvardNicaragua[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardNicaragua[[i]])
  # s_novac_harvardNicaragua[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardNicaragua[[i]])
  
  s_life_harvardUS[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardUS[[i]])
  s_20y_harvardUS[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardUS[[i]])
  s_30y_harvardUS[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardUS[[i]])
  s_life80take_harvardUS[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardUS[[i]])
  # s_novac_harvardUS[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_harvardUS[[i]])
}


# get the the scenario multiplier (only for 20y/30y/80take)
for(i in 1:length(s_life_harvardIndia))
{
  s_life_harvardIndia[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life_harvardIndia[[i]],LIFETIME = s_life_harvardIndia[[i]])
  s_20y_harvardIndia[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_20y_harvardIndia[[i]],LIFETIME = s_life_harvardIndia[[i]])
  s_30y_harvardIndia[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_30y_harvardIndia[[i]],LIFETIME = s_life_harvardIndia[[i]])
  s_life80take_harvardIndia[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life80take_harvardIndia[[i]],LIFETIME = s_life_harvardIndia[[i]])
  # s_novac_harvardIndia[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_novac_harvardIndia[[i]],LIFETIME = s_life_harvardIndia[[i]])
  
  s_life_harvardUganda[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life_harvardUganda[[i]],LIFETIME = s_life_harvardUganda[[i]])
  s_20y_harvardUganda[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_20y_harvardUganda[[i]],LIFETIME = s_life_harvardUganda[[i]])
  s_30y_harvardUganda[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_30y_harvardUganda[[i]],LIFETIME = s_life_harvardUganda[[i]])
  s_life80take_harvardUganda[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life80take_harvardUganda[[i]],LIFETIME = s_life_harvardUganda[[i]])
  # s_novac_harvardUganda[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_novac_harvardUganda[[i]],LIFETIME = s_life_harvardUganda[[i]])
  
  s_life_harvardNicaragua[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life_harvardNicaragua[[i]],LIFETIME = s_life_harvardNicaragua[[i]])
  s_20y_harvardNicaragua[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_20y_harvardNicaragua[[i]],LIFETIME = s_life_harvardNicaragua[[i]])
  s_30y_harvardNicaragua[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_30y_harvardNicaragua[[i]],LIFETIME = s_life_harvardNicaragua[[i]])
  s_life80take_harvardNicaragua[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life80take_harvardNicaragua[[i]],LIFETIME = s_life_harvardNicaragua[[i]])
  # s_novac_harvardNicaragua[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_novac_harvardNicaragua[[i]],LIFETIME = s_life_harvardNicaragua[[i]])
  
  s_life_harvardElSalvador[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life_harvardElSalvador[[i]],LIFETIME = s_life_harvardElSalvador[[i]])
  s_20y_harvardElSalvador[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_20y_harvardElSalvador[[i]],LIFETIME = s_life_harvardElSalvador[[i]])
  s_30y_harvardElSalvador[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_30y_harvardElSalvador[[i]],LIFETIME = s_life_harvardElSalvador[[i]])
  s_life80take_harvardElSalvador[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life80take_harvardElSalvador[[i]],LIFETIME = s_life_harvardElSalvador[[i]])
  # s_novac_harvardElSalvador[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_novac_harvardElSalvador[[i]],LIFETIME = s_life_harvardElSalvador[[i]])
  
  s_life_harvardUS[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life_harvardUS[[i]],LIFETIME = s_life_harvardUS[[i]])
  s_20y_harvardUS[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_20y_harvardUS[[i]],LIFETIME = s_life_harvardUS[[i]])
  s_30y_harvardUS[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_30y_harvardUS[[i]],LIFETIME = s_life_harvardUS[[i]])
  s_life80take_harvardUS[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life80take_harvardUS[[i]],LIFETIME = s_life_harvardUS[[i]])
  # s_novac_harvardUS[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_novac_harvardUS[[i]],LIFETIME = s_life_harvardUS[[i]])
}


# get the the indirect cohort reduction (only lifelong protection)
for(i in 1:length(s_life_harvardIndia))
{
  s_life_harvardIndia[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life_harvardIndia[[i]])
  s_20y_harvardIndia[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_20y_harvardIndia[[i]])
  s_30y_harvardIndia[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_30y_harvardIndia[[i]])
  s_life80take_harvardIndia[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life80take_harvardIndia[[i]])
  
  s_life_harvardUganda[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life_harvardUganda[[i]])
  s_20y_harvardUganda[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_20y_harvardUganda[[i]])
  s_30y_harvardUganda[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_30y_harvardUganda[[i]])
  s_life80take_harvardUganda[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life80take_harvardUganda[[i]])
  
  s_life_harvardNicaragua[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life_harvardNicaragua[[i]])
  s_20y_harvardNicaragua[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_20y_harvardNicaragua[[i]])
  s_30y_harvardNicaragua[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_30y_harvardNicaragua[[i]])
  s_life80take_harvardNicaragua[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life80take_harvardNicaragua[[i]])
  
  
  s_life_harvardElSalvador[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life_harvardElSalvador[[i]])
  s_20y_harvardElSalvador[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_20y_harvardElSalvador[[i]])
  s_30y_harvardElSalvador[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_30y_harvardElSalvador[[i]])
  s_life80take_harvardElSalvador[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life80take_harvardElSalvador[[i]])
  
  s_life_harvardUS[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life_harvardUS[[i]])
  s_20y_harvardUS[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_20y_harvardUS[[i]])
  s_30y_harvardUS[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_30y_harvardUS[[i]])
  s_life80take_harvardUS[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life80take_harvardUS[[i]])
}


# get the the multiplier (only for 20y/30y/80take)
for(i in 1:length(s_life_harvardIndia))
{
  s_life_harvardIndia[[i]]$multiplier = calculateMultiplier(DATALIST = s_life_harvardIndia[[i]])
  s_20y_harvardIndia[[i]]$multiplier = calculateMultiplier(DATALIST = s_20y_harvardIndia[[i]])
  s_30y_harvardIndia[[i]]$multiplier = calculateMultiplier(DATALIST = s_30y_harvardIndia[[i]])
  s_life80take_harvardIndia[[i]]$multiplier = calculateMultiplier(DATALIST = s_life80take_harvardIndia[[i]])
  
  s_life_harvardUganda[[i]]$multiplier = calculateMultiplier(DATALIST = s_life_harvardUganda[[i]])
  s_20y_harvardUganda[[i]]$multiplier = calculateMultiplier(DATALIST = s_20y_harvardUganda[[i]])
  s_30y_harvardUganda[[i]]$multiplier = calculateMultiplier(DATALIST = s_30y_harvardUganda[[i]])
  s_life80take_harvardUganda[[i]]$multiplier = calculateMultiplier(DATALIST = s_life80take_harvardUganda[[i]])
  
  s_life_harvardNicaragua[[i]]$multiplier = calculateMultiplier(DATALIST = s_life_harvardNicaragua[[i]])
  s_20y_harvardNicaragua[[i]]$multiplier = calculateMultiplier(DATALIST = s_20y_harvardNicaragua[[i]])
  s_30y_harvardNicaragua[[i]]$multiplier = calculateMultiplier(DATALIST = s_30y_harvardNicaragua[[i]])
  s_life80take_harvardNicaragua[[i]]$multiplier = calculateMultiplier(DATALIST = s_life80take_harvardNicaragua[[i]])
  
  s_life_harvardElSalvador[[i]]$multiplier = calculateMultiplier(DATALIST = s_life_harvardElSalvador[[i]])
  s_20y_harvardElSalvador[[i]]$multiplier = calculateMultiplier(DATALIST = s_20y_harvardElSalvador[[i]])
  s_30y_harvardElSalvador[[i]]$multiplier = calculateMultiplier(DATALIST = s_30y_harvardElSalvador[[i]])
  s_life80take_harvardElSalvador[[i]]$multiplier = calculateMultiplier(DATALIST = s_life80take_harvardElSalvador[[i]])
  
  s_life_harvardUS[[i]]$multiplier = calculateMultiplier(DATALIST = s_life_harvardUS[[i]])
  s_20y_harvardUS[[i]]$multiplier = calculateMultiplier(DATALIST = s_20y_harvardUS[[i]])
  s_30y_harvardUS[[i]]$multiplier = calculateMultiplier(DATALIST = s_30y_harvardUS[[i]])
  s_life80take_harvardUS[[i]]$multiplier = calculateMultiplier(DATALIST = s_life80take_harvardUS[[i]])
  
}

# get the the indirect cohort reduction for MAC (only catchup)
for(i in 1:length(s_life_harvardIndia))
{
  s_life_harvardIndia[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life_harvardIndia[[i]],CATCHUP = TRUE)
  s_20y_harvardIndia[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_20y_harvardIndia[[i]],CATCHUP = TRUE)
  s_30y_harvardIndia[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_30y_harvardIndia[[i]],CATCHUP = TRUE)
  s_life80take_harvardIndia[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life80take_harvardIndia[[i]],CATCHUP = TRUE)
  
  s_life_harvardUganda[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life_harvardUganda[[i]],CATCHUP = TRUE)
  s_20y_harvardUganda[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_20y_harvardUganda[[i]],CATCHUP = TRUE)
  s_30y_harvardUganda[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_30y_harvardUganda[[i]],CATCHUP = TRUE)
  s_life80take_harvardUganda[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life80take_harvardUganda[[i]],CATCHUP = TRUE)
  
  s_life_harvardNicaragua[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life_harvardNicaragua[[i]],CATCHUP = TRUE)
  s_20y_harvardNicaragua[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_20y_harvardNicaragua[[i]],CATCHUP = TRUE)
  s_30y_harvardNicaragua[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_30y_harvardNicaragua[[i]],CATCHUP = TRUE)
  s_life80take_harvardNicaragua[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life80take_harvardNicaragua[[i]],CATCHUP = TRUE)
  
  s_life_harvardElSalvador[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life_harvardElSalvador[[i]],CATCHUP = TRUE)
  s_20y_harvardElSalvador[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_20y_harvardElSalvador[[i]],CATCHUP = TRUE)
  s_30y_harvardElSalvador[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_30y_harvardElSalvador[[i]],CATCHUP = TRUE)
  s_life80take_harvardElSalvador[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life80take_harvardElSalvador[[i]],CATCHUP = TRUE)
  
  s_life_harvardUS[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life_harvardUS[[i]],CATCHUP = TRUE)
  s_20y_harvardUS[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_20y_harvardUS[[i]],CATCHUP = TRUE)
  s_30y_harvardUS[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_30y_harvardUS[[i]],CATCHUP = TRUE)
  s_life80take_harvardUS[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life80take_harvardUS[[i]],CATCHUP = TRUE)
  
}

# get the the multiplier for MAC (only for catchup)
for(i in 1:length(s_life_harvardIndia))
{
  s_life_harvardIndia[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life_harvardIndia[[i]],CATCHUP = TRUE)
  s_20y_harvardIndia[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_20y_harvardIndia[[i]],CATCHUP = TRUE)
  s_30y_harvardIndia[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_30y_harvardIndia[[i]],CATCHUP = TRUE)
  s_life80take_harvardIndia[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life80take_harvardIndia[[i]],CATCHUP = TRUE)
  
  s_life_harvardUganda[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life_harvardUganda[[i]],CATCHUP = TRUE)
  s_20y_harvardUganda[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_20y_harvardUganda[[i]],CATCHUP = TRUE)
  s_30y_harvardUganda[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_30y_harvardUganda[[i]],CATCHUP = TRUE)
  s_life80take_harvardUganda[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life80take_harvardUganda[[i]],CATCHUP = TRUE)
  
  s_life_harvardNicaragua[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life_harvardNicaragua[[i]],CATCHUP = TRUE)
  s_20y_harvardNicaragua[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_20y_harvardNicaragua[[i]],CATCHUP = TRUE)
  s_30y_harvardNicaragua[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_30y_harvardNicaragua[[i]],CATCHUP = TRUE)
  s_life80take_harvardNicaragua[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life80take_harvardNicaragua[[i]],CATCHUP = TRUE)
  
  s_life_harvardElSalvador[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life_harvardElSalvador[[i]],CATCHUP = TRUE)
  s_20y_harvardElSalvador[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_20y_harvardElSalvador[[i]],CATCHUP = TRUE)
  s_30y_harvardElSalvador[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_30y_harvardElSalvador[[i]],CATCHUP = TRUE)
  s_life80take_harvardElSalvador[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life80take_harvardElSalvador[[i]],CATCHUP = TRUE)
  
  s_life_harvardUS[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life_harvardUS[[i]],CATCHUP = TRUE)
  s_20y_harvardUS[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_20y_harvardUS[[i]],CATCHUP = TRUE)
  s_30y_harvardUS[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_30y_harvardUS[[i]],CATCHUP = TRUE)
  s_life80take_harvardUS[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life80take_harvardUS[[i]],CATCHUP = TRUE)
}


# get the the indirect cohort reduction for prevac cohort (only for lifelong)
for(i in 1:length(s_life_harvardIndia))
{
  s_life_harvardIndia[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life_harvardIndia[[i]],CATCHUP = TRUE)
  s_20y_harvardIndia[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_20y_harvardIndia[[i]],CATCHUP = TRUE)
  s_30y_harvardIndia[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_30y_harvardIndia[[i]],CATCHUP = TRUE)
  s_life80take_harvardIndia[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life80take_harvardIndia[[i]],CATCHUP = TRUE)
  
  s_life_harvardUganda[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life_harvardUganda[[i]],CATCHUP = TRUE)
  s_20y_harvardUganda[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_20y_harvardUganda[[i]],CATCHUP = TRUE)
  s_30y_harvardUganda[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_30y_harvardUganda[[i]],CATCHUP = TRUE)
  s_life80take_harvardUganda[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life80take_harvardUganda[[i]],CATCHUP = TRUE)
  
  s_life_harvardNicaragua[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life_harvardNicaragua[[i]],CATCHUP = TRUE)
  s_20y_harvardNicaragua[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_20y_harvardNicaragua[[i]],CATCHUP = TRUE)
  s_30y_harvardNicaragua[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_30y_harvardNicaragua[[i]],CATCHUP = TRUE)
  s_life80take_harvardNicaragua[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life80take_harvardNicaragua[[i]],CATCHUP = TRUE)
  
  s_life_harvardElSalvador[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life_harvardElSalvador[[i]],CATCHUP = TRUE)
  s_20y_harvardElSalvador[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_20y_harvardElSalvador[[i]],CATCHUP = TRUE)
  s_30y_harvardElSalvador[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_30y_harvardElSalvador[[i]],CATCHUP = TRUE)
  s_life80take_harvardElSalvador[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life80take_harvardElSalvador[[i]],CATCHUP = TRUE)
  
  s_life_harvardUS[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life_harvardUS[[i]],CATCHUP = TRUE)
  s_20y_harvardUS[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_20y_harvardUS[[i]],CATCHUP = TRUE)
  s_30y_harvardUS[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_30y_harvardUS[[i]],CATCHUP = TRUE)
  s_life80take_harvardUS[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life80take_harvardUS[[i]],CATCHUP = TRUE)
  
}

# get the the multiplier for prevac cohort (only for 20y/30y/80take)
for(i in 1:length(s_life_harvardIndia))
{
  s_life_harvardIndia[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life_harvardIndia[[i]],CATCHUP = TRUE)
  s_20y_harvardIndia[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_20y_harvardIndia[[i]],CATCHUP = TRUE)
  s_30y_harvardIndia[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_30y_harvardIndia[[i]],CATCHUP = TRUE)
  s_life80take_harvardIndia[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life80take_harvardIndia[[i]],CATCHUP = TRUE)
  
  s_life_harvardUganda[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life_harvardUganda[[i]],CATCHUP = TRUE)
  s_20y_harvardUganda[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_20y_harvardUganda[[i]],CATCHUP = TRUE)
  s_30y_harvardUganda[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_30y_harvardUganda[[i]],CATCHUP = TRUE)
  s_life80take_harvardUganda[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life80take_harvardUganda[[i]],CATCHUP = TRUE)
  
  s_life_harvardNicaragua[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life_harvardNicaragua[[i]],CATCHUP = TRUE)
  s_20y_harvardNicaragua[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_20y_harvardNicaragua[[i]],CATCHUP = TRUE)
  s_30y_harvardNicaragua[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_30y_harvardNicaragua[[i]],CATCHUP = TRUE)
  s_life80take_harvardNicaragua[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life80take_harvardNicaragua[[i]],CATCHUP = TRUE)
  
  s_life_harvardElSalvador[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life_harvardElSalvador[[i]],CATCHUP = TRUE)
  s_20y_harvardElSalvador[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_20y_harvardElSalvador[[i]],CATCHUP = TRUE)
  s_30y_harvardElSalvador[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_30y_harvardElSalvador[[i]],CATCHUP = TRUE)
  s_life80take_harvardElSalvador[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life80take_harvardElSalvador[[i]],CATCHUP = TRUE)
  
  s_life_harvardUS[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life_harvardUS[[i]],CATCHUP = TRUE)
  s_20y_harvardUS[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_20y_harvardUS[[i]],CATCHUP = TRUE)
  s_30y_harvardUS[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_30y_harvardUS[[i]],CATCHUP = TRUE)
  s_life80take_harvardUS[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life80take_harvardUS[[i]],CATCHUP = TRUE)
}

# combine the indirect effects and multipliers for prevac, catch up, postvac cohorts
for(i in 1:length(s_life_harvardIndia))
{
  s_life_harvardIndia[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life_harvardIndia[[i]],CATCHUP = TRUE)
  s_20y_harvardIndia[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_20y_harvardIndia[[i]],CATCHUP = TRUE)
  s_30y_harvardIndia[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_30y_harvardIndia[[i]],CATCHUP = TRUE)
  s_life80take_harvardIndia[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life80take_harvardIndia[[i]],CATCHUP = TRUE)
  
  s_life_harvardUganda[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life_harvardUganda[[i]],CATCHUP = TRUE)
  s_20y_harvardUganda[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_20y_harvardUganda[[i]],CATCHUP = TRUE)
  s_30y_harvardUganda[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_30y_harvardUganda[[i]],CATCHUP = TRUE)
  s_life80take_harvardUganda[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life80take_harvardUganda[[i]],CATCHUP = TRUE)
  
  s_life_harvardNicaragua[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life_harvardNicaragua[[i]],CATCHUP = TRUE)
  s_20y_harvardNicaragua[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_20y_harvardNicaragua[[i]],CATCHUP = TRUE)
  s_30y_harvardNicaragua[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_30y_harvardNicaragua[[i]],CATCHUP = TRUE)
  s_life80take_harvardNicaragua[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life80take_harvardNicaragua[[i]],CATCHUP = TRUE)
  
  s_life_harvardElSalvador[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life_harvardElSalvador[[i]],CATCHUP = TRUE)
  s_20y_harvardElSalvador[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_20y_harvardElSalvador[[i]],CATCHUP = TRUE)
  s_30y_harvardElSalvador[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_30y_harvardElSalvador[[i]],CATCHUP = TRUE)
  s_life80take_harvardElSalvador[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life80take_harvardElSalvador[[i]],CATCHUP = TRUE)
  
  s_life_harvardUS[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life_harvardUS[[i]],CATCHUP = TRUE)
  s_20y_harvardUS[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_20y_harvardUS[[i]],CATCHUP = TRUE)
  s_30y_harvardUS[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_30y_harvardUS[[i]],CATCHUP = TRUE)
  s_life80take_harvardUS[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life80take_harvardUS[[i]],CATCHUP = TRUE)
  
  
  s_life_harvardIndia[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life_harvardIndia[[i]],CATCHUP = TRUE)
  s_20y_harvardIndia[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_20y_harvardIndia[[i]],CATCHUP = TRUE)
  s_30y_harvardIndia[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_30y_harvardIndia[[i]],CATCHUP = TRUE)
  s_life80take_harvardIndia[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life80take_harvardIndia[[i]],CATCHUP = TRUE)
  
  s_life_harvardUganda[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life_harvardUganda[[i]],CATCHUP = TRUE)
  s_20y_harvardUganda[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_20y_harvardUganda[[i]],CATCHUP = TRUE)
  s_30y_harvardUganda[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_30y_harvardUganda[[i]],CATCHUP = TRUE)
  s_life80take_harvardUganda[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life80take_harvardUganda[[i]],CATCHUP = TRUE)
  
  s_life_harvardNicaragua[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life_harvardNicaragua[[i]],CATCHUP = TRUE)
  s_20y_harvardNicaragua[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_20y_harvardNicaragua[[i]],CATCHUP = TRUE)
  s_30y_harvardNicaragua[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_30y_harvardNicaragua[[i]],CATCHUP = TRUE)
  s_life80take_harvardNicaragua[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life80take_harvardNicaragua[[i]],CATCHUP = TRUE)
  
  s_life_harvardElSalvador[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life_harvardElSalvador[[i]],CATCHUP = TRUE)
  s_20y_harvardElSalvador[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_20y_harvardElSalvador[[i]],CATCHUP = TRUE)
  s_30y_harvardElSalvador[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_30y_harvardElSalvador[[i]],CATCHUP = TRUE)
  s_life80take_harvardElSalvador[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life80take_harvardElSalvador[[i]],CATCHUP = TRUE)
  
  s_life_harvardUS[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life_harvardUS[[i]],CATCHUP = TRUE)
  s_20y_harvardUS[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_20y_harvardUS[[i]],CATCHUP = TRUE)
  s_30y_harvardUS[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_30y_harvardUS[[i]],CATCHUP = TRUE)
  s_life80take_harvardUS[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life80take_harvardUS[[i]],CATCHUP = TRUE)
}

rm(i)
