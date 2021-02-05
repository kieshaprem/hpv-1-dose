source('code/functions_supporting.r')
source('code/1_loadData_hpvadvise.r')


s_life_hpvadviseIndia = prepData_HPVADVISE(DATALIST = hpvadviseIndia,STRATEGY = 'strategy_life')
s_20y_hpvadviseIndia = prepData_HPVADVISE(DATALIST = hpvadviseIndia,STRATEGY = 'strategy_20y')
s_30y_hpvadviseIndia = prepData_HPVADVISE(DATALIST = hpvadviseIndia,STRATEGY = 'strategy_30y')
s_life80take_hpvadviseIndia = prepData_HPVADVISE(DATALIST = hpvadviseIndia,STRATEGY = 'strategy_life80take')
s_novac_hpvadviseIndia = prepData_HPVADVISE(DATALIST = hpvadviseIndia,STRATEGY = 'strategy_novac')

s_life_hpvadviseNigeria = prepData_HPVADVISE(DATALIST = hpvadviseNigeria,STRATEGY = 'strategy_life')
s_20y_hpvadviseNigeria = prepData_HPVADVISE(DATALIST = hpvadviseNigeria,STRATEGY = 'strategy_20y')
s_30y_hpvadviseNigeria = prepData_HPVADVISE(DATALIST = hpvadviseNigeria,STRATEGY = 'strategy_30y')
s_life80take_hpvadviseNigeria = prepData_HPVADVISE(DATALIST = hpvadviseNigeria,STRATEGY = 'strategy_life80take')
s_novac_hpvadviseNigeria = prepData_HPVADVISE(DATALIST = hpvadviseNigeria,STRATEGY = 'strategy_novac')

s_life_hpvadviseUganda = prepData_HPVADVISE(DATALIST = hpvadviseUganda,STRATEGY = 'strategy_life')
s_20y_hpvadviseUganda = prepData_HPVADVISE(DATALIST = hpvadviseUganda,STRATEGY = 'strategy_20y')
s_30y_hpvadviseUganda = prepData_HPVADVISE(DATALIST = hpvadviseUganda,STRATEGY = 'strategy_30y')
s_life80take_hpvadviseUganda = prepData_HPVADVISE(DATALIST = hpvadviseUganda,STRATEGY = 'strategy_life80take')
s_novac_hpvadviseUganda = prepData_HPVADVISE(DATALIST = hpvadviseUganda,STRATEGY = 'strategy_novac')

s_life_hpvadviseVietnam = prepData_HPVADVISE(DATALIST = hpvadviseVietnam,STRATEGY = 'strategy_life')
s_20y_hpvadviseVietnam = prepData_HPVADVISE(DATALIST = hpvadviseVietnam,STRATEGY = 'strategy_20y')
s_30y_hpvadviseVietnam = prepData_HPVADVISE(DATALIST = hpvadviseVietnam,STRATEGY = 'strategy_30y')
s_life80take_hpvadviseVietnam = prepData_HPVADVISE(DATALIST = hpvadviseVietnam,STRATEGY = 'strategy_life80take')
s_novac_hpvadviseVietnam = prepData_HPVADVISE(DATALIST = hpvadviseVietnam,STRATEGY = 'strategy_novac')

s_life_hpvadviseCanada = prepData_HPVADVISE(DATALIST = hpvadviseCanada,STRATEGY = 'strategy_life')
s_20y_hpvadviseCanada = prepData_HPVADVISE(DATALIST = hpvadviseCanada,STRATEGY = 'strategy_20y')
s_30y_hpvadviseCanada = prepData_HPVADVISE(DATALIST = hpvadviseCanada,STRATEGY = 'strategy_30y')
s_life80take_hpvadviseCanada = prepData_HPVADVISE(DATALIST = hpvadviseCanada,STRATEGY = 'strategy_life80take')
s_novac_hpvadviseCanada = prepData_HPVADVISE(DATALIST = hpvadviseCanada,STRATEGY = 'strategy_novac')



# get the direct reduction 
for(i in 1:length(s_life_hpvadviseIndia))
{
  s_life_hpvadviseIndia[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "IND"])
  s_20y_hpvadviseIndia[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "IND"])
  s_30y_hpvadviseIndia[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "IND"])
  s_life80take_hpvadviseIndia[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "IND"])
  # s_novac_hpvadviseIndia[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "IND"])

  s_life_hpvadviseNigeria[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "NGA"])
  s_20y_hpvadviseNigeria[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "NGA"])
  s_30y_hpvadviseNigeria[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "NGA"])
  s_life80take_hpvadviseNigeria[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "NGA"])
  # s_novac_hpvadviseNigeria[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "NGA"])
  
  s_life_hpvadviseUganda[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "UGA"])
  s_20y_hpvadviseUganda[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "UGA"])
  s_30y_hpvadviseUganda[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "UGA"])
  s_life80take_hpvadviseUganda[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "UGA"])
  # s_novac_hpvadviseUganda[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "UGA"])
  
  s_life_hpvadviseVietnam[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "VNM"])
  s_20y_hpvadviseVietnam[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "VNM"])
  s_30y_hpvadviseVietnam[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "VNM"])
  s_life80take_hpvadviseVietnam[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "VNM"])
  # s_novac_hpvadviseVietnam[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "VNM"])
  
  s_life_hpvadviseCanada[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "CAN"])
  s_20y_hpvadviseCanada[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "CAN"])
  s_30y_hpvadviseCanada[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "CAN"])
  s_life80take_hpvadviseCanada[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "CAN"])
  # s_novac_hpvadviseCanada[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "CAN"])
}


# get the indirect reduction (only lifelong protection) 
for(i in 1:length(s_life_hpvadviseIndia))
{
  s_life_hpvadviseIndia[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseIndia[[i]])
  s_20y_hpvadviseIndia[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseIndia[[i]])
  s_30y_hpvadviseIndia[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseIndia[[i]])
  s_life80take_hpvadviseIndia[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseIndia[[i]])
  # s_novac_hpvadviseIndia[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseIndia[[i]])
  
  s_life_hpvadviseNigeria[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseNigeria[[i]])
  s_20y_hpvadviseNigeria[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseNigeria[[i]])
  s_30y_hpvadviseNigeria[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseNigeria[[i]])
  s_life80take_hpvadviseNigeria[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseNigeria[[i]])
  # s_novac_hpvadviseNigeria[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseNigeria[[i]])
  
  s_life_hpvadviseUganda[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseUganda[[i]])
  s_20y_hpvadviseUganda[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseUganda[[i]])
  s_30y_hpvadviseUganda[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseUganda[[i]])
  s_life80take_hpvadviseUganda[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseUganda[[i]])
  # s_novac_hpvadviseUganda[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseUganda[[i]])
  
  s_life_hpvadviseVietnam[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseVietnam[[i]])
  s_20y_hpvadviseVietnam[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseVietnam[[i]])
  s_30y_hpvadviseVietnam[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseVietnam[[i]])
  s_life80take_hpvadviseVietnam[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseVietnam[[i]])
  # s_novac_hpvadviseVietnam[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseVietnam[[i]])
  
  s_life_hpvadviseCanada[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseCanada[[i]])
  s_20y_hpvadviseCanada[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseCanada[[i]])
  s_30y_hpvadviseCanada[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseCanada[[i]])
  s_life80take_hpvadviseCanada[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseCanada[[i]])
  # s_novac_hpvadviseCanada[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_hpvadviseCanada[[i]])
}

# get the the scenario multiplier (only for 20y/30y/80take)
for(i in 1:length(s_life_hpvadviseIndia))
{
  s_life_hpvadviseIndia[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life_hpvadviseIndia[[i]],LIFETIME = s_life_hpvadviseIndia[[i]])
  s_20y_hpvadviseIndia[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_20y_hpvadviseIndia[[i]],LIFETIME = s_life_hpvadviseIndia[[i]])
  s_30y_hpvadviseIndia[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_30y_hpvadviseIndia[[i]],LIFETIME = s_life_hpvadviseIndia[[i]])
  s_life80take_hpvadviseIndia[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life80take_hpvadviseIndia[[i]],LIFETIME = s_life_hpvadviseIndia[[i]])
  # s_novac_hpvadviseIndia[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_novac_hpvadviseIndia[[i]],LIFETIME = s_life_hpvadviseIndia[[i]])
  
  s_life_hpvadviseNigeria[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life_hpvadviseNigeria[[i]],LIFETIME = s_life_hpvadviseNigeria[[i]])
  s_20y_hpvadviseNigeria[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_20y_hpvadviseNigeria[[i]],LIFETIME = s_life_hpvadviseNigeria[[i]])
  s_30y_hpvadviseNigeria[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_30y_hpvadviseNigeria[[i]],LIFETIME = s_life_hpvadviseNigeria[[i]])
  s_life80take_hpvadviseNigeria[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life80take_hpvadviseNigeria[[i]],LIFETIME = s_life_hpvadviseNigeria[[i]])
  # s_novac_hpvadviseNigeria[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_novac_hpvadviseNigeria[[i]],LIFETIME = s_life_hpvadviseNigeria[[i]])
  
  s_life_hpvadviseUganda[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life_hpvadviseUganda[[i]],LIFETIME = s_life_hpvadviseUganda[[i]])
  s_20y_hpvadviseUganda[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_20y_hpvadviseUganda[[i]],LIFETIME = s_life_hpvadviseUganda[[i]])
  s_30y_hpvadviseUganda[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_30y_hpvadviseUganda[[i]],LIFETIME = s_life_hpvadviseUganda[[i]])
  s_life80take_hpvadviseUganda[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life80take_hpvadviseUganda[[i]],LIFETIME = s_life_hpvadviseUganda[[i]])
  # s_novac_hpvadviseUganda[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_novac_hpvadviseUganda[[i]],LIFETIME = s_life_hpvadviseUganda[[i]])
  
  s_life_hpvadviseVietnam[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life_hpvadviseVietnam[[i]],LIFETIME = s_life_hpvadviseVietnam[[i]])
  s_20y_hpvadviseVietnam[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_20y_hpvadviseVietnam[[i]],LIFETIME = s_life_hpvadviseVietnam[[i]])
  s_30y_hpvadviseVietnam[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_30y_hpvadviseVietnam[[i]],LIFETIME = s_life_hpvadviseVietnam[[i]])
  s_life80take_hpvadviseVietnam[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life80take_hpvadviseVietnam[[i]],LIFETIME = s_life_hpvadviseVietnam[[i]])
  # s_novac_hpvadviseVietnam[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_novac_hpvadviseVietnam[[i]],LIFETIME = s_life_hpvadviseVietnam[[i]])
  
  s_life_hpvadviseCanada[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life_hpvadviseCanada[[i]],LIFETIME = s_life_hpvadviseCanada[[i]])
  s_20y_hpvadviseCanada[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_20y_hpvadviseCanada[[i]],LIFETIME = s_life_hpvadviseCanada[[i]])
  s_30y_hpvadviseCanada[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_30y_hpvadviseCanada[[i]],LIFETIME = s_life_hpvadviseCanada[[i]])
  s_life80take_hpvadviseCanada[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life80take_hpvadviseCanada[[i]],LIFETIME = s_life_hpvadviseCanada[[i]])
  # s_novac_hpvadviseCanada[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_novac_hpvadviseCanada[[i]],LIFETIME = s_life_hpvadviseCanada[[i]])
}


# get the the indirect cohort reduction (only lifelong protection)
for(i in 1:length(s_life_hpvadviseIndia))
{
  s_life_hpvadviseIndia[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life_hpvadviseIndia[[i]])
  s_20y_hpvadviseIndia[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_20y_hpvadviseIndia[[i]])
  s_30y_hpvadviseIndia[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_30y_hpvadviseIndia[[i]])
  s_life80take_hpvadviseIndia[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life80take_hpvadviseIndia[[i]])
  
  s_life_hpvadviseNigeria[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life_hpvadviseNigeria[[i]])
  s_20y_hpvadviseNigeria[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_20y_hpvadviseNigeria[[i]])
  s_30y_hpvadviseNigeria[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_30y_hpvadviseNigeria[[i]])
  s_life80take_hpvadviseNigeria[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life80take_hpvadviseNigeria[[i]])
  
  s_life_hpvadviseUganda[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life_hpvadviseUganda[[i]])
  s_20y_hpvadviseUganda[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_20y_hpvadviseUganda[[i]])
  s_30y_hpvadviseUganda[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_30y_hpvadviseUganda[[i]])
  s_life80take_hpvadviseUganda[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life80take_hpvadviseUganda[[i]])
  
  s_life_hpvadviseVietnam[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life_hpvadviseVietnam[[i]])
  s_20y_hpvadviseVietnam[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_20y_hpvadviseVietnam[[i]])
  s_30y_hpvadviseVietnam[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_30y_hpvadviseVietnam[[i]])
  s_life80take_hpvadviseVietnam[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life80take_hpvadviseVietnam[[i]])
  
  s_life_hpvadviseCanada[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life_hpvadviseCanada[[i]])
  s_20y_hpvadviseCanada[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_20y_hpvadviseCanada[[i]])
  s_30y_hpvadviseCanada[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_30y_hpvadviseCanada[[i]])
  s_life80take_hpvadviseCanada[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life80take_hpvadviseCanada[[i]])
}

# get the the multiplier (only for 20y/30y/80take)
for(i in 1:length(s_life_hpvadviseIndia))
{
  s_life_hpvadviseIndia[[i]]$multiplier = calculateMultiplier(DATALIST = s_life_hpvadviseIndia[[i]])
  s_20y_hpvadviseIndia[[i]]$multiplier = calculateMultiplier(DATALIST = s_20y_hpvadviseIndia[[i]])
  s_30y_hpvadviseIndia[[i]]$multiplier = calculateMultiplier(DATALIST = s_30y_hpvadviseIndia[[i]])
  s_life80take_hpvadviseIndia[[i]]$multiplier = calculateMultiplier(DATALIST = s_life80take_hpvadviseIndia[[i]])
  
  s_life_hpvadviseNigeria[[i]]$multiplier = calculateMultiplier(DATALIST = s_life_hpvadviseNigeria[[i]])
  s_20y_hpvadviseNigeria[[i]]$multiplier = calculateMultiplier(DATALIST = s_20y_hpvadviseNigeria[[i]])
  s_30y_hpvadviseNigeria[[i]]$multiplier = calculateMultiplier(DATALIST = s_30y_hpvadviseNigeria[[i]])
  s_life80take_hpvadviseNigeria[[i]]$multiplier = calculateMultiplier(DATALIST = s_life80take_hpvadviseNigeria[[i]])
  
  s_life_hpvadviseUganda[[i]]$multiplier = calculateMultiplier(DATALIST = s_life_hpvadviseUganda[[i]])
  s_20y_hpvadviseUganda[[i]]$multiplier = calculateMultiplier(DATALIST = s_20y_hpvadviseUganda[[i]])
  s_30y_hpvadviseUganda[[i]]$multiplier = calculateMultiplier(DATALIST = s_30y_hpvadviseUganda[[i]])
  s_life80take_hpvadviseUganda[[i]]$multiplier = calculateMultiplier(DATALIST = s_life80take_hpvadviseUganda[[i]])
  
  s_life_hpvadviseVietnam[[i]]$multiplier = calculateMultiplier(DATALIST = s_life_hpvadviseVietnam[[i]])
  s_20y_hpvadviseVietnam[[i]]$multiplier = calculateMultiplier(DATALIST = s_20y_hpvadviseVietnam[[i]])
  s_30y_hpvadviseVietnam[[i]]$multiplier = calculateMultiplier(DATALIST = s_30y_hpvadviseVietnam[[i]])
  s_life80take_hpvadviseVietnam[[i]]$multiplier = calculateMultiplier(DATALIST = s_life80take_hpvadviseVietnam[[i]])
  
  s_life_hpvadviseCanada[[i]]$multiplier = calculateMultiplier(DATALIST = s_life_hpvadviseCanada[[i]])
  s_20y_hpvadviseCanada[[i]]$multiplier = calculateMultiplier(DATALIST = s_20y_hpvadviseCanada[[i]])
  s_30y_hpvadviseCanada[[i]]$multiplier = calculateMultiplier(DATALIST = s_30y_hpvadviseCanada[[i]])
  s_life80take_hpvadviseCanada[[i]]$multiplier = calculateMultiplier(DATALIST = s_life80take_hpvadviseCanada[[i]])

}


# get the the indirect cohort reduction for MAC (only catchup)
for(i in 1:length(s_life_hpvadviseIndia))
{
  s_life_hpvadviseIndia[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life_hpvadviseIndia[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseIndia[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_20y_hpvadviseIndia[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseIndia[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_30y_hpvadviseIndia[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseIndia[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life80take_hpvadviseIndia[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseNigeria[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseNigeria[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_20y_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseNigeria[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_30y_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseNigeria[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life80take_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseUganda[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life_hpvadviseUganda[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseUganda[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_20y_hpvadviseUganda[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseUganda[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_30y_hpvadviseUganda[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseUganda[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life80take_hpvadviseUganda[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseVietnam[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseVietnam[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_20y_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseVietnam[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_30y_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseVietnam[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life80take_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseCanada[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life_hpvadviseCanada[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseCanada[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_20y_hpvadviseCanada[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseCanada[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_30y_hpvadviseCanada[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseCanada[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life80take_hpvadviseCanada[[i]],CATCHUP = TRUE)

}

# get the the multiplier for MAC (only for catchup)
for(i in 1:length(s_life_hpvadviseIndia))
{
  s_life_hpvadviseIndia[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life_hpvadviseIndia[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseIndia[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_20y_hpvadviseIndia[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseIndia[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_30y_hpvadviseIndia[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseIndia[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life80take_hpvadviseIndia[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseNigeria[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseNigeria[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_20y_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseNigeria[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_30y_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseNigeria[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life80take_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseUganda[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life_hpvadviseUganda[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseUganda[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_20y_hpvadviseUganda[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseUganda[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_30y_hpvadviseUganda[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseUganda[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life80take_hpvadviseUganda[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseVietnam[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseVietnam[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_20y_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseVietnam[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_30y_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseVietnam[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life80take_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseCanada[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life_hpvadviseCanada[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseCanada[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_20y_hpvadviseCanada[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseCanada[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_30y_hpvadviseCanada[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseCanada[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life80take_hpvadviseCanada[[i]],CATCHUP = TRUE)
}


# get the the indirect cohort reduction for prevac cohort (only for lifelong)
for(i in 1:length(s_life_hpvadviseIndia))
{
  s_life_hpvadviseIndia[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life_hpvadviseIndia[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseIndia[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_20y_hpvadviseIndia[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseIndia[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_30y_hpvadviseIndia[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseIndia[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life80take_hpvadviseIndia[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseNigeria[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseNigeria[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_20y_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseNigeria[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_30y_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseNigeria[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life80take_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseUganda[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life_hpvadviseUganda[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseUganda[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_20y_hpvadviseUganda[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseUganda[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_30y_hpvadviseUganda[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseUganda[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life80take_hpvadviseUganda[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseVietnam[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseVietnam[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_20y_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseVietnam[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_30y_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseVietnam[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life80take_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseCanada[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life_hpvadviseCanada[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseCanada[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_20y_hpvadviseCanada[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseCanada[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_30y_hpvadviseCanada[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseCanada[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life80take_hpvadviseCanada[[i]],CATCHUP = TRUE)
}

# get the the multiplier for prevac cohort (only for 20y/30y/80take)
for(i in 1:length(s_life_hpvadviseIndia))
{
  s_life_hpvadviseIndia[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life_hpvadviseIndia[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseIndia[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_20y_hpvadviseIndia[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseIndia[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_30y_hpvadviseIndia[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseIndia[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life80take_hpvadviseIndia[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseNigeria[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseNigeria[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_20y_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseNigeria[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_30y_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseNigeria[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life80take_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseUganda[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life_hpvadviseUganda[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseUganda[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_20y_hpvadviseUganda[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseUganda[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_30y_hpvadviseUganda[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseUganda[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life80take_hpvadviseUganda[[i]],CATCHUP = TRUE)

  s_life_hpvadviseVietnam[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseVietnam[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_20y_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseVietnam[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_30y_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseVietnam[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life80take_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseCanada[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life_hpvadviseCanada[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseCanada[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_20y_hpvadviseCanada[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseCanada[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_30y_hpvadviseCanada[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseCanada[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life80take_hpvadviseCanada[[i]],CATCHUP = TRUE)
}


# combine the indirect effects and multipliers for prevac, catch up, postvac cohorts
for(i in 1:length(s_life_hpvadviseIndia))
{
  s_life_hpvadviseIndia[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life_hpvadviseIndia[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseIndia[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_20y_hpvadviseIndia[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseIndia[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_30y_hpvadviseIndia[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseIndia[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life80take_hpvadviseIndia[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseNigeria[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseNigeria[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_20y_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseNigeria[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_30y_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseNigeria[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life80take_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseUganda[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life_hpvadviseUganda[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseUganda[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_20y_hpvadviseUganda[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseUganda[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_30y_hpvadviseUganda[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseUganda[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life80take_hpvadviseUganda[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseVietnam[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseVietnam[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_20y_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseVietnam[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_30y_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseVietnam[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life80take_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseCanada[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life_hpvadviseCanada[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseCanada[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_20y_hpvadviseCanada[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseCanada[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_30y_hpvadviseCanada[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseCanada[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life80take_hpvadviseCanada[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseIndia[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life_hpvadviseIndia[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseIndia[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_20y_hpvadviseIndia[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseIndia[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_30y_hpvadviseIndia[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseIndia[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life80take_hpvadviseIndia[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseNigeria[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseNigeria[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_20y_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseNigeria[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_30y_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseNigeria[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life80take_hpvadviseNigeria[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseUganda[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life_hpvadviseUganda[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseUganda[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_20y_hpvadviseUganda[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseUganda[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_30y_hpvadviseUganda[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseUganda[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life80take_hpvadviseUganda[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseVietnam[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseVietnam[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_20y_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseVietnam[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_30y_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseVietnam[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life80take_hpvadviseVietnam[[i]],CATCHUP = TRUE)
  
  s_life_hpvadviseCanada[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life_hpvadviseCanada[[i]],CATCHUP = TRUE)
  s_20y_hpvadviseCanada[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_20y_hpvadviseCanada[[i]],CATCHUP = TRUE)
  s_30y_hpvadviseCanada[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_30y_hpvadviseCanada[[i]],CATCHUP = TRUE)
  s_life80take_hpvadviseCanada[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life80take_hpvadviseCanada[[i]],CATCHUP = TRUE)
}


rm(i,hpvadviseIndia,hpvadviseNigeria,hpvadviseUganda,hpvadviseVietnam,hpvadviseCanada)
rm(s_novac_hpvadviseIndia,s_novac_hpvadviseNigeria,s_novac_hpvadviseUganda,s_novac_hpvadviseVietnam,s_novac_hpvadviseCanada)


