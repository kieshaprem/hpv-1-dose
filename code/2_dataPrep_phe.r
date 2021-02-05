source('code/functions_supporting.r')
load('data/phe/phe.rdata')

s_life_phe = prepData_PHE(STRATEGY = 'strategy_life')
s_20y_phe = prepData_PHE(STRATEGY = 'strategy_20y')
s_30y_phe = prepData_PHE(STRATEGY = 'strategy_30y')
s_life80take_phe = prepData_PHE(STRATEGY = 'strategy_life80take')
s_catchup_phe = prepData_PHE(STRATEGY = 'strategy_catchup')


# s_life_phe = s_life_phe[1:50]
# s_20y_phe = s_20y_phe[1:50]
# s_30y_phe = s_30y_phe[1:50]
# s_life80take_phe = s_life80take_phe[1:50]
# s_catchup_phe = s_catchup_phe[1:50]
# 


# get the direct reduction 
for(i in 1:length(s_life_phe))
{
  s_life_phe[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "GBR"],CATCHUP = FALSE,age_vac = 12)
  s_20y_phe[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "GBR"],CATCHUP = FALSE,age_vac = 12)
  s_30y_phe[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "GBR"],CATCHUP = FALSE,age_vac = 12)
  s_life80take_phe[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "GBR"],CATCHUP = FALSE,age_vac = 12)
  s_catchup_phe[[i]]$direct_reduction = createDirectReductionMatrix(p_vaccinetypecancer = hpv_distribution$hpv_9v[hpv_distribution$iso3 %in% "GBR"],age_vac = 12)
}


# get the indirect reduction (only lifelong protection) 
for(i in 1:length(s_life_phe))
{
  s_life_phe[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_phe[[i]])
  s_20y_phe[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_phe[[i]])
  s_30y_phe[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_phe[[i]])
  s_life80take_phe[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_phe[[i]])
  s_catchup_phe[[i]]$indirect_reduction = computeIndirectReduction(DATALIST = s_life_phe[[i]])
}

# get the the scenario multiplier (only for 20y/30y/80take/catchup)
for(i in 1:length(s_life_phe))
{
  s_life_phe[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life_phe[[i]],LIFETIME = s_life_phe[[i]])
  s_20y_phe[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_20y_phe[[i]],LIFETIME = s_life_phe[[i]])
  s_30y_phe[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_30y_phe[[i]],LIFETIME = s_life_phe[[i]])
  s_life80take_phe[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_life80take_phe[[i]],LIFETIME = s_life_phe[[i]])
  s_catchup_phe[[i]]$scenario_multiplier = calculateScenarioMultiplier(DATALIST = s_catchup_phe[[i]],LIFETIME = s_life_phe[[i]])
}


# get the the indirect cohort reduction (only lifelong protection)
for(i in 1:length(s_life_phe))
{
  s_life_phe[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life_phe[[i]])
  s_20y_phe[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_20y_phe[[i]])
  s_30y_phe[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_30y_phe[[i]])
  s_life80take_phe[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_life80take_phe[[i]])
  s_catchup_phe[[i]]$indirect_cohort_reduction = calculateIndirectCohortReduction(DATALIST = s_catchup_phe[[i]])
}

# get the the multiplier (only for 20y/30y/80take/catchup)
for(i in 1:length(s_life_phe))
{
  s_life_phe[[i]]$multiplier = calculateMultiplier(DATALIST = s_life_phe[[i]])
  s_20y_phe[[i]]$multiplier = calculateMultiplier(DATALIST = s_20y_phe[[i]])
  s_30y_phe[[i]]$multiplier = calculateMultiplier(DATALIST = s_30y_phe[[i]])
  s_life80take_phe[[i]]$multiplier = calculateMultiplier(DATALIST = s_life80take_phe[[i]])
  s_catchup_phe[[i]]$multiplier = calculateMultiplier(DATALIST = s_catchup_phe[[i]])
}


# get the the indirect cohort reduction for MAC (only catchup)
for(i in 1:length(s_life_phe))
{
  s_life_phe[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life_phe[[i]],CATCHUP = FALSE)
  s_20y_phe[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_20y_phe[[i]],CATCHUP = FALSE)
  s_30y_phe[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_30y_phe[[i]],CATCHUP = FALSE)
  s_life80take_phe[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_life80take_phe[[i]],CATCHUP = FALSE)
  s_catchup_phe[[i]]$indirect_cohort_reduction_mac = calculateIndirectCohortReductionMAC(DATALIST = s_catchup_phe[[i]],CATCHUP = TRUE)
}

# get the the multiplier for MAC (only for catchup)
for(i in 1:length(s_life_phe))
{
  s_life_phe[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life_phe[[i]],CATCHUP = FALSE)
  s_20y_phe[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_20y_phe[[i]],CATCHUP = FALSE)
  s_30y_phe[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_30y_phe[[i]],CATCHUP = FALSE)
  s_life80take_phe[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_life80take_phe[[i]],CATCHUP = FALSE)
  s_catchup_phe[[i]]$multiplier_mac = calculateMultiplierMAC(DATALIST = s_catchup_phe[[i]],CATCHUP = TRUE)
}


# get the the indirect cohort reduction for prevac cohort (only for lifelong)
for(i in 1:length(s_life_phe))
{
  s_life_phe[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life_phe[[i]])
  s_20y_phe[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_20y_phe[[i]])
  s_30y_phe[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_30y_phe[[i]])
  s_life80take_phe[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_life80take_phe[[i]])
  s_catchup_phe[[i]]$indirect_cohort_reduction_prevac = calculateIndirectCohortReductionPrevac(DATALIST = s_catchup_phe[[i]])
}

# get the the multiplier for prevac cohort (only for 20y/30y/80take/catchup)
for(i in 1:length(s_life_phe))
{
  s_life_phe[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life_phe[[i]])
  s_20y_phe[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_20y_phe[[i]])
  s_30y_phe[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_30y_phe[[i]])
  s_life80take_phe[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_life80take_phe[[i]])
  s_catchup_phe[[i]]$multiplier_prevac = calculateMultiplierPrevac(DATALIST = s_catchup_phe[[i]])
}


# combine the indirect effects and multipliers for prevac, catch up, postvac cohorts
for(i in 1:length(s_life_phe))
{
  s_life_phe[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life_phe[[i]],CATCHUP = FALSE)
  s_20y_phe[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_20y_phe[[i]],CATCHUP = FALSE)
  s_30y_phe[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_30y_phe[[i]],CATCHUP = FALSE)
  s_life80take_phe[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_life80take_phe[[i]],CATCHUP = FALSE)
  s_catchup_phe[[i]]$combined_indirect_effects = combineIndirectEffects(DATALIST = s_catchup_phe[[i]],CATCHUP = TRUE)
  
  s_life_phe[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life_phe[[i]],CATCHUP = FALSE)
  s_20y_phe[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_20y_phe[[i]],CATCHUP = FALSE)
  s_30y_phe[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_30y_phe[[i]],CATCHUP = FALSE)
  s_life80take_phe[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_life80take_phe[[i]],CATCHUP = FALSE)
  s_catchup_phe[[i]]$combined_multiplier = combineMultiplier(DATALIST = s_catchup_phe[[i]],CATCHUP = TRUE)
}



summary_runs = list()
summary_runs$strategy_life = c(which.min(abs(unlist(lapply(phe$strategy_life, sum))-quantile(unlist(lapply(phe$strategy_life, sum)),probs = 0.5))),
                               which.min(abs(unlist(lapply(phe$strategy_life, sum))-quantile(unlist(lapply(phe$strategy_life, sum)),probs = 0.25))),
                               which.min(abs(unlist(lapply(phe$strategy_life, sum))-quantile(unlist(lapply(phe$strategy_life, sum)),probs = 0.75))),
                               which.min(abs(unlist(lapply(phe$strategy_life, sum))-quantile(unlist(lapply(phe$strategy_life, sum)),probs = 0.1))),
                               which.min(abs(unlist(lapply(phe$strategy_life, sum))-quantile(unlist(lapply(phe$strategy_life, sum)),probs = 0.9))))#,
                               # which.min(abs(unlist(lapply(phe$strategy_life, sum))-quantile(unlist(lapply(phe$strategy_life, sum)),probs = 0.025))),
                               # which.min(abs(unlist(lapply(phe$strategy_life, sum))-quantile(unlist(lapply(phe$strategy_life, sum)),probs = 0.975))))

summary_runs$strategy_20y = summary_runs$strategy_life
summary_runs$strategy_30y = summary_runs$strategy_life
summary_runs$strategy_life80take = summary_runs$strategy_life
summary_runs$strategy_catchup = summary_runs$strategy_life


# summary_runs$strategy_20y = c(which.min(abs(unlist(lapply(phe$strategy_20y, sum))-quantile(unlist(lapply(phe$strategy_20y, sum)),probs = 0.5))),
#                               which.min(abs(unlist(lapply(phe$strategy_20y, sum))-quantile(unlist(lapply(phe$strategy_20y, sum)),probs = 0.25))),
#                               which.min(abs(unlist(lapply(phe$strategy_20y, sum))-quantile(unlist(lapply(phe$strategy_20y, sum)),probs = 0.75))),
#                               which.min(abs(unlist(lapply(phe$strategy_20y, sum))-quantile(unlist(lapply(phe$strategy_20y, sum)),probs = 0.1))),
#                               which.min(abs(unlist(lapply(phe$strategy_20y, sum))-quantile(unlist(lapply(phe$strategy_20y, sum)),probs = 0.9))))#,
#                               # which.min(abs(unlist(lapply(phe$strategy_20y, sum))-quantile(unlist(lapply(phe$strategy_20y, sum)),probs = 0.025))),
                              # which.min(abs(unlist(lapply(phe$strategy_20y, sum))-quantile(unlist(lapply(phe$strategy_20y, sum)),probs = 0.975))))

# summary_runs$strategy_30y = c(which.min(abs(unlist(lapply(phe$strategy_30y, sum))-quantile(unlist(lapply(phe$strategy_30y, sum)),probs = 0.5))),
#                               which.min(abs(unlist(lapply(phe$strategy_30y, sum))-quantile(unlist(lapply(phe$strategy_30y, sum)),probs = 0.25))),
#                               which.min(abs(unlist(lapply(phe$strategy_30y, sum))-quantile(unlist(lapply(phe$strategy_30y, sum)),probs = 0.75))),
#                               which.min(abs(unlist(lapply(phe$strategy_30y, sum))-quantile(unlist(lapply(phe$strategy_30y, sum)),probs = 0.1))),
#                               which.min(abs(unlist(lapply(phe$strategy_30y, sum))-quantile(unlist(lapply(phe$strategy_30y, sum)),probs = 0.9))))#,
#                               # which.min(abs(unlist(lapply(phe$strategy_30y, sum))-quantile(unlist(lapply(phe$strategy_30y, sum)),probs = 0.025))),
#                               # which.min(abs(unlist(lapply(phe$strategy_30y, sum))-quantile(unlist(lapply(phe$strategy_30y, sum)),probs = 0.975))))
# 
# summary_runs$strategy_life80take = c(which.min(abs(unlist(lapply(phe$strategy_life80take, sum))-quantile(unlist(lapply(phe$strategy_life80take, sum)),probs = 0.5))),
#                                      which.min(abs(unlist(lapply(phe$strategy_life80take, sum))-quantile(unlist(lapply(phe$strategy_life80take, sum)),probs = 0.25))),
#                                      which.min(abs(unlist(lapply(phe$strategy_life80take, sum))-quantile(unlist(lapply(phe$strategy_life80take, sum)),probs = 0.75))),
#                                      which.min(abs(unlist(lapply(phe$strategy_life80take, sum))-quantile(unlist(lapply(phe$strategy_life80take, sum)),probs = 0.1))),
#                                      which.min(abs(unlist(lapply(phe$strategy_life80take, sum))-quantile(unlist(lapply(phe$strategy_life80take, sum)),probs = 0.9))))#,
#                                      # which.min(abs(unlist(lapply(phe$strategy_life80take, sum))-quantile(unlist(lapply(phe$strategy_life80take, sum)),probs = 0.025))),
#                                      # which.min(abs(unlist(lapply(phe$strategy_life80take, sum))-quantile(unlist(lapply(phe$strategy_life80take, sum)),probs = 0.975))))
# 
# summary_runs$strategy_catchup = c(which.min(abs(unlist(lapply(phe$strategy_catchup, sum))-quantile(unlist(lapply(phe$strategy_catchup, sum)),probs = 0.5))),
#                                   which.min(abs(unlist(lapply(phe$strategy_catchup, sum))-quantile(unlist(lapply(phe$strategy_catchup, sum)),probs = 0.25))),
#                                   which.min(abs(unlist(lapply(phe$strategy_catchup, sum))-quantile(unlist(lapply(phe$strategy_catchup, sum)),probs = 0.75))),
#                                   which.min(abs(unlist(lapply(phe$strategy_catchup, sum))-quantile(unlist(lapply(phe$strategy_catchup, sum)),probs = 0.1))),
#                                   which.min(abs(unlist(lapply(phe$strategy_catchup, sum))-quantile(unlist(lapply(phe$strategy_catchup, sum)),probs = 0.9))))#,
#                                   # which.min(abs(unlist(lapply(phe$strategy_catchup, sum))-quantile(unlist(lapply(phe$strategy_catchup, sum)),probs = 0.025))),
#                                   # which.min(abs(unlist(lapply(phe$strategy_catchup, sum))-quantile(unlist(lapply(phe$strategy_catchup, sum)),probs = 0.975))))


rm(i,phe)
