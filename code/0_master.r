source('code/functions_supporting.r')


######################## STEP 1: Load data
# Read in PHE data (data only on desktop and onedrive)
# source('code/1_loadData_phe.r')

# Read in Harvard data
# source('code/1_loadData_harvard.r')

# Read in HPV-ADVISE data
# source('code/1_loadData_hpvadvise.r')


######################## STEP 2: Data preparation: calculate direct effects, indirect effects, and multiplier for the different scenarios

## PHE model
source('code/2_dataPrep_phe.r')

## Harvard model
source('code/2_dataPrep_harvard.r')

## HPV-ADVISE model
source('code/2_dataPrep_hpvadvise.r')

######################## STEP 3: Estimate vaccine impact

# Include the missing vaccination cohorts, corresponding to vaccination programmes in the years 2100--2120 
source('code/3_includeBirthcohorts2091to2110.r')

# Estimate the base case vaccine impact 
source('code/3_extractBaseCaseVaccineImpact.r')


## PHE model
source('code/3_calculateIndirectEffects_phe.r')

## HPV-ADVISE model
source('code/3_calculateIndirectEffects_hpvadvise.r')

## Harvard model
source('code/3_calculateIndirectEffects_harvard.r')

######################## STEP 4: Summarise vaccine impact (56.70501 mins)

## PHE model
source('code/4_summariseVaccineImpact_phe.r')

## HPV-ADVISE model
source('code/4_summariseVaccineImpact_hpvadviseIndia.r')
source('code/4_summariseVaccineImpact_hpvadviseNigeria.r')
source('code/4_summariseVaccineImpact_hpvadviseUganda.r')
source('code/4_summariseVaccineImpact_hpvadviseVietnam.r')

## HPV-ADVISE model
source('code/4_summariseVaccineImpact_harvardIndia.r')
source('code/4_summariseVaccineImpact_harvardUganda.r')
source('code/4_summariseVaccineImpact_harvardElSalvador.r')
source('code/4_summariseVaccineImpact_harvardNicaragua.r')
source('code/4_summariseVaccineImpact_harvardUS.r')

######################## STEP 5: Compute median vaccine impact
source('code/5_computeMedianVaccineImpact.r')

######################## STEP 6: Compute median vaccine impact by Income groups
source('code/6_computeRegionalMeanVaccineImpact.r')
source('code/6_computeRegionalCI10VaccineImpact.r')
source('code/6_computeRegionalCI90VaccineImpact.r')

######################## STEP 7: Plot results
source('code/7_plotCancersAverted_figure2.r')
source('code/7_plotCancersAverted_figure3.r')
source('code/7_plotCancersAverted_cumulative.r')

source('code/7_plotDeathsAverted_figure2.r')
source('code/7_plotDeathsAverted_figure3.r')
######################## STEP 8: Threshold costs

# source('code/8_getCostsByCountry.r')
source('code/functions_supporting.r')
## PHE model
source('code/8_calculateThresholdCosts_phe.r')

## HPV-ADVISE model
source('code/8_calculateThresholdCosts_hpvadviseIndia.r')
source('code/8_calculateThresholdCosts_hpvadviseNigeria.r')
source('code/8_calculateThresholdCosts_hpvadviseUganda.r')
source('code/8_calculateThresholdCosts_hpvadviseVietnam.r')
source('code/8_calculateThresholdCosts_hpvadviseCanada.r')

## HPV-ADVISE model
source('code/8_calculateThresholdCosts_harvardElSalvador.r')
source('code/8_calculateThresholdCosts_harvardIndia.r')
source('code/8_calculateThresholdCosts_harvardNicaragua.r')
source('code/8_calculateThresholdCosts_harvardUganda.r')
source('code/8_calculateThresholdCosts_harvardUS.r')

## PHE model
source('code/8_calculateThresholdCostsCET_phe.r')

## HPV-ADVISE model
source('code/8_calculateThresholdCostsCET_hpvadviseIndia.r')
source('code/8_calculateThresholdCostsCET_hpvadviseNigeria.r')
source('code/8_calculateThresholdCostsCET_hpvadviseUganda.r')
source('code/8_calculateThresholdCostsCET_hpvadviseVietnam.r')
source('code/8_calculateThresholdCostsCET_hpvadviseCanada.r')

## HPV-ADVISE model
source('code/8_calculateThresholdCostsCET_harvardElSalvador.r')
source('code/8_calculateThresholdCostsCET_harvardIndia.r')
source('code/8_calculateThresholdCostsCET_harvardNicaragua.r')
source('code/8_calculateThresholdCostsCET_harvardUganda.r')
source('code/8_calculateThresholdCostsCET_harvardUS.r')

source('code/8_summariseThresholdCosts.r')
source('code/8_summariseThresholdCosts_no_dish.r')
source('code/8_summariseThresholdCosts_no_disc_no_dish.r')

source('code/8_summariseThresholdCostsCET.r')
source('code/8_summariseThresholdCostsCET_no_dish.r')
source('code/8_summariseThresholdCostsCET_no_disc_no_dish.r')



######################## STEP 9: NNV

source('code/functions_supporting.r')
source('code/9_getVaccinatedCohort.r')

## PHE model
source('code/9_computeNNV_phe.r')
source('code/9_computeNNV_phe_method2.r')

## HPV-ADVISE model
source('code/9_computeNNV_hpvadvise.r')
source('code/9_computeNNV_hpvadvise_method2.r')

## HPV-ADVISE model
source('code/9_computeNNV_harvard.r')
source('code/9_computeNNV_harvard_method2.r')

# Summarise NNVs
source('code/9_summariseNNV.r')
source('code/9_summariseNNV_dish.r')
source('code/9_summariseNNV_method2.r')
source('code/9_summariseNNV_dish_method2.r')

# PLot NNVs
source('code/9_plotNNV.r')
source('code/9_plotNNV_method2.r')
