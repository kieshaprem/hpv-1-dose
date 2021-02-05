options(scipen=999)
# load('output/prime/vaccine_impact.rdata')
load('output/prime/vaccine_impact1931to2120.rdata')
total_output = total_output_1931to2120
source('code/2_dataPrep_hpvadvise.r')

head(total_output)
total_output$birth_year = total_output$year_vac - 10
total_output = total_output[total_output$age<100,]
total_output= data.frame(total_output)

total_output = total_output[total_output$iso3c %in% names(which(table(total_output$iso3c)>18000)),] 

estimateVaccineImpact_indirecteffects = function(DATALIST,OUTPUT = total_output)
{
  disc = data.frame(year = 1921:2250, disc = discount(0.03,STARTYEAR = 1921,LASTYEAR = 2250))
  iso3c =  (unique(as.character(OUTPUT$iso3c)))
  early_cohort = 1931
  
  # Extract country-specific p1618 value (used to convert PRIME outputs back to All types).
  p1618 = data.frame(iso3c = iso3c, p1618 = as.data.frame(data.global[match(x = iso3c,data.global$iso3),25])*0.01)
  phpv9 = data.frame(iso3c = iso3c, phpv9 = as.data.frame(hpv_distribution[match(x = iso3c,hpv_distribution$iso3),10])*0.01)
  colnames(p1618) = c('iso3c','p1618')
  colnames(phpv9) = c('iso3c','phpv9')
  p1618$p1618[is.na(p1618$p1618)] = mean(p1618$p1618,na.rm=TRUE)
  
  # DATALIST = s_life_hpvadviseIndia[[1]]
  # sum((DATALIST$combined_indirect_effects<0))
  # sum(((DATALIST$combined_indirect_effects+abs(DATALIST$combined_indirect_effects))/2<0))
  
  
  indirect_effects = (DATALIST$combined_indirect_effects+abs(DATALIST$combined_indirect_effects))/2
  multiplier = (DATALIST$combined_multiplier+abs(DATALIST$combined_multiplier))/2
  
  
  cohort_list = list()
  
  for (birthcohort_year in min(OUTPUT$birth_year):max(OUTPUT$birth_year)) 
  { 
    # Extract impact for 1 cohort.
    cohort_impact <- OUTPUT[OUTPUT$birth_year == birthcohort_year,]
    cohort_impact$p1618 = p1618[match(cohort_impact$iso3c,p1618$iso3c),2]
    cohort_impact$phpv9 = phpv9[match(cohort_impact$iso3c,phpv9$iso3c),2]
    diag_no <- (unique(cohort_impact$year_vac) - early_cohort) + 1
    cohort_impact$multiplier =  rep(indirect_effects[diag_no,] * multiplier[diag_no,],length(iso3c))
    cohort_impact$disc = disc$disc[match(x = cohort_impact$calendar_year,table = disc$year)]
    # sm = multiplier[diag_no,]
    
    
    # Type 16 / 18 cases only:
    cases_directly_averted  = (cohort_impact$prevac_cases_all - cohort_impact$postvac_cases_direct)#*sm
    deaths_directly_averted = (cohort_impact$prevac_deaths_all - cohort_impact$postvac_deaths_direct)#*sm
    dalys_directly_averted  = (cohort_impact$prevac_dalys_all - cohort_impact$postvac_dalys_direct)#*sm
    
    cases_indirectly_averted <- cohort_impact$prevac_cases_all*cohort_impact$multiplier
    deaths_indirectly_averted <- cohort_impact$prevac_deaths_all*cohort_impact$multiplier
    dalys_indirectly_averted <- cohort_impact$prevac_dalys_all*cohort_impact$multiplier
    
    # Convert to All types.
    prevac_cases_1618 <- cohort_impact$prevac_cases_all
    postvac_cases_1618 <- prevac_cases_1618 - (cases_directly_averted + cases_indirectly_averted)
    cohort_impact$prevac_cases_all <- prevac_cases_1618 / cohort_impact$p1618
    cohort_impact$postvac_cases_all <- postvac_cases_1618 + ((prevac_cases_1618/cohort_impact$p1618)*(cohort_impact$phpv9)-prevac_cases_1618) #(prevac_cases_1618 * ((1 - cohort_impact$p1618) / cohort_impact$p1618))
    cohort_impact$cases_averted = cohort_impact$prevac_cases_all - cohort_impact$postvac_cases_all 
    
    prevac_deaths_1618 <- cohort_impact$prevac_deaths_all 
    postvac_deaths_1618 <- prevac_deaths_1618 - (deaths_directly_averted + deaths_indirectly_averted)
    cohort_impact$prevac_deaths_all <- prevac_deaths_1618 / cohort_impact$p1618
    cohort_impact$postvac_deaths_all <- postvac_deaths_1618 + ((prevac_deaths_1618/cohort_impact$p1618)*(cohort_impact$phpv9)-prevac_deaths_1618) #(prevac_deaths_1618 * ((1 - cohort_impact$p1618) / cohort_impact$p1618))
    cohort_impact$deaths_averted = cohort_impact$prevac_deaths_all - cohort_impact$postvac_deaths_all 
    
    prevac_dalys_1618 <-  cohort_impact$prevac_dalys_all 
    postvac_dalys_1618 <- prevac_dalys_1618 - (dalys_directly_averted + dalys_indirectly_averted)
    cohort_impact$prevac_dalys_all <- prevac_dalys_1618 / cohort_impact$p1618
    cohort_impact$postvac_dalys_all <- postvac_dalys_1618 + ((prevac_dalys_1618/cohort_impact$p1618)*(cohort_impact$phpv9)-prevac_dalys_1618) #(prevac_dalys_1618 * ((1 - cohort_impact$p1618) / cohort_impact$p1618))
    cohort_impact$dalys_averted = cohort_impact$prevac_dalys_all - cohort_impact$postvac_dalys_all 
    
    
    cohort_impact$prevac_cases_all_disc = cohort_impact$prevac_cases_all*cohort_impact$disc
    cohort_impact$prevac_deaths_all_disc = cohort_impact$prevac_deaths_all*cohort_impact$disc
    cohort_impact$prevac_dalys_all_disc = cohort_impact$prevac_dalys_all*cohort_impact$disc
    
    cohort_impact$postvac_cases_all_disc = cohort_impact$postvac_cases_all*cohort_impact$disc
    cohort_impact$postvac_deaths_all_disc = cohort_impact$postvac_deaths_all*cohort_impact$disc
    cohort_impact$postvac_dalys_all_disc = cohort_impact$postvac_dalys_all*cohort_impact$disc
    
    cohort_impact$cases_averted_disc = cohort_impact$cases_averted*cohort_impact$disc
    cohort_impact$deaths_averted_disc = cohort_impact$deaths_averted*cohort_impact$disc
    cohort_impact$dalys_averted_disc = cohort_impact$dalys_averted*cohort_impact$disc
    # which(colnames(cohort_impact))
    
    cohort_impact = cohort_impact[,c(1:5,19:23,12,14,16,24,26,28,25,27,29,36:38)]
    # Assign this data to it's own vaccine_impact_$ table. Add to OUTPUT.
    cohort_list[[birthcohort_year-1920]] <- data.frame(cohort_impact)
    # print(birthcohort_year)
    rm(cohort_impact)
  }
  
  # Combine vaccine impact file and output.
  vaccine_impact <- do.call(rbind.data.frame, cohort_list)# rbindlist()
  return(vaccine_impact)
}

if(1)
{
  # calculate direct + indrect effects from the vaccine
  strategy_life = list()
  for(i in 1:length(s_life_hpvadviseIndia))
  {
    strategy_life[[i]]  = estimateVaccineImpact_indirecteffects(DATALIST = s_life_hpvadviseIndia[[i]])
    print(i)
  }
  save(strategy_life,file = paste0('output/prime/vaccine_impact_hpvadviseIndia_life.rdata'))
  rm(strategy_life)
  
  strategy_20y = list()
  for(i in 1:length(s_life_hpvadviseIndia))
  {
    strategy_20y[[i]]  = estimateVaccineImpact_indirecteffects(DATALIST = s_20y_hpvadviseIndia[[i]])
    print(i)
  }
  save(strategy_20y,file = paste0('output/prime/vaccine_impact_hpvadviseIndia_20y.rdata'))
  rm(strategy_20y,s_20y_hpvadviseIndia)
  
  
  strategy_30y = list()
  for(i in 1:length(s_life_hpvadviseIndia))
  {
    strategy_30y[[i]]  = estimateVaccineImpact_indirecteffects(DATALIST = s_30y_hpvadviseIndia[[i]])
    print(i)
  }
  save(strategy_30y,file = paste0('output/prime/vaccine_impact_hpvadviseIndia_30y.rdata'))
  rm(strategy_30y,s_30y_hpvadviseIndia)
  
  strategy_life80take = list()
  for(i in 1:length(s_life_hpvadviseIndia))
  {
    strategy_life80take[[i]]  = estimateVaccineImpact_indirecteffects(DATALIST = s_life80take_hpvadviseIndia[[i]])
    print(i)
  }
  save(strategy_life80take,file = paste0('output/prime/vaccine_impact_hpvadviseIndia_life80take.rdata'))
  rm(strategy_life80take,s_life80take_hpvadviseIndia)

}

if(1)
{
  # calculate direct + indrect effects from the vaccine
  strategy_life = list()
  for(i in 1:length(s_life_hpvadviseNigeria))
  {
    strategy_life[[i]]  = estimateVaccineImpact_indirecteffects(DATALIST = s_life_hpvadviseNigeria[[i]])
    print(i)
  }
  save(strategy_life,file = paste0('output/prime/vaccine_impact_hpvadviseNigeria_life.rdata'))
  rm(strategy_life)
  
  strategy_20y = list()
  for(i in 1:length(s_life_hpvadviseNigeria))
  {
    strategy_20y[[i]]  = estimateVaccineImpact_indirecteffects(DATALIST = s_20y_hpvadviseNigeria[[i]])
    print(i)
  }
  save(strategy_20y,file = paste0('output/prime/vaccine_impact_hpvadviseNigeria_20y.rdata'))
  rm(strategy_20y,s_20y_hpvadviseNigeria)
  
  
  strategy_30y = list()
  for(i in 1:length(s_life_hpvadviseNigeria))
  {
    strategy_30y[[i]]  = estimateVaccineImpact_indirecteffects(DATALIST = s_30y_hpvadviseNigeria[[i]])
    print(i)
  }
  save(strategy_30y,file = paste0('output/prime/vaccine_impact_hpvadviseNigeria_30y.rdata'))
  rm(strategy_30y,s_30y_hpvadviseNigeria)
  
  strategy_life80take = list()
  for(i in 1:length(s_life_hpvadviseNigeria))
  {
    strategy_life80take[[i]]  = estimateVaccineImpact_indirecteffects(DATALIST = s_life80take_hpvadviseNigeria[[i]])
    print(i)
  }
  save(strategy_life80take,file = paste0('output/prime/vaccine_impact_hpvadviseNigeria_life80take.rdata'))
  rm(strategy_life80take,s_life80take_hpvadviseNigeria)
  
}

if(1)
{
  # calculate direct + indrect effects from the vaccine
  strategy_life = list()
  for(i in 1:length(s_life_hpvadviseUganda))
  {
    strategy_life[[i]]  = estimateVaccineImpact_indirecteffects(DATALIST = s_life_hpvadviseUganda[[i]])
    print(i)
  }
  save(strategy_life,file = paste0('output/prime/vaccine_impact_hpvadviseUganda_life.rdata'))
  rm(strategy_life)
  
  strategy_20y = list()
  for(i in 1:length(s_life_hpvadviseUganda))
  {
    strategy_20y[[i]]  = estimateVaccineImpact_indirecteffects(DATALIST = s_20y_hpvadviseUganda[[i]])
    print(i)
  }
  save(strategy_20y,file = paste0('output/prime/vaccine_impact_hpvadviseUganda_20y.rdata'))
  rm(strategy_20y,s_20y_hpvadviseUganda)
  
  
  strategy_30y = list()
  for(i in 1:length(s_life_hpvadviseUganda))
  {
    strategy_30y[[i]]  = estimateVaccineImpact_indirecteffects(DATALIST = s_30y_hpvadviseUganda[[i]])
    print(i)
  }
  save(strategy_30y,file = paste0('output/prime/vaccine_impact_hpvadviseUganda_30y.rdata'))
  rm(strategy_30y,s_30y_hpvadviseUganda)
  
  strategy_life80take = list()
  for(i in 1:length(s_life_hpvadviseUganda))
  {
    strategy_life80take[[i]]  = estimateVaccineImpact_indirecteffects(DATALIST = s_life80take_hpvadviseUganda[[i]])
    print(i)
  }
  save(strategy_life80take,file = paste0('output/prime/vaccine_impact_hpvadviseUganda_life80take.rdata'))
  rm(strategy_life80take,s_life80take_hpvadviseUganda)
  
}

if(1)
{
  # calculate direct + indrect effects from the vaccine
  strategy_life = list()
  for(i in 1:length(s_life_hpvadviseVietnam))
  {
    strategy_life[[i]]  = estimateVaccineImpact_indirecteffects(DATALIST = s_life_hpvadviseVietnam[[i]])
    print(i)
  }
  save(strategy_life,file = paste0('output/prime/vaccine_impact_hpvadviseVietnam_life.rdata'))
  rm(strategy_life)
  
  strategy_20y = list()
  for(i in 1:length(s_life_hpvadviseVietnam))
  {
    strategy_20y[[i]]  = estimateVaccineImpact_indirecteffects(DATALIST = s_20y_hpvadviseVietnam[[i]])
    print(i)
  }
  save(strategy_20y,file = paste0('output/prime/vaccine_impact_hpvadviseVietnam_20y.rdata'))
  rm(strategy_20y,s_20y_hpvadviseVietnam)
  
  
  strategy_30y = list()
  for(i in 1:length(s_life_hpvadviseVietnam))
  {
    strategy_30y[[i]]  = estimateVaccineImpact_indirecteffects(DATALIST = s_30y_hpvadviseVietnam[[i]])
    print(i)
  }
  save(strategy_30y,file = paste0('output/prime/vaccine_impact_hpvadviseVietnam_30y.rdata'))
  rm(strategy_30y,s_30y_hpvadviseVietnam)
  
  strategy_life80take = list()
  for(i in 1:length(s_life_hpvadviseVietnam))
  {
    strategy_life80take[[i]]  = estimateVaccineImpact_indirecteffects(DATALIST = s_life80take_hpvadviseVietnam[[i]])
    print(i)
  }
  save(strategy_life80take,file = paste0('output/prime/vaccine_impact_hpvadviseVietnam_life80take.rdata'))
  rm(strategy_life80take,s_life80take_hpvadviseVietnam)
  
}

if(1)
{
  # calculate direct + indrect effects from the vaccine
  strategy_life = list()
  for(i in 1:length(s_life_hpvadviseCanada))
  {
    strategy_life[[i]]  = estimateVaccineImpact_indirecteffects(DATALIST = s_life_hpvadviseCanada[[i]])
    print(i)
  }
  save(strategy_life,file = paste0('output/prime/vaccine_impact_hpvadviseCanada_life.rdata'))
  rm(strategy_life)
  
  strategy_20y = list()
  for(i in 1:length(s_life_hpvadviseCanada))
  {
    strategy_20y[[i]]  = estimateVaccineImpact_indirecteffects(DATALIST = s_20y_hpvadviseCanada[[i]])
    print(i)
  }
  save(strategy_20y,file = paste0('output/prime/vaccine_impact_hpvadviseCanada_20y.rdata'))
  rm(strategy_20y,s_20y_hpvadviseCanada)
  
  
  strategy_30y = list()
  for(i in 1:length(s_life_hpvadviseCanada))
  {
    strategy_30y[[i]]  = estimateVaccineImpact_indirecteffects(DATALIST = s_30y_hpvadviseCanada[[i]])
    print(i)
  }
  save(strategy_30y,file = paste0('output/prime/vaccine_impact_hpvadviseCanada_30y.rdata'))
  rm(strategy_30y,s_30y_hpvadviseCanada)
  
  strategy_life80take = list()
  for(i in 1:length(s_life_hpvadviseCanada))
  {
    strategy_life80take[[i]]  = estimateVaccineImpact_indirecteffects(DATALIST = s_life80take_hpvadviseCanada[[i]])
    print(i)
  }
  save(strategy_life80take,file = paste0('output/prime/vaccine_impact_hpvadviseCanada_life80take.rdata'))
  rm(strategy_life80take,s_life80take_hpvadviseCanada)
  
}

