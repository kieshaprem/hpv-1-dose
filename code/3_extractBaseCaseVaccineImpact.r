options(scipen=999)
source('code/functions_supporting.r')
load('output/prime/vaccine_impact1931to2120.rdata')
total_output = total_output_1931to2120

disc = data.frame(year = 1931:2200, disc = discount(0.03,STARTYEAR = 1931,LASTYEAR = 2200))
head(total_output)
total_output$birth_year = total_output$year_vac - 10
total_output = total_output[total_output$age<100,]
total_output= data.frame(total_output)
total_output = total_output[total_output$iso3c %in% names(which(table(total_output$iso3c)>18000)),] 

estimateVaccineImpact = function(OUTPUT = total_output)
{
  iso3c = (unique(OUTPUT$iso3c))
  early_cohort = 1931
  
  # Extract country-specific p1618 value (used to convert PRIME outputs back to All types).
  p1618 = data.frame(iso3c = iso3c, p1618 = as.data.frame(data.global[match(x = iso3c,data.global$iso3),25])*0.01)
  phpv9 = data.frame(iso3c = iso3c, phpv9 = as.data.frame(hpv_distribution[match(x = iso3c,hpv_distribution$iso3),10])*0.01)
  colnames(p1618) = c('iso3c','p1618')
  colnames(phpv9) = c('iso3c','phpv9')
  p1618$p1618[is.na(p1618$p1618)] = mean(p1618$p1618,na.rm=TRUE)
  

  cohort_list = list()
  
  for (birthcohort_year in min(OUTPUT$birth_year):max(OUTPUT$birth_year))
  {
    # Extract impact for 1 cohort.
    cohort_impact <- OUTPUT[OUTPUT$birth_year == birthcohort_year,]
    cohort_impact$p1618 = p1618[match(cohort_impact$iso3c,p1618$iso3c),2]
    cohort_impact$phpv9 = phpv9[match(cohort_impact$iso3c,phpv9$iso3c),2]
    # cohort_impact
    cohort_list[[birthcohort_year-1919]] <- cohort_impact
    rm(cohort_impact)
  }
  
  # Combine vaccine impact file and output.
  vaccine_impact <- do.call(rbind.data.frame, cohort_list)# rbindlist()
  return(vaccine_impact)
}

vaccine_impact_0  = estimateVaccineImpact()
save(vaccine_impact_0,file = paste0('output/prime/vaccine_impact_0.rdata'))
rm(vaccine_impact_0)
