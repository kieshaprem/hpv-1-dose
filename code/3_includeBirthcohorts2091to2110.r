# source('code/2_dataPrep_phe.r')
load('output/prime/vaccine_impact.rdata')
# load(file = 'output/prime/vaccine_impact_inc_mort.rdata')
# load(file = 'output/prime/vaccine_impact_phe_life_0.rdata')
load('output/birthcohorts.rdata')

total_output = total_output[total_output$age<100,]
countries = names(birthcohorts)
cecx_birthcohort2090 = total_output[total_output$year_vac %in% 2100,]

# vaccineImpact_birthcohort2090 = vaccine_impact[vaccine_impact$year_vac %in% 2100,]

# bc = birthcohorts$AFG
# bc[1,1]*0.8

# check data consistency 
# cecx_birthcohort2090$cohort_size[cecx_birthcohort2090$iso3c %in% "AFG"]-
# vaccineImpact_birthcohort2090$cohort_size[vaccineImpact_birthcohort2090$iso3c %in% "AFG"]
# 
# (vaccineImpact_birthcohort2090$prevac_cases_all[vaccineImpact_birthcohort2090$iso3c %in% "AFG"]) - 
#   (cecx_birthcohort2090$cohort_size[cecx_birthcohort2090$iso3c %in% "AFG"]*cecx_birthcohort2090$prevac_inc_cecx[cecx_birthcohort2090$iso3c %in% "AFG"])
# 
# (vaccineImpact_birthcohort2090$postvac_cases_direct[vaccineImpact_birthcohort2090$iso3c %in% "AFG"]) - 
#   (cecx_birthcohort2090$cohort_size[cecx_birthcohort2090$iso3c %in% "AFG"]*cecx_birthcohort2090$postvac_inc_cecx[cecx_birthcohort2090$iso3c %in% "AFG"])
# 
# 
# (vaccineImpact_birthcohort2090$prevac_deaths_all[vaccineImpact_birthcohort2090$iso3c %in% "AFG"]) - 
#   (cecx_birthcohort2090$cohort_size[cecx_birthcohort2090$iso3c %in% "AFG"]*cecx_birthcohort2090$prevac_mort_cecx[cecx_birthcohort2090$iso3c %in% "AFG"])
# 
# (vaccineImpact_birthcohort2090$postvac_deaths_direct[vaccineImpact_birthcohort2090$iso3c %in% "AFG"]) - 
#   (cecx_birthcohort2090$cohort_size[cecx_birthcohort2090$iso3c %in% "AFG"]*cecx_birthcohort2090$postvac_mort_cecx[cecx_birthcohort2090$iso3c %in% "AFG"])
# 
# 
# (vaccineImpact_birthcohort2090$prevac_dalys_all[vaccineImpact_birthcohort2090$iso3c %in% "AFG"]) -
#   (cecx_birthcohort2090$cohort_size[cecx_birthcohort2090$iso3c %in% "AFG"]*cecx_birthcohort2090$prevac_yldyll[cecx_birthcohort2090$iso3c %in% "AFG"])
# 
# (vaccineImpact_birthcohort2090$postvac_dalys_direct[vaccineImpact_birthcohort2090$iso3c %in% "AFG"]) -
#   (cecx_birthcohort2090$cohort_size[cecx_birthcohort2090$iso3c %in% "AFG"]*cecx_birthcohort2090$postvac_yldyll[cecx_birthcohort2090$iso3c %in% "AFG"])



output = list()
for(co in 1:length(countries))
{
  iso3c = countries[co]
  output_birthyear = list()
  for(birthcohort_year in 2091:2110)
  {
    output_birthyear$iso3c = iso3c
    output_birthyear$year_vac = birthcohort_year + 10
    output_birthyear$age = 0:99
    output_birthyear$calendar_year = birthcohort_year:(birthcohort_year+99)
    output_birthyear$cohort_size = as.numeric(birthcohorts[[iso3c]][,birthcohort_year-2089])
    
    output_birthyear$prevac_inc_cecx = cecx_birthcohort2090$prevac_inc_cecx[cecx_birthcohort2090$iso3c %in% output_birthyear$iso3c]
    output_birthyear$postvac_inc_cecx = cecx_birthcohort2090$postvac_inc_cecx[cecx_birthcohort2090$iso3c %in% output_birthyear$iso3c]
    
    output_birthyear$prevac_mort_cecx = cecx_birthcohort2090$prevac_mort_cecx[cecx_birthcohort2090$iso3c %in% output_birthyear$iso3c]
    output_birthyear$postvac_mort_cecx = cecx_birthcohort2090$postvac_mort_cecx[cecx_birthcohort2090$iso3c %in% output_birthyear$iso3c]
    
    output_birthyear$prevac_yldyll = cecx_birthcohort2090$prevac_yldyll[cecx_birthcohort2090$iso3c %in% output_birthyear$iso3c]
    output_birthyear$postvac_yldyll = cecx_birthcohort2090$postvac_yldyll[cecx_birthcohort2090$iso3c %in% output_birthyear$iso3c]
    
    output_birthyear$prevac_cases_all = output_birthyear$cohort_size*cecx_birthcohort2090$prevac_inc_cecx[cecx_birthcohort2090$iso3c %in% output_birthyear$iso3c]
    output_birthyear$postvac_cases_direct = output_birthyear$cohort_size*cecx_birthcohort2090$postvac_inc_cecx[cecx_birthcohort2090$iso3c %in% output_birthyear$iso3c]
    output_birthyear$prevac_deaths_all = output_birthyear$cohort_size*cecx_birthcohort2090$prevac_mort_cecx[cecx_birthcohort2090$iso3c %in% output_birthyear$iso3c]
    output_birthyear$postvac_deaths_direct = output_birthyear$cohort_size*cecx_birthcohort2090$postvac_mort_cecx[cecx_birthcohort2090$iso3c %in% output_birthyear$iso3c]
    output_birthyear$prevac_dalys_all = output_birthyear$cohort_size*cecx_birthcohort2090$prevac_yldyll[cecx_birthcohort2090$iso3c %in% output_birthyear$iso3c]
    output_birthyear$postvac_dalys_direct = output_birthyear$cohort_size*cecx_birthcohort2090$postvac_yldyll[cecx_birthcohort2090$iso3c %in% output_birthyear$iso3c]
    output_birthyear$total_vaccinated = c(rep(0,10),rep(0.8,90))*output_birthyear$cohort_size
    output[[paste0(iso3c,'_',birthcohort_year)]] = data.frame(output_birthyear)
  }
}

total_output_2091to2110 = do.call(rbind.data.frame, output)
total_output_1931to2120 = rbind(total_output,total_output_2091to2110)

save(total_output_1931to2120,file = 'output/prime/vaccine_impact1931to2120.rdata' )

