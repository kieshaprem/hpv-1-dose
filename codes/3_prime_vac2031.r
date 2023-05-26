load('output/prime/results_prime_df_1931to2120.rdata')

results_prime_df[results_prime_df$calendar_year %in% 2021 & results_prime_df$vaccinated > 0,]

# vaccination tpo start at year 2031 
# means girls aged 15 years old and older will not be in the vaccinated cohort 

summary(results_prime_df$vaccinated[results_prime_df$calendar_year<2020])

table(results_prime_df$vaccinated[results_prime_df$calendar_year==2020])
table(results_prime_df$vaccinated[results_prime_df$calendar_year==2022])
table(results_prime_df$vaccinated[results_prime_df$calendar_year==2023])


table(results_prime_df$total_vaccinated[results_prime_df$calendar_year<2020])


names(results_prime_df)

for(years in 2021:2030)
{
  #years = 2021
  # results_prime_df[results_prime_df$calendar_year %in% years & results_prime_df$birthcohort > 2017,]
  i = which(results_prime_df$calendar_year %in% years & results_prime_df$birthcohort < 2017)
  results_prime_df$vaccinated[i] = 0 
  results_prime_df$immunized[i] = 0 
  
  results_prime_df$inc.cecx_postvac[i] = results_prime_df$inc.cecx_prevac[i] 
  results_prime_df$mort.cecx_postvac[i] = results_prime_df$mort.cecx_prevac[i]
  results_prime_df$disability_postvac[i] = results_prime_df$disability_prevac[i]
  results_prime_df$lifey_postvac[i] = results_prime_df$lifey_prevac[i]
  
  results_prime_df$cases_postvac[i] = results_prime_df$cases_prevac[i]
  results_prime_df$deaths_postvac[i] = results_prime_df$deaths_prevac[i]
  results_prime_df$dalys_postvac[i] = results_prime_df$dalys_prevac[i]
  
  results_prime_df$cases_averted_direct[i] = 0 
  results_prime_df$deaths_averted_direct[i] = 0 
  results_prime_df$dalys_averted_direct[i] = 0   
}



save(results_prime_df,file = 'output/prime/results_prime_df_1931to2120_vac2031.rdata')

