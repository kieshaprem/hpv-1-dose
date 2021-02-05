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
options(scipen=999)



coverage = c(array(0,length(1931:2020)),array(0.8,length(2021:2100)))
batch_file = data.frame(country_code = rep(as.character(unique(data.pop$country_code)),each = length(coverage)), 
                        year = rep(1931:2100,length(as.character(unique(data.pop$country_code)))),
                        age_first = 10, 
                        age_last = 10,
                        coverage =rep(coverage,length(as.character(unique(data.pop$country_code)))))


# Batch info/ input for all countries.
batch_input <- batch_file %>% dplyr::filter(year >= 1931)

# Save and read batch input file.
write.table(batch_input, "output/batch_input.csv", sep = ",", col.names = T, row.names = F)
batch_file <- fread("output/batch_input.csv", sep = ",", header= T)
country_code = unique(batch_file$country_code)
# Use BatchRun to generate results (all countries).

# Loop through all countries.
a <- 0
b <- 0
timeStart = print(Sys.time())
total_output <- NULL
temp_list <- list()
early_cohort <- 1931

batch_input_information <- batch_file %>% dplyr::filter(year >= early_cohort)

for(iso3c_code in country_code)
{  #iso3c_code = "AFG"
  
  # # Save vaccine impact header.
  # new_table_name <- paste(R, blank_table_name, sep = "_")
  # write.table(batch_header, new_table_name, sep = ",", row.names = F)
  # # saveRDS(batch_header, file = new_table_name)
  # 
  # # Set negative indirect.factors to zero.
  # indirect.cohort.reduction.list[[R]] <- (indirect.cohort.reduction.list[[R]] + abs(indirect.cohort.reduction.list[[R]])) / 2
  # scenario.multiplier.list[[R]] <- (scenario.multiplier.list[[R]] + abs(scenario.multiplier.list[[R]])) / 2  #test
  
  # Loop through all countries.
  
  
  
  # Country counter.
  a <- a + 1
  print(c(a,iso3c_code))
  
  
  # to run for all countries, set (a>0) 
  # (a>175) runs for only 2 countries ~ 177 - 175
  if (a>0) {
    
    # batch info/input for 1 country
    batch_input = batch_file %>% filter (country_code == iso3c_code)# %>% filter (year == y)
    
    #### debug -- check later
    # save and read batch input file for 1 country
    # sounds redundant but takes care of a wierd error in not running further
    write.table (batch_input, "output/temp.csv", sep=",", col.names = T, row.names=F)
    batch_input = fread ("output/temp.csv", sep = ",", header= TRUE)
    #####
    
    # register batch data
    RegisterBatchData (batch_input, force = T)
    
    # Batch run to generate results
    output <- BatchRun ()
    
    # Filter BatchRun outputs for specific country.
    country_impact <- output %>% dplyr::filter(country == iso3c_code)
    
    
    
    # # Extract country-specific p1618 value (used to convert PRIME outputs back to All types).
    # prime_table <- data.global %>% dplyr::filter(iso3 == iso3c_code)
    # p1618 <- (prime_table[1, 25] / 100)
    # 
    # indirect.cohort.reduction <- indirect.cohort.reduction.list[[R]]
    
    # Check if vaccine impact estimates are empty.
    if ((nrow(country_impact)) == 0) {
      stop ("No vaccine impact estimates for one or more countries.")
    }
    
    if ((nrow(country_impact)) > 0) {
      # Create temporary data table for vaccine impact.
      temp_impact <- data.table(age = 0:100)  ##
      # temp_impact$protection <- protection
      temp_impact$iso3c <- iso3c_code
      temp_impact$year_vac <- 0
      # setcolorder(temp_impact, c("protection", "iso3c", "year_vac", "age"))
      setcolorder(temp_impact, c("iso3c", "year_vac", "age"))
      
      # Format results to the required format. Birth cohort for 12 year olds in 2020 relates
      # to year 2008.
      first_birth_year <- 1931 - 10
      last_birth_year <- batch_input$year[length(batch_input$year)] - 10
      
      c <- 0
      for (birthcohort_year in first_birth_year:last_birth_year) 
      { ##
        # Cohort counter.
        b <- b + 1  # batch_file row (overall cohort no.)
        c <- c + 1  # batch_input row
        
        # Extract year of vaccination and current calendar year.
        temp_impact$year_vac <- birthcohort_year + 10  # could improve this
        temp_impact$calendar_year <- birthcohort_year + temp_impact$age
        
        # # Extract relevant section of health discounting vector.
        # disc.ben.yr <- disc.ben.full[c:(c+74)]
        
        # Extract impact for 1 cohort.
        cohort_impact <- country_impact %>%
          dplyr::filter(birthcohort == birthcohort_year)
        
        # Disease burden pre-vaccination.
        pre_vac <- cohort_impact %>%
          dplyr::filter(scenario == "pre-vaccination" & age < 101)
        
        # Disease burden post-vaccination.
        post_vac <- cohort_impact %>%
          dplyr::filter(scenario == "post-vaccination" & age < 101)
        
        # Estimate vaccine impact (cases, deaths and dalys averted) for all cohorts.
        if (unique(temp_impact$year_vac) > as.numeric(early_cohort - 1)) 
        {
          diag_no <- (unique(temp_impact$year_vac) - early_cohort) + 1
          # temp_impact$PHE_multiplier <- indirect.cohort.reduction[, diag_no] * scenario.multiplier.list[[R]][, diag_no]
          # s <- scenario.multiplier.list[[R]][, diag_no]
          temp_impact$cohort_size = pre_vac$cohort_size
          
          temp_impact$prevac_inc_cecx <- (pre_vac$inc.cecx ) 
          temp_impact$postvac_inc_cecx <- (post_vac$inc.cecx)# + (pre_vac$inc.cecx * ((1 - 1) / 1))) * pre_vac$cohort_size  # direct only #prec_all_prime - ((prec_all_prime - postc_all_prime) * (s + temp_impact$PHE_multiplier))
          
          temp_impact$prevac_mort_cecx <- (pre_vac$mort.cecx) #* pre_vac$cohort_size) / 1
          temp_impact$postvac_mort_cecx <- (post_vac$mort.cecx)# + 
          
          temp_impact$prevac_yldyll <- ((pre_vac$disability + pre_vac$lifey))# * pre_vac$cohort_size) / 1
          temp_impact$postvac_yldyll <- ((post_vac$disability + post_vac$lifey))#
          
          # prec_all_prime <- (pre_vac$inc.cecx * pre_vac$cohort_size) / p1618
          # postc_all_prime <- (post_vac$inc.cecx + (pre_vac$inc.cecx * ((1 - p1618) / p1618))) * pre_vac$cohort_size  # direct only
          temp_impact$prevac_cases_all <- (pre_vac$inc.cecx * pre_vac$cohort_size) / 1
          temp_impact$postvac_cases_direct <- (post_vac$inc.cecx + (pre_vac$inc.cecx * ((1 - 1) / 1))) * pre_vac$cohort_size  # direct only #prec_all_prime - ((prec_all_prime - postc_all_prime) * (s + temp_impact$PHE_multiplier))

          
          temp_impact$prevac_deaths_all <- (pre_vac$mort.cecx * pre_vac$cohort_size) / 1
          temp_impact$postvac_deaths_direct <- (post_vac$mort.cecx + (pre_vac$mort.cecx * ((1 - 1) / 1))) * pre_vac$cohort_size  # direct only #prec_all_prime - ((prec_all_prime - postc_all_prime) * (s + temp_impact$PHE_multiplier))
          
          # prey_all_prime <- ((pre_vac$disability + pre_vac$lifey) * pre_vac$cohort_size) / p1618
          # posty_all_prime <- ((post_vac$disability + post_vac$lifey) + ((pre_vac$disability + pre_vac$lifey) * ((1 - p1618) / p1618))) * pre_vac$cohort_size  # direct only
          temp_impact$prevac_dalys_all <- ((pre_vac$disability + pre_vac$lifey) * pre_vac$cohort_size) / 1
          temp_impact$postvac_dalys_direct <- ((post_vac$disability + post_vac$lifey) + ((pre_vac$disability + pre_vac$lifey) * ((1 - 1) / 1))) * pre_vac$cohort_size
          
          cases_averted =  (pre_vac$inc.cecx - post_vac$inc.cecx) * pre_vac$cohort_size
          deaths_averted =  (pre_vac$mort.cecx - post_vac$mort.cecx) * pre_vac$cohort_size
          dalys_averted =  (((pre_vac$disability - post_vac$disability) + (pre_vac$lifey - post_vac$lifey)) * pre_vac$cohort_size)
          
          # Compute number of people vaccinated.
          temp_impact$total_vaccinated <- post_vac$cohort_size * post_vac$vaccinated
        }
        # Assign this data to it's own vaccine_impact_$ table. Add to total_output.
        temp_list[[b]] <- temp_impact
        
      }
    }
  }
}         
          
  
timeEnd = print(Sys.time())
timeEnd - timeStart

# Save vaccine impact file and output.
total_output <- rbindlist(temp_list)
save(total_output, file = 'output/prime/vaccine_impact.rdata')
# saveRDS(total_output, file = new_table_name)




