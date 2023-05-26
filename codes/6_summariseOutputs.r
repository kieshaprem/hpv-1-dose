# start_time <- Sys.time()
load('output/impact_all.rdata')
# end_time <- Sys.time()
# end_time - start_time
source('code/functions_supporting.r')

REGIONNAMES = list("Low income",
                   c("Lower middle income","Upper middle income"),
                   "High income",
                   c("Low income","Lower middle income","Upper middle income","High income"))

REGIONLABEL = c('Low-income countries',
                'Middle-income countries',
                'High-income countries',
                'World')

SCENLABEL = c('20 years at 100% VE',
              '30 years at 100% VE',
              'Lifelong years at 80% VE')

disc = data.frame(year = 2020:2209, disc = discount(0.03,STARTYEAR = 2020,LASTYEAR = 2209))



getYvaluesPropPercentile = function(IMPACT,PROTECT,VAR = 'cases_averted',REGIONS,PERCENTILES,PERCENTILE = 0.5,DISCOUNTING=FALSE)
{
  # PROTECT = 2 
  # VAR = 'cases_averted'
  # REGIONS = REGIONNAMES[[4]]
  # IMPACT = impact_phe
  YVAL = array(NA,1)
  YALL = array(NA,c(1,length(IMPACT[[PROTECT]])))
  RUN=1
  if(DISCOUNTING) DISC = disc$disc[match(x = IMPACT[[1]][[RUN]][IMPACT[[1]][[RUN]]$incomegroup %in% REGIONS,'year'],table = disc$year)]
  if(!DISCOUNTING) DISC = disc$disc[match(x = IMPACT[[1]][[RUN]][IMPACT[[1]][[RUN]]$incomegroup %in% REGIONS,'year'],table = disc$year)]/disc$disc[match(x = IMPACT[[1]][[RUN]][IMPACT[[1]][[RUN]]$incomegroup %in% REGIONS,'year'],table = disc$year)]
  
  for(RUN in 1:length(IMPACT[[PROTECT]])) 
  {
    yvals1 = sum(IMPACT[[1]][[RUN]][IMPACT[[1]][[RUN]]$incomegroup %in% REGIONS,VAR]*DISC)
    yvals2 = sum(IMPACT[[PROTECT]][[RUN]][IMPACT[[PROTECT]][[RUN]]$incomegroup %in% REGIONS,VAR]*DISC)
    YALL[RUN] = (yvals1 - yvals2)/yvals1
  }
  
  YVAL[1] = quantile((YALL),probs = PERCENTILE)
  YVAL[YVAL < 0] =0
  return(YVAL)
}

# model=1
# REG =4
# yval = getYvaluesPercentile(IMPACT = impact_all[[model]],PROTECT = 2,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE, PERCENTILE = 0.5) 
# yval_disc= getYvaluesPercentile(IMPACT = impact_all[[model]],PROTECT = 2,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE, PERCENTILE = 0.5,DISCOUNTING =disc$disc)
# 
# getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 2,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]], PERCENTILE = 0.5,DISCOUNTING = FALSE)
# getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 2,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]], PERCENTILE = 0.5,DISCOUNTING = TRUE)

RESULTS_TIMESERIES = TRUE
RESULTS_PROPORTION = TRUE



if(RESULTS_TIMESERIES)
{
  ys = list()
  ys[[1]] =ys[[2]] =ys[[3]] = array(NA, c(10,190))
  YS_cases_timeseries = list(lic = ys,mic = ys, hic =ys, world = ys)
  YS_deaths_timeseries = list(lic = ys,mic = ys, hic =ys, world = ys)
  YS_cases_timeseries_dish = list(lic = ys,mic = ys, hic =ys, world = ys)
  YS_deaths_timeseries_dish = list(lic = ys,mic = ys, hic =ys, world = ys)
  for(REG in 1:4)
  {
    print(REG)
    for(model in 1:10)
    {
      YS_cases_timeseries[[REG]][[1]][model,] = getYvaluesPercentile(IMPACT = impact_all[[model]],PROTECT = 2,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE, PERCENTILE = 0.5)
      YS_cases_timeseries[[REG]][[2]][model,] = getYvaluesPercentile(IMPACT = impact_all[[model]],PROTECT = 3,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE, PERCENTILE = 0.5)
      YS_cases_timeseries[[REG]][[3]][model,] = getYvaluesPercentile(IMPACT = impact_all[[model]],PROTECT = 4,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE, PERCENTILE = 0.5)
      
      YS_deaths_timeseries[[REG]][[1]][model,] = getYvaluesPercentile(IMPACT = impact_all[[model]],PROTECT = 2,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE, PERCENTILE = 0.5)
      YS_deaths_timeseries[[REG]][[2]][model,] = getYvaluesPercentile(IMPACT = impact_all[[model]],PROTECT = 3,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE, PERCENTILE = 0.5)
      YS_deaths_timeseries[[REG]][[3]][model,] = getYvaluesPercentile(IMPACT = impact_all[[model]],PROTECT = 4,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE, PERCENTILE = 0.5)
      
      YS_cases_timeseries_dish[[REG]][[1]][model,] = getYvaluesPercentile(IMPACT = impact_all[[model]],PROTECT = 2,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE, PERCENTILE = 0.5,DISCOUNTING =disc$disc)
      YS_cases_timeseries_dish[[REG]][[2]][model,] = getYvaluesPercentile(IMPACT = impact_all[[model]],PROTECT = 3,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE, PERCENTILE = 0.5,DISCOUNTING =disc$disc)
      YS_cases_timeseries_dish[[REG]][[3]][model,] = getYvaluesPercentile(IMPACT = impact_all[[model]],PROTECT = 4,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE, PERCENTILE = 0.5,DISCOUNTING =disc$disc)
      
      YS_deaths_timeseries_dish[[REG]][[1]][model,] = getYvaluesPercentile(IMPACT = impact_all[[model]],PROTECT = 2,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE, PERCENTILE = 0.5,DISCOUNTING =disc$disc)
      YS_deaths_timeseries_dish[[REG]][[2]][model,] = getYvaluesPercentile(IMPACT = impact_all[[model]],PROTECT = 3,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE, PERCENTILE = 0.5,DISCOUNTING =disc$disc)
      YS_deaths_timeseries_dish[[REG]][[3]][model,] = getYvaluesPercentile(IMPACT = impact_all[[model]],PROTECT = 4,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE, PERCENTILE = 0.5,DISCOUNTING =disc$disc)
      
    }
  }
}

timeseries = list(YS_cases_timeseries=YS_cases_timeseries,
                  YS_deaths_timeseries=YS_deaths_timeseries,
                  YS_cases_timeseries_dish=YS_cases_timeseries_dish,
                  YS_deaths_timeseries_dish=YS_deaths_timeseries_dish)
                  
save(timeseries,file = 'output/plots/timeseries.rdata')
rm(YS_cases_timeseries,YS_deaths_timeseries,YS_cases_timeseries_dish,YS_deaths_timeseries_dish)
rm(timeseries,RESULTS_TIMESERIES)




if(RESULTS_PROPORTION)
{
  QUANTILE = c(0.1,0.9)
  ys = list()
  ys[[1]] =ys[[2]] =ys[[3]] = array(NA, c(10,3))

  YS_cases_proportion = list(lic = ys,mic = ys, hic =ys, world = ys)
  YS_deaths_proportion  = list(lic = ys,mic = ys, hic =ys, world = ys)
  YS_cases_proportion_dish = list(lic = ys,mic = ys, hic =ys, world = ys)
  YS_deaths_proportion_dish = list(lic = ys,mic = ys, hic =ys, world = ys)
  
  for(REG in 1:4)
  {
    print(REG)
    for(model in 1:10)
    {
      YS_cases_proportion[[REG]][[1]][model,1] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 2,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = 0.50,DISCOUNTING = FALSE))
      YS_cases_proportion[[REG]][[1]][model,2] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 2,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[1],DISCOUNTING = FALSE))
      YS_cases_proportion[[REG]][[1]][model,3] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 2,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[2],DISCOUNTING = FALSE))
      
      YS_cases_proportion[[REG]][[2]][model,1] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 3,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = 0.50,DISCOUNTING = FALSE))
      YS_cases_proportion[[REG]][[2]][model,2] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 3,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[1],DISCOUNTING = FALSE))
      YS_cases_proportion[[REG]][[2]][model,3] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 3,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[2],DISCOUNTING = FALSE))
      
      YS_cases_proportion[[REG]][[3]][model,1] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 4,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = 0.50,DISCOUNTING = FALSE))
      YS_cases_proportion[[REG]][[3]][model,2] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 4,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[1],DISCOUNTING = FALSE))
      YS_cases_proportion[[REG]][[3]][model,3] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 4,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[2],DISCOUNTING = FALSE))
      
      YS_deaths_proportion[[REG]][[1]][model,1] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 2,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = 0.50,DISCOUNTING = FALSE))
      YS_deaths_proportion[[REG]][[1]][model,2] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 2,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[1],DISCOUNTING = FALSE))
      YS_deaths_proportion[[REG]][[1]][model,3] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 2,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[2],DISCOUNTING = FALSE))
      
      YS_deaths_proportion[[REG]][[2]][model,1] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 3,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = 0.50,DISCOUNTING = FALSE))
      YS_deaths_proportion[[REG]][[2]][model,2] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 3,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[1],DISCOUNTING = FALSE))
      YS_deaths_proportion[[REG]][[2]][model,3] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 3,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[2],DISCOUNTING = FALSE))
      
      YS_deaths_proportion[[REG]][[3]][model,1] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 4,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = 0.50,DISCOUNTING = FALSE))
      YS_deaths_proportion[[REG]][[3]][model,2] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 4,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[1],DISCOUNTING = FALSE))
      YS_deaths_proportion[[REG]][[3]][model,3] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 4,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[2],DISCOUNTING = FALSE))
      
      YS_cases_proportion_dish[[REG]][[1]][model,1] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 2,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = 0.50,DISCOUNTING = TRUE))
      YS_cases_proportion_dish[[REG]][[1]][model,2] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 2,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[1],DISCOUNTING = TRUE))
      YS_cases_proportion_dish[[REG]][[1]][model,3] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 2,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[2],DISCOUNTING = TRUE))
      
      YS_cases_proportion_dish[[REG]][[2]][model,1] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 3,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = 0.50,DISCOUNTING = TRUE))
      YS_cases_proportion_dish[[REG]][[2]][model,2] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 3,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[1],DISCOUNTING = TRUE))
      YS_cases_proportion_dish[[REG]][[2]][model,3] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 3,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[2],DISCOUNTING = TRUE))
      
      YS_cases_proportion_dish[[REG]][[3]][model,1] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 4,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = 0.50,DISCOUNTING = TRUE))
      YS_cases_proportion_dish[[REG]][[3]][model,2] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 4,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[1],DISCOUNTING = TRUE))
      YS_cases_proportion_dish[[REG]][[3]][model,3] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 4,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[2],DISCOUNTING = TRUE))
      
      YS_deaths_proportion_dish[[REG]][[1]][model,1] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 2,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = 0.50,DISCOUNTING = TRUE))
      YS_deaths_proportion_dish[[REG]][[1]][model,2] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 2,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[1],DISCOUNTING = TRUE))
      YS_deaths_proportion_dish[[REG]][[1]][model,3] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 2,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[2],DISCOUNTING = TRUE))
      
      YS_deaths_proportion_dish[[REG]][[2]][model,1] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 3,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = 0.50,DISCOUNTING = TRUE))
      YS_deaths_proportion_dish[[REG]][[2]][model,2] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 3,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[1],DISCOUNTING = TRUE))
      YS_deaths_proportion_dish[[REG]][[2]][model,3] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 3,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[2],DISCOUNTING = TRUE))
      
      YS_deaths_proportion_dish[[REG]][[3]][model,1] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 4,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = 0.50,DISCOUNTING = TRUE))
      YS_deaths_proportion_dish[[REG]][[3]][model,2] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 4,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[1],DISCOUNTING = TRUE))
      YS_deaths_proportion_dish[[REG]][[3]][model,3] = (getYvaluesPropPercentile(IMPACT = impact_all[[model]],PROTECT = 4,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],PERCENTILE = QUANTILE[2],DISCOUNTING = TRUE))
      
    }
  }
  
}

proportion = list(YS_cases_proportion=YS_cases_proportion,
                  YS_deaths_proportion=YS_deaths_proportion,
                  YS_cases_proportion_dish=YS_cases_proportion_dish,
                  YS_deaths_proportion_dish=YS_deaths_proportion_dish)

save(proportion,file = 'output/plots/proportion.rdata')
rm(YS_cases_proportion,YS_deaths_proportion,YS_cases_proportion_dish,YS_deaths_proportion_dish)
rm(proportion,RESULTS_PROPORTION)
