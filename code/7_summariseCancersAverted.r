source('code/functions_supporting.r')

load('output/prime/protect_life_group.rdata')
load('output/prime/protect_20y_group.rdata')
load('output/prime/protect_30y_group.rdata')
load('output/prime/protect_life80take_group.rdata')

protect = list(protect_life_group,protect_20y_group,protect_30y_group,protect_life80take_group)

load('output/prime/protect_life_group_10prec.rdata')
load('output/prime/protect_20y_group_10prec.rdata')
load('output/prime/protect_30y_group_10prec.rdata')
load('output/prime/protect_life80take_group_10prec.rdata')

protect_p10 = list(protect_life_group_p10,protect_20y_group_p10,protect_30y_group_p10,protect_life80take_group_p10)


load('output/prime/protect_life_group_90prec.rdata')
load('output/prime/protect_20y_group_90prec.rdata')
load('output/prime/protect_30y_group_90prec.rdata')
load('output/prime/protect_life80take_group_90prec.rdata')

protect_p90 = list(protect_life_group_p90,protect_20y_group_p90,protect_30y_group_p90,protect_life80take_group_p90)


REGIONNAMES = list("Low income",
                   c("Lower middle income","Upper middle income"),
                   "High income",
                   c("Low income","Lower middle income","Upper middle income","High income"))

REGIONLABEL = c('Low-income countries',
                'Middle-income countries',
                'High-income countries',
                'World')
REG = 1 
model = 1

cancer_dish = list(slife = list(),s20y = list(),s30y=list(),slife80take = list())

for(SCEN in 1:4)
{
  for(REG in 1:4)
  {
    df = data.frame(year = 2020:2209,matrix(NA, nrow=length(2020:2209),ncol = 11))
    df[,2:12] = getYvalues(protect = protect,PROTECT = SCEN,VAR = 'cases_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)
    cancer_dish[[SCEN]][[REG]] = df
  }
}

signif(range(colSums(cancer_dish$s20y[[1]][2:101,2:12])),2)/1E6
signif(range(colSums(cancer_dish$s30y[[1]][2:101,2:12])),2)/1E6
signif(range(colSums(cancer_dish$slife80take[[1]][2:101,2:12])),2)/1E6
signif(range(colSums(cancer_dish$slife[[1]][2:101,2:12])),2)/1E6

signif(range(colSums(cancer_dish$s20y[[2]][2:101,2:12])),2)/1E6
signif(range(colSums(cancer_dish$s30y[[2]][2:101,2:12])),2)/1E6
signif(range(colSums(cancer_dish$slife80take[[2]][2:101,2:12])),2)/1E6
signif(range(colSums(cancer_dish$slife[[2]][2:101,2:12])),2)/1E6


signif(range(colSums(cancer_dish$s20y[[3]][2:101,2:12])),1)/1E6
signif(range(colSums(cancer_dish$s30y[[3]][2:101,2:12])),1)/1E6
signif(range(colSums(cancer_dish$slife80take[[3]][2:101,2:12])),1)/1E6
signif(range(colSums(cancer_dish$slife[[3]][2:101,2:12])),1)/1E6

signif(range(colSums(cancer_dish$s20y[[4]][2:101,2:12])),3)/1E6
signif(range(colSums(cancer_dish$s30y[[4]][2:101,2:12])),3)/1E6
signif(range(colSums(cancer_dish$slife80take[[4]][2:101,2:12])),3)/1E6
signif(range(colSums(cancer_dish$slife[[4]][2:101,2:12])),3)/1E6

signif(median(colSums(cancer_dish$s20y[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(cancer_dish$s30y[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(cancer_dish$slife80take[[4]][2:101,2:12])),3)/1E6

summary((colSums(cancer_dish$s30y[[4]][2:101,2:12]))/(colSums(cancer_dish$slife[[4]][2:101,2:12])))
summary((colSums(cancer_dish$slife80take[[4]][2:101,2:12]))/(colSums(cancer_dish$slife[[4]][2:101,2:12])))


cancer = list(slife = list(),s20y = list(),s30y=list(),slife80take = list())

for(SCEN in 1:4)
{
  for(REG in 1:4)
  {
    df = data.frame(year = 2020:2209,matrix(NA, nrow=length(2020:2209),ncol = 11))
    df[,2:12] = getYvalues(protect = protect,PROTECT = SCEN,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = TRUE)
    cancer[[SCEN]][[REG]] = df
  }
}

signif(range(colSums(cancer$s20y[[1]][2:101,2:12])),2)/1E6
signif(range(colSums(cancer$s30y[[1]][2:101,2:12])),2)/1E6
signif(range(colSums(cancer$slife80take[[1]][2:101,2:12])),2)/1E6
signif(range(colSums(cancer$slife[[1]][2:101,2:12])),2)/1E6

signif(range(colSums(cancer$s20y[[2]][2:101,2:12])),2)/1E6
signif(range(colSums(cancer$s30y[[2]][2:101,2:12])),2)/1E6
signif(range(colSums(cancer$slife80take[[2]][2:101,2:12])),2)/1E6
signif(range(colSums(cancer$slife[[2]][2:101,2:12])),2)/1E6

signif(range(colSums(cancer$s20y[[3]][2:101,2:12])),2)/1E6
signif(range(colSums(cancer$s30y[[3]][2:101,2:12])),2)/1E6
signif(range(colSums(cancer$slife80take[[3]][2:101,2:12])),2)/1E6
signif(range(colSums(cancer$slife[[3]][2:101,2:12])),2)/1E6

signif(median(colSums(cancer$s20y[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(cancer$s30y[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(cancer$slife80take[[4]][2:101,2:12])),3)/1E6

summary((colSums(cancer$s30y[[4]][2:101,2:12]))/(colSums(cancer$slife[[4]][2:101,2:12])))
summary((colSums(cancer$slife80take[[4]][2:101,2:12]))/(colSums(cancer$slife[[4]][2:101,2:12])))


cancer_p10 = list(slife = list(),s20y = list(),s30y=list(),slife80take = list())

for(SCEN in 1:4)
{
  for(REG in 1:4)
  {
    df = data.frame(year = 2020:2209,matrix(NA, nrow=length(2020:2209),ncol = 11))
    df[,2:12] = getYvalues(protect = protect_p10,PROTECT = SCEN,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = TRUE)
    cancer_p10[[SCEN]][[REG]] = df
  }
}

cancer_p90 = list(slife = list(),s20y = list(),s30y=list(),slife80take = list())

for(SCEN in 1:4)
{
  for(REG in 1:4)
  {
    df = data.frame(year = 2020:2209,matrix(NA, nrow=length(2020:2209),ncol = 11))
    df[,2:12] = getYvalues(protect = protect_p90,PROTECT = SCEN,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = TRUE)
    cancer_p90[[SCEN]][[REG]] = df
  }
}

paste0( signif(median(colSums(cancer$s20y[[4]][2:101,2:12])),3)/1E6, 
        ' million (', signif(median(colSums(cancer_p10$s20y[[4]][2:101,2:12])),3)/1E6, '–',
        signif(median(colSums(cancer_p90$s20y[[4]][2:101,2:12])),3)/1E6,')')

paste0( signif(median(colSums(cancer$s30y[[4]][2:101,2:12])),3)/1E6, 
        ' million (', signif(median(colSums(cancer_p10$s30y[[4]][2:101,2:12])),3)/1E6, '–',
        signif(median(colSums(cancer_p90$s30y[[4]][2:101,2:12])),3)/1E6,')')

paste0( signif(median(colSums(cancer$slife80take[[4]][2:101,2:12])),3)/1E6, 
        ' million (', signif(median(colSums(cancer_p10$slife80take[[4]][2:101,2:12])),3)/1E6, '–',
        signif(median(colSums(cancer_p90$slife80take[[4]][2:101,2:12])),3)/1E6,')')

signif(median(colSums(cancer_p10$s30y[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(cancer$s30y[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(cancer_p90$s30y[[4]][2:101,2:12])),3)/1E6

signif(median(colSums(cancer_p10$slife80take[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(cancer$slife80take[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(cancer_p90$slife80take[[4]][2:101,2:12])),3)/1E6




cancer_dish_p10 = list(slife = list(),s20y = list(),s30y=list(),slife80take = list())

for(SCEN in 1:4)
{
  for(REG in 1:4)
  {
    df = data.frame(year = 2020:2209,matrix(NA, nrow=length(2020:2209),ncol = 11))
    df[,2:12] = getYvalues(protect = protect_p10,PROTECT = SCEN,VAR = 'cases_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = TRUE)
    cancer_dish_p10[[SCEN]][[REG]] = df
  }
}

cancer_dish_p90 = list(slife = list(),s20y = list(),s30y=list(),slife80take = list())

for(SCEN in 1:4)
{
  for(REG in 1:4)
  {
    df = data.frame(year = 2020:2209,matrix(NA, nrow=length(2020:2209),ncol = 11))
    df[,2:12] = getYvalues(protect = protect_p90,PROTECT = SCEN,VAR = 'cases_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = TRUE)
    cancer_dish_p90[[SCEN]][[REG]] = df
  }
}


signif(median(colSums(cancer_dish_p10$s20y[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(cancer_dish$s20y[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(cancer_dish_p90$s20y[[4]][2:101,2:12])),3)/1E6

signif(median(colSums(cancer_dish_p10$s30y[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(cancer_dish$s30y[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(cancer_dish_p90$s30y[[4]][2:101,2:12])),3)/1E6

signif(median(colSums(cancer_dish_p10$slife80take[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(cancer_dish$slife80take[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(cancer_dish_p90$slife80take[[4]][2:101,2:12])),3)/1E6



# LIC v.s. HIC 
load('data/population/popsize.rdata')
popsize
colSums(cancer_dish$s20y[[1]][2:101,2:12]/popsize$lic)/
colSums(cancer_dish$s20y[[3]][2:101,2:12]/popsize$hic)

colSums(cancer_dish$s30y[[1]][2:101,2:12]/popsize$lic)/
colSums(cancer_dish$s30y[[3]][2:101,2:12]/popsize$hic)

colSums(cancer_dish$slife80take[[1]][2:101,2:12]/popsize$lic)/
  colSums(cancer_dish$slife80take[[3]][2:101,2:12]/popsize$hic)


colSums(cancer$s20y[[1]][2:101,2:12]/popsize$lic)/
  colSums(cancer$s20y[[3]][2:101,2:12]/popsize$hic)

colSums(cancer$s30y[[1]][2:101,2:12]/popsize$lic)/
  colSums(cancer$s30y[[3]][2:101,2:12]/popsize$hic)

colSums(cancer$slife80take[[1]][2:101,2:12]/popsize$lic)/
  colSums(cancer$slife80take[[3]][2:101,2:12]/popsize$hic)


colSums(cancer_dish$s20y[[1]][2:101,2:12]/popsize$lic[1])/
  colSums(cancer_dish$s20y[[3]][2:101,2:12]/popsize$hic[1])

colSums(cancer_dish$s30y[[1]][2:101,2:12]/popsize$lic[1])/
  colSums(cancer_dish$s30y[[3]][2:101,2:12]/popsize$hic[1])

colSums(cancer_dish$slife80take[[1]][2:101,2:12]/popsize$lic[1])/
  colSums(cancer_dish$slife80take[[3]][2:101,2:12]/popsize$hic[1])



death = list(slife = list(),s20y = list(),s30y=list(),slife80take = list())

for(SCEN in 1:4)
{
  for(REG in 1:4)
  {
    df = data.frame(year = 2020:2209,matrix(NA, nrow=length(2020:2209),ncol = 11))
    df[,2:12] = getYvalues(protect = protect,PROTECT = SCEN,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = TRUE)
    death[[SCEN]][[REG]] = df
  }
}

death_p10 = list(slife = list(),s20y = list(),s30y=list(),slife80take = list())

for(SCEN in 1:4)
{
  for(REG in 1:4)
  {
    df = data.frame(year = 2020:2209,matrix(NA, nrow=length(2020:2209),ncol = 11))
    df[,2:12] = getYvalues(protect = protect_p10,PROTECT = SCEN,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = TRUE)
    death_p10[[SCEN]][[REG]] = df
  }
}

death_p90 = list(slife = list(),s20y = list(),s30y=list(),slife80take = list())

for(SCEN in 1:4)
{
  for(REG in 1:4)
  {
    df = data.frame(year = 2020:2209,matrix(NA, nrow=length(2020:2209),ncol = 11))
    df[,2:12] = getYvalues(protect = protect_p90,PROTECT = SCEN,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = TRUE)
    death_p90[[SCEN]][[REG]] = df
  }
}

signif(median(colSums(death_p10$s20y[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(death$s20y[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(death_p90$s20y[[4]][2:101,2:12])),3)/1E6

signif(median(colSums(death_p10$s30y[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(death$s30y[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(death_p90$s30y[[4]][2:101,2:12])),3)/1E6

signif(median(colSums(death_p10$slife80take[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(death$slife80take[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(death_p90$slife80take[[4]][2:101,2:12])),3)/1E6


death_dish = list(slife = list(),s20y = list(),s30y=list(),slife80take = list())

for(SCEN in 1:4)
{
  for(REG in 1:4)
  {
    df = data.frame(year = 2020:2209,matrix(NA, nrow=length(2020:2209),ncol = 11))
    df[,2:12] = getYvalues(protect = protect,PROTECT = SCEN,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)
    death_dish[[SCEN]][[REG]] = df
  }
}



death_dish_p10 = list(slife = list(),s20y = list(),s30y=list(),slife80take = list())

for(SCEN in 1:4)
{
  for(REG in 1:4)
  {
    df = data.frame(year = 2020:2209,matrix(NA, nrow=length(2020:2209),ncol = 11))
    df[,2:12] = getYvalues(protect = protect_p10,PROTECT = SCEN,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = TRUE)
    death_dish_p10[[SCEN]][[REG]] = df
  }
}

death_dish_p90 = list(slife = list(),s20y = list(),s30y=list(),slife80take = list())

for(SCEN in 1:4)
{
  for(REG in 1:4)
  {
    df = data.frame(year = 2020:2209,matrix(NA, nrow=length(2020:2209),ncol = 11))
    df[,2:12] = getYvalues(protect = protect_p90,PROTECT = SCEN,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = TRUE)
    death_dish_p90[[SCEN]][[REG]] = df
  }
}


signif(median(colSums(death_dish_p10$s20y[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(death_dish$s20y[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(death_dish_p90$s20y[[4]][2:101,2:12])),3)/1E6

signif(median(colSums(death_dish_p10$s30y[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(death_dish$s30y[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(death_dish_p90$s30y[[4]][2:101,2:12])),3)/1E6

signif(median(colSums(death_dish_p10$slife80take[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(death_dish$slife80take[[4]][2:101,2:12])),3)/1E6
signif(median(colSums(death_dish_p90$slife80take[[4]][2:101,2:12])),3)/1E6




colSums(death_dish$s20y[[1]][2:101,2:12]/popsize$lic)/
  colSums(death_dish$s20y[[3]][2:101,2:12]/popsize$hic)

colSums(death_dish$s30y[[1]][2:101,2:12]/popsize$lic)/
  colSums(death_dish$s30y[[3]][2:101,2:12]/popsize$hic)

colSums(death_dish$slife80take[[1]][2:101,2:12]/popsize$lic)/
  colSums(death_dish$slife80take[[3]][2:101,2:12]/popsize$hic)



colSums(death$s20y[[1]][2:101,2:12]/popsize$lic)/
  colSums(death$s20y[[3]][2:101,2:12]/popsize$hic)

colSums(death$s30y[[1]][2:101,2:12]/popsize$lic)/
  colSums(death$s30y[[3]][2:101,2:12]/popsize$hic)

colSums(death$slife80take[[1]][2:101,2:12]/popsize$lic)/
  colSums(death$slife80take[[3]][2:101,2:12]/popsize$hic)

