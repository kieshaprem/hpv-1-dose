

incomegroup = read.csv('data/population/income_group.csv', as.is = TRUE)


incomegroup = data.frame(iso3c = incomegroup$iso3c,incomegroup = incomegroup$X2019)
incomegroup$incomegroup = as.character(incomegroup$incomegroup)

incomegroup$incomegroup[incomegroup$incomegroup %in% "L"] = "Low income"
incomegroup$incomegroup[incomegroup$incomegroup %in% "LM"] = "Lower middle income"
incomegroup$incomegroup[incomegroup$incomegroup %in% "UM"] = "Upper middle income"
incomegroup$incomegroup[incomegroup$incomegroup %in% "H"] = "High income"

save(incomegroup,file = 'data/population/incomegroup.rdata')
table(incomegroup$incomegroup)
