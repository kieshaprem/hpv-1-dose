load(file = 'data/population/incomegroup.rdata')
load('output/prime/results_prime_df_1931to2120.rdata')
source('code/2_dataPrep_phe_indirect_all.r')

save.image(file = '1-dose_batch/data_phe.rdata')
