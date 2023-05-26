source('code/2_dataPrep_hpvadvise_indirect_all.r')
load(file = 'data/population/incomegroup.rdata')
load('output/prime/results_prime_df_1931to2120.rdata')

save.image(file = '1-dose_batch/data_hpvadvise.rdata')
