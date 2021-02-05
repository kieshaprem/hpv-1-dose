require(data.table)


# Routine programme for girls at 10 year olds and catch-up up to 14 year olds (5 year cohorts: 10, 11, 12, 13, and 14Y)
# 
# File names for the five scenarios:
# 1.	Lifelong: Lifelong protection
# 2.	Catchup: Lifelong protection with a catch-up campaign in the first year
# 3.	Lifelong_64: Lifelong protection with 80% take
# 4.	Duration_20Y: 20 year duration of vaccine protection
# 5.	Duration_30Y: 30 year duration of vaccine protection
# 
# I made five txt files consisting of total cervical cancer cases for 101 years. And each row presents one scenario among 100 parameter sets.
# 
# I ran 16, 18 and OHR for squamous and adenos for these scenarios and the results in the txt files are the added value of these 6 groups – total number of cancer cases.
# lifelong = read.delim("data/phe/LifeLong.txt",header = FALSE)
# lifelong_64 = read.delim("data/phe/LifeLong_64.txt",header = FALSE)
# catchup = read.delim("data/phe/Catchup.txt",header = FALSE)
# duration_20Y = read.delim("data/phe/Duration_20Y.txt",header = FALSE)
# duration_30Y = read.delim("data/phe/Duration_30Y.txt",header = FALSE)

# •	Scenario 0: status quo
# •	Scenario 1: lifetime protection, vaccine efficacy (VE): 100% ,  (lifelong/catchup/2 dose)
# i.e. similar to the current two-dose assumptions
# •	Scenario 2: one-dose offers 20 years protection, VE: 100%       (duration_20Y)
# •	Scenario 3: one-dose offers 30 years protection, VE: 100%       (duration_30Y)
# •	Scenario 4: one-dose offers lifetime protection, VE: 80%        (lifelong_64)
# protection against persistent infection at 5-year time point. 

readPHE = function(SCEN)
{
  DATALIST = list()
  file16 = paste0("Ad_",SCEN,"_16_", seq(0,99,1),".txt")
  file18 = paste0("Ad_",SCEN,"_18_", seq(0,99,1),".txt")
  fileOHR = paste0("Ad_",SCEN,"_OHR_", seq(0,99,1),".txt")
  
  # file16 = paste0("Sq_",SCEN,"_16_", seq(0,99,1),".txt")
  # file18 = paste0("Sq_",SCEN,"_18_", seq(0,99,1),".txt")
  # fileOHR = paste0("Sq_",SCEN,"_OHR_", seq(0,99,1),".txt")
  
  for(file in 1:50) 
  {
    DATALIST[[file]] = read.delim(paste0("../../../Documents/OneDoseResult/",file16[file]),header = TRUE)[,2:66] +
      read.delim(paste0("../../../Documents/OneDoseResult/",file18[file]),header = TRUE)[,2:66] +
      read.delim(paste0("../../../Documents/OneDoseResult/",fileOHR[file]),header = TRUE)[,2:66]
  }
  return(DATALIST)
}

lifelong = readPHE(SCEN = 'Life')
lifelong_64 = readPHE(SCEN = 'Life_64')
catchup = readPHE(SCEN = 'Catchup')
duration_20Y = readPHE(SCEN = '20Y')
duration_30Y = readPHE(SCEN = '30Y')

phe = list()
phe$strategy_life = lifelong
phe$strategy_20y = duration_20Y
phe$strategy_30y = duration_30Y
phe$strategy_life80take = lifelong_64
phe$strategy_catchup = catchup

rm(lifelong,lifelong_64,duration_20Y,duration_30Y,catchup)
save(phe,file = 'data/phe/phe.rdata')

# require(grid)
# require(ggsci)
# cols = pal_npg(palette = 'nrc')(5)
# 
#  
# grid.newpage()
# pushViewport(plotViewport(c(3,3,1,1),yscale=c(0,2000),xscale=c(0,100)))
# grid.rect()
# grid.xaxis(gp=gpar(fontsize = 8))
# grid.yaxis(gp=gpar(fontsize = 8))
# grid.text('Number of cervical cancer cases',x = unit(-3,'lines'),rot = 90,gp=gpar(fontsize = 9, fontface='bold'))
# grid.text('Vaccine year',y = unit(-2.5,'lines'),rot = 0,gp=gpar(fontsize = 9, fontface='bold'))
# for(scen in 1:4)
# {
#   yval1 = as.vector(apply(phe[[scen]],2,function(x) quantile(x, 0.25)))
#   yval2 = as.vector(apply(phe[[scen]],2,function(x) quantile(x, 0.75)))
#   grid.polygon(x = c(seq(0,100,1),rev(seq(0,100,1))), y = c(yval1,rev(yval2)), default.units = 'native',gp = gpar(col =NA,fill= cols[scen],alpha = 0.15))
# }
# for(scen in 1:4)
# {
#   yval = as.vector(apply(phe[[scen]],2,function(x) quantile(x, 0.50)))
#   grid.lines(x = seq(0,100,1),y = yval, default.units = 'native',gp = gpar(lwd= 2,col = cols[scen]))
# }
# 
# for(scen in 1:4)
# {
#   grid.polygon(x = c(0.77,0.79,0.79,0.77),y = c(0.95,0.95,0.92,0.92) - (scen-1)*0.05,
#                default.units = 'npc',gp = gpar(col =NA,fill= cols[scen],alpha = 0.15)) 
#   grid.lines(x = c(0.77,0.79),y = c(0.935,0.935) - (scen-1)*0.05,
#                default.units = 'npc',gp = gpar(col = cols[scen],lwd = 2)) 
#                 
# }
# grid.text(c('Lifelong protection','20 years protection', '30 years protection', 'Lifelong, 80% take'),x = 0.8,y=seq(0.935,0.5,-0.05)[1:4],
#           just = 'left',gp=gpar(fontsize = 8, fontface='plain'))
# 
# 
# DATA = read.delim(paste0("../../../Desktop/OneDoseResult/Ad_20Y_16_", seq(0,99,1),".txt")[1],header = TRUE)
# 
# list.files(path = "../../../Desktop/OneDoseResult/") %in% paste0("Ad_30Y_16_", seq(0,99,1),".txt")
# list.files(path = "../../../Desktop/OneDoseResult/") %in% paste0("Ad_20Y_16_", seq(0,99,1),".txt")
# list.files(path = "../../../Desktop/OneDoseResult/") %in% paste0("Ad_30Y_16_", seq(0,99,1),".txt")
# 
# filenames = list.files(path = "../../../Desktop/OneDoseResult/")
# filenames[1001:2000]







