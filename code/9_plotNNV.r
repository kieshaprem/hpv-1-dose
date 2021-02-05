library(grid)
library(ggsci)
PRESENTATION = TRUE
reds = seq(100,250,35)/255
blues = seq(105,255,35)/255

COLS =c('black', 
        rgb(reds[1],0,0),
        rgb(reds[2],0,0),
        rgb(reds[3],0,0),
        rgb(reds[4],0,0),
        rgb(reds[5],0,0),
        rgb(0/255, 90/255, blues[1]),
        rgb(0/255, 90/255, blues[2]),
        rgb(0/255, 90/255, blues[3]),
        rgb(0/255, 90/255, blues[4]),
        rgb(0/255, 90/255, blues[5]))  #c('navy',pal_jama()(7))
load('output/nnvA.rdata')
load('output/nnvB.rdata')
load('output/nnvC.rdata')
load('output/nnvD.rdata')
load('output/nnvE.rdata')
load('output/nnvF.rdata')

load('output/nnvdishA.rdata')
load('output/nnvdishB.rdata')
load('output/nnvdishC.rdata')
load('output/nnvdishD.rdata')
load('output/nnvdishE.rdata')
load('output/nnvdishF.rdata')


REGIONNAMES = list("Low income",
                   c("Lower middle income","Upper middle income"),
                   "High income",
                   c("Low income","Lower middle income","Upper middle income","High income"))
REGIONLABEL = c('Low-income countries',
                'Middle-income countries',
                'High-income countries',
                'World')




pow = 7
YMAX = list(10^pow,10^pow,10^pow,10^pow)#28000,28000,28000,28000)
YAXIS = list(seq(1,7,1),
             seq(1,7,1),
             seq(1,7,1),
             seq(1,7,1))
YAXISNAME =  list(c(10,100,'1k','10k','100k','1M','10M'),
                  c(10,100,'1k','10k','100k','1M','10M'),
                  c(10,100,'1k','10k','100k','1M','10M'),
                  c(10,100,'1k','10k','100k','1M','10M'))
XAXISNAME =  c(expression(paste("0" %->% 1['20y'])),
               expression(paste("0" %->% 1['30y'])),
               expression(paste("0" %->% 1['VE:80%'])),
               expression(paste(1['20y'] %->% 2["life"])),
               expression(paste(1['30y'] %->% 2["life"])),
               expression(paste(1['VE:80%'] %->% 2["life"])))

png('output/plots/fig_nnv.png',width = 20,height = 23.5,units = 'cm',res = 700)
if(1)
{
  grid.newpage()
  pushViewport(plotViewport(c(3,4,1,0)))
  pushViewport(viewport(layout=grid.layout(nrow=4,ncol=2,width=rep(1,4),height=rep(1,4))))
  
  # grid.text(expression(paste(Delta," #dose"['(protection)'])),y=unit(-0,'lines'),rot=0,gp=gpar(fontface='bold',fontsize=11))
  grid.text(expression(paste("Change in number of vaccine doses"['(duration/extent of protection)'])),y=unit(-2.75,'lines'),rot=0,gp=gpar(fontface='bold',fontsize=11))
  
  for (REG in 1:4) 
  {
    pushViewport(viewport(layout.pos.row=REG,layout.pos.col=1))
    pushViewport(plotViewport(c(1,2,1,1),yscale=c(1,log10(YMAX[[REG]])),xscale=c(0.5,6.5)))
    grid.polygon(x=c(0.5,3.5,3.5,0.5), y=c(1,1,log10(YMAX[[REG]]),log10(YMAX[[REG]])),default.units = 'native',gp=gpar(fill='grey95',col=NA))
    grid.rect(gp=gpar(fill=NA))
    
    if(REG==1)
    {
      ypos = c(seq(0.85,0.15,-0.05), seq(0.60,0.5,-0.05))
      LEGENDNAMES = c('PHE: UK',paste0('HPV-ADVISE: ',c("India",'Nigeria','Uganda','Vietnam','Canada')),paste0('Harvard: ',c('India','Uganda','El Salvador','Nicaragua','US')),
                      '2 doses lifetime protection','1 dose 10y protection','1 dose 20y protection')   
      for(model in 1:11) grid.points(x=c(0.03),y=ypos[model],default.units = 'npc',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      for(model in 1:11) grid.text(LEGENDNAMES[model],x=0.05,y=ypos[model],default.units = 'npc',just = 'left',gp=gpar(col='grey40',fontsize=7))
      
    }
    if(REG==4)
    {
      grid.text(expression(paste("0" %->% "1"['20y / 30y / VE:80%'])),y=unit(-2.5,'lines'),x=0.25,default.units = 'npc',rot=0,gp=gpar(fontsize=9,col='black'))
      grid.text(expression(paste("1"['20 / 30y / VE:80%'] %->% "2"['life'])),y=unit(-2.5,'lines'),x=0.75,default.units = 'npc',rot=0,gp=gpar(fontsize=9,col='black'))
    }
    
    grid.xaxis(at = seq(1,6,1),label = c(XAXISNAME),gp=gpar(fontsize=6))
    grid.yaxis(at = YAXIS[[REG]],label = YAXISNAME[[REG]],gp=gpar(fontsize=8))
    grid.text(REGIONLABEL[REG],x=unit(-5.5,'lines'),rot=90,gp=gpar(fontsize=11,fontface='bold'))
    grid.text(paste0(LETTERS[REG]),y=unit(1,'npc')-unit(0.75,'lines'),x=unit(0.035,'npc'),gp=gpar(fontsize=8,fontface='bold'))
    grid.text("Number needed to give +1 dose\nto avert 1 cervical cancer case",x=unit(-4.85,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=9))
    grid.text(expression(paste("(",log[10]," scale)")),x=unit(-3.65,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=8))
    if(REG == 1)  grid.text("Benefits discounted at 0%",y=unit(1.2,'lines')+unit(1,'npc'),gp=gpar(fontface='bold',fontsize=11))
    
    for(model in 1:11) 
    {
      if(model<12) grid.lines(x=0.6+0.085*(model-1),y=log10(c(nnvA[[model]][2,REG],nnvA[[model]][5,REG])),default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=0.6+0.085*(model-1),y=log10(c(nnvA[[model]][1,REG])),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<12) grid.lines(x=1.6+0.085*(model-1),y=log10(c(nnvB[[model]][2,REG],nnvB[[model]][5,REG])),default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=1.6+0.085*(model-1),y=log10(c(nnvB[[model]][1,REG])),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<12) grid.lines(x=2.6+0.085*(model-1),y=log10(c(nnvC[[model]][2,REG],ifelse(nnvC[[model]][5,REG]>YMAX[[REG]],YMAX[[REG]],nnvC[[model]][5,REG]))),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=2.6+0.085*(model-1),y=log10(c(nnvC[[model]][1,REG])),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      # if(nnvC[[model]][5,REG]>YMAX[[REG]]) 
      if(model<12) grid.lines(x=3.6+0.085*(model-1),y=log10(c(nnvD[[model]][2,REG],ifelse(nnvD[[model]][5,REG]>YMAX[[REG]],YMAX[[REG]],nnvD[[model]][5,REG]))),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=3.6+0.085*(model-1),y=log10(c(nnvD[[model]][1,REG])),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<12) grid.lines(x=4.6+0.085*(model-1),y=log10(c(nnvE[[model]][2,REG],ifelse(nnvE[[model]][5,REG]>YMAX[[REG]],YMAX[[REG]],nnvE[[model]][5,REG]))),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=4.6+0.085*(model-1),y=log10(c(nnvE[[model]][1,REG])),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      # if(nnvC[[model]][5,REG]>YMAX[[REG]]) 
      if(model<12) grid.lines(x=5.6+0.085*(model-1),y=log10(c(nnvF[[model]][2,REG],ifelse(nnvF[[model]][5,REG]>YMAX[[REG]],YMAX[[REG]],nnvF[[model]][5,REG]))),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=5.6+0.085*(model-1),y=log10(c(nnvF[[model]][1,REG])),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      # yvals = getYvalues(protect = protect,PROTECT = 1,VAR = 'cases_averted_all_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = TRUE)[,model]
      # grid.lines(x=xvals,y=yvals,default.units = 'native',gp = gpar(lwd=1,col='black',lty = model))
      # 
      # yvals = getYvalues(protect = protect,PROTECT = 2,VAR = 'cases_averted_all_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = TRUE)[,model]
      # grid.lines(x=xvals,y=yvals,default.units = 'native',gp = gpar(lwd=1,col='darkred',lty = model))
    }
    popViewport()
    popViewport()
    
  }
  
  for (REG in 1:4) 
  {
    pushViewport(viewport(layout.pos.row=REG,layout.pos.col=2))
    pushViewport(plotViewport(c(1,2,1,1),yscale=c(1,log10(YMAX[[REG]])),xscale=c(0.5,6.5)))
    grid.polygon(x=c(0.5,3.5,3.5,0.5), y=c(1,1,log10(YMAX[[REG]]),log10(YMAX[[REG]])),default.units = 'native',gp=gpar(fill='grey95',col=NA))
    if(REG==4)
    {
      grid.text(expression(paste("0" %->% "1"['20y / 30y / VE:80%'])),y=unit(-2.5,'lines'),x=0.25,default.units = 'npc',rot=0,gp=gpar(fontsize=9,col='black'))
      grid.text(expression(paste("1"['20 / 30y / VE:80%'] %->% "2"['life'])),y=unit(-2.5,'lines'),x=0.75,default.units = 'npc',rot=0,gp=gpar(fontsize=9,col='black'))
    }
    grid.rect(gp=gpar(fill=NA))
    
    grid.xaxis(at = seq(1,6,1),label = c(XAXISNAME),gp=gpar(fontsize=6))
    grid.yaxis(at = YAXIS[[REG]],label = YAXISNAME[[REG]],gp=gpar(fontsize=8))
    grid.text(paste0(LETTERS[REG+4]),y=unit(1,'npc')-unit(0.75,'lines'),x=unit(0.035,'npc'),gp=gpar(fontsize=8,fontface='bold'))
    # if(REG == 1)  grid.text("Cervical cancers averted ('000)\n(10y protection from 1 dose)",x=unit(-4,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=11))
    # if(REG == 1)  grid.text("Number needed to give +1 dose\nto avert 1 cervical cancer case",x=unit(-4.85,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=11))
    # if(REG == 1)  grid.text(expression(paste("(",log[10]," scale)")),x=unit(-3.10,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=9))
    if(REG == 1)  grid.text("Benefits discounted at 3%",y=unit(1.2,'lines')+unit(1,'npc'),gp=gpar(fontface='bold',fontsize=11))
    # # grid.text(expression(paste(""%*%"10"^"3")),x=unit(-3,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=9))
    # xvals = seq(2020,2080,1)
    for(model in 1:11) 
    {
      if(model<12) grid.lines(x=0.6+0.085*(model-1),y=log10(c(nnvdishA[[model]][2,REG],nnvdishA[[model]][5,REG])),default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=0.6+0.085*(model-1),y=log10(c(nnvdishA[[model]][1,REG])),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<12) grid.lines(x=1.6+0.085*(model-1),y=log10(c(nnvdishB[[model]][2,REG],nnvdishB[[model]][5,REG])),default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=1.6+0.085*(model-1),y=log10(c(nnvdishB[[model]][1,REG])),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<12) grid.lines(x=2.6+0.085*(model-1),y=log10(c(nnvdishC[[model]][2,REG],ifelse(nnvdishC[[model]][5,REG]>YMAX[[REG]],YMAX[[REG]],nnvdishC[[model]][5,REG]))),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=2.6+0.085*(model-1),y=log10(c(nnvdishC[[model]][1,REG])),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      # if(nnvdishC[[model]][5,REG]>YMAX[[REG]]) 
      if(model<12) grid.lines(x=3.6+0.085*(model-1),y=log10(c(nnvdishD[[model]][2,REG],ifelse(nnvdishD[[model]][5,REG]>YMAX[[REG]],YMAX[[REG]],nnvdishD[[model]][5,REG]))),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=3.6+0.085*(model-1),y=log10(c(nnvdishD[[model]][1,REG])),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<12) grid.lines(x=4.6+0.085*(model-1),y=log10(c(nnvdishE[[model]][2,REG],ifelse(nnvdishE[[model]][5,REG]>YMAX[[REG]],YMAX[[REG]],nnvdishE[[model]][5,REG]))),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=4.6+0.085*(model-1),y=log10(c(nnvdishE[[model]][1,REG])),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      # if(nnvdishC[[model]][5,REG]>YMAX[[REG]]) 
      if(model<12) grid.lines(x=5.6+0.085*(model-1),y=log10(c(nnvdishF[[model]][2,REG],ifelse(nnvdishF[[model]][5,REG]>YMAX[[REG]],YMAX[[REG]],nnvdishF[[model]][5,REG]))),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=5.6+0.085*(model-1),y=log10(c(nnvdishF[[model]][1,REG])),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      # yvals = getYvalues(protect = protect,PROTECT = 1,VAR = 'cases_averted_all_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = TRUE)[,model]
      # grid.lines(x=xvals,y=yvals,default.units = 'native',gp = gpar(lwd=1,col='black',lty = model))
      # 
      # yvals = getYvalues(protect = protect,PROTECT = 2,VAR = 'cases_averted_all_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = TRUE)[,model]
      # grid.lines(x=xvals,y=yvals,default.units = 'native',gp = gpar(lwd=1,col='darkred',lty = model))
    }
    popViewport()
    popViewport()
    
  }
  popViewport()
  popViewport()
}
dev.off()



getOverall = function(RESULTS,REG = 4)
{
  n = 11
  med = lci = uci = array(NA,n)
  for(model in 1:11) 
  {
    med[model] = RESULTS[[model]][1,REG]
    lci[model] = RESULTS[[model]][2,REG]
    uci[model] = RESULTS[[model]][5,REG]
  }
  return(paste0( round(median(med),0),' (',round(median(lci),0),'–',round(median(uci),0), ')'))
}

getOverall(RESULTS = nnvA)
getOverall(RESULTS = nnvA,REG = 1)
getOverall(RESULTS = nnvA,REG = 2)
getOverall(RESULTS = nnvA,REG = 3)

getOverall(RESULTS = nnvdishA)
getOverall(RESULTS = nnvdishA,REG = 1)
getOverall(RESULTS = nnvdishA,REG = 3)

getOverall(RESULTS = nnvD)
getOverall(RESULTS = nnvD,REG = 1)
getOverall(RESULTS = nnvD,REG = 2)
getOverall(RESULTS = nnvD,REG = 3)

getOverall(RESULTS = nnvE)
getOverall(RESULTS = nnvE,REG = 1)
getOverall(RESULTS = nnvE,REG = 3)

getOverall(RESULTS = nnvF)
getOverall(RESULTS = nnvF,REG = 1)
getOverall(RESULTS = nnvF,REG = 3)



# > getOverall(RESULTS = nnvD)
# [1] "755 (327–2125)"
# > getOverall(RESULTS = nnvE)
# [1] "1633 (658–4910)"
# > getOverall(RESULTS = nnvF)
# [1] "1884 (837–5234)"
# Abstract
# 330 - 5230
