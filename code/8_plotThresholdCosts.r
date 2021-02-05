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

REGIONNAMES = list("Low income",
                   c("Lower middle income","Upper middle income"),
                   "High income",
                   c("Low income","Lower middle income","Upper middle income","High income"))
REGIONLABEL = c('Low-income countries',
                'Middle-income countries',
                'High-income countries',
                'World')

XAXISNAME =  c(expression(paste("0" %->% 1['20y'])),
               expression(paste("0" %->% 1['30y'])),
               expression(paste("0" %->% 1['VE:80%'])),
               expression(paste(1['20y'] %->% 2["life"])),
               expression(paste(1['30y'] %->% 2["life"])),
               expression(paste(1['VE:80%'] %->% 2["life"])))

load('output/costs/thresholdA.rdata')
load('output/costs/thresholdB.rdata')
load('output/costs/thresholdC.rdata')
load('output/costs/thresholdD.rdata')
load('output/costs/thresholdE.rdata')
load('output/costs/thresholdF.rdata')

load('output/costs/thresholdAcet.rdata')
load('output/costs/thresholdBcet.rdata')
load('output/costs/thresholdCcet.rdata')
load('output/costs/thresholdDcet.rdata')
load('output/costs/thresholdEcet.rdata')
load('output/costs/thresholdFcet.rdata')


YMAX = list(100,100,100,100)
YAXIS = list(seq(0,100,20),
             seq(0,100,20),
             seq(0,100,20),
             seq(0,100,20))
YAXISNAME =  list(seq(0,100,20),
                  seq(0,100,20),
                  seq(0,100,20),
                  seq(0,100,20))

png('output/plots/fig_thresholdcost_disc_dish_full.png',width = 20,height = 23.5,units = 'cm',res = 700)
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
    pushViewport(plotViewport(c(1,2,1,1),yscale=c(0,(YMAX[[REG]])),xscale=c(0.5,6.5)))
    grid.polygon(x=c(0.5,3.5,3.5,0.5), y=c(1,1,(YMAX[[REG]]),(YMAX[[REG]])),default.units = 'native',gp=gpar(fill='grey95',col=NA))
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
    # grid.text("",x=unit(-2.5,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=11))
    grid.text("Threshold costs for +1 dose\n(2017 US dollar)",x=unit(-3.5,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=11))
    if(REG == 1)  grid.text("Threshold of 1 GDP per capita",y=unit(1.2,'lines')+unit(1,'npc'),gp=gpar(fontface='bold',fontsize=11))
    
    

    # grid.text(expression(paste(""%*%"10"^"3")),x=unit(-3,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=9))
    # xvals = seq(2020,2080,1)
    for(model in 1:11) 
    {
      if(model<12) grid.lines(x=0.6+0.085*(model-1),y=c(thresholdA[[model]][3,REG],thresholdA[[model]][4,REG]),default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=0.6+0.085*(model-1),y=c(thresholdA[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<12) grid.lines(x=1.6+0.085*(model-1),y=c(thresholdB[[model]][3,REG],thresholdB[[model]][4,REG]),default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=1.6+0.085*(model-1),y=c(thresholdB[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<12) grid.lines(x=2.6+0.085*(model-1),y=c(thresholdC[[model]][3,REG],ifelse(thresholdC[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdC[[model]][4,REG])),
                             default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=2.6+0.085*(model-1),y=c(thresholdC[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      # if(thresholdC[[model]][4,REG]>YMAX[[REG]]) 
      if(model<12) grid.lines(x=3.6+0.085*(model-1),y=c(thresholdD[[model]][3,REG],ifelse(thresholdD[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdD[[model]][4,REG])),
                             default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=3.6+0.085*(model-1),y=c(thresholdD[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
  
      if(model<12) grid.lines(x=4.6+0.085*(model-1),y=c(thresholdE[[model]][3,REG],ifelse(thresholdE[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdE[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=4.6+0.085*(model-1),y=c(thresholdE[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      
      if(model<12) grid.lines(x=5.6+0.085*(model-1),y=c(thresholdF[[model]][3,REG],ifelse(thresholdF[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdF[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=5.6+0.085*(model-1),y=c(thresholdF[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
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
    pushViewport(plotViewport(c(1,2,1,1),yscale=c(-0,YMAX[[REG]]),xscale=c(0.5,6.5)))
    grid.polygon(x=c(0.5,3.5,3.5,0.5), y=c(0,0,YMAX[[REG]],YMAX[[REG]]),default.units = 'native',gp=gpar(fill='grey95',col=NA))
    
    
    if(REG==4)
    {
      grid.text(expression(paste("0" %->% "1"['20y / 30y / VE:80%'])),y=unit(-2.5,'lines'),x=0.25,default.units = 'npc',rot=0,gp=gpar(fontsize=9,col='black'))
      grid.text(expression(paste("1"['20 / 30y / VE:80%'] %->% "2"['life'])),y=unit(-2.5,'lines'),x=0.75,default.units = 'npc',rot=0,gp=gpar(fontsize=9,col='black'))
    }
    grid.rect(gp=gpar(fill=NA))
    
  
    grid.xaxis(at = seq(1,6,1),label = c(XAXISNAME),gp=gpar(fontsize=6))
    grid.yaxis(at = YAXIS[[REG]],label = YAXISNAME[[REG]],gp=gpar(fontsize=8))
    # grid.text(REGIONLABEL[REG],x=unit(-5.5,'lines'),rot=90,gp=gpar(fontsize=11,fontface='bold'))
    grid.text(paste0(LETTERS[REG+4]),y=unit(1,'npc')-unit(0.75,'lines'),x=unit(0.035,'npc'),gp=gpar(fontsize=8,fontface='bold'))
    # grid.text("",x=unit(-2.5,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=11))
    # grid.text("Threshold costs for +1 dose\n(2017 US dollar)",x=unit(-3.5,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=11))
    if(REG == 1)  grid.text("Threshold of < 1 GDP per capita",y=unit(1.2,'lines')+unit(1,'npc'),gp=gpar(fontface='bold',fontsize=11))
    
    
    # grid.text(expression(paste(""%*%"10"^"3")),x=unit(-3,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=9))
    # xvals = seq(2020,2080,1)
    for(model in 1:11) 
    {
      if(model<12) grid.lines(x=0.6+0.085*(model-1),y=c(thresholdAcet[[model]][3,REG],thresholdAcet[[model]][4,REG]),default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=0.6+0.085*(model-1),y=c(thresholdAcet[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<12) grid.lines(x=1.6+0.085*(model-1),y=c(thresholdBcet[[model]][3,REG],thresholdBcet[[model]][4,REG]),default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=1.6+0.085*(model-1),y=c(thresholdBcet[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<12) grid.lines(x=2.6+0.085*(model-1),y=c(thresholdCcet[[model]][3,REG],ifelse(thresholdCcet[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdCcet[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=2.6+0.085*(model-1),y=c(thresholdCcet[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      # if(thresholdCcet[[model]][4,REG]>YMAX[[REG]]) 
      if(model<12) grid.lines(x=3.6+0.085*(model-1),y=c(thresholdDcet[[model]][3,REG],ifelse(thresholdDcet[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdDcet[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=3.6+0.085*(model-1),y=c(thresholdDcet[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      
      if(model<12) grid.lines(x=4.6+0.085*(model-1),y=c(thresholdEcet[[model]][3,REG],ifelse(thresholdEcet[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdEcet[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=4.6+0.085*(model-1),y=c(thresholdEcet[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      
      if(model<12) grid.lines(x=5.6+0.085*(model-1),y=c(thresholdFcet[[model]][3,REG],ifelse(thresholdFcet[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdFcet[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=5.6+0.085*(model-1),y=c(thresholdFcet[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
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



load('output/costs/thresholdA_undis.rdata')
load('output/costs/thresholdB_undis.rdata')
load('output/costs/thresholdC_undis.rdata')
load('output/costs/thresholdD_undis.rdata')
load('output/costs/thresholdE_undis.rdata')
load('output/costs/thresholdF_undis.rdata')

load('output/costs/thresholdAcet_undis.rdata')
load('output/costs/thresholdBcet_undis.rdata')
load('output/costs/thresholdCcet_undis.rdata')
load('output/costs/thresholdDcet_undis.rdata')
load('output/costs/thresholdEcet_undis.rdata')
load('output/costs/thresholdFcet_undis.rdata')

YMAX = list(100,100,100,100)
YAXIS = list(seq(0,100,20),
             seq(0,100,20),
             seq(0,100,20),
             seq(0,100,20))
YAXISNAME =  list(seq(0,100,20),
                  seq(0,100,20),
                  seq(0,100,20),
                  seq(0,100,20))

png('output/plots/fig_thresholdcost_full.png',width = 20,height = 23.5,units = 'cm',res = 700)
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
    pushViewport(plotViewport(c(1,2,1,1),yscale=c(0,(YMAX[[REG]])),xscale=c(0.5,6.5)))
    grid.polygon(x=c(0.5,3.5,3.5,0.5), y=c(1,1,(YMAX[[REG]]),(YMAX[[REG]])),default.units = 'native',gp=gpar(fill='grey95',col=NA))
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
    # grid.text("",x=unit(-2.5,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=11))
    grid.text("Threshold costs for +1 dose\n(2017 US dollar)",x=unit(-3.5,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=11))
    if(REG == 1)  grid.text("Threshold of 1 GDP per capita",y=unit(1.2,'lines')+unit(1,'npc'),gp=gpar(fontface='bold',fontsize=11))
    
    
    
    # grid.text(expression(paste(""%*%"10"^"3")),x=unit(-3,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=9))
    # xvals = seq(2020,2080,1)
    for(model in 1:11) 
    {
      if(model<12) grid.lines(x=0.6+0.085*(model-1),y=c(thresholdA[[model]][3,REG],thresholdA[[model]][4,REG]),default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=0.6+0.085*(model-1),y=c(thresholdA[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<12) grid.lines(x=1.6+0.085*(model-1),y=c(thresholdB[[model]][3,REG],thresholdB[[model]][4,REG]),default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=1.6+0.085*(model-1),y=c(thresholdB[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<12) grid.lines(x=2.6+0.085*(model-1),y=c(thresholdC[[model]][3,REG],ifelse(thresholdC[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdC[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=2.6+0.085*(model-1),y=c(thresholdC[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      # if(thresholdC[[model]][4,REG]>YMAX[[REG]]) 
      if(model<12) grid.lines(x=3.6+0.085*(model-1),y=c(thresholdD[[model]][3,REG],ifelse(thresholdD[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdD[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=3.6+0.085*(model-1),y=c(thresholdD[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      
      if(model<12) grid.lines(x=4.6+0.085*(model-1),y=c(thresholdE[[model]][3,REG],ifelse(thresholdE[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdE[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=4.6+0.085*(model-1),y=c(thresholdE[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      
      if(model<12) grid.lines(x=5.6+0.085*(model-1),y=c(thresholdF[[model]][3,REG],ifelse(thresholdF[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdF[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=5.6+0.085*(model-1),y=c(thresholdF[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
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
    pushViewport(plotViewport(c(1,2,1,1),yscale=c(-0,YMAX[[REG]]),xscale=c(0.5,6.5)))
    grid.polygon(x=c(0.5,3.5,3.5,0.5), y=c(0,0,YMAX[[REG]],YMAX[[REG]]),default.units = 'native',gp=gpar(fill='grey95',col=NA))
    
    
    if(REG==4)
    {
      grid.text(expression(paste("0" %->% "1"['20y / 30y / VE:80%'])),y=unit(-2.5,'lines'),x=0.25,default.units = 'npc',rot=0,gp=gpar(fontsize=9,col='black'))
      grid.text(expression(paste("1"['20 / 30y / VE:80%'] %->% "2"['life'])),y=unit(-2.5,'lines'),x=0.75,default.units = 'npc',rot=0,gp=gpar(fontsize=9,col='black'))
    }
    grid.rect(gp=gpar(fill=NA))
    
    
    grid.xaxis(at = seq(1,6,1),label = c(XAXISNAME),gp=gpar(fontsize=6))
    grid.yaxis(at = YAXIS[[REG]],label = YAXISNAME[[REG]],gp=gpar(fontsize=8))
    # grid.text(REGIONLABEL[REG],x=unit(-5.5,'lines'),rot=90,gp=gpar(fontsize=11,fontface='bold'))
    grid.text(paste0(LETTERS[REG+4]),y=unit(1,'npc')-unit(0.75,'lines'),x=unit(0.035,'npc'),gp=gpar(fontsize=8,fontface='bold'))
    # grid.text("",x=unit(-2.5,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=11))
    # grid.text("Threshold costs for +1 dose\n(2017 US dollar)",x=unit(-3.5,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=11))
    if(REG == 1)  grid.text("Threshold of < 1 GDP per capita",y=unit(1.2,'lines')+unit(1,'npc'),gp=gpar(fontface='bold',fontsize=11))
    
    
    # grid.text(expression(paste(""%*%"10"^"3")),x=unit(-3,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=9))
    # xvals = seq(2020,2080,1)
    for(model in 1:11) 
    {
      if(model<12) grid.lines(x=0.6+0.085*(model-1),y=c(thresholdAcet[[model]][3,REG],thresholdAcet[[model]][4,REG]),default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=0.6+0.085*(model-1),y=c(thresholdAcet[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<12) grid.lines(x=1.6+0.085*(model-1),y=c(thresholdBcet[[model]][3,REG],thresholdBcet[[model]][4,REG]),default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=1.6+0.085*(model-1),y=c(thresholdBcet[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<12) grid.lines(x=2.6+0.085*(model-1),y=c(thresholdCcet[[model]][3,REG],ifelse(thresholdCcet[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdCcet[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=2.6+0.085*(model-1),y=c(thresholdCcet[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      # if(thresholdCcet[[model]][4,REG]>YMAX[[REG]]) 
      if(model<12) grid.lines(x=3.6+0.085*(model-1),y=c(thresholdDcet[[model]][3,REG],ifelse(thresholdDcet[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdDcet[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=3.6+0.085*(model-1),y=c(thresholdDcet[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      
      if(model<12) grid.lines(x=4.6+0.085*(model-1),y=c(thresholdEcet[[model]][3,REG],ifelse(thresholdEcet[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdEcet[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=4.6+0.085*(model-1),y=c(thresholdEcet[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      
      if(model<12) grid.lines(x=5.6+0.085*(model-1),y=c(thresholdFcet[[model]][3,REG],ifelse(thresholdFcet[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdFcet[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=5.6+0.085*(model-1),y=c(thresholdFcet[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
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


load('output/costs/thresholdA_disc.rdata')
load('output/costs/thresholdB_disc.rdata')
load('output/costs/thresholdC_disc.rdata')
load('output/costs/thresholdD_disc.rdata')
load('output/costs/thresholdE_disc.rdata')
load('output/costs/thresholdF_disc.rdata')

load('output/costs/thresholdAcet_disc.rdata')
load('output/costs/thresholdBcet_disc.rdata')
load('output/costs/thresholdCcet_disc.rdata')
load('output/costs/thresholdDcet_disc.rdata')
load('output/costs/thresholdEcet_disc.rdata')
load('output/costs/thresholdFcet_disc.rdata')

YMAX = list(700,700,700,700)
YAXIS = list(seq(0,700,100),
             seq(0,700,100),
             seq(0,700,100),
             seq(0,700,100))
YAXISNAME =  list(seq(0,700,100),
                  seq(0,700,100),
                  seq(0,700,100),
                  seq(0,700,100))

png('output/plots/fig_thresholdcost_disc_full.png',width = 20,height = 23.5,units = 'cm',res = 700)
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
    pushViewport(plotViewport(c(1,2,1,1),yscale=c(0,(YMAX[[REG]])),xscale=c(0.5,6.5)))
    grid.polygon(x=c(0.5,3.5,3.5,0.5), y=c(1,1,(YMAX[[REG]]),(YMAX[[REG]])),default.units = 'native',gp=gpar(fill='grey95',col=NA))
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
    # grid.text("",x=unit(-2.5,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=11))
    grid.text("Threshold costs for +1 dose\n(2017 US dollar)",x=unit(-3.5,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=11))
    if(REG == 1)  grid.text("Threshold of 1 GDP per capita",y=unit(1.2,'lines')+unit(1,'npc'),gp=gpar(fontface='bold',fontsize=11))
    
    
    
    # grid.text(expression(paste(""%*%"10"^"3")),x=unit(-3,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=9))
    # xvals = seq(2020,2080,1)
    for(model in 1:11) 
    {
      if(model<12) grid.lines(x=0.6+0.085*(model-1),y=c(thresholdA[[model]][3,REG],thresholdA[[model]][4,REG]),default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=0.6+0.085*(model-1),y=c(thresholdA[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<12) grid.lines(x=1.6+0.085*(model-1),y=c(thresholdB[[model]][3,REG],thresholdB[[model]][4,REG]),default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=1.6+0.085*(model-1),y=c(thresholdB[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<12) grid.lines(x=2.6+0.085*(model-1),y=c(thresholdC[[model]][3,REG],ifelse(thresholdC[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdC[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=2.6+0.085*(model-1),y=c(thresholdC[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      # if(thresholdC[[model]][4,REG]>YMAX[[REG]]) 
      if(model<12) grid.lines(x=3.6+0.085*(model-1),y=c(thresholdD[[model]][3,REG],ifelse(thresholdD[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdD[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=3.6+0.085*(model-1),y=c(thresholdD[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      
      if(model<12) grid.lines(x=4.6+0.085*(model-1),y=c(thresholdE[[model]][3,REG],ifelse(thresholdE[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdE[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=4.6+0.085*(model-1),y=c(thresholdE[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      
      if(model<12) grid.lines(x=5.6+0.085*(model-1),y=c(thresholdF[[model]][3,REG],ifelse(thresholdF[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdF[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=5.6+0.085*(model-1),y=c(thresholdF[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
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
    pushViewport(plotViewport(c(1,2,1,1),yscale=c(-0,YMAX[[REG]]),xscale=c(0.5,6.5)))
    grid.polygon(x=c(0.5,3.5,3.5,0.5), y=c(0,0,YMAX[[REG]],YMAX[[REG]]),default.units = 'native',gp=gpar(fill='grey95',col=NA))
    
    
    if(REG==4)
    {
      grid.text(expression(paste("0" %->% "1"['20y / 30y / VE:80%'])),y=unit(-2.5,'lines'),x=0.25,default.units = 'npc',rot=0,gp=gpar(fontsize=9,col='black'))
      grid.text(expression(paste("1"['20 / 30y / VE:80%'] %->% "2"['life'])),y=unit(-2.5,'lines'),x=0.75,default.units = 'npc',rot=0,gp=gpar(fontsize=9,col='black'))
    }
    grid.rect(gp=gpar(fill=NA))
    
    
    grid.xaxis(at = seq(1,6,1),label = c(XAXISNAME),gp=gpar(fontsize=6))
    grid.yaxis(at = YAXIS[[REG]],label = YAXISNAME[[REG]],gp=gpar(fontsize=8))
    # grid.text(REGIONLABEL[REG],x=unit(-5.5,'lines'),rot=90,gp=gpar(fontsize=11,fontface='bold'))
    grid.text(paste0(LETTERS[REG+4]),y=unit(1,'npc')-unit(0.75,'lines'),x=unit(0.035,'npc'),gp=gpar(fontsize=8,fontface='bold'))
    # grid.text("",x=unit(-2.5,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=11))
    # grid.text("Threshold costs for +1 dose\n(2017 US dollar)",x=unit(-3.5,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=11))
    if(REG == 1)  grid.text("Threshold of < 1 GDP per capita",y=unit(1.2,'lines')+unit(1,'npc'),gp=gpar(fontface='bold',fontsize=11))
    
    
    # grid.text(expression(paste(""%*%"10"^"3")),x=unit(-3,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=9))
    # xvals = seq(2020,2080,1)
    for(model in 1:11) 
    {
      if(model<12) grid.lines(x=0.6+0.085*(model-1),y=c(thresholdAcet[[model]][3,REG],thresholdAcet[[model]][4,REG]),default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=0.6+0.085*(model-1),y=c(thresholdAcet[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<12) grid.lines(x=1.6+0.085*(model-1),y=c(thresholdBcet[[model]][3,REG],thresholdBcet[[model]][4,REG]),default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=1.6+0.085*(model-1),y=c(thresholdBcet[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<12) grid.lines(x=2.6+0.085*(model-1),y=c(thresholdCcet[[model]][3,REG],ifelse(thresholdCcet[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdCcet[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=2.6+0.085*(model-1),y=c(thresholdCcet[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      # if(thresholdCcet[[model]][4,REG]>YMAX[[REG]]) 
      if(model<12) grid.lines(x=3.6+0.085*(model-1),y=c(thresholdDcet[[model]][3,REG],ifelse(thresholdDcet[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdDcet[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=3.6+0.085*(model-1),y=c(thresholdDcet[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      
      if(model<12) grid.lines(x=4.6+0.085*(model-1),y=c(thresholdEcet[[model]][3,REG],ifelse(thresholdEcet[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdEcet[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=4.6+0.085*(model-1),y=c(thresholdEcet[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      
      if(model<12) grid.lines(x=5.6+0.085*(model-1),y=c(thresholdFcet[[model]][3,REG],ifelse(thresholdFcet[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],thresholdFcet[[model]][4,REG])),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=5.6+0.085*(model-1),y=c(thresholdFcet[[model]][1,REG]),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
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
    lci[model] = RESULTS[[model]][3,REG]
    uci[model] = RESULTS[[model]][4,REG]
  }
  return(paste0( signif(median(med),3),' (',signif(median(lci),3),', ',signif(median(uci),3), ')'))
}

getOverall(RESULTS = thresholdA)
getOverall(RESULTS = thresholdA,REG = 1)
getOverall(RESULTS = thresholdA,REG = 3)

getOverall(RESULTS = thresholdAcet)
getOverall(RESULTS = thresholdAcet,REG = 1)
getOverall(RESULTS = thresholdAcet,REG = 3)


getOverall(RESULTS = thresholdF)
getOverall(RESULTS = thresholdF,REG = 1)
getOverall(RESULTS = thresholdF,REG = 3)


getOverall(RESULTS = thresholdF)

getOverall(RESULTS = thresholdD,REG = 1)
getOverall(RESULTS = thresholdD,REG = 3)

