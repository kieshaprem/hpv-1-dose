library(grid)
PRESENTATION = FALSE

load('output/prime/protect_life_group.rdata')
load('output/prime/protect_20y_group.rdata')
load('output/prime/protect_30y_group.rdata')
load('output/prime/protect_life80take_group.rdata')
source('code/functions_supporting.r')

protect = list(protect_life_group,protect_20y_group,protect_30y_group,protect_life80take_group)

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


YMAX = list(110000,110000,11000,150000)
YAXIS = list(seq(0,100000,25000),
             seq(0,100000,25000),
             seq(0,10000,2000),
             seq(0,150000,50000))
YAXISNAME =  list(seq(0,100,25),
                  seq(0,100,25),
                  seq(0,10,2),
                  seq(0,150,50))

png('output/plots/fig_canceraverted_dish.png',width = 20,height = 23.5,units = 'cm',res = 700)
if(1)
{
  grid.newpage()
  pushViewport(plotViewport(c(1,3,3,0)))
  pushViewport(viewport(layout=grid.layout(nrow=4,ncol=3,width=rep(1,4),height=rep(1,4))))
 
  pushViewport(viewport(layout.pos.row=1,layout.pos.col=1:3))
  pushViewport(plotViewport(c(0,2,0,1)))
  grid.text("Protection from 1 dose",y=unit(1,'npc')+unit(2,'lines'),gp=gpar(fontsize=12,fontface='bold'))
  popViewport()
  popViewport()
  
  for (SCEN in 1:3) 
  {
    pushViewport(viewport(layout.pos.row=1,layout.pos.col=SCEN))
    pushViewport(plotViewport(c(0,2,0,1)))
    grid.text(SCENLABEL[SCEN],y=unit(1,'npc')+unit(0.2,'lines'),gp=gpar(fontsize=11,fontface='bold'))
    popViewport()
    popViewport()
  }
  
  for (REG in 1:4) 
  {
    pushViewport(viewport(layout.pos.row=REG,layout.pos.col=1))
    pushViewport(plotViewport(c(1,2,1,1),yscale=c(0,YMAX[[REG]]),xscale=c(2020,2209)))
    
    grid.rect()
    grid.polygon(x = c(2120,2209,2209,2120),y=c(0,0,YMAX[[REG]],YMAX[[REG]]),default.units = 'native',gp = gpar(fill='grey80',col = NA))
    
    
    if(REG==1)
    {
      ypos = c(seq(0.95,0.15,-0.05), seq(0.60,0.5,-0.05))
      LEGENDNAMES = c('PHE: UK',paste0('HPV-ADVISE: ',c("India",'Nigeria','Uganda','Vietnam', 'Canada')),paste0('Harvard: ',c("India",'Uganda','El Salvador','Nicaragua','United States')))   
      COLS = c('black',rep('darkred',5),rep('steelblue',5))
      LTY = c(1,2:6,2:6)
      for(model in 1:11) grid.lines(x=c(0.02,0.13),y=ypos[model],default.units = 'npc',gp=gpar(col=COLS[model],lty=LTY[model],lwd=1.5))
      for(model in 1:11) grid.text(LEGENDNAMES[model],x=0.14,y=ypos[model],default.units = 'npc',just = 'left',gp=gpar(col=COLS[model],fontsize=7))
      # for(type in 6:8) grid.lines(x=c(0.02,0.13),y=ypos[type],default.units = 'npc',gp=gpar(col=COLS[type-5],lty=1,lwd=1.5))
      # for(type in 6:8) grid.text(LEGENDNAMES[type],x=0.14,y=ypos[type],default.units = 'npc',just = 'left',gp=gpar(col=COLS[type-5],fontsize=7))
    }
    # grid.xaxis()
    grid.xaxis(at = c(seq(2020,2209,10)),label = F,gp=gpar(fontsize=3))
    grid.xaxis(at = c(seq(2050,2209,50)),gp=gpar(fontsize=8))
    # grid.yaxis()
    grid.yaxis(at = YAXIS[[REG]],label = YAXISNAME[[REG]],gp=gpar(fontsize=10))
    grid.text(REGIONLABEL[REG],x=unit(-4.5,'lines'),rot=90,gp=gpar(fontsize=11,fontface='bold'))

    # if(REG == 1)  grid.text("Cervical cancers averted ('000)\n(10y protection from 1 dose)",x=unit(-4,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=11))
    grid.text("Cervical cancers averted",x=unit(-3.25,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=11))
    grid.text(expression(paste(""%*%"10"^"3")),x=unit(-3,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=9))
    
    xvals = seq(2020,2209,1)
    for(model in 1:11) 
    {
      yvals = getYvalues(protect = protect,PROTECT = 2,VAR = 'cases_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = TRUE)[,model]
      grid.lines(x=xvals,y=yvals,default.units = 'native',gp = gpar(lwd=1,col=COLS[model],lty = LTY[model]))
    }
    grid.rect(gp = gpar(fill=NA,lwd=1.5))
    popViewport()
    popViewport()
    
  }
  
  for (REG in 1:4) 
  {
    pushViewport(viewport(layout.pos.row=REG,layout.pos.col=2))
    pushViewport(plotViewport(c(1,2,1,1),yscale=c(0,YMAX[[REG]]),xscale=c(2020,2209)))
    grid.rect()
    grid.polygon(x = c(2120,2209,2209,2120),y=c(0,0,YMAX[[REG]],YMAX[[REG]]),default.units = 'native',gp = gpar(fill='grey80',col = NA))
    
    # grid.xaxis()
    grid.xaxis(at = c(seq(2020,2209,10)),label = F,gp=gpar(fontsize=3))
    grid.xaxis(at = c(seq(2050,2209,50)),gp=gpar(fontsize=8))
    # grid.yaxis()
    grid.yaxis(at = YAXIS[[REG]],label = YAXISNAME[[REG]],gp=gpar(fontsize=10))
    # grid.text(REGIONLABEL[REG],y=unit(1,'npc')+unit(1.2,'lines'),gp=gpar(fontsize=11,fontface='bold'))
    # grid.text("Cervical cancers averted",x=unit(-3.25,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=11))
    grid.text(expression(paste(""%*%"10"^"3")),x=unit(-3,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=9))
    xvals = seq(2020,2209,1)
    for(model in 1:11) 
    {
      yvals = getYvalues(protect = protect,PROTECT = 3,VAR = 'cases_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = TRUE)[,model]
      grid.lines(x=xvals,y=yvals,default.units = 'native',gp = gpar(lwd=1,col=COLS[model],lty = LTY[model]))
    }
    grid.rect(gp = gpar(fill=NA,lwd=1.5))
    popViewport()
    popViewport()
    
  }
  
  for (REG in 1:4) 
  {
    pushViewport(viewport(layout.pos.row=REG,layout.pos.col=3))
    pushViewport(plotViewport(c(1,2,1,1),yscale=c(0,YMAX[[REG]]),xscale=c(2020,2209)))
    grid.rect()
    grid.polygon(x = c(2120,2209,2209,2120),y=c(0,0,YMAX[[REG]],YMAX[[REG]]),default.units = 'native',gp = gpar(fill='grey80',col = NA))
    
    # grid.xaxis()
    grid.xaxis(at = c(seq(2020,2209,10)),label = F,gp=gpar(fontsize=3))
    grid.xaxis(at = c(seq(2050,2209,50)),gp=gpar(fontsize=8))
    # grid.yaxis()
    grid.yaxis(at = YAXIS[[REG]],label = YAXISNAME[[REG]],gp=gpar(fontsize=10))
    # grid.text(REGIONLABEL[REG],y=unit(1,'npc')+unit(1.2,'lines'),gp=gpar(fontsize=11,fontface='bold'))
    # if(REG == 1)  grid.text("Cervical cancers averted\n(lifelong protection at VE 80%)",x=unit(-4,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=11))
    grid.text(expression(paste(""%*%"10"^"3")),x=unit(-3,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=9))
    xvals = seq(2020,2209,1)
    for(model in 1:11) 
    {
      yvals = getYvalues(protect = protect,PROTECT = 4,VAR = 'cases_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = TRUE)[,model]
      grid.lines(x=xvals,y=yvals,default.units = 'native',gp = gpar(lwd=1,col=COLS[model],lty = LTY[model]))
    }
    grid.rect(gp = gpar(fill=NA,lwd=1.5))
    popViewport()
    popViewport()
    
  }
  
  popViewport()
  popViewport()
}
dev.off()


YMAX = list(10E5,10E5,10E4,15E5)
YAXIS = list(seq(0,10E5,2E5),
             seq(0,10E5,2E5),
             seq(0,10E4,2E4),
             seq(0,15E5,5E5))

YAXISNAME =  list(seq(0,1000,200),
                  seq(0,1000,200),
                  seq(0,100,20),
                  seq(0,1500,500))

png('output/plots/fig_canceraverted.png',width = 20,height = 23.5,units = 'cm',res = 700)
if(1)
{
  grid.newpage()
  pushViewport(plotViewport(c(1,3,3,0)))
  pushViewport(viewport(layout=grid.layout(nrow=4,ncol=3,width=rep(1,4),height=rep(1,4))))
  
  pushViewport(viewport(layout.pos.row=1,layout.pos.col=1:3))
  pushViewport(plotViewport(c(0,2,0,1)))
  grid.text("Protection from 1 dose",y=unit(1,'npc')+unit(2,'lines'),gp=gpar(fontsize=12,fontface='bold'))
  popViewport()
  popViewport()
  
  for (SCEN in 1:3) 
  {
    pushViewport(viewport(layout.pos.row=1,layout.pos.col=SCEN))
    pushViewport(plotViewport(c(0,2,0,1)))
    grid.text(SCENLABEL[SCEN],y=unit(1,'npc')+unit(0.2,'lines'),gp=gpar(fontsize=11,fontface='bold'))
    popViewport()
    popViewport()
  }
  
  for (REG in 1:4) 
  {
    pushViewport(viewport(layout.pos.row=REG,layout.pos.col=1))
    pushViewport(plotViewport(c(1,2,1,1),yscale=c(0,YMAX[[REG]]),xscale=c(2020,2209)))
    
    grid.rect()
    grid.polygon(x = c(2120,2209,2209,2120),y=c(0,0,YMAX[[REG]],YMAX[[REG]]),default.units = 'native',gp = gpar(fill='grey80',col = NA))
    
    
    if(REG==1)
    {
      ypos = c(seq(0.95,0.15,-0.05), seq(0.60,0.5,-0.05))
      LEGENDNAMES = c('PHE: UK',paste0('HPV-ADVISE: ',c("India",'Nigeria','Uganda','Vietnam', 'Canada')),paste0('Harvard: ',c("India",'Uganda','El Salvador','Nicaragua','United States')))   
      COLS = c('black',rep('darkred',5),rep('steelblue',5))
      LTY = c(1,2:6,2:6)
      for(model in 1:11) grid.lines(x=c(0.02,0.13),y=ypos[model],default.units = 'npc',gp=gpar(col=COLS[model],lty=LTY[model],lwd=1.5))
      for(model in 1:11) grid.text(LEGENDNAMES[model],x=0.14,y=ypos[model],default.units = 'npc',just = 'left',gp=gpar(col=COLS[model],fontsize=7))
      # for(type in 6:8) grid.lines(x=c(0.02,0.13),y=ypos[type],default.units = 'npc',gp=gpar(col=COLS[type-5],lty=1,lwd=1.5))
      # for(type in 6:8) grid.text(LEGENDNAMES[type],x=0.14,y=ypos[type],default.units = 'npc',just = 'left',gp=gpar(col=COLS[type-5],fontsize=7))
    }
    # grid.xaxis()
    grid.xaxis(at = c(seq(2020,2209,10)),label = F,gp=gpar(fontsize=3))
    grid.xaxis(at = c(seq(2050,2209,50)),gp=gpar(fontsize=8))
    # grid.yaxis()
    grid.yaxis(at = YAXIS[[REG]],label = YAXISNAME[[REG]],gp=gpar(fontsize=10))
    grid.text(REGIONLABEL[REG],x=unit(-4.5,'lines'),rot=90,gp=gpar(fontsize=11,fontface='bold'))
    
    # if(REG == 1)  grid.text("Cervical cancers averted ('000)\n(10y protection from 1 dose)",x=unit(-4,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=11))
    grid.text("Cervical cancers averted",x=unit(-3.25,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=11))
    grid.text(expression(paste(""%*%"10"^"3")),x=unit(-3,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=9))
    
    xvals = seq(2020,2209,1)
    for(model in 1:11) 
    {
      yvals = getYvalues(protect = protect,PROTECT = 2,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = TRUE)[,model]
      grid.lines(x=xvals,y=yvals,default.units = 'native',gp = gpar(lwd=1,col=COLS[model],lty = LTY[model]))
    }
    grid.rect(gp = gpar(fill=NA,lwd=1.5))
    popViewport()
    popViewport()
    
  }
  
  for (REG in 1:4) 
  {
    pushViewport(viewport(layout.pos.row=REG,layout.pos.col=2))
    pushViewport(plotViewport(c(1,2,1,1),yscale=c(0,YMAX[[REG]]),xscale=c(2020,2209)))
    grid.rect()
    grid.polygon(x = c(2120,2209,2209,2120),y=c(0,0,YMAX[[REG]],YMAX[[REG]]),default.units = 'native',gp = gpar(fill='grey80',col = NA))
    
    # grid.xaxis()
    grid.xaxis(at = c(seq(2020,2209,10)),label = F,gp=gpar(fontsize=3))
    grid.xaxis(at = c(seq(2050,2209,50)),gp=gpar(fontsize=8))
    # grid.yaxis()
    grid.yaxis(at = YAXIS[[REG]],label = YAXISNAME[[REG]],gp=gpar(fontsize=10))
    # grid.text(REGIONLABEL[REG],y=unit(1,'npc')+unit(1.2,'lines'),gp=gpar(fontsize=11,fontface='bold'))
    # grid.text("Cervical cancers averted",x=unit(-3.25,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=11))
    grid.text(expression(paste(""%*%"10"^"3")),x=unit(-3,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=9))
    xvals = seq(2020,2209,1)
    for(model in 1:11) 
    {
      yvals = getYvalues(protect = protect,PROTECT = 3,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = TRUE)[,model]
      grid.lines(x=xvals,y=yvals,default.units = 'native',gp = gpar(lwd=1,col=COLS[model],lty = LTY[model]))
    }
    grid.rect(gp = gpar(fill=NA,lwd=1.5))
    popViewport()
    popViewport()
    
  }
  
  for (REG in 1:4) 
  {
    pushViewport(viewport(layout.pos.row=REG,layout.pos.col=3))
    pushViewport(plotViewport(c(1,2,1,1),yscale=c(0,YMAX[[REG]]),xscale=c(2020,2209)))
    grid.rect()
    grid.polygon(x = c(2120,2209,2209,2120),y=c(0,0,YMAX[[REG]],YMAX[[REG]]),default.units = 'native',gp = gpar(fill='grey80',col = NA))
    
    # grid.xaxis()
    grid.xaxis(at = c(seq(2020,2209,10)),label = F,gp=gpar(fontsize=3))
    grid.xaxis(at = c(seq(2050,2209,50)),gp=gpar(fontsize=8))
    # grid.yaxis()
    grid.yaxis(at = YAXIS[[REG]],label = YAXISNAME[[REG]],gp=gpar(fontsize=10))
    # grid.text(REGIONLABEL[REG],y=unit(1,'npc')+unit(1.2,'lines'),gp=gpar(fontsize=11,fontface='bold'))
    # if(REG == 1)  grid.text("Cervical cancers averted\n(lifelong protection at VE 80%)",x=unit(-4,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=11))
    grid.text(expression(paste(""%*%"10"^"3")),x=unit(-3,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=9))
    xvals = seq(2020,2209,1)
    for(model in 1:11) 
    {
      yvals = getYvalues(protect = protect,PROTECT = 4,VAR = 'cases_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = TRUE)[,model]
      grid.lines(x=xvals,y=yvals,default.units = 'native',gp = gpar(lwd=1,col=COLS[model],lty = LTY[model]))
    }
    grid.rect(gp = gpar(fill=NA,lwd=1.5))
    popViewport()
    popViewport()
    
  }
  
  popViewport()
  popViewport()
}
dev.off()

