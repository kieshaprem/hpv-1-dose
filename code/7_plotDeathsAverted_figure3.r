library(grid)
PRESENTATION = FALSE

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

SCENLABEL = c('20 years at 100% VE',
              '30 years at 100% VE',
              'Lifelong years at 80% VE')


YMAX = list(0.30,0.30,0.30,0.30)
YAXIS = list(seq(0,0.3,0.1),
             seq(0,0.3,0.1),
             seq(0,0.3,0.1),
             seq(0,0.3,0.1))
YAXISNAME =  list(seq(0,30,10),
                  seq(0,30,10),
                  seq(0,30,10),
                  seq(0,30,10))

png('output/plots/fig_deathsUnaverted_dish_percent.png',width = 20,height = 23.5,units = 'cm',res = 700)
if(1)
{
  grid.newpage()
  pushViewport(plotViewport(c(1,4,3,0)))
  pushViewport(viewport(layout=grid.layout(nrow=4,ncol=3,width=rep(1,4),height=rep(1,4))))
  
  pushViewport(viewport(layout.pos.row=1,layout.pos.col=1:3))
  pushViewport(plotViewport(c(0,1,0,1)))
  grid.text("Protection from 1 dose",y=unit(1,'npc')+unit(2,'lines'),gp=gpar(fontsize=12,fontface='bold'))
  popViewport()
  popViewport()
  
  for (SCEN in 1:3) 
  {
    pushViewport(viewport(layout.pos.row=1,layout.pos.col=SCEN))
    pushViewport(plotViewport(c(0,1,0,1)))
    grid.text(SCENLABEL[SCEN],y=unit(1,'npc')+unit(0.2,'lines'),gp=gpar(fontsize=11,fontface='bold'))
    popViewport()
    popViewport()
  }
  
  COLS = c('black',rep('darkred',5),rep('steelblue',5))
  LTY = c(1,2:6,2:6)
  COUNTRIES = c('GBR','IND','NGA','UGA','VNM','CAN','IND','UGA','SLV','NIC','USA')
  MODELS = c('PHE','HPV-ADVISE','HARVARD')
  LEGENDNAMES = c('PHE: UK',paste0('HPV-ADVISE: ',c("India",'Nigeria','Uganda','Vietnam',"Canada")),paste0('Harvard: ',c("India",'Uganda','El Salvador','Nicaragua','United States')))   
  
  for (REG in 1:4) 
  {
    pushViewport(viewport(layout.pos.row=REG,layout.pos.col=1))
    pushViewport(plotViewport(c(1,1,1,1),yscale=c(0,YMAX[[REG]]),xscale=c(0.5,11.5)))
    grid.rect()
    
    grid.text(COUNTRIES, x = unit(1:11,'native'), y = unit(-1,'lines'), gp=gpar(fontsize=5.5))
    grid.text(MODELS, x = unit(c(1,4,9),'native'), y = unit(-2,'lines'), gp=gpar(fontsize=8,fontface = 'bold'))
    grid.xaxis(at = c(0.5,1.5,6.5,11.5),label = FALSE,gp=gpar(fontsize=15))
    
    # grid.yaxis()
    grid.yaxis(at = YAXIS[[REG]],label = YAXISNAME[[REG]],gp=gpar(fontsize=10))
    grid.text(REGIONLABEL[REG],x=unit(-4.7,'lines'), rot=90, gp=gpar(fontsize=11,fontface='bold'))
    grid.text("CC deaths not averted\nby 1-dose as a % 2-dose",x=unit(-3.2,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=10))
    for(model in 1:11)
    {
      yvals1 = sum(getYvalues(protect = protect,PROTECT = 1,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals2 = sum(getYvalues(protect = protect,PROTECT = 2,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals = (yvals1 - yvals2)/yvals1
      yvals1uci = sum(getYvalues(protect = protect_p90,PROTECT = 1,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals2uci = sum(getYvalues(protect = protect_p90,PROTECT = 2,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvalsuci = (yvals1uci - yvals2uci)/yvals1uci
      yvals1lci = sum(getYvalues(protect = protect_p10,PROTECT = 1,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals2lci = sum(getYvalues(protect = protect_p10,PROTECT = 2,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvalslci = (yvals1lci - yvals2lci)/yvals1lci
      yv = sort(c(yvals,yvalslci,yvalsuci))
      
      grid.polygon(x= c(model-0.25,model+0.25,model+0.25,model-0.25),y=c(0,0,yv[2],yv[2]),default.units = 'native',gp = gpar(col = NA,fill = COLS[model]))
      grid.lines(x= c(model,model),y=c(yv[2],yv[1]),default.units = 'native',gp = gpar(col = 'white'))
      grid.lines(x= c(model,model),y=c(yv[2],yv[3]),default.units = 'native',gp = gpar(col = COLS[model]))
      rm(yvals,yvals1,yvals2,yvals1lci,yvals2lci,yvalslci,yvalsuci,yvals2uci,yvals1uci)
    }
    
    popViewport()
    popViewport()
    
  }
  
  for (REG in 1:4) 
  {
    pushViewport(viewport(layout.pos.row=REG,layout.pos.col=2))
    pushViewport(plotViewport(c(1,1,1,1),yscale=c(0,YMAX[[REG]]),xscale=c(0.5,11.5)))
    grid.rect()
    
    grid.text(COUNTRIES, x = unit(1:11,'native'), y = unit(-1,'lines'), gp=gpar(fontsize=5.5))
    grid.text(MODELS, x = unit(c(1,4,9),'native'), y = unit(-2,'lines'), gp=gpar(fontsize=8,fontface = 'bold'))
    grid.xaxis(at = c(0.5,1.5,6.5,11.5),label = FALSE,gp=gpar(fontsize=15))
    
    # grid.yaxis()
    grid.yaxis(at = YAXIS[[REG]],label = YAXISNAME[[REG]],gp=gpar(fontsize=10))
    # grid.text(REGIONLABEL[REG],y=unit(1,'npc')+unit(1.2,'lines'),gp=gpar(fontsize=11,fontface='bold'))
    # if(REG == 1)  grid.text("Cervical cancers not averted\nby 1-dose as a % 2-dose\n(30y protection from 1 dose)",x=unit(-3.9,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=11))
    # if(REG == 1)  grid.text("Cervical cancers not averted\nby 1-dose as a % 2-dose\n(30y protection from 1 dose)",x=unit(-3.9,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=11))
    for(model in 1:11)
    {
      yvals1 = sum(getYvalues(protect = protect,PROTECT = 1,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals2 = sum(getYvalues(protect = protect,PROTECT = 3,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals = (yvals1 - yvals2)/yvals1
      yvals1uci = sum(getYvalues(protect = protect_p90,PROTECT = 1,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals2uci = sum(getYvalues(protect = protect_p90,PROTECT = 3,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvalsuci = (yvals1uci - yvals2uci)/yvals1uci
      yvals1lci = sum(getYvalues(protect = protect_p10,PROTECT = 1,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals2lci = sum(getYvalues(protect = protect_p10,PROTECT = 3,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvalslci = (yvals1lci - yvals2lci)/yvals1lci
      yv = sort(c(yvals,yvalslci,yvalsuci))
      
      grid.polygon(x= c(model-0.25,model+0.25,model+0.25,model-0.25),y=c(0,0,yv[2],yv[2]),default.units = 'native',gp = gpar(col = NA,fill = COLS[model]))
      grid.lines(x= c(model,model),y=c(yv[2],yv[1]),default.units = 'native',gp = gpar(col = 'white'))
      grid.lines(x= c(model,model),y=c(yv[2],yv[3]),default.units = 'native',gp = gpar(col = COLS[model]))
      rm(yvals,yvals1,yvals2,yvals1lci,yvals2lci,yvalslci,yvalsuci,yvals2uci,yvals1uci)
    }
    popViewport()
    popViewport()
    
  }
  
  for (REG in 1:4) 
  {
    pushViewport(viewport(layout.pos.row=REG,layout.pos.col=3))
    pushViewport(plotViewport(c(1,1,1,1),yscale=c(0,YMAX[[REG]]),xscale=c(0.5,11.5)))
    grid.rect()
    
    grid.text(COUNTRIES, x = unit(1:11,'native'), y = unit(-1,'lines'), gp=gpar(fontsize=5.5))
    grid.text(MODELS, x = unit(c(1,4,9),'native'), y = unit(-2,'lines'), gp=gpar(fontsize=8,fontface = 'bold'))
    grid.xaxis(at = c(0.5,1.5,6.5,11.5),label = FALSE,gp=gpar(fontsize=15))
    
    # grid.yaxis()
    grid.yaxis(at = YAXIS[[REG]],label = YAXISNAME[[REG]],gp=gpar(fontsize=10))
    # grid.text(REGIONLABEL[REG],y=unit(1,'npc')+unit(1.2,'lines'),gp=gpar(fontsize=11,fontface='bold'))
    # if(REG == 1)  grid.text("Cervical cancers not averted\nby 1-dose as a % 2-dose\n(lifelong protection at VE 80%)",x=unit(-3.9,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=11))
    for(model in 1:11)
    {
      yvals1 = sum(getYvalues(protect = protect,PROTECT = 1,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals2 = sum(getYvalues(protect = protect,PROTECT = 4,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals = (yvals1 - yvals2)/yvals1
      yvals1uci = sum(getYvalues(protect = protect_p90,PROTECT = 1,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals2uci = sum(getYvalues(protect = protect_p90,PROTECT = 4,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvalsuci = (yvals1uci - yvals2uci)/yvals1uci
      yvals1lci = sum(getYvalues(protect = protect_p10,PROTECT = 1,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals2lci = sum(getYvalues(protect = protect_p10,PROTECT = 4,VAR = 'deaths_averted_disc',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvalslci = (yvals1lci - yvals2lci)/yvals1lci
      yv = sort(c(yvals,yvalslci,yvalsuci))
      
      grid.polygon(x= c(model-0.25,model+0.25,model+0.25,model-0.25),y=c(0,0,yv[2],yv[2]),default.units = 'native',gp = gpar(col = NA,fill = COLS[model]))
      grid.lines(x= c(model,model),y=c(yv[2],yv[1]),default.units = 'native',gp = gpar(col = 'white'))
      grid.lines(x= c(model,model),y=c(yv[2],yv[3]),default.units = 'native',gp = gpar(col = COLS[model]))
      rm(yvals,yvals1,yvals2,yvals1lci,yvals2lci,yvalslci,yvalsuci,yvals2uci,yvals1uci)
    }
    popViewport()
    popViewport()
    
  }
  
  popViewport()
  popViewport()
}
dev.off()

YMAX = list(0.20,0.20,0.20,0.20)
YAXIS = list(seq(0,0.2,0.05),
             seq(0,0.2,0.05),
             seq(0,0.2,0.05),
             seq(0,0.2,0.05))
YAXISNAME =  list(seq(0,20,5),
                  seq(0,20,5),
                  seq(0,20,5),
                  seq(0,20,5))

png('output/plots/fig_deathsUnaverted_percent.png',width = 20,height = 23.5,units = 'cm',res = 700)
if(1)
{
  grid.newpage()
  pushViewport(plotViewport(c(1,4,3,0)))
  pushViewport(viewport(layout=grid.layout(nrow=4,ncol=3,width=rep(1,4),height=rep(1,4))))
  
  pushViewport(viewport(layout.pos.row=1,layout.pos.col=1:3))
  pushViewport(plotViewport(c(0,1,0,1)))
  grid.text("Protection from 1 dose",y=unit(1,'npc')+unit(2,'lines'),gp=gpar(fontsize=12,fontface='bold'))
  popViewport()
  popViewport()
  
  for (SCEN in 1:3) 
  {
    pushViewport(viewport(layout.pos.row=1,layout.pos.col=SCEN))
    pushViewport(plotViewport(c(0,1,0,1)))
    grid.text(SCENLABEL[SCEN],y=unit(1,'npc')+unit(0.2,'lines'),gp=gpar(fontsize=11,fontface='bold'))
    popViewport()
    popViewport()
  }
  
  COLS = c('black',rep('darkred',5),rep('steelblue',5))
  LTY = c(1,2:6,2:6)
  COUNTRIES = c('GBR','IND','NGA','UGA','VNM','CAN','IND','UGA','SLV','NIC','USA')
  MODELS = c('PHE','HPV-ADVISE','HARVARD')
  LEGENDNAMES = c('PHE: UK',paste0('HPV-ADVISE: ',c("India",'Nigeria','Uganda','Vietnam',"Canada")),paste0('Harvard: ',c("India",'Uganda','El Salvador','Nicaragua','United States')))   
  
  for (REG in 1:4) 
  {
    pushViewport(viewport(layout.pos.row=REG,layout.pos.col=1))
    pushViewport(plotViewport(c(1,1,1,1),yscale=c(0,YMAX[[REG]]),xscale=c(0.5,11.5)))
    grid.rect()
    
    grid.text(COUNTRIES, x = unit(1:11,'native'), y = unit(-1,'lines'), gp=gpar(fontsize=5.5))
    grid.text(MODELS, x = unit(c(1,4,9),'native'), y = unit(-2,'lines'), gp=gpar(fontsize=8,fontface = 'bold'))
    grid.xaxis(at = c(0.5,1.5,6.5,11.5),label = FALSE,gp=gpar(fontsize=15))
    
    # grid.yaxis()
    grid.yaxis(at = YAXIS[[REG]],label = YAXISNAME[[REG]],gp=gpar(fontsize=10))
    grid.text(REGIONLABEL[REG],x=unit(-4.7,'lines'), rot=90, gp=gpar(fontsize=11,fontface='bold'))
    grid.text("CC deaths not averted\nby 1-dose as a % 2-dose",x=unit(-3.2,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=10))
    for(model in 1:11)
    {
      yvals1 = sum(getYvalues(protect = protect,PROTECT = 1,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals2 = sum(getYvalues(protect = protect,PROTECT = 2,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals = (yvals1 - yvals2)/yvals1
      yvals1uci = sum(getYvalues(protect = protect_p90,PROTECT = 1,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals2uci = sum(getYvalues(protect = protect_p90,PROTECT = 2,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvalsuci = (yvals1uci - yvals2uci)/yvals1uci
      yvals1lci = sum(getYvalues(protect = protect_p10,PROTECT = 1,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals2lci = sum(getYvalues(protect = protect_p10,PROTECT = 2,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvalslci = (yvals1lci - yvals2lci)/yvals1lci
      yv = sort(c(yvals,yvalslci,yvalsuci))
      
      grid.polygon(x= c(model-0.25,model+0.25,model+0.25,model-0.25),y=c(0,0,yv[2],yv[2]),default.units = 'native',gp = gpar(col = NA,fill = COLS[model]))
      grid.lines(x= c(model,model),y=c(yv[2],yv[1]),default.units = 'native',gp = gpar(col = 'white'))
      grid.lines(x= c(model,model),y=c(yv[2],yv[3]),default.units = 'native',gp = gpar(col = COLS[model]))
      rm(yvals,yvals1,yvals2,yvals1lci,yvals2lci,yvalslci,yvalsuci,yvals2uci,yvals1uci)
    }
    
    popViewport()
    popViewport()
    
  }
  
  for (REG in 1:4) 
  {
    pushViewport(viewport(layout.pos.row=REG,layout.pos.col=2))
    pushViewport(plotViewport(c(1,1,1,1),yscale=c(0,YMAX[[REG]]),xscale=c(0.5,11.5)))
    grid.rect()
    
    grid.text(COUNTRIES, x = unit(1:11,'native'), y = unit(-1,'lines'), gp=gpar(fontsize=5.5))
    grid.text(MODELS, x = unit(c(1,4,9),'native'), y = unit(-2,'lines'), gp=gpar(fontsize=8,fontface = 'bold'))
    grid.xaxis(at = c(0.5,1.5,6.5,11.5),label = FALSE,gp=gpar(fontsize=15))
    
    # grid.yaxis()
    grid.yaxis(at = YAXIS[[REG]],label = YAXISNAME[[REG]],gp=gpar(fontsize=10))
    # grid.text(REGIONLABEL[REG],y=unit(1,'npc')+unit(1.2,'lines'),gp=gpar(fontsize=11,fontface='bold'))
    # if(REG == 1)  grid.text("Cervical cancers not averted\nby 1-dose as a % 2-dose\n(30y protection from 1 dose)",x=unit(-3.9,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=11))
    # if(REG == 1)  grid.text("Cervical cancers not averted\nby 1-dose as a % 2-dose\n(30y protection from 1 dose)",x=unit(-3.9,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=11))
    for(model in 1:11)
    {
      yvals1 = sum(getYvalues(protect = protect,PROTECT = 1,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals2 = sum(getYvalues(protect = protect,PROTECT = 3,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals = (yvals1 - yvals2)/yvals1
      yvals1uci = sum(getYvalues(protect = protect_p90,PROTECT = 1,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals2uci = sum(getYvalues(protect = protect_p90,PROTECT = 3,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvalsuci = (yvals1uci - yvals2uci)/yvals1uci
      yvals1lci = sum(getYvalues(protect = protect_p10,PROTECT = 1,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals2lci = sum(getYvalues(protect = protect_p10,PROTECT = 3,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvalslci = (yvals1lci - yvals2lci)/yvals1lci
      yv = sort(c(yvals,yvalslci,yvalsuci))
      
      grid.polygon(x= c(model-0.25,model+0.25,model+0.25,model-0.25),y=c(0,0,yv[2],yv[2]),default.units = 'native',gp = gpar(col = NA,fill = COLS[model]))
      grid.lines(x= c(model,model),y=c(yv[2],yv[1]),default.units = 'native',gp = gpar(col = 'white'))
      grid.lines(x= c(model,model),y=c(yv[2],yv[3]),default.units = 'native',gp = gpar(col = COLS[model]))
      rm(yvals,yvals1,yvals2,yvals1lci,yvals2lci,yvalslci,yvalsuci,yvals2uci,yvals1uci)
    }
    popViewport()
    popViewport()
    
  }
  
  for (REG in 1:4) 
  {
    pushViewport(viewport(layout.pos.row=REG,layout.pos.col=3))
    pushViewport(plotViewport(c(1,1,1,1),yscale=c(0,YMAX[[REG]]),xscale=c(0.5,11.5)))
    grid.rect()
    
    grid.text(COUNTRIES, x = unit(1:11,'native'), y = unit(-1,'lines'), gp=gpar(fontsize=5.5))
    grid.text(MODELS, x = unit(c(1,4,9),'native'), y = unit(-2,'lines'), gp=gpar(fontsize=8,fontface = 'bold'))
    grid.xaxis(at = c(0.5,1.5,6.5,11.5),label = FALSE,gp=gpar(fontsize=15))
    
    # grid.yaxis()
    grid.yaxis(at = YAXIS[[REG]],label = YAXISNAME[[REG]],gp=gpar(fontsize=10))
    # grid.text(REGIONLABEL[REG],y=unit(1,'npc')+unit(1.2,'lines'),gp=gpar(fontsize=11,fontface='bold'))
    # if(REG == 1)  grid.text("Cervical cancers not averted\nby 1-dose as a % 2-dose\n(lifelong protection at VE 80%)",x=unit(-3.9,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=11))
    for(model in 1:11)
    {
      yvals1 = sum(getYvalues(protect = protect,PROTECT = 1,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals2 = sum(getYvalues(protect = protect,PROTECT = 4,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals = (yvals1 - yvals2)/yvals1
      yvals1uci = sum(getYvalues(protect = protect_p90,PROTECT = 1,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals2uci = sum(getYvalues(protect = protect_p90,PROTECT = 4,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvalsuci = (yvals1uci - yvals2uci)/yvals1uci
      yvals1lci = sum(getYvalues(protect = protect_p10,PROTECT = 1,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvals2lci = sum(getYvalues(protect = protect_p10,PROTECT = 4,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
      yvalslci = (yvals1lci - yvals2lci)/yvals1lci
      yv = sort(c(yvals,yvalslci,yvalsuci))
      
      grid.polygon(x= c(model-0.25,model+0.25,model+0.25,model-0.25),y=c(0,0,yv[2],yv[2]),default.units = 'native',gp = gpar(col = NA,fill = COLS[model]))
      grid.lines(x= c(model,model),y=c(yv[2],yv[1]),default.units = 'native',gp = gpar(col = 'white'))
      grid.lines(x= c(model,model),y=c(yv[2],yv[3]),default.units = 'native',gp = gpar(col = COLS[model]))
      rm(yvals,yvals1,yvals2,yvals1lci,yvals2lci,yvalslci,yvalsuci,yvals2uci,yvals1uci)
    }
    popViewport()
    popViewport()
    
  }
  
  popViewport()
  popViewport()
}
dev.off()

yv_20y = list() 
for(model in 1:11)
{
  yvals1 = sum(getYvalues(protect = protect,PROTECT = 1,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
  yvals2 = sum(getYvalues(protect = protect,PROTECT = 2,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
  yvals = (yvals1 - yvals2)/1
  yvals1uci = sum(getYvalues(protect = protect_p90,PROTECT = 1,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
  yvals2uci = sum(getYvalues(protect = protect_p90,PROTECT = 2,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
  yvalsuci = (yvals1uci - yvals2uci)/1
  yvals1lci = sum(getYvalues(protect = protect_p10,PROTECT = 1,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
  yvals2lci = sum(getYvalues(protect = protect_p10,PROTECT = 2,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
  yvalslci = (yvals1lci - yvals2lci)/1
  yv_20y[[model]] = sort(c(yvals,yvalslci,yvalsuci))
  rm(yvals,yvals1,yvals2,yvals1lci,yvals2lci,yvalslci,yvalsuci,yvals2uci,yvals1uci)
}

yv_30y = list() 
for(model in 1:11)
{
  yvals1 = sum(getYvalues(protect = protect,PROTECT = 1,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
  yvals2 = sum(getYvalues(protect = protect,PROTECT = 3,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
  yvals = (yvals1 - yvals2)/1
  yvals1uci = sum(getYvalues(protect = protect_p90,PROTECT = 1,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
  yvals2uci = sum(getYvalues(protect = protect_p90,PROTECT = 3,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
  yvalsuci = (yvals1uci - yvals2uci)/1
  yvals1lci = sum(getYvalues(protect = protect_p10,PROTECT = 1,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
  yvals2lci = sum(getYvalues(protect = protect_p10,PROTECT = 3,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
  yvalslci = (yvals1lci - yvals2lci)/1
  yv_30y[[model]] = sort(c(yvals,yvalslci,yvalsuci))
  rm(yvals,yvals1,yvals2,yvals1lci,yvals2lci,yvalslci,yvalsuci,yvals2uci,yvals1uci)
}

yv_lifeve80 = list() 
for(model in 1:11)
{
  yvals1 = sum(getYvalues(protect = protect,PROTECT = 1,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
  yvals2 = sum(getYvalues(protect = protect,PROTECT = 4,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
  yvals = (yvals1 - yvals2)
  yvals1uci = sum(getYvalues(protect = protect_p90,PROTECT = 1,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
  yvals2uci = sum(getYvalues(protect = protect_p90,PROTECT = 4,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
  yvalsuci = (yvals1uci - yvals2uci)
  yvals1lci = sum(getYvalues(protect = protect_p10,PROTECT = 1,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
  yvals2lci = sum(getYvalues(protect = protect_p10,PROTECT = 4,VAR = 'deaths_averted',REGIONS = REGIONNAMES[[REG]],SMOOTH = FALSE)[,model])
  yvalslci = (yvals1lci - yvals2lci)
  yv_lifeve80[[model]] = sort(c(yvals,yvalslci,yvalsuci))
  rm(yvals,yvals1,yvals2,yvals1lci,yvals2lci,yvalslci,yvalsuci,yvals2uci,yvals1uci)
}

yv_20y = do.call("rbind", yv_20y)
yv_30y = do.call("rbind", yv_30y)
yv_lifeve80 = do.call("rbind", yv_lifeve80)

colMeans(yv_20y)
colMeans(yv_30y)
colMeans(yv_lifeve80)
