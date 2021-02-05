library(grid)
library(ggsci)
PRESENTATION = TRUE
reds = seq(100,250,50)/255
blues = seq(105,255,50)/255

COLS =c('black', 
        rgb(reds[1],0,0),
        rgb(reds[2],0,0),
        rgb(reds[3],0,0),
        rgb(reds[4],0,0),
        rgb(0/255, 90/255, blues[1]),
        rgb(0/255, 90/255, blues[2]),
        rgb(0/255, 90/255, blues[3]),
        rgb(0/255, 90/255, blues[4]))  #c('navy',pal_jama()(7))
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



10^4.75
YMAX = list(10^5.5,10^5.5,10^5.5,10^5.5)#28000,28000,28000,28000)
YAXIS = list(seq(1,5,1),
             seq(1,5,1),
             seq(1,5,1),
             seq(1,5,1))
YAXISNAME =  list(c(10,100,1000,10000,100000),
                  c(10,100,1000,10000,100000),
                  c(10,100,1000,10000,100000),
                  c(10,100,1000,10000,100000))
XAXISNAME =  c(expression(paste("0" %->% 1['20y'])),
               expression(paste("0" %->% 1['30y'])),
               expression(paste("0" %->% 1['VE:80%'])),
               expression(paste(1['20y'] %->% 2["life"])),
               expression(paste(1['30y'] %->% 2["life"])),
               expression(paste(1['VE:80%'] %->% 2["life"])))

png('output/plots/fig_nnv_dish.png',width = 38,height = 10.2,units = 'cm',res = 700)
if(1)
{
  grid.newpage()
  pushViewport(plotViewport(c(1.5,3,1,0)))
  pushViewport(viewport(layout=grid.layout(nrow=1,ncol=4,width=rep(1,4),height=rep(1,2))))
  
  # grid.text(expression(paste(Delta," #dose"['(protection)'])),y=unit(-0,'lines'),rot=0,gp=gpar(fontface='bold',fontsize=11))
  grid.text(expression(paste("Change in number of vaccine doses"['(duration/extent of protection)'])),y=unit(-0.75,'lines'),rot=0,gp=gpar(fontface='bold',fontsize=11))
  
  for (REG in 1:4) 
  {
    pushViewport(viewport(layout.pos.row=1,layout.pos.col=REG))
    pushViewport(plotViewport(c(2,2,1,1),yscale=c(1,log10(YMAX[[REG]])),xscale=c(0.5,6.5)))
    grid.polygon(x=c(0.5,3.5,3.5,0.5), y=c(1,1,log10(YMAX[[REG]]),log10(YMAX[[REG]])),default.units = 'native',gp=gpar(fill='grey95',col=NA))
    if(1)
    {
      # grid.text(('increasing #dose'),y=0.96,x=0.5,default.units = 'npc',rot=0,gp=gpar(fontsize=6,fontface='bold',col='black'))
      # grid.text(('nnv cost when increasing #dose'),y=0.96,x=0.5,default.units = 'npc',rot=0,gp=gpar(fontsize=6,fontface='bold',col='black'))
      # grid.text(expression(paste(Delta, 'nnv cost when increasing #dose')),y=0.96,x=0.5,default.units = 'npc',rot=0,gp=gpar(fontsize=7,fontface='bold',col='black'))
      # grid.text('increasing #dose',y=0.96,x=0.75,default.units = 'npc',rot=0,gp=gpar(fontsize=7,fontface='bold',col='black'))
      grid.text(expression(paste("0" %->% "1"['20y / 30y / VE:80%'])),y=unit(-2.5,'lines'),x=0.25,default.units = 'npc',rot=0,gp=gpar(fontsize=9,col='black'))
      grid.text(expression(paste("1"['20 / 30y / VE:80%'] %->% "2"['life'])),y=unit(-2.5,'lines'),x=0.75,default.units = 'npc',rot=0,gp=gpar(fontsize=9,col='black'))
    }
    grid.rect(gp=gpar(fill=NA))
    
    if(REG==1)
    {
      ypos = c(seq(0.95,0.55,-0.05))#, seq(0.60,0.5,-0.05))
      LEGENDNAMES = c('PHE: UK',paste0('HPV-ADVISE: ',c("India",'Nigeria','Uganda','Vietnam')),paste0('Harvard: ',c('India','Uganda','El Salvador','Nicaragua')),
                      '2 doses lifetime protection','1 dose 10y protection','1 dose 20y protection')   
      # COLS = c('black','darkred','steelblue')
      for(model in 1:9) grid.points(x=c(0.08),y=ypos[model],default.units = 'npc',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      # for(model in 1:5) grid.lines(x=c(0.02,0.13),y=ypos[model],default.units = 'npc',gp=gpar(col='grey40',lty=model,lwd=1.5))
      for(model in 1:9) grid.text(LEGENDNAMES[model],x=0.10,y=ypos[model],default.units = 'npc',just = 'left',gp=gpar(col='grey40',fontsize=7))
      # for(type in 6:8) grid.lines(x=c(0.02,0.13),y=ypos[type],default.units = 'npc',gp=gpar(col=COLS[type-5],lty=1,lwd=1.5))
      # for(type in 1:) grid.text(LEGENDNAMES[type],x=0.10,y=ypos[type],default.units = 'npc',just = 'left',gp=gpar(col=COLS[type-5],fontsize=7))
    }
    # grid.xaxis()
    grid.xaxis(at = seq(1,6,1),label = c(XAXISNAME),gp=gpar(fontsize=6))
    # grid.yaxis()
    grid.yaxis(at = YAXIS[[REG]],label = YAXISNAME[[REG]],gp=gpar(fontsize=8))
    grid.text(REGIONLABEL[REG],y=unit(1,'npc')+unit(1.2,'lines'),gp=gpar(fontsize=11,fontface='bold'))
    # if(REG == 1)  grid.text("Cervical cancers averted ('000)\n(10y protection from 1 dose)",x=unit(-4,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=11))
    if(REG == 1)  grid.text("Number needed to give +1 dose\nto avert 1 cervical cancer case",x=unit(-3.85,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=11))
    if(REG == 1)  grid.text(expression(paste("(",log[10]," scale)")),x=unit(-3.05,'lines'),rot=90,gp=gpar(fontface='plain',fontsize=9))
    # grid.text(expression(paste(""%*%"10"^"3")),x=unit(-3,'lines'),rot=90,gp=gpar(fontface='bold',fontsize=9))
    # xvals = seq(2020,2080,1)
    for(model in 1:9) 
    {
      if(model<10) grid.lines(x=0.6+0.1*(model-1),y=log10(c(nnvdishA[[model]][3,REG],nnvdishA[[model]][4,REG])),default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=0.6+0.1*(model-1),y=log10(c(nnvdishA[[model]][1,REG])),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<10) grid.lines(x=1.6+0.1*(model-1),y=log10(c(nnvdishB[[model]][3,REG],nnvdishB[[model]][4,REG])),default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=1.6+0.1*(model-1),y=log10(c(nnvdishB[[model]][1,REG])),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<10) grid.lines(x=2.6+0.1*(model-1),y=log10(c(nnvdishC[[model]][3,REG],ifelse(nnvdishC[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],nnvdishC[[model]][4,REG]))),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=2.6+0.1*(model-1),y=log10(c(nnvdishC[[model]][1,REG])),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      # if(nnvdishC[[model]][4,REG]>YMAX[[REG]]) 
      if(model<10) grid.lines(x=3.6+0.1*(model-1),y=log10(c(nnvdishD[[model]][3,REG],ifelse(nnvdishD[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],nnvdishD[[model]][4,REG]))),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=3.6+0.1*(model-1),y=log10(c(nnvdishD[[model]][1,REG])),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
      if(model<10) grid.lines(x=4.6+0.1*(model-1),y=log10(c(nnvdishE[[model]][3,REG],ifelse(nnvdishE[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],nnvdishE[[model]][4,REG]))),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=4.6+0.1*(model-1),y=log10(c(nnvdishE[[model]][1,REG])),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      # if(nnvdishC[[model]][4,REG]>YMAX[[REG]]) 
      if(model<10) grid.lines(x=5.6+0.1*(model-1),y=log10(c(nnvdishF[[model]][3,REG],ifelse(nnvdishF[[model]][4,REG]>YMAX[[REG]],YMAX[[REG]],nnvdishF[[model]][4,REG]))),
                              default.units = 'native',gp=gpar(col=COLS[model]))
      grid.points(x=5.6+0.1*(model-1),y=log10(c(nnvdishF[[model]][1,REG])),default.units = 'native',pch=16,gp=gpar(col=COLS[model],cex=0.3))
      
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

