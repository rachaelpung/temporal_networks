# reply to reviewer

library(grid)
library(data.table)

# saturation function
1-exp(-5) # 0.9932621

# create different contact staturation functions
contact_weight = data.table(dur_staturate=c(5,15,30,60,60),
                            scale=c(1,3,6,12,0))


# no. of contacts episodes vs duration of contacts
paneller=function(row = 1,column=1)
{
  if(row==1 & column==1){xlm=c(0,60); ylm=c(0,1)}
  if(!(row==1 & column==1)){xlm=c(0,20); ylm=c(0,20)}
 
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # col line
  col_line=col_hue_lancet

  # contact weightage over time
  if(row==1 & column==1){
    
    xx=0:60
    for(i in 1:contact_weight[,.N]){
      if(i!=5) yy = 1-exp(-xx/contact_weight$scale[i])
      if(i==5) yy = (0:60)/60
      grid.lines(xx, yy, default.units = 'native',gp=gpar(col=col_line[i]))
      
      grid.points(xx[xx==15], yy[xx==15], pch=15, default.units = 'native', gp=gpar(col=col_line[i],fill=col_line[i], cex=0.5))
      grid.points(xx[xx==30], yy[xx==30], pch=16, default.units = 'native', gp=gpar(col=col_line[i],fill=col_line[i], cex=0.5))
      grid.points(xx[xx==60], yy[xx==60], pch=17, default.units = 'native', gp=gpar(col=col_line[i],fill=col_line[i], cex=0.5))
      
    }
    
    
  }
   
  # effective contacts
  if(!(row==1 & column==1)){
    
    plot_data = data.table(contacts=rep(c(0,5,10,15,20), times=3),
                           duration=rep(c(15,30,60), each=5))
    
    plot_data = plot_data[rep(1:.N, times=5)]
    plot_data[, weight:=rep(c('5 min', '15 min', '30 min', '60 min', 'linear'), each=15)]
    plot_data[weight=='5 min', eff_contact:=1-exp(-duration/contact_weight$scale[1])]
    plot_data[weight=='15 min', eff_contact:=1-exp(-duration/contact_weight$scale[2])]
    plot_data[weight=='30 min', eff_contact:=1-exp(-duration/contact_weight$scale[3])]
    plot_data[weight=='60 min', eff_contact:=1-exp(-duration/contact_weight$scale[4])]
    plot_data[weight=='linear', eff_contact:=duration/60]
    plot_data[, contact_eff:=contacts*eff_contact]
    
    if(row==1 & column==2) { plot_data = plot_data[weight=='5 min']; col_line = col_hue_lancet[1]; legend_line = 'staturate after 5 mins'}
    if(row==1 & column==3) { plot_data = plot_data[weight=='15 min']; col_line = col_hue_lancet[2]; legend_line = 'staturate after 15 mins'}
    if(row==2 & column==1) { plot_data = plot_data[weight=='30 min']; col_line = col_hue_lancet[3]; legend_line = 'staturate after 30 mins'}
    if(row==2 & column==2) { plot_data = plot_data[weight=='60 min']; col_line = col_hue_lancet[4]; legend_line = 'staturate after 1 hr'}
    if(row==2 & column==3) { plot_data = plot_data[weight=='linear']; col_line = col_hue_lancet[5]; legend_line = 'linear'}
    line_shape=c(15,16,17)
    n_duration = c(15, 30, 60)
    legend_point = c('contact duration: 15 mins', 'contact duration: 30 mins', 'contact duration: 1 hr')
    y_legend = seq(0.95,0.75,-0.05)*20
    
    for(i in 1:3){
      grid.lines(plot_data[duration==n_duration[i]]$contacts, plot_data[duration==n_duration[i]]$contact_eff, default.units = 'native', gp=gpar(col=col_line))
      grid.points(plot_data[duration==n_duration[i]]$contacts[2:4], plot_data[duration==n_duration[i]]$contact_eff[2:4], pch=line_shape[i], default.units = 'native',
                  gp=gpar(col=col_line,fill=col_line, cex=0.5))
      
      
      grid.points(1, y_legend[i], pch=line_shape[i], default.units = 'native',gp=gpar(col=col_line,fill=col_line, cex=0.5))
      grid.text(legend_point[i], 1.5,y_legend[i], default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))

    }
    
    grid.lines(c(0.75,1.25), c(y_legend[4],y_legend[4]), default.units = 'native', gp=gpar(col=col_line))
    grid.text(legend_line, 1.5,y_legend[4], default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
    
    
  }
  
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # axis
  if(row==1 & column==1){
    grid.xaxis(at=seq(0,60,15),label=seq(0,60,15))
    grid.yaxis(at=seq(0,1,0.25),label=seq(0,1,0.25))
    grid.text('Contact weightage',x=unit(-3,'lines'),rot=90)
    grid.text('Contact duration (mins)',y=unit(-2.5,'lines'))
    
  } 
  
  if(!(row==1 & column==1)){
    grid.xaxis(at=seq(0,20,5),label=seq(0,20,5))
    grid.yaxis(at=seq(0,20,5),label=seq(0,20,5))
    grid.text('No. of effective contacts',x=unit(-3,'lines'),rot=90)
    grid.text('No. of contacts',y=unit(-2.5,'lines'))
    
  } 
  

  # labels
  if(row == 1 & column == 1) grid.text('A',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 2) grid.text('B',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 3) grid.text('C',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  if(row == 2 & column == 1) grid.text('D',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 2 & column == 2) grid.text('E',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 2 & column == 3) grid.text('F',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  popViewport()
  popViewport()
  
}


png('figure/effective_contact_supp.png',height=8*2,width=8*3,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=2,ncol=3)))

paneller(1,1)
paneller(1,2)
paneller(1,3)

paneller(2,1)
paneller(2,2)
paneller(2,3)

popViewport()
popViewport()
dev.off()

rm(paneller)



# p_k
paneller=function(row = 1,column=1)
{
  
  if(row==1) { xlm=c(0,120) }
  if(row==2 & column %in% c(1,2,3)) { xlm=c(0,50) }
  if(row==2 & column ==4) { xlm=c(0,100) }
  if(row==3 & column ==2) { xlm=c(0,50) }
  if(row==3 & column %in% c(1,3)) { xlm=c(0,100) }
  
  if(!(row==1 & column %in% c(3,4)) | (row==2 & column==1)) ylm=c(0,0.5)
  if(row==1 & column %in% c(3,4)) ylm=c(0,0.6)
  if(row==2 & column==1) ylm=c(0,0.7)
    
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  n = column + 4*(row-1)
  degree = copy(net[[n]]$el)
  degree = unique(degree[,c('node_i', 'node_j')])
  degree = degree[,.N, by=node_i]
  hist_breaks = c(10,10,10,10,5,5,5,10,10,5,10)
  degree = hist(degree$N, breaks=seq(0,max(degree$N)+hist_breaks[n], hist_breaks[n]), plot=FALSE)
  degree = data.table(min_deg=degree$breaks[-length(degree$breaks)], max_deg=degree$breaks[-1], 
                      counts=degree$counts, prop=degree$counts/sum(degree$counts))
  
  if(n %in% c(1:4)) col_line = col_hue_lancet[1]
  if(n %in% c(5)) col_line = col_hue_lancet[2]
  if(n %in% c(6:8)) col_line = col_hue_lancet[3]
  if(n %in% c(9)) col_line = col_hue_lancet[4]
  if(n %in% c(10:11)) col_line = col_hue_lancet[5]
  
  for(i in 1:degree[,.N]){
    grid.polygon(c(degree$min_deg[i], degree$max_deg[i],  degree$max_deg[i], degree$min_deg[i]), 
                 c(0,0, degree$prop[i], degree$prop[i]), default.units = 'native',gp=gpar(col='white', fill=col_line))
    
  }
  
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # axis
  if(row==1) { grid.xaxis(at=seq(0,120,10),label=seq(0,120,10)) }
  if(row==2 & column %in% c(1,2,3)) { grid.xaxis(at=seq(0,50,5),label=seq(0,50,5))   }
  if(row==2 & column ==4) { grid.xaxis(at=seq(0,100,10),label=seq(0,100,10))   }
  if(row==3 & column ==2) { grid.xaxis(at=seq(0,50,5),label=seq(0,50,5)) }
  if(row==3 & column %in% c(1,3)) { grid.xaxis(at=seq(0,100,10),label=seq(0,100,10))}
  
  if(row==1 & column %in% c(3,4)) grid.yaxis(at=seq(0,0.6,0.1),label=seq(0,60,10))
  if(row==2 & column==1) grid.yaxis(at=seq(0,0.7,0.1),label=seq(0,70,10))
  if(!(row==1 & column %in% c(3,4)) | (row==2 & column==1)) grid.yaxis(at=seq(0,0.5,0.1),label=seq(0,50,10))
  
  
  # labels
  grid.text('Proportion (%)',x=unit(-3,'lines'),rot=90)
  grid.text('Degree',y=unit(-2.5,'lines'))
  
  if(row == 1 & column == 1) grid.text('A',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 2) grid.text('B',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 3) grid.text('C',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 4) grid.text('D',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  if(row == 2 & column == 1) grid.text('E',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 2 & column == 2) grid.text('F',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 2 & column == 3) grid.text('G',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 2 & column == 4) grid.text('H',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  if(row == 3 & column == 1) grid.text('I',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 3 & column == 2) grid.text('J',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 3 & column == 3) grid.text('K',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 3 & column == 4) grid.text('L',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  # legend
  
  n_label = c('Cruise 1', 'Cruise 2', 'Cruise 3', 'Cruise 4', 'Community', 
              'School 1', 'School 2', 'School 3', 'Hospital', 'Workplace 1', 'Workplace 2')
  
  grid.text(n_label[n], x=unit(17,'lines'), y=unit(16.5,'lines'), just='right', gp=gpar(fontsize=unit(8,'pt')))
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  popViewport()
  popViewport()
  
}


png('figure/p_k_overall.png',height=8*3,width=8*4,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=3,ncol=4)))

paneller(1,1); paneller(1,2); paneller(1,3); paneller(1,4)
paneller(2,1); paneller(2,2); paneller(2,3); paneller(2,4)
paneller(3,1); paneller(3,2); paneller(3,3)

popViewport()
popViewport()
dev.off()

# p_k_log
paneller=function(row = 1,column=1)
{
  
  if(row==1) { xlm=log(c(1,1000),10) }
  if(row!=1) { xlm=log(c(1,100),10) }
  
  if(row==1) ylm=log(c(1,1000),10)
  if(row!=1) ylm=log(c(1,100),10)
  
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  n = column + 4*(row-1)
  degree = copy(net[[n]]$el)
  degree = unique(degree[,c('node_i', 'node_j')])
  degree = degree[,.N, by=node_i]
  hist_breaks = 1
  degree = hist(degree$N, breaks=seq(0,max(degree$N)+hist_breaks, hist_breaks), plot=FALSE)
  degree = data.table(min_deg=degree$breaks[-length(degree$breaks)], max_deg=degree$breaks[-1], 
                      counts=degree$counts, prop=degree$counts/sum(degree$counts))
  degree = degree[counts!=0]
  
  if(n %in% c(1:4)) col_line = col_hue_lancet[1]
  if(n %in% c(5)) col_line = col_hue_lancet[2]
  if(n %in% c(6:8)) col_line = col_hue_lancet[3]
  if(n %in% c(9)) col_line = col_hue_lancet[4]
  if(n %in% c(10:11)) col_line = col_hue_lancet[5]
  
  
  grid.points(log(degree$max_deg,10), log(degree$count,10), pch=15, default.units = 'native', gp=gpar(col=col_line,fill=col_line, cex=0.5))
  
  
  
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # axis
  if(row==1) { grid.xaxis(at=log(c(1,10,100,1000),10),label=expression(1,10,10^2,10^3)) }
  if(row!=1) { grid.xaxis(at=log(c(1,10,100),10),label=expression(1,10,10^2))   }
 
  if(row==1) grid.yaxis(at=log(c(1,10,100,1000),10),label=expression(1,10,10^2,10^3)) 
  if(row!=1) grid.yaxis(at=log(c(1,10,100),10),label=expression(1,10,10^2))
  
  
  # labels
  grid.text('Counts',x=unit(-3,'lines'),rot=90)
  grid.text('Degree',y=unit(-2.5,'lines'))
  
  if(row == 1 & column == 1) grid.text('A',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 2) grid.text('B',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 3) grid.text('C',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 4) grid.text('D',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  if(row == 2 & column == 1) grid.text('E',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 2 & column == 2) grid.text('F',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 2 & column == 3) grid.text('G',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 2 & column == 4) grid.text('H',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  if(row == 3 & column == 1) grid.text('I',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 3 & column == 2) grid.text('J',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 3 & column == 3) grid.text('K',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 3 & column == 4) grid.text('L',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  # legend
  
  n_label = c('Cruise 1', 'Cruise 2', 'Cruise 3', 'Cruise 4', 'Community', 
              'School 1', 'School 2', 'School 3', 'Hospital', 'Workplace 1', 'Workplace 2')
  
  grid.text(n_label[n], x=unit(17,'lines'), y=unit(16.5,'lines'), just='right', gp=gpar(fontsize=unit(8,'pt')))
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  popViewport()
  popViewport()
  
}


png('figure/p_k_overall_log.png',height=8*3,width=8*4,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=3,ncol=4)))

paneller(1,1); paneller(1,2); paneller(1,3); paneller(1,4)
paneller(2,1); paneller(2,2); paneller(2,3); paneller(2,4)
paneller(3,1); paneller(3,2); paneller(3,3)

popViewport()
popViewport()
dev.off()

