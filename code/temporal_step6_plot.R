source('code/temporal_library.R')
source('code/temporal_colour.R')
source('code/temporal_step4_load_results.R')

# p_k0
paneller=function(row = 1,column=1)
{
  
  xlm=c(0,10)
  ylm=c(0,1)
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  index = column + 4*(row-1)
  max_step = net[[index]]$max_step
  if(index %in% c(1:4)) col_line = col_hue_lancet[1]
  if(index %in% c(5)) col_line = col_hue_lancet[2]
  if(index %in% c(6:8)) col_line = col_hue_lancet[3]
  if(index %in% c(9)) col_line = col_hue_lancet[4]
  if(index %in% c(10:11)) col_line = col_hue_lancet[5]
  
  # plot every single distribution of k1|k0
  for(s in 1:max_step){
    
    data_temp = copy(results[[index]]$p_k0_temp[step==s]) # & k0!=0
    if(data_temp[,.N]==0) next
    
    if(data_temp[,.N]>=2 & data_temp[,sum(N)]>=10) grid.lines(data_temp[k0<=10]$k0, data_temp[k0<=10]$P, default.units = 'native',gp=gpar(col='gray90',lwd=0.75))
  
  }
  
  data_temp = copy(results[[index]]$p_k0_temp) 
  keep_step = data_temp[,.(.N, sum(N)), by=.(step)]
  keep_step = keep_step[N>=2 & V2>=10, step]
  if(length(keep_step)==0) next
  data_temp = data_temp[step %in% keep_step]
    
  n_step = uniqueN(data_temp$step)
  data_temp[, P:=P/n_step]
  data_temp = data_temp[, sum(P), by=.(k0)]
  setorder(data_temp, k0)
  grid.lines(data_temp[k0<=10]$k0, data_temp[k0<=10]$V1, default.units = 'native',gp=gpar(col=col_line,lwd=2))
    

  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # axis
  grid.xaxis(at=seq(0,10,2),label=seq(0,10,2))
  grid.yaxis(at=seq(0,1,0.25),label=seq(0,1,0.25))
  
  
  # labels
  grid.text('Probability (%)',x=unit(-3,'lines'),rot=90)
  grid.text(bquote(k[t]),y=unit(-2.5,'lines'))
  
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
  grid.lines(c(7.4,7.9), c(0.95,0.95), default.units = 'native', gp=gpar(col=col_line,lwd=2))
  
  if(index %in% c(1:4)) grid.text('Cruise', 8.15, 0.95, default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
  if(index %in% c(5)) grid.text('Community', 8.15, 0.95, default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
  if(index %in% c(6:8)) grid.text('School', 8.15, 0.95, default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
  if(index %in% c(9)) grid.text('Hospital', 8.15, 0.95, default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
  if(index %in% c(10:11)) grid.text('Work', 8.15, 0.95, default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
     
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  popViewport()
  popViewport()
  
}


png('figure/p_k0.png',height=8*3,width=8*4,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=3,ncol=4)))

paneller(1,1); paneller(1,2); paneller(1,3); paneller(1,4)
paneller(2,1); paneller(2,2); paneller(2,3); paneller(2,4)
paneller(3,1); paneller(3,2); paneller(3,3)

popViewport()
popViewport()
dev.off()


# p_k0_k1
paneller=function(row = 1,column=1)
{
  # if(column!=1) xlm=c(1,5)
  # if(column==1) xlm=c(0,10)
  xlm=c(0,10)
  ylm=c(0,1)
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  index = column + 4*(row-1)
  max_step = net[[index]]$max_step
  col_line = col_hue_lancet
  
  # if(column==1){
  
    # plot every single distribution of k1|k0
    for(s in 1:max_step){
      
      data_temp = copy(results[[index]]$p_k0_k1_temp[step==s]) # & k0!=0
      if(data_temp[,.N]==0) next
      
  
      # col_seq=c(4, 3, 8, 9, 6, 7, 5, 10)
      for(k in 1:5){
        # if(data_temp[k0==k-1,.N]>=2) grid.lines(data_temp[k0==k-1 & k1<=10]$k1, data_temp[k0==k-1 & k1<=10]$P, default.units = 'native',gp=gpar(col=col_line[col_seq[k]],lwd=0.75))
        if(data_temp[k0==k-1,.N]>=2 & data_temp[k0==k-1,sum(N)]>=10) grid.lines(data_temp[k0==k-1 & k1<=10]$k1, data_temp[k0==k-1 & k1<=10]$P, default.units = 'native',gp=gpar(col='gray90',lwd=0.75))
        
      }
    }
  
    for(k in 1:5){
      data_temp = copy(results[[index]]$p_k0_k1_temp[k0==k-1]) 
      keep_step = data_temp[,.(.N, sum(N)), by=.(step)]
      keep_step = keep_step[N>=2 & V2>=10, step]
      if(length(keep_step)==0) next
      data_temp = data_temp[step %in% keep_step]
      
      n_step = uniqueN(data_temp$step)
      data_temp[, P:=P/n_step]
      data_temp = data_temp[, sum(P), by=.(k1)]
      setorder(data_temp, k1)
      grid.lines(data_temp[k1<=10]$k1, data_temp[k1<=10]$V1, default.units = 'native',gp=gpar(col=col_line[k],lwd=2))
      
    }
  
  
  
  # }
  
  # if(column==2){
  #   for(s in 1:max_step){ 
  #     
  #     # data_temp = copy(results[[index]]$p_k0_r_temp[step==s & r==0 & k0!=0 & k0<=5])
  #     data_temp = copy(results[[index]]$p_k0_r_temp[step==s & k0!=0 & k0<=5])
  #     data_temp[,N_sum:=sum(N), by=.(k0)]
  #     data_temp = data_temp[r==0 & N_sum>=10]
  #     
  #     if(data_temp[,.N]==0) next
  #     
  #     
  #     grid.lines(data_temp$k0, data_temp$P, default.units = 'native',gp=gpar(col=col_line[4],lwd=0.75))
  #     # data_rand = results$p_k0_r_rand_avg[step==s & r==0 & k0<=5]
  #     # grid.lines(data_rand$k0, data_rand$P, default.units = 'native',gp=gpar(col='gray80',lwd=0.75))
  #      
  #   }
  # }
  # 
  # if(column==3){
  #   for(s in 1:max_step){ 
  #     
  #     # data_temp = copy(results[[index]]$p_k0_r_temp[step==s & r==k0 & k0!=0 & k0<=5])
  #     data_temp = copy(results[[index]]$p_k0_r_temp[step==s & k0!=0 & k0<=5])
  #     data_temp[,N_sum:=sum(N), by=.(k0)]
  #     data_temp = data_temp[r==k0 & N_sum>=10]
  #     
  #     if(data_temp[,.N]==0) next
  #     
  #     
  #     grid.lines(data_temp$k0, data_temp$P, default.units = 'native',gp=gpar(col=col_line[4],lwd=0.75))
  #     # data_rand = results$p_k0_r_rand_avg[step==s & r==k0 & k0<=5]
  #     # grid.lines(data_rand$k0, data_rand$P, default.units = 'native',gp=gpar(col='gray80',lwd=0.75))
  #     
  #   }
  # }
  
  
  
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # axis
  # if(column!=1) grid.xaxis(at=seq(1,5,1),label=seq(1,5,1))
  # if(column==1) grid.xaxis(at=seq(0,10,2),label=seq(0,10,2))
  grid.xaxis(at=seq(0,10,2),label=seq(0,10,2))
  grid.yaxis(at=seq(0,1,0.25),label=seq(0,1,0.25))
  
  
  # labels
  grid.text('Probability (%)',x=unit(-3,'lines'),rot=90)
  grid.text(bquote(k[t+1]),y=unit(-2.5,'lines'))
  
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
  grid.lines(c(8,8.5), c(0.95,0.95), default.units = 'native', gp=gpar(col=col_line[1],lwd=2))
  grid.lines(c(8,8.5), c(0.90,0.90), default.units = 'native', gp=gpar(col=col_line[2],lwd=2))
  grid.lines(c(8,8.5), c(0.85,0.85), default.units = 'native', gp=gpar(col=col_line[3],lwd=2))
  grid.lines(c(8,8.5), c(0.80,0.80), default.units = 'native', gp=gpar(col=col_line[4],lwd=2))
  grid.lines(c(8,8.5), c(0.75,0.75), default.units = 'native', gp=gpar(col=col_line[5],lwd=2))
  
  grid.text(bquote(k[t]==0), 8.75, 0.95, default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
  grid.text(bquote(k[t]==1), 8.75, 0.9, default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
  grid.text(bquote(k[t]==2), 8.75, 0.85, default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
  grid.text(bquote(k[t]==3), 8.75, 0.8, default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
  grid.text(bquote(k[t]==4), 8.75, 0.75, default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
  
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  popViewport()
  popViewport()
  
}


png('figure/p_k0_k1.png',height=8*3,width=8*4,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=3,ncol=4)))

paneller(1,1); paneller(1,2); paneller(1,3); paneller(1,4)
paneller(2,1); paneller(2,2); paneller(2,3); paneller(2,4)
paneller(3,1); paneller(3,2); paneller(3,3)

popViewport()
popViewport()
dev.off()


# p_r
paneller=function(row = 1,column=1)
{
  xlm=c(0,10); ylm=c(0,1) 
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  index = column+4*(row-1)
  max_step = net[[index]]$max_step
  
  if(index %in% c(1:4)) col_line = col_hue_lancet[1]
  if(index %in% c(5)) col_line = col_hue_lancet[2]
  if(index %in% c(6:8)) col_line = col_hue_lancet[3]
  if(index %in% c(9)) col_line = col_hue_lancet[4]
  if(index %in% c(10:11)) col_line = col_hue_lancet[5]
  
  for(s in 1:max_step){
    
    data_temp = copy(results[[index]]$p_r_temp[step==s & r<=10])
    # data_stat = copy(results[[index]]$p_r_stat[step==s & r<=10])
      
    if(data_temp[,.N]==0) next
    grid.lines(data_temp$r, data_temp$P, default.units = 'native',gp=gpar(col='gray90',lwd=0.75))
    # grid.lines(data_stat$r, data_stat$P, default.units = 'native',gp=gpar(col=col_line[3],lwd=0.75))
      
  }
  
  data_temp = copy(results[[index]]$p_r_temp) 
  # keep_step = data_temp[,.(.N, sum(N)), by=.(step)]
  # keep_step = keep_step[N>=2 & V2>=10, step]
  # if(length(keep_step)==0) next
  # data_temp = data_temp[step %in% keep_step]
  
  n_step = uniqueN(data_temp$step)
  data_temp[, P:=P/n_step]
  data_temp = data_temp[, sum(P), by=.(r)]
  setorder(data_temp, r)
  grid.lines(data_temp[r<=10]$r, data_temp[r<=10]$V1, default.units = 'native',gp=gpar(col=col_line,lwd=2))
  
  
    
  # mean and IQR
  # data_temp = results[[index]]$p_r_temp
  # data_temp = data_temp[,sum(r_P), by=.(step)]
  # grid.lines(c(quantile(data_temp$V1, c(0.025, 0.975))), c(0.875,0.875), 
  #              default.units = 'native',gp=gpar(col=col_line[4],lwd=0.75))
  # grid.points(c(quantile(data_temp$V1, 0.5)),c(0.875), pch=16,
  #             default.units = 'native',gp=gpar(col=col_line[4],fill=col_line[4], cex=0.3))
  #   
  # data_stat = results[[index]]$p_r_stat
  # data_stat = data_stat[,sum(r_P), by=.(step)]
  # grid.lines(c(quantile(data_stat$V1, c(0.025, 0.975))), c(0.925,0.925), 
  #            default.units = 'native',gp=gpar(col=col_line[3],lwd=0.75))
  # grid.points(c(quantile(data_stat$V1, 0.5)),c(0.925), pch=16,
  #             default.units = 'native',gp=gpar(col=col_line[3],fill=col_line[3], cex=0.3))
    

  
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # axis
  grid.xaxis(at=seq(0,10,2),label=seq(0,10,2))
  grid.yaxis(at=seq(0,1,0.25),label=seq(0,1,0.25))
  
  
  
  # labels
  # if(column==1){
  #   grid.text('Probability (%)',x=unit(-3,'lines'),rot=90)
  # }
  
  if(row == 1 & column == 1) grid.text('A',x=unit(-2,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 2) grid.text('B',x=unit(-2,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 3) grid.text('C',x=unit(-2,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 4) grid.text('D',x=unit(-2,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  if(row == 2 & column == 1) grid.text('D',x=unit(-2,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 2 & column == 2) grid.text('E',x=unit(-2,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 2 & column == 3) grid.text('F',x=unit(-2,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 2 & column == 4) grid.text('G',x=unit(-2,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  if(row == 3 & column == 1) grid.text('H',x=unit(-2,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 3 & column == 2) grid.text('I',x=unit(-2,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 3 & column == 3) grid.text('J',x=unit(-2,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  # legend
  grid.lines(c(7.4,7.9), c(0.95,0.95), default.units = 'native', gp=gpar(col=col_line,lwd=2))
  
  if(index %in% c(1:4)) grid.text('Cruise', 8.15, 0.95, default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
  if(index %in% c(5)) grid.text('Community', 8.15, 0.95, default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
  if(index %in% c(6:8)) grid.text('School', 8.15, 0.95, default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
  if(index %in% c(9)) grid.text('Hospital', 8.15, 0.95, default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
  if(index %in% c(10:11)) grid.text('Work', 8.15, 0.95, default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
  
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  popViewport()
  popViewport()
  
}

png('figure/p_r.png',height=8*3,width=8*4,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=3,ncol=4)))

paneller(1,1); paneller(1,2); paneller(1,3); paneller(1,4)
paneller(2,1); paneller(2,2); paneller(2,3); paneller(2,4)
paneller(3,1); paneller(3,2); paneller(3,3)

grid.text('Probability (%)',x=unit(-1.5,'lines'),rot=90)
grid.text('No. of contacts retained',y=unit(-1,'lines'))

popViewport()
popViewport()
dev.off()


# ridge plots
paneller=function(row = 1,column=1)
{
  xlm=c(0,1); ylm=c(0,0.25) 
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # draw ridgeline axis
  for(j in 1:5){
    yy = ylm[2]/5*(j-1)
    grid.lines(x=c(0,xlm[2]), y=c(yy,yy), default.units = 'native',gp=gpar(col='#FAFAFA',lwd=1))
  }
  
  if(column==1){
    for(n in 1:11){
      
      if(n %in% c(1:4)) {col_line=col_hue_lancet[1]; y_shift = 0.2}
      if(n==5) {col_line=col_hue_lancet[2]; y_shift = 0.15}
      if(n %in% c(6:8)) {col_line=col_hue_lancet[3]; y_shift = 0.1}
      if(n==9) {col_line=col_hue_lancet[4]; y_shift = 0.05}
      if(n %in% c(10,11)) {col_line=col_hue_lancet[5]; y_shift = 0}
      
      
      data_norm = results[[n]]$r_scale$scale_norm2
      # data_norm = hist(data_norm, breaks=seq(-0.5,1,0.05), plot=F)
      # data_norm = data.table(breaks=data_norm$breaks[-1], 
      #                        counts=data_norm$counts)
      # data_norm = data_norm[breaks>=0.1]
      # data_norm[, freq:=counts/sum(counts)]
      
      adj=1  
      data = data_norm[data_norm>0]
      data = density(data, adjust=adj, n=50, from=0, to=1)
      data = data.table(r=data$x, freq=data$y/sum(data$y) + y_shift)
      
      grid.lines(data$r, data$freq, default.units = 'native',gp=gpar(col=col_line))
      grid.polygon(c(data$r, rev(data$r)), c(data$freq, rep(y_shift, times=data[,.N])), 
                   default.units = 'native',gp=gpar(col=NA, fill=lightup(col_line, 0.3)))
      
      
   
    }
    
  }
  
  
  if(column==2){
    for(n in 1:11){
      
      if(n %in% c(1:4)) {y_shift = 0.2}
      if(n==5) {y_shift = 0.15}
      if(n %in% c(6:8)) {y_shift = 0.1}
      if(n==9) {y_shift = 0.05}
      if(n %in% c(10,11)) {y_shift = 0.0}
      
      
      
     
      
      # col_points = c("#440154FF", "#21908CFF", "#FDE725FF", "#5DC863FF", "#3B528BFF")# viridis_pal()(5)
      col_points = col_hue_lancet[c(1,5,2,3,4)]
     
      shape_points = c(21,22,24,23,25)
      type = sort(unique(results[[n]]$r_contact_type$contact_type))
      if(n==5) type=type[1:2]
      y_legend = seq(0.005*length(type), 0.005, -0.005)
      
      for(t in 1:length(type)){
        
        if(!(n==5 & t==2)){
          grid.points(results[[n]]$r_contact_type[contact_type==t & next_contact==1]$r,
                      results[[n]]$r_contact_type[contact_type==t & next_contact==1]$P_total*0.048 + y_shift, 
                      default.units = 'native', pch=shape_points[t], gp=gpar(col=lightup(col_points[t],0.1), fill=lightup(col_points[t], 0.3), cex=0.3))
          
        }
        
        if(n==5 & t==2){
          grid.points(results[[n]]$r_contact_type[contact_type==t & next_contact==1]$r,
                      results[[n]]$r_contact_type[contact_type %in% c(t,t+1) & next_contact==1, sum(P_total), by=.(step)]$V1*0.048 + y_shift, 
                      default.units = 'native', pch=shape_points[t], gp=gpar(col=lightup(col_points[t],0.1), fill=lightup(col_points[t], 0.3), cex=0.3))
          
        }
        
        # label
        if(n %in% c(1,5,6,9,10)) { 
          grid.points(0, y_shift + y_legend[t], default.units = 'native',
                      pch=shape_points[t], gp=gpar(col=lightup(col_points[t], 0.3), fill=lightup(col_points[t], 0.3), cex=0.3))
          
          if(n==1 & t==1) grid.text('Type of contact', -0.02, y_shift + y_legend[t] + 0.005, default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
          if(n==1 & t==1) grid.text('P-P (same cabin)', 0.03, y_shift + y_legend[t], default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
          if(n==1 & t==2) grid.text('P-P (diff cabin)', 0.03, y_shift + y_legend[t], default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
          if(n==1 & t==3) grid.text('C-C (same dept)', 0.03, y_shift + y_legend[t], default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
          if(n==1 & t==4) grid.text('C-C (diff dept)', 0.03, y_shift + y_legend[t], default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
          if(n==1 & t==5) grid.text('P-C', 0.03, y_shift + y_legend[t], default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
          
          if(n==5 & t==1) grid.text('Type of contact', -0.02, y_shift + y_legend[t] + 0.005, default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
          if(n==5 & t==1) grid.text('Household ', 0.03, y_shift + y_legend[t], default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
          if(n==5 & t==2) grid.text('Non-household ', 0.03, y_shift + y_legend[t], default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
          # if(n==5 & t==3) grid.text('Unknown', 0.03, y_shift + y_legend[t], default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
          
          if(n==6 & t==1) grid.text('Type of contact', -0.02, y_shift + y_legend[t] + 0.005, default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
          if(n==6 & t==1) grid.text('Classmates', 0.03, y_shift + y_legend[t], default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
          if(n==6 & t==2) grid.text('Non-classmates', 0.03, y_shift + y_legend[t], default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
          
          if(n %in% c(9,10) & t==1) grid.text('Type of contact', -0.02, y_shift + y_legend[t] + 0.005, default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
          if(n %in% c(9,10) & t==1) grid.text('Same dept', 0.03, y_shift + y_legend[t], default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
          if(n %in% c(9,10) & t==2) grid.text('Diff dept', 0.03, y_shift + y_legend[t], default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
          
          # y-axis
          # grid.yaxis(at=seq(y_shift,y_shift+0.048,0.024),label=c(0,50,100), gp=gpar(fontsize=unit(8,'pt')))
          grid.lines(c(-0.04,-0.04), c(y_shift, y_shift+0.048), default.units = 'native',gp=gpar(col='black'))
          grid.lines(c(-0.06,-0.04), c(y_shift,y_shift), default.units = 'native',gp=gpar(col='black'))
          grid.lines(c(-0.06,-0.04), c(y_shift+0.024,y_shift+0.024), default.units = 'native',gp=gpar(col='black'))
          grid.lines(c(-0.06,-0.04), c(y_shift+0.048,y_shift+0.048), default.units = 'native',gp=gpar(col='black'))
          if(n!=10) grid.text('0',x=-0.1, y=y_shift+0.001, default.units = 'native', gp=gpar(fontsize=unit(7,'pt')))
          if(n==10)grid.text('0',x=-0.1, y=y_shift, default.units = 'native', gp=gpar(fontsize=unit(7,'pt')))
          grid.text('50',x=-0.1, y=y_shift+0.024, default.units = 'native', gp=gpar(fontsize=unit(7,'pt')))
          if(n!=1) grid.text('100',x=-0.1, y=y_shift+0.048-0.001, default.units = 'native', gp=gpar(fontsize=unit(7,'pt')))
          if(n==1) grid.text('100',x=-0.1, y=y_shift+0.048, default.units = 'native', gp=gpar(fontsize=unit(7,'pt')))
          if(n==6) grid.text('Proportion (%)',x=unit(-2.5,'lines'),rot=90)
          
          
        }
        
      }
      
    }
    
  }
  
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # axis
  grid.xaxis(at=seq(0,1,0.25),label=seq(0,1,0.25))
  # grid.yaxis(at=seq(0,0.1,0.02),label=seq(0,0.1,0.02))
  
  
  
  # labels
  # grid.text('Density',x=unit(-3.5,'lines'),rot=90)
  grid.text(bquote('Retention index, '~ bar(r)),y=unit(-2.5,'lines'))
  
  if(column==1){
    y_shift=0.002
    grid.text('Cruise', x=-0.03, y=0.2+y_shift, default.units = 'native', just='right', gp=gpar(fontsize=unit(8,'pt')))
    grid.text('Community', x=-0.03, y=0.15+y_shift, default.units = 'native', just='right', gp=gpar(fontsize=unit(8,'pt')))
    grid.text('School', x=-0.03, y=0.1+y_shift, default.units = 'native', just='right', gp=gpar(fontsize=unit(8,'pt')))
    grid.text('Hospital', x=-0.03, y=0.05+y_shift, default.units = 'native', just='right', gp=gpar(fontsize=unit(8,'pt')))
    grid.text('Work', x=-0.03, y=0+y_shift, default.units = 'native', just='right', gp=gpar(fontsize=unit(8,'pt')))
  }
  
  if(row == 1 & column == 1) grid.text('A',x=unit(-2,'lines'),y=unit(26,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 2) grid.text('B',x=unit(-2,'lines'),y=unit(26,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  # grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  grid.lines(c(0,1),c(0,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  popViewport()
  popViewport()
  
}


# height = 16
png('figure/ridge_interactions_supp_fifteen_min.png',height=16,width=8*2,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(3,2,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=1,ncol=2)))

paneller(1,1)
paneller(1,2)

# grid.text('Fig 1. Contacts patterns in different settings, (a) distribution of contact repetition, r, over consecutive timesteps,\n(b) proportion of each type of contact retained for respective r', 
#           x=0, y=-0.05, default.units = 'native', just='left', gp=gpar(fontsize=unit(8,'pt')))


popViewport()
popViewport()
dev.off()


paneller=function(row = 1,column=1)
{
  n = column + (row-1)*4
  xlm=c(min(results[[n]]$r_scale$step),max(results[[n]]$r_scale$step)) 
  ylm=c(0,1) 
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  day_end = net[[n]]$steps_length$max_step
  
  # separate the days
  for(i in 1:(length(day_end)-1)){
    grid.lines(x=c(day_end[i],day_end[i]), y=ylm, default.units = 'native',gp=gpar(col='#FAFAFA',lwd=1))
  }
  
  # col line
  if(n %in% c(1:4)) {col_line=col_hue_lancet[1]}
  if(n==5) {col_line=col_hue_lancet[2]}
  if(n %in% c(6:8)) {col_line=col_hue_lancet[3]}
  if(n==9) {col_line=col_hue_lancet[4]}
  if(n %in% c(10,11)) {col_line=col_hue_lancet[5]}
  
  data = results[[n]]$r_scale
  setorder(data, step)
  
  for(d in 1:net[[n]]$steps_length[,.N]){
    
    min_step = net[[n]]$steps_length$min_step[d]
    max_step = net[[n]]$steps_length$max_step[d]
    
    check_diff = diff(data[step %in% min_step:max_step]$step)
    if(length(which(check_diff>50))!=0){
      
      cut_step = which(check_diff>50)
      end_step = length(data[step %in% min_step:max_step]$step)
      grid.lines(data[step %in% min_step:max_step]$step[1:cut_step], data[step %in% min_step:max_step]$scale_norm2[1:cut_step], default.units = 'native',gp=gpar(col=col_line))
      grid.lines(data[step %in% min_step:max_step]$step[(cut_step+1):end_step], data[step %in% min_step:max_step]$scale_norm2[(cut_step+1):end_step], default.units = 'native',gp=gpar(col=col_line))
      
    }else{
      grid.lines(data[step %in% min_step:max_step]$step, data[step %in% min_step:max_step]$scale_norm2, default.units = 'native',gp=gpar(col=col_line))
    }
    
  }
  
  
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # axis
  grid.xaxis(at=c(0,net[[n]]$steps_length$max_step),label=seq(0,length(net[[n]]$steps_length$day),1))
  grid.yaxis(at=seq(0,1,0.2),label=seq(0,1,0.2))
  
  
  # labels
  grid.text(bquote(bar(r)),x=unit(-3,'lines'),rot=90)
  grid.text('Day',y=unit(-2.5,'lines'))
  
  n_label = c('Cruise 1', 'Cruise 2', 'Cruise 3', 'Cruise 4', 'Community', 
              'High school 1', 'High school 2', 'High school 3', 
              'Hospital', 'Workplace 1', 'Workplace 2')
  
  grid.text(n_label[n], x=unit(0.1,'lines'), y=unit(0.7,'lines'), just='left', gp=gpar(fontsize=unit(8,'pt')))
  
  if(n==5) {col_line=col_hue_lancet[2]}
  if(n %in% c(6:8)) {col_line=col_hue_lancet[3]}
  if(n==9) {col_line=col_hue_lancet[4]}
  if(n %in% c(10,11)) {col_line=col_hue_lancet[5]}
  
  
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
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  popViewport()
  popViewport()
  
}



png('figure/r_correlation_supp.png',height=8*3,width=8*4,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=3,ncol=4)))

paneller(1,1)
paneller(1,2)
paneller(1,3)
paneller(1,4)

paneller(2,1)
paneller(2,2)
paneller(2,3)
paneller(2,4)

paneller(3,1)
paneller(3,2)
paneller(3,3)


popViewport()
popViewport()
dev.off()


# superspreading
paneller=function(row=1,column=1)
{
  
  xlm=c(0,1); ylm=c(0,1)
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # plot static, random characteristics
  
  # static
  p80 = c(0.8, 0.5) # 0.8, 0.5
  max_step=c(25,10)  # 25, 10
  line_type = c('dashed', 'dotdash', 'dotted')
  
  data = data.table(prop_steps = c(1,0),
                    pmf_nodes = c(p80[1],(1-p80[1])),
                    cmf_nodes = c(p80[1],1))
  
  grid.lines(c(0,data[1,]$cmf_nodes),c(1,1), default.units = 'native',gp=gpar(col='grey90', lty=line_type[3]))
  grid.lines(c(data[1,]$cmf_nodes,data[1,]$cmf_nodes),c(1,0), default.units = 'native',gp=gpar(col='grey90', lty=line_type[3]))
  grid.lines(c(data[1,]$cmf_nodes,data[2,]$cmf_nodes),c(0,0), default.units = 'native',gp=gpar(col='grey90', lty=line_type[3]))
  
  # random 
  for(m in 1:2){
    
    # random
    data = data.table(prop_steps = seq(max_step[m],1,-1)/max_step[m],
                      pmf_nodes = p80[m]^seq(max_step[m],1,-1))
    data[, cmf_nodes:=cumsum(pmf_nodes)]
    if(sum(data$pmf_nodes)<1) data = rbind(data, data.table(prop_steps=0,
                                                            pmf_nodes=1-sum(data$pmf_nodes),
                                                            cmf_nodes=1))
    if(sum(data$pmf_nodes)>1 & data[cmf_nodes<=1, max(cmf_nodes)]!=1){
      index = max(data[cmf_nodes<=1, which=T])
      data[index+1]$cmf_nodes = 1
      data[index+1]$pmf_nodes = 1-data[index]$cmf_nodes
      if(max_step[m]>3) data[(index+2):data[,.N]]$pmf_nodes = 0
      if(max_step[m]>3) data[(index+2):data[,.N]]$cmf_nodes = 1
    }
    
    if(max(data$cmf_nodes)!=1) data = rbind(data.table(prop_steps=0, cmf_nodes = 1), data)
    
    
    adj=3
    data = rep(data[cmf_nodes<=1]$prop_steps, times=data[cmf_nodes<=1]$pmf_nodes*1000)
    data = density(data, adjust=adj, n=500, from=min(data), to=1)
    data = data.table(prop_steps=data$x, cmf_nodes=data$y)
    data[, cmf_nodes:=1-cumsum(cmf_nodes)/sum(cmf_nodes)]
    
    
    grid.lines(data$cmf_nodes,data$prop_steps,
               default.units = 'native',gp=gpar(col='grey90', lty=line_type[m]))
    
    
  }
  
  # combine cruises, school and work into one main line with median, min, max
  data_temp = data.table()
  dens_temp = data.table()
  
  for(n in 1:11){
    index = n
    max_step = net[[index]]$max_step
    n_nodes = net[[index]]$n_nodes_total
    
    if(column==1) {data = copy(results[[index]]$node_r80_ct_esp)}
    if(column==2) {data = copy(results[[index]]$node_r80_ct_dur)}
    data = data[, .N, by=.(N)]
    setnames(data, c('steps', 'N'))
    data = rbind(data, data.table(steps=0, N=n_nodes-sum(data$N)))
    
    recorded_step = sum(net[[n]]$steps_length$length)
    
    data[, prop_steps:=steps/recorded_step]
    data[, pmf_nodes:=N/sum(N)]
    data[, cmf_nodes:=cumsum(pmf_nodes)]
    if(max(data$steps) < max_step) data = rbindlist(list(data, data.table(N=0,prop_steps=1, cmf_nodes=0)), fill=T)
    
    data[, net:=n]
    data_temp = rbind(data_temp,data)
    
    adj=3
    data_dens = rep(data$prop_steps, times=data$N)
    data_dens = density(data_dens, adjust=adj, n=500, from=0, to=1)
    data_dens = data.table(prop_steps=data_dens$x, pmf_nodes=data_dens$y, net=n)
    data_dens[, pmf_nodes:=pmf_nodes/sum(pmf_nodes)]
    data_dens[, cmf_nodes:=1-cumsum(pmf_nodes)/sum(pmf_nodes)]
    
    dens_temp = rbind(dens_temp, data_dens)
    
    
  }
  
  dens_temp_combine = data.table()
  net_name = c('cruise', 'community', 'school', 'hospital', 'work')
  
  for(n in 1:5){
    
    if(n==1){ data_dens = dens_temp[net%in%c(1:4), c('prop_steps', 'cmf_nodes')] }
    if(n==2){ data_dens = dens_temp[net==5, c('prop_steps', 'cmf_nodes')] }
    if(n==3){ data_dens = dens_temp[net%in%c(6:8), c('prop_steps', 'cmf_nodes')] }
    if(n==4){ data_dens = dens_temp[net==9, c('prop_steps', 'cmf_nodes')] }
    if(n==5){ data_dens = dens_temp[net%in%c(10:11), c('prop_steps', 'cmf_nodes')] }
    
    data_dens = data_dens[, .(med=median(cmf_nodes),
                              lwr=min(cmf_nodes), 
                              upp=max(cmf_nodes)), by=.(prop_steps)]
    data_dens[, net:=net_name[n]]
    
    dens_temp_combine = rbind(dens_temp_combine, data_dens)
    
  }
  
  # density temporal
  col_line = col_hue_lancet # col_hue_nature[c(4, 3, 8, 9, 6)]
  net_name = c('cruise', 'community', 'school', 'hospital', 'work')
  y_legend = seq(0.95,0.75,-0.05)
  
  for(n in 1:5){
    
    data = dens_temp_combine[net==net_name[n]]
    
    grid.lines(data$med, data$prop_steps, default.units = 'native',gp=gpar(col=col_line[n]))
    
    
    grid.polygon(c(data$lwr, rev(data$upp)),
                 c(data$prop_steps, rev(data$prop_steps)),
                 gp=gpar(col=NA,fill=lightup(col_line[n], 0.3)),default.units = 'native')
    
    # legend
    grid.lines(c(0.76,0.79),c(y_legend[n], y_legend[n]), default.units = 'native',gp=gpar(col=col_line[n]))
    grid.text(net_name[n], 0.81,y_legend[n], default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
    
    
    # draw dots
    if(n %in% c(1)){

      col_point_50 = 'grey85' # col_hue_nature[8]
      col_point_25 = 'grey60' # col_hue_nature[5]

      yy = 0.501002004
      xx = data[prop_steps>0.5 & prop_steps<0.5011, med]
      grid.points(xx, yy, default.units = 'native',pch=24, 
                  gp=gpar(col=col_point_50, cex=0.5)) # fill=col_point_50
      
      

      yy = 0.25050100
      xx = data[prop_steps>=0.25 & prop_steps<0.25051,  med]
      grid.points(xx, yy, default.units = 'native',pch=21, 
                  gp=gpar(col=col_point_50,cex=0.5)) #fill=col_point_50
      
    }
    
  }
  
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # axis
  grid.xaxis(at=seq(0,1,0.25),label=seq(0,1,0.25),gp=gpar(fontsize=unit(7,'pt')))
  grid.yaxis(at=seq(0,1,0.25),label=seq(0,1,0.25),gp=gpar(fontsize=unit(7,'pt')))
  

  # labels
  grid.text('Proportion of steps',x=unit(-3,'lines'),rot=90)
  grid.text('Proportion of nodes',y=unit(-2.5,'lines'))
  
  if(row == 1 & column == 1) grid.text('A',x=unit(-2.5,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 2) grid.text('B',x=unit(-2.5,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  # if(row == 2 & column == 1) grid.text('C',x=unit(-2.5,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  # if(row == 2 & column == 2) grid.text('D',x=unit(-2.5,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  # if(row==1 & column==1) {
  #   grid.lines(c(0,0.5),c(0,0))
  #   grid.lines(c(1,1,0.5),c(0,1,1))
  # }
  # if(row==1 & column==2) {
  grid.lines(c(0,0.8),c(0,0))
  grid.lines(c(1,1,0.8),c(0,1,1))
  # }
  # if(row==2) grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  popViewport()
  popViewport()
  
  
}


png('figure/superspreading.png',height=8,width=8*2,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=1,ncol=2)))

paneller(1,1)
paneller(1,2)
# paneller(2,1)
# paneller(2,2)

# grid.text('Proportion of nodes',y=unit(-1,'lines'))


popViewport()
popViewport()
dev.off()

rm(paneller)


# superspreading by day
paneller=function(row=1,column=1)
{
  xlm=c(0,1); ylm=c(0,1)
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # plot static, random characteristics
  
  # static
  p80 = c(0.8, 0.5) # 0.8, 0.5
  max_step=c(25,10)  # 25, 10
  line_type = c('dashed', 'dotdash', 'dotted')
  
  data = data.table(prop_steps = c(1,0),
                    pmf_nodes = c(p80[1],(1-p80[1])),
                    cmf_nodes = c(p80[1],1))
  
  grid.lines(c(0,data[1,]$cmf_nodes),c(1,1), default.units = 'native',gp=gpar(col='grey90', lty=line_type[3]))
  grid.lines(c(data[1,]$cmf_nodes,data[1,]$cmf_nodes),c(1,0), default.units = 'native',gp=gpar(col='grey90', lty=line_type[3]))
  grid.lines(c(data[1,]$cmf_nodes,data[2,]$cmf_nodes),c(0,0), default.units = 'native',gp=gpar(col='grey90', lty=line_type[3]))
  
  # random 
  for(m in 1:2){
    
    # random
    data = data.table(prop_steps = seq(max_step[m],1,-1)/max_step[m],
                      pmf_nodes = p80[m]^seq(max_step[m],1,-1))
    data[, cmf_nodes:=cumsum(pmf_nodes)]
    if(sum(data$pmf_nodes)<1) data = rbind(data, data.table(prop_steps=0,
                                                            pmf_nodes=1-sum(data$pmf_nodes),
                                                            cmf_nodes=1))
    if(sum(data$pmf_nodes)>1 & data[cmf_nodes<=1, max(cmf_nodes)]!=1){
      index = max(data[cmf_nodes<=1, which=T])
      data[index+1]$cmf_nodes = 1
      data[index+1]$pmf_nodes = 1-data[index]$cmf_nodes
      if(max_step[m]>3) data[(index+2):data[,.N]]$pmf_nodes = 0
      if(max_step[m]>3) data[(index+2):data[,.N]]$cmf_nodes = 1
    }
    
    if(max(data$cmf_nodes)!=1) data = rbind(data.table(prop_steps=0, cmf_nodes = 1), data)
    
    
    adj=3
    data = rep(data[cmf_nodes<=1]$prop_steps, times=data[cmf_nodes<=1]$pmf_nodes*1000)
    data = density(data, adjust=adj, n=500, from=min(data), to=1)
    data = data.table(prop_steps=data$x, cmf_nodes=data$y)
    data[, cmf_nodes:=1-cumsum(cmf_nodes)/sum(cmf_nodes)]
    
    
    grid.lines(data$cmf_nodes,data$prop_steps,
               default.units = 'native',gp=gpar(col='grey90', lty=line_type[m]))
    
    
  }
  
  # combine cruises, school and work into one main line with median, min, max
  data_temp = data.table()
  dens_temp = data.table()
  
  for(n in 1:11){
    index = n
    max_step = net[[index]]$max_step
    n_nodes = net[[index]]$n_nodes_total
    
    if(column==1) {data = copy(results[[index]]$node_r80_day_ct_esp)}
    if(column==2) {data = copy(results[[index]]$node_r80_day_ct_dur)}
    data = rbind(data, data.table(day=0, N=n_nodes-sum(data$N)))
    
    recorded_day = max(net[[n]]$steps_length$day)
    
    data[, prop_days:=day/recorded_day]
    data[, pmf_nodes:=N/sum(N)]
    data[, cmf_nodes:=cumsum(pmf_nodes)]
    if(max(data$day) < recorded_day) data = rbindlist(list(data, data.table(N=0,prop_days=1, cmf_nodes=0)), fill=T)
    
    data[, net:=n]
    data_temp = rbind(data_temp,data)
    
    adj=3
    data_dens = rep(data$prop_days, times=data$N)
    data_dens = density(data_dens, adjust=adj, n=500, from=0, to=1)
    data_dens = data.table(prop_days=data_dens$x, cmf_nodes=data_dens$y, net=n)
    data_dens[, cmf_nodes:=1-cumsum(cmf_nodes)/sum(cmf_nodes)]
    
    dens_temp = rbind(dens_temp, data_dens)
    
    
  }
  
  dens_temp_combine = data.table()
  net_name = c('cruise', 'community', 'school', 'hospital', 'work')
  
  for(n in 1:5){
    
    if(n==1){ data_dens = dens_temp[net%in%c(1:4), c('prop_days', 'cmf_nodes')] }
    if(n==2){ data_dens = dens_temp[net==5, c('prop_days', 'cmf_nodes')] }
    if(n==3){ data_dens = dens_temp[net%in%c(6:8), c('prop_days', 'cmf_nodes')] }
    if(n==4){ data_dens = dens_temp[net==9, c('prop_days', 'cmf_nodes')] }
    if(n==5){ data_dens = dens_temp[net%in%c(10:11), c('prop_days', 'cmf_nodes')] }
    
    data_dens = data_dens[, .(med=median(cmf_nodes),
                              lwr=min(cmf_nodes), 
                              upp=max(cmf_nodes)), by=.(prop_days)]
    data_dens[, net:=net_name[n]]
    
    dens_temp_combine = rbind(dens_temp_combine, data_dens)
    
  }
  
  # density temporal
  col_line = col_hue_lancet # col_hue_nature[c(4, 3, 8, 9, 6)]
  net_name = c('cruise', 'community', 'school', 'hospital', 'work')
  y_legend = seq(0.95,0.75,-0.05)
  
  for(n in 1:5){
    
    data = dens_temp_combine[net==net_name[n]]
    
    grid.lines(data$med, data$prop_days, default.units = 'native',gp=gpar(col=col_line[n]))
    
    
    grid.polygon(c(data$lwr, rev(data$upp)),
                 c(data$prop_days, rev(data$prop_days)),
                 gp=gpar(col=NA,fill=lightup(col_line[n], 0.3)),default.units = 'native')
    
    # legend
    grid.lines(c(0.76,0.79),c(y_legend[n], y_legend[n]), default.units = 'native',gp=gpar(col=col_line[n]))
    grid.text(net_name[n], 0.81,y_legend[n], default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
    
    
    # draw dots
    if(n %in% c(1)){

      col_point_50 = 'grey85' # col_hue_nature[8]
        col_point_25 = 'grey60' # col_hue_nature[5]

        yy = 0.501002004
        xx = data[prop_days>0.5 & prop_days<0.5011, med]
        grid.points(xx, yy, default.units = 'native',pch=24,
                    gp=gpar(col=col_point_50, cex=0.5)) # fill=col_point_50



        yy = 0.25050100
        xx = data[prop_days>=0.25 & prop_days<0.25051,  med]
        grid.points(xx, yy, default.units = 'native',pch=21,
                    gp=gpar(col=col_point_50,cex=0.5)) #fill=col_point_50

    }
    
  }
  
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # axis
  grid.xaxis(at=seq(0,1,0.25),label=seq(0,1,0.25),gp=gpar(fontsize=unit(7,'pt')))
  grid.yaxis(at=seq(0,1,0.25),label=seq(0,1,0.25),gp=gpar(fontsize=unit(7,'pt')))
  
  
  # labels
  grid.text('Proportion of days',x=unit(-3,'lines'),rot=90)
  grid.text('Proportion of nodes',y=unit(-2.5,'lines'))
  
  if(row == 1 & column == 1) grid.text('A',x=unit(-2.5,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 2) grid.text('B',x=unit(-2.5,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  # if(row == 2 & column == 1) grid.text('C',x=unit(-2.5,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  # if(row == 2 & column == 2) grid.text('D',x=unit(-2.5,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  # if(row==1 & column==1) {
  #   grid.lines(c(0,0.5),c(0,0))
  #   grid.lines(c(1,1,0.5),c(0,1,1))
  # }
  # if(row==1 & column==2) {
  grid.lines(c(0,0.8),c(0,0))
  grid.lines(c(1,1,0.8),c(0,1,1))
  # }
  # if(row==2) grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  popViewport()
  popViewport()
  
  
}


png('figure/superspreading_day.png',height=8,width=8*2,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=1,ncol=2)))

paneller(1,1)
paneller(1,2)
# paneller(2,1)
# paneller(2,2)

# grid.text('Proportion of nodes',y=unit(-1,'lines'))


popViewport()
popViewport()
dev.off()

rm(paneller)


# relative changes in unique contacts
# differences in the total contacts vs total unique contacts 
paneller=function(row=1,column=1)
{
  xlm=c(0.5,10.5); ylm=c(0,1.5)
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  data_diff = lapply(1:11, function(x){
    results[[x]]$contact_diff
  })
  data_diff = rbindlist(data_diff)
  
  for(net_type in 1:5){
    
    if(net_type == 1) {max_day=3; n=1:4; prop_steps=data_diff[net==1]$prop_steps; shape=21; x_shift=-0.3}
    if(net_type == 2) {max_day=3; n=5; prop_steps=data_diff[net==5]$prop_steps; shape=22; x_shift=-0.15}
    if(net_type == 3) {max_day=7; n=7; prop_steps=data_diff[net==7]$prop_steps; shape=23; x_shift=0}
    if(net_type == 4) {max_day=5; n=9; prop_steps=data_diff[net==9]$prop_steps; shape=24; x_shift=0.15}
    if(net_type == 5) {max_day=10; n=10:11; prop_steps=data_diff[net==10]$prop_steps; shape=25; x_shift=0.3}
  
    col_line=col_hue_lancet[net_type]
    
    med_diff = sapply(1:max_day, function(x){ median(data_diff[day==x & net %in% n]$med_diff) })
    min_diff = sapply(1:max_day, function(x){ min(data_diff[day==x & net %in% n]$lwr_diff) })
    max_diff = sapply(1:max_day, function(x){ max(data_diff[day==x & net %in% n]$upp_diff) })
    
    # if(net_type %in% c(3:5)){
    #   med_diff=predict(smooth.spline(1:max_day, med_diff), seq(1,max_day,1)); med_diff=med_diff$y
    #   min_diff=predict(smooth.spline(1:max_day, min_diff), seq(1,max_day,1)); min_diff=min_diff$y
    #   max_diff=predict(smooth.spline(1:max_day, max_diff), seq(1,max_day,1)); max_diff=max_diff$y
    # }
    
      
    # max_diff[max_diff>1]=1
   
    
    # grid.lines(prop_steps, med_diff, default.units = 'native', gp=gpar(col=col_line))
    # grid.points(prop_steps, med_diff, default.units = 'native',pch=shape, gp=gpar(col=col_line,fill=col_line,cex=0.5))
    # grid.polygon(c(prop_steps, rev(prop_steps)), c(min_diff, rev(max_diff)),
    #              gp=gpar(col=NA,fill=lightup(col_line, 0.2)),default.units = 'native')
    
    for(d in 1:max_day){
      grid.lines(c(d,d)+x_shift, c(min_diff[d], max_diff[d]), default.units = 'native', gp=gpar(col=col_line)) 
      grid.points(d+x_shift, med_diff[d], default.units = 'native',pch=shape, gp=gpar(col=col_line,fill=col_line,cex=0.25))
    }
    
    
  }
  
  
  
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  # axis for relative by days
  grid.xaxis(at=seq(1,10,1),label=seq(1,10,1),gp=gpar(fontsize=unit(7,'pt')))
  grid.yaxis(at=seq(0,1.5,0.25),label=seq(0,150,25),gp=gpar(fontsize=unit(7,'pt')))
   
  # labels
  grid.text('% increase',x=unit(-2.5,'lines'),rot=90)
  grid.text('Days',y=unit(-2.5,'lines'))
 
  if(row == 1 & column == 1) grid.text('A',x=unit(-2,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 2) grid.text('B',x=unit(-2,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 3) grid.text('C',x=unit(-2,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 4) grid.text('D',x=unit(-2,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 5) grid.text('E',x=unit(-2,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  popViewport()
  popViewport()
  
  
}


png('figure/test.png',height=8,width=8,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=1,ncol=1)))

paneller(1,1)


popViewport()
popViewport()
dev.off()

rm(paneller)





# frequency of steps
paneller=function(row=1,column=1)
{
  
  if(column==1){xlm=c(0,1); ylm=c(0,1)}
  if(column==2){xlm=c(0.5,10.5); ylm=c(0,1.5)}
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  if(column==1){
    # combine cruises, school and work into one main line with median, min, max
    data_temp = data.table()
    dens_temp = data.table()
    
    contact_pair = lapply(1:11, function(x){
      results[[x]]$n_contact_pair
    })
    contact_pair = rbindlist(contact_pair)
    
    for(n in 1:11){
      
      max_day = length(net[[n]]$steps_length$day)
      data_temp = rbind(data_temp, contact_pair[net==n])
      
      if(n %in% c(1:4)){ n_breaks=3+1}
      if(n==5){ n_breaks=3+1}
      if(n %in% c(6:8)){ n_breaks=7+1}
      if(n==9){ n_breaks=5+1}
      if(n %in% c(10,11)){ n_breaks=10+1}
      
      # if(n %in% c(1:4)){ n_breaks=3+1}
      # if(n==5){ n_breaks=3+1}
      # 
      # if(n==6){ n_breaks=4+1}
      # if(n==7){ n_breaks=7+1}
      # if(n==8){ n_breaks=5+1}
      # 
      # if(n==9){ n_breaks=5+1}
      # if(n %in% c(10,11)){ n_breaks=12+1}
      
      adj=3
      data_dens = rep(contact_pair[net==n]$day, times=contact_pair[net==n]$N)
      data_dens = data_dens/max_day
      data_dens = density(data_dens, adjust=adj, n=n_breaks, from=0, to=1)
      data_dens = data.table(prop_day=data_dens$x, cmf_counts=data_dens$y, net=n)
      data_dens[prop_day==0, cmf_counts:=0]
      data_dens[, cmf_counts:=cumsum(cmf_counts)/sum(cmf_counts)]
      dens_temp = rbind(dens_temp, data_dens)
      
      
    }
    
    dens_temp_combine = data.table()
    net_name = c('cruise', 'community', 'school', 'hospital', 'work')
    
    for(n in 1:5){
      
      if(n==1){ 
        net_n=c(1:4) 
        # data_dens = data_temp[net %in% net_n, ] 
        # data_dens[, prop_day:=day/3]
        # data_dens[, cmf_counts:=cum_P]
        # data_dens = rbind(data_dens, data.table(day=0,N=0,net=net_n, P=0,cum_P=0, prop_day=0, cmf_counts=0))
        # setorder(data_dens, net,day)
      }
      if(n==2){ net_n=5} 
      if(n==3){ net_n=c(6:8) }
      if(n==4){ net_n=9}
      if(n==5){ net_n=c(10:11)}
      
      # if(n!=3){
      #   data_dens = dens_temp[net %in% net_n, c('prop_day', 'cmf_counts')] 
      #   data_dens = data_dens[, .(med=median(cmf_counts),
      #                             lwr=min(cmf_counts), 
      #                             upp=max(cmf_counts)), by=.(prop_day)]
      #   data_dens[, net:=net_name[n]]
      #   dens_temp_combine = rbind(dens_temp_combine, data_dens)
      #   
      # }
      # if(n==3){
      #   data_dens = dens_temp[net %in% net_n, c('prop_day', 'cmf_counts')] 
      #   data_dens[, med:=cmf_counts]
      #   data_dens[, lwr:=cmf_counts]
      #   data_dens[, upp:=cmf_counts]
      #   data_dens[, net:=net_name[n]]
      #   data_dens[, cmf_counts:=NULL]
      #   dens_temp_combine = rbind(dens_temp_combine, data_dens)
      #   
      # }
      
      
      
      data_dens = dens_temp[net %in% net_n, c('prop_day', 'cmf_counts')]
      data_dens = data_dens[, .(med=median(cmf_counts),
                                lwr=min(cmf_counts),
                                upp=max(cmf_counts)), by=.(prop_day)]
      data_dens[, net:=net_name[n]]
      dens_temp_combine = rbind(dens_temp_combine, data_dens)
      
    }
    
    # density temporal
    col_line = col_hue_lancet 
    net_name = c('cruise', 'community', 'school', 'hospital', 'work')
    y_legend = c(0.25, 0.2, 0.15, 0.1, 0.05)
    pch_point = c(21,22,23,24,25)
    
    for(n in 1:5){
      
      if(n==1){ one_day_mark_min = 1/3; one_day_mark_max=NA}
      if(n==2){ one_day_mark_min = 1/3; one_day_mark_max=NA}
      if(n==3){ one_day_mark_min = 1/9; one_day_mark_max=1/4}
      if(n==4){ one_day_mark_min = 1/5; one_day_mark_max=NA}
      if(n==5){ one_day_mark_min = 1/12; one_day_mark_max=NA}
      
      data = dens_temp_combine[net==net_name[n]]
      
      
      if(n!=3) grid.lines(data$prop_day,data$med, default.units = 'native',gp=gpar(col=col_line[n]))
      if(n!=3) grid.polygon(c(data$prop_day, rev(data$prop_day)),
                     c(data$lwr, rev(data$upp)),
                     gp=gpar(col=NA,fill=lightup(col_line[n], 0.3)),default.units = 'native')
        
      if(n!=3) grid.points(data$prop_day,data$med, default.units = 'native',pch=pch_point[n], gp=gpar(col=col_line[n], fill=col_line[n], cex=0.25))
        
      
     
      
      
      # index = max(which(data$prop_day<=one_day_mark_min))
      # grid.points(data[index]$prop_day,data[index]$med, default.units = 'native',pch=pch_point[n], gp=gpar(col=col_line[n], fill=col_line[n], cex=0.3))
      # 
      # if(!is.na(one_day_mark_max)){
      #   index = max(which(data$prop_day<=one_day_mark_max))
      #   grid.points(data[index]$prop_day,data[index]$med, default.units = 'native',pch=pch_point[n], gp=gpar(col=col_line[n], fill=col_line[n], cex=0.3))
      # }
      
      
      
      # legend
      grid.lines(c(0.7,0.75),c(y_legend[n], y_legend[n]), default.units = 'native',gp=gpar(col=col_line[n]))
      grid.text(net_name[n], 0.77,y_legend[n], default.units = 'native', just='left', gp=gpar(fontsize=unit(6,'pt')))
      grid.points(0.725,y_legend[n], default.units = 'native',pch=pch_point[n], gp=gpar(col=col_line[n], fill=col_line[n], cex=0.25))
      
      
      if(n==3){
        

        for(s in 6:8){
          data = data_temp[net ==s]
          data = rbind(data.table(day=0,N=0,net=6,P=0,cum_P=0), data)
          grid.lines(data$day/max(data$day),data$cum_P, default.units = 'native',gp=gpar(col=lightup(col_line[n], 1)))
          grid.points(data$day/max(data$day),data$cum_P, default.units = 'native',pch=pch_point[n], gp=gpar(col=lightup(col_line[n], 1), fill=lightup(col_line[n], 1), cex=0.25))

        }

      }
      
      
    }    
  }

  if(column==2){
    
    data_diff = lapply(1:11, function(x){
      results[[x]]$contact_diff
    })
    data_diff = rbindlist(data_diff)
    
    for(net_type in 1:5){
      
      
      # if(net_type == 1) {max_day=3; n=1:4; prop_steps=data_diff[net==1]$prop_steps; shape=21; x_shift=-0.3}
      # if(net_type == 2) {max_day=3; n=5; prop_steps=data_diff[net==5]$prop_steps; shape=22; x_shift=-0.15}
      # if(net_type == 3) {max_day=7; n=6:8; prop_steps=data_diff[net==7]$prop_steps; shape=23; x_shift=0}
      # if(net_type == 4) {max_day=5; n=9; prop_steps=data_diff[net==9]$prop_steps; shape=24; x_shift=0.15}
      # if(net_type == 5) {max_day=10; n=10:11; prop_steps=data_diff[net==10]$prop_steps; shape=25; x_shift=0.3}
      
      if(net_type == 1) {max_day=3; n=1:4; shape=21; x_shift=-0.3}
      if(net_type == 2) {max_day=3; n=5; shape=22; x_shift=-0.15}
      if(net_type == 3) {max_day=7; n=6:8; shape=23; x_shift=0}
      if(net_type == 4) {max_day=5; n=9; shape=24; x_shift=0.15}
      if(net_type == 5) {max_day=10; n=10:11; shape=25; x_shift=0.3}
      
      col_line=col_hue_lancet[net_type]
      
      med_diff = sapply(1:max_day, function(x){ median(data_diff[day==x & net %in% n]$med_diff) })
      min_diff = sapply(1:max_day, function(x){ min(data_diff[day==x & net %in% n]$lwr_diff) })
      max_diff = sapply(1:max_day, function(x){ max(data_diff[day==x & net %in% n]$upp_diff) })
      
      # if(net_type %in% c(3:5)){
      #   med_diff=predict(smooth.spline(1:max_day, med_diff), seq(1,max_day,1)); med_diff=med_diff$y
      #   min_diff=predict(smooth.spline(1:max_day, min_diff), seq(1,max_day,1)); min_diff=min_diff$y
      #   max_diff=predict(smooth.spline(1:max_day, max_diff), seq(1,max_day,1)); max_diff=max_diff$y
      # }
      
      
      # max_diff[max_diff>1]=1
      
      
      # grid.lines(prop_steps, med_diff, default.units = 'native', gp=gpar(col=col_line))
      # grid.points(prop_steps, med_diff, default.units = 'native',pch=shape, gp=gpar(col=col_line,fill=col_line,cex=0.5))
      # grid.polygon(c(prop_steps, rev(prop_steps)), c(min_diff, rev(max_diff)),
      #              gp=gpar(col=NA,fill=lightup(col_line, 0.2)),default.units = 'native')
      
      for(d in 1:max_day){
        grid.lines(c(d,d)+x_shift, c(min_diff[d], max_diff[d]), default.units = 'native', gp=gpar(col=col_line)) 
        grid.points(d+x_shift, med_diff[d], default.units = 'native',pch=shape, gp=gpar(col=col_line,fill=col_line,cex=0.25))
      }
      
      
    }
    
  }
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # axis
  if(column==1){
    grid.xaxis(at=seq(0,1,0.25),label=seq(0,1,0.25),gp=gpar(fontsize=unit(7,'pt')))
    grid.yaxis(at=seq(0,1,0.25),label=seq(0,1,0.25),gp=gpar(fontsize=unit(7,'pt')))
  }
  if(column==2){
    # axis for relative by days
    grid.xaxis(at=seq(1,10,1),label=seq(1,10,1),gp=gpar(fontsize=unit(7,'pt')))
    grid.yaxis(at=seq(0,1.5,0.25),label=seq(0,150,25),gp=gpar(fontsize=unit(7,'pt')))
  
  }
  
  
  # labels
  if(column==1){
    grid.text('Proportion of contacts',x=unit(-3,'lines'),rot=90)
    grid.text('Proportion of steps',y=unit(-2.5,'lines'))
  }
  if(column==2){
    # labels
    grid.text('% increase',x=unit(-2.5,'lines'),rot=90)
    grid.text('Days',y=unit(-2.5,'lines'))
    
  }
  
  if(row == 1 & column == 1) grid.text('A',x=unit(-2.5,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 2) grid.text('B',x=unit(-2.5,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  # if(row == 2 & column == 1) grid.text('C',x=unit(-2.5,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  # if(row == 2 & column == 2) grid.text('D',x=unit(-2.5,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  popViewport()
  popViewport()
  
  
}


png('figure/relative_diff_freq.png',height=8,width=8*2,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=1,ncol=2)))

paneller(1,1)
paneller(1,2)


# grid.text('Proportion of nodes',y=unit(-1,'lines'))


popViewport()
popViewport()
dev.off()

rm(paneller)