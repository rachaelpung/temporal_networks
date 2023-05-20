source('code/temporal_library.R')
source('code/temporal_colour.R')

paneller=function(row = 1,column=1)
{
  if(column!=1) xlm=c(1,5)
  if(column==1) xlm=c(0,10)
  ylm=c(0,1)
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  index = row+8
  max_step = net[[index]]$max_step
  col_line = col_hue_nature
  
  if(column==1){
    for(s in 1:max_step){
      
      data_temp = copy(results[[index]]$p_k0_k1_temp[step==s]) # & k0!=0
      if(data_temp[,.N]==0) next
      
  
      col_seq=c(4, 3, 8, 9, 6, 7, 5, 10)
      for(k in 1:5){
        # if(data_temp[k0==k-1,.N]>=2) grid.lines(data_temp[k0==k-1 & k1<=10]$k1, data_temp[k0==k-1 & k1<=10]$P, default.units = 'native',gp=gpar(col=col_line[col_seq[k]],lwd=0.75))
        if(data_temp[k0==k-1,.N]>=2 & data_temp[k0==k-1,sum(N)]>=10) grid.lines(data_temp[k0==k-1 & k1<=10]$k1, data_temp[k0==k-1 & k1<=10]$P, default.units = 'native',gp=gpar(col=col_line[col_seq[k]],lwd=0.75))
        
      }
    }
  }
  
  if(column==2){
    for(s in 1:max_step){ 
      
      # data_temp = copy(results[[index]]$p_k0_r_temp[step==s & r==0 & k0!=0 & k0<=5])
      data_temp = copy(results[[index]]$p_k0_r_temp[step==s & k0!=0 & k0<=5])
      data_temp[,N_sum:=sum(N), by=.(k0)]
      data_temp = data_temp[r==0 & N_sum>=10]
      
      if(data_temp[,.N]==0) next
      
      
      grid.lines(data_temp$k0, data_temp$P, default.units = 'native',gp=gpar(col=col_line[4],lwd=0.75))
      # data_rand = results$p_k0_r_rand_avg[step==s & r==0 & k0<=5]
      # grid.lines(data_rand$k0, data_rand$P, default.units = 'native',gp=gpar(col='gray80',lwd=0.75))
       
    }
  }
  
  if(column==3){
    for(s in 1:max_step){ 
      
      # data_temp = copy(results[[index]]$p_k0_r_temp[step==s & r==k0 & k0!=0 & k0<=5])
      data_temp = copy(results[[index]]$p_k0_r_temp[step==s & k0!=0 & k0<=5])
      data_temp[,N_sum:=sum(N), by=.(k0)]
      data_temp = data_temp[r==k0 & N_sum>=10]
      
      if(data_temp[,.N]==0) next
      
      
      grid.lines(data_temp$k0, data_temp$P, default.units = 'native',gp=gpar(col=col_line[4],lwd=0.75))
      # data_rand = results$p_k0_r_rand_avg[step==s & r==k0 & k0<=5]
      # grid.lines(data_rand$k0, data_rand$P, default.units = 'native',gp=gpar(col='gray80',lwd=0.75))
      
    }
  }
  
  
  
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # axis
  if(column!=1) grid.xaxis(at=seq(1,5,1),label=seq(1,5,1))
  if(column==1) grid.xaxis(at=seq(0,10,2),label=seq(0,10,2))
  grid.yaxis(at=seq(0,1,0.25),label=seq(0,1,0.25))
  
  
  # labels
  grid.text('Probability (%)',x=unit(-3,'lines'),rot=90)
  grid.text(bquote(k[t]),y=unit(-2.5,'lines'))
  
  if(row == 1 & column == 1) grid.text('A',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 2 & column == 1) grid.text('B',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 3 & column == 1) grid.text('C',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 4 & column == 1) grid.text('D',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  popViewport()
  popViewport()
  
}


png('figure/p_k_N_10_net_9_11.png',height=8*3,width=8*3,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=3,ncol=3)))

paneller(1,1); paneller(1,2); paneller(1,3)
paneller(2,1); paneller(2,2); paneller(2,3)
paneller(3,1); paneller(3,2); paneller(3,3)
# paneller(4,1); paneller(4,2); paneller(4,3)
# paneller(5,1); paneller(5,2); paneller(5,3)
# paneller(6,1); paneller(6,2); paneller(6,3)
# paneller(7,1); paneller(7,2); paneller(7,3)
# paneller(8,1); paneller(8,2); paneller(8,3)
# paneller(9,1); paneller(9,2); paneller(9,3)
# paneller(10,1); paneller(10,2); paneller(10,3)
# paneller(11,1); paneller(11,2); paneller(11,3)

popViewport()
popViewport()
dev.off()


paneller=function(row = 1,column=1)
{
  xlm=c(0,10); ylm=c(0,1) 
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  index = column+4*(row-1)
  max_step = net[[index]]$max_step
  col_line = col_hue_nature
  
  for(s in 1:max_step){
    
    data_temp = copy(results[[index]]$p_r_temp[step==s & r<=10])
    data_stat = copy(results[[index]]$p_r_stat[step==s & r<=10])
      
    if(data_temp[,.N]==0) next
    grid.lines(data_temp$r, data_temp$P, default.units = 'native',gp=gpar(col=col_line[4],lwd=0.75))
    grid.lines(data_stat$r, data_stat$P, default.units = 'native',gp=gpar(col=col_line[3],lwd=0.75))
      
  }
    
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
  
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  popViewport()
  popViewport()
  
}

png('figure/p_r.png',height=8*3,width=8*4,units='cm',res=300,pointsize=10)
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

grid.text('Probability (%)',x=unit(-1.5,'lines'),rot=90)
grid.text('No. of contacts retained',y=unit(-1,'lines'))

popViewport()
popViewport()
dev.off()


paneller=function(row = 1,column=1)
{
  xlm=c(0,10); ylm=c(-4,0) 
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  index = column-4*(row-1)
  max_step = net[[index]]$max_step
  col_line = col_hue_nature
  
  for(s in 1:max_step){
    
    data_temp = copy(results[[index]]$p_r_temp[step==s & r<=10])
    data_stat = copy(results[[index]]$p_r_stat[step==s & r<=10])
    
    if(data_temp[,.N]==0) next
    grid.lines(data_temp$r, log(data_temp$P, base=10), default.units = 'native',gp=gpar(col=col_line[4],lwd=0.75))
    grid.lines(data_stat$r, log(data_stat$P, base=10), default.units = 'native',gp=gpar(col=col_line[3],lwd=0.75))
    
  }
  
  # data_temp = results[[index]]$p_r_temp
  # data_temp = data_temp[,sum(r_P), by=.(step)]
  # grid.lines(c(quantile(data_temp$V1, c(0.025, 0.975))), c(0.875,0.875), 
  #            default.units = 'native',gp=gpar(col=col_line[4],lwd=0.75))
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
  # grid.yaxis(at=seq(0,1,0.25),label=seq(0,1,0.25))
  grid.yaxis(at=seq(-4,0,1),label=c(expression(10^-4), expression(10^-3), 
                                    expression(10^-2), expression(10^-1), 1))
  
  
  
  # labels
  if(column==1){
    grid.text('Probability (%)',x=unit(-3,'lines'),rot=90)
  }
  
  if(row == 1 & column == 1) grid.text('A',x=unit(-2.5,'lines'),y=unit(10.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 2) grid.text('B',x=unit(-2.5,'lines'),y=unit(10.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 3) grid.text('C',x=unit(-2.5,'lines'),y=unit(10.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 4) grid.text('D',x=unit(-2.5,'lines'),y=unit(10.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  popViewport()
  popViewport()
  
}

png('figure/p_r_log.png',height=8,width=8*4,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=1,ncol=4)))

paneller(1,1)
paneller(1,2)
paneller(1,3)
paneller(1,4)

grid.text('No. of contacts retained',y=unit(-1,'lines'))

popViewport()
popViewport()
dev.off()



violin = function(x,yvec)
{
  
  # colnet = c('#E64B35FF', '#4DBBD5FF', '#00A087FF', '#3C5488FF', '#F39B7FFF', 
                        # '#8491B4FF', '#91D1C2FF', '#DC0000FF', '#7E6148FF', '#B09C85FF') 
                        
  colnet = c('#3C5488FF', '#00A087FF','#DC0000FF', '#7E6148FF') 
      
 
                    
  de = density(yvec)
  yv = c(de$x,rev(de$x))
  xv = x + c(de$y,-rev(de$y))*0.1 # scale the width of violin plots
  grid.polygon(xv,yv,gp=gpar(col=NA,fill=colnet[x]),default.units = 'native')
  
  wb = 0.05 # half-width of box
  grid.polygon(x+c(-wb,wb,wb,-wb), quantile(yvec,c(0.75,0.75,0.25,0.25)),
               default.units = 'native',gp=gpar(fill=NA, col='black')) 
  grid.lines(x+c(-wb,wb), quantile(yvec,c(0.5,0.5)), default.units = 'native',gp=gpar(fill=NA, col='black'))
  grid.lines(c(x,x), quantile(yvec,c(0.75,0.975)), default.units = 'native',gp=gpar(fill=NA, col='black'))
  grid.lines(c(x,x), quantile(yvec,c(0.25,0.025)), default.units = 'native',gp=gpar(fill=NA, col='black'))
  
}

paneller=function(row = 1,column=1)
{
  xlm=c(0,4);ylm=c(0,1)
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # add violins
  # data = rbeta(1000, shape1=5, shape2=5)
  # data = matrix(data, nrow=200, ncol=5)
  index = column-4*(row-1)
  data = copy(results[[index]]$p_r[[1]])
  
  for(n in 1:3){ violin(n, data[r==n,]$diff_rel) }
  
  
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # axis
  grid.xaxis(at=seq(1,3,1),label=seq(1,3,1),gp=gpar(fontsize=unit(7,'pt')))
  grid.yaxis(at=seq(0,1,0.25),label=seq(0,1,0.25),gp=gpar(fontsize=unit(7,'pt')))
  
  
  # labels
  grid.text('Probability (%)',x=unit(-3,'lines'),rot=90)
  
  if(row == 1 & column == 1) grid.text('A',x=unit(-2.5,'lines'),y=unit(10.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 2) grid.text('B',x=unit(-2.5,'lines'),y=unit(10.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 3) grid.text('C',x=unit(-2.5,'lines'),y=unit(10.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 4) grid.text('D',x=unit(-2.5,'lines'),y=unit(10.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  popViewport()
  popViewport()
  
}


png('figure/p_r_diff_rel.png',height=8,width=8*4,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=1,ncol=4)))

paneller(1,1)
paneller(1,2)
paneller(1,3)
paneller(1,4)

grid.text('Retain',y=unit(-1,'lines'))


popViewport()
popViewport()
dev.off()


paneller=function(row = 1,column=1)
{
  xlm=c(0,1); ylm=c(0,1) 
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  
  
  for(n in 1:11){
   
    if(n%in%c(1:4)){ col_line = col_hue_nature[4]; shape=21}
    if(n==5){ col_line = col_hue_nature[3]; shape=22}
    if(n%in%c(6:8)){ col_line = col_hue_nature[8]; shape=23}
    if(n==9){ col_line = col_hue_nature[9]; shape=24}
    if(n%in%c(10:11)){ col_line = col_hue_nature[6]; shape=25}
    
    data_temp = results[[n]]$p_r_temp
    data_temp = data_temp[,sum(r_P), by=.(step)]
    data_stat = results[[n]]$p_r_stat
    data_stat = data_stat[,sum(r_P), by=.(step)]
    
    # data_rand = results[[n]]$p_r_rand
    # data_rand = data_rand[,sum(r_P), by=.(step,set)]
    # data_rand = data_rand[,.(median(V1)), by=.(step)]
    
    data_rand = results[[n]]$p_r_rand_avg
    data_rand = data_rand[,sum(r_P), by=.(step)]
     
    data_norm = (data_temp$V1-data_rand$V1)/(data_stat$V1-data_rand$V1)
    
    grid.lines(quantile(data_norm, probs=c(0.25,0.75)), quantile(data_temp$V1,probs = c(0.5,0.5)), 
               default.units = 'native',gp=gpar(col=col_line,lwd=0.75))
    
    grid.lines(quantile(data_norm, probs=c(0.5,0.5)), quantile(data_temp$V1,probs = c(0.25,0.75)), 
               default.units = 'native',gp=gpar(col=col_line,lwd=0.75))
    
    grid.points(quantile(data_norm, probs=c(0.5)),quantile(data_temp$V1,probs = c(0.5)), pch=shape,
                default.units = 'native',gp=gpar(col=col_line,fill=col_line, cex=0.3))
  
  }
  
 
  
  
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # axis
  grid.xaxis(at=seq(0,1,0.25),label=seq(0,1,0.25))
  grid.yaxis(at=seq(0,1,0.25),label=seq(0,1,0.25))
  
  
  
  # labels
  grid.text('No. of contacts in each time step',x=unit(-3,'lines'),rot=90)
  grid.text('r',y=unit(-2.5,'lines'))
  
  
  if(row == 1 & column == 1) grid.text('A',x=unit(-2,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
 
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  popViewport()
  popViewport()
  
}

png('figure/contacts.png',height=8,width=8,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=1,ncol=1)))

paneller(1,1)

popViewport()
popViewport()
dev.off()



paneller=function(row = 1,column=1)
{
  xlm=c(0,1); ylm=c(0,0.3) 
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  n = column+4*(row-1)
  
  if(n%in%c(1:4)){ col_line = col_hue_nature[4]}
  if(n==5){ col_line = col_hue_nature[3]}
  if(n%in%c(6:8)){ col_line = col_hue_nature[8]}
  if(n==9){ col_line = col_hue_nature[9]}
  if(n%in%c(10:11)){ col_line = col_hue_nature[6]}
    
  data_temp = results[[n]]$p_r_temp
  data_temp = data_temp[,sum(r_P), by=.(step)]
  data_stat = results[[n]]$p_r_stat
  data_stat = data_stat[,sum(r_P), by=.(step)]
    
  # data_rand = results[[n]]$p_r_rand
  # data_rand = data_rand[,sum(r_P), by=.(step,set)]
  # data_rand = data_rand[,.(median(V1)), by=.(step)]
    
  data_rand = results[[n]]$p_r_rand_avg
  data_rand = data_rand[,sum(r_P), by=.(step)]
    
  data_norm = (data_temp$V1-data_rand$V1)/(data_stat$V1-data_rand$V1)
  data_norm = hist(data_norm, breaks=seq(-0.5,1,0.05), plot=F)
  data_norm = data.table(breaks=data_norm$breaks[-1], 
                         counts=data_norm$counts)
  data_norm = data_norm[breaks>=0.1]
  data_norm[, freq:=counts/sum(counts)]
    
  
  for(i in 1:nrow(data_norm)){
    x=data_norm$breaks[i]
    y=data_norm$freq[i]
      
    grid.polygon(c(x-0.05,x,x,x-0.05),
                 c(0,0,y,y),default.units = 'native',
                 gp=gpar(col=NA,fill=col_line))
      
      
  }
    
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # axis
  grid.xaxis(at=seq(0,1,0.25),label=seq(0,1,0.25))
  grid.yaxis(at=seq(0,0.3,0.1),label=seq(0,30,10))
  
  
  
  # labels
  grid.text('Frequency (%)',x=unit(-3,'lines'),rot=90)
  grid.text('r',y=unit(-2.5,'lines'))
  
  
  if(row == 1 & column == 1) grid.text('A',x=unit(-2,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  popViewport()
  popViewport()
  
}

png('figure/contacts_hist.png',height=8*3,width=8*4,units='cm',res=300,pointsize=10)
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





paneller=function(row=1,column=1)
{
  
  xlm=c(0,1); ylm=c(0,1)
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # plot static, random characteristics
  if(row==1){
    
    if(column==1) {p80 = 0.2; i=1}
    if(column==2) {p80 = 0.8; i=2}
    col_line = col_hue_nature[c(4, 3, 8, 9, 6)]
    max_step=c(10,25,50,100,250,500,1000)
    
    # static
    data = data.table(prop_steps = c(1,0),
                      pmf_nodes = c(p80,(1-p80)),
                      cmf_nodes = c(p80,1))
    
    # col_line = col_hue_nature[3]
    
    grid.lines(c(0,data[1,]$cmf_nodes),c(1,1), default.units = 'native',gp=gpar(col=col_line[i], lty='dashed'))
    grid.lines(c(data[1,]$cmf_nodes,data[1,]$cmf_nodes),c(1,0), default.units = 'native',gp=gpar(col=col_line[i], lty='dashed'))
    grid.lines(c(data[1,]$cmf_nodes,data[2,]$cmf_nodes),c(0,0), default.units = 'native',gp=gpar(col=col_line[i], lty='dashed'))
    
    for(m in 1:length(max_step)){

      # random
      data = data.table(prop_steps = seq(max_step[m],1,-1)/max_step[m],
                        pmf_nodes = p80^seq(max_step[m],1,-1))
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

      # adj=3
      # data = rep(data[cmf_nodes<1]$prop_steps, times=data[cmf_nodes<1]$pmf_nodes*1000)
      # data = density(data, adjust=adj, n=500, from=0, to=1)
      # data = data.table(prop_steps=data$x, cmf_nodes=data$y, net=n)
      # data[, cmf_nodes:=1-cumsum(cmf_nodes)/sum(cmf_nodes)]
      #
      # # col_line = col_hue_nature[9]
      # grid.lines(data$cmf_nodes,data$prop_steps,
      #            default.units = 'native',gp=gpar(col=col_line[i]))

      # empirical temporal step
      for(j in 1:nrow(data)){

        # horizontal and vertical
        if(j==1){
          grid.lines(c(0,data[j,]$cmf_nodes),
                     c(1,1),
                     default.units = 'native',gp=gpar(col=col_line[i])) # data[i,]$dur_hex

          grid.lines(c(data[j,]$cmf_nodes,data[j,]$cmf_nodes),
                     c(1,data[j+1,]$prop_steps),
                     default.units = 'native',gp=gpar(col=col_line[i])) # data[i,]$dur_hex

        }else{
          grid.lines(c(data[j-1,]$cmf_nodes,data[j,]$cmf_nodes),
                     c(data[j,]$prop_steps,data[j,]$prop_steps),
                     default.units = 'native',gp=gpar(col=col_line[i])) # data[i,]$dur_hex

          grid.lines(c(data[j,]$cmf_nodes,data[j,]$cmf_nodes),
                     c(data[j,]$prop_steps,data[j+1,]$prop_steps),
                     default.units = 'native',gp=gpar(col=col_line[i])) # data[i,]$dur_hex
        }

      }

    }
      
  }
    
    
  
  
    
  if(row==2){
    
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
      
      data[, prop_steps:=steps/max_step]
      data[, pmf_nodes:=N/sum(N)]
      data[, cmf_nodes:=cumsum(pmf_nodes)]
      if(max(data$steps) < max_step) data = rbindlist(list(data, data.table(N=0,prop_steps=1, cmf_nodes=0)), fill=T)
      
      data[, net:=n]
      data_temp = rbind(data_temp,data)
      
      adj=3
      data_dens = rep(data$prop_steps, times=data$N)
      data_dens = density(data_dens, adjust=adj, n=500, from=0, to=1)
      data_dens = data.table(prop_steps=data_dens$x, cmf_nodes=data_dens$y, net=n)
      data_dens[, cmf_nodes:=1-cumsum(cmf_nodes)/sum(cmf_nodes)]
      
      dens_temp = rbind(dens_temp, data_dens)
      
      
    }
    
    
    dens_temp_combine = data.table()
    net_name = c('cruise', 'haslemere', 'school', 'hospital', 'work')
    
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
    col_line = col_hue_nature[c(4, 3, 8, 9, 6)]
    net_name = c('cruise', 'haslemere', 'school', 'hospital', 'work')
    for(n in 1:5){

      data = dens_temp_combine[net==net_name[n]]
     
      grid.lines(data$med, data$prop_steps, default.units = 'native',gp=gpar(col=col_line[n]))
      
      
      grid.polygon(c(data$lwr, rev(data$upp)),
                   c(data$prop_steps, rev(data$prop_steps)),
                   gp=gpar(col=NA,fill=lightup(col_line[n], 0.6)),default.units = 'native')
      
      
      # draw dashed lines
      if(n %in% c(1)){
        
        col_line_50 = 'grey80' # col_hue_nature[8]
        col_line_25 = 'grey60' # col_hue_nature[5]
        
        yy = 0.501002004
        xx = data[prop_steps>0.5 & prop_steps<0.5011, med]
        grid.lines(c(0,xx,xx), c(yy,yy,0), default.units = 'native',gp=gpar(col=col_line_50, lty='dotted'))

        yy = 0.25050100
        xx = data[prop_steps>=0.25 & prop_steps<0.25051,  med]
        grid.lines(c(0,xx,xx), c(yy,yy,0), default.units = 'native',gp=gpar(col=col_line_25, lty='dotted'))
        
      }
      
    }
    
    
    
      # for(n in 1:11){
      # 
      # empirical line temporal
      # index = n # column-4*(row-1)
      # max_step = net[[index]]$max_step
      # n_nodes = net[[index]]$n_nodes_total
      # 
      # data = copy(results[[index]]$node_r80_ct_esp)
      # # data = data[, .(median(dur_med), .N), by=.(N)]
      # # data = data[, .(median(deg_med), .N), by=.(N)]
      # # setnames(data, c('steps', 'med_dur', 'N'))
      # data = data[, .N, by=.(N)]
      # setnames(data, c('steps', 'N'))
      # data = rbind(data, data.table(steps=0, N=n_nodes-sum(data$N)))
      # 
      # data[, prop_steps:=steps/max_step]
      # data[, pmf_nodes:=N/sum(N)]
      # data[, cmf_nodes:=cumsum(pmf_nodes)]
      # 
      # # col_hue = viridis_pal()(7)
      # # val=seq(0,1.5,length.out = 7)
      # # val=seq(0,log(7, base=10),length.out = 7)
      # # pal=gradient_n_pal(colours = col_hue, values = val)
      # # data[, dur_hex:=pal(log(med_dur, base=10))]
      # # data[, dur_hex:=pal(med_dur)]
      # # data[is.na(med_dur), dur_hex:=pal(max(val))]
      # # data[med_dur<1, dur_hex:=pal(0)]
      # 
      # # col_line = col_hue_nature[4]
      # 
      # if(n%in%c(1:4))col_line = col_hue_nature[4]
      # if(n==5)col_line = col_hue_nature[3]
      # if(n%in%c(6:8))col_line = col_hue_nature[8]
      # if(n==9)col_line = col_hue_nature[9]
      # if(n%in%c(10:11))col_line = col_hue_nature[6]
      # 
      # col_line_50 = 'grey80' # col_hue_nature[8]
      # col_line_25 = 'grey60' # col_hue_nature[5]
      # 
      # grid.lines(c(0,data$cmf_nodes),
      #            c(1,data$prop_steps),
      #            default.units = 'native',gp=gpar(col=col_line))
      # 
      # # empirical temporal step
      # for(i in 1:nrow(data)){
      #
      #   # horizontal and vertical
      #   if(i==1){
      #     grid.lines(c(0,data[i,]$cmf_nodes),
      #                c(1,1),
      #                default.units = 'native',gp=gpar(col=col_line)) # data[i,]$dur_hex
      #
      #     grid.lines(c(data[i,]$cmf_nodes,data[i,]$cmf_nodes),
      #                c(1,data[i,]$prop_steps),
      #                default.units = 'native',gp=gpar(col=col_line)) # data[i,]$dur_hex
      #
      #   }else{
      #     grid.lines(c(data[i-1,]$cmf_nodes,data[i,]$cmf_nodes),
      #                c(data[i-1,]$prop_steps,data[i-1,]$prop_steps),
      #                default.units = 'native',gp=gpar(col=col_line)) # data[i,]$dur_hex
      #
      #     grid.lines(c(data[i,]$cmf_nodes,data[i,]$cmf_nodes),
      #                c(data[i-1,]$prop_steps,data[i,]$prop_steps),
      #                default.units = 'native',gp=gpar(col=col_line)) # data[i,]$dur_hex
      #   }
      #
      #
      #
      #
      # }
      # 
      # draw dashed lines
      # if(n %in% c(1,5)){
      # 
      #   yy = 0.5
      #   xx = data[prop_steps>=yy, .(max(cmf_nodes))]
      #   grid.lines(c(0,xx,xx), c(yy,yy,1), default.units = 'native',gp=gpar(col=col_line_50, lty='dotted'))
      # 
      #   yy = 0.25
      #   xx = data[prop_steps>=yy, .(max(cmf_nodes))]
      #   grid.lines(c(0,xx,xx), c(yy,yy,1), default.units = 'native',gp=gpar(col=col_line_25, lty='dotted'))
      # 
      # 
      # }
      # 
      # }
    
    
    
    
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
  if(row == 2 & column == 1) grid.text('C',x=unit(-2.5,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 2 & column == 2) grid.text('D',x=unit(-2.5,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  if(row==1 & column==1) {
    grid.lines(c(0,0.2),c(0,0))
    grid.lines(c(1,1,0.2),c(0,1,1))
  }
  if(row==1 & column==2) {
    grid.lines(c(0,0.8),c(0,0))
    grid.lines(c(1,1,0.8),c(0,1,1))
  }
  if(row==2) grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  popViewport()
  popViewport()
  
  
}


png('figure/cum_rank_all_density.png',height=8*2,width=8*2,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=2,ncol=2)))

paneller(1,1)
paneller(1,2)
paneller(2,1)
paneller(2,2)

# grid.text('Proportion of nodes',y=unit(-1,'lines'))


popViewport()
popViewport()
dev.off()

rm(paneller)


paneller=function(row=1,column=1)
{
  
  xlm=c(1,10); ylm=c(0,1)
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  index = column+4*(row-1)
  data = copy(results[[index]]$node_s80_ct_dur)
  max_step = net[[index]]$max_step
  col_line = col_hue_nature[4]
    
  for(i in 1:(max_step-5)){
    
    if(data[step_start==i & dur<=10,.N]>1) grid.lines(data[step_start==i & dur<=10]$dur,data[step_start==i & dur<=10]$P,default.units = 'native',gp=gpar(col=col_line))
    
  }
  
  
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # axis
  grid.xaxis(at=seq(1,10,1),label=seq(1,10,1),gp=gpar(fontsize=unit(7,'pt')))
  grid.yaxis(at=seq(0,1,0.25),label=seq(0,1,0.25),gp=gpar(fontsize=unit(7,'pt')))
  
  
  # labels
  grid.text('Proportion',x=unit(-3,'lines'),rot=90)
  
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
  
  
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  popViewport()
  popViewport()
  
  
}


png('figure/sustain_p80.png',height=8*3,width=8*4,units='cm',res=300,pointsize=10)
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

grid.text('Duration',y=unit(-1,'lines'))

popViewport()
popViewport()
dev.off()

rm(paneller)