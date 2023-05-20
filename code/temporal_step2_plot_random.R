source('code/temporal_library.R')

# generate p_k0_r for random of varying mean deg
set = data.table(n_nodes = c(10,50,100))
set = set[rep(1:.N, each = 14)]
set[, mean_deg:=rep(c(0.005,0.01,0.03,0.05,seq(0.1,0.9,0.1),0.95), times=3)]
set[, mean_deg:=n_nodes*mean_deg]
set = set[n_nodes>mean_deg]

# for each combination, derive the p_k0 for lambda and the distribution of p_k0_k1_r_rand_avg
p_k0_r_rand_avg = lapply(1:nrow(set), function(x){
  
  mean_deg_set = set$mean_deg[x]
  n_nodes_set = set$n_nodes[x]
  
  p_k0_pois = data.table(k0=0:(n_nodes_set-1)) # for n_nodes less than 100
  p_k0_pois = p_k0_pois[k0<n_nodes_set]
  p_k0_pois[, P:=dpois(k0, mean_deg_set)]
  p_k0_pois[, P:=P/sum(P)]
  
  p_k0_k1_r_rand_avg_set = pmf_k0_k1_retain_avg(p_k0_pois, n_nodes_set)
  p_k0_r_rand_avg_set = p_k0_k1_r_rand_avg_set[, sum(P), by=.(k0,r)]
  setnames(p_k0_r_rand_avg_set, old='V1', new='P')
  
  p_k0_r_rand_avg_set[p_k0_pois, p_k0:=i.P, on=c(k0='k0')]
  
  return(p_k0_r_rand_avg_set)
  
})

paneller=function(row = 1,column=1)
{
  if(column==1) xlm=c(0,10)
  if(column==2) xlm=c(0,50)
  if(column==3) xlm=c(0,100)
  ylm=c(0,1)
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # extract set numbers
  if(column==1) set_num = which(set$n_nodes==10)
  if(column==2) set_num = which(set$n_nodes==50)
  if(column==3) set_num = which(set$n_nodes==100)
  
  col_hue = viridis_pal()(7)
  val=seq(0,1,length.out = 7)
  
  for(i in 1:length(set_num)){ 
    
    pal=gradient_n_pal(colours = col_hue, values = val)
    colLine = pal(set[set_num[i],mean_deg]/set[set_num[i],n_nodes])
    
    data = copy(p_k0_r_rand_avg[[set_num[i]]])
    if(row==1) data = data[r==0]
    if(row==2) data = data[r==k0]
    
    grid.lines(data$k0, data$P, default.units = 'native',gp=gpar(col=colLine,lwd=0.75))
   
  }
  
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # axis
  if(column==1) grid.xaxis(at=seq(0,10,2),label=seq(0,10,2))
  if(column==2) grid.xaxis(at=seq(0,50,10),label=seq(0,50,10))
  if(column==3) grid.xaxis(at=seq(0,100,20),label=seq(0,100,20))
  
  grid.yaxis(at=seq(0,1,0.25),label=seq(0,1,0.25))
  
  
  # labels
  grid.text('Probability (%)',x=unit(-3,'lines'),rot=90)
  grid.text(bquote(k[t]),y=unit(-2,'lines'))
  
  if(row == 1 & column == 1) grid.text('A',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 2) grid.text('B',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 3) grid.text('C',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 2 & column == 1) grid.text('D',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 2 & column == 2) grid.text('E',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 2 & column == 3) grid.text('F',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  if(row == 1 & column == 3){ colourbar(x_bottom_left = 105, y_bottom_left = -0.6, 
                                        y_length = 0.8, x_length = 5, 
                                        palette=col_hue)}

  
  popViewport()
  popViewport()
  
}

colourbar <- function(x_bottom_left = 0.5, y_bottom_left = 0.5, y_length = 0.1, x_length = 0.1, palette=NULL){
  
  colour_hex = palette
  cols <- colorRampPalette(colour_hex)
    
  y_start = seq(y_bottom_left, y_bottom_left+y_length,length.out = 126)[-126]
  y_end = seq(y_bottom_left, y_bottom_left+y_length,length.out = 126)[-1]
    
  for(i in 1:125){
    grid.polygon(c(x_bottom_left,x_bottom_left,x_bottom_left+x_length,x_bottom_left+x_length),
                 c(y_start[i],y_end[i],y_end[i],y_start[i]),
                 default.units = 'native',gp=gpar(fill=cols(125)[i], col=cols(125)[i]))
  }
    
    
  y_start <-  seq(y_bottom_left, y_bottom_left+y_length,length.out=3)
  label <- c('0', '0.5', '1')
    
    
  for(breaks in 1:length(y_start)){
      
    if(breaks %in% c(2:(length(y_start)-1))){
       grid.lines(c(x_bottom_left, x_bottom_left+(x_length/5)), 
                  c(y_start[breaks], y_start[breaks]), default.units = 'native',gp=gpar(col='black'))
       grid.lines(c(x_bottom_left+x_length, x_bottom_left+x_length-(x_length/5)), 
                  c(y_start[breaks], y_start[breaks]), default.units = 'native',gp=gpar(col='black'))
      }
      
      
    grid.text(label[breaks],x=x_bottom_left+2*x_length,y=y_start[breaks],gp = gpar(fontsize = 6), default.units = 'native')
      
  }
    
  grid.polygon(c(x_bottom_left,x_bottom_left,x_bottom_left+x_length,x_bottom_left+x_length),
               c(y_bottom_left,y_bottom_left+y_length,y_bottom_left+y_length,y_bottom_left),
               default.units = 'native',gp=gpar(fill=NA, col='black'))
    
    
  
}


png('figure/random_supp.png',height=16,width=24,units='cm',res=300,pointsize=10)
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



paneller=function(row = 1,column=1)
{
  if(row==1 & column==1) xlm=c(0,10)
  if(row==1 & column==2) xlm=c(0,50)
  if(row==1 & column==3) xlm=c(0,100)
  if(row==2) xlm=c(0,10)
  
  if(row==1) ylm=c(0,1)
  if(row==2 & column==1) ylm=c(0,10) 
  if(row==2 & column==2) ylm=c(0,50) 
  if(row==2 & column==3) ylm=c(0,100) 
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # gridlines
  if(row==1){
    for(j in 1:9){
      
      xx = j*xlm[2]/10
      yy = j*ylm[2]/10
      grid.lines(x=c(xx,xx), y=c(0,1), default.units = 'native',gp=gpar(col='#FAFAFA',lwd=1))
      grid.lines(x=c(0,xlm[2]), y=c(yy,yy), default.units = 'native',gp=gpar(col='#FAFAFA',lwd=1))
      
    }
  }
  if(row==2){
    for(j in 1:9){
      
      xx = j
      yy = j*ylm[2]/10
      grid.lines(x=c(xx,xx), y=c(0,ylm[2]), default.units = 'native',gp=gpar(col='#FAFAFA',lwd=1))
      grid.lines(x=c(0,xlm[2]), y=c(yy,yy), default.units = 'native',gp=gpar(col='#FAFAFA',lwd=1))
      
    }
  }
  
  
  
  # extract set numbers
  if(column==1) set_num = which(set$n_nodes==10 & set$mean_deg>=10*0.1 & set$mean_deg<=10*0.9)
  if(column==2) set_num = which(set$n_nodes==50 & set$mean_deg>=50*0.1 & set$mean_deg<=50*0.9)
  if(column==3) set_num = which(set$n_nodes==100 & set$mean_deg>=100*0.1 & set$mean_deg<=100*0.9)
  
  
  col_hue = viridis_pal()(7)
  val=seq(0,1,length.out = 7)
  
  for(i in 1:length(set_num)){ 
    
    pal=gradient_n_pal(colours = col_hue, values = val)
    colLine = pal(set[set_num[i],mean_deg]/set[set_num[i],n_nodes])
    
    data = copy(p_k0_r_rand_avg[[set_num[i]]])
    data[,P:=P*p_k0]
    data = data[,sum(P), by=.(r)]
    setnames(data, old='V1', new='P')
    data[,P:=P/sum(P)]
    
    if(row==1) {
      grid.lines(data$r, data$P, default.units = 'native',gp=gpar(col=colLine,lwd=0.75))
    }
    if(row==2){
      data[,P_cum:=cumsum(P)]
      mean_p=sum(data$r*data$P)
      lwr_p=data[which(P_cum>=0.25),min(r)]
      upp_p=data[which(P_cum>=0.75),min(r)]
      
      xx = (i/10)*xlm[2]
      grid.points(xx, mean_p, default.units = 'native',pch=16,gp=gpar(col=colLine, fill=colLine, cex=0.5))
      grid.lines(c(xx,xx), c(lwr_p,upp_p), default.units = 'native',gp=gpar(col=colLine,lwd=0.75))
    }
    
  }
  
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # axis
  if(row==1 & column==1) grid.xaxis(at=seq(0,10,2),label=seq(0,10,2))
  if(row==1 & column==2) grid.xaxis(at=seq(0,50,10),label=seq(0,50,10))
  if(row==1 & column==3) grid.xaxis(at=seq(0,100,20),label=seq(0,100,20))
  if(row==2) grid.xaxis(at=seq(1,9,1),label=c(seq(1,9,1)*ylm[2]/10))
  
  if(row==1) grid.yaxis(at=seq(0,1,0.25),label=seq(0,1,0.25))
  if(row==2 & column==1) grid.yaxis(at=seq(0,10,2),label=seq(0,10,2))
  if(row==2 & column==2) grid.yaxis(at=seq(0,50,10),label=seq(0,50,10))
  if(row==2 & column==3) grid.yaxis(at=seq(0,100,20),label=seq(0,100,20))
  
  # labels
  if(row==1) grid.text('Probability (%)',x=unit(-3,'lines'),rot=90)
  if(row==1) grid.text('No. of contacts retained',y=unit(-2.5,'lines'))
  
  if(row==2) grid.text('Mean no. of contacts retained',x=unit(-3,'lines'),rot=90)
  if(row==2) grid.text(bquote('Mean '~k[t]),y=unit(-2.5,'lines'))
  
  if(row == 1 & column == 1) grid.text('A',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 2) grid.text('B',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 1 & column == 3) grid.text('C',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 2 & column == 1) grid.text('D',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 2 & column == 2) grid.text('E',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  if(row == 2 & column == 3) grid.text('F',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  if(row == 1 & column == 3){ colourbar(x_bottom_left = 105, y_bottom_left = -0.6, 
                                        y_length = 0.8, x_length = 5, 
                                        palette=col_hue)}
  
  
  popViewport()
  popViewport()
  
}


png('figure/random_retain_supp.png',height=16,width=24,units='cm',res=300,pointsize=10)
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


mean_retain = data.table(expand.grid(mean_deg = seq(1,10,0.5), N=seq(1000,10000,500)))
mean_retain[, r:=mean_deg*mean_deg/N]

min(mean_retain$r); max(mean_retain$r)
log(min(mean_retain$r), base=10); log(max(mean_retain$r), base=10)

pal = gradient_n_pal(colours = magma(7, alpha = 1, begin = 0, end = 1, direction = 1),
                     values  = seq(-4,-1,0.05))
mean_retain[, r_hex:=pal(log(r,base=10))]


paneller=function(row = 1,column=1)
{
  xlm=c(1-0.25, 10+0.25); ylm=c(1-0.25, 10+0.25)
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  data = mean_retain

  for(i in 1:data[,.N]){ 
    
    grid.polygon(data[i,mean_deg]+0.25*c(-1,1,1,-1),
                 data[i,N]/1000+0.25*c(-1,-1,1,1),
                 default.units = 'native',gp=gpar(fill=data[i,r_hex],col=data[i,r_hex]))
    
    
  }
  
  
  popViewport()
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  

  grid.xaxis(at=seq(1,10,1), label=seq(1,10,1))
  grid.yaxis(at=seq(1,10,1), label=seq(1000,10000,1000))
  grid.text('N',y=unit(-2.5,'lines'))
  grid.text('Mean degree',x=unit(-3.5,'lines'),rot=90)

 
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  
  
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  colourbar(x_bottom_left = 10.8, y_bottom_left = 3, y_length = 5, x_width = 0.3)
  
  # caption
  # grid.text('A',x=unit(-3,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(10,'pt')))
  
  popViewport()
  popViewport()
  
}


colourbar <- function(x_bottom_left = 1.1, y_bottom_left = 0.4, y_length = 0.75, x_width = 0.2){
  
  colour_hex = colours = magma(7, alpha = 1, begin = 0, end = 1, direction = 1)
  cols = colorRampPalette(colour_hex)
                          
  y_start = seq(y_bottom_left, y_bottom_left+y_length,length.out = 126)[-126]
  y_end = seq(y_bottom_left, y_bottom_left+y_length,length.out = 126)[-1]
                          
  for(i in 1:125){
    grid.polygon(c(x_bottom_left,x_bottom_left,x_bottom_left+x_width,x_bottom_left+x_width),
    c(y_start[i],y_end[i],y_end[i],y_start[i]),
    default.units = 'native',gp=gpar(fill=cols(125)[i], col=cols(125)[i]))
  }
                          
  y_start = seq(y_bottom_left, y_bottom_left+y_length,length.out = 4)
  label = c(expression(10^-4), expression(10^-3), expression(10^-2), expression(10^-1))
  
                          
  for(breaks in 1:4){
    if(breaks %in% c(2:3)){
      grid.lines(c(x_bottom_left, x_bottom_left+(x_width/5)),
                 c(y_start[breaks], y_start[breaks]),default.units = 'native',gp=gpar(col='black')) 
      
      grid.lines(c(x_bottom_left+x_width, x_bottom_left+x_width-(x_width/5)),
                 c(y_start[breaks], y_start[breaks]),default.units = 'native',gp=gpar(col='black')) 
    }
                            
      grid.text(label[breaks],x=x_bottom_left+x_width+0.5,y=y_start[breaks]+0.1,gp = gpar(fontsize = 5),  default.units = 'native')
                            
  }
                          
  grid.text('Mean no. of contacts retained',x=x_bottom_left-0.3, y=y_bottom_left+(y_length/2),
            gp = gpar(fontsize = 5), rot=90, default.units = 'native')
  
   
                          
  grid.polygon(c(x_bottom_left,x_bottom_left,x_bottom_left+x_width,x_bottom_left+x_width),
               c(y_bottom_left,y_bottom_left+y_length,y_bottom_left+y_length,y_bottom_left),
               default.units = 'native',gp=gpar(fill=NA, col='black'))
                          
                          
}

png('figure/random_mean_contact_supp.png',height=8,width=8,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,2,2)))
pushViewport(viewport(layout=grid.layout(nrow=1,ncol=1)))

paneller(1,1)


popViewport()
popViewport()
dev.off()

rm(paneller)
rm(colourbar)
