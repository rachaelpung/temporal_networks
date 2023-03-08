#' List of functions for analysing outbreaks on temporal vs static networks
#' @author Rachael Pung

# edge list in respective time unit
network_time <- function(net, net_meta, time_unit){
  
  if(length(grep(pattern ='cruise', net_meta$network))==1){
    
    net[, step_start:=time_start-net_meta$time_start]
    net[, step_start:=ceiling(step_start/time_unit)]
    net[, steps:=floor(duration/time_unit)]
    net=net[steps>=1]
    # net[,step_end:=step_start+steps-1]
    
    steps = net$steps
    net = net[,c(1,2,8)]
    net = net[rep(1:nrow(net), times=steps)]
    
    steps = lapply(1:length(steps), function(x){ seq(0,steps[x]-1,1) })
    steps = unlist(steps)
    
    net[,step_start:=step_start+steps]
    setnames(net, 'step_start', 'step')
    
    net[,contact:=1]
    net[,next_step:=step+1]
    net[net,next_contact:=i.contact, on=c(node_i='node_i', node_j='node_j', next_step='step')]
    net[is.na(next_contact), next_contact:=0]
    
    return(net)
  } else{
    scale = 60  # 60 for non cruise, 60s interaction in 1hr window; 12 for haslemere
    net[, step_start:=time_start-net_meta$time_start]
    net[, step_start:=ceiling(step_start/(time_unit*scale))]
    net[, step_end:=time_end-net_meta$time_start]
    net[, step_end:=ceiling(step_end/(time_unit*scale))]
    
    # for high_school_2012 and high_school_2013
    net[, date_start:=as.Date(as.POSIXct(time_start, origin="1970-01-01", tz = 'Etc/GMT-7'))]
    net[, date_end:=as.Date(as.POSIXct(time_end, origin="1970-01-01", tz = 'Etc/GMT-7'))]
    # if(net[date_start!=date_end,.N] !=0) stop("contact overflowed to next day")
    
    # for contacts that last between two intervals, separate them
    net_subset = net[step_start!=step_end]
    net = net[step_start==step_end]
    
    rep_row=net_subset$step_end-net_subset$step_start+1
    step = lapply(1:nrow(net_subset), function(x){
      seq(net_subset[x,]$step_start,net_subset[x,]$step_end,1)
    })
    step=unlist(step)
    net_subset = net_subset[rep(1:nrow(net_subset), times=rep_row)]
    net_subset[, step_start:=step]
    net_subset[, step_end:=step]
    
    net_subset[!cumsum(rep_row), time_end:=NA]
    net_subset[!(c(cumsum(rep_row)[1:length(rep_row)-1]+1,1)), time_start:=NA]
    
    int = data.table(time=seq(net_meta$time_start, net_meta$time_end, time_unit*scale))
    int[,step:=1:nrow(int)]
    
    net_subset[, next_step:=step_start+1]
    
    net_subset[int, time:=i.time, on=c(next_step='step')]
    net_subset[is.na(time_end), time_end:=time]
    
    net_subset[int, time:=i.time, on=c(step_start='step')]
    net_subset[is.na(time_start), time_start:=time]
    
    
    net_subset[, duration:=time_end-time_start]
    net_subset[, time:=NULL]
    net_subset[, next_step:=NULL]
    
    net = rbind(net,net_subset)
    setorder(net, node_i, node_j, time_start, time_end)
    rm(net_subset)
    
    net = net[duration>0]
    net = net[,sum(duration), by=.(node_i, node_j, step_end)] 
    # net = net[,sum(duration), by=.(node_i, node_j, date_end)] 
    setnames(net, c('step_end', 'V1'), c('step', 'duration'))
    
    ## figure out how to tidy this part on time unit
    # filter for contacts that last for more than 15 mins in a 30 mins time window
    net=net[duration>=time_unit]
    net[,contact:=1]
    net[,next_step:=step+1]
    net[net,next_contact:=i.contact, on=c(node_i='node_i', node_j='node_j', next_step='step')]
    net[is.na(next_contact), next_contact:=0]
    
    return(net)
  }

}


# aggregate contacts by node and time unit
contact_time <- function(net){
  
  
  
  ct = net[,.(sum(contact), sum(next_contact)), by=.(node_i, step)]
  setnames(ct, c('node','step','k0','r')) # r for retain
  
  min_step = min(ct$step); max_step = max(ct$step)
  tmp=data.table(node = rep(unique(ct$node), each=max_step-min_step+1),
                 step = seq(min_step, max_step, 1))
  
  ct=merge(tmp, ct, by=c('node', 'step'), all=T)
  ct[is.na(k0), `:=`(k0=0, r=0)]
  
  ct[,next_step:=step+1]
  ct[ct, k1:=i.k0, on=c(next_step='step', node='node')]
  # NA for k1 occur due to truncation of data at end of study
  
  ct = ct[!is.na(k1),]
  
  return(ct)
}

# degree distribution
pmf_k0 <- function(ct){
  
  ct = ct[k0!=0]
  pmf = ct[,.N, by=.(k0)]
  setorder(pmf, k0)
  pmf[,P:=N/sum(N)]
  
  return(pmf)
}

# degree retained distribution conditional on degree in previous time step
pmf_k0_retain <- function(ct){
  
  pmf = ct[,.N, by=.(k0,r)]
  setorder(pmf, k0, r)
  pmf[,P:=N/sum(N), by=.(k0)]
  
  return(pmf)
}

# degree distribution in one time step conditional on degree in previous time step
pmf_k0_k1 <- function(ct){
  
  pmf = ct[,.N, by=.(k0,k1)]
  pmf = pmf[!is.na(k1)]
  setorder(pmf, k0, k1)
  pmf[,P:=N/sum(N), by=.(k0)]
  
  return(pmf)
}

# degree retained distribution 
pmf_retain <- function(p_k0_retain, p_k0){
  
  
}

# 
pmf_k0_retain_avg <- function(p_k0){
  
  x = pmf_k0_rand$P
  len=length(x)
  
  retain_rand = data.table(k0 = rep(1:len, each=len),
                           k1 = rep(1:len, times=len),
                           pmf_k0_k1 = x)
  retain_rand = retain_rand[rep(1:.N, times=len+1)]
  retain_rand[, r:=rep(0:len, each=len*len)]
  retain_rand = retain_rand[r<=k0 & r<=k1]
  retain_rand[, p:=k1/len]
  setorder(retain_rand, k0,k1,r)
  
  retain_rand[, p_r:=choose(k0,r)*(p^r)*((1-p)^(k0-r))]
  retain_rand[k1<k0, p_r:=p_r/sum(p_r), by=.(k0,k1)]
  sum(retain_rand$p_r)
  
  retain_rand[, p_r_combi:=pmf_k0_k1*p_r]
  sum(retain_rand$p_r_combi)
  
  
}

# base plot
plot_k <- function(p_k0, p_k0_r, p_k0_k1){
  
  colline = c('black','red','blue','green','yellow','orange','pink','turquoise')
  collegend = c("1 contact", "2 contact", '3 contact', '4 contact',
                '5 contact','6 contact','7 contact', '8 contact')
  
  plot(p_k0$k0, p_k0$P, type='l', xlab='no. of contacts in one time unit', ylab='prop')
  
  for(i in 1:8){
    
    if(i==1) plot(p_k0_r[k0==i]$r, p_k0_r[k0==i]$P, type='l', xlim=c(0,8), ylim=c(0,1),
                  xlab='no. of contacts retained in next time unit', ylab='prop', col=colline[i])  
    if(i!=1) lines(p_k0_r[k0==i]$r, p_k0_r[k0==i]$P, col=colline[i])
    if(i==8) legend("topright", legend=collegend, col=colline, lty=1, cex=0.6)
    
    
  }
  
  for(i in 1:8){
    
    if(i==1) plot(p_k0_k1[k0==i]$k1, p_k0_k1[k0==i]$P, type='l', xlim=c(0,8), ylim=c(0,1),
                  xlab='no. of contacts in next time unit', ylab='prop', col=colline[i])  
    if(i!=1) lines(p_k0_k1[k0==i]$k1, p_k0_k1[k0==i]$P, col=colline[i])
    if(i==8) legend("topright", legend=collegend, col=colline, lty=1, cex=0.6)
    
    
  }
  
  
  # compare with poisson distribution 
  p_k0_k1[, P_cum:=cumsum(P), by=.(k0)]
  p_pois = sapply(0:13, function(x){ cumsum(dpois(0:50, x))[x+1] }) # cum prob of less than equal to k0
  
  plot(p_k0_k1[k0==k1]$k0, p_k0_k1[k0==k1]$P_cum, xlab='no of contact in current unit', 
       ylab='prop', ylim=c(0,1), type='l')
  lines(0:13, p_pois, col='red')
  
}

# generate static network
network_stat <- function(net, sample_step){
  
  net = net[step==sample_step,c('node_i', 'node_j','step','contact')]
  net = rbind(net, net)
  net[,step:=rep(c(sample_step, sample_step+1), each =.N/2)]
  
  net[, next_step:=step+1]
  net[net, next_contact:=i.contact, on=c(node_i='node_i', node_j='node_j', next_step='step')]
  
  return(net)
}

# generate temporal network
network_temp <- function(net, sample_step){
  
  net = net[step %in% c(sample_step,sample_step+1),c('node_i', 'node_j','step','contact')]
  setorder(net, step)
  
  net[, next_step:=step+1]
  net[net, next_contact:=i.contact, on=c(node_i='node_i', node_j='node_j', next_step='step')]
  # net = net[step==sample_step]
  net[step==sample_step & is.na(next_contact), next_contact:=0]
  
  return(net)
}

# generate random network
network_rand <- function(net, sample_step){
  
  net = net[step==sample_step,c('node_i', 'node_j','step','contact')]
  
  graph_net = graph_from_data_frame(net, directed=T)
  n_edges = length(E(graph_net))
  n_nodes = length(V(graph_net))
  
  # randomise static network but retain degree distribution
  net_rand = lapply(1:100, function(x){
    
    rand = copy(graph_net)
    rand = rewire(rand, keeping_degseq(niter=n_edges))
    rand = as_edgelist(rand)
    rand = data.table(rand)
    setnames(rand, c('node_i', 'node_j'))
    
    rand[, step:=sample_step+1]
    rand[, contact:=1]
    rand = rbind(net, rand)
    
    rand[, next_step:=step+1]
    rand[rand, next_contact:=i.contact, on=c(node_i='node_i', node_j='node_j', next_step='step')]
    # rand = rand[step==sample_step]
    rand[step==sample_step & is.na(next_contact), next_contact:=0]
    
    # rand[, set:=x]
    
  })
  
  return(net_rand)
}


# 
