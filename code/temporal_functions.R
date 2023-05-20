#' List of functions for analysing outbreaks on temporal vs static networks
#' @author Rachael Pung

# edge list in respective time unit
network_time <- function(net, net_meta, time_unit, scale){
  
  if(length(grep(pattern ='cruise', net_meta$network))==1){
    
    net[, step_start:=time_start-net_meta$time_start]
    net[, step_start:=ceiling(step_start/(time_unit*scale))]
    net[, steps:=floor(duration/(time_unit*scale))]
    net=net[steps>=1]
    # net[,step_end:=step_start+steps-1]
    
    steps = net$steps
    net = net[,c('node_i','node_j','duration','step_start')]
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
    # scale = 60  # 60 for non cruise, 60s interaction in 1hr window; 12 for haslemere
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
  
  ct = net[,.(sum(duration), sum(contact), sum(next_contact)), by=.(node_i, step)]
  setnames(ct, c('node','step','duration','k0','r')) # r for retain
  
  min_step = min(ct$step); max_step = max(ct$step)
  tmp=data.table(node = rep(unique(ct$node), each=max_step-min_step+1),
                 step = seq(min_step, max_step, 1))
  
  ct=merge(tmp, ct, by=c('node', 'step'), all=T)
  ct[is.na(k0), `:=`(k0=0, r=0)]
  
  ct[,next_step:=step+1]
  ct[ct, k1:=i.k0, on=c(next_step='step', node='node')]
  # NA for k1 occur due to truncation of data at end of study
  
  ct = ct[!is.na(k1),]
  ct[is.na(duration), duration:=0]
  
  return(ct)
}

# degree distribution
pmf_k0 <- function(ct, n_nodes_total){
  
  ct = ct[k0!=0]
  pmf = ct[,.N, by=.(k0)]
  pmf = rbind(pmf, data.table(k0=0,N=n_nodes_total-sum(pmf$N)))
  setorder(pmf, k0)
  pmf[,P:=N/sum(N)]
  
  return(pmf)
}

# degree retained distribution conditional on degree in previous time step
pmf_k0_retain <- function(ct, n_nodes_total){
  
  pmf = ct[,.N, by=.(k0,r)]
  setorder(pmf, k0, r)
  pmf = pmf[k0!=0]
  pmf = rbind(data.table(k0=0,r=0,N=n_nodes_total-sum(pmf$N)), pmf)
  pmf[,P:=N/sum(N), by=.(k0)]
  setorder(pmf, k0, r)
  
  return(pmf)
}

# degree distribution in one time step conditional on degree in previous time step
pmf_k0_k1 <- function(ct, n_nodes_total){
  
  pmf = ct[,.N, by=.(k0,k1)]
  pmf = pmf[!is.na(k1)]
  pmf = rbind(data.table(k0=0,k1=0,N=n_nodes_total-sum(pmf$N)), pmf)
  setorder(pmf, k0, k1)
  pmf[,P:=N/sum(N), by=.(k0)]
  
  return(pmf)
}

# degree retained distribution empirical 
pmf_retain <- function(p_k0_retain, p_k0){
  
  pmf = copy(p_k0_retain)
  pmf[p_k0, p_k0:=i.P, on=c(k0='k0')]
  pmf = pmf[!is.na(p_k0)]
  
  pmf[, p_r:=P*p_k0]
  pmf = pmf[, sum(p_r), by=.(r)]
  setnames(pmf, old='V1', new='P')
  
  return(pmf)
}

# degree retained distribution by k0, k1 for random graphs
pmf_k0_k1_retain_avg <- function(p_k0, n_nodes){
  
  max_k0 = max(p_k0$k0)
  N = n_nodes
  N = N-1 # max number of contacts formed
  
  pmf = data.table(k0 = rep(p_k0$k0, each=p_k0[,.N]), 
                   k1 = rep(p_k0$k0, times=p_k0[,.N]),
                   p_k1 = p_k0$P)  # cause network in each time step is independent
  
  pmf = pmf[rep(1:.N, times=max_k0+1)]
  pmf[, r:=rep(0:max_k0, each=p_k0[,.N]*p_k0[,.N])] 
  pmf = pmf[r<=k0 & r<=k1]
  pmf[, p:=k1/N] # probability of retaining each contact
  setorder(pmf, k0, k1, r)
  
  pmf[, p_binom:=choose(k0,r)*(p^r)*((1-p)^(k0-r))]
  pmf[k1<k0, p_binom:=p_binom/sum(p_binom), by=.(k0,k1)]

  pmf[, P:=p_binom*p_k1]
  # sum(pmf$p_binom); sum(pmf$P)
  
  return(pmf)
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

plot_k_r <- function(p_k0_r_stat, p_k0_r_temp, p_k0_r_rand, p_k0_k1_r_rand_avg){
  
  max_k0 = max(p_k0$k0)
  
  p_k0_r = data.table(k0=1:max_k0)
  p_k0_r = p_k0_r[rep(1:.N, each=max_k0+1)]
  p_k0_r = p_k0_r[,r:=rep(0:max_k0, times=max_k0)]
  p_k0_r = p_k0_r[k0>=r]
  p_k0_r = merge(p_k0_r, p_k0_r_stat, by=c('k0','r'), all=T)
  p_k0_r = merge(p_k0_r, p_k0_r_temp, by=c('k0','r'), all=T)
  p_k0_r = merge(p_k0_r, p_k0_r_rand, by=c('k0','r'), all=T)
  
  p_k0_r_rand_avg = p_k0_k1_r_rand_avg[,sum(P), by=.(k0,r)]
  p_k0_r = merge(p_k0_r,p_k0_r_rand_avg, by=c('k0','r'), all=T)
  setnames(p_k0_r, c('k0', 'r', 'N_stat', 'P_stat', 'N_temp', 'P_temp', 'N_rand', 'P_rand', 'P_rand_avg'))
  
  p_k0_r[is.na(p_k0_r),] = 0
  p_k0_r = p_k0_r[k0>=1]
  
  plot(p_k0_r[r==0 & k0<=5]$k0,p_k0_r[r==0 & k0<=5]$P_rand_avg, type='l', ylim=c(0,1))
  lines(p_k0_r[r==0 & k0<=5]$k0,p_k0_r[r==0 & k0<=5]$P_temp, col='red')
  lines(p_k0_r[r==0 & k0<=5]$k0,p_k0_r[r==0 & k0<=5]$P_stat, col='blue')
  
  plot(p_k0_r[r==k0 & k0<=5]$k0,p_k0_r[r==k0 & k0<=5]$P_rand_avg, type='l', ylim=c(0,1))
  lines(p_k0_r[r==k0 & k0<=5]$k0,p_k0_r[r==k0 & k0<=5]$P_temp, col='red')
  lines(p_k0_r[r==k0 & k0<=5]$k0,p_k0_r[r==k0 & k0<=5]$P_stat, col='blue')
  
  
}

plot_r <- function(p_r_stat, p_r_temp, p_r_rand, p_r_rand_avg){
  
  max_r = max(p_r_stat$r)
  
  p_r = data.table(r=0:max_r)
  p_r = merge(p_r, p_r_stat, by=c('r'), all=T)
  p_r = merge(p_r, p_r_temp, by=c('r'), all=T)
  p_r = merge(p_r, p_r_rand, by=c('r'), all=T)
  p_r = merge(p_r, p_r_rand_avg, by=c('r'), all=T)
  setnames(p_r, c('r', 'P_stat', 'P_temp', 'P_rand', 'P_rand_avg'))
  
  p_r[is.na(p_r),] = 0
 
  plot(p_r[r<=5]$r,p_r[r<=5]$P_rand_avg, type='l', ylim=c(0,1))
  lines(p_r[r<=5]$r,p_r[r<=5]$P_temp, col='red')
  lines(p_r[r<=5]$r,p_r[r<=5]$P_stat, col='blue')
  
}

# generate static network
network_stat <- function(net, sample_step){
  
  net = net[step==sample_step,c('node_i', 'node_j','duration', 'step','contact')]
  net = rbind(net, net)
  net[,step:=rep(c(sample_step, sample_step+1), each =.N/2)]
  
  net[, next_step:=step+1]
  net[net, next_contact:=i.contact, on=c(node_i='node_i', node_j='node_j', next_step='step')]
  
  return(net)
}

# generate temporal network
network_temp <- function(net, sample_step){
  
  net = net[step %in% c(sample_step,sample_step+1),c('node_i', 'node_j', 'duration', 'step','contact')]
  setorder(net, step)
  
  net[, next_step:=step+1]
  net[net, next_contact:=i.contact, on=c(node_i='node_i', node_j='node_j', next_step='step')]
  # net = net[step==sample_step]
  net[step==sample_step & is.na(next_contact), next_contact:=0]
  
  return(net)
}

# generate random network
network_rand <- function(net, sample_step){
  
  net = net[step==sample_step,c('node_i','node_j','duration','step','contact')]
  
  graph_net = graph_from_data_frame(net, directed=T)
  # graph_net = set_edge_attr(graph_net, 'weight', value=net$duration)
  n_edges = length(E(graph_net))
  n_nodes = length(V(graph_net))
  
  # randomise static network but retain degree distribution
  net_rand = lapply(1:100, function(x){
    
    rand = copy(graph_net)
    rand = rewire(rand, keeping_degseq(niter=n_edges*100))
    rand = as_edgelist(rand)
    rand = data.table(rand)
    setnames(rand, c('node_i', 'node_j'))
    
    rand[, duration:=net$duration]
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

# tab k0_r
tab <- function(out, col_name){
  
  col_num = which(names(out[[1]]) == col_name)
  out_x = lapply(out, `[[`, col_num) # extract the relevant list from output
  
  tab_output = lapply(1:length(out_x), function(i){
    
    s=min(out[[i]]$el_stat$step)
    
    if(is.data.table(out_x[[i]])){
      out_x[[i]][,step:=s]
    } else if(length(out_x[[i]])!=0){
      # for p_k0_r_rand
      out_x = lapply(1:100, function(j){
        
        out_xx = copy(out_x[[i]][[j]])
        out_xx[, step:=s]
        out_xx[, set:=j]
      })
      
      out_x_data_table = rbindlist(out_x)
      return(out_x_data_table)
    }
    
  })
  tab_output = rbindlist(tab_output)
  
  if(col_name=='p_k0_k1_r_rand_avg'){ 
    tab_output=tab_output[, sum(P), by=.(k0,r,step)]
    setnames(tab_output, old='V1', new='P')
  }
  
  return(tab_output)
  
}

# check deg dist
check_deg_dist <- function(out){
  
  diff = lapply(1:(length(out)-1), function(x){
    
    temp_diff = copy(out[[x]][['p_k0_stat']])
    temp_diff[out[[x+1]][['p_k0_temp']], P2:=i.P, on=c(k0='k0')]
    temp_diff[!is.na(P2), diff:=P-P2]
    temp_diff[, step:=x]
    temp_diff[, net_type:='temp']
    
    # rand_diff = lapply(1:100, function(y){
    # 
    #   rand_set_diff = copy(out[[x]][['p_k0_stat']])
    #   rand_set_diff[out[[x]][['p_k0_rand']][[y]], P2:=i.P, on=c(k0='k0')]
    #   rand_set_diff[!is.na(P2), diff:=P-P2]
    #   rand_set_diff[, step:=x]
    #   rand_set_diff[, net_type:='rand']
    #   rand_set_diff[, set:=y]
    # 
    #   return(rand_set_diff)
    # })
  
    return(temp_diff)   
  })
  
  diff = rbindlist(diff)
  return(diff[diff > 0.1,])
  
}

diff_p_k0_r <- function(p_stat, p_temp, p_rand, p_rand_avg){
  
  # temporal and static network
  diff_temp_stat = copy(p_temp[(r==0 | r==k0)  & k0!=0])
  diff_temp_stat[r==0, P_stat:=0]
  diff_temp_stat[r==k0, P_stat:=1]
  setorder(diff_temp_stat, r, k0)
  setnames(diff_temp_stat, old='P', new='P_temp')
  
  diff_temp_stat[,diff_abs:=abs(P_temp-P_stat)]
  diff_temp_stat[r==0,diff_rel:=diff_abs/P_temp] # multiply by 100 to get %
  diff_temp_stat[r==k0,diff_rel:=diff_abs/P_stat]
  
  # temporal and random network
  diff_temp_rand = copy(p_temp[(r==0 | r==k0)  & k0!=0])
  diff_temp_rand = diff_temp_rand[rep(1:.N, times=100)]
  diff_temp_rand[, set:=rep(1:100, each=.N/100)]
  
  diff_temp_rand[p_rand, P_rand:=i.P, on=c(k0='k0', r='r', step='step', set='set') ]
  diff_temp_rand[is.na(P_rand), P_rand:=0]
  setorder(diff_temp_rand, r, step, set, k0)
  setnames(diff_temp_rand, old='P', new='P_temp')
  
  diff_temp_rand[,diff_abs:=abs(P_temp-P_rand)]
  diff_temp_rand[r==0,diff_rel:=diff_abs/P_rand] # multiply by 100 to get %
  diff_temp_rand[r==k0,diff_rel:=diff_abs/P_temp]
  
  # temporal and random avg network
  diff_temp_rand_avg = copy(p_temp[(r==0 | r==k0)  & k0!=0])
  diff_temp_rand_avg[p_rand_avg, P_rand_avg:=i.P, on=c(k0='k0', r='r', step='step') ]
  setorder(diff_temp_rand_avg, r, k0, step)
  setnames(diff_temp_rand_avg, old='P', new='P_temp')
  
  diff_temp_rand_avg[,diff_abs:=abs(P_temp-P_rand_avg)]
  diff_temp_rand_avg[r==0,diff_rel:=diff_abs/P_rand_avg] # multiply by 100 to get %
  diff_temp_rand_avg[r==k0,diff_rel:=diff_abs/P_temp]
  
  return(list(diff_temp_stat=diff_temp_stat,
              diff_temp_rand=diff_temp_rand,
              diff_temp_rand_avg=diff_temp_rand_avg))
}
  
diff_p_r <- function(p_stat, p_temp, p_rand, p_rand_avg){
  
  # temporal and static network
  diff_temp_stat = copy(p_stat)
  diff_temp_stat[p_temp, P_temp:=i.P, on=c(r='r', step='step')]
  diff_temp_stat[is.na(P_temp), P_temp:=0]
  setorder(diff_temp_stat, r)
  setnames(diff_temp_stat, old='P', new='P_stat')
  
  diff_temp_stat[,diff_abs:=abs(P_temp-P_stat)]
  diff_temp_stat[,diff_rel:=diff_abs/P_stat] # multiply by 100 to get %
  
  # temporal and random network
  diff_temp_rand = copy(p_temp)
  diff_temp_rand = diff_temp_rand[rep(1:.N, times=100)]
  diff_temp_rand[, set:=rep(1:100, each=.N/100)]
  
  diff_temp_rand[p_rand, P_rand:=i.P, on=c(r='r', step='step', set='set') ]
  diff_temp_rand[is.na(P_rand), P_rand:=0]
  # setorder(diff_temp_rand, r, step, set)
  setnames(diff_temp_rand, old='P', new='P_temp')
  
  diff_temp_rand[,diff_abs:=abs(P_temp-P_rand)]
  diff_temp_rand[,diff_rel:=diff_abs/P_temp] # multiply by 100 to get %
  
  # temporal and random avg network
  diff_temp_rand_avg =  copy(p_temp)
  diff_temp_rand_avg[p_rand_avg, P_rand_avg:=i.P, on=c(r='r', step='step') ]
  # setorder(diff_temp_rand_avg, r, step)
  setnames(diff_temp_rand_avg, old='P', new='P_temp')
  
  diff_temp_rand_avg[,diff_abs:=abs(P_temp-P_rand_avg)]
  diff_temp_rand_avg[,diff_rel:=diff_abs/P_temp] # multiply by 100 to get %
  
  return(list(diff_temp_stat=diff_temp_stat,
              diff_temp_rand=diff_temp_rand,
              diff_temp_rand_avg=diff_temp_rand_avg))
}

mean_r <- function(p_stat, p_temp, p_rand, p_rand_avg){
  
  p_stat[, r_P:=r*P]
  mean_r_stat = p_stat[, sum(r_P), by=.(step)]
  
  p_temp[, r_P:=r*P]
  mean_r_temp = p_temp[, sum(r_P), by=.(step)]
  
  p_rand[, r_P:=r*P]
  mean_r_rand = p_rand[, sum(r_P), by=.(step, set)]
  
  p_rand_avg[, r_P:=r*P]
  mean_r_rand_avg = p_rand_avg[, sum(r_P), by=.(step)]
  
  return(list(mean_r_stat=mean_r_stat, mean_r_temp=mean_r_temp,
              mean_r_rand=mean_r_rand, mean_r_rand_avg=mean_r_rand_avg))
  
}

# identify top nodes that account for 80% of the contacts in a time step or 80% of the contact duration
p80_contact <- function(net, sample_step, type=c('contacts', 'duration')){
  
  if(type=='contacts') net_sub = setorder(net[step==sample_step], -k0)
  if(type=='duration') net_sub = setorder(net[step==sample_step], -duration)
  
  if(net_sub[,.N]!=0 & sum(net_sub$k0)>1) {
    
    if(type=='contacts'){
      net_sub[,k0_cum:=cumsum(k0)]
      net_total = sum(net_sub$k0)
    }
    
    if(type=='duration'){
      net_sub[,k0_cum:=cumsum(duration)]
      net_total = sum(net_sub$duration)
    }
    
    if(length(net_sub[k0_cum<net_total*0.8, which=T])==0){
      net_row = 0
    }else{
      net_row = max(which(net_sub$k0_cum<net_total*0.8))
    }
    net_row = net_row + 1
    
    # find nodes that account for 80% of the contact episodes or contact duration
    net_p80 = net_sub[1:net_row, .(step,node)]  
    
    return(net_p80)
  } else{
    NULL
  }
  
}

# rank top nodes and compute duration and deg
node_rank <- function(net_p80, ct){
  
  net_r80 = net_p80[,.N, by=.(node)]
  setorder(net_r80, -N)
  setorder(net_p80, node)
  
  net_p80[, int_dur:=lead(step,n=1)-step, by=.(node)]
  int_dur = lapply(1:3, function(x){
    p=c(0.25,0.5,0.75)
    net_p80[,quantile(int_dur, probs=p[x], na.rm = T), by=.(node)]
  })
  net_r80[int_dur[[1]], dur_lwr:=i.V1, on=c(node='node')]
  net_r80[int_dur[[2]], dur_med:=i.V1, on=c(node='node')]
  net_r80[int_dur[[3]], dur_upp:=i.V1, on=c(node='node')]
  
  net_deg = ct[,.(median(k0)), by=.(node)]
  net_r80[net_deg, deg_med:=i.V1, on=c(node='node')]
  
  return(net_r80)
  
}

# proportion of nodes that consistently appear over time 
node_sustain <- function(net_p80){
  
  start_row = c(1,net_p80[int_dur!=1 | is.na(int_dur), which=T]+1)
  end_row = c(start_row-1)
  start_row=start_row[-length(start_row)]
  end_row = end_row[-1]
  
  net_p80[, step_start:=step]
  net_p80[start_row, step_end:=net_p80[end_row,step]]
  
  net_s80 = net_p80[!is.na(step_end)]
  net_s80[, step:=NULL]
  net_s80[, dur:=step_end-step_start+1]
  
  net_s80 = net_s80[,.N, by=.(step_start,dur)]
  setorder(net_s80, step_start, dur)
  net_s80[,P:=N/sum(N), by=.(step_start)]
  
  return(net_s80)
  
}

# scale networks
scale_network <- function(p_stat, p_temp, p_rand, p_rand_avg, p_temp_k0_r){
  
  p_stat = p_stat[,sum(r_P), by=.(step)]
  p_temp = p_temp[,sum(r_P), by=.(step)]
  p_rand = p_rand[,sum(r_P), by=.(step,set)]
  p_rand = p_rand[, median(V1), by=.(step)]
  p_rand_avg = p_rand_avg[,sum(r_P), by=.(step)]
  
  # retain steps if number of retained contacts is more than equal to 3
  # steps = p_temp_k0_r[r!=0, sum(N), by=.(step)]
  # steps = steps[V1>=3]$step
  
  
  r_scale = data.table(step = p_stat$step, stat=p_stat$V1, temp=p_temp$V1,
                       rand=p_rand$V1, rand_avg=p_rand_avg$V1)
  r_scale[, scale_norm:=(temp-rand)/(stat-rand)]
  r_scale[, scale_norm2:=(temp-rand_avg)/(stat-rand_avg)]
  r_scale = r_scale[-.N]
  
  r_scale = r_scale[temp!=0]
  # r_scale = r_scale[step%in%steps]
  
  return(r_scale)
}




