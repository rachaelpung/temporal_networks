#' List of functions for analysing outbreaks on temporal vs static networks
#' @author Rachael Pung

# edge list in respective time unit
network_time <- function(net, net_meta, time_unit){
  
  if(length(grep(pattern ='cruise|haslemere', net_meta$network))==1){
    
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
    scale = 4
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
    
    # net_subset = net_subset[rep(1:nrow(net_subset), each=2)]
    # net_subset[seq(1,nrow(net_subset),2), time_end:=NA]
    # net_subset[seq(2,nrow(net_subset),2), time_start:=NA]
    
    
    ##
    rep_row=net_subset$step_end-net_subset$step_start+1
    step = lapply(1:nrow(net_subset), function(x){
      seq(net_subset[x,]$step_start,net_subset[x,]$step_end,1)
    })
    step=unlist(step)
    net_subset = net_subset[rep(1:nrow(net_subset), times=rep_row)]
    net_subset[, step_start:=step]
    net_subset[, step_end:=step]
    
    test = copy(net_subset)
    test[, time_start:=ifelse(row_number()==1,time_start, NA), by=.(node_i, node_j, time_start)]
    
    test[!cumsum(rep_row), time_end:=NA]
    test[!(c(cumsum(rep_row)[1:length(rep_row)-1]+1,1)), time_start:=NA]
    net_subset=copy(test)
    
    int = data.table(time=seq(net_meta$time_start, net_meta$time_end, time_unit*scale))
    int[,step:=1:nrow(int)]
    
    net_subset[, next_step:=step_start+1]
    
    net_subset[int, time:=i.time, on=c(next_step='step')]
    net_subset[is.na(time_end), time_end:=time]
    
    net_subset[int, time:=i.time, on=c(step_start='step')]
    net_subset[is.na(time_start), time_start:=time]
    
    
    ##
    
    # int = data.table(time=seq(net_meta$time_start, net_meta$time_end, time_unit*scale))
    # int[,step:=1:nrow(int)]
    # net_subset[int, time:=i.time, on=c(step_end='step')]
    # net_subset[is.na(time_end), time_end:=time]
    # net_subset[is.na(time_start), time_start:=time]
    # 
    net_subset[, duration:=time_end-time_start]
    net_subset[, time:=NULL]
    
    net_subset[, next_step:=NULL]
    
    # net_subset[, step_start:=time_start-net_meta$time_start]
    # net_subset[, step_start:=ceiling(step_start/(time_unit*scale))]
    # net_subset[, step_end:=time_end-net_meta$time_start]
    # net_subset[, step_end:=ceiling(step_end/(time_unit*scale))]
    # 
    net = rbind(net,net_subset)
    setorder(net, node_i, node_j, time_start, time_end)
    rm(net_subset)
    
    net = net[duration>0]
    net = net[,sum(duration), by=.(node_i, node_j, step_end)] 
    # net = net[,sum(duration), by=.(node_i, node_j, date_end)] 
    setnames(net, c('step_end', 'V1'), c('step', 'duration'))
    
    ## figure out how to tidy this part on time unit
    # filter for contacts that last for more than 15 mins in a 30 mins time window
    net=net[duration>=900]
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
  setnames(ct, c('node','step','k0','k0_retain'))
  
  min_step = min(ct$step); max_step = max(ct$step)
  tmp=data.table(node = rep(unique(ct$node), each=max_step-min_step+1),
                 step = seq(min_step, max_step, 1))
  
  ct=full_join(ct, tmp, by=c('node', 'step'))
  ct[is.na(k0), `:=`(k0=0, k0_retain=0)]
  setorder(ct, node, step)
  
  ct[,next_step:=step+1]
  ct[ct, k1:=i.k0, on=c(next_step='step', node='node')]
  # NA for k1 occur due to truncation of data at end of study
  
  return(ct)
}


# rewires network
# takes an association matrix network, and returns a network in specified format ("graph","matrix","edgelist") 
# under the specified null ("edge","deg","latt","clust").

# retain all nodes
# select time unit randomly
# rewire the networks 
# one iteration will be one static graph
# multiple iterations will be one set of random graphs

network_null<-function(net,returns=c("graph","matrix","edgelist"),null=c("deg")){ 
  
  # sample network for a step as static network
  # extract network for the next step 
  # check how many nodes were retained
  # randomise network in the next step and check how many were retained
  # check the degree distribution in real life network in current step,
  # real life network in next step and in the randomised network
  
  sample_step = sample(c(net$step),1)
  n_node_net = uniqueN(c(net$node_i,net$node_j))
  
  net_i = net[step==sample_step,c('node_i', 'node_j')]
  n_node_net_i = uniqueN(c(net_i$node_i, net_i$node_j))
  
  
  net_i = graph_from_data_frame(net_i, directed=T) 
  #convert to igraph object, check if need to amend for weights
  
  # check how many links were retains for net in next step 
  
  
  n_edges = length(E(net_i))
  n_nodes = length(V(net_i))
  
  if(null=="deg"){
    # Degree null - using the same nodes and edges (and weights) and degree distribution (number of unique partners) 
    # and re-shuffling the edges around them:
    # Maintains: General network structure (n nodes, n links), daily consistency, 
    # Individual differences in social contact propensity, daily consistency. 
    # Randomises/losses: Clustering, hidden structure
    net_r_i<-rewire(net_i,keeping_degseq(niter=n_edges))
  }
 
  
  
  #object return type:
  # E(net_r_i)$weight<-eweights<-sample(E(net_i)$weight,length(E(net_i)$weight))
  if(returns=="graph"){return(net_r_i)}
  if(returns=="matrix"){return(as.matrix(as_adj(net_r_i,type="both",attr="weight")))}
  if(returns=="edgelist"){return(cbind(as_edgelist(net_r_i),eweights))}
}