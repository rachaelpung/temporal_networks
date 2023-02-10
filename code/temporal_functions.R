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
    if(net[date_start!=date_end,.N] !=0) stop("contact overflowed to next day")
    
    # for contacts that last between two intervals, separate them
    net_subset = net[step_start!=step_end]
    net = net[step_start==step_end]
    
    net_subset = net_subset[rep(1:nrow(net_subset), each=2)]
    net_subset[seq(1,nrow(net_subset),2), time_end:=NA]
    net_subset[seq(2,nrow(net_subset),2), time_start:=NA]
    
    int = data.table(time=seq(net_meta$time_start, net_meta$time_end, time_unit))
    int[,step:=1:nrow(int)]
    net_subset[int, time:=i.time, on=c(step_end='step')]
    net_subset[is.na(time_end), time_end:=time]
    net_subset[is.na(time_start), time_start:=time]
    
    net_subset[, duration:=time_end-time_start]
    net_subset[, time:=NULL]
    
    net_subset[, step_start:=time_start-net_meta$time_start]
    net_subset[, step_start:=ceiling(step_start/(time_unit*scale))]
    net_subset[, step_end:=time_end-net_meta$time_start]
    net_subset[, step_end:=ceiling(step_end/(time_unit*scale))]
    
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













#' Generates aggregated and temporal weighted edgelist
#' @param in_edgelist raw temporal edgelist
# make_edgelist <- function(in_edgelist){
#   
#   out_edgelist = vector('list', 4)
# 
#   # raw
#   out_edgelist[[4]] = copy(in_edgelist)
#   
#   record_days = max(c(in_edgelist$day_start, in_edgelist$day_end))
#   record_duration = 133200 # (5+24+8) hr x 60 mins x 60 sec
#   
#   # scale = log(1-0.95)/-60*60 # 95% saturation on exponential function after 1 hour of contact
#   # 
#   # max_weight = 1
#   # x0 = 45*60 # transmission risk is less than 5% for contacts lasting less than 30mins
#   # k = log((max_weight/0.95)-1)/(-(60*60-x0)) # 95% saturation on logistic function after 1 hour of contact
#   
#   summary_edgelist = copy(in_edgelist)
#   summary_edgelist = summary_edgelist[,.(cum_duration=sum(duration)), 
#                                        by=.(node_i, node_j, day_start)]
#   
#   # temporal, linear/exponential/logistic function saturation
#   # cumulative duration in a day
#   summary_edgelist[day_start==1, weight:=cum_duration/(60*60*5)]
#   summary_edgelist[day_start==2, weight:=cum_duration/(60*60*24)]
#   summary_edgelist[day_start==3, weight:=cum_duration/(60*60*8)]
#   out_edgelist[[1]] = copy(summary_edgelist)
#   # out_edgelist[[2]] = copy(summary_edgelist[, weight:=(1-exp(-scale*cum_duration))])
#   # out_edgelist[[3]] = copy(summary_edgelist[, weight:=max_weight/(1+exp(-k*(cum_duration-x0)))])
#   
#   
#   summary_edgelist = summary_edgelist[,.(cum_duration=sum(cum_duration),
#                                        contact_days=uniqueN(day_start)), 
#                                       by=.(node_i, node_j)]
#   
#   # static, linear/exponential/logistic function saturation
#   # cumulative duration
#   out_edgelist[[2]] = copy(summary_edgelist[, weight:=cum_duration/(record_duration)])
#   # out_edgelist[[5]] = copy(summary_edgelist[, weight:=1-exp(-scale*cum_duration)])
#   # out_edgelist[[6]] = copy(summary_edgelist[, weight:=max_weight/(1+exp(-k*(cum_duration-x0)))])
#   
#   # mean duration of contact per day
#   summary_edgelist = summary_edgelist[,.(mean_duration=(cum_duration/record_duration)*(60*60*24),
#                                        prob_contact=contact_days/record_days),
#                                       by=.(node_i, node_j)]
#   
#   # static, linear/exponential/logistic saturation 
#   # mean duration, prob of contact
#   out_edgelist[[3]] = copy(summary_edgelist[, weight:=prob_contact*mean_duration/(60*60*24)])
#   # out_edgelist[[8]] = copy(summary_edgelist[, weight:=prob_contact*(1-exp(-scale*mean_duration))])
#   # out_edgelist[[9]] = copy(summary_edgelist[, weight:=prob_contact*max_weight/(1+exp(-k*(mean_duration-x0)))])
#  
#   names(out_edgelist) = c('t_lin', 's_lin_cum', 's_lin_avg', 'raw')
#   
#   # names(out_edgelist) = c('t_lin', 't_exp', 't_log',
#   #                           's_lin_cum', 's_exp_cum', 's_log_cum',
#   #                           's_lin_avg', 's_exp_avg', 's_log_avg',
#   #                           'raw')
#    
#   return(out_edgelist)
# }
