#' List of functions for analysing outbreaks on temporal vs static networks
#' @author Rachael Pung
 

#' Generates aggregated and temporal weighted edgelist
#' @param in_edgelist raw temporal edgelist
make_edgelist <- function(in_edgelist){
  
  out_edgelist = vector('list', 4)

  # raw
  out_edgelist[[4]] = copy(in_edgelist)
  
  record_days = max(c(in_edgelist$day_start, in_edgelist$day_end))
  record_duration = 133200 # (5+24+8) hr x 60 mins x 60 sec
  
  # scale = log(1-0.95)/-60*60 # 95% saturation on exponential function after 1 hour of contact
  # 
  # max_weight = 1
  # x0 = 45*60 # transmission risk is less than 5% for contacts lasting less than 30mins
  # k = log((max_weight/0.95)-1)/(-(60*60-x0)) # 95% saturation on logistic function after 1 hour of contact
  
  summary_edgelist = copy(in_edgelist)
  summary_edgelist = summary_edgelist[,.(cum_duration=sum(duration)), 
                                       by=.(node_i, node_j, day_start)]
  
  # temporal, linear/exponential/logistic function saturation
  # cumulative duration in a day
  summary_edgelist[day_start==1, weight:=cum_duration/(60*60*5)]
  summary_edgelist[day_start==2, weight:=cum_duration/(60*60*24)]
  summary_edgelist[day_start==3, weight:=cum_duration/(60*60*8)]
  out_edgelist[[1]] = copy(summary_edgelist)
  # out_edgelist[[2]] = copy(summary_edgelist[, weight:=(1-exp(-scale*cum_duration))])
  # out_edgelist[[3]] = copy(summary_edgelist[, weight:=max_weight/(1+exp(-k*(cum_duration-x0)))])
  
  
  summary_edgelist = summary_edgelist[,.(cum_duration=sum(cum_duration),
                                       contact_days=uniqueN(day_start)), 
                                      by=.(node_i, node_j)]
  
  # static, linear/exponential/logistic function saturation
  # cumulative duration
  out_edgelist[[2]] = copy(summary_edgelist[, weight:=cum_duration/(record_duration)])
  # out_edgelist[[5]] = copy(summary_edgelist[, weight:=1-exp(-scale*cum_duration)])
  # out_edgelist[[6]] = copy(summary_edgelist[, weight:=max_weight/(1+exp(-k*(cum_duration-x0)))])
  
  # mean duration of contact per day
  summary_edgelist = summary_edgelist[,.(mean_duration=(cum_duration/record_duration)*(60*60*24),
                                       prob_contact=contact_days/record_days),
                                      by=.(node_i, node_j)]
  
  # static, linear/exponential/logistic saturation 
  # mean duration, prob of contact
  out_edgelist[[3]] = copy(summary_edgelist[, weight:=prob_contact*mean_duration/(60*60*24)])
  # out_edgelist[[8]] = copy(summary_edgelist[, weight:=prob_contact*(1-exp(-scale*mean_duration))])
  # out_edgelist[[9]] = copy(summary_edgelist[, weight:=prob_contact*max_weight/(1+exp(-k*(mean_duration-x0)))])
 
  names(out_edgelist) = c('t_lin', 's_lin_cum', 's_lin_avg', 'raw')
  
  # names(out_edgelist) = c('t_lin', 't_exp', 't_log',
  #                           's_lin_cum', 's_exp_cum', 's_log_cum',
  #                           's_lin_avg', 's_exp_avg', 's_log_avg',
  #                           'raw')
   
  return(out_edgelist)
}

#' Evaluates network properties for edgelist
#' @param in_edgelist input edgelist
#' @param record_days number of days of recorded contacts
#' https://cambridge-intelligence.com/keylines-faqs-social-network-analysis/
#' https://www.sscnet.ucla.edu/soc/faculty/mcfarland/soc112/cent-ans.htm
eval_prop <- function(in_edgelist, record_days){
  
  nodes = tab_prop = data.table(id=unique(c(in_edgelist$node_i,in_edgelist$node_j)))
  out_prop = data.table()  
  
  for(d in 1:record_days){
    
    if(record_days==1) graph = graph_from_data_frame(in_edgelist, directed = TRUE, vertices = nodes)
    if(record_days!=1) graph = graph_from_data_frame(in_edgelist[day_start==d], directed = TRUE, vertices = nodes)
    graph = as.undirected(graph, mode ='collapse', edge.attr.comb="first")
    
    # edge_attr_names(graph)
    # print(graph, e=TRUE, v=TRUE)

    tab_prop[, day:=d]
    tab_prop[, deg:=degree(graph, v=id)]
    tab_prop[, strength:=strength(graph, v=id, weights=E(graph)$weight)]
    tab_prop[, eigen:=eigen_centrality(graph)$vector]
    tab_prop[, between:=betweenness(graph, v=nodes$id, directed = FALSE, normalized = TRUE)]
    tab_prop[, close:=closeness(graph, vids = V(graph), mode='all', weights=E(graph)$weight)]
    tab_prop[, cluster:=transitivity(graph,"weighted", weights=E(graph)$weight)]
    # (wrt group of contacts at time of interaction or wrt to all known contacts)
    
    assort = assortativity(graph, tab_prop[,strength])
    
    out_prop = rbind(out_prop, tab_prop)
  }
    
  return(list(out_prop,
              assort))
  
}

#' Evaluates network properties distributions 
#' @param in_prop input table of properties
#' @param record_days number of days of recorded contacts
eval_prop_dist <- function(in_prop, record_days){
  
  prop = c('deg', 'strength', 'eigen', 'between', 'close', 'cluster')
  n_nodes = in_prop[,.N]
  
  out_prop_dist = data.table()  
  
  for(d in 1:record_days){
    for(p in 1:length(prop)){
     
      col=which(colnames(in_prop)==prop[p])
      val=in_prop[day==d][[col]]
      
      intervals = seq(0,1,0.1)
      if(p %in% c(1,2)){intervals = 0:n_nodes}
      if(p==5){intervals = 0:1000}
      
      counts = hist(val, breaks = intervals, plot=FALSE)$counts
      
      tab_prop_dist = data.table(prop=prop[p], breaks=intervals[-length(intervals)], counts, record_day=d)
      
      out_prop_dist = rbind(out_prop_dist, tab_prop_dist)
      
    }
  }
  
  return(out_prop_dist)
}