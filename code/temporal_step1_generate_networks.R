# check if edgelist is undirected
# check if edgelist is individual time packs or continuous contact episode
# http://www.math.pitt.edu/~lewicka/Semester_DiscrNetw_14/MNlecture22.pdf


set.seed(123)


# load in raw data set
load("data/el_cruise_1.RData")
load("data/el_cruise_3.RData")
load("data/el_haslemere.RData")
load("data/el_high_school_2012.RData")
load("data/el_work_2015.RData")


source("code/temporal_functions.R")
source("code/temporal_library.R")

# check how many contacts have cross over days interaction
el = el_cruise_1
cruise_date_start = as.Date('2020-11-06')
cruise_time_start = as.numeric(as.POSIXct(paste(cruise_date_start, 
                                                '19:00:00', sep = ' '), 
                                          tz = 'Etc/GMT-8'))

el[day_end>day_start,.N]
el[day_end>day_start,summary(duration)]

el_x = el[day_end>day_start,]
el = el[day_end==day_start,]

el_x_pairs = unique(el_x[,.(node_i,node_j)])
el_x_pairs[, match:=1]

el[el_x_pairs, match:=i.match, on=c(node_i='node_i', node_j='node_j')]
el_x = rbindlist(list(el_x, el[match==1,]), fill = TRUE)
el = el[is.na(match)]

el_x[,match:=NULL]
el[,match:=NULL]

el_x = lapply(1:el_x_pairs[,.N], function(x){
  
  # subset
  el_sub = el_x[node_i==el_x_pairs[x,node_i] & node_j==el_x_pairs[x,node_j]]
  diff=as.numeric()
  
  for(i in 1:3){
    
    el_sub_day = el_sub[day_start==i]
    el_sub = el_sub[day_start!=i]
    
    cum_dur = el_sub_day[,sum(duration)]
    if(i==1) max_dur = 5*60*60 
    if(i==2) max_dur = 24*60*60 
    
    if(cum_dur>max_dur) {
      
      setorder(el_sub_day, day_end)
      el_sub_day_fix = el_sub_day[day_start==day_end]
      el_sub_day_exp = el_sub_day[day_end>day_start]
      el_sub_day_exp = el_sub_day_exp[rep(nrow(el_sub_day_exp), 
                                          day_end-day_start+1)]
      
      chg_time = c(5,24)*60*60
      chg_time = cumsum(chg_time) + cruise_time_start
      
      row=el_sub_day_exp[,.N]
      index=i:(i+row-2)
      el_sub_day_exp[1:(row-1), time_end:=chg_time[index]]
      el_sub_day_exp[2:row, time_start:=chg_time[index]]
      
      el_sub_day_exp[,day_end:=seq(i, by=1, length.out = row)]
      el_sub_day_exp[,day_start:=seq(i, by=1, length.out = row)]
      el_sub_day_exp[,duration:=time_end-time_start]
      diff = c(diff, abs(el_sub_day_exp[row,duration]-el_sub_day[1,duration]))
      
      el_sub_day=rbind(el_sub_day_fix, el_sub_day_exp)
    }
    
    el_sub = rbind(el_sub, el_sub_day)
  }
  
  diff = data.table(duration=diff, 
                    node_i=el_x_pairs[x,node_i], 
                    node_j=el_x_pairs[x,node_j])
  return(list(el_sub,diff))
  
  
})

diff=lapply(el_x, `[[`, 2)
diff=rbindlist(diff)
diff=diff[!is.na(duration)]

el_x=lapply(el_x, `[[`, 1)
el_x=rbindlist(el_x)

el = rbind(el, el_x)
setorder(el, node_i, node_j, time_start, time_end)

# creates edgelist
el = make_edgelist(el)

# evaluate network properties of each node
# set up clusters
cl = makeCluster(detectCores())
clusterExport(cl, as.vector(lsf.str()))
registerDoParallel(cl)

foreach(i = 1:3, .packages = c('data.table', 'igraph'), .combine = 'c') %dopar%  {
  
  if(i %in% 1:3) record_days=3
  if(i %in% 4:9) record_days=1
  
  node_prop = eval_prop(el[[i]], record_days)
  save(node_prop,file= paste0("output/node_prop/node_prop_", i, ".rdata"))

}

# # stop clusters
# stopCluster(cl) 

# evaluate distribution of network properties
foreach(i = 1:9, .packages = c('data.table', 'igraph'), .combine = 'c') %dopar%  {
  
  if(i %in% 1:3) record_days=3
  if(i %in% 4:9) record_days=1
  
  load(paste0("output/node_prop/node_prop_", i, ".rdata"))
  node_prop_dist = eval_prop_dist(node_prop[[1]], record_days)
  save(node_prop_dist,file= paste0("output/node_prop_dist/node_prop_dist_", i, ".rdata"))
  
}


# load all node_prop data
file = dir('output/node_prop/')
node_prop=lapply(c(1,4,7), function(x){
  get(load(paste('output/node_prop/', file[x], sep = '')))
})
  
# evaluate nodes, links, global clustering coefficient 
net_prop = lapply(1:length(node_prop), function(x){
  
  d = node_prop[[x]][,unique(day)]
  num_nodes = num_links = sum_links = global_cc = as.numeric()
  
  for(i in 1:length(d)){
    num_nodes = c(num_nodes, node_prop[[x]][day==d[i],.N])
    num_links = c(num_links, node_prop[[x]][day==d[i],sum(deg)])
    sum_links = c(sum_links, node_prop[[x]][day==d[i],sum(strength)])
    global_cc = c(global_cc, node_prop[[x]][day==d[i],mean(cluster, na.rm=TRUE)])
  }
  
  data.table(day=d, num_nodes, num_links, sum_links, global_cc)
  
})


par(mfrow=c(3,3))

# degree distribution 
# histogram, mean, median, mode (static likely to overestimate cause its the cumulative)
# line graph, x axis day, y axis degree (temporal)
# quantify the variation over time
for(i in 1:9){
  
  day = node_prop[[i]][,unique(day)]
  for(d in day){
    
   label = floor(c(mean(node_prop[[i]][day==d, deg]),
                   median(node_prop[[i]][day==d, deg])))
   
   hist(node_prop[[i]][day==d, deg],
        main = paste('mean = ', label[1], ', median = ', label[2]),
        xlab ='deg', ylab = 'prob', freq = FALSE, 
        breaks = seq(0,1000,50)) 
    
  }
}

for(i in 1:3){
  
  setorder(node_prop[[i]], id, day)
  node_id = node_prop[[i]][,unique(id)]
  num_nodes = net_prop[[i]][,num_nodes]
  
  plot(1:3, node_prop[[i]][id==node_id[1], deg], type='l', ylim=c(0,500),
       xlab='day', ylab='deg')

  for(n in 2:num_nodes){
    
    if(node_prop[[i]][id==node_id[1], .N]>1){
      lines(node_prop[[i]][id==node_id[n], day], 
            node_prop[[i]][id==node_id[n], deg])
    }
  }
}

# strength distribution 
# histogram, mean, median, mode (static likely to overestimate cause its the cumulative)
# line graph, x axis day, y axis degree (temporal)
# quantify the variation over time
par(mfrow=c(3,3))

for(i in 1:9){
  
  day = node_prop[[i]][,unique(day)]
  for(d in day){
    
    label = floor(c(mean(node_prop[[i]][day==d, strength]),
                    median(node_prop[[i]][day==d, strength])))
    
    if(i %in%c(1,3,4,7,9)) {int=1;xmax=50}
    if(i %in%c(6)) {int=1;xmax=100}
    if(i %in%c(2,5,8)) {int=1;xmax=1000}
    
    hist_plot = hist(node_prop[[i]][day==d, strength],
         main = paste('mean = ', label[1], ', median = ', label[2]),
         xlab ='strength', ylab = 'prob', freq = FALSE, 
         breaks = seq(0,xmax,int), plot=FALSE) 
    
    plot(hist_plot$mids, hist_plot$counts/sum(hist_plot$counts), log='xy', 
         xlab='mid_degree', ylab='prob')
    
  }
}

# clustering coefficient (wrt group of contacts at time of interaction or wrt to all known contacts)
par(mfrow=c(3,3))

for(i in 1:9){
  
  day = node_prop[[i]][,unique(day)]
  for(d in day){
    
    label = signif(c(mean(node_prop[[i]][day==d, cluster], na.rm=T),
                    median(node_prop[[i]][day==d, cluster], na.rm=T)),
                   digits=1)
    
    hist(node_prop[[i]][day==d & !is.na(cluster), cluster],
         main = paste('mean = ', label[1], ', median = ', label[2]),
         xlab ='cluster', ylab = 'prob', freq = FALSE, 
         breaks = seq(0,1,0.1)) 
    
  }
}

# betweeness centrality (number of shortest path between all pairs of nodes that passes through that node)
par(mfrow=c(3,3))

for(i in 1:9){
  
  day = node_prop[[i]][,unique(day)]
  for(d in day){
    
    label = signif(c(mean(node_prop[[i]][day==d, between], na.rm=T),
                    median(node_prop[[i]][day==d, between], na.rm=T)),
                   digits=1)
    
    hist(node_prop[[i]][day==d & !is.na(between), between],
         main = paste('mean = ', label[1], ', median = ', label[2]),
         xlab ='between', ylab = 'prob', freq = FALSE, 
         breaks = seq(0,0.1,1e-3)) 
    
  }
}

# closeness centrality 
par(mfrow=c(3,3))

for(i in 1:9){
  
  boxplot(close~day, data= node_prop[[i]],
         main = '',
         xlab ='day', ylab = 'close') 
    
}

# eigencentrality 
par(mfrow=c(3,3))

for(i in 1:9){
  
  day = node_prop[[i]][,unique(day)]
  for(d in day){
    
    label = floor(c(mean(node_prop[[i]][day==d, eigen], na.rm=T),
                    median(node_prop[[i]][day==d, eigen], na.rm=T)))
    
    if(i %in%c(1,3,4,6,7,9)) {int=0.05;xmax=1}
    if(i %in%c(2,5,8)) {int=0.1;xmax=1}
    
    hist(node_prop[[i]][day==d & !is.na(eigen), eigen],
         main = paste('mean = ', label[1], ', median = ', label[2]),
         xlab ='eigen', ylab = 'prob', freq = FALSE, 
         breaks = seq(0,xmax,int)) 
    
  }
}
# generate network models
el_model = lapply(1:3, function(x){
  
  n = 1
  d = 2
  
  node = net_prop[[n]][day==d,num_nodes]
  link = net_prop[[n]][day==d,num_links]
  mean_strength = net_prop[[n]][day==d,sum_links]/link
  mean_cluster = net_prop[[n]][day==d,global_cc]
  
  # random graph
  if(x==1){
    graph = erdos.renyi.game(node, link, 'gnm')
    w = el[[1]][day_start==d, weight]
    w = sample(w)
    edge_attr(graph) = list(weight = w)
    edge_attr(graph, 'weight') = E(graph)$weight
  }
  
  # scale free
  
  
  # generate scale free network with largely similar number of links 
  # generate small world network, 
  # with number of neighbours determined by the number of links
  # and probability of relink by amending the clustering coefficient to be close to the observed one. 
  
  
  
})


# things to study
node_prop[[1]][,`:=` (mean_deg=mean(deg),
                      mean_strength=mean(strength),
                      sd_deg=sd(deg),
                      sd_strength=sd(strength),
                      median_deg=median(deg),
                      median_strength=median(strength),
                      max_deg=max(deg),
                      max_strength=max(strength),
                      min_deg=min(deg),
                      min_strength=min(strength),
                      skew_deg=skewness(deg),
                      skew_strength=skewness(strength)), by=.(id)]

# 1. degree at one point in time (x-axis) against expected degree (y-axis)
# one individual can have high (or low) expected degree but high degree in one day, 
# and low degree in another. Collectively if all individuals with high (or low) 
# expected degree has a random mix of high and low degree in a day (i.e. low correlation
# or coefficient of the gradient less than 1 or more than 1), then for someone with a high 
# expected degree (e.g. past data), their deg for that day can be lower (or higher) than expected . 
# In other words, hard to predict when super spreading will happen
plot(node_prop[[1]][day==1]$mean_deg, node_prop[[1]][day==1]$deg,xlim=c(0,350),ylim=c(0,350))
plot(node_prop[[1]][day==2]$mean_deg, node_prop[[1]][day==2]$deg,xlim=c(0,350),ylim=c(0,350))
plot(node_prop[[1]][day==3]$mean_deg, node_prop[[1]][day==3]$deg,xlim=c(0,350),ylim=c(0,350))
lines(0:600, 0:600, col='red')

plot(node_prop[[1]][day==1]$mean_strength, node_prop[[1]][day==1]$strength,xlim=c(0,8),ylim=c(0,8))
plot(node_prop[[1]][day==2]$mean_strength, node_prop[[1]][day==2]$strength,xlim=c(0,8),ylim=c(0,8))
plot(node_prop[[1]][day==3]$mean_strength, node_prop[[1]][day==3]$strength,xlim=c(0,8),ylim=c(0,8))
lines(0:6, 0:6, col='red')


# 2. COV of degree and strength, identify which has lower variation
# COV of degree or strength tells us the dispersion from the mean deg or strength
# COV of degree is quite uniformly distributed around 1, COV of strength less than 1
# if you have very strong (weak) strength of contact with others in a day, you are 
# likely to have the same behavior the next day with the same or other individuals
# COV of strength is less variable. I.e. High strength individuals tend to have 
# similar strength; dispersion around the mean is low

# Hypothesis: if correlated, it is a static network with higher deg/strength individuals 
# as superspreaders. If not correlated, then super spreading events are random
# and individuals involved cannot be predicted from past data

# if target only high degree individuals how many others do you then neglect?

plot(node_prop[[1]][day==1]$mean_deg, node_prop[[1]][day==1]$sd_deg,xlim=c(0,400),ylim=c(0,400))
lines(x=0:400, y=0:400, col='red')

plot(node_prop[[1]][day==1]$mean_strength, node_prop[[1]][day==1]$sd_strength,xlim=c(0,8),ylim=c(0,8))
lines(x=0:8, y=0:8, col='red')

# Hypothesis: if high median (mean) deg/strength and high positive skewness, then these individuals
# can potentially be super spreaders. High median/mean degree or strength but points are 
# randomly distributed, with points lying both positive and negative skew, then knowing the 
# median/mean degree of an individual doesn't provide info on the tendency for the individual 
# to under or overspread. i.e. individual can have many close interaction in a day, 
# then rest or rest for most of the time then interact a lot on one day and both can have 
# same median/mean deg/strength
plot(node_prop[[1]][day==1]$mean_strength, node_prop[[1]][day==1]$skew_strength)
plot(node_prop[[1]][day==1]$median_strength, node_prop[[1]][day==1]$skew_strength)
plot(node_prop[[1]][day==1]$skew_strength, node_prop[[1]][day==1]$mean_strength)
plot(node_prop[[1]][day==1]$skew_strength, node_prop[[1]][day==1]$median_strength)
lines(x=0:7, y=rep(0,8), col='red')

plot(node_prop[[1]][day==1]$median_deg, node_prop[[1]][day==1]$skew_deg)
plot(node_prop[[1]][day==1]$mean_deg, node_prop[[1]][day==1]$skew_deg)
lines(x=0:350, y=rep(0,351), col='red')

# 122C0002F637 low variance but high skewness of 0.7


# 3. degree in a day (x-axis) against cumulative duration (y-axis)
# Hypothesis: show if intervention should focus on limiting degree (group size) 
# or limiting duration of contact. e.g. does high degree in a day imply high strength
# do individuals tend to have many contacts but low strength or low number of contacts
# and high strength

# need to evaluate day by day and case by case basis
weight_per_contact = el[[1]][, .(mean(weight), median(weight)), by=c('node_i', 'day_start')]
node_prop[[1]][weight_per_contact, `:=` (mean_weight=i.V1,
                                         median_weight=i.V2), on=c(id='node_i', day='day_start')]


plot(node_prop[[1]][day==1]$mean_deg, node_prop[[1]][day==1]$mean_strength)


plot(node_prop[[1]][day==1]$deg, node_prop[[1]][day==1]$mean_weight)
plot(node_prop[[1]][day==2]$deg, node_prop[[1]][day==2]$mean_weight)
plot(node_prop[[1]][day==3]$deg, node_prop[[1]][day==3]$mean_weight)

plot(node_prop[[1]][day==1]$deg, node_prop[[1]][day==1]$median_weight)
plot(node_prop[[1]][day==2]$deg, node_prop[[1]][day==2]$median_weight)
plot(node_prop[[1]][day==3]$deg, node_prop[[1]][day==3]$median_weight)

# policy will determine if you remove people on the lower right corner or top left corner
# mask for transient or limit group size

# 4. correlation between duration of contact and inter-event duration
# Hypothesis: if correlated, helps predict the timing of the next contact event
# allows us to understand the burstiness of the network

# use the raw contact network
el_cruise_1[,total_episodes:=.N, by=.(node_i, node_j)]
el_cruise_1[,inter_event_duration:=lead(time_start)-time_end, by=.(node_i, node_j)]
el_cruise_1[,inter_event_duration_forward:=time_start-lag(time_end), by=.(node_i, node_j)]


load("~/Desktop/PhD/modelling COVID-19/cruise networks/Cruise network/output/nodes/dataNodes_20201106.RData")


# retain only pairs with more than one contact episodes
plot(el_cruise_1[!is.na(inter_event_duration)]$inter_event_duration, 
     el_cruise_1[!is.na(inter_event_duration)]$duration)

el_cruise_1[dataNodes, node_i_type:=i.TYPE, on=c(node_i='MID')]
el_cruise_1[dataNodes, node_j_type:=i.TYPE, on=c(node_j='MID')]

plot(el_cruise_1[!is.na(inter_event_duration) & node_i_type=='C' & node_j_type=='C' & duration > 60*60]$duration, 
     el_cruise_1[!is.na(inter_event_duration) & node_i_type=='C' & node_j_type=='C' & duration > 60*60]$inter_event_duration,
     xlab='duration', ylab='inter_event_duration')

plot(el_cruise_1[!is.na(inter_event_duration) & node_i_type=='P' & node_j_type=='P' & duration > 30*60]$inter_event_duration, 
     el_cruise_1[!is.na(inter_event_duration) & node_i_type=='P' & node_j_type=='P' & duration > 30*60]$duration,
     xlab='inter_event_duration', ylab='duration')

plot(el_cruise_1[!is.na(inter_event_duration) & ((node_i_type=='P' & node_j_type=='C') | (node_i_type=='C' & node_j_type=='P')) & duration > 30*60]$duration, 
     el_cruise_1[!is.na(inter_event_duration) & ((node_i_type=='P' & node_j_type=='C') | (node_i_type=='C' & node_j_type=='P')) & duration > 30*60]$inter_event_duration,
     xlab='duration', ylab='inter_event_duration')

loess_curve <- loess(inter_event_duration ~ duration, data=el_cruise_1[!is.na(inter_event_duration) & node_i_type=='P' & node_j_type=='P' & duration > 30*60])
smooth_curve <- predict(loess_curve) 
lines(smooth_curve, x=el_cruise_1[!is.na(inter_event_duration) & node_i_type=='P' & node_j_type=='P' & duration > 30*60]$duration, col='red')

hist_plot = hist(el_cruise_1[!is.na(inter_event_duration) & node_i_type=='P' & node_j_type=='P' & duration > 30*60]$inter_event_duration/el_cruise_1[!is.na(inter_event_duration) & node_i_type=='P' & node_j_type=='P' & duration > 30*60]$duration, 
     breaks=seq(0,135000,1), plot=FALSE)

plot(hist_plot$mids, hist_plot$counts/sum(hist_plot$counts), log='xy', 
     xlab='mid_int_event_duration_over_duration', ylab='prob')

# forward
hist_plot = hist(el_cruise_1[!is.na(inter_event_duration_forward) & node_i_type=='P' & node_j_type=='P' & duration > 30*60]$inter_event_duration/el_cruise_1[!is.na(inter_event_duration_forward) & node_i_type=='P' & node_j_type=='P' & duration > 30*60]$duration, 
                 breaks=seq(0,135000,1), plot=FALSE)

plot(hist_plot$mids, hist_plot$counts/sum(hist_plot$counts), log='xy', 
     xlab='mid_int_event_duration_over_duration', ylab='prob')



median(el_cruise_1[!is.na(inter_event_duration) & node_i_type=='P' & node_j_type=='P' & duration > 30*60]$inter_event_duration/el_cruise_1[!is.na(inter_event_duration) & node_i_type=='P' & node_j_type=='P' & duration > 30*60]$duration)
quantile(el_cruise_1[!is.na(inter_event_duration) & node_i_type=='P' & node_j_type=='P' & duration > 30*60]$inter_event_duration/el_cruise_1[!is.na(inter_event_duration) & node_i_type=='P' & node_j_type=='P' & duration > 30*60]$duration, probs=c(0.025,0.975))
# [1] 0.1617303,        2.5%       97.5% 
#                  0.01609281 15.24063189 

# for inter event duration / duration more than 1, 
# short inter event duration/ longer duration of contact 16% of the contact patterns
# for duration of contact more than x time of contact tracing limits,
# % of inter event duration over duration that is more than 1, are events that we can prevent? 
# one close contact have begets more close contact? 

# above is for overall
# below is for individual

test = lapply(1:1000, function(x){
  
  node_i_sample = sample(el_cruise_1[!is.na(inter_event_duration) & node_i_type=='P' & node_j_type=='P' & duration > 30*60,unique(node_i)], 1)
  node_j_sample = sample(el_cruise_1[!is.na(inter_event_duration) & node_i_type=='P' & node_j_type=='P' & duration > 30*60 & node_i==node_i_sample,unique(node_j)], 1)
  
  ratio = el_cruise_1[node_i==node_i_sample & node_j==node_j_sample]$inter_event_duration/el_cruise_1[node_i==node_i_sample & node_j==node_j_sample]$duration
  median = median(ratio, na.rm = T)
  upp95 = quantile(ratio, probs=c(0.975), na.rm = T)
  low95 = quantile(ratio, probs=c(0.025), na.rm = T)
 
  output = data.table(median=median, upp95=upp95, low95=low95)
  
  return(output)
})

test= rbindlist(test)
median(test$median)
median(test$upp95)
median(test$low95)

# do a negative binomial regression for all pairs and then pull strength? eg hierachical modelling?
# subset for people with few data points and those with more 
# https://stats.oarc.ucla.edu/r/dae/negative-binomial-regression/


# distinguish super spreaders, i.e. individuals with many contacts over multiple days and can spread to most
# persons at one event or over different events VS superspreading events, i.e. events where you can have multiple infectors

# For each individual, identify the time window where 50% of the deg is made, 
# and what is the corresponding strength. 
# are there any specific time periods where a lot of people made their contacts?
# or do super spreader spread out their contact events
nodes = unique(c(el_cruise_1$node_i,el_cruise_1$node_j))

test = lapply(1:10, function(x){
  
  el_node = el_cruise_1[day_start==1 & node_i==nodes[x]]
  setorder(el_node, time_start)
  el_node[,row_num:=1:.N]
  
  row_num=1:nrow(el_node)
  row_num=t(combn(row_num,2))[,2] 
  row_num=c(1:nrow(el_node), row_num)
  
  output=data.table(row_num, set_num=rep(1:nrow(el_node), times=seq(nrow(el_node),1,-1)))
  output=inner_join(output,el_node, by=c('row_num'))
  output[,cum_duration:=cumsum(duration), by=.(set_num)]
  
  output[,count:=1]
  output[which(duplicated(output, by=c('node_j','set_num'))==TRUE), count:=0,]
  output[,cum_degree:=cumsum(count), by=.(set_num)]
  
  output[, time_start_window:=rep(el_node$time_start, times=seq(nrow(el_node),1,-1))]
  deg_cutoff = el_node[, uniqueN(node_j)]*0.5
  
  output[,diff:=time_start-time_start_window]
  output[cum_degree>deg_cutoff,][output[cum_degree>deg_cutoff,which.min(diff)]]
  
  
})

test = rbindlist(test)

# can we predict a superspreader? at the time of super spreading what is the degree of the infector
# what is the expected degree of the person at other times
# if target only high degree individuals how many others do you then neglect?

# find the inter event duration by days 
# static assumes a uniform probability, but in reality it might be negative binomial distributed
el_pair=unique(el_cruise_1, by=c('node_i', 'node_j', 'day_start'))
el_pair=test[, c('node_i', 'node_j', 'day_start')]
el_pair[,inter_event_day:=lead(day_start)-day_start, by=.(node_i, node_j)]

el_pair_weight = el[[1]][, sum(weight), by=.(node_i, node_j, day_start)]
el_pair[el_pair_weight, weight:=i.V1, on=c(node_i='node_i', node_j='node_j', day_start='day_start')]

summary(el_pair[inter_event_day==1, weight])
summary(el_pair[inter_event_day==2, weight])

hist_1 = hist(el_pair[inter_event_day==1, weight], freq=FALSE, breaks = seq(0,1,0.05))
hist_2 = hist(el_pair[inter_event_day==2, weight], freq=FALSE, breaks = seq(0,1,0.05))

hist_plot = hist(el_pair[!is.na(inter_event_day)]$inter_event_day/el_pair[!is.na(inter_event_day)]$weight, seq(0,100000,1000), plot=FALSE)

plot(hist_plot$mids, hist_plot$counts/sum(hist_plot$counts), log='xy', 
     xlab='mid_int_event_duration_over_duration', ylab='prob')

# above for overall
# cant do for individual cause not enough data points


# below doesnt adjust by weight
hist(test$inter_event_day, breaks=c(0:3))
# % 0
n=uniqueN(test[,c('node_i', 'node_j')])
test[,.N, by=.(inter_event_day)]
test[,N:=.N, by=.(node_i, node_j)]
test[is.na(inter_event_day) & N==1,.N]

94142/(n)
2892/(n)

530828/(n)


# check if inter event duration in the past will affect future contact
# what are the implications if inter contact duration is negative binomially distributed
# does this explains why some transmission are negative binomially distributed

summary(el_cruise_1[!is.na(inter_event_duration_forward) ]$inter_event_duration)



# probability of contacts over time
cruise_date_start = as.Date('2021-01-29')
cruise_time_start = as.numeric(as.POSIXct(paste(cruise_date_start, 
                                                '19:00:00', sep = ' '), 
                                          tz = 'Etc/GMT-8'))

cruise_date_end = as.Date('2021-01-31')
cruise_time_end = as.numeric(as.POSIXct(paste(cruise_date_end, 
                                                '08:00:00', sep = ' '), 
                                          tz = 'Etc/GMT-8'))

haslemere_date_start = as.Date('2017-10-12')
haslemere_time_start = as.numeric(as.POSIXct(paste(haslemere_date_start, 
                                                '07:00:00', sep = ' '), 
                                          tz = 'UTC'))

haslemere_date_end = as.Date('2017-10-14')
haslemere_time_end = as.numeric(as.POSIXct(paste(haslemere_date_end, 
                                              '22:55:00', sep = ' '), 
                                        tz = 'UTC'))

min(el_high_school_2012$time_start)
as.POSIXct(1353303360, origin="1970-01-01", tz = 'Etc/GMT-7')
max(el_high_school_2012$time_end)
as.POSIXct(1354032880, origin="1970-01-01", tz = 'Etc/GMT-7')


high_sch_12_date_start = as.Date('2017-10-12')
high_sch_12_time_start = as.numeric(as.POSIXct(paste(high_sch_12_date_start, 
                                                   '07:00:00', sep = ' '), 
                                             tz = 'UTC'))

high_sch_12_date_end = as.Date('2017-10-14')
high_sch_12_time_end = as.numeric(as.POSIXct(paste(high_sch_12_date_end, 
                                                 '22:55:00', sep = ' '), 
                                           tz = 'UTC'))

cruise_time_start+3600
(cruise_time_end-cruise_time_start)/900

# for cruise and haslemere
el = copy(el_cruise_1)

el[, step_start:=time_start-cruise_time_start]
el[, step_start:=ceiling(step_start/60)]
el[, step_end:=floor(duration/60)]
el=el[step_end>=1]
# el_step[,step_end:=step_start+step_end-1]


step=el$step_end
el_step=el[,c(1,2,8)]
el_step=el_step[rep(1:nrow(el_step), times=step)]
step = lapply(1:length(step), function(x){
  
  seq(0,step[x]-1,1)
    
})

step=unlist(step)
el_step$step_start = el_step$step_start + step 
names(el_step)[3] = 'step'

xx=el_step[,sum(.N), by=.(node_i,step)]
setorder(xx,node_i,step)
xx[,next_step:=step+1]
xx[xx, V2:=i.V1, on=c(next_step='step', node_i='node_i')]
xx[is.na(V2), V2:=0]



bb=data.table(node_i=rep(unique(xx$node_i),each=149),
              step=1:149)

bb[xx,V1:=i.V1, on=c(node_i='node_i', step='step')]
bb[is.na(V1), V1:=0]
cc=bb[,.N, by=.(V1)]
plot(cc$V1, cc$N/sum(cc$N), type='l', xlab='no of contacts in one time unit', ylab='prop')


cc=xx[,.N, by=.(V1,V2)]
setorder(cc,V1,V2)
cc[,T:=sum(N), by=.(V1)]
cc[,P:=N/T]

plot(cc[V1==1]$V2,cc[V1==1]$P, type ='l', xlab='no of contact in next time unit', ylab='prop')
lines(cc[V1==2]$V2,cc[V1==2]$P, col='red')
lines(cc[V1==3]$V2,cc[V1==3]$P, col='blue')
lines(cc[V1==4]$V2,cc[V1==4]$P, col='green')
lines(cc[V1==5]$V2,cc[V1==5]$P, col='yellow')
lines(cc[V1==6]$V2,cc[V1==6]$P, col='orange')
lines(cc[V1==7]$V2,cc[V1==7]$P, col='pink')
lines(cc[V1==8]$V2,cc[V1==8]$P, col='turquoise')

legend("topright", legend=c("1 contact", "2 contact", '3 contact', '4 contact','5 contact','6 contact','7 contact', '8 contact'),
       col=c('black',"red", "blue", 'green','yellow','orange','pink','turquoise'), lty=1, cex=0.6)

cc[,P_cum:=cumsum(P), by=.(V1)]
plot(cc[V1==V2]$V1, cc[V1==V2]$P_cum, xlab='no of contact in current unit', ylab='prop',
     ylim=c(0,1), type='l')


# compare with poisson function
pois_dist = sapply(1:13, function(x){
  
  cumsum(dpois(0:50, x))[x+1]
  
})


lines(1:13, pois_dist, col='red')

# find unique contacts
yy=copy(el_step)
yy[, check:=1]
yy[,next_step:=step+1]
yy[yy, next_check:=i.check, on=c(next_step='step', node_i='node_i', node_j='node_j')]


zz=yy[,.(sum(.N),
      sum(next_check,na.rm=T)), by=.(node_i,step)]

zz[xx, V3:=i.V2, on=c(node_i='node_i', step='step', V1='V1')]

setnames(zz, old=c('V1','V2','V3'), new=c('no_cur','no_repeat','no_next'))

aa=zz[,.N, by=.(no_cur, no_repeat)]
setorder(aa, no_cur, no_repeat)
aa[,T:=sum(N), by=.(no_cur)]
aa[,P:=N/T]

plot(aa[no_cur==1]$no_repeat,aa[no_cur==1]$P, type ='l', xlab='no of repeated contact in next time unit', ylab='prop',xlim=c(0,8), ylim=c(0,1))
lines(aa[no_cur==2]$no_repeat,aa[no_cur==2]$P, col='red')
lines(aa[no_cur==3]$no_repeat,aa[no_cur==3]$P, col='blue')
lines(aa[no_cur==4]$no_repeat,aa[no_cur==4]$P, col='green')
lines(aa[no_cur==5]$no_repeat,aa[no_cur==5]$P, col='yellow')
lines(aa[no_cur==6]$no_repeat,aa[no_cur==6]$P, col='orange')
lines(aa[no_cur==7]$no_repeat,aa[no_cur==7]$P, col='pink')
lines(aa[no_cur==8]$no_repeat,aa[no_cur==8]$P, col='turquoise')

legend("topright", legend=c("1 contact", "2 contact", '3 contact', '4 contact','5 contact','6 contact','7 contact', '8 contact'),
       col=c('black',"red", "blue", 'green','yellow','orange','pink','turquoise'), lty=1, cex=0.6)


# for french high school and work

# set time window and time unit of interest (e.g. within 30 min window, if contact exist for more than 15 mins)
el = copy(el_high_school_2012)

min(el_high_school_2012$time_start)
max(el_high_school_2012$time_end)

as.POSIXct(1353303360, origin="1970-01-01", tz = 'Etc/GMT-7')

# step 1 is a range min_time_start to min_time_start + time window (e.g. 30*60)
el[, step_start:=time_start-min(el_high_school_2012$time_start)]
el[, step_start:=ceiling(step_start/3600)]
el[, step_end:=time_end-min(el_high_school_2012$time_start)]
el[, step_end:=ceiling(step_end/3600)]

el[, date_start:=as.Date(as.POSIXct(time_start, origin="1970-01-01", tz = 'Etc/GMT-7'))]
el[, date_end:=as.Date(as.POSIXct(time_end, origin="1970-01-01", tz = 'Etc/GMT-7'))]
el[date_start!=date_end,.N]

# for those contact that last between two intervals, make amendmends
el_subset = el[step_start!=step_end]
el=el[step_start==step_end]

el_subset = el_subset[rep(1:nrow(el_subset), each=2)]
el_subset[seq(1,nrow(el_subset),2), time_end:=NA]
el_subset[seq(2,nrow(el_subset),2), time_start:=NA]

int = data.table(time=seq(min(el_high_school_2012$time_start), max(el_high_school_2012$time_end)+60*60, 60*60))
int[,step:=1:nrow(int)]
el_subset[int, time:=i.time, on=c(step_end='step')]
el_subset[is.na(time_end), time_end:=time]
el_subset[is.na(time_start), time_start:=time]

el_subset[, duration:=time_end-time_start]
el_subset[, time:=NULL]

el_subset[, step_start:=time_start-min(el_high_school_2012$time_start)]
el_subset[, step_start:=ceiling(step_start/1800)]
el_subset[, step_end:=time_end-min(el_high_school_2012$time_start)]
el_subset[, step_end:=ceiling(step_end/1800)]

el=rbind(el,el_subset)
setorder(el, node_i, node_j, time_start, time_end)
rm(el_subset)

el=el[duration>0]

ee = el[,sum(duration), by=.(node_i, node_j, step_end)]
# ee = el[,sum(duration), by=.(node_i, node_j, date_end)]

ee=ee[V1>=900] # filter for contacts that last for more than 15 mins in a 30 mins time window
setnames(ee, 'step_end', 'step')
ee[,next_step:=step+1]
ee[ee, V2:=i.V1, on=c(next_step='step', node_i='node_i', node_j='node_j')]

ee[is.na(V2), V2:=0]

ff=ee[,.N,by=.(node_i, step)]

gg=data.table(node_i=rep(unique(ee$node_i),each=nrow(int)),
              step=1:nrow(int))
gg=data.table(node_i=rep(unique(ee$node_i),each=9),
              step=seq(min(el$date_start), max(el$date_end),1))

gg[ff,V1:=i.N, on=c(node_i='node_i', step='step')]
gg[is.na(V1), V1:=0]

hh=gg[,.N, by=.(V1)]
setorder(hh,V1)
plot(hh$V1, hh$N/sum(hh$N), type='l', xlab='no of contacts in one time unit', ylab='prop')

ii=ee[,.N, by=.(node_i, step)]
ii[,next_step:=step+1]
ii[ii, V2:=i.N, on=c(next_step='step', node_i='node_i')]
ii[is.na(V2), V2:=0]
setnames(ii, 'N', 'V1')


jj=ii[,.N, by=.(V1,V2)]
setorder(jj,V1,V2)
jj[,T:=sum(N), by=.(V1)]
jj[,P:=N/T]

plot(jj[V1==1]$V2,jj[V1==1]$P, type ='l', xlab='no of contact in next time unit', ylab='prop')
lines(jj[V1==2]$V2,jj[V1==2]$P, col='red')
lines(jj[V1==3]$V2,jj[V1==3]$P, col='blue')
lines(jj[V1==4]$V2,jj[V1==4]$P, col='green')
lines(jj[V1==5]$V2,jj[V1==5]$P, col='yellow')
lines(jj[V1==6]$V2,jj[V1==6]$P, col='orange')
lines(jj[V1==7]$V2,jj[V1==7]$P, col='pink')
lines(jj[V1==8]$V2,jj[V1==8]$P, col='turquoise')

legend("topright", legend=c("1 contact", "2 contact", '3 contact', '4 contact','5 contact','6 contact','7 contact', '8 contact'),
       col=c('black',"red", "blue", 'green','yellow','orange','pink','turquoise'), lty=1, cex=0.6)


# find unique contacts
yy=copy(ee)
yy[V2!=0, next_check:=1]


zz=yy[,.(sum(.N),
         sum(next_check,na.rm=T)), by=.(node_i,step)]

setnames(zz, old=c('V1','V2'), new=c('no_cur','no_repeat'))

aa=zz[,.N, by=.(no_cur, no_repeat)]
setorder(aa, no_cur, no_repeat)
aa[,T:=sum(N), by=.(no_cur)]
aa[,P:=N/T]

plot(aa[no_cur==1]$no_repeat,aa[no_cur==1]$P, type ='l', xlab='no of repeated contact in next time unit', ylab='prop',xlim=c(0,8), ylim=c(0,1))
lines(aa[no_cur==2]$no_repeat,aa[no_cur==2]$P, col='red')
lines(aa[no_cur==3]$no_repeat,aa[no_cur==3]$P, col='blue')
lines(aa[no_cur==4]$no_repeat,aa[no_cur==4]$P, col='green')
lines(aa[no_cur==5]$no_repeat,aa[no_cur==5]$P, col='yellow')
lines(aa[no_cur==6]$no_repeat,aa[no_cur==6]$P, col='orange')
lines(aa[no_cur==7]$no_repeat,aa[no_cur==7]$P, col='pink')
lines(aa[no_cur==8]$no_repeat,aa[no_cur==8]$P, col='turquoise')

legend("topright", legend=c("1 contact", "2 contact", '3 contact', '4 contact','5 contact','6 contact','7 contact', '8 contact'),
       col=c('black',"red", "blue", 'green','yellow','orange','pink','turquoise'), lty=1, cex=0.6)


