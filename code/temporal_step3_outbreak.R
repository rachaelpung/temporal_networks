source("code/temporal_library.R")
# source("code/temporal_functions.R")

# load contacts in temporal and random network
load('output/results/20230403/n_contact_temp_param_01_cruise_1_phh.RData')
load('output/results/20230403/n_contact_rand_param_01_cruise_1_phh.RData')
load('output/results/20230403/net_param_01_cruise_1.rdata')

n_nodes = unique(net$el$node_i)
length(n_nodes)
max_step = net$max_step
n_sample = 1000
time_unit_stat = 48 # cruise 1(15min), 2(30min), 4(1hr), 24(6 hr), 48(12 hours)
time_unit_temp = 96 # non cruise non haslemere 1(20sec), 3(1min), 15(5min), 45(15 min), 90(30mins), 180(1 hours), 1080(6 hr), 2160(12 hrs)
# haslemere 1(5min), 3(15min), 6(30min), 12(1hr), 72(6hr), 144(12 hr)

sample_node = sample(n_nodes, n_sample,  replace=T)
sample_set = sample(max_step-(time_unit_temp-1), n_sample, replace=T)
sample = data.table(node = sample_node, set=sample_set)

while(any(duplicated(sample))){
  
  sample = sample[-which(duplicated(sample)),]
  sample_node = sample(n_nodes, n_sample-sample[,.N],  replace=T)
  sample_set = sample(max_step-(time_unit_temp-1), n_sample-sample[,.N], replace=T)
  sample = rbind(sample, data.table(node=sample_node, set=sample_set))
  
}

n_stat = copy(sample)
n_stat = n_stat[rep(seq_len(n_stat[,.N]), each=time_unit_stat)]
n_stat[, step:=rep(0:(time_unit_stat-1), times=.N/time_unit_stat)]
n_stat[, step:=step+set]
n_stat[n_contact_temp, n_unique:=i.N, on=c(node='node', set='set', step='step')]
n_stat = n_stat[,.(sum(n_unique)), by=.(node, set)]
setnames(n_stat, old=c('V1'), new=c('n_unique'))
n_stat[, n_total:=n_unique*time_unit_temp/time_unit_stat,]


n_temp = copy(sample)
n_temp = n_temp[rep(seq_len(n_temp[,.N]), each=time_unit_temp)]
n_temp[, step:=rep(0:(time_unit_temp-1), times=.N/time_unit_temp)]
n_temp[, step:=step+set]
n_temp[n_contact_temp, n_unique:=i.N, on=c(node='node', set='set', step='step')]
n_temp[n_contact_temp[type=='total'], n_total:=i.N, on=c(node='node', step='step')]
n_temp = n_temp[,.(sum(n_unique), sum(n_total)), by=.(node, set)]
setnames(n_temp, old=c('V1','V2'), new=c('n_unique', 'n_total'))

n_rand = copy(sample)
n_rand = n_rand[rep(seq_len(n_rand[,.N]), each=time_unit_temp)]
n_rand[, step:=rep(0:(time_unit_temp-1), times=.N/time_unit_temp)]
n_rand[, step:=step+set]
n_rand[, step:=step+1]

n_rand[n_contact_rand, n_unique:=i.N, on=c(node='node', set='set', step='step')]
n_rand[n_contact_rand[type=='total'], n_total:=i.N, on=c(node='node', step='step')]
n_rand = n_rand[,.(sum(n_unique), sum(n_total)), by=.(node, set)]
setnames(n_rand, old=c('V1','V2'), new=c('n_unique', 'n_total'))



A = n_unique_total[type=='stat']$n_unique # n_stat$n_unique
B = n_unique_total[type=='temp']$n_unique # n_temp$n_unique
C = n_unique_total[type=='rand']$n_unique # n_rand$n_unique

max(c(A,B,C))

ax = seq(0,500,1)
hgA <- hist(A, breaks = ax, plot = FALSE) 
hgB <- hist(B, breaks = ax, plot = FALSE)
hgC <- hist(C, breaks = ax, plot = FALSE)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
c3 <- rgb(144,238,144, max = 255, alpha = 80, names = "lt.green")

plot(hgA, col = c1, xlim=c(0,20), xlab='No. of contacts', main='Unique contacts') 
plot(hgB, col = c2, add = TRUE)
plot(hgC, col = c3, add = TRUE)


D = n_unique_total[type=='stat']$n_total # n_stat$n_total
E = n_unique_total[type=='temp']$n_total # n_temp$n_total
G = n_unique_total[type=='rand']$n_total # n_rand$n_total

max(c(D,E,G))

ax = seq(0,600,1)
hgD <- hist(D, breaks = ax, plot = FALSE) 
hgE <- hist(E, breaks = ax, plot = FALSE)
hgG <- hist(G, breaks = ax, plot = FALSE)

plot(hgD, col = c1, xlim=c(0,20), xlab='No. of contacts', main='Total contacts') 
plot(hgE, col = c2, add = TRUE)
plot(hgG, col = c3, add = TRUE)


# next step
# static time unit of 1, cumulative time unit of 
# 2 (30min), 4(1hr), 24(6 hr), 48(12 hours)

# static time unit of 1, 2, 4


# for networks with large number of time steps
n_nodes = unique(net$el$node_i)
max_step = net$max_step
n_sample = 1000
time_unit_stat = 45 # cruise 1(15min), 2(30min), 4(1hr), 24(6 hr), 48(12 hours)
time_unit_temp = 90 # non cruise non haslemere 1(20sec), 3(1min), 15(5min), 45(15 min), 90(30mins), 180(1 hours), 1080(6 hr), 2160(12 hrs)

sample_set = lapply(1:length(n_nodes), function(x){ sample(max_step-(time_unit_temp-1), n_sample, replace=F) })
sample_set = unlist(sample_set)
sample = data.table(node = rep(n_nodes, each=n_sample), set=sample_set)

# save sample

# temporal
el_temp = net$el
el_temp = el_temp[,c(1,2,3)]
setorder(el_temp, node_i,step)

# load el_rand
load('output/results/20230403/el_rand_param_12_work_2015.RData')

# compute number of unique and total interactions
# set up clusters
set.seed(123)
cl = makeCluster(detectCores())
clusterExport(cl, as.vector(lsf.str()))
registerDoParallel(cl)

n_unique_total = foreach(n = 1:length(n_nodes), .packages = c('data.table'), .combine = 'rbind') %dopar%  {
  # foreach(n = 1:length(n_nodes), .packages = c('data.table'), .combine = 'c') %dopar%  {
  
  n_sample = sample[node==n_nodes[n]]
  el_n = el_temp[node_i == n_nodes[n]]
  
  el_u = copy(n_sample)
  el_u = el_u[rep(seq_len(el_u[,.N]), each=time_unit_temp)]
  el_u[, step:=rep(0:(time_unit_temp-1), times=.N/time_unit_temp)]
  el_u[, step:=step+set]
  el_t = copy(el_u)
  
  el_n_u = copy(el_n)
  el_n_u = el_n_u[rep(seq_len(el_n_u[,.N]), times=n_sample[,.N])]
  el_n_u[, set:=rep(n_sample$set, each=.N/n_sample[,.N])]
  el_n_u = el_n_u[step>=set]
  el_n_u = unique(el_n_u, by=c('node_i', 'node_j', 'set'))
  el_n_u = el_n_u[,.N, by=.(step, set)]
  el_u[el_n_u, N:=i.N, on=c(step='step', set='set')]
  # el_u[, type:='unique']
  el_u[is.na(N), N:=0]
  
  el_n_t = copy(el_n)
  el_n_t = el_n_t[,.N, by=.(step)]
  el_t[el_n_t, N:=i.N, on=c(step='step')]
  # el_t[, type:='total']
  el_t[is.na(N), N:=0]
  
  n_temp = cbind(el_u, el_t$N)
  n_stat = copy(n_temp)
  
  n_temp = n_temp[,.(sum(N), sum(V2)), by=.(node,set)]
  setnames(n_temp, old=c('V1','V2'), new=c('n_unique', 'n_total'))
  
  n_stat = n_stat[, time_unit:=step-set+1]
  n_stat = n_stat[time_unit<=time_unit_stat]
  n_stat = n_stat[,.(sum(N), sum(V2)), by=.(node,set)]
  setnames(n_stat, old=c('V1','V2'), new=c('n_unique', 'n_total'))
  
  
  el_n = el_rand[node_i == n_nodes[n] & set == 1]

  el_u$N = NULL
  el_t$N = NULL

  el_n_u = copy(el_n)
  el_n_u = el_n_u[rep(seq_len(el_n_u[,.N]), times=n_sample[,.N])]
  el_n_u[, set:=rep(n_sample$set, each=.N/n_sample[,.N])]
  el_n_u = el_n_u[step>=set]
  el_n_u = unique(el_n_u, by=c('node_i', 'node_j', 'set'))
  el_n_u = el_n_u[,.N, by=.(step, set)]
  el_u[el_n_u, N:=i.N, on=c(step='step', set='set')]
  # el_u[, type:='unique']
  el_u[is.na(N), N:=0]

  el_n_t = copy(el_n)
  el_n_t = el_n_t[,.N, by=.(step)]
  el_t[el_n_t, N:=i.N, on=c(step='step')]
  # el_t[, type:='total']
  el_t[is.na(N), N:=0]

  n_rand = cbind(el_u, el_t$N)
  n_rand = n_rand[,.(sum(N), sum(V2)), by=.(node,set)]
  setnames(n_rand, old=c('V1','V2'), new=c('n_unique', 'n_total'))

  n_uni_tot = rbind(n_stat, n_temp, n_rand)
  n_uni_tot[, type:=rep(c('stat', 'temp', 'rand'), each=1000)]
  
}

# stop clusters
stopCluster(cl) 



# haslemere

load('output/results/20230403/n_contact_temp_param_05_haslemere.RData')
load('output/results/20230403/n_contact_rand_param_05_haslemere.RData')
load('output/results/20230403/net_param_05_haslemere.rdata')
# 1-96, day 1, 97-143, night
# 144-240, day 2, 241-287, night
# 288-384, day 3
n_nodes = unique(net$el$node_i)
length(n_nodes)

net$el[step>=1 & step<=96, day:=1]
net$el[step>=144 & step<=240, day:=2]
net$el[step>=288 & step<=384, day:=3]

load("data/nl_haslemere.RData")
nl=copy(nl_haslemere)
nl$node=as.numeric(nl$node)
net$el[nl, hh_i:=i.household_no, on=c(node_i='node')]
net$el[nl, hh_j:=i.household_no, on=c(node_j='node')]

# temporal
el_temp_all = copy(net$el)
el_temp = copy(el_temp_all)
el_temp = el_temp[is.na(hh_i) | is.na(hh_j) | hh_i!=hh_j]
el_temp = el_temp[,c(1,2,3,8)]
setorder(el_temp, node_i,step,day)

# load el_rand
load('output/results/20230403/el_rand_param_05_haslemere.RData')

el_rand$node_i = as.numeric(el_rand$node_i)
el_rand$node_j = as.numeric(el_rand$node_j)

el_rand[nl, hh_i:=i.household_no, on=c(node_i='node')]
el_rand[nl, hh_j:=i.household_no, on=c(node_j='node')]

el_rand$step=el_rand$step-1
el_rand[step>=1 & step<=96, day:=1]
el_rand[step>=144 & step<=240, day:=2]
el_rand[step>=288 & step<=384, day:=3]

el_rand = el_rand[step>=1]

el_rand_all = copy(el_rand)
el_rand = copy(el_rand_all)
el_rand = el_rand[is.na(hh_i) | is.na(hh_j) | hh_i!=hh_j]
el_rand = el_rand[,c(1,2,3,4,7)]
setorder(el_rand, node_i,step,day)


# compute number of unique and total interactions
# set up clusters
set.seed(123)
cl = makeCluster(detectCores())
clusterExport(cl, as.vector(lsf.str()))
registerDoParallel(cl)


n_unique = foreach(n = 1:length(n_nodes), .packages = c('data.table'), .combine = 'rbind') %dopar%  {
  # foreach(n = 1:length(n_nodes), .packages = c('data.table'), .combine = 'c') %dopar%  {
  
  # n_sample = sample[node==n_nodes[n]]
  el_n = el_temp[node_i == n_nodes[n]]
  el_n = unique(el_n[,c(1,2,4)])
  
  # high school 2012
  # el_u = data.table(node=n_nodes[n], 
  #                   day=c(1,2,3,4,5,8,9,
  #                         2,3,4,5,8,9,
  #                         3,4,5,8,9,
  #                         4,5,8,9,
  #                         5,8,9,
  #                         8,9,
  #                         9), 
  #                   set=c(1,1,1,1,1,1,1,
  #                         2,2,2,2,2,2,
  #                         3,3,3,3,3,
  #                         4,4,4,4,
  #                         5,5,5,
  #                         6,6,
  #                         7), 
  #                   match=1) 
  
  # haslemere
  el_u = data.table(node=n_nodes[n], 
                    day=c(1,2,3,
                          2,3,
                          3), 
                    set=c(1,1,1,
                          2,2,
                          3), 
                    match=1) 
  
  el_n_u = copy(el_n)
  el_n_u = el_n_u[rep(seq_len(el_n_u[,.N]), times=max(el_u$set))]  
  el_n_u[, set:=rep(unique(el_u$set), each=.N/max(el_u$set))] 
  el_n_u = el_n_u[el_u, match:=i.match, on=c(node_i='node', day='day', set='set')]
  
  el_n_u = el_n_u[match==1,]
  el_n_u[, match:=NULL]
  
  el_n_u = unique(el_n_u, by=c('node_i', 'node_j', 'set'))
  el_n_u = el_n_u[,.N, by=.(day, set)]
  el_u[el_n_u, N:=i.N, on=c(day='day', set='set')]
  el_u[is.na(N), N:=0]
  
  el_n_u_temp = copy(el_u)
  el_n_u_temp = el_n_u_temp[, match:=NULL]
  
  
  # random
  el_n = el_rand[node_i == n_nodes[n] & set == 1]
  
  el_u$N = NULL
  
  
  el_n_u = copy(el_n)
  el_n_u = el_n_u[rep(seq_len(el_n_u[,.N]), times=max(el_u$set))]
  el_n_u[, set:=rep(unique(el_u$set), each=.N/max(el_u$set))]  
  el_n_u = el_n_u[el_u, match:=i.match, on=c(node_i='node', day='day', set='set')]
  
  el_n_u = el_n_u[match==1,]
  el_n_u[, match:=NULL]
  
  el_n_u = unique(el_n_u, by=c('node_i', 'node_j', 'set'))
  el_n_u = el_n_u[,.N, by=.(day, set)]
  el_u[el_n_u, N:=i.N, on=c(day='day', set='set')]
  el_u[is.na(N), N:=0]
  
  el_n_u_rand = copy(el_u)
  el_n_u_rand = el_n_u_rand[, match:=NULL]
  
  n_uni = rbind(el_n_u_temp, el_n_u_rand)
  n_uni[, type:=rep(c('temp', 'rand'), each=.N/2)]  
  
}

# stop clusters
stopCluster(cl) 

View(n_unique)



# for each of the network, have to convert the steps to days
# high school 2011 - 4days, high school 2012 - 5day, high school 2013 - 5days
# hospital - Monday, December 6, 2010 at 1:00 pm to Friday, December 10, 2010 at 2:00 pm
# work 2013 June 24 to July 3, 2013, work 2015, 0:00 on June 24, 2013

# high school 2012 day 1:0,320, day2:716,1067, day3:1437,1767, day4 2155,2538, day 5 2873,3198
# day8 5034, 5357, day9 5758,6080

# high school 2013 day 1:0,180, day 2:720,1044, day3: 1584,1908, day4: 2448,2772, day5:3312,3636

load('output/results/20230403/net_param_07_high_school_2012.rdata')
n_nodes = unique(net$el$node_i)
length(n_nodes)

# param 7 high school 2012
net$el[step>=0 & step<=320, day:=1]
net$el[step>=716 & step<=1067, day:=2]
net$el[step>=1437 & step<=1767, day:=3]
net$el[step>=2155 & step<=2538, day:=4]
net$el[step>=2873 & step<=3198, day:=5]
net$el[step>=5034 & step<=5357, day:=8]
net$el[step>=5758 & step<=6080, day:=9]

net$el[is.na(day)]

# param 8 high school 2013
net$el[step>=0 & step<=180, day:=1]
net$el[step>=720 & step<=1044, day:=2]
net$el[step>=1584 & step<=1908, day:=3]
net$el[step>=2448 & step<=2772, day:=4]
net$el[step>=3312 & step<=3636, day:=5]


net$el[is.na(day)]



load("data/nl_high_school_2012.RData")
nl=copy(nl_high_school_2012)
net$el[nl, hh_i:=i.class, on=c(node_i='node')]
net$el[nl, hh_j:=i.class, on=c(node_j='node')]

# temporal
el_temp_all = copy(net$el)
el_temp = copy(el_temp_all)
el_temp = el_temp[is.na(hh_i) | is.na(hh_j) | hh_i!=hh_j]
el_temp = el_temp[,c(1,2,3,8)]
setorder(el_temp, node_i,step,day)

# load el_rand
load('output/results/20230403/el_rand_param_07_high_school_2012.RData')

el_rand$node_i=as.numeric(el_rand$node_i)
el_rand$node_j=as.numeric(el_rand$node_j)

el_rand[nl, hh_i:=i.class, on=c(node_i='node')]
el_rand[nl, hh_j:=i.class, on=c(node_j='node')]

el_rand$step=el_rand$step-1
el_rand[step>=0 & step<=320, day:=1]
el_rand[step>=716 & step<=1067, day:=2]
el_rand[step>=1437 & step<=1767, day:=3]
el_rand[step>=2155 & step<=2538, day:=4]
el_rand[step>=2873 & step<=3198, day:=5]
el_rand[step>=5034 & step<=5357, day:=8]
el_rand[step>=5758 & step<=6080, day:=9]

el_rand[step>=0 & step<=320, day:=1]
el_rand[step>=0 & step<=180, day:=1]
el_rand[step>=720 & step<=1044, day:=2]
el_rand[step>=1584 & step<=1908, day:=3]
el_rand[step>=2448 & step<=2772, day:=4]
el_rand[step>=3312 & step<=3636, day:=5]

el_rand = el_rand[step>=1]

el_rand_all = copy(el_rand)
el_rand = copy(el_rand_all)
el_rand = el_rand[is.na(hh_i) | is.na(hh_j) | hh_i!=hh_j]
el_rand = el_rand[,c(1,2,3,4,7)]
setorder(el_rand, node_i,step,day)

