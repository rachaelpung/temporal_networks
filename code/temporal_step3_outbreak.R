source("code/temporal_library.R")
source("code/temporal_functions.R")

# load temporal and random edge list and node characteristics
load('output/results/20230403/net_param_01_cruise_1.rdata')
load('output/results/20230403/el_rand_param_01_cruise_1.RData')
load("data/nl_cruise_1.RData")

n_nodes = unique(net$el$node_i); length(n_nodes)
nl=copy(nl_cruise_1)

el_temp_all = copy(net$el)
el_temp = copy(el_temp_all)

el_temp[nl, type_i:=i.department, on=c(node_i='node')] # cohort, household_no, class, department
el_temp[nl, type_j:=i.department, on=c(node_j='node')]
el_temp = el_temp[is.na(type_i) | is.na(type_j) | type_i!=type_j]
el_temp = el_temp[,c('node_i', 'node_j', 'step', 'day_start')]
setorder(el_temp, node_i, step, day_start)

el_rand_all = copy(el_rand)
el_rand = copy(el_rand_all[set==1])
el_rand[, set:=NULL]

el_temp_uni_step=sort(unique(net$el$step))
el_rand_uni_step=sort(unique(el_rand$step))
length(setdiff(el_temp_uni_step,el_rand_uni_step))
length(setdiff(el_rand_uni_step,el_temp_uni_step))
step_diff = setdiff(el_temp_uni_step,el_rand_uni_step)
  
if(length(step_diff)!=0) el_rand = rbind(el_rand, el_temp[step %in% step_diff, c('node_i', 'node_j', 'step')])
  
el_rand[,.N] == el_temp[,.N]

el_rand[nl, type_i:=i.department, on=c(node_i='node')] # household_no, class
el_rand[nl, type_j:=i.department, on=c(node_j='node')]
el_rand = el_rand[is.na(type_i) | is.na(type_j) | type_i!=type_j]
setorder(el_rand, node_i, step)


el_rand[el_temp, day_start:=i.day_start, on=c(node_i='node_i', step='step')]

el_rand = el_rand[,c('node_i', 'node_j', 'step', 'day_start')]
setorder(el_rand, node_i, step, day_start)

uni_day = sort(unique(el_temp$day))

# number of unique contacts by respective days of observation
# set up clusters
set.seed(123)
cl = makeCluster(detectCores())
clusterExport(cl, as.vector(lsf.str()))
registerDoParallel(cl)

n_unique = foreach(n = 1:length(n_nodes), .packages = c('data.table'), .combine = 'rbind') %dopar%  {
  
  el_n = el_temp[node_i == n_nodes[n]]
  el_n = unique(el_n[,c(1,2,4)])
  
  # total
  # el_t = data.table(node=n_nodes[n], day_start=uni_day)
  # 
  # el_n_t = copy(el_n)
  # el_n_t = el_n_t[,.N, by=.(day_start)]
  # 
  # el_t[el_n_t, N:=i.N, on=c(day_start='day_start')]
  # el_t[is.na(N), N:=0]
  
  # unique
  el_u = data.table(node=n_nodes[n], day_start=rep(uni_day, times=length(uni_day)),
                    set=rep(uni_day, each=length(uni_day)), match=1)
  el_u = el_u[day_start>=set]
  
  el_n_u = copy(el_n)
  el_n_u = el_n_u[rep(seq_len(el_n_u[,.N]), times=length(uni_day))]  
  el_n_u[, set:=rep(unique(el_u$set), each=.N/length(uni_day))] 
  el_n_u = el_n_u[el_u, match:=i.match, on=c(node_i='node', day_start='day_start', set='set')]
  
  el_n_u = el_n_u[match==1,]
  el_n_u[, match:=NULL]
  
  el_n_u = unique(el_n_u, by=c('node_i', 'node_j', 'set'))
  el_n_u = el_n_u[,.N, by=.(day_start, set)]
  el_u[el_n_u, N:=i.N, on=c(day_start='day_start', set='set')]
  el_u[is.na(N), N:=0]
  
  el_n_u_temp = copy(el_u)
  el_n_u_temp = el_n_u_temp[, match:=NULL]
  # el_n_t_temp = copy(el_t)
  
  # random
  el_n = el_rand[node_i == n_nodes[n]]
  el_u$N = NULL
  # el_t$N = NULL
  
  # total
  # el_t = data.table(node=n_nodes[n], day_start=uni_day)
  # 
  # el_n_t = unique(el_n, by=c('node_i', 'node_j', 'day_start'))
  # el_n_t = el_n_t[,.N, by=.(day_start)]
  # 
  # el_t[el_n_t, N:=i.N, on=c(day_start='day_start')]
  # el_t[is.na(N), N:=0]
  
  # unique
  el_n_u = copy(el_n)
  el_n_u = el_n_u[rep(seq_len(el_n_u[,.N]), times=length(uni_day))]
  el_n_u[, set:=rep(unique(el_u$set), each=.N/length(uni_day))]  
  el_n_u = el_n_u[el_u, match:=i.match, on=c(node_i='node', day_start='day_start', set='set')]
  
  el_n_u = el_n_u[match==1,]
  el_n_u[, match:=NULL]
  
  el_n_u = unique(el_n_u, by=c('node_i', 'node_j', 'set'))
  el_n_u = el_n_u[,.N, by=.(day_start, set)]
  el_u[el_n_u, N:=i.N, on=c(day_start='day_start', set='set')]
  el_u[is.na(N), N:=0]
  
  el_n_u_rand = copy(el_u)
  el_n_u_rand = el_n_u_rand[, match:=NULL]
  # el_n_t_rand = copy(el_t)
  
  n_uni = rbind(el_n_u_temp, el_n_u_rand)
  n_uni[, type:=rep(c('temp', 'rand'), each=.N/2)]
  
  # n_uni_tot = rbindlist(list(el_n_u_temp, el_n_t_temp, el_n_u_rand, el_n_t_rand), fill=T)
  # n_uni_tot[, type:=rep(c('temp', 'rand'), each=.N/2)]  
  
}

# stop clusters
stopCluster(cl) 

save(n_unique,file="output/results/20230403/n_unique_param_09_hospital.RData")

n_unique = copy(n_unique[set==1])


