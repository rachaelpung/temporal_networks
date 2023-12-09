source("code/temporal_library.R")
source("code/temporal_functions.R")

# load temporal and random edge list and node characteristics
load('output/results/20230403/net_param_12_work_2015.rdata')
load('output/results/20230403/el_rand_param_12_work_2015.RData')
load("data/nl_work_2015.RData")

n_nodes = unique(net$el$node_i); length(n_nodes)
nl=copy(nl_work_2015); n=11

el_temp_all = copy(net$el)
el_temp = copy(el_temp_all)

# el_temp[nl, type_i:=i.department, on=c(node_i='node')] # cohort, household_no, class, department
# el_temp[nl, type_j:=i.department, on=c(node_j='node')]
# el_temp = el_temp[is.na(type_i) | is.na(type_j) | type_i!=type_j]
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

# el_rand[nl, type_i:=i.department, on=c(node_i='node')] # household_no, class
# el_rand[nl, type_j:=i.department, on=c(node_j='node')]
# el_rand = el_rand[is.na(type_i) | is.na(type_j) | type_i!=type_j]
setorder(el_rand, node_i, step)


el_rand[el_temp, day_start:=i.day_start, on=c(node_i='node_i', step='step')]

el_rand = el_rand[,c('node_i', 'node_j', 'step', 'day_start')]
setorder(el_rand, node_i, step, day_start)

uni_day = sort(unique(el_temp$day))
step_length = net$steps_length$length


# number of unique contacts by respective days of observation
# set up clusters
set.seed(123)
cl = makeCluster(detectCores())
clusterExport(cl, as.vector(lsf.str()))
registerDoParallel(cl)

n_unique_total = foreach(n = 1:length(n_nodes), .packages = c('data.table'), .combine = 'rbind') %dopar%  {
  
  el_n = el_temp[node_i == n_nodes[n]]
  el_n = unique(el_n[,c(1,2,4)])
  setorder(el_n, day_start, node_j)
  
  # unique
  el_u = copy(el_n)
  el_u = unique(el_u, by=c('node_i', 'node_j'))
  el_u = el_u[,.N, by=.(day_start)]
  if(length(el_u$day_start) != length(uni_day)){
    el_u = rbind(el_u, data.table(day_start=setdiff(uni_day,el_u$day_start), N=0))
  }
  setorder(el_u, day_start)
  el_u_temp = copy(el_u)
  
  # total
  el_t = copy(el_n)
  el_t = unique(el_t, by=c('node_i', 'node_j', 'day_start'))
  el_t = el_t[,.N, by=.(day_start)]
  if(length(el_t$day_start) != length(uni_day)){
    el_t = rbind(el_t, data.table(day_start=setdiff(uni_day,el_t$day_start), N=0))
  }
  setorder(el_t, day_start)
  el_t_temp = copy(el_t)
  
  # random
  el_n = el_rand[node_i == n_nodes[n]]
  
  # unique
  el_u = copy(el_n)
  el_u = unique(el_u, by=c('node_i', 'node_j'))
  el_u = el_u[,.N, by=.(day_start)]
  if(length(el_u$day_start) != length(uni_day)){
    el_u = rbind(el_u, data.table(day_start=setdiff(uni_day,el_u$day_start), N=0))
  }
  setorder(el_u, day_start)
  el_u_rand = copy(el_u)
  
  # total
  el_t = copy(el_n)
  el_t = unique(el_t, by=c('node_i', 'node_j', 'day_start'))
  el_t = el_t[,.N, by=.(day_start)]
  if(length(el_t$day_start) != length(uni_day)){
    el_t = rbind(el_t, data.table(day_start=setdiff(uni_day,el_t$day_start), N=0))
  }
  setorder(el_t, day_start)
  el_t_rand = copy(el_t)
  
  n_uni_tot = rbindlist(list(el_u_temp, el_t_temp, el_u_rand, el_t_rand))
  n_uni_tot[, type:=rep(c('temp_uni', 'temp_tot', 'rand_uni', 'rand_tot'), each=.N/4)]
  n_uni_tot[, node:=n_nodes[n]]
  n_uni_tot[, steps:=rep(step_length, times=.N/length(uni_day))]
  n_uni_tot[, cum_N:=cumsum(N), by=.(type)]
  n_uni_tot[, cum_steps:=cumsum(steps), by=.(type)]
  
  n_uni_tot[, prop_N:=(cum_N/(length(n_nodes)-1))]
  n_uni_tot[, prop_steps:=(cum_steps/sum(step_length))]
  
  
  
}

# stop clusters
stopCluster(cl) 

save(n_unique_total,file="output/results/20230703/n_unique_total_param_12_work_2015.RData")

# compute difference
contact_diff = lapply(1:length(n_nodes), function(x){
  
  temp_uni = n_unique_total[node == n_nodes[x] & type=='temp_uni']
  temp_tot = n_unique_total[node == n_nodes[x] & type=='temp_tot']
  
  temp_diff = data.table(node =  n_nodes[x],
                         day = temp_uni$day_start,
                         prop_steps = temp_uni$prop_steps,
                         rel_diff = (temp_tot$cum_N-temp_uni$cum_N) / temp_uni$cum_N)
  
})
contact_diff = rbindlist(contact_diff)
contact_diff[is.na(rel_diff), rel_diff:=0]
contact_diff = contact_diff[, .(median(rel_diff), quantile(rel_diff, 0.25), quantile(rel_diff, 0.75)), by=.(day, prop_steps)]
contact_diff[, net:=n]
contact_diff[, day:=1:.N]
setnames(contact_diff, c('day', 'prop_steps', 'med_diff', 'lwr_diff', 'upp_diff', 'net'))

# frequency of contact pairs by day
el_temp_day = copy(net$el)
setorder(el_temp_day, node_i, node_j, step, day_start)
el_temp_day = unique(el_temp_day[, c('node_i', 'node_j', 'day_start')])
el_temp_day[, lag:=lead(day_start)-day_start, by=.(node_i, node_j)]
contact_pair = el_temp_day[, .N, by=.(node_i, node_j)]
setorder(contact_pair, N)

n_contact_pair = contact_pair[, .N, by=.(N)]
setnames(n_contact_pair, c('day', 'N'))
setorder(n_contact_pair, day)
n_contact_pair[, net:=n]
n_contact_pair[, P:=N/sum(N)]
n_contact_pair[, cum_P:=cumsum(P)]



# save
load('output/results/20230703/results_param_12_work_2015.RData')
results$contact_diff = contact_diff
results$n_contact_pair = n_contact_pair
save(results, file='output/results/20230703/results_param_12_work_2015.RData')


