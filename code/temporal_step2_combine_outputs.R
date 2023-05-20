source("code/temporal_library.R")
source("code/temporal_functions.R")

# load outputs
list_folder = dir('output/20230403/', pattern = 'high_school_2012_step')
list_folder

output = lapply(1:length(list_folder), function(x){
  get(load(paste('output/20230403/', list_folder[x], sep = '')))
})

list_folder = dir('output/results/20230403/', pattern = 'net_param_05')
list_folder
load(paste('output/results/20230403/', list_folder, sep = ''))

# check deg distribution is approx similar in each time step
check_deg_dist(output)

# combine outputs
# tabulate the k0_r for stat, temp, rand, rand_avg
p_k0_stat = tab(output, col_name = 'p_k0_stat')
p_k0_temp = tab(output, col_name = 'p_k0_temp')
p_k0_rand = tab(output, col_name = 'p_k0_rand')

p_k0_k1_stat = tab(output, col_name = 'p_k0_k1_stat')
p_k0_k1_temp = tab(output, col_name = 'p_k0_k1_temp')
p_k0_k1_rand = tab(output, col_name = 'p_k0_k1_rand')

p_k0_r_stat = tab(output, col_name='p_k0_r_stat')
p_k0_r_temp = tab(output, col_name='p_k0_r_temp')
p_k0_r_rand = tab(output, col_name='p_k0_r_rand')
p_k0_r_rand_avg = tab(output, col_name='p_k0_k1_r_rand_avg')
p_k0_r = diff_p_k0_r(p_k0_r_stat, p_k0_r_temp, p_k0_r_rand, p_k0_r_rand_avg)

p_r_stat = tab(output, col_name='p_r_stat')
p_r_temp = tab(output, col_name='p_r_temp')
p_r_rand = tab(output, col_name='p_r_rand')
p_r_rand_avg = tab(output, col_name='p_r_rand_avg')
p_r = diff_p_r(p_r_stat, p_r_temp, p_r_rand, p_r_rand_avg)
avg_r = mean_r(p_r_stat, p_r_temp, p_r_rand, p_r_rand_avg)

node_p80_ct_esp = tab(output, col_name='node_p80_ct_esp')
node_r80_ct_esp = node_rank(node_p80_ct_esp, net$kl)
node_s80_ct_esp = node_sustain(node_p80_ct_esp)

node_p80_ct_dur = tab(output, col_name='node_p80_ct_dur')
node_r80_ct_dur = node_rank(node_p80_ct_dur, net$kl)
node_s80_ct_dur = node_sustain(node_p80_ct_dur)

r_scale = scale_network(p_r_stat, p_r_temp, p_r_rand, p_r_rand_avg, p_k0_r_temp)

results = list(p_k0_stat=p_k0_stat, p_k0_temp=p_k0_temp, p_k0_rand=p_k0_rand,
               p_k0_k1_stat=p_k0_k1_stat, p_k0_k1_temp=p_k0_k1_temp, p_k0_k1_rand=p_k0_k1_rand,
               p_k0_r_stat=p_k0_r_stat, p_k0_r_temp=p_k0_r_temp, p_k0_r_rand=p_k0_r_rand,
               p_k0_r_rand_avg=p_k0_r_rand_avg, p_k0_r=p_k0_r,
               p_r_stat=p_r_stat, p_r_temp=p_r_temp, p_r_rand=p_r_rand, 
               p_r_rand_avg=p_r_rand_avg, p_r=p_r, avg_r=avg_r,
               node_p80_ct_esp=node_p80_ct_esp, node_r80_ct_esp=node_r80_ct_esp, node_s80_ct_esp=node_s80_ct_esp,
               node_p80_ct_dur=node_p80_ct_dur, node_r80_ct_dur=node_r80_ct_dur, node_s80_ct_dur=node_s80_ct_dur,
               r_scale=r_scale)

save(results, file='output/results/20230403/results_param_07_high_school_2012.RData')

# combine networks
n_nodes = unique(net$el$node_i)
max_step = net$max_step

# temporal
el_temp = copy(net$el)
el_temp = el_temp[,c(1,2,3)] #el_temp[,c(1,2,4)]
setorder(el_temp, node_i,step)

load("data/nl_haslemere.RData")
nl = copy(nl_haslemere) # nl_cruise_1
el_temp[nl, cabin_i:=i.cabin_no, on=c(node_i="node")]
el_temp[nl, cabin_j:=i.cabin_no, on=c(node_j="node")]

el_temp[nl, cohort_i:=i.cohort, on=c(node_i="node")]
el_temp[nl, cohort_j:=i.cohort, on=c(node_j="node")]

nl$node = as.numeric(nl$node)
el_temp[nl, hh_i:=i.household_no, on=c(node_i="node")]
el_temp[nl, hh_j:=i.household_no, on=c(node_j="node")]

View(nl)


el_temp = el_temp[cabin_i==cabin_j]
el_temp = el_temp[cabin_i!=cabin_j]

el_temp = el_temp[cohort_i==cohort_j & cohort_i!='PASSENGER' & cohort_j!='PASSENGER']
el_temp = el_temp[cohort_i!=cohort_j & cohort_i!='PASSENGER' & cohort_j!='PASSENGER']

el_temp = el_temp[hh_i==hh_j]
el_temp = el_temp[hh_i!=hh_j]


el_temp[, `:=` (cabin_i=NULL, cabin_j=NULL)]
el_temp[, `:=` (cohort_i=NULL, cohort_j=NULL)]
el_temp[, `:=` (hh_i=NULL, hh_j=NULL)]

# compute number of unique and total interactions
# set up clusters
set.seed(123)
cl = makeCluster(detectCores())
clusterExport(cl, as.vector(lsf.str()))
registerDoParallel(cl)

n_contact_temp = foreach(n = 1:length(n_nodes), .packages = c('data.table'), .combine = 'rbind') %dopar%  {
# foreach(n = 1:length(n_nodes), .packages = c('data.table'), .combine = 'c') %dopar%  {
  
  el_n = el_temp[node_i == n_nodes[n]]
  el_u = data.table(step=rep(1:max_step, times=max_step), set=rep(1:max_step, each=max_step))
  el_t = data.table(step=1:max_step)
  el_u = el_u[step>=set]
  
  el_n_u = copy(el_n)
  el_n_u = el_n_u[rep(seq_len(el_n_u[,.N]), times=max_step)]
  el_n_u[, set:=rep(1:max_step, each=.N/max_step)]
  el_n_u = el_n_u[step>=set]
  el_n_u = unique(el_n_u, by=c('node_i', 'node_j', 'set'))
  el_n_u = el_n_u[,.N, by=.(step, set)]
  el_u[el_n_u, N:=i.N, on=c(step='step', set='set')]
  el_u[, type:='unique']
  el_u[is.na(N), N:=0]
  
  el_n_t = copy(el_n)
  el_n_t = el_n_t[,.N, by=.(step)]
  el_t[el_n_t, N:=i.N, on=c(step='step')]
  el_t[, type:='total']
  el_t[is.na(N), N:=0]
  
  el_n = rbindlist(list(el_u, el_t), fill=T, use.names = T)
  el_n[, node:=n_nodes[n]]
  
  # if(n<10){
  #   save(el_n,file= paste0("output/results/20230426/n_contact_temp_param_12_work_2015", '_node_000', n, ".rdata"))
  # } else if(n<100){
  #   save(el_n,file= paste0("output/results/20230426/n_contact_temp_param_12_work_2015", '_node_00', n, ".rdata"))
  # } else if(n<1000){
  #   save(el_n,file= paste0("output/results/20230426/n_contact_temp_param_12_work_2015", '_node_0', n, ".rdata"))
  # } else{
  #   save(el_n,file= paste0("output/results/20230426/n_contact_temp_param_12_work_2015", '_node_', n, ".rdata"))
  # }
  

}

# stop clusters
stopCluster(cl) 

# save
save(n_contact_temp, file='output/results/20230403/n_contact_temp_param_05_haslemere_nhh.RData')
# phh: passenger hh, pnhh: passenger nhh, chh:crew same cohort, cnhh:crew diff cohort

# random
# no_step = setdiff(c(1:max_step), unique(net$el$step))

el_rand = lapply(1:length(output), function(x){
  
  el_rand_step = rbindlist(output[[x]]$el_rand)
  el_rand_step[, set:=rep(1:100, each=.N/100)]
  min_step = min(el_rand_step$step)
  
  if(min_step==1) el_rand_step = el_rand_step[, c('node_i', 'node_j', 'step', 'set')]
  if(min_step!=1) el_rand_step = el_rand_step[step==min_step+1, c('node_i', 'node_j', 'step', 'set')]
  
  return(el_rand_step)
})

el_rand = rbindlist(el_rand)


save(el_rand, file='output/results/20230403/el_rand_param_01_cruise_1.RData')


  
  
load("output/results/20230403/el_rand_param_05_haslemere.RData")
el_rand[nl, cabin_i:=i.cabin_no, on=c(node_i="node")]
el_rand[nl, cabin_j:=i.cabin_no, on=c(node_j="node")]

el_rand[nl, cohort_i:=i.cohort, on=c(node_i="node")]
el_rand[nl, cohort_j:=i.cohort, on=c(node_j="node")]


el_rand$node_i = as.numeric(el_rand$node_i)
el_rand$node_j = as.numeric(el_rand$node_j)
el_rand[nl, hh_i:=i.household_no, on=c(node_i="node")]
el_rand[nl, hh_j:=i.household_no, on=c(node_j="node")]

el_rand = el_rand[cabin_i==cabin_j]
el_rand = el_rand[cabin_i!=cabin_j]

el_rand = el_rand[cohort_i==cohort_j & cohort_i!='PASSENGER' & cohort_j!='PASSENGER']
el_rand = el_rand[cohort_i!=cohort_j & cohort_i!='PASSENGER' & cohort_j!='PASSENGER']

el_rand = el_rand[hh_i==hh_j]
el_rand = el_rand[hh_i!=hh_j]

el_rand[, `:=` (cabin_i=NULL, cabin_j=NULL)]
el_rand[, `:=` (cohort_i=NULL, cohort_j=NULL)]
el_rand[, `:=` (hh_i=NULL, hh_j=NULL)]


# compute number of unique and total interactions
# set up clusters
set.seed(123)
cl = makeCluster(detectCores())
clusterExport(cl, as.vector(lsf.str()))
registerDoParallel(cl)

n_contact_rand = foreach(n = 1:length(n_nodes), .packages = c('data.table'), .combine = 'rbind') %dopar%  {
# foreach(n = 1:length(n_nodes), .packages = c('data.table'), .combine = 'c') %dopar%  {
    
  el_n = el_rand[node_i == n_nodes[n] & set == 1]
  el_u = data.table(step=rep(1:max_step, times=max_step), set=rep(1:max_step, each=max_step))
  el_t = data.table(step=1:max_step)
  el_u = el_u[step>=set]
  
  el_n_u = copy(el_n)
  el_n_u = el_n_u[rep(seq_len(el_n_u[,.N]), times=max_step)]
  el_n_u[, set:=rep(1:max_step, each=.N/max_step)]
  el_n_u = el_n_u[step>=set]
  el_n_u = unique(el_n_u, by=c('node_i', 'node_j', 'set'))
  el_n_u = el_n_u[,.N, by=.(step, set)]
  el_u[el_n_u, N:=i.N, on=c(step='step', set='set')]
  el_u[, type:='unique']
  el_u[is.na(N), N:=0]
  
  el_n_t = copy(el_n)
  el_n_t = el_n_t[,.N, by=.(step)]
  el_t[el_n_t, N:=i.N, on=c(step='step')]
  el_t[, type:='total']
  el_t[is.na(N), N:=0]
  
  el_n = rbindlist(list(el_u, el_t), fill=T, use.names = T)
  el_n[, node:=n_nodes[n]]
  
  # if(n<10){
  #   save(el_n,file= paste0("output/results/20230426/n_contact_rand_param_12_work_2015", '_node_000', n, ".rdata"))
  # } else if(n<100){
  #   save(el_n,file= paste0("output/results/20230426/n_contact_rand_param_12_work_2015", '_node_00', n, ".rdata"))
  # } else if(n<1000){
  #   save(el_n,file= paste0("output/results/20230426/n_contact_rand_param_12_work_2015", '_node_0', n, ".rdata"))
  # } else{
  #   save(el_n,file= paste0("output/results/20230426/n_contact_rand_param_12_work_2015", '_node_', n, ".rdata"))
  # }
  
}

 
# stop clusters
stopCluster(cl) 

# save
save(n_contact_rand, file='output/results/20230403/n_contact_rand_param_05_haslemere_nhh.RData')


# load compiled results
list_folder = dir('output/results/20230403/', pattern = 'results')
list_folder

results = lapply(1:length(list_folder), function(x){
  get(load(paste('output/results/20230403/', list_folder[x], sep = '')))
})

list_folder = dir('output/results/20230403/', pattern = 'net')
list_folder

net = lapply(1:length(list_folder), function(x){
  get(load(paste('output/results/20230403/', list_folder[x], sep = '')))
})
