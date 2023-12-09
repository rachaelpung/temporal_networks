source("code/temporal_library.R")
source("code/temporal_functions.R")

# load outputs
list_folder = dir('output/20230803/', pattern = 'work_2015_step')
list_folder

output = lapply(1:length(list_folder), function(x){
  get(load(paste('output/20230803/', list_folder[x], sep = '')))
})

list_folder = dir('output/results/20230803/', pattern = 'net_param_12')
list_folder
load(paste('output/results/20230803/', list_folder, sep = ''))

load("data/nl_work_2015.RData")
nl=copy(nl_work_2015)

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

# source("code/temporal_functions.R")
# load('output/results/20230703/net_param_12_work_2015.RData')
# load('output/results/20230703/results_param_12_work_2015.RData')
# load("data/nl_work_2015.RData")
# nl=copy(nl_work_2015)

# n=1; net=copy(net$el); r_scale=copy(results$r_scale)

# r_scale=copy(results$r_scale)
r_contact_type = retain_prop_contact_type(n=11, net$el, nl, r_scale)

# results$r_contact_type = r_contact_type

# save(results, file='output/results/20230703/results_param_12_work_2015.RData')



node_p80_day_ct_esp = p80_contact_day(net$steps_length, net$kl, net$n_nodes, type='contacts')
node_r80_day_ct_esp = node_rank_day(node_p80_day_ct_esp)

node_p80_day_ct_dur = p80_contact_day(net$steps_length, net$kl, net$n_nodes, type='duration')
node_r80_day_ct_dur = node_rank_day(node_p80_day_ct_esp)

# results$node_p80_day_ct_esp = node_p80_day_ct_esp
# results$node_r80_day_ct_esp = node_r80_day_ct_esp
# 
# results$node_p80_day_ct_dur = node_p80_day_ct_dur
# results$node_r80_day_ct_dur = node_r80_day_ct_dur
# 
# save(results, file='output/results/20230403/results_param_12_work_2015.RData')


# tidy up
contact_pair_delay = lapply(1:11, function(x){
  
  el_temp_day = copy(net[[x]]$el)
  setorder(el_temp_day, node_i, node_j, step, day_start)
  el_temp_day = unique(el_temp_day[, c('node_i', 'node_j', 'day_start')])
  el_temp_day[, lag:=lead(day_start)-day_start, by=.(node_i, node_j)]
  contact_pair = el_temp_day[, .N, by=.(node_i, node_j)]
  setorder(contact_pair, N)
  n_contact_pair = contact_pair[, .N, by=.(N)]
  setnames(n_contact_pair, c('day', 'N'))
  setorder(n_contact_pair, day)
  n_contact_pair[, net:=x]
  
  # delay = el_temp_day[!is.na(lag), ]
  # delay[, day_start:=NULL]
  # delay[, lag:=lag-1]
  # delay[contact_pair, N:=i.N, on=c(node_i='node_i', node_j='node_j')]
  # delay = delay[,.N, by=.(N, lag)]
  
  # setnames(delay, c('day', 'lag', 'N'))
  # setorder(delay, day)
  # delay[, net:=x]
  
  return(list(n_contact_pair=n_contact_pair))# , delay=delay))
  
})

contact_pair = lapply(contact_pair_delay, `[[`, 1) 
contact_pair = rbindlist(contact_pair)
contact_pair[, P:=N/sum(N), by=.(net)]
contact_pair[, cum_P:=cumsum(P), by=.(net)]


delay = lapply(contact_pair_delay, `[[`, 2) 
delay = rbindlist(delay)
delay[, P:=N/sum(N), by=.(net,day)]


data_diff = data.table()

for(n in 1:11){
  
  n_nodes = unique(n_unique_total[[n]]$node)
  
  diff = lapply(1:length(n_nodes), function(x){
    
    temp_uni = n_unique_total[[n]][node == n_nodes[x] & type=='temp_uni']
    temp_tot = n_unique_total[[n]][node == n_nodes[x] & type=='temp_tot']
    
    temp_diff = data.table(node =  n_nodes[x],
                           day = temp_uni$day_start,
                           prop_steps = temp_uni$prop_steps,
                           rel_diff = (temp_tot$cum_N-temp_uni$cum_N) / temp_uni$cum_N)
    
  })
  diff = rbindlist(diff)
  diff[is.na(rel_diff), rel_diff:=0]
  diff = diff[, .(median(rel_diff), quantile(rel_diff, 0.25), quantile(rel_diff, 0.75)), by=.(day, prop_steps)]
  diff[, net:=n]
  diff[, day:=1:.N]
  setnames(diff, c('day', 'prop_steps', 'med_diff', 'lwr_diff', 'upp_diff', 'net'))
  
  
  data_diff = rbind(data_diff, diff)
  
  print(n)
}

results = list(p_k0_stat=p_k0_stat, p_k0_temp=p_k0_temp, p_k0_rand=p_k0_rand,
               p_k0_k1_stat=p_k0_k1_stat, p_k0_k1_temp=p_k0_k1_temp, p_k0_k1_rand=p_k0_k1_rand,
               p_k0_r_stat=p_k0_r_stat, p_k0_r_temp=p_k0_r_temp, p_k0_r_rand=p_k0_r_rand,
               p_k0_r_rand_avg=p_k0_r_rand_avg, p_k0_r=p_k0_r,
               p_r_stat=p_r_stat, p_r_temp=p_r_temp, p_r_rand=p_r_rand, 
               p_r_rand_avg=p_r_rand_avg, p_r=p_r, avg_r=avg_r,
               node_p80_ct_esp=node_p80_ct_esp, node_r80_ct_esp=node_r80_ct_esp, node_s80_ct_esp=node_s80_ct_esp,
               node_p80_ct_dur=node_p80_ct_dur, node_r80_ct_dur=node_r80_ct_dur, node_s80_ct_dur=node_s80_ct_dur,
               r_scale=r_scale, r_contact_type=r_contact_type,
               node_p80_day_ct_esp=node_p80_day_ct_esp, node_r80_day_ct_esp=node_r80_day_ct_esp,
               node_p80_day_ct_dur=node_p80_day_ct_dur, node_r80_day_ct_dur=node_r80_day_ct_dur)

save(results, file='output/results/20230803/results_param_12_work_2015.RData')


# random
el_rand = lapply(1:length(output), function(x){
  
  el_rand_step = rbindlist(output[[x]]$el_rand)
  el_rand_step[, set:=rep(1:100, each=.N/100)]
  min_step = min(el_rand_step$step)
  
  # if(min_step==1) el_rand_step = el_rand_step[, c('node_i', 'node_j', 'step', 'set')]
  # if(min_step!=1) el_rand_step = el_rand_step[step==min_step+1, c('node_i', 'node_j', 'step', 'set')]
  
  el_rand_step = el_rand_step[step==min_step+1, c('node_i', 'node_j', 'step', 'set')]
  
  
  return(el_rand_step)
})

el_rand = rbindlist(el_rand)

net$max_step
max(el_rand$step)

el_rand$step=el_rand$step-1
el_rand = el_rand[step>=min(net$el$step)]

min(el_rand$step)
max(el_rand$step)

View(el_rand[step==min(el_rand$step)])
View(net$el[step==min(el_rand$step)])

View(el_rand[step==max(el_rand$step)])
View(net$el[step==max(el_rand$step)])

index=sample(unique(el_rand$step),1)
View(el_rand[step==index])
View(net$el[step==index])

save(el_rand, file='output/results/20230803/el_rand_param_12_work_2015.RData')

