source("code/temporal_library.R")
source("code/temporal_functions.R")

# load outputs from each time step
list_folder = dir('output/20230403/', pattern = 'work_2013_step')
list_folder

output = lapply(1:length(list_folder), function(x){
  get(load(paste('output/20230403/', list_folder[x], sep = '')))
})

# load network 
list_folder = dir('output/results/20230403/', pattern = 'net_param_11')
list_folder
load(paste('output/results/20230403/', list_folder, sep = ''))

# load nodelist
load("data/nl_work_2013.RData")
nl=copy(nl_work_2013)

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
r_contact_type = retain_prop_contact_type(n=11, net$el, nl, r_scale)

node_p80_day_ct_esp = p80_contact_day(net$steps_length, net$kl, net$n_nodes, type='contacts')
node_r80_day_ct_esp = node_rank_day(node_p80_day_ct_esp)

node_p80_day_ct_dur = p80_contact_day(net$steps_length, net$kl, net$n_nodes, type='duration')
node_r80_day_ct_dur = node_rank_day(node_p80_day_ct_esp)

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

save(el_rand, file='output/results/20230403/el_rand_param_11_work_2013.RData')

