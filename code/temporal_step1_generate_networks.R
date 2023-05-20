source("code/temporal_library.R")
source("code/temporal_functions.R")

# load param, edge list and meta data
param = fread("data/param.csv")
n = 5; el_name = param[n,net]
el = get(load(paste("data/el_", el_name, ".RData", sep=''))) 

load("data/el_meta.RData")
row = which(el_meta$network==el_name)

# edge list in respective time unit 
el = network_time(el, el_meta[row,], 
                  time_unit=param[n,time_unit], 
                  scale=param[n,scale]) 

# aggregate contacts by node and time unit
kl = contact_time(el)

# probability distributions 
p_k0 = pmf_k0(kl)
p_k0_r = pmf_k0_retain(kl)
p_k0_k1 = pmf_k0_k1(kl)

# quick plot
plot_k(p_k0, p_k0_r, p_k0_k1)

# compute probability distributions 
# set up clusters
set.seed(123)
cl = makeCluster(detectCores())
clusterExport(cl, as.vector(lsf.str()))
registerDoParallel(cl)

max_step = max(el$step)
n_nodes_total = uniqueN(c(el$node_i, el$node_j))

foreach(s = 1531:max_step, .packages = c('data.table', 'igraph'), .combine = 'c') %dopar%  {
  
  if(el[step==s,.N]>3){
    
  el_stat = network_stat(el, s)
  el_temp = network_temp(el, s)
  el_rand = network_rand(el, s)
  n_nodes = uniqueN(c(el_stat$node_i, el_stat$node_j))

  kl_stat = contact_time(el_stat)
  kl_temp = contact_time(el_temp)
  kl_rand = lapply(1:100, function(x){ contact_time(el_rand[[x]]) })
  
  p_k0_stat = pmf_k0(kl_stat, n_nodes_total)
  p_k0_temp = pmf_k0(kl_temp, n_nodes_total) 
  p_k0_rand = lapply(1:100, function(x){ pmf_k0(kl_rand[[x]], n_nodes_total) })
  
  p_k0_r_stat = pmf_k0_retain(kl_stat, n_nodes_total)
  p_k0_r_temp = pmf_k0_retain(kl_temp, n_nodes_total)
  p_k0_r_rand = lapply(1:100, function(x){ pmf_k0_retain(kl_rand[[x]], n_nodes_total) }) 
  
  p_k0_k1_stat = pmf_k0_k1(kl_stat, n_nodes_total)
  p_k0_k1_temp = pmf_k0_k1(kl_temp, n_nodes_total)
  p_k0_k1_rand = lapply(1:100, function(x){ pmf_k0_k1(kl_rand[[x]], n_nodes_total) })  
  
  p_r_stat = pmf_retain(p_k0_r_stat, p_k0_stat)
  p_r_temp = pmf_retain(p_k0_r_temp, p_k0_temp)
  p_r_rand = lapply(1:100, function(x){ pmf_retain(p_k0_r_rand[[x]], p_k0_rand[[x]]) })
  
  p_k0_k1_r_rand_avg = pmf_k0_k1_retain_avg(p_k0_stat, n_nodes)
  p_r_rand_avg = pmf_retain(p_k0_k1_r_rand_avg, p_k0_stat)
  
  node_p80_ct_esp = p80_contact(kl, s, type='contacts')
  node_p80_ct_dur = p80_contact(kl, s, type='duration')
  
  output = list(el_stat=el_stat, el_temp=el_temp, el_rand=el_rand, n_nodes=n_nodes,
                kl_stat=kl_stat, kl_temp=kl_temp, kl_rand=kl_rand,
                p_k0_stat=p_k0_stat, p_k0_temp=p_k0_temp, p_k0_rand=p_k0_rand,
                p_k0_r_stat=p_k0_r_stat, p_k0_r_temp=p_k0_r_temp, p_k0_r_rand=p_k0_r_rand,
                p_k0_k1_stat=p_k0_k1_stat, p_k0_k1_temp=p_k0_k1_temp, p_k0_k1_rand=p_k0_k1_rand,
                p_r_stat=p_r_stat, p_r_temp=p_r_temp, p_r_rand=p_r_rand,
                p_k0_k1_r_rand_avg=p_k0_k1_r_rand_avg, p_r_rand_avg=p_r_rand_avg,
                node_p80_ct_esp=node_p80_ct_esp, node_p80_ct_dur=node_p80_ct_dur)
  
  if(s<10){
    save(output,file= paste0("output/20230403/", el_name, '_step_000', s, ".rdata"))
  } else if(s<100){
    save(output,file= paste0("output/20230403/", el_name, '_step_00', s, ".rdata"))
  } else if(s<1000){
    save(output,file= paste0("output/20230403/", el_name, '_step_0', s, ".rdata"))
  } else{
    save(output,file= paste0("output/20230403/", el_name, '_step_', s, ".rdata"))
  }
  
  
  }
}


# stop clusters
stopCluster(cl) 

net = list(el=el, kl=kl, max_step=max_step, n_nodes_total=n_nodes_total)
save(net,file= paste0("output/net_param_", n, "_",el_name,".rdata"))

# # plot single run
# plot_k_r(p_k0_r_stat, p_k0_r_temp, p_k0_r_rand[[1]], p_k0_k1_r_rand_avg)
# plot_r(p_r_stat, p_r_temp, p_r_rand[[1]], p_r_rand_avg)

