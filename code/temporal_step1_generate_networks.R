source("code/temporal_functions.R")
source("code/temporal_library.R")

# load edge list and meta data
el_name = 'high_school_2012' # cruise_1, high_school_2012
el = get(load(paste("data/el_", el_name, ".RData", sep=''))) 

load("data/el_meta.RData")
row = which(el_meta$network==el_name)

# edge list in respective time unit 
el = network_time(el, el_meta[row,], time_unit=60) #time_unit=900 cruise, 300 haslemere, 60 for the rest

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

foreach(s = 1:max_step, .packages = c('data.table', 'igraph'), .combine = 'c') %dopar%  {
  
  el_stat = network_stat(el, s)
  el_temp = network_temp(el, s)
  el_rand = network_rand(el, s)
  n_nodes = uniqueN(c(el_stat$node_i, el_stat$node_j))

  kl_stat = contact_time(el_stat)
  kl_temp = contact_time(el_temp)
  kl_rand = lapply(1:100, function(x){ contact_time(el_rand[[x]]) })
  
  p_k0_stat = pmf_k0(kl_stat)
  p_k0_temp = pmf_k0(kl_temp) 
  p_k0_rand = lapply(1:100, function(x){ pmf_k0(kl_rand[[x]]) })
  
  p_k0_r_stat = pmf_k0_retain(kl_stat)
  p_k0_r_temp = pmf_k0_retain(kl_temp)
  p_k0_r_rand = lapply(1:100, function(x){ pmf_k0_retain(kl_rand[[x]]) }) 
  
  p_k0_k1_stat = pmf_k0_k1(kl_stat)
  p_k0_k1_temp = pmf_k0_k1(kl_temp)
  p_k0_k1_rand = lapply(1:100, function(x){ pmf_k0_k1(kl_rand[[x]]) })  
  
  p_r_stat = pmf_retain(p_k0_r_stat, p_k0_stat)
  p_r_temp = pmf_retain(p_k0_r_temp, p_k0_temp)
  p_r_rand = lapply(1:100, function(x){ pmf_retain(p_k0_r_rand[[x]], p_k0_rand[[x]]) })
  
  p_k0_k1_r_rand_avg = pmf_k0_k1_retain_avg(p_k0_stat, n_nodes)
  p_r_rand_avg = pmf_retain(p_k0_k1_r_rand_avg, p_k0_stat)
  
  output = list(el_stat=el_stat, el_temp=el_temp, el_rand=el_rand,n_nodes=n_nodes,
                kl_stat=kl_stat, kl_temp=kl_temp, kl_rand,kl_rand,
                p_k0_stat=p_k0_stat, p_k0_temp=p_k0_temp, p_k0_rand=p_k0_rand,
                p_k0_r_stat=p_k0_r_stat, p_k0_r_temp=p_k0_r_temp, p_k0_r_rand=p_k0_r_rand,
                p_k0_k1_stat=p_k0_k1_stat, p_k0_k1_temp=p_k0_k1_temp, p_k0_k1_rand=p_k0_k1_rand,
                p_k0_k1_r_rand_avg=p_k0_k1_r_rand_avg,p_r_rand_avg=p_r_rand_avg)
  
  if(s<10){
    save(output,file= paste0("output/20230310/", el_name, '_step_00', s, ".rdata"))
  } else if(p<100){
    save(output,file= paste0("output/20230310/", el_name, '_step_0', s, ".rdata"))
  } else{
    save(output,file= paste0("output/20230310/", el_name, '_step_', s, ".rdata"))
  }
  
  
}


# stop clusters
stopCluster(cl) 

# plot single run
plot_k_r(p_k0_r_stat, p_k0_r_temp, p_k0_r_rand[[1]], p_k0_k1_r_rand_avg)
plot_r(p_r_stat, p_r_temp, p_r_rand[[1]], p_r_rand_avg)







# finding inverse 
x = pmf_k0_rand$P
ginv(x)

x%*%ginv(x)%*%x
A=x%*%ginv(x)
B=rowSums(A)
C=colSums(A)

matrix(as.numeric(A)/rep(as.numeric(B), each=10), nrow=10,ncol=10, byrow = T)

C=t(A)/B

C%*%x





test = p_k0_k1_r_rand_avg[,sum(P), by=.(k0,r)]
test= p_r_rand_avg

for(i in 1:100){
  
  # tmp=copy(p_k0_r_rand[[i]][,c(1,2,4)])
  # names(tmp)[3] = paste('P_',i,sep='')
  # test = merge(test, tmp, by=c('k0','r'), all=T)
  
  tmp=copy(p_r_rand[[i]])
  names(tmp)[2] = paste('P_',i,sep='')
  test = merge(test, tmp, by=c('r'), all=T)
  
  
  
}
test[is.na(test),] = 0
# rowSums(test[,4:103])/100
rowSums(test[,3:102])/100
test[,1:2]

# calculate the value of p for temporal network 

# sensitivity analysis
# check if haslemere or cruise behave like french high school 
# or work data if aggregate by time window
# No, still retain original patterns

