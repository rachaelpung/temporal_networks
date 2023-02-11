source("code/temporal_functions.R")
source("code/temporal_library.R")

# load edge list and meta data
el_name = 'cruise_1' # cruise_1, high_school_2012
el = get(load(paste("data/el_", el_name, ".RData", sep=''))) 

load("data/el_meta.RData")
row = which(el_meta$network==el_name)

# edge list in respective time unit 
el = network_time(el, el_meta[row,], time_unit=900)

# aggregate contacts by node and time unit
kl = contact_time(el)

pmf_k0 = kl[,.N, by=.(k0)]
pmf_k0_retain = kl[,.N, by=.(k0,k0_retain)]
pmf_k0_k1 = kl[,.N, by=.(k0,k1)]
pmf_k0_k1=pmf_k0_k1[!is.na(k1)]

setorder(pmf_k0, k0)
setorder(pmf_k0_retain, k0, k0_retain)
setorder(pmf_k0_k1, k0,k1)

pmf_k0[,P:=N/sum(N)]
pmf_k0_retain[,P:=N/sum(N), by=.(k0)]
pmf_k0_k1[,P:=N/sum(N), by=.(k0)]

# quick plot
colline = c('black','red','blue','green','yellow','orange','pink','turquoise')
plot(pmf_k0$k0, pmf_k0$P, type='l', xlab='no. of contacts in one time unit', ylab='prop')

for(i in 1:8){
  
  if(i==1) plot(pmf_k0_retain[k0==i]$k0_retain, pmf_k0_retain[k0==i]$P, type='l', 
                xlab='no. of contacts retained in next time unit', ylab='prop', col=colline[i],
                xlim=c(0,8), ylim=c(0,1))  
  if(i!=1) lines(pmf_k0_retain[k0==i]$k0_retain, pmf_k0_retain[k0==i]$P, col=colline[i])
  if(i==8) legend("topright", legend=c("1 contact", "2 contact", '3 contact', '4 contact',
                                      '5 contact','6 contact','7 contact', '8 contact'),
                 col=colline, lty=1, cex=0.6)
  
  
}

for(i in 1:8){
  
  if(i==1) plot(pmf_k0_k1[k0==i]$k1, pmf_k0_k1[k0==i]$P, type='l', 
                xlab='no. of contacts in next time unit', ylab='prop', col=colline[i],
                xlim=c(0,8), ylim=c(0,1))  
  if(i!=1) lines(pmf_k0_k1[k0==i]$k1, pmf_k0_k1[k0==i]$P, col=colline[i])
  if(i==8) legend("topright", legend=c("1 contact", "2 contact", '3 contact', '4 contact',
                                       '5 contact','6 contact','7 contact', '8 contact'),
                  col=colline, lty=1, cex=0.6)
  
  
}

# compare with poisson distribution 
pmf_k0_k1[, P_cum:=cumsum(P), by=.(k0)]
pmf_pois = sapply(1:13, function(x){ cumsum(dpois(0:50, x))[x+1] })

plot(pmf_k0_k1[k0==k1]$k0, pmf_k0_k1[k0==k1]$P_cum, xlab='no of contact in current unit', ylab='prop', ylim=c(0,1), type='l')
lines(1:13, pmf_pois, col='red')


# check if haslemere or cruise behave like french high school 
# or work data if aggregate by time window
# No, still retain original patterns

# generate random and static networks with same degree distribution for comparison
# sample network for a step as static network
# extract network for the next step 
# check how many nodes were retained
# randomise network in the next step and check how many were retained
# check the degree distribution in real life network in current step,
# real life network in next step and in the randomised network

net=copy(el)


sample_step = sample(c(net$step),1)
n_node = uniqueN(c(net$node_i,net$node_j))

net_x = net[step==sample_step,c('node_i', 'node_j')]
net_y = net[step==sample_step+1,c('node_i', 'node_j')]
net_z = graph_from_data_frame(net_y, directed=T) 

n_edges = length(E(net_z))
n_nodes = length(V(net_z))
net_z = rewire(net_z,keeping_degseq(niter=n_edges))
net_z = as_edgelist(net_z)
net_z = data.table(net_z)
setnames(net_z, c('node_i', 'node_j'))

pmf_k_x = net_x[,.N, by=.(node_i)]
pmf_k_x = pmf_k_x[,.N, by=.(N)]
setnames(pmf_k_x, c('k','N')); setorder(pmf_k_x)

pmf_k_x = rbind(data.table(k=0, N=n_node-sum(pmf_k_x$N)), pmf_k_x)
pmf_k_x[,P:=N/sum(N)]


pmf_k_y = net_y[,.N, by=.(node_i)]
pmf_k_y = pmf_k_y[,.N, by=.(N)]
setnames(pmf_k_y, c('k','N')); setorder(pmf_k_y)

pmf_k_y = rbind(data.table(k=0, N=n_node-sum(pmf_k_y$N)), pmf_k_y)
pmf_k_y[,P:=N/sum(N)]

pmf_k_z = net_z[,.N, by=.(node_i)]
pmf_k_z = pmf_k_z[,.N, by=.(N)]
setnames(pmf_k_z, c('k','N')); setorder(pmf_k_z)

pmf_k_z = rbind(data.table(k=0, N=n_node-sum(pmf_k_z$N)), pmf_k_z)
pmf_k_z[,P:=N/sum(N)]

net_x[, step:=sample_step]; net_x[, contact:=1]
net_y[, step:=sample_step+1]; net_y[, contact:=1]
net_z[, step:=sample_step+1]; net_z[, contact:=1]

net_x_temp = copy(net_x)
net_x_rand = copy(net_x)

net_x_temp[, next_step:=step+1]
net_x_temp[net_y, next_contact:=i.contact, on=c(node_i='node_i', node_j='node_j', next_step='step')]
net_x_temp[is.na(next_contact), next_contact:=0]

net_x_rand[, next_step:=step+1]
net_x_rand[net_z, next_contact:=i.contact, on=c(node_i='node_i', node_j='node_j', next_step='step')]
net_x_rand[is.na(next_contact), next_contact:=0]

net_x_temp=net_x_temp[,.(sum(contact), sum(next_contact)), by=.(node_i)]
net_x_temp=net_x_temp[,.N, by=.(V1,V2)]
setnames(net_x_temp, c('k0','k0_retain', 'N'))
setorder(net_x_temp, k0, k0_retain)
net_x_temp[,P:=N/sum(N), by=.(k0)]

net_x_rand=net_x_rand[,.(sum(contact), sum(next_contact)), by=.(node_i)]
net_x_rand=net_x_rand[,.N, by=.(V1,V2)]
setnames(net_x_rand, c('k0','k0_retain', 'N'))
setorder(net_x_rand, k0, k0_retain)
net_x_rand[,P:=N/sum(N), by=.(k0)]


for(i in 1:8){
  pmf=copy(net_x_temp)
  
  if(i==1) plot(pmf[k0==i]$k0_retain, pmf[k0==i]$P, type='l', 
                xlab='no. of contacts retained in next time unit', ylab='prop', col=colline[i],
                xlim=c(0,8), ylim=c(0,1))  
  if(i!=1) lines(pmf[k0==i]$k0_retain, pmf[k0==i]$P, col=colline[i])
  if(i==8) legend("topright", legend=c("1 contact", "2 contact", '3 contact', '4 contact',
                                       '5 contact','6 contact','7 contact', '8 contact'),
                  col=colline, lty=1, cex=0.6)
  
  
}

