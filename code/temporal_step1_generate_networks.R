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
  
  p_k0_r_rand_avg = pmf_k0_retain_avg(p_k0_stat)
  p_r_rand_avg = pmf_retain(p_k0_r_rand_avg, p_k0_stat)
  
  output = list(el_stat=el_stat, el_temp=el_temp, el_rand=el_rand,
                kl_stat=kl_stat, kl_temp=kl_temp, kl_rand,kl_rand,
                p_k0_stat=p_k0_stat, p_k0_temp=p_k0_temp, p_k0_rand=p_k0_rand,
                p_k0_r_stat=p_k0_r_stat, p_k0_r_temp=p_k0_r_temp, p_k0_r_rand=p_k0_r_rand,
                p_k0_k1_stat=p_k0_k1_stat, p_k0_k1_temp=p_k0_k1_temp, p_k0_k1_rand=p_k0_k1_rand)
  
  if(p<10){
    save(output,file= paste0("output/20220308/", el_name, '_step_00', s, ".rdata"))
  } else if(p<100){
    save(output,file= paste0("output/20220308/", el_name, '_step_0', s, ".rdata"))
  } else{
    save(output,file= paste0("output/20220308/", el_name, '_step_', s, ".rdata"))
  }
  
  
}


# stop clusters
stopCluster(cl) 



# for each time step, 
# determine prob retain 0,1,2,... contacts
# determine prob retain contacts conditional of k0
# determine prob retain contacts conditional of k1
# for random network could calculate an average probability 
# plot










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

# for each kt, each kt+1, each r, find p(r) and the associated p 
len=length(x)
retain_stat = data.table(k0 = rep(1:len, each=len),
                         k1 = rep(1:len, times=len),
                         pmf_k0_k1 = as.numeric(diag(1,len)))
retain_stat = retain_stat[rep(1:.N, times=len+1)]
retain_stat[, r:=rep(0:len, each=len*len)]
retain_stat = retain_stat[r<=k0 & r<=k1]
retain_stat[, p:=1]
retain_stat[, p_r:=pmf_k0_k1*choose(k0,r)*(p^r)*((1-p)^(k0-r))]

# View(retain_stat[p_r!=0])
sum(retain_stat$p_r)

retain_rand = data.table(k0 = rep(1:len, each=len),
                         k1 = rep(1:len, times=len),
                         pmf_k0_k1 = x)
retain_rand = retain_rand[rep(1:.N, times=len+1)]
retain_rand[, r:=rep(0:len, each=len*len)]
retain_rand = retain_rand[r<=k0 & r<=k1]
retain_rand[, p:=k1/len]
setorder(retain_rand, k0,k1,r)

retain_rand[, p_r:=choose(k0,r)*(p^r)*((1-p)^(k0-r))]
retain_rand[k1<k0, p_r:=p_r/sum(p_r), by=.(k0,k1)]
sum(retain_rand$p_r)

retain_rand[, p_r_combi:=pmf_k0_k1*p_r]
sum(retain_rand$p_r_combi)

sum(pmf_k0_retain_temp$P)
test = retain_rand[, sum(p_r_combi), by=.(k0,r)]
setnames(test, old=c('V1'), new=c('P_rand'))

test[k0==r,P_stat:=1]
test[k0!=r,P_stat:=0]

test[pmf_k0_retain_temp, P:=i.P, on=c(k0='k0', r='k0_retain')]
test[r==0]

test[pmf_k0_retain_rand, P_rand_real:=i.P, on=c(k0='k0', r='k0_retain')]



plot(test[r==0 & k0<=5]$k0,test[r==0 & k0<=5]$P_rand, type='l', ylim=c(0,1))
lines(test[r==0 & k0<=5]$k0,test[r==0 & k0<=5]$P, col='red')
lines(test[r==0 & k0<=5]$k0,test[r==0 & k0<=5]$P_stat, col='blue')

plot(test[r==k0 & k0<=5]$k0,test[r==k0 & k0<=5]$P_rand, type='l', ylim=c(0,1))
lines(test[r==k0 & k0<=5]$k0,test[r==k0 & k0<=5]$P, col='red')
lines(test[r==k0 & k0<=5]$k0,test[r==k0 & k0<=5]$P_stat, col='blue')

k0=2
k1=0:3
pk1=c(0.1,0.25,0.4,0.25) # probability of k1
p=k1/3 # probability of retaining a link given k1
sum(p*pk1) # gives the probability of retaining link regardless of k1

choose(2,0)*((0.5)^0)*((1-0.5)^2) # 0.16
choose(2,1)*((0.5)^1)*((1-0.5)^1) # 0.48
choose(2,2)*((0.5)^2)*((1-0.5)^0) # 0.36

test = data.table(k0=rep(2,4), k1=0:3)
test=test[rep(1:nrow(test),each=3)]
test[,r:=rep(0:2, times=4)]

test[,pmf_k0_k1:=rep(pk1, each=3)]
test[,p:=k1/3]

test=test[r<=k1]
test[,p_r:=choose(k0,r)*(p^r)*((1-p)^(k0-r))]
test[k1<k0, p_r:=p_r/sum(p_r), by=.(k0,k1)]

test[, p_r_combi:=pmf_k0_k1*p_r]
sum(test$p_r_combi)


test[, sum(p_r_combi), by=.(k0,r)]


# ensure that random network average function and actual function is coded
# calculate the value of p for temporal network 

# sensitivity analysis
# check if haslemere or cruise behave like french high school 
# or work data if aggregate by time window
# No, still retain original patterns

