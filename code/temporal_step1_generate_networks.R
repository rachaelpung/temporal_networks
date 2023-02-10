source("code/temporal_functions.R")
source("code/temporal_library.R")

# load edge list and meta data
el_name = 'high_school_2012' # cruise_1
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







# generate random and static networks for comparison
# check if haslemere or cruise behave like french high school or work data


