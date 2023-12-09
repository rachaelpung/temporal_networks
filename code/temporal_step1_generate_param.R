source("code/temporal_library.R")
source("code/temporal_functions.R")

load("data/el_meta.RData")

net_duration = lapply(1:nrow(el_meta), function(x){
  
  el = get(load(paste("data/el_", el_meta[x,network], ".RData", sep=''))) 
  setorder(el, node_i, node_j, time_start)
  el[, inter_duration:=lead(time_start, n=1)-time_end, by=.(node_i, node_j)]
  
  dur = t(c(quantile(el$duration, probs=c(0.25,0.5,0.75), na.rm = T),
          quantile(el$inter_duration, probs=c(0.25,0.5,0.75), na.rm = T)))
  dur = data.table(dur)
  names(dur) = c('dur_lwr', 'dur_med', 'dur_upp', 'int_dur_lwr', 'int_dur_med', 'int_dur_upp')
  
  return(dur)
})

net_duration = rbindlist(net_duration)
net_duration$net = el_meta$network

param = data.table(net = c(net_duration$net, rep(net_duration[!grep('cruise', net)]$net, times=1)),
                   inter_dur = c(900,900,900,900,net_duration[!grep('cruise', net)]$int_dur_med,
                                 net_duration[!grep('cruise', net)]$int_dur_lwr),
                   time_unit = c(900,900,900,900,rep(net_duration[!grep('cruise', net)]$dur_lwr, times=2)))

param[, scale:=inter_dur/time_unit]

write.csv(param, 'data/param.csv', row.names = F)
