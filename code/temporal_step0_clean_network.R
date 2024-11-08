# load cruise data
load("~/Desktop/PhD/modelling COVID-19/cruise networks/Cruise network/output/edgelist/dataContact_20201106.RData")

dataContact$CONTACT_TYPE =NULL

# check if network is undirected
View(dataContact[1:100,])
View(dataContact[(MID.x=='111A00002C47' & MID.y=='122B0002AFDE' ) | (MID.x=='122B0002AFDE' & MID.y=='111A00002C47' )])
View(dataContact[100000:100100,])
View(dataContact[(MID.x=='122C0002FD8D' & MID.y=='122C0002F953' ) | (MID.x=='122C0002F953' & MID.y=='122C0002FD8D' )])

# check timestamp
dateStart = as.Date('2021-02-17')
dateEnd = as.Date('2021-02-19')

startTmp = as.numeric(as.POSIXct(paste(dateStart, '19:00:00', sep = ' '), tz = 'Etc/GMT-8'))
endTmp   = as.numeric(as.POSIXct(paste(dateEnd, '08:00:00', sep = ' '), tz = 'Etc/GMT-8'))

duration = endTmp-startTmp


# should return NULL
which(dataContact$START_TIMESTAMP < startTmp)
which(dataContact$START_TIMESTAMP > endTmp)
which(dataContact$END_TIMESTAMP > endTmp)
which(dataContact$END_TIMESTAMP < startTmp)

# should return something
dataContact[,.N]
dataContact[START_TIMESTAMP>=startTmp,.N]

# Day of interaction
dataContact[as.Date(as.POSIXct(START_TIMESTAMP, origin="1970-01-01"), tz = 'Etc/GMT-8') == dateStart,   day_start := 1]
dataContact[as.Date(as.POSIXct(START_TIMESTAMP, origin="1970-01-01"), tz = 'Etc/GMT-8') == dateStart+1, day_start := 2]
dataContact[as.Date(as.POSIXct(START_TIMESTAMP, origin="1970-01-01"), tz = 'Etc/GMT-8') == dateStart+2, day_start := 3]

dataContact[as.Date(as.POSIXct(END_TIMESTAMP, origin="1970-01-01"), tz = 'Etc/GMT-8') == dateStart,   day_end := 1]
dataContact[as.Date(as.POSIXct(END_TIMESTAMP, origin="1970-01-01"), tz = 'Etc/GMT-8') == dateStart+1, day_end := 2]
dataContact[as.Date(as.POSIXct(END_TIMESTAMP, origin="1970-01-01"), tz = 'Etc/GMT-8') == dateStart+2, day_end := 3]
View(dataContact[1:100,])

dataContact[day_end<day_start,.N]

# change edgelist header to node_i, node_j, time start, end, duration
setnames(dataContact, new=c('node_i', 'node_j', 'time_start', 'time_end', 'duration', 'day_start', 'day_end'))

# save
el_cruise_4 = copy(dataContact)
save(el_cruise_4, file='~/Desktop/PhD/modelling COVID-19/temporal networks/data/el_cruise_4.RData')


# load node data 
load("~/Desktop/PhD/modelling COVID-19/cruise networks/Cruise network/output/nodes/dataNodes_20210217.RData")

colnames(dataNodes)
dataNodes=dataNodes[,1:12]
setnames(dataNodes, new=c('node', 'cohort', 'type' , 'date', 'sap_no', 'cabin_no', 
                          'booking_no', 'age', 'gender', 'nationality', 'cruise_id', 'manifest_no')) 

nl_cruise_4 = copy(dataNodes)
save(nl_cruise_4, file='~/Desktop/PhD/modelling COVID-19/temporal networks/data/nl_cruise_4.RData')


# load haslemere data
# load edgelist and timelist
tl <- fread('~/Desktop/PhD/modelling COVID-19/generation interval/data/haslemere_time.csv')
el <- fread('~/Desktop/PhD/modelling COVID-19/generation interval/data/haslemere_edge.csv')

setnames(tl, c('interval', 'date_time'))
setnames(el, c('interval', 'node_i', 'node_j', 'distance'))
setorder(el, node_i, node_j, interval)

# distance threshold for a contact
dist_thres=10

# identify potential household contacts
tl[, hour_min:= as.numeric(paste(substr(date_time, 17, 18), substr(date_time, 20, 21), sep=''))]
tl[, day:=substr(date_time, 1, 3)]

tl[hour_min<800, loc:='hh_day']
tl[hour_min>=2000, loc:='hh_night']
tl[!(hour_min<800 | hour_min>=2000), loc:='nhh_day']

el[tl, loc:=i.loc, on=c(interval='interval')]
el[tl, date_time:=i.date_time, on=c(interval='interval')]
el[, time_start:=as.numeric(as.POSIXct(date_time, "%a %d %b %Y %H:%M:%S", tz='UTC'))]
el[, time_end:=time_start+5*60]
el[, day:=substr(date_time, 1, 3)]

# el[,ct:=0]
# el[distance<=dist_thres, ct:=1]

nl=el[loc %in% c('hh_day', 'hh_night') & distance<=dist_thres, uniqueN(loc), by=.(node_i,node_j,day)]
nl=nl[,sum(V1), by=.(node_i,node_j)]
table(nl$V1)

# contact hh, 1: household, 0: non-household
nl[V1>=5, ct_hh:=1]
nl[V1<5, ct_hh:=0]
nl[,V1:=NULL]

# undirected edgelist
nl=rbind(nl,nl)
nl[(.N/2+1):.N, `:=` (node_i=nl[1:(.N/2),node_j],
                      node_j=nl[1:(.N/2),node_i])]

# el[nl, ct_hh:=i.ct_hh, on=c(node_i='node_i', node_j='node_j')]
# el[is.na(ct_hh), ct_hh:=0]

el[day=='Thu', day_start:=1]
el[day=='Fri', day_start:=2]
el[day=='Sat', day_start:=3]

el[day=='Thu', day_end:=1]
el[day=='Fri', day_end:=2]
el[day=='Sat', day_end:=3]

el[,loc:=NULL]
el$date_time= el$interval = el$distance= el$day= NULL
# el=el[ct==1]

el[, diff:=time_start-lag(time_end), by=.(node_i, node_j, day_start)]
el[diff>0, diff := NA]

tmp1 = el[, which(is.na(diff))] + 1 
tmp2 = el[, which(is.na(diff))] 
rowStartContact = setdiff(tmp1, tmp2)
rowStartContact = rowStartContact[which(rowStartContact != el[,.N]+1)]
rowStartContact = rowStartContact -1

tmp1 = el[, which(is.na(diff))] - 1 
tmp2 = el[, which(is.na(diff))] 
rowEndContact = setdiff(tmp1, tmp2)
rowEndContact = rowEndContact[which(rowEndContact != 0)]

el_contiuous = el[rowStartContact]
el_contiuous[, time_end := el[rowEndContact, time_end]]
el_contiuous[, day_end := el[rowEndContact, day_start]]

rowOneOff=which(is.na(el$diff))
rowOneOff=rowOneOff[!(rowOneOff %in% rowStartContact)]
el_one_off=el[rowOneOff]
el_one_off[, time_end := el[rowOneOff, time_end]]
el_one_off[, day_end := el[rowOneOff, day_start]]


el_final = rbind(el_contiuous, el_one_off)
el_final[, diff := NULL]
el_final[, duration:=time_end-time_start]
  
nl_hh = data.table(id=sort(unique(c(nl[ct_hh==1, node_i],nl[ct_hh==1, node_j]))))

graph = graph_from_data_frame(unique(nl[ct_hh==1, .(node_i, node_j)]), directed = TRUE, vertices = nl_hh)
graph = as.undirected(graph, mode ='collapse', edge.attr.comb="first")

table(components(graph)$csize)

nl=data.table(node=names(components(graph)$membership), household_no=components(graph)$membership)


el_haslemere=copy(el_final)
nl_haslemere=copy(nl)
save(el_haslemere, file='~/Desktop/PhD/modelling COVID-19/temporal networks/data/el_haslemere.RData')
save(nl_haslemere, file='~/Desktop/PhD/modelling COVID-19/temporal networks/data/nl_haslemere.RData')


# load the rest of the temporal data and meta data
# http://www.sociopatterns.org/datasets/

# el.malawi, 14 days
load("~/Desktop/PhD/modelling COVID-19/generation interval/data/networks/004_el.work.15.RData")

el=copy(el.work.15) # el.high.school.11, el.high.school.12, el.high.school.13, el.hospital, el.work.13, el.work.15 
el=data.table(el)

el[, day:=as.Date(as.POSIXct(time, origin="1970-01-01"))]
el[, day:=as.numeric(day-min(day)+1)]


# colnames
setnames(el, old=c('time', 'id.x', 'id.y', 'day'), new=c('time_start', 'node_i', 'node_j', 'day_start')) 
# el.high.school.11, el.high.school.12, el.high.school.13, el.hospital


el=el[,c('node_i', 'node_j','time_start','day_start')]      
setorder(el, node_i, node_j, time_start, day_start)




el[,time_end:=time_start]
el[,time_start:=time_start-20]
       
el[, diff:=time_start-lag(time_end), by=.(node_i, node_j)]
el[diff>0, diff := NA]

tmp1 = el[, which(is.na(diff))] + 1 
tmp2 = el[, which(is.na(diff))] 
rowStartContact = setdiff(tmp1, tmp2)
rowStartContact = rowStartContact[which(rowStartContact != el[,.N]+1)]
rowStartContact = rowStartContact -1

tmp1 = el[, which(is.na(diff))] - 1 
tmp2 = el[, which(is.na(diff))] 
rowEndContact = setdiff(tmp1, tmp2)
rowEndContact = rowEndContact[which(rowEndContact != 0)]

el_contiuous = el[rowStartContact]
el_contiuous[, time_end := el[rowEndContact, time_end]]
el_contiuous[, day_end := el[rowEndContact, day_start]]

rowOneOff=which(is.na(el$diff))
rowOneOff=rowOneOff[!(rowOneOff %in% rowStartContact)]
el_one_off=el[rowOneOff]
el_one_off[, time_end := el[rowOneOff, time_end]]
el_one_off[, day_end := el[rowOneOff, day_start]]


el_final = rbind(el_contiuous, el_one_off)
el_final[, diff := NULL]
el_final[, duration:=time_end-time_start]

el_final=el_final[,c('node_i','node_j','time_start','time_end','duration','day_start','day_end')]
setorder(el_final, node_i, node_j, time_start)

el_work_2015 = copy(el_final)
save(el_work_2015, file='~/Desktop/PhD/modelling COVID-19/temporal networks/data/el_work_2015.RData')

# el.work.15.colocate, 14 days
# el.work.13.colocate, 14 days
# el.high.school.13.colocate, 7 days
# el.hospital.colocate, 3 days


# el.work.15
# el.work.13
# el.high.school.13
# el.high.school.12
# el.high.school.11
# el.hospital
nl=data.table(node=c(el$id.x, el$id.y), class=c(el$class.x, el$class.y))
nl=copy(meta.work.15)
nl=unique(nl)
setnames(nl, new=c('node', 'department'))

nl_work_2015 = copy(nl)
save(nl_work_2015, file='~/Desktop/PhD/modelling COVID-19/temporal networks/data/nl_work_2015.RData')

# networks meta data
el_meta_cruise = data.table(network = c('cruise_1', 'cruise_2', 'cruise_3', 'cruise_4'),
                     date_start = c(as.Date('2020-11-06'), as.Date('2020-11-20'), as.Date('2021-01-29'),as.Date('2021-02-17')),
                     date_end = c(as.Date('2020-11-08'), as.Date('2020-11-22'), as.Date('2021-01-31'),as.Date('2021-02-19')))

el_meta_cruise[, time_start:=as.numeric(as.POSIXct(paste(date_start, 
                                                  '19:00:00', sep = ' '), 
                                            tz = 'Etc/GMT-8'))]

el_meta_cruise[, time_end:=as.numeric(as.POSIXct(paste(date_end, 
                                                  '08:00:00', sep = ' '), 
                                            tz = 'Etc/GMT-8'))]

el_meta_haslemere = data.table(network = 'haslemere',
                               date_start = as.Date('2017-10-12'),
                               date_end = as.Date('2017-10-14'))

el_meta_haslemere[, time_start:=as.numeric(as.POSIXct(paste(date_start, 
                                                         '07:00:00', sep = ' '), 
                                                   tz = 'UTC'))]

el_meta_haslemere[, time_end:=as.numeric(as.POSIXct(paste(date_end, 
                                                       '22:55:00', sep = ' '), 
                                                 tz = 'UTC'))]

el_meta_repo = data.table(network=c('high_school_2011', 'high_school_2012', 'high_school_2013',
                                    'hospital', 'malawi', 'work_2013', 'work_2015'),
                          time_start=c(min(el_high_school_2011$time_start),
                                       min(el_high_school_2012$time_start),
                                       min(el_high_school_2013$time_start),
                                       min(el_hospital$time_start),
                                       min(el_malawi$time_start),
                                       min(el_work_2013$time_start),
                                       min(el_work_2015$time_start)),
                          time_end=c(max(el_high_school_2011$time_end),
                                     max(el_high_school_2012$time_end),
                                     max(el_high_school_2013$time_end),
                                     max(el_hospital$time_end),
                                     max(el_malawi$time_end),
                                     max(el_work_2013$time_end),
                                     max(el_work_2015$time_end)),
                          date_start=as.Date(c(NA,'2012-11-19','2012-12-02','2010-12-06',NA,'2013-06-24',NA)),
                          date_end=as.Date(c(NA,'2012-11-27','2012-12-06','2010-12-10',NA,'2013-07-03',NA)))
  
el_meta = rbind(el_meta_cruise, el_meta_haslemere, el_meta_repo)
el_meta[,type:=c(1,1,1,1,1,2,2,2,2,2,2,2)] # type 1: continuous contact for long periods, 2: multiple breaks in contact

save(el_meta, file='~/Desktop/PhD/modelling COVID-19/temporal networks/data/el_meta.RData')
