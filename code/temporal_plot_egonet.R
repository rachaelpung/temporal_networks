source('code/temporal_library.R')
source('code/temporal_functions.R')

# load temporal edgelist
list_folder = dir('output/results/20230403/', pattern = 'net_param_05')
load(paste('output/results/20230403/', list_folder, sep = ''))
el_temp = copy(net$el)

# load random edgelist
list_folder = dir('output/results/20230403/', pattern = 'el_rand_param_05')
load(paste('output/results/20230403/', list_folder, sep = ''))
el_rand = el_rand[set==1]

# random network preserves degree distribution 
# but the number of contacts formed in the next step is independent of previous step 
set.seed(111)
node_rand = el_rand[step==8,.N, node_i]
node_rand[, node_i:=sample(node_i,.N, replace=F)]
el_rand[step==8, node_i:=rep(node_rand$node_i, times=node_rand$N)]



# load nodelist
list_folder = dir('data/', pattern = 'nl_haslemere')
load(paste('data/', list_folder, sep = ''))
nl = copy(nl_haslemere)

# choose focal node and timestep
kl = copy(net$kl)
kl = kl[k0!=0]
table(kl$k0)
kl[k0==max(k0)]
focal_node = '73'
timestep = c(8,9)

# setup nodelist
nl$node = as.character(nl$node)
nl[node==focal_node, focal_am:=TRUE]
nl[node!=focal_node, focal_am:=FALSE]

# find nodes in contact with focal node for el_temp
node_t0 = el_temp[node_i==focal_node & step==8, node_j]
node_t1 = el_temp[node_i==focal_node & step==9, node_j]
node_old = setdiff(node_t0, node_t1)
node_new = setdiff(node_t1, node_t0)
node_fix = setdiff(node_t0, node_old)

nl[node %in% node_old, status_temp:='old']
nl[node %in% node_new, status_temp:='new']
nl[node %in% node_fix, status_temp:='fix']
nl[node %in% focal_node, status_temp:='focal']
nl[is.na(status_temp), status_temp:='non_focal']

# find nodes in contact with focal node for el_rand
node_t1 = el_rand[node_i==focal_node & step==8 & set==1, node_j]
node_old = setdiff(node_t0, node_t1)
node_new = setdiff(node_t1, node_t0)
node_fix = setdiff(node_t0, node_old)

nl[node %in% node_old, status_rand:='old']
nl[node %in% node_new, status_rand:='new']
nl[node %in% node_fix, status_rand:='fix']
nl[node %in% focal_node, status_rand:='focal']
nl[is.na(status_rand), status_rand:='non_focal']

nodelist = copy(nl)

# setup edgelist
el_temp = contact_type(n=5, el_temp)
el_rand = contact_type(n=5, el_rand)

# setup adjacency matrix
list_am = lapply(1:3, function(x){
  
  if(x==1){ el_step = el_temp[step==8, c('node_i', 'node_j')] } #, 'contact_type')] }
  if(x==2){ el_step = el_temp[step==9, c('node_i', 'node_j')] } #, 'contact_type')] }
  if(x==3){ el_step = el_rand[step==8, c('node_i', 'node_j')] } #, 'contact_type')] }
  
  graph_step =  graph_from_data_frame(el_step, directed = TRUE, vertices = nl)
  graph_step = as.undirected(graph_step, mode ='collapse', edge.attr.comb="first")
  am = as_adjacency_matrix(graph_step)
  am = as.matrix(am)
  
  return(am)
})

# setup network layout for both timesteps
node_id = nl$node
am_full = matrix(0,length(node_id),length(node_id),dimnames=list(node_id,node_id))
am_full[,focal_node] = sample(5, length(node_id), replace=T) # to ensure all nodes are in layout
am_full[focal_node,] = am_full[,focal_node]
# sample_row = sample(length(node_id), 50, replace=F); sample_col = sample(length(node_id), 100, replace=F)
# am_full[sample_row,sample_col] = sample(1, 50, replace=T)
# am_full[sample_col,sample_row] = am_full[sample_row,sample_col]

# to identify focal_assocs throughout period of study
# for(i in 1:length(timestep)){
#   am_u = list_am[[i]]
#   step_ids = rownames(am_u)
#   am_full[step_ids,step_ids] = am_full[step_ids,step_ids] + am_u
# }

focal_assocs = am_full[,focal_node]>0
focal_assocs[focal_node] = T
focal_assocs = colnames(am_full)[focal_assocs]
am_full_m = am_full[focal_assocs,focal_assocs]
# am = am_full_m
am = am_full

circ = T
pheight = 9;pwidth = 9
pres = 200

mint_mal = rev(sequential_hcl(length(25)+5,palette="Mint")) #matches age exactly

set.seed(222)
png(paste0("figure/overall_test_layout.png"),height=pheight,width=pwidth,units="in",res=pres)
par(mar=c(1,1,1,1),bg="white")
full_focal_layout = draw_focal_plot(am=am,nl=nodelist,el=el_temp,i=1,circlay=circ,inc_attr=F) #just to generate the point layout - the plot isnt actually needed
dev.off()


all_points = full_focal_layout  #saves the overall best point layout for the time-step networks built below:

# drawing the individual time-step networks:
range_scale = c(0,2)
circ = T; inc_attr = T; colour_edge_type = T; title_use = NULL; plot_all_points = T
i = 1
dev.off()

for(i in 1:length(timestep)){
  
  time = timestep[i]
  
  file.title<-paste0("timestep_00", time,"focalnet_circ_",circ,"_height",pheight,"_width",pwidth,"_res",pres," _date",Sys.Date())
  png(paste0("figure/timestepnets/",file.title,".png"),height=pheight,width=pwidth,units="in",res=pres)
  par(mar=c(1,1,1,1),bg="white")
  
  am_time = list_am[[i]]
  nl_time = copy(nl_haslemere)
  el_time = copy(el_haslemere[step %in% timestep])
  assocs = am_time[focal_node,]>0
  title_use = paste0("Timestep_00", time)

  if(sum(assocs)>1){ #if a real network
    draw_focal_plot(am=am_time,am_layout=full_focal_layout,
                    nl=nl_time,el=el_time,i=i,
                    circlay=circ,inc_attr=T,colour_edge_type=T,
                    title_use=title_use,plot_all_points=plot_all_points)
  }
  if(sum(assocs)==1){ #if just one link
    
    plot(full_focal_layout[!rownames(full_focal_layout) %in% c(focal_node,names(assocs)[assocs]),],type="p",xlim=range_scale,ylim=range_scale,pch=21,col="black",cex=0.5,bty="n",xaxt="n",yaxt="n",xlab="",ylab="",main=title_use)
    points(x=full_focal_layout[focal_node,1],y=full_focal_layout[focal_node,2],pch=21,col="red",bg=mint_mal[nl_time$Age[nl_time$focal_am]],cex=0.4,bty="n",xaxt="n",yaxt="n",xlab="",ylab="")
    points(x=full_focal_layout[names(assocs)[assocs],1],y=full_focal_layout[names(assocs)[assocs],2],pch=21,col=mint_mal[nl_time$Age[assocs]],bg=mint_mal[nl_time$Age[assocs]],cex=0.4)
    line.col<-ifelse(colour_edge_type,mint_mal[nl_time$Age[assocs]],mint_mal[nl_time$Age[nl_time$focal_am]])
    lines(x=c(full_focal_layout[focal_node,1],full_focal_layout[names(assocs)[assocs],1]),y=c(full_focal_layout[focal_node,2],full_focal_layout[names(assocs)[assocs],2]),col=line.col)
    #text(x=full_focal_layout[rownames(full_focal_layout) %in% focal_node,1],y=full_focal_layout[rownames(full_focal_layout) %in% focal_node,2],labels="M",col="red",cex=1.5)
    
  }
  if(sum(assocs)<1){ #if no links
    plot(full_focal_layout[!rownames(full_focal_layout) %in% c(focal_node),],type="p",xlim=range_scale,ylim=range_scale,pch=21,col="black",cex=0.5,bty="n",xaxt="n",yaxt="n",xlab="",ylab="",main=title_use)
    points(x=full_focal_layout[focal_node,1],y=full_focal_layout[focal_node,2],main=title_use,pch=21,col="red",bg=mint_mal[nl_time$Age[nl_time$focal_am]],cex=0.3)
    #text(x=full_focal_layout[rownames(full_focal_layout) %in% focal_node,1],y=full_focal_layout[rownames(full_focal_layout) %in% focal_node,2],labels="M",col="red",cex=1.5)
  }
  
  par(mar=c(1,1,1,1),bg="white")
  legend_image <- as.raster(matrix(rev(mint_mal), nrow=length(mint_mal)))
  rasterImage(legend_image, 1.0, 3.5, 1.15, 7.5)
  text(x=c(0.85,0.85), y = c(3.5,7.5), labels = c("Young","Old"),col=c("black"),srt=90,cex=1.2)
  
  dev.off()
  print(i)
  
 
}


paneller=function(row = 1,column=1)
{
  
  if(column==1) {
    time = timestep[1]
    el_time = el_temp[step==time]
    title_use = 'Fully static'
  }
  if(column==2) {
    time = timestep[2]
    el_time = el_temp[step==time]
    title_use = 'Temporal'
  }
  if(column==3) {
    time = timestep[1]
    el_time = el_rand[step==time]
    title_use = 'Fully dynamic'
  }
  
  am_time = list_am[[column]]
  nl_time = copy(nodelist)
  assocs = am_time[focal_node,]>0
  
  if(sum(assocs)>1){ #if a real network
    draw_focal_plot(am=am_time,am_layout=full_focal_layout,
                    nl=nl_time,el=el_time,i=column,
                    circlay=circ,inc_attr=T,colour_edge_type=T,
                    title_use=title_use,plot_all_points=plot_all_points)
    
    if(column==1) mtext('A', side=3, adj=0)
    if(column==2) mtext('B', side=3, adj=0)
    if(column==3) mtext('C', side=3, adj=0)
    
  }
  
  
  
}



# draw static, temporal, random 
png('figure/schematic2.png',height=8,width=24,units='cm',res=300,pointsize=10)
par(mfrow=c(1,3),mar=c(2,2,2,0),mgp=c(2,0.6,0),las=0)

paneller(1,1)
paneller(1,2)
paneller(1,3)

dev.off()


