#pre-load functions
make_network<-function(gbi){
  yb<-xab<-matrix(NA,ncol(gbi),ncol(gbi))
  colsums<-colSums(gbi)
  for(i in 1:ncol(gbi)){
    sums<-gbi[which(gbi[,i]>0),]
    if(is.null(dim(sums))==T) {xab[,i]<-sums} else {xab[,i]<-colSums(sums)}}
  yb<-matrix(colsums,ncol(gbi),ncol(gbi)); ya<-t(yb)
  am<-xab/(xab+(ya-xab)+(yb-xab))
  diag(am)<-0;am[is.nan(am)]<-0;rownames(am)<-colnames(am)<-colnames(gbi)
  am}

makeTrans = function(..., alpha=0.5) {
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  alpha = floor(255*alpha)  
  newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
  .makeTrans = function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  newColor = apply(newColor, 2, .makeTrans, alpha=alpha)
  return(newColor)
}

range_use<-function(x,min_use,max_use){ (x - min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)) * (max_use - min_use) + min_use } 

true_rank<-function(a){(1:length(unique(a)))[match(a,sort(unique(a)))]}


make_circ<-function(am_lay,min_val=0,max_val=1){ #current layout, min val to scale to, max val to scale to
  #making circular
  ps<-nrow(am_lay)
  dim_tl<-ceiling(sqrt(ps))*2 #needs *2 just to make sure we have enough points
  #grid_lay<-expand.grid(floor(-dim_tl/2):ceiling(dim_tl/2),((floor(-dim_tl/2))-0.5):(ceiling((dim_tl/2))+0.5))
  xpoints1<-floor(-dim_tl/2):ceiling(dim_tl/2)
  xpoints2<-xpoints1+0.5
  ypoints1<-xpoints1
  ypoints2<-xpoints2
  grid_lay1<-expand.grid(xpoints2,ypoints1)
  grid_lay2<-expand.grid(xpoints1,ypoints2)
  grid_lay3<-expand.grid(xpoints1,ypoints1)
  grid_lay4<-expand.grid(xpoints2,ypoints2)
  grid_lay<-rbind(grid_lay1,grid_lay2,grid_lay3,grid_lay4)
  grid_lay[,1]<-jitter(grid_lay[,1],1)
  grid_lay[,2]<-jitter(grid_lay[,2],1)
  euc_dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
  grid_lay[,3]<-apply(grid_lay,1,function(a)euc_dist(matrix(c(mean(grid_lay[,1]),mean(grid_lay[,2])),1,2),a))
  max_dist<-grid_lay[,3][((1:length(unique(grid_lay[,3])))[match(grid_lay[,3],sort(unique(grid_lay[,3])))])==ps]
  g_lay_c<-grid_lay[grid_lay[,3]<=max_dist,]
  g_lay_c[,1]<-scale(g_lay_c[,1])[,1]
  g_lay_c[,2]<-scale(g_lay_c[,2])[,1]
  am_lay[,1]<-scale(am_lay[,1])[,1]
  am_lay[,2]<-scale(am_lay[,2])[,1]
  distances <- rdist(am_lay[,1:2],g_lay_c[,1:2]) 
  sol <- solve_LSAP(t(distances))
  am_lay[as.numeric(sol),1:2]<-as.matrix(g_lay_c[,1:2])
  
  #standardise am_lay into desired range
  am_lay<-apply(am_lay,2,function(a)range_use(a,min_val,max_val))
  am_lay
}####finished making circular





draw_focal_plot<-function(am,am_layout=NULL,nl,el,i,circlay=T,inc_attr=T,colour_edge_type=F,title_use=NULL,plot_all_points=F){#takes the association matrix, the dataframe information  and the layout (if wanting to specify a layout e.g. to keep the layout the same across comparisons)
  nl$degree<-colSums(am>0)
  nl$wdegree<-colSums(am)
  nl$focal_am<-colnames(am) %in% focal_node
  focal_assocs<-am[,focal_node]>0
  focal_assocs[focal_node]<-T
  focal_assocs<-colnames(am)[focal_assocs]
  # am_old<-am
  # am<-am[focal_assocs,focal_assocs]
  # nl_old<-nl
  # nl<-nl[colnames(am_old) %in% colnames(am),]
  nl$v<-nl$id<-1:nrow(am)
  
  if(i %in% c(1,2)) nl$status = nl$status_temp
  if(i == 3) nl$status = nl$status_rand
  
  # make the needed igraph object from the association matrix
  am_i<-graph_from_adjacency_matrix(am,"undirected",weighted=T,diag=F)
  am_i_lay<-am_i
  
  # now, if a layout isn't given in the function, generate the circular ones wanted here:
  if(is.null(am_layout)){
    am_lay<-layout_nicely(am_i_lay)
    
    # add focal node focus:
    am_focal<-am
    am_focal[nl$focal_am,]<-am_focal[nl$focal_am,]*1000
    am_focal[nl$focal_am,]<-am_focal[nl$focal_am,]*1000
    am_i_focal<-graph_from_adjacency_matrix(am_focal,"undirected",weighted=T,diag=F)
    am_i_lay_focal<-am_i_focal
    am_lay<-layout_nicely(am_i_lay_focal)
    
    # plot.igraph(am_i_lay)
    if(circlay==T){
      am_lay<-make_circ(am_lay,min_val=0,max_val=1)#making circular
    }####finished making circular
    
    range_scale<-c(0,2)
    am_lay<-apply(am_lay,2,function(a)range_use(a,min(range_scale),max(range_scale)))
    
  } # finished making am_lay if layout null
  
  
  if(!is.null(am_layout)){
    range_scale<-c(0,2)
    if(is.null(rownames(am_layout))){am_lay<-am_layout}
    if(!is.null(rownames(am_layout))){am_lay<-am_layout[rownames(am),]}
  }
  
  # finished providing layout
  
  
  el_am_i<-data.table(as_edgelist(am_i))
  
  edgew<-E(am_i)$weight
  # edgew<-range_use(edgew,0.8,4.2)
  
  edge_from<-row(am)[lower.tri(am)][am[lower.tri(am)]>0]
  edge_to<-col(am)[lower.tri(am)][am[lower.tri(am)]>0]
  edge_focal<-nl$focal_am[edge_from] | nl$focal_am[edge_to]
  edgew_notfocals<-edgew_focals<-edgew
  edgew_notfocals[edge_focal]<-0
  edgew_focals[!edge_focal]<-0
  
  
  # el_am_i[el, contact_type:=i.contact_type, on=c(V1='node_i', V2='node_j')]
  el_am_i[V1==focal_node & V2 %in% nl[status=='old']$node, status:='old']
  el_am_i[V1==focal_node & V2 %in% nl[status=='new']$node, status:='new']
  el_am_i[V1==focal_node & V2 %in% nl[status=='fix']$node, status:='fix']
  
  # make edge col by contact type
  # edge_cols<-rep('#7BB7A6', length(edgew)) # rep(makeTrans(mint_mal[nl$degree[nl$focal_am]],alpha=0.9),length(edgew)) #all coloured by Malins age
  # edge_cols[el_am_i$contact_type == 1]<-'#005D67'
  # edge_cols[!edge_focal]<-makeTrans("grey",alpha=0.2)
  
  # make edge col by node status
  edge_cols<-rep(makeTrans("grey",alpha=0.2), length(edgew))
  edge_cols[el_am_i$status %in% c('old', 'fix')] <- '#20639B'  
  edge_cols[el_am_i$status %in% c('new')] <- makeTrans('#ED553B',alpha=0.8) 
  
  edge_curve<-ifelse(edge_focal,T,F)
  
  # make size of nodes
  ndeg<-nl$wdegree
  vert_sizes<-range_use(rank(ndeg),1,5)
  vert_sizes<-vert_sizes+0
  
  # make vert colours
  vert_bg<-vert_outline<-rep(makeTrans("grey",alpha=0.2),ncol(am))
  vert_shapes<-rep("circle",ncol(am))
  vert_labels<-rep(NA,ncol(am))
  vert_labels_cols<-"black";vert_labels_sizes<-range_use(vert_sizes,0.5,1)
  
  
  # including node attributes eg presence or absence of contact with focal node:
  vert_outline[colnames(am) %in% focal_node]<-'black'
    if(inc_attr==T){
      # vert_bg<-vert_outline<- makeTrans(mint_mal[nl$deg],alpha=0.9)
      vert_bg[which(nl$status=='focal')]<- '#B09C85'
      if(i==1) {
        vert_bg[which(nl$status=='old')]<-makeTrans('#20639B',alpha=0.4)
        vert_bg[which(nl$status=='fix')]<-makeTrans('#20639B',alpha=0.4)
      }
      if(i %in% c(2,3)) {
        vert_bg[which(nl$status=='fix')]<-makeTrans('#20639B',alpha=0.4)
        vert_bg[which(nl$status=='new')]<- makeTrans('#ED553B',alpha=0.4)
      }  
        
      vert_outline[colnames(am) %in% focal_node]<-'black'
      if(i==1){
        vert_outline[colnames(am) %in% nl[status %in% c('old','fix')]$node]<-makeTrans("black",alpha=0.9)
        vert_outline[!colnames(am) %in% nl[status %in% c('old','fix','focal')]$node]<-makeTrans("grey",alpha=0.2)
      }
      if(i %in% c(2,3)){
        vert_outline[colnames(am) %in% nl[status %in% c('old','fix','new')]$node]<-makeTrans("black",alpha=0.9)
        vert_outline[colnames(am) %in% nl[status %in% c('non_focal')]$node]<-makeTrans("grey",alpha=0.2)
      }
       # vert_outline[!colnames(am) %in% focal_node]<-makeTrans("black",alpha=0.9)
        
        #vert_labels[colnames(am) %in% focal_node]<-"M"
    }
  
  # if(colour_edge_type==T){
  #   #round((nl$Age[edge_from]+nl$Age[edge_to])/2) #for Mean colour based on average age
  #   #Colour based on Malins Associates Age
  #   Ages_from<-nl$Age[edge_from]
  #   Ages_to<-nl$Age[edge_to]
  #   Ages_from[nl$focal_am[edge_from]]<-Ages_to[nl$focal_am[edge_from]]
  #   edge_cols[edge_focal]<-makeTrans(mint_mal[Ages_from],alpha=0.9)[edge_focal]
  # }
  
  #dev.new(width=9,height=9)
  # Plotting
  # all points 
  if(plot_all_points==T){
    plot(am_layout[!rownames(am_layout) %in% rownames(am),],type="p",xlim=range_scale,ylim=range_scale,pch=21,col="black",cex=0.5,bty="n",xaxt="n",yaxt="n",xlab="",ylab="",main=title_use, cex.main=1.5)
  }
  # all bonds
  plot.igraph(am_i, layout=am_lay,rescale=F,edge.width=edgew_notfocals,edge.curved=edge_curve,edge.color=edge_cols,vertex.label=vert_labels,vertex.label.color=vert_labels_cols,vertex.label.cex=vert_labels_sizes,vertex.size=vert_sizes,vertex.color=vert_bg,vertex.frame.color=vert_outline,vertex.shape=vert_shapes,xlim=range_scale,ylim=range_scale,add=plot_all_points)
  # focal bonds
  plot.igraph(am_i, layout=am_lay,rescale=F,edge.width=edgew_focals,edge.curved=T,edge.color=edge_cols,vertex.label=vert_labels,vertex.label.color=vert_labels_cols,vertex.label.cex=vert_labels_sizes,vertex.size=vert_sizes,vertex.color=vert_bg,vertex.frame.color=vert_outline,vertex.shape=vert_shapes,xlim=range_scale,ylim=range_scale,add=T)
  
  
  # Malin text:
  # text(x=am_lay[rownames(am) %in% focal_node,1],y=am_lay[rownames(am) %in% focal_node,2],labels="M",col="red",cex=1.5)
  
  
  
  # if you wanted to generate a layout and then use it next time, this returns it
  if(is.null(am_layout)){
    rownames(am_lay)<-rownames(am)
    return(am_lay)}
}# ends draw_focal_plot function



# draw.network<-function(edgelist,nodelist,state=c('start','transit','end'),focal_node,timestep,main_label=NULL){
#   
#   range_use<-function(x,min_use,max_use){ (x - min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)) * (max_use - min_use) + min_use } #define for later
#   
#   edgelist_t0 = edgelist[step==timestep[1], c('node_i', 'node_j', 'contact_type')]
#   edgelist_t1 = edgelist[step==timestep[2], c('node_i', 'node_j', 'contact_type')]
#   nodelist[node==focal_node, focal_am:=TRUE]
#   nodelist[node!=focal_node, focal_am:=FALSE]
#   
#   # create igraph object from edgelist
#   graph_t0 = graph_from_data_frame(edgelist_t0, directed = TRUE, vertices = nodelist)
#   graph_t1 = graph_from_data_frame(edgelist_t1, directed = TRUE, vertices = nodelist)
#   
#   graph_t0 = as.undirected(graph_t0, mode ='collapse', edge.attr.comb="first")
#   graph_t1 = as.undirected(graph_t1, mode ='collapse', edge.attr.comb="first")
#   
#   # set layout of graph
#   graph_lay = layout_nicely(graph_t0)
#   
#   # make circular
#   ps = nrow(graph_lay)
#   dim_tl = ceiling(sqrt(ps))*2 #needs *2 just to make sure we have enough points
#   #grid_lay = expand.grid(floor(-dim_tl/2):ceiling(dim_tl/2),((floor(-dim_tl/2))-0.5):(ceiling((dim_tl/2))+0.5))
#   xpoints1 = floor(-dim_tl/2):ceiling(dim_tl/2)
#   xpoints2 = xpoints1+0.5
#   ypoints1 = xpoints1
#   ypoints2 = xpoints2
#   grid_lay1 = expand.grid(xpoints2,ypoints1)
#   grid_lay2 = expand.grid(xpoints1,ypoints2)
#   grid_lay3 = expand.grid(xpoints1,ypoints1)
#   grid_lay4 = expand.grid(xpoints2,ypoints2)
#   grid_lay = rbind(grid_lay1,grid_lay2,grid_lay3,grid_lay4)
#   grid_lay[,1] = jitter(grid_lay[,1],1)
#   grid_lay[,2] = jitter(grid_lay[,2],1)
#   euc_dist = function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
#   grid_lay[,3] = apply(grid_lay,1,function(a)euc_dist(matrix(c(mean(grid_lay[,1]),mean(grid_lay[,2])),1,2),a))
#   max_dist = grid_lay[,3][((1:length(unique(grid_lay[,3])))[match(grid_lay[,3],sort(unique(grid_lay[,3])))])==ps]
#   g_lay_c = grid_lay[grid_lay[,3]<=max_dist,]
#   g_lay_c[,1] = scale(g_lay_c[,1])[,1]
#   g_lay_c[,2] = scale(g_lay_c[,2])[,1]
#   graph_lay[,1] = scale(graph_lay[,1])[,1]
#   graph_lay[,2] = scale(graph_lay[,2])[,1]
#   distances = cdist(graph_lay[,1:2],g_lay_c[,1:2])
#   sol = solve_LSAP(t(distances))
#   graph_lay[as.numeric(sol),1:2] = as.matrix(g_lay_c[,1:2])
#   
#   #standardise graph.lay into desired range
#   range_scale<-c(0,2)
#   graph_lay<-apply(graph_lay,2,function(a)range_use(a,min(range_scale),max(range_scale)))
#   
#   
#   #Plotting information from here
#   makeTrans<-function(..., alpha=0.5) {
#     if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
#     alpha = floor(255*alpha)
#     newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
#     .makeTrans = function(col, alpha) {
#       rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)}
#     newColor = apply(newColor, 2, .makeTrans, alpha=alpha)
#     return(newColor)}
#   
#   mint_focal = rev(sequential_hcl(length(5)+5,palette="Mint")) 
# 
#   
#   #edge info
#   edgew = rep(1, edgelist_t0[,.N])
#   
#   am = as_adjacency_matrix(graph_t0)
#   edge_from<-row(am)[lower.tri(am)][am[lower.tri(am)]>0]
#   edge_to<-col(am)[lower.tri(am)][am[lower.tri(am)]>0]
#   
#   edge_focal = nodelist$focal_am[edge_from] | nodelist$focal_am[edge_to] 
#   edgew_notfocals = edgew_focals = edgew
#   edgew_notfocals[edge_focal] = 0
#   edgew_focals[!edge_focal] = 0
#   
#   
#   edge_cols<-rep(makeTrans(mint_focal[nodelist$deg[nodelist$focal_am]],alpha=0.9),length(edgew)) #all coloured by Malins age
#   edge_cols[!edge_focal]<-makeTrans("grey",alpha=0.2)
#   edge_curve<-ifelse(edge_focal,T,F)
#   
#   # make size of nodes
#   ndeg = edgelist_t0[,.N, by=.(node_i)]
#   nodelist[ndeg, deg:=i.N, on=c(node='node_i')]
#   nodelist[is.na(deg), deg:=0]
#   vert_sizes<-range_use(rank(nodelist$deg),2.5,6.5)
#   vert_sizes<-vert_sizes+0
#   
#   # make vert colours
#   vert_bg = vert_outline = rep('grey',ncol(am))
#   vert_shapes<-rep("circle",ncol(am))
#   vert_labels<-rep(NA,ncol(am))
#   vert_labels_cols<-"black"; vert_labels_sizes = range_use(vert_sizes,0.5,1)
#   
#   
#   # including node attributes eg deg:
#   vert_outline[colnames(am) %in% focal_node]<-"red"
#     # if(inc.attr==T){
#       vert_bg = vert_outline = makeTrans(mint_focal[nodelist$deg],alpha=0.9)
#       vert_outline[colnames(am) %in% focal_node] = "red"
#       vert_outline[!colnames(am) %in% focal_node] = makeTrans("black",alpha=0.9)
#       #vert_labels[colnames(am) %in% focal_node] = "F"
#     # }
#   
#   
#   #Plotting
#   #all points 
#   plot(graph_lay[!rownames(graph_lay) %in% rownames(am),],type="p",xlim=range_scale,ylim=range_scale,pch=21,col="black",cex=0.5,bty="n",xaxt="n",yaxt="n",xlab="",ylab="",main=main_label)
#   
#   #all bonds
#   plot.igraph(graph_t0, layout=graph_lay,rescale=F,edge.width=edgew_notfocals,edge_curved=edge_curve,edge.color=edge_cols,vertex.label=vert_labels,vertex.label.color=vert_labels_cols,vertex.label.cex=vert_labels_sizes,vertex.size=vert_sizes,vertex.color=vert_bg,vertex.frame.color=vert_outline,vertex.shape=vert_shapes,xlim=range_scale,ylim=range_scale,add=T)
#   #Malin bonds
#   plot.igraph(graph_t0, layout=graph_lay,rescale=F,edge.width=edgew_focals,edge_curved=T,edge.color=edge_cols,vertex.label=vert_labels,vertex.label.color=vert_labels_cols,vertex.label.cex=vert_labels_sizes,vertex.size=vert_sizes,vertex.color=vert_bg,vertex.frame.color=vert_outline,vertex.shape=vert_shapes,xlim=range_scale,ylim=range_scale,add=T)
#   
#   title(main=main_label, cex.main=2, adj=0, font.main=1, line=-2.5)
# }
