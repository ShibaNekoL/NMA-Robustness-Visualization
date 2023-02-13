comparisonStreams = function(hatmatrix, comparison){
  
  library(rlist)
  library(igraph)
  library(dplyr)
  
  directs <- hatmatrix$colNames
  
  hatMatrix <- hatmatrix$H
  
  rownames(hatMatrix) <- hatmatrix$rowNames
  
  #### bug：what if there is ":" in the treatments' names
  split <- function (dir) {strsplit(dir,":")}
  
  dims <- dim(hatMatrix)  
  
  #rows of comparison matrix 
  comparisons <- unlist(lapply(rownames(hatMatrix),unlist))

  comparisonToEdge <- function (comp) unlist (split(comp))
  
  directlist <- unlist(lapply(lapply(directs,split),unlist))
  
  # print(c("dir",directs))
  
  edgeList <- matrix( directlist, nc = 2, byrow = TRUE)
  # print(c("Edgelist"))
  # print(edgeList)
  
  # ###################################################
  # ### reorder comparison
  # 
  # if(paste0(strsplit(comparison,":")[[1]][2], ":", strsplit(comparison,":")[[1]][1]) %in% comparisons){
  #   comparison <- paste0(strsplit(comparison,":")[[1]][2], ":", strsplit(comparison,":")[[1]][1])
  # }
  # 
  # ###################################################
  
  g <- graph_from_edgelist(edgeList , directed=FALSE)
  g <- set.vertex.attribute(g,'label',value = V(g))
  g <- set.edge.attribute(g,'label',value = 1:dim(edgeList)[1])
  # edge_attr(g) <- list(label = E(g))
  # g <- set.edge.attribute(g,'label',value = E(g))
  #print(V(g)$label)
  #print(V(g)$name)
  #print(E(g))
  
  setWeights <- function (g,comparison,conMat) {
    set.edge.attribute(g,"weight",value=rep(0,dims[2]))
  }
  
  
  getFlow <- function(g,edge) {return(E(g)[edge]$flow)}
  
  sv <- function (comparison) {split(comparison)[[1]][1][1]}
  
  tv <- function (comparison) {split(comparison)[[1]][2][1]}
  
  initRowGraph <- function(comparison) {
    dedgeList <- lapply(1:length(directs),function(comp) {
      if(hatMatrix[comparison,comp]>0){
        # print(c("not switched",directs[comp],hatMatrix[comparison,comp]))
        return (c(sv(directs[comp]),tv(directs[comp])))
      }else{
        # print(c("switched",directs[comp],hatMatrix[comparison,comp]))
        return (c(tv(directs[comp]),sv(directs[comp])))
      }
    })
    dedgeList <- matrix( unlist(dedgeList), nc = 2, byrow = TRUE)
    # gg <- setFlow(g,comparison)
    # E(gg)$weight <- rep(0,dims[2])
    # return(gg)
    flows<-abs(hatMatrix[comparison,])
    dg <- graph_from_edgelist(dedgeList , directed = TRUE)
    E(dg)[]$weight <- rep(0,dims[2])
    E(dg)[]$flow <- abs(hatMatrix[comparison,])
    V(dg)[]$label <- V(dg)[]$name
    # E(dg)[]$label <- E(dg)[]$flow
    dg <- set.edge.attribute(dg,'label',value = 1:dim(dedgeList)[1])
    # dg <- set.edge.attribute(dg,'label',value = E(dg))
    # print(c("isdirected",is.directed(dg)))
    return(dg)
  }
  
  contribution = rep(0,dims[2])
  streams = list()
  names(contribution) <- c(1:dims[2])
  
  # 減掉flow
  reducePath <- function (g,comparison,spl) {
    pl <- length(spl[[1]])
    splE <- lapply(spl[[1]], function(e){
      return (E(g)[e[]])
    })
    flow <- min(unlist(lapply(splE, function(e){
      return(e$flow[])
    })))
    
    ###############################################1  
    # debug: spl -> spl[[1]]
    # debug: c(tail_of(g,e),head_of(g,e)) -> c(tail_of(g,e),head_of(g,e))
    #pathname = paste(names(unlist(lapply(spl[[1]],function(e){c(tail_of(g,e),head_of(g,e))}))), collapse = ":")
    path = names(unlist(lapply(spl[[1]],function(e){c(tail_of(g,e),head_of(g,e))})))
    
    ### my code
    # 取出一個path的各邊，不過這邊用|分隔
    #edges <- attr(spl[[1]], "vnames")
    # 直接把各邊head和tail拆開
    edge1 <- path[c(TRUE, FALSE)]
    edge2 <- path[c(FALSE, TRUE)]
    ###############################################1
    
    #print(c(head_of(g,spl),tail_of(g,spl)))
    
    ###############################################2
    substreams <- list(path=path,
                       stream = data.frame(flow=flow,length=floor(length(splE)),flowperedge=flow/floor(length(splE))),
                       edge = data.frame(head=edge1, tail=edge2)
                                         # comparison = paste0(t(apply(data.frame(head=edge1, tail=edge2), 1, sort))[,1],
                                         #                    ":",
                                         #                    t(apply(data.frame(head=edge1, tail=edge2), 1, sort))[,2]
                                         #                    )
                                         # )
                       )
    streams <<- list.append(streams, substreams)
    ###############################################2
    
    # print(c("to shortest path einai :",spl))
    gg <- Reduce(function(g, e){
      elabel <- e$label
      # print(c("pame plevra:",e,"dld",e$label))
      pfl <- e$flow[]
      g <- set.edge.attribute(g,"flow",e, pfl-flow)
      # print(c("h e",e,"einai pragmatika h ",elabel))
      cw <-  e$weight[] + (flow[1]/pl) 
      # print(c("flow",flow,"eweight",e$weight[]))
      contribution[elabel] <<- cw
      return(set.edge.attribute(g,"weight",e, cw))
    },splE, g)
    # print(c("graph before deleting edges", E(gg)$label))
    emptyEdges <- Reduce(function(removedEdges, e){
      e <- E(gg)[e[]]
      if(e$flow[[1]][[1]]==0){
        removedEdges <- c(removedEdges, e)
      }
      return(removedEdges)
    },splE, c())
    # print(c("edges to be removed",emptyEdges))
    return(delete_edges(gg, emptyEdges))
    # print(c("graph after deleting edges", E(gg)$label))
  }
  
  # 圖g上減掉邊
  reduceGraph <- function (g,comparison) {
    # 在圖g上找到從comparison的頭到尾sv到tv的最短邊
    # 我這邊把compariston typo改回comparison
    getshortest <- function (g,comparison) {
      floweights = lapply(edge_attr(g,"flow",E(g)), function(f){return(abs(2-f))})
      spths = suppressWarnings(
        get.shortest.paths(g,sv(comparison),tv(comparison),mode="out",output="epath",weights=floweights)
      )
      return(spths$epath)
    }
    # while(edge_connectivity(g,sv(comparison),tv(comparison))>0){
    spath <- getshortest(g,comparison)
    
    # 如果找到的最短邊還是大於0，在圖g上減掉，繼續下一輪迭代
    while(length(unlist(spath))>0){
      g <- reducePath(g,comparison,spath)
      spath <- getshortest(g,comparison)
    }
    # print("teleiwse")
    return(g)

  }
  
  # ptm <- proc.time()
  # gg <- reduceGraph (initRowGraph(comparison), comparison)
  reduceGraph (initRowGraph(comparison), comparison)
  # executionTime <- proc.time() - ptm
  # print(c("execution time",executionTime))
  
  names(contribution) <- directs
  contribution <- 100 * contribution
  ############################################3
  return(list(comp=comparison,
              streams=streams,
              contribution=contribution
              )
        )
  ############################################3
}
