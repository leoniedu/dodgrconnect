# ## create additional vertices to break long edges
# library(dplyr)
# library(dodgr)
# library(sf)
# 
# graph <- dodgr::weight_streetnet(hampi)
# tmp <- dodgr_insert_vertex(graph = graph, v1 = tail(graph$to_id,1), v2 = tail(graph$from_id,1))
# 
# hist(graph$d)
## keep unique interchangeable to,from ids to create vertices 
## FIX: need this?
##ids <- tibble(from_id=pmin(graph$to_id,graph$from_id), to_id=pmax(graph$to_id,graph$from_id))
           
break_long_edges <- function(graph, max_d=NULL) {            
  graph_newv <- graph
  if (is.null(max_d)) max_d <- median(graph$d)
  graph_large_d <- graph[graph$d>max_d,]
  j <- 1
  while((nrow(graph_large_d)>0)  && (max(graph_large_d$d)>max_0)) {
    max_0 <- max(graph_large_d$d)
    dim(graph_newv)
    graph_newv <- dodgr_insert_vertex(graph = graph_newv, v1 = graph_large_d$to_id[1], v2 = graph_large_d$from_id[1])
    graph_newv <- graph_newv[is.finite(graph_newv$d),]
    graph_large_d <- graph_newv[graph_newv$d>max_d,]
    dim(graph_newv)
    if((j%%100)==0) cli::cli_inform(glue::glue("{nrow(graph_large_d)} to go!"))
    j <- j+1
  }
  cli::cli_inform(glue::glue("{nrow(graph_newv)-nrow(graph)} edges created"))
  v <- dodgr::dodgr_vertices(graph)
  vnew <- dodgr::dodgr_vertices(graph_newv)
  cli::cli_inform(glue::glue("{nrow(vnew)-nrow(v)} vertices created"))
  graph_newv
}

# dim(graph)
# graph_new <- break_long_edges(graph)

