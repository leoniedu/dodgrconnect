
  
plot_graph <- function(graph, path=NULL, max_n_points=100) {
  require(ggplot2)
  require(ggrepel)
  v <- dodgr_vertices(graph)
  pts <- graph%>%
    transmute(x=(from_lon+to_lon)/2, y=(from_lat+to_lat)/2, id=edge_id, type="edge")%>%
    group_by(x, y,type)%>%
    summarise(id=stringr::str_flatten_comma(id))%>%
    ungroup()%>%
    bind_rows(v%>%transmute(x,y,id,type="vertice"))
  p <- ggplot(graph) +
    geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat, color=factor(component)), alpha=1)
  if (nrow(pts)<max_n_points) {
    p <- p +
    geom_label_repel(aes(x=x, y=y, label=id, color=type), data=pts)
  }
  path <- unlist(path)
  if (length(path)>0) {
    stopifnot(is.integer(path))
    gpath <- graph[unlist(path),]
    p <- p +
      geom_segment(aes(x = from_lon, y=from_lat, xend = to_lon, yend = to_lat), color="red", alpha=1/2, data=gpath, linewidth=3)
  }
  p
}

std_graph <- function(graph) {
  graph%>%
    ungroup%>%
    select(all_of(c("geom_num", "edge_id", "from_id", "from_lon", "from_lat", "to_id", 
                    "to_lon", "to_lat", "d", "d_weighted", "highway", "component", 
                    "time", "time_weighted")))%>%
    group_by(from_id, to_id)%>%
    arrange(d_weighted)%>%slice(1)%>%
    ungroup%>%
    mutate(edge_id=as.character(1:n()))
}
