#' Add vertices to graph by splitting edges
#'
#' Projects points onto the nearest edges in the graph and splits those edges at
#' the projection points. This maintains the network topology by creating new
#' vertices where points project onto existing edges. Each edge is split at the
#' projection point and new edges are created to maintain connectivity.
#'
#' @inheritParams dodgr::add_nodes_to_graph
#' @param graph A dodgr graph to modify
#' @param xy Matrix or data.frame of x-y coordinates of points to add. If points
#'        have an 'id' column, these IDs will be preserved, otherwise new IDs
#'        will be generated.
#' @param bidirectional Logical; if TRUE, creates bidirectional edges for each
#'        connection (default: FALSE)
#'
#' @return Modified graph with:
#'   - New vertices at projection points
#'   - Original edges split at projection points
#'   - All edge attributes preserved and properly weighted
#'   - Bidirectional edges if requested
#'
#' @details
#' This function:
#' 1. Projects each point onto its nearest edge using match_pts_to_graph
#' 2. Creates new vertices at projection points
#' 3. Splits existing edges at these points
#' 4. Maintains all original edge attributes and weights
#' 5. Preserves existing point IDs or generates new ones if not provided
#' 6. Creates bidirectional edges if requested
#'
#' @examples
#' # Create sample network
#' net <- weight_streetnet(dodgr_streetnet("hampi india"))
#'
#' # Add points by splitting edges (unidirectional)
#' pts <- data.frame(x = c(76.4, 76.5), y = c(15.3, 15.4))
#' net_split <- add_verts_to_graph(net, pts, bidirectional = FALSE)
#'
#' # Add points with bidirectional edges
#' net_split_bidir <- add_verts_to_graph(net, pts, bidirectional = TRUE)
#'
#' @seealso
#' [add_edges_to_graph()] for creating direct edges to vertices
#' @export
add_verts_to_graph <- function(graph,
                                xy,
                                bidirectional = TRUE) {
  
  stopifnot(nrow(xy)>0)
  
  # Standardize column names at the start
  gr_cols <- dodgr_graph_cols(graph)
  gr_cols <- unlist(gr_cols[which(!is.na(gr_cols))])
  graph_std <- graph[, gr_cols]  # standardise column names
  names(graph_std) <- names(gr_cols)
  graph_std$edge_id <- as.character(graph_std$edge_id)
  # Save original column mapping
  col_mapping <- setNames(gr_cols, names(gr_cols))
  
  # Process xy data
  if ("id" %in% names(xy)) {
    xy_id <- xy$id
    xy$id <- NULL
  } else {
    xy_id <- NULL
  }
  xy <- dodgr:::pre_process_xy(xy)
  
  # Add id column if not present
  if (is.null(xy_id)) {
    xy_id <- vapply(seq_len(nrow(xy)), 
                    function(i) paste0(sample(c(letters, LETTERS, 0:9), 10, replace = TRUE), collapse = ""),
                    character(1))
  }
  xy$id <- xy_id
  # Match points to edges
  closest_edges <- match_pts_to_graph(graph_std, xy[, c("x", "y")], distances = TRUE)
  # closest_edges <- closest_edges|>
  #   dplyr::group_by(index)|>
  #   mutate(edge_index=order(-abs(d_signed)), index=if_else(edge_index>1, NA, index))
  closest_edges$xy_index <- seq_len(nrow(closest_edges))
  closest_edges$xfr <- graph_std$xfr[closest_edges$index]
  closest_edges$yfr <- graph_std$yfr[closest_edges$index]
  closest_edges$xpt <- xy$x
  closest_edges$ypt <- xy$y
  # ## distance between projection point and the from vertice
  closest_edges$d_vert <- with(closest_edges,geodist::geodist(x=cbind(x,y), y=cbind(xfr,yfr), paired = TRUE, measure = "geodesic"))
  closest_edges$edge_id <- graph_std$edge_id[closest_edges$index]
  closest_edges <- closest_edges|>dplyr::arrange(index,d_vert)
  
  # Check for unconnected points
  not_connected <- sum(is.na(closest_edges$index))
  if (not_connected > 0) {
    cli::cli_alert_warning("{not_connected} points not connected ")
  }
  
  if (nrow(closest_edges) == 0) {
    return(graph)
  }
  
  # Update xy to only include connected points
  xy <- xy[closest_edges$xy_index,]
  
  # Initialize new edges dataframe
  new_edges <- data.frame()
  ## process by edge
  for (edge_idx in unique(closest_edges$index)) {
    if (is.na(edge_idx)) next
    # Get the edge to split
    edge <- graph_std[edge_idx,]
    if (edge$d==0) next
    closest_edge <- closest_edges|>dplyr::filter(index==edge_idx)
    projection_points <- closest_edge|>distinct(x,y,d_vert, .keep_all = TRUE)
    
    # Split edges at projection points
    edge_dif <- edge
    edge_new <- edge
    for (i in seq_len(nrow(projection_points))) {
      point_id <- xy$id[projection_points$xy_index[i]]
      point_x <- projection_points$x[i]
      point_y <- projection_points$y[i]
      d_i <- projection_points$d[i]
      
      # Split the edge at projection point
      edge_new <- dodgr::dodgr_insert_vertex(
        graph = edge_new,
        v1 = edge_dif$from,
        v2 = edge_dif$to,
        x = point_x,
        y = point_y
      )
      
      # Update edge_dif for next iteration - get the remaining part of the split edge
      edge_dif <- edge_new|>
        anti_join(edge, by = join_by(from, to))|>
        slice(2)
    }
    edge_new$graph_orig_idx <- edge_idx
    new_edges <- rbind(new_edges, edge_new)
  }
  if (bidirectional) {
    new_edges_rev <- new_edges%>%dplyr::rename(from=to, to=from, xfr=xto, xto=xfr, yfr=yto, yto=yfr)%>%dplyr::mutate(edge_id=paste0(edge_id, "_rev"))
    new_edges <- rbind(new_edges,new_edges_rev)
  }
  # Update graph with new edges
  graph_std$graph_orig_idx <- 1:nrow(graph_std)
  graph_std_new <- rbind(graph_std[-closest_edges$index,],
                       new_edges)
  result_orig <- graph[-unique(closest_edges$index),]
  result_new <- graph[new_edges$graph_orig_idx,]
  # Update only the standardized columns
  for (g in seq_along(gr_cols)) {
    col_std <- names(gr_cols)[g]
    orig_col <- col_mapping[g]
    if (col_std %in% names(new_edges) && nrow(new_edges) > 0) {
      result_new[, orig_col] <- new_edges[, col_std]
    }
  }
  if (is.character(result_new$edge_id)) {
    result_orig$edge_id <- as.character(result_orig$edge_id)
  }
  result_final <- dplyr::bind_rows(result_orig, result_new)
  
  # Return the final result
  return(result_final)
}

genhash <- function (len = 10) {
  paste0(sample(c(0:9, letters, LETTERS), size = len), collapse = "")
}
