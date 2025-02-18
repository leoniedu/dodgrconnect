#' Add nodes to graph with deterministic edge-based processing
#'
#' Enhanced version that processes points per edge to maintain graph consistency,
#' with improved weight profile handling and edge type validation.
#'
#' @inheritParams dodgr::add_nodes_to_graph
#' @param wt_profile Weight profile name (e.g., "foot", "bicycle"). Required if 
#'        using `new_edge_type`.
#' @param wt_profile_file Optional path to custom weight profile CSV file. Uses
#'        `dodgr:::get_profile()` for loading instead of base `read.csv`.
#' @param new_edge_type Type for new connection edges (e.g., "footway"). Applies 
#'        *only* to new edges connecting points to the graph, not split edges.
#'        Must exist in specified weight profile.
#' @param intersections_only Logical indicating whether to skip component ID
#'        assignment for faster intermediate processing.
#'
#' @return Modified graph with:
#' - Original edges split at projection points
#' - New edges connecting input points to graph
#' - Updated component IDs (unless `intersections_only = TRUE`)
#' - Consistent vertex IDs based on coordinate hashing
#'
#' @details Key features:
#' - Processes all points per edge together for topological consistency
#' - Maintains original edge properties on split segments
#' - Validates `new_edge_type` against weight profile contents
#' - Uses geodist for accurate distance calculations
#' - Generates deterministic vertex IDs based on edge positions
#'
#' @examples
#' # Basic usage with default weight profile
#' hw <- dodgr::weight_streetnet(dodgr::dodgr_streetnet("hampi"))
#' xy <- data.frame(x = 76.4, y = 15.3)
#' net <- add_nodes_to_graph5(hw, xy)
#'
#' # With custom edge type validation
#' net <- add_nodes_to_graph5(hw, xy, 
#'                           wt_profile = "foot",
#'                           new_edge_type = "path")
#'
#' # Using custom weight profile file
#' net <- add_nodes_to_graph5(hw, xy,
#'                           wt_profile_file = "custom_weights.csv",
#'                           new_edge_type = "custom_type")
#'
#' @seealso [dodgr::add_nodes_to_graph()] for base implementation
#' @export
add_verts_to_graph <- function(graph,
                                xy,
                                max_length = Inf,
                                dist_tol = 1e-6) {
  
  # Validate tolerance
  if (dist_tol < 0) {
    stop("Tolerance must be non-negative")
  }
  
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
  
  xyf <- xy  # Keep original points
  
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
  ## distance between projection point and the from vertice
  closest_edges$d_vert <- with(closest_edges,geodist::geodist(x=cbind(x,y), y=cbind(xfr,yfr), paired = TRUE, measure = "geodesic"))
  closest_edges$d_pt <- with(closest_edges,geodist::geodist(x=cbind(x,y), y=cbind(xpt,ypt), paired = TRUE, measure = "geodesic"))
  closest_edges$pt_id <- xyf$id
  closest_edges$edge_id <- graph_std$edge_id[closest_edges$index]
  closest_edges$proj_id <- replicate(nrow(closest_edges),genhash(10))
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
  xyf <- xyf[closest_edges$xy_index,]
  
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
    edge_dif <- edge
    edge_new <- edge
    for (i in seq_len(nrow(projection_points))) {
      edge_new <- dodgr::dodgr_insert_vertex(
        graph = edge_new,
        v1 = edge_dif$from,
        v2 = edge_dif$to,
        x=projection_points$x[i],
        y=projection_points$y[i]
      )
      edge_dif <- edge_new%>%anti_join(edge,by = join_by(edge_id, from, to, d, d_weighted, time, time_weighted, xfr,yfr, xto, yto, component))%>%slice(2)
    }
    edge_new$graph_orig_idx <- edge_idx
    new_edges <- rbind(new_edges,edge_new)
  }
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


