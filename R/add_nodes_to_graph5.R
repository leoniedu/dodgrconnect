#' Insert new nodes into a graph with custom edge weights
#'
#' This function extends `dodgr::add_nodes_to_graph` by adding the ability to specify
#' custom weights for new edges. While the original function inherits weights from
#' parent edges, this version allows customization through weight profiles.
#'
#' This inserts new nodes by extending lines from each input point to the edge
#' with the closest point of perpendicular intersection. That edge is then split
#' at that point of intersection, creating two new edges (or four for directed
#' edges). If `intersections_only = FALSE` (default), then additional edges are
#' inserted from those intersection points to the input points. If
#' `intersections_only = TRUE`, then nodes are added by splitting graph edges at
#' points of nearest perpendicular intersection, without adding additional edges
#' out to the actual input points.
#'
#' In the former case, the properties of those new edges, such as distance and
#' time weightings, are inherited from the edges which are intersected, and may
#' need to be manually modified after calling this function.
#'
#' @section Key Differences from dodgr::add_nodes_to_graph:
#' While the original function inherits all weights from parent edges, this version:
#' - Allows specifying custom highway types for new connecting edges
#' - Supports weight profile calculations for the new edges
#' - Enables custom surface types
#' - Falls back to inherited weights if no custom weights are specified
#' - Optionally accepts vertex IDs in the xy data frame
#'
#' @param graph A `data.frame` or `dodgr_streetnet` containing the graph edges
#' @param xy A `data.frame` or `matrix` of points to add. If a matrix, must have 2 columns
#'        for lon and lat coordinates. If a data.frame, must include 'lon' and 'lat' columns.
#'        Optionally can include 'id' column to specify vertex IDs.
#' @param wt_profile Name of weight profile to use
#' @param wt_profile_file Custom weight profile file
#' @param new_edge_type Type of new edges to create (e.g. "footway")
#' @param max_length Maximum length of new edges
#' @param dist_tol Tolerance for distance check (default: 1e-6)
#' @return A modified version of `graph`, with additional edges formed by
#'         breaking previous edges at nearest perpendicular intersections with the
#'         points, `xy`. When new_edge_type is specified, connecting edges use the
#'         specified weights and profiles.
#'
#' @examples
#' library(dodgr)
#' graph <- weight_streetnet(hampi, wt_profile = "foot")
#' dim(graph)
#'
#' verts <- dodgr_vertices(graph)
#' set.seed(2)
#' npts <- 10
#' xy <- data.frame(
#'     lon = min(verts$lon) + runif(npts) * diff(range(verts$lon)),
#'     lat = min(verts$lat) + runif(npts) * diff(range(verts$lat)),
#'     id = paste0("new_", 1:npts)  # Optional vertex IDs
#' )
#'
#' # Add points with inherited weights (like dodgr::add_nodes_to_graph)
#' graph1 <- add_nodes_to_graph3(graph, xy)
#'
#' # Add points with custom footpath weights
#' graph2 <- add_nodes_to_graph3(graph, xy,
#'                              new_edge_type = "footway",
#'                              wt_profile = "foot",
#'                              surface = "paved")
#' @export
add_nodes_to_graph5 <- function(graph,
                                xy,
                                wt_profile = NULL,
                                wt_profile_file = NULL,
                                new_edge_type = NULL,
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
    closest_edge <- closest_edges|>dplyr::filter(index==edge_idx)
    xy_edge <- xy[closest_edge$xy_index,]
    xyf_edge <- xyf[closest_edge$xy_index,]
    graph_edge <- graph_std[edge_idx,]
    projection_points <- closest_edge|>distinct(x,y,d_vert)
    ## we will first split the original edge along the unique projection points
    edge_split <- graph_edge[rep(1, nrow(projection_points)+1),]  # Create nrow(xy)+1 edges for split
    ## do the first split
    i=1
    edge_split$from[i] <- edge$from  # from start of the edge
    edge_split$xto[i] <- projection_points$x[i]
    edge_split$yto[i] <- projection_points$y[i]
    edge_split$d[i] <- projection_points$d_vert[i]
    if (edge_split$d[i]<0) edge_split$d[i] <- 0
    frac_d <- edge_split$d[i]/edge$d
    if (edge$d==0) frac_d <- 0
    edge_split$d_weighted[i] <- edge_split$d_weighted[i]*frac_d
    edge_split$time[i] <- edge$time*frac_d
    edge_split$time_weighted[i] <- edge$time_weighted[i]*frac_d
    edge_split$to[i] <- closest_edge$proj_id[i]
    
    ## do the middle splits
    if (nrow(projection_points)>2) {
      for (i in 2:(nrow(projection_points)-1)) {
        edge_split$from[i] <- edge_split$to[i-1]  # from start
        edge_split$xfr[i] <- edge_split$xto[i-1]
        edge_split$yfr[i] <- edge_split$yto[i-1]
        edge_split$xto[i] <- projection_points$x[i]
        edge_split$yto[i] <- projection_points$y[i]
        edge_split$to[i] <- closest_edge$proj_id[i]
        edge_split$d[i] <- projection_points$d_vert[i]-projection_points$d_vert[i-1]
        if (edge_split$d[i]<0) edge_split$d[i] <- 0
        frac_d <- edge_split$d[i]/edge$d
        if (edge$d==0) frac_d <- 0
        edge_split$d_weighted[i] <- edge_split$d_weighted[i]*frac_d
        edge_split$time[i] <- edge_split$time[i]*frac_d
        edge_split$time_weighted[i] <- edge_split$time_weighted[i]*frac_d
      }
    }
    ## do the last split
    i <- i+1
    edge_split$from[i] <- edge_split$to[i-1]
    edge_split$xfr[i] <- edge_split$xto[i-1]
    edge_split$yfr[i] <- edge_split$yto[i-1]
    edge_split$xto[i] <- edge$xto
    edge_split$yto[i] <- edge$yto
    edge_split$to[i] <- edge$to
    edge_split$d[i] <- edge$d-max(projection_points$d_vert)
    if (edge_split$d[i]<0) edge_split$d[i] <- 0
    frac_d <- edge_split$d[i]/edge$d
    if (edge$d==0) frac_d <- 0
    edge_split$d_weighted[i] <- edge_split$d_weighted[i]*frac_d
    edge_split$time[i] <- edge_split$time[i]*frac_d
    edge_split$time_weighted[i] <- edge_split$time_weighted[i]*frac_d
    #edge_split$edge_idx <- closest_edge$index
    ## now create the bidirectional connection edges
    edges_to_pts <- data.frame()
    for (i in seq_len(nrow(closest_edge))) {
      edge_new <- edge_split[c(1,1),]
      # Update endpoints for new edges
      edge_new$from[1] <- closest_edge$pt_id[i]
      edge_new$to[1] <- closest_edge$proj_id[i]
      edge_new$from[2] <- closest_edge$proj_id[i]
      edge_new$to[2] <- closest_edge$pt_id[i]
      # Update coordinates
      edge_new$xfr[1] <- closest_edge$xpt[i]
      edge_new$yfr[1] <- closest_edge$ypt[i]
      edge_new$xto[1] <- closest_edge$x[i]
      edge_new$yto[1] <- closest_edge$y[i]
      
      edge_new$xfr[2] <- closest_edge$x[i]
      edge_new$yfr[2] <- closest_edge$y[i]
      edge_new$xto[2] <- closest_edge$xpt[i]
      edge_new$yto[2] <- closest_edge$ypt[i]
      
      # Update distances and weights for new edges
      edge_new$d <- closest_edge$d_vert[i]
      if (is.null(new_edge_type)) {
        d_wt <- edge$d_weighted/edge$d
        # Use proportional weights from original edge
        edge_new$d_weighted <- edge_new$d * d_wt
        edge_new$time <- edge_new$d * (edge$time / edge$d)
        edge_new$time_weighted <- edge_new$d * (edge$time_weighted / edge$d)
        if (edge$d==0) {
          edge_new$d <- 0
          edge_new$d_weighted <- 0
          edge_new$time <- 0
          edge_new$time_weighted <- 0
        }
      } else {
        # Use new edge type weights
        #edge_new$highway <- new_edge_type
        wt_multiplier <- wt_profile_df$value[wt_profile_df$name == new_edge_type]
        if (length(wt_multiplier) == 0) {
          cli::cli_abort("Edge type {new_edge_type} not found in weight profile")
        }
        edge_new$d_weighted <- d_i / wt_multiplier
        edge_new <- dodgr:::set_maxspeed(edge_new, wt_profile, wt_profile_file) |>
          dodgr:::weight_by_num_lanes(wt_profile) |>
          dodgr:::calc_edge_time(wt_profile)
      }
      edges_to_pts <- rbind(edges_to_pts, edge_new)
    }
    # Combine split and new edges
    edge_split$edge_id <- paste0(edge_split$edge_id, "_", 1:nrow(edge_split))
    edges_to_pts$edge_id <- paste0("pt_",replicate(nrow(edges_to_pts), genhash(10)))
    edges_i <- rbind(edge_split, edges_to_pts)
    edges_i$graph_orig_idx <- edge_idx
    new_edges <- rbind(new_edges, edges_i)
  }
  new_edges <- new_edges |>
    dplyr::distinct(from, to, .keep_all = TRUE)
  result_orig <- graph[-unique(new_edges$graph_orig_idx),]
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