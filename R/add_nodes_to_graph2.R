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
#' @param intersections_only If TRUE, only add nodes at intersections between
#' edges, otherwise add all nodes
#' @param new_edge_type Type of new edges to be added
#' @param wt_profile Name of weight profile to use
#' @param wt_profile_file Custom weight profile file
#' @param surface Surface type for new edges
#' @param max_length Maximum length of new edges
#' @param tolerance Tolerance for distance check (default: 1e-3)
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
#' graph1 <- add_nodes_to_graph2(graph, xy)
#'
#' # Add points with custom footpath weights
#' graph2 <- add_nodes_to_graph2(graph, xy,
#'                              new_edge_type = "footway",
#'                              wt_profile = "foot",
#'                              surface = "paved")
#' @export
add_nodes_to_graph2 <- function (graph,
                                 xy,
                                 intersections_only = FALSE,
                                 new_edge_type = NULL,
                                 wt_profile = NULL,
                                 wt_profile_file = NULL,
                                 surface = NULL,
                                 max_length = Inf,
                                 tolerance = 1e-3) {
  
  # Validate tolerance
  if (!is.numeric(tolerance) || tolerance < 0) {
    stop("Tolerance must be a non-negative numeric value")
  }
  
  # Convert max_length to units object if provided
  if (!is.null(max_length)) {
    max_length <- units::set_units(max_length, "m")
  }
  
  # Get vertex IDs
  has_vertex_ids <- "from_id" %in% names(graph)
  if (has_vertex_ids) {
    message("Has vertex IDs: TRUE")
    vertex_ids <- unique(c(graph$from_id, graph$to_id))
    n_vertex_ids <- length(vertex_ids)
    message("Number of vertex IDs: ", n_vertex_ids)
  } else {
    message("Has vertex IDs: FALSE")
    message("Number of vertex IDs: 2")
  }
  
  # Generate new vertex IDs for the points if needed
  xyf <- data.frame(xy)
  xy <- pre_process_xy(xy)
  xyf <- xyf%>%select(-any_of(c("x", "y")))%>%bind_cols(xy)
  if (!"id" %in% colnames(xyf)) {
    xyf$id <- sapply(1:nrow(xy), function(i) {
      paste0(sample(c(letters, LETTERS, 0:9), 10, replace=TRUE), collapse="")
    })
  }
  
  # Find closest edges for each point
  message("Finding closest edges...")
  closest_edges_0 <- dodgr::match_pts_to_graph(graph = graph, xy = xy, connected = FALSE, distances = TRUE)%>%mutate(xy_index=1:n())
  closest_edges <- closest_edges_0%>%filter(abs(d_signed)<=as.numeric(max_length))
  not_connected <- nrow(closest_edges_0)-nrow(closest_edges)
  if (not_connected>0) {
    cli::cli_alert_warning("{not_connected} points not connected ")
  }
  if(nrow(closest_edges)==0) {
    return(graph)
  }
  ## 
  xy <- xy[closest_edges$xy_index,]
  xyf <- xyf[closest_edges$xy_index,]
  # Create new edges connecting points to their closest edges
  new_edges <- data.frame()
  for (i in seq_len(nrow(xy))) {
    edge_idx <- closest_edges$index[i]
    if (is.na(edge_idx)) next
    edge <- graph[edge_idx, ]
    point_x <- xy$x[i]
    point_y <- xy$y[i]
    point_id <- xyf$id[i]
    proj_x <- closest_edges$x[i]
    proj_y <- closest_edges$y[i]
    # Create new vertex ID for projection point
    proj_id <- paste0(sample(c(letters, LETTERS, 0:9), 10, replace=TRUE), collapse="")
    
    # Create edge from point to projection
    new_edge <- edge
    new_edge$edge_id <- paste0("new_", i)
    new_edge$from_id <- point_id
    new_edge$from_lon <- point_x
    new_edge$from_lat <- point_y
    new_edge$to_id <- proj_id
    new_edge$to_lon <- proj_x
    new_edge$to_lat <- proj_y
    new_edge$d <- abs(closest_edges$d_signed[i])
    if (!is.null(wt_profile)) {
      # Get weight profile
      wp <- dodgr:::get_profile(wt_profile, wt_profile_file)
      way_wt <- wp$value[wp$way == new_edge_type]
      if (length(way_wt) == 0) {
        stop(sprintf("No weight found for highway type '%s' in profile '%s'", 
                   new_edge_type, wt_profile))
      }
      
      # Calculate weights
      new_edge$highway <- new_edge_type
      new_edge$d_weighted <- new_edge$d / way_wt
      new_edge <- dodgr:::set_maxspeed(new_edge, wt_profile, wt_profile_file) |>
        dodgr:::weight_by_num_lanes(wt_profile) |>
        dodgr:::calc_edge_time(wt_profile)
    } else {
      new_edge$d_weighted <- new_edge$d * (edge$d_weighted / edge$d)
      new_edge$time <- new_edge$d * (edge$time / edge$d)
      new_edge$time_weighted <- new_edge$d * (edge$time_weighted / edge$d)
    }
    
    # Create reverse edge
    rev_edge <- new_edge
    rev_edge$edge_id <- paste0(new_edge$edge_id, "_rev")
    rev_edge$from_id <- new_edge$to_id
    rev_edge$from_lon <- new_edge$to_lon
    rev_edge$from_lat <- new_edge$to_lat
    rev_edge$to_id <- new_edge$from_id
    rev_edge$to_lon <- new_edge$from_lon
    rev_edge$to_lat <- new_edge$from_lat
    
    # Add edges to result
    new_edges <- rbind(new_edges, new_edge, rev_edge)
    
    # Split the original edge at the projection point
    edge1 <- edge
    edge1$edge_id <- paste0(edge$edge_id, "_split1_", i)
    edge1$to_id <- proj_id
    edge1$to_lon <- proj_x
    edge1$to_lat <- proj_y
    edge1$d <- as.numeric(units::set_units(
      geodist::geodist(
        matrix(c(edge1$from_lon, edge1$from_lat), ncol = 2, dimnames=list(NULL, c("x", "y"))),
        matrix(c(edge1$to_lon, edge1$to_lat), ncol = 2, dimnames=list(NULL, c("x", "y"))),
        measure = "geodesic"
      )[1,1],
      "m"
    ))
    edge1$d_weighted <- edge1$d * (edge$d_weighted / edge$d)
    edge1$time <- edge1$d * (edge$time / edge$d)
    edge1$time_weighted <- edge1$d * (edge$time_weighted / edge$d)
    
    edge2 <- edge
    edge2$edge_id <- paste0(edge$edge_id, "_split2_", i)
    edge2$from_id <- proj_id
    edge2$from_lon <- proj_x
    edge2$from_lat <- proj_y
    edge2$d <- as.numeric(units::set_units(
      geodist::geodist(
        matrix(c(edge2$from_lon, edge2$from_lat), ncol = 2, dimnames=list(NULL, c("x", "y"))),
        matrix(c(edge2$to_lon, edge2$to_lat), ncol = 2, dimnames=list(NULL, c("x", "y"))),
        measure = "geodesic"
      )[1,1],
      "m"
    ))
    edge2$d_weighted <- edge2$d * (edge$d_weighted / edge$d)
    edge2$time <- edge2$d * (edge$time / edge$d)
    edge2$time_weighted <- edge2$d * (edge$time_weighted / edge$d)
    
    # Add split edges to result
    new_edges <- bind_rows(new_edges, edge1, edge2)
  }
  
  # Remove original edges that were split and add new edges
  graph$edge_id <- as.character(graph$edge_id)
  result <- bind_rows(
      graph[-closest_edges$index,],  # Keep edges that weren't split
      new_edges                      # Add new edges
  )
  
  # Ensure result has same columns as input graph
  missing_cols <- setdiff(names(graph), names(result))
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      result[[col]] <- NA
    }
  }
  
  result <- result[, intersect(names(graph), names(result))]
  return(result)
}

# find_points_on_edge <- function(from_lon, from_lat, to_lon, to_lat,
#                                points_x, points_y, dist_tol = 1e-6) {
#   # Convert points to matrix format
#   points_mat <- matrix(c(points_x, points_y), ncol = 2)
#   start_mat <- matrix(c(from_lon, from_lat), nrow = 1)
#   end_mat <- matrix(c(to_lon, to_lat), nrow = 1)
#   
#   # Calculate edge vector
#   edge_vec <- c(to_lon - from_lon, to_lat - from_lat)
#   edge_length_sq <- sum(edge_vec^2)
#   
#   # If edge has zero length, return empty result
#   if (edge_length_sq < 1e-12) {
#     return(data.frame(x = numeric(0), y = numeric(0), dist = numeric(0), merge = logical(0)))
#   }
#   
#   # Calculate edge length in meters using geodist
#   edge_df <- data.frame(x = c(from_lon, to_lon), y = c(from_lat, to_lat))
#   edge_length_m <- as.numeric(geodist::geodist(edge_df, measure = "geodesic")[1,2])
#   
#   # For each point, calculate projection onto line
#   n_points <- length(points_x)
#   on_edge <- logical(n_points)
#   distances <- numeric(n_points)
#   merge_points <- logical(n_points)
#   
#   for (i in seq_len(n_points)) {
#     # Vector from edge start to point
#     point_vec <- c(points_x[i] - from_lon, points_y[i] - from_lat)
#     
#     # Calculate projection onto edge
#     t <- sum(point_vec * edge_vec) / edge_length_sq
#     
#     # Calculate nearest point based on projection parameter t
#     if (t < 0) {
#       # Point projects before start of edge, use start point
#       nearest_x <- from_lon
#       nearest_y <- from_lat
#       t_clamped <- 0
#     } else if (t > 1) {
#       # Point projects after end of edge, use end point
#       nearest_x <- to_lon
#       nearest_y <- to_lat
#       t_clamped <- 1
#     } else {
#       # Point projects onto edge, calculate projected point
#       nearest_x <- from_lon + t * edge_vec[1]
#       nearest_y <- from_lat + t * edge_vec[2]
#       t_clamped <- t
#     }
#     
#     # Calculate distance using geodist
#     point_df <- data.frame(
#       x = c(points_x[i], nearest_x),
#       y = c(points_y[i], nearest_y)
#     )
#     dist_m <- as.numeric(geodist::geodist(point_df, measure = "geodesic")[1,2])
#     
#     # Print debug info
#     message(sprintf("Point %d: (%g,%g)", i, points_x[i], points_y[i]))
#     message(sprintf("  Vector to point: %g, %g", point_vec[1], point_vec[2]))
#     message(sprintf("  Projection parameter t: %g", t))
#     message(sprintf("  Projected point: (%g,%g)", nearest_x, nearest_y))
#     message(sprintf("  Perpendicular distance (m): %g", dist_m))
#     message(sprintf("  Edge length (m): %g", edge_length_m))
#     
#     # Always include the point, but mark for merging if very close
#     on_edge[i] <- TRUE
#     distances[i] <- t_clamped
#     merge_points[i] <- dist_m < dist_tol
#     
#     if (merge_points[i]) {
#       message("  Point will be merged (distance < tolerance)")
#     } else {
#       message("  Point will get separate edges")
#     }
#   }
#   
#   # Return points that are on the edge
#   return(data.frame(
#     x = points_x[on_edge],
#     y = points_y[on_edge],
#     dist = distances[on_edge],
#     merge = merge_points[on_edge]
#   ))
# }
