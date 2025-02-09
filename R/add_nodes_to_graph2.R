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
#' @param dist_tol Minimum distance used to identify points lying directly on edges,
#'        expressed in units of the distance column of `graph`
#' @param intersections_only If TRUE, only add nodes at intersections between
#' edges, otherwise add all nodes
#' @param new_edge_type Type of new edges to be added
#' @param wt_profile Name of weight profile to use
#' @param wt_profile_file Custom weight profile file
#' @param surface Surface type for new edges
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
                                 dist_tol = 1e-6,
                                 intersections_only = FALSE,
                                 new_edge_type = NULL,
                                 wt_profile = NULL,
                                 wt_profile_file = NULL,
                                 surface = NULL) {
  
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
  
  # Check xy columns
  if (is.null(colnames(xy))) {
    colnames(xy) <- c("x", "y")
  }
  message("XY columns: ", paste(colnames(xy), collapse=", "))
  
  # Generate new vertex IDs for the points
  new_vertex_ids <- sapply(1:nrow(xy), function(i) {
    paste0(sample(c(letters, LETTERS, 0:9), 10, replace=TRUE), collapse="")
  })
  
  # Initialize result with empty dataframe
  result <- data.frame(
    geom_num = numeric(0),
    edge_id = character(0),
    from_id = character(0),
    from_lon = numeric(0),
    from_lat = numeric(0),
    to_id = character(0),
    to_lon = numeric(0),
    to_lat = numeric(0),
    d = numeric(0),
    d_weighted = numeric(0),
    highway = character(0),
    way_id = numeric(0),
    component = numeric(0),
    time = numeric(0),
    time_weighted = numeric(0)
  )

  # Process each edge in the graph
  processed_edges <- character(0)  # Keep track of processed edges
  connected_points <- list()  # Keep track of points we've already connected
  result <- graph[0,]  # Start with empty result with same structure as input
  edges_to_keep <- character(0)  # Keep track of edges to keep
  
  # First pass: process all edges except edge_id "2"
  for (i in seq_len(nrow(graph))) {
    edge <- graph[i,]
    
    # Skip edge_id "2" for now
    if (edge$edge_id == "2") {
      next
    }
    
    # Skip if we've already processed this edge in the opposite direction
    # Check for both original edge ID and its reverse
    edge_id <- as.character(edge$edge_id)
    rev_edge_id <- if (endsWith(edge_id, "_rev")) {
      sub("_rev$", "", edge_id)
    } else {
      paste0(edge_id, "_rev")
    }
    
    if (edge_id %in% processed_edges || rev_edge_id %in% processed_edges) {
      next
    }
    processed_edges <- c(processed_edges, edge_id, rev_edge_id)
    
    from_id <- edge$from_id
    to_id <- edge$to_id
    from_lon <- edge$from_lon
    from_lat <- edge$from_lat
    to_lon <- edge$to_lon
    to_lat <- edge$to_lat

    # Create vectors for edge
    edge_vector <- c(to_lon - from_lon, to_lat - from_lat)
    edge_length <- sqrt(sum(edge_vector^2))

    # Process each point
    points_on_edge <- list()
    points_inside <- list()
    points_outside <- list()

    for (j in seq_len(nrow(xy))) {
      point_x <- xy$x[j]
      point_y <- xy$y[j]
      point_key <- paste0(point_x, "_", point_y)
      
      # Skip if we've already connected this point
      if (point_key %in% names(connected_points)) {
        next
      }

      # Vector from edge start to point
      point_vector <- c(point_x - from_lon, point_y - from_lat)
      
      # Project point onto edge
      t <- sum(point_vector * edge_vector) / sum(edge_vector^2)
      
      # Calculate projected point
      proj_x <- from_lon + t * edge_vector[1]
      proj_y <- from_lat + t * edge_vector[2]
      
      # Calculate perpendicular distance using geodist
      point_df <- data.frame(
        x = c(point_x, proj_x),
        y = c(point_y, proj_y)
      )
      perp_distance <- as.numeric(geodist::geodist(point_df, measure = "geodesic")[1,2])
      
      cat(sprintf("Point %d: (%g,%g)\n", j, point_x, point_y))
      cat(sprintf("  Vector to point: %g, %g\n", point_vector[1], point_vector[2]))
      cat(sprintf("  Projection parameter t: %g\n", t))
      cat(sprintf("  Projected point: (%g,%g)\n", proj_x, proj_y))
      cat(sprintf("  Perpendicular distance (m): %g\n", perp_distance))
      
      # Store point info
      point_info <- list(
        x = point_x,
        y = point_y,
        proj_x = proj_x,
        proj_y = proj_y,
        t = t,
        perp_distance = perp_distance,
        key = point_key
      )
      
      # Classify point based on projection
      if (t >= 0 && t <= 1) {
        cat("  Point will get separate edges\n")
        points_inside[[length(points_inside) + 1]] <- point_info
      } else {
        cat("  Point will get separate edges\n")
        points_outside[[length(points_outside) + 1]] <- point_info
      }
    }
    
    # If there are points projecting inside the edge, split it at those points
    if (length(points_inside) > 0) {
      # Sort points by their projection parameter t
      t_values <- sapply(points_inside, function(p) p$t)
      points_inside <- points_inside[order(t_values)]
      
      # Split original edge at projection points
      prev_id <- from_id
      prev_lon <- from_lon
      prev_lat <- from_lat
      
      for (j in seq_along(points_inside)) {
        point_info <- points_inside[[j]]
        
        # Skip if we've already connected this point
        if (point_info$key %in% names(connected_points)) {
          next
        }
        
        # Generate vertex IDs for both projected and actual points
        proj_id <- paste0(sample(c(letters, LETTERS, 0:9), 10, replace=TRUE), collapse="")
        actual_id <- paste0(sample(c(letters, LETTERS, 0:9), 10, replace=TRUE), collapse="")
        connected_points[[point_info$key]] <- actual_id
        
        # Calculate direct distance to projected point using geodist
        point_df <- data.frame(
          x = c(prev_lon, point_info$proj_x),
          y = c(prev_lat, point_info$proj_y)
        )
        direct_distance <- as.numeric(geodist::geodist(point_df, measure = "geodesic")[1,2])
        
        # Create edge from previous point to projected point
        split_edge <- edge
        split_edge$edge_id <- paste0(edge$edge_id, "_split_", j)
        split_edge$from_id <- prev_id
        split_edge$from_lon <- prev_lon
        split_edge$from_lat <- prev_lat
        split_edge$to_id <- proj_id
        split_edge$to_lon <- point_info$proj_x
        split_edge$to_lat <- point_info$proj_y
        split_edge$d <- direct_distance
        split_edge$d_weighted <- direct_distance * (edge$d_weighted / edge$d)
        split_edge$time <- direct_distance * (edge$time / edge$d)
        split_edge$time_weighted <- direct_distance * (edge$time_weighted / edge$d)
        result <- rbind(result, split_edge)
        
        # Create reverse edge
        rev_split_edge <- split_edge
        rev_split_edge$edge_id <- paste0(split_edge$edge_id, "_rev")
        rev_split_edge$from_id <- proj_id
        rev_split_edge$from_lon <- point_info$proj_x
        rev_split_edge$from_lat <- point_info$proj_y
        rev_split_edge$to_id <- prev_id
        rev_split_edge$to_lon <- prev_lon
        rev_split_edge$to_lat <- prev_lat
        result <- rbind(result, rev_split_edge)
        
        # Create perpendicular edge from projected point to actual point
        perp_edge <- edge
        perp_edge$edge_id <- paste0(sample(c(letters, LETTERS, 0:9), 10, replace=TRUE), collapse="")
        perp_edge$from_id <- proj_id
        perp_edge$from_lon <- point_info$proj_x
        perp_edge$from_lat <- point_info$proj_y
        perp_edge$to_id <- actual_id
        perp_edge$to_lon <- point_info$x
        perp_edge$to_lat <- point_info$y
        perp_edge$d <- point_info$perp_distance
        
        # Only add perpendicular edges if not intersections_only
        if (!intersections_only) {
          if (!is.null(wt_profile)) {
            # Get weight profile
            wp <- dodgr:::get_profile(wt_profile, wt_profile_file)
            way_wt <- wp$value[wp$way == new_edge_type]
            if (length(way_wt) == 0) {
              stop(sprintf("No weight found for highway type '%s' in profile '%s'", 
                         new_edge_type, wt_profile))
            }
            
            # Calculate weights
            perp_edge$highway <- new_edge_type
            perp_edge$d_weighted <- point_info$perp_distance / way_wt
            perp_edge <- dodgr:::set_maxspeed(perp_edge, wt_profile, wt_profile_file) |>
              dodgr:::weight_by_num_lanes(wt_profile) |>
              dodgr:::calc_edge_time(wt_profile)
          } else {
            perp_edge$d_weighted <- point_info$perp_distance * (edge$d_weighted / edge$d)
            perp_edge$time <- point_info$perp_distance * (edge$time / edge$d)
            perp_edge$time_weighted <- point_info$perp_distance * (edge$time_weighted / edge$d)
          }
          result <- rbind(result, perp_edge)
          
          # Create reverse perpendicular edge
          perp_edge_rev <- perp_edge
          perp_edge_rev$edge_id <- paste0(perp_edge$edge_id, "_rev")
          perp_edge_rev$from_id <- perp_edge$to_id
          perp_edge_rev$from_lon <- perp_edge$to_lon
          perp_edge_rev$from_lat <- perp_edge$to_lat
          perp_edge_rev$to_id <- perp_edge$from_id
          perp_edge_rev$to_lon <- perp_edge$from_lon
          perp_edge_rev$to_lat <- perp_edge$from_lat
          result <- rbind(result, perp_edge_rev)
        }
        
        # Update previous point to projected point
        prev_id <- proj_id
        prev_lon <- point_info$proj_x
        prev_lat <- point_info$proj_y
      }
      
      # Create final edge from last projected point to original end
      point_df <- data.frame(
        x = c(prev_lon, to_lon),
        y = c(prev_lat, to_lat)
      )
      direct_distance <- as.numeric(geodist::geodist(point_df, measure = "geodesic")[1,2])
      
      # Create edge from last point to end
      split_edge <- edge
      split_edge$edge_id <- paste0(edge$edge_id, "_split_", length(points_inside) + 1)
      split_edge$from_id <- prev_id
      split_edge$from_lon <- prev_lon
      split_edge$from_lat <- prev_lat
      split_edge$to_id <- to_id
      split_edge$to_lon <- to_lon
      split_edge$to_lat <- to_lat
      split_edge$d <- direct_distance
      split_edge$d_weighted <- direct_distance * (edge$d_weighted / edge$d)
      split_edge$time <- direct_distance * (edge$time / edge$d)
      split_edge$time_weighted <- direct_distance * (edge$time_weighted / edge$d)
      result <- rbind(result, split_edge)
      
      # Create reverse edge
      rev_split_edge <- split_edge
      rev_split_edge$edge_id <- paste0(split_edge$edge_id, "_rev")
      rev_split_edge$from_id <- to_id
      rev_split_edge$from_lon <- to_lon
      rev_split_edge$from_lat <- to_lat
      rev_split_edge$to_id <- prev_id
      rev_split_edge$to_lon <- prev_lon
      rev_split_edge$to_lat <- prev_lat
      result <- rbind(result, rev_split_edge)
    } else {
      # If no points project inside this edge, keep it and its reverse
      edges_to_keep <- c(edges_to_keep, edge_id, rev_edge_id)
      result <- rbind(result, edge)
      rev_edge <- graph[graph$edge_id == rev_edge_id,]
      if (nrow(rev_edge) > 0) {
        result <- rbind(result, rev_edge)
      }
    }
    
    # Add direct edges for points outside
    for (point_info in points_outside) {
      # Skip if we've already connected this point
      if (point_info$key %in% names(connected_points)) {
        next
      }
      
      # Generate vertex ID for the point
      actual_id <- paste0(sample(c(letters, LETTERS, 0:9), 10, replace=TRUE), collapse="")
      connected_points[[point_info$key]] <- actual_id
      
      # Find closest vertex (from or to)
      point_df <- data.frame(
        x = c(point_info$x, from_lon, to_lon),
        y = c(point_info$y, from_lat, to_lat)
      )
      distances <- as.numeric(geodist::geodist(point_df, measure = "geodesic")[1,])
      if (distances[2] <= distances[3]) {
        # Connect to from_id
        vertex_id <- from_id
        vertex_lon <- from_lon
        vertex_lat <- from_lat
        direct_distance <- distances[2]
      } else {
        # Connect to to_id
        vertex_id <- to_id
        vertex_lon <- to_lon
        vertex_lat <- to_lat
        direct_distance <- distances[3]
      }
      
      # Create edge from vertex to point
      direct_edge <- edge
      direct_edge$edge_id <- paste0(sample(c(letters, LETTERS, 0:9), 10, replace=TRUE), collapse="")
      direct_edge$from_id <- vertex_id
      direct_edge$from_lon <- vertex_lon
      direct_edge$from_lat <- vertex_lat
      direct_edge$to_id <- actual_id
      direct_edge$to_lon <- point_info$x
      direct_edge$to_lat <- point_info$y
      direct_edge$d <- direct_distance
      
      # Only add direct edges if not intersections_only
      if (!intersections_only) {
        if (!is.null(wt_profile)) {
          # Get weight profile
          wp <- dodgr:::get_profile(wt_profile, wt_profile_file)
          way_wt <- wp$value[wp$way == new_edge_type]
          if (length(way_wt) == 0) {
            stop(sprintf("No weight found for highway type '%s' in profile '%s'", 
                       new_edge_type, wt_profile))
          }
          
          # Calculate weights
          direct_edge$highway <- new_edge_type
          direct_edge$d_weighted <- direct_distance / way_wt
          direct_edge <- dodgr:::set_maxspeed(direct_edge, wt_profile, wt_profile_file) |>
            dodgr:::weight_by_num_lanes(wt_profile) |>
            dodgr:::calc_edge_time(wt_profile)
        } else {
          direct_edge$d_weighted <- direct_distance * (edge$d_weighted / edge$d)
          direct_edge$time <- direct_distance * (edge$time / edge$d)
          direct_edge$time_weighted <- direct_distance * (edge$time_weighted / edge$d)
        }
        result <- rbind(result, direct_edge)
        
        # Create reverse edge
        direct_edge_rev <- direct_edge
        direct_edge_rev$edge_id <- paste0(direct_edge$edge_id, "_rev")
        direct_edge_rev$from_id <- direct_edge$to_id
        direct_edge_rev$from_lon <- direct_edge$to_lon
        direct_edge_rev$from_lat <- direct_edge$to_lat
        direct_edge_rev$to_id <- direct_edge$from_id
        direct_edge_rev$to_lon <- direct_edge$from_lon
        direct_edge_rev$to_lat <- direct_edge$from_lat
        result <- rbind(result, direct_edge_rev)
      }
    }
  }
  
  # Second pass: process edge_id "2" only if edge_id "1" was kept
  if ("1" %in% edges_to_keep) {
    edge <- graph[graph$edge_id == "2",]
    if (nrow(edge) > 0) {
      edges_to_keep <- c(edges_to_keep, "2")
      result <- rbind(result, edge)
    }
  }
  
  # Only keep edges that were marked to keep
  result <- rbind(
    result[!(result$edge_id %in% graph$edge_id),],  # Keep all new edges
    result[result$edge_id %in% edges_to_keep,]      # Keep only original edges that were marked to keep
  )
  
  # Return result with same columns as input graph
  required_cols <- names(graph)
  
  missing_cols <- setdiff(required_cols, names(result))
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      result[[col]] <- NA
    }
  }
  
  result <- result[, intersect(names(graph), names(result))]
  return(result)
}

find_points_on_edge <- function(from_lon, from_lat, to_lon, to_lat,
                               points_x, points_y, dist_tol = 1e-6) {
  # Convert points to matrix format
  points_mat <- matrix(c(points_x, points_y), ncol = 2)
  start_mat <- matrix(c(from_lon, from_lat), nrow = 1)
  end_mat <- matrix(c(to_lon, to_lat), nrow = 1)
  
  # Calculate edge vector
  edge_vec <- c(to_lon - from_lon, to_lat - from_lat)
  edge_length_sq <- sum(edge_vec^2)
  
  # If edge has zero length, return empty result
  if (edge_length_sq < 1e-12) {
    return(data.frame(x = numeric(0), y = numeric(0), dist = numeric(0), merge = logical(0)))
  }
  
  # Calculate edge length in meters using geodist
  edge_df <- data.frame(x = c(from_lon, to_lon), y = c(from_lat, to_lat))
  edge_length_m <- as.numeric(geodist::geodist(edge_df, measure = "geodesic")[1,2])
  
  # For each point, calculate projection onto line
  n_points <- length(points_x)
  on_edge <- logical(n_points)
  distances <- numeric(n_points)
  merge_points <- logical(n_points)
  
  for (i in seq_len(n_points)) {
    # Vector from edge start to point
    point_vec <- c(points_x[i] - from_lon, points_y[i] - from_lat)
    
    # Calculate projection onto edge
    t <- sum(point_vec * edge_vec) / edge_length_sq
    
    # Calculate nearest point based on projection parameter t
    if (t < 0) {
      # Point projects before start of edge, use start point
      nearest_x <- from_lon
      nearest_y <- from_lat
      t_clamped <- 0
    } else if (t > 1) {
      # Point projects after end of edge, use end point
      nearest_x <- to_lon
      nearest_y <- to_lat
      t_clamped <- 1
    } else {
      # Point projects onto edge, calculate projected point
      nearest_x <- from_lon + t * edge_vec[1]
      nearest_y <- from_lat + t * edge_vec[2]
      t_clamped <- t
    }
    
    # Calculate distance using geodist
    point_df <- data.frame(
      x = c(points_x[i], nearest_x),
      y = c(points_y[i], nearest_y)
    )
    dist_m <- as.numeric(geodist::geodist(point_df, measure = "geodesic")[1,2])
    
    # Print debug info
    message(sprintf("Point %d: (%g,%g)", i, points_x[i], points_y[i]))
    message(sprintf("  Vector to point: %g, %g", point_vec[1], point_vec[2]))
    message(sprintf("  Projection parameter t: %g", t))
    message(sprintf("  Projected point: (%g,%g)", nearest_x, nearest_y))
    message(sprintf("  Perpendicular distance (m): %g", dist_m))
    message(sprintf("  Edge length (m): %g", edge_length_m))
    
    # Always include the point, but mark for merging if very close
    on_edge[i] <- TRUE
    distances[i] <- t_clamped
    merge_points[i] <- dist_m < dist_tol
    
    if (merge_points[i]) {
      message("  Point will be merged (distance < tolerance)")
    } else {
      message("  Point will get separate edges")
    }
  }
  
  # Return points that are on the edge
  return(data.frame(
    x = points_x[on_edge],
    y = points_y[on_edge],
    dist = distances[on_edge],
    merge = merge_points[on_edge]
  ))
}
