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
add_nodes_to_graph3 <- function(graph,
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
  closest_edges$xy_index <- seq_len(nrow(closest_edges))
  
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
  
  for (i in seq_len(nrow(xy))) {
    edge_idx <- closest_edges$index[i]
    if (is.na(edge_idx)) next
    
    # Get the edge to split
    edge <- graph_std[edge_idx,]
    edge_f <- graph[edge_idx,]
    point_x <- xy$x[i]
    point_y <- xy$y[i]
    point_id <- xyf$id[i]
    proj_x <- closest_edges$x[i]
    proj_y <- closest_edges$y[i]
    
    # Create new vertex ID for projection point
    proj_id <- genhash(10L)
    
    # First split the original edge into two parts (bidirectional)
    edge_split <- edge[rep(1, 2),]  # Create 2 edges for split
    
    # First direction
    edge_split$to[1] <- proj_id  # from start to proj
    edge_split$xto[1] <- proj_x
    edge_split$yto[1] <- proj_y
    edge_split$from[2] <- proj_id  # from proj to end
    edge_split$xfr[2] <- proj_x
    edge_split$yfr[2] <- proj_y
    edge_split$xto[2] <- edge$xto
    edge_split$yto[2] <- edge$yto
    edge_split$to[2] <- edge$to
    
    # Calculate distances using geodist for consistency with dodgr
    xy_i <- data.frame(
      x = as.numeric(c(edge_split[1, "xfr"], edge_split[1, "xto"], edge_split[2, "xto"])),
      y = as.numeric(c(edge_split[1, "yfr"], edge_split[1, "yto"], edge_split[2, "yto"]))
    )
    dmat <- geodist::geodist(xy_i, measure = "geodesic")
    
    # Calculate proportional weights from original edge
    d_wt <- edge$d_weighted / edge$d
    
    # Check if point is very close to vertex (like dodgr)
    if (any(dmat[upper.tri(dmat)] < dist_tol)) {
      edge_split <- edge[1,, drop = FALSE]  # Single edge for close points
      edge_split_rev <- edge_split
      
      # Create reverse edge
      edge_split_rev$from <- edge_split$to
      edge_split_rev$to <- edge_split$from
      edge_split_rev$xfr <- edge_split$xto
      edge_split_rev$xto <- edge_split$xfr
      edge_split_rev$yfr <- edge_split$yto
      edge_split_rev$yto <- edge_split$yfr
      
      edge_split <- rbind(edge_split, edge_split_rev)
      
      # Determine which end is closer
      d_i_min <- c(1, 1, 2)[which.min(dmat[upper.tri(dmat)])]
      if (d_i_min == 1) {
        edge_split <- edge_split[2:1,]
      }
    } else {
      # Update distances and weights for split edges
      edge_split$d[1] <- dmat[1, 2]
      edge_split$d[2] <- dmat[2, 3]
      
      if (is.null(new_edge_type)) {
        # Use proportional weights from original edge
        edge_split$d_weighted <- edge_split$d * d_wt
        edge_split$time <- edge_split$d * (edge$time / edge$d)
        edge_split$time_weighted <- edge_split$d * (edge$time_weighted / edge$d)
        

        edge_split$edge_id <- paste0(edge_split$edge_id, "_", LETTERS[1:2])
      } else {
        # Use new edge type weights
        #edge_split$highway <- new_edge_type
        
        wt_profile_df <- if (!is.null(wt_profile_file)) {
          stop("fix loading profile")
          read.csv(wt_profile_file)
        } else {
          weight_profiles[[wt_profile]]
        }
        
        wt_multiplier <- wt_profile_df$value[wt_profile_df$highway == new_edge_type]
        if (length(wt_multiplier) == 0) {
          cli::cli_alert_warning("Edge type {new_edge_type} not found in weight profile, using 1")
          wt_multiplier <- 1
        }
        
        edge_split$d_weighted <- edge_split$d * wt_multiplier
        edge_split <- dodgr:::set_maxspeed(edge_split, wt_profile, wt_profile_file) |>
          dodgr:::weight_by_num_lanes(wt_profile) |>
          dodgr:::calc_edge_time(wt_profile)
        
        # Check for NAs in time calculations
        if (any(is.na(edge_split$time)) || any(is.na(edge_split$time_weighted))) {
          cli::cli_alert_warning("NAs found in time calculations for edge_split")
          print(edge_split[c("d", "d_weighted", "time", "time_weighted")])
        }
        
        edge_split$edge_id <- paste0(edge_split$edge_id, "_", LETTERS[1:2])
      }
    }
    
    # Calculate distance to new point using geodist
    d_i <- geodist::geodist(
      data.frame(x = point_x, y = point_y),
      data.frame(x = proj_x, y = proj_y),
      measure = "geodesic"
    )
    d_i <- as.numeric(d_i[1, 1])
    
    # Create edges to the new point (both directions)
    edge_new <- edge_split[c(1,1),]  # Create two new edges for bidirectional
    
    # Update endpoints for new edges
    edge_new$from[1] <- point_id
    edge_new$to[1] <- proj_id
    edge_new$from[2] <- proj_id
    edge_new$to[2] <- point_id
    
    # Update coordinates
    edge_new$xfr[1] <- point_x
    edge_new$yfr[1] <- point_y
    edge_new$xto[1] <- proj_x
    edge_new$yto[1] <- proj_y
    
    edge_new$xfr[2] <- proj_x
    edge_new$yfr[2] <- proj_y
    edge_new$xto[2] <- point_x
    edge_new$yto[2] <- point_y
    
    # Update distances and weights for new edges
    edge_new$d <- d_i

    if (is.null(new_edge_type)) {
      # Use proportional weights from original edge
      edge_new$d_weighted <- d_i * d_wt
      edge_new$time <- d_i * (edge$time / edge$d)
      edge_new$time_weighted <- d_i * (edge$time_weighted / edge$d)
      
      # Preserve other properties from original edge
      #edge_new$highway <- edge$highway
      #edge_new$maxspeed <- edge$maxspeed
      #edge_new$lanes <- edge$lanes
      
      edge_new$edge_id <- vapply(seq_len(nrow(edge_new)),
                              function(i) genhash(10),
                              character(1L))
    } else {
      # Use new edge type weights
      #edge_new$highway <- new_edge_type
      wt_multiplier <- wt_profile_df$value[wt_profile_df$highway == new_edge_type]
      if (length(wt_multiplier) == 0) {
        cli::cli_alert_warning("Edge type {new_edge_type} not found in weight profile, using 1")
        wt_multiplier <- 1
      }
      edge_new$d_weighted <- d_i * wt_multiplier
      edge_new <- dodgr:::set_maxspeed(edge_new, wt_profile, wt_profile_file) |>
        dodgr:::weight_by_num_lanes(wt_profile) |>
        dodgr:::calc_edge_time(wt_profile)
        
      # Check for NAs in time calculations
      if (any(is.na(edge_new$time)) || any(is.na(edge_new$time_weighted))) {
        cli::cli_alert_warning("NAs found in time calculations for edge_new")
        print(edge_new[c("d", "d_weighted", "time", "time_weighted")])
      }
        
      edge_new$edge_id <- vapply(seq_len(nrow(edge_new)),
                              function(i) genhash(10),
                              character(1L))
    }
    
    # Combine split and new edges
    edges_i <- rbind(edge_split, edge_new)
    
    # Combine all edges
    #cli::cli_inform("{i}: {nrow(edges_i)} new edges")
    edges_i$graph_orig_idx <- edge_idx
    new_edges <- rbind(new_edges, edges_i)
  }
  

  result_orig <- graph[-closest_edges$index[!is.na(closest_edges$index)],]
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