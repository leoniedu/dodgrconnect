#' Add direct edges between points and nearest vertices
#'
#' Creates new edges connecting input points directly to their nearest vertices
#' in the graph. Unlike [add_verts_to_graph()], this does not split existing edges
#' but creates direct connections to existing vertices.
#'
#' @inheritParams dodgr::add_nodes_to_graph
#' @param graph A dodgr graph to modify
#' @param xy Matrix or data.frame of x-y coordinates of points to add
#' @param wt_profile Name of weight profile (e.g., "foot", "bicycle", "motorcar")
#' @param wt_profile_file Path to custom weight profile JSON file
#' @param highway Type of highway for new edges (must exist in weight profile)
#' @param max_length Maximum allowed connection distance in meters (Inf = no limit)
#'
#' @return Modified graph with new bidirectional edges connecting input points
#'         to their nearest vertices. Edge weights and attributes are set according
#'         to the specified weight profile and highway type.
#'
#' @details
#' This function:
#' 1. Finds nearest vertices using [dodgr::match_points_to_verts()]
#' 2. Creates direct edges to these vertices if within max_length
#' 3. Applies weights based on specified profile and highway type
#' 4. Creates bidirectional connections automatically
#'
#' Weight profiles can be:
#' - Built-in dodgr profiles ("foot", "bicycle", "motorcar")
#' - Custom profiles from JSON file
#'
#' @examples
#' # Create sample network
#' net <- weight_streetnet(dodgr_streetnet("hampi india"))
#'
#' # Add points with residential street connections
#' pts <- data.frame(x = c(76.4, 76.5), y = c(15.3, 15.4))
#' net_connected <- add_edges_to_graph(
#'   net,
#'   pts,
#'   wt_profile = "motorcar",
#'   highway = "residential",
#'   max_length = 1000  # Max 1km connections
#' )
#'
#' # Using custom weight profile
#' net_custom <- add_edges_to_graph(
#'   net,
#'   pts,
#'   wt_profile_file = "path/to/profile.json",
#'   highway = "custom_type"
#' )
#'
#' @seealso
#' [add_verts_to_graph()] for adding points by splitting edges
#' [dodgr::match_points_to_verts()] for vertex matching details
#' @export
add_edges_to_graph <- function(graph,
                               xy,
                               wt_profile = NULL,
                               wt_profile_file = NULL,
                               highway = NULL,
                               max_length = Inf,
                               replace_component = TRUE) {
  
  # Validate inputs
  if (is.null(wt_profile) && is.null(wt_profile_file)) {
    cli::cli_abort("Must provide either wt_profile or wt_profile_file")
  }
  
  # Standardize column names at the start
  gr_cols <- dodgr_graph_cols(graph)
  gr_cols <- unlist(gr_cols[which(!is.na(gr_cols))])
  graph_std <- graph[, gr_cols]  # standardise column names
  names(graph_std) <- names(gr_cols)
  graph_std$edge_id <- as.character(graph_std$edge_id)
  # Save original column mapping
  col_mapping <- setNames(gr_cols, names(gr_cols))
  
  # Get weight profile
  wp <- dodgr:::get_profile(wt_profile = wt_profile, wt_profile_file)
  #wpw <- wp$weighting_profiles|>dplyr::filter(name==wt_profile)
  way_wt <- wp$value[wp$way == highway]
  if (length(way_wt) == 0) {
    stop(sprintf("No weight found for highway type '%s' in profile '%s'", 
                 highway, wt_profile))
  }
  # Pre-process xy
  if (!"id" %in% names(xy)) {
    xy_id <- vapply(seq_len(nrow(xy)), \(i) genhash(10), character(1))
  } else {
    xy_id <- xy$id
  }
  xy <- dodgr:::pre_process_xy(xy)
  xy$id <- xy_id
  
  # Match to vertices
  verts <- dodgr::dodgr_vertices(graph_std)
  matches <- dodgr::match_points_to_verts(verts, xy[, c("x", "y")])
  
  # Create connection edges
  new_edges <- lapply(seq_len(nrow(xy)), function(i) {
    pt <- xy[i, ]
    vert <- verts[matches[i], ]
    
    # Calculate distance
    d <- geodist::geodist(
      pt[, c("x", "y")],
      vert[, c("x", "y")],
      measure = "geodesic"
    )
    
    if (d > max_length) return(NULL)
    
    # Create edge pair
    edge_fwd <- data.frame(
      from = pt$id,
      to = vert$id,
      xfr = pt$x, yfr = pt$y,
      xto = vert$x, yto = vert$y,
      d = d,
      highway = highway %||% vert$highway,
      edge_id = genhash(10)
    )
    
    edge_rev <- edge_fwd
    edge_rev$from <- vert$id
    edge_rev$to <- pt$id
    edge_rev[, c("xfr", "yfr", "xto", "yto")] <- 
      edge_fwd[, c("xto", "yto", "xfr", "yfr")]
    edge_rev$edge_id <- genhash(10)
    
    # Apply weighting
    edges <- rbind(edge_fwd, edge_rev)
    edges$d_weighted <- edges$d / way_wt
    edges <- dodgr:::set_maxspeed(edges, wt_profile, wt_profile_file) |>
      dodgr:::weight_by_num_lanes(wt_profile) |>
      dodgr:::calc_edge_time(wt_profile)
    edges
  }) |> 
    Filter(f = Negate(is.null)) |>
    do.call(rbind, args = _)
  # Map standardized columns back to original
  result_new <- data.frame(highway=new_edges$highway)
  for (g in seq_along(gr_cols)) {
    col_std <- names(gr_cols)[g]
    orig_col <- names(graph)[col_mapping[g]]
    if (col_std %in% names(new_edges) && nrow(new_edges) > 0) {
      result_new[[orig_col]] <- new_edges[, col_std]
    }
  }
  
  # Ensure edge_id is character for consistency
  if (is.character(result_new$edge_id)) {
    graph$edge_id <- as.character(graph$edge_id)
  }
  
  # Combine original and new edges
  result_final <- dplyr::bind_rows(graph, result_new)
  if (replace_component) {
    result_final <- dodgr::dodgr_components(result_final%>%dplyr::select(-dplyr::any_of("component")))
  }
  # Return distinct edges
  dplyr::distinct(result_final, .keep_all = TRUE)
}

# Helper function
genhash <- function(n) {
  paste0(sample(c(letters, LETTERS, 0:9), n), collapse = "")
}
