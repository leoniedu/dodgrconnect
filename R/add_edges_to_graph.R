#' Add direct edges between points and nearest graph vertices
#' 
#' @inheritParams dodgr::add_nodes_to_graph
#' @param highway Type of highway for new edges (must exist in weight profile)
#' @param max_length Maximum connection distance in meters
#' @export
add_edges_to_graph <- function(graph,
                               xy,
                               wt_profile = NULL,
                               wt_profile_file = NULL,
                               highway = NULL,
                               max_length = Inf) {
  
  # Validate inputs
  if (is.null(wt_profile) && is.null(wt_profile_file)) {
    cli::cli_abort("Must provide either wt_profile or wt_profile_file")
  }
  
  # Get weight profile
  wp <- if (!is.null(wt_profile_file)) {
    dodgr:::get_profile(wt_profile_file)
  } else {
    if (!wt_profile %in% dodgr::weighting_profiles$weighting_profiles$name) {
      cli::cli_abort("Weight profile '{wt_profile}' not found in dodgr")
    }
    dodgr::weighting_profiles
  }
  wpw <- wp$weighting_profiles|>dplyr::filter(name==wt_profile)
  way_wt <- wpw$value[wpw$way == highway]
  if (length(way_wt) == 0) {
    stop(sprintf("No weight found for highway type '%s' in profile '%s'", 
                 highway, wt_profile))
  }
  # Process xy data
  pre_process_xy_id <- pre_process_xy
  if ("id" %in% names(xy)) {
    xy_id <- xy$id
    xy$id <- NULL
  } else {
    xy_id <- NULL
  }
  xy <- dodgr:::pre_process_xy(xy)
  if (is.null(xy_id)) {
    xy_id <- vapply(seq_len(nrow(xy)), \(i) genhash(10), character(1))
  }
  xyf <- bind_cols(xy, id=xy_id)
  
  # Match to vertices
  verts <- dodgr::dodgr_vertices(graph)
  matches <- dodgr::match_points_to_verts(verts, xy[, c("x", "y")])
  
  # Create connection edges
  new_edges <- lapply(seq_len(nrow(xy)), function(i) {
    pt <- xyf[i, ]
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
      highway = highway
    )
    
    edge_rev <- edge_fwd
    edge_rev$from <- vert$id
    edge_rev$to <- pt$id
    edge_rev[, c("xfr", "yfr", "xto", "yto")] <- 
      edge_fwd[, c("xto", "yto", "xfr", "yfr")]
    
    # Apply weighting
    edges <- dodgr:::set_maxspeed(rbind(edge_fwd, edge_rev), wt_profile, wt_profile_file) |>
      dodgr:::weight_by_num_lanes(wt_profile) |>
      dodgr:::calc_edge_time(wt_profile)
    
    edges$d_weighted <- edges$d / way_wt
    edges
  }) |> 
    Filter(f = Negate(is.null)) |>
    do.call(rbind, args = _)
  
  # Merge with original graph
  result_std <- dplyr::distinct(
    dplyr::bind_rows(graph, new_edges),
    from, to, .keep_all = TRUE
  )
  
}
