#' Connect Graph Components with Enhanced Weight Handling
#'
#' An enhanced version of dodgr::connect_components that properly handles weights,
#' speeds, and surface types when connecting disconnected components.
#'
#' @param graph A dodgr graph
#' @param method Method to use for connecting components; one of "shortest" or
#'        "centroid" (default)
#' @param connect_dist Maximum distance to connect components
#' @param wt_profile Weight profile to use for the connections
#' @param wt_profile_file Optional path to custom weight profile
#' @param connection_type Type of way to use for connections (e.g., "path", "footway")
#' @param surface Surface type to use for connections
#'
#' @return A dodgr graph with components connected
#' @export
#'
#' @examples
#' \dontrun{
#' graph <- dodgr::weight_streetnet(dodgr::hampi)
#' graph_connected <- connect_components(graph)
#' }
connect_components <- function(graph,
                             method = "centroid",
                             connect_dist = 100,
                             wt_profile = "foot",
                             wt_profile_file = NULL,
                             connection_type = "path",
                             surface = "paved") {
    # Get profile weight for connection_type
    wp <- dodgr:::get_profile(wt_profile, wt_profile_file)
    way_wt <- wp$value[wp$way == connection_type]
    if (length(way_wt) == 0) {
        stop(sprintf("No weight found for connection_type '%s' in profile '%s'",
                    connection_type, wt_profile))
    }

    # Store original cache status and revert on exit
    cache_on <- getOption("dodgr_cache_on")
    on.exit(options(dodgr_cache_on = cache_on))
    options(dodgr_cache_on = FALSE)

    # Get components
    comps <- dodgr::dodgr_components(graph)
    if (length(unique(comps$component)) == 1) {
        return(graph)
    }

    # Get vertices for each component
    verts <- dodgr::dodgr_vertices(graph)
    verts$component <- comps$component[match(verts$id, comps$id)]
    
    # Process each component except the largest one
    comp_sizes <- table(verts$component)
    main_comp <- as.numeric(names(comp_sizes)[which.max(comp_sizes)])
    main_verts <- verts[verts$component == main_comp, ]
    
    other_comps <- setdiff(unique(verts$component), main_comp)
    
    for (comp_id in other_comps) {
        comp_verts <- verts[verts$component == comp_id, ]
        
        # Calculate distances between component vertices and main component
        if (method == "centroid") {
            # Use component centroid
            comp_center <- c(mean(comp_verts$x), mean(comp_verts$y))
            main_center <- c(mean(main_verts$x), mean(main_verts$y))
            
            # Find closest vertices to centroids
            comp_dists <- geodist::geodist(comp_verts[, c("x", "y")],
                                         matrix(comp_center, ncol = 2))
            main_dists <- geodist::geodist(main_verts[, c("x", "y")],
                                         matrix(main_center, ncol = 2))
            
            comp_idx <- which.min(comp_dists)
            main_idx <- which.min(main_dists)
            
            # Calculate distance between selected vertices
            close_pairs <- matrix(c(comp_idx, main_idx), ncol = 2)
            dists <- geodist::geodist(comp_verts[comp_idx, c("x", "y")],
                                    main_verts[main_idx, c("x", "y")])
        } else {
            # Find shortest connections between components
            dists <- geodist::geodist(comp_verts[, c("x", "y")],
                                    main_verts[, c("x", "y")])
            close_pairs <- which(dists <= connect_dist, arr.ind = TRUE)
            
            if (nrow(close_pairs) == 0) {
                # If no connections within threshold, use single shortest connection
                min_idx <- which.min(dists)
                close_pairs <- matrix(c((min_idx - 1) %% nrow(comp_verts) + 1,
                                     (min_idx - 1) %/% nrow(comp_verts) + 1),
                                   ncol = 2)
                dists <- dists[close_pairs]
            }
        }
        
        # Create edges for all connections within threshold
        new_edges <- data.frame(
            from_id = comp_verts$id[close_pairs[, 1]],
            to_id = main_verts$id[close_pairs[, 2]],
            from_lon = comp_verts$x[close_pairs[, 1]],
            from_lat = comp_verts$y[close_pairs[, 1]],
            to_lon = main_verts$x[close_pairs[, 2]],
            to_lat = main_verts$y[close_pairs[, 2]],
            d = dists[close_pairs],
            d_weighted = dists[close_pairs] / way_wt,  # Initial weighting based on profile
            highway = connection_type,
            way_id = NA_character_,
            edge_id = paste0("conn_", comp_id, "_", seq_len(nrow(close_pairs))),
            component = 1,
            original_cmp = comp_id,
            is_connector = TRUE
        )
        
        # Add surface type
        new_edges$surface <- surface
        
        # Calculate weights using built-in functions
        new_edges <- dodgr:::set_maxspeed(new_edges, wt_profile, wt_profile_file) %>%
            dodgr:::weight_by_num_lanes(wt_profile) %>%
            dodgr:::calc_edge_time(wt_profile)
        
        # Add new edges to graph
        graph <- dplyr::bind_rows(graph, new_edges)
    }
    
    # Update component numbers
    comps <- dodgr::dodgr_components(graph)
    graph$component <- comps$component[match(graph$from_id, comps$id)]
    
    return(graph)
}
