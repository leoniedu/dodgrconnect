#' Connect Disjoint Components in a Graph
#'
#' This function connects disjoint components of a graph by adding edges between
#' vertices that are within a specified distance threshold. Components are processed
#' in order of size (largest first), and each component is connected to the
#' previously connected network.
#'
#' @param graph A graph object with spatial coordinates
#' @param distance_threshold Distance threshold in meters to connect components.
#' @param connection_type Type of new edges (e.g., "footway", "cycleway")
#' @param wt_profile Weighting profile to use (e.g., "foot", "bicycle")
#' @param wt_profile_file Path to profile file. If NULL, uses default profiles
#' @param surface Surface type for new connections (e.g., "paved", "asphalt")
#' @param n_components Maximum number of components to connect. Default Inf connects all
#'
#' @return Updated graph with new edges connecting components
#' @export
#'
#' @examples
#' graph <- dodgr::weight_streetnet(dodgr::hampi)
#' graph_connected <- connect_components(
#'     graph,
#'     distance_threshold = 200,
#'     connection_type = "footway",
#'     wt_profile = "foot",
#'     surface = "paved"
#' )
connect_components <- function(graph, 
                             distance_threshold = 20,
                             connection_type,
                             wt_profile,
                             wt_profile_file = NULL,
                             surface,
                             n_components = Inf) {
    ##TODO: order the components using minimum spanning tree
    # Find minimum spanning tree
    # g <- igraph::graph_from_adjacency_matrix(distances, weighted = TRUE, mode = "undirected")
    ## Store original cache status
    cache_status <- dodgr:::is_dodgr_cache_on()
    on.exit({
        if(cache_status) {
            dodgr::dodgr_cache_on()
        } else {
        dodgr::dodgr_cache_off()
        }
    })
    
    ## cache must be off/empty
    dodgr::dodgr_cache_off()
    dodgr::clear_dodgr_cache()
    
    # Input validation
    if (missing(connection_type) || missing(wt_profile) || missing(surface)) {
        stop("connection_type, wt_profile, and surface must be specified")
    }

    # Verify connection_type has valid speed in profile
    wp <- dodgr:::get_profile(wt_profile, wt_profile_file)
    way_speed <- wp$max_speed[wp$way == connection_type]
    if (length(way_speed) == 0 || way_speed <= 0) {
        stop(sprintf("No valid speed found for connection_type '%s' in profile '%s'", 
                    connection_type, wt_profile))
    }
    
    graph$edge_id <- as.character(graph$edge_id)
    ## cannot be named *component* or it conflicts with e.g. dodgr_to_sf
    graph$original_cmp <- graph$component
    graph$is_connector <- FALSE
    
    # Get vertices
    vertices <- dodgr::dodgr_vertices(graph)
    vertices$original_cmp <- vertices$component
    
    # Get max component number and limit by n_components
    max_comp <- min(max(vertices$original_cmp), n_components + 1)
    if (is.na(max_comp) || max_comp <= 1) {
        return(graph)  # Nothing to connect
    }
    
    # For each smaller component (2 up to max_comp)
    for (comp_id in 2:max_comp) {
        comp_verts <- vertices[vertices$original_cmp == comp_id, ]
        ## FIX: following line, even if components did not connect, still 
        ## trying to connect to them.
        main_verts <- vertices[vertices$original_cmp < comp_id, ]
        # Find all possible connections within threshold
        dists <- geodist::geodist(
            data.frame(lon = comp_verts$x, lat = comp_verts$y),
            data.frame(lon = main_verts$x, lat = main_verts$y),
            measure = "geodesic"
        )
        
        close_pairs <- which(dists <= distance_threshold, arr.ind = TRUE)
        
        if (nrow(close_pairs) == 0) {
            warning(sprintf("No connections found within %d meters between component %d and previously connected network", 
                          distance_threshold, comp_id))
            next
        }
        
        # Get profile weight for connection_type
        wp <- dodgr:::get_profile(wt_profile, wt_profile_file)
        way_wt <- wp$value[wp$way == connection_type]
        if (length(way_wt) == 0) {
            stop(sprintf("No weight found for connection_type '%s' in profile '%s'", 
                        connection_type, wt_profile))
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
        
        # Add all new edges to graph
        graph <- dplyr::bind_rows(data.frame(graph), new_edges)
        
        # Update components in graph
        graph$component[graph$original_cmp == comp_id] <- 1
    }
    
    # Check if we still have multiple components at the end
    n_remaining <- length(unique(graph$component))
    if (n_remaining > 1) {
        warning(sprintf("%d components remain disconnected", n_remaining - 1))
    }
    
    return(graph)
}
