#' Connect Graph Components
#'
#' This function connects disconnected components of a graph by adding edges between
#' vertices that are within a specified distance threshold. For graphs with more than
#' two components, it uses a minimum spanning tree (MST) approach to determine the
#' optimal set of connections between components.
#'
#' The function uses spatial indexing via sf::st_is_within_distance for efficient
#' distance calculations between vertices.
#'
#' @param graph A data frame representing a graph with at least x, y coordinates
#' @param distance_threshold Distance threshold in meters for connecting components
#' @param connection_type Type of way to use for connecting components (e.g., "path", "footway")
#' @param wt_profile Weight profile to use for the new edges
#' @param wt_profile_file Optional path to a weight profile file
#' @param surface Surface type for the new edges
#' @param components_to_join Components to connect
#' @return A modified graph with components connected
#' @importFrom sf st_as_sf st_is_within_distance
#' @importFrom units set_units
#' @noRd
connect_components3 <- function(graph, 
                             distance_threshold = 20,
                             connection_type,
                             wt_profile,
                             wt_profile_file = NULL,
                             surface = "paved",
                             components_to_join) {
    # Store original graph class
    graph_class <- class(graph)
    
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
    
    # Get unique components
    components <- unique(vertices$original_cmp)
    if (!all(components_to_join %in% components)) {
        stop("Some components_to_join are not present in the graph")
    }
    if (length(components) < 2) {
        stop("Nothing to connect")
    }

    # If no components to connect, return original graph
    if (length(components_to_join) == 1) {
        stop("Nothing to connect")
    }
    
    # to_connect <- tidyr::expand_grid(component_i=components_to_join, component_j=components_to_join)%>%
    #     arrange(component_i, component_j)%>%
    #     filter(component_i<component_j)
    # Track components that couldn't be connected
    #min_distances <- numeric()
    
    # For each pair of components in the components to connect
    comp1 <- components_to_join[1]
    connected_comps <- comp1
    graph0 <- graph[!graph$component %in%components_to_join, ] 
    graph1 <- graph[graph$component == comp1, ] 
    for (comp2 in components_to_join[-1]) {
        # Get vertices for each component
        vert1 <- dodgr_vertices(graph1)
        vert2 <- vertices[vertices$component == comp2, ]
        graph2 <- graph[graph$component == comp2, ] 
        # Convert to sf objects for distance calculation
        vert1_sf <- sf::st_as_sf(vert1, coords = c("x", "y"), crs = 4326, remove = FALSE)
        vert2_sf <- sf::st_as_sf(vert2, coords = c("x", "y"), crs = 4326, remove = FALSE)
        
        # Convert threshold to units object
        dist_threshold <- units::set_units(distance_threshold, "m")
        
        # Find close pairs using spatial indexing
        pairs <- sf::st_is_within_distance(vert1_sf, vert2_sf, 
                                         dist = dist_threshold, 
                                         sparse = TRUE)
        # Convert sparse format to matrix of pairs
        n_pairs <- sum(lengths(pairs))
        close_pairs <- if (n_pairs == 0) {
            matrix(ncol = 2, nrow = 0)
        } else {
            result <- matrix(ncol = 2, nrow = n_pairs)
            idx <- 1
            for (i in seq_along(pairs)) {
                matches <- pairs[[i]]
                if (length(matches) > 0) {
                    n <- length(matches)
                    result[idx:(idx + n - 1), ] <- cbind(rep(i, n), matches)
                    idx <- idx + n
                }
            }
            result
        }
        browser()
        if (nrow(close_pairs) > 0) {
            # Get profile weight for connection_type
            wp <- dodgr:::get_profile(wt_profile, wt_profile_file)
            way_wt <- wp$value[wp$way == connection_type]
            if (length(way_wt) == 0) {
                stop(sprintf("No weight found for connection_type '%s' in profile '%s'", 
                             connection_type, wt_profile))
            }
            
            # Create edges for all close pairs
            connected_comps <- c(connected_comps, comp2)
            
            # Add new nodes and connecting edges to graph1
            cat("\nBefore add_nodes_to_graph2:")
            cat("\nNumber of edges in graph1:", nrow(graph1))
            cat("\nNumber of edges in graph2:", nrow(graph2))
            
            graph1 <- add_nodes_to_graph2(graph1, vert2_sf[unique(close_pairs[,2]),] , intersections_only = FALSE,
                                          new_edge_type = connection_type,
                                          wt_profile = wt_profile,
                                          wt_profile_file = wt_profile_file,
                                          surface = surface)%>%
                bind_rows(graph2)%>%
                std_graph()%>%
                mutate(component=comp1)
        }
        class(graph1) <- graph_class
    }    
    # Get components that were successfully connected (in components to connect but not in disconnected pairs)
    disconnected_comps <- setdiff(components_to_join, connected_comps)
    # Issue warning if some components couldn't be connected
    if (length(disconnected_comps) > 0) {
        warning("Some components could not be connected due to distance_threshold: ", disconnected_comps)
    }
    bind_rows(graph0, graph1)%>%
        std_graph()
}
