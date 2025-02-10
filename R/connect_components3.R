#' Connect Graph Components
#'
#' This function connects multiple components of a graph by identifying vertices that
#' are within a specified distance threshold and adding edges between them. It uses
#' spatial indexing for efficient pair identification and add_nodes_to_graph2 for
#' proper edge creation.
#'
#' The function processes components sequentially, starting with the first component
#' and attempting to connect each subsequent component to the growing connected
#' component. For each pair of components, it:
#' 1. Uses spatial indexing via sf::st_is_within_distance to efficiently find
#'    vertex pairs that are within the distance threshold
#' 2. Uses add_nodes_to_graph2 to properly add the vertices and edges with correct
#'    weights and properties
#' 3. Combines the graphs and updates component IDs
#'
#' @param graph A data frame or dodgr_streetnet object containing the graph
#' @param distance_threshold Maximum distance in meters to consider for connecting vertices
#' @param connection_type Type of way to use for connecting components (e.g., "path", "footway")
#' @param wt_profile Weight profile to use for calculating edge weights and times
#' @param wt_profile_file Optional path to a custom weight profile file
#' @param surface Surface type for the new edges (e.g., "paved")
#' @param components_to_join Integer vector specifying the component IDs to connect in sequence
#'
#' @return A modified graph with the specified components connected. Components are
#'         connected in sequence, with each subsequent component being connected to
#'         the growing connected component if possible. Component IDs are updated
#'         to reflect the new connections. If a component cannot be connected
#'         (no vertices within distance_threshold), it remains unchanged.
#'
#' @examples
#' \dontrun{
#' library(dodgr)
#' # Load a street network
#' net <- weight_streetnet(hampi)
#' 
#' # Connect components 1, 2, and 3 with footway edges
#' net_connected <- connect_components3(
#'   graph = net,
#'   distance_threshold = 50,  # 50 meters
#'   connection_type = "footway",
#'   wt_profile = "foot",
#'   surface = "paved",
#'   components_to_join = c(1, 2, 3)
#' )
#' }
#'
#' @importFrom sf st_as_sf st_is_within_distance
#' @export
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
        cli::cli_inform("Trying to join {comp1} to {comp2}")
        # Get vertices for each component
        vert1 <- dodgr::dodgr_vertices(graph1)
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
            cli::cli_inform("Not possible to join {comp1} to {comp2}")
            matrix(ncol = 2, nrow = 0)
        } else {
            cli::cli_inform("Joining {comp1} to {comp2}")
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
                                          surface = surface, max_length = dist_threshold*1.1)%>%
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
