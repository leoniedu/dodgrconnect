#' Connect Graph Components
#'
#' This function connects disconnected components of a graph by adding edges between
#' vertices that are within a specified distance threshold. For graphs with more than
#' two components, it uses a minimum spanning tree (MST) approach to determine the
#' sequence of components to connect, starting from component 1 (assumed to be the
#' largest component). For each pair of components in the MST sequence, it adds edges
#' between all vertex pairs that are within the threshold distance.
#'
#' @param graph A data frame or dodgr_streetnet object with columns 'from_id', 'to_id',
#'        and 'component'. Vertex coordinates will be obtained from the graph using
#'        dodgr_vertices(), which expects coordinates in the graph's edge columns
#'        (from_lon, from_lat, to_lon, to_lat).
#' @param distance_threshold Distance threshold in meters. Vertices from different
#'        components that are closer than this threshold will be connected. Components
#'        that are farther apart than this threshold will remain disconnected, and a
#'        warning will be issued with the minimum distances between components.
#' @param distance_matrix Optional pre-computed distance matrix between components.
#'        If NULL (default), it will be calculated using component_distances().
#'        The matrix should have distances greater than distance_threshold set to NA.
#' @param connection_type Type of connection to create between components (e.g.,
#'        'footway', 'residential'). Must be a valid highway type in the weight profile.
#' @param wt_profile Weight profile to use for the new edges (e.g., 'foot', 'bicycle').
#' @param wt_profile_file Optional path to a custom weight profile JSON file.
#' @param surface Surface type for the new edges (e.g., 'paved', 'unpaved').
#'
#' @return A modified version of the input graph with new edges connecting components
#'         that are within distance_threshold of each other. The returned graph will
#'         have the same class and structure as the input graph. Components that could
#'         not be connected (due to being farther apart than distance_threshold) will
#'         remain disconnected.
#'
#' @details
#' The function follows these steps:
#' 1. Gets vertex coordinates from the graph using dodgr_vertices()
#' 2. Calculates (or uses provided) distances between all components
#' 3. For graphs with more than 2 components:
#'    - Creates a minimum spanning tree (MST) starting from component 1
#'    - Uses the MST to determine the sequence of components to connect
#' 4. For each component pair in the sequence:
#'    - Finds all vertex pairs within distance_threshold
#'    - Creates bidirectional edges between these vertices
#'    - Sets appropriate weights based on the weight profile
#' 5. Updates component IDs in the final graph
#'
#' The MST approach ensures that components are connected in a way that minimizes
#' the total distance of connections while maintaining connectivity.
#'
#' @examples
#' \dontrun{
#' library(dodgr)
#' net <- weight_streetnet(hampi)
#' 
#' # Connect components that are within 100 meters of each other
#' connected_net <- connect_components(net, 
#'                                   distance_threshold = 100,
#'                                   connection_type = "footway",
#'                                   wt_profile = "foot",
#'                                   surface = "paved")
#'                                   
#' # Use pre-computed distance matrix
#' dist_mat <- component_distances(net, distance_threshold = 100)
#' connected_net <- connect_components(net,
#'                                   distance_threshold = 100,
#'                                   distance_matrix = dist_mat,
#'                                   connection_type = "footway",
#'                                   wt_profile = "foot",
#'                                   surface = "paved")
#' }
#' @export
#' @importFrom cli cli_inform cli_progress_step
#' @importFrom glue glue
#' @importFrom checkmate assert_data_frame assert_subset assert_numeric assert_number test_class assert_flag
#' @importFrom dodgr dodgr_cache_off dodgr_cache_on
#' @importFrom dplyr bind_rows mutate %>%
#' @import dplyr
connect_components3 <- function(graph, 
                             distance_threshold = 20,
                             connection_type,
                             wt_profile,
                             wt_profile_file = NULL,
                             surface, components_to_join) {
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
        
        # Determine which set to iterate over (the smaller one)
        if (nrow(vert1_sf) <= nrow(vert2_sf)) {
            base_sf <- vert1_sf
            target_sf <- vert2_sf
            swap_indices <- FALSE
        } else {
            base_sf <- vert2_sf
            target_sf <- vert1_sf
            swap_indices <- TRUE
        }
        
        # Initialize close_pairs
        close_pairs <- matrix(ncol = 2, nrow = 0)
        
        # Process one vertex at a time
        for (i in seq_len(nrow(base_sf))) {
            # Calculate distances from this vertex to all vertices in target_sf
            dists <- sf::st_distance(base_sf[i,], target_sf)
            # Find indices where distance is below threshold
            close_idx <- which(dists <= dist_threshold)
            if (length(close_idx) > 0) {
                # Create pairs matrix for this vertex
                new_pairs <- cbind(rep(i, length(close_idx)), close_idx)
                # Add to existing pairs
                close_pairs <- rbind(close_pairs, new_pairs)
            }
        }
        
        # Swap indices back if we swapped the vertex sets
        if (swap_indices) {
            close_pairs <- close_pairs[, c(2,1), drop = FALSE]
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
