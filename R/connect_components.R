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
connect_components <- function(graph, 
                             distance_threshold = 20,
                             connection_type,
                             wt_profile,
                             wt_profile_file = NULL,
                             surface,
                             distance_matrix = NULL) {
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
    
    # Get max component number 
    max_comp <- max(vertices$original_cmp)
    
    # Calculate distances between components if not provided
    if (is.null(distance_matrix)) {
        # Get full distance matrix without threshold
        distance_matrix <- suppressWarnings(component_distances(graph))
    }

    # Get unique components
    components <- sort(unique(vertices$original_cmp))
    if (length(components) < 2) {
        return(graph)  # Nothing to connect
    }
    
    # Set distances > threshold to Inf before creating MST
    distance_matrix[distance_matrix > distance_threshold] <- Inf
    
    # Check if all components can be connected within threshold
    g_test <- igraph::graph_from_adjacency_matrix(distance_matrix, weighted = TRUE, 
                                                mode = "undirected")
    # Create MST from distance matrix to determine connection sequence
    mst <- igraph::mst(g_test)
    mst_edges <- igraph::as_edgelist(mst)
    mst_edges_df <- data.frame(mst_edges)
    names(mst_edges_df) <- c("component1", "component2")
    distance_l <- data.frame(which(distance_matrix>distance_threshold, arr.ind = TRUE))
    names(distance_l) <- c("component1", "component2")
    mst_edges_test <- merge(mst_edges_df, distance_l)
    if(nrow(mst_edges_test)>0) {
        cli::cli_abort("component connection {mst_edges_test} distance above threshold")
    }
    connected_comps <- unique(as.integer(mst_edges))
        # If no edges in MST, return original graph
    if (nrow(mst_edges) == 0) {
        return(graph)
    }
    
    # Initialize list to store edges that will connect components
    new_edges <- data.frame(
        from_id = character(0),
        to_id = character(0),
        from_lon = numeric(0),
        from_lat = numeric(0),
        to_lon = numeric(0),
        to_lat = numeric(0),
        d = numeric(0),
        d_weighted = numeric(0),
        highway = character(0),
        way_id = character(0),
        edge_id = character(0),
        original_cmp = integer(0),
        is_connector = logical(0),
        stringsAsFactors = FALSE
    )
    
    # Track components that couldn't be connected
    min_distances <- numeric()
    
    # For each pair of components in the MST
    for (i in seq_len(nrow(mst_edges))) {
        comp1 <- as.integer(mst_edges[i, 1])
        comp2 <- as.integer(mst_edges[i, 2])
        
        # Get vertices for each component
        vert1 <- vertices[vertices$component == comp1, ]
        vert2 <- vertices[vertices$component == comp2, ]
        
        # Convert to sf objects for distance calculation
        vert1_sf <- sf::st_as_sf(vert1, coords = c("x", "y"), crs = 4326)
        vert2_sf <- sf::st_as_sf(vert2, coords = c("x", "y"), crs = 4326)
        
        # Calculate all pairwise distances
        dists <- sf::st_distance(vert1_sf, vert2_sf)
        
        # Convert threshold to units object
        dist_threshold <- units::set_units(distance_threshold, "m")
        
        # Find all pairs within threshold
        close_pairs <- which(dists <= dist_threshold, arr.ind = TRUE)
        
        # Store minimum distance if components can't be connected
        min_dist <- min(dists)
        if (nrow(close_pairs) == 0) {
            min_distances <- c(min_distances, units::drop_units(min_dist))
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
            edges <- data.frame(
                from_id = vert1$id[close_pairs[, 1]],
                to_id = vert2$id[close_pairs[, 2]],
                from_lon = vert1$x[close_pairs[, 1]],
                from_lat = vert1$y[close_pairs[, 1]],
                to_lon = vert2$x[close_pairs[, 2]],
                to_lat = vert2$y[close_pairs[, 2]],
                d = units::drop_units(dists[close_pairs]),
                d_weighted = units::drop_units(dists[close_pairs]) / way_wt,  # Initial weighting based on profile
                highway = connection_type,
                way_id = NA_character_,
                edge_id = paste0("conn_", comp1, "_", seq_len(nrow(close_pairs))),
                original_cmp = comp1,
                is_connector = TRUE,
                stringsAsFactors = FALSE
            )
            # Add reverse edges
            edges_rev <- data.frame(
                from_id = vert2$id[close_pairs[, 2]],
                to_id = vert1$id[close_pairs[, 1]],
                from_lon = vert2$x[close_pairs[, 2]],
                from_lat = vert2$y[close_pairs[, 2]],
                to_lon = vert1$x[close_pairs[, 1]],
                to_lat = vert1$y[close_pairs[, 1]],
                d = units::drop_units(dists[close_pairs]),
                d_weighted = units::drop_units(dists[close_pairs]) / way_wt,  # Initial weighting based on profile
                highway = connection_type,
                way_id = NA_character_,
                edge_id = paste0("conn_", comp1, "_", seq_len(nrow(close_pairs))),
                original_cmp = comp1,
                is_connector = TRUE,
                stringsAsFactors = FALSE
            )
            new_edges <- rbind(new_edges, edges, edges_rev)
        }
    }
    
    # If no new edges were found, return original graph with warning
    if (nrow(new_edges) == 0) {
        return(graph)
    }
    
    # Calculate weights using built-in functions
    new_edges <- dodgr:::set_maxspeed(new_edges, wt_profile, wt_profile_file) |>
        dodgr:::weight_by_num_lanes(wt_profile) |>
        dodgr:::calc_edge_time(wt_profile)
    
    # Add surface type
    new_edges$surface <- surface
    
    # Store original graph class
    graph_class <- class(graph)
    
    # Ensure new_edges has same columns as graph
    missing_cols <- setdiff(names(graph), names(new_edges))
    for (col in missing_cols) {
        new_edges[[col]] <- NA
    }
    
    # Ensure columns are in same order
    new_edges <- new_edges[, names(graph)]
    
    # Combine original graph with new edges and restore class
    result <- rbind(graph, new_edges)
    
    # Update component IDs for all vertices in connected components
    # First, create a mapping of old component IDs to new ones
    comp_mapping <- stats::setNames(rep(min(connected_comps), length(connected_comps)), connected_comps)
    
    # Then update the component column using this mapping
    result$component <- ifelse(result$original_cmp %in% names(comp_mapping),
                             comp_mapping[as.character(result$original_cmp)],
                             result$original_cmp)
    class(result) <- graph_class
    
    # Get components that were successfully connected (in components to connect but not in disconnected pairs)
    disconnected_comps <- setdiff(components, connected_comps)
    # Issue warning if some components couldn't be connected
    if (length(disconnected_comps) > 0) {
        warning("Some components could not be connected due to distance_threshold: ", disconnected_comps)
    }
    
    return(result)
}

