#' Connect Graph Components (Simplified Version)
#'
#' This function connects disconnected components of a graph by adding edges between
#' vertices that are within a specified distance threshold. It directly connects vertices
#' between components without using a minimum spanning tree approach.
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
connect_components_simple <- function(graph, 
                                    distance_threshold = 20,
                                    connection_type,
                                    wt_profile,
                                    wt_profile_file = NULL,
                                    surface = "paved",
                                    components_to_join) {
    # Store original graph class
    graph_class <- class(graph)
    checkmate::check_integerish(components_to_join, lower=1, len=2)
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

    # Ensure edge_id is character and add is_connector flag
    graph$edge_id <- as.character(graph$edge_id)
    graph$is_connector <- FALSE

    # Get vertices with proper IDs
    vertices <- dodgr::dodgr_vertices(graph)
    vertices$original_cmp <- vertices$component
    
    # Get unique components
    components <- unique(vertices$component)
    if (!all(components_to_join %in% components)) {
        stop("Some components_to_join are not present in the graph")
    }
    if (length(components_to_join) < 2) {
        stop("Nothing to connect")
    }

    # Get weight for connection_type
    way_wt <- wp$value[wp$way == connection_type]
    if (length(way_wt) == 0) {
        stop(sprintf("No weight found for connection_type '%s' in profile '%s'", 
                    connection_type, wt_profile))
    }

    # Convert threshold to units object
    dist_threshold <- units::set_units(distance_threshold, "m")
    
    # Create empty list to store new edges
    new_edges <- list()

    cmp_joins <- data.frame(from=0, to=0)[0,]
    # For each pair of components
    #for (i in 1:(length(components_to_join)-1)) {
    for (i in 1) {
        comp1 <- components_to_join[i]
        
        for (j in (i+1):length(components_to_join)) {
            comp2 <- components_to_join[j]
            cli::cli_inform("Trying to join {comp1} to {comp2}")
            
            # Get vertices for each component
            vert1 <- vertices[vertices$component == comp1, ]
            vert2 <- vertices[vertices$component == comp2, ]
            
            # Convert vert2 to a matrix for match_pts_to_verts
            xy <- as.matrix(vert2[, c("x", "y")])
            colnames(xy) <- c("x", "y")
            
            # Create vertices matrix from component 1
            # Find closest vertices in component 1 for each vertex in component 2
            matches <- dodgr::match_pts_to_verts(
                verts = vert1,  # First argument: vertices from comp1
                xy = xy,        # Second argument: points from comp2
                connected = FALSE
            )
            
            # Calculate distances using sf
            p1 <- sf::st_as_sf(vert1[matches,], coords = c("x", "y"), crs=4326)
            p2 <- sf::st_as_sf(vert2[1:length(matches),], coords = c("x", "y"), crs=4326)
            distances <- as.numeric(sf::st_distance(p1, p2, by_element = TRUE))
            if (any(distances<=distance_threshold)) {
                cmp_joins <- rbind(cmp_joins, data.frame(from=comp1, to=comp2))
                cli::cli_inform("Joining {comp1} to {comp2}")
            } else {
                cli::cli_inform("Not possible to join {comp1} to {comp2}")
            }
            # Create edges for pairs within distance threshold
            for (k in seq_along(matches)) {  # k is index into pts/vert2
                if (!is.na(matches[k])) {
                    d <- distances[k]
                    if (d <= distance_threshold) {
                        # Create new edge with same structure as graph
                        new_edge <- data.frame(
                            from_id = vert1$id[matches[k]],  # Index into vert1
                            to_id = vert2$id[k],                  # Index into vert2
                            from_lon = vert1$x[matches[k]],  # Index into vert1
                            from_lat = vert1$y[matches[k]],  # Index into vert1
                            to_lon = vert2$x[k],                  # Index into vert2
                            to_lat = vert2$y[k],                  # Index into vert2
                            d = d,
                            highway = connection_type,
                            edge_id = paste0("connector_", comp1, "_", comp2, "_", matches[k], "_", k),
                            component = comp1,
                            original_cmp = NA_integer_,
                            is_connector = TRUE,
                            stringsAsFactors = FALSE
                        )

                        # Calculate time and weights using dodgr internal functions
                        new_edge <- dodgr:::set_maxspeed(new_edge, wt_profile, wt_profile_file) |>
                            dodgr:::weight_by_num_lanes(wt_profile) |>
                            dodgr:::calc_edge_time(wt_profile)

                        # Create reverse edge for bidirectional connection
                        rev_edge <- new_edge
                        rev_edge$edge_id <- paste0(new_edge$edge_id, "_rev")
                        rev_edge$from_id <- new_edge$to_id
                        rev_edge$from_lon <- new_edge$to_lon
                        rev_edge$from_lat <- new_edge$to_lat
                        rev_edge$to_id <- new_edge$from_id
                        rev_edge$to_lon <- new_edge$from_lon
                        rev_edge$to_lat <- new_edge$from_lat

                        # Add both edges to the graph
                        next_idx <- length(new_edges) + 1
                        new_edges[[next_idx]] <- new_edge
                        new_edges[[next_idx + 1]] <- rev_edge
                    }
                }
            }
        }
    }
    
    # If no new edges were created, return original graph
    if (length(new_edges) == 0) {
        warning("No components could be connected within the distance threshold")
        return(graph)
    }
    
    # Combine all new edges
    new_edges_df <- do.call(rbind, new_edges)
    # Add new edges to graph
    result <- bind_rows(graph, new_edges_df)
    class(result) <- graph_class
    attr(result, "joins") <- cmp_joins
    return(result)
}
