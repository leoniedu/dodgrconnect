#' Connect Graph Components (Simplified Version)
#'
#' This function connects two components of a graph by adding edges between
#' vertices that are within a specified distance threshold. It identifies the
#' closest vertices between components and creates bidirectional edges to connect them.
#'
#' The function uses spatial indexing and sf package for efficient distance calculations
#' between vertices. New edges inherit properties from the weight profile specified.
#'
#' @param graph A data frame or dodgr_streetnet object containing the graph
#' @param distance_threshold Maximum distance in meters to consider for connecting vertices
#' @param connection_type Type of way to use for connecting components (e.g., "path", "footway")
#' @param wt_profile Weight profile to use for calculating edge weights and times
#' @param wt_profile_file Optional path to a custom weight profile file
#' @param components_to_join Integer vector of length 2 specifying the two component IDs to connect
#'
#' @return A modified graph with the specified components connected. If components are
#'         successfully connected, their component IDs are updated to match the first component.
#'         If connection is not possible within the distance threshold, the graph remains unchanged.
#'
#' @details
#' The function performs the following steps:
#' 1. Validates inputs and checks if the connection_type exists in the weight profile
#' 2. For each vertex in the second component, finds the closest vertex in the first component
#' 3. If any pair of vertices are within distance_threshold, creates bidirectional edges
#' 4. New edges are created with proper weights and times using dodgr's internal functions
#' 5. Updates component IDs to reflect the new connection
#'
#' @examples
#' \dontrun{
#' library(dodgr)
#' # Load a street network
#' net <- weight_streetnet(hampi)
#' 
#' # Connect components 1 and 2 with footway edges
#' net_connected <- connect_components_simple(
#'   graph = net,
#'   distance_threshold = 50,  # 50 meters
#'   connection_type = "footway",
#'   wt_profile = "foot",
#'   components_to_join = c(1, 2)
#' )
#' }
#'
#' @importFrom sf st_as_sf st_is_within_distance
#' @export
connect_components_simple <- function(graph, 
                                    distance_threshold = 20,
                                    connection_type,
                                    wt_profile,
                                    wt_profile_file = NULL,
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
    if (missing(connection_type) || missing(wt_profile)) {
        stop("connection_type, and wt_profile must be specified")
    }
    checkmate::assert_integerish(components_to_join, lower=1, len=2)

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

    # For each pair of components
    #for (i in 1:(length(components_to_join)-1)) {
    for (i in 1) {
        comp1 <- components_to_join[i]
        cmp_joined <- comp1
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
                cmp_joined <- c(cmp_joined, comp2)
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
                            d_weighted = d / way_wt, 
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
    result$component[result$component%in%cmp_joined] <- cmp_joined[1]
    class(result) <- graph_class
    return(result)
}
