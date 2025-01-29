#' Add Nodes to a Graph with Enhanced Weight Handling
#'
#' An enhanced version of dodgr::add_nodes_to_graph that properly handles weights,
#' speeds, and surface types when adding new nodes to the graph.
#'
#' @param graph A dodgr graph
#' @param xy Matrix or data frame of x/y coordinates of points to add
#' @param dist_tol Distance tolerance for snapping points to edges
#' @param intersections_only If TRUE, only add points at intersections
#' @param new_edge_type Type of way to use for new edges (e.g., "path", "footway")
#' @param wt_profile Weight profile to use for new edges
#' @param wt_profile_file Optional path to custom weight profile
#' @param surface Surface type to use for new edges
#'
#' @return A dodgr graph with new nodes added
#' @export
#'
#' @examples
#' \dontrun{
#' graph <- dodgr::weight_streetnet(dodgr::hampi)
#' new_points <- data.frame(x = c(77.67, 77.68), y = c(15.33, 15.34))
#' graph_with_nodes <- add_nodes(graph, new_points)
#' }
add_nodes <- function(graph,
                     xy,
                     dist_tol = 1e-6,
                     intersections_only = FALSE,
                     new_edge_type = NULL,
                     wt_profile = "foot",
                     wt_profile_file = NULL,
                     surface = "paved") {
    
    # Input validation
    if (!is.data.frame(xy) && !is.matrix(xy)) {
        stop("'xy' must be a data.frame or matrix")
    }
    if (ncol(xy) != 2) {
        stop("'xy' must have exactly 2 columns for x and y coordinates")
    }
    
    # Convert matrix to data frame if necessary
    if (is.matrix(xy)) {
        xy <- as.data.frame(xy)
    }
    names(xy) <- c("x", "y")
    
    # Call internal implementation
    add_nodes_internal(graph = graph,
                      xy = xy,
                      dist_tol = dist_tol,
                      intersections_only = intersections_only,
                      new_edge_type = new_edge_type,
                      wt_profile = wt_profile,
                      wt_profile_file = wt_profile_file,
                      surface = surface)
}

# Helper function to generate random hash strings
genhash <- function(len = 10) {
    paste0(sample(c(letters, LETTERS, 0:9), size = len, replace = TRUE),
           collapse = "")
}

#' @keywords internal
add_nodes_internal <- function(graph,
                             xy,
                             dist_tol = 1e-6,
                             intersections_only = FALSE,
                             new_edge_type = NULL,
                             wt_profile = "foot",
                             wt_profile_file = NULL,
                             surface = "paved") {
    edges_split <- lapply(unique(index$n), function(i) {
        edges_to_split_i <- edges_to_split[which(edges_to_split$n == i), ]

        d_wt <- edges_to_split_i$d_weighted / edges_to_split_i$d
        t_scale <- edges_to_split_i$time / edges_to_split_i$d
        t_wt <- edges_to_split_i$time_weighted / edges_to_split_i$time

        new_edges_i <- lapply(seq_len(nrow(edges_to_split_i)), function(j) {
            edge_i <- edges_to_split_i[j, ]
            edge_i_new <- edge_i

            # Calculate distances between points
            pts_i <- pts[i, ]
            pts_mat <- matrix(c(edge_i$xfr, edge_i$yfr,
                              pts_i$x0, pts_i$y0,
                              edge_i$xto, edge_i$yto),
                            ncol = 2, byrow = TRUE)
            dmat <- geodist::geodist(pts_mat)
            d_i <- dmat[2, c(1, 3)]

            if (sum(d_i) > edge_i$d * (1 + sqrt(.Machine$double.eps))) {
                # Point is not on edge, so find closest point and re-map
                if (d_i[1] < d_i[2]) {
                    pts$x0[i] <- edge_i$xfr
                    pts$y0[i] <- edge_i$yfr
                    d_i <- c(0, edge_i$d)
                } else {
                    pts$x0[i] <- edge_i$xto
                    pts$y0[i] <- edge_i$yto
                    d_i <- c(edge_i$d, 0)
                }
            }

            if (any(d_i < sqrt(.Machine$double.eps))) {
                edge_i_new <- edge_i # Point is at one vertex
            } else if (abs(sum(d_i) - edge_i$d) > sqrt(.Machine$double.eps)) {
                edge_i_new <- edge_i[rep(1, 2), ]
                edge_i_new$to[1] <- edge_i_new$from[2] <- genhash(10L)
                edge_i_new$xto[1] <- edge_i_new$xfr[2] <- pts$x0[i]
                edge_i_new$yto[1] <- edge_i_new$yfr[2] <- pts$y0[i]

                edge_i_new$d[1] <- d_i[1]
                edge_i_new$d[2] <- d_i[2]

                # Re-order if necessary to maintain direction
                d_i_min <- c(1, 1, 2)[which.min(dmat[upper.tri(dmat)])]
                if (d_i_min == 1) {
                    edge_i_new <- edge_i_new[2:1, ]
                }
            } else {
                edge_i$d[1] <- dmat[1, 2]
                edge_i$d[2] <- dmat[2, 3]

                edge_i$d_weighted <- edge_i$d * d_wt
                edge_i$time <- edge_i$d * t_scale
                edge_i$time_weighted <- edge_i$time * t_wt

                edge_i$edge_id <- paste0(
                    edge_i$edge_id,
                    "_",
                    LETTERS[seq_len(nrow(edge_i))]
                )

                edge_i_new <- edge_i  # already 2 rows
            }

            if (!intersections_only) {
                # Then add edges out to new point:
                edge_i_new$from[1] <- edge_i_new$to[2] <- genhash(10L)
                edge_i_new$xfr[1] <- pts$x0[i]
                edge_i_new$yfr[1] <- pts$y0[i]
                edge_i_new$xto[2] <- pts$x0[i]
                edge_i_new$yto[2] <- pts$y0[i]

                edge_i_new$d <- d_i

                if (!is.null(new_edge_type)) {
                    # Use new edge type and surface for the connecting edges
                    edge_i_new$highway <- new_edge_type
                    edge_i_new$surface <- surface
                    
                    # Get profile weight for new_edge_type
                    wp <- dodgr:::get_profile(wt_profile, wt_profile_file)
                    way_wt <- wp$value[wp$way == new_edge_type]
                    if (length(way_wt) == 0) {
                        stop(sprintf("No weight found for new_edge_type '%s' in profile '%s'", 
                                    new_edge_type, wt_profile))
                    }
                    
                    # Set initial weighting based on profile
                    edge_i_new$d_weighted <- edge_i_new$d / way_wt
                    
                    # Apply additional weight calculations
                    edge_i_new <- dodgr:::set_maxspeed(edge_i_new, wt_profile, wt_profile_file) %>%
                        dodgr:::weight_by_num_lanes(wt_profile) %>%
                        dodgr:::calc_edge_time(wt_profile)
                } else {
                    # Inherit weights from original edge
                    edge_i_new$d_weighted <- edge_i_new$d * d_wt
                    edge_i_new$time <- edge_i_new$d * t_scale
                    edge_i_new$time_weighted <- edge_i_new$time * t_wt
                }

                edge_i_new$edge_id <- vapply(
                    seq_len(nrow(edge_i_new)),
                    function(i) genhash(10),
                    character(1L)
                )

                # New edges (connecting to points) should have NA original_edge_id
                edge_i_new$original_edge_id <- if (edge_id_type == "character") NA_character_ else NA_integer_

                edge_i <- dplyr::bind_rows(edge_i, edge_i_new)
            }

            return(edge_i)
        })
        return(dplyr::bind_rows(new_edges_i))
    })

    edges_split <- do.call(rbind, edges_split)

    # Then match edges_split back on to original graph:
    graph_to_add <- graph_to_add[edges_split$n, ]
    gr_cols <- gr_cols[which(!is.na(gr_cols))]
    for (g in seq_along(gr_cols)) {
        graph_to_add[, gr_cols[g]] <- edges_split[[names(gr_cols)[g]]]
    }
    if (!is.null(new_edge_type)) {
        graph_to_add$highway <- dplyr::coalesce(edges_split$highway, graph_to_add$highway)
    }
    if (!intersections_only) {
        graph_to_add$original_edge_id <- edges_split$original_edge_id
    }
    return(dplyr::bind_rows(graph, graph_to_add))
}
