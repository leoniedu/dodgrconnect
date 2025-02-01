# Helper function to generate random hash strings
genhash <- function (len = 10) {
    paste0 (sample (c (letters, LETTERS, 0:9), size = len, replace = TRUE),
            collapse = "")
}

#' Insert new nodes into a graph with custom edge weights
#'
#' This function extends `dodgr::add_nodes_to_graph` by adding the ability to specify
#' custom weights for new edges. While the original function inherits weights from
#' parent edges, this version allows customization through weight profiles.
#'
#' This inserts new nodes by extending lines from each input point to the edge
#' with the closest point of perpendicular intersection. That edge is then split
#' at that point of intersection, creating two new edges (or four for directed
#' edges). If `intersections_only = FALSE` (default), then additional edges are
#' inserted from those intersection points to the input points. If
#' `intersections_only = TRUE`, then nodes are added by splitting graph edges at
#' points of nearest perpendicular intersection, without adding additional edges
#' out to the actual input points.
#'
#' In the former case, the properties of those new edges, such as distance and
#' time weightings, are inherited from the edges which are intersected, and may
#' need to be manually modified after calling this function.
#'
#' @section Key Differences from dodgr::add_nodes_to_graph:
#' While the original function inherits all weights from parent edges, this version:
#' - Allows specifying custom highway types for new connecting edges
#' - Supports weight profile calculations for the new edges
#' - Enables custom surface types
#' - Falls back to inherited weights if no custom weights are specified
#'
#' @param graph A `data.frame` or `dodgr_streetnet` containing the graph edges
#' @param xy A `data.frame` or `matrix` of points to add. If a matrix, must have 2 columns
#'        for x and y coordinates. If a data.frame, must include 'x' and 'y' columns.
#' @param dist_tol Minimum distance used to identify points lying directly on edges,
#'        expressed in units of the distance column of `graph`
#' @param intersections_only If `FALSE` (default), adds edges to connect new points.
#'        If `TRUE`, only splits edges at intersection points.
#' @param new_edge_type Optional highway type for new edges connecting points to the network.
#'        If NULL (default), inherits type from parent edges.
#' @param wt_profile Name of weighting profile to use when new_edge_type is specified.
#'        Required if new_edge_type is specified. Common values: "foot", "bicycle", "motorcar".
#' @param wt_profile_file Name of locally-stored, `.json`-formatted version of weighting
#'        profiles. If NULL (default), uses the default profiles.
#' @param surface Surface type for new edges. Required if new_edge_type is specified.
#'        Common values: "paved", "unpaved", "asphalt".
#'
#' @return A modified version of `graph`, with additional edges formed by
#'         breaking previous edges at nearest perpendicular intersections with the
#'         points, `xy`. When new_edge_type is specified, connecting edges use the
#'         specified weights and profiles.
#'
#' @examples
#' library(dodgr)
#' graph <- weight_streetnet(hampi, wt_profile = "foot")
#' dim(graph)
#'
#' verts <- dodgr_vertices(graph)
#' set.seed(2)
#' npts <- 10
#' xy <- data.frame(
#'     x = min(verts$x) + runif(npts) * diff(range(verts$x)),
#'     y = min(verts$y) + runif(npts) * diff(range(verts$y))
#' )
#'
#' # Add points with inherited weights (like dodgr::add_nodes_to_graph)
#' graph1 <- add_nodes_to_graph2(graph, xy)
#'
#' # Add points with custom footpath weights
#' graph2 <- add_nodes_to_graph2(graph, xy,
#'                              new_edge_type = "footway",
#'                              wt_profile = "foot",
#'                              surface = "paved")
#' @export
add_nodes_to_graph2 <- function (graph,
                                 xy,
                                 dist_tol = 1e-6,
                                 intersections_only = FALSE,
                                 new_edge_type = NULL,
                                 wt_profile = NULL,
                                 wt_profile_file = NULL,
                                 surface = NULL) {
    
    # Input validation
    checkmate::assert_data_frame(graph)
    checkmate::assert(
        checkmate::check_data_frame(xy),
        checkmate::check_matrix(xy, ncols = 2),
        combine = "or"
    )
    if (is.matrix(xy)) {
        xy <- as.data.frame(xy)
        names(xy) <- c("x", "y")
    }
    checkmate::assert_names(names(xy), must.include = c("x", "y"))
    checkmate::assert_names(names(graph), must.include = c(
        "edge_id", "from_id", "to_id", "d", "d_weighted", 
        "time", "time_weighted", "highway"
    ))
    checkmate::assert_number(dist_tol, lower = 0)
    checkmate::assert_flag(intersections_only)
    checkmate::assert_character(new_edge_type, null.ok = TRUE, len = 1)
    checkmate::assert_character(wt_profile, null.ok = TRUE, len = 1)
    checkmate::assert_character(wt_profile_file, null.ok = TRUE, len = 1)
    checkmate::assert_character(surface, null.ok = TRUE, len = 1)
    
    # Validate weight profile parameters
    if (!is.null(new_edge_type)) {
        if (is.null(wt_profile)) {
            stop("'wt_profile' must be specified when 'new_edge_type' is provided")
        }
        if (is.null(surface)) {
            stop("'surface' must be specified when 'new_edge_type' is provided")
        }
        
        # Validate weight profile exists and contains the edge type
        wp <- tryCatch(
            dodgr:::get_profile(wt_profile, wt_profile_file),
            error = function(e) stop("Error loading weight profile: ", e$message)
        )
        if (!new_edge_type %in% wp$way) {
            stop(sprintf("'%s' is not a valid edge type in weight profile '%s'",
                       new_edge_type, wt_profile))
        }
    }
    
    # Add original_edge_id if not present
    if (!"original_edge_id" %in% names(graph)) {
        graph$original_edge_id <- graph$edge_id
    }
    edge_id_type <- typeof(graph$original_edge_id)
    
    graph$edge_id <- as.character(graph$edge_id)
    pts <- dodgr::match_pts_to_graph (graph, xy, distances = TRUE)
    xy <- dodgr:::pre_process_xy (xy)
    pts$x0 <- xy[, 1]
    pts$y0 <- xy[, 2]
    
    
    # Standardize graph columns
    gr_cols <- dodgr:::dodgr_graph_cols(graph)
    gr_cols[["highway"]] <- which(names(graph)=="highway")
    gr_cols[["original_edge_id"]] <- which(names(graph)=="original_edge_id")
    gr_cols <- unlist(gr_cols[which(!is.na(gr_cols))])
    graph_std <- graph[, gr_cols]  # standardise column names
    names(graph_std) <- names(gr_cols)

    # Expand index to include all potentially bi-directional edges:
    index <- lapply(seq_along(pts$index), function(i) {
        out <- which(
            (graph_std$from == graph_std$from[pts$index[i]] &
                 graph_std$to == graph_std$to[pts$index[i]]) |
                (graph_std$from == graph_std$to[pts$index[i]] &
                     graph_std$to == graph_std$from[pts$index[i]])
        )
        cbind(rep(i, length(out)), out)
    })
    index <- data.frame(do.call(rbind, index))
    names(index) <- c("n", "index")
    
    edges_to_split <- graph_std[index$index, ]
    graph_to_add <- graph[index$index, ]
    
    graph_std <- graph_std[-index$index, ]
    graph <- graph[-index$index, ]
    
    edges_to_split$n <- index$n
    
    edges_split <- lapply(unique(index$n), function(i) {
        edges_to_split_i <- edges_to_split[which(edges_to_split$n == i), ]
        d_wt <- edges_to_split_i$d_weighted / edges_to_split_i$d
        t_scale <- edges_to_split_i$time / edges_to_split_i$d
        t_wt <- edges_to_split_i$time_weighted / edges_to_split_i$time
        
        new_edges_i <- lapply(seq_len(nrow(edges_to_split_i)), function(e) {
            # Split edges either side of perpendicular points of intersection:
            edge_i <- edges_to_split_i[c(e, e), ]
            edge_i$to[1] <- edge_i$from[2] <- genhash()
            edge_i$xto[1] <- pts$x[i]
            edge_i$yto[1] <- pts$y[i]
            edge_i$xfr[2] <- pts$x[i]
            edge_i$yfr[2] <- pts$y[i]
            
            xy_i <- data.frame(
                x = as.numeric(c(edge_i[1, "xfr"], edge_i[1, "xto"], edge_i[2, "xto"])),
                y = as.numeric(c(edge_i[1, "yfr"], edge_i[1, "yto"], edge_i[2, "yto"]))
            )
            dmat <- geodist::geodist(xy_i)
            
            d_i <- geodist::geodist(
                pts[i, c("x", "y")],
                pts[i, c("x0", "y0")]
            )
            d_i <- as.numeric(d_i[1, 1])
            
            if (any(dmat[upper.tri(dmat)] < dist_tol)) {
                edge_i <- edges_to_split_i[e, ]
                edge_i_new <- rbind(edge_i, edge_i)  # for edges to new point
                # Reverse 2nd edge:
                edge_i_new$from[2] <- edge_i_new$to[1]
                edge_i_new$to[2] <- edge_i_new$from[1]
                edge_i_new$xfr[2] <- edge_i_new$xto[1]
                edge_i_new$xto[2] <- edge_i_new$xfr[1]
                edge_i_new$yfr[2] <- edge_i_new$yto[1]
                edge_i_new$yto[2] <- edge_i_new$yfr[1]
                
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
                    edge_i_new <- dodgr:::set_maxspeed(edge_i_new, wt_profile, wt_profile_file) |>
                        dodgr:::weight_by_num_lanes(wt_profile) |>
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
                edge_i <- rbind(edge_i, edge_i_new)
            }
            
            return(edge_i)
        })
        return(do.call(what = rbind, args = new_edges_i))
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
    return(rbind(graph, graph_to_add))
}
