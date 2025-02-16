devtools::load_all(here::here("../dodgr"))
devtools::load_all(here::here("../dodgrconnect"))
test_that("add_nodes_to_graph2 with simple line example", {
    # 1. Create a simple straight line graph (approximately 1km east-west in London)
    graph <- data.frame(
        from_id = c("a", "a"),           # Two edges representing same street
        to_id = c("b", "b"),             # in both directions
        d = c(1000, 1000),               # 1km length
        edge_id = c("e1", "e2"),
        component = c(1, 1),
        from_lon = c(-0.120, -0.120),    # Start at -0.120 lon
        from_lat = c(51.500, 51.500),    # At 51.500 lat
        to_lon = c(-0.110, -0.110),      # End at -0.110 lon (≈1km east)
        to_lat = c(51.500, 51.500),      # Same latitude
        d_weighted = c(1000, 1000),      # simple 1:1 weights
        time = c(1000, 1000),            # 1 second per meter
        time_weighted = c(1000, 1000),
        highway = c("primary", "primary")
    )

    # 2. Create six points:
    # - Two points on the line at 300m and 700m
    # - Two points 10m north of these points
    # - Two points 20m north of these points
    point_ids <- c("p1", "p2", "p3", "p4", "p5", "p6")  # store IDs separately
    
    # Calculate coordinates for points
    lon1 <- -0.120 + (0.010 * 0.3)  # 30% along the line
    lon2 <- -0.120 + (0.010 * 0.7)  # 70% along the line
    lat_offset1 <- 0.00009  # roughly 10m north (adjusted for haversine distance)
    lat_offset2 <- 0.00018  # roughly 20m north
    
    xy <- data.frame(
        lon = c(lon1, lon2,      # points on line
             lon1, lon2,       # points 10m above line
             lon1, lon2),      # points 20m above line
        lat = c(51.500, 51.500,  # on line
             51.500 + lat_offset1, 51.500 + lat_offset1,  # 10m above
             51.500 + lat_offset2, 51.500 + lat_offset2)  # 20m above
    )

    # 3. Test with intersections_only = FALSE
    result <- add_nodes_to_graph2(graph, xy, intersections_only = FALSE)
    
    # Print summary of results
    cat("\nNumber of edges in result:", nrow(result), "\n")
    
    # Show unique from-to pairs to understand connectivity
    connections <- unique(paste(result$from_id, "->", result$to_id))
    cat("\nUnique connections:\n")
    print(connections)
    
    # Verify distances
    cat("\nEdge distances:\n")
    distances <- data.frame(
        from_id = result$from_id,
        to_id = result$to_id,
        d = round(result$d, 1)
    )
    print(distances)

    # Print vertical edges for inspection
    cat("\nVertical edges (connections to points above):\n")
    print(result[abs(result$from_lat - result$to_lat) > 0, ])

    # Print horizontal segments for inspection
    cat("\nHorizontal segments (original line split):\n")
    print(result[result$from_lat == 51.500 & result$to_lat == 51.500, ])

    # 4. Test with intersections_only = TRUE
    result_intersections <- add_nodes_to_graph2(graph, xy, intersections_only = TRUE)
    
    # Print summary of results for intersections_only
    cat("\nWith intersections_only = TRUE:\n")
    cat("Number of edges in result:", nrow(result_intersections), "\n")
    
    # Show unique from-to pairs
    connections <- unique(paste(result_intersections$from_id, "->", result_intersections$to_id))
    cat("\nUnique connections:\n")
    print(connections)
    
    # Verify that no points are created above the line
    vertical_edges <- result_intersections[abs(result_intersections$from_lat - result_intersections$to_lat) > 0.00001, ]
    expect_equal(nrow(vertical_edges), 0)
    
    # Basic checks
    expect_true(nrow(result) > nrow(graph), "Should create more edges than original")
    expect_true(all(result$d >= 0), "All distances should be non-negative")
    expect_true(all(result$d < 1000), "All distances should be less than original edge length")
    
    # Check that intersection points are created
    # Note: Since we don't control the vertex IDs anymore, we'll check by coordinates
    intersection_points <- result[result$from_lat == 51.500 & result$from_lon %in% c(lon1, lon2), ]
    expect_true(nrow(intersection_points) > 0, "Should have edges at intersection points")
    
    # Check that we have edges going up to offset points
    vertical_edges <- result[abs(result$from_lat - result$to_lat) > 0.00001, ]
    expect_true(nrow(vertical_edges) > 0, "Should have vertical edges to points above")
    expect_true(all(round(vertical_edges$d) %in% c(10, 20)), 
               "Vertical edges should be approximately 10m or 20m")
    
    # Check total segment length
    segments <- result[result$from_lat == result$to_lat, ]  # Only horizontal segments
    expect_true(sum(segments$d) <= 2000, "Total length of segments should not exceed original edges")
})
