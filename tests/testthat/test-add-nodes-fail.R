library(dplyr)

test_that("add_nodes_to_graph with multiple points on same edge", {
    # Create a simple graph with one edge
    graph <- data.frame(
        from = c("a", "a"),           # Two edges representing same street
        to = c("b", "b"),             # in both directions
        d = c(1, 1),
        edge_id = c("e1", "e2"),
        component = c(1, 1),
        xfr = c(0, 0),
        yfr = c(0, 0),
        xto = c(1, 1),
        yto = c(1, 1),
        d_weighted = c(1, 1),
        time = c(1, 1),
        time_weighted = c(1, 1)
    )

    # Create two points that will both match to this edge
    xy <- data.frame(
        x = c(0.3, 0.7),  # Two points along the same edge
        y = c(0.3, 0.7)   # These will both match to the diagonal edge
    )

    # Run the function and examine output
    result <- add_nodes_to_graph(graph, xy)
    result_3 <- add_nodes_to_graph3(graph, xy)
    
    # Print details about the result
    print(paste("Number of rows in result:", nrow(result)))
    print(paste("Number of rows using add_nodes_to_graph3 :", nrow(result_3)))
    print("Result coordinates:")
    print(result[, c("xfr", "yfr", "xto", "yto")])
    
    # The result should have 6 edges (2 original edges each split into 3 parts)
    expect_equal(nrow(result), 6)
    
    # Check that coordinates are continuous
    result_forward <- result[result$from == "a", ]
    result_forward <- result_forward[order(result_forward$xfr), ]
    
    # Each edge should end where the next begins
    expect_equal(result_forward$xto[1], result_forward$xfr[2])
    expect_equal(result_forward$yto[1], result_forward$yfr[2])
    expect_equal(result_forward$xto[2], result_forward$xfr[3])
    expect_equal(result_forward$yto[2], result_forward$yfr[3])
    
    ## using add_nodes_to_graph2
    
    # Run the function and examine output
    result <- add_nodes_to_graph2(graph%>%
                                      rename(from_lat=xfr, from_lon=yfr, to_lat=xto, to_lon=yto), data.frame(xy))%>%
        rename(xfr=from_lat, yfr=from_lon, xto=to_lat, yto=to_lon)
    
    # Print details about the result
    print(paste("Number of rows in result:", nrow(result)))
    print("Result coordinates:")
    print(result[, c("xfr", "yfr", "xto", "yto")])
    
    # The result should have 6 edges (2 original edges each split into 3 parts)
    expect_equal(nrow(result), 6)
    
    # Check that coordinates are continuous
    result_forward <- result[result$from == "a", ]
    result_forward <- result_forward[order(result_forward$xfr), ]
    
    # Each edge should end where the next begins
    expect_equal(result_forward$xto[1], result_forward$xfr[2])
    expect_equal(result_forward$yto[1], result_forward$yfr[2])
    expect_equal(result_forward$xto[2], result_forward$xfr[3])
    expect_equal(result_forward$yto[2], result_forward$yfr[3])
    
    ## using add_nodes_to_graph3
    
    # Add ids to xy for add_nodes_to_graph3
    xy_with_id <- xy
    xy_with_id$id <- c("p1", "p2")
    
    # Run the function and examine output
    result <- add_nodes_to_graph3(graph, xy_with_id)
    
    # Print details about the result
    print(paste("Number of rows in result:", nrow(result)))
    print("Result coordinates:")
    print(result[, c("xfr", "yfr", "xto", "yto")])
    
    # The result should have 6 edges (2 original edges each split into 3 parts)
    expect_equal(nrow(result), 6)
    
    # Check that coordinates are continuous
    result_forward <- result[result$from == "a", ]
    result_forward <- result_forward[order(result_forward$xfr), ]
    
    # Each edge should end where the next begins
    expect_equal(result_forward$xto[1], result_forward$xfr[2])
    expect_equal(result_forward$yto[1], result_forward$yfr[2])
    expect_equal(result_forward$xto[2], result_forward$xfr[3])
    expect_equal(result_forward$yto[2], result_forward$yfr[3])
})
