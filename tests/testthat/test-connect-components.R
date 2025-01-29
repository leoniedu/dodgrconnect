test_that("connect_components validates input", {
    graph <- dodgr::weight_streetnet(dodgr::hampi)
    
    # Test invalid connection type
    expect_error(connect_components(graph, connection_type = "invalid_type"))
    
    # Test with valid input
    expect_no_error(connect_components(graph))
})

test_that("connect_components handles weights correctly", {
    graph <- dodgr::weight_streetnet(dodgr::hampi)
    
    # Create disconnected components by removing some edges
    edges_to_remove <- sample(1:nrow(graph), size = nrow(graph) * 0.1)
    disconnected_graph <- graph[-edges_to_remove, ]
    
    # Connect components
    connected_graph <- connect_components(disconnected_graph)
    
    # Check that weights are properly set
    new_edges <- connected_graph[connected_graph$is_connector == TRUE, ]
    expect_true(all(c("d_weighted", "time", "time_weighted") %in% names(new_edges)))
    expect_true(all(!is.na(new_edges$d_weighted)))
    expect_true(all(!is.na(new_edges$time)))
    expect_true(all(!is.na(new_edges$time_weighted)))
})

test_that("connect_components reduces number of components", {
    graph <- dodgr::weight_streetnet(dodgr::hampi)
    
    # Create disconnected components
    edges_to_remove <- sample(1:nrow(graph), size = nrow(graph) * 0.1)
    disconnected_graph <- graph[-edges_to_remove, ]
    
    # Get initial number of components
    initial_comps <- length(unique(dodgr::dodgr_components(disconnected_graph)$component))
    
    # Connect components
    connected_graph <- connect_components(disconnected_graph)
    
    # Get final number of components
    final_comps <- length(unique(dodgr::dodgr_components(connected_graph)$component))
    
    # Should have fewer components after connecting
    expect_lt(final_comps, initial_comps)
})
