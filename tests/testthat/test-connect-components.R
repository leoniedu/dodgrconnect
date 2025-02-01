library(testthat)
dodgr::dodgr_cache_off ()
dodgr::clear_dodgr_cache ()
profile_file <- here::here("tests/testthat/profile.json")
dodgr::write_dodgr_wt_profile(profile_file)


test_that("connect_components validates input", {
    graph <- dodgr::weight_streetnet(dodgr::hampi)
    
    # Test invalid connection type
    expect_error(
        connect_components(graph, connection_type = "invalid_type",
                                    distance_threshold = 500,
                                    wt_profile = "foot",
                                    surface = "paved"
    )
    )
    
    # Test with valid input
    expect_no_error(connect_components(graph, connection_type = "residential",
                                       distance_threshold = 500,
                                       wt_profile = "foot",
                                       surface = "paved"))
})

test_that("connect_components handles weights correctly", {
    graph <- dodgr::weight_streetnet(dodgr::hampi)
    
    # Create disconnected components by removing some edges
    edges_to_remove <- sample(1:nrow(graph), size = nrow(graph) * 0.1)
    disconnected_graph <- graph[-edges_to_remove, ]
    
    # Connect components
    connected_graph <- connect_components(disconnected_graph, connection_type = "residential",
                                          distance_threshold = 500,
                                          wt_profile = "foot",
                                          surface = "paved")
    
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
    initial_comps <- length(unique(disconnected_graph$component))
    
    # Connect components
    connected_graph <- connect_components(disconnected_graph, 
                                          distance_threshold = 500,
                                          connection_type = "footway",
                                          wt_profile = "foot",
                                          surface = "paved"
    )
    
    # Get final number of components
    final_comps <- length(unique(connected_graph$component))
    
    # Should have fewer components after connecting
    expect_lt(final_comps, initial_comps)
})
