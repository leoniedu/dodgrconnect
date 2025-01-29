test_that("add_nodes validates input", {
    graph <- dodgr::weight_streetnet(dodgr::hampi)
    
    # Test invalid xy input
    expect_error(add_nodes(graph, xy = "not a matrix"))
    expect_error(add_nodes(graph, xy = matrix(1:6, ncol = 3)))
    
    # Test with valid input
    xy <- matrix(c(77.67, 15.33, 77.68, 15.34), ncol = 2)
    expect_no_error(add_nodes(graph, xy))
})

test_that("add_nodes handles weight profiles correctly", {
    graph <- dodgr::weight_streetnet(dodgr::hampi)
    xy <- matrix(c(77.67, 15.33), ncol = 2)
    
    # Test with new edge type
    g1 <- add_nodes(graph, xy, new_edge_type = "path")
    expect_true("d_weighted" %in% names(g1))
    expect_true("time" %in% names(g1))
    expect_true("time_weighted" %in% names(g1))
    
    # Test with invalid edge type
    expect_error(add_nodes(graph, xy, new_edge_type = "invalid_type"))
    
    # Test weight inheritance
    g2 <- add_nodes(graph, xy, new_edge_type = NULL)
    expect_true(all(c("d_weighted", "time", "time_weighted") %in% names(g2)))
})

test_that("add_nodes maintains graph structure", {
    graph <- dodgr::weight_streetnet(dodgr::hampi)
    xy <- matrix(c(77.67, 15.33), ncol = 2)
    
    g1 <- add_nodes(graph, xy)
    
    # Check that original columns are preserved
    expect_true(all(names(graph) %in% names(g1)))
    
    # Check that new graph has more edges
    expect_gt(nrow(g1), nrow(graph))
    
    # Check that new edges have valid IDs
    expect_true(all(!is.na(g1$edge_id)))
})
