set.seed(123)
test_graph <- dodgr::dodgr_sample(dodgr::weight_streetnet(dodgr::hampi), nverts = 100)

test_that("break_long_edges works with default max_d", {
  # Create a simple test graph
  
  result <- break_long_edges(test_graph)
  
  # Check that the result is a data frame
  expect_s3_class(result, "data.frame")
  
  # Check that no edges are longer than the mean distance
  m_d <- mean(test_graph$d)+sd(test_graph$d)
  expect_true(all(result$d <= m_d))
  
  # Check that we have more edges than the original graph
  expect_gt(nrow(result), nrow(test_graph))
})

test_that("break_long_edges works with custom max_d", {

  max_d <- 20
  result <- break_long_edges(test_graph, max_d = max_d)
  
  # Check that no edges are longer than max_d
  expect_true(all(result$d <= max_d))
  
  # Check that the result contains finite distances
  expect_true(all(is.finite(result$d)))
})

test_that("break_long_edges handles empty graph", {
  empty_graph <- test_graph[0,]
  
  result <- break_long_edges(empty_graph)
  expect_equal(nrow(result), 0)
})

test_that("break_long_edges validates input correctly", {
  # Test missing columns
  bad_graph <- data.frame(x = 1, y = 2)
  class(bad_graph) <- class(test_graph)
  expect_error(break_long_edges(bad_graph), "Must be a subset of")
  
  # Test non-data.frame input
  expect_error(break_long_edges(list(a = 1)), "Must be of type 'data.frame'")
  
  # Test non-finite distances
  inf_graph <- test_graph
  inf_graph$d[1] <- Inf
  expect_error(break_long_edges(inf_graph), "Must be finite")
  
  # Test NA distances
  na_graph <- test_graph
  na_graph$d[1] <- NA
  expect_error(break_long_edges(na_graph), "Contains missing values")
})

test_that("break_long_edges handles graph with no long edges", {
  max_d <- max(test_graph$d)
  result <- break_long_edges(test_graph, max_d = max_d)
  
  # Should return the original graph unchanged
  expect_equal(nrow(result), nrow(test_graph))
  expect_equal(result, test_graph)
})

test_that("break_long_edges handles edges longer than 2*max_d", {
  # Create a graph with an edge much longer than max_d
  test_graph2 <- dplyr::arrange(test_graph, desc(d))
  
  max_d <- test_graph2$d[1]/5
  result <- break_long_edges(test_graph2, max_d = max_d)
  
  # The process should create multiple edges
  expect_gt(nrow(result), nrow(test_graph))
  
  # All final edges should be <= max_d
  expect_true(all(result$d <= max_d))
  
  # Should have created more than one new edge
  # For max_d <- test_graph2$d[1]/5, we expect at least 4 new edges
  expect_gt(nrow(result) - nrow(test_graph), 3)
})

test_that("break_long_edges handles verbosity parameter", {
  # Test invalid verbose values
  expect_error(break_long_edges(test_graph, verbose = -1), "Must be of type 'logical flag'")
  expect_error(break_long_edges(test_graph, verbose = NA), "May not be NA")
  expect_error(break_long_edges(test_graph, verbose = "high"), "Must be of type 'logical flag'")
  expect_error(break_long_edges(test_graph, verbose = c(TRUE,TRUE)), "Must have length 1")
  
  # Test invalid max_d values
  expect_error(break_long_edges(test_graph, max_d = -1), "Element 1 is not >= 0")
  expect_error(break_long_edges(test_graph, max_d = NA), "May not be NA")
  expect_error(break_long_edges(test_graph, max_d = "100"), "ust be of type 'number'")
  expect_error(break_long_edges(test_graph, max_d = c(1,2)), "Must have length 1")
  
  # Test that function works with all valid verbose values
  expect_error(break_long_edges(test_graph, verbose = FALSE), NA)
  expect_error(break_long_edges(test_graph, verbose = TRUE), NA)
})
