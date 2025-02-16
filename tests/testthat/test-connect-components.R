library(testthat)
library(dodgr)
dodgr::dodgr_cache_off ()
dodgr::clear_dodgr_cache ()
profile_file <- here::here("tests/testthat/profile.json")
dodgr::write_dodgr_wt_profile(profile_file)
library(sf)
library(dplyr)
# graph <- weight_streetnet (hampi)%>%
#     filter(component==1)
# from <- sample (graph$from_id, size = 1)
# to <- sample (graph$to_id, size = 1)
# dp <- dodgr_paths (graph, from = from, to = to, pairwise = TRUE)



test_that("connect_components validates input", {
    map0 <- dodgr::hampi%>%
        mutate(idnew=as.character(1:n()), osm_id=NULL)
    graph <- dodgr::weight_streetnet(map0, id_col = "idnew")
    g1 <- graph%>%dplyr::filter(component==1)
    g2 <- graph%>%dplyr::filter(component==2)
    #p1 <- tibble(x=76.44, y=15.32)%>%st_as_sf(coords=c("x", "y"))
    #p2 <- tibble(x=76.4923203, y=15.3603284)%>%st_as_sf(coords=c("x", "y"))
    v1 <- sf::st_as_sf(dodgr::dodgr_vertices(g1), coords=c("x", "y"), remove=FALSE)
    v2 <- sf::st_as_sf(dodgr::dodgr_vertices(g2), coords=c("x", "y"), remove=FALSE)%>%
        head(2)
    d1 <- dodgr::dodgr_distances(graph = g1, from = v2%>%sf::st_coordinates(), to=dodgr::dodgr_vertices(g1)%>%dplyr::select(x,y))
    ## since dodgr_distances snaps points to the graph, min distance is zero
    ## even though points are away from the graph
    apply(d1,1,min)
    ## lets create edges to the points with the regular dodgr::add_nodes_to_graph
    g1a <- dodgr::add_nodes_to_graph(g1, v2)
    d1a <- dodgr::dodgr_distances(graph = g1a, from = v2%>%sf::st_coordinates(), to=dodgr::dodgr_vertices(g1)%>%dplyr::select(x,y))
    md1a <- apply(d1a,1,min)
    md1a
    ## so distance increase with more nodes!
    ## now we connect using add_nodes_to_graph2. should not change minimum distance
    g1b <- add_nodes_to_graph2(g1, v2)%>%unique
    d1b <- dodgr::dodgr_distances(graph = g1b, from = v2%>%sf::st_coordinates(), to=dodgr::dodgr_vertices(g1)%>%dplyr::select(x,y))
    md1b <- apply(d1b,1,min)
    md1b
    ## but notice we have the vertice ids from v2
    ## well, now we want to know if these connections in fact connected the whole component 2
    ## combine components
    g1g2 <- bind_rows(g1b,
                      g2%>%dplyr::mutate(edge_id=as.character(edge_id), component=1))%>%    ungroup%>%
        mutate(edge_id=1:n())
    
    map_g1g2 <- dodgr::dodgr_to_sf(g1g2)
    ## check distance for v2 again
    d1c <- dodgr::dodgr_distances(graph = g1g2, from = v2%>%sf::st_coordinates(), to=dodgr::dodgr_vertices(g1)%>%dplyr::select(x,y))
    md1cw <- apply(d1c,1,which.min)
    md1c <- d1c[,md1cw]
    md1v <- unique(colnames(d1c)[md1cw])
    ## now the path
    dpc <- dodgr::dodgr_paths(graph = g1g2, from = v2$id, to=dodgr::dodgr_vertices(g1)%>%
                                  filter(id%in%md1v)%>%
                                  dplyr::select(id), vertices = FALSE)
    dpcv <- dodgr::dodgr_paths(graph = g1g2, from = v2$id, to=dodgr::dodgr_vertices(g1)%>%
                                  filter(id%in%md1v)%>%
                                  dplyr::select(id), vertices = TRUE)
    g1g2[unlist(dpc),]
    dodgr::dodgr_vertices(g1g2)%>%filter(id%in%unlist(dpcv))
    ## now check for a path from a vertice of g2 (excluding v2) and a vertice in g1
    v2p <- sf::st_as_sf(dodgr::dodgr_vertices(g2), coords=c("x", "y"), remove=FALSE)%>%
        tail(1)
    v1p <- sf::st_as_sf(dodgr::dodgr_vertices(g1), coords=c("x", "y"), remove=FALSE)%>%
        tail(1)
    v1p0 <- sf::st_as_sf(dodgr::dodgr_vertices(g1), coords=c("x", "y"), remove=FALSE)%>%
        head(1)
    #v1p <- tibble(x=76.44, y=15.32)%>%st_as_sf(coords=c("x", "y"))
    #g1g2o <- g1g2
    #g1g2 <- g1
    v2path <- dodgr::dodgr_paths(graph = graph, from = v1p0$id, to=v1p$id, vertices = TRUE)
    v2path <- dodgr::dodgr_paths(graph = graph, from = v2p$id, to=v1p$id, vertices = TRUE)
    #eid <- g1g2[as.integer(unlist(v2path)),]
    eid <- g1g2%>%filter(
        (from_id%in%unlist(v2path)) |(to_id%in%unlist(v2path)))
    library(ggplot2)
    map_g1g2%>%
        filter(edge_id%in%eid$edge_id) -> 
        map_path
    ggplot(map_path)+geom_sf()
    library(tidyr)
    
    
    library(ggplot2)
    library(dodgr)
    library(sf)
    st_crs(v2) <- sf::st_crs(hampi)
    st_crs(v2p) <- sf::st_crs(hampi)
    st_crs(v1p) <- sf::st_crs(hampi)
    ggplot(map_g1g2) + 
        #geom_sf()+ 
        #geom_sf(data=v2, color="red")+
        geom_sf(data=v2p, color="orange")+
        geom_sf(data=v1p, color="blue")+
        geom_sf(data=map_path, color="pink", size=1)
    bbox <- st_bbox(map_path)
    ggplot() +
        geom_sf(data = map_g1g2) +
        geom_sf(data = v2p, color = "orange") +
        geom_sf(data = v1p, color = "blue") +
        geom_sf(data = map_path, color = "pink", linewidth = 3, alpha=1/2)+
        coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
                 ylim = c(bbox["ymin"], bbox["ymax"]))
    
    
    
    mind1 <- names(apply(d1,1,min))
    ggplot(v1) + 
        geom_sf() + 
        geom_sf(data=v2, color="red")+
        geom_sf(data=v1%>%filter(id%in%mind1), color="blue")
    
    v1%>%filter(id=="3921522524")
    g1g2 <- bind_rows(g1v2,g2%>%dplyr::mutate(edge_id=as.character(edge_id), component=1))
    
    hist(as.numeric(d1))
    mean(d1)
    d12 <- dodgr::dodgr_distances(graph = g1g2, from = v2%>%sf::st_coordinates(), to=dodgr::dodgr_vertices(g1)%>%dplyr::select(x,y))
    hist(as.numeric(d12))
    
    # Test invalid connection type
    expect_error(
        connect_components(graph, connection_type = "invalid_type",
                                    distance_threshold = 500,
                                    wt_profile = "foot",
                                    surface = "paved"
    )
    )
    
    # Test with valid input
    expect_no_error(
        connect_components(graph, connection_type = "residential",
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

test_that("connect_components handles distance_matrix parameter", {
    graph <- dodgr::weight_streetnet(dodgr::hampi)
    
    # Create disconnected components by removing some edges
    edges_to_remove <- sample(1:nrow(graph), size = nrow(graph) * 0.1)
    disconnected_graph <- graph[-edges_to_remove, ]
    
    # Pre-calculate distance matrix
    dist_matrix <- component_distances(disconnected_graph, distance_threshold = 500)
    
    # Connect components using pre-calculated matrix
    connected_with_matrix <- connect_components(disconnected_graph, 
                                             distance_threshold = 500,
                                             distance_matrix = dist_matrix,
                                             connection_type = "footway",
                                             wt_profile = "foot",
                                             surface = "paved")
    
    # Connect components without matrix
    connected_without_matrix <- connect_components(disconnected_graph, 
                                                distance_threshold = 500,
                                                connection_type = "footway",
                                                wt_profile = "foot",
                                                surface = "paved")
    
    # Both methods should give same number of components
    expect_equal(length(unique(connected_with_matrix$component)),
                length(unique(connected_without_matrix$component)))
})

test_that("connect_components respects distance threshold", {
    # Create a simple graph with three components in a line
    # A -- 100m -- B -- 200m -- C
    test_graph <- data.frame(
        from_id = c("a1", "a2", "b1", "b2", "c1", "c2"),
        to_id = c("a2", "a1", "b2", "b1", "c2", "c1"),
        component = c(1L, 1L, 2L, 2L, 3L, 3L),
        from_lon = c(0, 0.001, 0.01, 0.011, 0.03, 0.031),  # roughly 100m, 200m gaps
        from_lat = c(0, 0, 0, 0, 0, 0),
        to_lon = c(0.001, 0, 0.011, 0.01, 0.031, 0.03),
        to_lat = c(0, 0, 0, 0, 0, 0),
        d = c(100, 100, 100, 100, 100, 100),
        d_weighted = c(100, 100, 100, 100, 100, 100),
        highway = "residential",
        way_id = c("1", "1", "2", "2", "3", "3"),
        edge_id = c("1", "2", "3", "4", "5", "6"),
        stringsAsFactors = FALSE
    )
    
    # With 150m threshold, should only connect A-B
    connected_150 <- connect_components(test_graph, 
                                     distance_threshold = 150,
                                     connection_type = "footway",
                                     wt_profile = "foot",
                                     surface = "paved")
    comp_150 <- unique(connected_150$component)
    expect_equal(length(comp_150), 2)  # Should still have 2 components
    
    # With 250m threshold, should connect all components
    connected_250 <- connect_components(test_graph, 
                                     distance_threshold = 250,
                                     connection_type = "footway",
                                     wt_profile = "foot",
                                     surface = "paved")
    comp_250 <- unique(connected_250$component)
    expect_equal(length(comp_250), 1)  # Should have 1 component
})

test_that("connect_components follows MST for connections", {
    # Create a triangle of components with distances:
    # A -- 100m -- B
    # |           /
    # 300m    200m
    # |       /
    # C ----/
    test_graph <- data.frame(
        from_id = c("a1", "a2", "b1", "b2", "c1", "c2"),
        to_id = c("a2", "a1", "b2", "b1", "c2", "c1"),
        component = c(1L, 1L, 2L, 2L, 3L, 3L),
        from_lon = c(0, 0.001, 0.01, 0.011, 0, 0.001),
        from_lat = c(0, 0, 0, 0, -0.03, -0.03),
        to_lon = c(0.001, 0, 0.011, 0.01, 0.001, 0),
        to_lat = c(0, 0, 0, 0, -0.03, -0.03),
        d = c(100, 100, 100, 100, 100, 100),
        d_weighted = c(100, 100, 100, 100, 100, 100),
        highway = "residential",
        way_id = c("1", "1", "2", "2", "3", "3"),
        edge_id = c("1", "2", "3", "4", "5", "6"),
        stringsAsFactors = FALSE
    )
    
    # With 250m threshold, should connect via A-B and B-C (MST)
    # rather than directly connecting A-C
    connected <- connect_components(test_graph, 
                                 distance_threshold = 250,
                                 connection_type = "footway",
                                 wt_profile = "foot",
                                 surface = "paved")
    
    # Should have exactly one component
    expect_equal(length(unique(connected$component)), 1)
    
    # Count new connector edges
    new_edges <- connected[connected$is_connector == TRUE, ]
    expect_true(nrow(new_edges) >= 4)  # At least 4 edges (2 pairs of bidirectional edges)
})


# lines <- g1g2 %>% 
#     filter(edge_id %in% unlist(v2path)) %>%
#     select(edge_id, from_lat, to_lat, from_lon, to_lon)%>%
#     pivot_longer(cols=c("from_lat", "from_lon", "to_lat", "to_lon"))%>%
#     separate_wider_delim(cols=name, delim='_', names=c("direction", "latlon"))%>%
#     pivot_wider(names_from = latlon)%>%
#     sf::st_as_sf(coords=c("lat", "lon"), crs = st_crs(hampi))%>%
#     group_by(edge_id)%>%
#     summarise()%>%
#     st_cast("LINESTRING")
