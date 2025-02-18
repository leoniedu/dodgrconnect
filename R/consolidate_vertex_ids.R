#' Consolidate vertex IDs based on coordinate precision
#' 
#' Ensures each unique coordinate pair (to 6 decimal places) has a single ID
#' 
#' @param graph A dodgr graph with columns including `from`, `to`, and coordinates
#' @return Modified graph with consolidated vertex IDs
#' @export
consolidate_vertex_ids <- function(graph) {
  # Get all vertices from graph
  verts <- dodgr::dodgr_vertices(graph)
  
  # Create new IDs based on 6 decimal place coordinates
  verts <- verts %>%
    dplyr::mutate(
      new_x = round(.data$x, 6),
      new_y = round(.data$y, 6),
      new_id = sprintf("%.6f,%.6f", .data$new_x, .data$new_y)
    )
  
  # Create mapping from old IDs to new consolidated IDs
  id_map <- stats::setNames(verts$new_id, verts$id)
  
  # Update from/to IDs in edge dataframe
  graph <- graph %>%
    dplyr::mutate(
      from_id = id_map[.data$from_id],
      to_id = id_map[.data$to_id]
    )
  
  # Update edge coordinates to match consolidated vertices
  coord_map <- verts %>%
    dplyr::distinct(.data$new_id, .data$new_x, .data$new_y)
  
  graph <- graph %>%
    select(-from_lat, -from_lon, -to_lat, -to_lon)%>%
    dplyr::left_join(coord_map, by = c("from_id" = "new_id")) %>%
    dplyr::rename(from_lon = "new_x", from_lat = "new_y") %>%
    dplyr::left_join(coord_map, by = c("to_id" = "new_id")) %>%
    dplyr::rename(to_lon = "new_x", to_lat = "new_y")
  
  # Remove any duplicate edges created by consolidation
  dplyr::distinct(graph, .data$from_id, .data$to_id, .keep_all = TRUE)
}