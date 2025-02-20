% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connect_components3.R
\name{connect_components3}
\alias{connect_components3}
\title{Connect Graph Components}
\usage{
connect_components3(
  graph,
  distance_threshold = 20,
  connection_type,
  wt_profile,
  wt_profile_file = NULL,
  components_to_join
)
}
\arguments{
\item{graph}{A data frame or dodgr_streetnet object containing the graph}

\item{distance_threshold}{Maximum distance in meters to consider for connecting vertices}

\item{connection_type}{Type of way to use for connecting components (e.g., "path", "footway")}

\item{wt_profile}{Weight profile to use for calculating edge weights and times}

\item{wt_profile_file}{Optional path to a custom weight profile file}

\item{components_to_join}{Integer vector specifying the component IDs to connect in sequence}
}
\value{
A modified graph with the specified components connected. Components are
connected in sequence, with each subsequent component being connected to
the growing connected component if possible. Component IDs are updated
to reflect the new connections. If a component cannot be connected
(no vertices within distance_threshold), it remains unchanged.
}
\description{
This function connects multiple components of a graph by identifying vertices that
are within a specified distance threshold and adding edges between them. It uses
spatial indexing for efficient pair identification and add_nodes_to_graph2 for
proper edge creation.
}
\details{
The function processes components sequentially, starting with the first component
and attempting to connect each subsequent component to the growing connected
component. For each pair of components, it:
\enumerate{
\item Uses spatial indexing via sf::st_is_within_distance to efficiently find
vertex pairs that are within the distance threshold
\item Uses add_nodes_to_graph2 to properly add the vertices and edges with correct
weights and properties
\item Combines the graphs and updates component IDs
}
}
\examples{
\dontrun{
library(dodgr)
# Load a street network
net <- weight_streetnet(hampi)

# Connect components 1, 2, and 3 with footway edges
net_connected <- connect_components3(
  graph = net,
  distance_threshold = 50,  # 50 meters
  connection_type = "footway",
  wt_profile = "foot",
  components_to_join = c(1, 2, 3)
)
}

}
