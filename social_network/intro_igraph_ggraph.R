# Visualization of Social Network
# Introduction to igraph and ggraph package

# Load required packeges
library(tidyverse)
library(igraph)
library(ggraph)
library(ggthemes)

# igraph could be used for plotting. 
# ggraph is an extension of ggplot2 package. It plots network as a ggplot layer process.

# But I should say people who actually do social network analysis seldom uses R, they prefer Gephi

# Basic ideas
# Vertex or node: refer to individuals
# Edges: The interconnections between individuals
# Both vertices and edges could have attributes. For vertex, it could be age, gender, party affiliation of individual. For edge, the most common one could be the weight of the edges, such as how many times you are meeting with another people in the network, or it could be the number of flights between cities in a week.

# Usually we visualize those attributes in social network graphs. 
# Use different colors to color the nodes with different attributes. 
# Adjusting the relative thickness of the edges in the network. The thicker the tie the higher the weight of edges.

# Social Network Data Structure
# There are many forms. Adjacency Matrix and Edgelist used the most.
# Adjacency Matrix
# Column and row names represet the vertex. A 1 in a cell indicates that an edge exists between two vertices. A 0 indicates that there is no edge present between those two vertices. You could use other numbers as well, those means the weight of the edges between vertices.

# Edge List
# Each row represents an edge between the two individual vertices in each column.
# This type of raw data is the most common form that people collect before starting social network analysis. Some people also likes to store vertices attribute in a separate dataset. 

# Directed and Undirected Networks
# Edges could have directions. The edges in undirected networks simply indicate that a relationship of some kind exists between two vertices. In reality, many network edges do have directionality. An arrow could represents an edge going from one vertex to another.

# Example: Visualize Germany Measles Transmission Network in 1861
measles <- read_csv("datasets/measles.csv")
# We have a edge list

# Make a igraph object

# g <- graph_from_edgelist(as.matrix(measles), directed = TRUE)
# graph_from_edgelist() requires a two column matrix, so you should first change it to matrix form.

g <- graph_from_data_frame(d = measles, 
                           directed = TRUE)

class(g)

# Start Plot
plot(g)

# Check Number of Vertices
gorder(g)
vcount(g)
# 187 vertices

# Check Number of Edges
gsize(g)
ecount(g)
# 184 edges

# Check if directed
is.directed(g)
# TRUE

# Check if weighted
is.weighted(g)
# FALSE

# Check Vertices
V(g)

# Check edges
E(g)

# Better plot, node size to 0, node label will be node's name
# Could use different layouts
plot(g, 
     vertex.label.color = "black", 
     edge.color = "gray77", 
     vertex.size = 0, 
     edge.arrow.size = 0.1, 
     layout = layout_with_fr(g))

plot(g, 
     vertex.label.color = "black", 
     edge.color = "gray77", 
     vertex.size = 0, 
     edge.arrow.size = 0.1, 
     layout = layout_in_circle(g))

plot(g, 
     vertex.label.color = "black", 
     edge.color = "gray77", 
     vertex.size = 0, 
     edge.arrow.size = 0.1, 
     layout = layout_as_tree(g))

# A more easier choice is layout_nicely()
# Tries to choose the most appropriate layout function for a given graph object
plot(g, 
     vertex.label.color = "black", 
     edge.color = "gray77", 
     vertex.size = 0, 
     edge.arrow.size = 0.1, 
     layout = layout_nicely(g))

# Showing all edges going out from vertex 184
incident(g, v = "184", mode = c("out"))

# Identifying neighbors
neighbors(g, v = "184", mode = c("all"))
# Patient 184 is patient 0

# Similar for vertex 45, could be considered as super transmitter
incident(g, v = "45", mode = c("out"))

neighbors(g, v = "45", mode = c("all"))

neighbors(g, v = "45", mode = c("out"))

# Find vertices that reachable in N steps
ego(g, order = 2, nodes = "184", mode = c("out"))

ego(g, order = 2, nodes = "45", mode = c("out"))

# Measure of Vertex Importance
# There are so many measurements you can use
# Degree: is a count of the number of connections a node has. For a directed network, you have both in degree and out degree. You can caculate total degree as well.

# Betweenness: centrality quantifies the extent to which a node lies in the shortest path
# between any two other nodes in the network, often playing a bridging role. Individuals with
# high betweenness are key bridges between different paths of a network. Individuals with low
# betweeness are not that significant to the overall connectedness of the network.

# Eigenvector centrality is a measure of how well connected a vertex is. Vertices with the highest eigenvector centrality are those that are connected to many others but especially to other vertices who themselves are highly connected to others.

degree(g, v = "184", mode = "all")
degree(g, v = "184", mode = "in")
degree(g, v = "184", mode = "out")

degree(g, v = "45", mode = "all")
degree(g, v = "45", mode = "in")
degree(g, v = "45", mode = "out")

# You can calculate all vertices degree
degree(g, mode = "all")
degree(g, mode = "in")
sort(degree(g, mode = "out"), decreasing = TRUE)

hist(degree(g, mode = "out"), breaks = 30)

# Assign attribute to your vertices
vertex_attr(g)
edge_attr(g)

V(g)$out_degree <- degree(g, mode = "out")
vertex_attr(g)

# Calculate the betweenness centrality of each vertex
V(g)$betweenness <- betweenness(g, directed = TRUE)
names(vertex_attr(g))

V(g)$betweenness
# Hugh disparity in betweenness scores

# Create plot with vertex size determined by betweenness score
plot(g, 
     vertex.label = NA, #vertex labels made NA so they do not show
     edge.color = "black", 
     vertex.size = sqrt(V(g)$betweenness+1), 
     # We could practice some data transformation
     # so that node  with 0 betweenness still be shown on graph, but we also get
     # relative importance
     edge.arrow.size = 0.05, 
     layout = layout_nicely(g), 
     main = "Germany Measles Transmission Network in 1861")

# Three patients for whom no information is known about who infected them.
# Vertex 184 appears ultimately responsible for spreading the disease to many other individuals even though they did not directly infect too many individuals. However, because Vertex 184 has no incoming edge in the network, they appear to have low betweenness.

# Ego Network: Focusing on Vertex 184
diameter(g)
farthest_vertices(g)

g184 <- make_ego_graph(g, order = diameter(g), 
                     nodes = "184", 
                     mode = "all")[[1]]

gorder(g184)
# 173

# Check the shortest paths between vertices
dists <- distances(g184, v = "184", mode = "out")
length(dists)

max(distances(g184, v = "184", mode = "out"))
min(dists)

# Create a color palette of length equal to the maximal geodesic distance plus one. 
# This is so that vertices of the same distance are plotted in the same color and patient zero also has its own color.
range(dists)
# We need 6 colors
colors <- c("black", "red", "orange", "blue", "dodgerblue", "cyan")
colors[1]

# Assign color attributes to vertices
V(g184)$color <- colors[dists+1]

vertex_attr(g184)

plot(g184, 
     vertex.label = dists,
     vertex.label.color = "white", 
     vertex.label.cex = .6, 
     edge.color = "black", 
     vertex.size = 7, 
     edge.arrow.size = .05,
     main = "Distances from Patient Zero"
)

# Undirected Network Visualization use ggraph package
# Data: 2016 National Network of Local Health Departments. The data was taken from a survey by the National Association of County and City Health Officials (NACCHO).

# Use survey to ask each health department to identify five health departments they
# connected to the most. Connections among health departments facilitate information sharing
# and coordination of services and are especially important during public health emergencies.

health.dep.edges <- read_csv("datasets/naccho2016clean.csv")
health.dep.nodes <- read_csv("datasets/naccho2016att.csv")

# The health department network shows partnerships, which would logically be represented by a single link between any two health departments that partner.

# Combine the edglist and attributes into a network object
health.dep.net <- graph_from_data_frame(d = health.dep.edges, 
                                        vertices = health.dep.nodes, 
                                        directed = FALSE)

vcount(health.dep.net)
ecount(health.dep.net)

plot(health.dep.net, 
     vertex.label = NA,
     vertex.size = 0, 
     layout = layout_nicely(health.dep.net))

# Check multiple ties
# We need to remove those ties
health.dep.edges %>%
  group_by(from, to) %>%
  tally() %>%
  ungroup() %>%
  arrange(desc(n))

# Local health departments do not typically partner with themselves, so there would be no loops in the network.
# Check loops
health.dep.edges %>%
  filter(from == to)

which(which_loop(health.dep.net) == TRUE)

# Check for loops and multiple edges
is_simple(health.dep.net)
# [1] FALSE

# Remove loops and multiple
health.dep.net <- simplify(health.dep.net,
                           remove.multiple = TRUE,
                           remove.loops = TRUE)

# Check again
is_simple(health.dep.net)
# [1] TRUE

# Count the number of edges in the network after removing
vcount(health.dep.net)
ecount(health.dep.net)

# Identify highly connected nodes using degree measurement
head(sort(degree(health.dep.net), decreasing = TRUE))

# Let's subset our network with focus on local health departments in California and New York
region.net <- induced_subgraph(health.dep.net, 
                               vids = which(V(health.dep.net)$state %in% c("NY", 
                                                                           "CA")))
# input of vids argument should be a numeric vector

vcount(region.net)
# 82

ecount(region.net)
# 157

# Caculate density of this regional network
edge_density(region.net, loops = FALSE)
# [1] 0.04727492

# Plot the network with ggraph
ggraph(graph = region.net, layout = "with_kk")+
  geom_edge_link()+
  # Nodes colored by state
  geom_node_point(aes(color = state))+
  ggtitle("The Network of Local Health Departments across California and New York")+
  theme_graph()

# Assign new graph attributes
region.net$degree <- degree(region.net)
# degree in or degree out does not matter at all

# Identify important nodes in each state using degree centrality
# New York
head(sort(region.net$degree[V(region.net)$state == "NY"], 
          decreasing = TRUE))

# California
head(sort(region.net$degree[V(region.net)$state == "CA"], 
          decreasing = TRUE))

# Same for betweenness
region.net$betweenness <- betweenness(region.net, 
                                      directed = FALSE)
# New York
head(sort(region.net$betweenness[V(region.net)$state == "NY"], 
          decreasing = TRUE))

# California
head(sort(region.net$betweenness[V(region.net)$state == "CA"], 
          decreasing = TRUE))

# We can use those measurements and we could assign them as vertex attributes
# Add degree to the nodes attributes
V(region.net)$degree <- degree(region.net)

# Plot with node size by degree, color by state, and use Kamada Kawai layout
ggraph(region.net, layout = "with_kk")+
  geom_edge_link()+
  geom_node_point(aes(size = degree, color = state))+
  geom_node_text(aes(label = name, size = 1), nudge_y = 0.25)+
  ggtitle("The Network of Local Health Departments across California and New York with Degree Centrality")+
  theme_graph()+
  guides(size = FALSE)+
  theme(plot.title = element_text(size = 11, hjust = 0.5))

# Focus only one state. State-Level Networks
# Local emergencies like the wildfires in California in 2018.

# Subset the network so it only includes CA
california.network <- induced_subgraph(health.dep.net, 
                                       vids = which(V(health.dep.net)$state == "CA"))

# Find the number of vertices
vcount(california.network)

# Find the number of edges
ecount(california.network)

# Find the density
edge_density(california.network, loops = FALSE)

head(sort(degree(california.network), decreasing = TRUE))

head(sort(betweenness(california.network, directed = FALSE), 
          decreasing = TRUE))

# Questions to explore: Are central health departments urban?
# Urban health departments are likely to be in more populated areas and to serve more people.
# It would make sense that urban health departments are more central to the network since they
# have more resources to use in forming and maintaining partnerships. 

# Nodes Colored by Continuous Variable
ggraph(california.network, layout = "with_kk")+
  geom_edge_link()+
  geom_node_point(aes(color = population / 100000, 
                      size = degree(california.network),
                      shape = rurality))+
  scale_size_continuous(range = c(0.5, 5))+
  geom_node_text(aes(label = name, size = 3), nudge_y = 0.2)+
  scale_color_gradient2("Population, in 10 thousands", 
                        low = "#e5f5f9", high = "#2ca25f", mid = "#99d8c9", 
                        guide = "colourbar")+
  ggtitle("The Network of Health Departments in California, colored by Population")+
  guides(color = guide_colorbar(barwidth = 10, barheight = 1))+
  theme_graph()+
  guides(size = FALSE,
         shape = guide_legend(title = "Rurality"))+
  theme(plot.title = element_text(hjust = 0.5, size = 11), 
        legend.position = "bottom")

# Source: 
# James Curley, Psychology Department, Columbia University, http://curleylab.psych.columbia.edu/curley.html


