library(shiny)
library(igraph)
library(visNetwork)
library(tidyverse)

ui <- fluidPage(
  # App title ----
  titlePanel("Food graph"),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      visNetworkOutput(outputId = "Plot")
      
    )
  )


server <- function(input, output){
  output$Plot <- renderVisNetwork({

    
    #df = read.csv("C:/Users/KE/Desktop/Ing.csv", stringsAsFactors = FALSE)
    dfE = read.csv("Ing.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
    
    dfE <- dfE %>%
      filter(!grepl(pattern = 'John*', Recipe))
    
    
    df1 <- head(dfE, 400)
    #df1 <- dfE
    
    
    
    df1 %>%
      filter(Ingredient %in% c('cardamom')) %>%
      distinct(Recipe) -> Query_Rec; df1 %>%
      filter(Recipe %in% Query_Rec$Recipe)%>%
      distinct(Ingredient) -> Query_Ing; df1 %>%
      filter(Ingredient %in% Query_Ing$Ingredient) %>%
      distinct(Recipe) %>%
      count()
    
    
    ####Recipe and Ingredients####
    nodes <- data.frame(id = c(unique(df1$Recipe), unique(df1$Ingredient)), group = c(rep("Recipe", length(unique(df1$Recipe))), rep("Ingredients", length(unique(df1$Ingredient)))))
    edges <- data.frame(from = df1$Recipe, to = df1$Ingredient)
    
    
    g <- graph_from_data_frame(df1, directed = F)
    data <- toVisNetworkData(g)
    
    #visNetwork(data$nodes, data$edges) %>%
    visNetwork(nodes, edges) %>%
      visGroups(groupname = "Recipe", color = list(background = "darkseagreen", border = "darkslategray")) %>%
      visNodes(color = list(highlight = "red")) %>%
      visEdges(color = list(highlight = "red")) %>%
      #visIgraphLayout(type = "full") %>%
      #visIgraphLayout(layout = "layout_in_circle") %>%
      #visIgraphLayout(layout = "layout_with_sugiyama") %>% 
      visInteraction(hover = TRUE, selectConnectedEdges = TRUE, navigationButtons = TRUE) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hideColor = 'rgba(200,200,200,0)', labelOnly = FALSE), nodesIdSelection = TRUE) %>% #, selectedBy = "group") %>%
      visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -10))
    #visPhysics(stabilization = TRUE)
    #visPhysics(solver = "repulsion")
    #visClusteringByHubsize()
    
    
    # ggraph(tbl_graph(df1, directed = F), layout = "graphopt") + 
    #   geom_node_point() +
    #   geom_edge_link(alpha = 0.8) + 
    #   scale_edge_width(range = c(0.2, 2)) +
    #   geom_node_text(aes(label = df1$Ingredient), repel = TRUE) +
    #   labs(edge_width = "phone.call") +
    #   theme_graph()
    
    
    #simpleNetwork(df1, height="100px", width="100px", charge = -100, zoom = T)
    
    
    
    b <- as.data.frame(betweenness(g))
    
    
    
    # plot(edge.betweenness.community(g), g, layout = layout_with_dh, vertex.size = 3, vertex.label.dist = 0.5, vertex.label = ifelse(V(g)$name %in% V(g)$name[1:12], V(g)$name, NA))
    # 
    # plot(leading.eigenvector.community(g), g, layout = layout_with_dh, vertex.size = 3, vertex.label.dist = 0.5, vertex.label = ifelse(V(g)$name %in% V(g)$name[1:12], V(g)$name, NA))
    # 
    # plot(walktrap.community(g), g, layout = layout_with_dh, vertex.size = 3, vertex.label.dist = 0.5, vertex.label = ifelse(V(g)$name %in% V(g)$name[1:12], V(g)$name, NA))
    
    
    ####Cluster####
    
    graph <- graph.data.frame(df1, directed = F)
    graph <- simplify(graph)
    
    #gc <- fastgreedy.community(graph)
    gc <- edge.betweenness.community(graph)
    #gc = cluster_optimal(graph)
    #modularity(gc)
    #membership(gc)
    
    V(graph)$community <- gc$membership
    
    nodes <- data.frame(id = V(graph)$name, 
                        title = V(graph)$name, 
                        group = c(rep("Recipe", length(unique(df1$Recipe))), rep("Ingredients", length(unique(df1$Ingredient))))
    )
    
    nodes <- nodes[order(nodes$id, decreasing = F),]
    edges <- get.data.frame(graph, what = "edges")[1:2]
    
    
    visNetwork(nodes, edges) %>%
      visGroups(groupname = "Recipe", color = list(background = "darkseagreen", border = "darkslategray")) %>%
      #visClusteringByColor(colors = V(graph)$community) %>%
      #visClusteringByConnection(nodes = 9) %>%
      #visClusteringByGroup(groups = V(graph)$community, label = "Group : ", shape = "ellipse", color = "blue", force = F) %>%
      
      visNodes(color = list(highlight = "red")) %>%
      visEdges(color = list(highlight = "red")) %>%
      
      #visIgraphLayout(type = "full") %>%
      #visIgraphLayout(layout = "layout_in_circle") %>%
      #visIgraphLayout(layout = "layout_with_sugiyama") %>% 
      visInteraction(hover = TRUE, selectConnectedEdges = TRUE, navigationButtons = TRUE) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 3, hideColor = 'rgba(200, 200, 200, 0.3)', labelOnly = FALSE), nodesIdSelection = TRUE) %>% #, selectedBy = "group") %>%
      visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -10))
    #visPhysics(stabilization = TRUE)
    #visPhysics(solver = "repulsion")
    #visClusteringByHubsize()
  })

}

shinyApp(ui = ui, server = server)
