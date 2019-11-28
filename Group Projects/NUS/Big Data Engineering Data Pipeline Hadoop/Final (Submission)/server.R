#install.packages('rsconnect')
#to deploy the app
#library(rsconnect)
#deployApp()
#terminateApp()

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(pacman)
pacman::p_load(tidyverse, shiny, shinyjs, stringr, DT, visNetwork, igraph, visNetwork)

##### ACCESSING THE DATABASE
##### 2 parts to adjust when switching to drill. Start and function: updaterecipes


##### ACCESSING THE DATABASE
##### 2 parts to adjust when switching to drill. Start and function: updaterecipes

db_select = "drill" #/ "local"

if (db_select == "local"){
  ######## USE THIS SECTION FOR LOCAL DB QUERY
  pacman::p_load(RSQLite)
  mydbconn = dbConnect(RSQLite::SQLite(), "Recipes.db")
  myrecipes = dbGetQuery(mydbconn, "SELECT * FROM Recipes_ID limit 1000")
  recList = myrecipes$ID
  query <- paste0("SELECT * FROM RIGraph WHERE CAST (ID AS INT) IN (", paste0(recList, collapse = ","), ");")
  
  RIGraph = dbGetQuery(mydbconn, query)
  
} else {
  ####### USE THIS SECTION FOR DRILL QUERY
  pacman::p_load(sergeant)
  mydbconn = drill_connection("localhost") # USE THIS FOR DRILL QUERY
  myrecipes <- drill_query(mydbconn, "SELECT * FROM hdfs.food.`Recipe` LIMIT 1000") %>% as.data.frame()
  recList = myrecipes$ID
  query <- paste0("SELECT * FROM hdfs.food.`Ing` WHERE CAST (ID AS INT) IN (", paste0(recList, collapse = ","), ");")
  
  RIGraph = drill_query(mydbconn, query) %>% as.data.frame()
}

#######
flavourslist = unique(RIGraph$Ingredient)

updaterecipes = function(input) {
  #default labels
  veggie = 0
  hasnuts = 1
  hasdairy = 1
  hasseafood = 1
  ######
  
  if (input$isVegan) {
    veggie = 1
  }
  if (input$isNuts) {
    hasnuts = 0
  }
  if (input$isDairy) {
    hasdairy = 0
  }
  if (input$isSeafood) {
    hasseafood = 0
  }
  
  
  if (db_select == "local"){
  ##### USE THIS SECTION FOR LOCAL DB QUERY
  
  myrecipes <<- dbGetQuery(mydbconn, 'SELECT * FROM Recipes_ID
                     WHERE vegetarian_label >= ? AND
                     nut_label <= ? AND
                     lactose <= ? AND
                     seafood <= ? LIMIT 1000', params = c(veggie, hasnuts, hasdairy, hasseafood)) %>% arrange(title)
  recList = myrecipes$ID
  query <- paste0("SELECT * FROM RIGraph WHERE CAST (ID AS INT) IN (", paste0(recList, collapse = ","), ");")
  RIGraph <<- dbGetQuery(mydbconn, query)
  
  } else {
  ##### USE THIS SECTION FOR DRILL
  search_query = paste0('SELECT * FROM hdfs.food.`Recipe` WHERE',
                        ' vegetarian_label >= ', veggie,
                        ' AND nut_label <= ', hasnuts,
                        ' AND lactose <= ', hasdairy,
                        ' AND seafood <= ', hasseafood,
                        'LIMIT 1000;')

  myrecipes <<- drill_query(mydbconn, search_query) %>% arrange(title)
  recList = myrecipes$ID
  query <- paste0("SELECT * FROM hdfs.food.`Ing` WHERE CAST (ID AS INT) IN (", paste0(recList, collapse = ","), ")")
  RIGraph <<- drill_query(mydbconn, query)
  }
  
  
  flavourslist <<- unique(RIGraph$Ingredient)
}

cleanstr = function(mystr) {
  #split string from python.
  mystr = str_replace(mystr, '\\[','')
  mystr = str_replace(mystr, '\\]','')
  str_extract_all(mystr, "'(.*?)'")
}

updatePage = function(chosenRecipe) {
  myDT = myrecipes %>%
    filter(ID == chosenRecipe)
  
  recipetable = DT::datatable(
      myDT %>%
        select(title, description, rating_stars, review_count, url, total_time_minutes) %>%
        `colnames<-`(c('Recipe','Description','Average Rating','Number of Ratings','URL', 'Preparation Time')) %>%
        gather("Properties",""),
      class = "table-primary",
      options = list(dom = 't',
                     pageLength = 10),
      rownames = FALSE
    )
  recipeimg = c('<img src="', myrecipes %>%
                               filter(ID == chosenRecipe) %>% pull(photo_url),'"width="380" height="300">')
  
  recipeInstructions = DT::datatable(
      as.data.frame(cleanstr(myDT %>% pull(instructions))) %>%
        `colnames<-`('Instructions'),
      options = list(dom = 't',
                     pageLength = 20)
    )
  recipeIngredients = DT::datatable(
      as.data.frame(cleanstr(myDT %>% pull(ingredients))) %>%
        `colnames<-`('Ingredients'),
      options = list(dom = 't',
                     pageLength = 20),
      rownames = FALSE)
  return(list(recipetable, recipeimg, recipeIngredients, recipeInstructions))
}

construct_graph = function(choseningredients) {
  graphID = RIGraph %>%
    filter(Ingredient %in% choseningredients) %>%
    pull(ID) %>% unique()

  graphID = sample(graphID)[1:20]
  graph_df = RIGraph %>%
    filter(ID %in% graphID)
  
  nodes <- data.frame(id = c(unique(graph_df$Recipe), unique(graph_df$Ingredient))) %>%
    mutate(item = case_when(id %in% graph_df$Recipe ~ "Recipe",
                            TRUE ~ "Ingredient"),
           title = id) %>%
    mutate(value = case_when(item == "Recipe" ~ 2,
                             TRUE ~ 1),
           shape = case_when(item == "Recipe" ~ "square",
                             TRUE ~ "circle"))
           #shadow = case_when(item == "Recipe" ~ TRUE,
           #                  TRUE ~ FALSE))
  
  ### Using igraph to get clusters.
  graph <- graph.data.frame(graph_df, directed = F)
  graph <- simplify(graph)
  
  #gc <- fastgreedy.community(graph)
  gc <- edge.betweenness.community(graph)
  V(graph)$community <- gc$membership
  
  nodes = nodes %>%
    mutate(group = V(graph)$community)
  
  edges <- data.frame(from = graph_df$Recipe, to = graph_df$Ingredient)
  
  return(list(nodes, edges))
}


function(input, output, session) {
  rvs = reactiveValues(recipes = NULL,
                       ingredients = NULL,
                       recrecipes = myrecipes %>% sample_n(6) %>% pull(ID), #These are recipe IDs
                       recipetable = NULL,
                       recipeimg = NULL,
                       recipeIngredients = NULL,
                       recipeInstructions = NULL,
                       IoTpantry = as.data.frame(c('blueberry','basil','lobster','poppy seed','mustard','apple')) %>%
                         `colnames<-`("Pantry"),
                       recIngredient = "salmon",
                       recIngredient2 = "basil")
######
# When the Apply button is clicked, Compute the graph
  observeEvent(input$updateRecipes, {
    shinyjs::enable('selectRecipe1')
    shinyjs::enable('selectRecipe2')
    shinyjs::enable('selectRecipe3')
    shinyjs::enable('selectRecipe4')
    shinyjs::enable('selectRecipe5')
    shinyjs::enable('selectRecipe6')
    
    updaterecipes(input)
    updateSelectInput(session, 'ManualSelect', choices = c('', myrecipes$title))
    rvs$recrecipes = myrecipes$ID %>%
      sample(6)
    output$test = renderText(nrow(myrecipes))
    updateSelectizeInput(session,
                         inputId = "IngredientsInput",
                         choices = flavourslist,
                         selected = "",
                         options = list()
    )
    output$recommendationText = NULL
    output$recommendationText2 = NULL
  }) #Update recipes button
  
  observeEvent(input$selectRecipe1, {
    updateTabsetPanel(session,
                      inputId = "main",
                      selected = "Recommendations")
    stuff = updatePage(rvs$recrecipes[1])
    rvs$recipetable = stuff[[1]]
    rvs$recipeimg = stuff[[2]]
    rvs$recipeIngredients = stuff[[3]]
    rvs$recipeInstructions = stuff[[4]]
  }) # Individual buttons below the relevant recipes. To update recommendations tab.
  observeEvent(input$selectRecipe2, {
    updateTabsetPanel(session,
                      inputId = "main",
                      selected = "Recommendations")
    stuff = updatePage(rvs$recrecipes[2])
    rvs$recipetable = stuff[[1]]
    rvs$recipeimg = stuff[[2]]
    rvs$recipeIngredients = stuff[[3]]
    rvs$recipeInstructions = stuff[[4]]
  })
  observeEvent(input$selectRecipe3, {
    updateTabsetPanel(session,
                      inputId = "main",
                      selected = "Recommendations")
    stuff = updatePage(rvs$recrecipes[3])
    rvs$recipetable = stuff[[1]]
    rvs$recipeimg = stuff[[2]]
    rvs$recipeIngredients = stuff[[3]]
    rvs$recipeInstructions = stuff[[4]]
  })
  observeEvent(input$selectRecipe4, {
    updateTabsetPanel(session,
                      inputId = "main",
                      selected = "Recommendations")
    stuff = updatePage(rvs$recrecipes[4])
    rvs$recipetable = stuff[[1]]
    rvs$recipeimg = stuff[[2]]
    rvs$recipeIngredients = stuff[[3]]
    rvs$recipeInstructions = stuff[[4]]
  })
  observeEvent(input$selectRecipe5, {
    updateTabsetPanel(session,
                      inputId = "main",
                      selected = "Recommendations")
    stuff = updatePage(rvs$recrecipes[5])
    rvs$recipetable = stuff[[1]]
    rvs$recipeimg = stuff[[2]]
    rvs$recipeIngredients = stuff[[3]]
    rvs$recipeInstructions = stuff[[4]]
  })
  observeEvent(input$selectRecipe6, {
    updateTabsetPanel(session,
                      inputId = "main",
                      selected = "Recommendations")
    stuff = updatePage(rvs$recrecipes[6])
    rvs$recipetable = stuff[[1]]
    rvs$recipeimg = stuff[[2]]
    rvs$recipeIngredients = stuff[[3]]
    rvs$recipeInstructions = stuff[[4]]
  })
  observeEvent(input$RecipeButton, {
    updateTabsetPanel(session,
                      inputId = "main",
                      selected = "Recommendations")
    stuff = updatePage(myrecipes %>% filter(title == input$ManualSelect) %>% pull(ID))
    rvs$recipetable = stuff[[1]]
    rvs$recipeimg = stuff[[2]]
    rvs$recipeIngredients = stuff[[3]]
    rvs$recipeInstructions = stuff[[4]]
  })
  
  observeEvent(input$useIOT, {
    toggle("PantryInput")
    updateSelectizeInput(session,
                         inputId = "PantryInput",
                         selected = "",
                         choices = rvs$IoTpantry)
  })
  
  observe({
    toggle("IngredientsInput", condition = !input$useIOT)
  })
  
  observeEvent(c(input$Return, input$Return2), {
    updateTabsetPanel(session,
                      inputId = "main",
                      selected = "User Selection")
  }) #Button on the Recommendations page.
  
  observeEvent(input$getRecommendations, { #GetRecommendations based on the ingredients he picked.
    if (input$useIOT) {
      myings = input$PantryInput
    } else {
      myings = input$IngredientsInput
    }
    shinyjs::enable('selectRecipe1')
    shinyjs::enable('selectRecipe2')
    shinyjs::enable('selectRecipe3')
    shinyjs::enable('selectRecipe4')
    shinyjs::enable('selectRecipe5')
    shinyjs::enable('selectRecipe6')
    output$norecipeText = NULL
    
    if (length(myings) > 0) {
    availablerecipes = RIGraph %>%
      filter(Ingredient %in% myings) %>%
      pull(ID)
    
    rvs$recrecipes = sample(availablerecipes)[1:6]
    output$recommendationText = renderText({c('<p style="font-size: 14px; color: royalblue; white-space: normal;">',
                                              myings[1], 'is often cooked with', rvs$recIngredient, '</p>')})
    if (length(myings) > 1) {
      output$recommendationText2 = renderText({c('<p style="font-size: 14px; color: royalblue; white-space: normal;">',
                                                 myings[2], 'is often cooked with',rvs$recIngredient2, '</p>')})
    }
    if (is.na(rvs$recrecipes[1])) {
      shinyjs::disable("selectRecipe1")
      output$norecipeText = renderText("No available recipes with these ingredients in your pantry.")
      output$recommendationText = NULL
    }
    
    if (is.na(rvs$recrecipes[2])) {
      shinyjs::disable("selectRecipe2")
    }
    if (is.na(rvs$recrecipes[3])) {
      shinyjs::disable("selectRecipe3")
    }
    if (is.na(rvs$recrecipes[4])) {
      shinyjs::disable("selectRecipe4")
    }
    if (is.na(rvs$recrecipes[5])) {
      shinyjs::disable("selectRecipe5")
    }
    if (is.na(rvs$recrecipes[6])) {
      shinyjs::disable("selectRecipe6")
    }
    } else {
      rvs$recrecipes = myrecipes$ID %>%
        sample(6)
      updateSelectizeInput(session,
                           inputId = "IngredientsInput",
                           selected = "",
                           options = list()
      )
      output$recommendationText = NULL
      output$recommendationText2 = NULL
    }

  }) #Randomise input. This is for testing.
  
  observeEvent(input$viewGraph, {
    if (input$useIOT) {
      myings = input$PantryInput
    } else {
      myings = input$IngredientsInput
    }
    
    if (length(myings) == 0) {
      showNotification(paste("No Ingredients Selected."), duration = 5, type = "error")
    } else {
      stuff = construct_graph(myings)
      
      output$mygraph = renderVisNetwork({
        visNetwork(stuff[[1]], stuff[[2]]) %>%
          #visIgraphLayout(type = "full") %>%
          #visIgraphLayout(layout = "layout_in_circle") %>%
          #visIgraphLayout(layout = "layout_with_sugiyama") %>%
          visInteraction(hover = TRUE, selectConnectedEdges = TRUE) %>%
          visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hideColor = 'rgba(200,200,200,0.2)', labelOnly = FALSE), nodesIdSelection = TRUE, selectedBy = "group") %>% 
          visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -10))
      })
      updateTabsetPanel(session,
                        inputId = "main",
                        selected = "Relational Analysis")
    }
  })
  observeEvent(input$randomRecipe, {
    shinyjs::enable('selectRecipe1')
    shinyjs::enable('selectRecipe2')
    shinyjs::enable('selectRecipe3')
    shinyjs::enable('selectRecipe4')
    shinyjs::enable('selectRecipe5')
    shinyjs::enable('selectRecipe6')
    
    rvs$recrecipes = myrecipes$ID %>%
      sample(6)
    updateSelectizeInput(session,
                         inputId = "IngredientsInput",
                         selected = "",
                         options = list()
                         )
    output$recommendationText = NULL
    output$recommendationText2 = NULL
  }) #Randomise input. This is for testing.

  
  observeEvent(input$Reset, {
    shinyjs::reset("selection-panel")
  })   #when the Reset button is clicked, remove all input values

  output$imgurl1 = renderText({c('<img src="',myrecipes %>%
                                   filter(ID == rvs$recrecipes[1]) %>% pull(photo_url),'"width="150" height="150">')}) #Update the pictures of all recipes.
  output$imgurl2 = renderText({c('<img src="',myrecipes %>%
                                   filter(ID == rvs$recrecipes[2]) %>% pull(photo_url),'"width="150" height="150">')})
  output$imgurl3 = renderText({c('<img src="',myrecipes %>%
                                   filter(ID == rvs$recrecipes[3]) %>% pull(photo_url),'"width="150" height="150">')})
  output$imgurl4 = renderText({c('<img src="',myrecipes %>%
                                   filter(ID == rvs$recrecipes[4]) %>% pull(photo_url),'"width="150" height="150">')})
  output$imgurl5 = renderText({c('<img src="',myrecipes %>%
                                   filter(ID == rvs$recrecipes[5]) %>% pull(photo_url),'"width="150" height="150">')})
  output$imgurl6 = renderText({c('<img src="',myrecipes %>%
                                   filter(ID == rvs$recrecipes[6]) %>% pull(photo_url),'"width="150" height="150">')})

  output$recipename1 = renderText({c('<p style="font-size: 14px; width: 150px; white-space: normal;">',myrecipes %>%
                                       filter(ID == rvs$recrecipes[1]) %>% pull(title), '</p>')})
  output$recipename2 = renderText({c('<p style="font-size: 14px; width: 150px; white-space: normal;">',myrecipes %>%
                                       filter(ID == rvs$recrecipes[2]) %>% pull(title), '</p>')})
  output$recipename3 = renderText({c('<p style="font-size: 14px; width: 150px; white-space: normal;">',myrecipes %>%
                                       filter(ID == rvs$recrecipes[3]) %>% pull(title), '</p>')})
  output$recipename4 = renderText({c('<p style="font-size: 14px; width: 150px; white-space: normal;">',myrecipes %>%
                                       filter(ID == rvs$recrecipes[4]) %>% pull(title), '</p>')})
  output$recipename5 = renderText({c('<p style="font-size: 14px; width: 150px; white-space: normal;">',myrecipes %>%
                                       filter(ID == rvs$recrecipes[5]) %>% pull(title), '</p>')})
  output$recipename6 = renderText({c('<p style="font-size: 14px; width: 150px; white-space: normal;">',myrecipes %>%
                                       filter(ID == rvs$recrecipes[6]) %>% pull(title), '</p>')})
  
  output$recipetable = DT::renderDataTable(rvs$recipetable)
  output$img = renderText(rvs$recipeimg)
  output$instructions = DT::renderDataTable(rvs$recipeInstructions)
  output$ingredients = DT::renderDataTable(rvs$recipeIngredients)
  output$recommendationText = NULL
  output$recommendationText2 = NULL
  
  output$IOTTable = DT::renderDataTable(rvs$IoTpantry, rownames = FALSE,
                                        class = "table-borderless",
                                        options = list(dom = 't',
                                                       paging = FALSE))
  output$norecipeText = NULL
  
  # session$onSessionEnded(function() {
  #   dbDisconnect(mydbconn)
  #   print('Hello, the session has ended')
  # })
}

