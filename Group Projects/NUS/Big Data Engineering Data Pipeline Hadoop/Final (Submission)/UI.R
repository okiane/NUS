library(pacman)
pacman::p_load(dplyr, shiny, shinyjs, shinythemes, shinycssloaders, DT, visNetwork)

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


######
fluidPage(theme = "bootstrap.css",
          
          br(),
          titlePanel("Recipes Recommender"),
          br(),
          # Setting up app defaults.
          
          useShinyjs(),
          inlineCSS(list("table" = "font-size: 12px")),
          
          ######
          # First tab
          tabsetPanel(id = "main",
                      type = "pills",
                      tabPanel("User Selection",
                               br(),
                               column(6,
                                      fluidRow(
                                        column(12,
                                               #this is the top right panel
                                               h3(print("1. Recipe Tags")),
                                               br(),
                                               h4(HTML("<b>Pick relevant tags for the recipes you want.</b>")),
                                               column(3,
                                                      tags$div(class = 'custom-switch',
                                                               tags$input(type = 'checkbox',
                                                                          class ="custom-control-input",
                                                                          id = 'isVegan'),
                                                               tags$label(class = "custom-control-label",
                                                                          style = "font-size:12px",
                                                                          'for' = "isVegan",
                                                                          HTML('Vegetarian Only')))
                                               ),
                                               column(3,
                                                      tags$div(class = 'custom-switch',
                                                               tags$input(type = 'checkbox',
                                                                          class ="custom-control-input",
                                                                          id = 'isNuts'),
                                                               tags$label(class = "custom-control-label",
                                                                          style = "font-size:12px",
                                                                          'for' = "isNuts",
                                                                          HTML('No Nut Allergen')))
                                               ),
                                               column(3,
                                                      tags$div(class = 'custom-switch',
                                                               tags$input(type = 'checkbox',
                                                                          class ="custom-control-input",
                                                                          id = 'isDairy'),
                                                               tags$label(class = "custom-control-label",
                                                                          style = "font-size:12px",
                                                                          'for' = "isDairy",
                                                                          HTML('No Dairy Products')))
                                               ),
                                               column(3,
                                                      tags$div(class = 'custom-switch',
                                                               tags$input(type = 'checkbox',
                                                                          class ="custom-control-input",
                                                                          id = 'isSeafood'),
                                                               tags$label(class = "custom-control-label",
                                                                          style = "font-size:12px",
                                                                          'for' = "isSeafood",
                                                                          HTML('No Seafood-allergens')))
                                               )
                                        )),
                                      fluidRow(
                                        column(12,
                                               actionButton("updateRecipes", "Update Recipes", class = "btn btn-outline-primary"))),
                                      br(),
                                      fluidRow(
                                        column(12,
                                               wellPanel(
                                                 h4(print("Search for a recipe that you fancy!")),
                                                 selectInput(inputId = 'ManualSelect',
                                                             label = '',
                                                             choices = '',
                                                             selected = ''),
                                                 actionButton("RecipeButton", "Go to Selected Recipe", class = "btn btn-outline-info")
                                               ))),
                                      fluidRow(
                                        column(12,
                                        h3(print("2. Recipe Recommendation based on ingredients!")))),
                                      fluidRow( #This is the ingredient panel.
                                        column(12,
                                               br(),
                                               wellPanel(
                                                 id = "ingredient-panel",
                                                 h4(HTML("<b>Pick ingredients that you want to cook with!</b>")),
                                                 h4(HTML("<b>You may also choose to get recipes for the ingredients found in your pantry.</b>")),
                                                 br(),
                                                 column(12,
                                                 tags$div(class = 'custom-checkbox',
                                                          tags$input(type = 'checkbox',
                                                                     class ="custom-control-input",
                                                                     id = 'useIOT'),
                                                          tags$label(class = "custom-control-label",
                                                                     style = "font-size:12px",
                                                                     'for' = "useIOT",
                                                                     HTML('Use Ingredients in your pantry')))),
                                                 br(),
                                                 selectizeInput("PantryInput", "",
                                                                options = list(
                                                                               maxItems = 2,
                                                                               placeholder = 'Pick up to two Ingredients from your pantry.',
                                                                               onInitialize = I('function() { this.setValue(""); }')),
                                                                choices = ""),
                                                 tags$head(tags$style(".selectize-control.single { width: auto; z-index: 3; }")),
                                                 selectizeInput("IngredientsInput",
                                                                label = "",
                                                                choices = flavourslist,
                                                                selected = NULL,
                                                                options = list(
                                                                  maxItems = 2,
                                                                  placeholder = 'Pick up to two Ingredients you want to cook with, eg. Cinnamon, Pork, ...',
                                                                  onInitialize = I('function() { this.setValue(""); }')
                                                                )),
                                                 fluidRow(
                                                   column(3,
                                                          actionButton("getRecommendations", "Get Recommendations", class = "btn btn-outline-primary")),
                                                   column(3,
                                                          actionButton("viewGraph", "Explore Connectivity", class = "btn btn-outline-success")),
                                                   column(3,
                                                          actionButton("randomRecipe", "Remove Ingredients (Randomise Recipes)", class = "btn btn-outline-danger"))),
                                                 fluidRow(
                                                   column(12,
                                                          br(),
                                                          htmlOutput("recommendationText"),
                                                          htmlOutput("recommendationText2"),
                                                          htmlOutput("norecipeText")))                                                        
                                               )))
                               ),
                               column(6,
                                      wellPanel(
                                        fluidRow(
                                          column(6, align = "center",
                                                 wellPanel(
                                                   htmlOutput("imgurl1"),
                                                   htmlOutput("recipename1"),
                                                   actionButton("selectRecipe1","Go to Recipe", class = "btn btn-outline-info"))
                                                   ),
                                          column(6, align = "center",
                                                 wellPanel(
                                                   htmlOutput("imgurl2"),
                                                   htmlOutput("recipename2"),
                                                   actionButton("selectRecipe2","Go to Recipe", class = "btn btn-outline-info"))
                                          )
                                        ),
                                        fluidRow(
                                          column(6, align = "center",
                                                 wellPanel(
                                                   htmlOutput("imgurl3"),
                                                   htmlOutput("recipename3"),
                                                   actionButton("selectRecipe3","Go to Recipe", class = "btn btn-outline-info"))
                                          ),
                                          column(6, align = "center",
                                                 wellPanel(
                                                   htmlOutput("imgurl4"),
                                                   htmlOutput("recipename4"),
                                                   actionButton("selectRecipe4","Go to Recipe", class = "btn btn-outline-info"))
                                          )
                                        ),
                                        fluidRow(
                                          column(6, align = "center",
                                                 wellPanel(
                                                   htmlOutput("imgurl5"),
                                                   htmlOutput("recipename5"),
                                                   actionButton("selectRecipe5","Go to Recipe", class = "btn btn-outline-info"))
                                          ),
                                          column(6, align = "center",
                                                 wellPanel(
                                                   htmlOutput("imgurl6"),
                                                   htmlOutput("recipename6"),
                                                   actionButton("selectRecipe6","Go to Recipe", class = "btn btn-outline-info"))
                                          ))
                                      ))),
                      tabPanel("Recommendations",
                               br(),
                               fluidRow(
                                 column(4,
                                        h3("These are your Recommendations.")),
                                 column(2,
                                        actionButton("Return", "Return to Selection", class = "btn btn-info"))),
                                br(),
                                fluidRow(
                                column(6,
                                       #Top left Panel
                                       # tags$ul(
                                       #   htmlOutput("ingredients1", container = tags$li, class = "custom-li-output")
                                       # )
                                       DT::dataTableOutput("recipetable")
                                ),
                                column(6,
                                       wellPanel(
                                       #top right panel
                                       htmlOutput("img") %>% withSpinner((type = 1)))
                                )),
                                fluidRow(
                                  column(6,
                                         DT::dataTableOutput("ingredients")),
                                  column(6,
                                         DT::dataTableOutput("instructions"))
                                )),
                      tabPanel("Relational Analysis",
                               fluidRow(column(2, actionButton("Return2", "Return to Selection", class = "btn btn-info"))),
                               br(),
                               visNetworkOutput("mygraph", width = "1000px", height = "800px")
                      )
           )
)
