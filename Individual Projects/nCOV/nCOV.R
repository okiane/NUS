library(shiny)
library(tidyverse)
library(gsheet)
library(DT)
library(ggplot2)
library(plotly)
library(lubridate)
library(forcats)
# library(googlesheets)


# url = 'https://docs.google.com/spreadsheets/d/1yZv9w9zRKwrGTaR-YzmAqMefw4wMlaXocejdxZaTs6w/edit#gid=561110064'
url = 'https://docs.google.com/spreadsheets/d/1UF2pSkFTURko2OvfHWWlFpDFAr1UxCBA4JLwlSP6KFo/htmlview?sle=true'
# df = gsheet2tbl(url)

df = read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE, check.names=FALSE)

LastUpdate = mdy_hm(last(colnames(df)))

# df1 <- df %>%
#   group_by(`Country/Region`) %>%
#   summarise(Infected=sum(Infected), Deaths=sum(Deaths), Recovered=sum(Recovered)) %>%
#   arrange(desc(Infected))

df1 <- df %>%
  mutate_all(funs(replace_na(.,0))) %>%
  select(-c(`Province/State`, 'Lat', 'Long',`First confirmed date in country (Est.)`)) %>%
  group_by(`Country/Region`) %>%
  summarize_all(sum, na.rm = TRUE)

# df1 <- inner_join(df1, df[c("Country/Region", "First confirmed date in country (Est.)")], by="Country/Region") %>%
#   group_by("Country/Region")

df1 <- df1 %>%
  pivot_longer(
    cols = 2:ncol(df1),
    names_to = "Date",
    # names_prefix = "wk",
    values_to = "Infected",
    values_drop_na = TRUE
  )

df1 <- df1 %>%
  mutate(Date = mdy_hm(df1$Date))
# mutate(`First confirmed date in country (Est.)` = mdy(`First confirmed date in country (Est.)`))

ui <- fluidPage(
  
  mainPanel(
    h3("Number of infected cases of novel coronovirus as of "),
    LastUpdate,
    br(),
    tags$a(href='https://docs.google.com/spreadsheets/d/1UF2pSkFTURko2OvfHWWlFpDFAr1UxCBA4JLwlSP6KFo/htmlview?sle=true', 'Source'),
    br(),
    br(),
    
    sliderInput(inputId='SelectDate', label='Date', min=min(df1$Date), max=max(df1$Date), value=c(max(df1$Date)), width='100%'),
    br(),
    
    selectInput(inputId="SelectCountry", label="Select Country",
                choices = sort(unique(df1$`Country/Region`)),
                multiple = TRUE,
                selected = sort(unique(df1$`Country/Region`)),
                selectize = TRUE,
                width = '100%'
                ),
    br(),
    
    plotlyOutput("map"), 
    br(),
    
    # checkboxInput(inputId="LM", label="Select Trendline"),
    
    plotlyOutput("trend"),
    br(),
    
    plotlyOutput("num"),
    br(),
    
    DTOutput("table"),
    br()
  )
  
)

# Define the server logic
server <- function(input, output, session){
  observe({
    # Invalidate this observer every second (1000 milliseconds)
    invalidateLater(10000, session)
  
  # url = 'https://docs.google.com/spreadsheets/d/1yZv9w9zRKwrGTaR-YzmAqMefw4wMlaXocejdxZaTs6w/edit#gid=561110064'
  url = 'https://docs.google.com/spreadsheets/d/1UF2pSkFTURko2OvfHWWlFpDFAr1UxCBA4JLwlSP6KFo/htmlview?sle=true'
  # df = gsheet2tbl(url)
  
  df = read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE, check.names=FALSE)
  })
  
  
  LastUpdate = mdy_hm(last(colnames(df)))
  
  # df1 <- df %>%
  #   group_by(`Country/Region`) %>%
  #   summarise(Infected=sum(Infected), Deaths=sum(Deaths), Recovered=sum(Recovered)) %>%
  #   arrange(desc(Infected))
  
  df1 <- df %>%
    mutate_all(funs(replace_na(.,0))) %>%
    select(-c(`Province/State`, 'Lat', 'Long',`First confirmed date in country (Est.)`)) %>%
    group_by(`Country/Region`) %>%
    summarize_all(sum, na.rm = TRUE)
  
  # df1 <- inner_join(df1, df[c("Country/Region", "First confirmed date in country (Est.)")], by="Country/Region") %>%
  #   group_by("Country/Region")
  
  df1 <- df1 %>%
    pivot_longer(
      cols = 2:ncol(df1),
      names_to = "Date",
      # names_prefix = "wk",
      values_to = "Infected",
      values_drop_na = TRUE
    )
  
  df1 <- df1 %>%
    mutate(Date = mdy_hm(df1$Date))
  # mutate(`First confirmed date in country (Est.)` = mdy(`First confirmed date in country (Est.)`))
  
  
  
  library(tidyverse)
  
  library(countrycode)
  
  df1$CountryCode = countrycode(df1$`Country/Region`, 'country.name', 'iso3c')

  output$table <- renderDataTable({
    df1 %>%
      filter(`Country/Region` %in% input$SelectCountry) %>%
      filter(Date <= input$SelectDate) %>%
      #select(-c('Province/State', 'CountryCode')) %>%
      group_by(`Country/Region`) %>%
      filter(Date == max(Date)) %>%
      summarise(Infected = sum(Infected)) %>%
      arrange(desc(Infected))
      
  })
  


  library(ggplot2)
  library(plotly)
  
  df1$Text <- with(df1, paste0("Infected in ",
                               `Country/Region`))

  output$num <- renderPlotly({
    library(forcats)
    ggplotly(df1 %>%
               #select(-c('Province/State', 'CountryCode')) %>%
               filter(Date <= input$SelectDate) %>%
               filter(Date == max(Date)) %>%
               group_by(`Country/Region`, Date, Infected) %>%
               arrange(desc(Infected)) %>%
               # summarise(Infected = sum(Infected)) %>%
               
               filter(`Country/Region` %in% input$SelectCountry) %>%
               # ggplot(aes(x=reorder(`Country/Region`, -Infected),
               ggplot(aes(x=reorder(`Country/Region`, -Infected),
                          y=Infected,
                          fill=`Country/Region`)) +
               geom_bar(stat='identity') +
               labs(y= "Cumulative Number of Infected", x = "Country") +
               theme(axis.text.x = element_text(angle = 45, hjust = 1),
                     legend.position = "none"),
             tooltip=c("Country/Region", "Infected")
    )
  
  
  })
  
  
  output$trend <- renderPlotly({
    ggplotly(df1 %>%
               # select(-c('Province/State', 'CountryCode')) %>%
               filter(Date <= input$SelectDate) %>%
               # mutate(Date = format(as.Date(df1$Date), "%d %b %Y")) %>%
               select(c(`Country/Region`, 'Date', 'Infected')) %>%
               filter(`Country/Region` %in% input$SelectCountry) %>%
               group_by(`Country/Region`, Date) %>%
               
               arrange(desc(Infected)) %>%
               
               # ggplot(aes(x=reorder(`Country/Region`, -Infected),
               ggplot(aes(x=Date,
                          y=Infected,
                          color=`Country/Region`)) +
               geom_line() +
               stat_smooth(alpha=0.1, method="auto", se=TRUE, fullrange=FALSE, level=0.95, size=0.1, linetype=0) +
               labs(y= "Cumulative Number of Infected", x = "Country")
      #          theme(axis.text.x = element_text(angle = 45, hjust = 1))) %>%
      #                # legend.position = "none")) %>%
      # layout(
      #   xaxis = list(categoryorder = "sum descending")
      )
  })
  

  output$map <- renderPlotly({
    plot_geo(d<-df1 %>% 
               filter(`Country/Region` %in% input$SelectCountry)) %>%
               filter(Date <= input$SelectDate) %>%
               group_by(`Country/Region`) %>% 
               slice(which.max(Date)) %>%
      add_trace(
        z = ~Infected, 
        color = ~Infected, 
        # size = ~Infected, 
        colors = 'Reds',
        text = ~Text, 
        locations = ~CountryCode,
        marker = list(line = list(width = 0.5)) #color = toRGB("grey"), 
      ) %>%
      
      colorbar(title = 'Cumulative Number of Infected') %>%
      layout(
        # title = 'Number of nCOV',
        geo = list(
          showframe = FALSE,
          # showcoastlines = FALSE,
          # projection = list(type = 'orthographic'),
          resolution = 50
        )
      )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)