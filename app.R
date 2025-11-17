library(tidyverse)
library(shiny)
library(DT)
library(plotly)
library(bslib)

eng <- read.csv("C:/Users/naufa/Documents/MATH 348/eng.csv", fileEncoding = "UTF-8")

home_obs <- data.frame(
  Team = eng$HomeTeam,
  Home_Away = "Home",
  TotalShots = eng$HS,
  TotalGoals = eng$FTHG,
  eng
  
)

away_obs <- data.frame(
  Team = eng$AwayTeam,
  Home_Away = "Away",
  TotalShots = eng$AS,
  TotalGoals = eng$FTAG,
  eng
  
)

eng_reshaped <- rbind(home_obs, away_obs)

# Optionally reorder columns for better readability
eng_reshaped <- eng_reshaped[, c("Team", "Home_Away", "TotalShots", "TotalGoals",
                                 
                                 setdiff(names(eng_reshaped), c("Team", "Home_Away", "TotalShots", "TotalGoals")))]

eng_reshaped$Home_Away = as.factor(eng_reshaped$Home_Away)
eng_reshaped$Team = as.factor(eng_reshaped$Team)

eng_reshaped <- eng_reshaped %>%
  mutate(Result = case_when(
    
    FTR == "D" ~ "Draw", # Draw is always Draw
    FTR == "H" & Home_Away == "Home" ~ "Win",
    FTR == "A" & Home_Away == "Away" ~ "Win",
    TRUE ~ "Loss"
    
  ))


ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel("English Football: Top Five Divisions 2005–2024"),
  
  p("This dashboard displays statistical data
from the top 5 English football leagues
(Premier League, Championship, League One,
League Two, and National League) covering
the period from 2005 to 2024. The data
includes team performance metrics, match
results, and other relevant football statistics."),
  
  fluidRow(
    column(
      
      4,
      wellPanel(
        
        h5("Scatter Plot Filters"),
        selectInput("Team_scatter", "Team",
                    
                    choices = unique(eng_reshaped$Team),
                    selected = unique(eng_reshaped$Team)[1],
                    multiple = FALSE),
        
        checkboxGroupInput("Home_Away_scatter", "Match Location",
                           choices = unique(eng_reshaped$Home_Away),
                           selected = unique(eng_reshaped$Home_Away))
        
      )
    ),
    column(
      
      8,
      plotlyOutput("scatterPlot"),
      hr(),
      dataTableOutput("dataTable1")
      
    )
  ),
  
  fluidRow(
    column(
      
      4,
      wellPanel(
        
        h5("Bar Plot Filters"),
        selectInput("Team_bar", "Team",
                    
                    choices = unique(eng_reshaped$Team),
                    selected = unique(eng_reshaped$Team)[1],
                    multiple = FALSE),
        
        checkboxGroupInput("Home_Away_bar", "Match Location",
                           choices = unique(eng_reshaped$Home_Away),
                           selected = unique(eng_reshaped$Home_Away))
        
      )
    ),
    column(
      
      8,
      plotlyOutput("barPlot"),
      hr(),
      dataTableOutput("dataTable2")
      
    )
  )
  
)

server <- function(input, output, session) {
  
  filtered_data_scatter <- reactive({
    req(input$Team_scatter)
    eng_reshaped %>%
      
      filter(Team == input$Team_scatter) %>%
      filter(Home_Away %in% input$Home_Away_scatter)
    
  })
  
  filtered_data_bar <- reactive({
    req(input$Team_bar)
    eng_reshaped %>%
      
      filter(Team == input$Team_bar) %>%
      filter(Home_Away %in% input$Home_Away_bar)
    
  })
  
  output$scatterPlot <- renderPlotly({
    df <- filtered_data_scatter()
    ggplotly(
      
      ggplot(df) +
        geom_point(aes(x = TotalShots, y = TotalGoals, color = Home_Away)
                   
                   , size = 1.5, alpha = 0.8) +
        labs(title = "Shots to Goal Conversion", x = "Total Shots", y = "Total Goals", color = "Match Location") +
        scale_color_brewer(palette = "Dark2") +
        theme_minimal(base_size = 14)
      
    )
  })
  
  output$barPlot <- renderPlotly({
    df2 <- filtered_data_bar() %>%
      
      count(Result, Home_Away)
    ggplotly(
      
      ggplot(df2, aes(x = as.factor(Result), y = n, fill = Home_Away)) +
        geom_col(position = position_dodge()) +
        labs(title = "Match Results, Since 2005/06", x = "Match Result"
             
             , y = "Count", fill = "Match Location") +
        scale_fill_brewer(palette = "Dark2") +
        theme_minimal(base_size = 14)
      
    )
  })
  
  output$dataTable1 <- renderDataTable({
    datatable(filtered_data_scatter(), options = list(pageLength = 10))
    
  })
  
  output$dataTable2 <- renderDataTable({
    datatable(filtered_data_bar(), options = list(pageLength = 10))
    
  })
}

shinyApp(ui = ui, server = server)



