# ===============================================
# Fill in the following fields
# ===============================================
# Title: App 3
# Description: Text Analysis of GOT
# Details: Performs and Visualizes Word Trend and Sentiment Analysis On Game of Thrones Script
# Author: Jenae Harris
# Date: 11/29/23


# ===============================================
# R packages
# (you can use other packages if you want to)
# ===============================================
library(shiny)
library(tidyverse) # for data manipulation and graphics
library(tidytext)  # for text mining
library(plotly)    # for web-interactive graphics
library(DT)        # to work with HTML table widgets


# =======================================================
# Sentiment Lexicons
# Comment these commands out if you are not doing sentiment analysis
# =======================================================
#bing = read_csv("bing.csv", col_types = "cc")
#afinn = read_csv("afinn.csv", col_types = "cc")
#nrc = read_csv("nrc.csv", col_types = "cc")
#loughran = read_csv("loughran.csv", col_types = "cc")


# ===============================================
# Import data
# ===============================================
dat = read_csv(
   file = "Game_of_Thrones_Script.csv", 
   col_names = c("Date", "Season", "Episode", "Title", "Name", "Sentence"),
   skip = 1,
   col_types = cols(
     Date = col_character(),
     Season = col_character(),
     Episode = col_character(),
     Title = col_character(),
     Name = col_character(),
     Sentence = col_character()
   ))

# for demo purposes in this "template", we use data starwars
# (but you will have to replace this with Game-of-Thrones data)




# ===============================================
# Define "ui" for application
# ===============================================
ui <- fluidPage(
  
  # Application title
  titlePanel("App 3: Game of Thrones Text Analysis"),
  hr(),
  
  # -------------------------------------------------------
  # Input widgets 
  # Customize the following dummy widgets with your own inputs
  # -------------------------------------------------------
  fluidRow(
    # replace with your widgets
    column(3,
           p(em("Choose A Season")),
           radioButtons(inputId = "season", 
                        label = "Seasons", 
                        choices = c("Season 1" = 1,
                                    "Season 2" = 2,
                                    "Season 3" = 3,
                                    "Season 4" = 4,
                                    "Season 5" = 5,
                                    "Season 6" = 6,
                                    "Season 7" = 7,
                                    "Season 8" = 8))
    ), # closes column 1
    column(3,
           p(em("Choose An Episode")),
           numericInput(inputId = "episode", 
                        label = "Episode", 
                        value = 1,
                        min = 1,
                        max = 10,
                        step = 1)
    ), # closes column 2
    column(3,
           p(em("Facet By Sentiment?")),
           checkboxInput(inputId = "sent_facet",
                         label = "Facet By Sentiment",
                         value = FALSE)
    ), # closes column 3
    column(3,
           p(em("Choose Number Of Words")),
           sliderInput(inputId = "num_sent", 
                       label = "Top N Words",
                       value = 1, 
                       min = 0,
                       max = 20)
           
    ) # closes column 4
    
  ), # closes fluidRow
  
  hr(), # horizontal rule
  
  # -------------------------------------------------------
  # Tabset Panel of outputs
  # Feel free to customize the following output elements
  # -------------------------------------------------------
  tabsetPanel(type = "tabs",
              # Panel for Analysis 1
              tabPanel("Word Trend Analysis",
                       h3("Character Frequency"),
                       plotOutput("plot1"),
                       hr(),
                       dataTableOutput('table1')),
              # Panel for Analysis 2
              tabPanel("Sentiment Analysis", 
                       h3("Word Sentiment"),
                       plotOutput("plot2"),
                       hr(),
                       dataTableOutput('table2'))
  ) # closes tabsetPanel
  
) # closes ui



# ===============================================
# Define server logic
# ===============================================
server <- function(input, output) {
  
  # ------------------------------------------------------------
  # Reactive objects
  # ------------------------------------------------------------
  dat_freq <- reactive({
    
    season = as.numeric(input$season)
    episode = as.numeric(input$episode)
    
    got_dat = dat |>
      filter(Season == paste("Season", input$season),
             Episode == paste("Episode", input$episode))

    tokens = got_dat |>
      unnest_tokens(output = word, input = Sentence) |>
      anti_join(stop_words, by = "word")
    
    got_words = tokens |>
      count(Name, sort = TRUE, name = "count") %>% 
      head(10)
    
    got_words
  })
  
  dat_sentiments = reactive({
    got_dat = dat |>
      filter(Season == paste("Season", input$season),
             Episode == paste("Episode", input$episode))
    
    tokens = got_dat |>
      unnest_tokens(output = word, input = Sentence) |>
      anti_join(stop_words, by = "word")
    
    got_sentiments = tokens |>
      inner_join(sentiments, by = "word") |>
      group_by(sentiment) |>
      count(word, sort = TRUE, name = "count")
    
    got_sentiments
  })
  
  
  # ===============================================
  # Outputs for the first TAB
  # ===============================================
  
  # code for plot1
  output$plot1 <- renderPlot({
    ggplot(data = dat_freq(), aes(count, reorder(Name, count), fill = Name)) +
      geom_col() +
      labs(title = paste0("Appearance of Character in Season ", input$season, " Episode ",input$episode),
           x = "Frequence", 
           y = "Characters") +
      theme_bw()
  })
  
  # code for table1
  output$table1 <- renderDataTable({
    dat_freq()
  })
  
  
  # ===============================================
  # Outputs for the second TAB
  # ===============================================
  
  # code for plot2
  output$plot2 <- renderPlot({
   
    
    if(input$sent_facet){
      dat_sentiments() |>
        slice_head(n = input$num_sent) |>
        ggplot() +
        geom_col(aes(x = reorder(word, count), y = count, fill = sentiment)) +
        coord_flip() +
        facet_wrap(~sentiment) +
        theme_bw() +
        labs(title = paste0(input$num_sent, " Most Common Words With An Associated Sentiment in Season ", input$season , " Episode ", input$episode, " (Faceted By Sentiment)"),
             x = "Word",
             y = "Frequence")
    }else {
      dat_sentiments() |>
        slice_head(n = input$num_sent) |>
        ggplot() +
        geom_col(aes(x = reorder(word, count), y = count, fill = sentiment)) +
        coord_flip() +
        theme_bw() +
        labs(title = paste0(input$num_sent, " Most Common Words With An Associated Sentiment in Season ", input$season , " Episode ", input$episode),
             x = "Word",
             y = "Frequence")
    }
  })
  
  # code for table2
  output$table2 <- renderDataTable({
    dat_sentiments()
  })
  

}


# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

