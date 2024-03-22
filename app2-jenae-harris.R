# Title: Retirement Contribution Calculator
# Description: Calculates and visualizes retirement fund
# Author: Jenae Harris
# Date: 11/15/23


# =======================================================
# Packages (you can use other packages if you want)
# =======================================================
library(shiny)
library(tidyverse) # for data manipulation and graphics
library(plotly)    # for web-interactive graphics
library(DT)        # to work with HTML table widgets


# ======================================================
# Auxiliary objects/functions 
# (that don't depend on input widgets)
# ======================================================
# You may want to add one or more auxiliary objects for your analysis
# (by "auxiliary" we mean anything that doesn't depend on input widgets)





# =======================================================
# Define UI for application
# =======================================================
ui <- fluidPage(
  
  # Application title
  titlePanel("Retirement Contribution Calculator"),
  
  # -------------------------------------------------------
  # Sidebar with input widgets 
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # inputs
      numericInput(inputId = "initial_salary",
                  label = "Initial Salary",
                  value = 80000,
                  min = 0,
                  step = 1000),
      numericInput(inputId = "growth_rate",
                   label = "Rate of Growth",
                   value = 0.02, 
                   min = 0,
                   max = 1,
                  step = 0.1),
      numericInput(inputId = "cont_percent",
                   label = "Contribution Percentage",
                   value = 0.15,
                   min = 0, 
                   max = 1,
                   step = 0.1),
      radioButtons(inputId = "num_periods",
                   label = "Number of Periods",
                   choices = c("Annually" = 1, 
                               "Semi-Annual" = 2, 
                               "Quarterly" = 4, 
                               "Bimonthly" = 6, 
                               "Monthly" = 12, 
                               "Weekly" = 52)),
      numericInput(inputId = "num_years",
                   label = "Years Invested",
                   value = 5,
                   min = 0,
                   step = 1),
      numericInput(inputId = "arr",
                   label = "Annual Rate of Return",
                   value = .08,
                   min = 0,
                   max = 1,
                   step = 0.1),
      numericInput(inputId = "target",
                   label = "Target Amount",
                   value = 3500,
                   min = 0,
                   step = 100),
      checkboxInput(inputId = "show_target",
                   label = "Show Target",
                   value = FALSE),
    ),  # closes sidebarPanel of inputs
    
    # -------------------------------------------------------
    # Main Panel with outputs: plots and table
    # -------------------------------------------------------
    mainPanel(
      h3("Balance Timeline"),
      plotlyOutput(outputId = "plot1"),
      hr(),
      h3("Composition of Retirement Balance"),
      plotlyOutput(outputId = "plot2"),
      hr(),
      h3("Retirement Contribution Over Time"),
      dataTableOutput(outputId = "table"),
    )
    
  ) # closes sidebarLayout
) # closes fluidPage (UI)


# ======================================================
# Define server logic
# ======================================================
server <- function(input, output) {
  
  # ------------------------------------------------------------
  # Reactive Balance table
  # (adapt code to get the appropriate Balance data table)
  # ------------------------------------------------------------
  tbl = reactive({
    
    initial_salary = as.numeric(input$initial_salary)
    growth_rate = as.numeric(input$growth_rate)
    cont_percent = as.numeric(input$cont_percent)
    num_periods = as.numeric(input$num_periods)
    arr = as.numeric(input$arr)
    target = as.numeric(input$target)
    show_target = as.numeric(input$show_target)
    n = as.numeric(input$num_years)
    
    t = 0
    bal = 0
    annual_salary = rep(0,n)
    annual_balance = c()
    annual_contri = c()
    
    for(year in 1:n) {
      annual_salary[year] = initial_salary * (1 + growth_rate)^(year - 1)
      per_contri = annual_salary[year] * (cont_percent)
      annual_contri[year] = per_contri

      for(period in 1:num_periods) {
        bal = bal * (1 + (arr / num_periods)) + (per_contri / num_periods)
      }
      annual_balance = c(annual_balance, bal)
      
    }
    
    # data.frame(num_periods)
    
    own = cumsum(annual_contri)
    growth = annual_balance - own
    own_pct = (own/annual_balance) * 100
    growth_pct = (growth / annual_balance) * 100
    hit_target = annual_balance >= target

    balance_tbl = data.frame(year = 1:n,
                             salary = annual_salary,
                             contribution = annual_contri,
                             balance = annual_balance,
                             own,
                             growth,
                             own_pct,
                             growth_pct,
                             hit_target
                             )
    
   
  })
  
  
  # ------------------------------------------------------------
  # Plot of balance timeline
  # (adapt code to make a timeline according to your analysis)
  # ------------------------------------------------------------
  output$plot1 <- renderPlotly({
    # the following code is for demo purposes only; adapt it!!!
   p = ggplot(data = tbl(), aes(x = year, y = balance)) +
      geom_line() + geom_area(fill = "blue", alpha = 0.3) + 
     geom_point(data = tbl(), aes(x = year, y = balance)) + 
     labs(title = "Balance Timeline", x = "Year", y = "Balance") +
     theme_bw()
                                                                      
   if(input$show_target) {
     p = p + geom_hline(yintercept = input$target) + annotate("text", x = 1.5, y = input$target, label = paste0("target ", input$t))
       
   }
   p
  })

  
  # ------------------------------------------------------------
  # Plot of balance decomposition
  # (adapt code to make a graphic according to your analysis)
  # ------------------------------------------------------------
  output$plot2 <- renderPlotly({
    tbl_long = tbl() |>
      pivot_longer(cols = own:growth, names_to = "type") |>
      ggplot(aes(x = year, y = value, fill = type)) + geom_col() +
      labs(title = "Composition of Retirement Balance", x = "Year", y = "Value") +
      theme_bw()
  })
  
  # ------------------------------------------------------------
  # Table with Retirement Balance data
  # (adapt code to display appropriate table)
  # ------------------------------------------------------------
  output$table <- renderDataTable({
    tbl() |>
      datatable() |>
      formatRound(columns = 2:8, digits = 2)
  })
 
} # closes server


# Run the application 
shinyApp(ui = ui, server = server)
