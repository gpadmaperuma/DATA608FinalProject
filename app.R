library(dplyr); library(plotly); library(shiny)

df<- read.csv("https://raw.githubusercontent.com/ChristinaValore/CUNY_DATA_608/master/Final%20Project/MTA_CLEANED.csv", header=TRUE, check.names = FALSE)

# Define UI ----
ui <- fluidPage(
    titlePanel("Subway on-time performance: 2017"),
    
    headerPanel(""),
    
    sidebarLayout(
        sidebarPanel(
            selectInput('nme', 'Subway line', unique(df$INDICATOR_NAME), selected='two'),
            actionButton('action', label = "Sort"),
            
            hr(),
            fluidRow(column(2, verbatimTextOutput("value")))),
        
        mainPanel(
            plotlyOutput('plot1')
        )
        
    )
)

# Define server logic ----
server <- function(input, output) {
    
    selected <- reactive({
        sub_df <- df %>% 
            filter(PERIOD_YEAR == 2017, INDICATOR_NAME == input$nme) %>% 
            mutate(PERIOD_MONTH = factor(PERIOD_MONTH, levels = unique(PERIOD_MONTH)[order(DAYS_ON_TIME, decreasing = ifelse((input$action %% 2) == 0,TRUE,FALSE))]))
        
    })
    
    output$plot1 <- renderPlotly({
        
        sub_df <- df %>% 
            filter(PERIOD_YEAR == 2017, INDICATOR_NAME == input$nme) %>% 
            mutate(PERIOD_MONTH = factor(PERIOD_MONTH, levels = unique(PERIOD_MONTH)[order(DAYS_ON_TIME, decreasing = ifelse((input$action %% 2) == 0,TRUE,FALSE))]))
        
        plot_ly(selected(), x = ~PERIOD_MONTH, y = ~DAYS_ON_TIME, color = ~PERIOD_MONTH, type='bar',
                mode = 'lines')
    })
    
}

# Run the app ----
shinyApp(ui = ui, server = server)