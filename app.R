library(shiny)
library(tidyverse)
library(lubridate)

load("data/wind_generation_PL2016.RData")

ui <- fluidPage(
   
   # Application title
   titlePanel("Wind energy generation in Poland 2016"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         dateInput("sdate", "Start date:", value = "2016-01-01", format = "yyyy-mm-dd",
                   min = "2016-01-01", max = "2016-12-31"),
         dateInput("edate", "End date:", value = "2016-12-31", format = "yyyy-mm-dd",
                   min = "2016-01-01", max = "2016-12-31"),
         br(),h4("Source of data:"),
         p("Information, data, obtained from the site www.pse.pl, according to the status of the site on 18/06/2018, partially processed"),
         p("Informacje, dane, pozyskane ze strony www.pse.pl wg. stanu strony na dzień 18.06.2018, przetworzona w części."),
         a("www.pse.pl", href = "www.pse.pl"),
         br(),br(),p("All files are availbel on my Github"),
         a("www.github.com/prcwiek/wind_generation_poland", href = "www.github.com/prcwiek/wind_generation_poland")
         
      ),

      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("genPlot"),
         h4("Summary of wind energy generation (MWh) in the selected period"),
         verbatimTextOutput("summary"),
         br(),h4("Total generation in the selected period"),
         verbatimTextOutput("total_generation"),
         br(),h4("Monthly values for 2016"),
         plotOutput("barPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  df <- reactive({
    req(input$sdate, input$edate)
    edate <- make_datetime(year = year(input$edate),
                           month = month(input$edate),
                           day = day(input$edate),
                           hour = 23, min = 00, tz ="UTC")
    dg16 %>% 
      select(date, generation.MWh) %>% 
      filter(date >= input$sdate & date <= edate) %>% 
      group_by(date) %>% 
      summarise(generation.MWh = sum(generation.MWh))
  })
  
   output$genPlot <- renderPlot({
     ggplot(data = df(),aes(x = date, y = generation.MWh, color = generation.MWh)) +
       xlab("Time") +
       ylab("Wind energy generation (MWh)") +
       geom_line() +
       theme_minimal() +
       theme(legend.position = "none")
   })
   
   output$summary <- renderPrint({
     summary(df()$generation.MWh)
   })
   
   output$total_generation <- renderPrint({
     req(input$sdate, input$edate)
     edate <- make_datetime(year = year(input$edate),
                            month = month(input$edate),
                            day = day(input$edate),
                            hour = 23, min = 00, tz ="UTC")
     df_tg <- dg16 %>%
       select(date, generation.MWh) %>%
       filter(date >= input$sdate & date <= edate)
     cat("  ")
     cat(format(sum(df_tg$generation.MWh), big.mark = ","))
     cat(" MWh")
   })
   
   output$barPlot <- renderPlot({
     dfp <- dg16 %>% 
       select(month, generation.MWh) %>% 
       group_by(month) %>% 
       summarise(generation.MWh = sum(generation.MWh))
     
     dfp$month <- gl(12,1,12,
                    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
     
     ggplot(dfp) +
       xlab("Month of 2016") +
       ylab("Wind energy generation (MWh)") +
       geom_col(aes(x = month, y = generation.MWh, fill = generation.MWh)) +
       geom_text(aes(x = month, y = generation.MWh, 
                     label = format(round(generation.MWh, 0), big.mark = ",")),
                 size = 4, vjust = -0.5) +
       theme_minimal() +
       theme(legend.position = "none") 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

