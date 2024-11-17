library(bslib)
library(lubridate)
library(scales)
library(shiny)
library(shinyjs)
library(tidyverse)

# Version with data from the year 2016
load("data/wind_generation_PL2016.RData")

cards <- list(
  card(
    full_screen = TRUE,
    card_header <- "Wind generation (MWh) in selected period",
    plotOutput("genPlot")
  ),
  card(
    full_screen = TRUE,
    #height = "200px",
    card_header("Monthly values for 2016"),
    plotOutput("barPlot", height = "80%")
  )
)

ui <- page_sidebar(

  shinyjs::useShinyjs(),
  
  theme = bs_theme(
    version = 5,
    bootswatch = "cosmo",
    base_font = font_google("Inter"),
    navbar_bg = bs_get_variables(bs_theme(bootswatch = "cosmo"), "primary") 
  ),
  
  title = "Wind energy generation in Poland 2016", 
  
  sidebar = sidebar(
    title = "Data range",
    dateInput("sdate", "Start date:", value = "2016-01-01", format = "yyyy-mm-dd",
              min = "2016-01-01", max = "2016-12-31"),
    dateInput("edate", "End date:", value = "2016-12-31", format = "yyyy-mm-dd",
    min = "2016-01-01", max = "2016-12-31"),
    br(),h5("Source of data:"),
    p("Information, data, obtained from the site www.pse.pl, according to the status of the site on 18/06/2018, partially processed"),
    p("Informacje, dane, pozyskane ze strony www.pse.pl wg. stanu strony na dzień 18.06.2018, przetworzona w części."),
    a("www.pse.pl", href = "www.pse.pl"),
   ),
  
  layout_columns(
    value_box(
      title = "Total generation in the selected period",
      value = textOutput("generation_sum"),
      showcase = bsicons::bs_icon("graph-up"),
      fill = TRUE,
      theme = "primary"
    ),
    value_box(
      title = textOutput("generation_min_date"),
      value = textOutput("generation_min"),
      showcase = bsicons::bs_icon("graph-down-arrow"),
      fill = TRUE,
      theme = "danger"
    ),
    value_box(
      title = textOutput("generation_max_date"),
      value = textOutput("generation_max"),
      showcase = bsicons::bs_icon("graph-up-arrow"),
      fill = TRUE,
      theme = "success"
    )
  ),
  
  layout_columns(
    col_widths = c(12, 12),
    row_heights = c(1,1),
    cards[[1]],
    cards[[2]]
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  df <- reactive({
    req(input$sdate, input$edate)
    edate <- make_datetime(year = year(input$edate),
                           month = month(input$edate),
                           day = day(input$edate),
                           hour = 23, min = 00, tz ="UTC")
    dg16 |> 
      select(date, month, generation.MWh) |> 
      filter(date >= input$sdate & date <= input$edate) |> 
      group_by(date) |> 
      summarise(generation.MWh = sum(generation.MWh))
    
  })
  
  output$genPlot <- renderPlot({
    ggplot(data = df(), aes(x = date, y = generation.MWh, color = generation.MWh)) +
      geom_line() +
      scale_y_continuous(
        breaks = seq(0, 7000, 1000),
        labels = label_number(big.mark = ","),
        limits = c(0, 7000)
      ) + 
      labs(
        x = ("Time"),
        y = ("Wind energy generation (MWh)"),
        position = "left",
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })

  output$barPlot <- renderPlot({
    dfp <- dg16 |>
      mutate(months_labels = as.character(month)) |> 
      mutate(months_labels = factor(months_labels, 
                                    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) |> 
      filter(date >= input$sdate & date <= input$edate) |> 
      select(month, months_labels, generation.MWh) |>
      group_by(months_labels) |>
      summarise(generation.MWh = sum(generation.MWh)) 

    
    ggplot(dfp) +
      geom_col(aes(x = months_labels, y = generation.MWh, fill = generation.MWh)) +
      geom_text(aes(x = months_labels, y = generation.MWh, 
                    label = format(round(generation.MWh, 0), big.mark = ",")),
                size = 4, vjust = -0.5) +
      scale_y_continuous(
        breaks = c(0, 500000, 1000000, 1500000, 2000000),
        labels = label_number(big.mark = ","),
        position = "left",
        limits = c(0, 1600000)) +
      labs(
        x = "Month of 2016",
        y = "Wind energy generation (MWh)"
      ) +
      theme_minimal() +
      theme(legend.position = "none") 

      
  })
  

  
  output$generation_sum <- renderText({
    paste0(
      format(round(sum(df()$generation.MWh), 0), big.mark = ","),
      " MWh"
    )
  })
  
  output$generation_min <- renderText({
    paste0(
      format(round(min(df()$generation.MWh), 0), big.mark = ","),
      " MWh"
    )
  })

  output$generation_min_date <- renderText({
    paste0(
      "Lowest generation on ",
      as.Date(df()$date[which.min(df()$generation.MWh)])
    )
  })

  output$generation_max <- renderText({
    paste0(
      format(round(max(df()$generation.MWh), 0), big.mark = ","),
      " MWh"
    )
  })
  output$generation_max_date <- renderText({
    paste0(
      "Highest generation on ",
      as.Date(df()$date[which.min(df()$generation.MWh)])
    )
  })
  
  # End application when a window or a tab is closed
  session$onSessionEnded(stopApp)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

