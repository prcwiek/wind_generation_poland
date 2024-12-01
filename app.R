library(bslib)
library(lubridate)
library(scales)
library(shiny)
library(shinyjs)
library(tidyverse)

shinyjs::useShinyjs()

today_date <- Sys.Date()

# Load file with data from the year 2016
load("data/wind_generation_PL2016.RData")

# Define cards --------------------------------------------------------------------------------
cards <- list(
  card(
    full_screen = TRUE,
    card_header <- "Wind generation (MWh) in selected period",
    plotOutput("genPlot2016")
  ),
  card(
    full_screen = TRUE,
    card_header("Monthly values for 2016"),
    plotOutput("barPlot", height = "80%")
  )
)

cards_daily <- list(
  card(
    full_screen = TRUE,
    card_header <- "Daily wind generation (MW)",
    plotOutput("genPlot_daily")
  )
)


# Define sidebar layout for 2016 data ---------------------------------------------------------

sidebar_gn2016 <- layout_sidebar(
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
      title = "Total generation",
      value = textOutput("generation_sum_2016"),
      showcase = bsicons::bs_icon("graph-up"),
      fill = TRUE,
      theme = "primary"
    ),
    value_box(
      title = textOutput("generation_min_date_2016"),
      value = textOutput("generation_min_2016"),
      showcase = bsicons::bs_icon("graph-down-arrow"),
      fill = TRUE,
      theme = "danger"
    ),
    value_box(
      title = textOutput("generation_max_date_2016"),
      value = textOutput("generation_max_2016"),
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

# Define sidebar layout for daily data --------------------------------------------------------

daily_generation <- layout_sidebar(
  sidebar = sidebar(
    title = "Select Date",
    dateInput("sdate_daily", label = NULL, value = today_date, format = "yyyy-mm-dd",
              min = "2024-06-14", max = today_date),
    hr(),
    p("PSE API allows to download data starting from 2024-06-14"),
    hr(),
    br(),h5("Source of data:"),
    p("Information, data, obtained from the site www.pse.pl, according to the status of the site on 18/06/2018, partially processed"),
    p("Informacje, dane, pozyskane ze strony www.pse.pl wg. stanu strony na dzień 18.06.2018, przetworzona w części."),
    a("www.pse.pl", href = "www.pse.pl"),
  ),
  
  # wind
  layout_columns(
    value_box(
      title = "Total daily wind production",
      value = textOutput("generation_sum"),
      showcase = bsicons::bs_icon("graph-up"),
      fill = TRUE,
      theme = "primary"
    ),
    value_box(
      title = textOutput("generation_min_hour"),
      value = textOutput("generation_min"),
      showcase = bsicons::bs_icon("graph-down-arrow"),
      fill = TRUE,
      theme = "danger"
    ),
    value_box(
      title = textOutput("generation_max_hour"),
      value = textOutput("generation_max"),
      showcase = bsicons::bs_icon("graph-up-arrow"),
      fill = TRUE,
      theme = "success"
    )
  ),
  
  # solar
  layout_columns(
    value_box(
      title = "Total daily solar production",
      value = textOutput("generation_sum_solar"),
      showcase = bsicons::bs_icon("graph-up"),
      fill = TRUE,
      theme = "bg-orange",
      class = "text-light"
    ),
    value_box(
      title = textOutput("generation_min_hour_solar"),
      value = textOutput("generation_min_solar"),
      showcase = bsicons::bs_icon("graph-down-arrow"),
      fill = TRUE,
      theme = "danger"
    ),
    value_box(
      title = textOutput("generation_max_hour_solar"),
      value = textOutput("generation_max_solar"),
      showcase = bsicons::bs_icon("graph-up-arrow"),
      fill = TRUE,
      theme = "success"
    )
  ),
  
  layout_columns(
    col_widths = c(12),
    row_heights = c(1,1),
    cards_daily[[1]]
  )
)

ui <- page_navbar(

  
  theme = bs_theme(
    version = 5,
    bootswatch = "cosmo",
    base_font = font_google("Inter"),
    navbar_bg = bs_get_variables(bs_theme(bootswatch = "cosmo"), "primary") 
  ),
  
  title = "Wind energy generation in Poland",
  fillable = "Daily generation",
  
  nav_panel("Daily generation", daily_generation),
  nav_panel("Generartion 2016", sidebar_gn2016)
  
)

server <- function(input, output, session) {
  
# Generation 2016 - server --------------------------------------------------------------------

  df2016 <- reactive({
    req(input$sdate, input$edate)

    dg16 |> 
      select(date, month, generation.MWh) |> 
      filter(date >= input$sdate & date <= input$edate) |> 
      group_by(date) |> 
      summarise(generation.MWh = sum(generation.MWh))
    
  })
  
  output$genPlot2016 <- renderPlot({
    ggplot(data = df2016(), aes(x = date, y = generation.MWh, color = generation.MWh)) +
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
    req(input$sdate, input$edate)
    
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
  
  output$generation_sum_2016 <- renderText({
    paste0(
      format(round(sum(df2016()$generation.MWh), 0), big.mark = ","),
      " MWh"
    )
  })
  
  output$generation_min_2016 <- renderText({
    paste0(
      format(round(min(df2016()$generation.MWh), 0), big.mark = ","),
      " MWh"
    )
  })

  output$generation_min_date_2016 <- renderText({
    paste0(
      "Lowest generation on ",
      as.Date(df2016()$date[which.min(df2016()$generation.MWh)])
    )
  })

  output$generation_max_2016 <- renderText({
    paste0(
      format(round(max(df2016()$generation.MWh), 0), big.mark = ","),
      " MWh"
    )
  })
  output$generation_max_date_2016 <- renderText({
    paste0(
      "Highest generation on ",
      as.Date(df2016()$date[which.max(df2016()$generation.MWh)])
    )
  })
  
  

# Daily generation - server -------------------------------------------------------------------
  df <- reactive({
    req(input$sdate_daily)

    ## Request to PSE API
    # https://api.raporty.pse.pl/
    # Raporty dobowe z funkcjonowania KSE - Wielkości podstawowe
    # {his-wlk-cal}
    
    link_pse_api <- paste0("https://api.raporty.pse.pl/api/his-wlk-cal?$filter=doba%20eq%20'",
                           input$sdate_daily,
                           "'")
    
    req_pse_api <- httr2::request(link_pse_api)
    resp_pse_api <- httr2::req_perform(req_pse_api)
    resp_body_pse_api <- httr2::resp_body_json(resp_pse_api)
    
    ddaily <- bind_rows(resp_body_pse_api$value)
    
    ddaily$udtczas <- as.POSIXct(ddaily$udtczas)
    
    ddaily |> 
      select(udtczas, udtczas_oreb, wi, pv) |>
      arrange(udtczas) |> 
      mutate(wi_mwh = wi * 0.25) |> 
      mutate(pv_mwh = pv * 0.25)
    
    
  })
  
  output$genPlot_daily <- renderPlot({
    df() |> 
      pivot_longer(cols = c("wi", "pv"),
                   names_to = "source",
                   values_to = "wi_pv_generation") |> 
      ggplot( aes(x = udtczas, y = wi_pv_generation, color = source)) +
      geom_line() +
      scale_y_continuous(
        breaks = seq(0, 15000, 1000),
        labels = label_number(big.mark = ","),
        limits = c(0, 15000)
      ) +
      labs(
        x = ("Time"),
        y = ("Wind and solar energy generation (MW)"),
        position = "left",
      ) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text=element_text(size=12),
            axis.title=element_text(size=14)) +
      scale_color_manual(values = c("orange", "blue"))
    
  })
  
  output$generation_sum <- renderText({
    paste0(
      format(round(sum(df()$wi_mwh, na.rm = TRUE), 0), big.mark = ","),
      " MWh"
    )
  })
  
  output$generation_min <- renderText({
    paste0(
      format(round(min(df()$wi, na.rm = TRUE), 0), big.mark = ","),
      " MW"
    )
  })
  
  output$generation_min_hour <- renderText({
    paste0(
      "Lowest wind generation at ",
      format(df()$udtczas[which.min(df()$wi)], format = "%H:%M")
    )
  })
  
  output$generation_max <- renderText({
    paste0(
      format(round(max(df()$wi, na.rm = TRUE), 0), big.mark = ","),
      " MW"
    )
  })
  output$generation_max_hour <- renderText({
    paste0(
      "Highest wind generation at ",
      format(df()$udtczas[which.max(df()$wi)], format = "%H:%M")
    )
  })
  
  
  ## solar generation
  output$generation_sum_solar <- renderText({
    paste0(
      format(round(sum(df()$pv_mwh, na.rm = TRUE), 0), big.mark = ","),
      " MWh"
    )
  })
  
  output$generation_min_solar <- renderText({
    paste0(
      format(round(min(df()$pv, na.rm = TRUE), 0), big.mark = ","),
      " MW"
    )
  })
  
  output$generation_min_hour_solar <- renderText({
    paste0(
      "Lowest generation at ",
      format(df()$udtczas[which.min(df()$pv)], format = "%H:%M")
    )
  })
  
  output$generation_max_solar <- renderText({
    paste0(
      format(round(max(df()$pv, na.rm = TRUE), 0), big.mark = ","),
      " MW"
    )
  })
  output$generation_max_hour_solar <- renderText({
    paste0(
      "Highest generation at ",
      format(df()$udtczas[which.max(df()$pv)], format = "%H:%M")
    )
  })
  
  # End application when a window or a tab is closed
  session$onSessionEnded(stopApp)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

