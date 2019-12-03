library(shinydashboard)
library(shiny)
library(tidyquant)
library(tidyverse)
library(highcharter)
library(shinycssloaders)

options(shiny.autoload = T)


# header ------------------------------------------------------------------
header <- dashboardHeader(title = shiny::tags$a(shiny::tags$img(
  src = "logo.svg",
  width = "200",
  height = "31"
)))


# sidebar -----------------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarSearchForm(
    textId = "ticker",
    buttonId = "searchButton",
    label = "Ticker..."
  ),
  sidebarMenu(
    id = "sidebarmenu",


    # menu items --------------------------------------------------------------
              # Technical Indicators--------------------
              menuItem("Technical Indicators",
                       tabName = 'tech_chart',
                       icon = icon('random'),
                       dateRangeInput("tech_date_range",
                                      "Date range:",
                                      start  = Sys.Date() %m-% years(1),
                                      end    = Sys.Date(),
                                      min    = "2000-01-01",
                                      max    = Sys.Date()),
                       menuSubItem("RSI","tab_ind_rsi"),
                       menuSubItem("MACD","tab_ind_macd"),
                       menuSubItem("Bollinger Bands","tab_ind_bbands"),
                       menuSubItem("Stochastic Oscillator","tab_ind_stoch"),
                       menuSubItem("Commondity Channel Index","tab_ind_cci"),
                       menuSubItem("AROON","tab_ind_aroon"),
                       menuSubItem("On Balance Volume","tab_ind_obv"),
                       menuSubItem("ADX","tab_ind_adx"),
                       menuSubItem("Simple MA","tab_ind_sma"),
                       menuSubItem("Exponential MA", "tab_ind_ema"),
                       conditionalPanel("input.sidebarmenu === 'tab_ind_rsi'",
                                        numericInput("rsi_n","Periods:",value = 14)),
                       conditionalPanel("input.sidebarmenu === 'tab_ind_macd'",
                                        numericInput("macd_nfast","Fast:",value = 12),
                                        numericInput("macd_nslow","Slow:",value = 26),
                                        numericInput("macd_nsig","Signal:",value = 9)),

                       conditionalPanel("input.sidebarmenu === 'tab_ind_bbands'",
                                        numericInput("bbands_n","Periods:",value = 20),
                                        numericInput("bbands_sd", "Std Dev:", value = 2)),
                       conditionalPanel("input.sidebarmenu === 'tab_ind_stoch'",
                                        numericInput("stoch_fastk","Fast K:",value = 14),
                                        numericInput("stoch_fastd", "Fast D",value = 3),
                                        numericInput("stoch_slowd","Slow D",value = 3)),
                       conditionalPanel("input.sidebarmenu === 'tab_ind_cci'",
                                        numericInput("cci_n","Periods:",value = 20)),
                       conditionalPanel("input.sidebarmenu === 'tab_ind_aroon'",
                                        numericInput("aroon_n","Periods:",value = 20)),
                       conditionalPanel("input.sidebarmenu === 'tab_ind_adx'",
                                        numericInput("adx_n","Periods:",value = 14)),
                       conditionalPanel("input.sidebarmenu === 'tab_ind_sma'",
                                        numericInput("sma_one","Periods:",value = 50),
                                        numericInput("sma_two","Periods:",value = 200)),
                       conditionalPanel("input.sidebarmenu === 'tab_ind_ema'",
                                        numericInput("ema_one","Periods:",value = 20),
                                        numericInput("ema_two","Periods:",value = 50))

              )
  ) # end sidebar menu
) # end dashboard sidebar


# body --------------------------------------------------------------------
body <- dashboardBody(
  shiny::tags$script(HTML("$('body').addClass('sidebar-mini');")),

  tabItems(
    # tab items ---------------------------------------------------------------
    tabItem(
      tabName = "econ",

      fluidRow()
    ),

    tabItem(
      tabName = "tab_ind_sma",
      fluidRow(column(12, withSpinner(highchartOutput("ind_sma", height = "850px"))))
    ),
    tabItem(
      tabName = "tab_ind_ema",
      fluidRow(column(12, withSpinner(highchartOutput("ind_ema", height = "850px"))))
    ),
    tabItem(
      tabName = "tab_ind_macd",
      fluidRow(column(12, withSpinner(highchartOutput("ind_macd", height = "850px"))))
    ),
    tabItem(
      tabName = "tab_ind_stoch",
      fluidRow(column(12, withSpinner(highchartOutput("ind_stoch", height = "850px"))))
    ),
    tabItem(
      tabName = "tab_ind_rsi",
      fluidRow(column(12, withSpinner(highchartOutput("ind_rsi", height = "850px"))))
    ),
    tabItem(
      tabName = "tab_ind_cci",
      fluidRow(column(12, withSpinner(highchartOutput("ind_cci", height = "850px"))))
    ),
    tabItem(
      tabName = "tab_ind_aroon",
      fluidRow(column(12, withSpinner(highchartOutput("ind_aroon", height = "850px"))))
    ),
    tabItem(
      tabName = "tab_ind_obv",
      fluidRow(column(12, withSpinner(highchartOutput("ind_obv", height = "850px"))))
    ),
    tabItem(
      tabName = "tab_ind_adx",
      fluidRow(column(12, withSpinner(highchartOutput("ind_adx", height = "850px"))))
    ),
    tabItem(
      tabName = "tab_ind_bbands",
      fluidRow(column(12, withSpinner(highchartOutput("ind_bbands", height = "850px"))))
    )
  ) # end tab items
) # end body



ui <- dashboardPage(title = "R Equity Research", header, sidebar, body, skin = "black")

server <- shinyServer(function(input, output) {

    # Technical Indicators------------------------------------------------
  tech_price <- reactive({
    req(input$ticker)
    req(input$tech_date_range)


    tq_get(input$ticker, from = input$tech_date_range[[1]] %m-% years(1),
           to = input$tech_date_range[[2]])

  })


  # SMA(x, n = 10, ...)
  output$ind_sma <- renderHighchart({

    req(input$sma_one)
    req(input$sma_two)

    ind_df <- tech_price() %>%
      tq_mutate(select = adjusted, mutate_fun = SMA, n = input$sma_one, col_rename = "sma_50") %>%
      tq_mutate(select = adjusted, mutate_fun = SMA, n = input$sma_two, col_rename = "sma_200") %>%
      filter(date >= input$tech_date_range[[1]])

    ind_xts <- xts(ind_df[-1],ind_df$date)
    sma_one_name <- paste0(input$sma_one," MA")
    sma_two_name <- paste0(input$sma_two," MA")

    highchart(type = "stock") %>%
      hc_yAxis_multiples(
        create_yaxis(2, height = c(2, 1), turnopposite = TRUE)
      ) %>%
      hc_add_series(ind_xts, yAxis = 0, name = input$ticker) %>%
      hc_add_series(ind_xts$sma_50, yAxis = 0, name = sma_one_name) %>%
      hc_add_series(ind_xts$sma_200,yAxis = 0, name = sma_two_name) %>%
      hc_add_series(ind_xts$volume, color = "gray", yAxis = 1, name = "Volume", type = "column") %>%
      hc_tooltip(valueDecimals = "2")
  })

  # EMA(x, n = 10, wilder = FALSE, ratio = NULL, ...)
  output$ind_ema <- renderHighchart({
    req(input$ema_one)
    req(input$ema_two)
    ind_df <- tech_price() %>%
      tq_mutate(select = adjusted, mutate_fun = EMA, n = input$ema_one,col_rename = "ema_20") %>%
      tq_mutate(select = adjusted, mutate_fun = EMA, n = input$ema_two, col_rename = "ema_50") %>%
      filter(date >= input$tech_date_range[[1]])

    ind_xts <- xts(ind_df[-1],ind_df$date)
    ema_one_name <- paste0(input$ema_one," EMA")
    ema_two_name <- paste0(input$ema_two," EMA")


    highchart(type = "stock") %>%

      hc_yAxis_multiples(
        create_yaxis(2, height = c(2, 1), turnopposite = TRUE)
      ) %>%

      hc_add_series(ind_xts, yAxis = 0, name = input$ticker) %>%
      hc_add_series(ind_xts$ema_20, yAxis = 0, name = ema_one_name) %>%
      hc_add_series(ind_xts$ema_50,yAxis = 0, name = ema_two_name) %>%
      hc_add_series(ind_xts$volume, color = "gray", yAxis = 1, name = "Volume", type = "column") %>%
      hc_tooltip(valueDecimals = "2")

  })

  output$ind_macd <- renderHighchart({
    req(input$macd_nfast)
    req(input$macd_nslow)
    req(input$macd_nsig)
    # MACD(x, nFast = 12, nSlow = 26, nSig = 9, maType, percent = TRUE, ...)
    ind_df <- tech_price() %>%
      tq_mutate(select = adjusted, mutate_fun = MACD,
                nFast = input$macd_nfast,
                nSlow = input$macd_nslow,
                nSig = input$macd_nsig) %>%
      mutate(diff = macd - signal) %>%
      filter(date >= input$tech_date_range[[1]])

    ind_xts <- xts(ind_df[-1],ind_df$date)

    highchart(type = "stock") %>%

      hc_yAxis_multiples(
        create_yaxis(2, height = c(2, 2), turnopposite = TRUE)
      ) %>%

      hc_add_series(ind_xts, yAxis = 0, name = input$ticker) %>%
      hc_add_series(ind_xts$macd, color = "blue", yAxis = 1, name = "MACD") %>%
      hc_add_series(ind_xts$signal, color = "green", yAxis = 1, name = "Signal") %>%
      hc_add_series(ind_xts$diff, color = "gray", yAxis = 1, type = "column", name = "Difference") %>%
      hc_tooltip(valueDecimals = "2")

  })
  # stoch(HLC, nFastK = 14, nFastD = 3, nSlowD = 3, maType, bounded = TRUE, smooth = 1, ...)
  # aapl %>% tq_mutate(select = c(high,low,close), mutate_fun = stoch)

  output$ind_stoch <- renderHighchart({
    req(input$stoch_fastk)
    req(input$stoch_fastd)
    req(input$stoch_slowd)

    ind_df <- tech_price() %>%
      tq_mutate(select = c(high,low,close), mutate_fun = stoch,
                nFastK= input$stoch_fastk,
                nFastD = input$stoch_fastd,
                nSlowD = input$stoch_slowd) %>%
      mutate(stoch = stoch *100,
             lower_level = 20,
             upper_level = 80) %>%
      filter(date >= input$tech_date_range[[1]])

    ind_xts <- xts(ind_df[-1],ind_df$date)

    highchart(type = "stock") %>%

      hc_yAxis_multiples(
        create_yaxis(2, height = c(2, 1), turnopposite = TRUE)
      ) %>%

      hc_add_series(ind_xts, yAxis = 0, name = input$ticker) %>%
      hc_add_series(ind_xts$stoch, color = "blue", yAxis = 1, name = "Oscillator") %>%
      hc_add_series(ind_xts$lower_level, color = "green",yAxis = 1, tooltip = list(pointFormat = "{point.y}")) %>%
      hc_add_series(ind_xts$upper_level, color = "red", yAxis = 1, tooltip = list(pointFormat = "{point.y}"))%>%
      hc_tooltip(valueDecimals = "2")

  })


  # RSI(price, n=14, maType="WMA", wts=ttrc[,"Volume"])
  # aapl %>% tq_mutate(select = adjusted, mutate_fun = RSI)

  output$ind_rsi <- renderHighchart({
    req(input$rsi_n)

    ind_df <- tech_price() %>%
      tq_mutate(select = adjusted, mutate_fun = RSI,
                n = input$rsi_n) %>%
      mutate(lower_level = 30,
             upper_level = 70) %>%
      filter(date >= input$tech_date_range[[1]])

    ind_xts <- xts(ind_df[-1],ind_df$date)

    highchart(type = "stock") %>%

      hc_yAxis_multiples(
        create_yaxis(2, height = c(2, 1), turnopposite = TRUE)
      ) %>%

      hc_add_series(ind_xts, yAxis = 0, name = input$ticker) %>%
      hc_add_series(ind_xts$rsi, color = "blue", yAxis = 1, name = "Oscillator") %>%
      hc_add_series(ind_xts$lower_level, color = "green",yAxis = 1, tooltip = list(pointFormat = "{point.y}")) %>%
      hc_add_series(ind_xts$upper_level, color = "red", yAxis = 1, tooltip = list(pointFormat = "{point.y}"))%>%
      hc_tooltip(valueDecimals = "2")


  })

  # CCI(HLC, n = 20, maType, c = 0.015, ...)

  output$ind_cci <- renderHighchart({
    req(input$cci_n)

    ind_df <- tech_price() %>%
      tq_mutate(select = c(high,low,close), mutate_fun = CCI,
                n = input$cci_n) %>%
      mutate(lower_level = -100,
             upper_level = 100) %>%
      filter(date >= input$tech_date_range[[1]])

    ind_xts <- xts(ind_df[-1],ind_df$date)

    highchart(type = "stock") %>%

      hc_yAxis_multiples(
        create_yaxis(2, height = c(2, 1), turnopposite = TRUE)
      ) %>%

      hc_add_series(ind_xts, yAxis = 0, name = input$ticker) %>%
      hc_add_series(ind_xts$cci, color = "blue", yAxis = 1, name = "CCI") %>%
      hc_add_series(ind_xts$lower_level, color = "green",yAxis = 1, name =  "Oversold",
                    tooltip = list(pointFormat = "{point.y}")) %>%
      hc_add_series(ind_xts$upper_level, color = "red", yAxis = 1, name = "Overbought", tooltip = list(pointFormat = "{point.y}"))%>%
      hc_tooltip(valueDecimals = "2")

  })
  # aroon(HL, n = 20)
  # aapl %>% tq_mutate(select = c(high,low), mutate_fun = aroon)

  output$ind_aroon <- renderHighchart({
    req(input$aroon_n)

    ind_df <- tech_price() %>%
      tq_mutate(select = c(high,low), mutate_fun = aroon,
                n = input$aroon_n) %>%
      filter(date >= input$tech_date_range[[1]]) %>%
      mutate(lower_level = -100,
             upper_level = 100)

    ind_xts <- xts(ind_df[-1],ind_df$date)

    highchart(type = "stock") %>%

      hc_yAxis_multiples(
        create_yaxis(3, height = c(2,1,1), turnopposite = TRUE)
      ) %>%

      hc_add_series(ind_xts, yAxis = 0, name = input$ticker) %>%
      hc_add_series(ind_xts$aroonUp, color = "green", yAxis = 1, name = "Up") %>%
      hc_add_series(ind_xts$aroonDn, color = "red", yAxis = 1, name = "Down" ) %>%
      hc_add_series(ind_xts$oscillator, color = "blue", yAxis = 2, name = "Oscillator") %>%
      hc_add_series(ind_xts$lower_level, color = "black",yAxis = 2,
                    tooltip = list(pointFormat = "{point.y}")) %>%
      hc_add_series(ind_xts$upper_level, color = "black", yAxis = 2, tooltip = list(pointFormat = "{point.y}"))%>%
      hc_tooltip(valueDecimals = "2")

  })

  # OBV(price, volume)

  output$ind_obv <- renderHighchart({

    ind_df <- tech_price() %>%
      tq_mutate_xy(adjusted,volume, mutate_fun = OBV) %>%
      filter(date >= input$tech_date_range[[1]])

    ind_xts <- xts(ind_df[-1],ind_df$date)

    highchart(type = "stock") %>%

      hc_yAxis_multiples(
        create_yaxis(2, height = c(2,2), turnopposite = TRUE)
      ) %>%

      hc_add_series(ind_xts, yAxis = 0, name = input$ticker) %>%
      hc_add_series(ind_xts$obv, color = "blue", yAxis = 1, name = "On Balance Volume") %>%
      hc_tooltip(valueDecimals = "2")

  })
  # Need to Join Seperately
  # ADX(HLC, n = 14, maType, ...)

  output$ind_adx <- renderHighchart({
    req(input$adx_n)

    ind_df <- tech_price() %>% tq_transmute(select = c(high,low,close), mutate_fun = ADX, n = input$adx_n) %>%
      mutate(date = as.Date(date)) %>%
      left_join(tech_price(),by = "date") %>%
      select(date,open,high,low,close,volume,adjusted,everything()) %>%
      filter(date >= input$tech_date_range[[1]])

    ind_xts <- xts(ind_df[-1],ind_df$date)

    highchart(type = "stock") %>%

      hc_yAxis_multiples(
        create_yaxis(3, height = c(2,1,1), turnopposite = TRUE)
      ) %>%

      hc_add_series(ind_xts, yAxis = 0, name = input$ticker) %>%
      hc_add_series(ind_xts$DIp, color = "green", yAxis = 1, name = "Positive") %>%
      hc_add_series(ind_xts$DIn, color = "red", yAxis = 1, name = "Negative") %>%
      hc_add_series(ind_xts$ADX, color = "black", yAxis = 2, name = "ADX") %>%
      hc_add_series(ind_xts$DX, color = "blue", yAxis = 2, name = "DX") %>%
      hc_tooltip(valueDecimals = "2")

  })

  output$ind_bbands <- renderHighchart({
    req(input$bbands_n)
    req(input$bbands_sd)

    # BBands(HLC, n = 20, maType, sd = 2, ...)
    ind_df <- tech_price() %>% tq_transmute(select = adjusted,
                                            mutate_fun = BBands,
                                            n= input$bbands_n,
                                            sd= input$bbands_sd) %>%
      mutate(date = as.Date(date)) %>%
      left_join(tech_price(),by = "date") %>%
      select(date,open,high,low,close,volume,adjusted,everything()) %>%
      mutate(upper_level = 1,
             lower_level = 0) %>%
      filter(date >= input$tech_date_range[[1]])


    ind_xts <- xts(ind_df[-1],ind_df$date)

    highchart(type = "stock") %>%

      hc_yAxis_multiples(
        create_yaxis(2, height = c(2,2), turnopposite = TRUE)
      ) %>%

      hc_add_series(ind_xts, yAxis = 0, name = input$ticker) %>%
      hc_add_series(ind_xts$dn, color = "green", yAxis = 0, name = "Lower") %>%
      hc_add_series(ind_xts$up, color = "red", yAxis = 0, name = "Upper") %>%
      hc_add_series(ind_xts$mavg, color = "black", yAxis = 0, name = "Moving Average") %>%
      hc_add_series(ind_xts$pctB, color = "blue", yAxis = 1, name = "% B") %>%
      hc_add_series(ind_xts$upper_level, color = "black", yAxis = 1, tooltip = list(pointFormat = "{point.y}")) %>%
      hc_add_series(ind_xts$lower_level, color = "black", yAxis = 1, tooltip = list(pointFormat = "{point.y}")) %>%
      hc_tooltip(valueDecimals = "2")


  })

  
  
  
})




shinyApp(ui, server)
