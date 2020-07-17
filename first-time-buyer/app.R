library(shiny)
library(shinyMobile)



source("R/helper-funs.R")
cur_mort_rates <- current_mortgage_rates()

home_prices <- tq_get("CSUSHPISA",
  get = "economic.data",
  from = as_date("1800-01-01")) %>%
  mutate(monthly_growth_rate = price / lag(price) - 1) %>%
  na.omit()



min_date <- min(home_prices$date, na.rm = T)
max_date <- max(home_prices$date, na.rm = T)


shinyApp(
  ui = f7Page(
    # useShinydashboard(),
    # title = "My app",
    # dark_mode = FALSE,
    # init = f7Init(skin = "ios", theme = "light"),
    f7SingleLayout(
      panels = tagList(
        f7Panel(
          title = "Mortgage Assumptions",
          side = "left",
          theme = "light",

          # inputs ------------------------------------------------------------------

          f7Radio(
            inputId = "term",
            label = "Mortgage Term",
            choices = c("30 year fixed", "15 year fixed"),
            selected = "30 year fixed"
          ),
          f7Text(
            inputId = "principal", label = "Loan Amount", value = scales::dollar(500000),
            placeholder = "$500,000"
          ),
          sliderInput("home_appreciation",
            "Annual Home Appreciation (avg from period)",
            min = min_date,
            max = max_date,
            value = c(min_date, max_date),
            timeFormat = "%b %Y", width = "80%"
          ),
          f7Radio(
            inputId = "plot_mode",
            label = "Plot Type",
            choices = c("relative","overlay"),
            selected = "relative"
          )
        )
      ),
      navbar = f7Navbar(
        title = "Mortgage Analytics",
        hairline = TRUE,
        shadow = TRUE,
        left_panel = TRUE
      ),
      toolbar = NULL,
      # main content
      f7Shadow(
        intensity = 16,
        hover = TRUE,
        f7Card(
          title = "Card header",
          plotlyOutput("pay_plot")
        )
      )
    )
  ),
  server = function(input, output) {

    amort_data <- reactive({
      loan_amount <- str_extract_all(input$principal, "\\d") %>%
        unlist() %>%
        str_flatten() %>%
        as.double()
      
      term <- switch(input$term,
                     "30 year fixed" = "thirty_year",
                         "15 year fixed" = "fifteen_year")

      int_rate <- cur_mort_rates %>%
        filter(mortgage == term) %>%
        pull(int_rate)

      duration <- switch(input$term,
        "30 year fixed" = 30,
        "15 year fixed" = 15
      )

      mnth_growth <- home_prices %>%
        filter(
          date >= input$home_appreciation[1],
          date <= input$home_appreciation[2]
        ) %>%
        summarize(monthly_growth_rate = mean(monthly_growth_rate, na.rm = T)) %>%
        pull(monthly_growth_rate)

      amort(
        principal = loan_amount, interest = int_rate, duration = duration,
        home_appreciation = mnth_growth
      ) %>% 
        print()
    })


    output$pay_plot <- renderPlotly({
      

      amort <- amort_data()

      duration <- switch(input$term,
        thirty_year = 30,
        fifteen_year = 15
      )
      
      duration <- 30

      plot <- amort$data %>%
        print() %>% 
        rowid_to_column() %>%
        left_join(
          tibble(date = seq.Date(Sys.Date(), Sys.Date() + years(duration), by = "months")) %>%
            rowid_to_column()
        ) %>%
        print() %>%
        mutate(profit = home_value - total_payed) %>%
        plot_ly(
          x = ~date,
          y = ~towards_principal,
          type = "bar",
          name = "Payed Towards Principal",
          alpha = 0.6
        ) %>%
        add_trace(
          y = ~interest,
          name = "Interest Payment"
        ) %>%
        add_trace(
          y = ~profit,
          name = "Net Profit"
        ) %>%
        add_lines(
          y = ~home_value,
          name = "Estimated Home Value",
          color = I("black"),
          line = list(
            dash = "dash"  
          )
        ) %>% 
        layout(
          barmode = input$plot_mode,
          bargap = 0,
          xaxis = list(title = ""),
          yaxis = list(title = "",side = "right",tickformat = "$.2s"),
          legend = list(orientation = "h")
        )
    })
  }
)
