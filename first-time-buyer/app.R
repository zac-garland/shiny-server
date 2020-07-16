# package load ------------------------------------------------------------
library(shiny)
library(bootstraplib)

# boot dash layout funs ---------------------------------------------------


boot_side_layout <- function(...) {
  div(class = "d-flex wrapper", ...)
}

boot_sidebar <- function(...) {
  div(
    class = "bg-light border-right sidebar-wrapper",
    div(class = "list-group list-group-flush", ...)
  )
}

boot_main <- function(...) {
  div(
    class = "page-content-wrapper",
    div(class = "container-fluid", ...)
  )
}





# css ---------------------------------------------------------------------

css_def <- "
body {
  overflow-x: hidden;
}

.container-fluid, .container-sm, .container-md, .container-lg, .container-xl {
    padding-left: 0px;
}

.sidebar-wrapper {
  min-height: 100vh;
  margin-left: -15rem;
  padding-left: 15px;
  padding-right: 15px;
  -webkit-transition: margin .25s ease-out;
  -moz-transition: margin .25s ease-out;
  -o-transition: margin .25s ease-out;
  transition: margin .25s ease-out;
}


.sidebar-wrapper .list-group {
  width: 15rem;
}

.page-content-wrapper {
  min-width: 100vw;
  padding: 20px;
}

.wrapper.toggled .sidebar-wrapper {
  margin-left: 0;
}

.sidebar-wrapper, .page-content-wrapper {
  padding-top: 20px;
}

.navbar{
  margin-bottom: 0px;
}

@media (max-width: 768px) {
  .sidebar-wrapper {
    padding-right: 0px;
    padding-left: 0px;

  }
}

@media (min-width: 768px) {
  .sidebar-wrapper {
    margin-left: 0;
  }

  .page-content-wrapper {
    min-width: 0;
    width: 100%;
  }

  .wrapper.toggled .sidebar-wrapper {
    margin-left: -15rem;
  }
}

"

extra_css <- "
.bg-light, .navbar.navbar-default {
    background-color: #000000 !important;
}

.sidebar-wrapper{
  background-color: #f8f9fa !important;
  color: #000000 !important;
  font-weight: bold;
}    

.navbar-light .navbar-brand, .navbar.navbar-default .navbar-brand {
    color: white;
}

.navbar ul li a{
  color: white !important;
}

.form-control {
    color: #ffffff;
    background-color: #1f1f1e;
}

.small-box .icon-large {
    color: white;
}


"

bs_theme_new(bootswatch = "lux")

# title -------------------------------------------------------------------
html_title <-
  '<span class="logo">
    <div style="display:inline-block;">
      <a href="https://www.google.com"><img src="https://jeroen.github.io/images/Rlogo.png" height="35"/></a>
      <b>my company name</b> a subtitle of application or dashboard
    </div>
  </span>'



# helper funs -------------------------------------------------------------

source("R/helper-funs.R")
cur_mort_rates <- current_mortgage_rates()

home_prices <- tq_get("CSUSHPISA",get = "economic.data",
                      from = as_date("1800-01-01")) %>% 
  mutate(monthly_growth_rate = price/lag(price)-1) %>% 
  na.omit()
  


# app ---------------------------------------------------------------------
ui <- tagList(
  tags$head(tags$style(HTML(css_def))),
  tags$head(tags$style(HTML(extra_css))),
  bootstrap(),
  navbarPage(
    collapsible = TRUE,
    title = HTML("Mortgage Analytics"),
    fluidPage(
      useShinydashboard(),
      boot_side_layout(
        boot_sidebar(
          selectInput(
            "term", "Mortgage Type",
            choices = c("30 year fixed" = "thirty_year", "15 year fixed" = "fifteen_year")
          ),
          textInput("principal", label = "Loan Amount", value = scales::dollar(500000)),
          sliderInput("home_appreciation", 
                         "Annual Home Appreciation (avg from period)", 
                         min = min(home_prices$date),
                         max = max(home_prices$date),
                      value = c(min(home_prices$date),max(home_prices$date)),
                      timeFormat="%b %Y",width = "80%"),
          selectInput(
            "plot_mode", "Plot Type",
            choices = c("overlay","relative")
          )
        ),
        boot_main(
          fluidRow(
            valueBox(htmlOutput("thirty_rate"), 
                   "Thirty Year Fixed Mortgage", 
                   icon = icon("line-chart"), 
                   color = "black"),
            valueBox(htmlOutput("fifteen_rate"), 
                     "Fifteen Year Fixed Mortgage", 
                     icon = icon("line-chart"), 
                     color = "black"),
            valueBox(htmlOutput("arm_rate"), 
                     "Adjustable Rate Mortgage", 
                     icon = icon("line-chart"), 
                     color = "black")
                   
                   ),
          fluidRow(
            valueBox(htmlOutput("total_interest"), 
                     "Total Interest Paid", 
                     icon = icon("line-chart"), 
                     color = "black"),
            valueBox(htmlOutput("total_paid"), 
                     "Fifteen Year Fixed Mortgage", 
                     icon = icon("line-chart"), 
                     color = "black"),
            valueBox(htmlOutput("pct_of_whole"), 
                     "Interest as % of Total", 
                     icon = icon("line-chart"), 
                     color = "black")
            
          ),
          plotlyOutput("pay_plot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$thirty_rate <- renderUI({
    thirty_year <- cur_mort_rates %>%
      filter(mortgage == "thirty_year") %>%
      pull(int_rate)
    
    tagList(h3(scales::percent(thirty_year/100,accuracy = 0.01),style="color:white;"))
    
    
  })
  
  output$fifteen_rate <- renderUI({
    rate <- cur_mort_rates %>%
      filter(mortgage == "fifteen_year") %>%
      pull(int_rate)
    
    tagList(h3(scales::percent(rate/100,accuracy = 0.01),style="color:white;"))
    
    
  })
  
  output$arm_rate <- renderUI({
    rate <- cur_mort_rates %>%
      filter(mortgage == "arm_loan") %>%
      pull(int_rate)
    
    tagList(h3(scales::percent(rate/100,accuracy = 0.01),style="color:white;"))
    
    
  })
  

  observe({
    if (!str_detect(input$principal, "\\$")) {
      updateTextInput(session, "principal", "Loan Amount",
        value = scales::dollar(as.numeric(input$principal))
      )
    }
  })

  amort_data <- reactive({
    loan_amount <- str_extract_all(input$principal, "\\d") %>%
      unlist() %>%
      str_flatten() %>%
      as.double()

    int_rate <- cur_mort_rates %>%
      filter(mortgage == input$term) %>%
      pull(int_rate)

    duration <- switch(input$term,
      thirty_year = 30,
      fifteen_year = 15
    )

    mnth_growth <- home_prices %>% 
      filter(date >= input$home_appreciation[1],
             date <= input$home_appreciation[2]) %>% 
      summarize(monthly_growth_rate = mean(monthly_growth_rate,na.rm=T)) %>% 
      pull(monthly_growth_rate)
    
    
    
    amort(
      principal = loan_amount, interest = int_rate, duration = duration,
      home_appreciation = mnth_growth
    )
  })


  output$pay_plot <- renderPlotly({
    amort <- amort_data()

    duration <- switch(input$term,
      thirty_year = 30,
      fifteen_year = 15
    )
    duration <- 30

    amort$data %>%
      rowid_to_column() %>%
      left_join(
        tibble(date = seq.Date(Sys.Date(), Sys.Date() + years(duration), by = "months")) %>%
          rowid_to_column()
      ) %>%
      print() %>%
      mutate(profit = home_value-total_payed) %>%
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
      layout(
        barmode = input$plot_mode,
        bargap = 0
      )
  })
}

shinyApp(ui, server)