# STA141B Project - Alvis Ip
# App Description: This app pulls resources from the Alpha Vantage API (https://www.alphavantage.co/) and 
# generates stock graphs with simple analyses upon request. Users are able to fetch daily, weekly, or monthly charts,
# and can review supplemental analysis on the company.

##LOAD PACKAGES
library(tidyverse)
library(jsonlite)
library(lubridate)
library(httr)
library(purrr)
library(ggplot2)
library(fpp2)
library(shiny)
library(shinyWidgets)

#UI GENERATION
ui <- fluidPage(
  #CSS tags for background/layout setup
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  tags$head(
    tags$style("label{font-family: Trebuchet MS}")
  ),
  setBackgroundColor(
    color = c("LightGreen", "SeaGreen"),
    gradient = "radial",
    direction = "top",
    shinydashboard = FALSE
  ),
   # Application title
   titlePanel(
     div(
       tags$img(height = 100, width = 177, src = "alphavantage.jpg", class = "pull-right"),
       tags$p(tags$br()),
       tags$p(tags$b(style = "font-family:Lucida Console", 
              "Fundamental Stock Analysis from ", 
              tags$a(href = "https://www.alphavantage.co/", "Alpha Vantage")))
     )),
   tags$p(tags$br()),
   tags$hr(),
   # Sidebar Panel
   sidebarLayout(
      sidebarPanel(
        textInput(inputId = "ticker",
                  label = "Enter Ticker Here:",
                  placeholder = "Ex: TSLA"),
        selectInput(inputId = "type",
                    label = "Frequency",
                    choices = list("-", "Daily", "Weekly", "Monthly")),
        actionButton(inputId = "go",
                     label = "Go!"),
        "(May take up to 30s)",
        tags$p(tags$br()),
        radioButtons(inputId = "analysis",
                     label = "Analysis Options:",
                     choices = list("Valuation" = "valuation",
                                    "Weekly Performance" = "week", 
                                    "Technical Analysis" = "ta", 
                                    "Volume/Liquidity" = "vol"
                                    ))
      ),
      # Show a plot of the generated distribution and potential errors
      mainPanel(
        textOutput("overcall"),
        plotOutput(outputId = "disPlot")
      )
   ),
  tags$hr(),
  # Summary and Analysis Panels
  tags$h1("Summary and Analysis", style = "font-family:Lucida Console"),
  wellPanel(
    fluidRow(
      column(12, align = "center",
        htmlOutput("data_title"),
        tableOutput("data_table")
      )
    )
  ),
  wellPanel(
    fluidRow(
      htmlOutput("week_perf")
    )
  ),
  fluidRow(
    column(12, align = "center", 
           tags$p(tags$b(style = "font-family:Arial", 
                    tags$i("~an app made by ", 
                    tags$a(href = "https://www.linkedin.com/in/alvis-ip/", "Alvis Ip"), "~"))))
    
  )
)

#SERVER GENERATION
server <- function(input, output) {
#BUILD GRAPH
  #Fetch data dependent on inputs, import it as table
  r <- eventReactive(input$go, {
    chart_time_type <- NULL
    freq_val = NULL
    if(input$type == "Daily") {
      chart_time_type <- "TIME_SERIES_DAILY"
    }
    if(input$type == "Weekly") {
      chart_time_type <- "TIME_SERIES_WEEKLY"
    } else if(input$type == "Monthly") {
      chart_time_type <- "TIME_SERIES_MONTHLY"
    }
     GET(
      "https://www.alphavantage.co/query?function={}&symbol={}&apikey={}",
      query = list(
        apikey = Sys.getenv("ALPHA_VANTAGE_KEY"),
        `function` = chart_time_type,
        symbol = input$ticker
      )
    )}
  )
  json <- eventReactive(input$go, content(r(), as = "text", encoding = "UTF-8"))
  values <- eventReactive(input$go, fromJSON(json(), flatten = TRUE))
  
  output$overcall <- renderText({ #Checks if frequency isn't fulfilled or if API calls are maxed out
    if(grepl("Invalid API call.", values()[[1]])) {
      return("Ticker not found. Note that not all NASDAQ securities are available.")
    }
    if(length(values()) == 1 | length(values_sma()) == 1 | length(values_ema()) == 1) {
      return("Please select a valid frequency! Due to call restrictions, please also note that API only allows 1 request per minute.")
    }
  })
  
  #Fetch only the date and opening prices of the table
  valuetbl <- eventReactive(input$go, {
      as_tibble(values()[[2]], flatten = TRUE)
    })
  date_price <- eventReactive(input$go, as.data.frame(t(valuetbl())))
  #Reformat
  complete_tbl <- eventReactive(input$go, {
    date_price() %>%
      mutate(dates = rownames(date_price())) %>%
      select(dates, open = V1)})
  
  dates_prices <- eventReactive(input$go, {
    data.frame(
      dates = {as.Date(complete_tbl()$dates, format = "%Y-%m-%d") %>% rev()}, 
      open = as.numeric(rev(complete_tbl()$open))
    )
  }
  )
  #Use ts from fpp2 to generate time-series data to plot
  dates.ts <- eventReactive(input$go, {
    freq_val = NULL
    if(input$type == "Daily") {
      freq_val <- 365
    }
    if(input$type == "Weekly") {
      freq_val <- 52
    } else if(input$type == "Monthly") {
      freq_val <- 12
    }
    dates_prices() %>%
      select(open = open) %>%
      ts(start = decimal_date(ymd(dates_prices()$dates[1])), freq = freq_val)
    })
  
#BUILD SMA - Uses the same code as above chunk, but for SMA GET request this time.
  sma <- eventReactive(input$go,{
    sma_interval <- NULL
    freq_val = NULL
    if(input$type == "Daily") {
      sma_interval <- "daily"
    }
    if(input$type == "Weekly") {
      sma_interval <- "weekly"
    } else if(input$type == "Monthly") {
      sma_interval <- "monthly"
    }
        GET(
          "https://www.alphavantage.co/query?",
          query = list(
            apikey = Sys.getenv("ALPHA_VANTAGE_KEY"),
            `function` = "SMA",
            interval =  sma_interval,
            symbol = input$ticker,
            time_period = 20,
            series_type = "open"
          )
        )
  })
  
  json_sma <- eventReactive(input$go, content(sma(), as = "text", encoding = "UTF-8"))
  values_sma <- eventReactive(input$go, fromJSON(json_sma(), flatten = TRUE))
  
  #Fetch only the date and opening prices of the table

  valuetbl_sma <- eventReactive(input$go, {
    if(input$type == "Daily") {
      as_tibble(values_sma()[[2]], flatten = TRUE) %>% select(1:length(valuetbl()))}
    else{
      as_tibble(values_sma()[[2]], flatten = TRUE)
    }
  })
  date_price_sma <- eventReactive(input$go, as.data.frame(t(valuetbl_sma())))
  #Reformat
  complete_tbl_sma <- eventReactive(input$go, {
    date_price_sma() %>%
      mutate(dates = rownames(date_price_sma())) %>%
      select(dates, open = V1)})
  
  dates_prices_sma <- eventReactive(input$go, {
    data.frame(dates = {as.Date(complete_tbl_sma()$dates, format = "%Y-%m-%d") %>% rev()}, 
      open = as.numeric(rev(complete_tbl_sma()$open))
    )
  }
  )
  #Use ts from fpp2 to generate time-series data to plot
  dates.ts_sma <- eventReactive(input$go, {
    freq_val_sma = NULL
    if(input$type == "Daily") {
      freq_val_sma <- 365
    }
    if(input$type == "Weekly") {
      freq_val_sma <- 52
    } else if(input$type == "Monthly") {
      freq_val_sma <- 12
    }
    dates_prices_sma() %>%
      select(open = open) %>%
      ts(start = decimal_date(ymd(dates_prices_sma()$dates[1])), freq = freq_val_sma)
  })

#BUILD EMA - Same as SMA, but for EMA GET request.
  ema <- eventReactive(input$go,{
    ema_interval <- NULL
    freq_val = NULL
    if(input$type == "Daily") {
      ema_interval <- "daily"
    }
    if(input$type == "Weekly") {
      ema_interval <- "weekly"
    } else if(input$type == "Monthly") {
      ema_interval <- "monthly"
    }
    GET(
      "https://www.alphavantage.co/query?",
      query = list(
        apikey = Sys.getenv("ALPHA_VANTAGE_KEY"),
        `function` = "ema",
        interval =  ema_interval,
        symbol = input$ticker,
        time_period = 20,
        series_type = "open"
      )
    )
  })
  
  json_ema <- eventReactive(input$go, content(ema(), as = "text", encoding = "UTF-8"))
  values_ema <- eventReactive(input$go, fromJSON(json_ema(), flatten = TRUE))
  
  #Fetch only the date and opening prices of the table
  
  valuetbl_ema <- eventReactive(input$go, {
    if(input$type == "Daily") {
      as_tibble(values_ema()[[2]], flatten = TRUE) %>% select(1:length(valuetbl()))}
    else{
      as_tibble(values_ema()[[2]], flatten = TRUE)
    }
  })
  date_price_ema <- eventReactive(input$go, as.data.frame(t(valuetbl_ema())))
  #Reformat
  complete_tbl_ema <- eventReactive(input$go, {
    date_price_ema() %>%
      mutate(dates = rownames(date_price_ema())) %>%
      select(dates, open = V1)})
  
  dates_prices_ema <- eventReactive(input$go, {
    data.frame(dates = {as.Date(complete_tbl_ema()$dates, format = "%Y-%m-%d") %>% rev()}, 
               open = as.numeric(rev(complete_tbl_ema()$open))
    )
  }
  )
  #Use ts from fpp2 to generate time-series data to plot
  dates.ts_ema <- eventReactive(input$go, {
    freq_val_ema = NULL
    if(input$type == "Daily") {
      freq_val_ema <- 365
    }
    if(input$type == "Weekly") {
      freq_val_ema <- 52
    } else if(input$type == "Monthly") {
      freq_val_ema <- 12
    }
    dates_prices_ema() %>%
      select(open = open) %>%
      ts(start = decimal_date(ymd(dates_prices_ema()$dates[1])), freq = freq_val_ema)
  })
  
#Display plots and corresponding aesthetics
  output$disPlot <- renderPlot({
    autoplot(dates.ts(), series = isolate(input$ticker)) + 
      autolayer(dates.ts_sma(), series = "10 Interval SMA") +
      autolayer(dates.ts_ema(), series = "10 Interval EMA") +
      ggtitle(paste0("$", isolate(input$ticker), " ", isolate(input$type), " Chart"))
  })

  
##ANALYSIS
  #Fetch data for only this week. Same as first code chunk, but is not reactive to frequency input.
  weekly_data <- eventReactive(input$go,{
    GET(
      "https://www.alphavantage.co/query?function={}&symbol={}&apikey={}",
      query = list(
        apikey = Sys.getenv("ALPHA_VANTAGE_KEY"),
        `function` = "TIME_SERIES_DAILY",
        symbol = input$ticker
      )
    )
  })
  json_w <- eventReactive(input$go, content(weekly_data(), as = "text", encoding = "UTF-8"))
  values_w <- eventReactive(input$go, fromJSON(json_w(), flatten = TRUE))
  
  valuetbl_w <- eventReactive(input$go, {
    as_tibble(values_w()[[2]], flatten = TRUE)
  })
  date_price_w <- eventReactive(input$go, as.data.frame(t(valuetbl_w())))
  #Reformat
  complete_tbl_w <- eventReactive(input$go, {
    date_price_w() %>%
      mutate(dates = rownames(date_price_w())) %>%
      select(dates, open = V1)})
  
  dates_prices_w <- eventReactive(input$go, {
    data.frame(
      dates = {as.Date(complete_tbl_w()$dates, format = "%Y-%m-%d") %>% rev()}, 
      open = as.numeric(rev(complete_tbl_w()$open))
    )
  }
  )
  display_table <- eventReactive(input$go, {
    as_tibble(values_w()[[2]], flatten = TRUE)
  })
  
#For Weekly Performance
  low_value <- eventReactive(input$go, {
    round(min(dates_prices_w()$open[nrow(dates_prices_w()):(nrow(dates_prices_w())-6)]), digits = 2)
  })
  date_low <- eventReactive(input$go, {
    dates_prices_w()$dates[dates_prices_w()$open == min(dates_prices_w()$open[nrow(dates_prices_w()):(nrow(dates_prices_w())-6)])]
  })
  high_value <- eventReactive(input$go, {
    round(max(dates_prices_w()$open[nrow(dates_prices_w()):(nrow(dates_prices_w())-6)]), digits = 2)
  })
  date_high <- eventReactive(input$go, {
    dates_prices_w()$dates[dates_prices_w()$open == max(dates_prices_w()$open[nrow(dates_prices_w()):(nrow(dates_prices_w())-6)])]
  })
  swing <- eventReactive(input$go, {
    round(((high_value() - low_value())/high_value())*100, digits = 3)
  })
  volatility <- eventReactive(input$go, {
    if(swing() < 7) {
      "Low Volatility"
      } 
    else if(swing() < 20) {
      "Medium Volatility"} 
    else {
      "High Volatility"}
  })
  
#For Technical Analysis - another SMA GET function static to just the daily charts
  sma_w <- eventReactive(input$go,{
    GET(
      "https://www.alphavantage.co/query?",
      query = list(
        apikey = Sys.getenv("ALPHA_VANTAGE_KEY"),
        `function` = "SMA",
        interval =  "daily",
        symbol = input$ticker,
        time_period = 20,
        series_type = "open"
      )
    )
  })
  
  json_sma_w <- eventReactive(input$go, content(sma_w(), as = "text", encoding = "UTF-8"))
  values_sma_w <- eventReactive(input$go, fromJSON(json_sma_w(), flatten = TRUE))
  
  valuetbl_sma_w <- eventReactive(input$go, {
      as_tibble(values_sma()[[2]], flatten = TRUE) #%>% select(1:length(valuetbl()))
    }
  )
  sma_price <- eventReactive(input$go, {as.numeric(valuetbl_sma_w()[[1,1]])})
  curr_price <- eventReactive(input$go, {as.numeric(valuetbl_w()[[1,1]])})
  a_or_b <- eventReactive(input$go, {
    if(curr_price() < sma_price()) {
      "below"
    }
    else if (curr_price() > sma_price()) {
      "above"
    }
    else{
      "at"
    }
  })
  r_or_s <- eventReactive(input$go, {
    if(a_or_b() == "below") {
      "support"
    }
    else if(a_or_b() == "above") {
      "resistance"
    }
    else{
      "consolidation"
    }
  })
#For volume/liquidity
  volume_df_w <- eventReactive(input$go, {
    date_price_w() %>%
      mutate(dates = rownames(date_price_w())) %>%
      select(dates, volume = V5)})
  volume_w <- eventReactive(input$go, {
    round(mean(as.numeric(volume_df_w()$volume[1:7])))
  })
  
  trade_volume <- eventReactive(input$go, {
    if(volume_w() < 50000) {
      "low volume"
    }
    else if(volume_w() <250000){
      "medium volume"
    }
    else {
      "high volume"
    }
  })
  liquidity <- eventReactive(input$go, {
    if(volume_w() < 50000) {
      "illiquid"
    }
    else if(volume_w() <250000){
      "somewhat liquid"
    }
    else {
      "highly liquid"
    }
  })
#OUTPUTS FOR ANALYSIS
  output$week_perf <- renderText({
    if(input$analysis == "week") {
        paste0("<h4>", isolate(input$ticker)," had a low value this week of ", "<b>$", low_value(),"</b>", " on ", "<b>", date_low(), "</b>", ",",
               " and a high value of ", "<b>$", high_value(), "</b>", " on ", "<b>", date_high(), "</b>.<br>",
               " An intraweek swing of <b>", swing(), "%</b> indicates the stock is a <b style = \"color:red;\">", volatility(), "</b> stock in the short term.</h4>")
      }
    else if(input$analysis == "ta") {
      if(a_or_b() == "below"){
        paste0("<h4>", "The current price of ", isolate(input$ticker), " ($", curr_price(), ") ", "is <b style = \"color:green;\">", a_or_b(), "</b> the 10-day moving average by $", round(abs(curr_price() - sma_price()), digits = 2), ".",
              " Expect <b style = \"color:green;\">", r_or_s(), "</b> at around $", round(curr_price()*0.95, digits = 2), ".")
      }
      else if(a_or_b() == "above") {
        paste0("<h4>", "The current price of ", isolate(input$ticker), " ($", curr_price(), ") ", "is <b style = \"color:red;\">", a_or_b(), "</b> the 10-day moving average by $", round(abs(curr_price() - sma_price()), digits = 2), ".",
              " Expect <b style = \"color:red;\">", r_or_s(), "</b> at around $", round(curr_price()*1.05, digits = 2), ".")
      }
      else if(a_or_b() == "at") {
        paste0("<h4>", "The current price of ", input$ticker, " ($", curr_price(), ") ", "is <b style = \"color:orange;\">", a_or_b(), "</b> the 10-day moving average by $", round(abs(curr_price() - sma_price()), digits = 2), ".",
              " Expect ", r_or_s(), "around around a similar price point.")
      }
    }
    else if(input$analysis == "vol") {
      if(liquidity() == "highly liquid") {
        paste0("<h4>The average number of shares traded over the week for ", isolate(input$ticker), " is <b>", volume_w(), "</b>. Given this amount of outstanding shares, ", isolate(input$ticker), " is currently a <b style = \"color:green;\">", trade_volume(),
               "</b> stock and is <b style = \"color:green;\">", liquidity(), "</b>. There should be no problem exeucting trades at market rate.</h4>")
      }
      else if(liquidity() == "somewhat liquid") {
        paste0("<h4>The average number of shares traded over the week for ", isolate(input$ticker), " is <b>", volume_w(), "</b>. Given this amount of outstanding shares, ", isolate(input$ticker), " is currently a <b style = \"color:orange;\">", trade_volume(),
               "</b> stock and is <b style = \"color:orange;\">", liquidity(), "</b>. Expect to have trades filled around <b>$", round(curr_price()*.98, digits = 2), "</b> to <b>$", round(curr_price()*1.02, digits = 2), "</b>.</h4>")
      }
      else if(liquidity() == "illiquid") {
        paste0("<h4>The average number of shares traded over the week for ", isolate(input$ticker), " is <b>", volume_w(), "</b>. Given this amount of outstanding shares, ", isolate(input$ticker), " is currently a <b style = \"color:red;\">", trade_volume(),
               "</b> stock and is <b style = \"color:red;\">", liquidity(), "</b>. It is ill-advised to trade this stock due to wide spreads. Expect to have trades filled anywhere from <b>$", round(curr_price()*.80, digits = 2), "</b> to <b>$", round(curr_price()*1.20, digits = 2), "</b>.</h4>")
      }
    }
    else if(input$analysis == "valuation") {
      weight <- 0
      if(a_or_b() == "below") {
        weight <- weight + 5
      }
      else {
        weight <- weight - 2
      }
      if (volatility() == "Low Volatility") {
        weight <- weight + 3
      }
      else if(volatility() == "High Volatility") {
        weight <- weight - 3
      }
      if(trade_volume() == "high volume") {
        weight <- weight + 3
      }
      else if(trade_volume() == "medium volume") {
        weight <- weight + 1
      }
      else {
        weight <- weight - 2
      }
      if(swing() < 1) {
        weight <- weight - 3
      }
      else if(swing() < 3) {
        weight <- weight + 1
      }
      else{
        weight <- weight + 3
      }
      
      if(weight < 5) {
        paste0("<h4>Given the price point, moving averages, volatility, and liquidity of ", isolate(input$ticker), ", we currently value this stock as </h4>","<h1><center><b style = \"color:red;\">overweight.</b></center></h1>",
               "<h4>Our indicators suggest this company is overpriced at current prices, and its proper valuation should be at a lower price point.</h4>")
      }
      else if(weight < 9) {
        paste0("<h4>Given the price point, moving averages, volatility, and liquidity of ", isolate(input$ticker), ", we currently value this stock as </h4>","<h1><center><b style = \"color:orange;\">equal weight.</b></center></h1>",
               "<h4>Our indicators suggest this company is fairly valued at its current price point.</h4>")
      }
      else {
        paste0("<h4>Given the price point, moving averages, volatility, and liquidity of ", isolate(input$ticker), ", we currently value this stock as </h4>","<h1><center><b style = \"color:green;\">underweight.</b></center></h1>",
               "<h4>Our indicators suggest this company is undervalued at current prices, and its proper valuation should be at a higher price point.</h4>")
      }
    }
  })
#OUTPUTS FOR WEEKLY SUMMARY  
  output$data_title <- renderText({
    if(!is.null(display_table())){
      paste("<h3><b> Week-to-Date Performance of", isolate(input$ticker),"</b></h3>")
    }
  })
  output$data_table <- renderTable({
    display_table() %>% select(1:7) %>% slice(1)
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
