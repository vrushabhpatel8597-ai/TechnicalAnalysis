# Step 1: Load required packages

library(shiny)
library(ggplot2)
library(quantmod)

# Step 1: Fetch historical stock data from Yahoo Finance

stock_symbol <- "AAPL"
start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-07-01")

stock_data <- getSymbols(
  stock_symbol,
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

head(stock_data)
tail(stock_data)

# Step 1 continued: Prepare stock data for visualization

stock_df <- data.frame(
  date = as.Date(index(stock_data)),
  open = as.numeric(Op(stock_data)),
  high = as.numeric(Hi(stock_data)),
  low = as.numeric(Lo(stock_data)),
  close = as.numeric(Cl(stock_data)),
  volume = as.numeric(Vo(stock_data)),
  adjusted = as.numeric(Ad(stock_data))
)

head(stock_df)
str(stock_df)


# Step 2: Create the Shiny dashboard interface and basic chart

aggregate_stock_data <- function(data, time_frame) {
  if (time_frame == "Daily") {
    return(data)
  }
  
  if (time_frame == "Weekly") {
    data$period <- format(data$date, "%Y-%U")
  } else {
    data$period <- format(data$date, "%Y-%m")
  }
  
  grouped_data <- split(data, data$period)
  
  result <- do.call(rbind, lapply(grouped_data, function(x) {
    data.frame(
      date = max(x$date),
      open = x$open[1],
      high = max(x$high),
      low = min(x$low),
      close = x$close[nrow(x)],
      volume = sum(x$volume),
      adjusted = x$adjusted[nrow(x)]
    )
  }))
  
  rownames(result) <- NULL
  result
}

ui <- fluidPage(
  titlePanel("AAPL Technical Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = min(stock_df$date),
        end = max(stock_df$date),
        min = min(stock_df$date),
        max = max(stock_df$date)
      ),
      
      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly"),
        selected = "Daily"
      ),
      
      selectInput(
        "chart_type",
        "Select Chart Type:",
        choices = c("Line", "Area", "Candlestick"),
        selected = "Line"
      ),
      
      checkboxGroupInput(
        "technical_indicators",
        "Select Technical Indicators:",
        choices = c("Moving Averages", "RSI", "MACD"),
        selected = "Moving Averages"
      )
    ),
    
    mainPanel(
      plotOutput("stock_chart", height = "650px")
    )
  )
)
server <- function(input, output) {
  output$stock_chart <- renderPlot({
    
    filtered_data <- stock_df[
      stock_df$date >= input$date_range[1] &
        stock_df$date <= input$date_range[2],
      ,
      drop = FALSE
    ]
    
    validate(
      need(
        nrow(filtered_data) > 0,
        "No stock data is available for the selected date range."
      )
    )
    
    filtered_data <- aggregate_stock_data(
      filtered_data,
      input$time_frame
    )
    
    # Calculate indicators only when enough data exists
    
    n_rows <- nrow(filtered_data)
    
    if (n_rows >= 20) {
      filtered_data$ma_short <- TTR::SMA(
        filtered_data$close,
        n = 20
      )
    } else {
      filtered_data$ma_short <- rep(
        NA_real_,
        n_rows
      )
    }
    
    if (n_rows >= 50) {
      filtered_data$ma_long <- TTR::SMA(
        filtered_data$close,
        n = 50
      )
    } else {
      filtered_data$ma_long <- rep(
        NA_real_,
        n_rows
      )
    }
    
    if (n_rows > 14) {
      filtered_data$rsi <- TTR::RSI(
        filtered_data$close,
        n = 14
      )
    } else {
      filtered_data$rsi <- rep(
        NA_real_,
        n_rows
      )
    }
    
    if (n_rows >= 35) {
      macd_result <- TTR::MACD(
        filtered_data$close,
        nFast = 12,
        nSlow = 26,
        nSig = 9
      )
      
      filtered_data$macd <- as.numeric(
        macd_result[, "macd"]
      )
      
      filtered_data$macd_signal <- as.numeric(
        macd_result[, "signal"]
      )
    } else {
      filtered_data$macd <- rep(
        NA_real_,
        n_rows
      )
      
      filtered_data$macd_signal <- rep(
        NA_real_,
        n_rows
      )
    }
    
    # Scale RSI and MACD only for visual overlay
    scale_to_price <- function(
    values,
    price_min,
    price_max
    ) {
      if (all(is.na(values))) {
        return(rep(NA_real_, length(values)))
      }
      
      value_range <- range(
        values,
        na.rm = TRUE
      )
      
      if (value_range[1] == value_range[2]) {
        return(
          rep(
            mean(c(price_min, price_max)),
            length(values)
          )
        )
      }
      
      price_min +
        (values - value_range[1]) /
        (value_range[2] - value_range[1]) *
        (price_max - price_min)
    }
    
    price_min <- min(
      filtered_data$low,
      na.rm = TRUE
    )
    
    price_max <- max(
      filtered_data$high,
      na.rm = TRUE
    )
    
    filtered_data$rsi_scaled <- scale_to_price(
      filtered_data$rsi,
      price_min,
      price_max
    )
    
    filtered_data$macd_scaled <- scale_to_price(
      filtered_data$macd,
      price_min,
      price_max
    )
    
    filtered_data$macd_signal_scaled <- scale_to_price(
      filtered_data$macd_signal,
      price_min,
      price_max
    )
    
    # Step 4: Generate Buy, Sell, and Hold signals
    
    previous_short_ma <- c(
      NA,
      head(filtered_data$ma_short, -1)
    )
    
    previous_long_ma <- c(
      NA,
      head(filtered_data$ma_long, -1)
    )
    
    valid_moving_averages <-
      !is.na(filtered_data$ma_short) &
      !is.na(filtered_data$ma_long) &
      !is.na(previous_short_ma) &
      !is.na(previous_long_ma)
    
    buy_condition <-
      valid_moving_averages &
      filtered_data$ma_short > filtered_data$ma_long &
      previous_short_ma <= previous_long_ma
    
    sell_condition <-
      valid_moving_averages &
      filtered_data$ma_short < filtered_data$ma_long &
      previous_short_ma >= previous_long_ma
    
    filtered_data$Signal <- "Hold"
    
    filtered_data$Signal[buy_condition] <- "Buy"
    filtered_data$Signal[sell_condition] <- "Sell"
    
    # Mark the first valid moving-average state
    first_valid_index <- which(valid_moving_averages)[1]
    
    if (!is.na(first_valid_index)) {
      if (
        filtered_data$ma_short[first_valid_index] >
        filtered_data$ma_long[first_valid_index]
      ) {
        filtered_data$Signal[first_valid_index] <- "Buy"
      } else if (
        filtered_data$ma_short[first_valid_index] <
        filtered_data$ma_long[first_valid_index]
      ) {
        filtered_data$Signal[first_valid_index] <- "Sell"
      }
    }
    
    # Create the selected chart type
    if (input$chart_type == "Candlestick") {
      p <- ggplot(filtered_data) +
        geom_segment(
          aes(
            x = date,
            xend = date,
            y = low,
            yend = high
          ),
          color = "black",
          linewidth = 0.6
        ) +
        geom_rect(
          aes(
            xmin = date - 0.3,
            xmax = date + 0.3,
            ymin = pmin(open, close),
            ymax = pmax(open, close),
            fill = close >= open
          ),
          color = "black"
        ) +
        scale_fill_manual(
          values = c(
            `FALSE` = "firebrick",
            `TRUE` = "forestgreen"
          ),
          guide = "none"
        )
    } else {
      p <- ggplot(
        filtered_data,
        aes(
          x = date,
          y = close
        )
      )
      
      if (input$chart_type == "Area") {
        p <- p +
          geom_area(
            fill = "steelblue",
            alpha = 0.4
          )
      } else {
        p <- p +
          geom_line(
            color = "steelblue",
            linewidth = 1
          )
      }
    }
    
    # Add Moving Average overlays
    if ("Moving Averages" %in% input$technical_indicators) {
      p <- p +
        geom_line(
          data = filtered_data,
          aes(
            x = date,
            y = ma_short
          ),
          color = "orange",
          linewidth = 1,
          na.rm = TRUE
        ) +
        geom_line(
          data = filtered_data,
          aes(
            x = date,
            y = ma_long
          ),
          color = "blue",
          linewidth = 1,
          na.rm = TRUE
        )
    }
    
    # Add RSI overlay
    if ("RSI" %in% input$technical_indicators) {
      p <- p +
        geom_line(
          data = filtered_data,
          aes(
            x = date,
            y = rsi_scaled
          ),
          color = "purple",
          linetype = "dashed",
          linewidth = 0.8,
          na.rm = TRUE
        )
    }
    
    # Add MACD overlays
    if ("MACD" %in% input$technical_indicators) {
      p <- p +
        geom_line(
          data = filtered_data,
          aes(
            x = date,
            y = macd_scaled
          ),
          color = "darkgreen",
          linewidth = 0.8,
          na.rm = TRUE
        ) +
        geom_line(
          data = filtered_data,
          aes(
            x = date,
            y = macd_signal_scaled
          ),
          color = "red",
          linewidth = 0.8,
          na.rm = TRUE
        )
    }
    
    # Add Buy and Sell annotations to the chart
    
    signal_data <- filtered_data[
      filtered_data$Signal != "Hold" &
        !is.na(filtered_data$close),
      ,
      drop = FALSE
    ]
    
    if (nrow(signal_data) > 0) {
      p <- p +
        geom_point(
          data = signal_data,
          aes(
            x = date,
            y = close,
            color = Signal
          ),
          size = 3
        ) +
        geom_text(
          data = signal_data,
          aes(
            x = date,
            y = close,
            label = Signal,
            color = Signal
          ),
          vjust = -0.8,
          size = 3.5,
          show.legend = FALSE
        ) +
        scale_color_manual(
          values = c(
            Buy = "darkgreen",
            Sell = "red"
          ),
          name = "Trading Signal"
        )
    }
    
    selected_indicators <- if (
      length(input$technical_indicators) == 0
    ) {
      "None"
    } else {
      paste(
        input$technical_indicators,
        collapse = ", "
      )
    }
    
    p +
      labs(
        title = "AAPL Stock Price",
        subtitle = paste(
          "Time frame:",
          input$time_frame,
          "| Indicators:",
          selected_indicators
        ),
        x = "Date",
        y = "Price (USD)"
      ) +
      theme_minimal()
  })
}

# Launch the Shiny dashboard

shinyApp(ui, server)
