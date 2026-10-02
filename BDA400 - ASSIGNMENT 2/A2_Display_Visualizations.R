source("A2_Stock_Data_Functions.R")

display_stock_data <- function(stock_data, rows = 6) {
  
  for (symbol in names(stock_data)) {
    cat("\n-----------------------------\n")
    cat("Stock:", symbol, "\n")
    cat("-----------------------------\n")
    
    print(head(stock_data[[symbol]], rows))
  }
}

plot_stock_data <- function(stock_data,
                            moving_average_window = 20) {
  
  old_settings <- par(no.readonly = TRUE)
  on.exit(par(old_settings))
  
  par(
    mfrow = c(length(stock_data), 1),
    mar = c(3, 4, 3, 2)
  )
  
  for (symbol in names(stock_data)) {
    
    stock_df <- stock_data[[symbol]]
    
    close_column <- grep(
      "\\.Close$",
      names(stock_df),
      value = TRUE
    )[1]
    
    closing_prices <- stock_df[[close_column]]
    
    plot(
      stock_df$Date,
      closing_prices,
      type = "l",
      col = "steelblue",
      lwd = 1.5,
      main = paste(
        symbol,
        "Closing Price and 20-Day Moving Average"
      ),
      xlab = "Date",
      ylab = "Closing Price"
    )
    
    lines(
      stock_df$Date,
      TTR::SMA(
        closing_prices,
        n = moving_average_window
      ),
      col = "red",
      lwd = 2
    )
    
    legend(
      "topleft",
      legend = c(
        "Closing Price",
        "20-Day Moving Average"
      ),
      col = c("steelblue", "red"),
      lty = 1,
      lwd = 2,
      bty = "n"
    )
  }
}

display_stock_data(stock_data)

print(statistics)

plot_stock_data(stock_data)