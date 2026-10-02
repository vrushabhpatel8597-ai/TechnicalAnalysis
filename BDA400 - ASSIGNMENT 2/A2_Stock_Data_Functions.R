library(quantmod)

load_stock_data <- function(file = "portfolio.txt") {
  
  symbols <- readLines(file)
  symbols <- trimws(symbols)
  symbols <- symbols[symbols != ""]
  
  stock_data <- list()
  
  for (symbol in symbols) {
    
    stock_xts <- getSymbols(
      Symbols = symbol,
      src = "yahoo",
      auto.assign = FALSE
    )
    
    stock_data[[symbol]] <- data.frame(
      Date = as.Date(index(stock_xts)),
      coredata(stock_xts),
      row.names = NULL
    )
  }
  
  return(stock_data)
}

stock_data <- load_stock_data("portfolio.txt")

names(stock_data)

head(stock_data$AAPL)

calculate_mode <- function(x) {
  x <- round(na.omit(x), 2)
  unique_x <- unique(x)
  counts <- tabulate(match(x, unique_x))
  unique_x[which.max(counts)]
}

calculate_statistics <- function(stock_data,
                                 moving_average_window = 20) {
  
  statistics_list <- lapply(names(stock_data), function(symbol) {
    
    stock_df <- stock_data[[symbol]]
    
    close_column <- grep(
      "\\.Close$",
      names(stock_df),
      value = TRUE
    )[1]
    
    closing_prices <- na.omit(stock_df[[close_column]])
    
    moving_average <- TTR::SMA(
      closing_prices,
      n = moving_average_window
    )
    
    latest_moving_average <- tail(
      na.omit(moving_average),
      1
    )
    
    data.frame(
      Symbol = symbol,
      Moving_Average_20_Days = as.numeric(
        latest_moving_average
      ),
      Mean = mean(closing_prices),
      Mode = calculate_mode(closing_prices),
      Median = median(closing_prices),
      Standard_Deviation = sd(closing_prices)
    )
  })
  
  do.call(rbind, statistics_list)
}

statistics <- calculate_statistics(stock_data)

print(statistics)