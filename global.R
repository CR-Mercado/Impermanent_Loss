# COINGECKO FUNCTIONS 
library(jsonlite)
library(anytime)
library(ggplot2)
library(plotly)

# Get available tokens ----
token_list <- fromJSON(
  readLines("https://api.coingecko.com/api/v3/coins/list")
)

down_selected_tokens <- c("ethereum",
                          "wrapped-bitcoin",
                          "chainlink",
                          "yearn-finance",
                          "uniswap",
                          "sushi")

# Get price history of a token ----

get_token_price_history <- function(id, date1, date2){ 
  
  if( !(id %in% token_list$id) ){ 
    stop("Didn't find id in token_list, did you accidentally use name?")
  }
  
  time1 = as.numeric(as.POSIXct(date1, format="%Y-%m-%d"))
  time2 = as.numeric(as.POSIXct(date2, format="%Y-%m-%d"))
  
  url = paste0(
    "https://api.coingecko.com/api/v3/coins/",
    id,
    "/market_chart/range?vs_currency=usd&from=",
    time1,
    "&to=",
    time2)
  
  print(url)
  
  price_history <- fromJSON(
    readLines(url)
  )

 price_history <- lapply(price_history, function(x){
    x <- as.data.frame(x)
    x <- data.frame(date_time = x[[1]], 
                    value = x[[2]])
    
    x$date_time <- anytime(x$date_time/1000)
    return(x)
  })
  
  return(price_history)
}

# Last 89 days data ----

eth <- get_token_price_history("ethereum", Sys.Date()-89, Sys.Date())
wbtc <- get_token_price_history("wrapped-bitcoin", Sys.Date()-89, Sys.Date())

# Diagnostics ----

length_check <- function(token1, token2){ 
  if( length(token1$prices$date_time) != length(token2$prices$date_time) ){ 
    stop("One token has more entries than the other, cannot safely divide.")
  }
}

time_check <- function(token1, token2){ 
  
  if( mean(token1$prices$date_time == token1$prices$date_time) != 1 ){ 
    warning("Time stamps are not perfectly aligned, but will continue.
            See: date_time_diff_plot() for diagnostics on time difference.")
  }
  }

## Differences in date-time 

date_time_diff_plot <- function(token1, token2){ 
  
  length_check(token1, token2)
  
  date_time_diff <- data.frame( 
    UNIX_time_diff = token1$prices$date_time - token2$prices$date_time,
    token1_time = token1$prices$date_time
  )
  
  dtd <- ggplot(data = date_time_diff,
                aes(x = token1_time, y = UNIX_time_diff)) + 
    geom_line() + labs(x = "Token 1 Time", y = "UNIX Time Diff") + 
    theme_classic()
  
  return(ggplotly(dtd))
  }


# Price Compare ----

price_compare <- function(token1, token2){ 
  
  length_check(token1, token2)
  
 
  

  
  }



