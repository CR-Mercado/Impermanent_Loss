# COINGECKO FUNCTIONS 
library(jsonlite)
library(anytime)

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

eth_price_history <- get_token_price_history("ethereum", Sys.Date()-89, Sys.Date())
wbtc <- get_token_price_history("wrapped-bitcoin", Sys.Date()-89, Sys.Date())


