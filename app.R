library(shiny)
library(jsonlite)
library(curl)
library(shinycssloaders)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Crypto Currency Follow Up"),
  
  # Sidebar with input selectors
  sidebarLayout(
    sidebarPanel(
      selectInput("curr",h3("Currency Selector"),choices=sort(c("BCH","ETH","DASH","ZEC","XMR","LTC","ETC","BTG","BTC")),multiple=TRUE,selected=sort(c("BCH","ETH","DASH","ZEC","XMR","LTC","ETC","BTG","BTC"))[1]),
      h3("Quick Quotient Calculator"),
      textInput("num1",h5("Coin Price"),value="",width="40%"),
      textInput("num2",h5("Ref Coin Price"),value="",width="40%"),
      br(),
      h5("Coin Price / Ref Coin Price result :"),
      width = 3
    ),
    
    # Main Panel
    mainPanel(
      tabsetPanel(
        tabPanel("All Stats",withSpinner(dataTableOutput("stats"),type=1,color="#d3d9e2")),
        tabPanel("Weekly Summary",withSpinner(dataTableOutput("weekly"),type=1,color="#d3d9e2"))),
      width=9
    )
  )
)

# Define server logic 
server <- function(input, output,session) {
  
  # Gather history data
  # Minutely for day 0-6
  # Hourly for day 7-30
  history_data <- NULL
  coin_list <- c("BCH","ETH","DASH","ZEC","XMR","LTC","ETC","BTG","BTC")
  
  # MINUTELY DATA FOR EACH COIN
  for (i in coin_list) {
    temp <- NULL
    minute_coin_url <- paste0("https://min-api.cryptocompare.com/data/histominute?fsym=",i,"&tsym=BTC&limit=9999&allData=true&e=CCCAGG")
    minute_usd_url <- paste0("https://min-api.cryptocompare.com/data/histominute?fsym=",i,"&tsym=USD&limit=9999&allData=true&e=CCCAGG")
    
    if (i != "BTC"){
      temp <- fromJSON(minute_coin_url)$Data
      temp$FromCoin <- i
      temp$ToCoin <- "BTC"
      temp$time <- as.POSIXct(as.numeric(as.character(temp$time)),origin="1970-01-01",tz="GMT")
      history_data <- rbind(history_data,temp)
    }
    
    temp <- NULL
    temp <- fromJSON(minute_usd_url)$Data
    temp$FromCoin <- i
    temp$ToCoin <- "USD"
    temp$time <- as.POSIXct(as.numeric(as.character(temp$time)),origin="1970-01-01",tz="GMT")
    history_data <- rbind(history_data,temp)
  }
  
  min_date <- min(history_data$time)
  
  # HOURLY DATA FOR EACH COIN
  for (i in coin_list) {
    temp <- NULL
    hour_coin_url <- paste0("https://min-api.cryptocompare.com/data/histohour?fsym=",i,"&tsym=BTC&limit=720&aggregate=3&e=CCCAGG")
    hour_usd_url <- paste0("https://min-api.cryptocompare.com/data/histohour?fsym=",i,"&tsym=USD&limit=720&aggregate=3&e=CCCAGG")
    if (i != "BTC"){
      temp <- fromJSON(hour_coin_url)$Data
      temp$FromCoin <- i
      temp$ToCoin <- "BTC"
      temp$time <- as.POSIXct(as.numeric(as.character(temp$time)),origin="1970-01-01",tz="GMT")
      temp <- temp[temp$time < min_date,]
      history_data <- rbind(history_data,temp)
    }
    temp <- NULL
    temp <- fromJSON(hour_usd_url)$Data
    temp$FromCoin <- i
    temp$ToCoin <- "USD"
    temp$time <- as.POSIXct(as.numeric(as.character(temp$time)),origin="1970-01-01",tz="GMT")
    temp <- temp[temp$time < min_date,]
    history_data <- rbind(history_data,temp)
  }
  
  # REAL TIME DATA
  real.time.data <- function() {
    rt <-fromJSON(paste0("https://min-api.cryptocompare.com/data/pricemultifull?fsyms=",paste(coin_list,collapse = ","),"&tsyms=BTC,USD"))
    temp.time <- Sys.time()
    for (i in coin_list) {
      temp <- data.frame(temp.time, 
                         getElement(rt$RAW,i)$BTC$PRICE,  
                         getElement(rt$RAW,i)$BTC$PRICE,  
                         getElement(rt$RAW,i)$BTC$PRICE,  
                         getElement(rt$RAW,i)$BTC$PRICE,  
                         getElement(rt$RAW,i)$BTC$LASTVOLUME,  
                         getElement(rt$RAW,i)$BTC$LASTVOLUMETO,  
                         i, 
                         "BTC")
      names(temp) <- names(history_data)
      history_data <<- rbind(history_data,temp)
      temp <- data.frame(temp.time, 
                         getElement(rt$RAW,i)$USD$PRICE,  
                         getElement(rt$RAW,i)$USD$PRICE,  
                         getElement(rt$RAW,i)$USD$PRICE,  
                         getElement(rt$RAW,i)$USD$PRICE,  
                         getElement(rt$RAW,i)$USD$LASTVOLUME,  
                         getElement(rt$RAW,i)$USD$LASTVOLUMETO,  
                         i, 
                         "USD")
      names(temp) <- names(history_data)
      history_data <<- rbind(history_data,temp)
    }
    print("executed")
  }
  
  # STATS CALCULATION
  stat.calc <- function() {
    real.time.data()
    stats <- NULL
    for (i in coin_list[-length(coin_list)]) {
      # REAL TIME DATA
      rt <- setNames(data.frame("Real Time Data",
                                i,
                                round(history_data$close[history_data$time==max(history_data$time) & history_data$FromCoin==i & history_data$ToCoin =="USD"]/history_data$close[history_data$time==max(history_data$time) & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"],4),
                                NA,
                                NA,
                                round(history_data$close[history_data$time==max(history_data$time) & history_data$FromCoin==i & history_data$ToCoin =="USD"],2),
                                NA,
                                NA,
                                NA,
                                NA,
                                NA,
                                round(history_data$close[history_data$time==max(history_data$time) & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"],2),
                                NA,
                                NA,
                                NA,
                                NA,
                                NA
      ),c("Desc","Coin","Quotient","Q Highest","Q Lowest","Coin Price","Coin Highest","Coin Lowest","Coin RT vs. Avg","Coin RT vs. High","Coin RT vs. Low","BTC Price","BTC Highest","BTC Lowest","BTC RT vs. Avg","BTC RT vs. High","BTC RT vs. Low"))
      
      # LAST HOUR DATA
      lh <- setNames(data.frame("Last Hour Data",
                                i,
                                round(mean(history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-3600 & history_data$FromCoin==i & history_data$ToCoin =="USD"]/history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-3600 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),4),
                                round(max(history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-3600 & history_data$FromCoin==i & history_data$ToCoin =="USD"]/history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-3600 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),4),
                                round(min(history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-3600 & history_data$FromCoin==i & history_data$ToCoin =="USD"]/history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-3600 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),4),
                                round(mean(history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-3600 & history_data$FromCoin==i & history_data$ToCoin =="USD"]),2),
                                round(max(history_data$high[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-3600 & history_data$FromCoin==i & history_data$ToCoin =="USD"]),2),
                                round(min(history_data$low[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-3600 & history_data$FromCoin==i & history_data$ToCoin =="USD"]),2),   
                                NA,
                                NA,
                                NA,
                                round(mean(history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-3600 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),2),
                                round(max(history_data$high[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-3600 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),2),
                                round(min(history_data$low[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-3600 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),2),   
                                NA,
                                NA,
                                NA
      ),c("Desc","Coin","Quotient","Q Highest","Q Lowest","Coin Price","Coin Highest","Coin Lowest","Coin RT vs. Avg","Coin RT vs. High","Coin RT vs. Low","BTC Price","BTC Highest","BTC Lowest","BTC RT vs. Avg","BTC RT vs. High","BTC RT vs. Low"))
      
      
      # LAST 4 HOURS DATA
      l4h <- setNames(data.frame("Last 4 Hours Data",
                                 i,
                                 round(mean(history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-14400 & history_data$FromCoin==i & history_data$ToCoin =="USD"]/history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-14400 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),4),
                                 round(max(history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-14400 & history_data$FromCoin==i & history_data$ToCoin =="USD"]/history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-14400 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),4),
                                 round(min(history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-14400 & history_data$FromCoin==i & history_data$ToCoin =="USD"]/history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-14400 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),4),
                                 round(mean(history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-14400 & history_data$FromCoin==i & history_data$ToCoin =="USD"]),2),
                                 round(max(history_data$high[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-14400 & history_data$FromCoin==i & history_data$ToCoin =="USD"]),2),
                                 round(min(history_data$low[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-14400 & history_data$FromCoin==i & history_data$ToCoin =="USD"]),2),   
                                 NA,
                                 NA,
                                 NA,
                                 round(mean(history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-14400 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),2),
                                 round(max(history_data$high[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-14400 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),2),
                                 round(min(history_data$low[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-14400 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),2),
                                 NA,
                                 NA,
                                 NA
      ),c("Desc","Coin","Quotient","Q Highest","Q Lowest","Coin Price","Coin Highest","Coin Lowest","Coin RT vs. Avg","Coin RT vs. High","Coin RT vs. Low","BTC Price","BTC Highest","BTC Lowest","BTC RT vs. Avg","BTC RT vs. High","BTC RT vs. Low"))
      
      
      # LAST 12 HOURS DATA
      l12h <- setNames(data.frame("Last 12 Hours Data",
                                  i,
                                  round(mean(history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-43200 & history_data$FromCoin==i & history_data$ToCoin =="USD"]/history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-43200 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),4),
                                  round(max(history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-43200 & history_data$FromCoin==i & history_data$ToCoin =="USD"]/history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-43200 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),4),
                                  round(min(history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-43200 & history_data$FromCoin==i & history_data$ToCoin =="USD"]/history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-43200 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),4),
                                  round(mean(history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-43200 & history_data$FromCoin==i & history_data$ToCoin =="USD"]),2),
                                  round(max(history_data$high[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-43200 & history_data$FromCoin==i & history_data$ToCoin =="USD"]),2),
                                  round(min(history_data$low[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-43200 & history_data$FromCoin==i & history_data$ToCoin =="USD"]),2),   
                                  NA,
                                  NA,
                                  NA,
                                  round(mean(history_data$close[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-43200 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),2),
                                  round(max(history_data$high[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-43200 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),2),
                                  round(min(history_data$low[history_data$time<=max(history_data$time) & history_data$time>=max(history_data$time)-43200 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),2),   
                                  NA,
                                  NA,
                                  NA
      ),c("Desc","Coin","Quotient","Q Highest","Q Lowest","Coin Price","Coin Highest","Coin Lowest","Coin RT vs. Avg","Coin RT vs. High","Coin RT vs. Low","BTC Price","BTC Highest","BTC Lowest","BTC RT vs. Avg","BTC RT vs. High","BTC RT vs. Low"))
      
      
      # CURRENT DAY DATA
      cd <- setNames(data.frame("Current Day Data",
                                i,
                                round(mean(history_data$close[as.Date(history_data$time)==as.Date(Sys.time())& history_data$FromCoin==i & history_data$ToCoin =="USD"]/history_data$close[as.Date(history_data$time)==as.Date(Sys.time()) & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),4),
                                round(max(history_data$close[as.Date(history_data$time)==as.Date(Sys.time()) & history_data$FromCoin==i & history_data$ToCoin =="USD"]/history_data$close[as.Date(history_data$time)==as.Date(Sys.time()) & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),4),
                                round(min(history_data$close[as.Date(history_data$time)==as.Date(Sys.time())& history_data$FromCoin==i & history_data$ToCoin =="USD"]/history_data$close[as.Date(history_data$time)==as.Date(Sys.time()) & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),4),
                                round(mean(history_data$close[as.Date(history_data$time)==as.Date(Sys.time())& history_data$FromCoin==i & history_data$ToCoin =="USD"]),2),
                                round(max(history_data$high[as.Date(history_data$time)==as.Date(Sys.time()) & history_data$FromCoin==i & history_data$ToCoin =="USD"]),2),
                                round(min(history_data$low[as.Date(history_data$time)==as.Date(Sys.time()) & history_data$FromCoin==i & history_data$ToCoin =="USD"]),2),
                                NA,
                                NA,
                                NA,
                                round(mean(history_data$close[as.Date(history_data$time)==as.Date(Sys.time()) & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),2),
                                round(max(history_data$high[as.Date(history_data$time)==as.Date(Sys.time()) & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),2),
                                round(min(history_data$low[as.Date(history_data$time)==as.Date(Sys.time()) & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),2),
                                NA,
                                NA,
                                NA
      ),c("Desc","Coin","Quotient","Q Highest","Q Lowest","Coin Price","Coin Highest","Coin Lowest","Coin RT vs. Avg","Coin RT vs. High","Coin RT vs. Low","BTC Price","BTC Highest","BTC Lowest","BTC RT vs. Avg","BTC RT vs. High","BTC RT vs. Low"))
      
      
      # LAST 7 DAYS DATA
      l7d <- setNames(data.frame("Last 7 Days Data",
                                 i,
                                 round(mean(history_data$close[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-7 & history_data$FromCoin==i & history_data$ToCoin =="USD"]/history_data$close[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-7 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),4),
                                 round(max(history_data$close[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-7 & history_data$FromCoin==i & history_data$ToCoin =="USD"]/history_data$close[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-7 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),4),
                                 round(min(history_data$close[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-7 & history_data$FromCoin==i & history_data$ToCoin =="USD"]/history_data$close[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-7 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),4),                     
                                 round(mean(history_data$close[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-7 & history_data$FromCoin==i & history_data$ToCoin =="USD"]),2),
                                 round(max(history_data$high[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-7 & history_data$FromCoin==i & history_data$ToCoin =="USD"]),2),
                                 round(min(history_data$low[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-7 & history_data$FromCoin==i & history_data$ToCoin =="USD"]),2),   
                                 NA,
                                 NA,
                                 NA,
                                 round(mean(history_data$close[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-7 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),2),
                                 round(max(history_data$high[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-7 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),2),
                                 round(min(history_data$low[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-7 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),2),   
                                 NA,
                                 NA,
                                 NA
      ),c("Desc","Coin","Quotient","Q Highest","Q Lowest","Coin Price","Coin Highest","Coin Lowest","Coin RT vs. Avg","Coin RT vs. High","Coin RT vs. Low","BTC Price","BTC Highest","BTC Lowest","BTC RT vs. Avg","BTC RT vs. High","BTC RT vs. Low"))
      
      
      
      # LAST 30 DAYS DATA
      l30d <- setNames(data.frame("Last 30 Days Data",
                                  i,
                                  round(mean(history_data$close[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-30 & history_data$FromCoin==i & history_data$ToCoin =="USD"]/history_data$close[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-30 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),4),
                                  round(max(history_data$close[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-30 & history_data$FromCoin==i & history_data$ToCoin =="USD"]/history_data$close[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-30 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),4),
                                  round(min(history_data$close[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-30 & history_data$FromCoin==i & history_data$ToCoin =="USD"]/history_data$close[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-30 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),4),
                                  round(mean(history_data$close[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-30 & history_data$FromCoin==i & history_data$ToCoin =="USD"]),2),
                                  round(max(history_data$high[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-30 & history_data$FromCoin==i & history_data$ToCoin =="USD"]),2),
                                  round(min(history_data$low[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-30 & history_data$FromCoin==i & history_data$ToCoin =="USD"]),2),   
                                  NA,
                                  NA,
                                  NA,
                                  round(mean(history_data$close[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-30 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),2),
                                  round(max(history_data$high[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-30 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),2),
                                  round(min(history_data$low[as.Date(history_data$time)<=as.Date(Sys.time()) & as.Date(history_data$time)>=as.Date(Sys.time())-30 & history_data$FromCoin=="BTC" & history_data$ToCoin =="USD"]),2),   
                                  NA,
                                  NA,
                                  NA
      ),c("Desc","Coin","Quotient","Q Highest","Q Lowest","Coin Price","Coin Highest","Coin Lowest","Coin RT vs. Avg","Coin RT vs. High","Coin RT vs. Low","BTC Price","BTC Highest","BTC Lowest","BTC RT vs. Avg","BTC RT vs. High","BTC RT vs. Low"))
      
      
      stats <- rbind(stats,rt,lh,l4h,l12h,cd,l7d,l30d)
      
    }
    
    for (i in coin_list[-length(coin_list)]){
      tempCoin <- stats$`Coin Price`[stats$Coin==i & stats$Desc=="Real Time Data"]
      tempBTC <- stats$`BTC Price`[stats$Coin==i & stats$Desc=="Real Time Data"]
      stats$`Coin RT vs. Avg`[stats$Desc!="Real Time Data" & stats$Coin==i] <- paste0(round((tempCoin-stats$`Coin Price`[stats$Desc!="Real Time Data" & stats$Coin==i])/stats$`Coin Price`[stats$Desc!="Real Time Data" & stats$Coin==i]*100,2),"%")
      stats$`Coin RT vs. High`[stats$Desc!="Real Time Data" & stats$Coin==i] <- paste0(round((tempCoin-stats$`Coin Highest`[stats$Desc!="Real Time Data" & stats$Coin==i])/stats$`Coin Highest`[stats$Desc!="Real Time Data" & stats$Coin==i]*100,2),"%")
      stats$`Coin RT vs. Low`[stats$Desc!="Real Time Data" & stats$Coin==i] <- paste0(round((tempCoin-stats$`Coin Lowest`[stats$Desc!="Real Time Data" & stats$Coin==i])/stats$`Coin Lowest`[stats$Desc!="Real Time Data" & stats$Coin==i]*100,2),"%")
      stats$`BTC RT vs. Avg`[stats$Desc!="Real Time Data" & stats$Coin==i] <- paste0(round((tempBTC-stats$`BTC Price`[stats$Desc!="Real Time Data" & stats$Coin==i])/stats$`BTC Price`[stats$Desc!="Real Time Data" & stats$Coin==i]*100,2),"%")
      stats$`BTC RT vs. High`[stats$Desc!="Real Time Data" & stats$Coin==i] <- paste0(round((tempBTC-stats$`BTC Highest`[stats$Desc!="Real Time Data" & stats$Coin==i])/stats$`BTC Highest`[stats$Desc!="Real Time Data" & stats$Coin==i]*100,2),"%")
      stats$`BTC RT vs. Low`[stats$Desc!="Real Time Data" & stats$Coin==i] <- paste0(round((tempBTC-stats$`BTC Lowest`[stats$Desc!="Real Time Data" & stats$Coin==i])/stats$`BTC Lowest`[stats$Desc!="Real Time Data" & stats$Coin==i]*100,2),"%")
      
    }
    
    return(stats)
  }
  
  weekly.calc <- function() {
    stats_summ <- NULL
    for (i in coin_list) {
      if (i != "BTC") {
        temp <-setNames(data.frame(i,
                                   stats$`Coin Price`[stats$Coin==i & stats$Desc=="Last 7 Days Data"],
                                   stats$`Coin Highest`[stats$Coin==i & stats$Desc=="Last 7 Days Data"],
                                   stats$`Coin Lowest`[stats$Coin==i & stats$Desc=="Last 7 Days Data"],
                                   stats$`Coin RT vs. Avg`[stats$Coin==i & stats$Desc=="Last 7 Days Data"],
                                   stats$`Coin RT vs. High`[stats$Coin==i & stats$Desc=="Last 7 Days Data"],
                                   stats$`Coin RT vs. Low`[stats$Coin==i & stats$Desc=="Last 7 Days Data"]
                                   
        ),c("Coin","Weekly Average","High","Low","RT vs. Avg","RT vs. High","RT vs. Low"))
      } else {
        temp <-setNames(data.frame(i,
                                   stats$`BTC Price`[stats$Coin=="BCH" & stats$Desc=="Last 7 Days Data"],
                                   stats$`BTC Highest`[stats$Coin=="BCH" & stats$Desc=="Last 7 Days Data"],
                                   stats$`BTC Lowest`[stats$Coin=="BCH" & stats$Desc=="Last 7 Days Data"],
                                   stats$`BTC RT vs. Avg`[stats$Coin=="BCH" & stats$Desc=="Last 7 Days Data"],
                                   stats$`BTC RT vs. High`[stats$Coin=="BCH" & stats$Desc=="Last 7 Days Data"],
                                   stats$`BTC RT vs. Low`[stats$Coin=="BCH" & stats$Desc=="Last 7 Days Data"]
                                   
        ),c("Coin","Weekly Average","High","Low","RT vs. Avg","RT vs. High","RT vs. Low"))
        
      }
      
      stats_summ <- rbind(stats_summ,temp)
    }
    
    return(stats_summ)
    
  } 
  
  
  num1 <- reactive({as.numeric(input$num1)})
  num2 <- reactive({as.numeric(input$num2)})
  output$result <- renderPrint({num1()/num2()}) 
  
  output$stats <- DT::renderDataTable(
    {invalidateLater(60000,session)
      stats <<- stat.calc()
      DT::datatable(stats[stats$Coin %in% input$curr,], extensions = 'FixedColumns',
                    options = list(
                      dom = 't',
                      scrollX = TRUE,
                      scrollY = TRUE,
                      fixedColumns = list(leftColumns = 3)
                    ))
    })
  
  output$weekly <- DT::renderDataTable(
    {invalidateLater(60000,session)
    stats_summ <<- weekly.calc()
    DT::datatable(stats_summ[order(stats_summ$`Weekly Average`,decreasing = TRUE),])
    })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)









# Run the application 
shinyApp(ui = ui, server = server)
