###Download data
get_eod <- function(tickers, exchange, filename){
  eod <- tibble()

  for(i in tickers){
    print(i)
    tick <- paste0(i,".",exchange)
    ticker.link_ts <- paste0("https://eodhistoricaldata.com/api/eod/",tick,"?&api_token=5aad812c21367&")
    eodLoop <- read.csv(url(ticker.link_ts))
    eodLoop <- eodLoop[-c(nrow(eodLoop)),]
    eodLoop$Code <- i
    eod <- rbind(eod,eodLoop)
  }
  eod$Date <- as.Date(eod$Date)
  #Upload to ftp-server
  ftp_link <- "/www/data"
  local_link <- paste0(getwd(),"/", filename)
  save(eod, file = filename )

  ncftpput_command <- paste("ncftpput -R -v -u '85810_ftp' -p 'LOVUpi928' linweb05.sbv.webhuset.no", ftp_link,local_link)
  system(ncftpput_command)

  #Delete
  unlink(local_link)

  as.tibble(eod)
}

#################################################################################
###########             Download ticker codes for exchange                     #############
#################################################################################
update_info <- function(country_code = "F",
                        exchange_filter = "OL",
                        filename = "norge_info.json"){

  ticker.link <- paste0("https://eodhistoricaldata.com/api/exchanges/",country_code,"?api_token=5aad812c21367&")
  data <- read.csv(url(ticker.link))
  if(exchange_filter != "OL") data <- data[-c(nrow(data)),] %>% dplyr::filter(Exchange == exchange_filter)
  tickers <- data$Code

  stock_info <- data.frame()
  for(i in tickers){
    result = tryCatch({

      ticker.link_fd0 <- paste0("https://eodhistoricaldata.com/api/fundamentals/",i,".",country_code,"?api_token=5aad812c21367&filter=General")
      stock_info0 <- tryCatch( as.data.frame(as.list(RJSONIO::fromJSON(ticker.link_fd0,null=NA,encoding="Latin1")), stringsAsFactors = FALSE), error = function() next)
      if(ncol(stock_info0) == 13) stock_info <- gtools::smartbind(stock_info,stock_info0)

    }, warning = function(w) {
      print(w)
    }, error = function(e) {
      print(e)
    })
    print(i)
  }
  stock_info1 <- stock_info
  #Filter data
  stock_info <- stock_info %>% dplyr::filter(Type == "Common Stock" & Sector != "" & Sector != "Other")

  local_link <- paste0("~/rankingEOD/data/",filename)
  json_file <- jsonlite::toJSON(stock_info)
  write_lines(json_file, path = local_link)
  #Upload to ftp-server
  ftp_link <- "/www/data"
  ncftpput_command <- paste("ncftpput -R -v -u '85810_ftp' -p 'LOVUpi928' linweb05.sbv.webhuset.no", ftp_link,local_link)
  system(ncftpput_command)
  return(stock_info1)

}


