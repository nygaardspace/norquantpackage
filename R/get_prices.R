###############################################################################
# Create daily prices xts object from master_tibble
#' @export
###############################################################################
long_to_xts <- function(master_tibble){
  close_all <- reshape2::dcast(master_tibble, Date ~ Code, value.var = "Close",fun.aggregate = mean)
  prices_daily <- xts::xts(close_all[-c(1)], order.by = close_all$Date)
  prices_daily <- zoo::na.locf(prices_daily)
  return(prices_daily)
}
###############################################################################
# Create monthly prices xts object from daily prices
#' @export
###############################################################################
xts_to_long <- function(returns_monthly){
  returns_monthly <- data.frame(day=zoo::index(returns_monthly), zoo::coredata(returns_monthly))
  returns_monthly_long1 <- reshape2::melt(as.data.frame(returns_monthly),id.vars="day")
  names(returns_monthly_long1) <- c("Date","Code","Close")
  returns_monthly_long1$Code <- as.character(returns_monthly_long1$Code)
  returns_monthly_long1$Date <- lubridate::ymd(returns_monthly_long1$Date)
  returns_monthly_long1 <- tibble::as.tibble(returns_monthly_long1)
  return(returns_monthly_long1)
}
###############################################################################
# Create monthly prices xts object from daily prices
#' @export
###############################################################################
daily_2_monthly <- function(prices_daily){
  prices_monthly <- prices_daily[ xts::endpoints(prices_daily, on="months", k=1), ]
  return(prices_monthly)
}



