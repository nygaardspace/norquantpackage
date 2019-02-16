#################NTOP FUNCTION########################
#' @export
ntop <- function
(
  data,
  topn = 1,
  dirMaxMin = TRUE
)
{
  temp = coredata(data)
  if(is.logical(temp)) temp[] = iif(!temp,NA,temp)
  if(topn == ncol(data)) {
    index = is.na(temp)
    temp[index] = 0
    temp[!index] = 1
    out = data
    out[] = ifna(temp / rowSums(temp),0)
    return( out )
  }
  index.n = rowSums(!is.na(temp))
  for( i in 1:nrow(data) ) {
    if( index.n[i] > 0 ) {
      o = sort.list(temp[i,], na.last = TRUE, decreasing = dirMaxMin)
      temp[i,] = 0
      n = min(topn, index.n[i])
      temp[i,o[1:n]] = 1/n
    } else temp[i,] = 0
  }
  out = data
  out[] = temp
  out
}
#################NTOP FUNCTION########################
#' @export
get_date <- function(date) {
  weekday <-  format(date, "%A")
  dato <-  format(date, "%Y-%m-%d")
  if(weekday == "Monday") weekday <- "mandag"
  if(weekday == "Tuesday") weekday <- "tirsdag"
  if(weekday == "Wednesday") weekday <- "onsdag"
  if(weekday == "Thursday") weekday <- "torsdag"
  if(weekday == "Friday") weekday <- "fredag"
  weekday <- paste0(weekday," (",dato,")")
  weekday
}
#################mapXts FUNCTION########################
#' @export
mapXts <- function(Xts) {
  season <- list()
  for (j in names(Xts)) {
    returns <- Xts[,j]
    returns <- na.omit(returns)
    returns <- apply(returns, 2, function(x) ifelse(is.finite(x), x, NA))
    season[[j]] <- table.CalendarReturns(returns)[-c(13)]
  }
  season
}

