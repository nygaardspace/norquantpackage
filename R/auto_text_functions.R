#################NTOP FUNCTION########################
#' @export
comment_index <- function
(
  return,
  index,
  weekday
)
{
  if(return > 2) return( paste0("Kraftig oppgang ",index," ",weekday ) )
  if(return > 1) return( paste0("Bra stigning ",index," ",weekday ) )
  if(return > 0.2) return( paste0("Positiv utvikling ",index," ",weekday ) )
  if(-0.2 < return & return < 0.2) return( paste0("Cirka uendret ",index," ",weekday ) )
  if(return < -0.2) return( paste0("Negativ utvikling ",index," ",weekday ) )
  if(return < -1) return( paste0("Negativ dag ",index," ",weekday ) )
  if(return < -2) return( paste0("Kraftig fall ",index," ",weekday ) )
}

#################NTOP FUNCTION########################
#' @export
replace_char <- function
(
  string
)
{
  string <- gsub("_", " ", string)
  string <- gsub("a-", "å", string)
  string <- gsub("o-", "ø", string)
  return(string)
}
