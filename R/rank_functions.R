#################MOMENTUM FUNCTION########################
#' Momentum calculation.
#' F.e. Lag1 = 0 means no lag, so uses latest price
#' Lag2 is 12 means
#' @export
momentum <- function(prices, lag1, lag2) {
  momentum12 = ( (xts::lag.xts(prices, lag2) -  xts::lag.xts(prices, lag1)) )/ xts::lag.xts(prices, lag1)
  last12 <- as.vector(momentum12[nrow(momentum12),])
  last12[is.na(last12)] <- median(na.omit(last12))
  return(last12)
}

#################REVERSAL FUNCTION########################
#' @export
reversal <- function(returns, no_month) {
  last_revn <- as.vector(returns[nrow(returns) - no_month ])
  last_revn[is.na(last_revn)] <- median(na.omit(last_revn))
  return(last_revn)
}

#################SEASONALITY#############################
#' @export
seasonality <- function(returns, add_month) {
  nrmonths <- tibble(Code=names(returns), nrmonths = apply(returns, 2, function(x) length(na.omit(zoo::coredata(x)))))
  date_close <- zoo::index(xts::last(returns))
  last_month <- ifelse((as.numeric(format(date_close,"%m")) +add_month) == 12, 12,  (as.numeric(format(date_close,"%m")) +add_month) %% 12)
  season_month <- c("01","02","03","04","05","06","07","08","09","10","11","12")
  seasonality_return = tibble(Code=names(returns),return = colMeans(zoo::coredata(returns[format(zoo::index(returns),"%m") == season_month[last_month],]), na.rm = TRUE)  )
  seasonality <- left_join(nrmonths, seasonality_return, by="Code")
  mean_seasonality <- median(seasonality$return, na.rm = TRUE)
  seasonality <- seasonality %>% dplyr::rowwise() %>% dplyr::mutate(return = ifelse(nrmonths < 50, mean_seasonality, return) )
  return(seasonality)
}

#################STDEV########################
#' @export
stdev <- function(returns, days) {
  st.dev <- apply(tail(returns,days), 2,sd)
  st.dev <- round(100*sqrt(252)*st.dev, 1)
  st.dev[is.na(st.dev)] <- median(st.dev, na.rm = TRUE)
  return(st.dev)
}

#################MFI########################
#' @export
my_mfi <- function(df_all, lookback) {
  no_row <- length(unique(df_all$symbol))
  dummy <- rep(NA, no_row)
  for(i in 1:no_row){
    ticker <- unique(df_all$symbol)[i]
    temp_ohlc <- df_all %>% filter(symbol == ticker) %>% top_n(22,date)
    #temp_ohlc <- na.omit(temp_ohlc)
    temp_ohlc_xts <- temp_ohlc[,c(-7)] %>% timetk::tk_xts(select=-date, date_var = date)
    names(temp_ohlc_xts) <- c("High","Close","Low","Open","Volume")
    ifelse(length(na.omit(temp_ohlc_xts))==0,mfi <- NA, mfi <- MFI(temp_ohlc_xts[,c("High","Low","Close")], temp_ohlc_xts[,"Volume"], 21))
    dummy[i] <- last(mfi)
  }
  mean_mfi <- mean(dummy, na.rm = TRUE)
  dummy[is.na(dummy)] <- mean_mfi

  return(dummy)
}

#################RESIDUAL########################
#' @export
residual <- function(master_tibble, dates_residual, benchmark_ticker, riskfree) {
  print(master_tibble)
  Ra <- master_tibble %>% dplyr::group_by(symbol) %>% dplyr::select(symbol, data) %>% unnest()
  Rb <- Ra  %>%  dplyr::filter(symbol == benchmark_ticker) %>% ungroup() %>% select(date,close)
  RaRb <- dplyr::left_join(Ra, Rb, by = c("date" = "date"))
  colnames(RaRb) <- c("symbol","date","Ra","Rb")
  print(master_tibble)
  RaRb <- na.omit(RaRb)
  #############Merge indexes and align dates#################
  #master_tibble <-  master_tibble %>% dplyr::group_by(symbol)  %>% tidyr::nest()
  #print(master_tibble)
  for(i in 1:12){

    RaRb1 <- RaRb %>% dplyr::filter(date >= dates_residual[48 - 36 - (i-1)], date <= dates_residual[49 - i])
    print(paste("Month: ", i))
    print2(subset(RaRb1,  symbol == benchmark_ticker))
    RaRb1 <- RaRb1 %>% dplyr::group_by(symbol) %>% dplyr::filter(n() > 15)

    RaRb_capm <- RaRb1 %>% tidyquant::tq_performance(Ra = Ra, Rb = Rb, Rf = riskfree, performance_fun = table.CAPM)

    RaRb_camp_ab <- RaRb_capm %>% dplyr::select(symbol,Alpha, Beta)
    extra_symbols <- setdiff(master_tibble$symbol, RaRb_camp_ab$symbol)
    extra_symbols <- tibble::tibble(symbol = extra_symbols, Alpha = NA_real_, Beta = NA_real_)
    RaRb_capm <- dplyr::bind_rows(RaRb_camp_ab, extra_symbols)
    RaRb_capm <- RaRb_capm %>% dplyr::arrange(symbol)
    alfa_col_name <- paste0("alfa", i)
    beta_col_name <- paste0("beta", i)

    RaRb_capm$Alpha[is.na(RaRb_capm$Alpha)] <- median(RaRb_capm$Alpha, na.rm = TRUE)
    RaRb_capm$Beta[is.na(RaRb_capm$Beta)] <- median(RaRb_capm$Beta, na.rm = TRUE)
    master_tibble[alfa_col_name] <- RaRb_capm$Alpha
    master_tibble[beta_col_name] <- RaRb_capm$Beta

  }
  print(master_tibble)
  master_tibble <- master_tibble %>% select(-data)
  print(master_tibble)
  #RESIDUALS
  master_tibble <- master_tibble %>% dplyr::rowwise() %>% dplyr::mutate(res1 = ret1m_rf - alfa1 - beta1*ret1b_rf)
  master_tibble <- master_tibble %>% dplyr::rowwise() %>% dplyr::mutate(res2 = ret2m_rf - alfa2 - beta2*ret2b_rf)
  master_tibble <- master_tibble %>% dplyr::rowwise() %>% dplyr::mutate(res3 = ret3m_rf - alfa3 - beta3*ret3b_rf)
  master_tibble <- master_tibble %>% dplyr::rowwise() %>% dplyr::mutate(res4 = ret4m_rf - alfa4 - beta4*ret4b_rf)
  master_tibble <- master_tibble %>% dplyr::rowwise() %>% dplyr::mutate(res5 = ret5m_rf - alfa5 - beta5*ret5b_rf)
  master_tibble <- master_tibble %>% dplyr::rowwise() %>% dplyr::mutate(res6 = ret6m_rf - alfa6 - beta6*ret6b_rf)
  master_tibble <- master_tibble %>% dplyr::rowwise() %>% dplyr::mutate(res7 = ret7m_rf - alfa7 - beta7*ret7b_rf)
  master_tibble <- master_tibble %>% dplyr::rowwise() %>% dplyr::mutate(res8 = ret8m_rf - alfa8 - beta8*ret8b_rf)
  master_tibble <- master_tibble %>% dplyr::rowwise() %>% dplyr::mutate(res9 = ret9m_rf - alfa9 - beta9*ret9b_rf)
  master_tibble <- master_tibble %>% dplyr::rowwise() %>% dplyr::mutate(res10 = ret10m_rf - alfa10 - beta10*ret10b_rf)
  master_tibble <- master_tibble %>% dplyr::rowwise() %>% dplyr::mutate(res11 = ret11m_rf - alfa11 - beta11*ret11b_rf)
  master_tibble <- master_tibble %>% dplyr::rowwise() %>% dplyr::mutate(res12 = ret12m_rf - alfa12 - beta12*ret12b_rf)

  ###RESIDUAL MOMENTUM
  master_tibble <- master_tibble %>% dplyr::rowwise() %>% dplyr::mutate(res_mom12_2 = (res3 + res4 + res5 + res6 + res7 + res8 + res9 + res10 + res11 + res12) / sd(c(res3,res4,res5,res6,res7,res8,res9,res10,res11,res12)))
  master_tibble <- master_tibble %>% dplyr::rowwise() %>% dplyr::mutate(res_mom12_1 = (res2 + res3 + res4 + res5 + res6 + res7 + res8 + res9 + res10 + res11 + res12) / sd(c(res2,res3,res4,res5,res6,res7,res8,res9,res10,res11,res12)))

  ###RESIDUAL REVERSAL
  master_tibble <- master_tibble %>% dplyr::rowwise() %>% dplyr::mutate(res_rev1 = res1)
  master_tibble <- master_tibble %>% dplyr::rowwise() %>% dplyr::mutate(res_rev2 = res1 + res2)
  return(master_tibble)
}

#################NORQUANT FAKTOR########################
#' @export
nquant <- function(master_tibble) {
  norq <- master_tibble %>% dplyr::rowwise() %>% dplyr::mutate(Norq = mean(c(rev1, -mom12_1, -season, stdev21)))
  return(norq)
}

#################RANK FUNCTION########################
#' Ranks all factors where rank 1 is best.
#' Columns to rank are: alfa,beta,ymcap,retm_rf,retb_rf,season,rev,res,res_mom12_2,res_mom12_1,res_rev1,res_rev2
#' @export
rank_norquant <- function(master_tibble) {
  master_tibble$mom12_2_faktor <- master_tibble$mom12_2
  master_tibble$rev1_faktor <- master_tibble$rev1
  master_tibble$season_faktor <- master_tibble$season
  master_tibble$stdev251_faktor <- master_tibble$stdev251

  master_tibble$res_mom12_2 <- rank(-master_tibble$res_mom12_2)
  master_tibble$res_mom12_1 <- rank(-master_tibble$res_mom12_1)
  master_tibble$res_rev1 <- rank(master_tibble$res_rev1)
  master_tibble$res_rev2 <- rank(master_tibble$res_rev2)
  master_tibble$beta <- rank(master_tibble$beta1)
  master_tibble$season <- rank(-master_tibble$season)
  master_tibble$rev1 <- rank(master_tibble$rev1)
  master_tibble$rev2 <- rank(master_tibble$rev2)
  master_tibble$mom12_1 <- rank(-master_tibble$mom12_1)
  master_tibble$mom12_2 <- rank(-master_tibble$mom12_2)
  master_tibble$stdev21 <- rank(master_tibble$stdev21)
  master_tibble$stdev42 <- rank(master_tibble$stdev42)
  master_tibble$stdev63 <- rank(master_tibble$stdev63)
  master_tibble$stdev251 <- rank(master_tibble$stdev251)
  master_tibble$Norq <- rank(master_tibble$Norq)
  master_tibble$ymcap <- rank(master_tibble$ymcap)
  return(master_tibble)
}

