## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE,
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)

## ---- eval = TRUE-------------------------------------------------------------
library(dispositionEffect)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

## ---- eval = TRUE-------------------------------------------------------------
portfolio_results_ts <- portfolio_compute(
	portfolio_transactions = investor, 
	market_prices = marketprices,
	time_series_DE = TRUE
)

## ---- eval=TRUE---------------------------------------------------------------
portfolio <- portfolio_results_ts$portfolio
dplyr::select(portfolio, -datetime)

## ---- eval=TRUE---------------------------------------------------------------
timeseries <- portfolio_results_ts$timeseries
head(timeseries)

## ---- eval=TRUE---------------------------------------------------------------
disposition_summary_ts(timeseries)

## ---- eval=TRUE, fig.width=8, fig.height=5, fig.align='center'----------------
timeseries %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("DE")) %>% 
  ggplot2::ggplot(ggplot2::aes(x = datetime, y = value, col = name)) +
  ggplot2::geom_line(size = 1.5) +
  ggplot2::scale_colour_viridis_d(alpha = 1) +
	ggplot2::labs(
		title = "Time Series Disposition Effect results",
		subtitle = "Method Count",
		x = "", y = ""
	) +
  ggplot2::theme(legend.position = "bottom")

## ---- eval=TRUE---------------------------------------------------------------
portfolio_results_ts_assets <- portfolio_compute(
	portfolio_transactions = investor, 
	market_prices = marketprices,
	time_series_DE = TRUE,
	assets_time_series_DE = c("ACO", "LSUG")
)

## ---- eval=TRUE---------------------------------------------------------------
timeseries_assets <- portfolio_results_ts_assets$timeseries
head(timeseries_assets)[, 2:6]
head(timeseries_assets)[, c(2:4, 7:8)]

## ---- eval=TRUE---------------------------------------------------------------
disposition_summary_ts(timeseries_assets)[, 2:6]
disposition_summary_ts(timeseries_assets)[, c(2:4, 7:8)]

## ---- eval=TRUE, fig.width=8, fig.height=5, fig.align='center'----------------
timeseries_assets %>% 
  tidyr::pivot_longer(cols = dplyr::contains("DE")) %>% 
  ggplot2::ggplot(ggplot2::aes(x = datetime, y = value, col = name)) +
  ggplot2::geom_line(size = 1.5) +
  ggplot2::scale_colour_viridis_d(alpha = 1) +
  ggplot2::facet_wrap(~ name, ncol = 2) +
	ggplot2::labs(
		title = "Assets Time Series Disposition Effect results",
		subtitle = "Method Count",
		x = "", y = ""
	) +
  ggplot2::theme(legend.position = "bottom")

## ---- eval=FALSE--------------------------------------------------------------
#  trx <- DEanalysis$transactions
#  mkt <- DEanalysis$marketprices
#  investor_id <- unique(trx$investor)
#  
#  res_list <- vector(mode = "list", length = length(investor_id))
#  for (i in seq_along(investor_id)) {
#    tmp_trx <- trx %>%
#      dplyr::filter(investor == investor_id[i])
#    tmp_res <- tryCatch(
#      dispositionEffect::portfolio_compute(
#        portfolio_transactions = tmp_trx,
#        market_prices = mkt,
#        time_series_DE = TRUE
#      ),
#      error = function(e) "Error"
#    )
#    res_list[[i]] <- tmp_res # save results
#    rm(tmp_trx, tmp_res)
#  }
#  
#  # extract time series results for each investor
#  timeseries_10_investors <- res_list %>%
#    purrr::map("timeseries")

## ---- eval=TRUE, include=FALSE------------------------------------------------
load("figures/ts_res.RData")

## ---- eval=TRUE---------------------------------------------------------------
purrr::map(timeseries_10_investors, disposition_summary_ts) %>% 
  dplyr::bind_rows() %>% 
  dplyr::filter(stat == "Mean") %>% 
  dplyr::arrange(desc(DETs_count))

## ---- eval=TRUE, fig.width=8, fig.height=5, fig.align='center'----------------
timeseries_10_investors %>% 
  dplyr::bind_rows() %>%
  tidyr::pivot_longer(cols = dplyr::contains("DE")) %>% 
  ggplot2::ggplot(ggplot2::aes(x = datetime, y = value, col = investor)) +
  ggplot2::geom_line(size = 0.75) +
  ggplot2::scale_colour_viridis_d(alpha = 0.9) +
  ggplot2::facet_wrap(~ name, nrow = 2, ncol = 1) +
	ggplot2::labs(
		title = "10 Investors Time Series Disposition Effect results",
		subtitle = "Method Count",
		x = "", y = ""
	) +
  ggplot2::theme(legend.position = "bottom")

