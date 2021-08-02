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
library(lubridate)
library(skimr)
library(ggplot2)
library(ggridges)
library(gghalves)

## ---- eval=FALSE--------------------------------------------------------------
#  help("DEanalysis")

## ---- eval=TRUE---------------------------------------------------------------
trx <- DEanalysis$transactions # transactions
head(trx)

## ---- eval=TRUE---------------------------------------------------------------
skimr::skim(trx)

## ---- eval=TRUE---------------------------------------------------------------
unique(trx$type)

## ---- eval=TRUE---------------------------------------------------------------
# number of transactions of each investor over years
trx %>% 
  dplyr::mutate(year = lubridate::year(datetime)) %>% 
  dplyr::count(investor, year) %>% 
  dplyr::arrange(year) %>% 
  tidyr::pivot_wider(names_from = year, values_from = n) %>% 
  dplyr::left_join(dplyr::count(trx, investor), by = "investor")

## ---- eval=TRUE---------------------------------------------------------------
mkt <- DEanalysis$marketprices # market prices
head(mkt)

## ---- eval=TRUE---------------------------------------------------------------
skimr::skim(mkt)

## ---- eval=TRUE---------------------------------------------------------------
mkt %>% 
  dplyr::mutate(year = lubridate::year(datetime)) %>% 
  dplyr::count(asset, year) %>% 
  dplyr::arrange(year) %>% 
  tidyr::pivot_wider(names_from = year, values_from = n) %>% 
  head(10)

## ---- eval=TRUE---------------------------------------------------------------
# Investor QZ621
trx_QZ621 <- dplyr::filter(trx, investor == "QZ621") # transactions
mkt_QZ621 <- dplyr::filter(mkt, asset %in% unique(trx_QZ621$asset)) # market prices

## ---- eval=TRUE---------------------------------------------------------------
p_res <- portfolio_compute(portfolio_transactions = trx_QZ621, market_prices = mkt_QZ621)
head(p_res)[, -5]
skimr::skim(p_res)

## ---- eval=TRUE---------------------------------------------------------------
de <- disposition_compute(gainslosses = p_res)
head(de)

## ---- eval=TRUE---------------------------------------------------------------
disposition_compute(gainslosses = p_res, aggregate_fun = mean, na.rm = TRUE)

## ---- eval=TRUE---------------------------------------------------------------
de_stat <- disposition_summary(gainslosses = p_res)
de_stat

## ---- eval=TRUE---------------------------------------------------------------
portfolio_compute(portfolio_transactions = trx_QZ621, market_prices = mkt_QZ621, method = "none") %>% 
  head()

## ---- eval=TRUE---------------------------------------------------------------
portfolio_compute(portfolio_transactions = trx_QZ621, market_prices = mkt_QZ621, method = "count") %>% 
  head()
portfolio_compute(portfolio_transactions = trx_QZ621, market_prices = mkt_QZ621, method = "total") %>% 
  head()
portfolio_compute(portfolio_transactions = trx_QZ621, market_prices = mkt_QZ621, method = "value") %>% 
  head()
portfolio_compute(portfolio_transactions = trx_QZ621, market_prices = mkt_QZ621, method = "duration") %>% 
  head()

## ---- eval=TRUE---------------------------------------------------------------
p_res_all <- portfolio_compute(portfolio_transactions = trx_QZ621, market_prices = mkt_QZ621, method = "all")
skimr::skim(p_res_all)

## ---- eval=TRUE---------------------------------------------------------------
disposition_compute(gainslosses = p_res_all, aggregate_fun = mean, na.rm = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  portfolio_compute(
#    portfolio_transactions = trx_QZ621,
#    market_prices = mkt_QZ621,
#    time_threshold = "60 mins"
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  portfolio_compute(
#    portfolio_transactions = trx_QZ621,
#    market_prices = mkt_QZ621,
#    exact_market_prices = FALSE
#  )

## ---- eval = FALSE------------------------------------------------------------
#  portfolio_compute(
#    portfolio_transactions = trx_QZ621,
#    market_prices = mkt_QZ621,
#    verbose = c(1, 1),
#    progress = TRUE
#  )

## ---- eval=TRUE---------------------------------------------------------------
# list of transactions separated by investor
trx_list <- trx %>% 
  dplyr::group_by(investor) %>% 
  dplyr::group_split()

## ---- eval=FALSE--------------------------------------------------------------
#  p_res_full <- purrr::map(trx_list, portfolio_compute, market_prices = mkt)

## ---- eval=TRUE, include=FALSE------------------------------------------------
load("figures/p_res_full.RData")

## ---- eval=TRUE---------------------------------------------------------------
de <- purrr::map(p_res_full, disposition_compute) %>% 
  dplyr::bind_rows()
skimr::skim(de)

## ---- eval=TRUE---------------------------------------------------------------
de_mean <- purrr::map(p_res_full, disposition_compute, aggregate_fun = mean, na.rm = TRUE) %>% 
  dplyr::bind_rows() %>% 
  dplyr::arrange(dplyr::desc(DE_count))
de_mean

## ---- eval=TRUE---------------------------------------------------------------
de_stat <- purrr::map(p_res_full, disposition_summary) %>% 
  dplyr::bind_rows()
head(de_stat, 7)

## ---- eval=TRUE, fig.width=8, fig.height=5, fig.align='center'----------------
ggplot(de, aes(x = DE_count)) +
	geom_histogram(aes(y = ..density..), color = "darkblue", fill = "yellow") +
  geom_density(aes(y = ..density..), color = "darkblue", fill = "yellow", alpha = 0.4) +
	scale_x_continuous(limits = c(-1, 1)) +
	theme(
		panel.background = element_rect(fill = "grey92"),
		plot.background = element_rect(fill = "grey85", colour = NA),
		plot.title = element_text(size = 20),
		legend.position = "none"
	) +
	labs(title = "Disposition Effect Distribution", x = "Disposition Effect", y = "Frequency")

## ---- eval=TRUE, include=FALSE------------------------------------------------
de_stat <- de_stat %>%
  dplyr::filter(stat != "StDev") %>% 
  dplyr::mutate(stat = factor(stat, levels = c("Min", "Q1", "Median", "Mean", "Q3", "Max")))

## ---- eval=TRUE, fig.width=8, fig.height=5, fig.align='center'----------------
ggplot(de_stat, aes(x = DE_count, y = stat, fill = stat)) +
	geom_density_ridges() +
  scale_fill_viridis_d() +
	scale_x_continuous(limits = c(-1, 1)) +
	theme(
		panel.background = element_rect(fill = "grey92"),
		plot.background = element_rect(fill = "grey85", colour = NA),
		plot.title = element_text(size = 20),
		legend.position = "none"
	) +
	labs(
		title = "Disposition Effect Statistics' Distributions",
		x = "Disposition Effect", y = ""
	)

## ---- eval=TRUE, fig.width=8, fig.height=5, fig.align='center'----------------
top5_assets <- trx %>% 
  dplyr::count(asset) %>% 
  dplyr::arrange(dplyr::desc(n)) %>% 
  dplyr::slice(1:6) %>% 
  dplyr::pull(asset)

dplyr::filter(de, asset %in% top5_assets) %>% 
  ggplot(aes(x = asset, y = DE_count, fill = asset)) +
	geom_half_boxplot(center = TRUE, width = 0.8, nudge = 0.02) +
	geom_half_violin(side = "r", nudge = 0.02, alpha = 0.8) +
	geom_jitter(color = "grey40") +
  scale_fill_viridis_d() +
	theme(
		panel.background = element_rect(fill = "grey92"),
		plot.background = element_rect(fill = "grey85", colour = NA),
		plot.title = element_text(size = 20),
		legend.position = "none"
	) +
	labs(title = "Volatility & Disposition Effect", x = "",	y = "Disposition Effect")

