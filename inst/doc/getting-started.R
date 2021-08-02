## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE,
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("dispositionEffect")

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("marcozanotti/dispositionEffect")

## ---- eval = TRUE-------------------------------------------------------------
library(dispositionEffect)

## ---- eval = TRUE-------------------------------------------------------------
head(investor)

## ---- eval = TRUE-------------------------------------------------------------
head(marketprices)

## ---- eval = TRUE-------------------------------------------------------------
portfolio_results <- portfolio_compute(
	portfolio_transactions = investor, 
	market_prices = marketprices,
	method = "count"
)
dplyr::select(portfolio_results, -datetime)

## ---- eval = TRUE-------------------------------------------------------------
# assets' disposition effects
disposition_effect(
	realized_gains = portfolio_results$RG_count, 
	paper_gains = portfolio_results$PG_count, 
  realized_losses = portfolio_results$RL_count, 
	paper_losses = portfolio_results$PL_count
)

## ---- eval = TRUE-------------------------------------------------------------
# investor's disposition effect
disposition_effect(
	realized_gains = portfolio_results$RG_count, 
	paper_gains = portfolio_results$PG_count, 
  realized_losses = portfolio_results$RL_count, 
	paper_losses = portfolio_results$PL_count
) %>% 
	mean(na.rm = TRUE)

## ---- eval = TRUE-------------------------------------------------------------
# assets' disposition effects
asset_de <- disposition_compute(gainslosses = portfolio_results)
asset_de

## -----------------------------------------------------------------------------
# investor's disposition effect
investor_de <- disposition_compute(gainslosses = portfolio_results, aggregate_fun = mean, na.rm = TRUE)
investor_de

## ---- eval = TRUE-------------------------------------------------------------
# investor's disposition effect summary statistics
disposition_summary(gainslosses = portfolio_results)

## ---- eval = TRUE, fig.width=8, fig.height=5, fig.align='center'--------------
library(ggplot2)

asset_de %>% 
	ggplot2::ggplot(ggplot2::aes(x = asset, y = DE_count, fill = asset)) +
	ggplot2::geom_col() +
	ggplot2::scale_fill_viridis_d() +
	ggplot2::labs(
		title = "Disposition Effect results of the traded assets",
		subtitle = "Method Count",
		x = "", y = ""
	)

