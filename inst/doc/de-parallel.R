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
library(foreach)
library(parallel)
library(doParallel)
library(future)
library(furrr)
library(bench)

## ---- eval=TRUE---------------------------------------------------------------
portfolio_compute_parallel <- function(portfolio_transactions, market_prices, plan = NULL, ...) {

	investors_id <- purrr::map_chr(portfolio_transactions, ~purrr::pluck(., "investor")[1])
	portfolio_compute_safe <- purrr::safely(portfolio_compute)

	if (is.null(plan)) {
	  ncores <- future::availableCores()
	  # if there are more than 2 cores than use parallel computing
	  # otherwise use sequential computing
	  # RULE: be polite, always leave at least 1 free core
	  if ((ncores - 1) > 1) {
		  new_plan <- "multisession" # since I am testing on Windows
	  } else {
		  new_plan <- "sequential"
	  }
	} else {
	  new_plan <- plan
	}
	
	old_plan <- future::plan(strategy = new_plan)

	res <- furrr::future_map(
		portfolio_transactions,
		portfolio_compute_safe,
		market_prices,
		...)

	res <- purrr::transpose(res)$result
	names(res) <- investors_id

	future::plan(old_plan) # set back the old plan

	return(res)

}

## ---- eval=TRUE---------------------------------------------------------------
trx <- DEanalysis$transactions %>% 
  dplyr::group_by(investor) %>% 
  dplyr::group_split()
mkt <- DEanalysis$marketprices

## ---- eval=FALSE--------------------------------------------------------------
#  res <- bench::mark(
#    "sequential" = portfolio_compute_parallel(trx, mkt, plan = "sequential"),
#    "parallel" = portfolio_compute_parallel(trx, mkt, plan = "multisession")
#  )
#  
#  res$expression <- c("sequential", "parallel")
#  res[, 1:8]

## ---- echo=FALSE, fig.align='center'------------------------------------------
knitr::include_graphics("figures/de-parallel-bench1.PNG")

## ---- eval=FALSE--------------------------------------------------------------
#  cl <- parallel::makeCluster(parallel::detectCores())
#  doParallel::registerDoParallel(cl)
#  foreach::foreach(i = seq_along(trx), .errorhandling = "pass") %dopar% {
#  
#    transactions <- dplyr::arrange(trx[[i]], datetime)
#    mrkt_prices <- dplyr::filter(
#      mkt,
#      asset %in% unique(transactions$asset) &
#        datetime <= max(transactions$datetime)
#    )
#    df <- dispositionEffect:::generate_data(transactions, mrkt_prices, subset = TRUE)
#    nm <- paste0("INV", i, ".RData")
#    save(df, file = nm)
#  
#  }
#  parallel::stopCluster(cl)

## ---- eval=FALSE--------------------------------------------------------------
#  portfolio_compute_onfiles <- function(files, plan = "sequential") {
#  
#    if (plan == "sequential") {
#  
#      res_list <- vector(mode = "list", length = length(files))
#      for (i in seq_along(files)) {
#        load(files[i]) # load the file
#        tmp_res <- tryCatch(
#          dispositionEffect::portfolio_compute(
#            portfolio_transactions = df$transactions,
#            market_prices = df$marketprices
#          ),
#          error = function(e) "Error"
#        )
#        res_list[[i]] <- tmp_res # save results
#        rm(df, tmp_res)
#      }
#  
#    } else {
#  
#      cl <- parallel::makeCluster(parallel::detectCores())
#      doParallel::registerDoParallel(cl)
#      res_list <-
#        foreach(i = seq_along(files), .errorhandling = "pass") %dopar% {
#          load(files[i]) # load the file
#          tmp_res <- tryCatch(
#            dispositionEffect::portfolio_compute(
#              portfolio_transactions = df$transactions,
#              market_prices = df$marketprices
#            ),
#            error = function(e) "Error"
#          )
#        }
#      parallel::stopCluster(cl)
#  
#    }
#  
#    return(res_list)
#  
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  files <- list.files(pattern = ".RData") # list all single .RData
#  
#  res <- bench::mark(
#    portfolio_compute_onfiles(files, plan = "sequential"),
#    portfolio_compute_onfiles(files, plan = "multisession")
#  )
#  
#  res$expression <- c("sequential", "parallel")
#  res[, 1:8]

## ---- echo=FALSE, fig.align='center'------------------------------------------
knitr::include_graphics("figures/de-parallel-bench2.PNG")

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
# system('systeminfo')
version

