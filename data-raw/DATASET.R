## code to prepare `DATASET` dataset goes here
#' Prices of 50,000 round cut diamonds.
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
#' @source \url{http://www.diamondse.info/}
"diamonds"
library(roxygen2)
# devtools::document()
library(zoo)
library(tsibble)
# data generate
data(EuStockMarkets)
tsbl <- as_tsibble(EuStockMarkets[,1])
EuStockMarkets=data.frame(EuStockMarkets)
tsbl$times=as.POSIXct(tsbl$index)
EuStockMarkets$times=as.POSIXct(as.character(tsbl$index))
EuStockMarkets=EuStockMarkets[,c('times','DAX','SMI','CAC','FTSE')]
# write.csv(EuStockMarkets,'./missImputeTS/data/EuStockMarkets.csv',row.names = F,fileEncoding = 'utf-8')
EuStockMarkets2=EuStockMarkets
usethis::use_data(EuStockMarkets2,overwrite = T)

