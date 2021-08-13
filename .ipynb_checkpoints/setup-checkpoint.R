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
write.csv(EuStockMarkets,'./missImputeTS/data/EuStockMarkets.csv',row.names = F,fileEncoding = 'utf-8')
EuStockMarkets2=EuStockMarkets
usethis::use_data(EuStockMarkets2,overwrite = T)
devtools::install_github('qkdrk7777775/missImputeTS')
library(missImputeTS)
data(EuStockMarkets)

###
devtools::install_github('qkdrk7777775/missImputeTS',force=T)
library(missImputeTS)
??missImputeTS
library(ggplot2)
??diamonds
??EuStockMarkets2
# data()
data(EuStockMarkets2)
?EuStockMarkets2


data("EuStockMarkets2")
EuStockMarkets2$times=lubridate::round_date(EuStockMarkets2$times,'1d')
temp=dplyr::full_join(EuStockMarkets2,
               data.frame(
                 times=seq(
                   min(EuStockMarkets2$times),
                   max(EuStockMarkets2$times),
                   3600*24)
               )
)

temp=temp[order(temp$times),]

temp=data.frame(times=temp$times,
                sapply(temp[,-grep('times',colnames(temp))],
                       function(x){zoo::na.approx(x,rule=2)}))

for(i in 2:5){
  set.seed(i)
  temp[sample(1:nrow(temp),nrow(temp)*.1),i]=NA
}
library(missImputeTS)
o=missTS(xmis=temp,time_var_name='times')

