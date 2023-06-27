#!/usr/bin/Rscript
# Update APP
install.packages('rsconnect')
library(rsconnect)
rsconnect::setAccountInfo(name='h172284',
                          token='2C6F9B54FB985E741B7204754EE2E723',
                          secret='YeNESbbouskHtZIqBICsAUa5PfEzJiQnH1H8sqJ4')
deployApp()
