#!/usr/bin/Rscript
# Update APP
install.packages('rsconnect')
install.packages("commonmark")
install.packages("httpuv") 
install.packages("kableExtra")
install.packages("later")
install.packages("promises")
install.packages("shiny")
install.packages("shinythemes")
install.packages("sourcetools")
install.packages("svglite")
install.packages("web")

library(rsconnect)
rsconnect::setAccountInfo(name='h172284',
                          token='2C6F9B54FB985E741B7204754EE2E723',
                          secret='YeNESbbouskHtZIqBICsAUa5PfEzJiQnH1H8sqJ4')
deployApp()
