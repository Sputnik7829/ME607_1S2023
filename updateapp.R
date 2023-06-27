#!/usr/bin/Rscript
# Update APP
install.packages('rsconnect')
library(rsconnect)
setAccountInfo(name={secrets.NAME},token={secrets.TOKEN},secret={secrets.SECRET})
deployApp()
