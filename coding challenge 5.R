rm(list=ls())
install.packages("fredr")
library(dplyr)  
library(zoo)    
library(ggplot2) 

install.packages("zoo")

install.packages("tidyverse")
library(fredr)
library(tidyverse)
?fredr_set_key()
api_key <- "api key"
fredr_set_key("api key")

series <- c (
  "USAUCSFRCONDOSMSAMID" ,
  "CSUSHPINSA" ,
  "USSTHPI"
)

get_data <- function(series_id){
  df <- fredr(series_id = series_id) |>
    mutate(d_value = c(NA, diff(value)))
  dfA <- df |>
    pivot_wider(names_from = series_id)
  colnames(dfA)[4]<- c(
    paste0("d_", series_id)
  )
  dfA
}

df1 <- get_data(series [1])
df2 <- get_data(series [2])
df3 <- get_data(series [3])
df4 <- get_data("NYSTHPI")

df <- full_join(df1,df2) |> full_join(df3) |> full_join(df4)

df |>
  mutate(d_USSTHPI = zoo::na.approx(d_USSTHPI, na.rm = FALSE)) |>
  ggplot(aes(x=date)) + 
  lims(x=c(Sys.Date() -3000, NA)) +
  geom_line(aes(y=d_USAUCSFRCONDOSMSAMID/1000),color='green') +
  geom_line(aes(y=d_CSUSHPINSA/3), color='red') +
  geom_line(aes(y=d_USSTHPI/10), color='black') +
  geom_point(aes(y=d_NYSTHPI/10), color='blue') +
  labs(y = "Housing Price Index")
