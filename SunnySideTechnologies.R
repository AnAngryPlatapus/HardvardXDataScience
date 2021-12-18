
library(mongolite)
library(tidyverse)
library(lubridate)
library(dplyr)

connection_string = 'mongodb://localhost:27017/champ'

bars_collection <- mongo(collection="alpacaBar", db="champ", connection_string)
bars_collection$count()
?mongo
?POSIXct
bars_df <- subset(bars_collection$find(limit= 100000000), select = c("time", "close", "volume", "ticker"))

dist_bars_df <- distinct(bars_df, time, .keep_all = TRUE) %>% group_by(ticker)
head(dist_bars_df)

head(dist_bars_df)
ind_df <- dist_bars_df %>% column_to_rownames(., var = "time")
as.POSIXlt('2017-10-19')
ind_df[as.POSIXct('2017-10-19'),]

dist_bars_df$time[1] %m+% days(1)
dist_bars_df$time[1] %m+% days(1)

percentage_change <- function (rowA, rowB) {
  duration <- paste(format(rowA$time, '%Y-%M-%D'), format(rowB$time, '%Y-%M-%D'), sep= " --> ") 
  percent_change <- rowA$close / rowB$close
  retVal <- data.frame(duration=duration, percent_change=percent_change, ticker=rowA$ticker)
  retVal
}

sapply(percentage_change(dist_bars_df[val,], dist_bars_df[(val+14),]), class) 

price_change_df = data.frame(duration=character(), percent_change=character(), ticker=character())
end <- 1000 - 21
for (val in 1:986) {
  price_change_df <- rbind(price_change_df, percentage_change(dist_bars_df[val,], dist_bars_df[(val+14),])) 
}
head(price_change_df)


mean(price_change_df$percent_change)
sd(price_change_df$percent_change)
pnorm(2, 1, .5, lower.tail=FALSE)
?pnorm
price_change_df %>%
  ggplot(aes(percent_change)) +
  geom_histogram(binwidth = 0.01, color = "black")
scale_x_continuous(breaks = seq(10, 90, by=10))
?pnorm

hist(price_change_df$percent_change)

price_change_df %>%
  ggplot(aes(percent_change)) +
  geom_boxplot()

