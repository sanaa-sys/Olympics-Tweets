#Ayesha Ali
#ID: 29587778
#14/9/21
library(tidyverse)
library(stringr) 
library(dplyr)
library(ggplot2)
library(naniar)
library(data.table)
#Q1
oly_data <- read_csv('C:/Users/AYESHA ALI/Documents/5145/Olympics_tweets.csv')
head(oly_data, 15)#first 15 rows
tail(oly_data, 15)#last 15 rows
#Q2
file.info('C:/Users/AYESHA ALI/Documents/5145/Olympics_tweets.csv')$size#file size
nrow(oly_data)#number of rows
#Q3
ncol(oly_data)#number of columns
spec(oly_data)#number of columns
#Q4 
filter <- oly_data %>%
	filter(!(grepl("[[:alpha:]]+",id)) & nchar(id) == 19) 
#write.csv(filter,"C:/Users/AYESHA ALI/Documents/5145/filtered_tweets_1.csv", row.names = FALSE)
#Q5
no_na <- filter %>% drop_na()
dates <- no_na %>% pull(date) 
range(as.Date(dates))
#Q6
row_jp <- filter %>%
	filter(grepl("Japan", text))
first_jp <- min(row_jp$date)
print(first_jp)
row_au <- filter %>%
	filter(grepl("Australia", text))
first_au <- min(row_au$date)
print(first_au)
#Q7
unique(filter[c('user_screen_name')])
#Q8
text <- filter$text
sum(lengths(regmatches(text, gregexpr("#", text))))
#Q9
filter_2 <- filter %>%
	select(c('id','user_screen_name','user_created_at','user_followers','user_friends','date')) %>%
	filter(user_friends > 1000 & user_followers > 1000)
write.csv(filter_2,"C:/Users/AYESHA ALI/Documents/5145/filtered_tweets_2.csv", row.names = FALSE)
#Q10
sum(is.na(filter_2$date))	
filter_2$user_created_at <- as.Date(filter_2$user_created_at , '%d/%m/%Y')
user_filter <- subset(filter_2, user_created_at < as.Date('2020-01-01'))
print(nrow(user_filter))
	