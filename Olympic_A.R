#Ayesha Ali
#ID: 29587778
#14/9/21
library(tidyverse)
library(dplyr)
library(ggplot2)
library(visdat)
library(data.table)
library(broom)
#Task 2
#Q1
oly_data <- read_csv('C:/Users/AYESHA ALI/Documents/5145/Olympics_tweets.csv')
vdim = dim(oly_data)
print(paste(c("The dimensions are: ", vdim), collapse=" "))
#Q2
oly_data %>% 
mutate(user_friends = as.integer(user_friends),
	retweet_count = as.integer(retweet_count),
	favorite_count = as.integer(favorite_count),
	user_followers = as.integer(user_followers),
	date = as.Date(date),
	user_created_at = as.Date(user_created_at),
	id = as.character(id),
	text = as.character(text),
	user_screen_name = as.character(user_screen_name),
	user_location = as.character(user_location),
	user_description = as.character(user_description),
	language = as.character(language),
	favorited = as.logical(favorited))
#Q3
dates <- oly_data %>% pull(date) 
min = min(dates, na.rm = TRUE)
max = max(dates, na.rm = TRUE)
text_min <- oly_data %>%
	select(c('text','date')) %>%
	filter(date == min)
print(text_min)
text_max <- oly_data %>%
	select(c('text','date')) %>%
	filter(date == max)
print(text_max)
#Q4
oly_data %>%
  vis_dat(warn_large_data = FALSE) +
  ggplot2::theme(legend.position = "bottom")
oly_data %>%
  vis_miss(warn_large_data = FALSE) +
  ggplot2::theme(legend.position = "bottom")
#Q5
oly_data <- select(oly_data, -retweet_count, -favorited,-favorite_count, -user_description,-language)
#Q6.1
oly_data <- mutate(oly_data,user_created_at_year = format(as.Date(user_created_at, format="%d/%m/%Y"), format = '%Y'))
#Q6.2
no_years <- oly_data %>%
	   group_by(user_created_at_year)%>%
	   summarize(n())
colnames(no_years) <- c('Year','Value')
no_years %>%
	 ggplot(aes(x = Year,y = Value ))+
	geom_bar(stat = "identity", alpha = 0.5) +
	labs(title = "Number of twitter accounts per year")
x11()
#Q6.3
avg_users <- oly_data %>% drop_na() %>%
	group_by(user_created_at_year)%>%
	summarize(avg = mean(user_followers, na.rm = TRUE))	
avg_users <- avg_users[6:16,]
avg_users %>%
	 ggplot(aes(x = user_created_at_year,y = avg))+
	geom_bar(stat = "identity", alpha = 0.5) +
	labs(title = "Avg user followers across different year")
x11()
#Q6.4
avg_fri <- oly_data %>% drop_na() %>% 
	group_by(user_created_at_year)%>%
	summarize(avg = mean(user_friends, na.rm = TRUE))
avg_fri <- avg_fri[6:16,]
avg_fri %>%
	 ggplot(aes(x = user_created_at_year,y = avg))+
	geom_bar(stat = "identity", alpha = 0.5) +
	labs(title = "Avg user friend across different year")
x11()
#Q6.5
#friends-users that a specific user follows (following)
#followers-users that follow a specific user 
#Q6.6
location <- oly_data %>% drop_na() %>% 
	group_by(user_location)%>%
	summarize(value = n())
location <- arrange(location, desc(value))
top_30 <- head(location,30)
print(top_30)
no_tweet <- sum(top_30$value)
print(no_tweet)
#Q7.1
oly_data <- mutate(oly_data,date_extracted = format(as.Date(user_created_at, format="%d/%m/%Y"), format = '%d/%m/%Y'))
#Q7.2
no_years <- oly_data %>%
	   group_by(date_extracted)%>%
	   summarize(value = n())
no_years <- arrange(no_years, value)
print(no_years[1,])
no_years %>%
	 ggplot(aes(x = date_extracted,y = value ))+
	geom_bar(stat = "identity", alpha = 0.5) +
	labs(title = "Number of tweets posted in different dates")
x11()
#Q7.3
length_text <- apply(oly_data[c('text')], 2, nchar)
cat_1 <- length(length_text[length_text >= 1 & length_text <= 40])
cat_2 <- length(length_text[length_text >= 41 & length_text <= 80])
cat_3 <- length(length_text[length_text >= 81 & length_text <= 120])
cat_4 <- length(length_text[length_text >= 121 & length_text <= 160])
cat_5 <- length(length_text[length_text >= 161 & length_text <= 200])
cat_6 <- length(length_text[length_text >= 201 & length_text <= 240])
cat_7 <- length(length_text[length_text >= 241])
text_length <- data.frame(categories = c('1-40','41-80','81-120','121-160','161-100','201-240','>= 241'),
values = c(cat_1,cat_2,cat_3,cat_4,cat_5,cat_6,cat_7))
text_length %>%
	 ggplot(aes(x = categories,y = values))+
	geom_bar(stat = "identity", alpha = 0.5) +
	labs(title = "Length of the text contained in each tweet")
x11()
#Q7.4
another <- lengths(regmatches(oly_data$text, gregexpr("@", oly_data$text)))
another_more <- length(another[another >= 3])
print(another_more)
all_another <- sum(another)
print(all_another)
#Task 3
#Q1
new_data <- read.csv('C:/Users/AYESHA ALI/Documents/5145/predictive_twitter_data.csv')
mod1 <- lm(relevanceJudge ~ ., new_data)
tidy(mod1)
glance(mod1)
coeff <- summary(mod1)$coefficients 
print(coeff)
# Installation
########################################################################################################
# Installation
# Run this to find out what version of R you have installed
R.Version()
# You want the $version.string to be at least version 4
# If not, quit RStudio and download the latest from https://cran.r-project.org/

# Once updated, run these lines
install.packages("tidyverse", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("labeling", dependencies = TRUE)
install.packages("farver", dependencies = TRUE)
install.packages("ellipsis", dependencies = TRUE)
install.packages("vctrs", dependencies = TRUE)

# After all that, try and run these two lines, a graph should appear in the bottom right corner
library(ggplot2)
ggplot(mpg) + geom_point(mapping = aes(x = displ, y=hwy, color=class))

# If you get an error because some package needs to be installed/reinstalled
# Use the install packages line, changing the package name to match what the error is complaining about
########################################################################################################



# Data frames
########################################################################################################
head(mtcars)

name = c("Jude", "Jason", "John")
employee_id = c(1, 2, 3)
department = c("Analytics", "Analytics", "HR")
staff = data.frame(name, employee_id, department)
staff

write.csv(staff, "staff.csv")
staff2 <- read.csv("staff.csv", stringsAsFactors = FALSE)
staff2 <- read.csv("staff.csv", stringsAsFactors = TRUE)

head(iris)
str(iris)
summary(iris)
plot(iris)

dim(iris)
nrow(iris)
ncol(iris)

names(iris)
iris$Sepal.Length
iris$Sepal.Length[1]
iris[1,1]
iris[1:3, c(1,5)]
iris[1:2,]

table(iris$Species)
table(iris$Species, iris$Petal.Length)
table(iris$Species, iris$Petal.Length < 2)
head(iris)
apply(iris[,1:4], 1, sum)
apply(iris[,1:4], 2, sum)

iris[1,1] <- 200
head(iris)
fix(iris)
head(iris)

# Missing values
NA
NaN
x <- c("1", "2", "cheese")
y <- as.numeric(x)
mean(y)
mean(y, na.rm = TRUE)
table(y, useNA = "always")
summary(y)
is.na(y)
table(is.na(y))
is.na("")
########################################################################################################



# Data visualisation
########################################################################################################
library(ggplot2)

head(data.frame(mpg))

ggplot(mpg)

ggplot(mpg) +
  geom_point(mapping=aes(x = displ, y = hwy))

ggplot(mpg[mpg$manufacturer=="audi",]) +
  geom_line(mapping=aes(x = displ, y = year))

ggplot(mpg[mpg$manufacturer=="audi",]) +
  geom_line(mapping=aes(x = displ, y = year)) +
  geom_point(mapping=aes(x = displ, y = year))

ggplot(mpg) +
  geom_point(mapping=aes(x = displ, y = hwy, color = class))

ggplot(mpg) +
  geom_point(mapping=aes(x = displ, y = hwy, size = cty))

ggplot(mpg) +
  geom_point(mapping=aes(x = displ, y = hwy, alpha = cty))

ggplot(mpg) +
  geom_point(mapping=aes(x = displ, y = hwy, shape = class))

ggplot(mpg) + 
  geom_point(mapping = aes(x = displ, y=hwy)) +
  facet_wrap(~class, nrow=2)

ggplot(mpg) + 
  geom_point(mapping = aes(x = displ, y=hwy)) +
  facet_grid(drv~cyl)

ggplot(mpg) + 
  geom_histogram(mapping = aes(x=displ))

ggplot(mpg) + 
  geom_histogram(mapping = aes(x=displ), binwidth=1)

ggplot(mpg) + 
  geom_histogram(mapping = aes(x=displ), bins=10)

ggplot(mpg) + 
  geom_bar(mapping=aes(x=manufacturer))

ggplot(mpg) + 
  geom_bar(mapping=aes(x=manufacturer, fill=trans))

ggplot(mpg) + 
  geom_bar(mapping=aes(x=manufacturer, fill=trans), position="fill")

ggplot(mpg) + 
  geom_bar(mapping=aes(x=manufacturer, fill=trans), position="dodge")

ggplot(mpg) + 
  geom_bar(mapping=aes(x=manufacturer, fill=drv), position="dodge") +
  scale_fill_brewer(palette = "RdPu")

ggplot(mpg) + geom_bar(mapping = aes(x = manufacturer))

ggplot(mpg) + geom_bar(mapping=aes(x=manufacturer, fill=trans), position="dodge") +
  labs(x = "Car Manufacturer", 
       y = "Number of models", 
       title = "Cars by Transmission Type",
       caption="data from fueleconomy.gov")
