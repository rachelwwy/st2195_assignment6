install.packages(pacman)
library(dplyr)
fx_data <- read.csv("fx2.csv")
library(data.table)
speeches_data <- fread("speeches2.csv", 
                       select = c("date", "contents"), 
                       colClasses = c(date="character", contents="character"))

#convert dates to same type by POSXict
speeches_data$date <- as.POSIXct(speeches_data$date)
fx_data$DATE <- as.POSIXct(fx_data$DATE)

combined_data <- fx_data %>% left_join(speeches_data, by = join_by("DATE" == "date")) 

#removing outliers
boxplot(combined_data$US.dollar.Euro)$out

#replacing NA values with latest values
library(tidyr)
combined_data_latest <- combined_data %>% fill(US.dollar.Euro, .direction = "down" )

#removing any NA 
combined_data_latest_no_na<- combined_data_latest %>% drop_na("US.dollar.Euro")

#exchange rate return
P <- combined_data_latest_no_na$US.dollar.Euro

exchange_rate <- function(P){
  (P[i]-P[i-1])/P[i-1]
    return((P[i]-P[i-1])/P[i-1])
  }

results = c()
for (i in 1:7213){
  results[i] = c(exchange_rate(P))   
print(results)
}

#extend the original dataset with good_news and bad_news
news_df <- combined_data %>% mutate(good_news = results > 0.005, 1, 0)
all_news <- news_df %>% mutate(bad_news = results < 0.005, 1, 0)


#Remove the entries for which contents column has NA values
all_news_no_na <- all_news[(!is.na(all_news$good_news) & !is.na(all_news$bad_news)), ]

#filter good_indicators with 20 common words
install.packages("tidyverse")
library(stringi)
good_news <- all_news_no_na %>% filter(good_news == 'TRUE')
 good_indicators <- as.data.frame(good_news$contents, responseName = "Freq",stringsAsFactors = TRUE)

top20_good <- good_indicators[1:20]

#write csv file for good_indicators
write.csv(top20_good, file = "good_indicators.csv")

#filter bad_indicators with 20 common words
bad_news <- all_news_no_na %>% filter(bad_news == TRUE)
bad_indicators <- table(bad_news$contents)
top20_bad <- bad_indicators[1:20]

write.csv(top20_bad, file = "bad_indicators.csv")
                   


