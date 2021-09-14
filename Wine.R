library(ggplot2)
library(dplyr)
library(viridis)
library(hrbrthemes)


#set string values as characters
#load the greek characters
wine = read.csv("wine.csv", stringsAsFactors = FALSE, encoding = 'UTF-8')


#remove descriptive columns from dataset
wine = wine[,-c(1,3)]


#Which countries have the highest count of wines in the dataset?
topten <-wine %>% group_by(country) %>% summarize(count=n()) %>% arrange(desc(count))

View(topten)

toptencount <- data.frame(
  Country=c("US","Italy","France","Spain","Chile","Argentina","Portugal","Australia","New Zealand","Austria"),  
  Count=c(62397,23478,21098,8258,5816,5631,5322,4957,3320,3057)
)

View(toptencount)

# Barplot
ggplot(toptencount, aes(x=Country, y=Count)) + 
  geom_bar(stat = "identity",fill="blue",) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")


#What are the top 10 countries?
selected_countries = wine %>% group_by(country) %>% summarize(count=n()) %>% arrange(desc(count)) %>% top_n(10) %>% select(country)
selected_countries


#changing the format from data fram to vector as.character referencing the country
selected_countries = as.character(selected_countries$country)
class(selected_countries)


#subsetting data selecting top ten countires and their points from wine
select_points = wine %>% filter(country %in% selected_countries) %>% select(country, points) %>% arrange(country)


#What is the Distribution of the top 10 Wine producing countries?
#What countries have the highest rated wine
ggplot(select_points, aes(x = reorder(country,points,median), y = points)) +
  geom_boxplot(aes(fill = country)) +
  xlab("Country") +
  ylab("Points") +
  ggtitle("Distribution of Top 10 Countries") +
  theme(plot.title = element_text(hjust = 0.5))


#What is the best value wine?
#Use intersect to find top 15% cheapest wines with high rating
top15percent=wine %>%
  arrange(desc(points)) %>%
  filter(points > quantile(points, prob = 0.85))

cheapest15percent=wine %>%
  arrange(price) %>%
  head(nrow(top15percent))

goodvalue = intersect(top15percent,cheapest15percent)
goodvalue



