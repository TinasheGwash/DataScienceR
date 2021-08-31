library(ggplot2)
library(gridExtra)


dataset <- read.csv('pseudo_facebook.tsv', sep = '\t')

names(dataset)


ggplot(data = dataset, aes (x = dob_day)) +
  geom_histogram() +
  scale_x_continuous(breaks = 1:31) +
  facet_wrap(~dob_month, ncol = 3) +
  ggtitle("Count of users by day of birth") + 
  xlab("Day of Birth") + 
  ylab("Count") +  
  theme_minimal() 


ggplot(data = dataset, aes(x = friend_count)) + 
  geom_histogram() +
  scale_x_continuous(limits = c(0, 1000)) +
  ggtitle("Friend count distribution") + 
  xlab("Friend Count") + 
  ylab("Count") + 
  theme_minimal()
  

ggplot(aes(x = friend_count), data = dataset) +
  geom_histogram(binwidth = 25) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 100)) +
  ggtitle("Friend count distribution") + 
  xlab("Friend Count") + 
  ylab("Count") + 
  theme_minimal()


ggplot(aes(x = friend_count), data = subset(dataset, !is.na(gender))) +
  geom_histogram() +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 200)) +
  facet_wrap(~gender) + 
  ggtitle("Friend count Distribution") + 
  xlab("Friend Count") + 
  ylab("Count") + 
  theme_minimal()
  

by(dataset$friend_count, dataset$gender, summary)
  

ggplot(aes(x = tenure), data = dataset) +
  geom_histogram(binwidth = 30, color = 'black', fill = '#099DD9') +
  ggtitle("Tenure Distribution") + 
  xlab("Tenure (Days)") + 
  ylab("Count") + 
  theme_minimal()


ggplot(aes(x = tenure/365), data = dataset) +
  geom_histogram(binwidth = .25, color = 'black', fill = '#F79420') +
  ggtitle("Tenure Distribution") + 
  xlab("Tenure") + 
  ylab("Count") + 
  theme_minimal()
  

ggplot(aes(x = tenure / 365), data = dataset) +
  geom_histogram(color = 'black', fill = '#F79420') +
  scale_x_continuous(breaks = seq(1, 7, 1), limits = c(0, 7)) +
  xlab('Number of years using Facebook') +
  ylab('Number of users in sample') +
  ggtitle("Tenure Distribution") + 
  theme_minimal()
  

ggplot(aes(x = age), data = dataset) +
  geom_histogram(binwidth = 1, fill = '#5760AB') +
  scale_x_continuous(breaks = seq(0, 113, 5)) +
  xlab('Age') +
  ylab('Count') +
  ggtitle("Age of Users Distribution") + 
  theme_minimal()
 
  
p1 <- ggplot(aes(x = friend_count), data = dataset) +
  geom_histogram() +
  xlab('Friend Count') +
  ylab('Count') +
  ggtitle("Friend count distribution") + 
  theme_minimal()
p2 <- p1 + scale_x_log10()
p3 <- p1 + scale_x_sqrt()
  

grid.arrange(p1, p2, p3, ncol = 1)
  

ggplot(aes(x = friend_count, y = ..count../sum(..count..)),
         data = subset(dataset, !is.na(gender))) +
  geom_freqpoly(aes(color = gender), binwidth=10) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) +
  xlab('Friend Count') +
  theme_minimal() +
  ylab('Proportion of users with that friend count')  +
  ggtitle("Proportion of friend count by gender")

  
  
  ggplot(aes(x = www_likes), data = subset(dataset, !is.na(gender))) +
    geom_freqpoly(aes(color = gender)) +
    theme_minimal() +
    scale_x_log10() +
    xlab('Likes') +
    ylab('Count') +
    ggtitle("Frequency of Likes by Gender") 
 

by(dataset$www_likes, dataset$gender, sum)
  
  
ggplot(data = subset(dataset, !is.na(gender)), aes(x = gender, y = friend_count)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 1000)) +
  xlab('Gender') +
  ylab('Friend Count') +
  ggtitle("Friend Count by Gender")


ggplot(data = subset(dataset, !is.na(gender)), aes(x = gender, y = friend_count)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 250)) +
  xlab('Gender') +
  ylab('Friend Count') +
  ggtitle("Friend Count by Gender")
  

by(dataset$friend_count, dataset$gender, summary)

  

names(dataset)

ggplot(data = subset(dataset, !is.na(gender)), aes(x = gender, y = friendships_initiated)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 500)) +
  xlab('Gender') +
  ylab('Friendships Initiated') +
  ggtitle("Friendships Initiated by Gender") +
  theme_minimal()
  
  
ggplot(data = subset(dataset, !is.na(gender)), aes(x = gender, y = friendships_initiated)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 150)) +
  xlab('Gender') +
  ylab('Friendships Initiated') +
  ggtitle("Friendships Initiated by Gender") +
  theme_minimal()
  

by(dataset$friendships_initiated, dataset$gender, summary)

  
  mobile_check_in <- NA

  dataset$mobile_check_in <- ifelse(dataset$mobile_likes > 0, 1, 0)

  dataset$mobile_check_in <- factor(dataset$mobile_check_in)

  summary(dataset$mobile_check_in)

  sum(dataset$mobile_check_in == 1)/length(dataset$mobile_check_in)
  
  
ggplot(aes(x = age, y = friend_count), data = dataset) +
  geom_point(alpha = 1/15, position = position_jitter(h = 0)) +
  xlim(13, 90) +
  coord_trans(y = 'sqrt') +
  xlab('Age') +
  ylab('Friend Count') +
  ggtitle("Friend count by age") +
  theme_minimal()
  

ggplot(data = dataset, aes(x = age, y = friendships_initiated)) +
  geom_jitter(alpha = 1/15, position = position_jitter(h = 0)) + 
  xlim(13, 90) +
  coord_trans(y = 'sqrt') +
  xlab('Age') +
  ylab('Friendship initiated') +
  ggtitle("Friendships initiated by age") +
  theme_minimal()
  
  
library(dplyr)
  
age_groups <- group_by(dataset, age)
dataset.fc_by_age <- summarise(age_groups, 
                            friend_count_mean = mean(friend_count),
                            friend_count_median = median(friend_count),
                            n = n())
  
  dataset.fc_by_age <- arrange(dataset.fc_by_age, age)
  
  head(dataset.fc_by_age)
  
  
  ggplot(data = dataset.fc_by_age, aes(x = age, y = friend_count_mean)) +
    geom_point() +
    xlab('Age') +
    ylab('Friend count mean') +
    ggtitle("Friend count mean by age") +
    theme_minimal()
   

  ggplot(data = dataset.fc_by_age, aes(x = age, y = friend_count_mean)) +
    geom_line() +
    xlab('Age') +
    ylab('Friend count mean') +
    ggtitle("Friend count mean by age") +
    theme_minimal()
  
  

  ggplot(aes(x = age, y = friend_count), data = dataset) +
    geom_point(alpha = 1/20, position = position_jitter(h = 0), color = 'orange') +
    xlim(13, 90) +
    coord_trans(y = 'sqrt') +
    geom_line(stat = 'summary', fun.y = mean) +
    geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .9), linetype = 2, color = 'blue') + 
    geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1), linetype = 2, color = 'blue') +
    geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .5), color = 'blue') +
    xlab('Age') +
    ylab('Friend count') +
    ggtitle("Friend count mean and quantiles") +
    theme_minimal()
  
  
  ggplot(aes(x = age, y = friend_count), data = dataset) +
    coord_cartesian(xlim = c(13, 70), ylim = c(0, 1000)) + 
    geom_point(alpha = 1/20, position = position_jitter(h = 0), color = 'orange') +
    geom_line(stat = 'summary', fun.y = mean) +
    geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .9), linetype = 2, color = 'blue') + 
    geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1), linetype = 2, color = 'blue') +
    geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .5), color = 'blue') +
    xlab('Age') +
    ylab('Friend count') +
    ggtitle("Friend count mean and quantiles") +
    theme_minimal()
  

  cor.test(dataset$age, dataset$friend_count, method = 'pearson')
  
  
  cor.test(dataset$www_likes_received, dataset$likes_received)
   
  
  
  
  
  
  
  
