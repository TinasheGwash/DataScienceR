library(ggplot2)

data(diamonds) 


View(diamonds)
names(diamonds)
summary(diamonds)


ggplot(data = diamonds, aes(x = price)) + 
  geom_histogram() +
  ggtitle("Diamond price distribution") + 
  xlab("Price") + 
  ylab("Count") + 
  theme_minimal() 
  

sum(diamonds$price < 500)

sum(diamonds$price < 250)

sum(diamonds$price >= 15000)

ggplot(data=diamonds) + 
  geom_histogram(binwidth=50, aes(x=diamonds$price)) + 
  ggtitle("Diamond price distribution") + 
  xlab("Diamond Price") + 
  ylab("Frequency") + 
  theme_minimal() + 
  xlim(0,2500)


ggplot(data=diamonds) + 
  ggtitle("Diamond price distribution by cut") + 
  xlab("Diamond Price") + 
  ylab("Frequency") + 
  theme_minimal() +
  geom_histogram(binwidth=100, aes(x=diamonds$price)) + 
  facet_wrap(~cut)


subset(diamonds, price == max(price))

subset(diamonds, price == min(price))


a = diamonds[which(diamonds$cut == "Fair"),]
b = diamonds[which(diamonds$cut == "Good"),]
c = diamonds[which(diamonds$cut == "Very Good"),]
d = diamonds[which(diamonds$cut == "Premium"),]
e = diamonds[which(diamonds$cut == "Ideal"),]

median(a$price)
median(b$price)
median(c$price)
median(d$price)
median(e$price)


ggplot(data=diamonds) + 
  geom_histogram(binwidth=50, aes(x=diamonds$price/diamonds$carat)) + 
  ggtitle("Diamond price per carat distribution by cut") + 
  xlab("Diamond Price per Carat") + 
  ylab("Frequency") + theme_minimal() + 
  facet_wrap(~cut)


ggplot(diamonds, aes(factor(cut), price, fill=cut)) + 
  geom_boxplot() + ggtitle("Diamond price by cut") + 
  xlab("Type of Cut") + 
  ylab("Diamond Price") + 
  coord_cartesian(ylim=c(0,7500))


ggplot(diamonds, aes(factor(clarity), price, fill=clarity)) + 
  geom_boxplot() + ggtitle("Diamond price by clarity") + 
  xlab("Type of Clarity") + 
  ylab("Diamond Price") + 
  coord_cartesian(ylim=c(0,7500))


ggplot(diamonds, aes(factor(color), price, fill=color)) + 
  geom_boxplot() + ggtitle("Diamond price by color") + 
  xlab("Type of Color") + 
  ylab("Diamond Price") + 
  coord_cartesian(ylim=c(0,8000))


ggplot(data=diamonds, aes(x=carat)) + 
  geom_freqpoly() + 
  ggtitle("Diamond frequency by carat") + 
  xlab("Carat Size") + 
  ylab("Count")



ggplot(aes(x = price, color = cut), data = diamonds) +
  facet_wrap(~color, ncol = 3) + 
  geom_histogram() + 
  scale_x_log10() + 
  scale_fill_brewer(type = 'qual') +
  ggtitle("Diamond price distribution by cut")


ggplot(data = diamonds, aes(x = table, y = price, color = cut)) + 
  geom_point(alpha = 1/5) + 
  scale_x_continuous(limits = c(50, 80), breaks = seq(50, 80, 2)) + 
  xlab("Table Range") + 
  ylab("Price")


diamonds$volume <- with(diamonds, x * y * z)

ggplot(data = diamonds, aes(x = volume, y = price, color = clarity)) +
  geom_point() + 
  scale_color_brewer(type = 'div') + 
  scale_y_log10() + 
  scale_x_continuous(limits = c(0, quantile(diamonds$volume, 0.99)))  + 
  ggtitle("Diamond price by volume grouped by clarity") + 
  xlab("Volume") + 
  ylab("Price")







