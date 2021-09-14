library(ggplot2)

bankdata <- read.csv("bank-full.csv", sep = ";")

str(bankdata)

summary(bankdata)

head(bankdata)


sample_dataset <- sample(1:nrow(bankdata), 20, replace = FALSE)
sample_dataset
bankdata[sample_dataset,]


str(bankdata$job)

levels(bankdata$job)

table(bankdata$job)

plot(table(bankdata$job))


hist(bankdata$age, col = "light blue", main="Frequency by Age",
     xlab="Age", ylab="Frequency") 


hist(bankdata$age, col = "light blue", freq = FALSE, main="Density by Age",
     xlab="Age", ylab="Density") 


ggplot(data = bankdata, aes(x = age),) +
  geom_histogram( fill = "light blue", color = "black") +
  labs(x = "Age", y = "Frequency", title = "Frequency by Age") + 
  theme_minimal()


bankdata$salary = rnorm(nrow(bankdata), mean = 25000, sd = 10000)


hist(bankdata$salary, col = "Red", main="Frequency by Salary",
     xlab="Salary", ylab="Frequency")


hist(bankdata$salary, col = "Red", freq = FALSE, main="Density by Salary",
     xlab="Salary", ylab="Density")

yeardata = ifelse(bankdata$month %in% c("jan", "feb", "mar", "apr", "may", "jun"), yes = "1st Half",no = "2nd Half")
bankdata$yeardata = yeardata


tabEDMar = table(bankdata$education, bankdata$marital)
tabEDMar


round(prop.table(tabEDMar)*100,3)

round(prop.table(tabEDMar,1)*100,3)

round(prop.table(tabEDMar,2)*100,3)


mosaicplot(tabEDMar)


ggplot(data = bankdata, aes(x = education, y = salary)) +
  geom_boxplot() +
  facet_wrap(~ marital, scales = "free_y") +
  labs(x = "Education", y = "Salary", title = "Salary by Education Levels") +
  theme(axis.text.x=element_text(angle=90)) 


ggplot(bankdata, aes(x = age, y = salary, color = y)) +
  geom_point() +
  facet_wrap(~ job, scales = "free_y") + 
  labs(x = "Age", y = "Salary", title = "Salary by Age Group") + 
  theme(axis.text.x=element_text(angle=90))


ggplot(bankdata, aes(x = age, y = salary, color = y)) +
  geom_point() +
  geom_smooth(method=lm) +
  geom_smooth(method=lm, aes(group = 1)) +
  facet_wrap(~ job, scales = "free_y") + 
  labs(x = "Age", y = "Salary", title = "Salary by Age Group") + 
  theme(axis.text.x=element_text(angle=90)) 


ggplot(bankdata, aes(x = job, fill = marital)) + 
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set2") + 
  labs(x = "Job", title = "Frequency Distribution") + 
  theme(axis.text.x=element_text(angle=90)) 
  

ggplot(bankdata, aes(x = job, fill = marital)) + 
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set2") + 
  facet_wrap(~ education, scales = "free_y") +
  labs(x = "Job", title = "Frequency Distribution") + 
  theme(axis.text.x=element_text(angle=90)) 












