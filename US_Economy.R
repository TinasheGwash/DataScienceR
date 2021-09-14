library(ggplot2)
library(dplyr)
library(scales)


# GDP data
us <- read.csv("usgdp.csv")
world <- read.csv("worldgdp.csv")

View(us)
View(world)

# Line chart with US gdp and World gdp
ggplot() + 
  geom_line(data = us, aes(x = Year, y = US, color = "blue")) +
  geom_line(data = world, aes(x = Year, y = World, color = "red")) +
  xlab('Year') +
  ylab('Rate') +
  theme_classic() +
  ggtitle("GDP Growth Rate") +
  scale_color_discrete(name = "",labels = c("U.S.", "World"))



# Unemployment Rate data
unemp <- read.csv("unemp.csv")
View(unemp)

names(unemp)[1]<-"Year"

# Unemployment line chart
ggplot() +
  geom_line(data = unemp, aes(x = Year, y = US, color = "blue")) +
  geom_line(data = unemp, aes(x = Year, y = World, color = "red")) +
  xlab('Year') +
  ylab('Rate') +
  theme_classic() +
  ggtitle("Unemployment Rate") +
  scale_color_discrete(name = "",labels = c("U.S.", "World"))



# Consumer Price Index data
cpi <- read.csv("cpi.csv")
View(cpi)

names(cpi)[1]<-"Year"


# Consumer Price Index line chart
ggplot() +
  geom_line(data = cpi, aes(x = Year, y = US, color = "blue")) +
  geom_line(data = cpi, aes(x = Year, y = World, color = "red")) +
  xlab('Year') +
  ylab('Price') +
  theme_classic() +
  ggtitle("Consumer Price Index") +
  scale_color_discrete(name = "",labels = c("U.S.", "World"))



# Labor force participation rate data
lpr <- read.csv("laborpr.csv")
View(lpr)

names(lpr)[1]<-"Year"

# LFPR line chart
ggplot() +
  geom_line(data = lpr, aes(x = Year, y = US, color = "blue")) +
  geom_line(data = lpr, aes(x = Year, y = World, color = "red")) +
  xlab('Year') +
  ylab('Rate') +
  theme_classic() +
  ggtitle("Labor Force Participation Rate (% of total population ages 15-64)") +
  scale_color_discrete(name = "",labels = c("U.S.", "World"))



# Import/Export Price Indexes (MXP) data
epi <- read.csv("epi.csv")
ipi <- read.csv("ipi.csv")
View(epi)
View(ipi)

# line chart with export and import indexes
ggplot() + 
  geom_line(data = epi, aes(x = Year, y = ExportPriceIndex, color = "Red")) +
  geom_line(data = ipi, aes(x = Year, y = ImportPriceIndex, color = "Blue")) +
  xlab('Year') +
  ylab('Price') +
  theme_classic() +
  ggtitle("US Import/Export Price Indexes (MXP)") +
  scale_color_discrete(name = "",labels = c("Import", "Export")) +
  scale_x_continuous(breaks= pretty_breaks())



library(plotly)
# Create export and import partners data set
Country <- c("Canada", "Mexico",	"China", "Japan",	"UK",	"Germany", "South Korea")
Export <- c(282242783021,	243314383193, 129893514886, 67602396570, 56243524085, 53497508196, 48326087662)
Import <- c(305878828303,	317207180158, 526022307970 ,	139797020593 , 53950438183,	119991393155,	73448658005)
data <- data.frame(Country, Export, Import)

# Grouped bar plot with export and import partners
plot_ly(data, x = ~Country, y = ~Export, type = 'bar', name = 'Export') %>%
  add_trace(y = ~Import, name = 'Import') %>%
  layout(yaxis = list(title = 'Total ($USD)'), title= 'US Import/Export Partners', barmode = 'group')
  

# #Another way by importing data through csv file
# exandim <- read.csv("ex-im.csv")
# View(exandim)

# plot_ly(exandim, x = ~Country, y = ~Export, type = 'bar', name = 'Export') %>%
# add_trace(y = ~Import, name = 'Import') %>%
# layout(yaxis = list(title = 'Total ($USD)'), barmode = 'group')



# Create export and import goodsdata set
Good <- c("Industrial Machinery","Electrical Machinery","Oil & Mineral Fuels","Aircraft","Motor Vehicles & Parts","Precision Instruments","Plastics","Precious Stones & Metals","Pharmaceuticals","Items nesoi")
Export <- c(202035662651,174424000525,139025423812,131143849163,130179115971,83637978332,61892626784,59589535207,44935704396,42436580208)
Import <- c(349027417006,356673459274,204018187723,20234676457,294555395962,86126615956,54851794483,58792694075,96585973544,91141510466)
data <- data.frame(Good, Export, Import)

# Grouped bar plot with export and import goods
plot_ly(data, x = ~Good, y = ~Export, type = 'bar', name = 'Export') %>%
  add_trace(y = ~Import, name = 'Import') %>%
  layout(yaxis = list(title = 'Total ($USD)'), title= 'US Import/Export Goods', barmode = 'group')


