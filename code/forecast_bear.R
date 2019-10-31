# notes ----

# author: Sarah Power
# contact: sarah.power@alaska.gov
# date 2019

# load ----
library(tidyverse)  
#library(FNGr)
#theme_set(theme_sleek())
#source('code/functions.R')

# data ----
data <- read_csv('data/oage_bear.csv') 

#analysis
lm23 <- lm(oage_3 ~ oage_2 , data = data)
lm230 <- lm(oage_3 ~ oage_2 + 0, data = data)
summary(lm23)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(lm23) # seems reasonalbly normal.

anova(lm23, lm230) #checkig nested model


# data2 ----
#more processing
data <- data %>%
  gather(key = oage, value = fish, oage_1:oage_4)

#check missing values There should be some for the older ages since those fish haven't returned yet
data[!complete.cases(data),]

#IF only the most recent years for age classes are missing remove them.   ... other wise figure out why they are missing!
data <- na.omit(data)  

data <- data %>%
  separate(oage, c("do_delete", "oage")) %>%
  select(-do_delete) 

data$oage <- as_factor(data$oage)

(med <- data %>%
  group_by(oage) %>%
  #tail()
  summarize(med = median(tail(na.omit(fish), 10))))

med$med[1]



# data different way ----
  

#data <- read_csv('data/oage_bear.csv')




# analysis ----
(last_yr <- max(data$year, na.rm =TRUE))
(this_yr <- last_yr +1)
(next_yr <- this_yr + 1)







# eda ----

ggplot(iris, aes(sepal_length, sepal_width, color=Species)) + 
  geom_point() +
  ylab('Sepal Width') + 
  xlab('Sepal Length')