# notes ----
# Bear Lake Late forecast
# author: Sarah Power
# contact: sarah.power@alaska.gov
# date 2019

# load ----
library(tidyverse)  
library(ggrepel)# use in graphing
library(ggpubr)# use in graphing
library(grid)# use in graphing
library(broom)#for cleaning up data, used in predction
library(caret)#used for cross validation 
library(Metrics)#used for cross validation

options(scipen = 999)
set.seed(100) # for reproducible results

# data ----
#Note oage = ocean age = years in the ocean.

data <- read_csv('data/oage_bear20.csv') %>%
  filter(year > 1991) #%>%
  #mutate(ln_oage_3 = log(oage_3))


# Check for complete data, Should only include NAs for most recent years
data[!complete.cases(data),]
tail(data,13)

(last_yr <- max(data$year, na.rm =TRUE))
(this_yr <- last_yr +1)
(next_yr <- this_yr + 1)

# check this particularly that this_yr-2 is capturing the right data point once
# all the data is entered
new_data <- data %>%
  filter(year == this_yr-2) %>%
  select(oage_2)

# more processing data long
data_l <- data %>%
  gather(key = oage, value = fish, c(oage_1:oage_4)) #, ln_oage_3))

# check missing values There should be some for the older ages since those fish
# haven't returned yet
data_l[!complete.cases(data_l),]

#IF only the most recent years for age classes are missing remove them.   ...
#other wise figure out why they are missing!
data_l <- na.omit(data_l)  

data_l <- data_l %>%
  separate(oage, c("do_delete", "oage")) %>%
  select(-do_delete) 

data_l$oage <- as_factor(data_l$oage)

# analysis ----

# For the ocean age 1, 2 and 4 fish. The expected return is the median value
# from the past ten years.
(quant <- data_l %>%
   group_by(oage) %>%
   na.omit(fish) %>%
   summarize(lwr90 = quantile(tail(fish, 10), c(0.10)), 
             est = median(tail(fish, 10)),
             upr90 = quantile(tail(fish, 10), c(.90))))

#mape <- function(actual, predicted){ # this is now done using library(Metrics).
#  mean(abs((actual - predicted)/actual))
#}
#mae(actual, predicted)

# For transformed data
my_exp_summary <- function(data, lev = NULL, model = NULL) {
  c(#RMSE = sqrt(mean((expm1(data$obs) - expm1(data$pred)^2))), # root mean square error
    Rsquared = summary(lm(pred ~ obs, data))$r.squared, # coefficient of variation
    MAE = mae(expm1(data$obs), expm1(data$pred)), # average difference
    MAPEEXP = Metrics::mape(expm1(data$obs), expm1(data$pred))) # mean absolute perc error
}

my_summary <- function(data, lev = NULL, model = NULL) {
  c(#RMSE = sqrt(mean((data$obs -data$pred)^2)),
    Rsquared = summary(lm(pred ~ obs, data))$r.squared,
    MAE = mae((data$obs), (data$pred)),
    MAPE = mape((data$obs), (data$pred)))
}

#model 1 ----
#This is a linear model where ocean age 3 returns are predicted based on ocean age 2 returns
lm32 <- lm(oage_3 ~ oage_2 , data = data)
summary(lm32)
#to annotate the graph need library(grid)
rsq <- summary(lm32)$adj.r.squared
pvalue <- summary(lm32)$coefficients[2,4]
rp <- paste0("adj.r^2 = ", round(rsq,2), "  pvalue = ", round(pvalue, 3))

dev.off()
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(lm32) # check for normality

newpoint <- broom::augment(lm32, newdata = new_data)
(pred <- predict(lm32, newdata = new_data, interval = "prediction", level = 0.80))
lwr <- pred[2] # needed for ggplot
upr <- pred[3]
predict(lm32, newdata = new_data, interval = "confidence", level = 0.80)

#Use to make 95% CI and PI 
minoage_2 <- min(data$oage_2, na.rm = TRUE)
maxoage_2 <- max(data$oage_2, na.rm = TRUE)
predx <- data.frame(oage_2 = seq(from = minoage_2, to = maxoage_2, by = (maxoage_2-minoage_2)/19))

# ... confidence interval
conf.int <- cbind(predx, predict(lm32, newdata = predx, interval = "confidence", level = 0.80))

# ... prediction interval
pred.int <- cbind(predx, predict(lm32, newdata = predx, interval = "prediction", level = 0.80))

g.pred <- ggplot(pred.int, aes(x = oage_2, y = fit)) +
  geom_smooth(data = pred.int, aes(ymin = lwr, ymax = upr), stat = "identity") + # prediction interval
  geom_smooth(data = conf.int, aes(ymin = lwr, ymax = upr), stat = "identity") + #confidence interval
  geom_point(data = data, aes(x = oage_2, y = oage_3)) + #plots all the points
  geom_text_repel(data = data, aes(x = oage_2, y = oage_3, label = year)) +
  geom_point(data = newpoint, aes(y = .fitted), size = 3, color = "red") + # adds this years new point
  geom_text_repel(data = newpoint, aes(x = oage_2, y = .fitted, label = round(.fitted, 0 )), adj = 1) +  
  #annotate("text", label = rp, x = 205000, y = 550000) + 
  stat_regline_equation(label.x = 100000, label.y = 600000) +
  theme_bw() +
  coord_cartesian(ylim = c(0, 500000), xlim = c(0, 850000)) +
  xlab("ocean age 2") +
  ylab("ocean age 3") #+ #ggtitle("oage_3 vs oage_2")
g.pred  
dev.off()
ggsave(filename = paste0("figures/oage_3_oage_2", ".png", sep = ""), device = png(), width = 7, height = 9, units = "in", dpi = 300)

#Repeated K- fold Cross validation

#check missing values There should be some for the older ages since those fish haven't returned yet
data[!complete.cases(data),]

#IF only the most recent years for age classes are missing remove them.   ... other wise figure out why they are missing!
data_cv <- data %>%
  select(oage_2, oage_3) %>%
  na.omit()  #can't have NA's for cross validation.

data_cv[!complete.cases(data_cv),]
#data <- data_cv
# define training control use one of the following lines
train_control <- trainControl(method = "cv", number = 3, summaryFunction = my_summary)
train_control <- trainControl(method = "repeatedcv", number = 3, repeats = 8, summaryFunction = my_summary)
#I used number of K-folds = 7 since I have 7*4 = 28 years for data
length(data$year)

# train the model Warning messages are okay.
model <- train(oage_3 ~ oage_2, data = data_cv, trControl=train_control, method="lm")
# summarize result
print(model)

#model 2 ---- this model is in a function call to prevent it from running since
#it was not the model chosen.
model2run <- function(run = 0){
   #This is a linear model where log transformed ocean age 3 returns are predicted based on ocean age 2 returns
   data <- data %>%
     mutate(ln_oage_3 = log(oage_3)) %>%
     filter(year > 1989)
   
   lmln32 <- lm(ln_oage_3 ~ oage_2 , data = data)
   summary(lmln32)
   lmln32 <- lm(log(oage_3)~ oage_2 , data = data)
   
   hist(resid(lm32))
   dev.off()
   layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
   plot(lmln32) # check for normality.
   
   newpoint <- broom::augment(lmln32, newdata = new_data)
   (pred <- exp(predict(lmln32, newdata = new_data, interval = "prediction", level = 0.80)))
   lwr <- pred[2]
   upr <- pred[3]
   exp(predict(lmln32, newdata = new_data, interval = "confidence", level = 0.80))
   
   #Use to make 95% CI and PI 
   minoage_2 <- min(data$oage_2, na.rm = TRUE)
   maxoage_2 <- max(data$oage_2, na.rm = TRUE)
   predx <- data.frame(oage_2 = seq(from = minoage_2, to = maxoage_2, by = (maxoage_2-minoage_2)/19))
   
   # ... confidence interval
   conf.int <- cbind(predx, exp(predict(lmln32, newdata = predx, interval = "confidence", level = 0.80)))
   
   # ... prediction interval
   pred.int <- cbind(predx, exp(predict(lmln32, newdata = predx, interval = "prediction", level = 0.80)))
   
   g.pred <- ggplot(pred.int, aes(x = oage_2, y = fit)) +
     geom_smooth(data = pred.int, aes(ymin = lwr, ymax = upr), stat = "identity") + # prediction interval
     geom_smooth(data = conf.int, aes(ymin = lwr, ymax = upr), stat = "identity") + #confidence interval
     geom_point(data = newpoint, aes(y = exp(.fitted)), size = 3, color = "red") + # adds this years new point
     geom_text_repel(data = newpoint, aes(x = oage_2, y = exp(.fitted), label = round(exp(.fitted), 0 )), adj = 4) +
     geom_point(data = data, aes(x = oage_2, y = oage_3)) + #plots all the points
     geom_text_repel(data = data, aes(x = oage_2, y = oage_3, label = year)) +
     #annotate("text", label = rp, x = 205000, y = 550000) + 
     #stat_regline_equation(label.x = 100000, label.y = 450000) +
     theme_bw() +
     xlab("ocean age 2") +
     ylab("ocean age 3") + #+ #ggtitle("oage_3 vs oage_2") +
     coord_cartesian(ylim = c(0, 500000), xlim = c(0, 850000))
   g.pred 
   ggsave(filename = paste0("figures/ln_oage_3_oage_2", ".png", sep = ""), device = png(), width = 7, height = 9, units = "in", dpi = 300)
   
   # cross validate the model
   
   data_cv <- data %>%
     select(oage_2, ln_oage_3) %>%
     na.omit()  #can't have NA's for cross validation.
   #data <- data_cv
   # define training control 
   train_control <- trainControl(method = "cv", number = 3, summaryFunction = my_exp_summary)
   train_control <- trainControl(method = "repeatedcv", number = 3, repeats = 8, summaryFunction = my_exp_summary)
   #I used number of K-folds = 3 so that there was still a good amount of data in each fold.
   length(data_cv$oage_2)
   
   # train the model
   model <- train(ln_oage_3 ~ oage_2, data = data_cv, trControl=train_control, method="lm")
   # summarize results
   print(model)
}

# median return by size ----

#forecast
pred
quant
quant[3,3] <- pred[1]
quant[3,2] <- pred[2]
quant[3,4] <- pred[3]

#check it matches worksheet.
lwr <- sum(quant$lwr90[1:2], quant$lwr90[4])
est <- sum(quant$est[1:2], quant$est[4])
upr <- sum(quant$upr90[1:2],  quant$upr90[4])

lwr <- sum(quant$lwr90[1:2], pred[2], quant$lwr90[4])
est <- sum(quant$est[1:2], pred[1], quant$est[4])
upr <- sum(quant$upr90[1:2], pred[3], quant$upr90[4])

# forecast ----
quant %>%
  summarize(lwr = sum(lwr),
          est = sum(est),
          upr = sum(upr))

(bear_f <- data.frame(est, lwr, upr))
bear_f$est

#additional for report ----
escapement_goal <- 156000
(harvest_est <- bear_f$est - escapement_goal )

