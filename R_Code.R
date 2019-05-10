#starting Project
#Iporting libraries
library(plyr)
library(dplyr)
library(forecast)
library(ggplot2)
library(tseries)
library(tidyverse)
library(stringr)
library(tidyr)
library(MASS)
library(readxl)
library(data.table)
library(Hmisc)
library(reshape2)
library(ISLR)

#checking working directory
getwd()

#Import Happiness data

#Data 2016
hap2016 = read.csv("C:/Users/harsh/Desktop/Courses/LBJ_Spring19/Database Management/Project/Data_Final_Project/online-data-chapter-2-whr-2016.csv")
View(hap2016)
#Taking only happiness score
hap16 = hap2016[,1:2]
View(hap16)
#renaming column name as hap_score_16
names(hap16)[2] = "hap_score_16"
View(hap16)

#Data 2017
hap2017 = read.csv("C:/Users/harsh/Desktop/Courses/LBJ_Spring19/Database Management/Project/Data_Final_Project/online-data-chapter-2-whr-2017.csv")
View(hap2017)
#Taking only happiness score
hap17 = hap2017[,1:2]
View(hap17)
#renaming column name as hap_score_17
names(hap17)[2] = "hap_score_17"
View(hap17)


#Data 2018
hap2018 = read.csv("C:/Users/harsh/Desktop/Courses/LBJ_Spring19/Database Management/Project/Data_Final_Project/online-data-chapter-2-whr-2018.csv")
View(hap2018)
#Taking only happiness score
hap18 = hap2018[,1:2]
View(hap18)
#renaming column name as hap_score_18
names(hap18)[2] = "hap_score_18"
View(hap18)

#Data 2019
hap2019 = read.csv("C:/Users/harsh/Desktop/Courses/LBJ_Spring19/Database Management/Project/Data_Final_Project/online-data-chapter-2-whr-2019.csv")
View(hap2019)
#Taking only happiness score
hap19 = hap2019[,1:2]
View(hap19)
#renaming column name as hap_score_19
names(hap19)[2] = "hap_score_19"
View(hap19)

#Hap Data Validation
hap_valid = read.csv()

#2016
eco2016i = read.csv("C:/Users/harsh/Desktop/Courses/LBJ_Spring19/Database Management/Project/Data_Final_Project/economic_freedom_2016.csv")
View(eco2016i)
#removing NA
eco2016 = na.omit(eco2016i)
eco16 = eco2016[,c(2,7)]
View(eco16)
#renaming colname as Country and eco_score16
names(eco16)[1] = "Country"
names(eco16)[2] = "eco_score_16"
#removinf N/A
eco16f = eco16[!grepl("N/A", eco16$eco_score_16),]
View(eco16f)

#2017
eco2017i = read.csv("C:/Users/harsh/Desktop/Courses/LBJ_Spring19/Database Management/Project/Data_Final_Project/economic_freedom_2017.csv")
View(eco2017i)
#removing NA
eco2017 = na.omit(eco2017i[,1:34])
eco17 = eco2017[,c(2,7)]
View(eco17)
#renaming colname as Country and eco_score_17
names(eco17)[1] = "Country" 
names(eco17)[2] = "eco_score_17"
#removinf N/A
eco17f = eco17[!grepl("N/A", eco17$eco_score_17),]
View(eco17f)

#2018
eco2018i = read.csv("C:/Users/harsh/Desktop/Courses/LBJ_Spring19/Database Management/Project/Data_Final_Project/economic_freedom_2018.csv")
View(eco2018i)
#removing NA
eco2018 = na.omit(eco2018i)
eco18 = eco2018[,c(2,7)]
View(eco18)
#renaming colname as Country and eco_score_18
names(eco18)[1] = "Country" 
names(eco18)[2] = "eco_score_18"
#removing N/A
eco18f = eco18[!grepl("N/A", eco18$eco_score_18),]
View(eco18f)

#2019
eco2019i = read.csv("C:/Users/harsh/Desktop/Courses/LBJ_Spring19/Database Management/Project/Data_Final_Project/economic_freedom_2019.csv")
View(eco2019i)
#removing NA
eco2019 = na.omit(eco2019i)
eco19 = eco2019[,c(2,7)]
View(eco19)
#renaming colname as Country and eco_score_19
names(eco19)[1] = "Country" 
names(eco19)[2] = "eco_score_19"
#removinf N/A
eco19f = eco19[!grepl("N/A", eco19$eco_score_19),]
View(eco19f)

#Eco Validation Data
eco_valid = read.csv("")

#Importing Freedom Index Data
FIW = read.csv("C:/Users/harsh/Desktop/Courses/LBJ_Spring19/Database Management/Project/Data_Final_Project/Freedom_Index.csv")
View(FIW)
#setting the first rwo as column names
colnames(FIW) = FIW[1,]
FIW = FIW[-1,]
View(FIW)
#Renaming Column as Country
names(FIW)[1] = "Country"
View(FIW)
#Correcting the index
rownames(FIW) = 1:nrow(FIW)
View(FIW)

#FIW 2016
FIW16 = FIW[c("Country", "2016")]
#renaming column as free_ind_2016
names(FIW16)[2] = "free_ind_2016"
View(FIW16)

#FIW 2017
FIW17 = FIW[c("Country", "2017")]
#renaming column as free_ind_2017
names(FIW17)[2] = "free_ind_2017"
View(FIW17)

#FIW 2018
FIW18 = FIW[c("Country", "2018")]
#renaming column as free_ind_2018
names(FIW18)[2] = "free_ind_2018"
View(FIW18)

#FIW 2019
FIW19 = FIW[c("Country", "2019")]
#renaming column as free_ind_2019
names(FIW19)[2] = "free_ind_2019"
View(FIW19)

#Vaidation Data
data_valid = read.csv("C:/Users/harsh/Desktop/Courses/LBJ_Spring19/Database Management/Project/Data_Final_Project/validation_data.csv", stringsAsFactors = FALSE)
View(data_valid)
data_valid = data_valid[(4:166),]
#correcting index
rownames(data_valid) <- 1:nrow(data_valid)
View(data_valid)
names(data_valid) = data_valid[1,]
data_valid = data_valid[-1,]
View(data_valid)
data_valid_f = data_valid[c(3,5,71,139)]
View(data_valid_f)
rownames(data_valid_f) = 1:nrow(data_valid_f)
View(data_valid_f)
str(data_valid_f)

#Removing Countries
data_valid_num = data_valid_f[,-1]
data_valid_country = data_valid_f[,1]

#adding country name
#2018
dataf = cbind(data[,1], data1)
names(dataf) = c("Country", "hap_score_t", "eco_t", "eco_t1", "eco_t2", "fi_t", "fi_t1", "fi_t2")
View(dataf)

#converting to num
indx_data_valid = sapply(data_valid_num, is.character)
data_valid_num[indx_data_valid] = lapply(data_valid_num[indx_data_valid], function(x) as.numeric(as.character(x)))
View(data_valid_num)
str(data_valid_num)
data_valid_f = cbind(data_valid_f[,1], data_valid_num)
View(data_valid_f)
names(data_valid_f)[1] = "Country"
View(data_valid_f)

#2018 Correlation dataframe
data_cor = merge(data_valid_f, dataf, by = "Country")
View(data_cor)
#correlating Political freedom
cor(data_cor$`HUMAN FREEDOM (Score)`, data_cor$fi_t)
plot(data_cor$`HUMAN FREEDOM (Score)`, data_cor$fi_t)

#correlating Economic Freedom
cor(data_cor$`ECONOMIC FREEDOM (Score)`, data_cor$eco_t)
plot(data_cor$`ECONOMIC FREEDOM (Score)`, data_cor$eco_t)
#correlating happiness
cor(data_cor$`PERSONAL FREEDOM (Score)`, data_cor$hap_score_t)
plot(data_cor$`PERSONAL FREEDOM (Score)`, data_cor$hap_score_t)




#Converting the dataframes into lists
list.df = list(hap18, eco18f, eco17f, eco16f, FIW18, FIW17, FIW16)
View(list.df)

list19.df = list(hap19, eco19f, eco18f, eco17f, FIW19, FIW18, FIW17)
View(list19.df)

#merging in a single datasheet
#Data 2018
data = Reduce(function(x,y) merge(x,y, by = c("Country"), accumulate = FALSE), list.df)
View(data)
data = as.data.frame(data)
class(data) #'x' must be numeric

#Data 2019
data_19 = Reduce(function(x,y) merge(x,y, by = c("Country"), accumulate = FALSE), list19.df)
View(data_19)

#removing country name
data1 = data[,-1]
data_191 = data_19[,-1]

#converting factor into numeric
indx = sapply(data1, is.factor)
data1[indx] = lapply(data1[indx], function(x) as.numeric(as.character(x)))
View(data1)
str(data1)
#2019 Data
indx19 = sapply(data_191, is.factor)
data_191[indx19] = lapply(data_191[indx19], function(x) as.numeric(as.character(x)))
View(data_191)
str(data_191)




#2019
data19f = cbind(data_19[,1], data_191)
names(data19f) = c("Country", "hap_score_t", "eco_t", "eco_t1", "eco_t2", "fi_t", "fi_t1", "fi_t2")
View(data19f)


#Correlation matrix
cormat = round(cor(data1),2)

#plotting cormat
melted_cormat = melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "White")+
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="Pearson\nCorrelation")

#Model 1 - Multiple Linear Regression
Mod1 = lm(hap_score_t ~ eco_t + eco_t1 + eco_t2 + fi_t + fi_t1+ fi_t2, data = dataf)
summary(Mod1)
AIC(Mod1)

#Model 2 (Eco_score_18)
Mod2 = lm(hap_score_t ~ eco_t , data = dataf)
summary(Mod2)
AIC(Mod2)

#Model 3 (Eco_score_18 + free_ind_2018)
Mod3 = lm(hap_score_t ~ eco_t + fi_t, data = dataf)
summary(Mod3)
AIC(Mod3)

#Model 4 (eco_score_18 + eco_score_17 + free_ind_2018)
Mod4 = lm(hap_score_t ~ eco_t + eco_t1 + fi_t, data = dataf)
summary(Mod4) #Both economic freedom index become non significant
AIC(Mod4)

#Model 5 (eco_free_17 + free_ind_2017)
Mod5 = lm(hap_score_t ~ eco_t1 + fi_t1, data = dataf)
summary(Mod5)
AIC(Mod5)

#Model Comparison
summary_table = cbind.data.frame(AIC(Mod1), AIC(Mod2), AIC(Mod3), AIC(Mod4), AIC(Mod5))
View(summary_table)
#Model 5 looks best

#Visualization

#histogram
hist(dataf$hap_score_t)

#Plotting data
plot(dataf$hap_score_t, dataf$eco_t)
abline(lm(dataf$hap_score_18 ~ dataf$eco_score_18))
ggplot(dataf, aes(hap_score_t)) + geom_density()

#Prediction using Model 5
hap_2019pr5 = predict(Mod5, data19f)
pr2019m5 = data.frame(data19f$Country, hap_2019pr5)
names(pr2019m5)[1] = "Country"
View(pr2019m5)

#Prediction using Model 3
hap_2019pr3 = predict(Mod3, data19f)
pr2019m3 = data.frame(data19f$Country, hap_2019pr3)
names(pr2019m3)[1] = "Country"
View(pr2019m3)

ggplot(pr2019m5, aes(hap_2019pr5)) + geom_density()
ggplot(pr2019m3, aes(hap_2019pr3)) + geom_density()

#Merging the predicted values with actual values
comp5 = merge.data.frame(hap19, pr2019m5, by = "Country")
View(comp5)

comp3 = merge.data.frame(hap19, pr2019m3, by = "Country")
View(comp3)

#t-test
t.test(comp5$hap_score_19, comp5$hap_2019pr5)
t.test(comp3$hap_score_19, comp3$hap_2019pr3)

#plot
plot(density(comp5$hap_score_19))
lines(density(comp5$hap_2019pr5), col = "Blue")
lines(density(comp3$hap_2019pr3), col = "Green")

#Logistic Regression
#Logisitic Regression Model 1
glm1 = glm(log(hap_score_t) ~ eco_t + eco_t1 + eco_t2 + fi_t + fi_t1+ fi_t2, data = dataf, family = gaussian())
summary(glm1)
AIC(glm1)
#Logistic Regression Model 2
glm2 = glm(log(hap_score_t) ~ eco_t , data = dataf)
summary(glm2)
AIC(glm2)
#Logisitic Regression Model 3
glm3 = glm(log(hap_score_t) ~ eco_t + fi_t, data = dataf)
summary(glm3)
AIC(glm3)
#Logisitic Regression Model 4
glm4 = glm(log(hap_score_t) ~ eco_t + eco_t1 + fi_t, data = dataf)
summary(glm4)
AIC(glm4)
#Logistic Regression Model 5
glm5 = glm(log(hap_score_t) ~ eco_t1 + fi_t1, data = dataf)
summary(glm5)
AIC(glm5)

#Prediction using Model 5
hap_2019prg5 = exp(predict(glm5, data19f))
pr2019glm5 = data.frame(data19f$Country, hap_2019prg5)
names(pr2019glm5)[1] = "Country"
View(pr2019glm5)

#Prediction using Model 3
hap_2019prg3 = exp(predict(glm3, data19f))
pr2019glm3 = data.frame(data19f$Country, hap_2019prg3)
names(pr2019glm3)[1] = "Country"
View(pr2019glm3)

#Merging the predicted values with actual values
gcomp5 = merge.data.frame(hap19, pr2019glm5, by = "Country")
View(gcomp5)

gcomp3 = merge.data.frame(hap19, pr2019glm3, by = "Country")
View(gcomp3)

#t-test
t.test(gcomp5$hap_score_19, gcomp5$hap_2019prg5)
t.test(gcomp3$hap_score_19, gcomp3$hap_2019prg3)

#plot
plot(density(gcomp5$hap_score_19))
lines(density(gcomp5$hap_2019prg5), col = "Blue")
lines(density(gcomp3$hap_2019prg3), col = "Green")

