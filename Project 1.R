data<-read.table("diamonds4.csv", header=TRUE, sep=",")
attach(data)
data

#set categorical variables as factors
clarity = factor(clarity)
color = factor(color)
cut = factor(cut)

result = lm(price ~., data=data)
summary(result)

par(mfrow=c(2,2))
plot(result$fitted.values, result$residuals, main="Plot of residuals against fits")
abline(h=0,col="red")
acf(result$residuals, main="ACF of Residuals")
qqnorm(result$residuals)
qqline(result$residuals, col="red")


#plot carat vs price

plot(carat, price, main='Price vs Carat')

#Fitting a full model shows issues with constant variance and nonnormality




#box-cox is near .5, but does not include .5. 
par(mfrow=c(1,1))
library('MASS')
boxcox(result, lambda = seq(.2, .5, .1))






#plot categorical variables to see distribution

#install.packages('ggplot2')
library(ggplot2)
# Color
ggplot(data.frame(data), aes(x=color)) +
  geom_bar()
# Cut
ggplot(data.frame(data), aes(x=cut)) +
  geom_bar()
# Clarity
ggplot(data.frame(data), aes(x=clarity)) +
  geom_bar()






#Lets try transforming y


#log y
data$log.price = log(price)
attach(data)
clarity = factor(clarity)
color = factor(color)
cut = as.factor(cut)

is.factor(cut)

log.result = lm(log.price ~ carat+clarity+color+cut, data=data)
summary(log.result)

#plot distribution of price before and after transformation
#Still right-skewed, but better
par(mfrow=c(2,1))
hist(price)
hist(log.price)


#regression assumption plots look bad now, but look better after also transforming carat

par(mfrow=c(2,2))
plot(log.result$fitted.values, log.result$residuals, main="Plot of residuals against fits")
abline(h=0,col="red")
acf(log.result$residuals, main="ACF of Residuals")
qqnorm(log.result$residuals)
qqline(log.result$residuals, col="red")

plot(carat, log.price, main='Price vs Carat')







#try with transformed y and carat

#right now, log transformation seems best
data$log.carat = log(carat)
attach(data)

full_result = lm(log.price ~ log.carat+clarity+color+cut, data=data)
summary(full_result)

#Plot distribution of carat before and after transformation, still right-skewed but better
par(mfrow=c(2,1))
hist(carat)
hist(log.carat)


#Regression assumptions look much better, still some issues with constant variance

par(mfrow=c(2,2))
plot(full_result$fitted.values, full_result$residuals, main="Plot of residuals against fits")
abline(h=0,col="red")
acf(full_result$residuals, main="ACF of Residuals")
qqnorm(full_result$residuals)
qqline(full_result$residuals, col="red")

plot(log.carat, log.price, main='Price vs Carat')






#boxplots look better after transforming y

boxplot(log.price~cut, main="Boxplot of Price by Cut")
boxplot(log.price~clarity, main="Boxplot of Price by Clarity")
boxplot(log.price~color, main="Boxplot of Price by Color")



#install.packages("RTools")
#pairwise comparison of color - not working for me
library(multcomp)
pairwise<-glht(full_result, linfct = mcp(cut= "Tukey"))
?glht
summary(pairwise)




#F-test for cut

reduced = lm(log.price ~ log.carat + clarity + color)
anova(reduced, full_result)

#F-test for clarity

reduced = lm(log.price ~ log.carat + cut + color)
anova(reduced, full_result)

#F-test for carat

reduced = lm(log.price ~ cut + clarity + color)
anova(reduced, full_result)


#F-test for color

reduced = lm(log.price ~ log.carat + clarity + cut)
anova(reduced, full_result)


#Checking for interactions with Carat
#!!!Only checks interactions with carat, not with other categorical predictors!

#Subset by color
a1<-subset(data,color=="D") 
a2<-subset(data,color=="E")
a3<-subset(data,color=="F")
a4<-subset(data,color=="G")
a5<-subset(data,color=="H")
a6<-subset(data,color=="I")
a7<-subset(data,color=="J")

##fit 7 separate regressions, one for each color
reg1<-lm(log.price ~ log.carat,data=a1)
reg2<-lm(log.price ~ log.carat,data=a2)
reg3<-lm(log.price ~ log.carat,data=a3)
reg4<-lm(log.price ~ log.carat,data=a4)
reg5<-lm(log.price ~ log.carat,data=a5)
reg6<-lm(log.price ~ log.carat,data=a6)
reg7<-lm(log.price ~ log.carat,data=a7)

par(mfrow=(c(3,3)))
#color D
plot(a1$log.carat, a1$log.price, main='Price vs Carat Color D')
abline(reg1,lty=1)

#Color E
plot(a2$log.carat, a2$log.price, main='Price vs Carat Color E')
abline(reg2,lty=1)

#color F
plot(a3$log.carat, a3$log.price, main='Price vs Carat Color F')
abline(reg3,lty=1)

#Color G
plot(a4$log.carat, a4$log.price, main='Price vs Carat Color G')
abline(reg4,lty=1)

#color H
plot(a5$log.carat, a5$log.price, main='Price vs Carat Color H')
abline(reg5,lty=1)

#Color G
plot(a6$log.carat, a6$log.price, main='Price vs Carat Color I')
abline(reg6,lty=1)

#Color G
plot(a7$log.carat, a7$log.price, main='Price vs Carat Color J')
abline(reg7,lty=1)


#looks like there might be an interaction, not sure
par(mfrow=c(1,1))
plot(log.carat, data$log.price, main = "Overall Price vs Carat by Color")
abline(reg1,lty=1, col='Black')
abline(reg2,lty=1, col='Blue')
abline(reg3,lty=1, col='Red')
abline(reg4,lty=1, col='Purple')
abline(reg5,lty=1, col='Orange')
abline(reg6,lty=1, col='Green')
abline(reg7,lty=1, col='Pink')
legend("topleft", c('D','E','F','G','H','I','J'), pch=1, col=c('black','blue','red','purple','orange','green','pink')) 


#F-test for an interaction term with Color shows the interaction term is valuable
interaction = lm(log.price ~ log.carat*color + clarity + cut)
summary(interaction)
anova(full_result, interaction)




#Same comparison for Cut

a1<-subset(data,cut="Good") 
a2<-subset(data,cut=="Ideal")
a3<-subset(data,cut=="Very Good")
a4<-subset(data,cut=="Astor Ideal")


##fit 4 separate regressions, one for each cut
reg1<-lm(log.price ~ log.carat,data=a1)
reg2<-lm(log.price ~ log.carat,data=a2)
reg3<-lm(log.price ~ log.carat,data=a3)
reg4<-lm(log.price ~ log.carat,data=a4)


par(mfrow=(c(2,2)))
#good cut
plot(a1$log.carat, a1$log.price, main='Price vs Carat Good Cut')
abline(reg1,lty=1)

#ideal cut
plot(a2$log.carat, a2$log.price, main='Price vs Carat Ideal Cut')
abline(reg2,lty=1)

#very good cut
plot(a3$log.carat, a3$log.price, main='Price vs Carat Very Good Cut')
abline(reg3,lty=1)

#astor ideal cut
plot(a4$log.carat, a4$log.price, main='Price vs Carat Astor Ideal Cut')
abline(reg4,lty=1)


#looks like there is not an interaction
par(mfrow=c(1,1))
plot(log.carat, data$log.price, main = "Overall Price vs Carat by Cut")
abline(reg1,lty=1, col='Black')
abline(reg2,lty=1, col='Blue')
abline(reg3,lty=1, col='Red')
abline(reg4,lty=1, col='Purple')
legend("topleft", c('Good','Ideal','Very Good','Astor Ideal'), pch=1, col=c('black','blue','red','purple')) 


#F-test for an interaction term with Cut shows the interaction is valuable, but less significant than
#the interaction with Color
interaction = lm(log.price ~ log.carat*cut + color + clarity)
summary(interaction)
anova(full_result, interaction)





#Same comparison for clarity
#Subset by color
a1<-subset(data,clarity=="IF") 
a2<-subset(data,clarity=="SI1")
a3<-subset(data,clarity=="SI2")
a4<-subset(data,clarity=="VS1")
a5<-subset(data,clarity=="VS2")
a6<-subset(data,clarity=="VVS1")
a7<-subset(data,clarity=="VVS2")
a8<-subset(data,clarity=="FL")


##fit 8 separate regressions, one for each clarity
reg1<-lm(log.price ~ log.carat,data=a1)
reg2<-lm(log.price ~ log.carat,data=a2)
reg3<-lm(log.price ~ log.carat,data=a3)
reg4<-lm(log.price ~ log.carat,data=a4)
reg5<-lm(log.price ~ log.carat,data=a5)
reg6<-lm(log.price ~ log.carat,data=a6)
reg7<-lm(log.price ~ log.carat,data=a7)
reg8<-lm(log.price ~ log.carat,data=a8)

par(mfrow=(c(3,3)))
#IF
plot(a1$log.carat, a1$log.price, main='Price vs Carat Clarity IF')
abline(reg1,lty=1)

#SI1
plot(a2$log.carat, a2$log.price, main='Price vs Carat Clarity SI1')
abline(reg2,lty=1)

#SI2
plot(a3$log.carat, a3$log.price, main='Price vs Carat Clarity SI2')
abline(reg3,lty=1)

#VS1
plot(a4$log.carat, a4$log.price, main='Price vs Carat Clarity VS1')
abline(reg4,lty=1)

#VS2
plot(a5$log.carat, a5$log.price, main='Price vs Carat Clarity VS2')
abline(reg5,lty=1)

#VVS1
plot(a6$log.carat, a6$log.price, main='Price vs Carat Clarity VVS1')
abline(reg6,lty=1)

#VVS2
plot(a7$log.carat, a7$log.price, main='Price vs Carat Clarity VVS2')
abline(reg7,lty=1)

#FL
plot(a8$log.carat, a8$log.price, main='Price vs Carat Clarity FL')
abline(reg8,lty=1)


#Looks like no interaction
par(mfrow=c(1,1))
plot(log.carat, data$log.price, main = "Overall Price vs Carat by Clarity")
abline(reg1,lty=1, col='Black')
abline(reg2,lty=1, col='Blue')
abline(reg3,lty=1, col='Red')
abline(reg4,lty=1, col='Purple')
abline(reg5,lty=1, col='Orange')
abline(reg6,lty=1, col='Green')
abline(reg7,lty=1, col='Pink')
abline(reg8,lty=1, col='Brown')
legend("topleft", c('IF','SI1','SI2','VS1','VS2','VVS1','VVS2','FL'), pch=1, col=c('black','blue','red','purple','orange','green','pink','brown')) 

#F-test for an interaction term with Clarity shows the interaction is valuable, but less
#significant than the other two interaction terms
interaction = lm(log.price ~ log.carat*clarity + color + cut)
summary(interaction)
anova(full_result, interaction)



#All interaction terms were significant when compared to a model with no interaction terms. 
#Let's test for significance in models with interaction terms

#Start with full model with every interaction term included
result = lm(log.price ~ log.carat*color + log.carat*cut + log.carat*clarity)


reduced = lm(log.price ~ log.carat*color + cut + log.carat*clarity)
anova(reduced, result)
#With all interactions already in the model, interaction of carat and cut is still significant

reduced = lm(log.price ~ color + log.carat*cut + log.carat*clarity)
anova(reduced, result)
#With all interactions already in the model, interaction of carat and color is still significant

reduced = lm(log.price ~ log.carat*color + log.carat*cut + clarity)
anova(reduced, result)
#With all interactions already in the model, interaction of carat and clarity is not significant





#Try comparing to a full model with only interactions b/w carat and color and carat and cut
result = lm(log.price ~ log.carat*color + log.carat*cut + clarity)



reduced = lm(log.price ~ log.carat*color + cut + clarity)
anova(reduced, result)
#With interaction of carat and color in the model, interaction of carat and cut is still significant

reduced = lm(log.price ~ color + log.carat*cut + clarity)
anova(reduced, result)
#With interaction of carat and cut in the model, interaction of carat and color is still significant




#Based on the partial F-Tests, our final model should include interaction terms with cut and color, 
#but not clarity

result = lm(log.price ~ log.carat*color + log.carat*cut + clarity)
summary(result)

#Calculate PRESS and R2 pred to check predictive power

PRESS = function(model){
  sum(((residuals(model) / (1 - lm.influence(model)$hat))^2))
}

press = PRESS(result)
press

sst = sum(anova(result)[,2])
pred_R2 = (1-(press/sst))*100
pred_R2

#The model looks like it will be successful for predictions


#train/test split to check adequacy
library(corpora)
sample_data = sample.df(data, nrow(data), size = 607)

sample_result = lm(log.price ~ log.carat*color + log.carat*cut + clarity, data=sample_data)
summary(sample_result)

press = PRESS(sample_result)
press
sst = sum(anova(result)[,2])
pred_R2 = (1-(press/sst))*100
pred_R2

#Looks like a very strong model, overfitting doesn't seem to be an issue

#check the levels of cut
#levels(cut)
#after reviewing the data, decide to collapse "very good" and "good" cuts into "Other" category
cut <- factor(c("Astor Ideal","Good","Ideal","very Good"), levels = c("Astor Ideal", "Ideal", "Other"))
#verify the re-leveling and contrasts
#levels(cut)
#contrasts(cut)

data

library("car")    
data$cut.recode = recode(cut, "c('Astor Ideal','Ideal')='Ideal'; else='Good'")

# Cut
ggplot(data.frame(data), aes(x=cut.recode)) +
  geom_bar()

data$color.recode = recode(color, "c('D', 'E', 'F')='Colorless'; else='Near-Colorless'")
ggplot(data.frame(data), aes(x=color.recode)) +
  geom_bar()


data$clarity.recode = recode(clarity, "c('FL', 'IF')='Flawless'; c('VVs1', 'VVS2')='Microscopic Flaws';c('Vs1', 'VS2')='Microscopic Flaws'; else='Visible Flaws'")
ggplot(data.frame(data), aes(x=clarity.recode)) +
  geom_bar()

attach(data)


color.recode = factor(color.recode)
cut.recode = factor(cut.recode)
clarity.recode = factor(clarity.recode)


is.factor(color.recode)
is.factor(cut.recode)
is.factor(clarity.recode)

levels(cut.recode)
levels(clarity.recode)
levels(color.recode)


result = lm(log.price ~ log.carat*color.recode + log.carat*cut.recode + clarity.recode)
summary(result)

