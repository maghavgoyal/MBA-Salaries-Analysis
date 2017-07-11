#Analysis of MBA SALARIES
#Name: Maghav Goyal
#EMail: maghavgoyal@gmail.com
#College: DTU

#Setting the working directory 
setwd("C:/Users/ANIL GOYAL/Desktop/New folder (2)/data sets")

#Reading the data and creating a data frame 
mba.df <-read.csv(paste("MBA.csv",sep=""),)

#Viewing the data-frame
View(mba.df)

#Summarizing the data
summary(mba.df)

#Creating a dataset of the placed students
placed.df<-mba.df[which(mba.df$salary>0),]

#Viewing the dataset
View(placed.df)

#Visualization
plot(placed.df$sex,placed.df$salary)
cor.test(placed.df$salary,placed.df$sex) # Not significant, low correlation

plot(placed.df$work_yrs,placed.df$salary)

#Removing the data of the people who didnt answer to the survey
placed1.df<-placed.df[(placed.df$salary>999),]
View(placed1.df)

plot(placed1.df$work_yrs,placed1.df$salary)
cor.test(placed1.df$work_yrs,placed1.df$salary) #Significant, highly correlated

plot(placed1.df$frstlang,placed1.df$salary)
cor.test(placed1.df$frstlang,placed1.df$salary) #significant, highly correlated

plot(placed1.df$quarter,placed1.df$salary)
cor.test(placed1.df$quarter,placed1.df$salary)  #insignificant,low correlation

plot(placed1.df$satis,placed1.df$salary)
cor.test(placed1.df$satis,placed1.df$salary)    #insignificant, low correlation

plot(placed1.df$f_avg,placed1.df$salary)
plot(placed1.df$f_avg,placed1.df$salary,xlim=c(2,4)) # Observe the values from x=2 to x=4
cor.test(placed1.df$f_avg,placed1.df$salary)     #insignificant

plot(placed1.df$s_avg,placed1.df$salary)
cor.test(placed1.df$f_avg,placed1.df$salary)     #insignificant, low correlation

plot(placed1.df$gmat_tpc,placed1.df$salary)
plot(placed1.df$gmat_tpc,placed1.df$salary,xlim=c(60,100)) # Observe the values from x=60 to x=100
cor.test(placed1.df$gmat_tpc,placed1.df$salary)     #insignificant, low correlation

plot(placed1.df$gmat_vpc,placed1.df$salary)
cor.test(placed1.df$gmat_vpc,placed1.df$salary)   #insignificant, low correlation


plot(placed1.df$gmat_qpc,placed1.df$salary)
cor.test(placed1.df$gmat_qpc,placed1.df$salary)   #insignificant, low correlation

plot(placed1.df$gmat_tot,placed1.df$salary)
cor.test(placed1.df$gmat_tot,placed1.df$salary)   #insignificant, low correlation

plot(placed1.df$sex,placed1.df$salary)
cor.test(placed1.df$sex,placed1.df$salary)       

plot(placed1.df$age,placed1.df$salary)
cor.test(placed1.df$age,placed1.df$salary)     #highly significant

#Multi-variate analysis
library(ggvis)
ggvis(~age,~salary,fill=~sex,data=placed1.df)

#to test the dependence of variables
library(vcd) 
mytable <- xtabs(~salary+sex, data=placed1.df)
chisq.test(mytable)

#Corrgram
library(corrgram)
corrgram(placed1.df)
corrgram(placed1.df, order=TRUE, lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)

#using Boruta for predicting the important variables
library(Boruta)
set.seed(123)
response<-placed1.df$salary
bor.results<-Boruta(placed1.df,response,maxRuns = 99)
plot(bor.results) 


#Red-rejected variables,Yellow-tentative variables,Green-important variable
#Sex,savg,work_yrs,age are significant


#Regression Model
fit<-lm(salary~.,data=placed1.df)
summary(fit)
fit <-glm(salary~.,data=placed1.df)
summary(fit)

fit<-lm(salary~sex+savg+workyrs+age,data=placed1.df)
summary(fit)

#Considering interaction terms upto 2nd degree
fit<-lm(salary~(sex+savg+workyrs+age)^2,data=placed1.df)
summary(fit)

#considering interaction terms upto 3rd degree
fit<-lm(salary~(sex+savg+workyrs+age)^3,data=placed1.df)
summary(fit)

#Considering interaction terms upto 4th degree
fit<-lm(salary~(sex+savg+workyrs+age)^4,data=placed1.df)
summary(fit)

mba1.df<-mba.df[which(mba.df$salary!=998 & mba.df$salary!=999),]
#To run logistic regression
mba1.df$salary[mba1.df$salary>0]<-1
mba1.df$salary[mba1.df$salary<=0]<-0 #Converting into categorical data


#Training and testing data-sets
train<-mba1.df[1:160,]
test<-mba1.df[161:193,]

model <- glm(salary ~.,family =binomial(link='logit'),data=train)
summary(model)

#ANOVA
anova(model, test="Chisq")
#The difference between the null deviance and the residual deviance shows how our model is doing against the null model (a model with only the intercept). The wider this gap, the better.


library(pscl)
pR2(model)

fitted.results <- predict(model,test1=subset(test,select=c(1,3,7,10)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)


#Accuracy of the model
misClasificError <- mean(fitted.results != test$salary)
print(paste('Accuracy',1-misClasificError))





