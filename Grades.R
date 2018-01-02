#Linear Regression Project on grades data
library(readr)
grades<- read.csv(file.choose())
dim(grades)
colnames(grades) # this will gives us the names of the 22 variables
table(grades$gender)       #Based on gender
table(grades$ethnicity)    #Based on Ethnicity
table(grades$passfail)     #Based on whether pass or fail

boxplot(grades$gpa,grades$quiz1,grades$quiz2,grades$quiz3,grades$quiz4,grades$quiz5,col=heat.colors(6))
boxplot(grades$total,col="red")
#install.packages("psych")
library(psych)
describe(grades$gpa)
hist(grades$gpa,xlab="GPA",ylab=" Frequency",main="Histogram of GPA",col="red")
boxplot(grades$gpa,col="red",main= "Boxplot of GPA")

describe(grades$quiz1)
hist(grades$quiz1,xlab="Quiz1",ylab=" Frequency",main="Histogram of Quiz1",col="red")
boxplot(grades$quiz1,col="red",main= "Boxplot of Quiz1")

describe(grades$quiz2)
hist(grades$gpa,xlab="Quiz2",ylab=" Frequency",main="Histogram of Quiz2",col="blue")
boxplot(grades$quiz2,col="blue",main= "Boxplot of Quiz2")

describe(grades$quiz3)
hist(grades$gpa,xlab="Quiz3",ylab=" Frequency",main="Histogram of Quiz3",col="green")
boxplot(grades$quiz3,col="green",main= "Boxplot of Quiz3")

describe(grades$quiz4)
hist(grades$gpa,xlab="Quiz4",ylab=" Frequency",main="Histogram of Qui4",col="yellow")
boxplot(grades$quiz4,col="yellow",main= "Boxplot of Quiz4")

describe(grades$quiz5)
hist(grades$gpa,xlab="Quiz5",ylab=" Frequency",main="Histogram of Quiz5",col="brown")
boxplot(grades$quiz5,col="brown",main= "Boxplot of Quiz5")

describe(grades$total)
hist(grades$gpa,xlab="Total",ylab=" Frequency",main="Histogram of total",col="orange")
boxplot(grades$total,col="orange",main= "Boxplot of total")

describe(grades$final)
hist(grades$final,xlab="Total",ylab=" Frequency",main="Histogram of final",col="green")
boxplot(grades$final,col="red",main= "Boxplot of Final")

#Scatter plots Predictor Vs Response Variable

plot(grades$gpa,grades$final,main= "gpa vs Final" , xlab = "gpa", ylab = "final",col="red",abline(lm(final~gpa,data=grades)))
plot(grades$quiz1,grades$final,main= "Quiz1 vs Final" , xlab = "quiz1", ylab = "final",col="red",abline(lm(final~quiz1,data=grades)))
plot(grades$quiz2,grades$final,main= "Quiz2 vs Final" , xlab = "Quiz2", ylab = "final",col="red",abline(lm(final~quiz2,data=grades)))
plot(grades$quiz2,grades$final,main= "Quiz3 vs Final" , xlab = "Quiz3", ylab = "final",col="red",abline(lm(final~quiz3,data=grades)))
plot(grades$quiz2,grades$final,main= "Quiz4 vs Final" , xlab = "Quiz4", ylab = "final",col="red",abline(lm(final~quiz4,data=grades)))
plot(grades$quiz2,grades$final,main= "Quiz5 vs Final" , xlab = "Quiz5", ylab = "final",col="red",abline(lm(final~quiz5,data=grades)))
plot(grades$total,grades$final,main= "total vs Final" , xlab = "total", ylab = "final",col="red",abline(lm(final~total,data=grades)))
plot(grades$percent,grades$final,main= " Percent vs Final" , xlab = "Quiz2", ylab = "final",col="red",abline(lm(final~percent,data=grades)))

#checking Correlation of each of these predictors with the  response variable

cor(grades$gpa,grades$final)   #correlation between  gpa & final
cor(grades$quiz1,grades$final) #correlation between  quiz1 & final
cor(grades$quiz2,grades$final) #correlation between  quiz2 & final
cor(grades$quiz3,grades$final) #correlation between  quiz3 & final
cor(grades$quiz4,grades$final) #correlation between  quiz4 & final
cor(grades$quiz5,grades$final) #correlation between  quiz5 & final
cor(grades$total,grades$final) #correlation between  total & final
cor(grades$percent,grades$final)   #correlation between  percent & final

# Building Linear Regresion Models for predicting final from other predictor variables

# final ~ gpa+quiz1+quiz2+quiz3+quiz4+quiz5
fg12345<-lm(final~gpa+quiz1+quiz2+quiz3+quiz4+quiz5,data=grades)
fg12345
summary(fg12345)
library(car)
vif(fg12345) # Variance inflation factor for our model fg12345


# final~gpa+quiz2+quiz3+quiz4+quiz5,data=grades
fg2345<-lm(final~gpa+quiz2+quiz3+quiz4+quiz5,data=grades)
summary(fg2345)
vif(fg2345)  # Variance inflation factor for our model fg2345


# final~gpa+quiz3,data=grades
fg3<-lm(final~gpa+quiz3,data=grades)
fg3
summary(fg3)
vif(fg3) # Variance inflation factor for our model fg


#final~total+quiz3,data=grades
ft3<-lm(final~total+quiz3,data=grades)
ft3
summary(ft3)
vif(ft3) # Variance inflation factor for our model ft3

#final~quiz2+quiz3,data=grades
f23<-lm(final~quiz2+quiz3,data=grades)
summary(f23)
vif(f23)  # Variance inflation factor for our model f23

# final3~quiz3
f3<-lm(final~quiz3,data=grades)
summary(f3)


# Finally we have selected 2 Linear Regression Models that predicts  "final" with a good Accuracy. 
# The models Selected are  'ft3' & 'f23'

#The Equation for our models are as follows

#For Model ft3 the equation of the regression line is given by
#final = 6.67358 + (0.69502*total) - (1.89162*quiz3)

#For Model f23 the equation of the regression line is given by 
# final = 39.7129 + (1.5407*quiz2) - (1.1862*quiz3)


dwt(ft3)  # For ft3 durbin watson Statistics comes out to be 2.115215

dwt(f23)  #For f23 durbin watson Statistics comes out to be 2.233423

#Finding the predicted Value of final through both the models made & adding them in the grades dataset
grades$predft3<-predict(ft3)  #For ft3 model
grades$predft3


grades$predf23<-predict(f23)
grades$predf23 #For f23 model

## error values of final through the models are below. These values are giving us the residuals of our model

grades$errft3<-residuals(ft3)
grades$errft3 # For Model ft3


grades$errf23<-residuals(f23)
grades$errf23 # For model f23

# Adding observation no.s against each row in the data set grades
grades$obsno<-c(1:105)
grades$obsno

View(grades)

#For model ft3,inserting the predicted values in the grades dataset  by column creation
predft3<-predict(ft3)
grades$predft3<-predft3
grades$predft3

#For Model f23,inserting the predicted values in the grades dataset  by column creation
predf23<-predict(f23)
grades$predf23<-predf23
grades$predf23

###Checking for Assumptions Test for ft3 & f23 Models

hist(grades$errft3,main = "Normality check for ft3 model", xlab="Residuals",col="orange")
hist(grades$errf23,main = "Normality check for f23 model", xlab="Residuals",col="yellow")

####2.  Independent of observations 
plot(grades$obsno,grades$errft3,col="red",main="Independence of  error for ft3",xlab= " obsv no", ylab="residuals")
plot(grades$obsno,grades$errf23,col="brown",main="Independence of  error for f23",xlab= " obsv no", ylab="residuals")

####3 Check of linear relationship 
plot(grades$total,grades$final,main="Linear Rltnship for ft3",xlab="total",ylab="Final",col="red")
plot(grades$quiz3,grades$final,main="Linear Rltnship for ft3",xlab="quiz3",ylab="Final",col="red")
plot(grades$quiz2,grades$final,main="Linear Rltnship for f23",xlab="quiz2",ylab="Final",col="brown")
plot(grades$quiz3,grades$final,main="Linear Rltnship for f23",xlab="quiz3",ylab="Final",col="brown")


####4 Check of Constant Error Variance : Homoscedacity
plot(grades$predft3,grades$errft3,col="red",main="Constant error variance ft3",xlab="Predited",ylab="errors",abline(h=0))
plot(grades$predf23,grades$errf23,col="brown",main="Constant error variance f23",xlab="Predited",ylab="errors",abline(h=0))

# for finding the confidence intervals & the predited values for ft3 & f23 models
confint(ft3)
fitted(ft3)

fitted(f23)
confint(f23)
predict(f23, interval="confidence")   #same as fitted command