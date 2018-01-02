#Task
#-----------------------------------------------------------------------------------------------------------
#Refer file grades.csv
#The school principal wants to build a predictive model for predicting final for his consumption. As a principal he is very keen to have good scores by his students. He has given this data file to you with a request to suggest an appropriate model.
# You are required to build at least 4 models with different sets of predictors (independent variables). Selection of sets of predictor/s is upon you. Different sets of predictors can be a single variable or more than one variable. However, selection of predictor/s should be based on some logic. For example, for predicting final score of students, roll number cannot be a logical predictor. 
#You will analyze all 4 models based on following points and recommend the best model to the Principal. 
#-----------------------------------------------------------------------------------------------------------
#1.	Describe data of response variable and predictors in terms of key summary statistics like mean, mode, median, standard deviation, range, skewness and kurtosis. Show histogram and box plots also for each variables. [hint: describe command in R]
#Each variable to be explained in 30 words maximum.
table(grades$gender)
table(grades$ethnicity)
table(grades$quiz3)
table(grades$final)
table(grades$gpa)
Summary(grades)
describe(grades)
summary(grades$gpa)
describe(grades$gpa)
hist(grades$gpa, main = "Histogram of GPA", xlab = "GPA", ylab = "Frequency", col = "Blue")
boxplot(grades$gpa,  main = "Box plot of GPA", xlab = "GPA", col = "Blue", horizontal = T)
stem(grades$gpa)
summary(grades$quiz1)
describe(grades$quiz1)
hist(grades$quiz1, main = "Histogram of quiz1", xlab = "Quiz1 Marks", ylab = "Marks", col = "Blue")
boxplot(grades$quiz1,  main = "Box plot of Quiz1", xlab = "Quiz1", col = "Blue", horizontal = T)
stem(grades$quiz1)
summary(grades$quiz2)
describe(grades$quiz2)
hist(grades$quiz2, main = "Histogram of quiz2", xlab = "Quiz2 Marks", ylab = "Frequency", col = "Blue")
boxplot(grades$quiz2,  main = "Box plot of Quiz2", xlab = "Quiz2", col = "Blue", horizontal = T)
stem(grades$quiz2)
summary(grades$quiz3)
describe(grades$quiz3)
hist(grades$quiz3, main = "Histogram of quiz3", xlab = "Quiz3 Marks", ylab = "Frequency", col = "Blue")
boxplot(grades$quiz3,  main = "Box plot of Quiz3", xlab = "Quiz3", col = "Blue", horizontal = T)
stem(grades$quiz3)
summary(grades$quiz4)
describe(grades$quiz4)
hist(grades$quiz4, main = "Histogram of quiz4", xlab = "Quiz4 Marks", ylab = "Frequency", col = "Blue")
boxplot(grades$quiz4,  main = "Box plot of Quiz4", xlab = "Quiz4", col = "Blue", horizontal = T)
stem(grades$quiz4)
summary(grades$quiz5)
describe(grades$quiz5)
hist(grades$quiz5, main = "Histogram of quiz5", xlab = "Quiz5 Marks", ylab = "Frequency", col = "Blue")
boxplot(grades$quiz5,  main = "Box plot of Quiz5", xlab = "Quiz5", col = "Blue", horizontal = T)
stem(grades$quiz5)
summary(grades$final)
describe(grades$final)
hist(grades$final, main = "Histogram of final", xlab = "Final Marks", ylab = "Frequencys", col = "Blue")
boxplot(grades$final,  main = "Box plot of Final", xlab = "Fianl", col = "Blue", horizontal = T)
stem(grades$final)
summary(grades$total)
describe(grades$total)
hist(grades$total, main = "Histogram of Total Marks", xlab = "Total Marks", ylab = "Frequency", col = "Blue")
boxplot(grades$total,  main = "Box plot of Total Marks", xlab = "Total", col = "Blue", horizontal = T)
stem(grades$total)

#2.	How predictor/s is related to response variable (final)? [hint: first plat scatter diagram followed by correlation test]
#Present diagram/s and correlations in the following space. Before diagrams explain relationship in 3 or 4 lines.
plot(final~gpa, data = grades, main= "Scatter plot of GPA vs Final", col= 'blue', xlab= 'GPA', ylab="Final")
plot(final~quiz1, data = grades, main= "Scatter plot of Quiz1 vs Final",col= 'blue', ylab= 'Fianal', xlab="Quiz1")
plot(final~quiz2, data = grades, main= "Scatter plot of Quiz2 vs Final",col= 'blue', ylab= 'Fianal', xlab="Quiz2")
plot(final~quiz3, data = grades, main= "Scatter plot of Quiz3 vs Final",col= 'blue', ylab= 'Fianal', xlab="Quiz3")
plot(final~quiz4, data = grades, main= "Scatter plot of Quiz4 vs Final",col= 'blue', ylab= 'Fianal', xlab="Quiz4")
plot(final~quiz5, data = grades, main= "Scatter plot of Quiz5 vs Final",col= 'blue', ylab= 'Fianal', xlab="Quiz5")
plot(final~total, data = grades, main= "Scatter plot of Total vs Final",col= 'blue', ylab= 'Fianal', xlab="Total")
cor.test(grades$final, grades$gpa)
cor.test(grades$final, grades$quiz1)
cor.test(grades$final, grades$quiz2)
cor.test(grades$final, grades$quiz3)
cor.test(grades$final, grades$quiz4)
cor.test(grades$final, grades$quiz5)
cor.test(grades$final, grades$total)

cor(grades$final, grades$gpa)
cor(grades$final, grades$quiz1)
cor(grades$final, grades$quiz2)
cor(grades$final, grades$quiz3)
cor(grades$final, grades$quiz4)
cor(grades$final, grades$quiz5)
cor(grades$final, grades$total)
#out put cor for above commands revelas the highly corelated variables are final and quiz3
model1<- lm(final ~ gpa+quiz1+quiz2+quiz3+quiz4+quiz5+total, data=grades)
model1
summary(model1)

model2<-lm(final ~ quiz1+quiz2+quiz3+quiz4+quiz5+total, data=grades)
model2
summary(model2)
#out put cor for above commands revelas the highly corelated variables are final and quiz3
vif(model2)
vif(model2)>5
#as Quiz1 and total variables are highly correlated among themselves so these 
#two variables were dropped and new model is model3 as follows.
model3<-lm(final ~ quiz2+quiz3+quiz4+quiz5, data=grades)
model3
summary(model3)
#From R output of summary(model3) it was found that in this model there is no significant slope for quiz2,quiz4,quiz5
#so, new model is model4 as follows
model4<-lm(final ~ quiz3, data=grades)
model4
summary(model4)
#lets check anova
Anova(model4)
#let check Durbin watson statistics valur for model4
durbinWatsonTest(model4)
plot(model4)
abline(model4)
summary(model4)
predict(model4)
residual<-residuals(model4)
residual
summary(residual)
plot()
scatterplot(final~quiz3|ethnicity, data=grades)
#Data Transformation
#Lets slice and dice data in to different visualizations patterns
Gen_1<-subset(grades, gender==1)
Gen_2<-subset(grades, gender==2)
summary(Gen_1$final)
summary(Gen_2$final)
ethn_1<-subset(grades, ethnicity==1)
ethn_2<-subset(grades, ethnicity==2)
ethn_3<-subset(grades, ethnicity==3)
ethn_4<-subset(grades, ethnicity==4)
ethn_5<-subset(grades, ethnicity==5)
summary(ethn_1$final)
summary(ethn_2$final)
summary(ethn_3$final)
summary(ethn_4$final)
summary(ethn_5$final)
#color by final
rg<-colorRampPalette(c("red", "green"))(105)
final_plot<-scatterplot3d(z=grades$final, y=grades$gpa, x=grades$quiz3, color =rg,
                          xlab = "Quiz3",
                          ylab = "GPA",  
                          zlab = "Final", 
                          main="3D Scatter Plot")
