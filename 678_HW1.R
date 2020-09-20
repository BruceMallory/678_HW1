#Bruce Mallory
#678 HW1
# Exercise 7.2
a<-5
b<-7
set.seed(678)
x<-runif(100,0,50)
e<-rnorm(100,0,3)
y<-a + b*x + e
#putting x and y into a data frame (to use in 7.3, becaue stan_glm wants the 
#data to be in a data.frame)
FakeData<-data.frame(x,y)

#(a) create the regression line using lm
yhat<-lm(y~x)
yhat

#(b) plot the data and the regression line on a scatterplot
plot(y~x)
abline(yhat, col="red")

#(c) add a descriptor on the display
text(10,300, paste("y-hat = ",round(yhat$coefficients[2],signif(2)),
     "x + ",round(yhat$coefficients[1],signif(2))), col="red")

#----------
#Exercise 7.3
#(a) Use stan_glm to do the same thing as 7.2
install.packages("rstanarm")
no
#I tried the installation first by answering Yes to the "needing compilation?" 
#and my Mac running OS 10.15.6 stalled.  Then I retried answer no and was
#successful
library(rstanarm)
yhat2<-stan_glm(y~x,data=FakeData)
yhat2

#(b) Scatterplot of Bayesian regression line

plot(y~x)
abline(yhat2, col="blue")
text(10,250, paste("y-hat2 = ",round(yhat2$coefficients[2],signif(2)),
                   "x + ",round(yhat2$coefficients[1],signif(2))), col="blue")
#Q: "What does 'best-fit' mean in this context?"
#A: best-fit" means that the linear model that is being used was "scored"
#and the given model has the lowest score.  In a traditional least-squares
#model the score is the some of the squared residuals.  The smaller the sum, the
#better the model fits the data.  The stan_glm is using the same scoring
#metric (I think), but is using Baysian methods to try to get a better fit.  In
#the work I have here, the model computed with lm() and stan_lm() are identical
#to the hundredths place.

#----------
#Exercise 7.6
ElecData<-read.table("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/ElectionsEconomy/data/hibbs.dat")
#With my import of the data, the first row of the data frame was the column headers,
#so I found the following code to make row1 = column headers and remove row1
library(tidyverse)
ElecData2<-as.data.frame(ElecData)
names(ElecData2) <- ElecData2 %>% slice(1) %>% unlist()
ElecData2 <- ElecData2 %>% slice(-1)

#Creating binary predictor x=0 (growth<2%), x=1 (growth>2%)
ElecData2<-mutate(ElecData2, predictor=ifelse(growth<2,0,1))
#Converting the vote column into numeric
ElecData2$vote <- as.numeric(as.character(ElecData2$vote))
#(a) Calculating the average for the growth<2% group and growth>2% group
mean0<-mean(ElecData2$vote[ElecData2$predictor==0])
mean1<-mean(ElecData2$vote[ElecData2$predictor==1])
t.test(ElecData2$vote[ElecData2$predictor==0],ElecData2$vote[ElecData2$predictor==1])
var0<-var(ElecData2$vote[ElecData2$predictor==0])
var1<-var(ElecData2$vote[ElecData2$predictor==1])
n0<-length(ElecData2$vote[ElecData2$predictor==0])
n1<-length(ElecData2$vote[ElecData2$predictor==1])
num<-var0*(n0-1)+var1*(n1-1)
denom<-n0 + n1 -2
SE<-sqrt(num/denom)
 #(b) Regression of vote share on the binary "predictor"
yhat3<-stan_glm(vote~predictor, data=ElecData2,prior_intercept=NULL,prior=NULL,prior_aux=NULL)
print(yhat3)

#Doing the t-test, the mean's were 49.3% & 54.8%.  Doing stan_glm on the indicator
#variable (predictor), gave a intercept of 49.4% (that would be mean0) with a 
#slope of 5.5% making mean1=54.8%.  The SE for the t-test was 5.0.  The SE for the
#indicator regression was 5.3.  Yup - they're close!


