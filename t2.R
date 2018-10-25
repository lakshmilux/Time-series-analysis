library(forecast)
library(tseries)
library(zoo)
library(xts)
library(lubridate)
library(Metrics)
library(quantmod)
library(caret)

candy <- read.csv(file.choose(),stringsAsFactors = F)


# Take a look at the class of the dataset US_candy_production
str(candy)

# Assign more meaningful variable names
colnames(candy)<-c("Period","candy_production")

# Convert data into time series dataset
attach(candy)

candyts<-ts(candy_production,c(1972,1),c(2017,8),12)
str(candyts)

#skewness(candyts)
plot(candyts)
abline(reg = lm(candyts~ time(candyts)),col="red")

cycle(candyts)
plot(aggregate(candyts,FUN = mean))#to display the trend


boxplot(candyts ~ cycle(candyts))


#candy.end <- floor(0.8*nrow(candy)) #select the first 80% of the data
#candy.train <- candy[1:candy.end,] #assign the first 80% of the data to the train set
#candy.test <- candy[(candy.end+1):nrow(candy),]

#horizon = nrow(candy.test)
#tr = ts(candy.train$candy_production,c(1972,1),c(2008,6),12)
#tes =ts(candy.train$candy_production,c(2008,7),c(2017,8),12)

index = 1:nrow(candy)
trainindex= createDataPartition(index,p=0.75,list=FALSE)

##process class sets as data frames
training = as.data.frame(candy[trainindex,])
rownames(training) = NULL
testing = as.data.frame(candy[-trainindex,])
rownames(testing) = NULL



cycle(tr)
plot(aggregate(tr,FUN = mean))#to display the trend


boxplot(tr ~ cycle(tr)



decomposetr <- decompose(tr,"multiplicative")
autoplot(decomposetr)

#adf.test(tr)#series is not stationary as p>0.05
adf.test(diff(log(tr)))#for multiplicative model

#acf(tr)
acf(diff(log(tr)))#for non-stationary series
pacf(diff(log(tr)))


tr.es <- HoltWinters(tr,beta=TRUE, gamma=TRUE)
tr.es
tr.es$fitted
plot(tr.es)
tr.es$SSE

tr%>% 
  HoltWinters(beta = TRUE, gamma = TRUE) %>% 
  forecast(h=horizon) %>% 
  plot()
lines(tes, col = "red")

#autoforecast
tr%>% 
  forecast(h=horizon) %>% 
  plot()
lines(tes, col = "red")

tr%>% 
  HoltWinters(beta = TRUE, gamma = TRUE) %>% 
  forecast(h=5,level = c(90,95))

tr%>% 
  HoltWinters(beta = TRUE, gamma = TRUE) %>% 
  forecast(h=5,level = c(90,95)) %>% 
  autoplot()

p = predict(tr.es,h=horizon,prediction.interval = T)
plot(tr.es,p)
accuracy(p,tes)

arimatr <- auto.arima(tr)
arimatr

library(e1071)
library(devtools)
install_github("ltorgo/performanceEstimation",ref="develop")
library(performanceEstimation)
library(DMwR)


                            
svmodel = svm(candy_production~.,data = candy.train)
summary(svmodel)
plot(svmodel,candy.train,col="red")

p = predict(svmodel,candy.test[,-1])
plot(candy.train$candy_production,p,col="red",pch=4)

error = candy.test$candy_production - p
rmse =sqrt(mean(error)^2)
rmse

                   
svmRbftune <- train(candy_production~., 
                    data = candy.train, method = "svmRadial",
                    tunelength = 14, trControl = trainControl(method ="cv"))


type="eps-regression" ##regression
u= -2 ## -3,-2,-1,0,1,2,3
gam=10^{u}; w= 4.5 ##1.5,-1,0.5,2,3,4
cost=10^{w}
##The higher the cost produce less support vectors, increases accuracy
##However we may overfit
OptModelsvm=tune(svm, candy_production~., data=training,
                 ranges=list(epsilon=seq(0,1,0.1)),cost = 2^seq(0.5,8,0.5))

