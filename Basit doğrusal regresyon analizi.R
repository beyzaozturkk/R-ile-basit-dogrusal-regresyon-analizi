employee_data<-read.csv('C:/Users/Burak/Desktop/Beyza/Verisetleri/employee_data.csv', header = TRUE , sep = "," , dec = ".")
model_data <- employee_data[c("Salary" ,"Experience")]
library(formattable)
formattable(model_data)
# random sample 
set.seed(145)
sample_Index <- sample(1:nrow(model_data) , size = 0.8*nrow(model_data))
train_Set <- model_data[sample_Index , ]
test_Set <- model_data[-sample_Index , ]
nrow(train_Set)
nrow(test_Set)
cor(train_Set)
hist(train_Set$Salary)
hist(train_Set$Experience)
boxplot(train_Set$Salary,horizontal = TRUE)
boxplot(train_Set$Experience,horizontal =TRUE)
library(ggplot2)
fig  <- ggplot(data  = train_Set , aes(x = Experience , y =  Salary)) +
  geom_point(size = 2) + 
  ylab("Maa??") + xlab("Deneyim")
fig       
library(outliers)
scores <- scores(train_Set , type = "z" , prob = 0.95)
any_True <- apply(scores , 1 , FUN = function(x) { any(x) }  )
which(any_True)
index <- which(any_True)
trainSetRemout <- train_Set[-index , ]
nrow(train_Set)
nrow(trainSetRemout)
cor(trainSetRemout)
hist(trainSetRemout$Salary)
hist(trainSetRemout$Experience)
boxplot(trainSetRemout$Salary,horizontal = TRUE)
boxplot(trainSetRemout$Experience,horizontal =TRUE)
fig2  <- ggplot(data  = trainSetRemout , aes(x = Experience , y =  Salary)) +
  geom_point(size = 2) + 
  ylab("Maa??") + xlab("Deneyim")
fig2
# Kayip gozlem kontrolu
library(mice)
md.pattern(train_Set)
### Model Olusturma ve Degerlendirme
model1 <- lm(Salary ~ Experience , data  = train_Set)
model2 <- lm(Salary ~ Experience , data  = trainSetRemout)
summary(model1)
summary(model2)
AIC(model1)
BIC(model1)
AIC(model2)
BIC(model2)
## Prediction
model1_Pred <- predict(model1, test_Set)
model2_Pred <- predict(model2, test_Set)
model1_PredData <- data.frame("actuals" = test_Set$Salary , "predictions" = model1_Pred)
model2_PredData <- data.frame("actuals" = test_Set$Salary , "predictions" = model2_Pred)
formattable(model1_PredData)
formattable(model2_PredData)
model1_Hata <- model1_PredData$actuals - model1_PredData$predictions
model2_Hata <- model2_PredData$actuals - model2_PredData$predictions
mse1 <- sum(model1_Hata^2) / nrow(model1_PredData)
mse2 <- sum(model2_Hata^2) / nrow(model2_PredData)
sqrt(mse1);sqrt(mse2)
## R2 RMSE VE MAE
library(caret)
R2(model1_PredData$predictions , model1_PredData$actuals )
R2(model2_PredData$predictions , model2_PredData$actuals )
#RMSE,K??k Ortalama Kare Hatas??, bir regresyon modelinin tahminlerinin ger??ek de??erlerden ne kadar uzakla??t??????n?? ??l??en bir hata metrikidir.
#D??????k olmas?? beklenir
RMSE(model1_PredData$predictions , model1_PredData$actuals )
RMSE(model2_PredData$predictions , model2_PredData$actuals )
MAE(model1_PredData$predictions , model1_PredData$actuals )
MAE(model2_PredData$predictions , model2_PredData$actuals )
##MAE,Ortalama Mutlak Hata, regresyon analizinde kullan??lan bir ba??ka hata metrikidir. 
#RMSE gibi, modelin tahminlerinin ger??ek de??erlerden ne kadar uzakla??t??????n?? ??l??er. 
#Ancak RMSE'den farkl?? olarak, hatalar??n mutlak de??erlerinin ortalamas??n?? al??r.

## Min - Max Accuracy 
model1_Accur <- mean(apply(model1_PredData , 1 , min) / apply(model1_PredData , 1 , max) )
model1_Accur
model2_Accur <- mean(apply(model2_PredData , 1 , min) / apply(model2_PredData , 1 , max) )
model2_Accur
## Mean Absolute  Percentage Error (MAPE)
#MAPE, ??zellikle tahminlerin do??rulu??unun y??zde olarak ifade edilmesi ??nemli oldu??unda kullan??l??r.

model1_MAPE <- mean( abs(model1_PredData$actuals - model1_PredData$predictions) /  
                      model1_PredData$actuals)
model2_MAPE <- mean( abs(model2_PredData$actuals - model2_PredData$predictions) /  
                      model2_PredData$actuals)
model1_MAPE;model_2MAPE

### K-Fold Cross Validation #################
#K-Fold Cross Validation, makine ????renmesi modellerinin performans??n?? de??erlendirmek i??in g????l?? ve yayg??n olarak kullan??lan bir tekniktir. 
#Modelin genelleme yetene??ini ??l??erek, daha g??venilir sonu??lar elde edilmesini sa??lar.
library(caret)

train.control <- trainControl(method = "cv" , number  = 10 , verboseIter = TRUE)

model1CV <- train(Salary ~ Experience , data = train_Set , method = "lm" ,
                  trControl = train.control
)
model2CV <- train(Salary ~ Experience, data = trainSetRemout , method = "lm" ,
                  trControl = train.control
)
model1CV
model2CV

model1CVPred <- predict(model1CV , test_Set)
model2CVPred <- predict(model2CV , test_Set)

caret::R2(model1CVPred , test_Set$Salary)
caret::R2(model2CVPred , test_Set$Salary)
caret::RMSE(model1CVPred , test_Set$Salary)
caret::RMSE(model2CVPred , test_Set$Salary)
caret::MAE(model1CVPred , test_Set$Salary)
caret::MAE(model2CVPred , test_Set$Salary)
