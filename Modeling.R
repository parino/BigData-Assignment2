rm(list = ls())

#libraries
library(data.table)
library(dplyr)
library(car)
library(MuMIn)
library(lmtest)
library(caret)
library(C50)
library(ROCR)
library(randomForest)


#read pre-processed training and test datasets
df_tr <- fread("C:/Users/u0117439/Documents/BigData-Assignment2/df_tr.csv",sep=";",header = T)
df_ts <- fread("C:/Users/u0117439/Documents/BigData-Assignment2/df_ts.csv",sep=";",header = T)

df_tr<-df_tr %>%  mutate_if(is.character, as.factor) 
df_ts<-df_ts %>%  mutate_if(is.character, as.factor) 
df_tr$BAD <- as.factor(df_tr$BAD)
df_ts$BAD <- as.factor(df_ts$BAD)


###################################### Logistic regression ###############################

fit.full<-glm(BAD~.,data = df_tr,family="binomial",na.action = "na.fail")
summary(fit.full)

fit<-glm(BAD~.^3,data = df_tr,family="binomial",na.action = "na.fail")
summary(fit)


#Variance inflation factors
vif(fit.full) #no multicollinearity

#model selection
ms <- dredge(fit.full,rank="AICc")
ms.bic <- dredge(fit.full,rank="BIC")

fit1 <- glm(BAD~.-YOJ,data = df_tr,family="binomial",na.action = "na.fail") #best model by AIC
summary(fit1)

fit2 <- glm(BAD~.-MORTDUE-REASON-VALUE-YOJ,data = df_tr,family="binomial",na.action = "na.fail") #best model by BIC
summary(fit2)

#Compare models
lrtest(fit1,fit2)
lrtest(fit1,fit.full)

x_train <- model.matrix( ~ .-1, train[,features])
lm = cv.glmnet(x=x_train,y = as.factor(train$y), intercept=FALSE ,family =   "binomial", alpha=1, nfolds=7)
best_lambda <- lm$lambda[which.min(lm$cvm)]


library(glmnet)
x.train <- model.matrix(~.-1,df_tr[,-1])
cv.lasso.fit <- cv.glmnet(x = x.train, y = df_tr$BAD, 
                          family = "binomial", alpha = 0, nfolds = 10)
best_lambda <- cv.lasso.fit$lambda[which.min(cv.lasso.fit$cvm)]

cv.lasso.fit <- glmnet(x = x.train, y = df_tr$BAD, 
                          family = "binomial", alpha = 0, lambda=best_lambda, nfolds = 10)


#Logistic regression (change fit.full for fit 1 also)
#ROC curve
p <- predict(model, newdata=df_ts, type="prob")
pr <- prediction(p[,2], df_ts$BAD)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf);abline(a=0,b=1)

#AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc



# define training control
train_control<- trainControl(method="cv", number=10)

# train the model 
model<- train(BAD~., data=df_tr, trControl=train_control, method="glm", family=binomial())
summary(model)


############################ Decison tree: C5.0 ##################################

#basic c5.0 #
tree_1 <- C5.0(df_tr$BAD~., data=df_tr)
summary(tree_1)
plot(tree_1)

# auc=0.87


# c5.0 with winnowing 
tree_2 <- C5.0(df_tr$BAD ~ ., data = df_tr, control = C5.0Control(winnow = TRUE))
summary(tree_2)
plot(tree_2)

# auc=0.873

# c5.0 with pruning control
tree_3 <-C5.0(df_tr$BAD ~ ., data = df_tr, control = C5.0Control(CF = .01))
summary(tree_3)
plot(tree_3)

# auc=0.86

# c5.0 with min cases control
tree_4 <-C5.0(df_tr$BAD ~ ., data = df_tr, control = C5.0Control(minCases = 150))
summary(tree_4)
plot(tree_4)

# auc=0.84

#prediction not working#
predmod<- predict(tree_1 , df_ts[,-1], type="prob")
str(predmod)
pred <- prediction(predictions = predmod, labels = df_ts$BAD)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")


# c5.0 with rules #
tree_5 <- C5.0(x=df_tr[,-1], y=as.factor(df_tr$BAD), rules=TRUE)
summary(tree_5)

# auc=0.83

# boosted tree # 
tree_boost <- C5.0(x = df_tr[,-1], y = as.factor(df_tr$BAD), trials = 30)
summary(tree_boost)
plot(tree_boost)

#auc calculation

prob.tree1 <- predict(tree_3, newdata=df_ts , type="prob")
pred.tree1 <- prediction(prob.tree1[,'1'], df_ts$BAD)
perf.tree1 <- performance(pred.tree1, measure="tpr", x.measure="fpr")
plot(perf.tree1)
auc.tree1 <- performance(pred.tree1, measure="auc")
auc.tree1 <- auc.tree1@y.values[[1]]
auc.tree1

#accuracy
acc.tree1 <- performance(pred.tree1, measure="acc")
acc.tree1 <- max(acc.tree1@y.values[[1]])
acc.tree1

# error rate
err.tree1 <- performance(pred.tree1, measure="err")
err.tree1 <- ave(err.tree1@y.values[[1]])
err.tree1


prob.tree1 <- predict(tree_5, newdata=df_ts , type="prob")
pred.tree1 <- prediction(prob.tree1[,'1'], df_ts$BAD)
perf.tree1 <- performance(pred.tree1, measure="tpr", x.measure="fpr")
plot(perf.tree1)
auc.tree1 <- performance(pred.tree1, measure="auc")
auc.tree1 <- auc.tree1@y.values[[1]]
auc.tree1


################################ Random Forest ###################################

set.seed(123)
rf<- randomForest(BAD~.,data=df_tr, importance = TRUE, ntree = 1000)
rf
#variable importance
varImpPlot(rf)

set.seed(123)
cv10_1 <- createMultiFolds(df_tr$BAD, k = 10, times = 10)

# Set up caret's trainControl object per above.
ctrl_1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv10_1)

rf.5<- train(x = df_tr[,-1], y = df_tr[,1], method = "rf", tuneLength = 3,
             ntree = 1000, trControl =ctrl_1)

rf.5


###Lets Predict the test data 

pr.rf=predict(rf.5,newdata = df_ts)
confusionMatrix(pr.rf,df_ts$BAD)


result.predicted.prob <- predict(rf.5, df_ts, type="prob") # Prediction


p <- predict(rf.5, newdata=df_ts, type="prob")
pr <- prediction(p[,2], df_ts$BAD)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf);abline(a=0,b=1)

#AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc






pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels )
pred2 <- prediction(abs(ROCR.simple$predictions + 
                          rnorm(length(ROCR.simple$predictions), 0, 0.1)), 
                    ROCR.simple$labels)
perf <- performance( pred, "tpr", "fpr" )
perf2 <- performance(pred2, "tpr", "fpr")




result.roc <- roc(df_ts$BAD, result.predicted.prob$versicolor) # Draw ROC curve.
plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")

result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)#to get threshold and accuracy



rf.roc<-roc(df_ts$BAD,rf.5$votes[,2])
plot(rf.roc)
auc(rf.roc)




library(ROCR)
predictions=as.vector(rf$votes[,2])
pred=prediction(predictions,df_ts$BAD)

perf_AUC=performance(pred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]

perf_ROC=performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))


p <- predict(rf, newdata=df_ts, type="vote")
pr <- prediction(p[,2], df_ts$BAD)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf);abline(a=0,b=1)

#AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


plot( prf, colorize = TRUE)
plot(prf2, add = TRUE, colorize = TRUE)
plot(perf.tree1, add = TRUE, colorize = TRUE)

