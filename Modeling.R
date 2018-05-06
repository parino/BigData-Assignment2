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

#Variance inflation factors
vif(fit.full) #no multicollinearity

#model selection
ms.aicc <- dredge(fit.full,rank="AICc")
ms.bic <- dredge(fit.full,rank="BIC")

fit1 <- glm(BAD~.-YOJ,data = df_tr,family="binomial",na.action = "na.fail") #best model by AIC
summary(fit1)

fit2 <- glm(BAD~.-MORTDUE-REASON-VALUE-YOJ,data = df_tr,family="binomial",na.action = "na.fail") #best model by BIC
summary(fit2)

#Compare models
lrtest(fit1,fit2)
lrtest(fit1,fit.full)

#ROC curve
p.logit <- predict(fit1, newdata=df_ts, type="response")
pr.logit <- prediction(p.logit, df_ts$BAD)
prf.logit <- performance(pr.logit, measure = "tpr", x.measure = "fpr")
plot(prf.logit);abline(a=0,b=1)

#AUC
auc.logit <- performance(pr.logit, measure = "auc")@y.values[[1]]
auc.logit


#With 10-fold cross validation
set.seed(123)
# define training control
train_control<- trainControl(method="cv", number=10)
# train the model 
fit1.cv<- train(BAD~.-YOJ, data=df_tr, trControl=train_control, method="glm", family=binomial())
summary(fit1.cv)


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


# c5.0 with rules #
tree_5 <- C5.0(x=df_tr[,-1], y=as.factor(df_tr$BAD), rules=TRUE)
summary(tree_5)

# auc=0.83


#auc calculation

prob.tree <- predict(tree_3, newdata=df_ts , type="prob")
pred.tree <- prediction(prob.tree[,'1'], df_ts$BAD)
perf.tree <- performance(pred.tree, measure="tpr", x.measure="fpr",cutoffs=seq(0,1,0.01))
plot(perf.tree)
auc.tree <- performance(pred.tree, measure="auc")
auc.tree <- auc.tree@y.values[[1]]
auc.tree

#accuracy
acc.tree <- performance(pred.tree, measure="acc")
acc.tree <- max(acc.tree@y.values[[1]])
acc.tree


################################ Random Forest ###################################

set.seed(123)
rf<- randomForest(BAD~.,data=df_tr, importance = TRUE, ntree = 1000)
rf

#variable importance
varImpPlot(rf)

#With 10-fold cross-validation

set.seed(123)
cv10_1 <- createMultiFolds(df_tr$BAD, k = 10, times = 10)

ctrl_1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv10_1)

rf.cv<- train(x = df_tr[,-1], y = df_tr[,1], method = "rf", tuneLength = 3,
             ntree = 1000, trControl =ctrl_1)
rf.cv


#AUC
p.rf <- predict(rf.cv, newdata=df_ts, type="prob")
pr.rf <- prediction(p.rf[,2], df_ts$BAD)
prf.rf <- performance(pr.rf, measure = "tpr", x.measure = "fpr")
plot(prf.rf);abline(a=0,b=1)

#AUC
auc.rf <- performance(pr.rf, measure = "auc")
auc.rf@y.values[[1]]


############################## Prediction performance ###################################

#confusion matrices

#logistic
confusionMatrix(data=as.factor(as.numeric(p.logit>0.5)),reference=df_ts$BAD)

#C5.0 tree
confusionMatrix(data=as.factor(as.numeric(prob.tree[,2]>0.5)),reference=df_ts$BAD)

#random forest
p.rf_raw=predict(rf.cv,newdata = df_ts,type="raw")
confusionMatrix(p.rf_raw,df_ts$BAD)


#ROC curves
plot(prf.logit,col=2)
plot(perf.tree, add = TRUE,col=3)
plot(prf.rf, add = TRUE,col=4)
abline(a=0,b=1)
legend("bottomright", legend=c("Logistic regression", "C5.0 decision tree","Random forest"),
       col=c("red", "green","blue"), lty=1, cex=0.8)

