rm(list = ls())

#libraries
library(data.table)
library(car)
library(ggplot2) #not used
library(MuMIn)
library(ROCR) 
library(ROSE) #
library(RColorBrewer) #
library(rattle)#
library(rpart)#
library(rpart.plot)#
library(C50)#
library(caret)
library(lattice)#
library(DMwR)#
library(pROC)#


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

#ROC curve
p <- predict(fit.full, newdata=df_ts, type="response")
pr <- prediction(p, df_ts$BAD)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf);abline(a=0,b=1)

#AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#model selection
ms <- dredge(fit.full,rank="AICc")

fit1 <- glm(BAD~LOAN+CLAGE+DELINQ+DEROG+NINQ+DEBTINC,data = df_tr,family="binomial",na.action = "na.fail") #2nd best model by BIC
summary(fit)

fit2 <- glm(BAD~CLAGE+DELINQ+DEROG+NINQ+DEBTINC,data = df_tr,family="binomial",na.action = "na.fail") #2nd best model by BIC
lrtest(fit1,fit.full)
library(lmtest)

############################### GAM ##############################
require(gam)
gam1 <- gam(BAD~s(LOAN)+s(MORTDUE)+s(VALUE)+REASON+JOB+s(YOJ)
            +s(DEROG)+s(DELINQ)+s(DELINQ)+s(CLAGE)+s(NINQ)
            +s(CLNO)+s(DEBTINC),data = df_tr,family="binomial")
summary(gam1)

p <- predict(gam1, newdata=df_ts, type="response")
pr <- prediction(p, df_ts$BAD)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf);abline(a=0,b=1)

#AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


##################################### Decision tree #####################################

#basic tree#
treeimb <- rpart(BAD ~ .,data = df_tr)
treeimb
pred.treeimb <- predict(treeimb, newdata = df_ts)
head(pred.treeimb)

# plots #
prp(treeimb)
fancyRpartPlot(treeimb)
printcp(treeimb)
plotcp(treeimb)
summary(treeimb)

################Using C5.0################

#make bad a factor#

df_tr$BAD <-as.factor(df_tr$BAD)
df_ts$BAD <-as.factor(df_ts$BAD)

#basic c5.0 #
tree_1 <- C5.0(df_tr[,-1], df_tr$BAD)
summary(tree_1)
plot(tree_1)

tree_1_pred  <- predict(tree_1, df_ts)
tree_1_probs <- predict(tree_1, df_ts, type ="prob")
postResample(tree_1_probs, df_ts$BAD)

#prediction not working#
predmod<- predict(tree_1 , df_ts[,-1], type="prob")
str(predmod)
pred <- prediction(predictions = predmod, labels = df_ts$BAD)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

# c5.0 with rules #
tree_mod1 <- C5.0(x=df_tr[,-1], y=as.factor(df_tr$BAD), rules=TRUE)
tree_mod1

# boosted tree # 
tree_boost <- C5.0(x = df_tr[,-1], y = as.factor(df_tr$BAD), trials = 5)
summary(tree_boost)
plot(tree_boost)


# tree for undersample #
tree.under <-rpart (BAD  ~ . , data= )
plot(tree.under)
prp(tree.under)
fancyRpartPlot(tree.under)
printcp(tree.under)
plotcp(tree.under)
summary(tree.under)



ctrl <- trainControl(method = "cv", number = 5)
tbmodel <- train(BAD ~ ., data = df_tr, method = "treebag",
                 trControl = ctrl)

predictors <- names(df_tr)[names(df_tr) != 'BAD']
pred <- predict(tbmodel$finalModel, df_ts[,predictors])

auc <- roc(df_ts$BAD, pred)
print(auc)



################################ Random Forest ###################################

library(randomForest)
set.seed(123)
rf<- randomForest(BAD~.,data=df_tr, importance = TRUE, ntree = 1000)
rf

summary(rf)
require(pROC)
rf.roc<-roc(df_ts$BAD,rf$votes[,2])
plot(rf.roc)
auc(rf.roc)

