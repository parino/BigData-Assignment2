rm(list = ls())

#libraries
library(data.table)
library(ggplot2) #not used
library(ROCR)
library(ROSE)
library(RColorBrewer)
library(rattle)
library(rpart)
library(rpart.plot)
library(C50)
library(caret)
library(lattice)
library(DMwR)


library(pROC)


#read pre-processed training and test datasets
df_tr <- fread("C:/Users/u0117439/Documents/BigData-Assignment2/df_tr.csv",sep=";",header = T)
df_ts <- fread("C:/Users/u0117439/Documents/BigData-Assignment2/df_ts.csv",sep=";",header = T)


#Variance inflation factors
corx<-cor(df_tr[,c(2:4,7:13)])
vif<-diag(solve(corx))
vif




ctrl <- trainControl(method = "cv", number = 5)
tbmodel <- train(BAD ~ ., data = df_tr, method = "treebag",
                 trControl = ctrl)

predictors <- names(df_tr)[names(df_tr) != 'BAD']
pred <- predict(tbmodel$finalModel, df_ts[,predictors])

auc <- roc(df_ts$BAD, pred)
print(auc)

###################################### Logistic regression ###############################

fit<-glm(BAD~.,data = df_tr,family="binomial")
summary(fit)

#ROC curve
p <- predict(fit, newdata=df_ts, type="response")
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

