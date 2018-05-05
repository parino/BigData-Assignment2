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

#ACC
acc <- performance(pr, measure = "acc")
acc <- max(acc@y.values[[1]])
acc

#stepwise selection

names(df_tr)
attach(df_tr)
fit1 <- glm(BAD~LOAN+MORTDUE+VALUE+REASON+JOB+YOJ+DEROG+DELINQ+CLAGE+NINQ+CLNO+DEBTINC ,family=binomial , dat=df_tr)
summary(fit1)
fitempty<- glm(BAD ~1 , family=binomial , data =df_tr )
summary(fitempty)
bothways=step(fitempty , list (lower=formula(fitempty) ,upper=formula(fit1)) ,
                direction="both")
backwards=step(fit1)
forwards=step(fitempty,scope=list(lower=formula(fitempty),upper=formula(fit1)) ,
                direction="forward") 
formula(backwards)
formula(forwards) 
formula(bothways) 

fit2 <- glm(BAD~LOAN+MORTDUE+VALUE+REASON+JOB+DEROG+DELINQ+CLAGE+NINQ+CLNO+DEBTINC,family=binomial , dat=df_tr)
summary(fit2)
summary(fit1)

library(lmtest)
lrtest(fit2,fit1)
##################################### Decision tree #####################################

#basic tree#
treeimb <- rpart(BAD ~ .,data = df_tr)
treeimb
prob.treeimb <- predict(treeimb, newdata=df_ts , type="prob")
pred.treeimb <- prediction(prob.treeimb[,'1'], df_ts$BAD)
perf.treeimb <- performance(pred.treeimb, measure="tpr", x.measure="fpr")
plot(perf.treeimb)
auc.treeimb <- performance(pred.treeimb, measure="auc")
auc.treeimb <- auc.treeimb@y.values[[1]]
auc.treeimb

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

prob.tree1 <- predict(tree_5, newdata=df_ts , type="prob")
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

# dealing with class imbalance problem #
table(df_tr$BAD)
#undersample
df_under <- ovun.sample(BAD ~ ., data = df_tr, method = "under",
                        p = 0.5, seed = 1)$data
table(df_under$BAD)
#oversample
df_over <- ovun.sample(BAD ~ ., data = df_tr, method = "over",
                       p = 0.5, seed = 1)$data
table(df_over$BAD)

# tree for undersample #
tree.under <-rpart (BAD  ~ . , data= )
plot(tree.under)
prp(tree.under)
fancyRpartPlot(tree.under)
printcp(tree.under)
plotcp(tree.under)
summary(tree.under)

