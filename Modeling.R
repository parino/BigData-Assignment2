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
INlibrary(lattice)
#read pre-processed training and test datasets
df_tr <- fread("/Users/anjabelcijan/Desktop/df_tr.csv",sep=";",header = T)
df_ts <- fread("/Users/anjabelcijan/Desktop/df_ts.csv",sep=";",header = T)


#Variance inflation factors
corx<-cor(df_tr[,c(2:4,7:13)])
vif<-diag(solve(corx))
vif

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
# acc=

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

