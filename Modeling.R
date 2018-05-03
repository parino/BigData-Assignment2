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

#read pre-processed training and test datasets
df <- fread("C:/Users/u0117439/Documents/BigData-Assignment2/df_tr.csv",sep=";",header = T)
df_ts <- fread("C:/Users/u0117439/Documents/BigData-Assignment2/df_ts.csv",sep=";",header = T)


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

pred.treeimb <- predict(treeimb, newdata = df_ts)
head(pred.treeimb)
# plots #
prp(treeimb)
fancyRpartPlot(treeimb)
printcp(treeimb)
plotcp(treeimb)
summary(treeimb)

#basic c5.0 #
tree_mod <- C5.0(df_tr[,-1], as.factor(df_tr$BAD),controL=C5.0Control(winnow = TRUE))
summary(tree_mod)
plot(tree_mod)
#prediction not working#
predmod<- predict(tree_mod , df_ts[,-1], type="prob")
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

# dealing with class imbalance problem #
table(df_tr$BAD )
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

