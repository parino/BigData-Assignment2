

df <- fread("C:/Users/u0117439/Desktop/MSc Statistics/Advanced Analytics in Bussiness/Assignments/Assignment 2/hmeq.csv",
            sep=",",header = T)
df_ts <- fread("C:/Users/u0117439/Desktop/MSc Statistics/Advanced Analytics in Bussiness/Assignments/Assignment 2/hmeq.csv",
            sep=",",header = T)

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

