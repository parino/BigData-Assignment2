rm(list = ls())

#libraries
library(pastecs)
library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(PerformanceAnalytics)
library(corrplot)
library(VIM)
library(DMwR)

#options
options(digits=2)
options(scipen=6)

df <- fread("C:/Users/u0117439/Documents/BigData-Assignment2/hmeq.csv",sep=",",header = T)

df<-df %>%  mutate_if(is.character, as.factor) 

head(df)
str(df)


########################## Exploratory analysis ###############################

#descriptive statistics
summary(df[,c("BAD","LOAN","MORTDUE","VALUE","YOJ","DEROG",
                "DELINQ","CLAGE","NINQ","CLNO","DEBTINC")])
stat.desc(df) 

table(df$BAD)
table(df$REASON);table(df$JOB)
xtabs(~BAD + REASON, data = df)
xtabs(~BAD + JOB, data = df)

#visual exploration: boxplots
myplot_fun <- function(data,x,y,laby){
  p <- ggplot(data,aes(x, y))
  p <- p + geom_boxplot() + labs(x="BAD",y=laby)
  p <- p + theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1))
  return(p) 
}

p_LOAN<-myplot_fun(df,factor(df$BAD),df$LOAN,laby="LOAN")
p_MORTDUE<-myplot_fun(df,factor(df$BAD),df$MORTDUE,laby="MORTDUE")
p_VALUE<-myplot_fun(df,factor(df$BAD),df$VALUE,laby="VALUE")
p_YOG<-myplot_fun(df,factor(df$BAD),df$YOJ,laby="YOJ")
p_DEROG<-myplot_fun(df,factor(df$BAD),df$DEROG,laby="DEROG")
p_DELINQ<-myplot_fun(df,factor(df$BAD),df$DELINQ,laby="DELINQ")
p_CLAGE<-myplot_fun(df,factor(df$BAD),df$CLAGE,laby="CLAGE")
p_NINQ<-myplot_fun(df,factor(df$BAD),df$NINQ,laby="NINQ")
p_CLNO<-myplot_fun(df,factor(df$BAD),df$CLNO,laby="CLNO")
p_DEBTINC<-myplot_fun(df,factor(df$BAD),df$DEBTINC,laby="DEBTINC")
grid.arrange(p_LOAN, p_MORTDUE, p_VALUE,p_YOG,p_DEROG,p_DELINQ,
             p_CLAGE,p_NINQ,p_CLNO,p_DEBTINC,ncol=5,nrow=2)


#Correlation of continuous variables
chart.Correlation(df[,c("LOAN","MORTDUE","VALUE","YOJ","CLAGE","DEBTINC")], histogram=TRUE, pch=19)

#Correlation of continuous and count variables
chart.Correlation(df[,c("LOAN","MORTDUE","VALUE","YOJ","DEROG","DELINQ",
                        "CLAGE","NINQ","CLNO","DEBTINC")], histogram=TRUE, pch=19)

#fancier
cor<-cor(df[,c("LOAN","MORTDUE","VALUE","YOJ","DEROG","DELINQ",
             "CLAGE","NINQ","CLNO","DEBTINC")],use = "complete.obs")
corrplot(cor)


###################################### Outliers ###############################

#Univariate ouliers
#only 2 most extreme observations of CLAGE seem mistakes
df$CLAGE[df$CLAGE>1000]<-NA


############################# Missing data ########################

#Visual representation
aggr_plot <- aggr(df, col=c('navyblue','red'), sortVars=TRUE, numbers=TRUE, labels=names(df), cex.axis=.5, gap=2, ylab=c("Histogram of missing data","Pattern"))

#mode-imputation of categorical variables
df[df$REASON=="",]$REASON<-"DebtCon"
df$REASON<-droplevels(df$REASON)
df[df$JOB=="",]$JOB<-"Other"
df$JOB<-droplevels(df$JOB)


#Split dataset 
#70-30 split
set.seed(123); index = sample(1:nrow(df), size = floor(nrow(df)*0.70))
df_tr = df[index,]
df_ts = df[-index,]


#median imputation of training set
num_cols <- names(df_tr)[sapply(df_tr, is.numeric)]
for(col in num_cols) {
  set(df_tr, i = which(is.na(df_tr[[col]])), j = col, value = median(df_tr[[col]], na.rm=TRUE))
}

#median imputation of test set (with training median)
num_cols_test <- names(df_ts)[sapply(df_ts, is.numeric)]
for(col in num_cols_test) {
  set(df_ts, i = which(is.na(df_ts[[col]])), j = col, value = median(df_tr[[col]], na.rm=TRUE))
}


#save pre-processed datasets
write.table(df_tr,"C:/Users/u0117439/Documents/BigData-Assignment2/df_tr.csv",sep=";",row.names = F)
write.table(df_ts,"C:/Users/u0117439/Documents/BigData-Assignment2/df_ts.csv",sep=";",row.names = F)




