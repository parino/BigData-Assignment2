#Extra things I tried, not to be included finally

rm(list = ls())

#libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ROSE)
library(DMwR)
library(GGally)
library(Rtsne)
library(plotly)
library(sparcl)
library(RSKC)
library(factoextra)


#read pre-processed training and test datasets
df_tr <- fread("C:/Users/u0117439/Documents/BigData-Assignment2/df_tr.csv",sep=";",header = T)
df_ts <- fread("C:/Users/u0117439/Documents/BigData-Assignment2/df_ts.csv",sep=";",header = T)

######################## Class imbalance ###############################
# dealing with class imbalance problem

table(df_tr$BAD)

#undersample
df_under <- ovun.sample(BAD ~ ., data = df_tr, method = "under",
                        p = 0.5, seed = 1)$data
table(df_under$BAD)

#oversample
df_over <- ovun.sample(BAD ~ ., data = df_tr, method = "over",
                       p = 0.5, seed = 1)$data
table(df_over$BAD)


#SMOTE
df_tr$BAD <- as.factor(df_tr$BAD)
df_tr$REASON <- as.factor(df_tr$REASON)
df_tr$JOB <- as.factor(df_tr$JOB)

df_smote <- SMOTE(BAD ~ ., df_tr, perc.over = 100, perc.under=200)
prop.table(table(df_smote$BAD))


###################################### Clustering: kmeans ###################################

df_cont<-df_tr[,c(2:4,7:13)]

bss <- numeric()
wss <- numeric()

# Run the algorithm for different values of k 
set.seed(123)
for(i in 1:10){
  
  # For each k, calculate betweenss and tot.withinss
  bss[i] <- kmeans(df_cont, centers=i)$betweenss
  wss[i] <- kmeans(df_cont, centers=i)$tot.withinss
}

# Between-cluster sum of squares vs Choice of k
p3 <- qplot(1:10, bss, geom=c("point", "line"), 
            xlab="Number of clusters", ylab="Between-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1))

# Total within-cluster sum of squares vs Choice of k
p4 <- qplot(1:10, wss, geom=c("point", "line"),
            xlab="Number of clusters", ylab="Total within-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1))

# Subplot
grid.arrange(p3, p4, ncol=2)
#k=5 or k=3

# Execution of k-means with k=5
set.seed(123)
cluster_kmeans <- kmeans(df_cont, centers=5)

# Mean values of each cluster
aggregate(df_cont, by=list(cluster_kmeans$cluster), mean)

ggpairs(cbind(df_cont, Cluster=as.factor(cluster_kmeans$cluster)),
        columns=1:6, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both")



################################ tsne #################################

tsne_df = df_tr %>% dplyr::select_if(is.numeric) %>% scale() %>% as_data_frame()
tsne_df = tsne_df[,-1]

set.seed(123); tsne = Rtsne(tsne_df,dims = 2, 
                            perplexity = 30, verbose = TRUE,
                            check_duplicates = F, max_iter = 5000)
set.seed(123); tsne_3D = Rtsne(tsne_df,dims = 3, 
                               perplexity = 30, verbose = TRUE,
                               check_duplicates = F, max_iter = 5000)

tsne_2D_tr = as_data_frame(tsne$Y)
tsne_3D_tr = as_data_frame(tsne_3D$Y)
# Plots t-SNE 2D
ggplot(bind_cols(PROB=df_tr$BAD,tsne_2D_tr), aes(x=V1, y=V2)) +
  geom_point(size=1, aes(col = PROB)) +
  xlab("") + ylab("") +
  ggtitle("Graph 4: t-SNE") + 
  theme(plot.title = element_text(hjust = 0.5))


# 3D Plot
plot_ly(data.table(tsne_3D_tr), x = ~V1, y = ~V2, z = ~V3, 
        color = ~df_tr$BAD, colors = c('#FFE1A1', '#683531')) %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'V1'),
                                        yaxis = list(title = 'V2'),
                                        zaxis = list(title = 'V3')))

tsne_3D

tsne_3D_tr2 <- cbind(df_tr,tsne_3D_tr)



################################ more clustering #############################

# Decide the number of clusters
df_tr_num = df_tr %>% select_if(is.numeric) %>% as.matrix() %>% scale()
set.seed(123); clest = Clest(df_tr_num, maxK = 6, alpha = 0.1, B = 10, B0 = 5, nstart = 100)

# Calculate the Regularization Parameter
set.seed(123); kperm_4 = KMeansSparseCluster.permute(df_tr_num, K=4, nperms = 5)

# Cluster
set.seed(123); rskm_4 = RSKC(d = df_tr_num, ncl = 4, alpha = 0.1, L1 = kperm_4$bestw)

# Visualization of Clusters
fviz_cluster(list(data = df_tr_num, cluster = rskm_4$labels),
             stand = FALSE, geom = "point",ellipse.type= "norm")

fviz_cluster(list(data = tsne_2D_tr, cluster = rskm_4$labels),
             stand = FALSE, geom = "point",ellipse.type = "norm")


# Important Variables for Clustering
cluster_vars = data.frame('Variables' = unlist(attributes(rskm_4$weights)),'Weights' = rskm_4$weights) %>% 
  arrange(desc(Weights)) %>% filter(Weights > 0.01)

# Interprete
cl_interprete = df_tr %>% dplyr::select(as.character(cluster_vars$Variables))
cl_interprete_all = df_tr
cl_interprete_all$cluster = as.factor(rskm_4$labels)
cl_interprete$cluster = as.factor(rskm_4$labels)
cl_interprete = dplyr::select(cl_interprete, cluster, everything())

profile = apply(cl_interprete[,-1], 2, function(x) tapply(x, cl_interprete$cluster, median_mode))

table(rskm_4$labels)


# Add cluster into df
df_tr$labels = as.factor(rskm_4$labels)

