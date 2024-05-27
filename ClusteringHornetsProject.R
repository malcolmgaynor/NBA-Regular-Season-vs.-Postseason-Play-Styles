#Clustering code for Hornets Project

#Import data
Partone <- read.csv("~/Downloads/Part1.csv")
Partone <- read.csv("~/Downloads/Part2.csv")
Partone <- read.csv("~/Downloads/Part3a.csv")
Partone <- read.csv("~/Downloads/Part3b.csv")
Partone <- read.csv("~/Downloads/Part3c.csv")
Partone <- read.csv("~/Downloads/Part3d.csv")
Partone <- read.csv("~/Downloads/Part3e.csv")
Partone <- read.csv("~/Downloads/Part4.csv")



#K-means clustering
# Loading package 
library(ClusterR) 
library(cluster) 
library(factoextra)


#Create subset of data for clustering
subsetclust1 <- Partone[, c("FGA", "X3PA", "X2PA", "FTA", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "Age", "Pace", "FTr", "Dist.", "X.FG..2P", "X.FG..0.3", "X.FG..3.10", "X.FG..10.16", "X.FG..16.3P", "X.FG..3P", "X.FG..Ast.d..2P", "X.FG.AST.d..3P", "X.FGA.Dunk", "X.3PA.Corner", "Bench.MIN", "Bench.FGA")]

#Elbow Method for finding the optimal number of clusters
fviz_nbclust(
  subsetclust1,
  kmeans,
  method = "wss",
  k.max = 25,
  verbose = FALSE
)

# Fitting K-Means clustering Model  
# to training dataset 
set.seed(240) # Setting seed 
kmeans.re <- kmeans(subsetclust1, centers = 3, nstart = 20) 
kmeans.re 

#Adding cluster numbers to initial dataset
Data2 <- Partone
Data2$subsetclust1 <- kmeans.re$cluster

# Cluster identification for  
# each observation 
kmeans.re$cluster 


Clusterdata <- Clusterdata %>%
  mutate(Cluster = kmeans.re$cluster) 

# Confusion Matrix 
cm <- table(subsetclust1$Venue, kmeans.re$cluster) 
cm 

## Visualizing clusters 
y_kmeans <- kmeans.re$cluster 
clusplot(subsetclust1[, c("variable", "variable")], 
         y_kmeans, 
         lines = 0, 
         shade = TRUE, 
         color = TRUE, 
         labels = 4, 
         plotchar = FALSE, 
         span = TRUE, 
         main = paste("Cluster Groupings"), 
         xlab = 'variable', 
         ylab = 'AwayFGpct') 

#Export data with cluster number attached to each observation
write.csv(Data2, file = "CLUSTEREDall.csv")
