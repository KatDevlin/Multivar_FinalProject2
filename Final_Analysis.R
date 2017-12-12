#######################################
## Kat Devlin & Barrett Alexander
## Multivariate Statistics, Spring '17
## Final Project - Analysis Script
#######################################

# This file contains only analysis syntax. All data cleaning, merging and 
# variable recoding is located in the `Final_CleanMerge.R` file of the `Multivar_FinalProject` repo.

setwd("~/Desktop/Multivar_FinalProject2")

.packages = c("maps", "plyr", "countrycode", "WDI", "RColorBrewer", "rworldmap", 
              "TeachingDemos", "fpc", "cluster")  
.inst <- .packages %in% installed.packages()  
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst],repos='http://cran.us.r-project.org')

# Libraries for map
library(maps)
library(RColorBrewer)
library(rworldmap)

## ********************* Needs to be UPDATED!!! *******************************
# First we made a map
s <- read.csv("FinalProjectData.csv", as.is = T)
sPDF <- joinCountryData2Map(s, joinCode = "ISO3", nameJoinColumn = "iso3")
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData(sPDF, nameColumnToPlot="Q6")
mapParams <- mapCountryData(sPDF, nameColumnToPlot="Q6", addLegend=FALSE, 
                            missingCountryCol="white",oceanCol="lightblue",
                            mapTitle = "")
do.call(addMapLegend, c(mapParams, legendWidth=0.5, legendMar = 2))

###################################
# Method 1: Cluster Analysis
###################################

par(ask=FALSE)

# load relevant libraries
library(TeachingDemos)
library(fpc)
library(cluster)

source("http://www.reuningscherer.net/STAT660/R/CSQPlot.r.txt")
CSQPlot(s[,-1], label="Pew Country Mean Data")

q <- s[-c(1,39:40)]  # df with only numeric columns

# These are obviously not scientific but we liked using these to eyeball potential patterns.
# Plus the faces are goofy. 
par(mai=c(.42,0.42,0.42,0.42))
faces(q[,c(1:40)], labels=s[,1], ncol=6)
stars(q[,c(1:40)], labels=as.character(s[,1]), ncol=6)
stars(q[,c(1:40)], labels=as.character(s[,1]), ncol=8, draw.segments = T)

# standardize the data
xnorm<-scale(q[,1:40])
rownames(xnorm)=s[,1]

# get the distance matrix
dist1 <- dist(xnorm, method = "euclidean")

# now do clustering – use WARD’s method
clust1 <- hclust(dist1, method = "ward.D")

# draw the dendrogram
plot(clust1, labels = s$country, cex=0.7, xlab="", ylab="Distance", 
     main="Clustering for Pew Countries")
rect.hclust(clust1, k=3)

# get membership vector (which country is in which group)
cuts = cutree(clust1, k=3)
cuts

# get list of countries in each group – note that group numbering is 
# arbitrary – there is no ‘ordering’ of groups
for (i in 1:3){
  print(paste("Countries in Cluster ",i))
  print(s$country[cuts==i])
  print (" ")
}


# Evaluate Number of Clusters
source("http://reuningscherer.net/stat660/R/HClusEval.R.txt")

hclus_eval(xnorm, dist_m = 'euclidean', clus_m = 'ward', plot_op = T)

# Make plot of three cluster solution in space desginated by first two principal components
clusplot(xnorm, cuts, color=TRUE, shade=TRUE, labels=2, lines=0,
         main="Pew Countries Three Cluster Plot, Ward's Method, First two PC")

# Make plot of three cluster solution in space desginated by first two
# two discriminant functions
plotcluster(xnorm, cuts, main="Three Cluster Solution in DA Space",
            xlab="First Discriminant Function", ylab="Second Discriminant Function")

