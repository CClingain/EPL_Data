########################################################################
### Clustering Analysis
########################################################################
library(dplyr)
EPL_Cluster <- EPL_aggregate %>% filter(Season=="16/17") %>% group_by(Team) %>% mutate(YellowCardSum = sum(Yellow_Cards))

EPL_Cluster <- EPL_Cluster %>% mutate(RedCardSum = sum(Red_Cards)) %>% mutate(GoalsSum = sum(Goals_Scored)) %>%
  mutate(GoalsAgainstSum = sum(Goals_Conceded)) %>% mutate(FoulsSum = sum(Fouls_Committed)) %>% mutate(FoulsAgainstSum = sum(Fouls_Against)) %>%
  mutate(CornersSum = sum(Corners)) %>% mutate(CornersConcededSum = sum(Corners_Conceded)) %>% mutate(ShotsSum = sum(Shots_Taken)) %>%
  mutate(ShotsTargetSum = sum(Shots_on_Target)) %>% mutate(ShotsCondededSum = sum(Shots_Conceded)) %>% mutate(ShotsTargetConSum = sum(Shots_on_Target_Conceded))
EPL_Cluster <- EPL_Cluster %>% filter(Week==38)
EPL_Cluster <- EPL_Cluster[c(1:4, 31:42)]

#See how many clusters are suggested to exist
require(mclust)
mcl <- Mclust(EPL_Cluster[c(5:16)])
summary(mcl)
#suggests 2 clusters
library(NbClust)
k.means <- NbClust(EPL_Cluster[c(5:16)],method='kmeans',index='ch')
k.means$Best.nc
#suggests 2 clusters VEV

set.seed(2011)
km.2 <- kmeans(EPL_Cluster[c(5:16)], 2, nstart= 100)
c.crit(km.2)
set.seed(2011)
km.3 <- kmeans(EPL_Cluster[c(5:16)], 3, nstart= 100)
c.crit(km.3) #not better
comp.2 <- cutree(hclust(dist(EPL_Cluster[,-(1:4)]),meth='complete'),2)
xtabs(~comp.2+km.2$cluster) #agreement
#Separate by GoalsSum and GoalsAgainstSum
plot(silhouette(km.2$clust, dist(EPL_Cluster[5:16])))
plot(EPL_Cluster[,7:8],col=km.2$clust,pch=1,cex=1.5, lwd=2)
text(EPL_Cluster[,7:8],labels=EPL_Cluster$Team,col=km.2$clust, cex=.5)
#Separate by ShotsTargetSum and ShotsTargetConSum
plot(EPL_Cluster[,c(14,16)],col=km.2$clust,pch=1,cex=1.5, lwd=2)
text(EPL_Cluster[,c(14,16)],labels=EPL_Cluster$Team,col=km.2$clust, cex=.5,pos=3)

cluster.labels = km.2$cluster
cbind(cluster.labels, EPL_Cluster$Team)
require(phyclust)
RRand(km.2$clust, comp.2)


#MDS -- want to separate by 2 dimensions
mds.2 <-cmdscale(dist(EPL_Cluster[,-(1:4)]),k=2)
pairs(mds.2)
plot(mds.2)
text(mds.2, labels = EPL_Cluster$Team, col = km.2$clust, cex=.5,pos=3)


#For reference
c.crit <- function(km.obj) {
  #based on k-means, for convenience due to amt of addl info in the km result object.
  #cd be generalized.
  sizes <- km.obj$size
  n <- sum(sizes)
  g <- length(sizes)
  msW<-sum(km.obj$withinss)/(n-g)
  overall.mean <- apply(km.obj$centers*km.obj$size,2,sum)/sum(km.obj$size)
  msB<-sum(km.obj$size*(t(t(km.obj$centers)-overall.mean))^2)/(g-1)
  list(msB=msB,msW=msW,C.g=msB/msW)
}
