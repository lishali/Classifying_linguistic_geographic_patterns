setwd(file.path("/Users/LishaLi/Desktop/215A/Lab2"))
options(warn=0)
library(maps)
library(ggplot2)
library(dplyr)
library(maps)
library(reshape2)
library("Matrix")
library("irlba")
library("microbenchmark")
library("GGally")
library("lattice")
library("ggplot2")
library("dplyr")
library("tools")
library("HSAUR")
library(RColorBrewer)
library(scales)
library(rgl)
library(stats)
library(amap)
library(mclust)

#I want my df to have the lat/long data so I can plot it after the analysis, this initial segment is doing that dirty work
load("/Users/LishaLi/Desktop/215A/Lab2/df.Rda")
df

small <- read.table("lingLocation.txt", head=T)
small <-tbl_df(small)
small
nrow(small)

#ok, I will try this unscaled for now, but there should be scaling...

matrix <- small[,4:471]
pca <- prcomp(matrix, scale = F, center = F)
pca.scale <- prcomp(matrix, scale =T, center =F)
plot(pca)
plot(pca.scale)
summary(pca)
#both are not doing what I want, I will try them, but really, I need to renormalize the count data per county by rates, rather than raw count

#PC5 has 98% of the variance

comp <- pca$rotation
comp <- data.frame(comp)
#see some 3-D plots of the first 3 components for fun: 
plot3d(comp$PC1, comp$PC2, comp$PC3)

comp95 <- comp[,1:5]
comp95 <- tbl_df(comp95)


basis.change <- as.matrix(matrix) %*% as.matrix(comp95)
nrow(basis.change)
basis.change <- data.frame(basis.change)
aggregate <- cbind(select(small, 1:3), basis.change)
aggregate <- tbl_df(aggregate)
save(aggregate, file="aggregate_zip.Rda")
aggregate.m <- select(aggregate, PC1:PC5)




#K-means

cluster.small.7 <- kmeans(aggregate.m, 7, nstart=25, iter.max=1000)
cluster.7 <- cluster.small.7$cluster

cluster.small.4 <- kmeans(aggregate.m, 4, nstart=25, iter.max=1000)
cluster.4 <- cluster.small.4$cluster

cluster.small.9 <- kmeans(aggregate.m, 9, nstart=25, iter.max=1000)
cluster.9 <- cluster.small.9$cluster

cluster.small.11 <- kmeans(aggregate.m, 11, nstart=25, iter.max=1000)
cluster.11 <- cluster.small.11$cluster

cluster.4 <- data.frame(cluster.4)
cluster.7 <-data.frame(cluster.7)
cluster.9 <-data.frame(cluster.9)
cluster.11<-data.frame(cluster.11)

aggregate <- cbind(aggregate, cluster.4, cluster.9, cluster.7, cluster.11)

class(aggregate$Latitude)
save(aggregate, file="clustered.small.aggregate.Rda")

nrow(aggregate)

state.df <- map_data("state")

blank.theme <-
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 
# Plot!

small.plot.4 <- ggplot(data=NULL) +
  geom_point(data=filter(aggregate, Longitude > -130), aes(x=Longitude, y=Latitude, color=factor(cluster.4)), size=2, alpha=1) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

small.plot.4


small.plot.9 <- ggplot(data=NULL) +
  geom_point(data=filter(aggregate, Longitude > -130), aes(x=Longitude, y=Latitude, color=factor(cluster.9)), size=4, alpha=1) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

small.plot.9

###################################################################################################
###################################################################################################

#all of this works way worse, I really need to reweigh by people in the cells

# I will do something simple, rescale by dividing each row, by the number of peple in cell
small.scale <-small
small.scale[,4:471] <- small[, 4:471]/small$Number.of.people.in.cell
#alright, scaled. let's do this!

small.scale


matrix <- small.scale[,4:471]
pca <- prcomp(matrix, scale = F, center = F)
plot(pca)
summary(pca)
#both are not doing what I want, I will try them, but really, I need to renormalize the count data per county by rates, rather than raw count

#PC63 is 95%of the variance



comp <- pca$rotation
comp <- data.frame(comp)
#see some 3-D plots of the first 3 components for fun: 
plot3d(comp$PC1, comp$PC2, comp$PC3)

comp95 <- comp[,1:65]
comp95 <- tbl_df(comp95)


basis.change <- as.matrix(matrix) %*% as.matrix(comp95)
nrow(basis.change)
basis.change <- data.frame(basis.change)
aggregate <- cbind(select(small, 1:3), basis.change)
aggregate.scaled <- tbl_df(aggregate)
save(aggregate.scaled, file="aggregate_scaled_small.Rda")
aggregate.m <- select(aggregate.scaled, PC1:PC65)
load("aggregate_scaled_small.Rda")
aggregate.scaled
#K-means

cluster.small.7 <- kmeans(aggregate.m, 7, nstart=25, iter.max=1000)
cluster.7 <- cluster.small.7$cluster

cluster.small.4 <- kmeans(aggregate.m, 4, nstart=25, iter.max=1000)
cluster.4 <- cluster.small.4$cluster

cluster.small.9 <- kmeans(aggregate.m, 9, nstart=25, iter.max=1000)
cluster.9 <- cluster.small.9$cluster

cluster.small.11 <- kmeans(aggregate.m, 11, nstart=25, iter.max=1000)
cluster.11 <- cluster.small.11$cluster

cluster.4 <- data.frame(cluster.4)
cluster.7 <-data.frame(cluster.7)
cluster.9 <-data.frame(cluster.9)
cluster.11<-data.frame(cluster.11)

aggregate <- cbind(aggregate, cluster.4, cluster.9, cluster.7, cluster.11)

class(aggregate$Latitude)
save(aggregate, file="clustered.small.aggregate.scaled.Rda")
load("clustered.small.aggregate.scaled.Rda")
nrow(aggregate)
#################################################

#k means with L1 (from amap)

#since this is count data, makes more sense to use L1 penalty....maybe I'll consider this for heirarchical clustering as well
L17 <- Kmeans(aggregate.m, 7, nstart=25, iter.max=1000, method="manhattan")
L1.7 <- L17$cluster
L1.7 <-data.frame(L1.7)
save(L1.7, file="L1_7.Rda")
nrow(L1.7)
L1.7$ID <-c(1:781)

L1.9<- Kmeans(aggregate.m, 9, nstart=25, iter.max=1000, method="manhattan")
L1.11<- Kmeans(aggregate.m, 11, nstart=25, iter.max=1000, method="manhattan")
L1.9 <- L1.9$cluster
L1.11 <- L1.11$cluster
L1.9 <-data.frame(L1.9)
L1.11 <-data.frame(L1.11)
aggregate <-cbind(aggregate, L1.9, L1.11)



pam.9.L1 <- pam(aggregate.m, 9, metric = "manhattan")
pam.9.L1
pam.9.L1 <- pam.9.L1$cluster
pam.11.L1 <- pam(aggregate.m, 11, metric = "manhattan")
pam.11.L1 <-pam.11.L1$cluster
aggregate <-cbind(aggregate, pam.11.L1, pam.9.L1)

save(aggregate, file="aggregate_L1.Rda")
load(file="aggregate_L1.Rda")
head(aggregate)

#################################################
#################################################
#################################################
#################################################

#hierarchihael

mclust <- Mclust(aggregate.m)
#we need to define a distance between any two lat, long points, let's use the hamming distance, since they are basically counts, and the paper suggests this as well



test <- hclust(, method = "complete")


state.df <- map_data("state")

blank.theme <-
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 
# Plot!

small.plot.4 <- ggplot(data=NULL) +
  geom_point(data=filter(aggregate, Longitude > -130), aes(x=Longitude, y=Latitude, color=factor(cluster.4)), size=4, alpha=1) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

small.plot.4

small.plot.7 <- ggplot(data=NULL) +
  geom_point(data=filter(aggregate, Longitude > -130), aes(x=Longitude, y=Latitude, color=factor(cluster.7)), size=4, alpha=1) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

small.plot.7


small.plot.9 <- ggplot(data=NULL) +
  geom_point(data=filter(aggregate, Longitude > -130), aes(x=Longitude, y=Latitude, color=factor(cluster.9)), size=4, alpha=1) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

small.plot.9

small.plot.11 <- ggplot(data=NULL) +
  geom_point(data=filter(aggregate, Longitude > -130), aes(x=Longitude, y=Latitude, color=factor(cluster.11)), size=4, alpha=0.75) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

small.plot.11

#this pretty much gives me the same info as the large PCA, only worse plots

#we can try other clustering algorithms.  Also it's cool that the clustering is confirmed by these two methods

names(aggregate)
L1.plot.7 <- ggplot(data=NULL) +
  geom_point(data=filter(aggregate, Longitude > -130), aes(x=Longitude, y=Latitude, color=factor(L1.7)), size=4, alpha=0.75) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

L1.plot.7

L1.plot.9 <- ggplot(data=NULL) +
  geom_point(data=filter(aggregate, Longitude > -130), aes(x=Longitude, y=Latitude, color=factor(L1.9)), size=4, alpha=0.75) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

L1.plot.9

L1.plot.11 <- ggplot(data=NULL) +
  geom_point(data=filter(aggregate, Longitude > -130), aes(x=Longitude, y=Latitude, color=factor(L1.11)), size=4, alpha=0.75) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

L1.plot.11

# I actually don't think L1 does as well, given that the northest and central america are still not differentiated by 7, whereas for L2 they already were.  This may have something to do with the fact that we took the rates for this dataset.  I should compare this with using L1 for just the per person dataset. 

#####################################################
#####################################################
#####################################################
#####################################################
#pam L1

pam9 <- ggplot(data=NULL) +
  geom_point(data=filter(aggregate, Longitude > -130), aes(x=Longitude, y=Latitude, color=factor(pam.9.L1)), size=4, alpha=0.75) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

pam9 #so texas is represented here already, but new york is not differeentialed

pam11 <- ggplot(data=NULL) +
  geom_point(data=filter(aggregate, Longitude > -130), aes(x=Longitude, y=Latitude, color=factor(pam.11.L1)), size=4, alpha=0.75) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

pam11 #this is really good.  aside from miami not really differentialed, it seems to cluster a lot more and reasonbly....
#####################################################
#####################################################
#####################################################
#####################################################

#Do EM, assuming normal behaviour....justify

#do unsupervised method.



#I can try heirarchical clustering, since similiarity can be defined via what the paper cares about...

# I will use covariance as my distance matrix between the observation row(so lat, long pairs)

d <- mutate(aggregate, ID = paste(Latitude, Longitude))
d <- d[,4:74]
colnames(d)
d <-select(d, ID,PC1:PC65))

#Sota, self organizing t algorithm...like the sound of this since doesn't push into metric

clValid

library(cluster)
library(kohonen)

#And also in throwing away parts of the dataset, because I almalgamate....

#finally do some predictions.... given your cross validation....



  