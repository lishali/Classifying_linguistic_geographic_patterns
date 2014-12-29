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

by.county <- readRDS(file="by_county.Rda")
by.county <-tbl_df(by.county)

nrow(by.county) #good
by.county[1503,]

rownames(by.county) <- by.county$county
#Let's do some PCA and clustering...eventually we will have to let go of a lot of our data, say 20%
filter(by.county, county ==NA)
#drop our county column to create a matrix
matrix <-select(by.county, -county)
matrix <- tbl_df(matrix)
matrix <- t(matrix)
matrix
names <-c(by.county[["county"]])
rownames(matrix) <- names
matrix <- matrix[,-1503]
matrix

#not scaled: 
#I don't think scaling makes sense here, because my variables are the question answers, which are in counts
#each county is a point in this dataset, so even if there are counties with more people in them, they would just be uniformly more points for each varaible.  


pca2 <- prcomp(matrix, scale = FALSE)
print(pca2)
plot(pca2)
summary(pca2)

summary(pca2$rotation)[,1] #this summarizes the first componet's data
#1.740e-01

plot(pca2, type='l')

# First 5 principal components (0.97%)
comp <- data.frame(pca2$rotation[,1:5])
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))
#This is my covariance data.  

library(rgl)
# Multi 3D plot
plot3d(comp$PC1, comp$PC2, comp$PC3) #first three components is 95% of the variance..
plot3d(comp$PC1, comp$PC3, comp$PC4)# just for fun


comp
#there are ways to determine how many clusters, and we'll look into that, but first let's just try a couple

# for instance, k =5
k <- kmeans(comp, 3, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16) #looks cute

plot3d(comp$PC1, comp$PC2, comp$PC3, col = k$clust)

k$clust

#cool, so this clusters the answers for us, that is, whoever answered a certain way will be in one cluster..

#LetS try to see what mapping this will show.
comp$ID <- rownames(comp)
head(comp)
names(comp)
cluster.county <- k$clust
cluster.county <-t(cluster.county)
cluster.county
class(cluster.county)
test <-cluster.county
test <- data.frame(test)
rownames(test)
cluster.county <- data.frame(cluster.county)
test$ID <-rownames(test)
cluster.county <- test
nrow(cluster.county)
head(cluster.county)
names(cluster.county)
cluster.county <- inner_join(comp, cluster.county, by="ID")
nrow(cluster.county)
cluster.county <-tbl_df(cluster.county)
names(cluster.county)[7] <- "cluster"
names(cluster.county)[6] <- "county"
cluster.county[,6:7]


cluster.county <-mutate(cluster.county, county=tolower(county))
cluster.county

############ NOW THE MAPS


# Load the outlines of the counties. this is actually coming from ggplot 2
county_df <- map_data("county")
nrow(county_df)
names(county_df) <- c("long", "lat", "group", "order", "state_name", "county")
county_df$state <- state.abb[match(county_df$state_name, tolower(state.name))]
county_df$state_name <- NULL

# Load the outlines of the states.
state_df <- map_data("state")
select(cluster.county, county)
# Combine together.
choropleth <- merge(county_df, cluster.county, by = "county")
choropleth <- choropleth[order(choropleth$order), ]
choropleth <- tbl_df(choropleth)
# Once you have the data in the right format, recreating the plot is
# straightforward.  Try looking at these plots separately to see what they are
# doing.
choropleth$cluster <- as.factor(choropleth$cluster)
is.factor(choropleth$cluster)
#choropleth$cluster <- cut(choropleth$cluster, breaks = c(seq(0,3, by = 1), 35))
p <- ggplot(data=NULL, aes(long, lat)) +
  geom_polygon(data= choropleth, aes(fill = cluster, group = group),
               colour = "white", alpha=1/2, size = 0.2) #+ 
  #geom_polygon(data = state_df, colour = "white", fill = NA, aes(group = group)) 
#some counties are not represented, no suprise.  
print(p)


######################################################################
######################################################################


#so 3 clusters looks pretty useless, let's try more: 



# for instance, k =5
k <- kmeans(comp, 5, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16) #looks cute

plot3d(comp$PC1, comp$PC2, comp$PC3, col = k$clust)

k$clust

#cool, so this clusters the answers for us, that is, whoever answered a certain way will be in one cluster..

#LetS try to see what mapping this will show.
comp$ID <- rownames(comp)
head(comp)
names(comp)
cluster.county <- k$clust
cluster.county <-t(cluster.county)
cluster.county
class(cluster.county)
test <-cluster.county
test <- data.frame(test)
rownames(test)
cluster.county <- data.frame(cluster.county)
test$ID <-rownames(test)
cluster.county <- test
nrow(cluster.county)
head(cluster.county)
names(cluster.county)
cluster.county <- inner_join(comp, cluster.county, by="ID")
nrow(cluster.county)
cluster.county <-tbl_df(cluster.county)
names(cluster.county)[7] <- "cluster"
names(cluster.county)[6] <- "county"
cluster.county[,6:7]


cluster.county <-mutate(cluster.county, county=tolower(county))
cluster.county

############ NOW THE MAPS


# Load the outlines of the counties. this is actually coming from ggplot 2
county_df <- map_data("county")
nrow(county_df)
names(county_df) <- c("long", "lat", "group", "order", "state_name", "county")
county_df$state <- state.abb[match(county_df$state_name, tolower(state.name))]
county_df$state_name <- NULL

# Load the outlines of the states.
state_df <- map_data("state")
select(cluster.county, county)
# Combine together.
choropleth <- merge(county_df, cluster.county, by = "county")
choropleth <- choropleth[order(choropleth$order), ]
choropleth <- tbl_df(choropleth)
# Once you have the data in the right format, recreating the plot is
# straightforward.  Try looking at these plots separately to see what they are
# doing.
choropleth$cluster <- as.factor(choropleth$cluster)
is.factor(choropleth$cluster)
#choropleth$cluster <- cut(choropleth$cluster, breaks = c(seq(0,3, by = 1), 35))
p <- ggplot(data=NULL, aes(long, lat)) +
  geom_polygon(data= choropleth, aes(fill = cluster, group = group),
               colour = "white", alpha=1/2, size = 0.2) #+ 
#geom_polygon(data = state_df, colour = "white", fill = NA, aes(group = group)) 
#some counties are not represented, no suprise.  
print(p)





#ty things uncentered

pca <- prcomp(t(matrix), scale = FALSE, center = FALSE)
#we needed to transpose so that the variables are interpreted as the quesitons, not the counties

print(pca)
plot(pca)
summary(pca)


# First 5 principal components
top5 <- data.frame(pca$x[,1:5]) #even higher amount of variance covered  
# 98150..
#should compare

top5[1,]==comp[1,] #so all different.  
#but by how much?


# Plot
plot(top5, pch=16, col=rgb(0,0,0,0.5))

# Multi 3D plot
plot3d(top5$PC1, top5$PC2, top5$PC3) #first three components is 95% of the variance..

top5$PC1

#there are ways to determine how many clusters, and we'll look into that, but first let's just try a couple

# for instance, k =5
k <- kmeans(top5, 10, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(top5, col=k$clust, pch=16) #looks cute

plot3d(top5$PC1, top5$PC2, top5$PC3, col = k$clust)



#I wander what the components are 
length(top5[,1])

length(comp[,1]) #this shoudl not be the length, it should be the number of questinos, not the number of counties...




######################################################################
######################################################################

#let's try on the lingDatasets instead

lingLocation <- read.table('lingLocation.txt', header = T)
lingLocation <-tbl_df(lingLocation)
LL <-select(lingLocation, -(1:3))
LL
#ll for lat long
pcall <- prcomp(t(LL), scale = FALSE, center = FALSE)
#we needed to transpose so that the variables are interpreted as the quesitons, not the counties

print(pcall)
plot(pcall)
summary(pcall)


# First 5 principal components : 0.98369%
FIVE <- data.frame(pcall$x[,1:5]) #even higher amount of variance covered  



# Plot
plot(FIVE, pch=16, col=rgb(0,0,0,0.5))

# Multi 3D plot
plot3d(FIVE$PC1, FIVE$PC2, FIVE$PC3) #first three components is 95% of the variance..

# for instance, k =5
k <- kmeans(FIVE, 10, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(FIVE, col=k$clust, pch=16) #looks cute

plot3d(FIVE$PC1, FIVE$PC2, FIVE$PC3, col = k$clust)

#alright, pretty shitty.  

######################################################################
######################################################################

#Let's try one more thing, to aggregate by state
by.state <- readRDS(file="by_state.Rda")
by.state <- tbl_df(by.state)
by.state

A <- select(by.state, -1)


#ll for lat long
PA <- prcomp(A, scale = FALSE, center = FALSE)
#we needed to transpose so that the variables are interpreted as the quesitons, not the counties

print(PA)
plot(PA)
summary(PA)


# First 5 principal components : 0.98369%
FIVE <- data.frame(PA$x[,1:5]) #even higher amount of variance covered  

FIVE$PC1


# Plot
plot(FIVE, pch=16, col=rgb(0,0,0,0.5))

# Multi 3D plot
plot3d(FIVE$PC1, FIVE$PC2, FIVE$PC3) #first three components is 95% of the variance..

# for instance, k =4
k <- kmeans(FIVE, 4, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(FIVE, col=k$clust, pch=16) #looks cute

plot3d(FIVE$PC1, FIVE$PC2, FIVE$PC3, col = k$clust)

 
######################################################################
######################################################################


# lets look at the interpretability of these components...maybe then
#we can track down the most informative questions, and then use their
#answers to colour the maps and see if anything interesting emerges



#what is the source of randomness...
#QQ-plots


#PCA done with L2?  We need L1 since this is count data
######################################################################
######################################################################

county_df <- map_data("county")
county_df <-tbl_df(county_df)




######################################################################
######################################################################



#Things to look at: 
  #50




