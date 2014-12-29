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
library(cluster)
library(kohonen)

#I want my df to have the lat/long data so I can plot it after the analysis, this initial segment is doing that dirty work
df <- readRDS(file="finalbinarized.Rda")
df <-tbl_df(df)
df


zipcode <- read.table('ZIP_CODES.txt', sep = ",", colClasses=c("character",rep("character", 6)))
zipcode <- tbl_df(zipcode)
colnames(zipcode)[1] <- "ZIP"    #, so it's the same name as LingDAta'
colnames(zipcode)[2] <- "lat"
colnames(zipcode)[3] <- "long"
colnames(zipcode)[5] <- "state"
colnames(zipcode)[6] <- "county"
nrow(zipcode)
colnames(zipcode)
zipcode <- select(zipcode, -V4, -V7)


test <-left_join(df, zipcode, by="ZIP")
nrow(test)
test <-tbl_df(test)
test

#sanity checks: 

test <- mutate(test, tolower(county.x))

test$county.x==test$county.y #mostly true except for NA
sum(is.na(test$county.y))
sum(is.na(test$county.x)) #i.e they are the same, so we can drop one.  

colnames(test)[4] <- "county"

test <- test[,1:474]
colnames(test)

df <-test
save(df,file="df.Rda")

################################################################################
################################################################################
################################################################################
#now the PCA

A <- df[,5:472]
A

####################################################################################
#PCA without centering, is much better at reducing.  Also, L1 norm should be the same for each 
#person, there is no need to center.  It also performs better than not centering.  
# I want to do L1 PCA because it makes mroe sense for counts, but the only R package I can find cannot be used for R 3.1.1 ...
#may try with scikit-learn package later. 
####################################################################################


pca <- prcomp(A, scale=FALSE, center = FALSE) # no need to scale as I still have counts
print(pca)
plot(pca)
summary(pca)  

comp <- pca$rotation
comp <- data.frame(comp)
#see some 3-D plots of the first 3 components for fun: 
plot3d(comp$PC1, comp$PC2, comp$PC3)
#really don't see much, hence we need to move on




comp85 <- pca$rotation[,1:85] #where 85% of the data is, coincixence that it is also the 85th component.  
comp95 <- pca$rotation[,1:161] #95% of the variance 

comp85 <-data.frame(comp85)
comp95 <-data.frame(comp95)

#so now we multiply A by comp to get the projection into this spanned linear subspace

basis.change85 <- as.matrix(A) %*% as.matrix(comp85)

basis.change95 <- as.matrix(A) %*% as.matrix(comp95)

final <- select(df, ID, CITY, ZIP, county, lat, long)
final <- mutate (final, lat=as.numeric(lat), long=as.numeric(long)) #will need them to be numeric to plot
class(final$lat)
class(final$long)

nrow(final) #474741
nrow(basis.change85) #474741

centered85 <- cbind(final, basis.change85)
centered85 <- tbl_df(centered85)
centered95 <- cbind(final, basis.change95)
centered95 <- tbl_df(centered95)

save(centered95, file="centered95.Rda")
save(centered85, file="centered85.Rda")
load("centered95.Rda")
load("centered85.Rda")
#create matrices of this projected data (onto the 85-dim and 161-dim subspace respectively)
#so we can do K-means or other clustering algorithms

centered85.m <-select(centered85, -c(ID:long))
centered95.m <-select(centered95, -c(ID:long))

################################################################################
#try kmeans on centered.  Let's try 4-10 clusters for now
################################################################################


######################################
######################################



#85
#7
k85.7 <- kmeans(centered85.m, 7, nstart=25, iter.max=1000)

#85
#4
k85.4 <- kmeans(centered85.m, 4, nstart=25, iter.max=1000)

#85
#9
k85.9 <- kmeans(centered85.m, 9, nstart=25, iter.max=1000)

#85
#11
k85.11 <- kmeans(centered85.m, 11, nstart=25, iter.max=1000)

#85
#13
k85.13 <- kmeans(centered85.m, 13, nstart=25, iter.max=1000)

######################################

#I will use Kmeans L1 just to see if it makes a difference on the 85 dataset. 

L1.large.7 <- Kmeans(centered85.m, 7, nstart=25, iter.max=1000, method="manhattan")
######################################

#95
#4
k95.4 <- kmeans(centered95.m, 4, nstart=25, iter.max=1000)

#95
#7
k95.7 <- kmeans(centered95.m, 7, nstart=25, iter.max=1000)

#95
#9
k95.9 <- kmeans(centered95.m, 9, nstart=25, iter.max=1000)


#################################################################################

#try other clustering algorithms: 
#I use clara because my dataset is quite big....so I want to sample instead...
#also crashed
pam.11 <- pam(centered85.m, 11, metric = "manhattan")
clara.cluster.11 <- clara.cluster.11$cluster 
clara.cluster.11 <- data.frame(clara.cluster.11)


#last try: Sota

sota <- sota(centered85.m, maxCycles = 11)
#################################################################################
#################################################################################
#putting it all together in one dataframe 

#we only care about the cluster columns

k85.4 <- k85.4$cluster
k85.7 <- k85.7$cluster
k85.9 <- k85.9$cluster
k85.11 <- k85.11$cluster
k95.4 <- k95.4$cluster
k95.7 <- k95.7$cluster
k95.9 <- k95.9$cluster

k85.4 <- data.frame(k85.4)
k85.7 <- data.frame(k85.7)
k85.9 <- data.frame(k85.9)
k85.11 <-data.frame(k85.11)
k95.4 <- data.frame(k95.4)
k95.7 <- data.frame(k95.7)
k95.9 <- data.frame(k95.9)
cluster85 <- cbind(k85.4, k85.7, k85.9, k85.11)
cluster95 <-cbind(k95.4, k95.7, k95.9)
cluster85 <-tbl_df(cluster85)
cluster95 <- tbl_df(cluster95)
summary(cluster85) #good, all working
summary(cluster95)#good, all working
save(cluster85, file="cluster85.Rda")
save(cluster95, file="cluster95.Rda")



#################################################################################
#################################################################################

rownames(cluster85) # now we inner_join with the original projected dataset, centered95/85.
cluster85$ID <- c(1:47471)
centered85$ID <- c(1:47471)
class(cluster85$ID)
class(centered85$ID)
centered85$ID==cluster85$ID

plottable85 <- inner_join(centered85, cluster85, by="ID")
nrow(plottable85)
plottable85 <- tbl_df(plottable85)
plottable85
save(plottable85, file="plottable85.Rda")
#likewise for the 95 data

cluster95$ID <- c(1:47471)
centered95$ID <- c(1:47471)
centered95$ID==cluster95$ID

plottable95 <- inner_join(centered95, cluster95, by="ID")
nrow(plottable95)
plottable95 <- tbl_df(plottable95)
plottable95
save(plottable95, file="plottable95.Rda")
load(file = "plottable95.Rda")
load(file = "plottable85.Rda")
########## PLOTS PLOTS PLOTS 
state.df <- map_data("state")

blank.theme <-
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 
# Plot!
p85.4 <- ggplot(data=NULL) +
  geom_point(data=filter(plottable85, long > -130), aes(x=long, y=lat, color=factor(k85.4)), size=2, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme
p85.7 <- ggplot(data=NULL) +
  geom_point(data=filter(plottable85, long > -130), aes(x=long, y=lat, color=factor(k85.7)), size=2, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  city.plot+
  blank.theme
p85.9 <- ggplot(data=NULL) +
  geom_point(data=filter(plottable85, long > -130), aes(x=long, y=lat, color=factor(k85.9)), size=1.5, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  city.plot+
  blank.theme
p85.11 <- ggplot(data=NULL) +
  geom_point(data=filter(plottable85, long > -130), aes(x=long, y=lat, color=factor(k85.11)), size=1.5, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  city.plot+
  blank.theme




p95.4 <- ggplot(data=NULL) +
  geom_point(data=filter(plottable95, long > -130), aes(x=long, y=lat, color=factor(k95.4)), size=1.5, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme
p95.7 <- ggplot(data=NULL) +
  geom_point(data=filter(plottable95, long > -130), aes(x=long, y=lat, color=factor(k95.7)), size=1.5, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  city.plot+
  blank.theme
p95.9 <- ggplot(data=NULL) +
  geom_point(data=filter(plottable95, long > -130), aes(x=long, y=lat, color=factor(k95.9)), size=1.5, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  city.plot+
  blank.theme




#######################################
p85.4
p85.7+ facet_wrap(~k85.7)
p85.9 + facet_wrap(~k85.9)
p85.11 #really don't see much at this point
##########
p95.4
p95.7
p95.9+ facet_wrap(~k95.9)
#if I want to add extra points (sf, NYC, MIAMI, DC, CHICAGO)

geom_point(data=NULL, aes(x = -122, y = 37, group="Berkeley"),
           color="orange", size=5)
#######################################
#looks like 9 clusters still have relevant information, let's look at 11 and 13 in case more is useful
# let's also randomly get ride of 20% of the points, see if we get the same clusters, do this 3 times
#finally, let's try a couple more clustering algorithms
#and finally, we can also use the zipcode datasets with normalized variance to see if things are useful. 



##############################################################################

######################################################################################
#PCA with L1 norm?  will it be easier? Nope, it crashed. Otherwise, L2 is more meaningful since we started taking averages, so the further things are, the more they should be penalized, since deviating from the average has a normal effect.  Of course, L2 is not optimal for that, something proportional to standard deviation and culmulative density would penalize the best.  But that is an approximation, L2 is still doing this more than L1, since L1 penalizes the same as counts, which is not what we want.  Will it be better

#other cluster algorithms

#which quetsions is best for predicting?  


#use the components to choose which two quetsions to look at, and see if they are good

#or rather, use all the other questions to predict what one would answer in another

#information entropy...etc


# can I rescale info on the county level, how much worse does it do

#use data to test on...


#make your 3-D plots to justify the number of clusters...did, it is nonintuitive




#other metrics.  Instead of using the hamming distance, we can see how many differences...or other things used in the paper to see if we get anything better?


cities <- c("New York", "San Francisco", "Miami", "Chicago")
Lat <- c(40.40, 37.7833, 25.787, 41.8369  )
Long <- c(-74.0059,-122.4167, -80.2241,-87.6847)
cities <- t(cities)
Lat <- t(Lat)
Long <- t(Long)
city <- cbind(cities, Lat, Long)
nrow(city)
city
colnames(city) <- c("city", "lat", "long")
city <-data.frame(city)
city.plot <- geom_text(data=city, mapping = aes(x=long, y=lat, label=as.factor(city)))

city$long <- c(-74.0059,-122.4167, -80.2241,-87.6847)
city$lat <- c(40.40, 37.7833, 25.787, 41.8369  )
class(city$lat)
city
