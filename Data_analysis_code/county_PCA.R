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


#let's do the final thing, in order to create nice choropleth plots, of using our county aggregated data, which also contains corrected geographic info


P <- readRDS(file = "finalbinarized.Rda")
P <-tbl_df(P)

by.county <-readRDS(file="by_county.Rda")
by.county <- tbl_df(by.county)
by.county

#this is missing other information, such as how many people per county, so I need to re mutate my P file

aggregate1 <- P %.% group_by(county) %.% summarise_each(funs(sum),-c(ID, CITY, ZIP, county))

by.county <-group_by(P, county) %>% mutate(population.county=n())

by.county <- by.county %.% group_by(county) %.% summarise_each(funs(sum), -c(ID, CITY, ZIP, county))
by.county <- arrange(by.county, -(population.county))
by.county
#I am going to remove the NA county, since it contains no geographic info
by.county <- filter(by.county, Q0501!=133)
#now I need to scale by the population, i.e I need to get a rate. 
#so I divide each entry by the number of people in county.

by.county[,2:469] <- by.county[,2:469]/by.county$population.county

#make it lowercase county so we can use map dataset to plot choropleth

by.county <- mutate(by.county, county=tolower(county))
save(by.county, file="scaled_by_county.Rda")



###############################
#now some PCA

matrix <- by.county[,2:469]
pca <- prcomp(matrix, scale = F, center = F)
plot(pca)
summary(pca)

#here are our components:

comp <- pca$rotation
comp <-data.frame(comp)
comp <- tbl_df(comp)


#  85% is at PC 57. let's just use that and then fudge around it later to check robustness

comp85 <- comp[,1:57]
comp95 <- comp[,1:110]
#now let's project our dataset onto this 57-dim subspace that contains 85% of the variance

proj85.m <- as.matrix(matrix)%*%as.matrix(comp85)
nrow(proj85.m) #correct, 1502 rows corresponding to number of counties
ncol(proj85.m) #57 columns corresponding to the number of dim of our subspace


proj95.m <- as.matrix(matrix)%*%as.matrix(comp95)
nrow(proj85.m) #correct, 1502 rows corresponding to number of counties
ncol(proj85.m) #57 columns corresponding to the number of dim of our subspace


projected.county <- cbind(select(by.county, county), proj85.m, proj95.m)
projected.county <-tbl_df(projected.county)

save(projected.county, file="county_pca.Rds")
load("county_pca.Rds")
#now let's cluster this dataset. 
#7
county.cluster.7 <- kmeans(proj85.m, 7, nstart=25, iter.max=1000)
county.cluster.7 <- county.cluster.7$cluster
county.cluster.7 <- t(county.cluster.7)
#9
county.cluster.9 <- kmeans(proj85.m, 9, nstart=25, iter.max=1000)
county.cluster.9 <- county.cluster.9$cluster
county.cluster.9 <- t(county.cluster.9)

#11
county.cluster.11 <- kmeans(proj85.m, 11, nstart=25, iter.max=1000)
county.cluster.11 <- county.cluster.11$cluster
county.cluster.11 <- t(county.cluster.11)


#########
#7
county.95.7 <- kmeans(proj95.m, 7, nstart=25, iter.max=1000)
county.95.7 <- county.95.7$cluster
county.95.7 <- t(county.95.7)
county.95.7
#9
county.95.9 <- kmeans(proj95.m, 9, nstart=25, iter.max=1000)
county.95.9 <- county.95.9$cluster
county.95.9 <- t(county.95.9)
county.95.11 <- kmeans(proj95.m, 11, nstart=25, iter.max=1000)
county.95.11 <- county.95.11$cluster
county.95.11 <- t(county.95.11)
county.95.11 
########

projected.county <- cbind(projected.county, county.95.11, county.95.9, county.95.7)
projected.county <- tbl_df(projected.county)
save(projected.county,file="clustered_county.Rda")

head(projected.county)

#Choropleth
#from ggplot maps data
county_df <- map_data("county") 
names(county_df) <- c("long", "lat", "group", "order", "state_name", "county")
state.df <- map_data("state")

choropleth <- left_join(projected.county, county_df, by = "county")
choropleth <- choropleth[order(choropleth$order), ]
choropleth <-tbl_df(choropleth)
choropleth$cluster.7 <- as.factor(choropleth$county.95.7)
choropleth$cluster.9 <- as.factor(choropleth$county.95.9)
choropleth$cluster.11 <- as.factor(choropleth$county.95.11)
is.factor(choropleth$cluster.7)
is.factor(choropleth$cluster.9)
is.factor(choropleth$cluster.11)
choropleth
save(choropleth, file="county_choropleth.Rda")


blank.theme <-
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 
# Plot!


p.7 <- ggplot(data=NULL, aes(long, lat)) +
  geom_polygon(data= choropleth, aes(fill = cluster.7, group = group),
               colour = "white", alpha=1/2, size = 0.2) + 
  geom_polygon(data = state.df, colour = "white", fill = NA, aes(group = group))
print(p.7)


p.9 <- ggplot(data=NULL, aes(long, lat)) +
  geom_polygon(data= choropleth, aes(fill = cluster.9, group = group),
               colour = "white", alpha=1/2, size = 0.2) + 
  geom_polygon(data = state.df, colour = "white", fill = NA, aes(group = group))
print(p.9)
 #these maps are not very good at showing things, most information about the averaging is lost. 


p.11 <- ggplot(data=NULL, aes(long, lat)) +
  geom_polygon(data= choropleth, aes(fill = cluster.11, group = group),
               colour = "white", alpha=1/2, size = 0.2) + 
  geom_polygon(data = state.df, colour = "white", fill = NA, aes(group = group))
print(p.11)

#the most clusters the worse this behaves.
#also, I could have made the data incorrectly, that is I could have mixed up the county informmation with the actual answers....

#on the county level, the information seems to be projected too much


#I have to say, this is pretty bad.  


#let's reuse the plottable data, but just average the cluster by counyt, or take the mode

load("plottable85.Rda")
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mode.plottable85 <- group_by(plottable85, county) %.% summarise_each(funs(Mode), -c(ID:PC85))
mode.plottable85 <- mutate(mode.plottable85, county=tolower(county))
mode.plottable85

choropleth.mode <- left_join(mode.plottable85, county_df, by = "county")
choropleth.mode <- choropleth.mode[order(choropleth.mode$order), ]
choropleth.mode <-tbl_df(choropleth.mode)
choropleth.mode$cluster.4 <- as.factor(choropleth.mode$k85.4)
choropleth.mode$cluster.7 <- as.factor(choropleth.mode$k85.7)
choropleth.mode$cluster.9 <- as.factor(choropleth.mode$k85.9)
choropleth.mode$cluster.11 <- as.factor(choropleth.mode$k85.11)
is.factor(choropleth.mode$cluster.7)
is.factor(choropleth.mode$cluster.9)
is.factor(choropleth.mode$cluster.11)
is.factor(choropleth.mode$cluster.4)
choropleth.mode
save(choropleth.mode, file="choropleth_county_mode.Rda")


# Plot!


p.7.mode <- ggplot(data=NULL, aes(long, lat)) +
  geom_polygon(data= choropleth.mode, aes(fill = cluster.7, group = group),
               colour = "white", alpha=1/2, size = 0.2) + 
  geom_polygon(data = state.df, colour = "white", fill = NA, aes(group = group))
print(p.7.mode)


p.9.mode <- ggplot(data=NULL, aes(long, lat)) +
  geom_polygon(data= choropleth.mode, aes(fill = cluster.9, group = group),
               colour = "white", alpha=1/2, size = 0.2) + 
  geom_polygon(data = state.df, colour = "white", fill = NA, aes(group = group))
print(p.9.mode)


p.11.mode <- ggplot(data=NULL, aes(long, lat)) +
  geom_polygon(data= choropleth.mode, aes(fill = cluster.11, group = group),
               colour = "white", alpha=1/2, size = 0.2) + 
  geom_polygon(data = state.df, colour = "white", fill = NA, aes(group = group))
print(p.11.mode)

p.4.mode <- ggplot(data=NULL, aes(long, lat)) +
  geom_polygon(data= choropleth.mode, aes(fill = cluster.4, group = group),
               colour = "white", alpha=1/2, size = 0.2) + 
  geom_polygon(data = state.df, colour = "white", fill = NA, aes(group = group))
print(p.4.mode)

load("choropleth_county_mode.Rds")


