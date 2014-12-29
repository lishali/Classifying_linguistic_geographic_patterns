setwd(file.path("/Users/LishaLi/Desktop/215A/Lab2"))
options(warn=0)
library(maps)
library(ggplot2)
library(dplyr)
library(cluster)


lingData <- read.table('lingData.txt', header = T)
lingLocation <- read.table('lingLocation.txt', header = T)
load("question_data.RData")

# lingData has a column for each question, and lingLocation has a column
# for each question x answer.  Sorry the columns in lingLocation are not usefully named,
# but it's not too tricky to figure out which is which.
# Note that you still need to clean this data (check for NA's, missing location data, etc.)
names(lingData)
names(lingLocation)
state.df <- map_data("state")

blank.theme <-
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 

############
# Make a plot similar to the website for the second person plural answers.
# You may want to join these data sets more efficiently than this.

# I choose the 4 most popular answers
#1: you all
#4: you guys
#7: you
#9: y'all

plural.second.person <- filter(lingData, Q050 %in% c(1, 4, 7,9), long > -125)
answers.q50 <- all.ans[['50']]

# Make the column to join on.  They must be the same type.
answers.q50$Q050 <- rownames(answers.q50)
plural.second.person$Q050 <- as.character(plural.second.person$Q050)
plural.second.person <- inner_join(plural.second.person, answers.q50, by="Q050")

# Plot!
ggplot(data=NULL) +
  geom_point(data=plural.second.person, aes(x=long, y=lat, color=ans), size=1, alpha=0.75) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group))+
facet_wrap(~ans, ncol=2)+
  blank.theme
#you and you guys look pretty useless, let's just plot those.  
plural.second.person$ans

you.neutral <- filter(plural.second.person, ans=="you" | ans=="you all")
you.neutral

you.neutral <- ggplot(data=NULL) +
  geom_point(data=you.neutral, aes(x=long, y=lat, color=ans), size=1.5, alpha=1) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) 
save(you.neutral, file="you_netural.Rda")
#ok, semi -useless, though I shoudl consider plotting like this for the clusters.




###############
# Plot the lingLocation data (which lives on a grid).  Note that this doesn't look great with
# state outlines.  You can probably do better!
ggplot(data=NULL) +
  geom_tile(data=filter(lingLocation, Longitude > -125),
            aes(x=Longitude, y=Latitude, color=log10(V12), fill=log10(V12))) +
  geom_polygon(data=state.df, colour = "gray", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

# 1:sneakers
#6: tennis shoes 


shoes <- filter(lingData, Q073 %in% c(1, 6), long > -125)
answers.q73 <- all.ans[['73']]

shoes
# Make the column to join on.  They must be the same type.
answers.q73$Q073 <- rownames(answers.q73)
shoes$Q073 <- as.character(shoes$Q073)
shoes <- inner_join(shoes, answers.q73, by="Q073")

shoes

shoes <- ggplot(data=NULL) +
  geom_point(data=shoes, aes(x=long, y=lat, color=ans), size=1, alpha=0.75) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) 
save(shoes, file="shoes.Rda")

#kinda on good for set difference....

#Sunshowers (a facet plot...)

sunshower <- filter(lingData, Q080 %in% c(1:9), long > -125)
answers.q80 <- all.ans[['80']]
answers.q80
answers.q80$Q080 <- rownames(answers.q80)
sunshower$Q080 <- as.character(sunshower$Q080)
sunshower <- inner_join(sunshower, answers.q80, by="Q080")
sunshower.plot <- ggplot(data=NULL) +
  geom_point(data=sunshower, aes(x=long, y=lat, color=ans), size=1.5, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  facet_wrap(~ans, ncol=3)
save(sunshower.plot, file="sunshower.Rda")
sunshower.plot


#what happens when we just cluster on these three manually chosen datasets, would be a good sanity check to see if our spectral methods were worth the effort
names(lingData)
small.data <- select(lingData, ID, CITY, STATE, ZIP, lat, long, Q050, Q073, Q080)
small.data
names(small.data)
#binarize it:

A <- small.data
binarize1 <- function(column) {
  B <- data.frame(index=1:nrow(A))
  for (i in 1:max(A[[column]])) {
    B[, paste0(column,i)] <- ifelse( A[[column]]==i,1,0)
  }
  return(select(B, -(index)))
}

test <- cbind(small.data, binarize1("Q073"), binarize1("Q080"), binarize1("Q050"))
head(test)
names(test)
cluster.test <- test[,10:38]
save(test, file="pleasenofreeze.Rds")
save(cluster.test, file="nocrap.Rds")
load("pleasenofreeze.Rds")
load("nocrap.Rds")
kmean.4 <- pam(cluster.test, 4, metric="manhattan")
kmean.7<- kmeans(cluster.test, 7)
kmean.9<- kmeans(cluster.test, 9)
kmean.11<- kmeans(cluster.test, 11)

save(kmean.4, file="4.Rds")
save(kmean.7, file="7.Rds")
save(kmean.9, file="9.Rds")
save(kmean.11, file="11.Rds")
load("9.Rds")
load("4.Rds")
load("7.Rds")
load("11.Rds")
plot.data <- cbind(test, kmean.4$clust, kmean.7$clust, kmean.9$clust, kmean.11$clust)
save(plot.data, file="plot_small_sample.Rds")

state.df <- map_data("state")

blank.theme <-
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 
names(plot.data)[40] <- "kmean7"
names(plot.data)[39] <- "kmean4"
names(plot.data)[41] <- "kmean9"
names(plot.data)[42] <- "kmean11"
head(state.df)
class(plot.data$long)
kmean4 <- ggplot(data=NULL) +
  geom_point(data=filter(plot.data, long > -130), aes(x=long, y=lat, color=factor(kmean4)), size=1, alpha=0.75) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  facet_wrap(~ kmean4, ncol = 2) +
  blank.theme

kmean4


kmean7 <- ggplot(data=NULL) +
  geom_point(data=filter(plot.data, long > -130), aes(x=long, y=lat, color=factor(kmean7)), size=1, alpha=0.75) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  facet_wrap(~ kmean7, ncol = 3) +
  blank.theme

kmean7

kmean9 <- ggplot(data=NULL) +
  geom_point(data=filter(plot.data, long > -130), aes(x=long, y=lat,color=factor(kmean9)), size=1, alpha=0.75) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  facet_wrap(~ kmean9, ncol = 3) +
  blank.theme

kmean9

#they are pretty overlapping... 

kmean11 <- ggplot(data=NULL) +
  geom_point(data=filter(plot.data, long > -130), aes(x=long, y=lat, color=factor(kmean11)), size=1, alpha=0.75) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme

kmean11







