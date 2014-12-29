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

#the zipcodes will get turned to numerics, so we need to read them as 
#characters to avoid dropping of 0s in front
lingData <- read.table('lingData.txt', header = T, colClasses=c("numeric", rep("character", 3), rep("numeric", 69)))
lingData <-tbl_df(lingData) #yeah!  Did not screw up the zips
lingLocation <- read.table('lingLocation.txt', header = T)
lingLocation <-tbl_df(lingLocation)
class(lingLocation[1,3]) #double checking it imported things correctly
load("question_data.RData")

#we will use the following zipcode data to help do sanity checks on the location info
# provided in the ling dataset and also to help generate better maps via ggplot

zipcode <- read.table('ZIP_CODES.txt', sep = ",", colClasses=c("character",rep("character", 6)))
zipcode <- tbl_df(zipcode)
colnames(zipcode)[1] <- "ZIP"    #, so it's the same name as LingDAta'
colnames(zipcode)[2] <- "lat"
colnames(zipcode)[3] <- "long"
colnames(zipcode)[5] <- "state"
colnames(zipcode)[6] <- "county"
################################################################################
################################################################################
################################################################################

#we now left_join the information of zipcode and county to lingData
test <- left_join(lingData, zipcode, by="ZIP")
test<-tbl_df(test)
select(test, CITY, STATE, ZIP, county, state, lat.x, long.x, lat.y, long.y)
#after joining, we will also check lat and long the same as long as they agree within a degree or two
#sanity check that the states match...

#finally also join the lingLocation data.  
#we need that the lat and long are numerics in the lingDATA frame, unforutnatley,
#this is the only hack way I can manage to make as.numeric work on this....
test.long.y <-test$long.y
long.y <-as.numeric(test.long.y)
test$long.y <- long.y
filter(test, is.numeric(test$long.y)==FALSE)
test.lat.y <-test$lat.y
lat.y <-as.numeric(test.lat.y)
class(lat.y)
test$lat.y <- lat.y

head(select(test, long.y, long.x, lat.y, lat.x, ZIP))
#check now whether the lat/long from the two datasets we left_joined agree within reasonable values
test <- mutate(test, long.diff = abs(long.y-long.x), lat.diff = abs(lat.x-lat.y))
filter(test, long.diff >2)
#ok, 42 entries do not agree within 2 degrees
test.long.diff <-filter(test, long.diff >5) #31 of them differ by at 10 degrees, that is more major, check them out
filter(test.long.diff, CITY != "Honolulu")
#16 of them are not Honolulu, but various werid places, we can check later, otherwise we are fine to replace them, I am quite happy 
#Only 8 of them are more than 5 apart, so the other places were just huge counties/zipcodes
#these 8 include: Barrow AK, and then Honolulu, so let's just check Barrow AK
test["Barrow"]
test.lat.diff <- filter(test, lat.diff >2) #31 of them differ by more than 2 degrees lat wise...they all seem to be from Hawaii
filter(test.lat.diff, CITY != "Honolulu")
#this data entry was terrible, the 6 entries that were not Honolulu were blahblah, honolulu spelled wrong, or PerlHarbour, not sure if that was a joke or very sad.

#ok, so I am assuming the data from Honolulu and Barrows are just different for the two datasets, but they still agree within some value, so I will just drop the zipcode dataset lat and long values

zip.lingLoc <- select(test, -(long.diff))
zip.lingLoc <- select(zip.lingLoc, -(long.y)) # lat.y, lat.diff
zip.lingLoc <- select(zip.lingLoc, -(lat.y))
zip.lingLoc <- select(zip.lingLoc, -(lat.diff))
colnames(zip.lingLoc)[73] <-"long"
colnames(zip.lingLoc)[72] <- "lat"
select(filter(zip.lingLoc, STATE != state), state, STATE, CITY, ZIP, ID)
#only 6 of them do not have matching state data...they don't exist...such as fhhjdhj...
#looking at the 6 closely, we clearly have data entry errors, happily, our zipcode data is not wrong, and we can just trust it...for instance, zip 
#52353 is in IA, next two problems are both in NJ, not XX and NY, no state named, TE, TX is correct as checkign with ZIP
# 04005 is in ME, not MA, 
#they also just read new york and put NY, didn't realize the ZIp was in Ca
#finally WAshing was not DC, it was actually MD (), Foxburg is in NJ not PA, ....so basically these data people were very bad at recording the dataset....and did the most naive thing, that did nto work on borderline cases such as NY in CA, not in NY
#solution is to just drop the STATE from lingDATA
zip.lingLoc <-select(zip.lingLoc, -(STATE))
colnames(zip.lingLoc)

#lingDATa and #lingLoc shoudl have hte same information, we can do a sanity check to see if they do
lingData
lingLocation

all.ans[50]
#so the answer table gives me the percentage of people who answered each question and all the answers


#so to make everything into binary data we need to use this all.ans table

head(zip.lingLoc)

#don't do all of them, just convert two questions to binary data.  

#convert to binary data
zip.lingLoc$Q050a <- ifelse( zip.lingLoc$Q050==1,1,0)
zip.lingLoc$Q050b <- ifelse(zip.lingLoc$Q050==2,1,0)
zip.lingLoc$Q050c <- ifelse(zip.lingLoc$Q050==3,1,0)
zip.lingLoc$Q050d <- ifelse(zip.lingLoc$Q050==4,1,0)
zip.lingLoc$Q050e <- ifelse(zip.lingLoc$Q050==5,1,0)
zip.lingLoc$Q050f <- ifelse(zip.lingLoc$Q050==6,1,0)
zip.lingLoc$Q050g <- ifelse(zip.lingLoc$Q050==7,1,0)
zip.lingLoc$Q050h <- ifelse(zip.lingLoc$Q050==8,1,0)
zip.lingLoc$Q050i <- ifelse(zip.lingLoc$Q050==9,1,0)

Q50bin <- select(zip.lingLoc, Q050, Q050a:Q050i)
Q50bin <- tbl_df(Q50bin)
colnames(Q50bin) <-c("Q50", "a", "b", "c", "d", "e", "f", "g", "h", "i")
names(Q50bin) 
#just as a sanity check,there should be one 1 per row
x <- select(Q50bin, a:i)
head(x)
x$check1 <- rowSums(x)
filter(x, check1 !=1)
#those may be our NAs, 1266 in total. As a crude check:
filter(Q50bin, Q50 ==0) #answer: yes
#so let's trim the question column out and make the matrix
Q50bin <- select(Q50bin, a:i)

#ok, I will sanity check no further because it seems reasonable my functions worked

#finally, let's do some PCA on this data.  
#centering does not seem needed since we only have binary data, so it's already centered
#likewise for scaling
pcas <- prcomp(Q50bin, center=FALSE, scale=FALSE)
pcas
#omg we only have unit vectors for eigenvalues...ok, makes sense, why did d become most popular?
#let's check the answer summary:
all.ans[50] #so d at  42%, then g at 24% then i then a at 13 adn 12 respectiely
#these were all correctly identified by the pca, so really that was not so useful at finding anything...

#hence, PCA on just one quesiton is trivial.  Now, if we do this for all of the questions, we have a bit more to uncover
#i.e it may be possible to reduce the dimension of what is going on because there will be correlations between quetsions
#i.e for Q50, I answer d alot and am also inclined to answer a for Q51 alot (hypothetically), so one is a good predicator of the other and most variance is seen at a specific linear comb of the two basis (for d and a with Q50 and Q51 respectively)

#now to use LingLoc is more reasonabel since the ZIP data is very crudely found anyways, just by matching lat long to the zip assosicated with it

lingData
lingLocation #(this is the amalgamated data) with cells..

#number of people in cells should equal the number of ones times 122 questions..
#rename to make it less confusing for myself
CellData <- lingLocation
names(CellData)[1] <- "Cell.pop"
names(CellData)
#sanity check
mutate(CellData, check1=sum(V4:V471))
x <- select(CellData, V4:V471)
CellData$check1 <- rowSums(x)
head(select(CellData, Cell.pop, check1))
filter(CellData, Cell.pop*50 != check1)
CellData$mean.answered <- CellData$check1/CellData$Cell.pop
summary(CellData$mean.answered) #so somehow most of them only included 67 questions...
summary(CellData)

nrow(CellData) #much smaller amalgamated dataset.  
names(CellData)
CellData.matrix <- CellData[, 4:471]
#let's just play with it for now. 

#unsure if I should normalize by the Cell.pop, since it would be large due to having sampled more dense cells
#the units are all counts
#the variables (x) are the different long.lat cells.  They have different populartions, but I want the variance to all be 1
#so I can compare the difference in questions accross questions appropriately.  Otherwise higher cells will dominate
#centering will make each row mean 0, which is not really what I want.  there is no constant being added to the responses, a count of the responese relaly means something


pca <- prcomp(na.omit(CellData.matrix), scale = TRUE) #nice, the Canadian spelling works!
names(pca)
pca["sdev"]
head(pca$x[,1:2])

#pca did it on the people rather than the quesitons...
#let me transpose and see if the problem persists
y <-t(CellData.matrix)
head(y)


pca2 <- prcomp(y, scale = TRUE)
pca2$x
#yea, that worked, wtf the first one??
plot(pca2)
#looks like the first component got it down pat

comp1 <- pca2$x[,1]
comp1 <- t(comp1)
comp1 <- data.frame(comp1)
comp1 <-tbl_df(comp1)
id <- rownames(comp1)
comp1 <- cbind(id=id, comp1)

colnames(comp1)
colnames(comp1)[2] <- "variance"
comp1$abs <- abs(comp1$variance)
arrange(comp1, -(abs))
#finally, I have rearranged to know what my variance is...but lost my qestions names...
#vert cool, so ther are several questions that caused the most variation in the data. 
#unforutnatly the seem to decrease linearly in significance....Let's look at the top 10

#v94 corresponds to? 

#the pca vectors are vectors with means for each column vector, so the first shows the 
#variance for quesitons 1 answer 1...ect
  
#what happens when I center?

pca3 <- prcomp(y, centre = FALSE, scale = TRUE)
plot(pca3)

comp1b <- pca3$x[,1]
comp1b <- t(comp1b)
comp1b <- data.frame(comp1b)
comp1b <-tbl_df(comp1b)
id <- rownames(comp1b)
comp1b <- cbind(id=id, comp1b)
  
colnames(comp1b)
colnames(comp1b)[2] <- "variance"
comp1b$abs <- abs(comp1b$variance)
arrange(comp1b, -(abs))

sum(comp1$variance, -comp1b$variance)

#ok, centering made no difference, as kind of expected...

#ok, I really don't understand why there are only so many V columns...need to check 
class(all.ans)
summary(all.ans)
#there are 122 questions
#each list item is a dataframe



q.number <- data.frame(count = 1:122)
q.number$count <- 0
head(q.number)
q.number[121,]

for (i in 1:length(all.ans)) { 
  q.number[i,1] <- nrow(data.frame(all.ans[i]))
}

colSums(q.number) #697 That was the number of columsn I shoudl have for LingData

q.number[50:122,]
sum(q.number[50:122,]) #even from question 50 I have 490 questions.... but

ncol(lingLocation) #471, there is a descrepancy....

#ok, I will make my own info from the lingLocation data...

zip.lingLoc
###################################################################################
#Convert all into binary

head(zip.lingLoc[,1:15])
nrow(zip.lingLoc)
length(unique(c(zip.lingLoc$CITY, zip.lingLoc$ZIP)))
length(unique(zip.lingLoc$ZIP))
#so we can average by zip... 

max(zip.lingLoc$Q050)



binarize <- function(column) {
  for (i in 1:max(zip.lingLoc[column])) {
    zip.lingLoc[paste0(column,i)] <- ifelse( zip.lingLoc[column]==i,1,0)
  }
}

for (i in 1:max(zip.lingLoc["Q052"])) {
  zip.lingLoc[paste0("Q052",i)] <- ifelse( zip.lingLoc["Q052"]==i,1,0)
}

binarize("Q053")


names(zip.lingLoc)
select(zip.lingLoc, Q0511:Q0531)[1:40,]

1:max(zip.lingLoc["Q051"])

zip.lingLoc <- select(zip.lingLoc, -(Q051i))

for i in colnames(zip.lingLoc)[50:122]
 binarize(i)

nrow(zip.lingLoc)
###############################################################################
###############################################################################
###############################################################################

A <- zip.lingLoc
names(zip.lingLoc)

for (i in 1:max(A["Q050"])) {
  A[paste0("Q050",i)] <- ifelse( A["Q050"]==i,1,0)
}

binarize1 <- function(column) {
  B <- data.frame(index=1:nrow(A))
  for (i in 1:max(A[[column]])) {
    B[, paste0(column,i)] <- ifelse( A[[column]]==i,1,0)
  }
  return(select(B, -(index)))
}
#do.call

#r.bind(list)

A <- cbind(A, binarize1("Q053"))
A <- cbind(A, select(binarize1("Q051")))
binarize1("Q053")
A
A[, "Q0532"]

names(P)



############
names(zip.lingLoc)
names <- colnames(zip.lingLoc)[4:70]
names
P <- do.call(cbind, lapply(names, binarize1))
P <- cbind(select(A, ID, CITY, ZIP, county), P)
P <-tbl_df(P)
select(filter(P, ID==47000), Q0501:Q0522)
filter(P, ID==47000)

#I should do some more sanity checks, but this seems pretty good: 
P
filter(P, county==NA)
#I can actually write a function that makes sure sav
saveRDS(P, file="finalbinarized.Rda")
savedfinal <- tbl_df(savedfinal)

final <- select(final, -V4, -V7)
final
ncol(final)
nrow(unique(final["ZIP"])) #11712
nrow(unique(final["county"])) #1503

final <- P

aggregate1 <- final %.% group_by(county) %.% summarise_each(funs(sum),-c(ID, CITY, ZIP, county))
#by_county <-group_by(final, county) 
#by_county <-tbl_df(by_county)
#aggregate1 <- by_county %>% summarise_each(funs(sum), -c(ID, CITY, ZIP, lat, long, state, county))
aggregate1 <- tbl_df(aggregate1)
aggregate1
filter(aggregate1, county==NA)
nrow(aggregate1) #1503
check <- group_by(final, county) %>% summarize(n=n())
nrow(check) #yes!  same number of rows, so my aggregate1 worked

by.county <- aggregate1
by.county[1500:1503,]
saveRDS(by.county, file="by_county.Rda")
#################################################################################
#################################################################################

saved <- readRDS(file="by_county.Rda")
head(saved)
###############################################################################
############################################################################


######################################################################
######################################################################

names <- colnames(states)[4:70]
names
A <- states
test <- do.call(cbind, lapply(names, binarize1))
test <- cbind(select(A, ID, CITY, ZIP, state), test)
test <-tbl_df(test)
summary(test)


stateSUM <- test %.% group_by(state) %.% summarise_each(funs(sum),-c(ID, CITY, ZIP, state))
stateSUM

saveRDS(stateSUM, file="by_state.Rda")

