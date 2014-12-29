setwd(file.path("/Users/LishaLi/Desktop/215A/Lab2"))
options(warn=0)
library(maps)
library(ggplot2)
library(dplyr)
library(maps)
#the zipcodes will get turned to numerics, so we need to read them as 
#characters to avoid dropping of 0s in front
lingData <- read.table('lingData.txt', header = T, colClasses=c("numeric", rep("character", 3), rep("numeric", 69)))
lingData <-tbl_df(lingData) #yeah!  Did not screw up the zips
lingLocation <- read.table('lingLocation.txt', header = T)
lingLocation <-tbl_df(lingLocation)
class(lingLocation[1,3]) #double checking it imported things correctly
load("question_data.RData")

zipcode <- read.table('ZIP_CODES.txt', sep = ",", colClasses=c("character",rep("character", 6)))
zipcode <- tbl_df(zipcode)
colnames(zipcode)[1] <- "ZIP"    #, so it's the same name as LingDAta'
colnames(zipcode)[2] <- "lat"
colnames(zipcode)[3] <- "long"
colnames(zipcode)[5] <- "state"
colnames(zipcode)[6] <- "county"
nrow(zipcode)
colnames(zipcode)

group_by(zipcode, ZIP)

#now see if we can reference this data: 


#I want to do an leftjoin between my zipcode data and my lingLocation
nrow(lingData)
nrow(zipcode) # I only care for the zipcodes in lingLocation
nrow(unique(lingData["ZIP"]))#only this many zipcodes that are unique, that's fine...
nrow(unique(zipcode["ZIP"]))

test <- left_join(lingData, zipcode, by="ZIP")

test<-tbl_df(test)
test
select(test, CITY, STATE, ZIP, county, state, lat.x, long.x, lat.y, long.y)
#let me make lat and long the same as long as they agree within a degree or two
#sanity check that the states match...


#finally also join the lingLocation data.  
mutate(test, lat.y = as.numeric(lat.y), long.y = as.numeric(long.y))
colnames(test)
as.numeric(test[3, "lat.y"])
is.numeric(test$lat.y)
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

head(zip.lingLoc["county"]) #they are all uppercase...whereas the ggplot2 data will be lower case, so let's just change it
county <- tolower(zip.lingLoc$county)
zip.lingLoc$county <- county
head(zip.lingLoc["county"])
#great it worked!

#deal with these problems later:
sum(is.na(zip.lingLoc$lat))
sum(is.na(zip.lingLoc$long))
###############################################################################
#let's first copy and paste the question example here: 

############
# Make a plot similar to the website for the second person plural answers.
# You may want to join these data sets more efficiently than this.
#plural.second.person <- filter(lingData, Q050 %in% c(1, 2, 9), long > -125)
#we keep things in the continental US
answers.q50 <- all.ans[['50']]
#this gives us the answers to everything for question 50
head(lingData["Q050"]%in% c(1,2,9))


# Make the column to join on.  They must be the same type.
answers.q50$Q050 <- rownames(answers.q50)
plural.second.person$Q050 <- as.character(plural.second.person$Q050)
plural.second.person <- inner_join(plural.second.person, answers.q50, by="Q050")

#########################

#finally, let's try to use this data to plot...before we do that, we need the maps data

county_df <- map_data("county")
head(county_df["county"])
colnames(county_df)
names(county_df) <- c("long", "lat", "group", "order", "state_name", "county")
county_df$state <- state.abb[match(county_df$state_name, tolower(state.name))]
county_df$state_name <- NULL #renamed state to lower abb so delete state_name now


# Load the outlines of the states.
state_df <- map_data("state")
names(state_df)

head(county_df)

nrow(zip.lingLoc)
# Combine together.
mapDATAcombine <- left_join(zip.lingLoc, select(county_df, group, order, county, state), by = c("county"))
nrow(mapDATAcombine)
mapDATAcombine <- mapDATAcombine[order(mapDATAcombine$order), ]
head(select(group_by(mapDATAcombine, order), order))
sum(is.na(mapDATAcombine$order))
#this next variable needs to be what we are plotting...but cut into breaks
names(mapDATAcombine)
nrow(filter(mapDATAcombine, long.x-long.y >5)) #ok, so everything is within 5 degrees, that is not great, but I trust my zip data for now, I will just ignore the county data
nrow(filter(mapDATAcombine, lat.x-lat.y >3)) #lat data is doing very well..only 6 differ by more than 3 degrees

colnames(mapDATAcombine)[77] <-"lat" #just use the zip.linLoc Data
colnames(mapDATAcombine)[78] <-"long"
mapDATAcombine <- tbl_df(mapDATAcombine)
names(mapDATAcombine)
summary(mapDATAcombine["Q050"])
#this is to create questions we care about and map: 

ggplot()+geom_histogram(mapDATAcombine, aes(y=Q050))
nrow(mapDATAcombine)
short <- mapDATAcombine[1:50000, ]
s <- ggplot(data=NULL, aes(long, lat)) +
  geom_polygon(data=short, aes(fill = Q0100, group = group), colour = "white", alpha=1/2, size = 0.2) + 
  geom_polygon(data = state_df, colour = "white", fill = NA, aes(group = group)) 

print(s)

head(mapDATAcombine["Q050"])
#too big to plot below:
p <- ggplot(data=NULL, aes(long, lat)) +
  geom_polygon(data=mapDATAcombine, aes(fill = Q05, group = group), colour = "white", alpha=1/2, size = 0.2) + 
  geom_polygon(data = state_df, colour = "white", fill = NA, aes(group = group)) 

print(p)


#rearrange descending by number of people in the cell

#pretty cool that the lat,long plot kind of looks like the US, and the
#high population samples came from high populartion density regions.  
lingloc <- ggplot(lingLocation, aes(y = Latitude, x = Longitude))
lingloc + geom_point(alpha = 0.75, aes(colour = Number.of.people.in.cell))

lingLocation <-tbl_df(lingLocation)
colnames(lingLocation)
colnames(lingData)
lingData["ZIP"]

#datareduction (PCA:SVD, eigenvalues...why scale or not, interpretable, aggreagation in bigger geographic chunks, mixture models spectral clusterings, and any other thing i want, think about what I am doing makes sense, interpretable)
#look at visualization...etc.  

#orthogonality condition makes second less interpretable.  but it is still useful to reduce dimensions, then use clustering to make sense of data 
#idea of the random model that generated this

#stability.  Results robust?  
#read the papers, asesss the experimenters ambitions, how realistic?  

#use maps with ggplot, using maps...etc  


# lingData has a column for each question, and lingLocation has a column
# for each question x answer.  Sorry the columns in lingLocation are not usefully named,
# but it's not too tricky to figure out which is which.
# Note that you still need to clean this data (check for NA's, missing location data, etc.)
names(lingData)
names(lingLocation)
state.df <- map_data("state")
state.df <- tbl_df(state.df)
blank.theme <-
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 



# Plot!
ggplot(data=NULL) +
  geom_point(data=plural.second.person, aes(x=long, y=lat, color=ans), size=3, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme



###############
# Plot the lingLocation data (which lives on a grid).  Note that this doesn't look great with
# state outlines.  You can probably do better!
ggplot(data=NULL) +
  geom_tile(data=filter(lingLocation, Longitude > -125),
            aes(x=Longitude, y=Latitude, color=log10(V12), fill=log10(V12))) +
  geom_polygon(data=state.df, colour = "gray", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme



#ok, this hurts my eyes, let's try with geom_polygon:

ggplot(data=NULL) +
  geom_tile(data=filter(mapDATA, Longitude > -125),
            aes(x=Longitude, y=Latitude, color=log10(V12), fill=log10(V12)))+
  geom_polygon(data=state.df, colour = "gray", fill = NA, aes(x=long, y=lat, group=group))
+blank.theme

#geom_polygon(data= choropleth, aes(fill = rate_d, group = group),
#colour = "white", alpha=1/2, size = 0.2)

linLoc <- filter(lingLocation, Longitude >-125)

head(state.df)
