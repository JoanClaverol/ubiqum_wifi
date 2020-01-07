# MENTOR: code structure
# GOAL: 
# DESCRIPTION: 
# DEVELOPER: 
# DATE: 


library(readr)
library(caret)
library(dplyr)
library(ggplot2)

# Load data----
trainingData <- read.csv("Data etc/trainingData.csv")

#How much of the values are not detected and therefore 100----
# Remove columns (WAP) where all the values = 100 (WAP was not detected)
uniquelength <- sapply(trainingData,function(x) length(unique(x)))
trainingData <- subset(trainingData, select=uniquelength>1)

# MENTOR: AVOID THESE TYPES OF FUNCTIONS (Rmd.)
length(trainingData)
nrow(trainingData) #19937 rows
str(trainingData) #removed are all columns (55) with only WAP100. Left = 474

# Remove rows (WAP) where all the values = 100 (WAP was not detected)----
keep <- apply(trainingData[,1:465], 1, function(x) length(unique(x[!is.na(x)])) != 1)
trainingData <- trainingData[keep, ]
nrow(trainingData) #19861 85 rows removed

# Change WAP values so that no signal is 0 and highest signal is 104----
trainingData[trainingData == 100] <- -105 # if 100 than make it -105
trainingData[,1:465] <- trainingData[,1:465] + 105  #make all WAP's positive 
#by adding +105
summary(trainingData$WAP111) #check conversion of WAP's to positives

# Converting data types----
# MENTOR: AVOID HARD CODING
trainingData$FLOOR <- as.factor(trainingData$FLOOR)
trainingData$BUILDINGID <- as.factor(trainingData$BUILDINGID)
trainingData$RELATIVEPOSITION <- as.factor(trainingData$RELATIVEPOSITION)
trainingData$USERID <- as.factor(trainingData$USERID)
trainingData$PHONEID <- as.factor(trainingData$PHONEID)

# Check distribution of signal strength with plots----
# traning data
# MENTOR: AVOID HARD CODING, SPECIFY VARIABLE NAMES (SELECT())
x <- trainingData[,1:465]
x <- stack(x)
x <-x[-grep(0, x$values),]
# MENTOR: PLOT IN RMD. 
hist(x$values, xlab = "WAP strength", 
     main = "Distribution of WAPs signal stength (Training set)", col = "red")

ggplot() +
  geom_histogram(data = x, aes(values), fill = "blue", alpha = 1, binwidth = 5) +
  ggtitle("Distribution of WAPs signal strength (Training set)") +
  xlab("WAP strength")

# Convert Longitude and Latitude values to absolute values----
# Latitude
trainingData$LATITUDE <- trainingData$LATITUDE - min(trainingData$LATITUDE)
trainingData$LATITUDE<-round(trainingData$LATITUDE, digits = 1)

# Longitude
trainingData$LONGITUDE <- trainingData$LONGITUDE - min(trainingData$LONGITUDE)
trainingData$LONGITUDE <- round(trainingData$LONGITUDE, digits = 1)

# check maximum values for longitude and latitude----
max(trainingData$LATITUDE)
max(trainingData$LONGITUDE)

#remove all rows where LONGITUDE values from test set that are higher than in train set
#testData<-testData[!(testData$LONGITUDE > 390.5194), ]

# Locations at which users logged in 
# Red colour is outside the room, black inside
Plot_long_lat <- ggplot(trainingData, aes(trainingData$LONGITUDE, trainingData$LATITUDE))
Plot_long_lat + geom_point(colour = as.factor(trainingData$RELATIVEPOSITION)) +
  xlim(0, 400) +
  ylim(0, 300) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle ("Locations trainsset which users logged in (Red = outside room, black = inside)")

# Training log in locations----
ggplot() +
  geom_point(data = trainingData, aes(x = LONGITUDE, y = LATITUDE, colour = "Training dataset")) +
  #geom_point(data = testData, aes(x = LONGITUDE, y = LATITUDE, colour = "Test dataset")) +
  ggtitle("Log In Locations (Training set)") 

# Subset data per building----
BB0 <- subset(trainingData, BUILDINGID == 0)
BB1 <- subset(trainingData, BUILDINGID == 1)
BB2 <- subset(trainingData, BUILDINGID == 2)

# Plot Users per building----
# BB0----
# MENTOR: GOAL & DESCRIPTION OF THE FUNCTION
plotForUSer<-function(builNum) {
  builId<- paste("B.", builNum, sep = " " )
  plotTitle<- paste("Building", builNum, ": Which users account for floor location", 
                    sep=" ")
  
  message(paste("building Id:", builId))
  message(paste("Plot title:", plotTitle))
  
  
  BB0%>% 
    ggplot()+
    geom_point(aes(x=LONGITUDE, y= LATITUDE, color=USERID)) + 
    facet_grid(. ~ FLOOR) + 
    labs(title=plotTitle) + 
    theme_linedraw(base_size = 11, base_family = "") + 
    theme(plot.title = element_text(hjust = 0.5, face="bold"))
}

plotForUSer("0")

# BB1----
plotForUSer<-function(builNum) {
  builId<- paste("B.", builNum, sep = " " )
  plotTitle<- paste("Building", builNum, ": Which users account for floor location", 
                    sep=" ")
  
  message(paste("building Id:", builId))
  message(paste("Plot title:", plotTitle))
  
  
  BB1 %>% 
    ggplot()+
    geom_point(aes(x=LONGITUDE, y= LATITUDE, color=USERID)) + 
    facet_grid(. ~ FLOOR) + 
    labs(title=plotTitle) + 
    theme_linedraw(base_size = 11, base_family = "") + 
    theme(plot.title = element_text(hjust = 0.5, face="bold"))
}

plotForUSer("1")

# BB2----
plotForUSer<-function(builNum) {
  builId<- paste("B.", builNum, sep = " " )
  plotTitle<- paste("Building", builNum, ": Which users account for floor location", 
                    sep=" ")
  
  message(paste("building Id:", builId))
  message(paste("Plot title:", plotTitle))
  
  
  BB2 %>% 
    ggplot()+
    geom_point(aes(x=LONGITUDE, y= LATITUDE, color=USERID)) + 
    facet_grid(. ~ FLOOR) + 
    labs(title=plotTitle) + 
    theme_linedraw(base_size = 11, base_family = "") + 
    theme(plot.title = element_text(hjust = 0.5, face="bold"))
}

plotForUSer("2")

#Plot number of locations by User----
plot(trainingData$USERID,
     type ="h",
     xlab="USER NUMBER", ylab="frequency",
     main="Number of locations by User",
     col="turquoise3")

#Plot number of locations by Phone id----
plot(trainingData$PHONEID,
     type ="h",
     xlab="PHONE ID", ylab="frequency",
     lwd = 5,
     main="Number of locations by Phone",
     col="turquoise3")

#Inspect location registrations on relative position 1 = in, 2= out----
# MENTOR: HARD CODING
BB0----
  BB0%>%
  filter(FLOOR==0)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # about 20 outside room

BB0%>%
  filter(FLOOR==1)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # 19-29 outside room

BB0%>%
  filter(FLOOR==2)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # 19-20 outside room

BB0%>%
  filter(FLOOR==3)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # 19-30 outside room

#BB1----
BB1%>%
  filter(FLOOR==0)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # mostly 20 outside room

BB1%>%
  filter(FLOOR==1)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # high counts inside room

BB1%>%
  filter(FLOOR==2)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # low inside, 10-20 outside

BB1%>%
  filter(FLOOR==3)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # low inside,about 20 outside

#BB2----
BB2%>%
  filter(FLOOR==0)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # also inside but mostly outside

BB2%>%
  filter(FLOOR==1)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # relatively high counts inside room

BB2%>%
  filter(FLOOR==2)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # 10-20 in- and outside

BB2%>%
  filter(FLOOR==3)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # lower inside,about 20 outside

BB2%>%
  filter(FLOOR==4)%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  # 10 only outside

# Remove columns (WAP) where all the values = 100 (WAP was not detected)----
uniquelength <- sapply(BB0,function(x) length(unique(x)))
BB0 <- subset(BB0, select=uniquelength>1)
uniquelength <- sapply(BB1,function(x) length(unique(x)))
BB1 <- subset(BB1, select=uniquelength>1)
uniquelength <- sapply(BB2,function(x) length(unique(x)))
BB2 <- subset(BB2, select=uniquelength>1)

# Remove rows (WAP) where all the values = 0 (WAP was not detected)
keep <- apply(BB0[,1:200], 1, function(x) length(unique(x[!is.na(x)])) != 1)
BB0[keep, ]
uniquelength <- sapply(BB1,function(x) length(unique(x)))
BB1 <- subset(BB1, select=uniquelength>1)
uniquelength <- sapply(BB2,function(x) length(unique(x)))
BB2 <- subset(BB2, select=uniquelength>1)

#### Subset Building_Floor ####
# B0----
BB0Floor0 <- subset(BB0, FLOOR == 0)
BB0Floor1 <- subset(BB0, FLOOR == 1)
BB0Floor2 <- subset(BB0, FLOOR == 2)
BB0Floor3 <- subset(BB0, FLOOR == 3)

# B1----
BB1Floor0 <- subset(BB1, FLOOR == 0)
BB1Floor1 <- subset(BB1, FLOOR == 1)
BB1Floor2 <- subset(BB1, FLOOR == 2)
BB1Floor3 <- subset(BB1, FLOOR == 3)

# B2----
BB2Floor0 <- subset(BB2, FLOOR == 0)
BB2Floor1 <- subset(BB2, FLOOR == 1)
BB2Floor2 <- subset(BB2, FLOOR == 2)
BB2Floor3 <- subset(BB2, FLOOR == 3)
BB2Floor4 <- subset(BB2, FLOOR == 4)

#Remove columns (WAP) where all the values = 100 (WAP was not detected)----
#uniquelength <- sapply(BB0Floor0,function(x) length(unique(x)))
#BB0Floor0 <- subset(BB0Floor0, select=uniquelength>1)
#str(BB0Floor0)
#Remove rows (WAP) where all the values = 100 (WAP was not detected)----
#keep <- apply(BB0Floor0[,1:125], 1, function(x) length(unique(x[!is.na(x)])) != 1)
#BB0Floor0[keep, ]

# Check userid/phone per building----
unique(BB0$USERID) # 2 different user IDs -> 11 1
unique(BB0$PHONEID) # 2 different phone IDs -> 13 14
unique(BB1$USERID) # 12 different user IDs -> 2  4  7  8  9 16 10 11 13 14 17 18
unique(BB1$PHONEID) # 11 different phone IDs -> 23 18  6  1 14  8 13 17  7 22 10
unique(BB2$USERID) # 16 different user IDs -> 2 11  3  5  6  7  8  9 10 12 13 14 15 16 17 18
unique(BB2$PHONEID) # 15 different phone IDs -> 23 13 16  3 19  6  1 14  8 24 17  7 11 22 10

#### 3D scatterplot of floors per building ####
## BB0----
BB0Floor0$z <- 0
BB0Floor1$z <- 1
BB0Floor2$z <- 2
BB0Floor3$z <- 3

PPPlotBB0 <- rbind(BB0Floor0,BB0Floor1)
PPPlotBB0 <- rbind(PPPlotBB0,BB0Floor2)
PPPlotBB0 <- rbind(PPPlotBB0,BB0Floor3)

PPPlotBB0 <- PPPlotBB0[,201:209]
z <- PPPlotBB0$z
x <- PPPlotBB0$LONGITUDE
y <- PPPlotBB0$LATITUDE
scatterplot3d::scatterplot3d(x, y, z, pch = 20, angle = 45, 
                             color = PPPlotBB0$RELATIVEPOSITION, main = "Building 0 Log In points")

# BB1----
BB1Floor0$z <- 0
BB1Floor1$z <- 1
BB1Floor2$z <- 2
BB1Floor3$z <- 3

PPPlotBB1 <- rbind(BB1Floor0,BB1Floor1)
PPPlotBB1 <- rbind(PPPlotBB1,BB1Floor2)
PPPlotBB1 <- rbind(PPPlotBB1,BB1Floor3)
PPPlotBB1
PPPlotBB1 <- PPPlotBB1[,208:216]
a <- PPPlotBB1$z
b <- PPPlotBB1$LONGITUDE
c <- PPPlotBB1$LATITUDE
scatterplot3d::scatterplot3d(a, b, c, angle = 60, pch = PPPlotBB1$z, 
                             color = PPPlotBB1$RELATIVEPOSITION, 
                             main = "Building 1 Log In points" )
# 
# BB2----
BB2Floor0$z <- 0
BB2Floor1$z <- 1
BB2Floor2$z <- 2
BB2Floor3$z <- 3
BB2Floor4$z <- 4

PPPlotBB2 <- rbind(BB2Floor0,BB2Floor1)
PPPlotBB2 <- rbind(PPPlotBB2,BB2Floor2)
PPPlotBB2 <- rbind(PPPlotBB2,BB2Floor3)
PPPlotBB2 <- rbind(PPPlotBB2,BB2Floor4)
PPPlotBB2 <- PPPlotBB2[,204:212]
c <- PPPlotBB2$z
a <- PPPlotBB2$LONGITUDE
b <- PPPlotBB2$LATITUDE
scatterplot3d::scatterplot3d(a, b, c, angle = 20, pch = PPPlotBB2$z, 
                             color = PPPlotBB2$RELATIVEPOSITION)
BB0 #5,249 rows, 208 columns
BB1 #5,196 rows, 215 columns
BB2 #9,492 rows, 211 columns
View(BB2)

# Check where is the highest signal strength----
# MENTOR: HARD CODING
which(BB0[,1:200] == 30)
which(BB0[,1:200] == 30, arr.ind=TRUE)
which(BB1[,1:207] == 30)
which(BB1[,1:207] == 30, arr.ind=TRUE)
which(BB2[,1:203] == 30)
which(BB2[,1:203] == 30, arr.ind=TRUE)

# ~~~~Signal Strenghts ~~~~~~~~~----

#Good Signal----
Signal0_60 <- which(apply(trainingData[, 1:465], 1, function(x) 
  length(which(x > 60))) > 0)

GoodSignal <- trainingData[Signal0_60, ]

Plot_GoodSignal <- ggplot(GoodSignal, aes(GoodSignal$LONGITUDE, 
                                          GoodSignal$LATITUDE))
Plot_GoodSignal + 
  geom_point(colour = as.factor(GoodSignal$RELATIVEPOSITION)) +
  ggtitle("Log in locations where WAP signal was high") +
  xlab("Longitude") +
  ylab("Latitude")

# Remove columns (WAP) where all the values = 0 (WAP was not detected)
uniquelength <- sapply(GoodSignal,function(x) length(unique(x)))
GoodSignal <- subset(GoodSignal, select=uniquelength>1)

# Remove rows (WAP) where all the values = 0 (WAP was not detected)
keep <- apply(GoodSignal[,1:183], 1, function(x) length(unique(x[!is.na(x)])) != 1)
GoodSignal[keep, ]

Plot_GoodSignal <- ggplot(GoodSignal, aes(GoodSignal$LONGITUDE, 
                                          GoodSignal$LATITUDE))
Plot_GoodSignal + 
  geom_point(colour = as.factor(GoodSignal$RELATIVEPOSITION)) +
  ggtitle("Log in locations where WAP signal was high") +
  xlab("Longitude") +
  ylab("Latitude")

# Medium Signal----
def <- which(apply(trainingData[, 1:465], 1, function(x) length(which(x > 20 & x < 60))) > 0)
Mediumsignal <- trainingData[def, ]

ms <- ggplot(Mediumsignal, aes(Mediumsignal$LONGITUDE, Mediumsignal$LATITUDE))
ms + geom_point(colour = as.factor(Mediumsignal$RELATIVEPOSITION))

# Remove columns (WAP) where all the values = 0 (WAP was not detected)
uniquelength <- sapply(Mediumsignal,function(x) length(unique(x)))
Mediumsignal <- subset(Mediumsignal, select=uniquelength>1)

# Remove rows (WAP) where all the values = 0 (WAP was not detected)
keep <- apply(Mediumsignal[,1:183], 1, function(x) length(unique(x[!is.na(x)])) != 1)
Mediumsignal[keep, ]

# Bad Signal----
ghi <- which(apply(trainingData[, 1:465], 1, function(x) length(which(x > 0 & x < 20))) > 0)
BadSignal <- trainingData[ghi, ]

bs <- ggplot(BadSignal, aes(BadSignal$LONGITUDE, BadSignal$LATITUDE))
bs + geom_point(colour = as.factor(BadSignal$RELATIVEPOSITION))