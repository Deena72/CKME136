news <- read.csv("https://raw.githubusercontent.com/Deena72/CKME136/master/OnlineNewsPopularity.csv",na.strings = c("","NA"), stringsAsFactors = F, header = T)
news <- data.frame(news)

########  PREPROCESSING AND DATA CLEANING #########
str(news)      ### all variables are numeric
head(news)
### remove non-predictive columns of url and timedelta
news <- news[, -c(1:2)]
### check summary of the data
summary(news)
attach(news)
### Draw histograms to get a sense of the data
dim(news)
dev.off()
par(mfrow=c(3,4))
length(news)
for(i in 1:length(news))
{hist(news[,i], xlab=names(news)[i])}
dev.off()
### Get rid of errors (only keep instances where rate value < 1)
news <- news[news[,3]<1,]  # from n_unique_tokens  (1 error spotted)
news <- news[news[,4]<1,]  # from n_non_stop_words variable (none spotted)
news <- news[news[,5]<1,]  # from n_non_stop_unique_tokens variable (none spotted)

### CHECK MISSING VALUES
sum(is.na(news))    # there are no missing values

#### CREATING CATEGORICAL VARIABLES

colnames(news)
# convert the dummy variables to categorical variables for the news channel
news$channel <- "Other" 
news$channel[news$data_channel_is_lifestyle==1] <- "Lifestyle"
news$channel[news$data_channel_is_entertainment==1] <- "Entertainment"
news$channel[news$data_channel_is_bus==1] <- "Business"
news$channel[news$data_channel_is_socmed==1] <- "Social Media"
news$channel[news$data_channel_is_tech==1] <- "Technology"
news$channel[news$data_channel_is_world==1] <- "World"

sum(is.na(news$channel))
# Create the news Channel variable 
news$channel <-  factor(news$channel, levels = c("Other",
                                                 "Business", 
                                                 "Entertainment", 
                                                 "Lifestyle", 
                                                 "Technology", 
                                                 "World",
                                                 "Social Media"))


# convert the dummy variables to categorical variables for the weekdays
news$weekday <- NA
news$weekday [news$weekday_is_monday==1] <- "Monday"
news$weekday [news$weekday_is_tuesday==1] <- "Tuesday"
news$weekday [news$weekday_is_wednesday==1] <- "Wednesday"
news$weekday [news$weekday_is_thursday==1] <- "Thursday"
news$weekday [news$weekday_is_friday==1] <- "Friday"
news$weekday [news$weekday_is_saturday==1] <- "Saturday"
news$weekday [news$weekday_is_sunday==1] <- "Sunday"

# create the weekday variable 
news$weekday <- factor(news$weekday, levels = c( "Monday", "Tuesday", "Wednesday", "Thursday",
                                                 "Friday", "Saturday", "Sunday"))

# Check if is_weekend is the same as factors of weekday_is_saturday, weekday_is_sunday
sum(news$is_weekend) == sum(news$weekday_is_saturday) + sum(news$weekday_is_sunday)  # TRUE, hence perfect +ve correlation

# Check if the days of the week differ in determing the shares
library(ggplot2)
p1 <- ggplot(data=news, aes(as.factor(weekday), log(shares)))
p1 + geom_boxplot()

### It shows that all days of the week have same effect except weekends, so remove all weekdays and just keep the variable is_weekend
colnames(news)
# Remove unwanted dummy variables for the news channel and the days of the week that were transformed into categorical
news <- news[,-c(12:17, 30:36, 61)]
dim(news)    ## After changing to categorical vars, new dataset has 47 variables                                                
news$is_weekend <- as.factor(news$is_weekend) #transform is_weekend to categorical
news <- news[, c(1:23, 25:45, 24, 47, 46)]# rearrange columns so that DV is the last column and categorical vars are towards the end



### CHECK CORRELATIONS
### use findCorrelation in library caret to decide which numeric variables columns to remove for a correlation threshold of 0.85
install.packages("caret")
library(corrplot)
library(caret)
dim(news)
x <- findCorrelation(news[,1:44], cutoff = .85, exact = FALSE) #get highly correlated numeric IVs to remove
noncornews <- news[,-x]  #keep only those not correlated
dim(noncornews)  
colnames(noncornews)
cormatrix <- cor(noncornews[,1:18], method = "pearson")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cormatrix, method="color", col=col(200),   
         type="upper", order="hclust", 
         tl.col="black", tl.srt=90, #Text label color and rotation
         # Combine with significance
         sig.level = 0.01, 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE
)
### dataset is reduced to 21 variables instead of 47 after removing variables of > 0.85 correlation

####Call the new dataset cleannews
cleannews <- noncornews

#### CHECK DATA SKEWDNESS AND OUTLIERS
summary(cleannews)  
hist(cleannews$num_videos)       #highly skewed

### Fix skewdness of num_videos
length(which(cleannews$num_videos==0))  ## check how many instances of no videos
length(which(cleannews$num_videos>=1))  ## check how many instances having video
### Changing this variable into a categorical binary variable of with video= 1 or no video=0 will make it more balanced
cleannews$video <- NA
cleannews$video [cleannews$num_videos==0] <- 0
cleannews$video [cleannews$num_videos>=1] <- 1
cleannews$video <- as.factor(cleannews$video)
colnames(cleannews)
cleannews <- cleannews[, c(1:20, 22, 21)]  ## rearrange columns
cleannews <- cleannews[,-2]  # remove the numeric variable of videos

#### Fix skewdness of shares
hist(cleannews$shares)           #highly skewed
hist(log(cleannews$shares))      #non-skewed

### Create regression dataset with log(shares as DV)
newsreg <- cleannews
newsreg$shares <- log(newsreg$shares)
colnames(newsreg)


### FEATURE SELECTION FOR REGRESSION
### Backward Elimination
full=lm(shares~., data=newsreg)
summary(full)
step(full, data=newsreg, direction='backward') 

###Forward Selection
null=lm(shares~1, data=newsreg)
step(null, scope=list(lower=null, upper=full), direction='forward')

#### Final dataset to work with for regression
newsreg <- newsreg[,c(1:5, 7, 9, 12:13, 15:21)]  
dim(newsreg)
colnames(newsreg)

##### boxplots for numeric values only
boxplot(newsreg[,c(1:12)], las=2,  col = c(2:15), par(mar = c(12, 3, 0.5, 4)+ 0.1)) 

### Fix some skewdness in the data through taking log
newsreg$LDA_00 <- log(newsreg$LDA_00)
newsreg$LDA_01 <- log(newsreg$LDA_01)
newsreg$LDA_02 <- log(newsreg$LDA_02)
newsreg$LDA_03 <- log(newsreg$LDA_03)
boxplot(newsreg[,c(2:5)], las=2,  col = c(2:15) , par(mar = c(12, 3, 0.5, 4)+ 0.1))
dev.off()

### PREPARE CLASSIFICATION DATASET (DV is Categorical)
newsclass <- cleannews
newsclass$shares <- as.factor(ifelse(newsclass$shares > 1400,1,0))

### FEATURE SELECTION FOR CLASSIFICATION
### Backward Elimination
full=glm(shares~., data=newsclass, family=binomial(link='logit'))
summary(full)
step(full, data=newsclass, direction='backward') 

###Forward Selection
null=glm(shares~1, data=newsclass, family=binomial(link='logit'))
step(null, scope=list(lower=null, upper=full), direction='forward')

#### Final dataset to work with for classification
newsclass <- newsclass[,c(1:4, 7, 9, 12, 15:21)] 
### Fix skewed variables 
boxplot(newsclass[,c(1:10)], las=2,  col = c(2:15), par(mar = c(12, 3, 0.5, 4)+ 0.1))
newsclass$LDA_00 <- log(newsclass$LDA_00)
newsclass$LDA_01 <- log(newsclass$LDA_01)
newsclass$LDA_02 <- log(newsclass$LDA_02)
boxplot(newsclass[,c(2:4)], las=2,  col = c(2:15) , par(mar = c(12, 3, 0.5, 4)+ 0.1))


### save the two datframes as csv files to be used in WEKA
write.csv(newsreg, "~/Desktop/newsreg.csv")
write.csv(newsclass, "~/Desktop/newsclass.csv")