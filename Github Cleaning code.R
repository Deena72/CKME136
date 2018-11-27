news <- read.csv("~/Desktop/OnlineNewsPopularity.csv",na.strings = c("","NA"), stringsAsFactors = F, header = T)
news <- data.frame(news)
# Checking the dataset
str(news)      ### all variables are numeric
head(news)
#remove non-predictive columns of url and timedelta 

news <- news[, -c(1:2)]
summary(news)


#Draw histograms to get a sense of the data

dim(news)
par(mfrow=c(3,4))
length(news)
for(i in 1:length(news))
{hist(news[,i], xlab=names(news)[i])}


#Get rid of errors (only keep instances where rate value < 1)

news <- news[news[,3]<1,]  # from n_unique_tokens  (1 error spotted)
news <- news[news[,4]<1,]  # from n_non_stop_words variable (none spotted)
news <- news[news[,5]<1,]  # from n_non_stop_unique_tokens variable (none spotted)


#Check Missing Values

sum(is.na(news))    # there are no missing values

#### CREATING CATEGORICAL VARIABLES

colnames(news)

#Convert the dummy variables to categorical variables for the weekdays

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
p1 <- ggplot(data=news, aes(as.factor(weekday), log(shares)))   #log shares is better as it is skewed
p1 + geom_boxplot()

### It shows that all days of the week have same effect except weekends, so remove all weekdays and just keep the variable is_weekend

# Remove unwanted dummy variables for the the days of the week that were transformed into categorical
news <- news[,-c(30:36, 60)]

dim(news)    ## After changing to categorical vars, new dataset has 52 variables  

#transform binary variables of 0, 1 into categorical (i.e all data channels and is_weekend)

news$is_weekend <- as.factor(news$is_weekend) 
news$data_channel_is_lifestyle <- as.factor(news$data_channel_is_lifestyle)
news$data_channel_is_entertainment <- as.factor(news$data_channel_is_entertainment)
news$data_channel_is_bus <- as.factor(news$data_channel_is_bus)
news$data_channel_is_socmed <- as.factor(news$data_channel_is_socmed)
news$data_channel_is_tech <- as.factor(news$data_channel_is_tech)
news$data_channel_is_world <- as.factor(news$data_channel_is_world)
news <- news[, c(1:11, 18:29, 31:51, 12:17, 30, 52)]# rearrange columns so that DV is the last column and categorical vars are towards the end


## CHECK CORRELATIONS
#Use findCorrelation in library caret to decide which numeric variables columns to remove for a correlation threshold of 0.85

#Remove Correlated Variables

library(corrplot)
library(caret)
dim(news)
x <- findCorrelation(news[,1:44], cutoff = .85, exact = FALSE) #get highly correlated numeric IVs to remove (correlation of 0.85 or higher)
noncornews <- news[,-x]  #keep only those not correlated
dim(noncornews)   
colnames(noncornews)


#Draw correlation matrix
dev.off()
cormatrix <- cor(noncornews[,1:18], method = "pearson")  ### only numeric vars
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cormatrix, method="color", col=col(200),   
         type="upper", order="hclust", 
         tl.col="black", tl.srt=45, tl.cex= 0.7, #Text label color and rotation
         # Combine with significance
         sig.level = 0.01, 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE
)
### dataset is reduced to 26 variables instead of 52 after removing variables of > 0.85 correlation


## CHECK DATA SKEWDNESS AND OUTLIERS   


### Fix skewdness of num_videos
cleannews <- noncornews
summary(cleannews)  

par(mfrow = c(1:2))
hist(cleannews$num_videos)           # highly skewed
hist(log(cleannews$num_videos))  ## taking the log does not fix it


length(which(cleannews$num_videos==0))  ## check how many instances not having videos
length(which(cleannews$num_videos>=1))  ## check how many instances having video
### Changing this variable into a categorical binary variable of with video= 1 or no video=0 will make it more balanced
cleannews$video <- NA
cleannews$video [cleannews$num_videos==0] <- 0
cleannews$video [cleannews$num_videos>=1] <- 1
cleannews$video <- as.factor(cleannews$video)
colnames(cleannews)
cleannews <- cleannews[, c(1:25, 27, 26)]  ## rearrange columns
cleannews <- cleannews[,-2]  # remove the numeric variable of videos num_videos

####Fix skewdness of shares
par(mfrow = c(1:2))
hist(cleannews$shares)           #highly skewed
hist(log(cleannews$shares))      #non-skewed

## FEATURE SELECTION OF REGRESSION DATASET 

#Create Regression Dataset with log(shares) as DV

newsreg <- cleannews
newsreg$shares <- log(newsreg$shares)
colnames(newsreg)

#Feature selection through Backward Elimination

full=lm(shares~., data=newsreg)
summary(full)
step(full, data=newsreg, direction='backward') 

#Feature selection through Forward Selection

null=lm(shares~1, data=newsreg)
step(null, scope=list(lower=null, upper=full), direction='forward')

#Remove variables based on results of feature selection 

newsreg <- newsreg[,c(1:5, 7, 9, 12:13, 15:20, 22:26)]  
dim(newsreg)
colnames(newsreg)


## FIX OTHER DATA SKEWDNESS
##### boxplots for numeric values only
dev.off()
boxplot(newsreg[,c(1:12)],
        main = "Multiple boxplots to check skewdness",
        par(mar = c(4, 12, 2, 0.5)),
        las = 2,
        col = c(2:13),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)

### Fix some skewdness in the data through taking log
newsreg$LDA_00 <- log(newsreg$LDA_00)
newsreg$LDA_01 <- log(newsreg$LDA_01)
newsreg$LDA_02 <- log(newsreg$LDA_02)
newsreg$LDA_03 <- log(newsreg$LDA_03)
boxplot(newsreg[,c(2:5)], las=2,  col = c(2:15) , par(mar = c(12, 3, 0.5, 4)+ 0.1))


## NORMALIZE NUMERIC VARS FOR REGRESSION DATASET 
#This feature is important since the scale used for the values for each variable might be different. 
#The best practice is to normalize the data and transform all the values to a common scale.

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }  # create normalize function
newsreg_norm <- as.data.frame(lapply(newsreg[1:12], normalize)) #only numeric vars are normalized, the rest are factors
newsreg <- cbind(newsreg_norm, newsreg[,c(13:20)]) 
str(newsreg)

#########################################################
#########################################################

### CLASSIFICATION DATASET
#Create a categorical DV 

newsclass <- cleannews
newsclass$shares <- as.factor(ifelse(newsclass$shares > 1400,1,0))


### FEATURE SELECTION FOR CLASSIFICATION
#Feature selection through Backward Elimination

full=glm(shares~., data=newsclass, family=binomial(link='logit'))
summary(full)
step(full, data=newsclass, direction='backward')

#Feature selection through Forward Selection

null=glm(shares~1, data=newsclass, family=binomial(link='logit'))
step(null, scope=list(lower=null, upper=full), direction='forward')

#Remove variables based on results of feature selection 

newsclass <- newsclass[,c(1:4, 7, 9, 12, 15:21, 23:26)] 
dim(newsclass)
colnames(newsclass)


## FIX DATA SKEWDNESS OF CLASSIFICATION DATASET

boxplot(newsclass[,c(1:10)],
        main = "Multiple boxplots to check skewdness",
        par(mar = c(4, 12, 2, 0.5)),
        las = 2,
        col = c(2:11),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)
newsclass$LDA_00 <- log(newsclass$LDA_00)
newsclass$LDA_01 <- log(newsclass$LDA_01)
newsclass$LDA_02 <- log(newsclass$LDA_02)
boxplot(newsclass[,c(2:4)], las=2,  col = c(2:15) , par(mar = c(12, 3, 0.5, 4)+ 0.1))


## NORMALIZE NUMERIC VARS FOR CLASSIFICATION DATASET

newsclass_norm <- as.data.frame(lapply(newsclass[1:10], normalize)) #only numeric vars are normalized, the rest are factors
newsclass <- cbind(newsclass_norm, newsclass[,c(11:18)]) 
str(newsclass)


## SAVING DATASETS FOR FURTHER ANALYSIS
#Save as .csv files

write.csv(newsreg, "~/Desktop/newsreg.csv")
write.csv(newsclass, "~/Desktop/newsclass.csv")


#Save as .Rda files

save(newsreg, file = "~/Desktop/newsreg.Rda")   
save(newsclass, file = "~/Desktop/newsclass.Rda") 



