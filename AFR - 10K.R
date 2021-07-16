################################### Read Data ##########################################
########################################################################################

            #Customize with your own working directory
setwd("C:/Users/diego/Documents/R/Projects/GitHub_Projects/Apartment For Rent project")

dir.create("Files")

library(tidyverse)
library(readr)
library(dplyr)
library(data.table) 
library(readxl)
library(ggplot2)

#Import Dataset
AFR <- read_excel("./Files/apartments_for_rent_classified_10K.xlsx")
AFR <- as.data.frame(AFR)
class(AFR)
str(AFR)
dim(AFR)
summary(AFR)
unique(AFR)
colnames(AFR)

################################ Preprocessing ############################################
########################################################################################


#id,body,tile
AFR <- AFR[,-c(1,3,4)]

#category
table(AFR$category)
unique(AFR$category)


x <- filter(AFR, AFR$category == "housing/rent/home" | AFR$category == "housing/rent/short_term")
            #only aprtments are taken into account
AFR$category[ which(AFR$category == "housing/rent/home") ] <- NA
AFR$category[ which(AFR$category == "housing/rent/short_term") ] <- NA
AFR <- AFR[!is.na(AFR$category),] 


#amenities 
table(AFR$amenities)
unique(AFR$amenities)

x <- filter(AFR, AFR$amenities == "null" | AFR$amenities == "none" | AFR$amenities == "None" | is.na(AFR$amenities))
AFR$amenities <- sapply(strsplit(AFR$amenities, ","), length) #replaced with amount of amenities

min(AFR$amenities)
max(AFR$amenities)

AFR$amenities <- as.numeric(AFR$amenities)

#create ordinal version of amenities

AFR <- AFR %>% 
  mutate(amenities2 = case_when((amenities <= 5) ~ "Basic",
                                    (amenities > 5 & amenities <= 11) ~ "Comfort",
                                    (amenities > 11) ~ "Luxury"))


#get correlation between bathrooms and bedrooms

data <- AFR [,c(3,4)]
x<- filter(data, data$bathrooms == "null" | is.na(data$bathrooms))
data$bathrooms[ which(data$bathrooms == "null") ] <- NA
x<- filter(data, data$bedrooms == "null" | is.na(data$bedrooms) | data$bedrooms == "0")
data$bedrooms[ which(data$bedrooms == "null") ] <- NA
data$bedrooms[ which(data$bedrooms == "0") ] <- NA

data <- data[complete.cases(data), ]
data$bathrooms <- as.numeric(data$bathrooms)
data$bedrooms <- as.numeric(data$bedrooms)

cor(data$bathrooms,data$bedrooms)

ggplot(data = data,mapping = aes(x = bathrooms, y = bedrooms)) +
  geom_point(color = "black", position = "jitter") + 
  geom_smooth(method='lm', color = "red") +
  labs(title = "Bathrooms vs Bedrooms",
       x = "N_bathrooms",
       y = "N_bedrooms") 
  

#Bathrooms
table(AFR$bathrooms)
unique(AFR$bathrooms)
x<- filter(AFR, AFR$bathrooms == "null" | is.na(AFR$bathrooms))

AFR$bathrooms[ which(AFR$bathrooms == "null") ] <- AFR$bedrooms[ which(AFR$bathrooms == "null") ] 
x<- filter(AFR, AFR$bathrooms == "null" | is.na(AFR$bathrooms))

table(AFR$bathrooms)

AFR$bathrooms[ which(AFR$bathrooms == "null") ] <- 1 
AFR$bathrooms[ which(AFR$bathrooms == "0") ] <- 1
table(AFR$bathrooms)

AFR$bathrooms <- as.numeric(AFR$bathrooms)

#Bedrooms
table(AFR$bedrooms)
unique(AFR$bedrooms)

x<- filter(AFR, AFR$bedrooms == "0" | AFR$bedrooms == "null")

AFR$bedrooms[ which(AFR$bedrooms == "0") ] <- AFR$bathrooms[ which(AFR$bedrooms == "0") ]
AFR$bedrooms[ which(AFR$bedrooms == "null") ] <- AFR$bathrooms[ which(AFR$bedrooms == "null") ] 

table(AFR$bedrooms)
unique(AFR$bedrooms)

AFR$bedrooms <- as.numeric(AFR$bedrooms)

cor(AFR$bathrooms,AFR$bedrooms)

#currency 
table(AFR$currency)
unique(AFR$currency)


#fee
table(AFR$fee)
unique(AFR$fee)

#has_photo
table(AFR$has_photo)
unique(AFR$has_photo)

#create numeric version of has_photo
AFR <- AFR %>% 
  mutate(has_photo2 = case_when((has_photo == "No") ~ 0,
                                   (has_photo == "Thumbnail") ~ 1,
                                   (has_photo == "Yes") ~ 2))

#pets_allowed
table(AFR$pets_allowed)
unique(AFR$pets_allowed)
AFR$pets_allowed[ which(AFR$pets_allowed == "null") ] <- "None"


#create numeric version of pets_allowed
AFR <- AFR %>% 
  mutate(pets_allowed2 = case_when((pets_allowed == "None") ~ 0,
                                (pets_allowed == "Dogs") ~ 1,
                                (pets_allowed == "Cats") ~ 1,
                                (pets_allowed == "Cats,Dogs") ~ 2))
         


#price
table(AFR$price)
unique(AFR$price)
x <- filter(AFR, is.na(AFR$price) | AFR$price == "null" | AFR$price == "None" | AFR$price == "none" | AFR$price == "No")
dim(x)

AFR$price <- as.numeric(AFR$price)

#create ordinal version of price
Q1 <- quantile(AFR$price, 0.25)
Q1
Q3 <- quantile(AFR$price, 0.75)
Q3


AFR <- AFR %>% 
       mutate(price_category = case_when((price <= Q1) ~ "Low",
                                          (price > Q1 & price <= Q3) ~ "Medium",
                                          (price > Q3) ~ "High"))


#price_display 
table(AFR$price)
unique(AFR$price_display)


#price type
table(AFR$price_type)
unique(AFR$price_type)

x<- filter(AFR, AFR$price_type == "Weekly" | is.na(AFR$price_type) | AFR$price_type == "Monthly|Weekly")
                      #only monthly prices are taken into account
AFR$price_type[ which(AFR$price_type == "Weekly") ] <- NA
AFR$price_type[ which(AFR$price_type == "Monthly|Weekly") ] <- "Monthly"

AFR <- AFR[!is.na(AFR$price_type),] 


#square feat
table((AFR$square_feet))
AFR$square_feet<- as.numeric(AFR$square_feet)

#address 
table(AFR$address)
unique(AFR$address)


#Latitude
table(AFR$latitude)
x <- filter(AFR, is.na(AFR$latitude) | AFR$latitude == "null" | AFR$latitude == "None" | AFR$latitude == "none" | AFR$latitude == "No")
dim(x)
AFR$latitude[ which(AFR$latitude == "null") ] <- NA
AFR <- AFR[!is.na(AFR$latitude),] 
AFR$latitude <- as.numeric(AFR$latitude)
AFR$latitude <- AFR$latitude/10000 


#Longitude
table(AFR$longitude)
x <- filter(AFR, is.na(AFR$longitude) | AFR$longitude == "null" | AFR$longitude == "None" | AFR$longitude == "none" | AFR$longitude == "No") 
dim(x)
AFR$longitude <- as.numeric(AFR$longitude)
AFR$longitude <- AFR$longitude/10000 


#cityname
table(AFR$cityname)
unique(AFR$cityname)

x<- filter(AFR, AFR$cityname == "null" | is.na(AFR$cityname) | AFR$cityname == "none" | AFR$cityname == "None")

x<- filter(AFR, AFR$latitude == "39.8163" | AFR$longitude == "-98.5576")

AFR$cityname[AFR$latitude == "39.8163" | AFR$longitude == "-98.5576"] <- "Oak"

AFR$state[AFR$latitude == "39.8163" | AFR$longitude == "-98.5576"] <- "KS"

x<- filter(AFR, AFR$cityname == "null" | is.na(AFR$cityname) | AFR$cityname == "none" | AFR$cityname == "None")

AFR$cityname[AFR$latitude == "28.459" | AFR$longitude == "-82.1971"] <- "Trilby"
AFR$state[AFR$latitude == "28.459" | AFR$longitude == "-82.1971"] <- "FL"


#state
table(AFR$state)
unique(AFR$state)

x<- filter(AFR, AFR$state == "null" | is.na(AFR$state) | AFR$state == "none" | AFR$state == "None")
#NOTA: gli stati sono 51 perché Washington D.C, appare come uno stato autonomo nella sigla "DC = district of columbia"
#WARNING: There are 51 states. District of Columbia (DC) is considered as the 51st state.

#ANALISI source
table(AFR$source)
unique(AFR$source)

#create numeric version of source

data <- as.data.frame(table(AFR$source))
colnames(data) <- c("source", "source2")
data$source2 <- seq(1:nrow(data))

AFR <- merge(AFR,data,
               by.x = "source",
               by.y = "source",
               all = TRUE)


#time 
table(AFR$time)
unique(AFR$time)
#Not interpretable


AFR <- AFR[,-c(2,6,7,11,12,14,19)] #these variables are removed as being derivable, single category or not interpretable

#Save table
write.table(AFR, file = "./Files/AFR_10K_Preprocessed.txt", append = FALSE, quote = TRUE, sep = " ", na = "NA", dec = ".", row.names = FALSE,col.names = TRUE, fileEncoding = "")

AFR <- read.csv("./Files/AFR_10K_Preprocessed.txt", sep="")

#create folder to save plots
dir.create("Files/Plots")

#source
table(AFR$source)
unique(AFR$source)

#source2 
summary(AFR$source2)
unique(AFR$source2)
table(AFR$source2)
range(AFR$source2)
min(AFR$source2)
max(AFR$source2)
mean(AFR$source2)
sd(AFR$source2)
var(AFR$source2)

#amenities 
summary(AFR$amenities)
unique(AFR$amenities)
table(AFR$amenities)
range(AFR$amenities)
min(AFR$amenities)
max(AFR$amenities)
mean(AFR$amenities)
sd(AFR$amenities)
var(AFR$amenities)

#amenities2
table(AFR$amenities2)
unique(AFR$amenities2)

ggplot(data = AFR) +
  geom_bar(mapping = aes(x = amenities2, fill = amenities2)) +
  labs(title = "Amenities",
       x = "Amenities",
       y = "Frequency") 

#Bathrooms 
summary(AFR$bathrooms)
unique(AFR$bathrooms)
table(AFR$bedrooms)
range(AFR$bathrooms)
min(AFR$bathrooms)
max(AFR$bathrooms)
mean(AFR$bathrooms)
sd(AFR$bathrooms)
var(AFR$bathrooms)

ggplot(data = AFR) +
  geom_bar(mapping = aes(x = bathrooms)) +
  labs(title = "Bathrooms",
       x = "Bathrooms",
       y = "Frequency") 


boxplot(AFR$bathrooms,xlab="Bathrooms",main = "Boxplot of Bathrooms") 
mtext(paste("Outliers: ", paste(unique(boxplot.stats(AFR$bathrooms)$out), collapse = ", ")))

boxplot.stats(AFR$bathrooms)$out
min(boxplot.stats(AFR$bathrooms)$out)
max(boxplot.stats(AFR$bathrooms)$out)

x <- filter(AFR, AFR$bathrooms >= min(boxplot.stats(AFR$bathrooms)$out))

hist(AFR$bathrooms, xlab = "Bathrooms",freq = TRUE, color = AFR$bathrooms, main = "Histogram of Bathrooms")


#Bedrooms
summary(AFR$bedrooms)
unique(AFR$bedrooms)
table(AFR$bedrooms)
range(AFR$bedrooms)
min(AFR$bedrooms)
max(AFR$bedrooms)
mean(AFR$bedrooms)
sd(AFR$bedrooms)
var(AFR$bedrooms)

ggplot(data = AFR) +
  geom_bar(mapping = aes(x = bedrooms)) +
  labs(title = "Bedrooms",
       x = "Bedrooms",
       y = "Frequency")

boxplot(AFR$bedrooms,xlab="Bedrooms",main = "Boxplot of Bedrooms")
mtext(paste("Outliers: ", paste(unique(boxplot.stats(AFR$bedrooms)$out), collapse = ", ")))

hist(AFR$bedrooms,xlab = "Bedrooms",freq = TRUE,color = AFR$bedrooms, main = "Histogram of Bedrooms")

#Bedrooms vs Bathrooms
ggplot(data = AFR) +
  geom_point(mapping = aes(x = bathrooms, y = bedrooms),position = "jitter") + 
  labs(title = "Bathrooms vs Bedrooms",
       x = "N_bathrooms",
       y = "N_bedrooms")


#pets_allowed 
unique(AFR$pets_allowed)
table(AFR$pets_allowed)


ggplot(data = AFR, main = "Barplot of Pets Allowed") +
  geom_bar(mapping = aes(x = pets_allowed, fill= pets_allowed)) +
  labs(title = "Histogram of Pets allowed",
       x = "Pets allowance",
       y = "Frequency")

#Pets_allowed2 
summary(AFR$pets_allowed2)
unique(AFR$pets_allowed2)
table(AFR$pets_allowed2)
range(AFR$pets_allowed2)
min(AFR$pets_allowed2)
max(AFR$pets_allowed2)
mean(AFR$pets_allowed2)
sd(AFR$pets_allowed2)
var(AFR$pets_allowed2)


#price
summary(AFR$price)
range(AFR$price)
min(AFR$price)
max(AFR$price)
mean(AFR$price)
sd(AFR$price)
var(AFR$price)



boxplot(AFR$price,xlab="Price", main = "Boxplot of Price")
mtext(paste("Min Outlier: ", min(boxplot.stats(AFR$price)$out)," -  Max Outlier: ",max(boxplot.stats(AFR$price)$out), collapse = ", "))
boxplot.stats(AFR$price)$out #outliers detection according to IQR (interquartile range Methodology)
min(boxplot.stats(AFR$price)$out)
max(boxplot.stats(AFR$price)$out)
x <- filter(AFR, AFR$price >= min(boxplot.stats(AFR$price)$out))
AFR$price[ which(AFR$price >= min(boxplot.stats(AFR$price)$out)) ] <- NA
AFR <- AFR[!is.na(AFR$price),] 

ggplot(AFR, aes(x=price)) + 
  geom_density(fill="grey", color="red", alpha=0.9, bw=0.5)

quantile(AFR$price)

#price_category 

ggplot(data = AFR) +
  geom_bar(mapping = aes(x = price_category, fill = price_category)) +
  labs(title = "Histogram of Price Categories",
       x = "Price Category",
       y = "Frequency")

#square_feet 
summary(AFR$square_feet)
range(AFR$square_feet)
min(AFR$square_feet)
max(AFR$square_feet)
mean(AFR$square_feet)
sd(AFR$square_feet)
var(AFR$square_feet)

boxplot(AFR$square_feet, xlab = "Square_feet")

boxplot.stats(AFR$square_feet)$out #outliers detection using IQR (interquartile range Methodology)
min(boxplot.stats(AFR$square_feet)$out)
max(boxplot.stats(AFR$square_feet)$out)

up <-  min(boxplot.stats(AFR$square_feet)$out) 

boxplot(AFR$square_feet,xlab="Square Feet",main = "Box Plot of Square Feet 2")
mtext(paste("Lower Range: ",up, collapse = ", "))

x <- filter(AFR, AFR$square_feet >= up)
 ggplot(data = x, aes(x= price, y= square_feet)) +
   geom_point() 

ggplot(AFR, aes(x=square_feet)) + 
  geom_density(fill="grey", color="blue", alpha=0.9, bw=0.5) 

#latitude 
summary(AFR$latitude)
range(AFR$latitude)

x <- filter(AFR, AFR$latitude <= -90.00 | AFR$latitude >= 90.00) 

#longitude 
summary(AFR$longitude)
range(AFR$longitude)

x <- filter(AFR, AFR$longitude<= -180.00 | AFR$longitude>= 180.00) 

##final descriptive statistics
# library(summarytools)
# view(dfSummary(AFR[,-c(9,10)]))
# view(freq(AFR[,-c(9,10)]))
# view(descr(AFR[,-c(9,10)]))


#Save table 
write.table(AFR, file = "./Files/AFR_10K_for_Visualization.txt", append = FALSE, quote = TRUE, sep = " ", na = "NA", dec = ".", row.names = FALSE,col.names = TRUE, fileEncoding = "")

AFR <- read.csv("./Files/AFR_10K_for_Visualization.txt", sep="")

dim(AFR)
str(AFR)


############################## Data Visualization  ##################################
#####################################################################################
library(dplyr) 
#library(ggmap)
#library(maps)
#library(mapproj)
#library(mapdata)

s <-  map_data("state")
s <- s[,-c(4,6)]



ggplot(s, aes(x = long, 
              y = lat, 
              group = group, 
              fill = region)) +
  geom_polygon(color = 'black') +
  coord_map("polyconic") +
  guides(fill= F) 

usa <- read.csv("./Files/USA_Data.txt", sep="")

usa <- usa %>% 
  filter(country == 'United States')%>%
  group_by(province) %>%
  mutate(count = n()) %>%
  select(province,country)

unique(usa$province)

usa$province <- tolower(usa$province)

data <- merge(s, usa,
              by.x = 'region',
              by.y = 'province',
              all = TRUE)
             
data <- data[,-5]


ggplot(data, aes(x = long, 
                 y = lat,
                 group = group,
                 fill = region)) +
  geom_polygon(color = 'black')


state_info <- cbind(state.abb,state.name)
state_info <- as.data.frame(state_info)
state_info$state.name <- tolower(state_info$state.name)


mean_price <- AFR %>%
  group_by(state) %>%
  summarise(mean_price = mean(price)) %>%
  arrange(desc(mean_price))
  


mean_square <-  AFR %>%
  group_by(state) %>%
  summarise(mean_square = mean(square_feet)) %>%
  arrange(desc(mean_square))


means <- merge(mean_price,mean_square,
      by.x = "state",
      by.y = "state",
      all = TRUE)


means <- merge(means,state_info,
               by.x = "state",
               by.y = "state.abb",
               all = TRUE)

means$state.name[ which(means$state == "DC") ] <- "district of columbia"


data <- merge(data,means,
               by.x = "region",
               by.y = "state.name",
               all=TRUE)


ggplot(data, aes(x = long, y = lat, 
                 group = group,
                 fill = mean_price)) +
      geom_polygon(color = 'black') + 
      coord_map("polyconic") +
      scale_fill_gradient2(low = "white", high = "red") +
      theme_void() +
      ggtitle("Mean price per state") 

barplot(means$mean_price ~ means$state, 
              main = "Mean Price per state", 
              xlab= "State",
              ylab= "Mean Price")

ggplot(data, aes(x = long, y = lat, 
                 group = group,
                 fill = mean_square)) +
  geom_polygon(color = 'black') + 
  coord_map("polyconic") +
  scale_fill_gradient2(low = "white", high = "blue") +
  theme_void() +
  ggtitle("Mean square feet per state")

barplot(means$mean_square ~ means$state, 
        main = "Mean square feet per state", 
        xlab= "State",
        ylab= "Mean Square ft") 

ggplot(data = AFR) +
  geom_point(mapping = aes(x = price, y = square_feet, color = state),position = "jitter") +
  labs(title = "Price vs square feet (and State)",
       x = "Price",
       y = "Square Feet") 


ggplot(data = AFR) +
  geom_point(mapping = aes(x = bathrooms, y = square_feet, color = bathrooms),position = "jitter") +
  labs(title = "Bathrooms vs square feet (and bathrooms)",
       x = "Bathrooms",
       y = "Square Feet") 

ggplot(data = AFR) +
  geom_point(mapping = aes(x = bedrooms, y = square_feet, color = bedrooms),position = "jitter") +
  labs(title = "Bedrooms vs square feet (and bedrooms)",
       x = "Bedrooms",
       y = "Square Feet") 

ggplot(data = AFR) +
  geom_point(mapping = aes(x = bedrooms, y = bathrooms),position = "jitter") +
  labs(title = "Bedrooms vs Bathrooms 2",
       x = "Bedrooms",
       y = "Bathrooms") 

ggplot(data = AFR) +
  geom_bar(mapping = aes(x = has_photo, fill = has_photo )) +
  labs(title = "Has_photo (Histogram)",
       x = "Bedrooms",
       y = "Frequency") +
  coord_flip()

ggplot(data = AFR, main = "Barplot of Pets Allowed") +
  geom_bar(mapping = aes(x = pets_allowed, fill= pets_allowed)) +
  labs(title = "Histogram of Pets allowed",
       x = "Pets allowance",
       y = "Frequency")
  

ggplot(data = AFR) +
  geom_bar(mapping = aes(x = state, fill = state)) + 
  coord_flip() +
  labs(title = "Amount of apartment per State",
       x = "State",
       y = "Frequency")

ggplot(data = AFR) +
  geom_bar(mapping = aes(x = source, fill = source)) +
  coord_flip() +
  labs(title = "Amount of apartment per Source",
       x = "Source",
       y = "Frequency")

Bedrooms <- as.factor(AFR$bedrooms)
Bathrooms <- as.factor(AFR$bathrooms)

ggplot(data = AFR) +
  geom_bar(mapping = aes(x = bathrooms, fill = Bedrooms)) +
  labs(title = "N_Bathrooms (Histogram)",
       x = "N_Bathrooms",
       y = "Frequency")

ggplot(data = AFR) +
  geom_bar(mapping = aes(x = bedrooms, fill = Bathrooms)) +
  labs(title = "N_Bedrooms (Histogram)",
       x = "N_Bedrooms",
       y = "Frequency")

AFR <- AFR[,-c(9,10)]# drop cityname and state as being derivable


#Save table
write.table(AFR, file = "./Files/AFR_10K_for_models.txt", append = FALSE, quote = TRUE, sep = " ", na = "NA", dec = ".", row.names = FALSE,col.names = TRUE, fileEncoding = "")

AFR <- read.csv("./Files/AFR_10K_for_models.txt", sep="")


################ Correlation and Contingency Tables #####################
######################################################################

dim(AFR)
str(AFR)

##Correlation
library(corrplot)
COR <- AFR[,-c(1,5,6,11,14)]
COR_stat <- cor(COR)
head(round(COR_stat,2))
corrplot(corr = COR_stat, 
         method = "number",
         tl.col = "red", 
         tl.cex = 1.5,
         number.cex=1, 
         order = "AOE")


#Contingency Tables
library(vcd)
CON <- AFR[,c(1,5,6,11,14)]

table(CON$has_photo,CON$source)
table(CON$amenities2,CON$pets_allowed)

mosaic(~ amenities2 + pets_allowed , 
       data = CON,
       main = "Amenities and Pets", 
       shade = TRUE, 
       legend = TRUE)

mosaic(~ source + has_photo , 
       data = CON,
       main = "Pictures by Websites", 
       shade = TRUE, 
       legend = TRUE)

mosaic(~ price_category + amenities2 , 
       data = CON,
       main = "Price category according to Amenities", 
       shade = TRUE, 
       legend = TRUE)



############################  Data Mining ############################
######################################################################

AFR <- read.csv("./Files/AFR_10K_for_models.txt", sep="")

# In some situations, the data generating mechanism can create predictors that only have a single unique value (i.e. a "zero-variance predictor"). For many models (excluding tree-based models), this may cause the model to crash or the fit to be unstable. Therefore such values are excluded.

table(AFR$source)
AFR$source[ which(AFR$source == "Home Rentals") ] <- NA
AFR$source[ which(AFR$source == "Real Estate Agent") ] <- NA
AFR$source[ which(AFR$source == "RENTCafé") ] <- NA
AFR$source[ which(AFR$source == "tenantcloud") ] <- NA
AFR$source[ which(AFR$source == "rentbits") ] <- NA
AFR <- AFR[!is.na(AFR$source),] 
dim(AFR)
AFR$source[ which(AFR$source == "RentDigs.com") ] <- "RentDigs"

write.table(AFR, file = "./Files/AFR_10K_for_Data_Mining.txt", append = FALSE, quote = TRUE, sep = " ", na = "NA", dec = ".", row.names = FALSE,col.names = TRUE, fileEncoding = "")

############################################# REGRESSION ##############################

AFR <- read.csv("./Files/AFR_10K_for_Data_Mining.txt", sep="")

AFR[,c(2,3,4,7,8,9,10,12,13,15)] <- sapply(AFR[c(2,3,4,7,8,9,10,12,13,15)],as.numeric)
AFR[,c(1,5,6,11,14)] <- lapply(AFR[,c(1,5,6,11,14)], factor)

#use only numerical variables for Regression
AFR <- AFR[,-c(1,5,6,11,14)]

#create variable "Services" as mean of bathrooms and bedrooms because they have high correlation
cor(AFR$bathrooms,AFR$bedrooms)

Services <- as.data.frame(rowMeans(AFR[,c('bathrooms', 'bedrooms')], na.rm = TRUE))
colnames(Services)<- "Services"

AFR <- cbind(AFR,Services)

AFR <- AFR[,-c(2,3)]

#Data Splitting

set.seed(54321) 

library(caret)
inTrain <- createDataPartition(y=AFR$price, p=0.75, list = FALSE)
training <- AFR[inTrain,]
testing <- AFR[-inTrain,]
dim(training)

fitControl <- trainControl(method = 'cv',
                           number = 5, 
                           verboseIter = F)


                                  #  Linear Regression (LR)
#setup trainControl
LR <- train(price ~ ., 
             data = training, 
             method = "lm", 
             trControl= fitControl,
             preProcess = c("center", "scale"))

LR
summary(LR)
LR$finalModel
LR$results
LR$bestTune

#Variables Importance
VarImp_LR <- varImp(LR, scale = TRUE)
VarImp_LR
plot(VarImp_LR, main = "Variables' Importance in Linear Regression")

#prediction
prediction_LR <- predict(LR,testing)


Performance_LR <- postResample(pred = prediction_LR, obs = testing$price)#compare predicted values (pred) with observed (real) ones (obs) and returns performances' measures  for regression

# Regression performance Measure
Performance_LR <- as.data.frame(Performance_LR)
Regression_Performance_Measure <- c("RMSE","Rsquared","MAE")
Performance_LR <- cbind(Regression_Performance_Measure,Performance_LR)
Performance_LR


                                #Neural Network (NN_r) 
library(nnet)

#setup trainControl
NN_r <- train(price ~ ., 
             data = training, 
             method = "nnet", 
             trControl= fitControl,
             preProcess = c("center", "scale"))

NN_r
summary(NN_r)
NN_r$finalModel
NN_r$results
NN_r$bestTune

#Variables' Importance
VarImp_NN_r <- varImp(NN_r, scale = TRUE)
VarImp_NN_r

plot(VarImp_NN_r, main = "Variables' Importance in Neural Network (Regression)")

#prediction
prediction_NN_r <- predict(NN_r,testing)

# Regression performance Measures
Performance_NN_r <- postResample(pred = prediction_NN_r, obs = testing$price)
Performance_NN_r <- as.data.frame(Performance_NN_r)
Performance_NN_r <- cbind(Regression_Performance_Measure,Performance_NN_r)
Performance_NN_r



                      #Support Vector Machines with Linear Kernel (SVML_r) 
library(kernlab)

#setup trainControl
SVML_r <- train(price ~ ., 
              data = training, 
              method = "svmLinear", 
              trControl= fitControl,
              preProcess = c("center", "scale"))

SVML_r
summary(SVML_r)
SVML_r$finalModel
SVML_r$results
SVML_r$bestTune

#Variables' Importance
VarImp_SVML_r <- varImp(SVML_r, scale = TRUE)
VarImp_SVML_r

plot(VarImp_SVML_r, main = "Variables' Importance in SVM Linear Kernel (Regression)")

#prediction
prediction_SVML_r <- predict(SVML_r,testing)

# Regression performance Measures
Performance_SVML_r <- postResample(pred = prediction_SVML_r, obs = testing$price)
Performance_SVML_r <- as.data.frame(Performance_SVML_r)
Performance_SVML_r <- cbind(Regression_Performance_Measure,Performance_SVML_r)
Performance_SVML_r


                          #Support Vector Machines with Polynomial Kernel (SVMP_r) 

#setup trainControl
SVMP_r <- train(price ~ ., 
                data = training, 
                method = "svmPoly", 
                trControl= fitControl,
                preProcess = c("center", "scale"))

SVMP_r
summary(SVMP_r)
SVMP_r$finalModel
SVMP_r$results
SVMP_r$bestTune

#Variables' Importance
VarImp_SVMP_r <- varImp(SVMP_r, scale = TRUE)
VarImp_SVMP_r

plot(VarImp_SVMP_r, main = "Variables' Importance in SVM Polynomial Kernel (Regression)")

#prediction
prediction_SVMP_r <- predict(SVMP_r,testing)

# Regression performance Measures
Performance_SVMP_r <- postResample(pred = prediction_SVMP_r, obs = testing$price)
Performance_SVMP_r <- as.data.frame(Performance_SVMP_r)
Performance_SVMP_r <- cbind(Regression_Performance_Measure,Performance_SVMP_r)
Performance_SVMP_r

                  #Support Vector Machines with Radial Basis Function Kernel (SVMR_r) 

#setup trainControl
SVMR_r <- train(price ~ ., 
                data = training, 
                method = "svmRadial", 
                trControl= fitControl,
                preProcess = c("center", "scale"))

SVMR_r
summary(SVMR_r)
SVMR_r$finalModel
SVMR_r$results
SVMR_r$bestTune

#Variables' Importance
VarImp_SVMR_r <- varImp(SVMR_r, scale = TRUE)
VarImp_SVMR_r

plot(VarImp_SVMR_r, main = "Variables' Importance in SVM Radial Kernel (Regression)")

#prediction
prediction_SVMR_r <- predict(SVMR_r,testing)

# Regression performance Measures
Performance_SVMR_r <- postResample(pred = prediction_SVMR_r, obs = testing$price)
Performance_SVMR_r <- as.data.frame(Performance_SVMR_r)
Performance_SVMR_r <- cbind(Regression_Performance_Measure,Performance_SVMR_r)
Performance_SVMR_r

                                        #K-Nearest Neighbors (KNN_r) 

#setup trainControl
KNN_r <- train(price ~ ., 
               data = training, 
               method = "knn", 
               trControl= fitControl,
               preProcess = c("center", "scale"))

KNN_r
summary(KNN_r)
KNN_r$finalModel
KNN_r$results
KNN_r$bestTune

#Variables' Importance
VarImp_KNN_r <- varImp(KNN_r, scale = TRUE)
VarImp_KNN_r

plot(VarImp_KNN_r, main = "Variables' Importance in k-Nearest Neighbors (Regression)")

#prediction
prediction_KNN_r <- predict(KNN_r,testing)

# Regression performance Measures
Performance_KNN_r <- postResample(pred = prediction_KNN_r, obs = testing$price)
Performance_KNN_r <- as.data.frame(Performance_KNN_r)
Performance_KNN_r <- cbind(Regression_Performance_Measure,Performance_KNN_r)
Performance_KNN_r


                          #Decision Tree (DT_r) 

#setup trainControl
DT_r <- train(price ~ ., 
               data = training, 
               method = "rpart", 
               trControl= fitControl,
               preProcess = c("center", "scale"))

DT_r
summary(DT_r)
DT_r$finalModel
DT_r$results
DT_r$bestTune

#Variables' Importance
VarImp_DT_r <- varImp(DT_r, scale = TRUE)
VarImp_DT_r

plot(VarImp_DT_r, main = "Variables' Importance in Decision Tree (Regression)")

#prediction
prediction_DT_r <- predict(DT_r,testing)

# Regression performance Measures
Performance_DT_r <- postResample(pred = prediction_DT_r, obs = testing$price)
Performance_DT_r <- as.data.frame(Performance_DT_r)
Performance_DT_r <- cbind(Regression_Performance_Measure,Performance_DT_r)
Performance_DT_r





Regression_Performances_summary <- cbind(Regression_Performance_Measure,
                                         Performance_LR$Performance_LR,
                                         Performance_NN_r$Performance_NN_r,
                                         Performance_SVML_r$Performance_SVML_r,
                                         Performance_SVMP_r$Performance_SVMP_r,
                                         Performance_SVMR_r$Performance_SVMR_r,
                                         Performance_KNN_r$Performance_KNN_r,
                                         Performance_DT_r$Performance_DT_r)

Regression_Performances_summary <- as.data.frame(Regression_Performances_summary)
Regression_Performances_summary

colnames(Regression_Performances_summary)<- c("Regression_Performance_Measure","LR","NN","SVML","SVMP","SVMR","KNN","DT")

Regression_Performances_summary[,c(2,3,4,5,6,7,8)] <- sapply(Regression_Performances_summary[c(2,3,4,5,6,7,8)],as.numeric)

Regression_Performances_summary

#### Rsquared comparison for Regression Models

Models <- colnames(Regression_Performances_summary[,c(2,3,4,5,6,7,8)])

Rsquared <- c(Regression_Performances_summary[2,2],
              Regression_Performances_summary[2,3],
              Regression_Performances_summary[2,4],
              Regression_Performances_summary[2,5],
              Regression_Performances_summary[2,6],
              Regression_Performances_summary[2,7],
              Regression_Performances_summary[2,8])



data <- as.data.frame(cbind(Models,Rsquared))
data$Rsquared <- as.numeric(data$Rsquared)



ggplot(aes(x = Models, y = Rsquared), data = data) + 
  geom_bar (colour="black", stat = "identity", fill = c("red","blue","brown","pink","green","white")) + 
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) + 
  ggtitle ("Comparative R2") +
  xlab ("Models") +
  ylab ("Rsquared Value")  +
 geom_text(aes(label=paste(round(Rsquared*100,1),"%")),
                position=position_dodge(1),
                vjust=-0.5)


############################################# CLASSIFICATION ########################
####################################################################################

AFR <- read.csv("./Files/AFR_10K_for_Data_Mining.txt", sep="")
library(caret)

AFR[,c(2,3,4,7,8,9,10,12,13,15)] <- sapply(AFR[c(2,3,4,7,8,9,10,12,13,15)],as.numeric)
AFR[,c(1,5,6,11,14)] <- lapply(AFR[,c(1,5,6,11,14)], factor)

#remove correlated and derivable variables for Classification (numerical variables are preferred)
AFR <- AFR[,c(1,3,4,5,6,8,9,10,11,14)] 

#create variable "Services" as mean of bathrooms and bedrooms because they have high correlation
cor(AFR$bathrooms,AFR$bedrooms)

Services <- as.data.frame(rowMeans(AFR[,c('bathrooms', 'bedrooms')], na.rm = TRUE))
colnames(Services)<- "Services"

AFR <- cbind(AFR,Services)

AFR <- AFR[,-c(2,3)]


#Data Splitting
set.seed(54321)  

inTrain <- createDataPartition(y = AFR$price_category, p = 0.75, list = FALSE)
training <- AFR[inTrain,]
testing <- AFR[-inTrain,]
dim(training)

#setup trainControl
fitControl <- trainControl(method = 'cv', 
                           number = 5, 
                           verboseIter = FALSE,
                           classProbs =  TRUE)

                                          #Naive Bayes (NB)
library(naivebayes)

grid <-  expand.grid( laplace = c(0,0.5,1.0),
                      usekernel = TRUE,
                      adjust = c(0,0.5,1.0))



#setup trainControl
NB <- train(price_category ~ ., 
            data = training, 
            method = "naive_bayes",
            trControl= fitControl,
            tuneGrid = grid,
            preProcess = c("center", "scale"),
            metric = "Accuracy")
NB
summary(NB)
NB$finalModel
NB$results
NB$bestTune
plot(NB, main = "Naive Bayes accuracy (Cross Validation)")

#Variables' Importance
VarImp_NB <- varImp(NB, scale = TRUE) 
VarImp_NB

plot(VarImp_NB, main = "Variables' Importance in Naive Bayes")

#prediction
prediction_NB  <- predict(NB,testing)
prediction_NB
plot(prediction_NB, col = c("red","blue","green"), main = "Prediction with Naive Bayes", ylab = "frequency", xlab = "Price category")

#Confusion matrix
confusion_matrix_NB <- confusionMatrix(prediction_NB,testing$price_category)
confusion_matrix_NB


#Classification performance Measures                                      
Performance_NB <- postResample(pred = prediction_NB, obs = testing$price_category)
Performance_NB
Performance_NB <- as.data.frame(Performance_NB)
Classification_Performance_Measure <- c("Accuracy","Kappa")
Performance_NB <- cbind(Classification_Performance_Measure,Performance_NB)
Performance_NB


                 #Support Vector Machines with Linear Kernel (SVMLK)
grid <-  expand.grid(C = c(1:5))

library(kernlab)
SVMLK <- train(price_category ~ ., 
             data = training, 
             method = "svmLinear",
             trControl= fitControl,
             tuneGrid = grid,
             preProcess = c("center", "scale"),
             metric = "Accuracy") #It takes about 2 minutes to run

SVMLK
summary(SVMLK)
SVMLK$finalModel
SVMLK$results
SVMLK$bestTune
plot(SVMLK, main = "SVM Linear Kernel accuracy (Cross Validation)")

#Variables Importance
VarImp_SVMLK <- varImp(SVMLK, scale = TRUE)
VarImp_SVMLK

plot(VarImp_SVMLK, main = "Variables' Importance in SVM with Linear Kernel")

#prediction
prediction_SVMLK <- predict(SVMLK,testing)
prediction_SVMLK
plot(prediction_SVMLK, col = c("red","blue","green"), main="Prediction with SVM Linear", ylab="frequency", xlab="Price category")

#Confusion matrix
confusion_matrix_SVMLK <- confusionMatrix(prediction_SVMLK,testing$price_category)
confusion_matrix_SVMLK

# Classification performance Measure
Performance_SVMLK <- postResample(pred = prediction_SVMLK, obs = testing$price_category)
Performance_SVMLK
Performance_SVMLK <- as.data.frame(Performance_SVMLK)
Performance_SVMLK <- cbind(Classification_Performance_Measure,Performance_SVMLK)
Performance_SVMLK


              #Support Vector Machines with Radial Basis Function Kernel (SVMRK)

grid <-  expand.grid(C = c(1:5))

#setup trainControl
SVMRK <- train(price_category ~ ., 
               data = training, 
               method = "svmRadialCost",
               trControl= fitControl,
               tuneGrid = grid,
               preProcess = c("center", "scale"),
               metric = "Accuracy") ## It takes about 5 minutes to run
                                    

SVMRK
summary(SVMRK)
SVMRK$finalModel
SVMRK$results
SVMRK$bestTune
plot(SVMRK, main = "SVM Radial Kernel accuracy (Cross Validation)")

#Variables Importance
VarImp_SVMRK <- varImp(SVMRK, scale = TRUE)
VarImp_SVMRK

plot(VarImp_SVMRK, main = "Variables' Importance in SVM with Radial Kernel")

#prediction
prediction_SVMRK <- predict(SVMRK,testing)
prediction_SVMRK
plot(prediction_SVMRK, col = c("red","blue","green"), main="Prediction with SVM Radial", ylab="frequency", xlab="Price category")

#Confusion matrix
confusion_matrix_SVMRK <- confusionMatrix(prediction_SVMRK,testing$price_category)
confusion_matrix_SVMRK

# Classification performance Measure
Performance_SVMRK <- postResample(pred = prediction_SVMRK, obs = testing$price_category)
Performance_SVMRK
Performance_SVMRK <- as.data.frame(Performance_SVMRK)
Performance_SVMRK <- cbind(Classification_Performance_Measure,Performance_SVMRK)
Performance_SVMRK


                  #Support Vector Machines with Polynomial Kernel (SVMPL) 

grid <-  expand.grid( C = c(1,2,3),
                      degree = c(1,2,3),
                      scale = c(0.01,0.1))

#setup trainControl
SVMPL <- train(price_category ~ ., 
               data = training, 
               method = "svmPoly",
               trControl= fitControl,
               tuneGrid = grid,
               preProcess = c("center", "scale"),
               metric = "Accuracy")# it takes about 13 minutes to run
                                  

SVMPL
summary(SVMPL)
SVMPL$finalModel
SVMPL$results
SVMPL$bestTune
plot(SVMPL, main = "SVM Polynomial Kernel accuracy (Cross Validation)")

#Variables' Importance
VarImp_SVMPL <- varImp(SVMPL, scale=TRUE)
VarImp_SVMPL

plot(VarImp_SVMPL, main = "Variables' Importance in SVM with Polynomial Kernel")

#prediction
prediction_SVMPL <- predict(SVMPL,testing)
prediction_SVMPL
plot(prediction_SVMPL, col = c("red","blue","green"), main="Prediction with SVM Polynomial", ylab="frequency", xlab="Price category")

#Confusion matrix
confusion_matrix_SVMPL <- confusionMatrix(prediction_SVMPL,testing$price_category)
confusion_matrix_SVMPL
  
#Classification performance Measures                                      
Performance_SVMPL <- postResample(pred = prediction_SVMPL, obs = testing$price_category)
Performance_SVMPL
Performance_SVMPL <- as.data.frame(Performance_SVMPL)
Performance_SVMPL <- cbind(Classification_Performance_Measure,Performance_SVMPL)
Performance_SVMPL



                                    #Neural Network (NN)
library(nnet)
grid <-  expand.grid(size = seq(from = 1, to = 5, by = 1),
           decay = seq(from = 0.0, to = 0.5, by = 0.1))

NN <- train(price_category ~ ., 
            data = training, 
            method = "nnet",
            trControl= fitControl,
            tuneGrid = grid,
            preProcess = c("center", "scale"),
            metric = "Accuracy") # It takes about 3 minutes to run
                                 

NN
summary(NN)
NN$finalModel
NN$results
NN$bestTune
plot(NN,main = "Neural Network accuracy (Cross Validation)")

#prediction
prediction_NN  <- predict(NN,testing)
prediction_NN 
plot(prediction_NN, col = c("red","blue","green"), main="Prediction with Neural Network", ylab="frequency", xlab="Price category")

#Confusion matrix
confusion_matrix_NN <- confusionMatrix(prediction_NN,testing$price_category)
confusion_matrix_NN

#Classification performance Measures
Performance_NN <- postResample(pred = prediction_NN, obs = testing$price_category)
Performance_NN
Performance_NN <- as.data.frame(Performance_NN)
Performance_NN <- cbind(Classification_Performance_Measure,Performance_NN)
Performance_NN




                                    #Decision Tree (DT)
library(rpart)


grid <-  expand.grid(cp = seq(from = 0.01, to = 0.05, by = 0.01))

DT <- train(price_category ~ ., 
            data = training, 
            method = "rpart",
            trControl= fitControl,
            tuneGrid = grid,
            preProcess = c("center", "scale"),
            metric = "Accuracy")

DT
summary(DT)
DT$finalModel
DT$results
DT$bestTune
plot(DT, main = "Decision Tree accuracy (Cross Validation)")

#Variables' Importance
VarImp_DT <- varImp(DT, scale=TRUE)
VarImp_DT

plot(VarImp_DT, main = "Variables' Importance in Decision Tree")

#prediction
prediction_DT  <- predict(DT,testing)
prediction_DT
plot(prediction_DT, col = c("red","blue","green"), main="Prediction with Decision Tree", ylab="frequency", xlab = "Price category")

#Confusion matrix
confusion_matrix_DT <- confusionMatrix(prediction_DT,testing$price_category)
confusion_matrix_DT

#Classification performance Measures
Performance_DT <- postResample(pred = prediction_DT, obs = testing$price_category)
Performance_DT
Performance_DT <- as.data.frame(Performance_DT) 
Performance_DT <- cbind(Classification_Performance_Measure,Performance_DT)
Performance_DT


                                 
                                        # K- Nearest Neighbors (KNN)

grid <-  expand.grid(k = c(1:10))

KNN <- train(price_category ~ ., 
            data = training, 
            method = "knn",
            trControl= fitControl,
            tuneGrid = grid,
            preProcess = c("center", "scale"),
            metric = "Accuracy")

KNN
summary(KNN)
KNN$finalModel
KNN$results
KNN$bestTune
plot(KNN, main = "K- Nearest Neighbors accuracy (Cross Validation)")

#Variables' Importance
VarImp_KNN<- varImp(KNN, scale=TRUE)
VarImp_KNN

plot(VarImp_KNN, main = "Variables' Importance in K- Nearest Neighbors")

#prediction
prediction_KNN  <- predict(KNN,testing)
prediction_KNN
plot(prediction_KNN, col = c("red","blue","green"), main="Prediction with K- Nearest Neighbors", ylab="frequency", xlab = "Price category")

#Confusion matrix
confusion_matrix_KNN <- confusionMatrix(prediction_KNN,testing$price_category)
confusion_matrix_KNN

#Classification performance Measures
Performance_KNN <- postResample(pred = prediction_KNN, obs = testing$price_category)
Performance_KNN
Performance_KNN <- as.data.frame(Performance_KNN) 
Performance_KNN <- cbind(Classification_Performance_Measure,Performance_KNN)
Performance_KNN


Classification_Performances_summary <- cbind(Classification_Performance_Measure,
                                         Performance_SVMLK$Performance_SVMLK,
                                         Performance_SVMRK$Performance_SVMRK,
                                         Performance_SVMPL$Performance_SVMPL,
                                         Performance_NB$Performance_NB,
                                         Performance_NN$Performance_NN,
                                         Performance_DT$Performance_DT,
                                         Performance_KNN$Performance_KNN)

Classification_Performances_summary <- as.data.frame(Classification_Performances_summary)

colnames(Classification_Performances_summary)<- c("Classification_Performance_Measure",
                                                  "SVM Linear Kernel",
                                                  "SVM Radial Kernel",
                                                  "SVM Polynomial Kernel",
                                                  "Naive Bayes",
                                                  "Neural Network",
                                                  "Decision Tree",
                                                  "K Nearest Neighbor")




Classification_Performances_summary[,c(2,3,4,5,6,7,8)] <- sapply(Classification_Performances_summary[c(2,3,4,5,6,7,8)],as.numeric)


#### Accuracy comparison for Classification Models
Models <- colnames(Classification_Performances_summary[,c(2:8)])
Accuracy <- c(Classification_Performances_summary[1,2:8])

data <- as.data.frame(cbind(Models,Accuracy))
data$Accuracy <- as.numeric(data$Accuracy)


ggplot(aes(x = as.character(Models), y = Accuracy), data = data) + 
  geom_bar (colour="black", stat = "identity", 
            fill=c("red", "pink", "blue","yellow","orange","gray","brown")) + 
  theme(axis.text.x = element_text(angle=45,hjust = 1)) + 
  ggtitle ("Comparative Accuracy") +
  xlab ("Models") +
  ylab ("Accuracy Value")  +
  geom_text(aes(label = paste(round(Accuracy*100,1),"%")),
            position=position_dodge(0.1),
            vjust=-0.5)



