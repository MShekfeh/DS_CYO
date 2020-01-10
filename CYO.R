set.seed(1)

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos= "http://cran.us.r-project.org")
# clustering algorithms
if(!require(cluster)) install.packages("cluster", repos= "http://cran.us.r-project.org")   
if(!require(factoextra)) install.packages("factoextra", repos="http://cran.us.r-project.org")  # clustering algorithms & visualization
if(!require(matrixStats)) install.packages("matrixStats", repos="http://cran.us.r-project.org")
if(!require(recommenderlab)) install.packages("recommenderlab", repos ="http://cran.us.r-project.org")

download.file("https://github.com/MShekfeh/DS_CYO/raw/master/food_world_cup_data.csv", "food_world_cup_data.csv")
MyData <- read.csv(file="./food_world_cup_data.csv", header=TRUE, sep=",")

# Rename some columns because they have long names
colnames(MyData)[2] <- "Knowledge"
colnames(MyData)[3] <- "Interest"
colnames(MyData)[44] <- "Gender"
colnames(MyData)[45] <- "Age"
colnames(MyData)[46] <- "Income"
colnames(MyData)[47] <- "Education"
colnames(MyData)[48] <- "Region"
##################################
# preprocessing
###################################

colClean <- function(x){ colnames(x) <- gsub("Please.rate.how.much.you.like.the.traditional.cuisine.of.", "", colnames(x));  x } 
MyData <- colClean(MyData)
#colnames(MyData) <- str_replace(colnames(MyData),"\\.|\\S","")

# id is the set of numeric columns in the dataset 
#and they represent the countries 
id <- c(4:43)
MyData[,id] <- lapply(MyData[,id], function(x) factor(x))
MyData[,id] <- lapply(MyData[,id], function(x) x %>% na_if(" "))
num_col_with_0 = function(i){MyData[,i] <- MyData[,i] %>% replace_na(0)}
factor_cols = function(j){MyData[,j] <- factor(MyData[,j]); MyData[,j] <- MyData[,j] %>% replace_na("0"); MyData[,j]}
MyData[,id] <- sapply(id,factor_cols)
MyData[,id] <- lapply(MyData[,id], function(x) as.numeric(as.character(x)))
MyData[,id] <- sapply(id,num_col_with_0)

MyData <- MyData %>% mutate(Age = factor(Age), Income = factor(Income), Education = factor(Education), Region = factor(Region))
MyData$Interest <- str_replace(MyData$Interest,"\\ÃS","")
MyData <- MyData %>% mutate(Interest = factor(Interest))
levels(MyData$Age)[levels(MyData$Age) == ""] <- "unknown"
levels(MyData$Income)[levels(MyData$Income) == ""] <- "unknown"
levels(MyData$Education)[levels(MyData$Education) == ""] <- "unknown"
levels(MyData$Region)[levels(MyData$Region) == ""] <- "unknown"
levels(MyData$Gender)[levels(MyData$Gender) == ""] <- "unknown"

#######################################
#Essential Exploratory Data Analysis
#######################################

MyData %>% group_by(Gender)%>% summarize(count = n(), ratio = count/nrow(MyData))
MyData %>% group_by(Income)%>% summarize(count = n(), ratio = count/nrow(MyData))
MyData %>% group_by(Education)%>% summarize(count = n(), ratio = count/nrow(MyData))
MyData %>% group_by(Age)%>% summarize(count = n(), ratio = count/nrow(MyData))
MyData %>% group_by(Region)%>% summarize(count = n(), ratio = count/nrow(MyData))

clean_data <- MyData %>% filter(Education != "unknown" & Age != "unknown" & Income != "unknown" & Region != "unknown")
clean_data <- clean_data %>% mutate(Age = factor(Age), Income = factor(Income), Education = factor(Education), Region = factor(Region), Gender = factor(Gender))


#Visualizing the distance matrix between data points
# This may take one or two minutes
distance <- get_dist(MyData[,id])
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#Find the correlated variables, correlation between ratings of different cuisines
# reduced data, in case we want to include only the independent cuisines in the analysis
x<- as.matrix(MyData[,id])
cor_mat = cor(x)
hc = findCorrelation(cor_mat, cutoff=0.5) 
hc = sort(hc)
reduced_Data = x[,-c(hc)]
reduced_Data %>% colnames()

#############
#PCA
#############

x <- as.matrix(MyData[,id])
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           age = MyData$Age) %>%
  ggplot(aes(pc_1, pc_2, color = age)) +
  geom_point()

data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           income = MyData$Income) %>%
  ggplot(aes(pc_1, pc_2, color = income)) +
  geom_point()

data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           edu = MyData$Education) %>%
  ggplot(aes(pc_1, pc_2, color = edu)) +
  geom_point()

# Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
fviz_eig(pc)

# Unsupervised Clustering using kmeans algorithm
#5 clusters and 25 repetitions
knn <- kmeans(MyData[,id], centers = 5, nstart = 25)

table(knn$cluster, MyData$Income)

MyData %>%
  as_tibble() %>%
  mutate(cluster = knn$cluster,
         taste = rowWeightedMedians(x)) %>%
  ggplot(aes(taste, Age, color = factor(cluster), label = taste)) +
  geom_text()
MyData %>%
  as_tibble() %>%
  mutate(cluster = knn$cluster,
         taste = rowWeightedMedians(x)) %>%
  ggplot(aes(taste, Income, color = factor(cluster), label = taste)) +
  geom_text()
MyData %>%
  as_tibble() %>%
  mutate(cluster = knn$cluster,
         taste = rowWeightedMedians(x)) %>%
  ggplot(aes(taste, Education, color = factor(cluster), label = taste)) +
  geom_text()
MyData %>%
  as_tibble() %>%
  mutate(cluster = knn$cluster,
         taste = rowWeightedMedians(x)) %>%
  ggplot(aes(taste, Region, color = factor(cluster), label = taste)) +
  geom_text()

# Remove unwanted columns that will not help in prediction
Data <- subset(MyData, select = - c(RespondentID, Knowledge, Interest))

##############################################
#partitioning the dataset to get it ready for modeling
##############################################

set.seed(1991)
#### Create an 80-20 partitioning of the data#######
test_index <- createDataPartition(y = Data$Age, times = 1, p = 0.2, list = FALSE)
train_set <- Data[-test_index,]
test_set <- Data[test_index,]

train_knn <- train(Age ~ ., data = train_set,
                   method = "knn",
                   tuneGrid = data.frame(k = seq(5, 21, 2 )))

plot(train_knn)

y_hat_knn <- predict(train_knn, test_set)
confusionMatrix(data = y_hat_knn, reference = test_set$Age)$overall["Accuracy"]

#Train a RandomForest model to predict the Age
# This may take few minutes

train_rf <- train(Age ~ ., data = train_set,
                                        method = "rf",
                                      tuneGrid = data.frame(mtry = seq(50, 100, 10 )))
 plot(train_rf)
 predict(train_rf, test_set, type = "prob") %>% head()
 
 y_hat_rf <- predict(train_rf, test_set)
 confusionMatrix(data = y_hat_knn, reference = test_set$Age)$overall["Accuracy"]

 ##################################################
#user-based recommendation using Recommender Lab 
# predict user's cuisine preferences ased on their ratings in the dataset
# convert the ratings of users into a realRatingMatrix where rows are users
#and each column is a different cuisine
#The values in the cells are the different ratings of each user to the cuisine that 
#they like or not
 ################################################
 
set.seed(1991)
x <- Data[,1:40]
# Convert bad ratings (1 and 2) to -1
x[x<3 & x>0] <- -1
# Convert good ratings (3,4 and 5) into 1
x[x>=3] <- 1

x <- as.matrix(x)

ratings <- as(x, "realRatingMatrix")

#3 ratings of 20% of users are excluded for testing
# 80% of data will be used for training

e <- evaluationScheme(ratings, method="split", train=0.8, given=-3)

##Use the popular method for prediction
model_pop <- Recommender(getData(e, "train"), "POPULAR")

prediction_pop <- predict(model_pop, getData(e, "known"), type="ratings")

rmse_popular <- calcPredictionAccuracy(prediction_pop, getData(e, "unknown"))[1]
rmse_popular

##Use the UCBF method for prediction
model <- Recommender(getData(e, "train"), method = "UBCF", 
                         param=list(normalize = "center", method="Cosine", nn=50))
prediction <- predict(model, getData(e, "known"), type="ratings")

rmse_ubcf <- calcPredictionAccuracy(prediction, getData(e, "unknown"))[1]
rmse_ubcf