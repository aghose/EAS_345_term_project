library(shiny)
library(dplyr)
require(graphics)
require(grDevices)
require(class)
library(caret)

mortgage_data <- read.csv("mortgage_data.csv")

set.seed(3423)

#Simplifies the data so that I'm only looking at loans that were either accepted (1,2) or denied (3),
#Creates boolean columns for whether loans were approved and whether there was a co_applicant
mortgage_data_subset0 <- mortgage_data %>% 
  filter(action_taken %in% c(1,2,3)) %>%
  mutate(loan_approved = if_else(action_taken==3,
                                 true = FALSE,
                                 false = TRUE)) %>%
  mutate(co_applicant = if_else(co_applicant_race_1 != 8,
                                true = TRUE,
                                false= FALSE)) %>%
  relocate(co_applicant, .before= co_applicant_race_name_1) %>%
  relocate(loan_approved, .before= action_taken)

"KNN models"

#First, I will further clean the data to get a more workable data set
knn_data_set <- mortgage_data_subset0 %>%
  mutate(applicant_white = if_else(applicant_race_1==5,
                                   true = 0,
                                   false = 1)) %>%
  mutate(co_applicant_white = if_else(co_applicant_race_1==5,
                                      true = 0,
                                      false = 1)) %>%
  select(-(contains("name") | contains("race"))) %>%
  #Changing co-applicant from boolean to binary so KNN algorithm can
  #Calculate the Euclidean distance easier
  mutate(co_applicant = if_else(co_applicant==TRUE,
                                true = 0,
                                false = 1)) %>%
  #similar thing for pre-aproval
  mutate(preapproval = if_else(preapproval == 3,
                               true = 1,
                               false = 0)) %>%
  select(-(c(1, 5, denial_reason_1:number_of_1_to_4_family_units))) %>%
  relocate(loan_approved, .after= last_col()) #%>%
  #sample_n(1000)

#Creating a function to normalize all my variables
normalize <- function(x) {
  return( (x - min(x))/(max(x) - min(x)))
}

#Normalizing my dataset
knn_data_set_norm <- as.data.frame(lapply(knn_data_set[, 1:8], normalize))

n <- round(nrow(knn_data_set_norm) * 0.9) #roughly 90% of my data
k <- round(sqrt(nrow(knn_data_set_norm))) #roughly the sqrt of my total observations

knn_train <- knn_data_set_norm[1:n,]
knn_test <- knn_data_set_norm[(n+1):nrow(knn_data_set_norm),]

knn_train_target <- knn_data_set[1:n, 9]
knn_test_target <- knn_data_set[(n+1):nrow(knn_data_set), 9]

#Time for the actual model
#K = k
knn_model_357 <- knn(train = knn_train, test = knn_test, 
                    cl= knn_train_target, k= k)

knn_model_357

tbl_357 <- table(knn_test_target, knn_model_357)
tbl_357

knn_model_356 <- knn(train = knn_train, test = knn_test, 
                     cl= knn_train_target, k= 356)
tbl_356 <- table(knn_test_target, knn_model_356)

knn_model_358 <- knn(train = knn_train, test = knn_test, 
                     cl= knn_train_target, k= 358)
tbl_358 <- table(knn_test_target, knn_model_358)

knn_model_4 <- knn(train = knn_train, test = knn_test, 
                     cl= knn_train_target, k= 4)
tbl_4 <- table(knn_test_target, knn_model_4)

knn_model_5 <- knn(train = knn_train, test = knn_test, 
                   cl= knn_train_target, k= 5)
tbl_5 <- table(knn_test_target, knn_model_5)

knn_model_6 <- knn(train = knn_train, test = knn_test, 
                   cl= knn_train_target, k= 6)
tbl_6 <- table(knn_test_target, knn_model_6)

#KMeans - feed it multiple columns, i.e. loan_amount, race, income, and see what clusters it makes
"K-Means"
#A list of the columnnames so I don't have to keep referring back to the data
clnnames <- c("loan_amount_000s", "preapproval", "co_applicant", 
              "applicant_sex", "co_applicant_sex", "applicant_income_000s",
              "applicant_white", "co_applicant_white","loan_approved")


kmeans_dataset <-knn_data_set %>% select(clnnames[c(1,2,3,6,7,9)])

kmeans_model_03 <- kmeans(kmeans_dataset,3)
kmeans_model_04 <- kmeans(kmeans_dataset,4)
kmeans_model_05 <- kmeans(kmeans_dataset,5)
kmeans_model_06 <- kmeans(kmeans_dataset,6)   
kmeans_model_07 <- kmeans(kmeans_dataset,7)
kmeans_model_08 <- kmeans(kmeans_dataset,8)
kmeans_model_09 <- kmeans(kmeans_dataset,9)
