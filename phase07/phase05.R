#Author: Akash Ghose
#install.packages("caret")
#install.packages('e1071', dependencies=TRUE)
library(dplyr)
require(graphics)
require(grDevices)
require(class)
library(caret)

mortgage_data <- read.csv("cleaned_data/mortgage_data.csv")

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


#Some rudimentary EDA - Ignore these
with(mortgage_data_subset0, plot(loan_amount_000s, action_taken))
with(mortgage_data_subset0, plot(loan_amount_000s, applicant_income_000s))
with(mortgage_data_subset0, plot(applicant_income_000s, loan_amount_000s))

"LM models"

#LM - loan vs applicant_income and applicant_race
lm_model_00 <- lm(loan_amount_000s ~ applicant_income_000s+
                    applicant_race_1, 
                  data = mortgage_data_subset0)
lm_model_00
summary(lm_model_00)
plot(lm_model_00)

#Going to add more variables to perhaps make the lm_model more accurate as a predictor
#LM - loan vs income level, applicant_income, applicant_race and presence of co_applicant
lm_model_01 <- lm(loan_amount_000s ~ applicant_income_000s+
                    applicant_race_1+
                    co_applicant, 
                  data = mortgage_data_subset0)
lm_model_01
summary(lm_model_01)
plot(lm_model_01)
#ggplot(lm_model_01) + aes(col=applicant_race_1)

#LM - loan vs income level, applicant_income, applicant_race and co_applicant_race
lm_model_02 <- lm(loan_amount_000s ~ applicant_income_000s+
                    applicant_race_1+
                    co_applicant_race_1, 
                  data = mortgage_data_subset0)
lm_model_02
summary(lm_model_02)
plot(lm_model_02)

#Going to try using the LM algorithm to try to predict income instead
lm_model_03 <- lm(applicant_income_000s ~ loan_amount_000s+
                    applicant_race_1+
                    co_applicant, 
                  data = mortgage_data_subset0)
lm_model_03
summary(lm_model_03)
plot(lm_model_03)

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
  relocate(loan_approved, .after= last_col())

#Creating a function to normalize all my variables
normalize <- function(x) {
  return( (x - min(x))/(max(x) - min(x)))
}

#Normalizing my dataset
knn_data_set_norm <- as.data.frame(lapply(knn_data_set[, 1:8], normalize))

n <- 114953 #roughly 90% of my data
k <- 357 #roughly the sqrt of my total observations

knn_train <- knn_data_set_norm[1:n,]
knn_test <- knn_data_set_norm[(n+1):nrow(knn_data_set_norm),]

knn_train_target <- knn_data_set[1:n, 9]
knn_test_target <- knn_data_set[(n+1):nrow(knn_data_set), 9]

#Time for the actual model
#K = k
knn_model_00 <- knn(train = knn_train, test = knn_test, 
                    cl= knn_train_target, k= k)

#knn_model_00

tbl_00 <- table(knn_test_target, knn_model_00)
tbl_00

plot(knn_model_00)
plot(tbl_00)
cmtx <-confusionMatrix(tbl_00)
cmtx

#K = 5
knn_model_01 <- knn(train = knn_train, test = knn_test, 
                    cl= knn_train_target, k= 5)
tbl_01 <- table(knn_test_target, knn_model_01)
plot(knn_model_01)
tbl_01
confusionMatrix(tbl_01)

#KMeans - feed it multiple columns, i.e. loan_amount, race, income, and see what clusters it makes
"K-Means"
kmeans_dataset <- knn_data_set_norm
kmeans_model_00 <- kmeans(kmeans_dataset,5)
kmeans_model_00$size
kmeans_model_00$centers
kmeans_model_00$betweenss/kmeans_model_00$totss

#A list of the columnnames so I don't have to keep referring back to the data
clnnames <- c("loan_amount_000s", "preapproval", "co_applicant", 
              "applicant_sex", "co_applicant_sex", "applicant_income_000s",
              "applicant_white", "co_applicant_white","loan_approved")

kmeans_dataset <- knn_data_set
kmeans_model_01 <- kmeans(kmeans_dataset,5)
kmeans_model_01$size
kmeans_model_01$centers
kmeans_model_01$betweenss/kmeans_model_00$totss

kmeans_dataset <-knn_data_set %>% select(clnnames[c(1,2,3,6,7,9)])
kmeans_model_02 <- kmeans(kmeans_dataset,5)
kmeans_model_02$size
kmeans_model_02$centers
kmeans_model_02$betweenss/kmeans_model_00$totss   
