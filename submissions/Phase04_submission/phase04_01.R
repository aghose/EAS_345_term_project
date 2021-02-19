#Author: Akash Ghose

library(dplyr)
library(readr)
library(ggplot2)

#load the cleaned data
FBI_arrest_by_race_total <- read_csv("cleaned_data/FBI_arrest_by_race_total.csv")
FBI_arrest_by_race_under18 <- read_csv("cleaned_data/FBI_arrest_by_race_under18.csv")
FBI_arrest_by_race_18_and_over <- read_csv("cleaned_data/FBI_arrest_by_race_18_and_over.csv")
mortgage_data <- read_csv("cleaned_data/mortgage_data.csv")




--------------------------------------------------------------------------------
  #FBI arrest data EDA
  
  #Initial look at the cleaned data sets
  View(FBI_arrest_by_race_18_and_over)
  View(FBI_arrest_by_race_under18)
  View(FBI_arrest_by_race_total)
  head(FBI_arrest_by_race_total)
  colnames(FBI_arrest_by_race_total)

  #Combine non-white races into a single column
  #Combine %non-whites into a single column
  #Drop first column because it is useless
  #relocate the new rows to a better position for readability
  alt_FBI_arrest_by_race_total <-FBI_arrest_by_race_total %>%
    mutate(Non_white = .[[5]]+.[[6]]+.[[7]]+.[[8]]) %>%
    mutate("% Non_white" = .[[11]]+.[[12]]+.[[13]]+.[[14]]) %>%
    select(-c(1)) %>%
    relocate(Non_white, .after = "White") %>%
    relocate("% Non_white", .after = "% White")
  alt_FBI_arrest_by_race_under18 <-FBI_arrest_by_race_under18 %>%
    mutate(Non_white = .[[5]]+.[[6]]+.[[7]]+.[[8]]) %>%
    mutate("% Non_white" = .[[11]]+.[[12]]+.[[13]]+.[[14]]) %>%
    select(-c(1)) %>%
    relocate(Non_white, .after = "White") %>%
    relocate("% Non_white", .after = "% White")
  alt_FBI_arrest_by_race_18_and_over <-FBI_arrest_by_race_18_and_over %>%
    mutate(Non_white = .[[5]]+.[[6]]+.[[7]]+.[[8]]) %>%
    mutate("% Non_white" = .[[11]]+.[[12]]+.[[13]]+.[[14]]) %>%
    select(-c(1)) %>%
    relocate(Non_white, .after = "White") %>%
    relocate("% Non_white", .after = "% White")
  
  "TODO:"
  #Plot histograms of total offences charged separated by White, non_white and black
  ggplot(alt_FBI_arrest_by_race_total, aes(x="White")) + geom_bar()
  
--------------------------------------------------------------------------------
  #Mortgage data EDA
  
  #Initial look at the cleaned data set
  View(mortgage_data)
  summary(mortgage_data)
  
  mortgage_data <- select(mortgage_data, -c(1)) 

  boxplot(mortgage_data$loan_amount_000s)
  
  ggplot(mortgage_data, aes(y=loan_amount_000s, x=applicant_race_name_1, 
                            color=applicant_race_name_1)) + geom_boxplot()
  
  ggplot(mortgage_data, aes(x=action_taken_name)) + geom_bar()
  
  barplot <- ggplot(mortgage_data, aes(x=action_taken, 
                                       color=applicant_race_name_1)) + geom_bar()
  barplot
    
