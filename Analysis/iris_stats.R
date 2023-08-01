#### packages ####
library(tidyverse)
# loads the tidyverse packages which contain
# a bunch of useful data manipulation and
# plotting functions

#### data ####
head(iris)
# displays the first 6 rows of the iris dataset

str(iris)
# provides information about the structure of
# the dataset

#### creating a functions for summary stats ####
summary_stats <- function(df){
  loop_df <- data.frame(mean = c(),
                        median = c(),
                        mode = c(),
                        sd = c())
  for (col in 1:length(df)) {
    if (class(df[[col]]) == "factor") {
      loop_table <- data.frame(table(df[[col]]))
      colnames(loop_table)[1] <- colnames(df)[col]
      colnames(loop_table)[2] <- "Count"
      print(loop_table)
    }
    if (class(df[[col]]) == "numeric") {
      mean_num <- mean(df[[col]], na.rm = TRUE)
      sd_num <- sd(df[[col]], na.rm = TRUE)
      median_num <- median(df[[col]], na.rm = TRUE)
      quart1_num <- quantile(df[[col]], probs = 0.25,
                             na.rm = TRUE)
      quart3_num <- quantile(df[[col]], probs = 0.75,
                             na.rm = TRUE)
      
      loop_row <- c(mean_num, sd_num, quart1_num, 
                    median_num, quart3_num)
      loop_df <- rbind(loop_df, loop_row)
      rownames(loop_df)[nrow(loop_df)] <- colnames(df)[col]
    }
  }
  colnames(loop_df)[1] <- "Mean"
  colnames(loop_df)[2] <- "Standard Deviation"
  colnames(loop_df)[3] <- "Median"
  colnames(loop_df)[4] <- "1st Quartile"
  colnames(loop_df)[5] <- "3rd Quartile"
  return(loop_df)
    
}
# creates a functions that provides summary counts
# and statistics for a given data frame

#### summary stats for iris data ####
summary_stats(iris)
# uses the summary_stats function on the iris data
