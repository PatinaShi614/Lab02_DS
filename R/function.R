library(tidyverse)
library(dplyr)
library(readr)
library(here)
library(ggplot2)
library(Hmisc)
library(janitor)
library(stringr)

# setwd("..")
# save(DRG_data, file = 'DataScience_Lab02/data/DRG_data.RData')
# Reading Data
DRG_Data_Question_1 <- DRG_data %>% clean_names()
# Modifying the Data for Better Plot for Function 1

Mod_DRG_Data <- DRG_Data_Question_1 %>% select(drg_definition,average_covered_charges,average_total_payments,average_medicare_payments) %>%
  mutate(code=str_sub(drg_definition,1,3)) %>%
  clean_names() %>%
  filter(average_covered_charges < 750000) %>%
  filter(average_total_payments < 125000) %>%
  filter(average_medicare_payments < 125000)

options(scipen=10000)                                       # To not allow exponential values

#' Function 1
#'
#' @param df a dataframe
#' @param y a string name for variable y in the dataframe df
#' @param x a string name for variable x in the dataframe df
#'
#' @return boxplot of payments by DRG code
#' @export
#'
#' @examples
#'
plots_function <- function(df, y, x) {                      # Defining the function
  plot <- ggplot(df, aes_string(y = y, x = x,color=x)) +      # Box Plot
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90)) +
    labs (
      x = str_to_title(gsub('_', ' ', x)),              # Formatting for x & y axis
      y = str_to_title(gsub('_', ' ', y)),
      title = paste0('Box Plot of ', str_to_title(gsub('_', ' ', y)), ' by DRG Code')
    ) +                                            # Graph Title
    theme(plot.title = element_text(hjust = 0.5)) +     # Title Center Aligned
    scale_fill_brewer(palette = 'Paired') +             # Color Coding
    theme(legend.position="none") +                     # Legend Removed
    theme(axis.text.x = element_text(size = 5))         # x-Axis Label Size Adjusted

  return(plot)
}



## Function 2
DRG_data_2 <- DRG_data %>% # A df to create Function 2
  select("DRG Definition", "Average Medicare Payments") %>%
  rename(DRG_Code = "DRG Definition",
         Avg_Medicare_Payments = "Average Medicare Payments")
Mod_DRG_Data_q2 <- DRG_data_2 # A df to combine Function 1 and 2


#' Function 2
#'
#' @param x a string name for variable x
#'
#' @return a table that calculates statistics over all of the DRG codes for average Medicare payments
#' @export
#'
#' @examples
statistic_2 <- function (x) { # A statistics function that includes options for mean, sd, or median
  if (x =='mean') {
    Mod_DRG_Data_q2 %>%
      group_by(DRG_Code) %>%
      summarise_at(vars(Avg_Medicare_Payments),
                   list('Mean Average Medicare Payments' = mean))
  }
  else if (x =='sd') {
    Mod_DRG_Data_q2 %>%
      group_by(DRG_Code) %>%
      summarise_at(vars(Avg_Medicare_Payments),
                   list('SD of Average Medicare Payments' = sd))
  }
  else if (x =='median')
    Mod_DRG_Data_q2 %>%
    group_by(DRG_Code) %>%
    summarise_at(vars(Avg_Medicare_Payments),
                 list('Median of Average Medicare Payments' = median))
}


