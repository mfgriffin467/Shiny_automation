# LIST OF CHANGES NEEDED

# CORRECETD - totals way too small
# Clean up ML code
# Add select all functionality
# Source bottleneck features
# Add skills visualisation
# Fix wildc# Speed up with data cleansing? COnsolidate tables
# Wildard selections - add select / deselect all
# Create general dashbard




#TO DO

# Post on github and shinyIO
# Rename automation 

# Clean up ML code

# Figure out gaps vs 2010 - looks like some professions aggregated eg postsecondary teachers, misc farm workers
# Hide drop downs for specifc tabs
# Re-run ML for 2018
# Fix ranking for main visuals
# Write up aims in Word



#OPTIONAL
# Look at data science jobs / companey themes ???

## global.R ##
library(shiny)
library(shinydashboard)
library(tidyr)
library(dplyr)
library(ggplot2)
library(googleVis)
library(DT)
library(stringr) 
library(RColorBrewer)
library(shinyWidgets)

# Add search job list or other columns


data_df = read.csv("~/Analytics/Courses/NYC_DSA/R_studio/Projects/ML_impacts/clean_dataset_comb.csv",stringsAsFactors=FALSE)

choice_year = c(2010,2012,2014,2016,2018)

# create list of states 
choice_state = data_df %>% 
  distinct(STATE)

choice_state = append(choice_state,'SELECT ALL')

# create list of job groups
choice_job = data_df %>% 
  distinct(top_level_job_category_desc)

choice_job = append(choice_job,'SELECT ALL')

# create list of visible columns
keep_columns = c(
  'YEAR',
  'STATE',
  'OCC_CODE',
  'OCC_TITLE',
  'TOT_EMP',
  'H_MEAN',
  'econ_value',
  'Rank',
  'Probability',
  'major_job_description',
  'top_level_job_category_desc',
  'prob_automation_class',
  'cognitive_abilities',
  'physical_abilities',
  'psychomotor_abilities',
  'sensory_abilities'  
  )