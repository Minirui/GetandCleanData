Read Me
========================================================

The goal of this analysis is to produce a tidy data set as outlined in the Getting and Cleaning Data course project.   

The run_analysis.R file needs the provided "UCI HAR Dataset" folder in the working directory. The general flow of the script:

 * Loads needed packages and data files
 * Attaches the variable names as found in the features file
 * Adds activites and volunter number to data sets
 * Reduces the variables to those that are mean and standard deviation of a measurement. This is done by looking for those variables containing mean() and std() in their name. Other variables containing mean in the  name were not included. 
 * Renaming the activities according to activity labels
 * From the first tidy data set the second tidy data set is created from by averaging all varaiables by activity and volunter number.  
 * Saves the second tidy data set to file tidy.txt


The Code book 