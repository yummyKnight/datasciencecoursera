#Part 1
# Write a function named 'pollutantmean' that calculates the mean of a pollutant 
# (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' 
#takes three arguments: 'directory', 'pollutant', and 'id'. 
#Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate 
#matter data from the directory specified in the 'directory' argument and returns 
#the mean of the pollutant across all of the monitors, ignoring any missing values 
# coded as NA.

pollutantmean <- function(directory, pollutant, id = 1:332) {
    # create empty vector
    pollutant_vector <- vector()
    
    for (i in id) {
        # create filename by adding numbers to ".csv"
        filename <- paste(sprintf("%03d",i), "csv",sep = ".")
        # read csv to dataframe
        df <- read.csv(paste(directory, filename, sep = "\\"))
        # add needed column to vector
        pollutant_vector <- append(pollutant_vector,df[[pollutant]])
    }
    # calculate result
    final_mean <- mean(pollutant_vector, na.rm = TRUE)
    final_mean
}
#pollutantmean("specdata", "nitrate", 70:72)