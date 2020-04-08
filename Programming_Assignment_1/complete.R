# Write a function that reads a directory full of files and reports the number of 
#completely observed cases in each data file. The function should return a data frame 
#where the first column is the name of the file and the second column is the number of 
#complete cases.
complete <- function(directory, id = 1:332){
    
    complete_cases_vector <- vector()
    for (i in id) {
        # create filename by adding numbers to ".csv"
        filename <- paste(sprintf("%03d",i), "csv",sep = ".")
        # read csv to dataframe
        input_df <- read.csv(paste(directory, filename, sep = "\\"))
        # append count of not NA observation for this df
        complete_cases_vector <- append(complete_cases_vector, sum(complete.cases(input_df)))
        
    }
    df = data.frame(id = id, nobs = complete_cases_vector)
    df
    
}
# Test
#complete("specdata", 1)
#complete("specdata", 30:25)
#complete("specdata", c(2, 4, 8, 10, 12))
#complete("specdata", 3)

