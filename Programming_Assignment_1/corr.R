# Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate 
#for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. The function should return a 
#vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should 
#return a numeric vector of length 0.

corr <- function(directory, threshold = 0){
    corr_vector <- vector()
    for (i in list.files(directory)) {
        # read csv to dataframe
        input_df <- read.csv(paste(directory, i, sep = "\\"))
        good_cases = complete.cases(input_df)
        # Has data passed the threshold?
        if (sum(good_cases) > threshold) {
            clean_df = input_df[good_cases,]
            # append cor between 2 columns to vector
            corr_vector <- append(corr_vector, cor(clean_df[["sulfate"]], clean_df[["nitrate"]]))
        }
        
    }
    corr_vector
}
# Tests
#cr <- corr("specdata", 150)
#summary(cr)
#cr <- corr("specdata", 400)
#summary(cr)
#cr <- corr("specdata", 5000)
#summary(cr)
#cr <- corr("specdata")
#summary(cr)