outcome_data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
best <- function(state, outcome){
    binded_col_name_num = c("heart attack" = 11,"heart failure" = 17,"pneumonia" = 23)
    if (!(state %in% unique(outcome_data$State)))
        stop("invalid state")
    if (!(outcome %in% names(binded_col_name_num)))
        stop("invalid outcome")
    data_specific_to_state = outcome_data[outcome_data$State == state, ]
    desired_col_data <- data_specific_to_state[, binded_col_name_num[[outcome]]]
    desired_col_data <- as.numeric(desired_col_data)
    hospial_names <- with(data_specific_to_state, 
                          Hospital.Name[desired_col_data == min(desired_col_data, na.rm = TRUE)])
    sort(hospial_names)[1]
}
