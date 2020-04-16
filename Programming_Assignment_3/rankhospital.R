rankhospital <- function(state, outcome, num = "best") {
    outcome_data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
    binded_col_name_num = c("heart attack" = 11,"heart failure" = 17,"pneumonia" = 23)
    
    if (!(state %in% unique(outcome_data$State)))
        stop("invalid state")
    
    if (!(outcome %in% names(binded_col_name_num)))
        stop("invalid outcome")

    if (!(num %in% c("best", "worst")) & !is.numeric(num))
        stop("invalid num")
    
    data_specific_to_state = outcome_data[outcome_data$State == state, ]
    if (is.numeric(num) & num > nrow(data_specific_to_state))
        return(NA)
    
    desired_col_data <- data_specific_to_state[, binded_col_name_num[[outcome]]]
    desired_col_data <- as.numeric(desired_col_data)
    newdata <- data_specific_to_state[order(desired_col_data, data_specific_to_state$Hospital.Name, na.last = NA),]
    if (num == "best") return(head(newdata$Hospital.Name, 1))
    if (num == "worst") return(tail(newdata$Hospital.Name, 1))
    return(newdata$Hospital.Name[num])

}
