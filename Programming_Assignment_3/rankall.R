rankall <- function(outcome, num = "best") {
    outcome_data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
    binded_col_name_num = c("heart attack" = 11,"heart failure" = 17,"pneumonia" = 23)
    
    if (!(outcome %in% names(binded_col_name_num)))
        stop("invalid outcome")
    
    if (!(num %in% c("best", "worst")) & !is.numeric(num))
        stop("invalid num")
    
    desired_col_data <- outcome_data[, binded_col_name_num[[outcome]]]
    desired_col_data <- as.numeric(desired_col_data)
    
    decr = ifelse(num == "worst", TRUE, FALSE)
    
    newdata <- outcome_data[order(outcome_data$State, desired_col_data, outcome_data$Hospital.Name, na.last = NA,
                                  decreasing = decr),]
    
    if (num == "best" | num == "worst") num = 1
    out_data_frame <- data.frame(row.names = c("hospital","state"))
    for (state in sort(unique(outcome_data$State))) {
        hospitals_names <- newdata[newdata$State == state, ]$Hospital.Name
        if (length(hospitals_names) < num) {
            
            out_data_frame <- rbind(out_data_frame,data.frame(hospital = NA, state = state))
            next()
        }  
        out_data_frame <- rbind(out_data_frame, data.frame(hospital = hospitals_names[[num]], state = state))

    }
    out_data_frame
}
