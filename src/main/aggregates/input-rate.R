sampleInputRate <- function(sample){
    amountOfTransactions <- nrow(sample)
    timeInBetween = max(sample$time_create) - min(sample$time_create)
    timeInSeconds = timeInBetween / 1000
    return(amountOfTransactions / timeInSeconds)
}