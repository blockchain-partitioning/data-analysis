source("aggregates/util.R")

transactionsPerSecond <- function(sample){
    sample <- sample[sample$status == "success",]
    amountOfTransactions <- nrow(sample)
    timeInBetween = max(sample$time_final) - min(sample$time_create)
    timeInSeconds = timeInBetween / 1000
    return(amountOfTransactions / timeInSeconds)
}

throughputInTreeForRound <- function(resultsPath, round){
    leafs <- leafSamples(resultsPath, round)
    amountOfSamples <- length(leafs[[1]])
    throughputs <- c()
    for (i in 1 : amountOfSamples) {
        finishTimes <- c()
        startTimes <- c()
        amountOfTransactions <- 0
        for (j in 1 : length(leafs)) {
            samples <- leafs[[j]]
            sample <- samples[[i]]
            successFul = sample[sample$status == "success",]
            finishTimes[length(finishTimes) + 1] <- max(successFul$time_final)
            startTimes[length(startTimes) + 1] <- min(successFul$time_create)
            amountOfTransactions <- amountOfTransactions + nrow(successFul)
        }
        timeDifference <- max(finishTimes) - min(startTimes)
        differenceInSeconds = timeDifference / 1000
        throughputs[length(throughputs) + 1] <- amountOfTransactions / differenceInSeconds
    }
    return(mean(throughputs))
}

throughputInSingularForRound <- function(path, round){
    samples <- getSamplesForRound(path, round)
    throughputs <- c()
    for (i in 1 : length(samples)) {
        sample <- samples[[i]]
        throughputs[length(throughputs) + 1] <- transactionsPerSecond(sample[sample$status == "success",])
    }
    return(throughputs)
}


throughputInTree <- function(resultsPath, amountOfRounds){
    type <- "transactions"
    rounds <- c()
    for (i in 1 : amountOfRounds) {
        roundName <- getRoundName(i, type)
        rounds[length(rounds) + 1] <- throughputInTreeForRound(resultsPath, roundName)
    }
    return(rounds)
}

throughputInSingular <- function(resultsPath, amountOfRounds){
    type <- "transactions"
    rounds <- list()
    path <- file.path(resultsPath, "singular")
    for (i in 1 : amountOfRounds) {
        roundName <- getRoundName(i, type)
        throughputs <- throughputInSingularForRound(path, roundName)
        rounds[[length(rounds) + 1]] <- throughputs
    }
    return(rounds)
}