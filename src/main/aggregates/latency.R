source("aggregates/util.R")

sampleLatenciesInSeconds <- function(sample){
    return((sample[["time_final"]] - sample[["time_create"]]) / 1000)
}

meanLatencyInSeconds <- function(sample){
    sample <- sample[sample$status == "success",]
    return(mean(sampleLatenciesInSeconds(sample)))
}

latencyInRound <- function(path, roundName){
    samples <- getSamplesForRound(path, roundName)
    latencies <- c()
    for (i in 1 : length(samples)) {
        sample <- samples[[i]]
        latencies[length(latencies) + 1] <- meanLatencyInSeconds(sample[sample$status == "success",])
    }
    return(latencies)
}

meanLatencyInRoundForTree <- function(resultsPath, roundName){
    leafs <- leafSamples(resultsPath, roundName)
    amountOfSamples <- length(leafs[[1]])
    latencies <- c()
    for (i in 1 : amountOfSamples) {
        sampleLatencies <- c()
        for (j in 1 : length(leafs)) {
            samples <- leafs[[j]]
            sample <- samples[[i]]
            successFul = sample[sample$status == "success",]
            sampleLatencies[length(sampleLatencies) + 1] <- meanLatencyInSeconds(successFul)
        }
        latencies[length(latencies) + 1] <- mean(sampleLatencies)
    }
    return(latencies)
}

latencyInTree <- function(resultsPath, amountOfRounds){
    type <- "transactions"
    rounds <- c()
    for (i in 1 : amountOfRounds) {
        roundName <- getRoundName(i, type)
        rounds[length(rounds) + 1] <- meanLatencyInRoundForTree(resultsPath, roundName)
    }
    return(rounds)
}

latencyInSingular <- function(resultsPath, amountOfRounds) {
    type <- "transactions"
    rounds <- c()
    path <- file.path(resultsPath, "singular")
    for (i in 1 : amountOfRounds) {
        roundName <- getRoundName(i, type)
        rounds[length(rounds) + 1] <- latencyInRound(path, roundName)
    }
    return(rounds)
}