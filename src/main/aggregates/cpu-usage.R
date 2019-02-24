source("aggregates/util.R")

# 1. Get keys for peers only grepl(peer) !grepl(chaincode)
# 2. Use key to get cpuUsage from statistics$key$cpu_percent

peerContainerIds <- function(frame){
    peers <- frame$peers
    ids <- c()
    for (i in 1 : length(peers$info$NAME)) {
        name <- peers$info$NAME[i]
        if (grepl("peer", name, fixed=TRUE) && !grepl("chaincode", name, fixed=TRUE)) {
            ids[length(ids) + 1] <- peers$key[i]
        }
    }
    return(ids)
}

meanCpuUsageForContainers <- function(frame, containerIds){
    containerCpuUsageMeans <- c()
    for (i in 1 : length(containerIds)) {
        id <- containerIds[i]
        stats <- frame$statistics[[id]]
        cpuPercent <- mean(as.numeric(stats$cpu_percent))
        containerCpuUsageMeans[length(containerCpuUsageMeans) + 1] <- cpuPercent
    }
    return (mean(containerCpuUsageMeans))
}

cpuUsage <- function(sample){
    containerIds <- peerContainerIds(sample)
    return(meanCpuUsageForContainers(sample, containerIds))
}

cpuUsageInTreeForRound <- function(resultsPath, roundName){
    leafs <- leafSamples(resultsPath, roundName)
    amountOfSamples <- length(leafs[[1]])
    cpuUsageMeans <- c()
    for (i in 1 : amountOfSamples) {
        sampleCpuUsages <- c()
        for (j in 1 : length(leafs)) {
            samples <- leafs[[j]]
            sample <- samples[[i]]
            sampleCpuUsages[length(sampleCpuUsages) + 1] <- cpuUsage(sample)
        }
        cpuUsageMeans[length(cpuUsageMeans) + 1] <- mean(sampleCpuUsages)
    }

    return(mean(cpuUsageMeans))
}

cpuUsageInSingularForRound <- function(path, round){
    samples <- getSamplesForRound(path, round)
    cpuUsages <- c()
    for (i in 1 : length(samples)) {
        sample <- samples[[i]]
        cpuUsages[length(cpuUsages) + 1] <- cpuUsage(sample)
    }
    return(cpuUsages)
}

cpuUsageInTree <- function(resultsPath, amountOfRounds){
    type <- "resources"
    rounds <- c()
    for (i in 1 : amountOfRounds) {
        roundName <- getRoundName(i, type)
        rounds[length(rounds) + 1] <- cpuUsageInTreeForRound(resultsPath, roundName)
    }
    return(rounds)
}

cpuUsageInSingular <- function(resultsPath, amountOfRounds){
    type <- "resources"
    rounds <- c()
    path <- file.path(resultsPath, "singular")
    for (i in 1 : amountOfRounds) {
        roundName <- getRoundName(i, type)
        rounds[length(rounds) + 1] <- cpuUsageInSingularForRound(path, roundName)
    }
    return(rounds)
}