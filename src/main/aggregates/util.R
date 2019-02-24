library("jsonlite")

getSample <- function(filePath){
    print(filePath)
    sample <- read_json(filePath, simplifyDataFrame = TRUE)
    return(sample)
}

getSamplesForRound <- function(path, matchToRound){
    files <- list.files(path)
    samples <- list()
    for (i in 1 : length(files)) {
        fileName <- files[i]
        if (grepl(matchToRound, fileName, fixed = TRUE)) {
            samples[[length(samples) + 1]] <- getSample(file.path(path, fileName))
        }
    }
    return(samples)
}

getTreeFramesForRound <- function(paths, matchToRound){
    sampleFrames <- list()
    for (i in 1 : length(paths)) {
        directoryPath <- paths[i]
        samples <- getSamplesForRound(directoryPath[[1]], matchToRound)
        sampleFrames[[length(sampleFrames) + 1]] <- samples
    }
    return(sampleFrames)
}

leafSamples <- function(resultsPath, round){
    treePath <- file.path(resultsPath, "tree")
    directories <- list.dirs(treePath)
    paths <- c()
    for (i in 2 : length(directories)) {
        path <- directories[i]
        if (! grepl("root", path, fixed = TRUE)) {
            paths[i - 1] <- path
        }
    }

    return(getTreeFramesForRound(paths, round))
}

getRoundName <- function(number, type) {
    return(paste("round-", number, "-", type, sep = ""))
}
