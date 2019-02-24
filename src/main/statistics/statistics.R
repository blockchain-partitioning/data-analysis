library("effsize")

amountOfPeers <- c(4, 8, 16, 32)
amountOfPartitions <- c(2, 4, 8)
printLinearRegression <- function(data, controlData, treatmentData){
    # model = lm(throughput ~ factor(type) , data = data)
    # print(model)
    print("Model summary")
    # print(summary(model))
    print("Shapiro - Control")
    printShapiroOnData(controlData)
    print("Shapiro - Treatment")
    printShapiroOnData(treatmentData)
    print("Kurskal-Wallis - Type")
    # print(kruskal.test(throughput ~ factor(type), data = data))
    print("T Test - Treament v. Control")
    printPairedTTestOnData(controlData, treatmentData)
    print("Anova")
    # anv <- anova(model)
    # print(anv)
    # a1 <- aov(throughput ~ factor(type) , data = data)
    print("Tukey - Type")
    # posthoc <- TukeyHSD(x = a1, 'factor(type)', conf.level = 0.8)
    # print(posthoc)
    # print(summary(anv))
}

printShapiroOnData <- function(data){
    if ("partitions" %in% colnames(data))
    {
        for (peer in amountOfPeers) {
            for (partition in amountOfPartitions) {
                if ((peer %% partition) == 0) {
                    print(paste0("Partitions: ", partition))
                    print(paste0("Peers: ", peer))
                    print(shapiro.test(subset(data, partitions == partition & peers == peer)$throughput))
                }
            }
        }
    }
}

printPairedTTestOnData <- function(controlData, treatmentData){
    if ("partitions" %in% colnames(controlData) && "partitions" %in% colnames(treatmentData))
    {
        for (peer in amountOfPeers) {
            for (partition in amountOfPartitions) {
                if ((peer %% partition) == 0) {
                    print(paste0("Partitions: ", partition))
                    print(paste0("Peers: ", peer))
                    controlPaired <- subset(controlData, partitions == partition & peers == peer)
                    treatmentPaired <- subset(treatmentData, partitions == partition & peers == peer)
                    print(t.test(treatmentPaired$throughput, controlPaired$throughput, paired = TRUE, alternative = "two.sided"))
                }
            }
        }
    }
}

partitionMean <- function(treeData, partition){
    partitionData <- treeData[treeData$partitions == partition,]
    return(mean(partitionData$throughput, na.rm = TRUE))
}

partitionMeans <- function(treeData){
    partitions <- unique(treeData$partitions)
    return(sapply(partitions, function(partition) partitionMean(treeData, partition)))
}

effectSize <- function(partition, singularPartitionData, treePartitionData){
    print(paste("Cohen's D - Partition ", partition))
    effectSize <- cohen.d(treePartitionData$throughput, singularPartitionData$throughput)
    print(effectSize)
}

difference <- function(partition, singularPartitionData, treePartitionData){
    print(paste("Mean Difference - Control vs. Partition ", partition))
    print(mean(treePartitionData$throughput - singularPartitionData$throughput))
}

differences <- function(means, singularData, partitions, data, treeData){
    for (i in 1 : length(partitions)) {
        partition <- partitions[i]
        singularPartitionData <- singularData[singularData$partitions == partition,]
        treePartitionData <- treeData[treeData$partitions == partition,]
        effectSize(partition, singularPartitionData, treePartitionData)
    }
    for (i in 1 : length(partitions)) {
        partition <- partitions[i]
        singularPartitionData <- singularData[singularData$partitions == partition,]
        treePartitionData <- treeData[treeData$partitions == partition,]
        difference(partition, singularPartitionData, treePartitionData)
    }
}

compareControlToPartitions <- function(controlData, treatmentData, data){
    partitions <- unique(treatmentData$partitions)
    print(partitions)
    means <- partitionMeans(treatmentData)
    factors <- c()
    for (i in 1 : length(partitions)) {
        partition = partitions[i]
        partitionMean <- means[i]
        singularPartitionData <- controlData[controlData$partitions == partition,]
        factor <- partitionMean / mean(singularPartitionData$throughput, na.rm = TRUE)
        factors[length(factors) + 1] <- factor
    }

    print("==Control vs. Treatment==")
    print("Mean - Throughput (Control):")
    print(mean(controlData$throughput), na.rm = TRUE)
    print("Mean - Throughput (Partitions):")
    names(means) <- partitions
    print(means)
    print(mean(treatmentData$throughput), na.rm = TRUE)
    print("Max - Throughput (Control):")
    print(max(controlData$throughput))
    print("Max - Throughput (Partitions):")
    print(max(treatmentData$throughput))
    print("Throughput increases (Control/# of Partitions):")
    print(factors)
    meanFactor = mean(treatmentData$throughput / controlData$throughput, na.rm = TRUE)
    print("Mean - Throughput increase (Control/Treatment):")
    print(meanFactor)
    print("Standard deviation")
    print("Control")
    print(sd(controlData$throughput))
    print("Tree")
    print(sd(treatmentData$throughput))
    print(sd(data$throughput))
    print("Effect sizes - Treatment vs. Control (Partitions)")
    differences(means, controlData, partitions, data, treatmentData)
    print("Effect sizes - Treatment vs. Control (Grouped)")
    treatment <- treatmentData$throughput
    control <- controlData$throughput
    effectSize <- cohen.d(treatment, control)
    print(effectSize)
}

comparePartitions <- function(treeData) {
    print("==Partition vs. partition==")
    partitions <- unique(treeData$partitions)
    M <- matrix(nrow = length(partitions), ncol = length(partitions))
    means <- partitionMeans(treeData)
    for (col in 1 : length(partitions)) {
        colMean <- means[col]
        for (row in 1 : length(partitions)) {
            if (col == row) {
                M[row, col] <- NA
            }
            else {
                rowMean <- means[row]
                if (colMean > rowMean) {
                    M[row, col] <- colMean / rowMean
                }
            }
        }
    }
    dimnames(M) = list(
    + partitions, # row names
    + partitions) # column names
    print("Throughput increases (Partitions):")
    print(M)
}

throughputFactorIncrease <- function(data, controlData, treatmentData){
    print("=Throughput=")
    compareControlToPartitions(controlData, treatmentData, data)
    comparePartitions(treatmentData)
}

throughputDifferenceSingularTree <- function(treeData, singularData) {
    print(shapiro.test(treeData$throughput))
    print(shapiro.test(singularData$throughput))
    print(t.test(treeData$throughput, singularData$throughput, alternative = "two.sided", var.equal = FALSE))
    print("Mean Difference - Treatment vs. Control")
}
throughputDifferenceBetweenPartitions <- function() {
}
latencyDifferenceBetweenPartitions <- function() {
}
cpuUsageDifferenceBetweenPartitions <- function() {
}
statistics <- function(data, hasControl, hasTreatment) {
    treatmentData <- data.frame()
    if (hasTreatment) {
        treatmentData <- data[data$type == "treatment",]
    }
    controlData <- data.frame()
    if (hasControl) {
        controlData <- data[data$type == "control",]
    }

    printLinearRegression(data, controlData, treatmentData)
    # throughputDifferenceSingularTree(treatmentData, controlData)
    # throughputFactorIncrease(data, controlData, treatmentData)
}