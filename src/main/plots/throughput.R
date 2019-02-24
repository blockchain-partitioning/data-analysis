library("dplyr")
plotTreatmentThroughputPerExperiment <- function(data, peersInExperiment, colorsToPickFrom, experimentNumbers){
    ggplot(data = data, aes(x = inputrate, y = throughput, color = name, linetype = name, group = name)) +
        geom_smooth(se = FALSE, size = 0.7) +
        scale_colour_manual(name = "Peers/Partitions",
        labels = unique(data$label),
        values = sapply(peersInExperiment, function(x) colorsToPickFrom[x])
        ) +
        scale_linetype_manual(name = "Peers/Partitions",
        labels = unique(data$label),
        values = experimentNumbers
        ) +
        theme_bw() +
        labs(y = "Total Throughput (Tx/s)", x = "Input rate per Partition (Tx/s)")
    ggsave(normalizePath(file.path(imagesPath, "throughput", "treatment-throughputs.", imageOutputFileExtention)), width = 7, height = 4)
}

plotControlThroughputPerExperiment <- function(data, peersInExperiment, colorsToPickFrom, experimentNumbers){
    ggplot(data = data, aes(x = inputrate, y = throughput, color = name, linetype = name, group = name)) +
        geom_smooth(se = FALSE, size = 0.7) +
        scale_colour_manual(name = "Peers",
        labels = unique(data$label),
        values = sapply(peersInExperiment, function(x) colorsToPickFrom[x])
        ) +
        scale_linetype_manual(name = "Peers",
        labels = unique(data$label),
        values = unique(data$peers)
        ) +
        theme_bw() +
        labs(y = "Throughput (Tx/s)", x = "Input rate (Tx/s)")

    ggsave(normalizePath(file.path(imagesPath, "throughput", "control-throughputs.", imageOutputFileExtention)), width = 7, height = 4)
}

plotTreeThroughputBoxPlot <- function(data){
    ggplot(data, aes(x = partitions, y = throughput)) +
        geom_boxplot(aes(group = name, colour = factor(totalpeers))) +
        scale_x_discrete(limits = c(2, 4, 8)) +
        coord_flip() +
        scale_colour_discrete(name = "Total Peers",
        limits = c(2, 4, 8, 16, 32)
        ) +
        theme_bw() +
        labs(x = "Partitions", y = "Throughput (Tx/s)")
    ggsave(normalizePath(file.path(imagesPath, "throughput", "treatment-throughputs-boxplot.", imageOutputFileExtention)), width = 7, height = 4)
}

plotTreeThroughputBarsPlot <- function(data){
    ggplot(data) +
        geom_bar(stat = "identity", position = "dodge", aes(x = partitions, y = throughput, group = name, color = name, fill = name, alpha = 0.5)) +
        scale_x_discrete(limits = c(2, 4, 8)) +
        scale_alpha(guide = FALSE) +
        theme_bw() +
        labs(x = "Partitions", y = "Throughput (Tx/s)")
    ggsave(normalizePath(file.path(imagesPath, "throughput", "treatment-barsplot.", imageOutputFileExtention)), width = 7, height = 4)
    data$partitions = factor(data$partitions,
    labels = c(2, 4, 8))
}

plotThroughputDifference <- function(data){
    ggplot(data = data, aes(x = type, y = throughput, color = name, shape = name, linetype = name, group = name)) +
        geom_point() +
        geom_line(size = 0.7) +
        scale_colour_manual(name = "Peers/Partitions",
        labels = unique(data$name),
        values = sapply(peersInExperiment, function(x) colorsToPickFrom[x])
        ) +
        scale_linetype_manual(name = "Peers/Partitions",
        labels = unique(data$name),
        values = experimentNumbers
        ) +
        scale_shape_manual(name = "Peers/Partitions",
        labels = unique(data$name),
        values = sapply(experimentNumbers, function(x) x + 15)
        ) +
        theme_bw() +
        labs(y = "Mean - Throughput (Tx/s)", x = "Variation")
    ggsave(normalizePath(file.path(imagesPath, "throughput", "throughput-differences.", imageOutputFileExtention)), width = 7, height = 4)
}

plotThroughputForPeerFacets <- function(data, outputPrefix){
    ggplot(data = data, aes(x = inputrate, y = throughput, color = factor(interaction(type, partitions)), group = interaction(type, partitions))) +
        geom_smooth(se = FALSE, size = 0.7) +
        scale_colour_manual(name = "Partitions",
        labels = graphLabels,
        values = sapply(c(1, 2, 3, 4, 5, 6), function(x) colorsToPickFrom[x])
        ) +
        facet_grid(cols = vars(peers),
        labeller = labeller(.cols = label_both)) +
        theme_bw() +
        labs(x = "Input-rate (Tx/s)", y = "Throughput (Tx/s)")
    ggsave(normalizePath(file.path(imagesPath, "throughput", paste0(outputPrefix, "-peer-facet-throughputs.", imageOutputFileExtention))), width = 7, height = 4)
}

plotThroughputForPeerFacetsPaper <- function(data, outputPrefix){
    ggplot(data = data, aes(x = inputrate, y = throughput, color = factor(interaction(type, partitions)), group = interaction(type, partitions))) +
        geom_smooth(se = FALSE, size = 0.7) +
        scale_colour_manual(name = "Partitions",
        labels = graphLabels,
        values = sapply(c(1, 2, 3, 4, 5, 6), function(x) colorsToPickFrom[x])
        ) +
        facet_wrap(~ peers,
        ncol = 2,
        labeller = labeller(.cols = label_both)) +
        theme_ieee() +
        labs(x = "Input-rate (Tx/s)", y = "Throughput (Tx/s)")
    ggsave(normalizePath(file.path(imagesPath,"paper", "throughput", paste0(outputPrefix, "-peer-facet-throughputs.", imageOutputFileExtention))), width = 6, height = 6)
}

experimentThroughputPlots = function(treatmentData, controlData, allData, outputPrefix){
    plotThroughputForPeerFacets(allData, outputPrefix);
    # plotTreatmentThroughputPerExperiment(treatmentData, peersInExperiment, colorsToPickFrom, experimentNumbers)
    # plotTreeThroughputBoxPlot(treatmentData)
    # plotTreeThroughputBarsPlot(treatmentData)
    # plotControlThroughputPerExperiment(controlData, peersInExperiment, colorsToPickFrom, experimentNumbers)
    # groupedByNameAndType <- ddply(data, .(name, type), summarize, throughput = throughput)
    # plotThroughputDifference(groupedByNameAndType)
}