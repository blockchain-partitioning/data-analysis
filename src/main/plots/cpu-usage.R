plotTreeCpuUsagePerExperiment <- function(data, peersInExperiment, colorsToPickFrom, experimentNumbers){
    ggplot(data = data, aes(x = inputrate, y = cpupercent, color = name, shape = name, linetype = name, group = name)) +
        geom_point() +
        geom_line() +
        scale_colour_manual(name = "Peers/Partitions",
        labels = unique(data$pap),
        values = sapply(peersInExperiment, function(x) colorsToPickFrom[x])
        ) +
        scale_linetype_manual(name = "Peers/Partitions",
        labels = unique(data$pap),
        values = experimentNumbers
        ) +
        scale_shape_manual(name = "Peers/Partitions",
        labels = unique(data$pap),
        values = sapply(experimentNumbers, function(x) x + 15)
        ) +
        labs(y = "Total Mean - CPU-usage per Peer (%)", x = "Input rate per Partition (Tx/s)")
    ggsave(normalizePath(file.path(imagesPath, "cpu-usage", "tree-cpu-usage.", imageOutputFileExtention)), width = 7, height = 4)
}

plotSingularCpuUsagePerExperiment <- function(data, peersInExperiment, colorsToPickFrom, experimentNumbers){
    ggplot(data = data, aes(x = inputrate, y = cpupercent, color = name, shape = name, linetype = name, group = name)) +
        geom_point() +
        geom_line() +
        scale_colour_manual(name = "Peers/Partitions",
        labels = unique(data$pap),
        values = sapply(peersInExperiment, function(x) colorsToPickFrom[x])
        ) +
        scale_linetype_manual(name = "Peers/Partitions",
        labels = unique(data$pap),
        values = experimentNumbers
        ) +
        scale_shape_manual(name = "Peers/Partitions",
        labels = unique(data$pap),
        values = sapply(experimentNumbers, function(x) x + 15)
        ) +
        labs(y = "Mean - CPU-usage per Peer (%)", x = "Input rate (Tx/s)")

    ggsave(normalizePath(file.path(imagesPath, "cpu-usage", "singular-cpu-usage.", imageOutputFileExtention)), width = 7, height = 4)
}

plotTreeCpuUsageBoxPlot <- function(data){
    ggplot(data, aes(x = partitions, y = cpupercent)) +
        geom_boxplot(aes(group = name, colour = factor(totalpeers))) +
        scale_x_discrete(limits = c(2, 4, 8)) +
        coord_flip() +
        scale_colour_discrete(name = "Total Peers",
        limits = c(2, 4, 8, 16, 32)
        ) +
        labs(x = "Partitions", y = "Mean - CPU-usage per Peer (%)")
    ggsave(normalizePath(file.path(imagesPath, "cpu-usage", "tree-cpu-usage-boxplot.", imageOutputFileExtention)), width = 7, height = 4)
}

plotTreeCpuUsageBarsPlot <- function(data){
    ggplot(data) +
        geom_bar(stat = "identity", position = "dodge", aes(x = partitions, y = cpupercent, group = name, color = name, fill = name, alpha = 0.5)) +
        scale_x_discrete(limits = c(2, 4, 8)) +
        scale_alpha(guide = FALSE) +
        labs(x = "Partitions", y = "Mean - CPU-usage per Peer (%)")
    ggsave(normalizePath(file.path(imagesPath, "cpu-usage", "tree-barsplot.", imageOutputFileExtention)), width = 7, height = 4)
    data$partitions = factor(data$partitions,
    labels = c(2, 4, 8))
}

plotCpuUsageDifference <- function(data){
    ggplot(data = data, aes(x = type, y = cpupercent, color = name, shape = name, linetype = name, group = name)) +
        geom_point() +
        geom_line() +
        scale_colour_manual(name = "Peers/Partitions",
        labels = unique(data$pap),
        values = sapply(peersInExperiment, function(x) colorsToPickFrom[x])
        ) +
        scale_linetype_manual(name = "Peers/Partitions",
        labels = unique(data$pap),
        values = experimentNumbers
        ) +
        scale_shape_manual(name = "Peers/Partitions",
        labels = unique(data$pap),
        values = sapply(experimentNumbers, function(x) x + 15)
        ) +
        labs(y = "Mean - CPU-usage per Peer (%)", x = "Variation")
    ggsave(normalizePath(file.path(imagesPath, "cpu-usage", "cpu-usage-differences.", imageOutputFileExtention)), width = 7, height = 4)
}

plotCpuUsageForPeerFacets <- function(data, outputPrefix){
    ggplot(data = data, aes(x = inputrate, y = cpupercent, color = factor(interaction(type, partitions)), group = interaction(type, partitions))) +
        geom_smooth(se = FALSE, size = 0.7) +
        scale_colour_manual(name = "Partitions",
        labels = graphLabels,
        values = sapply(c(1, 2, 3, 4, 5, 6), function(x) colorsToPickFrom[x])
        ) +
        facet_grid(cols = vars(peers),
        labeller = labeller(.cols = label_both)) +
        theme_bw() +
        labs(x = "Input-rate (Tx/s)", y = "CPU-Usage (%)")
    ggsave(normalizePath(file.path(imagesPath, "cpu-usage", paste0(outputPrefix, "-peer-facet-cpu-usage.", imageOutputFileExtention))), width = 7, height = 4)
}

plotCpuUsageForPeerFacetsPaper <- function(data, outputPrefix){
    ggplot(data = data, aes(x = inputrate, y = cpupercent, color = factor(interaction(type, partitions)), group = interaction(type, partitions))) +
        geom_smooth(se = FALSE, size = 0.7) +
        scale_colour_manual(name = "Partitions",
        labels = graphLabels,
        values = sapply(c(1, 2, 3, 4, 5, 6), function(x) colorsToPickFrom[x])
        ) +
        facet_wrap(~ peers,
        ncol = 2,
        labeller = labeller(.cols = label_both)) +
        theme_ieee() +
        labs(x = "Input-rate (Tx/s)", y = "CPU-Usage (%)")
    ggsave(normalizePath(file.path(imagesPath,"paper", "cpu-usage", paste0(outputPrefix, "-peer-facet-cpu-usage.", imageOutputFileExtention))), width = 6, height = 6)
}

experimentCpuUsagePlots = function(treeData, singularData, data, outputPrefix){
    plotCpuUsageForPeerFacets(data, outputPrefix)
    # plotTreeCpuUsagePerExperiment(treeData, peersInExperiment, colorsToPickFrom, experimentNumbers)
    # plotTreeCpuUsageBoxPlot(treeData)
    # plotTreeCpuUsageBarsPlot(treeData)
    # plotSingularCpuUsagePerExperiment(singularData, peersInExperiment, colorsToPickFrom, experimentNumbers)
    # groupedByNameAndType <-ddply(data, .(name, type, pap), summarize, cpupercent=mean(cpupercent, na.rm=TRUE))
    # plotCpuUsageDifference(groupedByNameAndType)
}