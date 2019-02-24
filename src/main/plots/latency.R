library("dplyr")
# plotTreeLatencyPerExperiment <- function(data, peersInExperiment, colorsToPickFrom, experimentNumbers){
#     ggplot(data = data, aes(x = inputrate, y = latency, color = name, shape = name, linetype = name, group = name)) +
#         geom_point() +
#         geom_line() +
#         scale_colour_manual(name = "Peers/Partitions",
#         labels = unique(data$pap),
#         values = sapply(peersInExperiment, function(x) colorsToPickFrom[x])
#         ) +
#         scale_linetype_manual(name = "Peers/Partitions",
#         labels = unique(data$pap),
#         values = experimentNumbers
#         ) +
#         scale_shape_manual(name = "Peers/Partitions",
#         labels = unique(data$pap),
#         values = sapply(experimentNumbers, function(x) x + 15)
#         ) +
#         labs(y = "Mean - Latency per Partition (s)", x = "Input rate per Partition (Tx/s)")
#     ggsave(normalizePath(file.path(imagesPath, "latency", "tree-latency.", imageOutputFileExtention)), width = 7, height = 4)
# }
#
# plotSingularLatencyPerExperiment <- function(data, peersInExperiment, colorsToPickFrom, experimentNumbers){
#     ggplot(data = data, aes(x = inputrate, y = latency, color = name, shape = name, linetype = name, group = name)) +
#         geom_point() +
#         geom_line() +
#         scale_colour_manual(name = "Peers/Partitions",
#         labels = unique(data$pap),
#         values = sapply(peersInExperiment, function(x) colorsToPickFrom[x])
#         ) +
#         scale_linetype_manual(name = "Peers/Partitions",
#         labels = unique(data$pap),
#         values = experimentNumbers
#         ) +
#         scale_shape_manual(name = "Peers/Partitions",
#         labels = unique(data$pap),
#         values = sapply(experimentNumbers, function(x) x + 15)
#         ) +
#         labs(y = "Mean - Latency (s)", x = "Input rate (Tx/s)")
#
#     ggsave(normalizePath(file.path(imagesPath, "latency", "singular-latency.", imageOutputFileExtention)), width = 7, height = 4)
# }
#
# plotTreeLatencyBoxPlot <- function(data){
#     ggplot(data, aes(x = partitions, y = latency)) +
#         geom_boxplot(aes(group = name, colour = factor(totalpeers))) +
#         scale_x_discrete(limits = c(2, 4, 8)) +
#         coord_flip() +
#         scale_colour_discrete(name = "Peers",
#         limits = c(2, 4, 8, 16, 32)
#         ) +
#         labs(x = "Partitions", y = "Mean - Latency per Partition (s)")
#     ggsave(normalizePath(file.path(imagesPath, "latency", "tree-latency-boxplot.", imageOutputFileExtention)), width = 7, height = 4)
# }
#
# plotTreeLatencyBarsPlot <- function(data){
#     ggplot(data) +
#         geom_bar(stat = "identity", position = "dodge", aes(x = partitions, y = latency, group = name, color = name, fill = name, alpha = 0.3)) +
#         scale_x_discrete(limits = c(2, 4, 8)) +
#         scale_alpha(name = "Experiment", guide = FALSE) +
#         labs(x = "Partitions", y = "Mean - Latency per Partition (s)")
#
#     ggsave(normalizePath(file.path(imagesPath, "latency", "tree-latency-barsplot.", imageOutputFileExtention)), width = 7, height = 4)
#     data$partitions = factor(data$partitions,
#     labels = c(2, 4, 8))
# }
#
# plotLatencyDifference <- function(data){
#     ggplot(data = data, aes(x = type, y = latency, color = name, shape = name, linetype = name, group = name)) +
#         geom_point() +
#         geom_line() +
#         scale_colour_manual(name = "Peers/Partitions",
#         labels = unique(data$pap),
#         values = sapply(peersInExperiment, function(x) colorsToPickFrom[x])
#         ) +
#         scale_linetype_manual(name = "Peers/Partitions",
#         labels = unique(data$pap),
#         values = experimentNumbers
#         ) +
#         scale_shape_manual(name = "Peers/Partitions",
#         labels = unique(data$pap),
#         values = sapply(experimentNumbers, function(x) x + 15)
#         ) +
#         labs(y = "Mean - Latency (s)", x = "Variation")
#     ggsave(normalizePath(file.path(imagesPath, "latency", "latency-differences.", imageOutputFileExtention)), width = 7, height = 4)
# }

plotLatencyForPeerFacets <- function(data, outputPrefix){
    ggplot(data = data, aes(x = inputrate, y = latency / 1000, color = factor(interaction(type, partitions)), group = interaction(type, partitions))) +
        geom_smooth(se = FALSE, size = 0.7) +
        scale_colour_manual(name = "Partitions",
        labels = graphLabels,
        values = sapply(c(1, 2, 3, 4, 5, 6), function(x) colorsToPickFrom[x])
        ) +
        facet_grid(cols = vars(peers),
        labeller = labeller(.cols = label_both)) +
        theme_bw() +
        labs(x = "Input-rate (Tx/s)", y = "Latency (s)")
    ggsave(normalizePath(file.path(imagesPath, "latency", paste0(outputPrefix, "-peer-facet-latencies.", imageOutputFileExtention))), width = 7, height = 4)
}

plotLatencyForPeerFacetsPaper <- function(data, outputPrefix){
    ggplot(data = data, aes(x = inputrate, y = latency / 1000, color = factor(interaction(type, partitions)), group = interaction(type, partitions))) +
        geom_smooth(se = FALSE, size = 0.7) +
        scale_colour_manual(name = "Partitions",
        labels = graphLabels,
        values = sapply(c(1, 2, 3, 4, 5, 6), function(x) colorsToPickFrom[x])
        ) +
        facet_wrap(~ peers,
        ncol = 2,
        labeller = labeller(.cols = label_both)) +
        theme_ieee() +
        labs(x = "Input-rate (Tx/s)", y = "Latency (s)")
    ggsave(normalizePath(file.path(imagesPath, "latency","paper", paste0(outputPrefix, "-peer-facet-latencies.", imageOutputFileExtention))), width = 6, height = 6)
}


experimentLatencyPlots <- function(treeData, singularData, data, outputPrefix){
    plotLatencyForPeerFacets(data, outputPrefix);
    # plotTreeLatencyPerExperiment(treeData, peersInExperiment, colorsToPickFrom, experimentNumbers)
    # plotTreeLatencyBoxPlot(treeData)
    # plotTreeLatencyBarsPlot(treeData)
    # plotSingularLatencyPerExperiment(singularData, peersInExperiment, colorsToPickFrom, experimentNumbers)
    # groupedByNameAndType <- ddply(data, .(name, type, pap), summarize, latency = mean(latency, na.rm=TRUE))
    # plotLatencyDifference(groupedByNameAndType)
}