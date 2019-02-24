library("ggplot2")
library("plyr")
library("svglite") # Not imported by ggplot2 for some reason. Be sure to install this manually.

source("plots/throughput.R")
source("plots/latency.R")
source("plots/cpu-usage.R")

imagesPath <- file.path("..", "resources", "images")
colorsToPickFrom <- c("#332288",
"#88CCEE",
"#44AA99",
"#117733",
"#999933",
"#DDCC77")
# 332288, 88CCEE, 44AA99, 117733, 999933, DDCC77, CC6677, 882255, AA4499 <- distinctive colors
experimentNumbers <- c(1, 2, 1, 2, 3, 1, 2, 3, 1, 2, 3)
peersInExperiment <- c(2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5)

theme_ieee <- function (base_size = 16, base_family = "")
{
    half_line <- base_size / 2
    theme(
    line = element_line(colour = "grey30", size = 0.5, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", colour = "grey30", size = 0.5, linetype = 1),
    text = element_text(family = base_family, face = "plain", colour = "black", size = base_size, lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), debug = FALSE),
    axis.text = element_text(size = rel(0.8), colour = "grey30"),
    axis.text.x = element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.y = element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.ticks = element_line(colour = "grey30"),
    axis.ticks.length = unit(half_line / 2, "pt"),
    axis.title.x = element_text(margin = margin(t = 0.8 * half_line, b = 0.8 * half_line / 2)),
    axis.title.y = element_text(angle = 90, margin = margin(r = 0.8 * half_line, l = 0.8 * half_line / 2)),
    legend.background = element_rect(colour = NA),
    legend.margin = unit(0.2, "cm"),
    legend.key = element_blank(),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.8)),
    legend.text.align = NULL,
    legend.title = element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    panel.background = element_rect(fill = "white", colour = "grey30"),
    panel.border = element_rect(fill = NA, colour = "grey30", size = 0.3),
    panel.grid.major = element_line(colour = "grey30", size = 0.25),
    panel.grid.minor = element_line(colour = "grey30", size = 0.1),
    panel.margin = unit(half_line, "pt"),
    panel.margin.x = NULL,
    panel.margin.y = NULL,
    panel.ontop = FALSE,
    strip.background = element_blank(),
    strip.text = element_text(colour = "grey30", size = rel(0.8)),
    strip.text.x = element_text(margin = margin(t = half_line, b = half_line)),
    strip.text.y = element_text(angle = - 90, margin = margin(l = half_line, r = half_line)),
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),
    plot.background = element_rect(colour = "white"),
    plot.title = element_text(size = rel(1.2), margin = margin(b = half_line * 1.2)),
    plot.margin = margin(half_line, half_line, half_line, half_line),
    complete = TRUE)
}

plotExperiments <- function(data, outputPrefix, hasControl, hasTreatment) {
    treatmentData <- data.frame()
    if (hasTreatment) {
        treatmentData <- data[data$type == "treatment",]
    }
    controlData <- data.frame()
    if (hasControl) {
        controlData <- data[data$type == "control",]
    }

    experimentThroughputPlots(treatmentData, controlData, data, outputPrefix)
    experimentLatencyPlots(treatmentData, controlData, data, outputPrefix)
    experimentCpuUsagePlots(treatmentData, controlData, data, outputPrefix)
}