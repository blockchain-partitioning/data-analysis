library("jsonlite")
library("ggplot2")
source("plots/plots.R")
source("statistics/statistics.R")

readAggregatedDataFromFile <- function(path){
    return(read_json(path, simplifyDataFrame = TRUE))
}

imageOutputFileExtention <- "pdf"
aggregateFilePath <- file.path("..", "resources", "aggregated-data.json")
maxAggregateFilePath <- file.path("..", "resources", "aggregated-max-data.json")

# Get aggreated data from a previously created file (much faster)
graphLabels <- c("2 (Control)", "2 (Treatment)", "4 (Control)", "4 (Treatment)", "8 (Control)", "8 (Treatment)")
data <- readAggregatedDataFromFile(aggregateFilePath)
colorsToPickFrom <- c("#332288",
"#88CCEE",
"#44AA99",
"#117733",
"#999933",
"#DDCC77")
plotExperiments(data, "equal", TRUE, TRUE)

#
maxData <- readAggregatedDataFromFile(maxAggregateFilePath)
graphLabels <- c("2", "4", "8")
colorsToPickFrom <- c("#88CCEE",
"#117733",
"#DDCC77")
plotExperiments(maxData, "max", FALSE, TRUE)


# statistics(data, TRUE, TRUE)
# statistics(maxData, FALSE, TRUE)