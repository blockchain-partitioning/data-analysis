"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const Path = require("path");
const Fs = require("fs-extra");
const _ = require("lodash");
const isDirectory = (source) => Fs.lstatSync(source).isDirectory();
const isFile = (source) => !isDirectory(source);
const getSubDirectories = (source) => Fs.readdirSync(source).map(name => Path.join(source, name)).filter(isDirectory);
const getFiles = (source) => Fs.readdirSync(source).map(name => Path.join(source, name)).filter(isFile);
class DataAggregator {
    constructor(sampleSize, roundPrefix, amountOfRounds, hasControl, hasTreatment) {
        this.data = [];
        this.amountOfRoundsPerExperiment = amountOfRounds;
        this.sampleSize = sampleSize || 10;
        this.roundPrefix = roundPrefix;
        this.hasControl = hasControl;
        this.hasTreatment = hasTreatment;
    }
    aggregate(fromPath, amountOfPeers, amountOfPartitions) {
        this.data = [];
        this.amountOfPeers = amountOfPeers;
        this.amountOfPartitions = amountOfPartitions;
        for (let roundNumber = 1; roundNumber <= this.amountOfRoundsPerExperiment; roundNumber++) {
            const resultsPath = Path.join(fromPath, "results");
            const transactionsSent = roundNumber * 32 * 60 * 3;
            const transactionRoundName = this.roundTypeFileName(roundNumber, "transactions");
            const resourcesRoundName = this.roundTypeFileName(roundNumber, "resources");
            if (this.hasControl) {
                this.aggregateControlSamples(resultsPath, transactionRoundName, resourcesRoundName, transactionsSent);
            }
            if (this.hasTreatment) {
                this.aggregateTreatmentSamples(resultsPath, transactionRoundName, resourcesRoundName, transactionsSent);
            }
        }
        return this.data;
    }
    roundTypeFileName(roundNumber, type) {
        return `${this.roundPrefix}-${roundNumber}-${type}`;
    }
    aggregateControlSamples(resultsPath, transactionRoundName, resourcesRoundName, transactionsSent) {
        const path = Path.join(resultsPath, "singular");
        const filePaths = getFiles(path);
        const transactionPaths = filePaths.filter(path => {
            return path.indexOf(transactionRoundName) > -1;
        });
        const resourcePaths = filePaths.filter(path => {
            return path.indexOf(resourcesRoundName) > -1;
        });
        for (let i = 0; i < this.sampleSize; i++) {
            this.data.push(this.takeControlSample(transactionPaths[i], resourcePaths[i], transactionsSent));
        }
    }
    aggregateTreatmentSamples(resultsPath, transactionRoundName, resourcesRoundName, transactionsSent) {
        const treePath = Path.join(resultsPath, "tree");
        for (let sampleNumber = 0; sampleNumber < this.sampleSize; sampleNumber++) {
            this.data.push(this.takeTreatmentSample(treePath, transactionRoundName, resourcesRoundName, transactionsSent, sampleNumber));
        }
    }
    calculateThroughput(transactions) {
        const firstCreatedTransaction = _.minBy(transactions, "time_create");
        const lastFinishedTransaction = _.maxBy(transactions, "time_final");
        const totalDurationInMilliseconds = lastFinishedTransaction.time_final - firstCreatedTransaction.time_create;
        const totalDurationInSeconds = totalDurationInMilliseconds / 1000;
        return transactions.length / totalDurationInSeconds;
    }
    calculateInputRate(transactions) {
        const firstCreatedTransaction = _.minBy(transactions, "time_create");
        const lastCreatedTransaction = _.maxBy(transactions, "time_create");
        const totalDurationInMilliseconds = lastCreatedTransaction.time_create - firstCreatedTransaction.time_create;
        const totalDurationInSeconds = totalDurationInMilliseconds / 1000;
        return transactions.length / totalDurationInSeconds;
    }
    calculateMeanLatency(transactions) {
        const latencies = transactions.map((transaction) => {
            return transaction.time_final - transaction.time_create;
        });
        return _.mean(latencies);
    }
    static successfulTransactions(transactions) {
        return transactions.filter((transaction) => {
            if (transaction.status === "success") {
                return transaction;
            }
        });
    }
    calculateCpuUsage(resources) {
        const peers = resources.peers.filter(peer => {
            return (peer.info.NAME.indexOf('chaincode') < 0 && peer.info.NAME.indexOf('peer') > -1);
        });
        const keys = peers.map(peer => {
            return peer.key;
        });
        const means = [];
        keys.forEach(key => {
            const resource = resources.statistics[key];
            means.push(_.mean(resource.cpu_percent));
        });
        return _.mean(means);
    }
    takeControlSample(transactionsFilePath, resourcesFilePath, transactionsSent) {
        let sample = {};
        sample = this.appendCommonSampleProperties(sample);
        sample.transactions = transactionsSent;
        sample.type = "control";
        console.log(transactionsFilePath);
        console.log(resourcesFilePath);
        const transactions = Fs.readJsonSync(transactionsFilePath);
        const successfulTransactions = DataAggregator.successfulTransactions(transactions);
        if (successfulTransactions != undefined && successfulTransactions.length > 0) {
            sample.throughput = this.calculateThroughput(successfulTransactions);
            sample.latency = this.calculateMeanLatency(successfulTransactions);
        }
        else {
            sample.throughput = 0;
            sample.latency = 0;
        }
        sample.inputrate = this.calculateInputRate(transactions);
        const resources = Fs.readJsonSync(resourcesFilePath);
        sample.cpupercent = this.calculateCpuUsage(resources);
        return sample;
    }
    takeTreatmentSample(treePath, transactionRoundName, resourcesRoundName, transactionsSent, sampleNumber) {
        console.log("Sample:", sampleNumber);
        console.log(transactionRoundName);
        console.log(resourcesRoundName);
        let sample = {};
        sample = this.appendCommonSampleProperties(sample);
        sample.transactions = transactionsSent;
        sample.type = "treatment";
        const leafDirectories = getSubDirectories(treePath).filter(path => {
            return path.indexOf("root") === -1;
        });
        let amountOfTransactions = 0;
        let amountOfSuccessfulTransactions = 0;
        const firstCreatedTransactions = [];
        const firstCreatedSuccessfulTransactions = [];
        const lastFinishedSuccessfulTransactions = [];
        const lastCreatedTransactions = [];
        const meanLatencies = [];
        const meanCpuPercentages = [];
        for (let j = 0; j < leafDirectories.length; j++) {
            const leafPath = leafDirectories[j];
            const filePaths = getFiles(leafPath);
            const transactionPaths = filePaths.filter(path => {
                return path.indexOf(transactionRoundName) > -1;
            });
            const resourcePaths = filePaths.filter(path => {
                return path.indexOf(resourcesRoundName) > -1;
            });
            let transactions = Fs.readJsonSync(transactionPaths[sampleNumber]);
            amountOfTransactions += transactions.length;
            const resources = Fs.readJsonSync(resourcePaths[sampleNumber]);
            const successfulTransactions = DataAggregator.successfulTransactions(transactions);
            amountOfSuccessfulTransactions += successfulTransactions.length;
            firstCreatedTransactions.push(_.minBy(transactions, "time_create"));
            const firstCreatedTransaction = _.minBy(successfulTransactions, "time_create");
            if (firstCreatedTransaction) {
                firstCreatedSuccessfulTransactions.push(firstCreatedTransaction);
            }
            const lastFinishedSuccessfulTransaction = _.maxBy(successfulTransactions, "time_final");
            if (lastFinishedSuccessfulTransaction) {
                lastFinishedSuccessfulTransactions.push(lastFinishedSuccessfulTransaction);
            }
            lastCreatedTransactions.push(_.maxBy(transactions, "time_create"));
            meanLatencies.push(this.calculateMeanLatency(successfulTransactions));
            meanCpuPercentages.push(this.calculateCpuUsage(resources));
        }
        const firstCreatedTransaction = _.minBy(firstCreatedTransactions, "time_create");
        sample.throughput = this.calculateTreatmentThroughput(firstCreatedTransaction, lastFinishedSuccessfulTransactions, amountOfSuccessfulTransactions);
        sample.inputrate = this.calculateTreatmentInputRate(lastCreatedTransactions, firstCreatedTransaction, amountOfTransactions);
        sample.latency = _.mean(meanLatencies);
        sample.cpupercent = _.mean(meanCpuPercentages);
        return sample;
    }
    calculateTreatmentInputRate(lastCreatedTransactions, firstCreatedTransaction, amountOfTransactions) {
        const lastCreatedTransaction = _.maxBy(lastCreatedTransactions, "time_create");
        const totalCreationDurationInMilliseconds = lastCreatedTransaction.time_create - firstCreatedTransaction.time_create;
        const totalCreationDurationInSeconds = totalCreationDurationInMilliseconds / 1000;
        return amountOfTransactions / totalCreationDurationInSeconds;
    }
    calculateTreatmentThroughput(firstCreatedTransaction, lastFinishedTransactions, amountOfSuccessfulTransactions) {
        if (lastFinishedTransactions.length > 0) {
            const lastFinishedTransaction = _.maxBy(lastFinishedTransactions, "time_final");
            const totalDurationInMilliseconds = lastFinishedTransaction.time_final - firstCreatedTransaction.time_create;
            const totalDurationInSeconds = totalDurationInMilliseconds / 1000;
            return amountOfSuccessfulTransactions / totalDurationInSeconds;
        }
        else {
            return 0;
        }
    }
    appendCommonSampleProperties(sample) {
        sample.name = `${this.amountOfPartitions}-partitions`;
        sample.peers = this.amountOfPeers;
        sample.partitions = this.amountOfPartitions;
        sample.label = `${this.amountOfPeers}/${this.amountOfPartitions}`;
        return sample;
    }
}
exports.default = DataAggregator;
//# sourceMappingURL=aggregator.js.map