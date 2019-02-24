const Path = require('path');
const Fs = require('fs-extra');
const DataAggregator = require('./aggregates/aggregator').default;

const resourcePath = Path.join("C:", "Users", "rober", "Desktop", "experiments", "src", "resources", "comparison");

const peers = [4,8,16,32];
const partitions = [2,4,8];
const roundPath = Path.resolve(Path.join('..', 'resources', 'aggregated-data.json'));
let roundData = [];
let maxPath = Path.resolve(Path.join('..', 'resources', 'aggregated-max-data.json'));
let maxData = [];

const roundAggregator = new DataAggregator(10, "round", 8, true, true);
const maxAggregator = new DataAggregator(10, "max", 5, false, true);
peers.forEach(amountOfPeers => {
    partitions.forEach(amountOfPartitions => {
        if (amountOfPeers % amountOfPartitions === 0) {
            const path = Path.join(resourcePath, `${amountOfPeers}-peers`, `${amountOfPartitions}-partitions`);
            console.log(path);
            const roundAggregates = roundAggregator.aggregate(path, amountOfPeers, amountOfPartitions);
            roundData = roundData.concat(roundAggregates);
            const maxAggregates = maxAggregator.aggregate(path, amountOfPeers, amountOfPartitions);
            maxData = maxData.concat(maxAggregates);
        }
    })
});

Fs.outputJSONSync(roundPath, roundData);
Fs.outputJSONSync(maxPath, maxData);