/// <reference types="node" />
interface RoundSample {
    name: string;
    type: string;
    peers: number;
    partitions: number;
    inputrate: number;
    throughput: number;
    latency: number;
    cpupercent: number;
    transactions: number;
    label: string;
}
interface Transaction {
    id: string;
    status: string;
    time_create: number;
    time_final: number;
    time_endorse: number;
    time_order: number;
    result: Buffer;
    verified: boolean;
    error_flags: number;
    error_messages: [];
}
export default class DataAggregator {
    private data;
    private amountOfRoundsPerExperiment;
    private amountOfPeers;
    private amountOfPartitions;
    private sampleSize;
    private roundPrefix;
    private hasControl;
    private hasTreatment;
    constructor(sampleSize: number, roundPrefix: string, amountOfRounds: number, hasControl: boolean, hasTreatment: boolean);
    aggregate(fromPath: string, amountOfPeers: number, amountOfPartitions: number): RoundSample[];
    roundTypeFileName(roundNumber: number, type: string): string;
    private aggregateControlSamples;
    private aggregateTreatmentSamples;
    private calculateThroughput;
    private calculateInputRate;
    private calculateMeanLatency;
    static successfulTransactions(transactions: Transaction[]): Transaction[];
    private calculateCpuUsage;
    private takeControlSample;
    private takeTreatmentSample;
    private calculateTreatmentInputRate;
    private calculateTreatmentThroughput;
    private appendCommonSampleProperties;
}
export {};
