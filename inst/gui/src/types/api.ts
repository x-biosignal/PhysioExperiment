// API Response types

export interface ApiResponse<T> {
  success: boolean
  data?: T
  error?: string
}

export interface DatasetInfo {
  id: string
  name: string
  nChannels: number
  nTimepoints: number
  samplingRate: number
  duration: number
  channels: string[]
  assays: string[]
  events?: EventData[]
  metadata?: Record<string, unknown>
}

export interface ChannelData {
  index: number
  label: string
  type: 'EEG' | 'EMG' | 'ECG' | 'EOG' | 'MISC'
  unit?: string
  reference?: string
  position?: { x: number; y: number; z?: number }
}

export interface EventData {
  onset: number
  duration: number
  type: string
  value?: number | string
}

export interface SignalData {
  signals: Record<string, number[]>
  time: number[]
  channels: string[]
  samplingRate: number
}

export interface SpectrogramData {
  power: number[][]
  frequencies: number[]
  times: number[]
  samplingRate: number
}

export interface ConnectivityData {
  matrix: number[][]
  method: string
  freqBand?: [number, number]
  channelNames: string[]
}

export interface StatisticsResult {
  statistic: number[]
  pvalue: number[]
  significant: boolean[]
  times?: number[]
  correction?: string
}

// Request parameter types

export interface ImportParams {
  path: string
  format?: 'edf' | 'bdf' | 'brainvision' | 'hdf5' | 'csv' | 'auto'
  channels?: number[]
}

export interface FilterParams {
  low?: number
  high?: number
  order?: number
  type?: 'butter' | 'fir'
  outputAssay?: string
}

export interface EpochParams {
  tmin: number
  tmax: number
  eventType?: string
  baseline?: [number, number]
}

export interface SpectrogramParams {
  channel: number
  windowSize: number
  overlap: number
  windowType?: 'hanning' | 'hamming' | 'blackman' | 'rectangular'
}

export interface ConnectivityParams {
  method: 'coherence' | 'plv' | 'pli' | 'wpli' | 'correlation'
  freqBand?: [number, number]
  channels?: number[]
}

export interface TTestParams {
  conditionA: string
  conditionB: string
  paired?: boolean
  correction?: 'bonferroni' | 'fdr' | 'none'
}

// EMG Processing types

export interface EMGEnvelopeParams {
  method?: 'lowpass' | 'rms' | 'hilbert'
  cutoffFreq?: number
  windowMs?: number
  outputAssay?: string
}

export interface EMGRMSParams {
  windowMs?: number
  overlap?: number
  channels?: number[]
  outputAssay?: string
}

export interface MVCNormalizeParams {
  mvc?: number[]
  mvcMethod?: 'peak' | 'mean' | 'percentile'
  percentile?: number
  channels?: number[]
  outputAssay?: string
}

export interface OnsetDetectionParams {
  method?: 'double_threshold' | 'single_threshold' | 'tkeo' | 'hodges'
  thresholdSd?: number
  minDurationMs?: number
  channel: number
}

export interface OnsetDetectionResult {
  onsets: number[]
  offsets: number[]
  durations: number[]
  onsetTimes: number[]
  offsetTimes: number[]
  method: string
  thresholdSd: number
}

export interface CoContractionParams {
  agonist: number
  antagonist: number
  method?: 'overlap' | 'ratio' | 'rudolph'
  windowMs?: number
}

export interface CoContractionResult {
  cci: number | number[]
  times?: number[]
  method: string
  windowMs?: number
}

export interface EMGFeaturesParams {
  windowMs?: number
  overlap?: number
  features?: string[]
  channels?: number[]
}

export interface EMGFeaturesResult {
  features: Record<string, number[][]>
  featureNames: string[]
  times: number[]
  channels: string[]
  windowMs: number
}

// Fatigue Analysis types

export interface FrequencyAnalysisParams {
  channel: number
  windowSize?: number
  overlap?: number
  freqRange?: [number, number]
}

export interface FrequencyResult {
  mdf?: number[]
  mnf?: number[]
  times: number[]
  samplingRate: number
  windowSize: number
}

export interface FatigueIndexParams {
  channel: number
  method?: 'mdf_slope' | 'mnf_slope' | 'spectral_ratio' | 'amplitude_ratio'
  windowSize?: number
  overlap?: number
}

export interface FatigueIndexResult {
  fatigueIndex: number
  slope: number
  intercept: number
  rSquared: number
  method: string
}

export interface FatigueAnalysisResult {
  mdf: number[][]
  mnf: number[][]
  fatigueIndices: number[]
  times: number[]
  channels: string[]
}

// Muscle Synergy types

export interface NMFSynergyParams {
  nSynergies?: number
  nReplicates?: number
  maxIterations?: number
  tolerance?: number
  normalizeMuscles?: boolean
}

export interface NMFSynergyResult {
  weights: number[][]        // W matrix (n_muscles x n_synergies)
  activations: number[][]    // H matrix (n_synergies x n_timepoints)
  vaf: number
  reconstruction: number[][]
  residuals: number[][]
  nSynergies: number
  muscleNames: string[]
}

export interface OptimalSynergyParams {
  vafThreshold?: number
  maxSynergies?: number
  nReplicates?: number
  method?: 'threshold' | 'elbow' | 'aic'
}

export interface OptimalSynergyResult {
  optimalN: number
  vafValues: number[]
  vafSd: number[]
  nSynergiesRange: number[]
  method: string
  threshold: number
}

export interface SynergySimilarityParams {
  id1: string
  id2: string
  metric?: 'correlation' | 'cosine' | 'dot_product'
  matchSynergies?: boolean
}

export interface SynergySimilarityResult {
  similarityMatrix: number[][]
  meanSimilarity: number
  matching?: number[]
  metric: string
}

export interface SynergySimilarityTestResult {
  observedSimilarity: number
  pValue: number
  permutationDistribution: number[]
  significant: boolean
}
