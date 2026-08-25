// Workflow definitions for PhysioExperiment Analysis System
// Inspired by Biojupy with advanced dynamic report generation

export type DatasetType = 'EEG' | 'EMG' | 'ECG' | 'EOG' | 'GAIT' | 'MOTION' | 'GENERIC'

export type AnalysisCategory =
  | 'preprocessing'
  | 'timefreq'
  | 'erp'
  | 'connectivity'
  | 'network'
  | 'statistics'
  | 'advanced'
  | 'dimred'
  | 'biomech'
  | 'emg'
  | 'fatigue'
  | 'synergy'
  | 'pattern'

export interface AnalysisMethodParam {
  name: string
  type: 'number' | 'select' | 'boolean' | 'range'
  label: string
  default: number | string | boolean | [number, number]
  options?: Array<{ value: string | number; label: string }>
  min?: number
  max?: number
  step?: number
  unit?: string
  description?: string
}

export interface AnalysisMethod {
  id: string
  name: string
  nameJa: string
  category: AnalysisCategory
  description: string
  descriptionJa: string
  icon: string
  params: AnalysisMethodParam[]
  outputType: 'plot' | 'table' | 'metric' | 'matrix' | 'timeseries'
  estimatedTime: 'fast' | 'medium' | 'slow'
  dependencies?: string[] // Other method IDs that must run first
}

export interface WorkflowStep {
  methodId: string
  required: boolean
  order: number
  defaultParams?: Record<string, unknown>
}

export interface Workflow {
  id: string
  name: string
  nameJa: string
  description: string
  descriptionJa: string
  datasetTypes: DatasetType[]
  icon: string
  color: string
  steps: WorkflowStep[]
  reportSections: ReportSection[]
}

export interface ReportSection {
  id: string
  title: string
  titleJa: string
  methodIds: string[]
  layout: 'full' | 'half' | 'third' | 'grid'
}

// ============ Analysis Methods Library ============

export const analysisMethods: Record<string, AnalysisMethod> = {
  // Preprocessing
  bandpass: {
    id: 'bandpass',
    name: 'Bandpass Filter',
    nameJa: 'バンドパスフィルタ',
    category: 'preprocessing',
    description: 'Apply bandpass filter to remove unwanted frequencies',
    descriptionJa: '不要な周波数成分を除去するバンドパスフィルタ',
    icon: 'filter',
    params: [
      { name: 'lowFreq', type: 'number', label: '低域カットオフ', default: 1, min: 0.1, max: 100, step: 0.1, unit: 'Hz' },
      { name: 'highFreq', type: 'number', label: '高域カットオフ', default: 40, min: 1, max: 500, step: 1, unit: 'Hz' }
    ],
    outputType: 'timeseries',
    estimatedTime: 'fast'
  },
  notch: {
    id: 'notch',
    name: 'Notch Filter',
    nameJa: 'ノッチフィルタ',
    category: 'preprocessing',
    description: 'Remove powerline noise (50/60 Hz)',
    descriptionJa: '商用電源ノイズ (50/60 Hz) を除去',
    icon: 'zap-off',
    params: [
      { name: 'frequency', type: 'select', label: '周波数', default: 50, options: [
        { value: 50, label: '50 Hz (日本/欧州)' },
        { value: 60, label: '60 Hz (北米)' }
      ]}
    ],
    outputType: 'timeseries',
    estimatedTime: 'fast'
  },
  zscore: {
    id: 'zscore',
    name: 'Z-Score Normalization',
    nameJa: 'Z正規化',
    category: 'preprocessing',
    description: 'Normalize signal to zero mean and unit variance',
    descriptionJa: '平均0、分散1に正規化',
    icon: 'percent',
    params: [],
    outputType: 'timeseries',
    estimatedTime: 'fast'
  },

  // Time-Frequency
  spectrogram: {
    id: 'spectrogram',
    name: 'Spectrogram (STFT)',
    nameJa: 'スペクトログラム',
    category: 'timefreq',
    description: 'Time-frequency representation using Short-Time Fourier Transform',
    descriptionJa: '短時間フーリエ変換による時間周波数表現',
    icon: 'activity',
    params: [
      { name: 'windowSize', type: 'number', label: '窓サイズ', default: 256, min: 64, max: 1024, step: 64 },
      { name: 'hopSize', type: 'number', label: 'ホップサイズ', default: 32, min: 8, max: 256, step: 8 }
    ],
    outputType: 'matrix',
    estimatedTime: 'medium'
  },
  wavelet: {
    id: 'wavelet',
    name: 'Wavelet Transform',
    nameJa: 'ウェーブレット変換',
    category: 'timefreq',
    description: 'Continuous wavelet transform using Morlet wavelets',
    descriptionJa: 'Morletウェーブレットによる連続ウェーブレット変換',
    icon: 'waves',
    params: [
      { name: 'freqMin', type: 'number', label: '最小周波数', default: 1, min: 0.5, max: 50, unit: 'Hz' },
      { name: 'freqMax', type: 'number', label: '最大周波数', default: 60, min: 10, max: 200, unit: 'Hz' },
      { name: 'nCycles', type: 'number', label: 'サイクル数', default: 7, min: 3, max: 15 }
    ],
    outputType: 'matrix',
    estimatedTime: 'slow'
  },
  psd: {
    id: 'psd',
    name: 'Power Spectral Density',
    nameJa: 'パワースペクトル密度',
    category: 'timefreq',
    description: 'Compute power spectral density',
    descriptionJa: 'パワースペクトル密度を計算',
    icon: 'bar-chart-2',
    params: [
      { name: 'windowSize', type: 'number', label: '窓サイズ', default: 256, min: 64, max: 1024 }
    ],
    outputType: 'plot',
    estimatedTime: 'fast'
  },
  bandpower: {
    id: 'bandpower',
    name: 'Band Power Analysis',
    nameJa: 'バンドパワー解析',
    category: 'timefreq',
    description: 'Extract power in standard frequency bands (delta, theta, alpha, beta, gamma)',
    descriptionJa: '標準周波数帯域のパワーを抽出',
    icon: 'bar-chart',
    params: [],
    outputType: 'table',
    estimatedTime: 'fast'
  },

  // ERP Analysis
  epochExtraction: {
    id: 'epochExtraction',
    name: 'Epoch Extraction',
    nameJa: 'エポック抽出',
    category: 'erp',
    description: 'Extract time-locked epochs around events',
    descriptionJa: 'イベント周辺の時間ロックエポックを抽出',
    icon: 'scissors',
    params: [
      { name: 'preTime', type: 'number', label: '開始時間', default: 0.2, min: 0, max: 2, step: 0.1, unit: 's' },
      { name: 'postTime', type: 'number', label: '終了時間', default: 0.8, min: 0.1, max: 5, step: 0.1, unit: 's' }
    ],
    outputType: 'timeseries',
    estimatedTime: 'fast'
  },
  erpComputation: {
    id: 'erpComputation',
    name: 'ERP Computation',
    nameJa: 'ERP計算',
    category: 'erp',
    description: 'Compute event-related potentials by averaging epochs',
    descriptionJa: 'エポック平均による事象関連電位の計算',
    icon: 'git-merge',
    params: [
      { name: 'baselineCorrect', type: 'boolean', label: 'ベースライン補正', default: true }
    ],
    outputType: 'plot',
    estimatedTime: 'fast',
    dependencies: ['epochExtraction']
  },

  // Connectivity
  plv: {
    id: 'plv',
    name: 'Phase Locking Value (PLV)',
    nameJa: '位相同期値 (PLV)',
    category: 'connectivity',
    description: 'Measure phase synchronization between channels',
    descriptionJa: 'チャンネル間の位相同期を測定',
    icon: 'link',
    params: [
      { name: 'lowFreq', type: 'number', label: '低域', default: 8, unit: 'Hz' },
      { name: 'highFreq', type: 'number', label: '高域', default: 13, unit: 'Hz' }
    ],
    outputType: 'matrix',
    estimatedTime: 'medium'
  },
  pli: {
    id: 'pli',
    name: 'Phase Lag Index (PLI)',
    nameJa: '位相遅れ指標 (PLI)',
    category: 'connectivity',
    description: 'Volume conduction-robust phase synchronization measure',
    descriptionJa: 'ボリューム伝導に頑健な位相同期指標',
    icon: 'link-2',
    params: [
      { name: 'lowFreq', type: 'number', label: '低域', default: 8, unit: 'Hz' },
      { name: 'highFreq', type: 'number', label: '高域', default: 13, unit: 'Hz' }
    ],
    outputType: 'matrix',
    estimatedTime: 'medium'
  },
  coherence: {
    id: 'coherence',
    name: 'Coherence',
    nameJa: 'コヒーレンス',
    category: 'connectivity',
    description: 'Magnitude-squared coherence between channels',
    descriptionJa: 'チャンネル間のコヒーレンス',
    icon: 'radio',
    params: [
      { name: 'lowFreq', type: 'number', label: '低域', default: 8, unit: 'Hz' },
      { name: 'highFreq', type: 'number', label: '高域', default: 13, unit: 'Hz' }
    ],
    outputType: 'matrix',
    estimatedTime: 'medium'
  },
  correlation: {
    id: 'correlation',
    name: 'Correlation Matrix',
    nameJa: '相関行列',
    category: 'connectivity',
    description: 'Pearson correlation between channels',
    descriptionJa: 'チャンネル間のピアソン相関',
    icon: 'grid',
    params: [],
    outputType: 'matrix',
    estimatedTime: 'fast'
  },

  // Network Analysis
  networkMetrics: {
    id: 'networkMetrics',
    name: 'Network Metrics',
    nameJa: 'ネットワーク指標',
    category: 'network',
    description: 'Compute graph-theoretic network metrics',
    descriptionJa: 'グラフ理論に基づくネットワーク指標',
    icon: 'share-2',
    params: [
      { name: 'threshold', type: 'number', label: '閾値', default: 0.5, min: 0, max: 1, step: 0.1 }
    ],
    outputType: 'table',
    estimatedTime: 'medium',
    dependencies: ['plv']
  },
  networkVisualization: {
    id: 'networkVisualization',
    name: 'Network Visualization',
    nameJa: 'ネットワーク可視化',
    category: 'network',
    description: 'Visualize brain network as graph',
    descriptionJa: '脳ネットワークのグラフ可視化',
    icon: 'git-branch',
    params: [],
    outputType: 'plot',
    estimatedTime: 'fast',
    dependencies: ['networkMetrics']
  },

  // Statistics
  ttest: {
    id: 'ttest',
    name: 'Pointwise t-test',
    nameJa: '点ごとt検定',
    category: 'statistics',
    description: 'Compare two conditions with pointwise t-test',
    descriptionJa: '2条件の点ごとt検定',
    icon: 'divide',
    params: [
      { name: 'alpha', type: 'number', label: '有意水準', default: 0.05, min: 0.001, max: 0.1, step: 0.01 },
      { name: 'fdrCorrection', type: 'boolean', label: 'FDR補正', default: true }
    ],
    outputType: 'plot',
    estimatedTime: 'fast',
    dependencies: ['erpComputation']
  },
  anova: {
    id: 'anova',
    name: 'One-way ANOVA',
    nameJa: '一元配置分散分析',
    category: 'statistics',
    description: 'Compare multiple groups with ANOVA',
    descriptionJa: '複数群の分散分析',
    icon: 'columns',
    params: [],
    outputType: 'table',
    estimatedTime: 'fast',
    dependencies: ['epochExtraction']
  },
  effectSize: {
    id: 'effectSize',
    name: "Cohen's d Effect Size",
    nameJa: '効果量 (Cohen\'s d)',
    category: 'statistics',
    description: 'Compute standardized effect size',
    descriptionJa: '標準化効果量を計算',
    icon: 'trending-up',
    params: [],
    outputType: 'metric',
    estimatedTime: 'fast',
    dependencies: ['epochExtraction']
  },
  clusterPermutation: {
    id: 'clusterPermutation',
    name: 'Cluster Permutation Test',
    nameJa: 'クラスター置換検定',
    category: 'statistics',
    description: 'Multiple comparison correction using cluster-based permutation',
    descriptionJa: 'クラスターベースの多重比較補正',
    icon: 'shuffle',
    params: [
      { name: 'nPermutations', type: 'number', label: '置換回数', default: 1000, min: 100, max: 10000 }
    ],
    outputType: 'plot',
    estimatedTime: 'slow',
    dependencies: ['epochExtraction']
  },

  // Advanced
  dtw: {
    id: 'dtw',
    name: 'Dynamic Time Warping',
    nameJa: '動的時間伸縮',
    category: 'advanced',
    description: 'Compute similarity between signals with DTW',
    descriptionJa: '動的時間伸縮による信号類似度',
    icon: 'git-pull-request',
    params: [],
    outputType: 'plot',
    estimatedTime: 'medium'
  },

  // Dimensionality Reduction
  pca: {
    id: 'pca',
    name: 'Principal Component Analysis',
    nameJa: '主成分分析',
    category: 'dimred',
    description: 'Reduce dimensionality with PCA',
    descriptionJa: 'PCAによる次元削減',
    icon: 'minimize-2',
    params: [
      { name: 'nComponents', type: 'number', label: '成分数', default: 3, min: 2, max: 10 }
    ],
    outputType: 'plot',
    estimatedTime: 'medium'
  },

  // Biomechanics
  cycleNormalization: {
    id: 'cycleNormalization',
    name: 'Cycle Normalization',
    nameJa: 'サイクル正規化',
    category: 'biomech',
    description: 'Normalize movement to 0-100% cycle',
    descriptionJa: '動作を0-100%サイクルに正規化',
    icon: 'repeat',
    params: [
      { name: 'targetLength', type: 'number', label: 'ポイント数', default: 101, min: 51, max: 201 }
    ],
    outputType: 'plot',
    estimatedTime: 'fast'
  },
  phaseSegmentation: {
    id: 'phaseSegmentation',
    name: 'Phase Segmentation',
    nameJa: 'フェーズ分割',
    category: 'biomech',
    description: 'Segment movement into phases',
    descriptionJa: '動作をフェーズに分割',
    icon: 'layers',
    params: [],
    outputType: 'plot',
    estimatedTime: 'fast'
  },
  symmetryAnalysis: {
    id: 'symmetryAnalysis',
    name: 'Symmetry Analysis',
    nameJa: '対称性解析',
    category: 'biomech',
    description: 'Analyze bilateral symmetry',
    descriptionJa: '左右対称性を解析',
    icon: 'copy',
    params: [],
    outputType: 'table',
    estimatedTime: 'fast'
  },

  // ============ EMG-Specific Methods ============

  // EMG Preprocessing
  emgBandpass: {
    id: 'emgBandpass',
    name: 'EMG Bandpass Filter',
    nameJa: 'EMGバンドパスフィルタ',
    category: 'emg',
    description: 'Standard EMG bandpass filter (20-500 Hz)',
    descriptionJa: '標準EMGバンドパスフィルタ（20-500 Hz）',
    icon: 'filter',
    params: [
      { name: 'lowFreq', type: 'number', label: '低域カットオフ', default: 20, min: 10, max: 50, step: 5, unit: 'Hz', description: '20 Hz推奨（動作アーチファクト除去）' },
      { name: 'highFreq', type: 'number', label: '高域カットオフ', default: 500, min: 200, max: 1000, step: 50, unit: 'Hz', description: '450-500 Hz推奨' },
      { name: 'filterOrder', type: 'number', label: 'フィルタ次数', default: 4, min: 2, max: 8, step: 2 }
    ],
    outputType: 'timeseries',
    estimatedTime: 'fast'
  },

  rectification: {
    id: 'rectification',
    name: 'Full-Wave Rectification',
    nameJa: '全波整流',
    category: 'emg',
    description: 'Convert EMG signal to absolute values',
    descriptionJa: 'EMG信号を絶対値に変換',
    icon: 'bar-chart',
    params: [],
    outputType: 'timeseries',
    estimatedTime: 'fast',
    dependencies: ['emgBandpass']
  },

  emgEnvelope: {
    id: 'emgEnvelope',
    name: 'EMG Envelope Detection',
    nameJa: 'EMGエンベロープ検出',
    category: 'emg',
    description: 'Extract muscle activation envelope using low-pass filter',
    descriptionJa: '低域通過フィルタによる筋活動エンベロープ抽出',
    icon: 'trending-up',
    params: [
      { name: 'cutoffFreq', type: 'number', label: 'カットオフ周波数', default: 6, min: 2, max: 20, step: 1, unit: 'Hz', description: '歩行:6 Hz、速い動作:10-20 Hz' },
      { name: 'method', type: 'select', label: '方法', default: 'lowpass', options: [
        { value: 'lowpass', label: '低域通過フィルタ' },
        { value: 'rms', label: 'RMS (移動平均)' },
        { value: 'hilbert', label: 'ヒルベルト変換' }
      ]}
    ],
    outputType: 'timeseries',
    estimatedTime: 'fast',
    dependencies: ['rectification']
  },

  rmsCalculation: {
    id: 'rmsCalculation',
    name: 'RMS Amplitude',
    nameJa: 'RMS振幅',
    category: 'emg',
    description: 'Calculate root mean square amplitude',
    descriptionJa: '二乗平均平方根（RMS）振幅を計算',
    icon: 'activity',
    params: [
      { name: 'windowSize', type: 'number', label: '窓サイズ', default: 100, min: 20, max: 500, step: 10, unit: 'ms', description: '静的:250-500ms、動的:50-100ms' }
    ],
    outputType: 'timeseries',
    estimatedTime: 'fast'
  },

  mavCalculation: {
    id: 'mavCalculation',
    name: 'Mean Absolute Value',
    nameJa: '平均絶対値 (MAV)',
    category: 'emg',
    description: 'Calculate mean absolute value for muscle activation level',
    descriptionJa: '筋活動レベルの平均絶対値を計算',
    icon: 'trending-up',
    params: [
      { name: 'windowSize', type: 'number', label: '窓サイズ', default: 100, min: 20, max: 500, step: 10, unit: 'ms' }
    ],
    outputType: 'timeseries',
    estimatedTime: 'fast'
  },

  mvcNormalization: {
    id: 'mvcNormalization',
    name: 'MVC Normalization',
    nameJa: 'MVC正規化',
    category: 'emg',
    description: 'Normalize EMG to maximum voluntary contraction',
    descriptionJa: '最大随意収縮（MVC）に対する正規化',
    icon: 'percent',
    params: [
      { name: 'mvcWindow', type: 'number', label: 'MVC窓', default: 500, min: 250, max: 1000, unit: 'ms', description: 'MVCピーク検出用窓サイズ' },
      { name: 'mvcMethod', type: 'select', label: 'MVC算出法', default: 'peak', options: [
        { value: 'peak', label: 'ピーク値' },
        { value: 'mean', label: '平均値' },
        { value: 'median', label: '中央値' }
      ]}
    ],
    outputType: 'timeseries',
    estimatedTime: 'fast',
    dependencies: ['emgEnvelope']
  },

  // EMG Onset/Timing Analysis
  onsetDetection: {
    id: 'onsetDetection',
    name: 'Muscle Onset Detection',
    nameJa: '筋活動開始検出',
    category: 'emg',
    description: 'Detect muscle activation onset and offset timing',
    descriptionJa: '筋活動の開始・終了タイミングを検出',
    icon: 'clock',
    params: [
      { name: 'method', type: 'select', label: '検出方法', default: 'double_threshold', options: [
        { value: 'single_threshold', label: 'シングル閾値法' },
        { value: 'double_threshold', label: 'ダブル閾値法' },
        { value: 'adaptive', label: '適応閾値法' },
        { value: 'tkeo', label: 'TKEO法' }
      ]},
      { name: 'threshold', type: 'number', label: '閾値 (SD)', default: 3, min: 1, max: 10, step: 0.5, description: 'ベースラインSDの倍数' },
      { name: 'minDuration', type: 'number', label: '最小持続時間', default: 50, min: 10, max: 200, unit: 'ms' }
    ],
    outputType: 'table',
    estimatedTime: 'fast',
    dependencies: ['emgEnvelope']
  },

  activationTiming: {
    id: 'activationTiming',
    name: 'Activation Timing Analysis',
    nameJa: '筋活動タイミング解析',
    category: 'emg',
    description: 'Analyze muscle activation timing relative to movement phases',
    descriptionJa: '動作フェーズに対する筋活動タイミングの解析',
    icon: 'git-commit',
    params: [
      { name: 'referenceEvent', type: 'select', label: '基準イベント', default: 'heel_strike', options: [
        { value: 'heel_strike', label: '踵接地' },
        { value: 'toe_off', label: '足趾離地' },
        { value: 'movement_onset', label: '動作開始' },
        { value: 'peak_force', label: 'ピーク力' }
      ]}
    ],
    outputType: 'table',
    estimatedTime: 'fast',
    dependencies: ['onsetDetection']
  },

  coContractionIndex: {
    id: 'coContractionIndex',
    name: 'Co-Contraction Index',
    nameJa: '共収縮指数',
    category: 'emg',
    description: 'Calculate antagonist-agonist co-contraction index',
    descriptionJa: '拮抗筋-主動筋の共収縮指数を計算',
    icon: 'git-merge',
    params: [
      { name: 'method', type: 'select', label: '計算方法', default: 'ratio', options: [
        { value: 'ratio', label: '拮抗筋/主動筋比' },
        { value: 'overlap', label: 'オーバーラップ法' },
        { value: 'wasted', label: 'Wasted Contraction法' }
      ]},
      { name: 'normalize', type: 'boolean', label: 'MVC正規化', default: true }
    ],
    outputType: 'metric',
    estimatedTime: 'fast',
    dependencies: ['mvcNormalization']
  },

  // EMG Fatigue Analysis
  medianFrequency: {
    id: 'medianFrequency',
    name: 'Median Frequency (MDF)',
    nameJa: '中央周波数',
    category: 'fatigue',
    description: 'Calculate median frequency for fatigue assessment',
    descriptionJa: '疲労評価のための中央周波数を計算',
    icon: 'bar-chart-2',
    params: [
      { name: 'windowSize', type: 'number', label: '窓サイズ', default: 512, min: 256, max: 2048 },
      { name: 'overlap', type: 'number', label: 'オーバーラップ', default: 50, min: 0, max: 90, unit: '%' }
    ],
    outputType: 'timeseries',
    estimatedTime: 'medium'
  },

  meanFrequency: {
    id: 'meanFrequency',
    name: 'Mean Frequency (MNF)',
    nameJa: '平均周波数',
    category: 'fatigue',
    description: 'Calculate mean frequency for fatigue assessment',
    descriptionJa: '疲労評価のための平均周波数を計算',
    icon: 'bar-chart-2',
    params: [
      { name: 'windowSize', type: 'number', label: '窓サイズ', default: 512, min: 256, max: 2048 },
      { name: 'overlap', type: 'number', label: 'オーバーラップ', default: 50, min: 0, max: 90, unit: '%' }
    ],
    outputType: 'timeseries',
    estimatedTime: 'medium'
  },

  fatigueIndex: {
    id: 'fatigueIndex',
    name: 'Fatigue Index',
    nameJa: '疲労指数',
    category: 'fatigue',
    description: 'Calculate fatigue index from frequency spectrum shift',
    descriptionJa: '周波数スペクトルシフトから疲労指数を計算',
    icon: 'trending-down',
    params: [
      { name: 'method', type: 'select', label: '方法', default: 'mdf_slope', options: [
        { value: 'mdf_slope', label: 'MDF傾き' },
        { value: 'mnf_slope', label: 'MNF傾き' },
        { value: 'spectral_ratio', label: '低/高周波比' }
      ]},
      { name: 'lowBand', type: 'range', label: '低周波帯', default: [15, 45] as [number, number], unit: 'Hz' },
      { name: 'highBand', type: 'range', label: '高周波帯', default: [95, 200] as [number, number], unit: 'Hz' }
    ],
    outputType: 'metric',
    estimatedTime: 'medium',
    dependencies: ['medianFrequency', 'meanFrequency']
  },

  spectralCompression: {
    id: 'spectralCompression',
    name: 'Spectral Compression',
    nameJa: 'スペクトル圧縮',
    category: 'fatigue',
    description: 'Analyze spectral compression during sustained contraction',
    descriptionJa: '持続収縮中のスペクトル圧縮を解析',
    icon: 'minimize-2',
    params: [],
    outputType: 'plot',
    estimatedTime: 'medium',
    dependencies: ['medianFrequency']
  },

  // EMG Time-Frequency Analysis
  emgSpectrogram: {
    id: 'emgSpectrogram',
    name: 'EMG Spectrogram',
    nameJa: 'EMGスペクトログラム',
    category: 'timefreq',
    description: 'Time-frequency representation of EMG signal',
    descriptionJa: 'EMG信号の時間-周波数表現',
    icon: 'grid',
    params: [
      { name: 'windowSize', type: 'number', label: '窓サイズ', default: 256, min: 64, max: 1024 },
      { name: 'freqMax', type: 'number', label: '最大周波数', default: 500, min: 200, max: 1000, unit: 'Hz' },
      { name: 'colormap', type: 'select', label: 'カラーマップ', default: 'hot', options: [
        { value: 'hot', label: 'Hot' },
        { value: 'jet', label: 'Jet' },
        { value: 'viridis', label: 'Viridis' }
      ]}
    ],
    outputType: 'matrix',
    estimatedTime: 'medium'
  },

  emgWavelet: {
    id: 'emgWavelet',
    name: 'EMG Wavelet Analysis',
    nameJa: 'EMGウェーブレット解析',
    category: 'timefreq',
    description: 'Continuous wavelet transform for EMG analysis',
    descriptionJa: 'EMG解析用連続ウェーブレット変換',
    icon: 'waves',
    params: [
      { name: 'waveletType', type: 'select', label: 'ウェーブレット', default: 'mexh', options: [
        { value: 'mexh', label: 'Mexican Hat' },
        { value: 'morl', label: 'Morlet' },
        { value: 'db4', label: 'Daubechies 4' }
      ]},
      { name: 'freqMin', type: 'number', label: '最小周波数', default: 10, unit: 'Hz' },
      { name: 'freqMax', type: 'number', label: '最大周波数', default: 500, unit: 'Hz' }
    ],
    outputType: 'matrix',
    estimatedTime: 'slow'
  },

  // Muscle Synergy Analysis
  muscleSynergy: {
    id: 'muscleSynergy',
    name: 'Muscle Synergy (NMF)',
    nameJa: '筋シナジー (NMF)',
    category: 'synergy',
    description: 'Extract muscle synergies using non-negative matrix factorization',
    descriptionJa: '非負値行列因子分解による筋シナジー抽出',
    icon: 'layers',
    params: [
      { name: 'nSynergies', type: 'number', label: 'シナジー数', default: 4, min: 2, max: 10, description: 'VAF>90%を基準に選択' },
      { name: 'nReplicates', type: 'number', label: '反復回数', default: 50, min: 10, max: 100 },
      { name: 'maxIterations', type: 'number', label: '最大イテレーション', default: 1000, min: 100, max: 10000 },
      { name: 'convergenceTol', type: 'number', label: '収束閾値', default: 0.0001, min: 0.000001, max: 0.01 }
    ],
    outputType: 'plot',
    estimatedTime: 'slow',
    dependencies: ['mvcNormalization', 'cycleNormalization']
  },

  synergyVAF: {
    id: 'synergyVAF',
    name: 'Synergy VAF Analysis',
    nameJa: 'シナジーVAF解析',
    category: 'synergy',
    description: 'Determine optimal number of synergies using variance accounted for',
    descriptionJa: '説明分散比によるシナジー数の決定',
    icon: 'pie-chart',
    params: [
      { name: 'vafThreshold', type: 'number', label: 'VAF閾値', default: 90, min: 80, max: 99, unit: '%' },
      { name: 'maxSynergies', type: 'number', label: '最大シナジー数', default: 8, min: 4, max: 12 }
    ],
    outputType: 'plot',
    estimatedTime: 'slow'
  },

  synergySimilarity: {
    id: 'synergySimilarity',
    name: 'Synergy Similarity',
    nameJa: 'シナジー類似度',
    category: 'synergy',
    description: 'Compare synergy patterns between conditions or groups',
    descriptionJa: '条件間・群間のシナジーパターン比較',
    icon: 'git-compare',
    params: [
      { name: 'metric', type: 'select', label: '類似度指標', default: 'correlation', options: [
        { value: 'correlation', label: '相関係数' },
        { value: 'cosine', label: 'コサイン類似度' },
        { value: 'dot_product', label: '内積' }
      ]}
    ],
    outputType: 'matrix',
    estimatedTime: 'medium',
    dependencies: ['muscleSynergy']
  },

  // Motor Unit Analysis
  motorUnitDecomposition: {
    id: 'motorUnitDecomposition',
    name: 'Motor Unit Decomposition',
    nameJa: '運動単位分解',
    category: 'advanced',
    description: 'Decompose HD-EMG into individual motor unit activity',
    descriptionJa: 'HD-EMGを個別運動単位活動に分解',
    icon: 'git-branch',
    params: [
      { name: 'method', type: 'select', label: '方法', default: 'bss', options: [
        { value: 'bss', label: 'ブラインド信号分離 (BSS)' },
        { value: 'ckc', label: '畳み込みカーネル補償 (CKC)' }
      ]},
      { name: 'nUnits', type: 'number', label: '推定単位数', default: 10, min: 5, max: 50 }
    ],
    outputType: 'plot',
    estimatedTime: 'slow'
  },

  firingRate: {
    id: 'firingRate',
    name: 'Firing Rate Analysis',
    nameJa: '発火率解析',
    category: 'advanced',
    description: 'Analyze motor unit firing rate patterns',
    descriptionJa: '運動単位発火率パターンの解析',
    icon: 'zap',
    params: [
      { name: 'smoothWindow', type: 'number', label: '平滑化窓', default: 100, min: 50, max: 500, unit: 'ms' }
    ],
    outputType: 'plot',
    estimatedTime: 'medium',
    dependencies: ['motorUnitDecomposition']
  },

  muapMorphology: {
    id: 'muapMorphology',
    name: 'MUAP Morphology',
    nameJa: 'MUAP形態解析',
    category: 'advanced',
    description: 'Analyze motor unit action potential morphology',
    descriptionJa: '運動単位活動電位の形態解析',
    icon: 'activity',
    params: [
      { name: 'features', type: 'select', label: '特徴量', default: 'all', options: [
        { value: 'all', label: '全特徴量' },
        { value: 'amplitude', label: '振幅のみ' },
        { value: 'duration', label: '持続時間のみ' },
        { value: 'phases', label: '相数のみ' }
      ]}
    ],
    outputType: 'table',
    estimatedTime: 'medium',
    dependencies: ['motorUnitDecomposition']
  },

  // EMG Pattern Recognition
  emgFeatureExtraction: {
    id: 'emgFeatureExtraction',
    name: 'EMG Feature Extraction',
    nameJa: 'EMG特徴量抽出',
    category: 'pattern',
    description: 'Extract time and frequency domain features for classification',
    descriptionJa: '分類用の時間・周波数領域特徴量を抽出',
    icon: 'hash',
    params: [
      { name: 'timeFeatures', type: 'select', label: '時間領域特徴', default: 'all', options: [
        { value: 'all', label: '全特徴 (RMS, MAV, WL, ZC, SSC)' },
        { value: 'rms_mav', label: 'RMS, MAV のみ' },
        { value: 'wl_zc_ssc', label: 'WL, ZC, SSC のみ' }
      ]},
      { name: 'freqFeatures', type: 'boolean', label: '周波数領域特徴', default: true },
      { name: 'windowSize', type: 'number', label: '窓サイズ', default: 250, min: 100, max: 500, unit: 'ms' }
    ],
    outputType: 'table',
    estimatedTime: 'fast'
  },

  emgClassification: {
    id: 'emgClassification',
    name: 'EMG Pattern Classification',
    nameJa: 'EMGパターン分類',
    category: 'pattern',
    description: 'Classify EMG patterns using machine learning',
    descriptionJa: '機械学習によるEMGパターン分類',
    icon: 'target',
    params: [
      { name: 'classifier', type: 'select', label: '分類器', default: 'lda', options: [
        { value: 'lda', label: '線形判別分析 (LDA)' },
        { value: 'svm', label: 'サポートベクターマシン (SVM)' },
        { value: 'knn', label: 'k近傍法 (KNN)' },
        { value: 'rf', label: 'ランダムフォレスト' }
      ]},
      { name: 'crossValidation', type: 'number', label: '交差検証', default: 5, min: 3, max: 10, description: 'k-fold数' }
    ],
    outputType: 'table',
    estimatedTime: 'medium',
    dependencies: ['emgFeatureExtraction']
  },

  confusionMatrix: {
    id: 'confusionMatrix',
    name: 'Confusion Matrix',
    nameJa: '混同行列',
    category: 'pattern',
    description: 'Display classification confusion matrix',
    descriptionJa: '分類の混同行列を表示',
    icon: 'grid',
    params: [],
    outputType: 'matrix',
    estimatedTime: 'fast',
    dependencies: ['emgClassification']
  }
}

// ============ Workflow Definitions ============

export const workflows: Record<string, Workflow> = {
  // EEG Event-Related Potential Workflow
  eegERP: {
    id: 'eegERP',
    name: 'EEG ERP Analysis',
    nameJa: 'EEG 事象関連電位解析',
    description: 'Standard ERP analysis pipeline for cognitive neuroscience',
    descriptionJa: '認知神経科学のための標準ERP解析パイプライン',
    datasetTypes: ['EEG'],
    icon: 'brain',
    color: 'blue',
    steps: [
      { methodId: 'bandpass', required: true, order: 1, defaultParams: { lowFreq: 0.1, highFreq: 30 } },
      { methodId: 'notch', required: false, order: 2 },
      { methodId: 'epochExtraction', required: true, order: 3, defaultParams: { preTime: 0.2, postTime: 0.8 } },
      { methodId: 'erpComputation', required: true, order: 4 },
      { methodId: 'ttest', required: true, order: 5 },
      { methodId: 'effectSize', required: false, order: 6 },
      { methodId: 'clusterPermutation', required: false, order: 7 }
    ],
    reportSections: [
      { id: 'preprocessing', title: 'Preprocessing', titleJa: '前処理', methodIds: ['bandpass', 'notch'], layout: 'half' },
      { id: 'erp', title: 'ERP Results', titleJa: 'ERP結果', methodIds: ['erpComputation'], layout: 'full' },
      { id: 'statistics', title: 'Statistical Analysis', titleJa: '統計解析', methodIds: ['ttest', 'effectSize', 'clusterPermutation'], layout: 'full' }
    ]
  },

  // EEG Spectral/Connectivity Workflow
  eegConnectivity: {
    id: 'eegConnectivity',
    name: 'EEG Connectivity Analysis',
    nameJa: 'EEG 接続性解析',
    description: 'Functional connectivity and network analysis for EEG',
    descriptionJa: 'EEGの機能的接続性とネットワーク解析',
    datasetTypes: ['EEG'],
    icon: 'share-2',
    color: 'purple',
    steps: [
      { methodId: 'bandpass', required: true, order: 1, defaultParams: { lowFreq: 1, highFreq: 45 } },
      { methodId: 'spectrogram', required: false, order: 2 },
      { methodId: 'bandpower', required: true, order: 3 },
      { methodId: 'plv', required: true, order: 4, defaultParams: { lowFreq: 8, highFreq: 13 } },
      { methodId: 'pli', required: false, order: 5 },
      { methodId: 'networkMetrics', required: true, order: 6 },
      { methodId: 'networkVisualization', required: true, order: 7 }
    ],
    reportSections: [
      { id: 'spectral', title: 'Spectral Analysis', titleJa: 'スペクトル解析', methodIds: ['spectrogram', 'bandpower'], layout: 'half' },
      { id: 'connectivity', title: 'Connectivity', titleJa: '接続性', methodIds: ['plv', 'pli'], layout: 'half' },
      { id: 'network', title: 'Network Analysis', titleJa: 'ネットワーク解析', methodIds: ['networkMetrics', 'networkVisualization'], layout: 'full' }
    ]
  },

  // ============ EMG Workflows ============

  // Basic EMG Processing Workflow
  emgBasic: {
    id: 'emgBasic',
    name: 'EMG Basic Processing',
    nameJa: 'EMG 基本処理',
    description: 'Standard EMG preprocessing and amplitude analysis pipeline',
    descriptionJa: '標準的なEMG前処理と振幅解析パイプライン',
    datasetTypes: ['EMG'],
    icon: 'activity',
    color: 'green',
    steps: [
      { methodId: 'emgBandpass', required: true, order: 1, defaultParams: { lowFreq: 20, highFreq: 500 } },
      { methodId: 'notch', required: true, order: 2 },
      { methodId: 'rectification', required: true, order: 3 },
      { methodId: 'emgEnvelope', required: true, order: 4, defaultParams: { cutoffFreq: 6, method: 'lowpass' } },
      { methodId: 'rmsCalculation', required: true, order: 5, defaultParams: { windowSize: 100 } },
      { methodId: 'mvcNormalization', required: false, order: 6 },
      { methodId: 'onsetDetection', required: false, order: 7 },
      { methodId: 'ttest', required: false, order: 8 }
    ],
    reportSections: [
      { id: 'preprocessing', title: 'Preprocessing', titleJa: '前処理', methodIds: ['emgBandpass', 'notch', 'rectification'], layout: 'third' },
      { id: 'envelope', title: 'Envelope & Amplitude', titleJa: 'エンベロープ・振幅', methodIds: ['emgEnvelope', 'rmsCalculation', 'mvcNormalization'], layout: 'full' },
      { id: 'timing', title: 'Timing Analysis', titleJa: 'タイミング解析', methodIds: ['onsetDetection'], layout: 'half' },
      { id: 'statistics', title: 'Statistics', titleJa: '統計', methodIds: ['ttest'], layout: 'half' }
    ]
  },

  // EMG Fatigue Analysis Workflow
  emgFatigue: {
    id: 'emgFatigue',
    name: 'EMG Fatigue Analysis',
    nameJa: 'EMG 疲労解析',
    description: 'Analyze muscle fatigue using spectral parameters',
    descriptionJa: 'スペクトルパラメータを用いた筋疲労解析',
    datasetTypes: ['EMG'],
    icon: 'trending-down',
    color: 'orange',
    steps: [
      { methodId: 'emgBandpass', required: true, order: 1, defaultParams: { lowFreq: 20, highFreq: 500 } },
      { methodId: 'notch', required: true, order: 2 },
      { methodId: 'emgSpectrogram', required: true, order: 3 },
      { methodId: 'medianFrequency', required: true, order: 4 },
      { methodId: 'meanFrequency', required: true, order: 5 },
      { methodId: 'fatigueIndex', required: true, order: 6 },
      { methodId: 'spectralCompression', required: false, order: 7 },
      { methodId: 'rmsCalculation', required: false, order: 8 }
    ],
    reportSections: [
      { id: 'preprocessing', title: 'Preprocessing', titleJa: '前処理', methodIds: ['emgBandpass', 'notch'], layout: 'half' },
      { id: 'spectral', title: 'Spectral Analysis', titleJa: 'スペクトル解析', methodIds: ['emgSpectrogram', 'medianFrequency', 'meanFrequency'], layout: 'full' },
      { id: 'fatigue', title: 'Fatigue Assessment', titleJa: '疲労評価', methodIds: ['fatigueIndex', 'spectralCompression'], layout: 'full' },
      { id: 'amplitude', title: 'Amplitude Changes', titleJa: '振幅変化', methodIds: ['rmsCalculation'], layout: 'half' }
    ]
  },

  // EMG Muscle Synergy Workflow
  emgSynergy: {
    id: 'emgSynergy',
    name: 'Muscle Synergy Analysis',
    nameJa: '筋シナジー解析',
    description: 'Extract and analyze muscle synergies using NMF',
    descriptionJa: 'NMFによる筋シナジーの抽出と解析',
    datasetTypes: ['EMG'],
    icon: 'layers',
    color: 'purple',
    steps: [
      { methodId: 'emgBandpass', required: true, order: 1, defaultParams: { lowFreq: 20, highFreq: 450 } },
      { methodId: 'notch', required: true, order: 2 },
      { methodId: 'rectification', required: true, order: 3 },
      { methodId: 'emgEnvelope', required: true, order: 4, defaultParams: { cutoffFreq: 10 } },
      { methodId: 'mvcNormalization', required: true, order: 5 },
      { methodId: 'cycleNormalization', required: true, order: 6, defaultParams: { targetLength: 101 } },
      { methodId: 'synergyVAF', required: true, order: 7, defaultParams: { vafThreshold: 90 } },
      { methodId: 'muscleSynergy', required: true, order: 8, defaultParams: { nSynergies: 4 } },
      { methodId: 'synergySimilarity', required: false, order: 9 }
    ],
    reportSections: [
      { id: 'preprocessing', title: 'Signal Processing', titleJa: '信号処理', methodIds: ['emgBandpass', 'notch', 'rectification', 'emgEnvelope'], layout: 'half' },
      { id: 'normalization', title: 'Normalization', titleJa: '正規化', methodIds: ['mvcNormalization', 'cycleNormalization'], layout: 'half' },
      { id: 'synergy', title: 'Synergy Extraction', titleJa: 'シナジー抽出', methodIds: ['synergyVAF', 'muscleSynergy'], layout: 'full' },
      { id: 'comparison', title: 'Comparison', titleJa: '比較', methodIds: ['synergySimilarity'], layout: 'full' }
    ]
  },

  // EMG Gait Analysis Workflow
  emgGait: {
    id: 'emgGait',
    name: 'Gait EMG Analysis',
    nameJa: '歩行EMG解析',
    description: 'EMG analysis during walking with timing and co-contraction',
    descriptionJa: '歩行時のタイミングと共収縮のEMG解析',
    datasetTypes: ['EMG', 'GAIT'],
    icon: 'footprints',
    color: 'teal',
    steps: [
      { methodId: 'emgBandpass', required: true, order: 1, defaultParams: { lowFreq: 20, highFreq: 500 } },
      { methodId: 'notch', required: true, order: 2 },
      { methodId: 'rectification', required: true, order: 3 },
      { methodId: 'emgEnvelope', required: true, order: 4, defaultParams: { cutoffFreq: 6 } },
      { methodId: 'mvcNormalization', required: true, order: 5 },
      { methodId: 'cycleNormalization', required: true, order: 6 },
      { methodId: 'onsetDetection', required: true, order: 7 },
      { methodId: 'activationTiming', required: true, order: 8 },
      { methodId: 'coContractionIndex', required: true, order: 9 },
      { methodId: 'muscleSynergy', required: false, order: 10 },
      { methodId: 'symmetryAnalysis', required: false, order: 11 }
    ],
    reportSections: [
      { id: 'preprocessing', title: 'Signal Processing', titleJa: '信号処理', methodIds: ['emgBandpass', 'notch', 'rectification', 'emgEnvelope'], layout: 'half' },
      { id: 'normalization', title: 'Normalization', titleJa: '正規化', methodIds: ['mvcNormalization', 'cycleNormalization'], layout: 'half' },
      { id: 'timing', title: 'Activation Timing', titleJa: '筋活動タイミング', methodIds: ['onsetDetection', 'activationTiming'], layout: 'full' },
      { id: 'coordination', title: 'Muscle Coordination', titleJa: '筋協調', methodIds: ['coContractionIndex', 'muscleSynergy', 'symmetryAnalysis'], layout: 'full' }
    ]
  },

  // EMG Motor Unit Analysis Workflow (Advanced)
  emgMotorUnit: {
    id: 'emgMotorUnit',
    name: 'Motor Unit Analysis',
    nameJa: '運動単位解析',
    description: 'Advanced motor unit decomposition and analysis',
    descriptionJa: '高度な運動単位分解と解析',
    datasetTypes: ['EMG'],
    icon: 'git-branch',
    color: 'indigo',
    steps: [
      { methodId: 'emgBandpass', required: true, order: 1, defaultParams: { lowFreq: 20, highFreq: 500 } },
      { methodId: 'notch', required: true, order: 2 },
      { methodId: 'motorUnitDecomposition', required: true, order: 3 },
      { methodId: 'firingRate', required: true, order: 4 },
      { methodId: 'muapMorphology', required: true, order: 5 },
      { methodId: 'correlation', required: false, order: 6 },
      { methodId: 'ttest', required: false, order: 7 }
    ],
    reportSections: [
      { id: 'preprocessing', title: 'Preprocessing', titleJa: '前処理', methodIds: ['emgBandpass', 'notch'], layout: 'half' },
      { id: 'decomposition', title: 'Motor Unit Decomposition', titleJa: '運動単位分解', methodIds: ['motorUnitDecomposition'], layout: 'full' },
      { id: 'properties', title: 'MU Properties', titleJa: '運動単位特性', methodIds: ['firingRate', 'muapMorphology'], layout: 'full' },
      { id: 'statistics', title: 'Statistics', titleJa: '統計', methodIds: ['correlation', 'ttest'], layout: 'half' }
    ]
  },

  // EMG Pattern Recognition Workflow
  emgPattern: {
    id: 'emgPattern',
    name: 'EMG Pattern Recognition',
    nameJa: 'EMGパターン認識',
    description: 'EMG classification for prosthetic/robot control',
    descriptionJa: '義肢・ロボット制御のためのEMG分類',
    datasetTypes: ['EMG'],
    icon: 'target',
    color: 'cyan',
    steps: [
      { methodId: 'emgBandpass', required: true, order: 1, defaultParams: { lowFreq: 20, highFreq: 500 } },
      { methodId: 'notch', required: true, order: 2 },
      { methodId: 'emgFeatureExtraction', required: true, order: 3, defaultParams: { timeFeatures: 'all', freqFeatures: true } },
      { methodId: 'pca', required: false, order: 4, defaultParams: { nComponents: 5 } },
      { methodId: 'emgClassification', required: true, order: 5, defaultParams: { classifier: 'lda' } },
      { methodId: 'confusionMatrix', required: true, order: 6 }
    ],
    reportSections: [
      { id: 'preprocessing', title: 'Preprocessing', titleJa: '前処理', methodIds: ['emgBandpass', 'notch'], layout: 'half' },
      { id: 'features', title: 'Feature Extraction', titleJa: '特徴量抽出', methodIds: ['emgFeatureExtraction', 'pca'], layout: 'full' },
      { id: 'classification', title: 'Classification Results', titleJa: '分類結果', methodIds: ['emgClassification', 'confusionMatrix'], layout: 'full' }
    ]
  },

  // Original EMG Analysis (kept for compatibility)
  emgAnalysis: {
    id: 'emgAnalysis',
    name: 'EMG Quick Analysis',
    nameJa: 'EMG 簡易解析',
    description: 'Quick EMG analysis for general use',
    descriptionJa: '汎用的な簡易EMG解析',
    datasetTypes: ['EMG'],
    icon: 'zap',
    color: 'lime',
    steps: [
      { methodId: 'bandpass', required: true, order: 1, defaultParams: { lowFreq: 20, highFreq: 450 } },
      { methodId: 'notch', required: true, order: 2 },
      { methodId: 'epochExtraction', required: true, order: 3 },
      { methodId: 'erpComputation', required: true, order: 4 },
      { methodId: 'bandpower', required: false, order: 5 },
      { methodId: 'correlation', required: false, order: 6 },
      { methodId: 'ttest', required: false, order: 7 }
    ],
    reportSections: [
      { id: 'preprocessing', title: 'Signal Processing', titleJa: '信号処理', methodIds: ['bandpass', 'notch'], layout: 'half' },
      { id: 'activation', title: 'Muscle Activation', titleJa: '筋活動', methodIds: ['erpComputation', 'bandpower'], layout: 'full' },
      { id: 'statistics', title: 'Statistics', titleJa: '統計', methodIds: ['correlation', 'ttest'], layout: 'half' }
    ]
  },

  // ECG Analysis Workflow
  ecgAnalysis: {
    id: 'ecgAnalysis',
    name: 'ECG/HRV Analysis',
    nameJa: 'ECG/心拍変動解析',
    description: 'Electrocardiogram and heart rate variability analysis',
    descriptionJa: '心電図と心拍変動解析',
    datasetTypes: ['ECG'],
    icon: 'heart',
    color: 'red',
    steps: [
      { methodId: 'bandpass', required: true, order: 1, defaultParams: { lowFreq: 0.5, highFreq: 40 } },
      { methodId: 'notch', required: true, order: 2 },
      { methodId: 'psd', required: true, order: 3 },
      { methodId: 'bandpower', required: true, order: 4 },
      { methodId: 'ttest', required: false, order: 5 }
    ],
    reportSections: [
      { id: 'preprocessing', title: 'Signal Quality', titleJa: '信号品質', methodIds: ['bandpass', 'notch'], layout: 'half' },
      { id: 'hrv', title: 'HRV Analysis', titleJa: 'HRV解析', methodIds: ['psd', 'bandpower'], layout: 'full' },
      { id: 'statistics', title: 'Statistics', titleJa: '統計', methodIds: ['ttest'], layout: 'half' }
    ]
  },

  // Gait/Motion Analysis Workflow
  gaitAnalysis: {
    id: 'gaitAnalysis',
    name: 'Gait Analysis',
    nameJa: '歩行解析',
    description: 'Comprehensive gait cycle and kinematic analysis',
    descriptionJa: '歩行サイクルとキネマティクス解析',
    datasetTypes: ['GAIT', 'MOTION'],
    icon: 'footprints',
    color: 'orange',
    steps: [
      { methodId: 'bandpass', required: false, order: 1, defaultParams: { lowFreq: 0.5, highFreq: 20 } },
      { methodId: 'cycleNormalization', required: true, order: 2 },
      { methodId: 'phaseSegmentation', required: true, order: 3 },
      { methodId: 'symmetryAnalysis', required: true, order: 4 },
      { methodId: 'dtw', required: false, order: 5 },
      { methodId: 'pca', required: false, order: 6 },
      { methodId: 'ttest', required: false, order: 7 }
    ],
    reportSections: [
      { id: 'normalization', title: 'Cycle Normalization', titleJa: 'サイクル正規化', methodIds: ['cycleNormalization', 'phaseSegmentation'], layout: 'full' },
      { id: 'symmetry', title: 'Symmetry Analysis', titleJa: '対称性解析', methodIds: ['symmetryAnalysis'], layout: 'half' },
      { id: 'comparison', title: 'Group Comparison', titleJa: '群間比較', methodIds: ['dtw', 'pca', 'ttest'], layout: 'full' }
    ]
  },

  // Generic/Exploratory Workflow
  exploratory: {
    id: 'exploratory',
    name: 'Exploratory Analysis',
    nameJa: '探索的解析',
    description: 'Flexible workflow for custom analysis',
    descriptionJa: 'カスタム解析のための柔軟なワークフロー',
    datasetTypes: ['EEG', 'EMG', 'ECG', 'EOG', 'GAIT', 'MOTION', 'GENERIC'],
    icon: 'search',
    color: 'gray',
    steps: [
      { methodId: 'bandpass', required: false, order: 1 },
      { methodId: 'spectrogram', required: false, order: 2 },
      { methodId: 'psd', required: false, order: 3 },
      { methodId: 'correlation', required: false, order: 4 },
      { methodId: 'pca', required: false, order: 5 }
    ],
    reportSections: [
      { id: 'results', title: 'Analysis Results', titleJa: '解析結果', methodIds: [], layout: 'full' }
    ]
  }
}

// ============ Helper Functions ============

export function getWorkflowsForDatasetType(datasetType: DatasetType): Workflow[] {
  return Object.values(workflows).filter(w => w.datasetTypes.includes(datasetType))
}

export function getRequiredMethods(workflow: Workflow): AnalysisMethod[] {
  return workflow.steps
    .filter(s => s.required)
    .map(s => analysisMethods[s.methodId])
    .filter(Boolean)
}

export function getOptionalMethods(workflow: Workflow): AnalysisMethod[] {
  return workflow.steps
    .filter(s => !s.required)
    .map(s => analysisMethods[s.methodId])
    .filter(Boolean)
}

export function getMethodsByCategory(category: AnalysisCategory): AnalysisMethod[] {
  return Object.values(analysisMethods).filter(m => m.category === category)
}

export function getCategoryLabel(category: AnalysisCategory): string {
  const labels: Record<AnalysisCategory, string> = {
    preprocessing: '前処理',
    timefreq: '時間周波数',
    erp: 'ERP',
    connectivity: '接続性',
    network: 'ネットワーク',
    statistics: '統計',
    advanced: '高度な解析',
    dimred: '次元削減',
    biomech: 'バイオメカニクス',
    emg: 'EMG処理',
    fatigue: '疲労解析',
    synergy: '筋シナジー',
    pattern: 'パターン認識'
  }
  return labels[category]
}

export function getCategoryColor(category: AnalysisCategory): string {
  const colors: Record<AnalysisCategory, string> = {
    preprocessing: '#6366f1',
    timefreq: '#8b5cf6',
    erp: '#06b6d4',
    connectivity: '#10b981',
    network: '#f59e0b',
    statistics: '#ef4444',
    advanced: '#ec4899',
    dimred: '#84cc16',
    biomech: '#f97316',
    emg: '#22c55e',
    fatigue: '#eab308',
    synergy: '#a855f7',
    pattern: '#06b6d4'
  }
  return colors[category]
}
