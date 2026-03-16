// EMG Analysis API endpoints

import { api } from './client'
import type {
  ApiResponse,
  DatasetInfo,
  EMGEnvelopeParams,
  EMGRMSParams,
  MVCNormalizeParams,
  OnsetDetectionParams,
  OnsetDetectionResult,
  CoContractionParams,
  CoContractionResult,
  EMGFeaturesParams,
  EMGFeaturesResult,
  FrequencyAnalysisParams,
  FrequencyResult,
  FatigueIndexParams,
  FatigueIndexResult,
  FatigueAnalysisResult,
  NMFSynergyParams,
  NMFSynergyResult,
  OptimalSynergyParams,
  OptimalSynergyResult,
  SynergySimilarityParams,
  SynergySimilarityResult,
  SynergySimilarityTestResult,
} from '@/types/api'

const BASE = '/api/v1/datasets'

// EMG Processing API
export const emgApi = {
  // Rectify EMG signal
  rectify: (
    id: string,
    params?: { channels?: number[]; outputAssay?: string }
  ) =>
    api.post<ApiResponse<DatasetInfo>>(`${BASE}/${id}/emg/rectify`, {
      channels: params?.channels?.join(','),
      outputAssay: params?.outputAssay,
    }),

  // Compute EMG envelope
  envelope: (id: string, params?: EMGEnvelopeParams) =>
    api.post<ApiResponse<DatasetInfo>>(`${BASE}/${id}/emg/envelope`, params),

  // Compute RMS
  rms: (id: string, params?: EMGRMSParams) =>
    api.post<ApiResponse<DatasetInfo>>(`${BASE}/${id}/emg/rms`, {
      ...params,
      channels: params?.channels?.join(','),
    }),

  // Compute MAV
  mav: (
    id: string,
    params?: {
      windowMs?: number
      overlap?: number
      channels?: number[]
      outputAssay?: string
    }
  ) =>
    api.post<ApiResponse<DatasetInfo>>(`${BASE}/${id}/emg/mav`, {
      ...params,
      channels: params?.channels?.join(','),
    }),

  // MVC Normalize
  mvcNormalize: (id: string, params?: MVCNormalizeParams) =>
    api.post<ApiResponse<DatasetInfo>>(`${BASE}/${id}/emg/mvc-normalize`, {
      ...params,
      mvc: params?.mvc?.join(','),
      channels: params?.channels?.join(','),
    }),

  // Detect muscle onset
  detectOnset: (id: string, params: OnsetDetectionParams) =>
    api.post<ApiResponse<OnsetDetectionResult>>(
      `${BASE}/${id}/emg/detect-onset`,
      params
    ),

  // Compute co-contraction index
  coContraction: (id: string, params: CoContractionParams) =>
    api.post<ApiResponse<CoContractionResult>>(
      `${BASE}/${id}/emg/co-contraction`,
      params
    ),

  // Extract EMG features
  extractFeatures: (id: string, params?: EMGFeaturesParams) =>
    api.post<ApiResponse<EMGFeaturesResult>>(`${BASE}/${id}/emg/features`, {
      ...params,
      features: params?.features?.join(','),
      channels: params?.channels?.join(','),
    }),
}

// Fatigue Analysis API
export const fatigueApi = {
  // Compute median frequency
  medianFrequency: (id: string, params: FrequencyAnalysisParams) =>
    api.post<ApiResponse<FrequencyResult>>(
      `${BASE}/${id}/fatigue/median-frequency`,
      {
        ...params,
        freqRange: params.freqRange?.join(','),
      }
    ),

  // Compute mean frequency
  meanFrequency: (id: string, params: FrequencyAnalysisParams) =>
    api.post<ApiResponse<FrequencyResult>>(
      `${BASE}/${id}/fatigue/mean-frequency`,
      {
        ...params,
        freqRange: params.freqRange?.join(','),
      }
    ),

  // Compute fatigue index
  fatigueIndex: (id: string, params: FatigueIndexParams) =>
    api.post<ApiResponse<FatigueIndexResult>>(
      `${BASE}/${id}/fatigue/index`,
      params
    ),

  // Comprehensive fatigue analysis
  analyze: (
    id: string,
    params?: { channels?: number[]; windowSize?: number; overlap?: number }
  ) =>
    api.post<ApiResponse<FatigueAnalysisResult>>(
      `${BASE}/${id}/fatigue/analyze`,
      {
        ...params,
        channels: params?.channels?.join(','),
      }
    ),
}

// Muscle Synergy Analysis API
export const synergyApi = {
  // NMF synergy extraction
  nmf: (id: string, params?: NMFSynergyParams) =>
    api.post<ApiResponse<NMFSynergyResult>>(`${BASE}/${id}/synergy/nmf`, params),

  // Find optimal number of synergies
  findOptimal: (id: string, params?: OptimalSynergyParams) =>
    api.post<ApiResponse<OptimalSynergyResult>>(
      `${BASE}/${id}/synergy/optimal`,
      params
    ),

  // Compare synergy similarity between two datasets
  similarity: (params: SynergySimilarityParams) =>
    api.post<ApiResponse<SynergySimilarityResult>>(
      '/api/v1/synergy/similarity',
      params
    ),

  // Statistical test for synergy similarity
  similarityTest: (params: {
    id1: string
    id2: string
    nPermutations?: number
    metric?: 'correlation' | 'cosine'
  }) =>
    api.post<ApiResponse<SynergySimilarityTestResult>>(
      '/api/v1/synergy/similarity-test',
      params
    ),
}
