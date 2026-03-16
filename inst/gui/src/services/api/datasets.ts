// Dataset API endpoints

import { api } from './client'
import type {
  ApiResponse,
  DatasetInfo,
  SignalData,
  SpectrogramData,
  ConnectivityData,
  StatisticsResult,
  ImportParams,
  FilterParams,
  EpochParams,
  SpectrogramParams,
  ConnectivityParams,
  TTestParams,
} from '@/types/api'

const BASE = '/api/v1/datasets'

// Dataset CRUD operations
export const datasetsApi = {
  // List all datasets
  list: () => api.get<ApiResponse<DatasetInfo[]>>(BASE),

  // Import a new dataset
  import: (params: ImportParams) =>
    api.post<ApiResponse<DatasetInfo>>(BASE, params),

  // Get dataset info
  get: (id: string) => api.get<ApiResponse<DatasetInfo>>(`${BASE}/${id}`),

  // Delete dataset
  delete: (id: string) => api.delete<ApiResponse<void>>(`${BASE}/${id}`),

  // Get signal data
  getSignals: (
    id: string,
    params?: {
      channels?: number[]
      start?: number
      end?: number
      downsample?: number
    }
  ) => {
    const query = new URLSearchParams()
    if (params?.channels) query.set('channels', params.channels.join(','))
    if (params?.start !== undefined) query.set('start', String(params.start))
    if (params?.end !== undefined) query.set('end', String(params.end))
    if (params?.downsample) query.set('downsample', String(params.downsample))

    const queryString = query.toString()
    return api.get<ApiResponse<SignalData>>(
      `${BASE}/${id}/signals${queryString ? `?${queryString}` : ''}`
    )
  },

  // Apply filter
  filter: (id: string, params: FilterParams) =>
    api.post<ApiResponse<DatasetInfo>>(`${BASE}/${id}/filter`, params),

  // Apply reference change
  rereference: (
    id: string,
    params: { method: string; channels?: string[]; outputAssay?: string }
  ) => api.post<ApiResponse<DatasetInfo>>(`${BASE}/${id}/rereference`, params),

  // Resample
  resample: (
    id: string,
    params: { targetRate: number; method?: string; outputAssay?: string }
  ) => api.post<ApiResponse<DatasetInfo>>(`${BASE}/${id}/resample`, params),

  // Epoch data
  epoch: (id: string, params: EpochParams) =>
    api.post<ApiResponse<DatasetInfo>>(`${BASE}/${id}/epoch`, params),

  // Compute spectrogram
  spectrogram: (id: string, params: SpectrogramParams) =>
    api.post<ApiResponse<SpectrogramData>>(`${BASE}/${id}/spectrogram`, params),

  // Compute wavelet transform
  wavelet: (
    id: string,
    params: { frequencies: number[]; nCycles?: number; channel?: number }
  ) => api.post<ApiResponse<SpectrogramData>>(`${BASE}/${id}/wavelet`, params),

  // Compute band power
  bandPower: (
    id: string,
    params?: {
      bands?: Record<string, [number, number]>
      method?: string
      relative?: boolean
    }
  ) => api.post<ApiResponse<Record<string, number[]>>>(`${BASE}/${id}/bandpower`, params),

  // Compute connectivity
  connectivity: (id: string, params: ConnectivityParams) =>
    api.post<ApiResponse<ConnectivityData>>(`${BASE}/${id}/connectivity`, params),

  // Statistical tests
  tTest: (id: string, params: TTestParams) =>
    api.post<ApiResponse<StatisticsResult>>(`${BASE}/${id}/statistics/ttest`, params),

  anova: (id: string, params: { groups: string }) =>
    api.post<ApiResponse<StatisticsResult>>(`${BASE}/${id}/statistics/anova`, params),

  clusterPermutation: (
    id: string,
    params: {
      condition1?: number[]
      condition2?: number[]
      nPermutations?: number
      threshold?: number
    }
  ) =>
    api.post<ApiResponse<StatisticsResult>>(
      `${BASE}/${id}/statistics/cluster-permutation`,
      params
    ),
}
