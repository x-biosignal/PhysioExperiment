// React Query hooks for dataset operations

import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { datasetsApi } from '@/services/api'
import type {
  ImportParams,
  FilterParams,
  EpochParams,
  SpectrogramParams,
  ConnectivityParams,
  TTestParams,
} from '@/types/api'

// Query keys
export const datasetKeys = {
  all: ['datasets'] as const,
  lists: () => [...datasetKeys.all, 'list'] as const,
  list: () => [...datasetKeys.lists()] as const,
  details: () => [...datasetKeys.all, 'detail'] as const,
  detail: (id: string) => [...datasetKeys.details(), id] as const,
  signals: (id: string) => [...datasetKeys.detail(id), 'signals'] as const,
}

// List datasets
export function useDatasets() {
  return useQuery({
    queryKey: datasetKeys.list(),
    queryFn: async () => {
      const response = await datasetsApi.list()
      if (!response.success) throw new Error(response.error)
      return response.data ?? []
    },
  })
}

// Get single dataset
export function useDataset(id: string | null) {
  return useQuery({
    queryKey: datasetKeys.detail(id ?? ''),
    queryFn: async () => {
      if (!id) throw new Error('No dataset ID')
      const response = await datasetsApi.get(id)
      if (!response.success) throw new Error(response.error)
      return response.data
    },
    enabled: !!id,
  })
}

// Get signal data
export function useSignals(
  id: string | null,
  params?: { channels?: number[]; start?: number; end?: number; downsample?: number }
) {
  return useQuery({
    queryKey: [...datasetKeys.signals(id ?? ''), params],
    queryFn: async () => {
      if (!id) throw new Error('No dataset ID')
      const response = await datasetsApi.getSignals(id, params)
      if (!response.success) throw new Error(response.error)
      return response.data
    },
    enabled: !!id,
    staleTime: 5 * 60 * 1000,
  })
}

// Import dataset mutation
export function useImportDataset() {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: (params: ImportParams) => datasetsApi.import(params),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: datasetKeys.lists() })
    },
  })
}

// Delete dataset mutation
export function useDeleteDataset() {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: (id: string) => datasetsApi.delete(id),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: datasetKeys.lists() })
    },
  })
}

// Filter mutation
export function useApplyFilter() {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: ({ id, params }: { id: string; params: FilterParams }) =>
      datasetsApi.filter(id, params),
    onSuccess: (_, { id }) => {
      queryClient.invalidateQueries({ queryKey: datasetKeys.detail(id) })
    },
  })
}

// Epoch mutation
export function useApplyEpoch() {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: ({ id, params }: { id: string; params: EpochParams }) =>
      datasetsApi.epoch(id, params),
    onSuccess: (_, { id }) => {
      queryClient.invalidateQueries({ queryKey: datasetKeys.detail(id) })
    },
  })
}

// Spectrogram query
export function useSpectrogram(id: string | null, params: SpectrogramParams | null) {
  return useQuery({
    queryKey: [...datasetKeys.detail(id ?? ''), 'spectrogram', params],
    queryFn: async () => {
      if (!id || !params) throw new Error('Missing parameters')
      const response = await datasetsApi.spectrogram(id, params)
      if (!response.success) throw new Error(response.error)
      return response.data
    },
    enabled: !!id && !!params,
    staleTime: 10 * 60 * 1000,
  })
}

// Connectivity query
export function useConnectivity(id: string | null, params: ConnectivityParams | null) {
  return useQuery({
    queryKey: [...datasetKeys.detail(id ?? ''), 'connectivity', params],
    queryFn: async () => {
      if (!id || !params) throw new Error('Missing parameters')
      const response = await datasetsApi.connectivity(id, params)
      if (!response.success) throw new Error(response.error)
      return response.data
    },
    enabled: !!id && !!params,
    staleTime: 10 * 60 * 1000,
  })
}

// T-test mutation
export function useTTest() {
  return useMutation({
    mutationFn: ({ id, params }: { id: string; params: TTestParams }) =>
      datasetsApi.tTest(id, params),
  })
}

// Cluster permutation mutation
export function useClusterPermutation() {
  return useMutation({
    mutationFn: ({ id, params }: { id: string; params: { condition1?: number[]; condition2?: number[]; nPermutations?: number; threshold?: number } }) =>
      datasetsApi.clusterPermutation(id, params),
  })
}
