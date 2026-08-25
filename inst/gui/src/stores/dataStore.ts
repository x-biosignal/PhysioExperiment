import { create } from 'zustand'
import { devtools, persist } from 'zustand/middleware'
import { testDatasets, generateDatasetSignals, generateTimeArray } from '@/data/testData'

export interface Dataset {
  id: string
  name: string
  filePath?: string
  nChannels: number
  nTimepoints: number
  samplingRate: number
  duration: number
  channels: ChannelInfo[]
  assays: string[]
  events?: EventInfo[]
  metadata?: Record<string, unknown>
  createdAt: Date
}

export interface ChannelInfo {
  index: number
  label: string
  type: 'EEG' | 'EMG' | 'ECG' | 'EOG' | 'MISC'
  unit?: string
  reference?: string
}

export interface EventInfo {
  onset: number
  duration: number
  type: string
  value?: number | string
}

interface SignalCache {
  data: number[][]
  timeRange: [number, number]
  channels: number[]
  samplingRate: number
  timestamp: number
}

// Generated signal data cache
interface GeneratedSignalData {
  signals: Record<string, number[]>
  time: number[]
}

interface DataState {
  // Datasets
  datasets: Record<string, Dataset>
  activeDatasetId: string | null

  // Loading states
  loadingStates: Record<string, boolean>
  errors: Record<string, string | null>

  // Signal cache for visualization
  signalCache: Record<string, SignalCache>

  // Generated test data signals
  generatedSignals: Record<string, GeneratedSignalData>

  // Actions
  addDataset: (dataset: Dataset) => void
  updateDataset: (id: string, updates: Partial<Dataset>) => void
  removeDataset: (id: string) => void
  setActiveDataset: (id: string | null) => void
  setLoading: (id: string, loading: boolean) => void
  setError: (id: string, error: string | null) => void
  cacheSignal: (datasetId: string, cache: SignalCache) => void
  getSignalCache: (datasetId: string) => SignalCache | null
  getGeneratedSignals: (datasetId: string) => GeneratedSignalData | null
  ensureGeneratedSignals: (datasetId: string) => void
  clearCache: (datasetId?: string) => void
  clearAll: () => void
  loadTestDatasets: () => void
}

const MAX_CACHE_AGE = 5 * 60 * 1000 // 5 minutes

export const useDataStore = create<DataState>()(
  devtools(
    persist(
      (set, get) => ({
        datasets: {},
        activeDatasetId: null,
        loadingStates: {},
        errors: {},
        signalCache: {},
        generatedSignals: {},

    addDataset: (dataset) =>
      set((state) => ({
        datasets: { ...state.datasets, [dataset.id]: dataset },
      })),

    updateDataset: (id, updates) =>
      set((state) => ({
        datasets: {
          ...state.datasets,
          [id]: { ...state.datasets[id], ...updates },
        },
      })),

    removeDataset: (id) =>
      set((state) => {
        const { [id]: _, ...rest } = state.datasets
        const { [id]: __, ...cacheRest } = state.signalCache
        return {
          datasets: rest,
          signalCache: cacheRest,
          activeDatasetId: state.activeDatasetId === id ? null : state.activeDatasetId,
        }
      }),

    setActiveDataset: (id) => {
      console.log('setActiveDataset called with:', id)
      set({ activeDatasetId: id })
      console.log('activeDatasetId after set:', get().activeDatasetId)
    },

    setLoading: (id, loading) =>
      set((state) => ({
        loadingStates: { ...state.loadingStates, [id]: loading },
      })),

    setError: (id, error) =>
      set((state) => ({
        errors: { ...state.errors, [id]: error },
      })),

    cacheSignal: (datasetId, cache) =>
      set((state) => ({
        signalCache: {
          ...state.signalCache,
          [datasetId]: { ...cache, timestamp: Date.now() },
        },
      })),

    getSignalCache: (datasetId) => {
      const cache = get().signalCache[datasetId]
      if (!cache) return null
      if (Date.now() - cache.timestamp > MAX_CACHE_AGE) {
        // Cache expired
        get().clearCache(datasetId)
        return null
      }
      return cache
    },

    clearCache: (datasetId) =>
      set((state) => {
        if (datasetId) {
          const { [datasetId]: _, ...rest } = state.signalCache
          return { signalCache: rest }
        }
        return { signalCache: {} }
      }),

    clearAll: () =>
      set({
        datasets: {},
        activeDatasetId: null,
        loadingStates: {},
        errors: {},
        signalCache: {},
        generatedSignals: {},
      }),

    getGeneratedSignals: (datasetId) => {
      // Pure getter - no side effects during render
      return get().generatedSignals[datasetId] ?? null
    },

    ensureGeneratedSignals: (datasetId) => {
      // Action to regenerate signals if missing - call from useEffect
      const existing = get().generatedSignals[datasetId]
      if (existing) return

      const dataset = get().datasets[datasetId]
      if (dataset && dataset.metadata?.isDemo) {
        const testData = testDatasets.find(td => td.id === datasetId)
        if (testData) {
          console.log('Regenerating signals for:', datasetId)
          const signals = generateDatasetSignals(testData)
          const time = generateTimeArray(testData.samplingRate, Math.min(testData.duration, 60))
          const newData = { signals, time }
          set((state) => ({
            generatedSignals: { ...state.generatedSignals, [datasetId]: newData }
          }))
        }
      }
    },

    loadTestDatasets: () => {
      const newDatasets: Record<string, Dataset> = {}
      const newGeneratedSignals: Record<string, GeneratedSignalData> = {}

      testDatasets.forEach((testData) => {
        // Convert test dataset to Dataset format
        const dataset: Dataset = {
          id: testData.id,
          name: testData.name,
          nChannels: testData.nChannels,
          nTimepoints: testData.nTimepoints,
          samplingRate: testData.samplingRate,
          duration: testData.duration,
          channels: testData.channels.map((ch, idx) => ({
            index: idx,
            label: ch.label,
            type: testData.type as 'EEG' | 'EMG' | 'ECG' | 'EOG' | 'MISC',
            unit: ch.unit,
          })),
          assays: ['raw'],
          events: testData.events.map((evt) => ({
            onset: evt.time,
            duration: 0,
            type: evt.type,
            value: evt.label,
          })),
          metadata: { isDemo: true, dataType: testData.type },
          createdAt: new Date(),
        }

        newDatasets[testData.id] = dataset

        // Generate signals for this dataset
        const signals = generateDatasetSignals(testData)
        const time = generateTimeArray(
          testData.samplingRate,
          Math.min(testData.duration, 60)
        )
        newGeneratedSignals[testData.id] = { signals, time }
      })

      set((state) => ({
        datasets: { ...state.datasets, ...newDatasets },
        generatedSignals: { ...state.generatedSignals, ...newGeneratedSignals },
      }))
    },
  }),
  {
    name: 'physio-data-store',
    // Only persist small data - not the large signal arrays
    partialize: (state) => ({
      datasets: state.datasets,
      activeDatasetId: state.activeDatasetId,
      // Don't persist generatedSignals - too large for localStorage
    }),
  }
)))
