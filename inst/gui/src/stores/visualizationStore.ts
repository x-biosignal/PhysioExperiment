import { create } from 'zustand'
import { devtools } from 'zustand/middleware'

export type DisplayMode = 'butterfly' | 'stacked'
export type ColorPalette = 'viridis' | 'plasma' | 'inferno' | 'magma' | 'RdBu' | 'coolwarm'

interface SignalViewerState {
  timeRange: [number, number]
  visibleChannels: number[]
  displayMode: DisplayMode
  amplitudeScale: number
  showEvents: boolean
  showGrid: boolean
  cursorPosition: number | null
  selection: [number, number] | null
  playbackSpeed: number
  isPlaying: boolean
}

interface SpectrogramState {
  channel: number
  freqRange: [number, number]
  windowSize: number
  overlap: number
  colorScale: 'linear' | 'log'
  colorPalette: ColorPalette
}

interface TopomapState {
  time: number
  showContours: boolean
  showElectrodes: boolean
  showLabels: boolean
  colorPalette: ColorPalette
  animating: boolean
  animationSpeed: number
}

interface NetworkState {
  threshold: number
  layout: 'force' | 'circular' | 'grid'
  showLabels: boolean
  nodeSize: 'degree' | 'betweenness' | 'fixed'
  edgeWidth: 'weight' | 'fixed'
}

interface VisualizationState {
  signalViewer: SignalViewerState
  spectrogram: SpectrogramState
  topomap: TopomapState
  network: NetworkState

  // Synchronized time cursor across views
  syncedTime: number | null
  syncEnabled: boolean

  // Actions
  setTimeRange: (range: [number, number]) => void
  setVisibleChannels: (channels: number[]) => void
  setDisplayMode: (mode: DisplayMode) => void
  setAmplitudeScale: (scale: number) => void
  toggleEvents: () => void
  toggleGrid: () => void
  setCursorPosition: (position: number | null) => void
  setSelection: (selection: [number, number] | null) => void
  setPlaying: (playing: boolean) => void
  setPlaybackSpeed: (speed: number) => void

  // Spectrogram actions
  setSpectrogramChannel: (channel: number) => void
  setFreqRange: (range: [number, number]) => void
  setWindowSize: (size: number) => void

  // Topomap actions
  setTopomapTime: (time: number) => void
  toggleTopomapContours: () => void
  toggleTopomapElectrodes: () => void
  setTopomapAnimating: (animating: boolean) => void

  // Network actions
  setNetworkThreshold: (threshold: number) => void
  setNetworkLayout: (layout: 'force' | 'circular' | 'grid') => void

  // Sync actions
  setSyncedTime: (time: number | null) => void
  toggleSync: () => void

  // Reset
  resetVisualization: () => void
}

const initialSignalViewer: SignalViewerState = {
  timeRange: [0, 10],
  visibleChannels: [],
  displayMode: 'stacked',
  amplitudeScale: 1,
  showEvents: true,
  showGrid: true,
  cursorPosition: null,
  selection: null,
  playbackSpeed: 1,
  isPlaying: false,
}

const initialSpectrogram: SpectrogramState = {
  channel: 0,
  freqRange: [0, 100],
  windowSize: 256,
  overlap: 0.5,
  colorScale: 'log',
  colorPalette: 'viridis',
}

const initialTopomap: TopomapState = {
  time: 0,
  showContours: true,
  showElectrodes: true,
  showLabels: false,
  colorPalette: 'RdBu',
  animating: false,
  animationSpeed: 1,
}

const initialNetwork: NetworkState = {
  threshold: 0.5,
  layout: 'force',
  showLabels: true,
  nodeSize: 'degree',
  edgeWidth: 'weight',
}

export const useVisualizationStore = create<VisualizationState>()(
  devtools((set) => ({
    signalViewer: initialSignalViewer,
    spectrogram: initialSpectrogram,
    topomap: initialTopomap,
    network: initialNetwork,
    syncedTime: null,
    syncEnabled: true,

    // Signal viewer actions
    setTimeRange: (range) =>
      set((state) => ({
        signalViewer: { ...state.signalViewer, timeRange: range },
      })),

    setVisibleChannels: (channels) =>
      set((state) => ({
        signalViewer: { ...state.signalViewer, visibleChannels: channels },
      })),

    setDisplayMode: (mode) =>
      set((state) => ({
        signalViewer: { ...state.signalViewer, displayMode: mode },
      })),

    setAmplitudeScale: (scale) =>
      set((state) => ({
        signalViewer: { ...state.signalViewer, amplitudeScale: scale },
      })),

    toggleEvents: () =>
      set((state) => ({
        signalViewer: { ...state.signalViewer, showEvents: !state.signalViewer.showEvents },
      })),

    toggleGrid: () =>
      set((state) => ({
        signalViewer: { ...state.signalViewer, showGrid: !state.signalViewer.showGrid },
      })),

    setCursorPosition: (position) =>
      set((state) => ({
        signalViewer: { ...state.signalViewer, cursorPosition: position },
        syncedTime: state.syncEnabled ? position : state.syncedTime,
      })),

    setSelection: (selection) =>
      set((state) => ({
        signalViewer: { ...state.signalViewer, selection },
      })),

    setPlaying: (playing) =>
      set((state) => ({
        signalViewer: { ...state.signalViewer, isPlaying: playing },
      })),

    setPlaybackSpeed: (speed) =>
      set((state) => ({
        signalViewer: { ...state.signalViewer, playbackSpeed: speed },
      })),

    // Spectrogram actions
    setSpectrogramChannel: (channel) =>
      set((state) => ({
        spectrogram: { ...state.spectrogram, channel },
      })),

    setFreqRange: (range) =>
      set((state) => ({
        spectrogram: { ...state.spectrogram, freqRange: range },
      })),

    setWindowSize: (size) =>
      set((state) => ({
        spectrogram: { ...state.spectrogram, windowSize: size },
      })),

    // Topomap actions
    setTopomapTime: (time) =>
      set((state) => ({
        topomap: { ...state.topomap, time },
        syncedTime: state.syncEnabled ? time : state.syncedTime,
      })),

    toggleTopomapContours: () =>
      set((state) => ({
        topomap: { ...state.topomap, showContours: !state.topomap.showContours },
      })),

    toggleTopomapElectrodes: () =>
      set((state) => ({
        topomap: { ...state.topomap, showElectrodes: !state.topomap.showElectrodes },
      })),

    setTopomapAnimating: (animating) =>
      set((state) => ({
        topomap: { ...state.topomap, animating },
      })),

    // Network actions
    setNetworkThreshold: (threshold) =>
      set((state) => ({
        network: { ...state.network, threshold },
      })),

    setNetworkLayout: (layout) =>
      set((state) => ({
        network: { ...state.network, layout },
      })),

    // Sync actions
    setSyncedTime: (time) => set({ syncedTime: time }),
    toggleSync: () => set((state) => ({ syncEnabled: !state.syncEnabled })),

    // Reset
    resetVisualization: () =>
      set({
        signalViewer: initialSignalViewer,
        spectrogram: initialSpectrogram,
        topomap: initialTopomap,
        network: initialNetwork,
        syncedTime: null,
      }),
  }))
)
