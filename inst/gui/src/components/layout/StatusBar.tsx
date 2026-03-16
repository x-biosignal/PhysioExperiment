import { useDataStore } from '@/stores/dataStore'
import { useVisualizationStore } from '@/stores/visualizationStore'
import { Activity, Clock, Database, Cpu } from 'lucide-react'

export function StatusBar() {
  const datasets = useDataStore((state) => Object.keys(state.datasets).length)
  const activeDataset = useDataStore((state) =>
    state.activeDatasetId ? state.datasets[state.activeDatasetId] : null
  )
  const { syncedTime, syncEnabled } = useVisualizationStore()

  const formatTime = (seconds: number | null) => {
    if (seconds === null) return '--:--:---'
    const mins = Math.floor(seconds / 60)
    const secs = Math.floor(seconds % 60)
    const ms = Math.floor((seconds % 1) * 1000)
    return `${mins.toString().padStart(2, '0')}:${secs.toString().padStart(2, '0')}.${ms.toString().padStart(3, '0')}`
  }

  return (
    <footer className="flex h-6 items-center justify-between border-t bg-card px-4 text-xs text-muted-foreground">
      {/* Left Section */}
      <div className="flex items-center gap-4">
        <div className="flex items-center gap-1.5">
          <Database className="h-3 w-3" />
          <span>{datasets} dataset{datasets !== 1 ? 's' : ''} loaded</span>
        </div>

        {activeDataset && (
          <>
            <span className="text-border">|</span>
            <div className="flex items-center gap-1.5">
              <Activity className="h-3 w-3" />
              <span>
                {activeDataset.nChannels} channels @ {activeDataset.samplingRate} Hz
              </span>
            </div>
          </>
        )}
      </div>

      {/* Center Section - Time Display */}
      <div className="flex items-center gap-2">
        <Clock className="h-3 w-3" />
        <span className="font-mono">
          Time: {formatTime(syncedTime)}
        </span>
        {syncEnabled && (
          <span className="rounded bg-primary/20 px-1 text-primary">SYNC</span>
        )}
      </div>

      {/* Right Section */}
      <div className="flex items-center gap-4">
        <div className="flex items-center gap-1.5">
          <Cpu className="h-3 w-3" />
          <span>Processing: Idle</span>
        </div>
        <span className="text-border">|</span>
        <span>PhysioExperiment v1.0.0</span>
      </div>
    </footer>
  )
}
