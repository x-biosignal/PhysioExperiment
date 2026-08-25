import { useEffect, useRef, useState, useCallback, useMemo } from 'react'
import { useVisualizationStore } from '@/stores/visualizationStore'
import { useDataStore } from '@/stores/dataStore'
import { formatTime, getSignalColorPalette } from '@/lib/utils'

interface SignalCanvasProps {
  signals: Record<string, number[]>
  time: number[]
  visibleChannels: string[]
  timeRange: [number, number]
  amplitude: number
  cursorTime: number | null
  onTimeClick: (time: number) => void
}

function SignalCanvas({
  signals,
  time,
  visibleChannels,
  timeRange,
  amplitude,
  cursorTime,
  onTimeClick
}: SignalCanvasProps) {
  const canvasRef = useRef<HTMLCanvasElement>(null)
  const containerRef = useRef<HTMLDivElement>(null)

  const draw = useCallback(() => {
    const canvas = canvasRef.current
    const container = containerRef.current
    if (!canvas || !container) return

    const ctx = canvas.getContext('2d')
    if (!ctx) return

    const rect = container.getBoundingClientRect()
    const dpr = window.devicePixelRatio || 1
    canvas.width = rect.width * dpr
    canvas.height = rect.height * dpr
    canvas.style.width = `${rect.width}px`
    canvas.style.height = `${rect.height}px`
    ctx.scale(dpr, dpr)

    const width = rect.width
    const height = rect.height

    ctx.fillStyle = 'hsl(var(--background))'
    ctx.fillRect(0, 0, width, height)

    const nChannels = visibleChannels.length
    if (nChannels === 0 || time.length === 0) return

    const channelHeight = height / nChannels
    const colors = getSignalColorPalette(nChannels)

    const startIdx = time.findIndex(t => t >= timeRange[0])
    const endIdx = time.findIndex(t => t > timeRange[1])
    const visibleTimeIndices = {
      start: Math.max(0, startIdx),
      end: endIdx === -1 ? time.length : endIdx
    }

    const timeWidth = timeRange[1] - timeRange[0]

    visibleChannels.forEach((channelName, channelIdx) => {
      const signal = signals[channelName]
      if (!signal) return

      const yCenter = channelHeight * (channelIdx + 0.5)
      const yScale = channelHeight * 0.4 * amplitude

      ctx.fillStyle = colors[channelIdx % colors.length]
      ctx.font = '12px Inter, sans-serif'
      ctx.fillText(channelName, 8, yCenter - channelHeight * 0.3)

      ctx.beginPath()
      ctx.strokeStyle = colors[channelIdx % colors.length]
      ctx.lineWidth = 1

      let firstPoint = true
      for (let i = visibleTimeIndices.start; i < visibleTimeIndices.end; i++) {
        const x = ((time[i] - timeRange[0]) / timeWidth) * width
        const y = yCenter - signal[i] * yScale

        if (firstPoint) {
          ctx.moveTo(x, y)
          firstPoint = false
        } else {
          ctx.lineTo(x, y)
        }
      }
      ctx.stroke()

      if (channelIdx < nChannels - 1) {
        ctx.strokeStyle = 'hsl(var(--border))'
        ctx.lineWidth = 0.5
        ctx.beginPath()
        ctx.moveTo(0, channelHeight * (channelIdx + 1))
        ctx.lineTo(width, channelHeight * (channelIdx + 1))
        ctx.stroke()
      }
    })

    if (cursorTime !== null && cursorTime >= timeRange[0] && cursorTime <= timeRange[1]) {
      const x = ((cursorTime - timeRange[0]) / timeWidth) * width
      ctx.strokeStyle = 'hsl(var(--primary))'
      ctx.lineWidth = 1
      ctx.setLineDash([4, 4])
      ctx.beginPath()
      ctx.moveTo(x, 0)
      ctx.lineTo(x, height)
      ctx.stroke()
      ctx.setLineDash([])

      ctx.fillStyle = 'hsl(var(--primary))'
      ctx.font = '11px Inter, sans-serif'
      ctx.fillText(formatTime(cursorTime), x + 4, 14)
    }

    ctx.fillStyle = 'hsl(var(--muted-foreground))'
    ctx.font = '10px Inter, sans-serif'
    const tickCount = 5
    for (let i = 0; i <= tickCount; i++) {
      const t = timeRange[0] + (timeRange[1] - timeRange[0]) * (i / tickCount)
      const x = (i / tickCount) * width
      ctx.fillText(formatTime(t), x, height - 4)
    }
  }, [signals, time, visibleChannels, timeRange, amplitude, cursorTime])

  useEffect(() => {
    draw()
    const resizeObserver = new ResizeObserver(draw)
    if (containerRef.current) {
      resizeObserver.observe(containerRef.current)
    }
    return () => resizeObserver.disconnect()
  }, [draw])

  const handleClick = (e: React.MouseEvent) => {
    const canvas = canvasRef.current
    if (!canvas) return
    const rect = canvas.getBoundingClientRect()
    const x = e.clientX - rect.left
    const clickTime = timeRange[0] + (x / rect.width) * (timeRange[1] - timeRange[0])
    onTimeClick(clickTime)
  }

  return (
    <div ref={containerRef} className="h-full w-full">
      <canvas
        ref={canvasRef}
        onClick={handleClick}
        className="cursor-crosshair"
      />
    </div>
  )
}

interface ControlPanelProps {
  timeRange: [number, number]
  totalDuration: number
  amplitude: number
  displayMode: string
  onTimeRangeChange: (range: [number, number]) => void
  onAmplitudeChange: (amp: number) => void
  onDisplayModeChange: (mode: 'butterfly' | 'stacked') => void
}

function ControlPanel({
  timeRange,
  totalDuration,
  amplitude,
  displayMode,
  onTimeRangeChange,
  onAmplitudeChange,
  onDisplayModeChange
}: ControlPanelProps) {
  const windowSize = timeRange[1] - timeRange[0]

  const handleScroll = (direction: 'left' | 'right') => {
    const step = windowSize * 0.5
    const newStart = direction === 'left'
      ? Math.max(0, timeRange[0] - step)
      : Math.min(totalDuration - windowSize, timeRange[0] + step)
    onTimeRangeChange([newStart, newStart + windowSize])
  }

  const handleZoom = (direction: 'in' | 'out') => {
    const center = (timeRange[0] + timeRange[1]) / 2
    const newSize = direction === 'in' ? windowSize / 2 : windowSize * 2
    const clampedSize = Math.min(totalDuration, Math.max(0.1, newSize))
    const newStart = Math.max(0, center - clampedSize / 2)
    const newEnd = Math.min(totalDuration, newStart + clampedSize)
    onTimeRangeChange([newEnd - clampedSize, newEnd])
  }

  return (
    <div className="flex items-center gap-4 border-t bg-card px-4 py-2">
      <div className="flex items-center gap-1">
        <button onClick={() => handleScroll('left')} className="rounded p-1 hover:bg-accent" title="左にスクロール">
          <svg className="h-5 w-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M15 19l-7-7 7-7" />
          </svg>
        </button>
        <button onClick={() => handleScroll('right')} className="rounded p-1 hover:bg-accent" title="右にスクロール">
          <svg className="h-5 w-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 5l7 7-7 7" />
          </svg>
        </button>
      </div>

      <div className="flex items-center gap-1">
        <button onClick={() => handleZoom('out')} className="rounded p-1 hover:bg-accent" title="ズームアウト">
          <svg className="h-5 w-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0zM13 10H7" />
          </svg>
        </button>
        <button onClick={() => handleZoom('in')} className="rounded p-1 hover:bg-accent" title="ズームイン">
          <svg className="h-5 w-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0zM10 7v6m3-3H7" />
          </svg>
        </button>
        <span className="ml-2 text-sm text-muted-foreground">{windowSize.toFixed(1)}s</span>
      </div>

      <div className="h-4 w-px bg-border" />

      <div className="flex items-center gap-2">
        <span className="text-sm text-muted-foreground">振幅:</span>
        <input type="range" min="0.1" max="5" step="0.1" value={amplitude} onChange={(e) => onAmplitudeChange(parseFloat(e.target.value))} className="w-24" />
        <span className="text-sm">{amplitude.toFixed(1)}x</span>
      </div>

      <div className="h-4 w-px bg-border" />

      <div className="flex items-center gap-2">
        <span className="text-sm text-muted-foreground">表示:</span>
        <select value={displayMode} onChange={(e) => onDisplayModeChange(e.target.value as 'butterfly' | 'stacked')} className="rounded border bg-background px-2 py-1 text-sm">
          <option value="stacked">スタック</option>
          <option value="butterfly">バタフライ</option>
        </select>
      </div>

      <div className="flex-1" />
      <div className="text-sm text-muted-foreground">{formatTime(timeRange[0])} - {formatTime(timeRange[1])}</div>
    </div>
  )
}

interface ChannelListProps {
  channels: string[]
  visibleChannels: string[]
  onToggleChannel: (channel: string) => void
  onSelectAll: () => void
  onSelectNone: () => void
}

function ChannelList({ channels, visibleChannels, onToggleChannel, onSelectAll, onSelectNone }: ChannelListProps) {
  return (
    <div className="w-48 flex-shrink-0 border-r flex flex-col">
      <div className="border-b p-3">
        <h3 className="text-sm font-medium">チャンネル</h3>
        <div className="mt-2 flex gap-2">
          <button onClick={onSelectAll} className="text-xs text-primary hover:underline">全選択</button>
          <button onClick={onSelectNone} className="text-xs text-primary hover:underline">全解除</button>
        </div>
      </div>
      <div className="flex-1 overflow-auto p-2">
        {channels.map((channel) => (
          <label key={channel} className="flex items-center gap-2 rounded px-2 py-1 hover:bg-accent cursor-pointer">
            <input type="checkbox" checked={visibleChannels.includes(channel)} onChange={() => onToggleChannel(channel)} className="rounded" />
            <span className="text-sm truncate">{channel}</span>
          </label>
        ))}
      </div>
    </div>
  )
}

export function SignalViewer() {
  const { activeDatasetId, datasets, generatedSignals, ensureGeneratedSignals } = useDataStore()
  const { signalViewer, setTimeRange, setAmplitudeScale, setDisplayMode, setCursorPosition, syncedTime } = useVisualizationStore()

  const activeDataset = activeDatasetId ? datasets[activeDatasetId] : null
  // Subscribe directly to generatedSignals for reactive updates
  const generatedData = activeDatasetId ? generatedSignals[activeDatasetId] ?? null : null
  // Memoize channel labels based on dataset ID to avoid reference changes
  const channelLabels = useMemo(
    () => activeDataset?.channels.map(ch => ch.label) ?? [],
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [activeDatasetId]
  )

  // Debug logging
  console.log('SignalViewer render:', {
    activeDatasetId,
    hasActiveDataset: !!activeDataset,
    hasGeneratedData: !!generatedData,
    signalKeys: generatedData ? Object.keys(generatedData.signals) : [],
    channelLabels
  })

  const [visibleChannels, setVisibleChannels] = useState<string[]>([])
  const [lastDatasetId, setLastDatasetId] = useState<string | null>(null)

  // Initialize visible channels when dataset changes
  useEffect(() => {
    if (activeDatasetId && activeDatasetId !== lastDatasetId && channelLabels.length > 0) {
      console.log('Initializing visibleChannels for dataset:', activeDatasetId, channelLabels.slice(0, 8))
      setVisibleChannels(channelLabels.slice(0, 8))
      setLastDatasetId(activeDatasetId)
    }
  }, [activeDatasetId, lastDatasetId, channelLabels])

  // Ensure generated signals exist for demo datasets
  const isDemo = activeDataset?.metadata?.isDemo
  useEffect(() => {
    if (activeDatasetId && isDemo && !generatedData) {
      ensureGeneratedSignals(activeDatasetId)
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [activeDatasetId, isDemo])

  // Initialize time range only once per dataset
  const [timeRangeInitialized, setTimeRangeInitialized] = useState(false)
  useEffect(() => {
    if (activeDatasetId !== lastDatasetId) {
      setTimeRangeInitialized(false)
    }
  }, [activeDatasetId, lastDatasetId])

  useEffect(() => {
    if (activeDataset && !timeRangeInitialized) {
      const initialWindow = Math.min(10, activeDataset.duration)
      setTimeRange([0, initialWindow])
      setTimeRangeInitialized(true)
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [activeDataset, timeRangeInitialized])

  const handleToggleChannel = (channel: string) => {
    setVisibleChannels((prev) => prev.includes(channel) ? prev.filter((c) => c !== channel) : [...prev, channel])
  }

  if (!activeDataset) {
    return (
      <div className="flex h-full min-h-[400px] items-center justify-center text-muted-foreground bg-card">
        <div className="text-center p-8">
          <svg className="mx-auto h-12 w-12" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z" />
          </svg>
          <p className="mt-4 text-lg">データセットが選択されていません</p>
          <p className="mt-2 text-sm">データブラウザに戻ってデータセットを選択してください</p>
          <p className="mt-4 text-xs opacity-50">activeDatasetId: {activeDatasetId ?? 'null'}</p>
        </div>
      </div>
    )
  }

  if (!generatedData || Object.keys(generatedData.signals).length === 0) {
    return (
      <div className="flex h-full min-h-[400px] items-center justify-center text-muted-foreground bg-card">
        <div className="text-center p-8">
          <svg className="mx-auto h-12 w-12" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z" />
          </svg>
          <p className="mt-4 text-lg">信号データが生成されていません</p>
          <p className="mt-2 text-sm">データブラウザでデモデータを再読み込みしてください</p>
          <p className="mt-4 text-xs opacity-50">データセット: {activeDataset.name}</p>
          <p className="text-xs opacity-50">ID: {activeDatasetId}</p>
        </div>
      </div>
    )
  }

  // Use generated test data or create simple mock if not available
  const signalData = generatedData?.signals ?? {}
  const timeData = generatedData?.time ?? []

  // Filter to visible channels only
  const displaySignals: Record<string, number[]> = {}
  visibleChannels.forEach((ch) => {
    if (signalData[ch]) {
      displaySignals[ch] = signalData[ch]
    }
  })

  console.log('SignalViewer main render:', {
    visibleChannelsCount: visibleChannels.length,
    displaySignalsCount: Object.keys(displaySignals).length,
    timeDataLength: timeData.length
  })

  return (
    <div className="flex h-full min-h-[500px] flex-col bg-background">
      <div className="border-b px-4 py-2">
        <div className="flex items-center gap-4">
          <h2 className="font-medium">{activeDataset.name}</h2>
          <span className="text-sm text-muted-foreground">
            {activeDataset.nChannels}ch · {activeDataset.samplingRate}Hz · {formatTime(activeDataset.duration)}
          </span>
          <span className="text-xs text-muted-foreground ml-auto">
            表示: {visibleChannels.length}チャンネル / {timeData.length}サンプル
          </span>
        </div>
      </div>

      <div className="flex flex-1 overflow-hidden min-h-[400px]">
        <ChannelList
          channels={channelLabels}
          visibleChannels={visibleChannels}
          onToggleChannel={handleToggleChannel}
          onSelectAll={() => setVisibleChannels(channelLabels)}
          onSelectNone={() => setVisibleChannels([])}
        />

        <div className="flex flex-1 flex-col">
          <div className="flex-1">
            <SignalCanvas
              signals={displaySignals}
              time={timeData}
              visibleChannels={visibleChannels}
              timeRange={signalViewer.timeRange}
              amplitude={signalViewer.amplitudeScale}
              cursorTime={syncedTime}
              onTimeClick={setCursorPosition}
            />
          </div>

          <ControlPanel
            timeRange={signalViewer.timeRange}
            totalDuration={activeDataset.duration}
            amplitude={signalViewer.amplitudeScale}
            displayMode={signalViewer.displayMode}
            onTimeRangeChange={setTimeRange}
            onAmplitudeChange={setAmplitudeScale}
            onDisplayModeChange={setDisplayMode}
          />
        </div>
      </div>
    </div>
  )
}
