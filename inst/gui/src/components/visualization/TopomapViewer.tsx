import { useEffect, useRef, useState, useMemo } from 'react'
import { useDataStore } from '@/stores/dataStore'
import { useVisualizationStore } from '@/stores/visualizationStore'
import { interpolateTopomap, electrode1020Positions, computeBandPower } from '@/lib/signalProcessing'
import { formatTime } from '@/lib/utils'

// Color maps for topomap
const topomapColors = {
  'RdBu': [
    [103, 0, 31], [178, 24, 43], [214, 96, 77], [244, 165, 130],
    [253, 219, 199], [247, 247, 247], [209, 229, 240], [146, 197, 222],
    [67, 147, 195], [33, 102, 172], [5, 48, 97]
  ].reverse(),
  'Spectral': [
    [158, 1, 66], [213, 62, 79], [244, 109, 67], [253, 174, 97],
    [254, 224, 139], [255, 255, 191], [230, 245, 152], [171, 221, 164],
    [102, 194, 165], [50, 136, 189], [94, 79, 162]
  ].reverse(),
  'Coolwarm': [
    [59, 76, 192], [98, 130, 234], [141, 176, 254], [184, 208, 249],
    [221, 221, 221], [245, 196, 173], [244, 154, 123], [222, 96, 77],
    [180, 4, 38]
  ]
}

function interpolateTopomapColor(value: number, colormap: number[][]): [number, number, number] {
  const n = colormap.length - 1
  const idx = Math.max(0, Math.min(1, (value + 1) / 2)) * n  // Map -1..1 to 0..1
  const lower = Math.floor(idx)
  const upper = Math.min(lower + 1, n)
  const t = idx - lower

  return [
    Math.round(colormap[lower][0] * (1 - t) + colormap[upper][0] * t),
    Math.round(colormap[lower][1] * (1 - t) + colormap[upper][1] * t),
    Math.round(colormap[lower][2] * (1 - t) + colormap[upper][2] * t)
  ]
}

interface TopomapCanvasProps {
  grid: number[][]
  electrodeValues: Record<string, number>
  colormap: keyof typeof topomapColors
  showElectrodes: boolean
  showLabels: boolean
  showContours: boolean
}

function TopomapCanvas({
  grid,
  electrodeValues,
  colormap,
  showElectrodes,
  showLabels,
  showContours
}: TopomapCanvasProps) {
  const canvasRef = useRef<HTMLCanvasElement>(null)
  const containerRef = useRef<HTMLDivElement>(null)

  useEffect(() => {
    const canvas = canvasRef.current
    const container = containerRef.current
    if (!canvas || !container || grid.length === 0) return

    const ctx = canvas.getContext('2d')
    if (!ctx) return

    const rect = container.getBoundingClientRect()
    const size = Math.min(rect.width, rect.height)
    const dpr = window.devicePixelRatio || 1
    canvas.width = size * dpr
    canvas.height = size * dpr
    canvas.style.width = `${size}px`
    canvas.style.height = `${size}px`
    ctx.scale(dpr, dpr)

    const padding = 40
    const plotSize = size - padding * 2
    const centerX = size / 2
    const centerY = size / 2
    const radius = plotSize / 2

    // Clear
    ctx.fillStyle = 'hsl(var(--background))'
    ctx.fillRect(0, 0, size, size)

    // Draw head outline
    ctx.strokeStyle = 'hsl(var(--foreground))'
    ctx.lineWidth = 2
    ctx.beginPath()
    ctx.arc(centerX, centerY, radius, 0, 2 * Math.PI)
    ctx.stroke()

    // Draw nose
    ctx.beginPath()
    ctx.moveTo(centerX - 15, centerY - radius)
    ctx.lineTo(centerX, centerY - radius - 20)
    ctx.lineTo(centerX + 15, centerY - radius)
    ctx.stroke()

    // Draw ears
    ctx.beginPath()
    ctx.ellipse(centerX - radius - 5, centerY, 8, 20, 0, 0, 2 * Math.PI)
    ctx.stroke()
    ctx.beginPath()
    ctx.ellipse(centerX + radius + 5, centerY, 8, 20, 0, 0, 2 * Math.PI)
    ctx.stroke()

    // Draw interpolated topomap
    const resolution = grid.length
    const imageData = ctx.createImageData(resolution, resolution)

    // Find data range for normalization
    let minVal = Infinity, maxVal = -Infinity
    for (const row of grid) {
      for (const val of row) {
        if (!isNaN(val)) {
          minVal = Math.min(minVal, val)
          maxVal = Math.max(maxVal, val)
        }
      }
    }
    const range = maxVal - minVal || 1

    for (let y = 0; y < resolution; y++) {
      for (let x = 0; x < resolution; x++) {
        const val = grid[y][x]
        const idx = (y * resolution + x) * 4

        if (isNaN(val)) {
          imageData.data[idx + 3] = 0  // Transparent
        } else {
          const normalized = (val - minVal) / range * 2 - 1  // -1 to 1
          const [r, g, b] = interpolateTopomapColor(normalized, topomapColors[colormap])
          imageData.data[idx] = r
          imageData.data[idx + 1] = g
          imageData.data[idx + 2] = b
          imageData.data[idx + 3] = 255
        }
      }
    }

    // Create temp canvas for scaling
    const tempCanvas = document.createElement('canvas')
    tempCanvas.width = resolution
    tempCanvas.height = resolution
    const tempCtx = tempCanvas.getContext('2d')!
    tempCtx.putImageData(imageData, 0, 0)

    // Draw scaled image with clipping
    ctx.save()
    ctx.beginPath()
    ctx.arc(centerX, centerY, radius, 0, 2 * Math.PI)
    ctx.clip()
    ctx.drawImage(tempCanvas, padding, padding, plotSize, plotSize)
    ctx.restore()

    // Draw contour lines if enabled
    if (showContours) {
      ctx.strokeStyle = 'rgba(0, 0, 0, 0.3)'
      ctx.lineWidth = 0.5
      const contourLevels = 5
      for (let level = 1; level < contourLevels; level++) {
        const threshold = minVal + (range * level) / contourLevels
        drawContour(ctx, grid, threshold, padding, plotSize, resolution)
      }
    }

    // Draw electrodes
    if (showElectrodes) {
      for (const [name, pos] of Object.entries(electrode1020Positions)) {
        if (!(name in electrodeValues)) continue

        const x = padding + pos.x * plotSize
        const y = padding + pos.y * plotSize

        // Electrode dot
        ctx.fillStyle = 'hsl(var(--foreground))'
        ctx.beginPath()
        ctx.arc(x, y, 4, 0, 2 * Math.PI)
        ctx.fill()

        // Label
        if (showLabels) {
          ctx.fillStyle = 'hsl(var(--foreground))'
          ctx.font = '9px Inter, sans-serif'
          ctx.textAlign = 'center'
          ctx.fillText(name, x, y - 8)
        }
      }
    }

    // Draw colorbar
    const colorbarX = size - 25
    const colorbarHeight = plotSize * 0.6
    const colorbarTop = (size - colorbarHeight) / 2

    for (let i = 0; i < colorbarHeight; i++) {
      const value = 1 - (i / colorbarHeight) * 2  // 1 to -1
      const [r, g, b] = interpolateTopomapColor(value, topomapColors[colormap])
      ctx.fillStyle = `rgb(${r},${g},${b})`
      ctx.fillRect(colorbarX, colorbarTop + i, 12, 1)
    }

    ctx.strokeStyle = 'hsl(var(--border))'
    ctx.strokeRect(colorbarX, colorbarTop, 12, colorbarHeight)

    ctx.fillStyle = 'hsl(var(--foreground))'
    ctx.font = '9px Inter, sans-serif'
    ctx.textAlign = 'left'
    ctx.fillText(`${maxVal.toFixed(1)}`, colorbarX - 25, colorbarTop + 4)
    ctx.fillText(`${minVal.toFixed(1)}`, colorbarX - 25, colorbarTop + colorbarHeight + 4)

  }, [grid, electrodeValues, colormap, showElectrodes, showLabels, showContours])

  return (
    <div ref={containerRef} className="h-full w-full flex items-center justify-center">
      <canvas ref={canvasRef} />
    </div>
  )
}

function drawContour(
  ctx: CanvasRenderingContext2D,
  grid: number[][],
  threshold: number,
  padding: number,
  plotSize: number,
  resolution: number
) {
  const scale = plotSize / resolution

  for (let y = 0; y < resolution - 1; y++) {
    for (let x = 0; x < resolution - 1; x++) {
      const v00 = grid[y][x]
      const v10 = grid[y][x + 1]
      const v01 = grid[y + 1][x]
      const v11 = grid[y + 1][x + 1]

      if (isNaN(v00) || isNaN(v10) || isNaN(v01) || isNaN(v11)) continue

      // Marching squares (simplified)
      const above00 = v00 >= threshold
      const above10 = v10 >= threshold
      const above01 = v01 >= threshold
      const above11 = v11 >= threshold

      const code = (above00 ? 1 : 0) + (above10 ? 2 : 0) + (above01 ? 4 : 0) + (above11 ? 8 : 0)

      if (code === 0 || code === 15) continue

      const px = padding + x * scale
      const py = padding + y * scale

      ctx.beginPath()

      // Draw contour segment based on marching squares code
      if (code === 1 || code === 14) {
        const t1 = (threshold - v00) / (v10 - v00)
        const t2 = (threshold - v00) / (v01 - v00)
        ctx.moveTo(px + t1 * scale, py)
        ctx.lineTo(px, py + t2 * scale)
      } else if (code === 2 || code === 13) {
        const t1 = (threshold - v00) / (v10 - v00)
        const t2 = (threshold - v10) / (v11 - v10)
        ctx.moveTo(px + t1 * scale, py)
        ctx.lineTo(px + scale, py + t2 * scale)
      }
      // ... more cases can be added for complete marching squares

      ctx.stroke()
    }
  }
}

export function TopomapViewer() {
  const { activeDatasetId, datasets, getGeneratedSignals } = useDataStore()
  useVisualizationStore()

  const [colormap, setColormap] = useState<keyof typeof topomapColors>('RdBu')
  const [showElectrodes, setShowElectrodes] = useState(true)
  const [showLabels, setShowLabels] = useState(true)
  const [showContours, setShowContours] = useState(false)
  const [displayMode, setDisplayMode] = useState<'amplitude' | 'power'>('amplitude')
  const [selectedBand, setSelectedBand] = useState<'delta' | 'theta' | 'alpha' | 'beta' | 'gamma'>('alpha')
  const [currentTime, setCurrentTime] = useState(0)
  const [isPlaying, setIsPlaying] = useState(false)

  const activeDataset = activeDatasetId ? datasets[activeDatasetId] : null
  const generatedData = activeDatasetId ? getGeneratedSignals(activeDatasetId) : null

  const channelLabels = activeDataset?.channels.map(ch => ch.label) ?? []

  // Animation
  useEffect(() => {
    if (!isPlaying || !activeDataset) return

    const interval = setInterval(() => {
      setCurrentTime(t => {
        const next = t + 0.1
        return next >= activeDataset.duration ? 0 : next
      })
    }, 100)

    return () => clearInterval(interval)
  }, [isPlaying, activeDataset])

  // Get electrode values at current time
  const electrodeValues = useMemo(() => {
    if (!generatedData || !activeDataset) return {}

    const values: Record<string, number> = {}
    const samplingRate = activeDataset.samplingRate
    const sampleIdx = Math.floor(currentTime * samplingRate)

    for (const ch of channelLabels) {
      const signal = generatedData.signals[ch]
      if (!signal || !electrode1020Positions[ch]) continue

      if (displayMode === 'amplitude') {
        // Get amplitude at current time
        const idx = Math.min(sampleIdx, signal.length - 1)
        values[ch] = signal[idx]
      } else {
        // Compute band power
        const bandRanges = {
          delta: [0.5, 4],
          theta: [4, 8],
          alpha: [8, 13],
          beta: [13, 30],
          gamma: [30, 100]
        }
        const [low, high] = bandRanges[selectedBand]

        // Use a window around current time
        const windowSize = Math.floor(samplingRate * 0.5)  // 500ms window
        const start = Math.max(0, sampleIdx - windowSize / 2)
        const end = Math.min(signal.length, sampleIdx + windowSize / 2)
        const windowSignal = signal.slice(start, end)

        values[ch] = computeBandPower(windowSignal, samplingRate, low, high)
      }
    }

    return values
  }, [generatedData, activeDataset, currentTime, displayMode, selectedBand, channelLabels])

  // Interpolate topomap
  const topomapGrid = useMemo(() => {
    if (Object.keys(electrodeValues).length === 0) return []
    return interpolateTopomap(electrodeValues, electrode1020Positions, 64)
  }, [electrodeValues])

  if (!activeDataset) {
    return (
      <div className="flex h-full items-center justify-center text-muted-foreground">
        <div className="text-center">
          <svg className="mx-auto h-12 w-12" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M9 20l-5.447-2.724A1 1 0 013 16.382V5.618a1 1 0 011.447-.894L9 7m0 13l6-3m-6 3V7m6 10l4.553 2.276A1 1 0 0021 18.382V7.618a1 1 0 00-.553-.894L15 4m0 13V4m0 0L9 7" />
          </svg>
          <p className="mt-4">EEGデータセットを選択してください</p>
        </div>
      </div>
    )
  }

  // Check if dataset has EEG channels
  const hasEEGChannels = channelLabels.some(ch => electrode1020Positions[ch])

  if (!hasEEGChannels) {
    return (
      <div className="flex h-full items-center justify-center text-muted-foreground">
        <div className="text-center">
          <p>このデータセットには10-20システムのEEG電極がありません</p>
          <p className="text-sm mt-2">利用可能なチャンネル: {channelLabels.join(', ')}</p>
        </div>
      </div>
    )
  }

  return (
    <div className="flex h-full flex-col">
      {/* Toolbar */}
      <div className="border-b px-4 py-2 flex items-center gap-4 flex-wrap">
        <div className="flex items-center gap-2">
          <label className="text-sm text-muted-foreground">表示:</label>
          <select
            value={displayMode}
            onChange={(e) => setDisplayMode(e.target.value as 'amplitude' | 'power')}
            className="rounded border bg-background px-2 py-1 text-sm"
          >
            <option value="amplitude">振幅</option>
            <option value="power">帯域パワー</option>
          </select>
        </div>

        {displayMode === 'power' && (
          <div className="flex items-center gap-2">
            <label className="text-sm text-muted-foreground">帯域:</label>
            <select
              value={selectedBand}
              onChange={(e) => setSelectedBand(e.target.value as typeof selectedBand)}
              className="rounded border bg-background px-2 py-1 text-sm"
            >
              <option value="delta">Delta (0.5-4 Hz)</option>
              <option value="theta">Theta (4-8 Hz)</option>
              <option value="alpha">Alpha (8-13 Hz)</option>
              <option value="beta">Beta (13-30 Hz)</option>
              <option value="gamma">Gamma (30-100 Hz)</option>
            </select>
          </div>
        )}

        <div className="flex items-center gap-2">
          <label className="text-sm text-muted-foreground">カラーマップ:</label>
          <select
            value={colormap}
            onChange={(e) => setColormap(e.target.value as keyof typeof topomapColors)}
            className="rounded border bg-background px-2 py-1 text-sm"
          >
            <option value="RdBu">Red-Blue</option>
            <option value="Spectral">Spectral</option>
            <option value="Coolwarm">Cool-Warm</option>
          </select>
        </div>

        <div className="flex items-center gap-3">
          <label className="flex items-center gap-1 text-sm">
            <input
              type="checkbox"
              checked={showElectrodes}
              onChange={(e) => setShowElectrodes(e.target.checked)}
              className="rounded"
            />
            電極
          </label>
          <label className="flex items-center gap-1 text-sm">
            <input
              type="checkbox"
              checked={showLabels}
              onChange={(e) => setShowLabels(e.target.checked)}
              className="rounded"
            />
            ラベル
          </label>
          <label className="flex items-center gap-1 text-sm">
            <input
              type="checkbox"
              checked={showContours}
              onChange={(e) => setShowContours(e.target.checked)}
              className="rounded"
            />
            等高線
          </label>
        </div>
      </div>

      {/* Main content */}
      <div className="flex-1 flex">
        {/* Topomap */}
        <div className="flex-1 p-4">
          <div className="h-full">
            <TopomapCanvas
              grid={topomapGrid}
              electrodeValues={electrodeValues}
              colormap={colormap}
              showElectrodes={showElectrodes}
              showLabels={showLabels}
              showContours={showContours}
            />
          </div>
        </div>

        {/* Controls */}
        <div className="w-64 border-l p-4">
          <h3 className="text-sm font-medium mb-4">時間コントロール</h3>

          <div className="space-y-4">
            <div>
              <label className="text-sm text-muted-foreground">現在時刻</label>
              <div className="mt-1 text-2xl font-mono">{formatTime(currentTime)}</div>
            </div>

            <div>
              <input
                type="range"
                min={0}
                max={activeDataset.duration}
                step={0.01}
                value={currentTime}
                onChange={(e) => setCurrentTime(Number(e.target.value))}
                className="w-full"
              />
            </div>

            <div className="flex gap-2">
              <button
                onClick={() => setCurrentTime(0)}
                className="rounded border px-3 py-1 text-sm hover:bg-accent"
              >
                ⏮
              </button>
              <button
                onClick={() => setIsPlaying(!isPlaying)}
                className="rounded bg-primary px-4 py-1 text-sm text-primary-foreground hover:bg-primary/90"
              >
                {isPlaying ? '⏸ 停止' : '▶ 再生'}
              </button>
              <button
                onClick={() => setCurrentTime(activeDataset.duration)}
                className="rounded border px-3 py-1 text-sm hover:bg-accent"
              >
                ⏭
              </button>
            </div>

            <div>
              <label className="text-sm text-muted-foreground">データセット</label>
              <p className="mt-1 text-sm font-medium">{activeDataset.name}</p>
              <p className="text-xs text-muted-foreground">
                {activeDataset.nChannels}ch · {activeDataset.samplingRate}Hz · {formatTime(activeDataset.duration)}
              </p>
            </div>

            <div>
              <label className="text-sm text-muted-foreground">検出電極</label>
              <p className="mt-1 text-sm">
                {Object.keys(electrodeValues).length} / {channelLabels.length}
              </p>
              <div className="mt-2 max-h-32 overflow-auto">
                <div className="flex flex-wrap gap-1">
                  {Object.entries(electrodeValues).map(([ch, val]) => (
                    <span key={ch} className="rounded bg-secondary px-1.5 py-0.5 text-xs">
                      {ch}: {val.toFixed(1)}
                    </span>
                  ))}
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}
