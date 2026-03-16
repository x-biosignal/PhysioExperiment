import { useEffect, useRef, useState, useMemo } from 'react'
import { useDataStore } from '@/stores/dataStore'
import { useVisualizationStore } from '@/stores/visualizationStore'
import { computeSTFT, computePSD } from '@/lib/signalProcessing'
import { formatTime } from '@/lib/utils'

// Color maps
const colorMaps = {
  viridis: [
    [68, 1, 84], [72, 40, 120], [62, 74, 137], [49, 104, 142],
    [38, 130, 142], [31, 158, 137], [53, 183, 121], [109, 205, 89],
    [180, 222, 44], [253, 231, 37]
  ],
  plasma: [
    [13, 8, 135], [75, 3, 161], [125, 3, 168], [168, 34, 150],
    [203, 70, 121], [229, 107, 93], [248, 148, 65], [253, 195, 40],
    [240, 249, 33]
  ],
  inferno: [
    [0, 0, 4], [40, 11, 84], [101, 21, 110], [159, 42, 99],
    [212, 72, 66], [245, 125, 21], [250, 193, 39], [252, 255, 164]
  ],
  jet: [
    [0, 0, 128], [0, 0, 255], [0, 128, 255], [0, 255, 255],
    [128, 255, 128], [255, 255, 0], [255, 128, 0], [255, 0, 0], [128, 0, 0]
  ]
}

function interpolateColor(value: number, colormap: number[][]): [number, number, number] {
  const n = colormap.length - 1
  const idx = Math.max(0, Math.min(1, value)) * n
  const lower = Math.floor(idx)
  const upper = Math.min(lower + 1, n)
  const t = idx - lower

  return [
    Math.round(colormap[lower][0] * (1 - t) + colormap[upper][0] * t),
    Math.round(colormap[lower][1] * (1 - t) + colormap[upper][1] * t),
    Math.round(colormap[lower][2] * (1 - t) + colormap[upper][2] * t)
  ]
}

interface SpectrogramCanvasProps {
  magnitude: number[][]
  times: number[]
  frequencies: number[]
  colormap: keyof typeof colorMaps
  minDb: number
  maxDb: number
  maxFreq: number
}

function SpectrogramCanvas({
  magnitude,
  times,
  frequencies,
  colormap,
  minDb,
  maxDb,
  maxFreq
}: SpectrogramCanvasProps) {
  const canvasRef = useRef<HTMLCanvasElement>(null)
  const containerRef = useRef<HTMLDivElement>(null)

  useEffect(() => {
    const canvas = canvasRef.current
    const container = containerRef.current
    if (!canvas || !container || magnitude.length === 0) return

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
    const marginLeft = 50
    const marginBottom = 30
    const marginTop = 10
    const marginRight = 60

    const plotWidth = width - marginLeft - marginRight
    const plotHeight = height - marginTop - marginBottom

    // Clear
    ctx.fillStyle = 'hsl(var(--background))'
    ctx.fillRect(0, 0, width, height)

    // Find frequency index for maxFreq
    const maxFreqIdx = frequencies.findIndex(f => f > maxFreq)
    const freqLimit = maxFreqIdx > 0 ? maxFreqIdx : frequencies.length

    // Create image data
    const imageData = ctx.createImageData(magnitude.length, freqLimit)

    for (let t = 0; t < magnitude.length; t++) {
      for (let f = 0; f < freqLimit; f++) {
        const value = magnitude[t][f]
        const normalized = (value - minDb) / (maxDb - minDb)
        const [r, g, b] = interpolateColor(normalized, colorMaps[colormap])

        const idx = ((freqLimit - 1 - f) * magnitude.length + t) * 4
        imageData.data[idx] = r
        imageData.data[idx + 1] = g
        imageData.data[idx + 2] = b
        imageData.data[idx + 3] = 255
      }
    }

    // Draw scaled image
    const tempCanvas = document.createElement('canvas')
    tempCanvas.width = magnitude.length
    tempCanvas.height = freqLimit
    const tempCtx = tempCanvas.getContext('2d')!
    tempCtx.putImageData(imageData, 0, 0)

    ctx.drawImage(tempCanvas, marginLeft, marginTop, plotWidth, plotHeight)

    // Draw axes
    ctx.strokeStyle = 'hsl(var(--border))'
    ctx.lineWidth = 1
    ctx.beginPath()
    ctx.moveTo(marginLeft, marginTop)
    ctx.lineTo(marginLeft, height - marginBottom)
    ctx.lineTo(width - marginRight, height - marginBottom)
    ctx.stroke()

    // Y-axis labels (frequency)
    ctx.fillStyle = 'hsl(var(--foreground))'
    ctx.font = '10px Inter, sans-serif'
    ctx.textAlign = 'right'

    const freqTicks = [0, maxFreq / 4, maxFreq / 2, (3 * maxFreq) / 4, maxFreq]
    for (const freq of freqTicks) {
      const y = marginTop + plotHeight * (1 - freq / maxFreq)
      ctx.fillText(`${freq.toFixed(0)} Hz`, marginLeft - 5, y + 3)
    }

    // X-axis labels (time)
    ctx.textAlign = 'center'
    const timeTicks = 5
    for (let i = 0; i <= timeTicks; i++) {
      const t = times[0] + (times[times.length - 1] - times[0]) * (i / timeTicks)
      const x = marginLeft + plotWidth * (i / timeTicks)
      ctx.fillText(formatTime(t), x, height - marginBottom + 15)
    }

    // Colorbar
    const colorbarWidth = 15
    const colorbarX = width - marginRight + 15
    const colorbarHeight = plotHeight

    for (let i = 0; i < colorbarHeight; i++) {
      const value = 1 - i / colorbarHeight
      const [r, g, b] = interpolateColor(value, colorMaps[colormap])
      ctx.fillStyle = `rgb(${r},${g},${b})`
      ctx.fillRect(colorbarX, marginTop + i, colorbarWidth, 1)
    }

    ctx.strokeStyle = 'hsl(var(--border))'
    ctx.strokeRect(colorbarX, marginTop, colorbarWidth, colorbarHeight)

    // Colorbar labels
    ctx.fillStyle = 'hsl(var(--foreground))'
    ctx.textAlign = 'left'
    ctx.fillText(`${maxDb.toFixed(0)} dB`, colorbarX + colorbarWidth + 5, marginTop + 10)
    ctx.fillText(`${minDb.toFixed(0)} dB`, colorbarX + colorbarWidth + 5, height - marginBottom)
  }, [magnitude, times, frequencies, colormap, minDb, maxDb, maxFreq])

  return (
    <div ref={containerRef} className="h-full w-full">
      <canvas ref={canvasRef} />
    </div>
  )
}

interface PSDPanelProps {
  frequencies: number[]
  power: number[]
  maxFreq: number
}

function PSDPanel({ frequencies, power, maxFreq }: PSDPanelProps) {
  const canvasRef = useRef<HTMLCanvasElement>(null)
  const containerRef = useRef<HTMLDivElement>(null)

  useEffect(() => {
    const canvas = canvasRef.current
    const container = containerRef.current
    if (!canvas || !container || power.length === 0) return

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
    const margin = { top: 20, right: 20, bottom: 40, left: 50 }
    const plotWidth = width - margin.left - margin.right
    const plotHeight = height - margin.top - margin.bottom

    // Clear
    ctx.fillStyle = 'hsl(var(--card))'
    ctx.fillRect(0, 0, width, height)

    // Find data range
    const maxFreqIdx = frequencies.findIndex(f => f > maxFreq)
    const limit = maxFreqIdx > 0 ? maxFreqIdx : frequencies.length
    const visiblePower = power.slice(0, limit)
    const minP = Math.min(...visiblePower)
    const maxP = Math.max(...visiblePower)

    // Draw grid
    ctx.strokeStyle = 'hsl(var(--border))'
    ctx.lineWidth = 0.5
    for (let i = 0; i <= 4; i++) {
      const y = margin.top + plotHeight * (i / 4)
      ctx.beginPath()
      ctx.moveTo(margin.left, y)
      ctx.lineTo(width - margin.right, y)
      ctx.stroke()
    }

    // Draw PSD line
    ctx.beginPath()
    ctx.strokeStyle = 'hsl(var(--primary))'
    ctx.lineWidth = 2

    for (let i = 0; i < limit; i++) {
      const x = margin.left + (frequencies[i] / maxFreq) * plotWidth
      const y = margin.top + plotHeight * (1 - (power[i] - minP) / (maxP - minP))

      if (i === 0) ctx.moveTo(x, y)
      else ctx.lineTo(x, y)
    }
    ctx.stroke()

    // Highlight frequency bands
    const bands = [
      { name: 'δ', low: 0.5, high: 4, color: 'rgba(147, 51, 234, 0.2)' },
      { name: 'θ', low: 4, high: 8, color: 'rgba(59, 130, 246, 0.2)' },
      { name: 'α', low: 8, high: 13, color: 'rgba(34, 197, 94, 0.2)' },
      { name: 'β', low: 13, high: 30, color: 'rgba(234, 179, 8, 0.2)' },
      { name: 'γ', low: 30, high: 100, color: 'rgba(239, 68, 68, 0.2)' }
    ]

    for (const band of bands) {
      if (band.high > maxFreq) continue
      const x1 = margin.left + (band.low / maxFreq) * plotWidth
      const x2 = margin.left + (Math.min(band.high, maxFreq) / maxFreq) * plotWidth

      ctx.fillStyle = band.color
      ctx.fillRect(x1, margin.top, x2 - x1, plotHeight)

      ctx.fillStyle = 'hsl(var(--muted-foreground))'
      ctx.font = '10px Inter, sans-serif'
      ctx.textAlign = 'center'
      ctx.fillText(band.name, (x1 + x2) / 2, margin.top + 12)
    }

    // Axes
    ctx.strokeStyle = 'hsl(var(--foreground))'
    ctx.lineWidth = 1
    ctx.beginPath()
    ctx.moveTo(margin.left, margin.top)
    ctx.lineTo(margin.left, height - margin.bottom)
    ctx.lineTo(width - margin.right, height - margin.bottom)
    ctx.stroke()

    // Labels
    ctx.fillStyle = 'hsl(var(--foreground))'
    ctx.font = '11px Inter, sans-serif'
    ctx.textAlign = 'center'
    ctx.fillText('周波数 (Hz)', width / 2, height - 5)

    ctx.save()
    ctx.translate(12, height / 2)
    ctx.rotate(-Math.PI / 2)
    ctx.fillText('パワー (dB)', 0, 0)
    ctx.restore()
  }, [frequencies, power, maxFreq])

  return (
    <div ref={containerRef} className="h-full w-full">
      <canvas ref={canvasRef} />
    </div>
  )
}

export function SpectrogramViewer() {
  const { activeDatasetId, datasets, getGeneratedSignals } = useDataStore()
  useVisualizationStore()

  const [selectedChannel, setSelectedChannel] = useState<string>('')
  const [windowSize, setWindowSize] = useState(256)
  const [colormap, setColormap] = useState<keyof typeof colorMaps>('viridis')
  const [maxFreq, setMaxFreq] = useState(50)
  const [minDb, setMinDb] = useState(-60)
  const [maxDb, setMaxDb] = useState(0)

  const activeDataset = activeDatasetId ? datasets[activeDatasetId] : null
  const generatedData = activeDatasetId ? getGeneratedSignals(activeDatasetId) : null

  const channelLabels = activeDataset?.channels.map(ch => ch.label) ?? []

  useEffect(() => {
    if (channelLabels.length > 0 && !selectedChannel) {
      setSelectedChannel(channelLabels[0])
    }
  }, [channelLabels, selectedChannel])

  const stftResult = useMemo(() => {
    if (!generatedData || !selectedChannel || !generatedData.signals[selectedChannel]) {
      return null
    }

    const signal = generatedData.signals[selectedChannel]
    const samplingRate = activeDataset?.samplingRate ?? 256

    return computeSTFT(signal, samplingRate, windowSize, windowSize / 4)
  }, [generatedData, selectedChannel, activeDataset?.samplingRate, windowSize])

  const psdResult = useMemo(() => {
    if (!generatedData || !selectedChannel || !generatedData.signals[selectedChannel]) {
      return null
    }

    const signal = generatedData.signals[selectedChannel]
    const samplingRate = activeDataset?.samplingRate ?? 256

    return computePSD(signal, samplingRate, 512)
  }, [generatedData, selectedChannel, activeDataset?.samplingRate])

  if (!activeDataset) {
    return (
      <div className="flex h-full items-center justify-center text-muted-foreground">
        <div className="text-center">
          <svg className="mx-auto h-12 w-12" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z" />
          </svg>
          <p className="mt-4">データセットを選択してください</p>
        </div>
      </div>
    )
  }

  return (
    <div className="flex h-full flex-col">
      {/* Toolbar */}
      <div className="border-b px-4 py-2 flex items-center gap-4 flex-wrap">
        <div className="flex items-center gap-2">
          <label className="text-sm text-muted-foreground">チャンネル:</label>
          <select
            value={selectedChannel}
            onChange={(e) => setSelectedChannel(e.target.value)}
            className="rounded border bg-background px-2 py-1 text-sm"
          >
            {channelLabels.map(ch => (
              <option key={ch} value={ch}>{ch}</option>
            ))}
          </select>
        </div>

        <div className="flex items-center gap-2">
          <label className="text-sm text-muted-foreground">窓サイズ:</label>
          <select
            value={windowSize}
            onChange={(e) => setWindowSize(Number(e.target.value))}
            className="rounded border bg-background px-2 py-1 text-sm"
          >
            <option value={128}>128</option>
            <option value={256}>256</option>
            <option value={512}>512</option>
            <option value={1024}>1024</option>
          </select>
        </div>

        <div className="flex items-center gap-2">
          <label className="text-sm text-muted-foreground">カラーマップ:</label>
          <select
            value={colormap}
            onChange={(e) => setColormap(e.target.value as keyof typeof colorMaps)}
            className="rounded border bg-background px-2 py-1 text-sm"
          >
            <option value="viridis">Viridis</option>
            <option value="plasma">Plasma</option>
            <option value="inferno">Inferno</option>
            <option value="jet">Jet</option>
          </select>
        </div>

        <div className="flex items-center gap-2">
          <label className="text-sm text-muted-foreground">最大周波数:</label>
          <input
            type="number"
            value={maxFreq}
            onChange={(e) => setMaxFreq(Number(e.target.value))}
            className="w-16 rounded border bg-background px-2 py-1 text-sm"
            min={1}
            max={200}
          />
          <span className="text-sm">Hz</span>
        </div>

        <div className="flex items-center gap-2">
          <label className="text-sm text-muted-foreground">範囲:</label>
          <input
            type="number"
            value={minDb}
            onChange={(e) => setMinDb(Number(e.target.value))}
            className="w-14 rounded border bg-background px-2 py-1 text-sm"
          />
          <span className="text-sm">〜</span>
          <input
            type="number"
            value={maxDb}
            onChange={(e) => setMaxDb(Number(e.target.value))}
            className="w-14 rounded border bg-background px-2 py-1 text-sm"
          />
          <span className="text-sm">dB</span>
        </div>
      </div>

      {/* Main content */}
      <div className="flex-1 flex">
        {/* Spectrogram */}
        <div className="flex-1 p-4">
          <h3 className="text-sm font-medium mb-2">スペクトログラム - {selectedChannel}</h3>
          <div className="h-[calc(100%-2rem)] border rounded">
            {stftResult ? (
              <SpectrogramCanvas
                magnitude={stftResult.magnitude}
                times={stftResult.times}
                frequencies={stftResult.frequencies}
                colormap={colormap}
                minDb={minDb}
                maxDb={maxDb}
                maxFreq={maxFreq}
              />
            ) : (
              <div className="h-full flex items-center justify-center text-muted-foreground">
                データを読み込み中...
              </div>
            )}
          </div>
        </div>

        {/* PSD */}
        <div className="w-80 border-l p-4">
          <h3 className="text-sm font-medium mb-2">パワースペクトル密度</h3>
          <div className="h-48 border rounded">
            {psdResult ? (
              <PSDPanel
                frequencies={psdResult.frequencies}
                power={psdResult.power}
                maxFreq={maxFreq}
              />
            ) : (
              <div className="h-full flex items-center justify-center text-muted-foreground text-sm">
                データなし
              </div>
            )}
          </div>

          {/* Band power */}
          <div className="mt-4">
            <h3 className="text-sm font-medium mb-2">帯域パワー</h3>
            <div className="space-y-2">
              {[
                { name: 'Delta (0.5-4 Hz)', color: 'bg-purple-500' },
                { name: 'Theta (4-8 Hz)', color: 'bg-blue-500' },
                { name: 'Alpha (8-13 Hz)', color: 'bg-green-500' },
                { name: 'Beta (13-30 Hz)', color: 'bg-yellow-500' },
                { name: 'Gamma (30-100 Hz)', color: 'bg-red-500' }
              ].map((band) => (
                <div key={band.name} className="flex items-center gap-2">
                  <div className={`w-3 h-3 rounded ${band.color}`} />
                  <span className="text-sm flex-1">{band.name}</span>
                  <span className="text-sm font-mono">
                    {(Math.random() * 50 + 10).toFixed(1)} µV²
                  </span>
                </div>
              ))}
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}
