import { useState, useEffect, useRef } from 'react'
import { useDataStore } from '@/stores/dataStore'
import {
  bandpassFilter,
  notchFilter,
  extractEpochs,
  computeERP,
  baselineCorrect,
  pointwiseTTest,
  computeConnectivityMatrix,
  computeSTFT,
  computePSD,
  waveletTransform,
  extractBandPowers,
  computePLI,
  computeWPLI,
  computeCoherence,
  computeCorrelationMatrix,
  createAdjacencyMatrix,
  computeNodeDegree,
  computeClusteringCoefficient,
  computeCharacteristicPathLength,
  computeGlobalEfficiency,
  computeBetweennessCentrality,
  oneWayANOVA,
  cohensD,
  fdrCorrection,
  clusterPermutationTest,
  dtwDistance,
  computePCA,
  normalizeToPercent,
  detectThresholdEvents,
  segmentPhases
} from '@/lib/signalProcessing'

type TabId = 'preprocess' | 'timefreq' | 'epoch' | 'connectivity' | 'network' | 'statistics' | 'advanced' | 'dimred' | 'biomech'

interface TabProps {
  label: string
  isActive: boolean
  onClick: () => void
}

function Tab({ label, isActive, onClick }: TabProps) {
  return (
    <button
      onClick={onClick}
      className={`px-4 py-2 text-sm font-medium border-b-2 transition-colors ${
        isActive
          ? 'border-primary text-primary'
          : 'border-transparent text-muted-foreground hover:text-foreground'
      }`}
    >
      {label}
    </button>
  )
}

// ========== Preprocess Panel ==========
function PreprocessPanel() {
  const { activeDatasetId, datasets, generatedSignals, ensureGeneratedSignals } = useDataStore()
  const activeDataset = activeDatasetId ? datasets[activeDatasetId] : null
  const generatedData = activeDatasetId ? generatedSignals[activeDatasetId] ?? null : null

  // Ensure signals are generated for demo datasets
  useEffect(() => {
    if (activeDatasetId && activeDataset?.metadata?.isDemo && !generatedData) {
      ensureGeneratedSignals(activeDatasetId)
    }
  }, [activeDatasetId, activeDataset, generatedData, ensureGeneratedSignals])

  const [lowFreq, setLowFreq] = useState(1)
  const [highFreq, setHighFreq] = useState(40)
  const [selectedChannel, setSelectedChannel] = useState('')
  const [notchFreq, setNotchFreq] = useState<50 | 60 | null>(null)
  const [filteredSignal, setFilteredSignal] = useState<number[] | null>(null)
  const [isProcessing, setIsProcessing] = useState(false)

  const canvasRef = useRef<HTMLCanvasElement>(null)
  const containerRef = useRef<HTMLDivElement>(null)

  const channelLabels = activeDataset?.channels.map(ch => ch.label) ?? []

  useEffect(() => {
    if (channelLabels.length > 0 && !selectedChannel) {
      setSelectedChannel(channelLabels[0])
    }
  }, [channelLabels, selectedChannel])

  const handleApplyFilter = () => {
    console.log('handleApplyFilter called', { generatedData: !!generatedData, selectedChannel, activeDataset: !!activeDataset })
    if (!generatedData || !selectedChannel || !activeDataset) {
      console.log('Missing data - cannot apply filter')
      return
    }

    setIsProcessing(true)
    setTimeout(() => {
      let signal = generatedData.signals[selectedChannel]
      console.log('Signal to filter:', { channel: selectedChannel, signalLength: signal?.length })
      if (!signal) {
        console.log('No signal found for channel:', selectedChannel)
        setIsProcessing(false)
        return
      }

      // Apply bandpass filter
      console.log('Applying bandpass filter:', { lowFreq, highFreq, samplingRate: activeDataset.samplingRate })
      signal = bandpassFilter(signal, activeDataset.samplingRate, lowFreq, highFreq)
      console.log('Filtered signal length:', signal.length)

      // Apply notch filter if selected
      if (notchFreq) {
        signal = notchFilter(signal, activeDataset.samplingRate, notchFreq)
      }

      setFilteredSignal(signal)
      console.log('Filter applied, filteredSignal set')
      setIsProcessing(false)
    }, 100)
  }

  // Draw comparison
  useEffect(() => {
    console.log('Draw useEffect triggered', {
      canvas: !!canvasRef.current,
      container: !!containerRef.current,
      generatedData: !!generatedData,
      selectedChannel,
      filteredSignal: !!filteredSignal
    })
    const canvas = canvasRef.current
    const container = containerRef.current
    if (!canvas || !container || !generatedData || !selectedChannel) {
      console.log('Missing elements for drawing')
      return
    }

    const originalSignal = generatedData.signals[selectedChannel]
    if (!originalSignal) {
      console.log('No original signal for channel:', selectedChannel)
      return
    }
    console.log('Drawing with originalSignal length:', originalSignal.length)

    const ctx = canvas.getContext('2d')
    if (!ctx) return

    const rect = container.getBoundingClientRect()
    console.log('Container rect:', { width: rect.width, height: rect.height })
    const dpr = window.devicePixelRatio || 1
    canvas.width = rect.width * dpr
    canvas.height = rect.height * dpr
    canvas.style.width = `${rect.width}px`
    canvas.style.height = `${rect.height}px`
    ctx.scale(dpr, dpr)

    const width = rect.width
    const height = rect.height
    const margin = { top: 20, right: 20, bottom: 30, left: 60 }

    // Use explicit colors for canvas (CSS variables don't work in canvas)
    ctx.fillStyle = '#f8fafc'
    ctx.fillRect(0, 0, width, height)

    // Show first 2 seconds
    const samplingRate = activeDataset?.samplingRate ?? 256
    const samples = Math.min(samplingRate * 2, originalSignal.length)
    const time = generatedData.time.slice(0, samples)

    // Find data range
    const originalSlice = originalSignal.slice(0, samples)
    const filteredSlice = filteredSignal?.slice(0, samples)

    let minVal = Math.min(...originalSlice)
    let maxVal = Math.max(...originalSlice)
    if (filteredSlice) {
      minVal = Math.min(minVal, ...filteredSlice)
      maxVal = Math.max(maxVal, ...filteredSlice)
    }
    const range = maxVal - minVal || 1

    const plotWidth = width - margin.left - margin.right
    const plotHeight = height - margin.top - margin.bottom

    // Draw grid
    ctx.strokeStyle = '#e2e8f0'
    ctx.lineWidth = 0.5
    for (let i = 0; i <= 4; i++) {
      const y = margin.top + (plotHeight * i) / 4
      ctx.beginPath()
      ctx.moveTo(margin.left, y)
      ctx.lineTo(width - margin.right, y)
      ctx.stroke()
    }

    // Draw original signal
    ctx.beginPath()
    ctx.strokeStyle = '#94a3b8'
    ctx.lineWidth = 1
    for (let i = 0; i < samples; i++) {
      const x = margin.left + (i / samples) * plotWidth
      const y = margin.top + plotHeight * (1 - (originalSlice[i] - minVal) / range)
      if (i === 0) ctx.moveTo(x, y)
      else ctx.lineTo(x, y)
    }
    ctx.stroke()

    // Draw filtered signal
    if (filteredSlice) {
      ctx.beginPath()
      ctx.strokeStyle = '#3b82f6'
      ctx.lineWidth = 1.5
      for (let i = 0; i < samples; i++) {
        const x = margin.left + (i / samples) * plotWidth
        const y = margin.top + plotHeight * (1 - (filteredSlice[i] - minVal) / range)
        if (i === 0) ctx.moveTo(x, y)
        else ctx.lineTo(x, y)
      }
      ctx.stroke()
    }

    // Legend
    ctx.fillStyle = '#94a3b8'
    ctx.font = '11px Inter, sans-serif'
    ctx.fillText('原信号', margin.left, margin.top - 5)
    if (filteredSignal) {
      ctx.fillStyle = '#3b82f6'
      ctx.fillText('フィルタ後', margin.left + 60, margin.top - 5)
    }

    // X axis labels
    ctx.fillStyle = '#1e293b'
    ctx.textAlign = 'center'
    for (let i = 0; i <= 4; i++) {
      const t = (time[samples - 1] * i) / 4
      ctx.fillText(t.toFixed(1) + 's', margin.left + (plotWidth * i) / 4, height - 5)
    }

    // Y axis label
    ctx.save()
    ctx.translate(12, height / 2)
    ctx.rotate(-Math.PI / 2)
    ctx.textAlign = 'center'
    ctx.fillText('振幅 (µV)', 0, 0)
    ctx.restore()

  }, [generatedData, selectedChannel, filteredSignal, activeDataset])

  return (
    <div className="space-y-6">
      {/* Filter Settings */}
      <section className="rounded-lg border p-4">
        <h3 className="font-medium">バンドパスフィルタ</h3>
        <div className="mt-4 grid gap-4 md:grid-cols-4">
          <div>
            <label className="block text-sm">チャンネル</label>
            <select
              value={selectedChannel}
              onChange={(e) => { setSelectedChannel(e.target.value); setFilteredSignal(null) }}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
            >
              {channelLabels.map(ch => (
                <option key={ch} value={ch}>{ch}</option>
              ))}
            </select>
          </div>
          <div>
            <label className="block text-sm">低域カットオフ (Hz)</label>
            <input
              type="number"
              value={lowFreq}
              onChange={(e) => setLowFreq(parseFloat(e.target.value))}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
              step="0.1"
              min="0.1"
            />
          </div>
          <div>
            <label className="block text-sm">高域カットオフ (Hz)</label>
            <input
              type="number"
              value={highFreq}
              onChange={(e) => setHighFreq(parseFloat(e.target.value))}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
              step="1"
              min="1"
            />
          </div>
          <div>
            <label className="block text-sm">ノッチフィルタ</label>
            <select
              value={notchFreq ?? ''}
              onChange={(e) => setNotchFreq(e.target.value ? Number(e.target.value) as 50 | 60 : null)}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
            >
              <option value="">なし</option>
              <option value="50">50 Hz (日本/欧州)</option>
              <option value="60">60 Hz (北米)</option>
            </select>
          </div>
        </div>
        <button
          onClick={handleApplyFilter}
          disabled={isProcessing || !selectedChannel}
          className="mt-4 rounded-md bg-primary px-4 py-2 text-sm text-primary-foreground hover:bg-primary/90 disabled:opacity-50"
        >
          {isProcessing ? '処理中...' : 'フィルタを適用'}
        </button>
      </section>

      {/* Preview */}
      <section className="rounded-lg border p-4">
        <h3 className="font-medium mb-4">プレビュー (最初の2秒)</h3>
        <div ref={containerRef} className="h-64 w-full">
          <canvas ref={canvasRef} />
        </div>
      </section>
    </div>
  )
}

// ========== Time-Frequency Panel ==========
function TimeFreqPanel() {
  const { activeDatasetId, datasets, generatedSignals, ensureGeneratedSignals } = useDataStore()
  const activeDataset = activeDatasetId ? datasets[activeDatasetId] : null
  const generatedData = activeDatasetId ? generatedSignals[activeDatasetId] ?? null : null

  useEffect(() => {
    if (activeDatasetId && activeDataset?.metadata?.isDemo && !generatedData) {
      ensureGeneratedSignals(activeDatasetId)
    }
  }, [activeDatasetId, activeDataset, generatedData, ensureGeneratedSignals])

  const [selectedChannel, setSelectedChannel] = useState('')
  const [analysisType, setAnalysisType] = useState<'spectrogram' | 'wavelet' | 'psd' | 'bandpower'>('spectrogram')
  const [result, setResult] = useState<unknown>(null)
  const [isProcessing, setIsProcessing] = useState(false)

  const canvasRef = useRef<HTMLCanvasElement>(null)
  const containerRef = useRef<HTMLDivElement>(null)

  const channelLabels = activeDataset?.channels.map(ch => ch.label) ?? []

  useEffect(() => {
    if (channelLabels.length > 0 && !selectedChannel) {
      setSelectedChannel(channelLabels[0])
    }
  }, [channelLabels, selectedChannel])

  const handleAnalyze = () => {
    if (!generatedData || !selectedChannel || !activeDataset) return

    setIsProcessing(true)
    setTimeout(() => {
      const signal = generatedData.signals[selectedChannel]
      if (!signal) {
        setIsProcessing(false)
        return
      }

      const samplingRate = activeDataset.samplingRate
      let analysisResult: unknown

      switch (analysisType) {
        case 'spectrogram':
          analysisResult = computeSTFT(signal, samplingRate, 256, 32)
          break
        case 'wavelet':
          const freqs = Array.from({ length: 30 }, (_, i) => 1 + i * 2) // 1-59 Hz
          analysisResult = waveletTransform(signal, samplingRate, freqs)
          break
        case 'psd':
          analysisResult = computePSD(signal, samplingRate)
          break
        case 'bandpower':
          analysisResult = extractBandPowers(signal, samplingRate)
          break
      }

      setResult(analysisResult)
      setIsProcessing(false)
    }, 100)
  }

  // Draw results
  useEffect(() => {
    const canvas = canvasRef.current
    const container = containerRef.current
    if (!canvas || !container || !result) return

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
    const margin = { top: 30, right: 60, bottom: 40, left: 60 }

    ctx.fillStyle = '#f8fafc'
    ctx.fillRect(0, 0, width, height)

    const plotWidth = width - margin.left - margin.right
    const plotHeight = height - margin.top - margin.bottom

    if (analysisType === 'spectrogram' || analysisType === 'wavelet') {
      const data = result as { frequencies: number[]; times: number[]; magnitude?: number[][]; power?: number[][] }
      const matrix = data.magnitude || data.power
      if (!matrix || matrix.length === 0) return

      const nTimes = matrix.length
      const nFreqs = matrix[0].length

      // Find min/max
      let minVal = Infinity, maxVal = -Infinity
      for (const row of matrix) {
        for (const val of row) {
          if (isFinite(val)) {
            minVal = Math.min(minVal, val)
            maxVal = Math.max(maxVal, val)
          }
        }
      }
      const range = maxVal - minVal || 1

      // Draw heatmap
      const cellWidth = plotWidth / nTimes
      const cellHeight = plotHeight / nFreqs

      for (let t = 0; t < nTimes; t++) {
        for (let f = 0; f < nFreqs; f++) {
          const val = matrix[t][f]
          if (!isFinite(val)) continue

          const norm = (val - minVal) / range
          const r = Math.round(255 * norm)
          const g = Math.round(100 * (1 - norm))
          const b = Math.round(255 * (1 - norm))
          ctx.fillStyle = `rgb(${r}, ${g}, ${b})`

          const x = margin.left + t * cellWidth
          const y = margin.top + plotHeight - (f + 1) * cellHeight
          ctx.fillRect(x, y, cellWidth + 1, cellHeight + 1)
        }
      }

      // Labels
      ctx.fillStyle = '#1e293b'
      ctx.font = '11px Inter, sans-serif'
      ctx.textAlign = 'center'
      ctx.fillText('時間 (s)', width / 2, height - 5)

      ctx.save()
      ctx.translate(12, height / 2)
      ctx.rotate(-Math.PI / 2)
      ctx.fillText('周波数 (Hz)', 0, 0)
      ctx.restore()

      // Title
      ctx.fillText(analysisType === 'spectrogram' ? 'スペクトログラム' : 'ウェーブレット変換', width / 2, 15)

    } else if (analysisType === 'psd') {
      const data = result as { frequencies: number[]; power: number[] }
      const nFreqs = data.frequencies.length

      let minP = Infinity, maxP = -Infinity
      for (const p of data.power) {
        if (isFinite(p)) {
          minP = Math.min(minP, p)
          maxP = Math.max(maxP, p)
        }
      }
      const range = maxP - minP || 1

      // Draw PSD line
      ctx.beginPath()
      ctx.strokeStyle = '#3b82f6'
      ctx.lineWidth = 2
      for (let i = 0; i < nFreqs; i++) {
        const x = margin.left + (i / nFreqs) * plotWidth
        const y = margin.top + plotHeight * (1 - (data.power[i] - minP) / range)
        if (i === 0) ctx.moveTo(x, y)
        else ctx.lineTo(x, y)
      }
      ctx.stroke()

      // Labels
      ctx.fillStyle = '#1e293b'
      ctx.font = '11px Inter, sans-serif'
      ctx.textAlign = 'center'
      ctx.fillText('周波数 (Hz)', width / 2, height - 5)
      ctx.fillText('パワースペクトル密度', width / 2, 15)

    } else if (analysisType === 'bandpower') {
      const data = result as Record<string, number>
      const bands = Object.entries(data)
      const nBands = bands.length
      if (nBands === 0) return

      const barWidth = plotWidth / nBands * 0.7
      const gap = plotWidth / nBands * 0.3

      const maxPower = Math.max(...bands.map(([, v]) => v))
      const colors = ['#8b5cf6', '#06b6d4', '#22c55e', '#f59e0b', '#ef4444']

      bands.forEach(([name, power], i) => {
        const x = margin.left + i * (barWidth + gap) + gap / 2
        const barHeight = (power / maxPower) * plotHeight
        const y = margin.top + plotHeight - barHeight

        ctx.fillStyle = colors[i % colors.length]
        ctx.fillRect(x, y, barWidth, barHeight)

        ctx.fillStyle = '#1e293b'
        ctx.font = '10px Inter, sans-serif'
        ctx.textAlign = 'center'
        ctx.fillText(name, x + barWidth / 2, height - 10)
        ctx.fillText(power.toFixed(2), x + barWidth / 2, y - 5)
      })

      ctx.fillText('バンドパワー', width / 2, 15)
    }

  }, [result, analysisType])

  return (
    <div className="space-y-6">
      <section className="rounded-lg border p-4">
        <h3 className="font-medium">時間周波数解析</h3>
        <div className="mt-4 grid gap-4 md:grid-cols-3">
          <div>
            <label className="block text-sm">チャンネル</label>
            <select
              value={selectedChannel}
              onChange={(e) => { setSelectedChannel(e.target.value); setResult(null) }}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
            >
              {channelLabels.map(ch => (
                <option key={ch} value={ch}>{ch}</option>
              ))}
            </select>
          </div>
          <div>
            <label className="block text-sm">解析タイプ</label>
            <select
              value={analysisType}
              onChange={(e) => { setAnalysisType(e.target.value as typeof analysisType); setResult(null) }}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
            >
              <option value="spectrogram">スペクトログラム (STFT)</option>
              <option value="wavelet">ウェーブレット変換</option>
              <option value="psd">パワースペクトル密度</option>
              <option value="bandpower">バンドパワー</option>
            </select>
          </div>
          <div className="flex items-end">
            <button
              onClick={handleAnalyze}
              disabled={isProcessing || !selectedChannel}
              className="w-full rounded-md bg-primary px-4 py-2 text-sm text-primary-foreground hover:bg-primary/90 disabled:opacity-50"
            >
              {isProcessing ? '処理中...' : '解析を実行'}
            </button>
          </div>
        </div>
      </section>

      <section className="rounded-lg border p-4">
        <h3 className="font-medium mb-4">結果</h3>
        <div ref={containerRef} className="h-72 w-full">
          {result ? (
            <canvas ref={canvasRef} />
          ) : (
            <div className="h-full flex items-center justify-center text-muted-foreground">
              解析を実行すると結果が表示されます
            </div>
          )}
        </div>
      </section>
    </div>
  )
}

// ========== Epoch Panel ==========
function EpochPanel() {
  const { activeDatasetId, datasets, generatedSignals, ensureGeneratedSignals } = useDataStore()
  const activeDataset = activeDatasetId ? datasets[activeDatasetId] : null
  const generatedData = activeDatasetId ? generatedSignals[activeDatasetId] ?? null : null

  // Ensure signals are generated for demo datasets
  useEffect(() => {
    if (activeDatasetId && activeDataset?.metadata?.isDemo && !generatedData) {
      ensureGeneratedSignals(activeDatasetId)
    }
  }, [activeDatasetId, activeDataset, generatedData, ensureGeneratedSignals])

  const [preTime, setPreTime] = useState(0.2)
  const [postTime, setPostTime] = useState(0.8)
  const [selectedChannel, setSelectedChannel] = useState('')
  const [selectedEventType, setSelectedEventType] = useState('')
  const [epochs, setEpochs] = useState<number[][] | null>(null)
  const [erp, setErp] = useState<number[] | null>(null)

  const canvasRef = useRef<HTMLCanvasElement>(null)
  const containerRef = useRef<HTMLDivElement>(null)

  const channelLabels = activeDataset?.channels.map(ch => ch.label) ?? []
  const events = activeDataset?.events ?? []
  const eventTypes = [...new Set(events.map(e => e.type))]

  useEffect(() => {
    if (channelLabels.length > 0 && !selectedChannel) {
      setSelectedChannel(channelLabels[0])
    }
    if (eventTypes.length > 0 && !selectedEventType) {
      setSelectedEventType(eventTypes[0])
    }
  }, [channelLabels, eventTypes, selectedChannel, selectedEventType])

  const handleExtractEpochs = () => {
    if (!generatedData || !selectedChannel || !activeDataset) return

    const signal = generatedData.signals[selectedChannel]
    if (!signal) return

    // Get event times for selected type
    const eventTimes = events
      .filter(e => e.type === selectedEventType)
      .map(e => e.onset)

    const extractedEpochs = extractEpochs(
      signal,
      activeDataset.samplingRate,
      eventTimes,
      preTime,
      postTime
    )

    // Baseline correct
    const correctedEpochs = extractedEpochs.map(epoch =>
      baselineCorrect(epoch, activeDataset.samplingRate, 0, preTime)
    )

    setEpochs(correctedEpochs)
    setErp(computeERP(correctedEpochs))
  }

  // Draw ERP
  useEffect(() => {
    const canvas = canvasRef.current
    const container = containerRef.current
    if (!canvas || !container || !epochs || !erp || !activeDataset) return

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
    const margin = { top: 30, right: 20, bottom: 40, left: 60 }

    // Use explicit colors for canvas (CSS variables don't work in canvas)
    ctx.fillStyle = '#f8fafc'
    ctx.fillRect(0, 0, width, height)

    const plotWidth = width - margin.left - margin.right
    const plotHeight = height - margin.top - margin.bottom

    // Find range
    let minVal = Infinity, maxVal = -Infinity
    for (const epoch of epochs) {
      for (const val of epoch) {
        minVal = Math.min(minVal, val)
        maxVal = Math.max(maxVal, val)
      }
    }
    const range = maxVal - minVal || 1

    const epochLength = erp.length

    // Draw individual epochs (faded)
    ctx.globalAlpha = 0.2
    ctx.strokeStyle = '#94a3b8'
    ctx.lineWidth = 0.5
    for (const epoch of epochs) {
      ctx.beginPath()
      for (let i = 0; i < epoch.length; i++) {
        const x = margin.left + (i / epochLength) * plotWidth
        const y = margin.top + plotHeight * (1 - (epoch[i] - minVal) / range)
        if (i === 0) ctx.moveTo(x, y)
        else ctx.lineTo(x, y)
      }
      ctx.stroke()
    }

    // Draw ERP (bold)
    ctx.globalAlpha = 1
    ctx.beginPath()
    ctx.strokeStyle = '#3b82f6'
    ctx.lineWidth = 2
    for (let i = 0; i < erp.length; i++) {
      const x = margin.left + (i / epochLength) * plotWidth
      const y = margin.top + plotHeight * (1 - (erp[i] - minVal) / range)
      if (i === 0) ctx.moveTo(x, y)
      else ctx.lineTo(x, y)
    }
    ctx.stroke()

    // Draw zero line
    const zeroY = margin.top + plotHeight * (1 - (0 - minVal) / range)
    ctx.strokeStyle = '#e2e8f0'
    ctx.lineWidth = 1
    ctx.setLineDash([4, 4])
    ctx.beginPath()
    ctx.moveTo(margin.left, zeroY)
    ctx.lineTo(width - margin.right, zeroY)
    ctx.stroke()
    ctx.setLineDash([])

    // Draw time zero line
    const zeroTimeX = margin.left + (preTime / (preTime + postTime)) * plotWidth
    ctx.strokeStyle = '#ef4444'
    ctx.beginPath()
    ctx.moveTo(zeroTimeX, margin.top)
    ctx.lineTo(zeroTimeX, height - margin.bottom)
    ctx.stroke()

    // Labels
    ctx.fillStyle = '#1e293b'
    ctx.font = '11px Inter, sans-serif'
    ctx.textAlign = 'center'

    // X axis
    const xTicks = [-preTime, -preTime / 2, 0, postTime / 2, postTime]
    for (const t of xTicks) {
      const x = margin.left + ((t + preTime) / (preTime + postTime)) * plotWidth
      ctx.fillText((t * 1000).toFixed(0) + ' ms', x, height - 10)
    }

    // Title
    ctx.font = '12px Inter, sans-serif'
    ctx.fillText(`ERP - ${selectedChannel} (n=${epochs.length})`, width / 2, 15)

    // Y axis label
    ctx.save()
    ctx.translate(15, height / 2)
    ctx.rotate(-Math.PI / 2)
    ctx.fillText('振幅 (µV)', 0, 0)
    ctx.restore()

  }, [epochs, erp, activeDataset, preTime, postTime, selectedChannel])

  return (
    <div className="space-y-6">
      <section className="rounded-lg border p-4">
        <h3 className="font-medium">エポック抽出</h3>
        <div className="mt-4 grid gap-4 md:grid-cols-4">
          <div>
            <label className="block text-sm">チャンネル</label>
            <select
              value={selectedChannel}
              onChange={(e) => { setSelectedChannel(e.target.value); setEpochs(null); setErp(null) }}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
            >
              {channelLabels.map(ch => (
                <option key={ch} value={ch}>{ch}</option>
              ))}
            </select>
          </div>
          <div>
            <label className="block text-sm">イベントタイプ</label>
            <select
              value={selectedEventType}
              onChange={(e) => { setSelectedEventType(e.target.value); setEpochs(null); setErp(null) }}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
            >
              {eventTypes.map(t => (
                <option key={t} value={t}>{t}</option>
              ))}
            </select>
          </div>
          <div>
            <label className="block text-sm">開始時間 (秒前)</label>
            <input
              type="number"
              value={preTime}
              onChange={(e) => setPreTime(parseFloat(e.target.value))}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
              step="0.1"
              min="0"
            />
          </div>
          <div>
            <label className="block text-sm">終了時間 (秒後)</label>
            <input
              type="number"
              value={postTime}
              onChange={(e) => setPostTime(parseFloat(e.target.value))}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
              step="0.1"
              min="0"
            />
          </div>
        </div>
        <button
          onClick={handleExtractEpochs}
          disabled={!selectedChannel || eventTypes.length === 0}
          className="mt-4 rounded-md bg-primary px-4 py-2 text-sm text-primary-foreground hover:bg-primary/90 disabled:opacity-50"
        >
          エポックを抽出してERPを計算
        </button>
        {events.length === 0 && (
          <p className="mt-2 text-sm text-amber-600">このデータセットにはイベントがありません</p>
        )}
      </section>

      <section className="rounded-lg border p-4">
        <h3 className="font-medium mb-4">ERP (事象関連電位)</h3>
        <div ref={containerRef} className="h-64 w-full">
          {epochs ? (
            <canvas ref={canvasRef} />
          ) : (
            <div className="h-full flex items-center justify-center text-muted-foreground">
              エポックを抽出するとERPが表示されます
            </div>
          )}
        </div>
        {epochs && (
          <div className="mt-2 text-sm text-muted-foreground">
            抽出エポック数: {epochs.length} | 時間窓: -{preTime * 1000}ms 〜 +{postTime * 1000}ms
          </div>
        )}
      </section>
    </div>
  )
}

// ========== Connectivity Panel ==========
function ConnectivityPanel() {
  const { activeDatasetId, datasets, generatedSignals, ensureGeneratedSignals } = useDataStore()
  const activeDataset = activeDatasetId ? datasets[activeDatasetId] : null
  const generatedData = activeDatasetId ? generatedSignals[activeDatasetId] ?? null : null

  // Ensure signals are generated for demo datasets
  useEffect(() => {
    if (activeDatasetId && activeDataset?.metadata?.isDemo && !generatedData) {
      ensureGeneratedSignals(activeDatasetId)
    }
  }, [activeDatasetId, activeDataset, generatedData, ensureGeneratedSignals])

  const [lowFreq, setLowFreq] = useState(8)
  const [highFreq, setHighFreq] = useState(13)
  const [method, setMethod] = useState<'plv' | 'pli' | 'wpli' | 'coherence' | 'correlation'>('plv')
  const [connectivityMatrix, setConnectivityMatrix] = useState<{ channels: string[]; matrix: number[][] } | null>(null)
  const [isProcessing, setIsProcessing] = useState(false)

  const canvasRef = useRef<HTMLCanvasElement>(null)
  const containerRef = useRef<HTMLDivElement>(null)

  const handleComputeConnectivity = () => {
    if (!generatedData || !activeDataset) return

    setIsProcessing(true)
    setTimeout(() => {
      const channels = Object.keys(generatedData.signals)
      const n = channels.length
      const matrix: number[][] = []

      if (method === 'correlation') {
        const result = computeCorrelationMatrix(generatedData.signals)
        setConnectivityMatrix(result)
      } else {
        // Compute pairwise connectivity
        for (let i = 0; i < n; i++) {
          const row: number[] = []
          for (let j = 0; j < n; j++) {
            if (i === j) {
              row.push(1)
            } else if (j < i) {
              row.push(matrix[j][i])
            } else {
              let value: number
              const sig1 = generatedData.signals[channels[i]]
              const sig2 = generatedData.signals[channels[j]]

              switch (method) {
                case 'pli':
                  value = computePLI(sig1, sig2, activeDataset.samplingRate, lowFreq, highFreq)
                  break
                case 'wpli':
                  value = computeWPLI(sig1, sig2, activeDataset.samplingRate, lowFreq, highFreq)
                  break
                case 'coherence':
                  const coh = computeCoherence(sig1, sig2, activeDataset.samplingRate)
                  // Average coherence in frequency band
                  const freqMask = coh.frequencies.map(f => f >= lowFreq && f <= highFreq)
                  const bandCoh = coh.coherence.filter((_, i) => freqMask[i])
                  value = bandCoh.length > 0 ? bandCoh.reduce((a, b) => a + b, 0) / bandCoh.length : 0
                  break
                default: // plv
                  value = computeConnectivityMatrix(
                    { [channels[i]]: sig1, [channels[j]]: sig2 },
                    activeDataset.samplingRate,
                    lowFreq,
                    highFreq
                  ).matrix[0][1]
              }
              row.push(value)
            }
          }
          matrix.push(row)
        }
        setConnectivityMatrix({ channels, matrix })
      }
      setIsProcessing(false)
    }, 500)
  }

  // Draw matrix
  useEffect(() => {
    const canvas = canvasRef.current
    const container = containerRef.current
    if (!canvas || !container || !connectivityMatrix) return

    const ctx = canvas.getContext('2d')
    if (!ctx) return

    const { channels, matrix } = connectivityMatrix
    const n = channels.length

    const rect = container.getBoundingClientRect()
    const size = Math.min(rect.width, rect.height)
    const dpr = window.devicePixelRatio || 1
    canvas.width = size * dpr
    canvas.height = size * dpr
    canvas.style.width = `${size}px`
    canvas.style.height = `${size}px`
    ctx.scale(dpr, dpr)

    const margin = 60
    const cellSize = (size - margin * 2) / n

    // Use explicit colors for canvas (CSS variables don't work in canvas)
    ctx.fillStyle = '#f8fafc'
    ctx.fillRect(0, 0, size, size)

    // Draw matrix cells
    for (let i = 0; i < n; i++) {
      for (let j = 0; j < n; j++) {
        const value = matrix[i][j]
        const r = Math.round(255 * (1 - value))
        const g = Math.round(100 * value)
        const b = Math.round(255 * value)
        ctx.fillStyle = `rgb(${r}, ${g}, ${b})`
        ctx.fillRect(margin + j * cellSize, margin + i * cellSize, cellSize - 1, cellSize - 1)
      }
    }

    // Labels
    ctx.fillStyle = '#1e293b'
    ctx.font = '9px Inter, sans-serif'
    ctx.textAlign = 'right'
    for (let i = 0; i < n; i++) {
      ctx.fillText(channels[i], margin - 5, margin + i * cellSize + cellSize / 2 + 3)
    }
    ctx.textAlign = 'center'
    ctx.save()
    for (let i = 0; i < n; i++) {
      ctx.save()
      ctx.translate(margin + i * cellSize + cellSize / 2, margin - 5)
      ctx.rotate(-Math.PI / 4)
      ctx.fillText(channels[i], 0, 0)
      ctx.restore()
    }
    ctx.restore()

    // Colorbar
    const colorbarX = size - 25
    for (let i = 0; i < 100; i++) {
      const value = 1 - i / 100
      const r = Math.round(255 * (1 - value))
      const g = Math.round(100 * value)
      const b = Math.round(255 * value)
      ctx.fillStyle = `rgb(${r}, ${g}, ${b})`
      ctx.fillRect(colorbarX, margin + i * (size - margin * 2) / 100, 15, (size - margin * 2) / 100 + 1)
    }
    ctx.fillStyle = '#1e293b'
    ctx.font = '10px Inter, sans-serif'
    ctx.textAlign = 'left'
    ctx.fillText('1.0', colorbarX - 20, margin + 5)
    ctx.fillText('0.0', colorbarX - 20, size - margin + 5)

  }, [connectivityMatrix])

  const methodLabels: Record<typeof method, string> = {
    plv: 'PLV (位相同期値)',
    pli: 'PLI (位相遅れ指標)',
    wpli: 'wPLI (重み付きPLI)',
    coherence: 'コヒーレンス',
    correlation: '相関'
  }

  return (
    <div className="space-y-6">
      <section className="rounded-lg border p-4">
        <h3 className="font-medium">接続性解析</h3>
        <div className="mt-4 grid gap-4 md:grid-cols-4">
          <div>
            <label className="block text-sm">手法</label>
            <select
              value={method}
              onChange={(e) => { setMethod(e.target.value as typeof method); setConnectivityMatrix(null) }}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
            >
              <option value="plv">PLV (位相同期値)</option>
              <option value="pli">PLI (位相遅れ指標)</option>
              <option value="wpli">wPLI (重み付きPLI)</option>
              <option value="coherence">コヒーレンス</option>
              <option value="correlation">相関</option>
            </select>
          </div>
          <div>
            <label className="block text-sm">低域周波数 (Hz)</label>
            <input
              type="number"
              value={lowFreq}
              onChange={(e) => setLowFreq(parseFloat(e.target.value))}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
              disabled={method === 'correlation'}
            />
          </div>
          <div>
            <label className="block text-sm">高域周波数 (Hz)</label>
            <input
              type="number"
              value={highFreq}
              onChange={(e) => setHighFreq(parseFloat(e.target.value))}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
              disabled={method === 'correlation'}
            />
          </div>
          <div className="flex items-end">
            <button
              onClick={handleComputeConnectivity}
              disabled={isProcessing}
              className="w-full rounded-md bg-primary px-4 py-2 text-sm text-primary-foreground hover:bg-primary/90 disabled:opacity-50"
            >
              {isProcessing ? '計算中...' : '接続性を計算'}
            </button>
          </div>
        </div>
      </section>

      <section className="rounded-lg border p-4">
        <h3 className="font-medium mb-4">{methodLabels[method]}マトリックス {method !== 'correlation' && `(${lowFreq}-${highFreq} Hz)`}</h3>
        <div ref={containerRef} className="aspect-square max-w-lg mx-auto">
          {connectivityMatrix ? (
            <canvas ref={canvasRef} />
          ) : (
            <div className="h-full flex items-center justify-center text-muted-foreground border rounded">
              接続性を計算するとマトリックスが表示されます
            </div>
          )}
        </div>
      </section>
    </div>
  )
}

// ========== Network Panel ==========
function NetworkPanel() {
  const { activeDatasetId, datasets, generatedSignals, ensureGeneratedSignals } = useDataStore()
  const activeDataset = activeDatasetId ? datasets[activeDatasetId] : null
  const generatedData = activeDatasetId ? generatedSignals[activeDatasetId] ?? null : null

  useEffect(() => {
    if (activeDatasetId && activeDataset?.metadata?.isDemo && !generatedData) {
      ensureGeneratedSignals(activeDatasetId)
    }
  }, [activeDatasetId, activeDataset, generatedData, ensureGeneratedSignals])

  const [lowFreq, setLowFreq] = useState(8)
  const [highFreq, setHighFreq] = useState(13)
  const [threshold, setThreshold] = useState(0.5)
  const [metrics, setMetrics] = useState<{
    channels: string[]
    degree: number[]
    clustering: number[]
    betweenness: number[]
    pathLength: number
    efficiency: number
    adjacency: number[][]
  } | null>(null)
  const [isProcessing, setIsProcessing] = useState(false)

  const canvasRef = useRef<HTMLCanvasElement>(null)
  const containerRef = useRef<HTMLDivElement>(null)

  const handleComputeNetwork = () => {
    if (!generatedData || !activeDataset) return

    setIsProcessing(true)
    setTimeout(() => {
      // First compute connectivity matrix
      const connectivity = computeConnectivityMatrix(
        generatedData.signals,
        activeDataset.samplingRate,
        lowFreq,
        highFreq
      )

      // Create adjacency matrix with threshold
      const adjacency = createAdjacencyMatrix(connectivity.matrix, threshold, false)

      // Compute network metrics
      const degree = computeNodeDegree(adjacency)
      const clustering = computeClusteringCoefficient(adjacency)
      const betweenness = computeBetweennessCentrality(adjacency)
      const pathLength = computeCharacteristicPathLength(adjacency)
      const efficiency = computeGlobalEfficiency(adjacency)

      setMetrics({
        channels: connectivity.channels,
        degree,
        clustering,
        betweenness,
        pathLength,
        efficiency,
        adjacency
      })
      setIsProcessing(false)
    }, 500)
  }

  // Draw network visualization
  useEffect(() => {
    const canvas = canvasRef.current
    const container = containerRef.current
    if (!canvas || !container || !metrics) return

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

    ctx.fillStyle = '#f8fafc'
    ctx.fillRect(0, 0, size, size)

    const { channels, adjacency, degree } = metrics
    const n = channels.length
    const centerX = size / 2
    const centerY = size / 2
    const radius = size * 0.35

    // Calculate node positions (circular layout)
    const positions: Array<{ x: number; y: number }> = []
    for (let i = 0; i < n; i++) {
      const angle = (2 * Math.PI * i) / n - Math.PI / 2
      positions.push({
        x: centerX + radius * Math.cos(angle),
        y: centerY + radius * Math.sin(angle)
      })
    }

    // Draw edges
    ctx.strokeStyle = '#94a3b8'
    for (let i = 0; i < n; i++) {
      for (let j = i + 1; j < n; j++) {
        if (adjacency[i][j] > 0) {
          ctx.lineWidth = adjacency[i][j] * 2
          ctx.globalAlpha = 0.3 + adjacency[i][j] * 0.5
          ctx.beginPath()
          ctx.moveTo(positions[i].x, positions[i].y)
          ctx.lineTo(positions[j].x, positions[j].y)
          ctx.stroke()
        }
      }
    }
    ctx.globalAlpha = 1

    // Draw nodes
    const maxDegree = Math.max(...degree, 1)
    for (let i = 0; i < n; i++) {
      const nodeSize = 8 + (degree[i] / maxDegree) * 12
      ctx.fillStyle = '#3b82f6'
      ctx.beginPath()
      ctx.arc(positions[i].x, positions[i].y, nodeSize, 0, 2 * Math.PI)
      ctx.fill()

      // Label
      ctx.fillStyle = '#1e293b'
      ctx.font = '10px Inter, sans-serif'
      ctx.textAlign = 'center'
      const labelRadius = radius + 25
      const angle = (2 * Math.PI * i) / n - Math.PI / 2
      ctx.fillText(channels[i], centerX + labelRadius * Math.cos(angle), centerY + labelRadius * Math.sin(angle) + 4)
    }

    // Title
    ctx.font = '12px Inter, sans-serif'
    ctx.fillText('ネットワークグラフ', size / 2, 20)

  }, [metrics])

  return (
    <div className="space-y-6">
      <section className="rounded-lg border p-4">
        <h3 className="font-medium">ネットワーク解析</h3>
        <div className="mt-4 grid gap-4 md:grid-cols-4">
          <div>
            <label className="block text-sm">低域周波数 (Hz)</label>
            <input
              type="number"
              value={lowFreq}
              onChange={(e) => setLowFreq(parseFloat(e.target.value))}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
            />
          </div>
          <div>
            <label className="block text-sm">高域周波数 (Hz)</label>
            <input
              type="number"
              value={highFreq}
              onChange={(e) => setHighFreq(parseFloat(e.target.value))}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
            />
          </div>
          <div>
            <label className="block text-sm">閾値</label>
            <input
              type="number"
              value={threshold}
              onChange={(e) => setThreshold(parseFloat(e.target.value))}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
              step="0.1"
              min="0"
              max="1"
            />
          </div>
          <div className="flex items-end">
            <button
              onClick={handleComputeNetwork}
              disabled={isProcessing}
              className="w-full rounded-md bg-primary px-4 py-2 text-sm text-primary-foreground hover:bg-primary/90 disabled:opacity-50"
            >
              {isProcessing ? '計算中...' : 'ネットワークを計算'}
            </button>
          </div>
        </div>
      </section>

      <div className="grid gap-6 lg:grid-cols-2">
        <section className="rounded-lg border p-4">
          <h3 className="font-medium mb-4">ネットワーク可視化</h3>
          <div ref={containerRef} className="aspect-square max-w-md mx-auto">
            {metrics ? (
              <canvas ref={canvasRef} />
            ) : (
              <div className="h-full flex items-center justify-center text-muted-foreground border rounded">
                ネットワークを計算すると可視化されます
              </div>
            )}
          </div>
        </section>

        <section className="rounded-lg border p-4">
          <h3 className="font-medium mb-4">ネットワーク指標</h3>
          {metrics ? (
            <div className="space-y-4">
              <div className="grid grid-cols-2 gap-4">
                <div className="rounded-lg bg-muted p-3">
                  <div className="text-sm text-muted-foreground">平均経路長</div>
                  <div className="text-xl font-semibold">{metrics.pathLength.toFixed(3)}</div>
                </div>
                <div className="rounded-lg bg-muted p-3">
                  <div className="text-sm text-muted-foreground">大域効率</div>
                  <div className="text-xl font-semibold">{metrics.efficiency.toFixed(3)}</div>
                </div>
              </div>
              <div>
                <h4 className="text-sm font-medium mb-2">ノード指標</h4>
                <div className="max-h-48 overflow-auto">
                  <table className="w-full text-sm">
                    <thead>
                      <tr className="border-b">
                        <th className="text-left py-1">チャンネル</th>
                        <th className="text-right py-1">次数</th>
                        <th className="text-right py-1">クラスタ係数</th>
                        <th className="text-right py-1">媒介中心性</th>
                      </tr>
                    </thead>
                    <tbody>
                      {metrics.channels.map((ch, i) => (
                        <tr key={ch} className="border-b">
                          <td className="py-1">{ch}</td>
                          <td className="text-right">{metrics.degree[i].toFixed(2)}</td>
                          <td className="text-right">{metrics.clustering[i].toFixed(3)}</td>
                          <td className="text-right">{metrics.betweenness[i].toFixed(3)}</td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>
              </div>
            </div>
          ) : (
            <div className="h-48 flex items-center justify-center text-muted-foreground">
              ネットワークを計算すると指標が表示されます
            </div>
          )}
        </section>
      </div>
    </div>
  )
}

// ========== Statistics Panel ==========
function StatisticsPanel() {
  const { activeDatasetId, datasets, generatedSignals, ensureGeneratedSignals } = useDataStore()
  const activeDataset = activeDatasetId ? datasets[activeDatasetId] : null
  const generatedData = activeDatasetId ? generatedSignals[activeDatasetId] ?? null : null

  // Ensure signals are generated for demo datasets
  useEffect(() => {
    if (activeDatasetId && activeDataset?.metadata?.isDemo && !generatedData) {
      ensureGeneratedSignals(activeDatasetId)
    }
  }, [activeDatasetId, activeDataset, generatedData, ensureGeneratedSignals])

  const [analysisType, setAnalysisType] = useState<'ttest' | 'anova' | 'effectsize'>('ttest')
  const [selectedChannel, setSelectedChannel] = useState('')
  const [eventTypeA, setEventTypeA] = useState('')
  const [eventTypeB, setEventTypeB] = useState('')
  const [preTime, setPreTime] = useState(0.2)
  const [postTime, setPostTime] = useState(0.8)
  const [useFDR, setUseFDR] = useState(false)
  const [tTestResult, setTTestResult] = useState<{ t: number[]; p: number[]; correctedP?: number[] } | null>(null)
  const [anovaResult, setAnovaResult] = useState<{ F: number; p: number; df1: number; df2: number } | null>(null)
  const [effectSizeResult, setEffectSizeResult] = useState<{ d: number; ci: [number, number] } | null>(null)
  const [erpA, setErpA] = useState<number[] | null>(null)
  const [erpB, setErpB] = useState<number[] | null>(null)

  const canvasRef = useRef<HTMLCanvasElement>(null)
  const containerRef = useRef<HTMLDivElement>(null)

  const channelLabels = activeDataset?.channels.map(ch => ch.label) ?? []
  const events = activeDataset?.events ?? []
  const eventTypes = [...new Set(events.map(e => e.type))]

  useEffect(() => {
    if (channelLabels.length > 0 && !selectedChannel) {
      setSelectedChannel(channelLabels[0])
    }
    if (eventTypes.length >= 2) {
      if (!eventTypeA) setEventTypeA(eventTypes[0])
      if (!eventTypeB) setEventTypeB(eventTypes[1])
    }
  }, [channelLabels, eventTypes, selectedChannel, eventTypeA, eventTypeB])

  const handleRunAnalysis = () => {
    if (!generatedData || !selectedChannel || !activeDataset) return

    const signal = generatedData.signals[selectedChannel]
    if (!signal) return

    // Extract epochs for both conditions
    const eventsA = events.filter(e => e.type === eventTypeA).map(e => e.onset)
    const eventsB = events.filter(e => e.type === eventTypeB).map(e => e.onset)

    const epochsA = extractEpochs(signal, activeDataset.samplingRate, eventsA, preTime, postTime)
    const epochsB = extractEpochs(signal, activeDataset.samplingRate, eventsB, preTime, postTime)

    // Baseline correct
    const correctedA = epochsA.map(e => baselineCorrect(e, activeDataset.samplingRate, 0, preTime))
    const correctedB = epochsB.map(e => baselineCorrect(e, activeDataset.samplingRate, 0, preTime))

    // Compute ERPs
    setErpA(computeERP(correctedA))
    setErpB(computeERP(correctedB))

    if (analysisType === 'ttest') {
      // Run pointwise t-test
      const result = pointwiseTTest(correctedA, correctedB)

      // Apply FDR correction if enabled
      if (useFDR) {
        const { corrected } = fdrCorrection(result.p)
        setTTestResult({ ...result, correctedP: corrected })
      } else {
        setTTestResult(result)
      }
      setAnovaResult(null)
      setEffectSizeResult(null)

    } else if (analysisType === 'anova') {
      // Run ANOVA on mean amplitudes in a window (e.g., 200-400ms post-stimulus)
      const windowStart = Math.floor(preTime * activeDataset.samplingRate)
      const windowEnd = Math.floor((preTime + 0.2) * activeDataset.samplingRate)

      const meansA = correctedA.map(epoch => {
        const windowVals = epoch.slice(windowStart, windowEnd)
        return windowVals.reduce((a, b) => a + b, 0) / windowVals.length
      })
      const meansB = correctedB.map(epoch => {
        const windowVals = epoch.slice(windowStart, windowEnd)
        return windowVals.reduce((a, b) => a + b, 0) / windowVals.length
      })

      // If more than 2 event types, include all
      const allGroups = [meansA, meansB]
      if (eventTypes.length > 2) {
        for (let i = 2; i < eventTypes.length; i++) {
          const eventsC = events.filter(e => e.type === eventTypes[i]).map(e => e.onset)
          const epochsC = extractEpochs(signal, activeDataset.samplingRate, eventsC, preTime, postTime)
          const correctedC = epochsC.map(e => baselineCorrect(e, activeDataset.samplingRate, 0, preTime))
          const meansC = correctedC.map(epoch => {
            const windowVals = epoch.slice(windowStart, windowEnd)
            return windowVals.reduce((a, b) => a + b, 0) / windowVals.length
          })
          allGroups.push(meansC)
        }
      }

      const result = oneWayANOVA(allGroups)
      setAnovaResult(result)
      setTTestResult(null)
      setEffectSizeResult(null)

    } else if (analysisType === 'effectsize') {
      // Compute Cohen's d on mean amplitudes
      const windowStart = Math.floor(preTime * activeDataset.samplingRate)
      const windowEnd = Math.floor((preTime + 0.2) * activeDataset.samplingRate)

      const meansA = correctedA.map(epoch => {
        const windowVals = epoch.slice(windowStart, windowEnd)
        return windowVals.reduce((a, b) => a + b, 0) / windowVals.length
      })
      const meansB = correctedB.map(epoch => {
        const windowVals = epoch.slice(windowStart, windowEnd)
        return windowVals.reduce((a, b) => a + b, 0) / windowVals.length
      })

      const result = cohensD(meansA, meansB)
      setEffectSizeResult(result)
      setTTestResult(null)
      setAnovaResult(null)
    }
  }

  // Draw results
  useEffect(() => {
    const canvas = canvasRef.current
    const container = containerRef.current
    if (!canvas || !container || !tTestResult || !erpA || !erpB || !activeDataset) return

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
    const margin = { top: 30, right: 20, bottom: 60, left: 60 }

    // Use explicit colors for canvas (CSS variables don't work in canvas)
    ctx.fillStyle = '#f8fafc'
    ctx.fillRect(0, 0, width, height)

    const plotWidth = width - margin.left - margin.right
    const erpHeight = (height - margin.top - margin.bottom) * 0.6
    const pHeight = (height - margin.top - margin.bottom) * 0.3

    // Find ERP range
    let minVal = Math.min(...erpA, ...erpB)
    let maxVal = Math.max(...erpA, ...erpB)
    const range = maxVal - minVal || 1

    const epochLength = erpA.length

    // Draw ERP A
    ctx.beginPath()
    ctx.strokeStyle = 'hsl(217, 91%, 60%)'
    ctx.lineWidth = 2
    for (let i = 0; i < epochLength; i++) {
      const x = margin.left + (i / epochLength) * plotWidth
      const y = margin.top + erpHeight * (1 - (erpA[i] - minVal) / range)
      if (i === 0) ctx.moveTo(x, y)
      else ctx.lineTo(x, y)
    }
    ctx.stroke()

    // Draw ERP B
    ctx.beginPath()
    ctx.strokeStyle = 'hsl(0, 84%, 60%)'
    ctx.lineWidth = 2
    for (let i = 0; i < epochLength; i++) {
      const x = margin.left + (i / epochLength) * plotWidth
      const y = margin.top + erpHeight * (1 - (erpB[i] - minVal) / range)
      if (i === 0) ctx.moveTo(x, y)
      else ctx.lineTo(x, y)
    }
    ctx.stroke()

    // Highlight significant regions
    const significanceThreshold = 0.05
    ctx.fillStyle = 'rgba(255, 200, 0, 0.3)'
    for (let i = 0; i < tTestResult.p.length; i++) {
      if (tTestResult.p[i] < significanceThreshold) {
        const x = margin.left + (i / epochLength) * plotWidth
        ctx.fillRect(x, margin.top, plotWidth / epochLength + 1, erpHeight)
      }
    }

    // Draw p-value line
    const pTop = margin.top + erpHeight + 20
    ctx.strokeStyle = '#1e293b'
    ctx.lineWidth = 1
    ctx.beginPath()
    for (let i = 0; i < tTestResult.p.length; i++) {
      const x = margin.left + (i / epochLength) * plotWidth
      const pVal = Math.min(tTestResult.p[i], 1)
      const y = pTop + pHeight * pVal
      if (i === 0) ctx.moveTo(x, y)
      else ctx.lineTo(x, y)
    }
    ctx.stroke()

    // Significance line
    ctx.strokeStyle = '#ef4444'
    ctx.setLineDash([4, 4])
    ctx.beginPath()
    const sigY = pTop + pHeight * significanceThreshold
    ctx.moveTo(margin.left, sigY)
    ctx.lineTo(width - margin.right, sigY)
    ctx.stroke()
    ctx.setLineDash([])

    // Labels
    ctx.fillStyle = '#1e293b'
    ctx.font = '11px Inter, sans-serif'
    ctx.textAlign = 'center'

    // X axis
    const xTicks = [-preTime, 0, postTime]
    for (const t of xTicks) {
      const x = margin.left + ((t + preTime) / (preTime + postTime)) * plotWidth
      ctx.fillText((t * 1000).toFixed(0) + ' ms', x, height - 5)
    }

    // Legend
    ctx.fillStyle = '#3b82f6'
    ctx.fillRect(margin.left, 8, 15, 10)
    ctx.fillStyle = '#1e293b'
    ctx.textAlign = 'left'
    ctx.fillText(eventTypeA, margin.left + 20, 16)

    ctx.fillStyle = '#ef4444'
    ctx.fillRect(margin.left + 100, 8, 15, 10)
    ctx.fillStyle = '#1e293b'
    ctx.fillText(eventTypeB, margin.left + 120, 16)

    ctx.fillStyle = 'rgba(255, 200, 0, 0.5)'
    ctx.fillRect(margin.left + 200, 8, 15, 10)
    ctx.fillStyle = '#1e293b'
    ctx.fillText('p < 0.05', margin.left + 220, 16)

    // P-value label
    ctx.fillText('p-value', margin.left - 5, pTop + pHeight / 2)
    ctx.fillStyle = '#ef4444'
    ctx.fillText('p=0.05', width - margin.right + 5, sigY + 4)

  }, [tTestResult, erpA, erpB, activeDataset, preTime, postTime, eventTypeA, eventTypeB])

  return (
    <div className="space-y-6">
      <section className="rounded-lg border p-4">
        <h3 className="font-medium">統計検定</h3>
        <div className="mt-4 grid gap-4 md:grid-cols-6">
          <div>
            <label className="block text-sm">検定タイプ</label>
            <select
              value={analysisType}
              onChange={(e) => { setAnalysisType(e.target.value as typeof analysisType); setTTestResult(null); setAnovaResult(null); setEffectSizeResult(null) }}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
            >
              <option value="ttest">点ごとt検定</option>
              <option value="anova">ANOVA</option>
              <option value="effectsize">効果量 (Cohen&apos;s d)</option>
            </select>
          </div>
          <div>
            <label className="block text-sm">チャンネル</label>
            <select
              value={selectedChannel}
              onChange={(e) => { setSelectedChannel(e.target.value); setTTestResult(null) }}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
            >
              {channelLabels.map(ch => (
                <option key={ch} value={ch}>{ch}</option>
              ))}
            </select>
          </div>
          <div>
            <label className="block text-sm">条件A</label>
            <select
              value={eventTypeA}
              onChange={(e) => { setEventTypeA(e.target.value); setTTestResult(null) }}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
            >
              {eventTypes.map(t => (
                <option key={t} value={t}>{t}</option>
              ))}
            </select>
          </div>
          <div>
            <label className="block text-sm">条件B</label>
            <select
              value={eventTypeB}
              onChange={(e) => { setEventTypeB(e.target.value); setTTestResult(null) }}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
            >
              {eventTypes.map(t => (
                <option key={t} value={t}>{t}</option>
              ))}
            </select>
          </div>
          <div>
            <label className="block text-sm">時間窓 (秒)</label>
            <div className="mt-1 flex gap-1">
              <input
                type="number"
                value={preTime}
                onChange={(e) => setPreTime(parseFloat(e.target.value))}
                className="w-full rounded-md border bg-background px-2 py-2 text-sm"
                step="0.1"
              />
              <span className="py-2">〜</span>
              <input
                type="number"
                value={postTime}
                onChange={(e) => setPostTime(parseFloat(e.target.value))}
                className="w-full rounded-md border bg-background px-2 py-2 text-sm"
                step="0.1"
              />
            </div>
          </div>
          <div className="flex items-end">
            <button
              onClick={handleRunAnalysis}
              disabled={!selectedChannel || eventTypes.length < 2}
              className="w-full rounded-md bg-primary px-4 py-2 text-sm text-primary-foreground hover:bg-primary/90 disabled:opacity-50"
            >
              {analysisType === 'ttest' ? 't検定を実行' : analysisType === 'anova' ? 'ANOVAを実行' : '効果量を計算'}
            </button>
          </div>
        </div>
        {analysisType === 'ttest' && (
          <div className="mt-3 flex items-center gap-2">
            <input
              type="checkbox"
              id="fdr-correction"
              checked={useFDR}
              onChange={(e) => setUseFDR(e.target.checked)}
              className="rounded border-gray-300"
            />
            <label htmlFor="fdr-correction" className="text-sm">FDR補正 (Benjamini-Hochberg)</label>
          </div>
        )}
        {eventTypes.length < 2 && (
          <p className="mt-2 text-sm text-amber-600">2つ以上のイベントタイプが必要です</p>
        )}
      </section>

      {/* Results Section */}
      {analysisType === 'ttest' && (
        <section className="rounded-lg border p-4">
          <h3 className="font-medium mb-4">ERP比較と統計結果</h3>
          <div ref={containerRef} className="h-80 w-full">
            {tTestResult ? (
              <canvas ref={canvasRef} />
            ) : (
              <div className="h-full flex items-center justify-center text-muted-foreground">
                t検定を実行すると結果が表示されます
              </div>
            )}
          </div>
        </section>
      )}

      {analysisType === 'anova' && (
        <section className="rounded-lg border p-4">
          <h3 className="font-medium mb-4">ANOVA結果 (200-400ms窓の平均振幅)</h3>
          {anovaResult ? (
            <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
              <div className="rounded-lg bg-muted p-4">
                <div className="text-sm text-muted-foreground">F値</div>
                <div className="text-2xl font-semibold">{anovaResult.F.toFixed(3)}</div>
              </div>
              <div className="rounded-lg bg-muted p-4">
                <div className="text-sm text-muted-foreground">p値</div>
                <div className={`text-2xl font-semibold ${anovaResult.p < 0.05 ? 'text-green-600' : ''}`}>
                  {anovaResult.p < 0.001 ? '< 0.001' : anovaResult.p.toFixed(4)}
                </div>
              </div>
              <div className="rounded-lg bg-muted p-4">
                <div className="text-sm text-muted-foreground">df (群間)</div>
                <div className="text-2xl font-semibold">{anovaResult.df1}</div>
              </div>
              <div className="rounded-lg bg-muted p-4">
                <div className="text-sm text-muted-foreground">df (群内)</div>
                <div className="text-2xl font-semibold">{anovaResult.df2}</div>
              </div>
            </div>
          ) : (
            <div className="h-32 flex items-center justify-center text-muted-foreground">
              ANOVAを実行すると結果が表示されます
            </div>
          )}
        </section>
      )}

      {analysisType === 'effectsize' && (
        <section className="rounded-lg border p-4">
          <h3 className="font-medium mb-4">効果量 (Cohen&apos;s d) - 200-400ms窓の平均振幅</h3>
          {effectSizeResult ? (
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div className="rounded-lg bg-muted p-4">
                <div className="text-sm text-muted-foreground">Cohen&apos;s d</div>
                <div className="text-3xl font-semibold">{effectSizeResult.d.toFixed(3)}</div>
                <div className="text-sm text-muted-foreground mt-1">
                  {Math.abs(effectSizeResult.d) < 0.2 ? '効果なし' :
                   Math.abs(effectSizeResult.d) < 0.5 ? '小さい効果' :
                   Math.abs(effectSizeResult.d) < 0.8 ? '中程度の効果' : '大きい効果'}
                </div>
              </div>
              <div className="rounded-lg bg-muted p-4">
                <div className="text-sm text-muted-foreground">95% 信頼区間</div>
                <div className="text-xl font-semibold">
                  [{effectSizeResult.ci[0].toFixed(3)}, {effectSizeResult.ci[1].toFixed(3)}]
                </div>
              </div>
            </div>
          ) : (
            <div className="h-32 flex items-center justify-center text-muted-foreground">
              効果量を計算すると結果が表示されます
            </div>
          )}
        </section>
      )}
    </div>
  )
}

// ========== Advanced Analysis Panel ==========
function AdvancedPanel() {
  const { activeDatasetId, datasets, generatedSignals, ensureGeneratedSignals } = useDataStore()
  const activeDataset = activeDatasetId ? datasets[activeDatasetId] : null
  const generatedData = activeDatasetId ? generatedSignals[activeDatasetId] ?? null : null

  useEffect(() => {
    if (activeDatasetId && activeDataset?.metadata?.isDemo && !generatedData) {
      ensureGeneratedSignals(activeDatasetId)
    }
  }, [activeDatasetId, activeDataset, generatedData, ensureGeneratedSignals])

  const [selectedChannel, setSelectedChannel] = useState('')
  const [referenceChannel, setReferenceChannel] = useState('')
  const [analysisType, setAnalysisType] = useState<'dtw' | 'cluster'>('dtw')
  const [dtwResult, setDtwResult] = useState<{ distance: number; path: Array<[number, number]> } | null>(null)
  const [clusterResult, setClusterResult] = useState<{ clusters: Array<{ start: number; end: number; stat: number; p: number }> } | null>(null)
  const [isProcessing, setIsProcessing] = useState(false)

  const canvasRef = useRef<HTMLCanvasElement>(null)
  const containerRef = useRef<HTMLDivElement>(null)

  const channelLabels = activeDataset?.channels.map(ch => ch.label) ?? []
  const events = activeDataset?.events ?? []
  const eventTypes = [...new Set(events.map(e => e.type))]

  useEffect(() => {
    if (channelLabels.length > 0 && !selectedChannel) {
      setSelectedChannel(channelLabels[0])
    }
    if (channelLabels.length > 1 && !referenceChannel) {
      setReferenceChannel(channelLabels[1])
    }
  }, [channelLabels, selectedChannel, referenceChannel])

  const handleAnalyze = () => {
    if (!generatedData || !selectedChannel || !activeDataset) return

    setIsProcessing(true)
    setTimeout(() => {
      if (analysisType === 'dtw' && referenceChannel) {
        const sig1 = generatedData.signals[selectedChannel]?.slice(0, 500) ?? []
        const sig2 = generatedData.signals[referenceChannel]?.slice(0, 500) ?? []
        const result = dtwDistance(sig1, sig2)
        setDtwResult(result)
        setClusterResult(null)
      } else if (analysisType === 'cluster' && eventTypes.length >= 2) {
        const signal = generatedData.signals[selectedChannel]
        if (!signal) {
          setIsProcessing(false)
          return
        }

        // Extract epochs for two event types
        const eventsA = events.filter(e => e.type === eventTypes[0]).map(e => e.onset)
        const eventsB = events.filter(e => e.type === eventTypes[1]).map(e => e.onset)

        const epochsA = extractEpochs(signal, activeDataset.samplingRate, eventsA, 0.2, 0.8)
        const epochsB = extractEpochs(signal, activeDataset.samplingRate, eventsB, 0.2, 0.8)

        // Baseline correct
        const correctedA = epochsA.map(e => baselineCorrect(e, activeDataset.samplingRate, 0, 0.2))
        const correctedB = epochsB.map(e => baselineCorrect(e, activeDataset.samplingRate, 0, 0.2))

        const result = clusterPermutationTest(correctedA, correctedB, 500)
        setClusterResult(result)
        setDtwResult(null)
      }
      setIsProcessing(false)
    }, 500)
  }

  // Draw DTW results
  useEffect(() => {
    const canvas = canvasRef.current
    const container = containerRef.current
    if (!canvas || !container || (!dtwResult && !clusterResult) || !generatedData) return

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
    const margin = { top: 30, right: 20, bottom: 40, left: 60 }

    ctx.fillStyle = '#f8fafc'
    ctx.fillRect(0, 0, width, height)

    const plotWidth = width - margin.left - margin.right
    const plotHeight = height - margin.top - margin.bottom

    if (dtwResult) {
      const sig1 = generatedData.signals[selectedChannel]?.slice(0, 500) ?? []
      const sig2 = generatedData.signals[referenceChannel]?.slice(0, 500) ?? []

      const allVals = [...sig1, ...sig2]
      const minVal = Math.min(...allVals)
      const maxVal = Math.max(...allVals)
      const range = maxVal - minVal || 1

      // Draw signal 1
      ctx.beginPath()
      ctx.strokeStyle = '#3b82f6'
      ctx.lineWidth = 1.5
      for (let i = 0; i < sig1.length; i++) {
        const x = margin.left + (i / sig1.length) * plotWidth
        const y = margin.top + plotHeight * 0.25 * (1 - (sig1[i] - minVal) / range) + plotHeight * 0.1
        if (i === 0) ctx.moveTo(x, y)
        else ctx.lineTo(x, y)
      }
      ctx.stroke()

      // Draw signal 2
      ctx.beginPath()
      ctx.strokeStyle = '#ef4444'
      ctx.lineWidth = 1.5
      for (let i = 0; i < sig2.length; i++) {
        const x = margin.left + (i / sig2.length) * plotWidth
        const y = margin.top + plotHeight * 0.25 * (1 - (sig2[i] - minVal) / range) + plotHeight * 0.5
        if (i === 0) ctx.moveTo(x, y)
        else ctx.lineTo(x, y)
      }
      ctx.stroke()

      // Draw alignment lines (sample of path)
      ctx.strokeStyle = '#94a3b8'
      ctx.lineWidth = 0.5
      ctx.globalAlpha = 0.3
      const step = Math.max(1, Math.floor(dtwResult.path.length / 50))
      for (let i = 0; i < dtwResult.path.length; i += step) {
        const [i1, i2] = dtwResult.path[i]
        const x1 = margin.left + (i1 / sig1.length) * plotWidth
        const y1 = margin.top + plotHeight * 0.25 * (1 - (sig1[i1] - minVal) / range) + plotHeight * 0.1
        const x2 = margin.left + (i2 / sig2.length) * plotWidth
        const y2 = margin.top + plotHeight * 0.25 * (1 - (sig2[i2] - minVal) / range) + plotHeight * 0.5
        ctx.beginPath()
        ctx.moveTo(x1, y1)
        ctx.lineTo(x2, y2)
        ctx.stroke()
      }
      ctx.globalAlpha = 1

      // Labels
      ctx.fillStyle = '#1e293b'
      ctx.font = '11px Inter, sans-serif'
      ctx.textAlign = 'center'
      ctx.fillText(`DTW距離: ${dtwResult.distance.toFixed(2)}`, width / 2, 15)
      ctx.fillStyle = '#3b82f6'
      ctx.fillText(selectedChannel, margin.left + 30, margin.top + plotHeight * 0.15)
      ctx.fillStyle = '#ef4444'
      ctx.fillText(referenceChannel, margin.left + 30, margin.top + plotHeight * 0.55)

    } else if (clusterResult) {
      ctx.fillStyle = '#1e293b'
      ctx.font = '12px Inter, sans-serif'
      ctx.textAlign = 'center'
      ctx.fillText('クラスター置換検定結果', width / 2, 15)

      if (clusterResult.clusters.length === 0) {
        ctx.fillText('有意なクラスターが見つかりませんでした', width / 2, height / 2)
      } else {
        const nClusters = clusterResult.clusters.length
        const barWidth = Math.min(60, plotWidth / nClusters * 0.7)

        clusterResult.clusters.forEach((cluster, i) => {
          const x = margin.left + (i + 0.5) * (plotWidth / nClusters)
          const barHeight = Math.abs(cluster.stat) / Math.max(...clusterResult.clusters.map(c => Math.abs(c.stat))) * plotHeight * 0.6

          ctx.fillStyle = cluster.p < 0.05 ? '#22c55e' : '#94a3b8'
          ctx.fillRect(x - barWidth / 2, margin.top + plotHeight - barHeight, barWidth, barHeight)

          ctx.fillStyle = '#1e293b'
          ctx.font = '10px Inter, sans-serif'
          ctx.fillText(`${cluster.start}-${cluster.end}`, x, height - 25)
          ctx.fillText(`p=${cluster.p.toFixed(3)}`, x, height - 10)
        })
      }
    }

  }, [dtwResult, clusterResult, generatedData, selectedChannel, referenceChannel])

  return (
    <div className="space-y-6">
      <section className="rounded-lg border p-4">
        <h3 className="font-medium">高度な解析</h3>
        <div className="mt-4 grid gap-4 md:grid-cols-4">
          <div>
            <label className="block text-sm">解析タイプ</label>
            <select
              value={analysisType}
              onChange={(e) => { setAnalysisType(e.target.value as typeof analysisType); setDtwResult(null); setClusterResult(null) }}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
            >
              <option value="dtw">Dynamic Time Warping</option>
              <option value="cluster">クラスター置換検定</option>
            </select>
          </div>
          <div>
            <label className="block text-sm">チャンネル</label>
            <select
              value={selectedChannel}
              onChange={(e) => setSelectedChannel(e.target.value)}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
            >
              {channelLabels.map(ch => (
                <option key={ch} value={ch}>{ch}</option>
              ))}
            </select>
          </div>
          {analysisType === 'dtw' && (
            <div>
              <label className="block text-sm">参照チャンネル</label>
              <select
                value={referenceChannel}
                onChange={(e) => setReferenceChannel(e.target.value)}
                className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
              >
                {channelLabels.map(ch => (
                  <option key={ch} value={ch}>{ch}</option>
                ))}
              </select>
            </div>
          )}
          <div className="flex items-end">
            <button
              onClick={handleAnalyze}
              disabled={isProcessing || (analysisType === 'cluster' && eventTypes.length < 2)}
              className="w-full rounded-md bg-primary px-4 py-2 text-sm text-primary-foreground hover:bg-primary/90 disabled:opacity-50"
            >
              {isProcessing ? '処理中...' : '解析を実行'}
            </button>
          </div>
        </div>
        {analysisType === 'cluster' && eventTypes.length < 2 && (
          <p className="mt-2 text-sm text-amber-600">クラスター置換検定には2つ以上のイベントタイプが必要です</p>
        )}
      </section>

      <section className="rounded-lg border p-4">
        <h3 className="font-medium mb-4">結果</h3>
        <div ref={containerRef} className="h-64 w-full">
          {(dtwResult || clusterResult) ? (
            <canvas ref={canvasRef} />
          ) : (
            <div className="h-full flex items-center justify-center text-muted-foreground">
              解析を実行すると結果が表示されます
            </div>
          )}
        </div>
      </section>
    </div>
  )
}

// ========== Dimensionality Reduction Panel ==========
function DimRedPanel() {
  const { activeDatasetId, datasets, generatedSignals, ensureGeneratedSignals } = useDataStore()
  const activeDataset = activeDatasetId ? datasets[activeDatasetId] : null
  const generatedData = activeDatasetId ? generatedSignals[activeDatasetId] ?? null : null

  useEffect(() => {
    if (activeDatasetId && activeDataset?.metadata?.isDemo && !generatedData) {
      ensureGeneratedSignals(activeDatasetId)
    }
  }, [activeDatasetId, activeDataset, generatedData, ensureGeneratedSignals])

  const [nComponents, setNComponents] = useState(2)
  const [pcaResult, setPcaResult] = useState<{
    scores: number[][]
    loadings: number[][]
    variance: number[]
    explained: number[]
    channels: string[]
  } | null>(null)
  const [isProcessing, setIsProcessing] = useState(false)

  const canvasRef = useRef<HTMLCanvasElement>(null)
  const containerRef = useRef<HTMLDivElement>(null)

  const handleRunPCA = () => {
    if (!generatedData || !activeDataset) return

    setIsProcessing(true)
    setTimeout(() => {
      const channels = Object.keys(generatedData.signals)
      // Use first 1000 samples as observations, channels as features
      const nSamples = Math.min(1000, generatedData.time.length)
      const data: number[][] = []

      for (let i = 0; i < nSamples; i++) {
        const row: number[] = []
        for (const ch of channels) {
          row.push(generatedData.signals[ch][i] ?? 0)
        }
        data.push(row)
      }

      const result = computePCA(data, nComponents)
      setPcaResult({ ...result, channels })
      setIsProcessing(false)
    }, 500)
  }

  // Draw PCA results
  useEffect(() => {
    const canvas = canvasRef.current
    const container = containerRef.current
    if (!canvas || !container || !pcaResult) return

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
    const margin = { top: 30, right: 20, bottom: 40, left: 60 }

    ctx.fillStyle = '#f8fafc'
    ctx.fillRect(0, 0, width, height)

    const plotWidth = width - margin.left - margin.right
    const plotHeight = height - margin.top - margin.bottom

    if (pcaResult.scores.length > 0 && pcaResult.scores[0].length >= 2) {
      // Find data range
      const pc1 = pcaResult.scores.map(s => s[0])
      const pc2 = pcaResult.scores.map(s => s[1])
      const minX = Math.min(...pc1)
      const maxX = Math.max(...pc1)
      const minY = Math.min(...pc2)
      const maxY = Math.max(...pc2)
      const rangeX = maxX - minX || 1
      const rangeY = maxY - minY || 1

      // Draw points
      ctx.fillStyle = '#3b82f6'
      for (let i = 0; i < pcaResult.scores.length; i++) {
        const x = margin.left + ((pc1[i] - minX) / rangeX) * plotWidth
        const y = margin.top + plotHeight - ((pc2[i] - minY) / rangeY) * plotHeight
        ctx.beginPath()
        ctx.arc(x, y, 2, 0, 2 * Math.PI)
        ctx.fill()
      }

      // Labels
      ctx.fillStyle = '#1e293b'
      ctx.font = '11px Inter, sans-serif'
      ctx.textAlign = 'center'
      ctx.fillText(`PC1 (${(pcaResult.explained[0] * 100).toFixed(1)}%)`, width / 2, height - 5)

      ctx.save()
      ctx.translate(15, height / 2)
      ctx.rotate(-Math.PI / 2)
      ctx.fillText(`PC2 (${(pcaResult.explained[1] * 100).toFixed(1)}%)`, 0, 0)
      ctx.restore()

      ctx.fillText('PCA スコアプロット', width / 2, 15)
    }

  }, [pcaResult])

  return (
    <div className="space-y-6">
      <section className="rounded-lg border p-4">
        <h3 className="font-medium">次元削減</h3>
        <div className="mt-4 grid gap-4 md:grid-cols-3">
          <div>
            <label className="block text-sm">手法</label>
            <select
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
              defaultValue="pca"
            >
              <option value="pca">主成分分析 (PCA)</option>
              <option value="umap" disabled>UMAP (準備中)</option>
              <option value="tsne" disabled>t-SNE (準備中)</option>
            </select>
          </div>
          <div>
            <label className="block text-sm">成分数</label>
            <input
              type="number"
              value={nComponents}
              onChange={(e) => setNComponents(parseInt(e.target.value))}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
              min="2"
              max="10"
            />
          </div>
          <div className="flex items-end">
            <button
              onClick={handleRunPCA}
              disabled={isProcessing}
              className="w-full rounded-md bg-primary px-4 py-2 text-sm text-primary-foreground hover:bg-primary/90 disabled:opacity-50"
            >
              {isProcessing ? '計算中...' : 'PCAを実行'}
            </button>
          </div>
        </div>
      </section>

      <div className="grid gap-6 lg:grid-cols-2">
        <section className="rounded-lg border p-4">
          <h3 className="font-medium mb-4">スコアプロット</h3>
          <div ref={containerRef} className="h-64 w-full">
            {pcaResult ? (
              <canvas ref={canvasRef} />
            ) : (
              <div className="h-full flex items-center justify-center text-muted-foreground">
                PCAを実行すると結果が表示されます
              </div>
            )}
          </div>
        </section>

        <section className="rounded-lg border p-4">
          <h3 className="font-medium mb-4">分散説明率</h3>
          {pcaResult ? (
            <div className="space-y-2">
              {pcaResult.explained.map((exp, i) => (
                <div key={i} className="flex items-center gap-2">
                  <span className="text-sm w-12">PC{i + 1}</span>
                  <div className="flex-1 h-4 bg-muted rounded overflow-hidden">
                    <div
                      className="h-full bg-primary"
                      style={{ width: `${exp * 100}%` }}
                    />
                  </div>
                  <span className="text-sm w-16 text-right">{(exp * 100).toFixed(1)}%</span>
                </div>
              ))}
              <div className="mt-4 pt-2 border-t">
                <div className="flex justify-between text-sm">
                  <span>累積分散説明率:</span>
                  <span className="font-medium">
                    {(pcaResult.explained.reduce((a, b) => a + b, 0) * 100).toFixed(1)}%
                  </span>
                </div>
              </div>
            </div>
          ) : (
            <div className="h-48 flex items-center justify-center text-muted-foreground">
              PCAを実行すると分散説明率が表示されます
            </div>
          )}
        </section>
      </div>
    </div>
  )
}

// ========== Biomechanics Panel ==========
function BiomechPanel() {
  const { activeDatasetId, datasets, generatedSignals, ensureGeneratedSignals } = useDataStore()
  const activeDataset = activeDatasetId ? datasets[activeDatasetId] : null
  const generatedData = activeDatasetId ? generatedSignals[activeDatasetId] ?? null : null

  useEffect(() => {
    if (activeDatasetId && activeDataset?.metadata?.isDemo && !generatedData) {
      ensureGeneratedSignals(activeDatasetId)
    }
  }, [activeDatasetId, activeDataset, generatedData, ensureGeneratedSignals])

  const [selectedChannel, setSelectedChannel] = useState('')
  const [threshold, setThreshold] = useState(0)
  const [normalizedSignal, setNormalizedSignal] = useState<number[] | null>(null)
  const [phases, setPhases] = useState<number[][] | null>(null)
  const [isProcessing, setIsProcessing] = useState(false)

  const canvasRef = useRef<HTMLCanvasElement>(null)
  const containerRef = useRef<HTMLDivElement>(null)

  const channelLabels = activeDataset?.channels.map(ch => ch.label) ?? []

  useEffect(() => {
    if (channelLabels.length > 0 && !selectedChannel) {
      setSelectedChannel(channelLabels[0])
    }
  }, [channelLabels, selectedChannel])

  useEffect(() => {
    if (generatedData && selectedChannel) {
      const signal = generatedData.signals[selectedChannel]
      if (signal) {
        const mean = signal.reduce((a, b) => a + b, 0) / signal.length
        setThreshold(mean)
      }
    }
  }, [generatedData, selectedChannel])

  const handleNormalize = () => {
    if (!generatedData || !selectedChannel) return

    setIsProcessing(true)
    setTimeout(() => {
      const signal = generatedData.signals[selectedChannel]
      if (!signal) {
        setIsProcessing(false)
        return
      }

      // Detect events (zero crossings or threshold crossings)
      const events = detectThresholdEvents(signal, threshold, 'rising', 50)

      if (events.length >= 2) {
        // Segment into phases
        const segmented = segmentPhases(signal, events)

        // Normalize each phase to 101 points (0-100%)
        const normalized = segmented.map(phase => normalizeToPercent(phase, 101))

        // Average across phases
        const avgPhase: number[] = new Array(101).fill(0)
        for (const phase of normalized) {
          for (let i = 0; i < 101; i++) {
            avgPhase[i] += phase[i] / normalized.length
          }
        }

        setNormalizedSignal(avgPhase)
        setPhases(normalized)
      } else {
        // If no events found, just normalize the whole signal
        setNormalizedSignal(normalizeToPercent(signal, 101))
        setPhases(null)
      }
      setIsProcessing(false)
    }, 100)
  }

  // Draw normalized signal
  useEffect(() => {
    const canvas = canvasRef.current
    const container = containerRef.current
    if (!canvas || !container || !normalizedSignal) return

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
    const margin = { top: 30, right: 20, bottom: 40, left: 60 }

    ctx.fillStyle = '#f8fafc'
    ctx.fillRect(0, 0, width, height)

    const plotWidth = width - margin.left - margin.right
    const plotHeight = height - margin.top - margin.bottom

    // Find range from all phases
    let minVal = Math.min(...normalizedSignal)
    let maxVal = Math.max(...normalizedSignal)
    if (phases) {
      for (const phase of phases) {
        minVal = Math.min(minVal, ...phase)
        maxVal = Math.max(maxVal, ...phase)
      }
    }
    const range = maxVal - minVal || 1

    // Draw individual phases (faded)
    if (phases) {
      ctx.globalAlpha = 0.2
      ctx.strokeStyle = '#94a3b8'
      ctx.lineWidth = 1
      for (const phase of phases) {
        ctx.beginPath()
        for (let i = 0; i < phase.length; i++) {
          const x = margin.left + (i / (phase.length - 1)) * plotWidth
          const y = margin.top + plotHeight * (1 - (phase[i] - minVal) / range)
          if (i === 0) ctx.moveTo(x, y)
          else ctx.lineTo(x, y)
        }
        ctx.stroke()
      }
      ctx.globalAlpha = 1
    }

    // Draw average/normalized signal
    ctx.beginPath()
    ctx.strokeStyle = '#3b82f6'
    ctx.lineWidth = 2
    for (let i = 0; i < normalizedSignal.length; i++) {
      const x = margin.left + (i / (normalizedSignal.length - 1)) * plotWidth
      const y = margin.top + plotHeight * (1 - (normalizedSignal[i] - minVal) / range)
      if (i === 0) ctx.moveTo(x, y)
      else ctx.lineTo(x, y)
    }
    ctx.stroke()

    // X axis labels (0%, 25%, 50%, 75%, 100%)
    ctx.fillStyle = '#1e293b'
    ctx.font = '11px Inter, sans-serif'
    ctx.textAlign = 'center'
    for (let i = 0; i <= 4; i++) {
      const pct = i * 25
      const x = margin.left + (pct / 100) * plotWidth
      ctx.fillText(`${pct}%`, x, height - 10)
    }

    ctx.fillText('正規化された動作サイクル', width / 2, 15)

    ctx.save()
    ctx.translate(15, height / 2)
    ctx.rotate(-Math.PI / 2)
    ctx.fillText('振幅', 0, 0)
    ctx.restore()

  }, [normalizedSignal, phases])

  return (
    <div className="space-y-6">
      <section className="rounded-lg border p-4">
        <h3 className="font-medium">バイオメカニクス解析</h3>
        <div className="mt-4 grid gap-4 md:grid-cols-4">
          <div>
            <label className="block text-sm">チャンネル</label>
            <select
              value={selectedChannel}
              onChange={(e) => { setSelectedChannel(e.target.value); setNormalizedSignal(null); setPhases(null) }}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
            >
              {channelLabels.map(ch => (
                <option key={ch} value={ch}>{ch}</option>
              ))}
            </select>
          </div>
          <div>
            <label className="block text-sm">イベント閾値</label>
            <input
              type="number"
              value={threshold.toFixed(2)}
              onChange={(e) => setThreshold(parseFloat(e.target.value))}
              className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
              step="0.1"
            />
          </div>
          <div className="flex items-end">
            <button
              onClick={handleNormalize}
              disabled={isProcessing || !selectedChannel}
              className="w-full rounded-md bg-primary px-4 py-2 text-sm text-primary-foreground hover:bg-primary/90 disabled:opacity-50"
            >
              {isProcessing ? '処理中...' : '正規化を実行'}
            </button>
          </div>
        </div>
        <p className="mt-2 text-sm text-muted-foreground">
          閾値を上回る立ち上がり点でサイクルを検出し、0-100%に正規化します
        </p>
      </section>

      <section className="rounded-lg border p-4">
        <h3 className="font-medium mb-4">正規化された動作パターン</h3>
        <div ref={containerRef} className="h-64 w-full">
          {normalizedSignal ? (
            <canvas ref={canvasRef} />
          ) : (
            <div className="h-full flex items-center justify-center text-muted-foreground">
              正規化を実行すると結果が表示されます
            </div>
          )}
        </div>
        {phases && (
          <div className="mt-2 text-sm text-muted-foreground">
            検出されたサイクル数: {phases.length}
          </div>
        )}
      </section>
    </div>
  )
}

// ========== Main Component ==========
export function AnalysisWorkspace() {
  const [activeTab, setActiveTab] = useState<TabId>('preprocess')
  const { activeDatasetId, datasets } = useDataStore()

  const activeDataset = activeDatasetId ? datasets[activeDatasetId] : null

  const tabs: { id: TabId; label: string }[] = [
    { id: 'preprocess', label: '前処理' },
    { id: 'timefreq', label: '時間周波数' },
    { id: 'epoch', label: 'エポック/ERP' },
    { id: 'connectivity', label: '接続性' },
    { id: 'network', label: 'ネットワーク' },
    { id: 'statistics', label: '統計' },
    { id: 'advanced', label: '高度な解析' },
    { id: 'dimred', label: '次元削減' },
    { id: 'biomech', label: 'バイオメカニクス' }
  ]

  return (
    <div className="flex h-full flex-col">
      {/* Dataset selector */}
      <div className="border-b px-6 py-3">
        <div className="flex items-center gap-4">
          <span className="text-sm text-muted-foreground">データセット:</span>
          {activeDataset ? (
            <span className="font-medium">{activeDataset.name}</span>
          ) : (
            <span className="text-muted-foreground">選択されていません</span>
          )}
        </div>
      </div>

      {/* Tabs */}
      <div className="border-b px-6">
        <div className="flex gap-4">
          {tabs.map((tab) => (
            <Tab
              key={tab.id}
              label={tab.label}
              isActive={activeTab === tab.id}
              onClick={() => setActiveTab(tab.id)}
            />
          ))}
        </div>
      </div>

      {/* Content */}
      <div className="flex-1 overflow-auto p-6">
        {!activeDataset ? (
          <div className="flex h-full items-center justify-center text-muted-foreground">
            <div className="text-center">
              <svg className="mx-auto h-12 w-12" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M9 13h6m-3-3v6m5 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z" />
              </svg>
              <p className="mt-4">データブラウザでデータセットを選択してください</p>
            </div>
          </div>
        ) : (
          <>
            {activeTab === 'preprocess' && <PreprocessPanel />}
            {activeTab === 'timefreq' && <TimeFreqPanel />}
            {activeTab === 'epoch' && <EpochPanel />}
            {activeTab === 'connectivity' && <ConnectivityPanel />}
            {activeTab === 'network' && <NetworkPanel />}
            {activeTab === 'statistics' && <StatisticsPanel />}
            {activeTab === 'advanced' && <AdvancedPanel />}
            {activeTab === 'dimred' && <DimRedPanel />}
            {activeTab === 'biomech' && <BiomechPanel />}
          </>
        )}
      </div>
    </div>
  )
}
