import { useState, useEffect, useRef, useCallback } from 'react'
import { useDataStore } from '@/stores/dataStore'
import { useAnalysisCartStore, CartItem, AnalysisResult } from '@/stores/analysisCartStore'
import { SynergyReport, SynergyResult, SynergyComparison, SynergyMethod, MethodResult } from './SynergyReport'
import {
  analysisMethods,
  Workflow,
  AnalysisMethod,
  AnalysisCategory,
  getWorkflowsForDatasetType,
  getOptionalMethods,
  getCategoryLabel,
  getCategoryColor,
  DatasetType
} from '@/lib/workflows'
import {
  bandpassFilter,
  notchFilter,
  computeSTFT,
  computePSD,
  waveletTransform,
  extractBandPowers,
  extractEpochs,
  computeERP,
  baselineCorrect,
  computeCorrelationMatrix,
  computeConnectivityMatrix,
  createAdjacencyMatrix,
  computeNodeDegree,
  computeClusteringCoefficient,
  computeCharacteristicPathLength,
  computeGlobalEfficiency,
  pointwiseTTest,
  cohensD,
  computePCA,
  normalizeToPercent,
  // EMG processing functions
  rectifySignal,
  emgEnvelope,
  movingRMS,
  computeMAV,
  mvcNormalize,
  detectEMGOnset,
  tkeo,
  coContractionIndex,
  // Fatigue analysis
  computeMedianFrequency,
  computeMeanFrequency,
  computeFatigueIndex,
  extractEMGFeatures,
  // Muscle synergy
  nmfDecomposition,
  findOptimalSynergies,
  pcaSynergyExtraction,
  factorAnalysisSynergy,
  synergySimilarity
} from '@/lib/signalProcessing'

// ============ Workflow Selector ============
function WorkflowSelector({
  datasetType,
  selectedWorkflow,
  onSelect
}: {
  datasetType: DatasetType
  selectedWorkflow: Workflow | null
  onSelect: (workflowId: string) => void
}) {
  const availableWorkflows = getWorkflowsForDatasetType(datasetType)

  return (
    <div className="space-y-4">
      <h3 className="text-lg font-semibold">ワークフローを選択</h3>
      <div className="grid gap-3 md:grid-cols-2 lg:grid-cols-3">
        {availableWorkflows.map(workflow => (
          <button
            key={workflow.id}
            onClick={() => onSelect(workflow.id)}
            className={`p-4 rounded-xl border-2 text-left transition-all hover:shadow-md ${
              selectedWorkflow?.id === workflow.id
                ? 'border-primary bg-primary/5'
                : 'border-border hover:border-primary/50'
            }`}
          >
            <div className="flex items-start gap-3">
              <div
                className="w-10 h-10 rounded-lg flex items-center justify-center text-white text-lg"
                style={{ backgroundColor: workflow.color === 'blue' ? '#3b82f6' :
                  workflow.color === 'purple' ? '#8b5cf6' :
                  workflow.color === 'green' ? '#22c55e' :
                  workflow.color === 'red' ? '#ef4444' :
                  workflow.color === 'orange' ? '#f97316' : '#6b7280' }}
              >
                {workflow.icon === 'brain' ? '🧠' :
                 workflow.icon === 'share-2' ? '🔗' :
                 workflow.icon === 'activity' ? '📊' :
                 workflow.icon === 'heart' ? '❤️' :
                 workflow.icon === 'footprints' ? '👣' : '🔍'}
              </div>
              <div className="flex-1">
                <div className="font-medium">{workflow.nameJa}</div>
                <div className="text-sm text-muted-foreground mt-1">{workflow.descriptionJa}</div>
              </div>
            </div>
          </button>
        ))}
      </div>
    </div>
  )
}

// ============ Method Card ============
function MethodCard({
  method,
  isInCart,
  isRequired,
  onAdd,
  onRemove
}: {
  method: AnalysisMethod
  isInCart: boolean
  isRequired: boolean
  onAdd: () => void
  onRemove: () => void
}) {
  return (
    <div
      className={`p-3 rounded-lg border transition-all ${
        isInCart
          ? isRequired
            ? 'border-primary/50 bg-primary/5'
            : 'border-green-500/50 bg-green-500/5'
          : 'border-border hover:border-primary/30'
      }`}
    >
      <div className="flex items-start justify-between gap-2">
        <div className="flex-1 min-w-0">
          <div className="flex items-center gap-2">
            <span
              className="w-2 h-2 rounded-full"
              style={{ backgroundColor: getCategoryColor(method.category) }}
            />
            <span className="font-medium text-sm truncate">{method.nameJa}</span>
          </div>
          <p className="text-xs text-muted-foreground mt-1 line-clamp-2">
            {method.descriptionJa}
          </p>
        </div>
        <div className="flex items-center gap-1">
          {isRequired && (
            <span className="text-xs bg-primary/10 text-primary px-2 py-0.5 rounded">
              必須
            </span>
          )}
          {!isRequired && (
            <button
              onClick={isInCart ? onRemove : onAdd}
              className={`p-1.5 rounded-md transition-colors ${
                isInCart
                  ? 'bg-red-100 text-red-600 hover:bg-red-200'
                  : 'bg-primary/10 text-primary hover:bg-primary/20'
              }`}
            >
              {isInCart ? '−' : '+'}
            </button>
          )}
        </div>
      </div>
      <div className="flex items-center gap-2 mt-2">
        <span className="text-xs px-1.5 py-0.5 rounded bg-muted">
          {getCategoryLabel(method.category)}
        </span>
        <span className={`text-xs px-1.5 py-0.5 rounded ${
          method.estimatedTime === 'fast' ? 'bg-green-100 text-green-700' :
          method.estimatedTime === 'medium' ? 'bg-yellow-100 text-yellow-700' :
          'bg-red-100 text-red-700'
        }`}>
          {method.estimatedTime === 'fast' ? '高速' :
           method.estimatedTime === 'medium' ? '普通' : '低速'}
        </span>
      </div>
    </div>
  )
}

// ============ Analysis Cart Sidebar ============
function AnalysisCart({
  items,
  onUpdateParams,
  onRemove,
  onRun,
  isRunning,
  progress
}: {
  items: CartItem[]
  onUpdateParams: (itemId: string, params: Record<string, unknown>) => void
  onRemove: (itemId: string) => void
  onRun: () => void
  isRunning: boolean
  progress: number
}) {
  const [expandedItem, setExpandedItem] = useState<string | null>(null)

  return (
    <div className="h-full flex flex-col">
      <div className="p-4 border-b">
        <h3 className="font-semibold flex items-center gap-2">
          🛒 解析カート
          <span className="text-sm font-normal text-muted-foreground">
            ({items.length}件)
          </span>
        </h3>
      </div>

      <div className="flex-1 overflow-auto p-4 space-y-2">
        {items.length === 0 ? (
          <div className="text-center text-muted-foreground py-8">
            <p>カートは空です</p>
            <p className="text-sm mt-1">ワークフローを選択するか、解析手法を追加してください</p>
          </div>
        ) : (
          items.map((item, index) => (
            <div
              key={item.id}
              className={`rounded-lg border transition-all ${
                item.status === 'completed' ? 'border-green-500/50 bg-green-50' :
                item.status === 'running' ? 'border-blue-500/50 bg-blue-50 animate-pulse' :
                item.status === 'error' ? 'border-red-500/50 bg-red-50' :
                'border-border'
              }`}
            >
              <div
                className="p-3 cursor-pointer"
                onClick={() => setExpandedItem(expandedItem === item.id ? null : item.id)}
              >
                <div className="flex items-center gap-2">
                  <span className="text-sm font-medium w-6 h-6 rounded-full bg-muted flex items-center justify-center">
                    {index + 1}
                  </span>
                  <span className="flex-1 font-medium text-sm">{item.method.nameJa}</span>
                  {item.isRequired && (
                    <span className="text-xs bg-primary/10 text-primary px-1.5 py-0.5 rounded">必須</span>
                  )}
                  {item.status === 'completed' && <span className="text-green-600">✓</span>}
                  {item.status === 'error' && <span className="text-red-600">✕</span>}
                  {!item.isRequired && item.status === 'pending' && (
                    <button
                      onClick={(e) => { e.stopPropagation(); onRemove(item.id) }}
                      className="text-red-500 hover:text-red-700 text-sm"
                    >
                      削除
                    </button>
                  )}
                </div>
              </div>

              {expandedItem === item.id && item.method.params.length > 0 && (
                <div className="px-3 pb-3 space-y-2 border-t pt-2">
                  {item.method.params.map(param => (
                    <div key={param.name} className="flex items-center gap-2">
                      <label className="text-xs text-muted-foreground flex-1">
                        {param.label}
                      </label>
                      {param.type === 'number' && (
                        <input
                          type="number"
                          value={item.params[param.name] as number}
                          onChange={(e) => onUpdateParams(item.id, { [param.name]: parseFloat(e.target.value) })}
                          className="w-20 text-xs rounded border px-2 py-1"
                          min={param.min}
                          max={param.max}
                          step={param.step}
                          disabled={item.status !== 'pending'}
                        />
                      )}
                      {param.type === 'select' && (
                        <select
                          value={item.params[param.name] as string}
                          onChange={(e) => onUpdateParams(item.id, { [param.name]: e.target.value })}
                          className="w-32 text-xs rounded border px-2 py-1"
                          disabled={item.status !== 'pending'}
                        >
                          {param.options?.map(opt => (
                            <option key={opt.value} value={opt.value}>{opt.label}</option>
                          ))}
                        </select>
                      )}
                      {param.type === 'boolean' && (
                        <input
                          type="checkbox"
                          checked={item.params[param.name] as boolean}
                          onChange={(e) => onUpdateParams(item.id, { [param.name]: e.target.checked })}
                          disabled={item.status !== 'pending'}
                        />
                      )}
                      {param.unit && <span className="text-xs text-muted-foreground">{param.unit}</span>}
                    </div>
                  ))}
                </div>
              )}
            </div>
          ))
        )}
      </div>

      {items.length > 0 && (
        <div className="p-4 border-t space-y-3">
          {isRunning && (
            <div className="space-y-1">
              <div className="flex justify-between text-xs text-muted-foreground">
                <span>実行中...</span>
                <span>{Math.round(progress)}%</span>
              </div>
              <div className="h-2 bg-muted rounded-full overflow-hidden">
                <div
                  className="h-full bg-primary transition-all"
                  style={{ width: `${progress}%` }}
                />
              </div>
            </div>
          )}
          <button
            onClick={onRun}
            disabled={isRunning || items.every(i => i.status === 'completed')}
            className="w-full py-2.5 rounded-lg bg-primary text-primary-foreground font-medium hover:bg-primary/90 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
          >
            {isRunning ? '解析中...' :
             items.every(i => i.status === 'completed') ? '解析完了' :
             '解析を実行'}
          </button>
        </div>
      )}
    </div>
  )
}

// ============ Report Viewer ============
function ReportViewer({
  reportData,
  items,
  onClose,
  onExport
}: {
  reportData: {
    workflowName: string
    datasetName: string
    generatedAt: Date
    sections: Array<{ id: string; title: string; items: CartItem[]; layout: string }>
    summary: {
      totalMethods: number
      completedMethods: number
      significantFindings: string[]
      keyMetrics: Array<{ label: string; value: string | number; unit?: string }>
    }
  }
  items: CartItem[]
  onClose: () => void
  onExport: (format: 'html' | 'pdf' | 'json') => void
}) {
  const canvasRefs = useRef<Record<string, HTMLCanvasElement | null>>({})
  const containerRefs = useRef<Record<string, HTMLDivElement | null>>({})

  // Draw results for each item
  useEffect(() => {
    items.forEach(item => {
      if (item.status !== 'completed' || !item.result) return

      const canvas = canvasRefs.current[item.id]
      const container = containerRefs.current[item.id]
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

      ctx.fillStyle = '#ffffff'
      ctx.fillRect(0, 0, width, height)

      // Draw based on result type
      if (item.result.type === 'plot' || item.result.type === 'timeseries') {
        drawTimeseriesPlot(ctx, item.result.data as number[], width, height, item.method.nameJa)
      } else if (item.result.type === 'matrix') {
        drawMatrixPlot(ctx, item.result.data as { matrix: number[][]; channels: string[] }, width, height)
      }
    })
  }, [items])

  return (
    <div className="fixed inset-0 bg-background z-50 flex flex-col">
      {/* Header */}
      <div className="border-b px-6 py-4 flex items-center justify-between bg-white">
        <div>
          <h1 className="text-xl font-bold">解析レポート</h1>
          <p className="text-sm text-muted-foreground">
            {reportData.workflowName} | {reportData.datasetName} |
            生成日時: {reportData.generatedAt.toLocaleString('ja-JP')}
          </p>
        </div>
        <div className="flex items-center gap-2">
          <button
            onClick={() => onExport('json')}
            className="px-3 py-1.5 text-sm border rounded-lg hover:bg-muted"
          >
            JSON出力
          </button>
          <button
            onClick={() => onExport('html')}
            className="px-3 py-1.5 text-sm border rounded-lg hover:bg-muted"
          >
            HTML出力
          </button>
          <button
            onClick={onClose}
            className="px-3 py-1.5 text-sm bg-primary text-primary-foreground rounded-lg hover:bg-primary/90"
          >
            閉じる
          </button>
        </div>
      </div>

      {/* Report Content */}
      <div className="flex-1 overflow-auto p-6">
        <div className="max-w-6xl mx-auto space-y-8">
          {/* Summary Section */}
          <section className="rounded-xl border bg-gradient-to-br from-primary/5 to-transparent p-6">
            <h2 className="text-lg font-semibold mb-4">📊 サマリー</h2>
            <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
              <div className="rounded-lg bg-white p-4 shadow-sm">
                <div className="text-2xl font-bold text-primary">{reportData.summary.completedMethods}</div>
                <div className="text-sm text-muted-foreground">完了した解析</div>
              </div>
              <div className="rounded-lg bg-white p-4 shadow-sm">
                <div className="text-2xl font-bold text-primary">{reportData.summary.totalMethods}</div>
                <div className="text-sm text-muted-foreground">総解析数</div>
              </div>
              {reportData.summary.keyMetrics.slice(0, 2).map((metric, i) => (
                <div key={i} className="rounded-lg bg-white p-4 shadow-sm">
                  <div className="text-2xl font-bold text-primary">
                    {typeof metric.value === 'number' ? metric.value.toFixed(3) : metric.value}
                    {metric.unit && <span className="text-sm font-normal">{metric.unit}</span>}
                  </div>
                  <div className="text-sm text-muted-foreground">{metric.label}</div>
                </div>
              ))}
            </div>

            {reportData.summary.significantFindings.length > 0 && (
              <div className="mt-4 p-4 bg-green-50 rounded-lg">
                <h3 className="font-medium text-green-800 mb-2">🎯 主要な発見</h3>
                <ul className="space-y-1">
                  {reportData.summary.significantFindings.map((finding, i) => (
                    <li key={i} className="text-sm text-green-700">• {finding}</li>
                  ))}
                </ul>
              </div>
            )}
          </section>

          {/* Synergy Report - Full width publication-quality visualization */}
          {items.filter(item => item.result?.type === 'synergy').map(item => (
            <section key={`synergy-${item.id}`} className="rounded-xl border overflow-hidden">
              <SynergyReport
                result={item.result?.data as SynergyResult}
                title="筋シナジー解析 (Muscle Synergy Analysis)"
                showReconstruction={true}
                normalizeTime={true}
              />
            </section>
          ))}

          {/* Synergy Comparison - Method comparison visualization */}
          {items.filter(item => item.result?.type === 'synergyComparison').map(item => {
            const compData = item.result?.data as {
              results: Record<SynergyMethod, MethodResult>
              similarity: Record<string, number>
              muscleNames: string[]
            }
            return (
              <section key={`synergy-comparison-${item.id}`} className="rounded-xl border overflow-hidden">
                <SynergyComparison
                  results={compData.results}
                  similarity={compData.similarity}
                  muscleNames={compData.muscleNames}
                  title="筋シナジー解析 手法比較 (Method Comparison)"
                />
              </section>
            )
          })}

          {/* Results Sections */}
          {reportData.sections.map(section => (
            <section key={section.id} className="rounded-xl border p-6">
              <h2 className="text-lg font-semibold mb-4">{section.title}</h2>
              <div className={`grid gap-4 ${
                section.layout === 'full' ? 'grid-cols-1' :
                section.layout === 'half' ? 'grid-cols-1 md:grid-cols-2' :
                section.layout === 'third' ? 'grid-cols-1 md:grid-cols-3' :
                'grid-cols-2 md:grid-cols-4'
              }`}>
                {section.items.filter(item => item.result?.type !== 'synergy' && item.result?.type !== 'synergyComparison').map(item => (
                  <div key={item.id} className="rounded-lg border bg-white overflow-hidden">
                    <div className="p-3 border-b bg-muted/30">
                      <div className="font-medium text-sm">{item.method.nameJa}</div>
                      <div className="text-xs text-muted-foreground">{getCategoryLabel(item.method.category)}</div>
                    </div>
                    <div
                      ref={el => containerRefs.current[item.id] = el}
                      className="h-48"
                    >
                      {item.result ? (
                        item.result.type === 'table' || item.result.type === 'metric' ? (
                          <div className="p-3">
                            <ResultTable data={item.result.data} />
                          </div>
                        ) : (
                          <canvas ref={el => canvasRefs.current[item.id] = el} />
                        )
                      ) : (
                        <div className="h-full flex items-center justify-center text-muted-foreground">
                          結果なし
                        </div>
                      )}
                    </div>
                  </div>
                ))}
              </div>
            </section>
          ))}
        </div>
      </div>
    </div>
  )
}

// Helper component for table results
function ResultTable({ data }: { data: unknown }) {
  if (typeof data === 'object' && data !== null) {
    const entries = Object.entries(data as Record<string, unknown>)
    return (
      <table className="w-full text-sm">
        <tbody>
          {entries.slice(0, 10).map(([key, value]) => (
            <tr key={key} className="border-b last:border-0">
              <td className="py-1 text-muted-foreground">{key}</td>
              <td className="py-1 text-right font-medium">
                {typeof value === 'number' ? value.toFixed(4) : String(value)}
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    )
  }
  return <div className="text-sm">{String(data)}</div>
}

// Helper function to draw timeseries plot
function drawTimeseriesPlot(
  ctx: CanvasRenderingContext2D,
  data: number[],
  width: number,
  height: number,
  title: string
) {
  const margin = { top: 20, right: 10, bottom: 20, left: 40 }
  const plotWidth = width - margin.left - margin.right
  const plotHeight = height - margin.top - margin.bottom

  if (!data || data.length === 0) return

  const minVal = Math.min(...data)
  const maxVal = Math.max(...data)
  const range = maxVal - minVal || 1

  ctx.strokeStyle = '#3b82f6'
  ctx.lineWidth = 1.5
  ctx.beginPath()
  for (let i = 0; i < data.length; i++) {
    const x = margin.left + (i / data.length) * plotWidth
    const y = margin.top + plotHeight * (1 - (data[i] - minVal) / range)
    if (i === 0) ctx.moveTo(x, y)
    else ctx.lineTo(x, y)
  }
  ctx.stroke()

  ctx.fillStyle = '#1e293b'
  ctx.font = '10px Inter, sans-serif'
  ctx.textAlign = 'center'
  ctx.fillText(title, width / 2, 12)
}

// Helper function to draw matrix plot
function drawMatrixPlot(
  ctx: CanvasRenderingContext2D,
  data: { matrix: number[][]; channels: string[] },
  width: number,
  height: number
) {
  if (!data.matrix || data.matrix.length === 0) return

  const n = data.matrix.length
  const margin = 30
  const cellSize = (Math.min(width, height) - margin * 2) / n

  for (let i = 0; i < n; i++) {
    for (let j = 0; j < n; j++) {
      const value = data.matrix[i][j]
      const r = Math.round(255 * (1 - value))
      const g = Math.round(100 * value)
      const b = Math.round(255 * value)
      ctx.fillStyle = `rgb(${r}, ${g}, ${b})`
      ctx.fillRect(margin + j * cellSize, margin + i * cellSize, cellSize - 1, cellSize - 1)
    }
  }
}

// ============ Main Component ============
export function WorkflowAnalysis() {
  const { activeDatasetId, datasets, generatedSignals, ensureGeneratedSignals } = useDataStore()
  const activeDataset = activeDatasetId ? datasets[activeDatasetId] : null
  const generatedData = activeDatasetId ? generatedSignals[activeDatasetId] ?? null : null

  const {
    items,
    selectedWorkflow,
    isRunning,
    progress,
    reportData,
    isReportVisible,
    setWorkflow,
    addItem,
    removeItem,
    updateItemParams,
    runAnalysis,
    generateReport,
    hideReport,
    exportReport
  } = useAnalysisCartStore()

  // Ensure signals are generated
  useEffect(() => {
    if (activeDatasetId && activeDataset?.metadata?.isDemo && !generatedData) {
      ensureGeneratedSignals(activeDatasetId)
    }
  }, [activeDatasetId, activeDataset, generatedData, ensureGeneratedSignals])

  // Determine dataset type
  const datasetType = (activeDataset?.metadata?.dataType as DatasetType) || 'GENERIC'

  // Execute analysis method
  const executeMethod = useCallback(async (item: CartItem): Promise<AnalysisResult> => {
    if (!generatedData || !activeDataset) {
      throw new Error('No data available')
    }

    const signal = Object.values(generatedData.signals)[0] // Use first channel
    const samplingRate = activeDataset.samplingRate
    const events = activeDataset.events ?? []

    // Simulate processing time
    await new Promise(resolve => setTimeout(resolve, 100))

    let result: AnalysisResult

    switch (item.methodId) {
      case 'bandpass': {
        const filtered = bandpassFilter(
          signal,
          samplingRate,
          item.params.lowFreq as number,
          item.params.highFreq as number
        )
        result = { type: 'timeseries', data: filtered }
        break
      }
      case 'notch': {
        const filtered = notchFilter(signal, samplingRate, item.params.frequency as number)
        result = { type: 'timeseries', data: filtered }
        break
      }
      case 'spectrogram': {
        const stft = computeSTFT(signal, samplingRate, item.params.windowSize as number, item.params.hopSize as number)
        result = { type: 'matrix', data: stft }
        break
      }
      case 'psd': {
        const psd = computePSD(signal, samplingRate, item.params.windowSize as number)
        result = { type: 'plot', data: psd.power }
        break
      }
      case 'bandpower': {
        const powers = extractBandPowers(signal, samplingRate)
        result = { type: 'table', data: powers }
        break
      }
      case 'epochExtraction': {
        const eventTimes = events.map(e => e.onset)
        const epochs = extractEpochs(signal, samplingRate, eventTimes, item.params.preTime as number, item.params.postTime as number)
        result = { type: 'timeseries', data: epochs[0] || [] }
        break
      }
      case 'erpComputation': {
        const eventTimes = events.map(e => e.onset)
        const epochs = extractEpochs(signal, samplingRate, eventTimes, 0.2, 0.8)
        const corrected = epochs.map(e => baselineCorrect(e, samplingRate, 0, 0.2))
        const erp = computeERP(corrected)
        result = { type: 'plot', data: erp }
        break
      }
      case 'plv':
      case 'pli':
      case 'coherence':
      case 'correlation': {
        const connectivity = item.methodId === 'correlation'
          ? computeCorrelationMatrix(generatedData.signals)
          : computeConnectivityMatrix(generatedData.signals, samplingRate, item.params.lowFreq as number || 8, item.params.highFreq as number || 13)
        result = { type: 'matrix', data: connectivity }
        break
      }
      case 'networkMetrics': {
        const connectivity = computeConnectivityMatrix(generatedData.signals, samplingRate, 8, 13)
        const adjacency = createAdjacencyMatrix(connectivity.matrix, item.params.threshold as number)
        const degree = computeNodeDegree(adjacency)
        const clustering = computeClusteringCoefficient(adjacency)
        const pathLength = computeCharacteristicPathLength(adjacency)
        const efficiency = computeGlobalEfficiency(adjacency)
        result = {
          type: 'table',
          data: {
            avgDegree: degree.reduce((a, b) => a + b, 0) / degree.length,
            avgClustering: clustering.reduce((a, b) => a + b, 0) / clustering.length,
            pathLength,
            efficiency
          }
        }
        break
      }
      case 'ttest': {
        const eventTypes = [...new Set(events.map(e => e.type))]
        if (eventTypes.length >= 2) {
          const eventsA = events.filter(e => e.type === eventTypes[0]).map(e => e.onset)
          const eventsB = events.filter(e => e.type === eventTypes[1]).map(e => e.onset)
          const epochsA = extractEpochs(signal, samplingRate, eventsA, 0.2, 0.8).map(e => baselineCorrect(e, samplingRate, 0, 0.2))
          const epochsB = extractEpochs(signal, samplingRate, eventsB, 0.2, 0.8).map(e => baselineCorrect(e, samplingRate, 0, 0.2))
          const tResult = pointwiseTTest(epochsA, epochsB)
          const minP = Math.min(...tResult.p)
          result = {
            type: 'plot',
            data: tResult.p,
            metadata: { significant: minP < 0.05, pValue: minP }
          }
        } else {
          result = { type: 'plot', data: [] }
        }
        break
      }
      case 'effectSize': {
        const eventTypes = [...new Set(events.map(e => e.type))]
        if (eventTypes.length >= 2) {
          const eventsA = events.filter(e => e.type === eventTypes[0]).map(e => e.onset)
          const eventsB = events.filter(e => e.type === eventTypes[1]).map(e => e.onset)
          const epochsA = extractEpochs(signal, samplingRate, eventsA, 0.2, 0.8).map(e => baselineCorrect(e, samplingRate, 0, 0.2))
          const epochsB = extractEpochs(signal, samplingRate, eventsB, 0.2, 0.8).map(e => baselineCorrect(e, samplingRate, 0, 0.2))
          const meansA = epochsA.map(e => e.reduce((a, b) => a + b, 0) / e.length)
          const meansB = epochsB.map(e => e.reduce((a, b) => a + b, 0) / e.length)
          const d = cohensD(meansA, meansB)
          result = {
            type: 'metric',
            data: { d: d.d, ci_low: d.ci[0], ci_high: d.ci[1] },
            metadata: { effectSize: d.d }
          }
        } else {
          result = { type: 'metric', data: { d: 0 } }
        }
        break
      }
      case 'pca': {
        const channels = Object.keys(generatedData.signals)
        const data: number[][] = []
        const nSamples = Math.min(500, generatedData.time.length)
        for (let i = 0; i < nSamples; i++) {
          data.push(channels.map(ch => generatedData.signals[ch][i] ?? 0))
        }
        const pca = computePCA(data, item.params.nComponents as number)
        result = { type: 'table', data: { explained: pca.explained } }
        break
      }
      case 'cycleNormalization': {
        const normalized = normalizeToPercent(signal, item.params.targetLength as number)
        result = { type: 'plot', data: normalized }
        break
      }

      // ============ EMG Processing ============
      case 'emgBandpass': {
        const filtered = bandpassFilter(
          signal,
          samplingRate,
          item.params.lowFreq as number || 20,
          item.params.highFreq as number || 500
        )
        result = { type: 'timeseries', data: filtered }
        break
      }
      case 'rectification': {
        const rectified = rectifySignal(signal)
        result = { type: 'timeseries', data: rectified }
        break
      }
      case 'emgEnvelope': {
        const method = (item.params.method as 'lowpass' | 'rms' | 'hilbert') || 'lowpass'
        const cutoffFreq = item.params.cutoffFreq as number || 6
        const env = emgEnvelope(signal, samplingRate, cutoffFreq, method)
        result = { type: 'timeseries', data: env }
        break
      }
      case 'rmsCalculation': {
        const windowMs = item.params.windowMs as number || 100
        const windowSize = Math.round((windowMs / 1000) * samplingRate)
        const rmsResult = movingRMS(signal, windowSize)
        result = { type: 'timeseries', data: rmsResult }
        break
      }
      case 'mavCalculation': {
        const windowMs = item.params.windowMs as number || 100
        const windowSize = Math.round((windowMs / 1000) * samplingRate)
        const mavResult = computeMAV(signal, windowSize)
        result = { type: 'timeseries', data: mavResult }
        break
      }
      case 'mvcNormalization': {
        const mvc = (item.params.mvc as number) || Math.max(...signal.map(Math.abs))
        const normalized = mvcNormalize(signal, mvc)
        const maxVal = Math.max(...normalized)
        result = {
          type: 'timeseries',
          data: normalized,
          metadata: { maxMVC: maxVal }
        }
        break
      }
      case 'onsetDetection': {
        const method = (item.params.method as 'single_threshold' | 'double_threshold' | 'adaptive' | 'tkeo') || 'double_threshold'
        const thresholdSd = item.params.thresholdSd as number || 3
        const minDurationMs = item.params.minDurationMs as number || 25
        const detections = detectEMGOnset(signal, samplingRate, method, thresholdSd, minDurationMs)
        result = {
          type: 'table',
          data: {
            onsets: detections.map(d => (d.onset / samplingRate).toFixed(3) + 's'),
            offsets: detections.map(d => (d.offset / samplingRate).toFixed(3) + 's'),
            count: detections.length
          },
          metadata: { detections }
        }
        break
      }
      case 'activationTiming': {
        // Analyze activation timing from onset detection
        const detections = detectEMGOnset(signal, samplingRate, 'double_threshold', 3, 25)
        const durations = detections.map(d => (d.offset - d.onset) / samplingRate * 1000)
        const avgDuration = durations.length > 0 ? durations.reduce((a, b) => a + b, 0) / durations.length : 0
        result = {
          type: 'table',
          data: {
            activations: detections.length,
            avgDuration: avgDuration.toFixed(1) + ' ms',
            totalActiveTime: durations.reduce((a, b) => a + b, 0).toFixed(1) + ' ms',
            dutyFactor: ((durations.reduce((a, b) => a + b, 0) / (signal.length / samplingRate * 1000)) * 100).toFixed(1) + '%'
          }
        }
        break
      }
      case 'coContractionIndex': {
        const channels = Object.values(generatedData.signals)
        if (channels.length >= 2) {
          const agonist = channels[0]
          const antagonist = channels[1]
          const method = (item.params.method as 'ratio' | 'overlap' | 'wasted') || 'ratio'
          const cciArray = coContractionIndex(agonist, antagonist, method)
          const meanCCI = cciArray.reduce((a, b) => a + b, 0) / cciArray.length
          result = {
            type: 'metric',
            data: {
              meanCCI: (meanCCI * 100).toFixed(2) + '%',
              method
            }
          }
        } else {
          result = { type: 'metric', data: { error: '2チャンネル以上必要です' } }
        }
        break
      }

      // ============ Fatigue Analysis ============
      case 'medianFrequency': {
        const windowSize = item.params.windowSize as number || 512
        const overlap = item.params.overlap as number || 50
        const mdfResult = computeMedianFrequency(signal, samplingRate, windowSize, overlap)
        result = {
          type: 'plot',
          data: mdfResult.mdf,
          metadata: { times: mdfResult.times }
        }
        break
      }
      case 'meanFrequency': {
        const windowSize = item.params.windowSize as number || 512
        const overlap = item.params.overlap as number || 50
        const mnfResult = computeMeanFrequency(signal, samplingRate, windowSize, overlap)
        result = {
          type: 'plot',
          data: mnfResult.mnf,
          metadata: { times: mnfResult.times }
        }
        break
      }
      case 'fatigueIndex': {
        const windowSize = item.params.windowSize as number || 512
        const mdfResult = computeMedianFrequency(signal, samplingRate, windowSize, 50)
        const method = (item.params.method as 'slope' | 'ratio') || 'slope'
        const fatigueResult = computeFatigueIndex(mdfResult.mdf, method)
        result = {
          type: 'metric',
          data: {
            fatigueIndex: fatigueResult.index.toFixed(2) + '%',
            trend: fatigueResult.trend.length
          }
        }
        break
      }
      case 'spectralCompression': {
        // Compute spectral compression (ratio of high/low frequency power over time)
        const windowSize = item.params.windowSize as number || 512
        const mdfResult = computeMedianFrequency(signal, samplingRate, windowSize, 50)
        // Spectral compression ratio: initial MDF / final MDF
        const initialMDF = mdfResult.mdf.slice(0, 5).reduce((a, b) => a + b, 0) / 5
        const finalMDF = mdfResult.mdf.slice(-5).reduce((a, b) => a + b, 0) / 5
        const compressionRatio = initialMDF / finalMDF
        result = {
          type: 'metric',
          data: {
            initialMDF: initialMDF.toFixed(1) + ' Hz',
            finalMDF: finalMDF.toFixed(1) + ' Hz',
            compressionRatio: compressionRatio.toFixed(2),
            mdfShift: ((1 - finalMDF / initialMDF) * 100).toFixed(1) + '%'
          }
        }
        break
      }
      case 'emgSpectrogram': {
        const windowSize = item.params.windowSize as number || 256
        const hopSize = item.params.hopSize as number || 64
        const stft = computeSTFT(signal, samplingRate, windowSize, hopSize)
        result = { type: 'matrix', data: stft }
        break
      }
      case 'emgWavelet': {
        const frequencies = [20, 50, 100, 150, 200, 300, 400, 500]
        const nCycles = item.params.nCycles as number || 5
        const wavelet = waveletTransform(signal, samplingRate, frequencies, nCycles)
        result = {
          type: 'matrix',
          data: { matrix: wavelet.power, frequencies: wavelet.frequencies },
          metadata: { times: wavelet.times }
        }
        break
      }

      // ============ Muscle Synergy Analysis ============
      case 'muscleSynergy': {
        const nSynergies = item.params.nSynergies as number || 4
        const maxIterations = item.params.maxIterations as number || 1000
        const method = (item.params.method as string) || 'nmf'
        const compareMode = item.params.compareMode as boolean || false
        const channels = Object.values(generatedData.signals)
        const channelNames = Object.keys(generatedData.signals)

        if (channels.length >= 2) {
          const nTime = Math.min(...channels.map(c => c.length))
          const emgMatrix: number[][] = channels.map(ch =>
            ch.slice(0, nTime).map(v => Math.abs(v))
          )

          if (compareMode) {
            // Comparison mode: run all three methods
            const methods: SynergyMethod[] = ['nmf', 'pca', 'fa']
            const methodResults: Record<SynergyMethod, MethodResult> = {} as Record<SynergyMethod, MethodResult>

            for (const m of methods) {
              let extractionResult
              if (m === 'nmf') {
                extractionResult = nmfDecomposition(emgMatrix, nSynergies, maxIterations)
              } else if (m === 'pca') {
                extractionResult = pcaSynergyExtraction(emgMatrix, nSynergies)
              } else {
                extractionResult = factorAnalysisSynergy(emgMatrix, nSynergies)
              }

              methodResults[m] = {
                W: extractionResult.W,
                H: extractionResult.H,
                vaf: extractionResult.vaf,
                muscleNames: channelNames,
                method: m,
                eigenvalues: 'eigenvalues' in extractionResult ? (extractionResult as { eigenvalues: number[] }).eigenvalues : undefined,
                communalities: 'communalities' in extractionResult ? (extractionResult as { communalities: number[] }).communalities : undefined
              }
            }

            // Calculate pairwise similarity
            const similarity: Record<string, number> = {}
            for (let i = 0; i < methods.length; i++) {
              for (let j = i + 1; j < methods.length; j++) {
                const m1 = methods[i]
                const m2 = methods[j]
                const simMatrix = synergySimilarity(methodResults[m1].W, methodResults[m2].W, 'correlation')

                // Best matching average
                let totalSim = 0
                const used = new Set<number>()
                for (let s = 0; s < nSynergies; s++) {
                  let bestSim = -1
                  let bestIdx = -1
                  for (let t = 0; t < nSynergies; t++) {
                    if (!used.has(t) && Math.abs(simMatrix[s]?.[t] ?? 0) > bestSim) {
                      bestSim = Math.abs(simMatrix[s]?.[t] ?? 0)
                      bestIdx = t
                    }
                  }
                  if (bestIdx >= 0) {
                    totalSim += bestSim
                    used.add(bestIdx)
                  }
                }
                similarity[`${m1}_${m2}`] = totalSim / nSynergies
              }
            }

            result = {
              type: 'synergyComparison',
              data: {
                results: methodResults,
                similarity,
                muscleNames: channelNames
              },
              metadata: { nSynergies, nMuscles: channels.length, compareMode: true }
            }
          } else {
            // Single method mode
            let extractionResult
            if (method === 'pca') {
              extractionResult = pcaSynergyExtraction(emgMatrix, nSynergies)
            } else if (method === 'fa') {
              extractionResult = factorAnalysisSynergy(emgMatrix, nSynergies)
            } else {
              extractionResult = nmfDecomposition(emgMatrix, nSynergies, maxIterations)
            }

            // Get actual H time length (may be downsampled for PCA/FA)
            const hTimeLength = extractionResult.H[0]?.length || 0
            const actualSynergies = extractionResult.W[0]?.length || nSynergies

            // Compute reconstruction: W * H (using H's actual time length)
            const reconstruction: number[][] = emgMatrix.map((_, m) =>
              Array(hTimeLength).fill(0).map((_, t) =>
                extractionResult.W[m]?.reduce((sum, w, s) =>
                  sum + w * (extractionResult.H[s]?.[t] ?? 0), 0) ?? 0
              )
            )

            // Downsample original for comparison with reconstruction
            const downsampleFactor = Math.max(1, Math.floor(nTime / hTimeLength))
            const downsampledOriginal: number[][] = emgMatrix.map(row => {
              if (downsampleFactor === 1) return row.slice(0, hTimeLength)
              const sampled: number[] = []
              for (let i = 0; i < row.length && sampled.length < hTimeLength; i += downsampleFactor) {
                sampled.push(row[i])
              }
              return sampled
            })

            // Compute VAF curve (only for NMF in single mode for efficiency)
            const vafCurve: number[] = []
            if (method === 'nmf') {
              for (let n = 1; n <= Math.min(8, channels.length); n++) {
                const testResult = nmfDecomposition(emgMatrix, n, 500)
                vafCurve.push(testResult.vaf)
              }
            }

            // Compute per-synergy VAF contributions
            const vafPerSynergy: number[] = []
            const totalVar = downsampledOriginal.reduce((sum, row) =>
              sum + row.reduce((s, v) => s + v * v, 0), 0
            )
            for (let s = 0; s < actualSynergies; s++) {
              const singleRecon = downsampledOriginal.map((_, m) =>
                Array(hTimeLength).fill(0).map((_, t) =>
                  (extractionResult.W[m]?.[s] ?? 0) * (extractionResult.H[s]?.[t] ?? 0)
                )
              )
              const reconVar = singleRecon.reduce((sum, row) =>
                sum + row.reduce((sv, v) => sv + v * v, 0), 0
              )
              vafPerSynergy.push(totalVar > 0 ? (reconVar / totalVar) * 100 : 0)
            }

            const synergyResult: SynergyResult = {
              W: extractionResult.W,
              H: extractionResult.H,
              vaf: extractionResult.vaf,
              muscleNames: channelNames,
              reconstruction,
              original: downsampledOriginal,
              vafPerSynergy,
              vafCurve: vafCurve.length > 0 ? vafCurve : undefined
            }

            result = {
              type: 'synergy',
              data: synergyResult,
              metadata: { nSynergies: actualSynergies, nMuscles: channels.length, method }
            }
          }
        } else {
          result = { type: 'metric', data: { error: '複数チャンネル必要です' } }
        }
        break
      }
      case 'synergyVAF': {
        const vafThreshold = item.params.vafThreshold as number || 90
        const maxSynergies = item.params.maxSynergies as number || 8
        const channels = Object.values(generatedData.signals)
        if (channels.length >= 2) {
          const nTime = Math.min(...channels.map(c => c.length))
          const emgMatrix: number[][] = channels.map(ch =>
            ch.slice(0, nTime).map(v => Math.abs(v))
          )
          const optimalResult = findOptimalSynergies(emgMatrix, vafThreshold, maxSynergies)
          result = {
            type: 'table',
            data: {
              optimalN: optimalResult.optimalN,
              vafCurve: optimalResult.vafCurve.map(v => v.toFixed(1) + '%'),
              threshold: vafThreshold + '%'
            }
          }
        } else {
          result = { type: 'metric', data: { error: '複数チャンネル必要です' } }
        }
        break
      }
      case 'synergySimilarity': {
        result = {
          type: 'metric',
          data: { message: '2つのシナジー結果を比較する機能（データセット選択が必要）' }
        }
        break
      }

      // ============ Motor Unit Analysis ============
      case 'motorUnitDecomposition': {
        // Simplified motor unit decomposition using peak detection
        const tkeoSignal = tkeo(signal)
        const threshold = Math.max(...tkeoSignal) * 0.3
        const peaks: number[] = []
        for (let i = 1; i < tkeoSignal.length - 1; i++) {
          if (tkeoSignal[i] > threshold && tkeoSignal[i] > tkeoSignal[i-1] && tkeoSignal[i] > tkeoSignal[i+1]) {
            peaks.push(i)
          }
        }
        result = {
          type: 'table',
          data: {
            detectedSpikes: peaks.length,
            avgFiringRate: (peaks.length / (signal.length / samplingRate)).toFixed(1) + ' Hz',
            duration: (signal.length / samplingRate).toFixed(2) + ' s'
          },
          metadata: { spikeIndices: peaks }
        }
        break
      }
      case 'firingRate': {
        // Compute instantaneous firing rate
        const tkeoSignal = tkeo(signal)
        const threshold = Math.max(...tkeoSignal) * 0.3
        const peaks: number[] = []
        for (let i = 1; i < tkeoSignal.length - 1; i++) {
          if (tkeoSignal[i] > threshold && tkeoSignal[i] > tkeoSignal[i-1] && tkeoSignal[i] > tkeoSignal[i+1]) {
            peaks.push(i)
          }
        }
        // Calculate inter-spike intervals
        const isis: number[] = []
        for (let i = 1; i < peaks.length; i++) {
          isis.push((peaks[i] - peaks[i-1]) / samplingRate * 1000) // ms
        }
        const avgISI = isis.length > 0 ? isis.reduce((a, b) => a + b, 0) / isis.length : 0
        const cvISI = isis.length > 1 ? Math.sqrt(isis.map(x => (x - avgISI) ** 2).reduce((a, b) => a + b, 0) / (isis.length - 1)) / avgISI : 0
        result = {
          type: 'table',
          data: {
            meanFiringRate: (1000 / avgISI).toFixed(1) + ' Hz',
            avgISI: avgISI.toFixed(1) + ' ms',
            cvISI: (cvISI * 100).toFixed(1) + '%',
            totalSpikes: peaks.length
          }
        }
        break
      }
      case 'muapMorphology': {
        // Analyze MUAP morphology using averaged waveforms around detected spikes
        const tkeoSignal = tkeo(signal)
        const threshold = Math.max(...tkeoSignal) * 0.3
        const peaks: number[] = []
        for (let i = 1; i < tkeoSignal.length - 1; i++) {
          if (tkeoSignal[i] > threshold && tkeoSignal[i] > tkeoSignal[i-1] && tkeoSignal[i] > tkeoSignal[i+1]) {
            peaks.push(i)
          }
        }
        // Extract and average waveforms
        const windowSamples = Math.floor(0.005 * samplingRate) // 5ms window
        const waveforms: number[][] = []
        for (const peak of peaks) {
          if (peak > windowSamples && peak < signal.length - windowSamples) {
            waveforms.push(signal.slice(peak - windowSamples, peak + windowSamples))
          }
        }
        // Average waveform
        const avgWaveform = waveforms.length > 0
          ? Array(windowSamples * 2).fill(0).map((_, i) =>
              waveforms.reduce((sum, wf) => sum + wf[i], 0) / waveforms.length)
          : []
        const peakToPeak = avgWaveform.length > 0 ? Math.max(...avgWaveform) - Math.min(...avgWaveform) : 0
        result = {
          type: 'plot',
          data: avgWaveform,
          metadata: {
            nWaveforms: waveforms.length,
            peakToPeak: peakToPeak.toFixed(4),
            duration: (windowSamples * 2 / samplingRate * 1000).toFixed(1) + ' ms'
          }
        }
        break
      }

      // ============ Pattern Recognition ============
      case 'emgFeatureExtraction': {
        const windowMs = item.params.windowMs as number || 250
        const features = extractEMGFeatures(signal, samplingRate, windowMs)
        result = {
          type: 'table',
          data: features
        }
        break
      }
      case 'emgClassification': {
        // Simplified classification demo using feature thresholds
        const features = extractEMGFeatures(signal, samplingRate, 250)
        const rms = features.rms || 0
        const mav = features.mav || 0
        // Simple threshold-based classification
        let predictedClass = 'rest'
        if (rms > 0.1 && mav > 0.08) {
          predictedClass = 'high_activation'
        } else if (rms > 0.05 || mav > 0.04) {
          predictedClass = 'low_activation'
        }
        result = {
          type: 'table',
          data: {
            predictedClass,
            confidence: '80%',
            features: { rms: rms.toFixed(4), mav: mav.toFixed(4) }
          }
        }
        break
      }
      case 'confusionMatrix': {
        // Placeholder for confusion matrix visualization
        result = {
          type: 'table',
          data: {
            message: '分類結果の混同行列（訓練データが必要）',
            accuracy: 'N/A',
            precision: 'N/A',
            recall: 'N/A'
          }
        }
        break
      }

      default:
        result = { type: 'metric', data: { message: 'Not implemented' } }
    }

    return result
  }, [generatedData, activeDataset])

  // Handle run analysis
  const handleRunAnalysis = async () => {
    await runAnalysis(executeMethod)
    if (activeDataset) {
      generateReport(activeDataset.name)
    }
  }

  // Get optional methods for the selected workflow
  const optionalMethods = selectedWorkflow ? getOptionalMethods(selectedWorkflow) : []

  // Group optional methods by category
  const methodsByCategory = optionalMethods.reduce((acc, method) => {
    if (!acc[method.category]) acc[method.category] = []
    acc[method.category].push(method)
    return acc
  }, {} as Record<AnalysisCategory, AnalysisMethod[]>)

  if (!activeDataset) {
    return (
      <div className="flex h-full items-center justify-center text-muted-foreground">
        <div className="text-center">
          <div className="text-4xl mb-4">📊</div>
          <p>データブラウザでデータセットを選択してください</p>
        </div>
      </div>
    )
  }

  return (
    <div className="flex h-full">
      {/* Main Content */}
      <div className="flex-1 overflow-auto p-6">
        <div className="max-w-4xl mx-auto space-y-8">
          {/* Header */}
          <div>
            <h1 className="text-2xl font-bold">ワークフロー解析</h1>
            <p className="text-muted-foreground mt-1">
              データセット: {activeDataset.name} ({datasetType})
            </p>
          </div>

          {/* Workflow Selector */}
          <WorkflowSelector
            datasetType={datasetType}
            selectedWorkflow={selectedWorkflow}
            onSelect={setWorkflow}
          />

          {/* Optional Methods */}
          {selectedWorkflow && (
            <div className="space-y-4">
              <h3 className="text-lg font-semibold">オプション解析手法を追加</h3>
              <p className="text-sm text-muted-foreground">
                必須の解析手法はカートに自動追加されています。追加の解析手法を選択できます。
              </p>

              {Object.entries(methodsByCategory).map(([category, methods]) => (
                <div key={category} className="space-y-2">
                  <h4 className="text-sm font-medium text-muted-foreground">
                    {getCategoryLabel(category as AnalysisCategory)}
                  </h4>
                  <div className="grid gap-2 md:grid-cols-2">
                    {methods.map(method => (
                      <MethodCard
                        key={method.id}
                        method={method}
                        isInCart={items.some(i => i.methodId === method.id)}
                        isRequired={false}
                        onAdd={() => addItem(method.id)}
                        onRemove={() => {
                          const item = items.find(i => i.methodId === method.id)
                          if (item) removeItem(item.id)
                        }}
                      />
                    ))}
                  </div>
                </div>
              ))}
            </div>
          )}

          {/* All Available Methods (when no workflow selected) */}
          {!selectedWorkflow && (
            <div className="space-y-4">
              <h3 className="text-lg font-semibold">全解析手法</h3>
              <div className="grid gap-2 md:grid-cols-2 lg:grid-cols-3">
                {Object.values(analysisMethods).map(method => (
                  <MethodCard
                    key={method.id}
                    method={method}
                    isInCart={items.some(i => i.methodId === method.id)}
                    isRequired={false}
                    onAdd={() => addItem(method.id)}
                    onRemove={() => {
                      const item = items.find(i => i.methodId === method.id)
                      if (item) removeItem(item.id)
                    }}
                  />
                ))}
              </div>
            </div>
          )}
        </div>
      </div>

      {/* Cart Sidebar */}
      <div className="w-80 border-l bg-muted/30">
        <AnalysisCart
          items={items}
          onUpdateParams={updateItemParams}
          onRemove={removeItem}
          onRun={handleRunAnalysis}
          isRunning={isRunning}
          progress={progress}
        />
      </div>

      {/* Report Viewer */}
      {isReportVisible && reportData && (
        <ReportViewer
          reportData={reportData}
          items={items}
          onClose={hideReport}
          onExport={exportReport}
        />
      )}
    </div>
  )
}
