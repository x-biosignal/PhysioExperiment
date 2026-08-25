/**
 * SynergyReport - Publication-quality muscle synergy analysis visualization
 *
 * Displays NMF decomposition results in a format suitable for academic publications:
 * - Synergy weight vectors (W matrix) as grouped bar charts
 * - Temporal activation patterns (H matrix) as time series
 * - VAF curve with optimal synergy selection
 * - Reconstruction quality metrics
 * - Summary statistics table
 */

import { useRef, useEffect, useState, useCallback } from 'react'

// ============ Types ============
export interface SynergyResult {
  W: number[][]           // Synergy weights: n_muscles x n_synergies
  H: number[][]           // Activation patterns: n_synergies x n_timepoints
  vaf: number             // Total VAF
  muscleNames: string[]   // Names of muscles
  reconstruction?: number[][] // Reconstructed EMG
  original?: number[][]   // Original EMG data
  vafPerSynergy?: number[] // VAF contribution per synergy
  vafCurve?: number[]     // VAF for 1, 2, ..., n synergies
}

interface SynergyReportProps {
  result: SynergyResult
  title?: string
  showReconstruction?: boolean
  normalizeTime?: boolean  // Normalize to 0-100% cycle
  onExport?: (format: 'png' | 'svg' | 'pdf') => void
}

// ============ Color Palettes ============
const SYNERGY_COLORS = [
  '#E63946', // Red
  '#457B9D', // Blue
  '#2A9D8F', // Teal
  '#E9C46A', // Yellow
  '#F4A261', // Orange
  '#9B5DE5', // Purple
  '#00BBF9', // Cyan
  '#00F5D4', // Mint
]

// ============ Utility Functions ============
function normalizeToRange(arr: number[], min = 0, max = 1): number[] {
  const arrMin = Math.min(...arr)
  const arrMax = Math.max(...arr)
  const range = arrMax - arrMin || 1
  return arr.map(v => min + (v - arrMin) / range * (max - min))
}

function computeR2(original: number[], reconstructed: number[]): number {
  const mean = original.reduce((a, b) => a + b, 0) / original.length
  const ssTot = original.reduce((sum, v) => sum + (v - mean) ** 2, 0)
  const ssRes = original.reduce((sum, v, i) => sum + (v - reconstructed[i]) ** 2, 0)
  return 1 - ssRes / ssTot
}

// ============ Muscle Colors (for when grouping by synergy) ============
const MUSCLE_COLORS = [
  '#264653', '#2a9d8f', '#e9c46a', '#f4a261', '#e76f51',
  '#606c38', '#283618', '#dda15e', '#bc6c25', '#003049',
  '#d62828', '#f77f00', '#fcbf49', '#eae2b7', '#8338ec'
]

type GroupBy = 'muscle' | 'synergy'

// ============ Synergy Weights Chart ============
function SynergyWeightsChart({
  W,
  muscleNames,
  width = 600,
  height = 300
}: {
  W: number[][]
  muscleNames: string[]
  width?: number
  height?: number
}) {
  const canvasRef = useRef<HTMLCanvasElement>(null)
  const [groupBy, setGroupBy] = useState<GroupBy>('muscle')

  useEffect(() => {
    const canvas = canvasRef.current
    if (!canvas) return
    const ctx = canvas.getContext('2d')
    if (!ctx) return

    const dpr = window.devicePixelRatio || 1
    canvas.width = width * dpr
    canvas.height = height * dpr
    canvas.style.width = `${width}px`
    canvas.style.height = `${height}px`
    ctx.scale(dpr, dpr)

    // Clear
    ctx.fillStyle = '#ffffff'
    ctx.fillRect(0, 0, width, height)

    const nMuscles = W.length
    const nSynergies = W[0]?.length || 0
    if (nMuscles === 0 || nSynergies === 0) return

    const margin = { top: 40, right: 120, bottom: 60, left: 60 }
    const plotWidth = width - margin.left - margin.right
    const plotHeight = height - margin.top - margin.bottom

    // Normalize weights per synergy (max = 1)
    const normalizedW: number[][] = []
    for (let m = 0; m < nMuscles; m++) {
      normalizedW.push([])
      for (let s = 0; s < nSynergies; s++) {
        const maxInSynergy = Math.max(...W.map(row => row[s]))
        normalizedW[m].push(W[m][s] / (maxInSynergy || 1))
      }
    }

    // Y-axis grid and labels
    ctx.strokeStyle = '#e5e7eb'
    ctx.lineWidth = 1
    for (let i = 0; i <= 5; i++) {
      const y = margin.top + plotHeight * (1 - i / 5)
      ctx.beginPath()
      ctx.moveTo(margin.left, y)
      ctx.lineTo(margin.left + plotWidth, y)
      ctx.stroke()

      ctx.fillStyle = '#6b7280'
      ctx.font = '11px Inter, sans-serif'
      ctx.textAlign = 'right'
      ctx.fillText((i * 0.2).toFixed(1), margin.left - 8, y + 4)
    }

    // Y-axis label
    ctx.save()
    ctx.translate(15, margin.top + plotHeight / 2)
    ctx.rotate(-Math.PI / 2)
    ctx.fillStyle = '#374151'
    ctx.font = '12px Inter, sans-serif'
    ctx.textAlign = 'center'
    ctx.fillText('Normalized Weight', 0, 0)
    ctx.restore()

    if (groupBy === 'muscle') {
      // Group by muscle (original layout)
      const groupWidth = plotWidth / nMuscles
      const barWidth = (groupWidth * 0.8) / nSynergies
      const barGap = groupWidth * 0.2 / (nSynergies + 1)

      // Draw bars
      for (let m = 0; m < nMuscles; m++) {
        const groupX = margin.left + m * groupWidth + groupWidth * 0.1

        for (let s = 0; s < nSynergies; s++) {
          const x = groupX + s * (barWidth + barGap)
          const barHeight = normalizedW[m][s] * plotHeight
          const y = margin.top + plotHeight - barHeight

          ctx.fillStyle = SYNERGY_COLORS[s % SYNERGY_COLORS.length]
          ctx.fillRect(x, y, barWidth, barHeight)

          ctx.strokeStyle = 'rgba(0,0,0,0.1)'
          ctx.strokeRect(x, y, barWidth, barHeight)
        }

        // Muscle label
        ctx.fillStyle = '#374151'
        ctx.font = '10px Inter, sans-serif'
        ctx.textAlign = 'center'
        ctx.save()
        ctx.translate(margin.left + (m + 0.5) * groupWidth, margin.top + plotHeight + 10)
        ctx.rotate(-Math.PI / 4)
        const label = muscleNames[m] || `M${m + 1}`
        ctx.fillText(label.length > 10 ? label.slice(0, 10) + '...' : label, 0, 0)
        ctx.restore()
      }

      // Legend (Synergies)
      const legendX = margin.left + plotWidth + 20
      let legendY = margin.top + 10
      ctx.font = 'bold 11px Inter, sans-serif'
      ctx.fillStyle = '#374151'
      ctx.textAlign = 'left'
      ctx.fillText('Synergies', legendX, legendY)
      legendY += 20

      for (let s = 0; s < nSynergies; s++) {
        ctx.fillStyle = SYNERGY_COLORS[s % SYNERGY_COLORS.length]
        ctx.fillRect(legendX, legendY - 10, 14, 14)
        ctx.fillStyle = '#374151'
        ctx.font = '11px Inter, sans-serif'
        ctx.fillText(`Syn ${s + 1}`, legendX + 20, legendY)
        legendY += 18
      }
    } else {
      // Group by synergy
      const groupWidth = plotWidth / nSynergies
      const barWidth = (groupWidth * 0.8) / nMuscles
      const barGap = groupWidth * 0.2 / (nMuscles + 1)

      // Draw bars
      for (let s = 0; s < nSynergies; s++) {
        const groupX = margin.left + s * groupWidth + groupWidth * 0.1

        for (let m = 0; m < nMuscles; m++) {
          const x = groupX + m * (barWidth + barGap)
          const barHeight = normalizedW[m][s] * plotHeight
          const y = margin.top + plotHeight - barHeight

          ctx.fillStyle = MUSCLE_COLORS[m % MUSCLE_COLORS.length]
          ctx.fillRect(x, y, barWidth, barHeight)

          ctx.strokeStyle = 'rgba(0,0,0,0.1)'
          ctx.strokeRect(x, y, barWidth, barHeight)
        }

        // Synergy label
        ctx.fillStyle = '#374151'
        ctx.font = '11px Inter, sans-serif'
        ctx.textAlign = 'center'
        ctx.fillText(`Syn ${s + 1}`, margin.left + (s + 0.5) * groupWidth, margin.top + plotHeight + 20)
      }

      // Legend (Muscles)
      const legendX = margin.left + plotWidth + 10
      let legendY = margin.top + 5
      ctx.font = 'bold 10px Inter, sans-serif'
      ctx.fillStyle = '#374151'
      ctx.textAlign = 'left'
      ctx.fillText('Muscles', legendX, legendY)
      legendY += 14

      const maxLegendItems = Math.min(nMuscles, 12)
      for (let m = 0; m < maxLegendItems; m++) {
        ctx.fillStyle = MUSCLE_COLORS[m % MUSCLE_COLORS.length]
        ctx.fillRect(legendX, legendY - 8, 10, 10)
        ctx.fillStyle = '#374151'
        ctx.font = '9px Inter, sans-serif'
        const label = muscleNames[m] || `M${m + 1}`
        ctx.fillText(label.length > 8 ? label.slice(0, 8) + '..' : label, legendX + 14, legendY)
        legendY += 13
      }
      if (nMuscles > maxLegendItems) {
        ctx.fillStyle = '#6b7280'
        ctx.fillText(`+${nMuscles - maxLegendItems} more`, legendX, legendY)
      }
    }

    // Title
    ctx.fillStyle = '#1f2937'
    ctx.font = 'bold 14px Inter, sans-serif'
    ctx.textAlign = 'center'
    ctx.fillText('A. Synergy Weight Vectors (W)', width / 2, 20)

  }, [W, muscleNames, width, height, groupBy])

  return (
    <div className="relative">
      {/* Dropdown for grouping selection */}
      <div className="absolute top-0 right-0 z-10">
        <select
          value={groupBy}
          onChange={(e) => setGroupBy(e.target.value as GroupBy)}
          className="text-xs px-2 py-1 border rounded bg-white shadow-sm focus:outline-none focus:ring-1 focus:ring-blue-500"
        >
          <option value="muscle">Group by Muscle</option>
          <option value="synergy">Group by Synergy</option>
        </select>
      </div>
      <canvas ref={canvasRef} />
    </div>
  )
}

// ============ Activation Patterns Chart (Faceted) ============
function ActivationPatternsChart({
  H,
  normalizeTime = true,
  width = 600,
  height = 400
}: {
  H: number[][]
  normalizeTime?: boolean
  width?: number
  height?: number
}) {
  const canvasRef = useRef<HTMLCanvasElement>(null)

  useEffect(() => {
    const canvas = canvasRef.current
    if (!canvas) return
    const ctx = canvas.getContext('2d')
    if (!ctx) return

    const nSynergies = H.length
    const nTimepoints = H[0]?.length || 0
    if (nSynergies === 0 || nTimepoints === 0) return

    // Calculate dynamic height based on number of synergies
    const facetHeight = 80
    const totalHeight = Math.max(height, 60 + nSynergies * facetHeight + 50)

    const dpr = window.devicePixelRatio || 1
    canvas.width = width * dpr
    canvas.height = totalHeight * dpr
    canvas.style.width = `${width}px`
    canvas.style.height = `${totalHeight}px`
    ctx.scale(dpr, dpr)

    ctx.fillStyle = '#ffffff'
    ctx.fillRect(0, 0, width, totalHeight)

    const margin = { top: 40, right: 30, bottom: 50, left: 70 }
    const plotWidth = width - margin.left - margin.right
    const facetGap = 10

    // Normalize each activation pattern
    const normalizedH = H.map(row => normalizeToRange(row, 0, 1))

    // Title
    ctx.fillStyle = '#1f2937'
    ctx.font = 'bold 14px Inter, sans-serif'
    ctx.textAlign = 'center'
    ctx.fillText('B. Synergy Activation Patterns (H)', width / 2, 20)

    // Draw each synergy in its own facet
    for (let s = 0; s < nSynergies; s++) {
      const facetTop = margin.top + s * (facetHeight + facetGap)
      const facetPlotHeight = facetHeight - 15

      // Facet background
      ctx.fillStyle = '#fafafa'
      ctx.fillRect(margin.left - 5, facetTop, plotWidth + 10, facetHeight)

      // Facet border
      ctx.strokeStyle = '#e5e7eb'
      ctx.lineWidth = 1
      ctx.strokeRect(margin.left - 5, facetTop, plotWidth + 10, facetHeight)

      // Synergy label on the left
      ctx.fillStyle = SYNERGY_COLORS[s % SYNERGY_COLORS.length]
      ctx.font = 'bold 11px Inter, sans-serif'
      ctx.textAlign = 'right'
      ctx.fillText(`Syn ${s + 1}`, margin.left - 12, facetTop + facetPlotHeight / 2 + 4)

      // Y-axis ticks (0 and 1)
      ctx.fillStyle = '#9ca3af'
      ctx.font = '9px Inter, sans-serif'
      ctx.textAlign = 'right'
      ctx.fillText('1', margin.left - 2, facetTop + 8)
      ctx.fillText('0', margin.left - 2, facetTop + facetPlotHeight)

      // Draw horizontal grid lines
      ctx.strokeStyle = '#e5e7eb'
      ctx.lineWidth = 0.5
      ctx.beginPath()
      ctx.moveTo(margin.left, facetTop + facetPlotHeight / 2)
      ctx.lineTo(margin.left + plotWidth, facetTop + facetPlotHeight / 2)
      ctx.stroke()

      // Fill area under curve
      ctx.fillStyle = SYNERGY_COLORS[s % SYNERGY_COLORS.length] + '20'
      ctx.beginPath()
      ctx.moveTo(margin.left, facetTop + facetPlotHeight)
      for (let t = 0; t < nTimepoints; t++) {
        const x = margin.left + (t / (nTimepoints - 1)) * plotWidth
        const y = facetTop + facetPlotHeight * (1 - normalizedH[s][t])
        ctx.lineTo(x, y)
      }
      ctx.lineTo(margin.left + plotWidth, facetTop + facetPlotHeight)
      ctx.closePath()
      ctx.fill()

      // Draw activation line
      ctx.strokeStyle = SYNERGY_COLORS[s % SYNERGY_COLORS.length]
      ctx.lineWidth = 2
      ctx.beginPath()
      for (let t = 0; t < nTimepoints; t++) {
        const x = margin.left + (t / (nTimepoints - 1)) * plotWidth
        const y = facetTop + facetPlotHeight * (1 - normalizedH[s][t])
        if (t === 0) ctx.moveTo(x, y)
        else ctx.lineTo(x, y)
      }
      ctx.stroke()
    }

    // X-axis labels (only at bottom)
    const xAxisY = margin.top + nSynergies * (facetHeight + facetGap)
    const xLabels = normalizeTime ? ['0%', '25%', '50%', '75%', '100%'] : ['0', '', '', '', '1']
    for (let i = 0; i < xLabels.length; i++) {
      const x = margin.left + (i / (xLabels.length - 1)) * plotWidth
      ctx.fillStyle = '#6b7280'
      ctx.font = '11px Inter, sans-serif'
      ctx.textAlign = 'center'
      ctx.fillText(xLabels[i], x, xAxisY + 15)
    }

    // X-axis label
    ctx.fillStyle = '#374151'
    ctx.font = '12px Inter, sans-serif'
    ctx.textAlign = 'center'
    ctx.fillText(normalizeTime ? 'Movement Cycle (%)' : 'Time', margin.left + plotWidth / 2, xAxisY + 35)

    // Y-axis label (rotated, centered on all facets)
    ctx.save()
    ctx.translate(12, margin.top + (nSynergies * (facetHeight + facetGap)) / 2)
    ctx.rotate(-Math.PI / 2)
    ctx.fillStyle = '#374151'
    ctx.font = '12px Inter, sans-serif'
    ctx.textAlign = 'center'
    ctx.fillText('Activation (normalized)', 0, 0)
    ctx.restore()

  }, [H, normalizeTime, width, height])

  return <canvas ref={canvasRef} />
}

// ============ VAF Curve Chart ============
function VAFCurveChart({
  vafCurve,
  optimalN,
  threshold = 90,
  width = 400,
  height = 250
}: {
  vafCurve: number[]
  optimalN: number
  threshold?: number
  width?: number
  height?: number
}) {
  const canvasRef = useRef<HTMLCanvasElement>(null)

  useEffect(() => {
    const canvas = canvasRef.current
    if (!canvas) return
    const ctx = canvas.getContext('2d')
    if (!ctx) return

    const dpr = window.devicePixelRatio || 1
    canvas.width = width * dpr
    canvas.height = height * dpr
    canvas.style.width = `${width}px`
    canvas.style.height = `${height}px`
    ctx.scale(dpr, dpr)

    ctx.fillStyle = '#ffffff'
    ctx.fillRect(0, 0, width, height)

    const margin = { top: 40, right: 30, bottom: 50, left: 60 }
    const plotWidth = width - margin.left - margin.right
    const plotHeight = height - margin.top - margin.bottom

    // Y-axis grid
    ctx.strokeStyle = '#e5e7eb'
    ctx.lineWidth = 1
    for (let i = 0; i <= 5; i++) {
      const y = margin.top + plotHeight * (1 - i / 5)
      const val = i * 20
      ctx.beginPath()
      ctx.moveTo(margin.left, y)
      ctx.lineTo(margin.left + plotWidth, y)
      ctx.stroke()

      ctx.fillStyle = '#6b7280'
      ctx.font = '11px Inter, sans-serif'
      ctx.textAlign = 'right'
      ctx.fillText(`${val}%`, margin.left - 8, y + 4)
    }

    // Threshold line
    const thresholdY = margin.top + plotHeight * (1 - threshold / 100)
    ctx.strokeStyle = '#ef4444'
    ctx.lineWidth = 1
    ctx.setLineDash([5, 5])
    ctx.beginPath()
    ctx.moveTo(margin.left, thresholdY)
    ctx.lineTo(margin.left + plotWidth, thresholdY)
    ctx.stroke()
    ctx.setLineDash([])

    ctx.fillStyle = '#ef4444'
    ctx.font = '10px Inter, sans-serif'
    ctx.textAlign = 'left'
    ctx.fillText(`${threshold}% threshold`, margin.left + plotWidth - 80, thresholdY - 5)

    // VAF curve
    ctx.strokeStyle = '#3b82f6'
    ctx.lineWidth = 2
    ctx.beginPath()
    for (let i = 0; i < vafCurve.length; i++) {
      const x = margin.left + ((i + 1) / vafCurve.length) * plotWidth
      const y = margin.top + plotHeight * (1 - vafCurve[i] / 100)
      if (i === 0) ctx.moveTo(x, y)
      else ctx.lineTo(x, y)
    }
    ctx.stroke()

    // Data points
    for (let i = 0; i < vafCurve.length; i++) {
      const x = margin.left + ((i + 1) / vafCurve.length) * plotWidth
      const y = margin.top + plotHeight * (1 - vafCurve[i] / 100)

      ctx.beginPath()
      ctx.arc(x, y, i + 1 === optimalN ? 6 : 4, 0, Math.PI * 2)
      ctx.fillStyle = i + 1 === optimalN ? '#22c55e' : '#3b82f6'
      ctx.fill()
      ctx.strokeStyle = '#ffffff'
      ctx.lineWidth = 2
      ctx.stroke()
    }

    // X-axis labels
    for (let i = 0; i < vafCurve.length; i++) {
      const x = margin.left + ((i + 1) / vafCurve.length) * plotWidth
      ctx.fillStyle = i + 1 === optimalN ? '#22c55e' : '#6b7280'
      ctx.font = i + 1 === optimalN ? 'bold 11px Inter, sans-serif' : '11px Inter, sans-serif'
      ctx.textAlign = 'center'
      ctx.fillText(`${i + 1}`, x, margin.top + plotHeight + 20)
    }

    // Axis labels
    ctx.fillStyle = '#374151'
    ctx.font = '12px Inter, sans-serif'
    ctx.textAlign = 'center'
    ctx.fillText('Number of Synergies', margin.left + plotWidth / 2, margin.top + plotHeight + 40)

    ctx.save()
    ctx.translate(15, margin.top + plotHeight / 2)
    ctx.rotate(-Math.PI / 2)
    ctx.fillText('VAF (%)', 0, 0)
    ctx.restore()

    // Optimal indicator
    const optimalX = margin.left + (optimalN / vafCurve.length) * plotWidth
    ctx.fillStyle = '#22c55e'
    ctx.font = '10px Inter, sans-serif'
    ctx.textAlign = 'center'
    ctx.fillText(`Optimal: ${optimalN}`, optimalX, margin.top - 5)

    // Title
    ctx.fillStyle = '#1f2937'
    ctx.font = 'bold 14px Inter, sans-serif'
    ctx.textAlign = 'center'
    ctx.fillText('C. VAF vs Number of Synergies', width / 2, 20)

  }, [vafCurve, optimalN, threshold, width, height])

  return <canvas ref={canvasRef} />
}

// ============ Reconstruction Quality Chart ============
function ReconstructionQualityChart({
  original,
  reconstruction,
  muscleNames,
  width = 400,
  height = 250
}: {
  original: number[][]
  reconstruction: number[][]
  muscleNames: string[]
  width?: number
  height?: number
}) {
  const canvasRef = useRef<HTMLCanvasElement>(null)
  const [r2Values, setR2Values] = useState<number[]>([])

  useEffect(() => {
    // Calculate R² for each muscle
    const r2s = original.map((orig, i) => {
      const recon = reconstruction[i] || []
      if (orig.length === 0 || recon.length === 0) return 0
      return computeR2(orig, recon)
    })
    setR2Values(r2s)
  }, [original, reconstruction])

  useEffect(() => {
    const canvas = canvasRef.current
    if (!canvas || r2Values.length === 0) return
    const ctx = canvas.getContext('2d')
    if (!ctx) return

    const dpr = window.devicePixelRatio || 1
    canvas.width = width * dpr
    canvas.height = height * dpr
    canvas.style.width = `${width}px`
    canvas.style.height = `${height}px`
    ctx.scale(dpr, dpr)

    ctx.fillStyle = '#ffffff'
    ctx.fillRect(0, 0, width, height)

    const margin = { top: 40, right: 30, bottom: 60, left: 60 }
    const plotWidth = width - margin.left - margin.right
    const plotHeight = height - margin.top - margin.bottom

    const nMuscles = r2Values.length
    const barWidth = (plotWidth / nMuscles) * 0.7
    const barGap = (plotWidth / nMuscles) * 0.3

    // Y-axis grid
    ctx.strokeStyle = '#e5e7eb'
    ctx.lineWidth = 1
    for (let i = 0; i <= 5; i++) {
      const y = margin.top + plotHeight * (1 - i / 5)
      ctx.beginPath()
      ctx.moveTo(margin.left, y)
      ctx.lineTo(margin.left + plotWidth, y)
      ctx.stroke()

      ctx.fillStyle = '#6b7280'
      ctx.font = '11px Inter, sans-serif'
      ctx.textAlign = 'right'
      ctx.fillText((i * 0.2).toFixed(1), margin.left - 8, y + 4)
    }

    // Draw bars
    for (let m = 0; m < nMuscles; m++) {
      const x = margin.left + m * (barWidth + barGap) + barGap / 2
      const barHeight = Math.max(0, r2Values[m]) * plotHeight
      const y = margin.top + plotHeight - barHeight

      // Color based on R²
      const r2 = r2Values[m]
      let color = '#ef4444' // Red for poor
      if (r2 >= 0.9) color = '#22c55e' // Green for excellent
      else if (r2 >= 0.8) color = '#84cc16' // Light green for good
      else if (r2 >= 0.7) color = '#eab308' // Yellow for acceptable
      else if (r2 >= 0.5) color = '#f97316' // Orange for fair

      ctx.fillStyle = color
      ctx.fillRect(x, y, barWidth, barHeight)

      // R² value on top
      ctx.fillStyle = '#374151'
      ctx.font = '9px Inter, sans-serif'
      ctx.textAlign = 'center'
      ctx.fillText(r2.toFixed(2), x + barWidth / 2, y - 5)

      // Muscle label
      ctx.save()
      ctx.translate(x + barWidth / 2, margin.top + plotHeight + 10)
      ctx.rotate(-Math.PI / 4)
      ctx.fillStyle = '#374151'
      ctx.font = '10px Inter, sans-serif'
      ctx.textAlign = 'right'
      const label = muscleNames[m] || `M${m + 1}`
      ctx.fillText(label.length > 8 ? label.slice(0, 8) + '..' : label, 0, 0)
      ctx.restore()
    }

    // Y-axis label
    ctx.save()
    ctx.translate(15, margin.top + plotHeight / 2)
    ctx.rotate(-Math.PI / 2)
    ctx.fillStyle = '#374151'
    ctx.font = '12px Inter, sans-serif'
    ctx.textAlign = 'center'
    ctx.fillText('R\u00B2', 0, 0)
    ctx.restore()

    // Title
    ctx.fillStyle = '#1f2937'
    ctx.font = 'bold 14px Inter, sans-serif'
    ctx.textAlign = 'center'
    ctx.fillText('D. Reconstruction Quality (R\u00B2)', width / 2, 20)

  }, [r2Values, muscleNames, width, height])

  return <canvas ref={canvasRef} />
}

// ============ Summary Statistics Table ============
function SummaryStatisticsTable({
  result,
  r2Values
}: {
  result: SynergyResult
  r2Values: number[]
}) {
  const nSynergies = result.W[0]?.length || 0
  const nMuscles = result.muscleNames.length
  const avgR2 = r2Values.length > 0 ? r2Values.reduce((a, b) => a + b, 0) / r2Values.length : 0

  // Calculate dominant muscle for each synergy
  const dominantMuscles = Array(nSynergies).fill(0).map((_, s) => {
    let maxWeight = 0
    let maxMuscle = ''
    for (let m = 0; m < nMuscles; m++) {
      if (result.W[m][s] > maxWeight) {
        maxWeight = result.W[m][s]
        maxMuscle = result.muscleNames[m]
      }
    }
    return { muscle: maxMuscle, weight: maxWeight }
  })

  return (
    <div className="rounded-lg border bg-white overflow-hidden">
      <div className="bg-gray-50 px-4 py-3 border-b">
        <h3 className="font-semibold text-gray-900">E. Summary Statistics</h3>
      </div>
      <div className="p-4">
        <table className="w-full text-sm">
          <tbody>
            <tr className="border-b">
              <td className="py-2 text-gray-600">Number of Synergies</td>
              <td className="py-2 font-medium text-right">{nSynergies}</td>
            </tr>
            <tr className="border-b">
              <td className="py-2 text-gray-600">Number of Muscles</td>
              <td className="py-2 font-medium text-right">{nMuscles}</td>
            </tr>
            <tr className="border-b">
              <td className="py-2 text-gray-600">Total VAF</td>
              <td className="py-2 font-medium text-right">{result.vaf.toFixed(1)}%</td>
            </tr>
            <tr className="border-b">
              <td className="py-2 text-gray-600">Mean R\u00B2</td>
              <td className="py-2 font-medium text-right">{avgR2.toFixed(3)}</td>
            </tr>
          </tbody>
        </table>

        <div className="mt-4">
          <h4 className="text-sm font-medium text-gray-700 mb-2">Per-Synergy Details</h4>
          <table className="w-full text-sm">
            <thead>
              <tr className="border-b">
                <th className="py-2 text-left text-gray-600">Synergy</th>
                <th className="py-2 text-left text-gray-600">Dominant Muscle</th>
                <th className="py-2 text-right text-gray-600">VAF Contrib.</th>
              </tr>
            </thead>
            <tbody>
              {dominantMuscles.map((dm, i) => (
                <tr key={i} className="border-b last:border-0">
                  <td className="py-2">
                    <span
                      className="inline-block w-3 h-3 rounded-full mr-2"
                      style={{ backgroundColor: SYNERGY_COLORS[i % SYNERGY_COLORS.length] }}
                    />
                    Syn {i + 1}
                  </td>
                  <td className="py-2">{dm.muscle}</td>
                  <td className="py-2 text-right">
                    {result.vafPerSynergy ? `${result.vafPerSynergy[i]?.toFixed(1)}%` : '-'}
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  )
}

// ============ Main Component ============
export function SynergyReport({
  result,
  title = 'Muscle Synergy Analysis',
  showReconstruction = true,
  normalizeTime = true,
  onExport
}: SynergyReportProps) {
  const reportRef = useRef<HTMLDivElement>(null)
  const [r2Values, setR2Values] = useState<number[]>([])

  // Calculate R² values
  useEffect(() => {
    if (result.original && result.reconstruction) {
      const r2s = result.original.map((orig, i) => {
        const recon = result.reconstruction?.[i] || []
        if (orig.length === 0 || recon.length === 0) return 0
        return computeR2(orig, recon)
      })
      setR2Values(r2s)
    }
  }, [result])

  // Generate VAF curve if not provided
  const vafCurve = result.vafCurve || [result.vaf]
  const optimalN = result.W[0]?.length || 1

  const handleExport = useCallback((format: 'png' | 'svg' | 'pdf') => {
    if (onExport) {
      onExport(format)
    } else {
      // Default export as PNG
      const element = reportRef.current
      if (!element) return
      // Would need html2canvas or similar library for actual export
      console.log('Export not implemented:', format)
    }
  }, [onExport])

  // Defensive check for valid data
  if (!result || !result.W || !result.H || result.W.length === 0 || result.H.length === 0) {
    return (
      <div className="bg-white p-6">
        <h1 className="text-2xl font-bold text-gray-900">{title}</h1>
        <div className="mt-4 p-4 bg-yellow-50 border border-yellow-200 rounded-lg">
          <p className="text-yellow-800">シナジー解析結果がありません。データを確認してください。</p>
          <p className="text-sm text-yellow-600 mt-2">
            W: {result?.W?.length ?? 0} muscles, H: {result?.H?.length ?? 0} synergies
          </p>
        </div>
      </div>
    )
  }

  return (
    <div ref={reportRef} className="bg-white p-6 space-y-6">
      {/* Header */}
      <div className="flex items-center justify-between border-b pb-4">
        <div>
          <h1 className="text-2xl font-bold text-gray-900">{title}</h1>
          <p className="text-sm text-gray-500 mt-1">
            Non-negative Matrix Factorization (NMF) Results
          </p>
        </div>
        <div className="flex gap-2">
          <button
            onClick={() => handleExport('png')}
            className="px-3 py-1.5 text-sm bg-gray-100 hover:bg-gray-200 rounded-lg transition-colors"
          >
            Export PNG
          </button>
          <button
            onClick={() => handleExport('svg')}
            className="px-3 py-1.5 text-sm bg-gray-100 hover:bg-gray-200 rounded-lg transition-colors"
          >
            Export SVG
          </button>
        </div>
      </div>

      {/* Main visualization grid */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Synergy Weights - Full width */}
        <div className="rounded-lg border p-4 bg-gray-50 lg:col-span-2">
          <SynergyWeightsChart
            W={result.W}
            muscleNames={result.muscleNames}
            width={1100}
            height={350}
          />
        </div>

        {/* Activation Patterns - Full width for faceted view */}
        <div className="rounded-lg border p-4 bg-gray-50 lg:col-span-2">
          <ActivationPatternsChart
            H={result.H}
            normalizeTime={normalizeTime}
            width={1100}
            height={100 + result.H.length * 90}
          />
        </div>

        {/* VAF Curve */}
        <div className="rounded-lg border p-4 bg-gray-50">
          <VAFCurveChart
            vafCurve={vafCurve}
            optimalN={optimalN}
            width={400}
            height={250}
          />
        </div>

        {/* Reconstruction Quality */}
        {showReconstruction && result.original && result.reconstruction && (
          <div className="rounded-lg border p-4 bg-gray-50">
            <ReconstructionQualityChart
              original={result.original}
              reconstruction={result.reconstruction}
              muscleNames={result.muscleNames}
              width={400}
              height={250}
            />
          </div>
        )}
      </div>

      {/* Summary Statistics */}
      <div className="max-w-md">
        <SummaryStatisticsTable result={result} r2Values={r2Values} />
      </div>

      {/* Methods description */}
      <div className="rounded-lg border bg-blue-50 p-4">
        <h3 className="font-semibold text-blue-900 mb-2">Methods</h3>
        <p className="text-sm text-blue-800">
          Muscle synergies were extracted using Non-negative Matrix Factorization (NMF)
          with multiplicative update rules. The EMG data matrix (muscles x time) was
          decomposed into synergy weights (W) and activation patterns (H). The optimal
          number of synergies was determined using a VAF threshold of 90%. Each synergy
          represents a group of muscles activated together with a common temporal pattern.
        </p>
      </div>

      {/* Footer */}
      <div className="text-xs text-gray-400 text-center pt-4 border-t">
        Generated by PhysioExperiment | {new Date().toLocaleString('ja-JP')}
      </div>
    </div>
  )
}

export default SynergyReport

// ============ Types for Comparison ============
export type SynergyMethod = 'nmf' | 'pca' | 'fa'

export interface MethodResult extends SynergyResult {
  method: SynergyMethod
  eigenvalues?: number[]
  communalities?: number[]
}

interface SynergyComparisonProps {
  results: Record<SynergyMethod, MethodResult>
  similarity: Record<string, number>
  muscleNames: string[]
  title?: string
}

const METHOD_LABELS: Record<SynergyMethod, { name: string; nameJa: string; color: string }> = {
  nmf: { name: 'NMF', nameJa: '非負値行列因子分解', color: '#E63946' },
  pca: { name: 'PCA', nameJa: '主成分分析', color: '#457B9D' },
  fa: { name: 'FA', nameJa: '因子分析', color: '#2A9D8F' }
}

// ============ Method Comparison Card ============
function MethodCard({
  result,
  muscleNames,
  isSelected,
  onSelect
}: {
  result: MethodResult
  muscleNames: string[]
  isSelected: boolean
  onSelect: () => void
}) {
  const canvasRef = useRef<HTMLCanvasElement>(null)
  const methodInfo = METHOD_LABELS[result.method]

  useEffect(() => {
    const canvas = canvasRef.current
    if (!canvas) return
    const ctx = canvas.getContext('2d')
    if (!ctx) return

    const width = 280
    const height = 160
    const dpr = window.devicePixelRatio || 1
    canvas.width = width * dpr
    canvas.height = height * dpr
    canvas.style.width = `${width}px`
    canvas.style.height = `${height}px`
    ctx.scale(dpr, dpr)

    ctx.fillStyle = '#ffffff'
    ctx.fillRect(0, 0, width, height)

    const nMuscles = result.W.length
    const nSynergies = result.W[0]?.length || 0
    if (nMuscles === 0 || nSynergies === 0) return

    const margin = { top: 25, right: 10, bottom: 30, left: 35 }
    const plotWidth = width - margin.left - margin.right
    const plotHeight = height - margin.top - margin.bottom

    // Normalize W
    const maxW = Math.max(...result.W.flat())
    const normalizedW = result.W.map(row => row.map(v => v / (maxW || 1)))

    // Draw as heatmap
    const cellWidth = plotWidth / nSynergies
    const cellHeight = plotHeight / nMuscles

    for (let m = 0; m < nMuscles; m++) {
      for (let s = 0; s < nSynergies; s++) {
        const intensity = normalizedW[m][s]
        const r = Math.round(255 - intensity * 200)
        const g = Math.round(255 - intensity * 100)
        const b = Math.round(255 - intensity * 50)
        ctx.fillStyle = `rgb(${r},${g},${b})`
        ctx.fillRect(
          margin.left + s * cellWidth,
          margin.top + m * cellHeight,
          cellWidth - 1,
          cellHeight - 1
        )
      }
    }

    // Title
    ctx.fillStyle = methodInfo.color
    ctx.font = 'bold 12px Inter, sans-serif'
    ctx.textAlign = 'center'
    ctx.fillText(methodInfo.name, width / 2, 15)

    // X-axis (synergies)
    ctx.fillStyle = '#6b7280'
    ctx.font = '9px Inter, sans-serif'
    for (let s = 0; s < nSynergies; s++) {
      ctx.fillText(
        `S${s + 1}`,
        margin.left + (s + 0.5) * cellWidth,
        margin.top + plotHeight + 15
      )
    }

    // Y-axis (muscles) - show first few
    ctx.textAlign = 'right'
    const maxLabels = Math.min(nMuscles, 6)
    const step = Math.ceil(nMuscles / maxLabels)
    for (let m = 0; m < nMuscles; m += step) {
      const label = muscleNames[m]?.slice(0, 4) || `M${m + 1}`
      ctx.fillText(label, margin.left - 3, margin.top + (m + 0.5) * cellHeight + 3)
    }

  }, [result, muscleNames])

  return (
    <div
      onClick={onSelect}
      className={`
        rounded-lg border-2 p-3 cursor-pointer transition-all
        ${isSelected ? 'border-blue-500 bg-blue-50' : 'border-gray-200 hover:border-gray-300'}
      `}
    >
      <canvas ref={canvasRef} className="mx-auto" />
      <div className="mt-2 text-center">
        <div className="text-lg font-bold" style={{ color: methodInfo.color }}>
          VAF: {result.vaf.toFixed(1)}%
        </div>
        <div className="text-xs text-gray-500">{methodInfo.nameJa}</div>
      </div>
    </div>
  )
}

// ============ Similarity Matrix ============
function SimilarityMatrix({
  similarity,
  methods
}: {
  similarity: Record<string, number>
  methods: SynergyMethod[]
}) {
  return (
    <div className="rounded-lg border bg-white p-4">
      <h3 className="font-semibold text-gray-900 mb-3">Method Similarity</h3>
      <table className="w-full text-sm">
        <thead>
          <tr>
            <th className="p-2"></th>
            {methods.map(m => (
              <th key={m} className="p-2 text-center" style={{ color: METHOD_LABELS[m].color }}>
                {METHOD_LABELS[m].name}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {methods.map((m1, i) => (
            <tr key={m1}>
              <td className="p-2 font-medium" style={{ color: METHOD_LABELS[m1].color }}>
                {METHOD_LABELS[m1].name}
              </td>
              {methods.map((m2, j) => {
                if (i === j) {
                  return <td key={m2} className="p-2 text-center text-gray-400">-</td>
                }
                const key = i < j ? `${m1}_${m2}` : `${m2}_${m1}`
                const sim = similarity[key] ?? 0
                const bgColor = sim > 0.8 ? 'bg-green-100' : sim > 0.6 ? 'bg-yellow-100' : 'bg-red-100'
                return (
                  <td key={m2} className={`p-2 text-center font-mono ${bgColor}`}>
                    {sim.toFixed(3)}
                  </td>
                )
              })}
            </tr>
          ))}
        </tbody>
      </table>
      <p className="text-xs text-gray-500 mt-2">
        値は対応するシナジー間の平均相関係数（高いほど類似）
      </p>
    </div>
  )
}

// ============ VAF Comparison Chart ============
function VAFComparisonChart({
  results,
  width = 300,
  height = 200
}: {
  results: Record<SynergyMethod, MethodResult>
  width?: number
  height?: number
}) {
  const canvasRef = useRef<HTMLCanvasElement>(null)
  const methods = Object.keys(results) as SynergyMethod[]

  useEffect(() => {
    const canvas = canvasRef.current
    if (!canvas) return
    const ctx = canvas.getContext('2d')
    if (!ctx) return

    const dpr = window.devicePixelRatio || 1
    canvas.width = width * dpr
    canvas.height = height * dpr
    canvas.style.width = `${width}px`
    canvas.style.height = `${height}px`
    ctx.scale(dpr, dpr)

    ctx.fillStyle = '#ffffff'
    ctx.fillRect(0, 0, width, height)

    const margin = { top: 30, right: 20, bottom: 50, left: 50 }
    const plotWidth = width - margin.left - margin.right
    const plotHeight = height - margin.top - margin.bottom

    const barWidth = plotWidth / methods.length * 0.6
    const barGap = plotWidth / methods.length * 0.4

    // Y-axis grid
    ctx.strokeStyle = '#e5e7eb'
    ctx.lineWidth = 1
    for (let i = 0; i <= 5; i++) {
      const y = margin.top + plotHeight * (1 - i / 5)
      const val = i * 20
      ctx.beginPath()
      ctx.moveTo(margin.left, y)
      ctx.lineTo(margin.left + plotWidth, y)
      ctx.stroke()

      ctx.fillStyle = '#6b7280'
      ctx.font = '10px Inter, sans-serif'
      ctx.textAlign = 'right'
      ctx.fillText(`${val}%`, margin.left - 5, y + 3)
    }

    // Draw bars
    methods.forEach((method, i) => {
      const x = margin.left + i * (barWidth + barGap) + barGap / 2
      const vaf = results[method].vaf
      const barHeight = (vaf / 100) * plotHeight
      const y = margin.top + plotHeight - barHeight

      ctx.fillStyle = METHOD_LABELS[method].color
      ctx.fillRect(x, y, barWidth, barHeight)

      // VAF value on top
      ctx.fillStyle = '#374151'
      ctx.font = 'bold 11px Inter, sans-serif'
      ctx.textAlign = 'center'
      ctx.fillText(`${vaf.toFixed(1)}%`, x + barWidth / 2, y - 5)

      // Method label
      ctx.fillStyle = METHOD_LABELS[method].color
      ctx.font = 'bold 11px Inter, sans-serif'
      ctx.fillText(METHOD_LABELS[method].name, x + barWidth / 2, margin.top + plotHeight + 20)
    })

    // Title
    ctx.fillStyle = '#1f2937'
    ctx.font = 'bold 12px Inter, sans-serif'
    ctx.textAlign = 'center'
    ctx.fillText('VAF Comparison', width / 2, 15)

    // Y-axis label
    ctx.save()
    ctx.translate(12, margin.top + plotHeight / 2)
    ctx.rotate(-Math.PI / 2)
    ctx.fillStyle = '#374151'
    ctx.font = '10px Inter, sans-serif'
    ctx.textAlign = 'center'
    ctx.fillText('VAF (%)', 0, 0)
    ctx.restore()

  }, [results, width, height, methods])

  return <canvas ref={canvasRef} />
}

// ============ Main Comparison Component ============
export function SynergyComparison({
  results,
  similarity,
  muscleNames,
  title = 'Muscle Synergy Method Comparison'
}: SynergyComparisonProps) {
  const methods = Object.keys(results || {}) as SynergyMethod[]

  // Defensive check
  if (!results || methods.length === 0) {
    return (
      <div className="bg-white p-6">
        <h1 className="text-2xl font-bold text-gray-900">{title}</h1>
        <div className="mt-4 p-4 bg-yellow-50 border border-yellow-200 rounded-lg">
          <p className="text-yellow-800">比較結果がありません。解析を再実行してください。</p>
        </div>
      </div>
    )
  }

  const [selectedMethod, setSelectedMethod] = useState<SynergyMethod>(methods[0])
  const [enabledMethods, setEnabledMethods] = useState<Set<SynergyMethod>>(new Set(methods))

  const toggleMethod = (method: SynergyMethod) => {
    const newEnabled = new Set(enabledMethods)
    if (newEnabled.has(method)) {
      if (newEnabled.size > 1) {
        newEnabled.delete(method)
        if (selectedMethod === method) {
          setSelectedMethod([...newEnabled][0])
        }
      }
    } else {
      newEnabled.add(method)
    }
    setEnabledMethods(newEnabled)
  }

  const selectedResult = results[selectedMethod]

  return (
    <div className="bg-white p-6 space-y-6">
      {/* Header */}
      <div className="border-b pb-4">
        <h1 className="text-2xl font-bold text-gray-900">{title}</h1>
        <p className="text-sm text-gray-500 mt-1">
          Compare muscle synergy extraction methods: NMF, PCA, and Factor Analysis
        </p>
      </div>

      {/* Method Selection */}
      <div className="flex items-center gap-4 p-4 bg-gray-50 rounded-lg">
        <span className="text-sm font-medium text-gray-700">Methods:</span>
        {methods.map(method => (
          <label key={method} className="flex items-center gap-2 cursor-pointer">
            <input
              type="checkbox"
              checked={enabledMethods.has(method)}
              onChange={() => toggleMethod(method)}
              className="rounded"
            />
            <span
              className="font-medium"
              style={{ color: METHOD_LABELS[method].color }}
            >
              {METHOD_LABELS[method].name}
            </span>
          </label>
        ))}
      </div>

      {/* Comparison Grid */}
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-4">
        {[...enabledMethods].map(method => (
          <MethodCard
            key={method}
            result={{ ...results[method], muscleNames }}
            muscleNames={muscleNames}
            isSelected={selectedMethod === method}
            onSelect={() => setSelectedMethod(method)}
          />
        ))}
      </div>

      {/* Metrics Row */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <VAFComparisonChart
          results={Object.fromEntries(
            [...enabledMethods].map(m => [m, results[m]])
          ) as Record<SynergyMethod, MethodResult>}
        />
        <SimilarityMatrix
          similarity={similarity}
          methods={[...enabledMethods]}
        />
      </div>

      {/* Detailed View of Selected Method */}
      <div className="border-t pt-6">
        <h2 className="text-xl font-semibold text-gray-900 mb-4">
          Detailed View: {METHOD_LABELS[selectedMethod].name}
          <span className="text-sm font-normal text-gray-500 ml-2">
            ({METHOD_LABELS[selectedMethod].nameJa})
          </span>
        </h2>
        <SynergyReport
          result={{ ...selectedResult, muscleNames }}
          title=""
          showReconstruction={false}
        />
      </div>

      {/* Methods Description */}
      <div className="rounded-lg border bg-blue-50 p-4">
        <h3 className="font-semibold text-blue-900 mb-2">Methods Description</h3>
        <div className="text-sm text-blue-800 space-y-2">
          <p>
            <strong>NMF (Non-negative Matrix Factorization):</strong>
            Constrains both weights and activations to be non-negative,
            providing parts-based representation naturally suited for muscle activation.
          </p>
          <p>
            <strong>PCA (Principal Component Analysis):</strong>
            Maximizes variance explained, eigenvectors taken as absolute values
            for synergy interpretation. Computationally efficient.
          </p>
          <p>
            <strong>FA (Factor Analysis):</strong>
            Models underlying latent factors with varimax rotation for
            interpretable structure. Accounts for unique variance per muscle.
          </p>
        </div>
      </div>

      {/* Footer */}
      <div className="text-xs text-gray-400 text-center pt-4 border-t">
        Generated by PhysioExperiment | {new Date().toLocaleString('ja-JP')}
      </div>
    </div>
  )
}
