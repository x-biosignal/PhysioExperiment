import { type ClassValue, clsx } from 'clsx'
import { twMerge } from 'tailwind-merge'

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs))
}

export function formatDuration(seconds: number): string {
  const hours = Math.floor(seconds / 3600)
  const mins = Math.floor((seconds % 3600) / 60)
  const secs = Math.floor(seconds % 60)

  if (hours > 0) {
    return `${hours}:${mins.toString().padStart(2, '0')}:${secs.toString().padStart(2, '0')}`
  }
  return `${mins}:${secs.toString().padStart(2, '0')}`
}

export function formatTime(seconds: number): string {
  const mins = Math.floor(seconds / 60)
  const secs = (seconds % 60).toFixed(2)
  return mins > 0 ? `${mins}:${secs.padStart(5, '0')}` : `${secs}s`
}

export function formatFileSize(bytes: number): string {
  if (bytes === 0) return '0 B'
  const k = 1024
  const sizes = ['B', 'KB', 'MB', 'GB', 'TB']
  const i = Math.floor(Math.log(bytes) / Math.log(k))
  return `${parseFloat((bytes / Math.pow(k, i)).toFixed(2))} ${sizes[i]}`
}

export function formatBytes(bytes: number): string {
  if (bytes === 0) return '0 B'
  const k = 1024
  const sizes = ['B', 'KB', 'MB', 'GB', 'TB']
  const i = Math.floor(Math.log(bytes) / Math.log(k))
  return `${parseFloat((bytes / Math.pow(k, i)).toFixed(2))} ${sizes[i]}`
}

export function formatFrequency(hz: number): string {
  if (hz >= 1000) {
    return `${(hz / 1000).toFixed(1)} kHz`
  }
  return `${hz} Hz`
}

export function debounce<T extends (...args: unknown[]) => unknown>(
  func: T,
  wait: number
): (...args: Parameters<T>) => void {
  let timeoutId: ReturnType<typeof setTimeout> | null = null

  return (...args: Parameters<T>) => {
    if (timeoutId) {
      clearTimeout(timeoutId)
    }
    timeoutId = setTimeout(() => {
      func(...args)
    }, wait)
  }
}

export function throttle<T extends (...args: unknown[]) => unknown>(
  func: T,
  limit: number
): (...args: Parameters<T>) => void {
  let inThrottle = false

  return (...args: Parameters<T>) => {
    if (!inThrottle) {
      func(...args)
      inThrottle = true
      setTimeout(() => {
        inThrottle = false
      }, limit)
    }
  }
}

// Color scales for signal visualization
export const CHANNEL_COLORS = [
  '#3b82f6', // blue
  '#22c55e', // green
  '#ef4444', // red
  '#f59e0b', // amber
  '#8b5cf6', // violet
  '#06b6d4', // cyan
  '#ec4899', // pink
  '#14b8a6', // teal
  '#f97316', // orange
  '#6366f1', // indigo
]

export function getChannelColor(index: number): string {
  return CHANNEL_COLORS[index % CHANNEL_COLORS.length]
}

export function getSignalColorPalette(count: number): string[] {
  const colors: string[] = []
  for (let i = 0; i < count; i++) {
    colors.push(CHANNEL_COLORS[i % CHANNEL_COLORS.length])
  }
  return colors
}

// Viridis color scale for spectrograms
export function viridisColor(t: number): string {
  // Simplified viridis approximation
  const r = Math.round(68 + 187 * t)
  const g = Math.round(1 + 203 * t - 73 * t * t)
  const b = Math.round(84 + 96 * t - 200 * t * t)
  return `rgb(${Math.max(0, Math.min(255, r))}, ${Math.max(0, Math.min(255, g))}, ${Math.max(0, Math.min(255, b))})`
}

// Generate time ticks for axis
export function generateTimeTicks(start: number, end: number, maxTicks: number = 10): number[] {
  const range = end - start
  const roughStep = range / maxTicks

  // Find a nice step size
  const magnitude = Math.pow(10, Math.floor(Math.log10(roughStep)))
  const residual = roughStep / magnitude

  let niceStep: number
  if (residual >= 5) niceStep = 5 * magnitude
  else if (residual >= 2) niceStep = 2 * magnitude
  else niceStep = magnitude

  const ticks: number[] = []
  const firstTick = Math.ceil(start / niceStep) * niceStep

  for (let tick = firstTick; tick <= end; tick += niceStep) {
    ticks.push(Math.round(tick * 1000) / 1000) // Round to avoid floating point errors
  }

  return ticks
}
