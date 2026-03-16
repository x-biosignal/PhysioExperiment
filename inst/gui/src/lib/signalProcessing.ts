// Signal processing utilities for physiological data analysis

/**
 * Compute Short-Time Fourier Transform (STFT) for spectrogram
 */
export function computeSTFT(
  signal: number[],
  samplingRate: number,
  windowSize: number = 256,
  hopSize: number = 64,
  windowType: 'hann' | 'hamming' | 'blackman' = 'hann'
): { frequencies: number[]; times: number[]; magnitude: number[][] } {
  const nSamples = signal.length
  const nFreqs = Math.floor(windowSize / 2) + 1
  const nFrames = Math.floor((nSamples - windowSize) / hopSize) + 1

  // Create window function
  const window = createWindow(windowSize, windowType)

  // Initialize output arrays
  const magnitude: number[][] = []
  const times: number[] = []
  const frequencies: number[] = []

  // Compute frequency bins
  for (let i = 0; i < nFreqs; i++) {
    frequencies.push((i * samplingRate) / windowSize)
  }

  // Compute STFT for each frame
  for (let frame = 0; frame < nFrames; frame++) {
    const startIdx = frame * hopSize
    times.push(startIdx / samplingRate)

    // Extract and window the frame
    const frameData: number[] = []
    for (let i = 0; i < windowSize; i++) {
      frameData.push(signal[startIdx + i] * window[i])
    }

    // Compute FFT magnitude
    const fftResult = fft(frameData)
    const frameMag: number[] = []
    for (let i = 0; i < nFreqs; i++) {
      const mag = Math.sqrt(Math.pow(fftResult.real[i], 2) + Math.pow(fftResult.imag[i], 2))
      // Convert to dB with floor
      frameMag.push(20 * Math.log10(Math.max(mag, 1e-10)))
    }
    magnitude.push(frameMag)
  }

  return { frequencies, times, magnitude }
}

/**
 * Create window function
 */
function createWindow(size: number, type: 'hann' | 'hamming' | 'blackman'): number[] {
  const window: number[] = []
  for (let i = 0; i < size; i++) {
    const x = (2 * Math.PI * i) / (size - 1)
    switch (type) {
      case 'hann':
        window.push(0.5 * (1 - Math.cos(x)))
        break
      case 'hamming':
        window.push(0.54 - 0.46 * Math.cos(x))
        break
      case 'blackman':
        window.push(0.42 - 0.5 * Math.cos(x) + 0.08 * Math.cos(2 * x))
        break
    }
  }
  return window
}

/**
 * Simple FFT implementation (Cooley-Tukey radix-2)
 */
function fft(signal: number[]): { real: number[]; imag: number[] } {
  const n = signal.length
  const real: number[] = [...signal]
  const imag: number[] = new Array(n).fill(0)

  // Bit reversal
  let j = 0
  for (let i = 0; i < n - 1; i++) {
    if (i < j) {
      const tempReal = real[i]
      real[i] = real[j]
      real[j] = tempReal
      const tempImag = imag[i]
      imag[i] = imag[j]
      imag[j] = tempImag
    }
    let k = n >> 1
    while (k <= j) {
      j -= k
      k >>= 1
    }
    j += k
  }

  // FFT
  for (let len = 2; len <= n; len *= 2) {
    const halfLen = len / 2
    const angle = (-2 * Math.PI) / len
    for (let i = 0; i < n; i += len) {
      for (let k = 0; k < halfLen; k++) {
        const theta = angle * k
        const cos = Math.cos(theta)
        const sin = Math.sin(theta)
        const tReal = real[i + k + halfLen] * cos - imag[i + k + halfLen] * sin
        const tImag = real[i + k + halfLen] * sin + imag[i + k + halfLen] * cos
        real[i + k + halfLen] = real[i + k] - tReal
        imag[i + k + halfLen] = imag[i + k] - tImag
        real[i + k] += tReal
        imag[i + k] += tImag
      }
    }
  }

  return { real, imag }
}

/**
 * Bandpass filter using windowed sinc
 */
export function bandpassFilter(
  signal: number[],
  samplingRate: number,
  lowFreq: number,
  highFreq: number,
  order: number = 101
): number[] {
  // Create bandpass filter kernel
  const kernel = createBandpassKernel(samplingRate, lowFreq, highFreq, order)

  // Apply convolution
  return convolve(signal, kernel)
}

/**
 * Create bandpass filter kernel using windowed sinc
 */
function createBandpassKernel(
  samplingRate: number,
  lowFreq: number,
  highFreq: number,
  order: number
): number[] {
  const kernel: number[] = []
  const nyquist = samplingRate / 2
  const lowNorm = lowFreq / nyquist
  const highNorm = highFreq / nyquist
  const halfOrder = Math.floor(order / 2)

  for (let i = 0; i < order; i++) {
    const n = i - halfOrder
    let h: number

    if (n === 0) {
      h = 2 * (highNorm - lowNorm)
    } else {
      h = (Math.sin(Math.PI * n * highNorm) - Math.sin(Math.PI * n * lowNorm)) / (Math.PI * n)
    }

    // Apply Hamming window
    const window = 0.54 - 0.46 * Math.cos((2 * Math.PI * i) / (order - 1))
    kernel.push(h * window)
  }

  // Normalize
  const sum = kernel.reduce((a, b) => a + b, 0)
  return kernel.map(k => k / sum)
}

/**
 * Convolution operation
 */
function convolve(signal: number[], kernel: number[]): number[] {
  const result: number[] = []
  const halfKernel = Math.floor(kernel.length / 2)

  for (let i = 0; i < signal.length; i++) {
    let sum = 0
    for (let j = 0; j < kernel.length; j++) {
      const idx = i - halfKernel + j
      if (idx >= 0 && idx < signal.length) {
        sum += signal[idx] * kernel[j]
      }
    }
    result.push(sum)
  }

  return result
}

/**
 * Notch filter for powerline noise removal
 */
export function notchFilter(
  signal: number[],
  samplingRate: number,
  centerFreq: number = 50,
  bandwidth: number = 2
): number[] {
  // Use bandstop approach: original - bandpass around notch frequency
  const notchLow = centerFreq - bandwidth / 2
  const notchHigh = centerFreq + bandwidth / 2

  // Simple notch using difference
  const notchSignal = bandpassFilter(signal, samplingRate, notchLow, notchHigh, 51)
  return signal.map((s, i) => s - notchSignal[i])
}

/**
 * Compute band power in specified frequency range
 */
export function computeBandPower(
  signal: number[],
  samplingRate: number,
  lowFreq: number,
  highFreq: number
): number {
  const filtered = bandpassFilter(signal, samplingRate, lowFreq, highFreq)
  const power = filtered.reduce((sum, x) => sum + x * x, 0) / filtered.length
  return power
}

/**
 * Compute power spectral density
 */
export function computePSD(
  signal: number[],
  samplingRate: number,
  windowSize: number = 256
): { frequencies: number[]; power: number[] } {
  // Pad signal to power of 2
  const paddedLength = Math.pow(2, Math.ceil(Math.log2(windowSize)))
  const paddedSignal = [...signal.slice(0, paddedLength)]
  while (paddedSignal.length < paddedLength) {
    paddedSignal.push(0)
  }

  // Apply window
  const window = createWindow(paddedLength, 'hann')
  const windowedSignal = paddedSignal.map((s, i) => s * window[i])

  // Compute FFT
  const { real, imag } = fft(windowedSignal)

  // Compute power spectrum
  const nFreqs = Math.floor(paddedLength / 2) + 1
  const frequencies: number[] = []
  const power: number[] = []

  for (let i = 0; i < nFreqs; i++) {
    frequencies.push((i * samplingRate) / paddedLength)
    const p = (Math.pow(real[i], 2) + Math.pow(imag[i], 2)) / paddedLength
    power.push(10 * Math.log10(Math.max(p, 1e-10)))
  }

  return { frequencies, power }
}

/**
 * Extract epochs from continuous signal based on events
 */
export function extractEpochs(
  signal: number[],
  samplingRate: number,
  eventTimes: number[],
  preTime: number,  // seconds before event
  postTime: number  // seconds after event
): number[][] {
  const epochs: number[][] = []
  const preSamples = Math.floor(preTime * samplingRate)
  const postSamples = Math.floor(postTime * samplingRate)

  for (const eventTime of eventTimes) {
    const eventSample = Math.floor(eventTime * samplingRate)
    const startSample = eventSample - preSamples
    const endSample = eventSample + postSamples

    if (startSample >= 0 && endSample <= signal.length) {
      const epoch = signal.slice(startSample, endSample)
      epochs.push(epoch)
    }
  }

  return epochs
}

/**
 * Compute average (ERP)
 */
export function computeERP(epochs: number[][]): number[] {
  if (epochs.length === 0) return []

  const epochLength = epochs[0].length
  const erp: number[] = new Array(epochLength).fill(0)

  for (const epoch of epochs) {
    for (let i = 0; i < epochLength; i++) {
      erp[i] += epoch[i]
    }
  }

  return erp.map(x => x / epochs.length)
}

/**
 * Baseline correction
 */
export function baselineCorrect(
  epoch: number[],
  samplingRate: number,
  baselineStart: number,
  baselineEnd: number
): number[] {
  const startSample = Math.floor(baselineStart * samplingRate)
  const endSample = Math.floor(baselineEnd * samplingRate)

  let sum = 0
  for (let i = startSample; i < endSample; i++) {
    sum += epoch[i]
  }
  const baseline = sum / (endSample - startSample)

  return epoch.map(x => x - baseline)
}

/**
 * Compute t-test between two groups of epochs
 */
export function tTest(
  group1: number[][],
  group2: number[][],
  sampleIdx: number
): { t: number; p: number; df: number } {
  const n1 = group1.length
  const n2 = group2.length

  // Extract values at sample index
  const values1 = group1.map(e => e[sampleIdx])
  const values2 = group2.map(e => e[sampleIdx])

  // Compute means
  const mean1 = values1.reduce((a, b) => a + b, 0) / n1
  const mean2 = values2.reduce((a, b) => a + b, 0) / n2

  // Compute variances
  const var1 = values1.reduce((sum, x) => sum + Math.pow(x - mean1, 2), 0) / (n1 - 1)
  const var2 = values2.reduce((sum, x) => sum + Math.pow(x - mean2, 2), 0) / (n2 - 1)

  // Pooled standard error
  const se = Math.sqrt(var1 / n1 + var2 / n2)

  // t-statistic
  const t = (mean1 - mean2) / se

  // Degrees of freedom (Welch-Satterthwaite)
  const varTerm1 = var1 / n1
  const varTerm2 = var2 / n2
  const df = Math.pow(varTerm1 + varTerm2, 2) /
    (Math.pow(varTerm1, 2) / (n1 - 1) + Math.pow(varTerm2, 2) / (n2 - 1))

  // p-value approximation using normal distribution for large df
  const p = 2 * (1 - normalCDF(Math.abs(t)))

  return { t, p, df }
}

/**
 * Pointwise t-test across all samples
 */
export function pointwiseTTest(
  group1: number[][],
  group2: number[][]
): { t: number[]; p: number[] } {
  if (group1.length === 0 || group2.length === 0) {
    return { t: [], p: [] }
  }

  const nSamples = group1[0].length
  const tValues: number[] = []
  const pValues: number[] = []

  for (let i = 0; i < nSamples; i++) {
    const result = tTest(group1, group2, i)
    tValues.push(result.t)
    pValues.push(result.p)
  }

  return { t: tValues, p: pValues }
}

/**
 * Normal CDF approximation
 */
function normalCDF(x: number): number {
  const a1 = 0.254829592
  const a2 = -0.284496736
  const a3 = 1.421413741
  const a4 = -1.453152027
  const a5 = 1.061405429
  const p = 0.3275911

  const sign = x < 0 ? -1 : 1
  x = Math.abs(x) / Math.sqrt(2)

  const t = 1.0 / (1.0 + p * x)
  const y = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * Math.exp(-x * x)

  return 0.5 * (1.0 + sign * y)
}

/**
 * 10-20 electrode positions (normalized coordinates)
 */
export const electrode1020Positions: Record<string, { x: number; y: number }> = {
  'Fp1': { x: 0.30, y: 0.13 },
  'Fp2': { x: 0.70, y: 0.13 },
  'F7': { x: 0.13, y: 0.30 },
  'F3': { x: 0.35, y: 0.30 },
  'Fz': { x: 0.50, y: 0.30 },
  'F4': { x: 0.65, y: 0.30 },
  'F8': { x: 0.87, y: 0.30 },
  'T3': { x: 0.07, y: 0.50 },
  'C3': { x: 0.30, y: 0.50 },
  'Cz': { x: 0.50, y: 0.50 },
  'C4': { x: 0.70, y: 0.50 },
  'T4': { x: 0.93, y: 0.50 },
  'T5': { x: 0.13, y: 0.70 },
  'P3': { x: 0.35, y: 0.70 },
  'Pz': { x: 0.50, y: 0.70 },
  'P4': { x: 0.65, y: 0.70 },
  'T6': { x: 0.87, y: 0.70 },
  'O1': { x: 0.30, y: 0.87 },
  'Oz': { x: 0.50, y: 0.87 },
  'O2': { x: 0.70, y: 0.87 },
}

/**
 * Inverse Distance Weighting interpolation for topographic maps
 */
export function interpolateTopomap(
  values: Record<string, number>,
  positions: Record<string, { x: number; y: number }>,
  resolution: number = 64,
  power: number = 2
): number[][] {
  const grid: number[][] = []
  const electrodes = Object.keys(values)

  for (let y = 0; y < resolution; y++) {
    const row: number[] = []
    for (let x = 0; x < resolution; x++) {
      const px = x / (resolution - 1)
      const py = y / (resolution - 1)

      // Check if point is within head circle
      const dx = px - 0.5
      const dy = py - 0.5
      const dist = Math.sqrt(dx * dx + dy * dy)

      if (dist > 0.5) {
        row.push(NaN)
        continue
      }

      // IDW interpolation
      let sumWeights = 0
      let sumValues = 0

      for (const elec of electrodes) {
        const pos = positions[elec]
        if (!pos) continue

        const d = Math.sqrt(Math.pow(px - pos.x, 2) + Math.pow(py - pos.y, 2))

        if (d < 0.001) {
          // Very close to electrode
          sumWeights = 1
          sumValues = values[elec]
          break
        }

        const weight = 1 / Math.pow(d, power)
        sumWeights += weight
        sumValues += weight * values[elec]
      }

      row.push(sumWeights > 0 ? sumValues / sumWeights : 0)
    }
    grid.push(row)
  }

  return grid
}

/**
 * Compute connectivity (PLV - Phase Locking Value)
 */
export function computePLV(
  signal1: number[],
  signal2: number[],
  samplingRate: number,
  lowFreq: number,
  highFreq: number
): number {
  // Filter signals to frequency band
  const filtered1 = bandpassFilter(signal1, samplingRate, lowFreq, highFreq)
  const filtered2 = bandpassFilter(signal2, samplingRate, lowFreq, highFreq)

  // Compute instantaneous phase using Hilbert transform approximation
  const phase1 = hilbertPhase(filtered1)
  const phase2 = hilbertPhase(filtered2)

  // Compute PLV
  let sumCos = 0
  let sumSin = 0
  const n = phase1.length

  for (let i = 0; i < n; i++) {
    const phaseDiff = phase1[i] - phase2[i]
    sumCos += Math.cos(phaseDiff)
    sumSin += Math.sin(phaseDiff)
  }

  return Math.sqrt(Math.pow(sumCos, 2) + Math.pow(sumSin, 2)) / n
}

/**
 * Compute Hilbert transform phase (simplified)
 */
function hilbertPhase(signal: number[]): number[] {
  const phases: number[] = []

  for (let i = 1; i < signal.length - 1; i++) {
    // Approximate derivative
    const derivative = (signal[i + 1] - signal[i - 1]) / 2
    // Approximate phase
    const phase = Math.atan2(derivative, signal[i])
    phases.push(phase)
  }

  // Pad edges
  phases.unshift(phases[0])
  phases.push(phases[phases.length - 1])

  return phases
}

/**
 * Compute connectivity matrix for all channel pairs
 */
export function computeConnectivityMatrix(
  signals: Record<string, number[]>,
  samplingRate: number,
  lowFreq: number,
  highFreq: number
): { channels: string[]; matrix: number[][] } {
  const channels = Object.keys(signals)
  const n = channels.length
  const matrix: number[][] = []

  for (let i = 0; i < n; i++) {
    const row: number[] = []
    for (let j = 0; j < n; j++) {
      if (i === j) {
        row.push(1)
      } else if (j < i) {
        row.push(matrix[j][i])
      } else {
        const plv = computePLV(
          signals[channels[i]],
          signals[channels[j]],
          samplingRate,
          lowFreq,
          highFreq
        )
        row.push(plv)
      }
    }
    matrix.push(row)
  }

  return { channels, matrix }
}

/**
 * Downsample signal
 */
export function downsample(signal: number[], factor: number): number[] {
  const result: number[] = []
  for (let i = 0; i < signal.length; i += factor) {
    result.push(signal[i])
  }
  return result
}

/**
 * Z-score normalization
 */
export function zscore(signal: number[]): number[] {
  const mean = signal.reduce((a, b) => a + b, 0) / signal.length
  const std = Math.sqrt(
    signal.reduce((sum, x) => sum + Math.pow(x - mean, 2), 0) / signal.length
  )
  return signal.map(x => (x - mean) / (std || 1))
}

/**
 * Peak detection
 */
export function findPeaks(
  signal: number[],
  threshold: number = 0,
  minDistance: number = 1
): number[] {
  const peaks: number[] = []

  for (let i = 1; i < signal.length - 1; i++) {
    if (signal[i] > signal[i - 1] &&
        signal[i] > signal[i + 1] &&
        signal[i] > threshold) {
      // Check minimum distance from last peak
      if (peaks.length === 0 || i - peaks[peaks.length - 1] >= minDistance) {
        peaks.push(i)
      }
    }
  }

  return peaks
}

// ============ Additional Analysis Methods from PhysioAnalysis ============

/**
 * Hilbert Transform - compute analytic signal
 */
export function hilbertTransform(signal: number[]): { amplitude: number[]; phase: number[] } {
  const n = signal.length
  const paddedLength = Math.pow(2, Math.ceil(Math.log2(n)))

  // Pad signal
  const padded = [...signal]
  while (padded.length < paddedLength) padded.push(0)

  // FFT
  const { real, imag } = fft(padded)

  // Create analytic signal in frequency domain
  // H(f) = 2 for f > 0, 1 for f = 0, 0 for f < 0
  const analyticReal: number[] = []
  const analyticImag: number[] = []

  for (let i = 0; i < paddedLength; i++) {
    if (i === 0 || i === paddedLength / 2) {
      analyticReal.push(real[i])
      analyticImag.push(imag[i])
    } else if (i < paddedLength / 2) {
      analyticReal.push(2 * real[i])
      analyticImag.push(2 * imag[i])
    } else {
      analyticReal.push(0)
      analyticImag.push(0)
    }
  }

  // Inverse FFT (simplified)
  const amplitude: number[] = []
  const phase: number[] = []

  for (let i = 0; i < n; i++) {
    // Approximate using original and derivative
    const derivative = i > 0 && i < n - 1
      ? (signal[i + 1] - signal[i - 1]) / 2
      : 0
    const hilbert = derivative // Simplified Hilbert

    amplitude.push(Math.sqrt(signal[i] * signal[i] + hilbert * hilbert))
    phase.push(Math.atan2(hilbert, signal[i]))
  }

  return { amplitude, phase }
}

/**
 * Instantaneous amplitude via Hilbert transform
 */
export function instantaneousAmplitude(signal: number[]): number[] {
  return hilbertTransform(signal).amplitude
}

/**
 * Instantaneous phase via Hilbert transform
 */
export function instantaneousPhase(signal: number[]): number[] {
  return hilbertTransform(signal).phase
}

/**
 * Wavelet Transform using Morlet wavelet
 */
export function waveletTransform(
  signal: number[],
  samplingRate: number,
  frequencies: number[],
  nCycles: number = 7
): { frequencies: number[]; times: number[]; power: number[][] } {
  const nSamples = signal.length
  const times = Array.from({ length: nSamples }, (_, i) => i / samplingRate)
  const power: number[][] = []

  for (const freq of frequencies) {
    const freqPower: number[] = []
    const sigma = nCycles / (2 * Math.PI * freq)
    const waveletLength = Math.min(Math.floor(6 * sigma * samplingRate), nSamples)
    const halfLen = Math.floor(waveletLength / 2)

    // Create Morlet wavelet
    const waveletReal: number[] = []
    const waveletImag: number[] = []
    for (let i = 0; i < waveletLength; i++) {
      const t = (i - halfLen) / samplingRate
      const gaussian = Math.exp(-t * t / (2 * sigma * sigma))
      waveletReal.push(gaussian * Math.cos(2 * Math.PI * freq * t))
      waveletImag.push(gaussian * Math.sin(2 * Math.PI * freq * t))
    }

    // Convolve signal with wavelet
    for (let i = 0; i < nSamples; i++) {
      let sumReal = 0
      let sumImag = 0
      for (let j = 0; j < waveletLength; j++) {
        const signalIdx = i - halfLen + j
        if (signalIdx >= 0 && signalIdx < nSamples) {
          sumReal += signal[signalIdx] * waveletReal[j]
          sumImag += signal[signalIdx] * waveletImag[j]
        }
      }
      freqPower.push(sumReal * sumReal + sumImag * sumImag)
    }
    power.push(freqPower)
  }

  return { frequencies, times, power }
}

/**
 * Band Power extraction for standard EEG bands
 */
export function extractBandPowers(
  signal: number[],
  samplingRate: number
): Record<string, number> {
  const bands: Record<string, [number, number]> = {
    delta: [0.5, 4],
    theta: [4, 8],
    alpha: [8, 13],
    beta: [13, 30],
    gamma: [30, 100]
  }

  const powers: Record<string, number> = {}
  for (const [name, [low, high]] of Object.entries(bands)) {
    const maxFreq = samplingRate / 2
    if (high <= maxFreq) {
      powers[name] = computeBandPower(signal, samplingRate, low, Math.min(high, maxFreq - 1))
    }
  }

  return powers
}

/**
 * Phase Lag Index (PLI) - reduces volume conduction bias
 */
export function computePLI(
  signal1: number[],
  signal2: number[],
  samplingRate: number,
  lowFreq: number,
  highFreq: number
): number {
  const filtered1 = bandpassFilter(signal1, samplingRate, lowFreq, highFreq)
  const filtered2 = bandpassFilter(signal2, samplingRate, lowFreq, highFreq)

  const phase1 = instantaneousPhase(filtered1)
  const phase2 = instantaneousPhase(filtered2)

  let sum = 0
  const n = phase1.length

  for (let i = 0; i < n; i++) {
    const phaseDiff = phase1[i] - phase2[i]
    sum += Math.sign(Math.sin(phaseDiff))
  }

  return Math.abs(sum / n)
}

/**
 * Weighted Phase Lag Index (wPLI)
 */
export function computeWPLI(
  signal1: number[],
  signal2: number[],
  samplingRate: number,
  lowFreq: number,
  highFreq: number
): number {
  const filtered1 = bandpassFilter(signal1, samplingRate, lowFreq, highFreq)
  const filtered2 = bandpassFilter(signal2, samplingRate, lowFreq, highFreq)

  const phase1 = instantaneousPhase(filtered1)
  const phase2 = instantaneousPhase(filtered2)

  let sumNumerator = 0
  let sumDenominator = 0
  const n = phase1.length

  for (let i = 0; i < n; i++) {
    const phaseDiff = phase1[i] - phase2[i]
    const imagPart = Math.sin(phaseDiff)
    sumNumerator += Math.abs(imagPart) * Math.sign(imagPart)
    sumDenominator += Math.abs(imagPart)
  }

  return sumDenominator > 0 ? Math.abs(sumNumerator / sumDenominator) : 0
}

/**
 * Coherence between two signals
 */
export function computeCoherence(
  signal1: number[],
  signal2: number[],
  samplingRate: number,
  windowSize: number = 256
): { frequencies: number[]; coherence: number[] } {
  const nFreqs = Math.floor(windowSize / 2) + 1
  const frequencies: number[] = []
  for (let i = 0; i < nFreqs; i++) {
    frequencies.push((i * samplingRate) / windowSize)
  }

  // Compute PSDs and cross-spectrum using Welch's method
  const hopSize = Math.floor(windowSize / 2)
  const nSegments = Math.floor((signal1.length - windowSize) / hopSize) + 1

  const psd1: number[] = new Array(nFreqs).fill(0)
  const psd2: number[] = new Array(nFreqs).fill(0)
  const crossReal: number[] = new Array(nFreqs).fill(0)
  const crossImag: number[] = new Array(nFreqs).fill(0)

  const window = createWindow(windowSize, 'hann')

  for (let seg = 0; seg < nSegments; seg++) {
    const start = seg * hopSize
    const seg1 = signal1.slice(start, start + windowSize).map((s, i) => s * window[i])
    const seg2 = signal2.slice(start, start + windowSize).map((s, i) => s * window[i])

    const fft1 = fft(seg1)
    const fft2 = fft(seg2)

    for (let i = 0; i < nFreqs; i++) {
      psd1[i] += fft1.real[i] * fft1.real[i] + fft1.imag[i] * fft1.imag[i]
      psd2[i] += fft2.real[i] * fft2.real[i] + fft2.imag[i] * fft2.imag[i]
      // Cross-spectrum: conj(fft1) * fft2
      crossReal[i] += fft1.real[i] * fft2.real[i] + fft1.imag[i] * fft2.imag[i]
      crossImag[i] += -fft1.imag[i] * fft2.real[i] + fft1.real[i] * fft2.imag[i]
    }
  }

  // Compute coherence: |Pxy|^2 / (Pxx * Pyy)
  const coherence: number[] = []
  for (let i = 0; i < nFreqs; i++) {
    const crossMag2 = crossReal[i] * crossReal[i] + crossImag[i] * crossImag[i]
    const denom = psd1[i] * psd2[i]
    coherence.push(denom > 0 ? crossMag2 / denom : 0)
  }

  return { frequencies, coherence }
}

/**
 * Correlation matrix for multiple signals
 */
export function computeCorrelationMatrix(
  signals: Record<string, number[]>
): { channels: string[]; matrix: number[][] } {
  const channels = Object.keys(signals)
  const n = channels.length
  const matrix: number[][] = []

  for (let i = 0; i < n; i++) {
    const row: number[] = []
    for (let j = 0; j < n; j++) {
      if (i === j) {
        row.push(1)
      } else if (j < i) {
        row.push(matrix[j][i])
      } else {
        row.push(pearsonCorrelation(signals[channels[i]], signals[channels[j]]))
      }
    }
    matrix.push(row)
  }

  return { channels, matrix }
}

/**
 * Pearson correlation coefficient
 */
export function pearsonCorrelation(x: number[], y: number[]): number {
  const n = Math.min(x.length, y.length)
  const meanX = x.slice(0, n).reduce((a, b) => a + b, 0) / n
  const meanY = y.slice(0, n).reduce((a, b) => a + b, 0) / n

  let sumXY = 0
  let sumX2 = 0
  let sumY2 = 0

  for (let i = 0; i < n; i++) {
    const dx = x[i] - meanX
    const dy = y[i] - meanY
    sumXY += dx * dy
    sumX2 += dx * dx
    sumY2 += dy * dy
  }

  const denom = Math.sqrt(sumX2 * sumY2)
  return denom > 0 ? sumXY / denom : 0
}

// ============ Network Analysis Functions ============

/**
 * Create adjacency matrix from connectivity matrix with threshold
 */
export function createAdjacencyMatrix(
  connectivityMatrix: number[][],
  threshold: number,
  binary: boolean = true
): number[][] {
  const n = connectivityMatrix.length
  const adjacency: number[][] = []

  for (let i = 0; i < n; i++) {
    const row: number[] = []
    for (let j = 0; j < n; j++) {
      if (i === j) {
        row.push(0) // No self-loops
      } else if (connectivityMatrix[i][j] >= threshold) {
        row.push(binary ? 1 : connectivityMatrix[i][j])
      } else {
        row.push(0)
      }
    }
    adjacency.push(row)
  }

  return adjacency
}

/**
 * Compute node degree
 */
export function computeNodeDegree(adjacencyMatrix: number[][]): number[] {
  const n = adjacencyMatrix.length
  const degrees: number[] = []

  for (let i = 0; i < n; i++) {
    let degree = 0
    for (let j = 0; j < n; j++) {
      degree += adjacencyMatrix[i][j]
    }
    degrees.push(degree)
  }

  return degrees
}

/**
 * Compute clustering coefficient for each node
 */
export function computeClusteringCoefficient(adjacencyMatrix: number[][]): number[] {
  const n = adjacencyMatrix.length
  const clustering: number[] = []

  for (let i = 0; i < n; i++) {
    // Find neighbors
    const neighbors: number[] = []
    for (let j = 0; j < n; j++) {
      if (adjacencyMatrix[i][j] > 0) neighbors.push(j)
    }

    const k = neighbors.length
    if (k < 2) {
      clustering.push(0)
      continue
    }

    // Count edges between neighbors
    let edges = 0
    for (let a = 0; a < k; a++) {
      for (let b = a + 1; b < k; b++) {
        if (adjacencyMatrix[neighbors[a]][neighbors[b]] > 0) {
          edges++
        }
      }
    }

    const maxEdges = (k * (k - 1)) / 2
    clustering.push(maxEdges > 0 ? edges / maxEdges : 0)
  }

  return clustering
}

/**
 * Compute shortest path lengths using Floyd-Warshall
 */
export function computeShortestPaths(adjacencyMatrix: number[][]): number[][] {
  const n = adjacencyMatrix.length
  const dist: number[][] = []

  // Initialize distances
  for (let i = 0; i < n; i++) {
    const row: number[] = []
    for (let j = 0; j < n; j++) {
      if (i === j) {
        row.push(0)
      } else if (adjacencyMatrix[i][j] > 0) {
        row.push(1 / adjacencyMatrix[i][j]) // For weighted: use inverse as distance
      } else {
        row.push(Infinity)
      }
    }
    dist.push(row)
  }

  // Floyd-Warshall
  for (let k = 0; k < n; k++) {
    for (let i = 0; i < n; i++) {
      for (let j = 0; j < n; j++) {
        if (dist[i][k] + dist[k][j] < dist[i][j]) {
          dist[i][j] = dist[i][k] + dist[k][j]
        }
      }
    }
  }

  return dist
}

/**
 * Compute characteristic path length
 */
export function computeCharacteristicPathLength(adjacencyMatrix: number[][]): number {
  const paths = computeShortestPaths(adjacencyMatrix)
  const n = paths.length
  let sum = 0
  let count = 0

  for (let i = 0; i < n; i++) {
    for (let j = 0; j < n; j++) {
      if (i !== j && paths[i][j] < Infinity) {
        sum += paths[i][j]
        count++
      }
    }
  }

  return count > 0 ? sum / count : Infinity
}

/**
 * Compute global efficiency
 */
export function computeGlobalEfficiency(adjacencyMatrix: number[][]): number {
  const paths = computeShortestPaths(adjacencyMatrix)
  const n = paths.length
  let sum = 0

  for (let i = 0; i < n; i++) {
    for (let j = 0; j < n; j++) {
      if (i !== j && paths[i][j] < Infinity && paths[i][j] > 0) {
        sum += 1 / paths[i][j]
      }
    }
  }

  return sum / (n * (n - 1))
}

/**
 * Compute betweenness centrality (simplified)
 */
export function computeBetweennessCentrality(adjacencyMatrix: number[][]): number[] {
  const n = adjacencyMatrix.length
  const betweenness: number[] = new Array(n).fill(0)

  for (let s = 0; s < n; s++) {
    // BFS from source s
    const dist: number[] = new Array(n).fill(Infinity)
    const sigma: number[] = new Array(n).fill(0)
    const pred: number[][] = Array.from({ length: n }, () => [])

    dist[s] = 0
    sigma[s] = 1
    const queue: number[] = [s]
    const stack: number[] = []

    while (queue.length > 0) {
      const v = queue.shift()!
      stack.push(v)

      for (let w = 0; w < n; w++) {
        if (adjacencyMatrix[v][w] > 0) {
          if (dist[w] === Infinity) {
            dist[w] = dist[v] + 1
            queue.push(w)
          }
          if (dist[w] === dist[v] + 1) {
            sigma[w] += sigma[v]
            pred[w].push(v)
          }
        }
      }
    }

    // Back-propagation
    const delta: number[] = new Array(n).fill(0)
    while (stack.length > 0) {
      const w = stack.pop()!
      for (const v of pred[w]) {
        delta[v] += (sigma[v] / sigma[w]) * (1 + delta[w])
      }
      if (w !== s) {
        betweenness[w] += delta[w]
      }
    }
  }

  // Normalize
  const factor = (n - 1) * (n - 2)
  return betweenness.map(b => factor > 0 ? b / factor : 0)
}

// ============ Statistical Functions ============

/**
 * One-way ANOVA
 */
export function oneWayANOVA(groups: number[][]): { F: number; p: number; df1: number; df2: number } {
  const k = groups.length
  const ns = groups.map(g => g.length)
  const N = ns.reduce((a, b) => a + b, 0)

  // Group means
  const means = groups.map(g => g.reduce((a, b) => a + b, 0) / g.length)

  // Grand mean
  const grandMean = groups.flat().reduce((a, b) => a + b, 0) / N

  // Between-group sum of squares
  let SSB = 0
  for (let i = 0; i < k; i++) {
    SSB += ns[i] * Math.pow(means[i] - grandMean, 2)
  }

  // Within-group sum of squares
  let SSW = 0
  for (let i = 0; i < k; i++) {
    for (const val of groups[i]) {
      SSW += Math.pow(val - means[i], 2)
    }
  }

  const df1 = k - 1
  const df2 = N - k

  const MSB = SSB / df1
  const MSW = SSW / df2

  const F = MSW > 0 ? MSB / MSW : 0

  // Approximate p-value using F-distribution (simplified)
  const p = 1 - fCDF(F, df1, df2)

  return { F, p, df1, df2 }
}

/**
 * F-distribution CDF approximation
 */
function fCDF(x: number, d1: number, d2: number): number {
  if (x <= 0) return 0
  const beta = d2 / (d2 + d1 * x)
  return 1 - incompleteBeta(d2 / 2, d1 / 2, beta)
}

/**
 * Incomplete beta function approximation
 */
function incompleteBeta(a: number, b: number, x: number): number {
  // Simplified approximation using continued fraction
  if (x < 0 || x > 1) return 0
  if (x === 0) return 0
  if (x === 1) return 1

  const maxIter = 100
  const eps = 1e-10

  let f = 1
  let c = 1
  let d = 1 - (a + b) * x / (a + 1)
  if (Math.abs(d) < eps) d = eps
  d = 1 / d

  for (let m = 1; m <= maxIter; m++) {
    const m2 = 2 * m
    let aa = m * (b - m) * x / ((a + m2 - 1) * (a + m2))
    d = 1 + aa * d
    if (Math.abs(d) < eps) d = eps
    c = 1 + aa / c
    if (Math.abs(c) < eps) c = eps
    d = 1 / d
    f *= d * c

    aa = -(a + m) * (a + b + m) * x / ((a + m2) * (a + m2 + 1))
    d = 1 + aa * d
    if (Math.abs(d) < eps) d = eps
    c = 1 + aa / c
    if (Math.abs(c) < eps) c = eps
    d = 1 / d
    const delta = d * c
    f *= delta

    if (Math.abs(delta - 1) < eps) break
  }

  const betaFunc = Math.exp(
    lgamma(a) + lgamma(b) - lgamma(a + b) +
    a * Math.log(x) + b * Math.log(1 - x)
  )

  return betaFunc * f / a
}

/**
 * Log gamma function approximation (Lanczos)
 */
function lgamma(x: number): number {
  const c = [
    76.18009172947146, -86.50532032941677, 24.01409824083091,
    -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5
  ]

  let y = x
  let tmp = x + 5.5
  tmp -= (x + 0.5) * Math.log(tmp)
  let sum = 1.000000000190015
  for (let j = 0; j < 6; j++) {
    sum += c[j] / ++y
  }
  return -tmp + Math.log(2.5066282746310005 * sum / x)
}

/**
 * Cohen's d effect size
 */
export function cohensD(group1: number[], group2: number[]): { d: number; ci: [number, number] } {
  const n1 = group1.length
  const n2 = group2.length

  const mean1 = group1.reduce((a, b) => a + b, 0) / n1
  const mean2 = group2.reduce((a, b) => a + b, 0) / n2

  const var1 = group1.reduce((sum, x) => sum + Math.pow(x - mean1, 2), 0) / (n1 - 1)
  const var2 = group2.reduce((sum, x) => sum + Math.pow(x - mean2, 2), 0) / (n2 - 1)

  // Pooled standard deviation
  const pooledSD = Math.sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2))

  const d = pooledSD > 0 ? (mean1 - mean2) / pooledSD : 0

  // 95% CI approximation
  const se = Math.sqrt((n1 + n2) / (n1 * n2) + (d * d) / (2 * (n1 + n2)))
  const ci: [number, number] = [d - 1.96 * se, d + 1.96 * se]

  return { d, ci }
}

/**
 * Multiple comparison correction (FDR - Benjamini-Hochberg)
 */
export function fdrCorrection(pValues: number[], alpha: number = 0.05): { corrected: number[]; significant: boolean[] } {
  const n = pValues.length
  const indexed = pValues.map((p, i) => ({ p, i }))
  indexed.sort((a, b) => a.p - b.p)

  const corrected: number[] = new Array(n)
  const significant: boolean[] = new Array(n)

  let maxP = 0
  for (let i = n - 1; i >= 0; i--) {
    const rank = i + 1
    const adjusted = Math.min(indexed[i].p * n / rank, 1)
    maxP = Math.max(adjusted, maxP)
    corrected[indexed[i].i] = maxP
    significant[indexed[i].i] = maxP < alpha
  }

  return { corrected, significant }
}

/**
 * Cluster-based permutation test (simplified)
 */
export function clusterPermutationTest(
  group1: number[][],
  group2: number[][],
  nPermutations: number = 1000,
  _clusterAlpha: number = 0.05
): { clusters: Array<{ start: number; end: number; stat: number; p: number }> } {
  const nSamples = group1[0]?.length ?? 0
  if (nSamples === 0) return { clusters: [] }

  // Compute observed t-statistics
  const observedT = pointwiseTTest(group1, group2).t

  // Find clusters in observed data
  const tThreshold = 2.0 // Approximate t-threshold
  const observedClusters = findClusters(observedT, tThreshold)

  // Permutation test
  const allData = [...group1, ...group2]
  const n1 = group1.length
  const permutationMaxStats: number[] = []

  for (let perm = 0; perm < nPermutations; perm++) {
    // Shuffle labels
    const shuffled = [...allData].sort(() => Math.random() - 0.5)
    const permGroup1 = shuffled.slice(0, n1)
    const permGroup2 = shuffled.slice(n1)

    const permT = pointwiseTTest(permGroup1, permGroup2).t
    const permClusters = findClusters(permT, tThreshold)

    const maxStat = permClusters.length > 0
      ? Math.max(...permClusters.map(c => Math.abs(c.stat)))
      : 0
    permutationMaxStats.push(maxStat)
  }

  // Compute p-values for observed clusters
  const clusters = observedClusters.map(cluster => {
    const pValue = permutationMaxStats.filter(s => s >= Math.abs(cluster.stat)).length / nPermutations
    return { ...cluster, p: pValue }
  })

  return { clusters }
}

/**
 * Find clusters of significant values
 */
function findClusters(
  values: number[],
  threshold: number
): Array<{ start: number; end: number; stat: number }> {
  const clusters: Array<{ start: number; end: number; stat: number }> = []
  let inCluster = false
  let clusterStart = 0
  let clusterSum = 0

  for (let i = 0; i < values.length; i++) {
    if (Math.abs(values[i]) > threshold) {
      if (!inCluster) {
        inCluster = true
        clusterStart = i
        clusterSum = 0
      }
      clusterSum += values[i]
    } else if (inCluster) {
      clusters.push({ start: clusterStart, end: i - 1, stat: clusterSum })
      inCluster = false
    }
  }

  if (inCluster) {
    clusters.push({ start: clusterStart, end: values.length - 1, stat: clusterSum })
  }

  return clusters
}

// ============ Dynamic Time Warping ============

/**
 * DTW distance between two signals
 */
export function dtwDistance(
  signal1: number[],
  signal2: number[]
): { distance: number; path: Array<[number, number]> } {
  const n = signal1.length
  const m = signal2.length

  // Cost matrix
  const dtw: number[][] = Array.from({ length: n + 1 }, () =>
    new Array(m + 1).fill(Infinity)
  )
  dtw[0][0] = 0

  // Fill cost matrix
  for (let i = 1; i <= n; i++) {
    for (let j = 1; j <= m; j++) {
      const cost = Math.abs(signal1[i - 1] - signal2[j - 1])
      dtw[i][j] = cost + Math.min(dtw[i - 1][j], dtw[i][j - 1], dtw[i - 1][j - 1])
    }
  }

  // Backtrack to find optimal path
  const path: Array<[number, number]> = []
  let i = n, j = m
  while (i > 0 && j > 0) {
    path.unshift([i - 1, j - 1])
    const options = [
      { cost: dtw[i - 1][j - 1], di: -1, dj: -1 },
      { cost: dtw[i - 1][j], di: -1, dj: 0 },
      { cost: dtw[i][j - 1], di: 0, dj: -1 }
    ]
    const best = options.reduce((a, b) => a.cost < b.cost ? a : b)
    i += best.di
    j += best.dj
  }

  return { distance: dtw[n][m], path }
}

/**
 * Warp signal1 to match signal2 using DTW path
 */
export function dtwWarp(signal: number[], targetLength: number, path: Array<[number, number]>): number[] {
  const warped = new Array(targetLength).fill(0)
  const counts = new Array(targetLength).fill(0)

  for (const [i, j] of path) {
    if (j < targetLength && i < signal.length) {
      warped[j] += signal[i]
      counts[j]++
    }
  }

  return warped.map((v, i) => counts[i] > 0 ? v / counts[i] : 0)
}

// ============ Dimensionality Reduction ============

/**
 * Principal Component Analysis
 */
export function computePCA(
  data: number[][],
  nComponents: number = 2
): { scores: number[][]; loadings: number[][]; variance: number[]; explained: number[] } {
  const n = data.length
  const m = data[0]?.length ?? 0
  if (n === 0 || m === 0) {
    return { scores: [], loadings: [], variance: [], explained: [] }
  }

  // Center data
  const means: number[] = new Array(m).fill(0)
  for (let j = 0; j < m; j++) {
    for (let i = 0; i < n; i++) {
      means[j] += data[i][j]
    }
    means[j] /= n
  }

  const centered: number[][] = data.map(row =>
    row.map((val, j) => val - means[j])
  )

  // Compute covariance matrix
  const cov: number[][] = []
  for (let i = 0; i < m; i++) {
    const row: number[] = []
    for (let j = 0; j < m; j++) {
      let sum = 0
      for (let k = 0; k < n; k++) {
        sum += centered[k][i] * centered[k][j]
      }
      row.push(sum / (n - 1))
    }
    cov.push(row)
  }

  // Power iteration to find principal components
  const loadings: number[][] = []
  const variance: number[] = []
  const covCopy = cov.map(row => [...row])

  for (let comp = 0; comp < Math.min(nComponents, m); comp++) {
    // Initialize random vector
    let v: number[] = Array.from({ length: m }, () => Math.random() - 0.5)
    let eigenvalue = 0

    // Power iteration
    for (let iter = 0; iter < 100; iter++) {
      // Multiply by covariance matrix
      const newV: number[] = new Array(m).fill(0)
      for (let i = 0; i < m; i++) {
        for (let j = 0; j < m; j++) {
          newV[i] += covCopy[i][j] * v[j]
        }
      }

      // Compute norm
      const norm = Math.sqrt(newV.reduce((sum, x) => sum + x * x, 0))
      if (norm < 1e-10) break

      eigenvalue = norm
      v = newV.map(x => x / norm)
    }

    loadings.push(v)
    variance.push(eigenvalue)

    // Deflate covariance matrix
    for (let i = 0; i < m; i++) {
      for (let j = 0; j < m; j++) {
        covCopy[i][j] -= eigenvalue * v[i] * v[j]
      }
    }
  }

  // Compute scores
  const scores: number[][] = centered.map(row =>
    loadings.map(loading =>
      row.reduce((sum, val, j) => sum + val * loading[j], 0)
    )
  )

  // Compute explained variance ratio
  const totalVariance = variance.reduce((a, b) => a + Math.abs(b), 0)
  const explained = variance.map(v => totalVariance > 0 ? Math.abs(v) / totalVariance : 0)

  return { scores, loadings, variance, explained }
}

// ============ Movement Analysis Functions ============

/**
 * Normalize movement to percentage of cycle
 */
export function normalizeToPercent(
  signal: number[],
  targetLength: number = 101
): number[] {
  const n = signal.length
  const normalized: number[] = []

  for (let i = 0; i < targetLength; i++) {
    const pos = (i / (targetLength - 1)) * (n - 1)
    const idx = Math.floor(pos)
    const frac = pos - idx

    if (idx >= n - 1) {
      normalized.push(signal[n - 1])
    } else {
      normalized.push(signal[idx] * (1 - frac) + signal[idx + 1] * frac)
    }
  }

  return normalized
}

/**
 * Detect events using threshold crossing
 */
export function detectThresholdEvents(
  signal: number[],
  threshold: number,
  direction: 'rising' | 'falling' | 'both' = 'both',
  minDistance: number = 1
): number[] {
  const events: number[] = []

  for (let i = 1; i < signal.length; i++) {
    const crossed = direction === 'both'
      ? (signal[i - 1] < threshold && signal[i] >= threshold) ||
        (signal[i - 1] > threshold && signal[i] <= threshold)
      : direction === 'rising'
        ? signal[i - 1] < threshold && signal[i] >= threshold
        : signal[i - 1] > threshold && signal[i] <= threshold

    if (crossed && (events.length === 0 || i - events[events.length - 1] >= minDistance)) {
      events.push(i)
    }
  }

  return events
}

/**
 * Segment signal into phases based on events
 */
export function segmentPhases(
  signal: number[],
  eventIndices: number[]
): number[][] {
  const phases: number[][] = []

  for (let i = 0; i < eventIndices.length - 1; i++) {
    const start = eventIndices[i]
    const end = eventIndices[i + 1]
    phases.push(signal.slice(start, end + 1))
  }

  return phases
}

// ============ EMG-Specific Analysis Functions ============

/**
 * Full-wave rectification (absolute value)
 */
export function rectifySignal(signal: number[]): number[] {
  return signal.map(x => Math.abs(x))
}

/**
 * EMG envelope detection
 */
export function emgEnvelope(
  signal: number[],
  samplingRate: number,
  cutoffFreq: number = 6,
  method: 'lowpass' | 'rms' | 'hilbert' = 'lowpass'
): number[] {
  // First rectify the signal
  const rectified = rectifySignal(signal)

  switch (method) {
    case 'lowpass':
      // Low-pass filter the rectified signal
      return lowpassFilter(rectified, samplingRate, cutoffFreq)

    case 'rms':
      // Moving RMS
      const windowSize = Math.floor(samplingRate / cutoffFreq)
      return movingRMS(rectified, windowSize)

    case 'hilbert':
      // Hilbert transform envelope
      return hilbertTransform(signal).amplitude

    default:
      return rectified
  }
}

/**
 * Low-pass filter using windowed sinc
 */
export function lowpassFilter(
  signal: number[],
  samplingRate: number,
  cutoffFreq: number,
  order: number = 101
): number[] {
  const kernel: number[] = []
  const nyquist = samplingRate / 2
  const normFreq = cutoffFreq / nyquist
  const halfOrder = Math.floor(order / 2)

  for (let i = 0; i < order; i++) {
    const n = i - halfOrder
    let h: number

    if (n === 0) {
      h = 2 * normFreq
    } else {
      h = Math.sin(Math.PI * n * normFreq) / (Math.PI * n)
    }

    // Apply Hamming window
    const window = 0.54 - 0.46 * Math.cos((2 * Math.PI * i) / (order - 1))
    kernel.push(h * window)
  }

  // Normalize
  const sum = kernel.reduce((a, b) => a + b, 0)
  const normalizedKernel = kernel.map(k => k / sum)

  // Convolve
  const result: number[] = []
  const halfKernel = Math.floor(normalizedKernel.length / 2)

  for (let i = 0; i < signal.length; i++) {
    let s = 0
    for (let j = 0; j < normalizedKernel.length; j++) {
      const idx = i - halfKernel + j
      if (idx >= 0 && idx < signal.length) {
        s += signal[idx] * normalizedKernel[j]
      }
    }
    result.push(s)
  }

  return result
}

/**
 * Moving RMS calculation
 */
export function movingRMS(signal: number[], windowSize: number): number[] {
  const result: number[] = []
  const halfWindow = Math.floor(windowSize / 2)

  for (let i = 0; i < signal.length; i++) {
    let sumSq = 0
    let count = 0

    for (let j = Math.max(0, i - halfWindow); j < Math.min(signal.length, i + halfWindow + 1); j++) {
      sumSq += signal[j] * signal[j]
      count++
    }

    result.push(Math.sqrt(sumSq / count))
  }

  return result
}

/**
 * Mean Absolute Value (MAV) calculation
 */
export function computeMAV(signal: number[], windowSize: number): number[] {
  const result: number[] = []
  const halfWindow = Math.floor(windowSize / 2)

  for (let i = 0; i < signal.length; i++) {
    let sum = 0
    let count = 0

    for (let j = Math.max(0, i - halfWindow); j < Math.min(signal.length, i + halfWindow + 1); j++) {
      sum += Math.abs(signal[j])
      count++
    }

    result.push(sum / count)
  }

  return result
}

/**
 * MVC Normalization
 */
export function mvcNormalize(
  signal: number[],
  mvcValue: number
): number[] {
  return signal.map(x => (x / mvcValue) * 100)
}

/**
 * Find MVC value from a signal
 */
export function findMVCValue(
  signal: number[],
  samplingRate: number,
  windowMs: number = 500,
  method: 'peak' | 'mean' | 'median' = 'peak'
): number {
  const windowSamples = Math.floor(windowMs * samplingRate / 1000)
  const envelope = emgEnvelope(signal, samplingRate, 6, 'lowpass')

  if (method === 'peak') {
    // Find the maximum of the moving window average
    let maxVal = 0
    for (let i = 0; i <= envelope.length - windowSamples; i++) {
      const windowMean = envelope.slice(i, i + windowSamples).reduce((a, b) => a + b, 0) / windowSamples
      if (windowMean > maxVal) maxVal = windowMean
    }
    return maxVal
  } else if (method === 'mean') {
    // Return overall mean
    return envelope.reduce((a, b) => a + b, 0) / envelope.length
  } else {
    // Return median
    const sorted = [...envelope].sort((a, b) => a - b)
    return sorted[Math.floor(sorted.length / 2)]
  }
}

/**
 * EMG Onset Detection
 */
export function detectEMGOnset(
  signal: number[],
  samplingRate: number,
  method: 'single_threshold' | 'double_threshold' | 'adaptive' | 'tkeo' = 'double_threshold',
  thresholdSD: number = 3,
  minDurationMs: number = 50
): Array<{ onset: number; offset: number }> {
  const envelope = emgEnvelope(signal, samplingRate, 10, 'lowpass')
  const minDurationSamples = Math.floor(minDurationMs * samplingRate / 1000)

  // Calculate baseline statistics (first 10% of signal assumed to be rest)
  const baselineLength = Math.floor(signal.length * 0.1)
  const baseline = envelope.slice(0, Math.max(baselineLength, 100))
  const baselineMean = baseline.reduce((a, b) => a + b, 0) / baseline.length
  const baselineStd = Math.sqrt(
    baseline.reduce((sum, x) => sum + Math.pow(x - baselineMean, 2), 0) / baseline.length
  )

  const threshold = baselineMean + thresholdSD * baselineStd

  let processedSignal = envelope

  if (method === 'tkeo') {
    // Apply Teager-Kaiser Energy Operator
    processedSignal = tkeo(signal)
    const tkeoMean = processedSignal.slice(0, baselineLength).reduce((a, b) => a + b, 0) / baselineLength
    const tkeoStd = Math.sqrt(
      processedSignal.slice(0, baselineLength).reduce((sum, x) => sum + Math.pow(x - tkeoMean, 2), 0) / baselineLength
    )
    const tkeoThreshold = tkeoMean + thresholdSD * tkeoStd

    return detectThresholdCrossings(processedSignal, tkeoThreshold, minDurationSamples)
  }

  if (method === 'double_threshold') {
    // Double threshold: higher threshold for onset, lower for offset
    const onsetThreshold = threshold
    const offsetThreshold = baselineMean + (thresholdSD * 0.5) * baselineStd

    return detectDoubleThresholdCrossings(processedSignal, onsetThreshold, offsetThreshold, minDurationSamples)
  }

  // Single threshold or adaptive
  return detectThresholdCrossings(processedSignal, threshold, minDurationSamples)
}

/**
 * Teager-Kaiser Energy Operator
 */
export function tkeo(signal: number[]): number[] {
  const result: number[] = [0]

  for (let i = 1; i < signal.length - 1; i++) {
    result.push(signal[i] * signal[i] - signal[i - 1] * signal[i + 1])
  }

  result.push(0)
  return result
}

/**
 * Detect threshold crossings
 */
function detectThresholdCrossings(
  signal: number[],
  threshold: number,
  minDuration: number
): Array<{ onset: number; offset: number }> {
  const events: Array<{ onset: number; offset: number }> = []
  let inBurst = false
  let onsetIdx = 0

  for (let i = 0; i < signal.length; i++) {
    if (!inBurst && signal[i] > threshold) {
      inBurst = true
      onsetIdx = i
    } else if (inBurst && signal[i] <= threshold) {
      if (i - onsetIdx >= minDuration) {
        events.push({ onset: onsetIdx, offset: i })
      }
      inBurst = false
    }
  }

  // Handle case where signal ends during burst
  if (inBurst && signal.length - onsetIdx >= minDuration) {
    events.push({ onset: onsetIdx, offset: signal.length - 1 })
  }

  return events
}

/**
 * Detect double threshold crossings
 */
function detectDoubleThresholdCrossings(
  signal: number[],
  onsetThreshold: number,
  offsetThreshold: number,
  minDuration: number
): Array<{ onset: number; offset: number }> {
  const events: Array<{ onset: number; offset: number }> = []
  let inBurst = false
  let onsetIdx = 0

  for (let i = 0; i < signal.length; i++) {
    if (!inBurst && signal[i] > onsetThreshold) {
      inBurst = true
      onsetIdx = i
    } else if (inBurst && signal[i] <= offsetThreshold) {
      if (i - onsetIdx >= minDuration) {
        events.push({ onset: onsetIdx, offset: i })
      }
      inBurst = false
    }
  }

  if (inBurst && signal.length - onsetIdx >= minDuration) {
    events.push({ onset: onsetIdx, offset: signal.length - 1 })
  }

  return events
}

/**
 * Co-contraction Index calculation
 */
export function coContractionIndex(
  agonist: number[],
  antagonist: number[],
  method: 'ratio' | 'overlap' | 'wasted' = 'ratio'
): number[] {
  const n = Math.min(agonist.length, antagonist.length)
  const result: number[] = []

  for (let i = 0; i < n; i++) {
    const ag = Math.abs(agonist[i])
    const ant = Math.abs(antagonist[i])

    switch (method) {
      case 'ratio':
        // Antagonist/agonist ratio
        result.push(ag > 0 ? ant / ag : 0)
        break

      case 'overlap':
        // Overlap method: 2 * min(ag, ant) / (ag + ant)
        const sum = ag + ant
        result.push(sum > 0 ? (2 * Math.min(ag, ant)) / sum : 0)
        break

      case 'wasted':
        // Wasted contraction: min(ag, ant)
        result.push(Math.min(ag, ant))
        break
    }
  }

  return result
}

/**
 * Calculate mean co-contraction index over a window
 */
export function meanCoContractionIndex(
  agonist: number[],
  antagonist: number[],
  method: 'ratio' | 'overlap' | 'wasted' = 'overlap'
): number {
  const cci = coContractionIndex(agonist, antagonist, method)
  return cci.reduce((a, b) => a + b, 0) / cci.length
}

/**
 * Median Frequency calculation for fatigue analysis
 */
export function computeMedianFrequency(
  signal: number[],
  samplingRate: number,
  windowSize: number = 512,
  overlap: number = 50
): { times: number[]; mdf: number[] } {
  const hopSize = Math.floor(windowSize * (1 - overlap / 100))
  const nFrames = Math.floor((signal.length - windowSize) / hopSize) + 1

  const times: number[] = []
  const mdf: number[] = []

  for (let frame = 0; frame < nFrames; frame++) {
    const startIdx = frame * hopSize
    times.push((startIdx + windowSize / 2) / samplingRate)

    // Extract frame and compute PSD
    const frameData = signal.slice(startIdx, startIdx + windowSize)
    const { frequencies, power } = computePSD(frameData, samplingRate, windowSize)

    // Find median frequency
    const linearPower = power.map(p => Math.pow(10, p / 10))
    const totalPower = linearPower.reduce((a, b) => a + b, 0)
    const halfPower = totalPower / 2

    let cumSum = 0
    let medianIdx = 0
    for (let i = 0; i < linearPower.length; i++) {
      cumSum += linearPower[i]
      if (cumSum >= halfPower) {
        medianIdx = i
        break
      }
    }

    mdf.push(frequencies[medianIdx] || 0)
  }

  return { times, mdf }
}

/**
 * Mean Frequency calculation for fatigue analysis
 */
export function computeMeanFrequency(
  signal: number[],
  samplingRate: number,
  windowSize: number = 512,
  overlap: number = 50
): { times: number[]; mnf: number[] } {
  const hopSize = Math.floor(windowSize * (1 - overlap / 100))
  const nFrames = Math.floor((signal.length - windowSize) / hopSize) + 1

  const times: number[] = []
  const mnf: number[] = []

  for (let frame = 0; frame < nFrames; frame++) {
    const startIdx = frame * hopSize
    times.push((startIdx + windowSize / 2) / samplingRate)

    // Extract frame and compute PSD
    const frameData = signal.slice(startIdx, startIdx + windowSize)
    const { frequencies, power } = computePSD(frameData, samplingRate, windowSize)

    // Find mean frequency (weighted average)
    const linearPower = power.map(p => Math.pow(10, p / 10))
    const totalPower = linearPower.reduce((a, b) => a + b, 0)

    let weightedSum = 0
    for (let i = 0; i < frequencies.length; i++) {
      weightedSum += frequencies[i] * linearPower[i]
    }

    mnf.push(totalPower > 0 ? weightedSum / totalPower : 0)
  }

  return { times, mnf }
}

/**
 * Fatigue Index calculation
 */
export function computeFatigueIndex(
  mdfOrMnf: number[],
  method: 'slope' | 'ratio' = 'slope'
): { index: number; trend: number[] } {
  const n = mdfOrMnf.length
  if (n < 2) return { index: 0, trend: mdfOrMnf }

  if (method === 'slope') {
    // Linear regression to find slope
    const x = Array.from({ length: n }, (_, i) => i)
    const xMean = (n - 1) / 2
    const yMean = mdfOrMnf.reduce((a, b) => a + b, 0) / n

    let numerator = 0
    let denominator = 0
    for (let i = 0; i < n; i++) {
      numerator += (x[i] - xMean) * (mdfOrMnf[i] - yMean)
      denominator += Math.pow(x[i] - xMean, 2)
    }

    const slope = denominator > 0 ? numerator / denominator : 0
    const intercept = yMean - slope * xMean

    const trend = x.map(xi => intercept + slope * xi)

    // Fatigue index: percent change
    const startVal = mdfOrMnf[0]
    const index = startVal > 0 ? (slope * n / startVal) * 100 : 0

    return { index, trend }
  } else {
    // Ratio method: compare first and last quarter
    const quarterLen = Math.floor(n / 4)
    const firstQuarter = mdfOrMnf.slice(0, quarterLen)
    const lastQuarter = mdfOrMnf.slice(-quarterLen)

    const firstMean = firstQuarter.reduce((a, b) => a + b, 0) / quarterLen
    const lastMean = lastQuarter.reduce((a, b) => a + b, 0) / quarterLen

    const index = firstMean > 0 ? ((firstMean - lastMean) / firstMean) * 100 : 0

    return { index, trend: mdfOrMnf }
  }
}

/**
 * EMG Feature Extraction for pattern recognition
 */
export function extractEMGFeatures(
  signal: number[],
  samplingRate: number,
  windowSizeMs: number = 250
): Record<string, number> {
  const windowSize = Math.floor(windowSizeMs * samplingRate / 1000)
  const features: Record<string, number> = {}

  // Time domain features

  // MAV - Mean Absolute Value
  features.mav = signal.reduce((sum, x) => sum + Math.abs(x), 0) / signal.length

  // RMS - Root Mean Square
  features.rms = Math.sqrt(signal.reduce((sum, x) => sum + x * x, 0) / signal.length)

  // WL - Waveform Length
  let wl = 0
  for (let i = 1; i < signal.length; i++) {
    wl += Math.abs(signal[i] - signal[i - 1])
  }
  features.wl = wl

  // ZC - Zero Crossings
  let zc = 0
  const threshold = 0.01 * features.mav // Small threshold to avoid noise
  for (let i = 1; i < signal.length; i++) {
    if ((signal[i] > 0 && signal[i - 1] < 0) || (signal[i] < 0 && signal[i - 1] > 0)) {
      if (Math.abs(signal[i] - signal[i - 1]) > threshold) {
        zc++
      }
    }
  }
  features.zc = zc

  // SSC - Slope Sign Changes
  let ssc = 0
  for (let i = 1; i < signal.length - 1; i++) {
    const diff1 = signal[i] - signal[i - 1]
    const diff2 = signal[i + 1] - signal[i]
    if ((diff1 > 0 && diff2 < 0) || (diff1 < 0 && diff2 > 0)) {
      if (Math.abs(diff1) > threshold && Math.abs(diff2) > threshold) {
        ssc++
      }
    }
  }
  features.ssc = ssc

  // IEMG - Integrated EMG
  features.iemg = signal.reduce((sum, x) => sum + Math.abs(x), 0)

  // VAR - Variance
  const mean = signal.reduce((a, b) => a + b, 0) / signal.length
  features.var = signal.reduce((sum, x) => sum + Math.pow(x - mean, 2), 0) / (signal.length - 1)

  // Frequency domain features
  const { frequencies, power } = computePSD(signal, samplingRate, Math.min(windowSize, 256))
  const linearPower = power.map(p => Math.pow(10, p / 10))
  const totalPower = linearPower.reduce((a, b) => a + b, 0)

  // MNF - Mean Frequency
  let weightedSum = 0
  for (let i = 0; i < frequencies.length; i++) {
    weightedSum += frequencies[i] * linearPower[i]
  }
  features.mnf = totalPower > 0 ? weightedSum / totalPower : 0

  // MDF - Median Frequency
  let cumSum = 0
  const halfPower = totalPower / 2
  for (let i = 0; i < linearPower.length; i++) {
    cumSum += linearPower[i]
    if (cumSum >= halfPower) {
      features.mdf = frequencies[i]
      break
    }
  }
  features.mdf = features.mdf || 0

  // PKF - Peak Frequency
  let maxPower = 0
  let pkfIdx = 0
  for (let i = 0; i < linearPower.length; i++) {
    if (linearPower[i] > maxPower) {
      maxPower = linearPower[i]
      pkfIdx = i
    }
  }
  features.pkf = frequencies[pkfIdx] || 0

  return features
}

/**
 * Non-negative Matrix Factorization (NMF) for muscle synergy analysis
 * Simplified implementation using multiplicative update rules
 */
export function nmfDecomposition(
  data: number[][],  // muscles x time points
  nSynergies: number,
  maxIterations: number = 1000,
  _tolerance: number = 1e-4
): { W: number[][]; H: number[][]; vaf: number } {
  const nMuscles = data.length
  const nTimePoints = data[0]?.length || 0

  if (nMuscles === 0 || nTimePoints === 0) {
    return { W: [], H: [], vaf: 0 }
  }

  // Initialize W and H with random positive values
  let W: number[][] = Array.from({ length: nMuscles }, () =>
    Array.from({ length: nSynergies }, () => Math.random() * 0.1 + 0.01)
  )
  let H: number[][] = Array.from({ length: nSynergies }, () =>
    Array.from({ length: nTimePoints }, () => Math.random() * 0.1 + 0.01)
  )

  // Multiplicative update rules
  for (let iter = 0; iter < maxIterations; iter++) {
    // Update H: H = H .* (W'V) ./ (W'WH)
    const WtV = matMul(transpose(W), data)
    const WtW = matMul(transpose(W), W)
    const WtWH = matMul(WtW, H)

    for (let i = 0; i < nSynergies; i++) {
      for (let j = 0; j < nTimePoints; j++) {
        H[i][j] *= WtWH[i][j] > 1e-10 ? WtV[i][j] / WtWH[i][j] : 0
        H[i][j] = Math.max(H[i][j], 1e-10) // Prevent zero
      }
    }

    // Update W: W = W .* (VH') ./ (WHH')
    const VHt = matMul(data, transpose(H))
    const HHt = matMul(H, transpose(H))
    const WHHt = matMul(W, HHt)

    for (let i = 0; i < nMuscles; i++) {
      for (let j = 0; j < nSynergies; j++) {
        W[i][j] *= WHHt[i][j] > 1e-10 ? VHt[i][j] / WHHt[i][j] : 0
        W[i][j] = Math.max(W[i][j], 1e-10)
      }
    }

    // Normalize W columns and scale H accordingly
    for (let j = 0; j < nSynergies; j++) {
      let norm = 0
      for (let i = 0; i < nMuscles; i++) {
        norm += W[i][j] * W[i][j]
      }
      norm = Math.sqrt(norm)
      if (norm > 1e-10) {
        for (let i = 0; i < nMuscles; i++) {
          W[i][j] /= norm
        }
        for (let t = 0; t < nTimePoints; t++) {
          H[j][t] *= norm
        }
      }
    }
  }

  // Calculate VAF (Variance Accounted For)
  const WH = matMul(W, H)
  let ssResid = 0
  let ssTotal = 0
  const grandMean = data.flat().reduce((a, b) => a + b, 0) / (nMuscles * nTimePoints)

  for (let i = 0; i < nMuscles; i++) {
    for (let j = 0; j < nTimePoints; j++) {
      ssResid += Math.pow(data[i][j] - WH[i][j], 2)
      ssTotal += Math.pow(data[i][j] - grandMean, 2)
    }
  }

  const vaf = ssTotal > 0 ? (1 - ssResid / ssTotal) * 100 : 0

  return { W, H, vaf }
}

/**
 * Matrix multiplication helper
 */
function matMul(A: number[][], B: number[][]): number[][] {
  const m = A.length
  const n = B[0]?.length || 0
  const k = A[0]?.length || 0

  const result: number[][] = Array.from({ length: m }, () => new Array(n).fill(0))

  for (let i = 0; i < m; i++) {
    for (let j = 0; j < n; j++) {
      for (let p = 0; p < k; p++) {
        result[i][j] += A[i][p] * B[p][j]
      }
    }
  }

  return result
}

/**
 * Matrix transpose helper
 */
function transpose(A: number[][]): number[][] {
  const m = A.length
  const n = A[0]?.length || 0

  const result: number[][] = Array.from({ length: n }, () => new Array(m).fill(0))

  for (let i = 0; i < m; i++) {
    for (let j = 0; j < n; j++) {
      result[j][i] = A[i][j]
    }
  }

  return result
}

/**
 * Find optimal number of synergies using VAF threshold
 */
export function findOptimalSynergies(
  data: number[][],
  vafThreshold: number = 90,
  maxSynergies: number = 8
): { optimalN: number; vafCurve: number[] } {
  const vafCurve: number[] = []

  for (let n = 1; n <= maxSynergies; n++) {
    const { vaf } = nmfDecomposition(data, n, 500)
    vafCurve.push(vaf)

    if (vaf >= vafThreshold) {
      return { optimalN: n, vafCurve }
    }
  }

  return { optimalN: maxSynergies, vafCurve }
}

/**
 * Calculate synergy similarity (correlation between synergy vectors)
 */
export function synergySimilarity(
  W1: number[][],
  W2: number[][],
  metric: 'correlation' | 'cosine' | 'dot' = 'correlation'
): number[][] {
  const n1 = W1[0]?.length || 0  // number of synergies in W1
  const n2 = W2[0]?.length || 0  // number of synergies in W2

  const similarity: number[][] = []

  for (let i = 0; i < n1; i++) {
    const row: number[] = []
    const vec1 = W1.map(w => w[i])

    for (let j = 0; j < n2; j++) {
      const vec2 = W2.map(w => w[j])

      switch (metric) {
        case 'correlation':
          row.push(pearsonCorrelation(vec1, vec2))
          break

        case 'cosine':
          const dot = vec1.reduce((sum, v, k) => sum + v * vec2[k], 0)
          const norm1 = Math.sqrt(vec1.reduce((sum, v) => sum + v * v, 0))
          const norm2 = Math.sqrt(vec2.reduce((sum, v) => sum + v * v, 0))
          row.push(norm1 > 0 && norm2 > 0 ? dot / (norm1 * norm2) : 0)
          break

        case 'dot':
          row.push(vec1.reduce((sum, v, k) => sum + v * vec2[k], 0))
          break
      }
    }
    similarity.push(row)
  }

  return similarity
}

/**
 * PCA-based muscle synergy extraction
 * Uses eigenvalue decomposition of covariance matrix
 */
export function pcaSynergyExtraction(
  data: number[][],  // muscles x time points
  nSynergies: number
): { W: number[][]; H: number[][]; vaf: number; eigenvalues: number[] } {
  const nMuscles = data.length
  const nTimePoints = data[0]?.length || 0

  if (nMuscles === 0 || nTimePoints === 0) {
    return { W: [], H: [], vaf: 0, eigenvalues: [] }
  }

  // Downsample if too many time points (for performance)
  const maxPoints = 2000
  const downsampleFactor = Math.max(1, Math.floor(nTimePoints / maxPoints))
  const sampledData: number[][] = data.map(row => {
    if (downsampleFactor === 1) return row
    const sampled: number[] = []
    for (let i = 0; i < row.length; i += downsampleFactor) {
      sampled.push(row[i])
    }
    return sampled
  })
  const nSampledPoints = sampledData[0].length

  // Center the data (subtract mean for each muscle)
  const centeredData: number[][] = []
  for (let m = 0; m < nMuscles; m++) {
    const mean = sampledData[m].reduce((a, b) => a + b, 0) / nSampledPoints
    centeredData.push(sampledData[m].map(v => v - mean))
  }

  // Compute covariance matrix (muscles x muscles)
  const covMatrix: number[][] = Array.from({ length: nMuscles }, () =>
    new Array(nMuscles).fill(0)
  )
  for (let i = 0; i < nMuscles; i++) {
    for (let j = 0; j <= i; j++) {
      let cov = 0
      for (let t = 0; t < nSampledPoints; t++) {
        cov += centeredData[i][t] * centeredData[j][t]
      }
      cov /= (nSampledPoints - 1)
      covMatrix[i][j] = cov
      covMatrix[j][i] = cov
    }
  }

  // Power iteration method for eigenvalue decomposition
  const { eigenvectors, eigenvalues } = powerIterationEigen(covMatrix, Math.min(nSynergies, nMuscles))

  // Limit synergies to number of muscles
  const actualSynergies = Math.min(nSynergies, nMuscles)

  // W = eigenvectors (muscles x actualSynergies)
  const W: number[][] = eigenvectors

  // H = W' * centered_data (project data onto principal components)
  const H: number[][] = Array.from({ length: actualSynergies }, () =>
    new Array(nSampledPoints).fill(0)
  )
  for (let s = 0; s < actualSynergies; s++) {
    for (let t = 0; t < nSampledPoints; t++) {
      for (let m = 0; m < nMuscles; m++) {
        H[s][t] += W[m][s] * centeredData[m][t]
      }
    }
  }

  // Make W non-negative by taking absolute values (common practice for synergy interpretation)
  const absW: number[][] = W.map(row => row.map(v => Math.abs(v)))

  // Normalize W columns
  for (let s = 0; s < actualSynergies; s++) {
    let norm = 0
    for (let m = 0; m < nMuscles; m++) {
      norm += absW[m][s] * absW[m][s]
    }
    norm = Math.sqrt(norm)
    if (norm > 1e-10) {
      for (let m = 0; m < nMuscles; m++) {
        absW[m][s] /= norm
      }
    }
  }

  // Make H non-negative by rectifying
  const absH: number[][] = H.map(row => row.map(v => Math.max(0, v)))

  // Calculate VAF
  const totalVariance = eigenvalues.reduce((a, b) => a + b, 0)
  const explainedVariance = eigenvalues.slice(0, nSynergies).reduce((a, b) => a + b, 0)
  const vaf = totalVariance > 0 ? (explainedVariance / totalVariance) * 100 : 0

  return { W: absW, H: absH, vaf, eigenvalues }
}

/**
 * Power iteration method for computing top k eigenvectors
 */
function powerIterationEigen(
  matrix: number[][],
  k: number,
  maxIter: number = 100
): { eigenvectors: number[][]; eigenvalues: number[] } {
  const n = matrix.length
  const eigenvectors: number[][] = Array.from({ length: n }, () => new Array(k).fill(0))
  const eigenvalues: number[] = []

  // Working copy of matrix for deflation
  let A = matrix.map(row => [...row])

  for (let i = 0; i < k; i++) {
    // Initialize random vector
    let v = Array.from({ length: n }, () => Math.random() - 0.5)
    let vNorm = Math.sqrt(v.reduce((sum, x) => sum + x * x, 0))
    v = v.map(x => x / vNorm)

    // Power iteration
    for (let iter = 0; iter < maxIter; iter++) {
      // Av
      const Av = A.map(row => row.reduce((sum, val, j) => sum + val * v[j], 0))

      // Normalize
      vNorm = Math.sqrt(Av.reduce((sum, x) => sum + x * x, 0))
      if (vNorm < 1e-10) break
      v = Av.map(x => x / vNorm)
    }

    // Eigenvalue = v' * A * v
    const Av = A.map(row => row.reduce((sum, val, j) => sum + val * v[j], 0))
    const eigenvalue = v.reduce((sum, vi, j) => sum + vi * Av[j], 0)
    eigenvalues.push(eigenvalue)

    // Store eigenvector
    for (let j = 0; j < n; j++) {
      eigenvectors[j][i] = v[j]
    }

    // Deflate matrix: A = A - eigenvalue * v * v'
    for (let p = 0; p < n; p++) {
      for (let q = 0; q < n; q++) {
        A[p][q] -= eigenvalue * v[p] * v[q]
      }
    }
  }

  return { eigenvectors, eigenvalues }
}

/**
 * Factor Analysis (FA) based muscle synergy extraction
 * Uses iterative principal axis factoring with varimax rotation
 */
export function factorAnalysisSynergy(
  data: number[][],  // muscles x time points
  nFactors: number,
  maxIterations: number = 20  // Reduced for performance
): { W: number[][]; H: number[][]; vaf: number; communalities: number[] } {
  const nMuscles = data.length
  const nTimePoints = data[0]?.length || 0

  if (nMuscles === 0 || nTimePoints === 0) {
    return { W: [], H: [], vaf: 0, communalities: [] }
  }

  // Downsample if too many time points (for performance)
  const maxPoints = 2000
  const downsampleFactor = Math.max(1, Math.floor(nTimePoints / maxPoints))
  const sampledData: number[][] = data.map(row => {
    if (downsampleFactor === 1) return row
    const sampled: number[] = []
    for (let i = 0; i < row.length; i += downsampleFactor) {
      sampled.push(row[i])
    }
    return sampled
  })
  const nSampledPoints = sampledData[0].length

  // Center and standardize the data
  const standardizedData: number[][] = []
  for (let m = 0; m < nMuscles; m++) {
    const mean = sampledData[m].reduce((a, b) => a + b, 0) / nSampledPoints
    const std = Math.sqrt(
      sampledData[m].reduce((sum, v) => sum + (v - mean) ** 2, 0) / (nSampledPoints - 1)
    ) || 1
    standardizedData.push(sampledData[m].map(v => (v - mean) / std))
  }

  // Compute correlation matrix
  const corrMatrix: number[][] = Array.from({ length: nMuscles }, () =>
    new Array(nMuscles).fill(0)
  )
  for (let i = 0; i < nMuscles; i++) {
    for (let j = 0; j <= i; j++) {
      let corr = 0
      for (let t = 0; t < nSampledPoints; t++) {
        corr += standardizedData[i][t] * standardizedData[j][t]
      }
      corr /= (nSampledPoints - 1)
      corrMatrix[i][j] = corr
      corrMatrix[j][i] = corr
    }
  }

  // Initialize communalities (use squared multiple correlations as initial estimates)
  let communalities = new Array(nMuscles).fill(0.5)

  // Limit factors to number of muscles
  const actualFactors = Math.min(nFactors, nMuscles)

  // Iterative principal axis factoring
  let loadings: number[][] = []

  for (let iter = 0; iter < maxIterations; iter++) {
    // Create reduced correlation matrix (correlation - uniqueness on diagonal)
    const reducedCorr = corrMatrix.map((row, i) =>
      row.map((val, j) => i === j ? communalities[i] : val)
    )

    // Extract factors using eigendecomposition
    const { eigenvectors, eigenvalues } = powerIterationEigen(reducedCorr, actualFactors)

    // Compute loadings: L = V * sqrt(eigenvalues)
    loadings = eigenvectors.map(row =>
      row.map((v, j) => v * Math.sqrt(Math.max(0, eigenvalues[j] || 0)))
    )

    // Update communalities
    const newCommunalities = loadings.map(row =>
      row.reduce((sum, l) => sum + l * l, 0)
    )

    // Check convergence
    const maxDiff = Math.max(...communalities.map((c, i) => Math.abs(c - newCommunalities[i])))
    communalities = newCommunalities.map(c => Math.min(c, 0.999)) // Cap at 0.999

    if (maxDiff < 1e-3) break  // Relaxed convergence
  }

  // Apply varimax rotation
  const rotatedLoadings = varimaxRotation(loadings, actualFactors)

  // W = rotated loadings (make non-negative)
  const W: number[][] = rotatedLoadings.map(row => row.map(v => Math.abs(v)))

  // Normalize W columns
  for (let f = 0; f < actualFactors; f++) {
    let norm = 0
    for (let m = 0; m < nMuscles; m++) {
      norm += W[m][f] * W[m][f]
    }
    norm = Math.sqrt(norm)
    if (norm > 1e-10) {
      for (let m = 0; m < nMuscles; m++) {
        W[m][f] /= norm
      }
    }
  }

  // H = factor scores (W' * sampled data, rectified)
  const H: number[][] = Array.from({ length: actualFactors }, () =>
    new Array(nSampledPoints).fill(0)
  )
  for (let f = 0; f < actualFactors; f++) {
    for (let t = 0; t < nSampledPoints; t++) {
      for (let m = 0; m < nMuscles; m++) {
        H[f][t] += W[m][f] * standardizedData[m][t]
      }
      H[f][t] = Math.max(0, H[f][t]) // Rectify
    }
  }

  // Calculate VAF from communalities
  const totalVariance = nMuscles // For standardized data
  const explainedVariance = communalities.reduce((a, b) => a + b, 0)
  const vaf = (explainedVariance / totalVariance) * 100

  return { W, H, vaf, communalities }
}

/**
 * Varimax rotation for factor loadings
 */
function varimaxRotation(
  loadings: number[][],
  nFactors: number,
  maxIter: number = 50
): number[][] {
  const nMuscles = loadings.length
  let rotated = loadings.map(row => [...row])

  for (let iter = 0; iter < maxIter; iter++) {
    let converged = true

    // Rotate pairs of factors
    for (let i = 0; i < nFactors - 1; i++) {
      for (let j = i + 1; j < nFactors; j++) {
        // Calculate rotation angle
        let u = 0, v = 0
        for (let m = 0; m < nMuscles; m++) {
          const li = rotated[m][i]
          const lj = rotated[m][j]
          const a = li * li - lj * lj
          const b = 2 * li * lj
          u += a
          v += b
        }

        // Rotation angle
        const phi = 0.25 * Math.atan2(v, u)
        if (Math.abs(phi) > 1e-6) {
          converged = false
          const cos = Math.cos(phi)
          const sin = Math.sin(phi)

          // Apply rotation
          for (let m = 0; m < nMuscles; m++) {
            const li = rotated[m][i]
            const lj = rotated[m][j]
            rotated[m][i] = li * cos + lj * sin
            rotated[m][j] = -li * sin + lj * cos
          }
        }
      }
    }

    if (converged) break
  }

  return rotated
}

/**
 * Unified synergy extraction interface
 */
export type SynergyMethod = 'nmf' | 'pca' | 'fa'

export interface SynergyExtractionResult {
  W: number[][]
  H: number[][]
  vaf: number
  method: SynergyMethod
  eigenvalues?: number[]
  communalities?: number[]
}

export function extractSynergies(
  data: number[][],
  nSynergies: number,
  method: SynergyMethod = 'nmf'
): SynergyExtractionResult {
  switch (method) {
    case 'nmf': {
      const result = nmfDecomposition(data, nSynergies)
      return { ...result, method: 'nmf' }
    }
    case 'pca': {
      const result = pcaSynergyExtraction(data, nSynergies)
      return { ...result, method: 'pca' }
    }
    case 'fa': {
      const result = factorAnalysisSynergy(data, nSynergies)
      return { ...result, method: 'fa' }
    }
    default:
      throw new Error(`Unknown synergy method: ${method}`)
  }
}

/**
 * Compare synergies across different methods
 */
export function compareSynergyMethods(
  data: number[][],
  nSynergies: number,
  methods: SynergyMethod[] = ['nmf', 'pca', 'fa']
): {
  results: Record<SynergyMethod, SynergyExtractionResult>
  similarity: Record<string, number>
} {
  const results: Record<string, SynergyExtractionResult> = {}

  for (const method of methods) {
    results[method] = extractSynergies(data, nSynergies, method)
  }

  // Calculate pairwise similarity between methods
  const similarity: Record<string, number> = {}
  for (let i = 0; i < methods.length; i++) {
    for (let j = i + 1; j < methods.length; j++) {
      const m1 = methods[i]
      const m2 = methods[j]
      const simMatrix = synergySimilarity(results[m1].W, results[m2].W, 'correlation')

      // Use Hungarian algorithm approximation for best matching
      let totalSim = 0
      const used = new Set<number>()
      for (let s = 0; s < nSynergies; s++) {
        let bestSim = -1
        let bestIdx = -1
        for (let t = 0; t < nSynergies; t++) {
          if (!used.has(t) && Math.abs(simMatrix[s][t]) > bestSim) {
            bestSim = Math.abs(simMatrix[s][t])
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

  return { results: results as Record<SynergyMethod, SynergyExtractionResult>, similarity }
}
