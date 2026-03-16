#!/usr/bin/env node
// Generate sample_eeg.csv for PhysioExperiment
// Usage: node inst/extdata/generate_csv.js

const fs = require('fs');
const path = require('path');

// Mulberry32 PRNG (seedable, deterministic)
function mulberry32(a) {
  return function() {
    a |= 0; a = a + 0x6D2B79F5 | 0;
    var t = Math.imul(a ^ a >>> 15, 1 | a);
    t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t;
    return ((t ^ t >>> 14) >>> 0) / 4294967296;
  };
}

// Box-Muller transform for Gaussian noise
function gaussianNoise(rng, sd) {
  const u1 = rng();
  const u2 = rng();
  return sd * Math.sqrt(-2 * Math.log(u1)) * Math.cos(2 * Math.PI * u2);
}

const rng = mulberry32(42);
const sr = 250;
const n = 500;
const freqs = { Fz: 10, Cz: 8, Pz: 12, Oz: 6 };
const channels = ['Fz', 'Cz', 'Pz', 'Oz'];

let lines = ['time,' + channels.join(',')];

for (let i = 0; i < n; i++) {
  const t = i / sr;
  const vals = [t.toFixed(6)];
  for (const ch of channels) {
    const f = freqs[ch];
    const signal = Math.sin(2 * Math.PI * f * t) + gaussianNoise(rng, 0.3);
    vals.push(signal.toFixed(6));
  }
  lines.push(vals.join(','));
}

const outPath = path.join(path.dirname(process.argv[1] || __filename), 'sample_eeg.csv');
fs.writeFileSync(outPath, lines.join('\n') + '\n');
console.log('Wrote ' + n + ' rows to ' + outPath);
