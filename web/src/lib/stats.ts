// Statistics helpers that reproduce the R functions app.R relies on.

export function mean(x: ArrayLike<number>): number {
  let s = 0;
  for (let i = 0; i < x.length; i++) s += x[i];
  return x.length ? s / x.length : NaN;
}

export function meanBool(x: ArrayLike<boolean | number>): number {
  let s = 0;
  for (let i = 0; i < x.length; i++) if (x[i]) s++;
  return x.length ? s / x.length : NaN;
}

// R quantile(type = 7) for several probabilities from one sort.
export function quantiles(x: ArrayLike<number>, probs: number[]): number[] {
  const s = Float64Array.from(x).sort();
  const n = s.length;
  return probs.map((p) => {
    if (n === 0) return NaN;
    const h = (n - 1) * p;
    const lo = Math.floor(h);
    const hi = Math.min(lo + 1, n - 1);
    return s[lo] + (h - lo) * (s[hi] - s[lo]);
  });
}

function sd(x: ArrayLike<number>): number {
  const m = mean(x);
  let ss = 0;
  for (let i = 0; i < x.length; i++) ss += (x[i] - m) ** 2;
  return Math.sqrt(ss / (x.length - 1));
}

// stats::density(x, n = 512, adjust) with the default nrd0 bandwidth, Gaussian
// kernel, and cut = 3, mirroring density_curve_df() in app.R. The density is
// summed exactly rather than through R's binned FFT, so curves match visually.
export function densityCurve(values: ArrayLike<number>, n = 512, adjust = 1.05): { x: number[]; y: number[] } {
  const x: number[] = [];
  for (let i = 0; i < values.length; i++) if (Number.isFinite(values[i])) x.push(values[i]);
  if (x.length === 0) return { x: [], y: [] };

  const unique = new Set(x);
  const gauss = (z: number, s: number) => Math.exp(-0.5 * z * z) / (s * Math.sqrt(2 * Math.PI));

  if (unique.size === 1) {
    const center = x[0];
    const bw = Math.max(0.05, x.length > 1 ? sd(x) : 0, Math.abs(center) * 0.01);
    const gx: number[] = [];
    const gy: number[] = [];
    for (let i = 0; i < n; i++) {
      const v = center - 4 * bw + (8 * bw * i) / (n - 1);
      gx.push(v);
      gy.push(gauss((v - center) / bw, bw));
    }
    return { x: gx, y: gy };
  }

  const [q25, q75] = quantiles(x, [0.25, 0.75]);
  const s = sd(x);
  let lo = Math.min(s, (q75 - q25) / 1.34);
  if (!(lo > 0)) lo = s || Math.abs(x[0]) || 1;
  const bw = 0.9 * lo * Math.pow(x.length, -0.2) * adjust;

  let min = Infinity;
  let max = -Infinity;
  for (const v of x) {
    if (v < min) min = v;
    if (v > max) max = v;
  }
  const from = min - 3 * bw;
  const to = max + 3 * bw;
  const gx: number[] = [];
  const gy: number[] = [];
  const inv = 1 / (x.length * bw * Math.sqrt(2 * Math.PI));
  for (let i = 0; i < n; i++) {
    const g = from + ((to - from) * i) / (n - 1);
    let acc = 0;
    for (const v of x) {
      const z = (g - v) / bw;
      if (z > -8 && z < 8) acc += Math.exp(-0.5 * z * z);
    }
    gx.push(g);
    gy.push(acc * inv);
  }
  return { x: gx, y: gy };
}

// ---- qbeta: inverse regularized incomplete beta ------------------------------
function lgamma(z: number): number {
  const c = [76.18009172947146, -86.50532032941677, 24.01409824083091,
             -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5];
  let x = z;
  let y = z;
  let tmp = x + 5.5;
  tmp -= (x + 0.5) * Math.log(tmp);
  let ser = 1.000000000190015;
  for (const cj of c) ser += cj / ++y;
  return -tmp + Math.log((2.5066282746310005 * ser) / x);
}

function betacf(a: number, b: number, x: number): number {
  const eps = 3e-16;
  const fpmin = 1e-300;
  let c = 1;
  let d = 1 - ((a + b) * x) / (a + 1);
  if (Math.abs(d) < fpmin) d = fpmin;
  d = 1 / d;
  let h = d;
  for (let m = 1; m <= 1000; m++) {
    const m2 = 2 * m;
    let aa = (m * (b - m) * x) / ((a - 1 + m2) * (a + m2));
    d = 1 + aa * d;
    if (Math.abs(d) < fpmin) d = fpmin;
    c = 1 + aa / c;
    if (Math.abs(c) < fpmin) c = fpmin;
    d = 1 / d;
    h *= d * c;
    aa = (-(a + m) * (a + b + m) * x) / ((a + m2) * (a + 1 + m2));
    d = 1 + aa * d;
    if (Math.abs(d) < fpmin) d = fpmin;
    c = 1 + aa / c;
    if (Math.abs(c) < fpmin) c = fpmin;
    d = 1 / d;
    const del = d * c;
    h *= del;
    if (Math.abs(del - 1) < eps) break;
  }
  return h;
}

export function pbeta(x: number, a: number, b: number): number {
  if (x <= 0) return 0;
  if (x >= 1) return 1;
  const bt = Math.exp(lgamma(a + b) - lgamma(a) - lgamma(b) + a * Math.log(x) + b * Math.log(1 - x));
  return x < (a + 1) / (a + b + 2) ? (bt * betacf(a, b, x)) / a : 1 - (bt * betacf(b, a, 1 - x)) / b;
}

export function qbeta(p: number, a: number, b: number): number {
  let lo = 0;
  let hi = 1;
  for (let i = 0; i < 200; i++) {
    const mid = (lo + hi) / 2;
    if (pbeta(mid, a, b) < p) lo = mid;
    else hi = mid;
    if (hi - lo < 1e-14) break;
  }
  return (lo + hi) / 2;
}

// Axis ticks at 1/2/5 x 10^k steps, about `target` across the range.
export function niceTicks(lo: number, hi: number, target = 6, step?: number): number[] {
  if (!(hi > lo)) return [lo];
  let s = step;
  if (!s) {
    const raw = (hi - lo) / target;
    const mag = Math.pow(10, Math.floor(Math.log10(raw)));
    const r = raw / mag;
    s = (r >= 5 ? 10 : r >= 2 ? 5 : r >= 1 ? 2 : 1) * mag;
  }
  const out: number[] = [];
  for (let v = Math.ceil(lo / s) * s; v <= hi + s * 1e-9; v += s) out.push(Math.abs(v) < s * 1e-9 ? 0 : v);
  return out;
}
