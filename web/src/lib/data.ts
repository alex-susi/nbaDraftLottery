import type { Core, Meta, Methodology, SinglePick, Summary, TeamBundle, TeamFile } from "./types";

// Decoding is shared by the browser loader and the Node parity tests.

export function decodeCore(meta: Meta, buf: ArrayBuffer): Core {
  const { mu, team_slot1, team_slot2 } = meta.core;
  return {
    mu: new Float32Array(buf, mu.offset, mu.length),
    teamSlot1: new Uint8Array(buf, team_slot1.offset, team_slot1.length),
    teamSlot2: new Uint8Array(buf, team_slot2.offset, team_slot2.length),
  };
}

export function decodeTeamBundle(file: TeamFile, nDraws: number, buf: ArrayBuffer): TeamBundle {
  const bundle: TeamBundle = { f32: new Map(), u8: new Map() };
  for (const [key, [kind, offset]] of Object.entries(file.blocks)) {
    if (kind === "f32") bundle.f32.set(key, new Float32Array(buf, offset, nDraws));
    else bundle.u8.set(key, new Uint8Array(buf, offset, nDraws));
  }
  return bundle;
}

const dataUrl = (path: string) => `${import.meta.env.BASE_URL}data/${path}`;

async function fetchBuffer(path: string): Promise<ArrayBuffer> {
  const res = await fetch(dataUrl(path));
  if (!res.ok) throw new Error(`Could not load ${path} (${res.status})`);
  return res.arrayBuffer();
}

async function fetchJson<T>(path: string): Promise<T> {
  const res = await fetch(dataUrl(path));
  if (!res.ok) throw new Error(`Could not load ${path} (${res.status})`);
  return res.json() as Promise<T>;
}

// Each loader runs once; later calls share the same promise.
let metaPromise: Promise<Meta> | null = null;
export const loadMeta = () => (metaPromise ??= fetchJson<Meta>("meta.json"));

let corePromise: Promise<{ meta: Meta; core: Core }> | null = null;
export function loadMetaAndCore(): Promise<{ meta: Meta; core: Core }> {
  corePromise ??= Promise.all([loadMeta(), fetchBuffer("core.bin")])
    .then(([meta, buf]) => ({ meta, core: decodeCore(meta, buf) }));
  return corePromise;
}

let methodologyPromise: Promise<Methodology> | null = null;
export const loadMethodology = () => (methodologyPromise ??= fetchJson<Methodology>("methodology.json"));

let summaryPromise: Promise<Summary> | null = null;
export const loadSummary = () => (summaryPromise ??= fetchJson<Summary>("summary.json"));

const singleCache = new Map<string, Promise<Record<string, SinglePick>>>();
export function loadSingle(team: string): Promise<Record<string, SinglePick>> {
  let p = singleCache.get(team);
  if (!p) {
    p = fetchJson<Record<string, SinglePick>>(`single/${team}.json`);
    p.catch(() => singleCache.delete(team));
    singleCache.set(team, p);
  }
  return p;
}

const bundleCache = new Map<string, Promise<TeamBundle>>();

export function loadTeamBundle(meta: Meta, team: string): Promise<TeamBundle> {
  let p = bundleCache.get(team);
  if (!p) {
    const file = meta.team_files[team];
    p = fetchBuffer(file.file).then((buf) => decodeTeamBundle(file, meta.n_draws, buf));
    p.catch(() => bundleCache.delete(team));
    bundleCache.set(team, p);
  }
  return p;
}
