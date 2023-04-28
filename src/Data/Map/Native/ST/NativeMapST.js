
export const empty = () => new Map()

export const memberSTFnImpl = (k, map) => map.has(k)

export const lookupSTFnImpl = Nothing => Just => (k, map) => {
  const v = map.get(k) 
  if (v === undefined) return Nothing
  return Just(v)
}

export const insertSTFnImpl = (k, v, map) => map.set(k, v)

export const removeSTFnImpl = (k, map) => map.delete(k)

export const entriesSTFnImpl = tuple => set => Array.from(set.entries()).map(([k, v]) => tuple(k, v))

export const sizeSTFn = map => map.size

export const fromEntriesImpl = fst => snd => entries => new Map(entries.map(t => [fst(t), snd(t)]))

