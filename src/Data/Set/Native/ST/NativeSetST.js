
export const empty = () => new Set()

export const memberSTFnImpl = (k, set) => set.has(k)

export const insertSTFnImpl = (k, set) => set.add(k)

export const removeSTFnImpl = (k, set) => set.delete(k)

export const entriesSTFn = set => set.entries()

export const sizeSTFn = set => set.size

export const fromEntries = entries => () => new Set(entries)

