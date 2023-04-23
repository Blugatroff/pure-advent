
export const read = ref => ref.value

export const whileImpl = (condition, body) => {
  while (condition()) body()
}

export const write = (value, ref) => ref.value = value

export const modify = (f, ref) => ref.value = f(ref.value)

