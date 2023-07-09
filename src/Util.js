
export const performanceNow = () => performance.now()

export const fromBigIntImpl = Nothing => Just => n => {
    if (n > Number.MAX_SAFE_INTEGER) return Nothing;
    if (n < Number.MIN_SAFE_INTEGER) return Nothing;
    return Just(new Number(n) | 0);
}

export const traceRuntimeImpl = now => consoleError => label => f => a => {
  const start = now();
  const v = f(a);
  const end = now();
  consoleError(label + ": " + (end - start))();
  return v;
}

