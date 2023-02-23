
export const performanceNow = () => performance.now()

export const fromBigIntImpl = Nothing => Just => n => {
    if (n > Number.MAX_SAFE_INTEGER) return Nothing;
    if (n < Number.MIN_SAFE_INTEGER) return Nothing;
    return Just(new Number(n) | 0);
}
