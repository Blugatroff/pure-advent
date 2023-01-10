
export const parseNumberImpl = Left => Right => s => {
    if (s.length === 0) return Left(new Error("failed to parse " + s));
    const n = Number(s);
    if (isNaN(n) || !isFinite(n)) return Left(new Error("failed to parse " + s));
    return Right(n);
};
