export const entriesImpl = Tuple => obj => 
    () => Object.entries(obj).map(([k, v]) => Tuple(k)(v));

export const values = obj => () => Object.values(obj);

export const keys = obj => () => Object.keys(obj);
