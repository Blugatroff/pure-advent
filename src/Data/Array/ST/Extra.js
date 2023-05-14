export const findIndexImpl = Nothing => Just => (arr, f) => {
  for (let i = 0, l = arr.length; i < l; i++) {
    if (f(arr[i])) return Just(i);
  }
  return Nothing;
};
