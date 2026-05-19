// borrowed from https://github.com/DanielleMaywood/glexer
export function slice_bytes(string, start, size) {
  return string.slice(start, start + size);
}

// borrowed from https://github.com/DanielleMaywood/glexer
export function drop_byte(string) {
  return string.slice(1);
}

// borrowed from https://github.com/lpil/splitter
export function compile_binary_pattern(patterns) {
  let pattern = "";
  let cursor = patterns;
  while (cursor.tail) {
    if (pattern !== "") pattern += "|";
    pattern += escapeRegExp(cursor.head);
    cursor = cursor.tail;
  }
  return new RegExp(pattern);
}

// borrowed from https://github.com/lpil/splitter
export function split_before(splitter, string) {
  const match = string.match(splitter);

  if (!match) return [string, ""]; // No delimiter found

  const split_point = match.index;
  return [string.slice(0, split_point), string.slice(split_point)];
}
