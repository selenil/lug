import { bitArraySlice } from "./gleam.mjs";

// borrowed from https://github.com/DanielleMaywood/glexer
export function drop_byte(bits) {
    return bitArraySlice(bits, 8);
}

// borrowed from https://github.com/lpil/splitter
export function compile_binary_pattern(patterns) {
    const encoder = new TextEncoder();
    const compiled = [];
    let cursor = patterns;
    while (cursor.tail) {
        compiled.push(encoder.encode(cursor.head));
        cursor = cursor.tail;
    }
    return compiled;
}

// borrowed from https://github.com/lpil/splitter
export function split_before(patterns, bits) {
    for (let index = 0; index < bits.byteSize; index++) {
        for (const pattern of patterns) {
            if (matchesAt(bits, pattern, index)) {
                const splitPoint = index * 8;
                return [
                    bitArraySlice(bits, 0, splitPoint),
                    bitArraySlice(bits, splitPoint),
                ];
            }
        }
    }

    return [bits, bitArraySlice(bits, bits.bitSize)];
}

function matchesAt(bits, pattern, index) {
    if (index + pattern.length > bits.byteSize) return false;
    for (let offset = 0; offset < pattern.length; offset++) {
        if (bits.byteAt(index + offset) !== pattern[offset]) return false;
    }
    return true;
}

export function nil() {
    return undefined;
}
