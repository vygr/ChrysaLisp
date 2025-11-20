// Array operations WASM module - simulates NumPy-style operations
// This demonstrates the kind of compute workload Python/NumPy would provide
// Compile with:
//   clang --target=wasm32 -O3 -nostdlib -Wl,--no-entry \
//         -Wl,--export=sum_array -Wl,--export=dot_product \
//         -Wl,--export=array_multiply -Wl,--export=mean \
//         -Wl,--export-memory \
//         array_ops.c -o array_ops.wasm

// Memory layout:
// 0x0000 - 0x1000 : Input array 1 (4096 bytes = 512 doubles)
// 0x1000 - 0x2000 : Input array 2 (4096 bytes = 512 doubles)
// 0x2000 - 0x3000 : Output array (4096 bytes = 512 doubles)

// Sum all elements in an array
// Input: offset to array, count of elements
// Returns: sum
long sum_array(long offset, long count) {
    long* arr = (long*)offset;
    long sum = 0;
    for (long i = 0; i < count; i++) {
        sum += arr[i];
    }
    return sum;
}

// Dot product of two arrays
// Input: offset1, offset2, count
// Returns: dot product
long dot_product(long offset1, long offset2, long count) {
    long* arr1 = (long*)offset1;
    long* arr2 = (long*)offset2;
    long result = 0;
    for (long i = 0; i < count; i++) {
        result += arr1[i] * arr2[i];
    }
    return result;
}

// Element-wise multiply two arrays, store in output
// Input: in_offset1, in_offset2, out_offset, count
// Returns: 0 on success
long array_multiply(long in1_offset, long in2_offset, long out_offset, long count) {
    long* in1 = (long*)in1_offset;
    long* in2 = (long*)in2_offset;
    long* out = (long*)out_offset;

    for (long i = 0; i < count; i++) {
        out[i] = in1[i] * in2[i];
    }
    return 0;
}

// Calculate mean of array
// Input: offset, count
// Returns: mean (as integer, no floating point)
long mean(long offset, long count) {
    if (count == 0) return 0;
    return sum_array(offset, count) / count;
}

// Matrix multiplication (2x2 matrices as flat arrays)
// Input: matrix1_offset, matrix2_offset, result_offset
// Returns: 0 on success
long matrix_mult_2x2(long m1_offset, long m2_offset, long result_offset) {
    long* m1 = (long*)m1_offset;
    long* m2 = (long*)m2_offset;
    long* result = (long*)result_offset;

    // 2x2 matrix multiplication
    result[0] = m1[0] * m2[0] + m1[1] * m2[2];
    result[1] = m1[0] * m2[1] + m1[1] * m2[3];
    result[2] = m1[2] * m2[0] + m1[3] * m2[2];
    result[3] = m1[2] * m2[1] + m1[3] * m2[3];

    return 0;
}
