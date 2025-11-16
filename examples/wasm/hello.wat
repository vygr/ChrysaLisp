;; Minimal WebAssembly test module
;; Compile with: wat2wasm hello.wat -o hello.wasm

(module
  ;; Simple add function
  (func $add (export "add") (param $a i64) (param $b i64) (result i64)
    local.get $a
    local.get $b
    i64.add
  )

  ;; Multiply function
  (func $mul (export "mul") (param $a i64) (param $b i64) (result i64)
    local.get $a
    local.get $b
    i64.mul
  )

  ;; Return constant
  (func $answer (export "answer") (result i64)
    i64.const 42
  )

  ;; Fibonacci
  (func $fib (export "fib") (param $n i64) (result i64)
    (if (result i64) (i64.le_s (local.get $n) (i64.const 1))
      (then (local.get $n))
      (else
        (i64.add
          (call $fib (i64.sub (local.get $n) (i64.const 1)))
          (call $fib (i64.sub (local.get $n) (i64.const 2)))
        )
      )
    )
  )
)
