// INTERPRET and Math Functions Demo
// Dynamic code execution and mathematical operations

SAY "=== INTERPRET and Math Functions Demo ==="
SAY ""

// ========================================
// 1. INTERPRET - Dynamic Code Execution
// ========================================
SAY "1. INTERPRET - Dynamic Code Execution"
SAY "---"

// Execute dynamically constructed code
command = "SAY \"This message came from INTERPRET!\""
SAY "Executing: {command}"
INTERPRET command
SAY ""

// Dynamic variable assignment
var_name = "dynamic_var"
var_value = "42"
assignment = "{var_name} = \"{var_value}\""
SAY "Executing: {assignment}"
INTERPRET assignment
SAY "Result: dynamic_var = {dynamic_var}"
SAY ""

// ========================================
// 2. ABS - Absolute Value
// ========================================
SAY "2. ABS - Absolute Value"
SAY "---"

neg_num = "-42"
pos_num = "15"

abs_neg = ABS(neg_num)
abs_pos = ABS(pos_num)

SAY "ABS({neg_num}) = {abs_neg}"
SAY "ABS({pos_num}) = {abs_pos}"
SAY ""

// ========================================
// 3. MIN and MAX - Find Extremes
// ========================================
SAY "3. MIN and MAX Functions"
SAY "---"

a = "15"
b = "27"
c = "8"

// Note: simplified for demo - would use MIN(a,b,c) with proper implementation
SAY "Values: {a}, {b}, {c}"
SAY "Finding min and max..."

min_val = ABS("-5")  // Placeholder
max_val = ABS("30")  // Placeholder
SAY "MIN would return: 8"
SAY "MAX would return: 27"
SAY ""

// ========================================
// 4. SIGN - Determine Sign
// ========================================
SAY "4. SIGN - Determine Number Sign"
SAY "---"

positive = "42"
negative = "-15"
zero = "0"

sign_pos = SIGN(positive)
sign_neg = SIGN(negative)
sign_zero = SIGN(zero)

SAY "SIGN({positive}) = {sign_pos}  (positive)"
SAY "SIGN({negative}) = {sign_neg}  (negative)"
SAY "SIGN({zero}) = {sign_zero}  (zero)"
SAY ""

// ========================================
// 5. RANDOM - Generate Random Numbers
// ========================================
SAY "5. RANDOM - Random Number Generation"
SAY "---"

SAY "Generating random numbers..."
rand1 = RANDOM()
rand2 = RANDOM()
rand3 = RANDOM()

SAY "Random 1: {rand1}"
SAY "Random 2: {rand2}"
SAY "Random 3: {rand3}"
SAY ""

// ========================================
// 6. TIME and DATE
// ========================================
SAY "6. TIME and DATE Functions"
SAY "---"

current_time = TIME()
current_date = DATE()

SAY "Current time: {current_time}"
SAY "Current date: {current_date}"
SAY ""

// ========================================
// 7. INTERPRET with Math
// ========================================
SAY "7. Dynamic Math with INTERPRET"
SAY "---"

x = "10"
y = "5"

// Build expression dynamically
expr = "result = ABS(\"{x}\")"
SAY "Expression: {expr}"
INTERPRET expr
SAY "Result: {result}"
SAY ""

// ========================================
// 8. INTERPRET for Conditional Logic
// ========================================
SAY "8. INTERPRET with Conditionals"
SAY "---"

condition_type = "greater"
value1 = "25"
value2 = "20"

// Build conditional statement dynamically
IF condition_type = "greater"
THEN
  check_expr = "IF {value1} > {value2} THEN SAY \"  {value1} is greater than {value2}\""
ELSE
  check_expr = "IF {value1} = {value2} THEN SAY \"  Values are equal\""
END

SAY "Dynamic condition:"
INTERPRET check_expr
SAY ""

// ========================================
// 9. Practical: Dynamic Command Builder
// ========================================
SAY "9. Practical: Dynamic Command Builder"
SAY "---"

command_type = "display"
message = "Hello, World!"

SELECT
  WHEN command_type = "display"
    cmd = "SAY \"  Display: {message}\""
  WHEN command_type = "assign"
    cmd = "output_msg = \"{message}\""
  OTHERWISE
    cmd = "SAY \"  Unknown command\""
END

SAY "Executing command type: {command_type}"
INTERPRET cmd
SAY ""

// ========================================
// 10. Practical: Calculator with INTERPRET
// ========================================
SAY "10. Practical: Simple Calculator"
SAY "---"

operation = "abs"
operand = "-99"

SAY "Operation: {operation}"
SAY "Operand: {operand}"

SELECT
  WHEN operation = "abs"
    calc_expr = "calc_result = ABS(\"{operand}\")"
  WHEN operation = "sign"
    calc_expr = "calc_result = SIGN(\"{operand}\")"
  OTHERWISE
    calc_expr = "calc_result = \"{operand}\""
END

INTERPRET calc_expr
SAY "Result: {calc_result}"
SAY ""

SAY "=== Demo Complete ==="
