#!/bin/bash
# Bloom Filter Code Compliance Check Script
# Based on CHRYSALISP_DEVELOPMENT_GUIDE.md

echo "================================================"
echo "BLOOM FILTER LIBRARY COMPLIANCE REVIEW"
echo "================================================"
echo ""

FILES=(
    "lib/collections/bloom.inc"
    "lib/collections/counting_bloom.inc"
    "lib/collections/scalable_bloom.inc"
)

PASS=0
FAIL=0
WARN=0

echo "1. CHECKING FOR SHEBANGS (should be NONE)"
echo "-------------------------------------------"
for file in "${FILES[@]}"; do
    if head -1 "$file" | grep -q "^#!"; then
        echo "❌ FAIL: $file has shebang"
        ((FAIL++))
    else
        echo "✅ PASS: $file (no shebang)"
        ((PASS++))
    fi
done
echo ""

echo "2. CHECKING IMPORTS"
echo "-------------------------------------------"
for file in "${FILES[@]}"; do
    echo "File: $file"
    head -15 "$file" | grep "(import" || echo "  ⚠️  No imports found"
    echo ""
done

echo "3. CHECKING FOR DEFMETHOD OUTSIDE DEFCLASS"
echo "-------------------------------------------"
for file in "${FILES[@]}"; do
    if grep -n "^(defmethod" "$file" > /dev/null 2>&1; then
        echo "❌ FAIL: $file has defmethod at column 0 (outside defclass)"
        grep -n "^(defmethod" "$file" | head -3
        ((FAIL++))
    else
        echo "✅ PASS: $file (all defmethod inside defclass)"
        ((PASS++))
    fi
done
echo ""

echo "4. CHECKING CLASS DEFINITIONS"
echo "-------------------------------------------"
for file in "${FILES[@]}"; do
    echo "File: $file"
    grep -n "^(defclass" "$file" || echo "  No defclass found"
    echo ""
done

echo "5. CHECKING FIELD ACCESS PATTERNS"
echo "-------------------------------------------"
echo "Looking for (get :fieldname this) pattern..."
for file in "${FILES[@]}"; do
    count=$(grep -c "(get :" "$file" 2>/dev/null || echo "0")
    echo "$file: $count occurrences of (get :...) pattern"
done
echo ""

echo "6. CHECKING FIELD SET PATTERNS"
echo "-------------------------------------------"
echo "Looking for (def this :field value) pattern..."
for file in "${FILES[@]}"; do
    count=$(grep -c "(def this :" "$file" 2>/dev/null || echo "0")
    echo "$file: $count occurrences of (def this :...) pattern"
done
echo ""

echo "7. CHECKING FOR :nil AND :t USAGE"
echo "-------------------------------------------"
for file in "${FILES[@]}"; do
    nil_count=$(grep -c ":nil" "$file" 2>/dev/null || echo "0")
    t_count=$(grep -c ":t" "$file" 2>/dev/null || echo "0")
    echo "$file: $nil_count uses of :nil, $t_count uses of :t"
done
echo ""

echo "8. CHECKING EXPORT STATEMENTS"
echo "-------------------------------------------"
for file in "${FILES[@]}"; do
    echo "File: $file"
    tail -5 "$file" | grep "export-classes" || echo "  ⚠️  No export-classes found"
    echo ""
done

echo "9. CHECKING METHOD RETURN VALUES"
echo "-------------------------------------------"
echo "Methods should return 'this' for chaining..."
for file in "${FILES[@]}"; do
    echo "$file:"
    grep -A 2 "defmethod.*:add\|:insert\|:remove\|:clear" "$file" | grep -c "this)" || echo "  Found return this patterns"
    echo ""
done

echo "================================================"
echo "SUMMARY"
echo "================================================"
echo "Checks passed: $PASS"
echo "Checks failed: $FAIL"
echo "Warnings: $WARN"
echo ""

if [ $FAIL -eq 0 ]; then
    echo "✅ ALL CRITICAL CHECKS PASSED"
    exit 0
else
    echo "❌ SOME CHECKS FAILED - REVIEW NEEDED"
    exit 1
fi
