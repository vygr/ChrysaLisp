# PowerShell script for building WASM target on Windows

Write-Host "=== ChrysaLisp WASM Build Script ===" -ForegroundColor Cyan
Write-Host ""

# Create output directories
Write-Host "Creating WASM output directories..."
New-Item -ItemType Directory -Force -Path obj/wasm32/WASM32/sys | Out-Null
New-Item -ItemType Directory -Force -Path obj/wasm32/WASM32/web | Out-Null

# Set environment for WASM build
$env:CPU = "wasm32"
$env:ABI = "WASM32"
"wasm32" | Out-File -FilePath cpu -Encoding ASCII
"WASM32" | Out-File -FilePath abi -Encoding ASCII
"Web" | Out-File -FilePath os -Encoding ASCII

Write-Host "CPU: $env:CPU"
Write-Host "ABI: $env:ABI"
Write-Host ""

# Build boot image using TUI (we need a host to run the build)
Write-Host "Building WASM boot image..."

$tuiPath = "obj/x86_64/WIN64/Windows/main_tui.exe"
$vp64BootPath = "obj/vp64/VP64/sys/boot_image"
$bootImagePath = "obj/x86_64/WIN64/sys/boot_image"

if (Test-Path $tuiPath) {
    if (Test-Path $bootImagePath) {
        Write-Host "Using existing TUI to build WASM boot image..."
        & $tuiPath $bootImagePath -run cmd/make.lisp boot
    } elseif (Test-Path $vp64BootPath) {
        Write-Host "Using VP64 emulator to build WASM boot image..."
        & $tuiPath -e $vp64BootPath -run cmd/make.lisp boot
    } else {
        Write-Host "ERROR: No boot image found." -ForegroundColor Red
        exit 1
    }
} else {
    Write-Host "ERROR: No TUI executable found. Please run 'make install' first." -ForegroundColor Red
    exit 1
}

Write-Host ""
Write-Host "=== WASM Build Complete ===" -ForegroundColor Green
Write-Host ""
Write-Host "Boot image should be at: obj/wasm32/WASM32/sys/boot_image"
Write-Host ""
Write-Host "Note: This boot image contains WASM bytecode, not native code."
Write-Host "To use it, you'll need to:"
Write-Host "1. Generate WASM module wrapper (not yet implemented)"
Write-Host "2. Load in browser or Node.js"
Write-Host "3. Provide JavaScript host interface"
