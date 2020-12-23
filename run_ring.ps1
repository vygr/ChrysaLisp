$dir = Get-Location
. "$dir\funcs.ps1"


$useem, $num_cpu = use_emulator $args 64
if ( $useem -eq $TRUE ){
    $HCPU = "vp64"
    $HABI = "VP64"
    $HOS  = "Windows"
    Write-Output "Using emulator", $useem, $cpus
}

if ( $num_cpu -gt 64){
	$num_cpu = 64
}

for ($cpu=$num_cpu-1; $cpu -ge 0; $cpu--){
	$links=""
	for ($lcpu=$cpu-1; $lcpu -le $cpu+1; $lcpu++){
		$wp = wrap $lcpu $num_cpu
		$c1 = zero_pad $cpu
		$c2 = zero_pad $wp
		$links += add_link $c1 $c2 $links
	}
	boot_cpu_gui $cpu "$links"
}
