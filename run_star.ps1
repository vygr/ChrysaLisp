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

$links_to_0=""
for ($cpu=$num_cpu-1; $cpu -gt 0; $cpu--){
	$links=""
	$c1 = zero_pad $cpu
	$c2 = zero_pad 0
	$links += add_link $c1 $c2 $links
	$links_to_0+="$links"
	boot_cpu_gui $cpu "$links"
}
boot_cpu_gui $cpu "$links_to_0"
