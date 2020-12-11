$dir = Get-Location
. "$dir\funcs.ps1"


if ( $args.count -lt 1 ){
	$num_cpu = 64
}
else {
	$num_cpu = $args[0]
	if ( $num_cpu -gt 1000){
		$num_cpu = 1000
	}
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
