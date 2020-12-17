$dir = Get-Location
. "$dir\funcs.ps1"


if ( $args.count -lt 1 ){
	$num_cpu = 64
}
else {
	$num_cpu = $args[0]
	if ( $num_cpu -gt 64){
		$num_cpu = 64
	}
}

for ($cpu=$num_cpu-1; $cpu -ge 0; $cpu--){
	$links=""

	$lcpu=$((($cpu-1) / 2))
	$c1 = zero_pad $cpu
	$c2 = zero_pad $lcpu
	$links += add_link $c1 $c2 $links

	$lcpu=$((($cpu*2)+1))
	if ( $lcpu -lt $num_cpu ){
		$c1 = zero_pad $cpu
		$c2 = zero_pad $lcpu
		$links += add_link $c1 $c2 $links
	}

	$lcpu=$((($cpu*2)+2))
	if ( $lcpu -lt $num_cpu ){
		$c1 = zero_pad $cpu
		$c2 = zero_pad $lcpu
		$links += add_link $c1 $c2 $links
	}

	boot_cpu_gui $cpu "$links"
}
