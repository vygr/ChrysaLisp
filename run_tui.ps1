$dir = Get-Location
. "$dir\funcs.ps1"

#have we got a paramater ?
if ( $args.count -lt 1 ){
	$cpus = 10
}
else {
	$cpus = $args[0]
	if ( $cpus -gt 16){
		$cpus = 16
	}
}

for ($cpu = $cpus - 1; $cpu -ge 0; $cpu--){
	$links=""
	for ($lcpu = 0; $lcpu -lt $cpus; $lcpu++)
	{
		$c1 = zero_pad $cpu
		$c2 = zero_pad $lcpu

		$links += add_link $c1 $c2 $links
	}
	boot_cpu_tui $cpu "$links"
}
