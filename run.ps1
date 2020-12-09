$HCPU = "x86_64"
$HABI = "WIN64"
$HOS  = "Windows"

$dir = Get-Location
. "$dir\funcs.ps1"


if ( $args.count -lt 1 ){
	$cpus = 10
}
else {
	$cpus = $args[0]
	if ( $cpus -gt 1000){
		$cpus = 1000
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
	boot_cpu_gui $cpu "$links"
}





