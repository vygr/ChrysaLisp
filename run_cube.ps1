$dir = Get-Location
. "$dir\funcs.ps1"


if ( $args.count -lt 1 ){
	$num_cpu = 4
}
else {
	$num_cpu = $args[0]
	if ( $num_cpu -gt 4){
		$num_cpu = 4
	}
}

for ($cpu_z=$num_cpu-1; $cpu_z -ge 0; $cpu_z--){
	for ($cpu_y=$num_cpu-1; $cpu_y -ge 0; $cpu_y--){
		for ($cpu_x=$num_cpu-1; $cpu_x -ge 0; $cpu_x--){
			$cpu=$(($cpu_z*$num_cpu*$num_cpu + $cpu_y*$num_cpu + $cpu_x))
			$links=""
			for ($lcpu_z=$cpu_z-1; $lcpu_z -le $cpu_z+1; $lcpu_z++){
				$wp = wrap $lcpu_z $num_cpu
				$c1 = zero_pad $cpu
				$c2 = zero_pad $(($wp*$num_cpu*$num_cpu + $cpu_y*$num_cpu + $cpu_x))
				$links += add_link $c1 $c2 $links
			}
			for ($lcpu_y=$cpu_y-1; $lcpu_y -le $cpu_y+1; $lcpu_y++){
				$wp = wrap $lcpu_y $num_cpu
				$c1 = zero_pad $cpu
				$c2 = zero_pad $(($cpu_z*$num_cpu*$num_cpu + $wp*$num_cpu + $cpu_x))
				$links += add_link $c1 $c2 $links
			}
			for ($lcpu_x=$cpu_x-1; $lcpu_x -le $cpu_x+1; $lcpu_x++){
				$wp = wrap $lcpu_x $num_cpu
				$c1 = zero_pad $cpu
				$c2 = zero_pad $(($cpu_z*$num_cpu*$num_cpu + $cpu_y*$num_cpu + $wp))
				$links += add_link $c1 $c2 $links
			}
			boot_cpu_gui $cpu "$links"
		}
	}
}




