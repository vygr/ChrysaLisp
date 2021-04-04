$HCPU = "x86_64"
$HABI = "WIN64"
$HOS  = "Windows"


$dir = Get-Location
. "$dir\funcs.ps1"


$use_em = $FALSE;
$ncpu = 4;
$bcpu = 0;
$showhelp = $FALSE;

for ($count = 0 ; $count -lt $args.count ; $count ++ ){
    $marg = $args[$count]
    if ( $marg -eq '-e'){
        $use_em = $TRUE;
    }
    elseif ( $marg -eq '-n'){
        $ncpu = $args[$count+1];
        $count ++;
    }
    elseif ( $marg -eq '-b'){
        $bcpu = $args[$count+1];
        $count ++;
    }
    else {
        $showhelp = $TRUE;
    }    
}

if ( $use_em -eq $TRUE ){
    $HCPU = "vp64";
    $HABI = "VP64";
    $HOS  = "Windows";
}

if ( $bcpu -eq 0 ){
    . "$dir\stop.ps1"
}


if ( $ncpu -gt 4){
	$ncpu = 4;
}

if ( $showhelp -eq $TRUE ){
		Write-Output "[-n cnt] number of nodes";
		Write-Output "[-b base] base offset";
		Write-Output "[-e] emulator mode";
		Write-Output "[-h] help";
}
else {
	$num_cpu = $ncpu;
    for ($cpu_z=$num_cpu-1; $cpu_z -ge 0; $cpu_z--){
	    for ($cpu_y=$num_cpu-1; $cpu_y -ge 0; $cpu_y--){
		    for ($cpu_x=$num_cpu-1; $cpu_x -ge 0; $cpu_x--){
			    $cpu=$(($cpu_z*$num_cpu*$num_cpu + $cpu_y*$num_cpu + $cpu_x))
			    $links=""
			    for ($lcpu_z=$cpu_z-1; $lcpu_z -le $cpu_z+1; $lcpu_z++){
				    $wp = wrap $lcpu_z $num_cpu
				    $c1 = $cpu
				    $c2 = $(($wp*$num_cpu*$num_cpu + $cpu_y*$num_cpu + $cpu_x))
				    $links += add_link $c1 $c2 $links
			    }
			    for ($lcpu_y=$cpu_y-1; $lcpu_y -le $cpu_y+1; $lcpu_y++){
				    $wp = wrap $lcpu_y $num_cpu
				    $c1 = $cpu
				    $c2 = $(($cpu_z*$num_cpu*$num_cpu + $wp*$num_cpu + $cpu_x))
				    $links += add_link $c1 $c2 $links
			    }
			    for ($lcpu_x=$cpu_x-1; $lcpu_x -le $cpu_x+1; $lcpu_x++){
				    $wp = wrap $lcpu_x $num_cpu
				    $c1 = $cpu
				    $c2 = $(($cpu_z*$num_cpu*$num_cpu + $cpu_y*$num_cpu + $wp))
				    $links += add_link $c1 $c2 $links
			    }
			    boot_cpu_gui $cpu "$links"
		    }
	    }
    }
}




