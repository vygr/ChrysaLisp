$HCPU = "x86_64"
$HABI = "WIN64"
$HOS  = "Windows"


$dir = Get-Location
. "$dir\funcs.ps1"


$use_em = $FALSE;
$ncpu = 64;
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


if ( $ncpu -gt 64){
	$ncpu = 64;
}

if ( $showhelp -eq $TRUE ){
		Write-Output "[-n cnt] number of nodes";
		Write-Output "[-b base] base offset";
		Write-Output "[-e] emulator mode";
		Write-Output "[-h] help";
}
else {
	$num_cpu = $ncpu;
    for ($cpu=$num_cpu-1; $cpu -ge 0; $cpu--){
	    $links=""

	    $lcpu=$((($cpu-1) / 2))
	    $c1 = $cpu
	    $c2 = $lcpu
	    $links += add_link $c1 $c2 $links

	    $lcpu=$((($cpu*2)+1))
	    if ( $lcpu -lt $num_cpu ){
		    $c1 = $cpu
		    $c2 = $lcpu
		    $links += add_link $c1 $c2 $links
	    }

	    $lcpu=$((($cpu*2)+2))
	    if ( $lcpu -lt $num_cpu ){
		    $c1 = $cpu
		    $c2 = $lcpu
		    $links += add_link $c1 $c2 $links
	    }

	    boot_cpu_gui $cpu "$links"
    }
}
