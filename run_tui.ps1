$dir = Get-Location
. "$dir\funcs.ps1"

$use_em = $FALSE;
$ncpu = 8;
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
    $HCPU = "vp64"
    $HABI = "VP64"
    $HOS  = "Windows"
}

if ( $bcpu -eq 0 ){
    . "$dir\stop.ps1"
}


if ( $ncpu -gt 16){
	$ncpu = 16
}

if ( $showhelp -eq $TRUE ){
		Write-Output "[-n cnt] number of nodes";
		Write-Output "[-b base] base offset";
		Write-Output "[-e] emulator mode";
		Write-Output "[-h] help";
}
else {
    $cpus = $ncpu;
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
}
