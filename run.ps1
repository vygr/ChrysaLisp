$HCPU = "x86_64"
$HABI = "WIN64"
$HOS  = "Windows"

$dir = Get-Location
. "$dir\funcs.ps1"

#################################################
#
# start of header
#
#################################################
$use_em = $FALSE;
$ncpu = 10;
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

    for ($cpu = $ncpu - 1; $cpu -ge 0; $cpu--){
	    $links=""
	    for ($lcpu = 0; $lcpu -lt $ncpu; $lcpu++)
	    {
		    #$c1 = zero_pad $cpu
		    #$c2 = zero_pad $lcpu

		    $links += add_link $cpu $lcpu $links
	    }
	    boot_cpu_gui $cpu "$links"
    }
}
