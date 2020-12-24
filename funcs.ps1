$HCPU = "x86_64"
$HABI = "WIN64"
$HOS  = "Windows"

function zero_pad {

    param (
        $c
    )

    if ( $c -lt 100 ){
    	$zp = "0$c"
    }
    else {
    	$zp = $c
    }

    if ( $c -lt 10 ){
    	$zp = "0$zp"
    }

    $zp
}

function add_link
{
	param (
		$src,
		$dst,
		$links
	)
	if ( $src -ne $dst ){
		if ( $src -lt $dst ){
			$nl="$src-$dst"
		}
		else {
			$nl="$dst-$src"
		}

		if ( $links.IndexOf($nl) -eq -1 ){
			$rv = "-l $nl "
		}
		else {
			$rv = ""
		}
	}

	$rv
}

function wrap
{
	param (
		$cpu,
		$num_cpu
	)

	$wp=$(($cpu % $num_cpu))
	if ( $wp -lt 0 ){
		$wp=$(($wp + $num_cpu))
	}

	$wp
}

function boot_cpu_gui
{
	param (
		$cpu,
		$lnk
	)
	if ( $cpu -lt 1 ){
		Start-Process -FilePath ./obj/$HCPU/$HABI/$HOS/main_gui -NoNewWindow -ArgumentList "obj/$HCPU/$HABI/sys/boot_image $link -run gui/gui/gui.lisp"
	}
	else {
		Start-Process -FilePath ./obj/$HCPU/$HABI/$HOS/main_gui -NoNewWindow -ArgumentList "obj/$HCPU/$HABI/sys/boot_image $link"
	}
}

function boot_cpu_tui
{
	param (
		$cpu,
		$lnk
	)
	if ( $cpu -lt 1 ){
		Start-Process -FilePath ./obj/$HCPU/$HABI/$HOS/main_gui -NoNewWindow -ArgumentList "obj/$HCPU/$HABI/sys/boot_image $link -run apps/terminal/tui.lisp"
	}
	else {
		Start-Process -FilePath ./obj/$HCPU/$HABI/$HOS/main_gui -NoNewWindow -ArgumentList "obj/$HCPU/$HABI/sys/boot_image $link"
	}
}

function use_emulator
{
    param (
        $arglist,
        $cpucount
    )
    $rv = $FALSE;
    $co = $cpucount

    for ($count = 0 ; $count -lt $arglist.count ; $count ++ ){
        if ( $arglist[$count] -eq '-e'){
            $rv = $TRUE;
        }
        else {
            $co = $arglist[$count]
        }    
    }

    $rv, $co
}