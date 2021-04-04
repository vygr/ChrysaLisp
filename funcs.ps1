$NHCPU = "x86_64"
$NHABI = "WIN64"
$NHOS  = "Windows"

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
    $src = zero_pad $($src + $bcpu);
    $dst = zero_pad $($dst + $bcpu);
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
		$link
	)
    $cmd = "./obj/$($NHCPU)/$($NHABI)/$($NHOS)/main_gui"
	if ( $cpu -lt 1 ){
		Start-Process -FilePath $cmd -NoNewWindow -ArgumentList "obj/$($HCPU)/$($HABI)/sys/boot_image $link -run gui/gui/gui.lisp"
	}
	else {
		Start-Process -FilePath $cmd -NoNewWindow -ArgumentList "obj/$($HCPU)/$($HABI)/sys/boot_image $link"
	}
}

function boot_cpu_tui
{
	param (
		$cpu,
		$link
	)
    $cmd = "./obj/$($NHCPU)/$($NHABI)/$($NHOS)/main_tui";
	if ( $cpu -lt 1 ){
		Start-Process -FilePath $cmd -NoNewWindow -ArgumentList "obj/$(HCPU)/$(HABI)/sys/boot_image $link -run apps/terminal/tui.lisp"
	}
	else {
		Start-Process -FilePath $cmd -NoNewWindow -ArgumentList "obj/$(HCPU)/$(HABI)/sys/boot_image $link"
	}
}

