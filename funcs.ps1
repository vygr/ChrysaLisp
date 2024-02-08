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
		$front,
		$cpu,
		$link
	)
    $cmd = "./obj/$($NHCPU)/$($NHABI)/$($NHOS)/main_gui"
	if ( $cpu -lt 1 ){
		if ( $front -eq $FALSE ){
			Start-Process -FilePath $cmd -NoNewWindow -ArgumentList "obj/$($HCPU)/$($HABI)/sys/boot_image $link -run gui/gui/gui.lisp"
		}
		else {
			$process = Start-Process -FilePath $cmd -NoNewWindow -ArgumentList "obj/$($HCPU)/$($HABI)/sys/boot_image $link -run gui/gui/gui.lisp" -PassThru -Wait
			if ( $process.ExitCode -eq 0){
				Stop-Process -Name main_gui -Force 2>&1 | out-null
			}
		}
	}
	else {
		Start-Process -FilePath $cmd -NoNewWindow -ArgumentList "obj/$($HCPU)/$($HABI)/sys/boot_image $link"
	}
}

function boot_cpu_tui
{
	param (
		$front,
		$cpu,
		$link
	)
    $cmd = "./obj/$($NHCPU)/$($NHABI)/$($NHOS)/main_tui";
	if ( $cpu -lt 1 ){
		if ( $front -eq $FALSE ){
			Start-Process -FilePath $cmd -NoNewWindow -ArgumentList "obj/$($HCPU)/$($HABI)/sys/boot_image $link -run apps/tui/tui.lisp"
		}
		else {
			$process = Start-Process -FilePath $cmd -NoNewWindow -ArgumentList "obj/$($HCPU)/$($HABI)/sys/boot_image $link -run apps/tui/tui.lisp" -PassThru -Wait
			if ( $process.ExitCode -eq 0){
				Stop-Process -Name main_tui -Force 2>&1 | out-null
			}
		}
	}
	else {
		Start-Process -FilePath $cmd -NoNewWindow -ArgumentList "obj/$($HCPU)/$($HABI)/sys/boot_image $link"
	}
}

