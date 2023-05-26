#Start ChrysaLisp on framebuffer

OS=`cat os`
CPU=`cat cpu`
ABI=`cat abi`

exec obj/$CPU/$ABI/$OS/main_gui obj/$CPU/$ABI/sys/boot_image -run gui/gui/gui.lisp $2 $3
