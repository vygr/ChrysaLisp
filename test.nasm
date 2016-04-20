%macro set_src 1-*
	;%1... = paramaters
	%assign _src_cnt 0
	%rep %0
		%define _src_%[_src_cnt] %1
		%assign _src_cnt _src_cnt + 1
		%rotate 1
	%endrep
%endmacro

%macro set_dst 1-*
	;%1... = paramaters
	%assign _dst_cnt 0
	%rep %0
		%define _dst_%[_dst_cnt] %1
		%assign _dst_cnt _dst_cnt + 1
		%rotate 1
	%endrep
%endmacro

%macro map_print 0
	%warning src => dst
	%assign %%i 0
	%rep _src_cnt
		%warning map entry %%i: _src_%[%%i] => _dst_%[%%i]
		%assign %%i %%i + 1
	%endrep
%endmacro

%macro map_rotate 2
	;%1 = dst index
	;%2 = src index
	%assign %%i 0
	%rep _src_cnt
		%xdefine %%s_%[%%i] _src_%[%%i]
		%xdefine %%d_%[%%i] _dst_%[%%i]
		%assign %%i %%i + 1
	%endrep
	%assign %%j %2
	%rep %2 - %1
		%assign %%i %%j - 1
		%define _src_%[%%j] %%s_%[%%i]
		%define _dst_%[%%j] %%d_%[%%i]
		%assign %%j %%i
	%endrep
	%define _src_%[%1] %%s_%[%2]
	%define _dst_%[%1] %%d_%[%2]
%endmacro

set_src 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
set_dst a, b, c, d, e, f, g, h, i, j
map_print
map_rotate 2, 7
map_print
