;;;;;;;;;;;;;;;
; SDL interface
;;;;;;;;;;;;;;;

(defq SDL_RELEASED 0)
(defq SDL_PRESSED 1)

(defq SDL_QUIT 0x100)
(defq SDL_WINDOWEVENT 0x200)
(defq SDL_KEYDOWN 0x300)
(defq SDL_KEYUP 0x301)
(defq SDL_MOUSEMOTION 0x400)
(defq SDL_MOUSEBUTTONDOWN 0x401)
(defq SDL_MOUSEBUTTONUP 0x402)
(defq SDL_MOUSEWHEEL 0x403)

(defq SDL_WINDOWEVENT_SHOWN 0x1)
(defq SDL_WINDOWEVENT_HIDDEN 0x2)
(defq SDL_WINDOWEVENT_EXPOSED 0x3)
(defq SDL_WINDOWEVENT_MOVED 0x4)
(defq SDL_WINDOWEVENT_RESIZED 0x5)
(defq SDL_WINDOWEVENT_SIZE_CHANGED 0x6)
(defq SDL_WINDOWEVENT_MINIMIZED 0x7)
(defq SDL_WINDOWEVENT_MAXIMIZED 0x8)
(defq SDL_WINDOWEVENT_RESTORED 0x9)
(defq SDL_WINDOWEVENT_ENTER 0xa)
(defq SDL_WINDOWEVENT_LEAVE 0xb)
(defq SDL_WINDOWEVENT_FOCUS_GAINED 0xc)
(defq SDL_WINDOWEVENT_FOCUS_LOST 0xd)
(defq SDL_WINDOWEVENT_CLOSE 0xe)
(defq SDL_WINDOWEVENT_TAKE_FOCUS 0xf)
(defq SDL_WINDOWEVENT_HIT_TEST 0x10)

(structure +sdl_common_event 0
	;SDL_KEYDOWN, SDL_KEYUP etc
	(uint type timestamp))

(structure +sdl_keysym 0
	(int scancode sym)
	(ushort mod)
	(uint unused))

(structure +sdl_keyboard_event 0
	(uint type timestamp windowid)
	(ubyte state repeat padding2 padding3)
	(int scancode sym)
	(ushort mod)
	(uint unused))

(structure +sdl_mouse_motion_event 0
	(uint type timestamp windowid which state)
	(int x y xrel yrel))

(structure +sdl_mouse_button_event 0
	(uint type timestamp windowid which)
	(ubyte button state clicks padding1)
	(int x y))

(structure +sdl_mouse_wheel_event 0
	(uint type timestamp windowid which)
	(int x y)
	(uint direction))

(structure +sdl_quit_event 0
	(uint type timestamp))

(structure +sdl_window_event 0
	(uint type timestamp windowid)
	(ubyte event padding1 padding2 padding3)
	(int data1 data2))
