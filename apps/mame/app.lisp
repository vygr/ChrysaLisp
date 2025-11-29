;MAME for ChrysaLisp - Main Launcher Application
;
;This launcher provides a GUI for selecting and running arcade game ROMs
;through the MAME emulator running on ChrysaLisp's PII adapter layer.

;debug options
(case 0
(0 (import "lib/debug/frames.inc"))
(1 (import "lib/debug/profile.inc"))
(2 (import "lib/debug/debug.inc")))

(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/files/files.inc")

;UI layout
(defq
	+border+ 2
	+spacing+ 3
	+min_width+ 640
	+min_height+ 480
	+list_width+ 300
	+list_height+ 400
)

;MAME state
(defq
	*mame_lib* :nil
	*rom_path* "roms/"
	*roms_list* (list)
	*selected_rom* :nil
	*running* :t
)

;Load MAME adapter library (would be JIT compiled C++ code)
(defun load-mame-lib ()
	;TODO: When MAME adapter is built, load it here
	;This would use (ffi) to load the compiled MAME adapter
	;For now, this is a placeholder
	(print "Loading MAME adapter library...")
	;(setq *mame_lib* (ffi "obj/mame/mame_adapter.so"))
	(print "MAME adapter library loaded"))

;Scan for ROM files in the ROM directory
(defun scan-roms ()
	(print "Scanning for ROMs in: " *rom_path*)
	(setq *roms_list* (list))

	;Scan for .zip files (MAME ROMs are typically .zip)
	(each! (lambda ((. _ :get_name))
		(when (ends-with ".zip")
			(push *roms_list* _)))
		(sort (cat (files-all *rom_path*))))

	(print "Found " (length *roms_list*) " ROM(s)"))

;Launch MAME with selected ROM
(defun launch-mame (rom_name)
	(print "Launching MAME with ROM: " rom_name)

	;TODO: This is where we would:
	;1. Call into the MAME adapter C++ code via FFI
	;2. Initialize MAME with the selected ROM
	;3. Run the emulation loop
	;4. Handle input/video/audio

	;For now, just show a message
	(ui-window *window* ()
		(ui-title-bar _ "MAME - " rom_name (0 0 *env_window_col*))
		(ui-flow _ (:flow_down :flow_flags +flow_down_fill+)
			(ui-label _ (:text "MAME emulation would run here" :color +argb_white+))
			(ui-label _ (:text (cat "ROM: " rom_name) :color +argb_cyan+))
			(ui-label _ (:text "Press ESC to return to launcher" :color +argb_grey+))
		)
	)

	;TODO: Enter emulation loop here
	;For now, just wait a bit then return
	(task-sleep 2000000)
	(print "MAME emulation ended"))

;Create main window
(defun create-window ()
	(ui-window *window* ()
		;Title bar
		(ui-title-bar *title* "MAME Launcher" (0 0 *env_window_col*))

		;Main content
		(ui-flow _ (:flow_down :flow_flags +flow_down_fill+ :gap +spacing+)

			;Info text
			(ui-label _ (:text "MAME - Multiple Arcade Machine Emulator" :color +argb_white+))
			(ui-label _ (:text "Select a ROM to emulate:" :color +argb_grey+))

			;ROM list
			(ui-scroll *rom_scroll* (:min_width +list_width+ :min_height +list_height+
									 :color *env_toolbar_col*)
				(ui-flow *rom_flow* (:flow_down :gap 1)
					(each! (lambda (rom_name)
						(ui-button _ (:text rom_name :color *env_toolbar2_col*
									 :border 0 :id rom_name)))
						*roms_list*)
				)
			)

			;Control buttons
			(ui-flow _ (:flow_right :gap +spacing+)
				(ui-button *refresh_btn* (:text "Refresh ROMs"))
				(ui-button *settings_btn* (:text "Settings"))
				(ui-button *quit_btn* (:text "Quit"))
			)

			;Status bar
			(ui-label *status* (:text (cat "Found " (length *roms_list*) " ROM(s)")
								:color +argb_grey+ :font *env_medium_font*))
		)
	))

;Event handlers
(defun action-refresh ()
	(scan-roms)
	(. *status* :set_text (cat "Found " (length *roms_list*) " ROM(s)"))
	(print "ROM list refreshed"))

(defun action-rom-selected (rom_name)
	(print "Selected ROM: " rom_name)
	(setq *selected_rom* rom_name)
	(launch-mame rom_name))

(defun action-settings ()
	(print "Settings dialog would open here"))

(defun action-quit ()
	(setq *running* :nil))

;Event map for UI components
(defq *event_map* (scatter (Fmap 31)
	(cat (. *refresh_btn* :get_id)) action-refresh
	(cat (. *settings_btn* :get_id)) action-settings
	(cat (. *quit_btn* :get_id)) action-quit
))

;Add ROM button handlers
(each! (lambda (rom_name)
	(. *event_map* :insert (cat (. _ :get_id))
		`(action-rom-selected ,rom_name)))
	(. *rom_flow* :children))

;Dispatch action with error handling
(defun dispatch-action (&rest action)
	(catch (eval action)
		(progn (prin _) (print) :t)))

;Main event loop
(defun main ()
	(print "=== MAME for ChrysaLisp ===")

	;Initialize
	(load-mame-lib)
	(scan-roms)
	(create-window)

	;Show window
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))

	;Event loop
	(while *running*
		(defq *msg* (mail-read (task-mbox)))
		(cond
			((defq id (getf *msg* +ev_msg_target_id)
				   action (. *event_map* :find id))
				;Dispatch bound action
				(dispatch-action action))
			((= (getf *msg* +ev_msg_type) +ev_type_key_down)
				;Handle key events
				(defq key (getf *msg* +ev_msg_key_key))
				(when (= key +char_esc)
					(action-quit)))
			(:t
				;Pass to window
				(. *window* :event *msg*))))

	;Cleanup
	(gui-sub-rpc *window*)
	(print "MAME launcher exited"))
