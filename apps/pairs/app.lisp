(import "././login/env.inc")
(import "gui/lisp.inc")

(enums +event 0
    (enum close)
    (enum scramble solve)
    (enum click))

(enums +select 0
    (enum main tip timer))

; Configuration and State
(defq *config_file* (cat *env_home* "pairs.tre")
      *grid_w* 10
      *grid_h* 5
      *tile_count* (* *grid_w* *grid_h*)
      ; The pool of characters to form pairs from
      *char_pool* "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*"
      ; Settings
      *match_score* 10
      *mismatch_penalty* 1
      ; Game State
      *values* (list)       ; The hidden characters (as integer codes)
      *states* (list)       ; 0=Hidden, 1= revealed (temp), 2=Matched
      *score* 0
      *first_pick* :nil     ; Index of first card flipped
      *locked* :nil         ; UI lock during mismatch timer
      *font* (create-font "fonts/OpenSans-Bold.ctf" 32)
      *running* :t)

(ui-window *window* ()
    (ui-title-bar *title* "Find Pairs" (0xea19) +event_close)
    (ui-tool-bar *toolbar* ()
        (ui-buttons (0xe972 0xe9ce) +event_scramble))
    (ui-grid *grid* (:grid_width *grid_w* :grid_height *grid_h*
                     :color *env_window_col* :font *font*)
        (each (lambda (i)
            (. (ui-button _ (:min_width 60 :min_height 60)) 
               :connect (+ +event_click i))) 
            (range 0 *tile_count*)))
    (ui-label *status* (:text "Score: 0" :flow_flags +flow_flag_align_hcenter)))

(defun tooltips ()
    (def *window* :tip_mbox (elem-get select +select_tip))
    (ui-tool-tips *toolbar* '("scramble" "solve/peek")))

(defun config-default ()
    (scatter (Emap)
        :values (list)
        :states (list)
        :score 0))

(defun scramble ()
    ; Convert string to list of char codes, shuffle, select subset, duplicate, shuffle again
    (defq all_chars (shuffle (map code *char_pool*))
          pool (slice all_chars 0 (/ *tile_count* 2)))
    (setq *values* (shuffle (cat pool pool))
          *states* (map (lambda (_) 0) (range 0 *tile_count*))
          *score* 0
          *first_pick* :nil
          *locked* :nil)
    (update-view))

(defun config-load ()
    (if (and (defq data (if (defq stream (file-stream *config_file*)) (tree-load stream)))
             (= (length (. data :find :values)) *tile_count*))
        (progn
            (setq *values* (. data :find :values)
                  *states* (. data :find :states)
                  *score* (. data :find :score)
                  *first_pick* :nil 
                  *locked* :nil)
            
            ; Handle legacy string data if present
            (if (and (nempty? *values*) (str? (first *values*)))
                 (setq *values* (map code *values*)))
            
            ; Find currently selected tiles (state 1)
            (defq picks (list))
            (each (lambda (s) (if (= s 1) (push picks (!)))) *states*)
            
            (cond
                ((= (length picks) 1)
                    ; One card revealed, restore state
                    (setq *first_pick* (first picks)))
                ((> (length picks) 1)
                    ; Was saved during a mismatch/lock state. Reset these specific tiles to hidden.
                    (each (# (elem-set *states* %0 0)) picks))))
        (scramble)))

(defun config-save ()
    (when (defq stream (file-stream *config_file* +file_open_write))
        (tree-save stream (scatter (Emap)
            :values *values*
            :states *states*
            :score *score*))))

(defun update-view ()
    (defq children (. *grid* :children)
          i 0
          all_matched :t)
    
    (while (< i *tile_count*)
        (defq btn (elem-get children i)
              val (elem-get *values* i)
              state (elem-get *states* i))
        
        (cond
            ((= state 0) ; Hidden
                (def btn :text "" :color +argb_grey4 :ink_color +argb_white)
                (setq all_matched :nil))
            ((= state 1) ; Selected (Temporary)
                (def btn :text (char val) :color +argb_white :ink_color +argb_black)
                (setq all_matched :nil))
            ((= state 2) ; Matched
                (def btn :text (char val) :color +argb_green :ink_color +argb_white)))
        
        (.-> btn (:constrain :t) :dirty)
        (++ i))
    
    (if all_matched
        (def *status* :text (cat "WINNER! Score: " (str *score*)) :color +argb_green)
        (def *status* :text (cat "Score: " (str *score*)) :color *env_window_col*))
    (.-> *status* :layout :dirty))

(defun try-click (index)
    (when (and (not *locked*) 
               (= (elem-get *states* index) 0)) ; Can only click hidden tiles
        (if *first_pick*
            ; Second selection
            (progn
                (elem-set *states* index 1)
                (update-view)
                
                (defq val1 (elem-get *values* *first_pick*)
                      val2 (elem-get *values* index))
                
                (if (eql val1 val2)
                    (progn 
                        ; Match found
                        (elem-set *states* *first_pick* 2)
                        (elem-set *states* index 2)
                        (setq *score* (+ *score* *match_score*)
                              *first_pick* :nil)
                        (update-view))
                    (progn
                        ; Mismatch
                        (setq *score* (- *score* *mismatch_penalty*)
                              *locked* :t)
                        ; Set timer to hide cards after delay
                        (mail-timeout (elem-get select +select_timer) 1000000 0)
                        (update-view))))
            ; First selection
            (progn
                (setq *first_pick* index)
                (elem-set *states* index 1)
                (update-view)))))

(defun solve ()
    ; Cheat function: reveal all
    (setq *states* (map (lambda (_) 2) *states*))
    (update-view))

(defun dispatch-action (id)
    (cond
        ((= id +event_close)
            (setq *running* :nil))
        ((= id +event_scramble)
            (scramble))
        ((= id +event_solve)
            (solve))
        ((>= id +event_click)
            (try-click (- id +event_click)))))

(defun main ()
    (defq select (task-mboxes +select_size))
    (config-load)
    (tooltips)
    (bind '(x y w h) (apply view-locate (. *window* :pref_size)))
    (gui-add-front-rpc (. *window* :change x y w h))
    (update-view)
    
    (while *running*
        (defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
        (cond
            ((= idx +select_tip)
                (if (defq view (. *window* :find_id (getf msg +mail_timeout_id)))
                    (. view :show_tip)))
            ((= idx +select_timer)
                ; Mismatch timer fired - hide temporary cards
                (when *locked*
                    (setq *states* (map (lambda (s) (if (= s 1) 0 s)) *states*)
                          *first_pick* :nil
                          *locked* :nil)
                    (update-view)))
            ((= (getf msg +ev_msg_type) +ev_type_action)
                (dispatch-action (getf msg +ev_msg_target_id)))
            (:t (. *window* :event msg))))
    
    (config-save)
    (gui-sub-rpc *window*))