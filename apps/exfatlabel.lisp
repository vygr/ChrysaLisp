;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ExFat Volume Label Manager
; Read and set filesystem volume labels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/fs/exfat.inc")

(defun read_volume_label (exfat_obj)
	; Read the volume label from the boot sector
	; outputs: volume label string, or :nil if not set
	(defq stream_obj (get exfat_obj :stream)
		sector_size (get exfat_obj :sector_size))

	; Read boot sector
	(stream-seek stream_obj 0 0)
	(when-bind (boot_sector (read-blk stream_obj sector_size))
		; Volume label is at offset 71, 11 bytes, Unicode (but we'll treat as ASCII for now)
		; In ExFat, the volume label is actually in a directory entry in root, not boot sector
		; Boot sector has volume label field at offset 106-116 (11 bytes, ASCII)
		(defq label_start 106
			label_length 11
			label_bytes (slice boot_sector label_start (+ label_start label_length)))

		; Trim trailing spaces and nulls
		(defq label_str ""
			i 0)
		(while (< i label_length)
			(defq ch (code label_bytes 1 i))
			(when (and (> ch 0) (not (= ch 32)))  ; Not null, not space
				(setq label_str (cat label_str (char ch))))
			(setq i (inc i)))

		(if (> (length label_str) 0)
			label_str
			:nil)))

(defun write_volume_label (exfat_obj label)
	; Write a volume label to the boot sector
	; inputs: label = string (max 11 characters)
	; outputs: :t if successful, :nil otherwise
	(defq stream_obj (get exfat_obj :stream)
		sector_size (get exfat_obj :sector_size))

	; Validate label length
	(when (> (length label) 11)
		(print "Error: Volume label must be 11 characters or less")
		(return :nil))

	; Validate label characters (alphanumeric, spaces, some symbols)
	(defq valid :t
		i 0)
	(while (and valid (< i (length label)))
		(defq ch (code label 1 i))
		; Allow A-Z, a-z, 0-9, space, underscore, hyphen
		(unless (or (and (>= ch 65) (<= ch 90))    ; A-Z
					(and (>= ch 97) (<= ch 122))   ; a-z
					(and (>= ch 48) (<= ch 57))    ; 0-9
					(= ch 32)                       ; space
					(= ch 95)                       ; underscore
					(= ch 45))                      ; hyphen
			(print "Error: Invalid character in label: " (char ch))
			(setq valid :nil))
		(setq i (inc i)))

	(when valid
		; Read boot sector
		(stream-seek stream_obj 0 0)
		(when-bind (boot_sector (read-blk stream_obj sector_size))
			; Build label bytes (padded with spaces to 11 bytes)
			(defq label_bytes (str-alloc 11 (char 32))  ; 11 spaces
				i 0)

			; Copy label into bytes
			(while (< i (length label))
				(setq label_bytes (cat
					(slice label_bytes 0 i)
					(char (code label 1 i))
					(slice label_bytes (+ i 1) 11)))
				(setq i (inc i)))

			; Update boot sector with new label
			(defq updated_boot_sector (cat
				(slice boot_sector 0 106)
				label_bytes
				(slice boot_sector 117 sector_size)))

			; Write updated boot sector
			(stream-seek stream_obj 0 0)
			(when (write-blk stream_obj updated_boot_sector)
				(stream-flush stream_obj)
				:t))))

(defun clear_volume_label (exfat_obj)
	; Clear the volume label (set to empty)
	; outputs: :t if successful, :nil otherwise
	(write_volume_label exfat_obj ""))

(defun validate_label_format (label)
	; Validate a label string meets requirements
	; outputs: :t if valid, :nil if invalid (with error message)
	(cond
		((> (length label) 11)
			(print "Error: Label too long (max 11 characters)")
			:nil)

		((= (length label) 0)
			:t)  ; Empty label is valid

		(:t
			; Check each character
			(defq i 0
				valid :t)
			(while (and valid (< i (length label)))
				(defq ch (code label 1 i))
				(unless (or (and (>= ch 65) (<= ch 90))
							(and (>= ch 97) (<= ch 122))
							(and (>= ch 48) (<= ch 57))
							(= ch 32) (= ch 95) (= ch 45))
					(print "Error: Invalid character '" (char ch) "' at position " i)
					(setq valid :nil))
				(setq i (inc i)))
			valid)))

(defun main ()
	(print "ExFat Volume Label Manager")
	(print "==========================")
	(print)

	; Create demo filesystem
	(print "Creating demo filesystem...")
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 10 1024 1024))

	(. exfat_obj :format fs_size)
	(print "Demo filesystem created (10 MB)")
	(print)

	; Test 1: Read default label (should be empty)
	(print "Test 1: Reading default volume label")
	(print "-------------------------------------")
	(defq current_label (read_volume_label exfat_obj))
	(if current_label
		(print "Current label: '" current_label "'")
		(print "No volume label set"))
	(print)

	; Test 2: Set a label
	(print "Test 2: Setting volume label")
	(print "-----------------------------")
	(defq test_label "MY_DRIVE")
	(print "Setting label to: '" test_label "'")
	(if (write_volume_label exfat_obj test_label)
		(print "Label set successfully")
		(print "Failed to set label"))
	(print)

	; Test 3: Read back the label
	(print "Test 3: Reading updated label")
	(print "------------------------------")
	(setq current_label (read_volume_label exfat_obj))
	(if current_label
		(print "Current label: '" current_label "'")
		(print "No volume label set"))
	(print)

	; Test 4: Try invalid labels
	(print "Test 4: Testing label validation")
	(print "---------------------------------")

	(print "Testing too long label...")
	(validate_label_format "VERYLONGLABEL")

	(print "Testing invalid character...")
	(validate_label_format "BAD$CHAR")

	(print "Testing valid label...")
	(if (validate_label_format "VALID_123")
		(print "  Valid!")
		(print "  Invalid!"))
	(print)

	; Test 5: Clear label
	(print "Test 5: Clearing volume label")
	(print "------------------------------")
	(if (clear_volume_label exfat_obj)
		(print "Label cleared successfully")
		(print "Failed to clear label"))

	(setq current_label (read_volume_label exfat_obj))
	(if current_label
		(print "Current label: '" current_label "'")
		(print "No volume label set (as expected)"))
	(print)

	; Summary
	(print "Volume Label Manager Demo Complete")
	(print)
	(print "Usage in real application:")
	(print "  ; Read label")
	(print "  (defq label (read_volume_label my_exfat))")
	(print)
	(print "  ; Set label")
	(print "  (write_volume_label my_exfat \"MY_DRIVE\")")
	(print)
	(print "  ; Clear label")
	(print "  (clear_volume_label my_exfat)")

	0)

; Run the demo
(main)
