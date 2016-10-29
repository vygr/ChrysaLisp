%ifndef CLASS_TEXT_1234
%define CLASS_TEXT_1234

%include 'class/class_view.inc'

def_class text, view
def_method create, class/text/create
def_method new, class/text/new
def_method init, class/text/init
def_method set_font, class/text/set_font, static, {r0, r1, r2}
def_method set_text, class/text/set_text, static, {r0, r1}
def_method get_text, class/text/get_text, static, {r0}, {r1}
def_method set_text_color, class/text/set_text_color, static, {r0, r1}

def_method deinit, class/text/deinit, override
def_method pref_size, class/text/pref_size, override
def_method draw, class/text/draw, override

def_struc text, view
	ptr text_font
	ptr text_string
	ulong text_text_color
	ptr text_words
def_struc_end

%endif
