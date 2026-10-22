// ChrysaLisp on a phone or a tablet, iOS or Android.
//
// The app is the GUI host program, main.cpp, with SDL3 under it, run in the
// VP64 emulator: neither system lets an app make code it can run, so there
// is no native boot image. The files the system reads come with the app as
// one tar, chrysalisp.tar, see mobile/bundle.sh. It is unpacked to a folder
// of the app's own the first time, and again when the app has a new one, and
// the host program is started in that folder.
//
// An app can not start another process, so it is one node, the desktop.

#include <SDL3/SDL.h>
#include <SDL3/SDL_main.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

// the host program's own main, main.cpp is built with main named this
extern int host_main(int argc, char *argv[]);

static const char *bundle_name = "chrysalisp.tar";
static const char *stamp_name = ".mobile_stamp";

// make every folder above this path
static void make_dirs(const char *path)
{
	char buf[1024];
	size_t len = strlen(path);
	if (len >= sizeof(buf)) return;
	memcpy(buf, path, len + 1);
	for (char *p = buf + 1; *p; p++)
	{
		if (*p != '/') continue;
		*p = 0;
		mkdir(buf, 0755);
		*p = '/';
	}
}

static uint64_t octal(const char *s, size_t n)
{
	uint64_t v = 0;
	for (size_t i = 0; i < n && s[i]; i++)
	{
		if (s[i] < '0' || s[i] > '7') continue;
		v = (v << 3) | (uint64_t)(s[i] - '0');
	}
	return v;
}

static bool read_all(SDL_IOStream *io, void *buf, size_t len)
{
	return SDL_ReadIO(io, buf, len) == len;
}

// the stamp the folder was last unpacked from, or "" if none
static void read_stamp(char *buf, size_t len)
{
	buf[0] = 0;
	FILE *f = fopen(stamp_name, "rb");
	if (!f) return;
	size_t n = fread(buf, 1, len - 1, f);
	buf[n] = 0;
	fclose(f);
}

// Unpack the tar to the current folder, unless it is the one that was last.
// The stamp is its first file, and is written last, so an unpack that was
// stopped part way is done again. A tar of the ustar format, files and
// folders only, see mobile/bundle.sh
static bool unpack_bundle()
{
	// in the app, Android's assets are found by the name, iOS has it in
	// the bundle's folder
	char where[1024];
#if defined(SDL_PLATFORM_ANDROID)
	snprintf(where, sizeof(where), "%s", bundle_name);
#else
	const char *base = SDL_GetBasePath();
	snprintf(where, sizeof(where), "%s%s", base ? base : "", bundle_name);
#endif
	SDL_IOStream *io = SDL_IOFromFile(where, "rb");
	if (!io)
	{
		SDL_Log("ChrysaLisp: no %s in the app: %s", bundle_name, SDL_GetError());
		return false;
	}
	static char block[512];
	static char data[65536];
	char new_stamp[512] = {0};
	char old_stamp[512];
	read_stamp(old_stamp, sizeof(old_stamp));
	bool ok = true;
	int files = 0;
	while (read_all(io, block, sizeof(block)))
	{
		if (!block[0]) break;
		char name[256];
		snprintf(name, sizeof(name), "%.155s%s%.100s", block + 345, block[345] ? "/" : "", block);
		uint64_t size = octal(block + 124, 12);
		char type = block[156];
		uint64_t padded = (size + 511) & ~(uint64_t)511;
		if (!strcmp(name, stamp_name))
		{
			size_t n = size < sizeof(new_stamp) - 1 ? (size_t)size : sizeof(new_stamp) - 1;
			if (!read_all(io, new_stamp, n)) { ok = false; break; }
			new_stamp[n] = 0;
			SDL_SeekIO(io, (Sint64)(padded - n), SDL_IO_SEEK_CUR);
			if (!strcmp(new_stamp, old_stamp))
			{
				SDL_CloseIO(io);
				SDL_Log("ChrysaLisp: files are up to date");
				return true;
			}
			SDL_Log("ChrysaLisp: unpacking files");
			continue;
		}
		if (type == '5')
		{
			char dir[260];
			snprintf(dir, sizeof(dir), "%s/", name);
			make_dirs(dir);
			continue;
		}
		if (type != '0' && type != 0)
		{
			// not a file, skip what it has
			SDL_SeekIO(io, (Sint64)padded, SDL_IO_SEEK_CUR);
			continue;
		}
		make_dirs(name);
		FILE *f = fopen(name, "wb");
		if (!f) SDL_Log("ChrysaLisp: can not write %s", name);
		uint64_t left = padded, want = size;
		while (left)
		{
			size_t n = left < sizeof(data) ? (size_t)left : sizeof(data);
			if (!read_all(io, data, n)) { ok = false; break; }
			if (f && want)
			{
				size_t w = want < n ? (size_t)want : n;
				fwrite(data, 1, w, f);
				want -= w;
			}
			left -= n;
		}
		if (f) fclose(f);
		if (!ok) break;
		++files;
	}
	SDL_CloseIO(io);
	if (ok && new_stamp[0])
	{
		FILE *f = fopen(stamp_name, "wb");
		if (f)
		{
			fputs(new_stamp, f);
			fclose(f);
		}
	}
	SDL_Log("ChrysaLisp: unpacked %d files%s", files, ok ? "" : ", the tar was cut short");
	return ok;
}

// Three fingers down at once show the keyboard of the screen, or hide it.
// The GUI has no way to say when it wants one, and a phone has no other.
static bool SDLCALL keyboard_watch(void *userdata, SDL_Event *e)
{
	(void)userdata;
	if (e->type != SDL_EVENT_FINGER_DOWN) return true;
	int count = 0;
	SDL_Finger **fingers = SDL_GetTouchFingers(e->tfinger.touchID, &count);
	SDL_free(fingers);
	if (count != 3) return true;
	SDL_Window *window = SDL_GetWindowFromID(e->tfinger.windowID);
	if (!window) window = SDL_GetKeyboardFocus();
	if (!window) return true;
	if (SDL_TextInputActive(window)) SDL_StopTextInput(window);
	else SDL_StartTextInput(window);
	return true;
}

int main(int argc, char *argv[])
{
	(void)argc;
	(void)argv;
	SDL_SetHint(SDL_HINT_ORIENTATIONS, "LandscapeLeft LandscapeRight Portrait PortraitUpsideDown");
	// a finger is a finger, and not also a mouse, the GUI is told of each
	SDL_SetHint(SDL_HINT_TOUCH_MOUSE_EVENTS, "0");
	SDL_SetHint(SDL_HINT_MOUSE_TOUCH_EVENTS, "0");

	// the folder of the app's own, it can be written to and lasts
	char *root = SDL_GetPrefPath("ChrysaLisp", "ChrysaLisp");
	if (!root || chdir(root) != 0)
	{
		SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, "ChrysaLisp", "No folder for the files of the app.", nullptr);
		return 1;
	}
	SDL_Log("ChrysaLisp: in %s", root);
	SDL_free(root);
	if (!unpack_bundle())
	{
		struct stat st;
		if (stat("obj/vp64/VP64/sys/boot_image", &st) != 0)
		{
			SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, "ChrysaLisp", "The files of the app could not be unpacked.", nullptr);
			return 1;
		}
	}

	SDL_AddEventWatch(keyboard_watch, nullptr);

	// one node, the desktop, by itself: it does not look for others on the
	// network, which a phone would ask the user about
	static char host[] = "chrysalisp";
	static char image[] = "obj/vp64/VP64/sys/boot_image";
	static char emu[] = "-e";
	static char run[] = "-run";
	static char script[] =
		"(progn (def *root_env* '*env_node_solo* :t) (node-net :full 1) (import {service/gui/app.lisp}))";
	static char *args[] = {host, image, emu, run, script, nullptr};
	return host_main(5, args);
}
