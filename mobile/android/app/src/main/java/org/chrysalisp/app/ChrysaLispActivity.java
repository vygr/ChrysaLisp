package org.chrysalisp.app;

import org.libsdl.app.SDLActivity;

// SDL's activity, it loads libSDL3.so and libmain.so and runs SDL_main of
// mobile/src/mobile_main.cpp on a thread of its own
public class ChrysaLispActivity extends SDLActivity {
    @Override
    protected String[] getLibraries() {
        return new String[] { "SDL3", "main" };
    }
}
