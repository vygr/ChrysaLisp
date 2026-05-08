#if defined(_HOST_AUDIO)
#if _HOST_AUDIO == 0

#include <SDL.h>
#include <SDL_mixer.h>
#include <SDL_rwops.h>

#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define MAX_SFX 256

struct SoundEffect {
    uint32_t handle;
    Mix_Chunk* chunk;
};

static SoundEffect soundEffects[MAX_SFX];
static int sfxCount = 0;
static uint32_t nextHandle = 0x1000;

void logSDLError(const char* msg) {
    fprintf(stderr, "%s error: %s\n", msg, Mix_GetError());
}

int host_audio_init()
{
	SDL_SetMainReady();
    if (SDL_Init(SDL_INIT_AUDIO) < 0) {
        logSDLError("SDL_Init");
        return -1;
    }

    if (Mix_OpenAudio(44100, MIX_DEFAULT_FORMAT, 2, 2048) < 0) {
        logSDLError("Mix_OpenAudio");
        return -1;
    }

    sfxCount = 0;
    nextHandle = 0x1000;
    return 0;
}

int host_audio_deinit()
{
    for (int i = 0; i < sfxCount; ++i) {
        Mix_FreeChunk(soundEffects[i].chunk);
    }
    sfxCount = 0;
    Mix_CloseAudio();
    SDL_QuitSubSystem(SDL_INIT_AUDIO);

    return (0);
}

uint32_t host_audio_add_sfx(const char* filePath) {
    const char* ext = strrchr(filePath, '.');
    if (!ext || (strcmp(ext, ".wav") != 0 && strcmp(ext, ".WAV") != 0)) {
        logSDLError("Invalid file extension. Only .wav files are supported.");
        return -1;
    }

    if (sfxCount >= MAX_SFX) {
        logSDLError("Maximum number of sound effects reached.");
        return -1;
    }

    Mix_Chunk* chunk = Mix_LoadWAV(filePath);
    if (!chunk) {
        logSDLError("Mix_LoadWAV");
        return -1;
    }

    uint32_t handle = nextHandle++;
    soundEffects[sfxCount].handle = handle;
    soundEffects[sfxCount].chunk = chunk;
    sfxCount++;
    return handle;
}

int host_audio_play_sfx(uint32_t handle) {
    for (int i = 0; i < sfxCount; ++i) {
        if (soundEffects[i].handle == handle) {
            Mix_PlayChannel(-1, soundEffects[i].chunk, 0); // Play on any available channel
            return 0;
        }
    }
    
    logSDLError("Invalid handle");
    return -1;
}

int host_audio_change_sfx(uint32_t handle, int state) {
    bool found = false;
    for (int i = 0; i < sfxCount; ++i) {
        if (soundEffects[i].handle == handle) {
            found = true;
            break;
        }
    }

    if (!found) {
        logSDLError("Invalid handle");
        return -1;
    }

    int channel = Mix_Playing(-1);
    if (channel == -1) {
        logSDLError("No channel is currently playing");
        return -2;
    }

    switch (state) {
    case 1: // Pause
        Mix_Pause(channel);
        break;
    case 0: // Resume
        Mix_Resume(channel);
        break;
    case -1: // Stop
        Mix_HaltChannel(channel);
        break;
    default:
        logSDLError("Invalid state");
    }

    return 0;
}

int host_audio_remove_sfx(uint32_t handle) {
    for (int i = 0; i < sfxCount; ++i) {
        if (soundEffects[i].handle == handle) {
            Mix_FreeChunk(soundEffects[i].chunk);
            // Swap with the last element to keep array packed
            soundEffects[i] = soundEffects[sfxCount - 1];
            sfxCount--;
            return 0;
        }
    }
    
    logSDLError("Invalid handle");
    return -1;
}

void (*host_audio_funcs[]) = {
	(void*)host_audio_init,
	(void*)host_audio_deinit,
    (void*)host_audio_add_sfx,
    (void*)host_audio_play_sfx,
    (void*)host_audio_change_sfx,
    (void*)host_audio_remove_sfx,
};

#endif
#endif