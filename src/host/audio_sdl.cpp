#if defined(_HOST_AUDIO)
#if _HOST_AUDIO == 0

#include <SDL.h>
#include <SDL_mixer.h>
#include <SDL_rwops.h>

#include <stdint.h>
#include <unordered_map>
#include <string>
#include <iostream>

struct SoundEffect {
    Mix_Chunk* chunk;
    std::string filename;
};

static std::unordered_map<uint32_t, SoundEffect> soundEffectMap;
static uint32_t nextHandle = 0x1000;

void logSDLError(const char* msg) {
    std::cerr << msg << " error: " << Mix_GetError() << std::endl;
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

    nextHandle = 0x1000;
    return 0;
}

int host_audio_deinit()
{
    for (auto& entry : soundEffectMap) {
        Mix_FreeChunk(entry.second.chunk);
    }
    soundEffectMap.clear();
    Mix_CloseAudio();
    SDL_QuitSubSystem(SDL_INIT_AUDIO);

    return (0);
}

bool hasWavExtension(const std::string& filePath) {
    std::string extension = filePath.substr(filePath.find_last_of(".") + 1);
    return extension == "wav" || extension == "WAV";
}

uint32_t host_audio_add_sfx(const char* filePath) {
    if (!hasWavExtension(filePath)) {
        logSDLError("Invalid file extension. Only .wav files are supported.");
        return -1;
    }

    Mix_Chunk* chunk = Mix_LoadWAV(filePath);
    if (!chunk) {
        logSDLError("Mix_LoadWAV");
        return 0-1;
    }

    SoundEffect soundEffect = { chunk, filePath };
    uint32_t handle = nextHandle++;
    soundEffectMap[handle] = soundEffect;
    return handle;
}

int host_audio_play_sfx(uint32_t handle) {
    if (soundEffectMap.find(handle) == soundEffectMap.end()) {
        logSDLError("Invalid handle");
        return(-1);
    }

    Mix_PlayChannel(-1, soundEffectMap[handle].chunk, 0); // Play on any available channel

    return (0);
}

int host_audio_change_sfx(uint32_t handle, int state) {
    if (soundEffectMap.find(handle) == soundEffectMap.end()) {
        logSDLError("Invalid handle");
        return (-1);
    }

    int channel = Mix_Playing(-1);
    if (channel == -1) {
        logSDLError("No channel is currently playing");
        return (-2);
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

    return (0);
}

int host_audio_remove_sfx(uint32_t handle) {
    if (soundEffectMap.find(handle) == soundEffectMap.end()) {
        logSDLError("Invalid handle");
        return (-1);
    }

    Mix_FreeChunk(soundEffectMap[handle].chunk);
    soundEffectMap.erase(handle);

    return(0);
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
