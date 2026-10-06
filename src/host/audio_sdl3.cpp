#if defined(_HOST_AUDIO)
#if _HOST_AUDIO == 1

// SDL3 AUDIO driver. SDL3 gives the device and reads a wav file, the
// mixing is done by the mixer in mixer.h, which has no SDL in it, so there
// is no mixer library to depend on. The lock held round every call to the
// mixer is the lock of the stream.

#include <SDL3/SDL.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include "mixer.h"

static SDL_AudioStream *stream = nullptr;

static void logAudioError(const char *msg)
{
	fprintf(stderr, "%s error: %s\n", msg, SDL_GetError());
}

// SDL asks for more, on its audio thread, with the stream locked

static void SDLCALL mix_callback(void *userdata, SDL_AudioStream *s, int additional_amount, int total_amount)
{
	float mix[1024 * MIXER_CHANNELS];
	int frames = additional_amount / (int)(sizeof(float) * MIXER_CHANNELS);
	while (frames > 0)
	{
		int n = frames < 1024 ? frames : 1024;
		mixer_mix(mix, n);
		SDL_PutAudioStreamData(s, mix, (int)(sizeof(float) * MIXER_CHANNELS * n));
		frames -= n;
	}
}

int host_audio_init()
{
	if (!SDL_Init(SDL_INIT_AUDIO))
	{
		logAudioError("SDL_Init");
		return -1;
	}
	SDL_AudioSpec spec = {SDL_AUDIO_F32, MIXER_CHANNELS, MIXER_RATE};
	mixer_reset();
	stream = SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, &spec, mix_callback, nullptr);
	if (!stream)
	{
		logAudioError("SDL_OpenAudioDeviceStream");
		return -1;
	}
	SDL_ResumeAudioStreamDevice(stream);
	return 0;
}

int host_audio_deinit()
{
	if (stream)
	{
		SDL_DestroyAudioStream(stream);
		stream = nullptr;
	}
	while (float *samples = mixer_take()) SDL_free(samples);
	SDL_QuitSubSystem(SDL_INIT_AUDIO);
	return 0;
}

uint32_t host_audio_add_sfx(const char *filePath)
{
	const char *ext = strrchr(filePath, '.');
	if (!ext || (strcmp(ext, ".wav") != 0 && strcmp(ext, ".WAV") != 0))
	{
		fprintf(stderr, "Invalid file extension. Only .wav files are supported.\n");
		return -1;
	}
	// read it, and turn it into what the mix works in
	SDL_AudioSpec src_spec;
	Uint8 *src_data = nullptr, *dst_data = nullptr;
	Uint32 src_len = 0;
	int dst_len = 0;
	if (!SDL_LoadWAV(filePath, &src_spec, &src_data, &src_len))
	{
		logAudioError("SDL_LoadWAV");
		return -1;
	}
	SDL_AudioSpec dst_spec = {SDL_AUDIO_F32, MIXER_CHANNELS, MIXER_RATE};
	bool ok = SDL_ConvertAudioSamples(&src_spec, src_data, (int)src_len, &dst_spec, &dst_data, &dst_len);
	SDL_free(src_data);
	if (!ok)
	{
		logAudioError("SDL_ConvertAudioSamples");
		return -1;
	}
	if (stream) SDL_LockAudioStream(stream);
	uint32_t handle = mixer_add((float*)dst_data, dst_len / (int)(sizeof(float) * MIXER_CHANNELS));
	if (stream) SDL_UnlockAudioStream(stream);
	if (!handle)
	{
		SDL_free(dst_data);
		fprintf(stderr, "Maximum number of sound effects reached.\n");
		return -1;
	}
	return handle;
}

int host_audio_play_sfx(uint32_t handle, int pan)
{
	// pan is -255 (full left) to 255 (full right), 0 is centre
	if (!stream) return -1;
	SDL_LockAudioStream(stream);
	int result = mixer_play(handle, pan);
	SDL_UnlockAudioStream(stream);
	if (result == -1) fprintf(stderr, "Invalid handle\n");
	return result;
}

int host_audio_change_sfx(uint32_t handle, int state)
{
	// every voice that is playing this sound, 1 pause, 0 resume, -1 stop
	if (!stream) return -1;
	if (state < -1 || state > 1)
	{
		fprintf(stderr, "Invalid state\n");
		return 0;
	}
	SDL_LockAudioStream(stream);
	int result = mixer_change(handle, state);
	SDL_UnlockAudioStream(stream);
	if (result == -1) fprintf(stderr, "Invalid handle\n");
	return result;
}

int host_audio_remove_sfx(uint32_t handle)
{
	if (stream) SDL_LockAudioStream(stream);
	float *samples = mixer_remove(handle);
	if (stream) SDL_UnlockAudioStream(stream);
	if (!samples)
	{
		fprintf(stderr, "Invalid handle\n");
		return -1;
	}
	SDL_free(samples);
	return 0;
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
