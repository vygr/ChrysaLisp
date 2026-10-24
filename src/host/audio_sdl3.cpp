#if defined(_HOST_AUDIO)
#if _HOST_AUDIO == 1

// SDL3 AUDIO driver. SDL3 gives the device and reads a wav file, the
// mixing is done by the mixer in mixer.h, which has no SDL in it, so there
// is no mixer library to depend on. The lock held round every call to the
// mixer is the lock of the stream.
//
// The microphone is a recording stream of SDL's, which keeps what it hears
// till it is read, made 16 bit mono at MIC_RATE by SDL, see mic.h. What is
// mixed is the reference of the echo cancelling, and the echo of it is
// taken from what the microphone hears, see aec.h.

#include <SDL3/SDL.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include "mixer.h"
#include "mic.h"
#include "aec.h"

static SDL_AudioStream *stream = nullptr;
static SDL_AudioStream *mic = nullptr;
static bool mic_tone = false;

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
		aec_far_put(mix, n);
		SDL_PutAudioStreamData(s, mix, (int)(sizeof(float) * MIXER_CHANNELS * n));
		frames -= n;
	}
}

// -hidden in the args, a desktop that is never seen is not heard either:
// what is played plays as it would, at no volume. Chris, of the app draw
// test: "I can hear your Boing test... :)". Nor does it need a device: it
// plays to SDL's dummy driver, at the pace of one, on a machine with none,
// a Pi whose only sound is a screen that is off

extern char **host_argv;

static bool host_audio_hidden(void)
{
	if (host_argv) for (char **arg = host_argv; *arg; arg++) if (!strcmp(*arg, "-hidden")) return true;
	return false;
}

int host_audio_init()
{
	if (host_audio_hidden()) SDL_SetHint(SDL_HINT_AUDIO_DRIVER, "dummy");
	if (!SDL_Init(SDL_INIT_AUDIO))
	{
		logAudioError("SDL_Init");
		return -1;
	}
	SDL_AudioSpec spec = {SDL_AUDIO_F32, MIXER_CHANNELS, MIXER_RATE};
	mixer_reset();
	aec_far_rate(MIXER_RATE);
	stream = SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, &spec, mix_callback, nullptr);
	if (!stream)
	{
		logAudioError("SDL_OpenAudioDeviceStream");
		return -1;
	}
	if (host_audio_hidden()) SDL_SetAudioStreamGain(stream, 0.0f);
	SDL_ResumeAudioStreamDevice(stream);
	return 0;
}

int64_t host_audio_mic_close();

int host_audio_deinit()
{
	host_audio_mic_close();
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

// the microphone, opened, read, closed. Read gives how many samples it
// put in samples, up to max, 0 if none have come, -1 if it is not open

int64_t host_audio_mic_open()
{
	if (mic || mic_tone) return 0;
	if (host_audio_hidden())
	{
		mic_tone = true;
		mic_tone_reset(SDL_GetTicksNS() / 1000);
		return 0;
	}
	SDL_AudioSpec spec = {SDL_AUDIO_S16, 1, MIC_RATE};
	mic = SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_RECORDING, &spec, nullptr, nullptr);
	if (!mic)
	{
		logAudioError("SDL_OpenAudioDeviceStream recording");
		return -1;
	}
	SDL_ResumeAudioStreamDevice(mic);
	aec_reset();
	return 0;
}

int64_t host_audio_mic_read(int16_t *samples, int max)
{
	if (mic_tone) return mic_tone_take(samples, max, SDL_GetTicksNS() / 1000);
	if (!mic) return -1;
	int got = SDL_GetAudioStreamData(mic, samples, max * (int)sizeof(int16_t));
	if (got < 0) return -1;
	got /= (int)sizeof(int16_t);
	aec_near(samples, got);
	return got;
}

int64_t host_audio_mic_close()
{
	if (mic)
	{
		SDL_DestroyAudioStream(mic);
		mic = nullptr;
	}
	mic_tone = false;
	return 0;
}

// a stream played as it comes, see mixer.h. Open, of a rate and 1 or 2
// channels, gives its handle, 0 if there is no room or no device; write
// how many samples it took; left how many frames are still to be played

int64_t host_audio_stream_open(uint64_t rate, uint64_t channels)
{
	if (!stream) return 0;
	SDL_LockAudioStream(stream);
	uint32_t handle = mixer_stream_open(rate, channels);
	SDL_UnlockAudioStream(stream);
	return handle;
}

int64_t host_audio_stream_write(uint32_t handle, const int16_t *samples, int frames)
{
	if (!stream) return -1;
	SDL_LockAudioStream(stream);
	int result = mixer_stream_write(handle, samples, frames);
	SDL_UnlockAudioStream(stream);
	return result;
}

int64_t host_audio_stream_close(uint32_t handle)
{
	if (!stream) return -1;
	SDL_LockAudioStream(stream);
	int result = mixer_stream_close(handle);
	SDL_UnlockAudioStream(stream);
	return result;
}

int64_t host_audio_stream_left(uint32_t handle)
{
	if (!stream) return -1;
	SDL_LockAudioStream(stream);
	int result = mixer_stream_left(handle);
	SDL_UnlockAudioStream(stream);
	return result;
}

void (*host_audio_funcs[]) = {
	(void*)host_audio_init,
	(void*)host_audio_deinit,
	(void*)host_audio_add_sfx,
	(void*)host_audio_play_sfx,
	(void*)host_audio_change_sfx,
	(void*)host_audio_remove_sfx,
	(void*)host_audio_mic_open,
	(void*)host_audio_mic_read,
	(void*)host_audio_mic_close,
	(void*)host_audio_stream_open,
	(void*)host_audio_stream_write,
	(void*)host_audio_stream_close,
	(void*)host_audio_stream_left,
};

#endif
#endif
