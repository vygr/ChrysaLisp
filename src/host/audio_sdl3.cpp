#if defined(_HOST_AUDIO)
#if _HOST_AUDIO == 1

// SDL3 AUDIO driver. SDL3 gives the device and reads a wav file, the
// mixing is done here, so there is no mixer library to depend on.

#include <SDL3/SDL.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define MAX_SFX 256
#define MAX_VOICES 32
#define MIX_RATE 44100
#define MIX_CHANNELS 2

// a sound effect, held as float stereo at the mix rate
struct SoundEffect
{
	uint32_t handle;
	float *samples;
	int frames;
};

// a sound effect that is playing
struct Voice
{
	const SoundEffect *sfx;
	int position;
	float left, right;
	bool paused;
	uint64_t started;
};

static SoundEffect soundEffects[MAX_SFX];
static int sfxCount = 0;
static uint32_t nextHandle = 0x1000;
static Voice voices[MAX_VOICES];
static uint64_t playCount = 0;
static SDL_AudioStream *stream = nullptr;

// the limiter. Up to the knee, 90% of full scale, the mix is left as it
// is. Over the knee it is eased in under full scale, the further over
// the harder, and never reaches it. The gain of the whole mix is brought
// down at once for a peak, and is then let back up slowly, so a loud
// moment is turned down, not clipped.
#define LIMIT_KNEE 0.9f
#define LIMIT_RELEASE (1.0f / (0.25f * MIX_RATE))
static float limiter_gain = 1.0f;

static float limiter_want(float peak)
{
	// the gain that takes a peak to where it should be
	if (peak <= LIMIT_KNEE) return 1.0f;
	const float room = 1.0f - LIMIT_KNEE;
	return (LIMIT_KNEE + room * (1.0f - SDL_expf((LIMIT_KNEE - peak) / room))) / peak;
}

static void logAudioError(const char *msg)
{
	fprintf(stderr, "%s error: %s\n", msg, SDL_GetError());
}

// SDL asks for more, on its audio thread, with the stream locked.
// Each voice is added to the mix, and the mix is limited.

static void SDLCALL mix_callback(void *userdata, SDL_AudioStream *s, int additional_amount, int total_amount)
{
	float mix[1024 * MIX_CHANNELS];
	int frames = additional_amount / (int)(sizeof(float) * MIX_CHANNELS);
	while (frames > 0)
	{
		int n = frames < 1024 ? frames : 1024;
		memset(mix, 0, sizeof(float) * MIX_CHANNELS * n);
		for (int v = 0; v < MAX_VOICES; ++v)
		{
			Voice *voice = &voices[v];
			if (!voice->sfx || voice->paused) continue;
			int left = voice->sfx->frames - voice->position;
			int count = left < n ? left : n;
			const float *src = voice->sfx->samples + voice->position * MIX_CHANNELS;
			for (int i = 0; i < count; ++i)
			{
				mix[i * 2] += src[i * 2] * voice->left;
				mix[i * 2 + 1] += src[i * 2 + 1] * voice->right;
			}
			voice->position += count;
			if (voice->position >= voice->sfx->frames) voice->sfx = nullptr;
		}
		for (int i = 0; i < n; ++i)
		{
			float l = mix[i * 2], r = mix[i * 2 + 1];
			float peak = l < 0.0f ? -l : l;
			float peak_r = r < 0.0f ? -r : r;
			if (peak_r > peak) peak = peak_r;
			float want = limiter_want(peak);
			if (want < limiter_gain) limiter_gain = want;
			else limiter_gain += (want - limiter_gain) * LIMIT_RELEASE;
			mix[i * 2] = l * limiter_gain;
			mix[i * 2 + 1] = r * limiter_gain;
		}
		SDL_PutAudioStreamData(s, mix, (int)(sizeof(float) * MIX_CHANNELS * n));
		frames -= n;
	}
}

static const SoundEffect *find_sfx(uint32_t handle)
{
	for (int i = 0; i < sfxCount; ++i)
	{
		if (soundEffects[i].handle == handle) return &soundEffects[i];
	}
	return nullptr;
}

int host_audio_init()
{
	if (!SDL_Init(SDL_INIT_AUDIO))
	{
		logAudioError("SDL_Init");
		return -1;
	}
	SDL_AudioSpec spec = {SDL_AUDIO_F32, MIX_CHANNELS, MIX_RATE};
	memset(voices, 0, sizeof(voices));
	sfxCount = 0;
	nextHandle = 0x1000;
	limiter_gain = 1.0f;
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
	for (int i = 0; i < sfxCount; ++i) SDL_free(soundEffects[i].samples);
	sfxCount = 0;
	memset(voices, 0, sizeof(voices));
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
	if (sfxCount >= MAX_SFX)
	{
		fprintf(stderr, "Maximum number of sound effects reached.\n");
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
	SDL_AudioSpec dst_spec = {SDL_AUDIO_F32, MIX_CHANNELS, MIX_RATE};
	bool ok = SDL_ConvertAudioSamples(&src_spec, src_data, (int)src_len, &dst_spec, &dst_data, &dst_len);
	SDL_free(src_data);
	if (!ok)
	{
		logAudioError("SDL_ConvertAudioSamples");
		return -1;
	}
	uint32_t handle = nextHandle++;
	if (stream) SDL_LockAudioStream(stream);
	soundEffects[sfxCount].handle = handle;
	soundEffects[sfxCount].samples = (float*)dst_data;
	soundEffects[sfxCount].frames = dst_len / (int)(sizeof(float) * MIX_CHANNELS);
	sfxCount++;
	if (stream) SDL_UnlockAudioStream(stream);
	return handle;
}

int host_audio_play_sfx(uint32_t handle, int pan)
{
	// pan is -255 (full left) to 255 (full right), 0 is centre
	if (!stream) return -1;
	SDL_LockAudioStream(stream);
	const SoundEffect *sfx = find_sfx(handle);
	if (sfx)
	{
		// a free voice, or else the one that has played the longest
		Voice *voice = &voices[0];
		for (int v = 0; v < MAX_VOICES; ++v)
		{
			if (!voices[v].sfx)
			{
				voice = &voices[v];
				break;
			}
			if (voices[v].started < voice->started) voice = &voices[v];
		}
		if (pan < -255) pan = -255;
		if (pan > 255) pan = 255;
		voice->sfx = sfx;
		voice->position = 0;
		voice->left = (pan > 0 ? 255 - pan : 255) / 255.0f;
		voice->right = (pan < 0 ? 255 + pan : 255) / 255.0f;
		voice->paused = false;
		voice->started = ++playCount;
	}
	SDL_UnlockAudioStream(stream);
	if (!sfx)
	{
		fprintf(stderr, "Invalid handle\n");
		return -1;
	}
	return 0;
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
	int result = -1;
	SDL_LockAudioStream(stream);
	const SoundEffect *sfx = find_sfx(handle);
	if (sfx)
	{
		result = -2;
		for (int v = 0; v < MAX_VOICES; ++v)
		{
			if (voices[v].sfx != sfx) continue;
			result = 0;
			if (state == -1) voices[v].sfx = nullptr;
			else voices[v].paused = state == 1;
		}
	}
	SDL_UnlockAudioStream(stream);
	if (result == -1) fprintf(stderr, "Invalid handle\n");
	return result;
}

int host_audio_remove_sfx(uint32_t handle)
{
	float *samples = nullptr;
	if (stream) SDL_LockAudioStream(stream);
	for (int i = 0; i < sfxCount; ++i)
	{
		if (soundEffects[i].handle != handle) continue;
		// stop it, then swap with the last element to keep the array packed
		for (int v = 0; v < MAX_VOICES; ++v)
		{
			if (voices[v].sfx == &soundEffects[i]) voices[v].sfx = nullptr;
			else if (voices[v].sfx == &soundEffects[sfxCount - 1]) voices[v].sfx = &soundEffects[i];
		}
		samples = soundEffects[i].samples;
		soundEffects[i] = soundEffects[sfxCount - 1];
		sfxCount--;
		break;
	}
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
