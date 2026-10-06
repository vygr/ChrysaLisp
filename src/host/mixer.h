#ifndef HOST_MIXER_H
#define HOST_MIXER_H

// The sound mixer of the host AUDIO drivers. Nothing here knows of SDL, or
// of any other library. Sounds are held as float stereo at the mix rate,
// up to 32 of them play at once, each with its pan, and the mix is limited,
// not clipped.
//
// A driver gives three things. It opens a device that plays float stereo at
// MIXER_RATE, and when the device wants more it calls mixer_mix, most likely
// on a thread of the host. It reads a sound file into float stereo at
// MIXER_RATE for mixer_add. And it holds a lock of its own round every call
// here, mixer_mix as well, there is no locking in the mixer.
//
// The mixing is on a thread of the host and not a task of ChrysaLisp for
// good reason. A task is only run when the tasks before it let go, and a
// device that is not fed in time is heard.

#include <stdint.h>
#include <string.h>
#include <math.h>

#define MIXER_MAX_SFX 256
#define MIXER_MAX_VOICES 32
#define MIXER_RATE 44100
#define MIXER_CHANNELS 2

// a sound effect, held as float stereo at the mix rate
struct MixerSound
{
	uint32_t handle;
	float *samples;
	int frames;
};

// a sound effect that is playing
struct MixerVoice
{
	const MixerSound *sfx;
	int position;
	float left, right;
	bool paused;
	uint64_t started;
};

static MixerSound mixer_sounds[MIXER_MAX_SFX];
static int mixer_sound_count = 0;
static uint32_t mixer_next_handle = 0x1000;
static MixerVoice mixer_voices[MIXER_MAX_VOICES];
static uint64_t mixer_play_count = 0;

// the limiter. Up to the knee, 90% of full scale, the mix is left as it
// is. Over the knee it is eased in under full scale, the further over
// the harder, and never reaches it. The gain of the whole mix is brought
// down at once for a peak, and is then let back up slowly, so a loud
// moment is turned down, not clipped.
#define MIXER_LIMIT_KNEE 0.9f
#define MIXER_LIMIT_RELEASE (1.0f / (0.25f * MIXER_RATE))
static float mixer_limiter_gain = 1.0f;

static float mixer_limiter_want(float peak)
{
	// the gain that takes a peak to where it should be
	if (peak <= MIXER_LIMIT_KNEE) return 1.0f;
	const float room = 1.0f - MIXER_LIMIT_KNEE;
	return (MIXER_LIMIT_KNEE + room * (1.0f - expf((MIXER_LIMIT_KNEE - peak) / room))) / peak;
}

static const MixerSound *mixer_find(uint32_t handle)
{
	for (int i = 0; i < mixer_sound_count; ++i)
	{
		if (mixer_sounds[i].handle == handle) return &mixer_sounds[i];
	}
	return nullptr;
}

// no sounds, and nothing playing. It does not free the samples, see mixer_take

static void mixer_reset()
{
	memset(mixer_voices, 0, sizeof(mixer_voices));
	mixer_sound_count = 0;
	mixer_next_handle = 0x1000;
	mixer_limiter_gain = 1.0f;
}

// the next frames of the mix, float stereo, each voice added in and the
// whole limited. This is what the device is fed.

static void mixer_mix(float *mix, int frames)
{
	memset(mix, 0, sizeof(float) * MIXER_CHANNELS * frames);
	for (int v = 0; v < MIXER_MAX_VOICES; ++v)
	{
		MixerVoice *voice = &mixer_voices[v];
		if (!voice->sfx || voice->paused) continue;
		int left = voice->sfx->frames - voice->position;
		int count = left < frames ? left : frames;
		const float *src = voice->sfx->samples + voice->position * MIXER_CHANNELS;
		for (int i = 0; i < count; ++i)
		{
			mix[i * 2] += src[i * 2] * voice->left;
			mix[i * 2 + 1] += src[i * 2 + 1] * voice->right;
		}
		voice->position += count;
		if (voice->position >= voice->sfx->frames) voice->sfx = nullptr;
	}
	for (int i = 0; i < frames; ++i)
	{
		float l = mix[i * 2], r = mix[i * 2 + 1];
		float peak = l < 0.0f ? -l : l;
		float peak_r = r < 0.0f ? -r : r;
		if (peak_r > peak) peak = peak_r;
		float want = mixer_limiter_want(peak);
		if (want < mixer_limiter_gain) mixer_limiter_gain = want;
		else mixer_limiter_gain += (want - mixer_limiter_gain) * MIXER_LIMIT_RELEASE;
		mix[i * 2] = l * mixer_limiter_gain;
		mix[i * 2 + 1] = r * mixer_limiter_gain;
	}
}

// a sound, float stereo at the mix rate. The mixer keeps the samples till
// mixer_remove or mixer_take hands them back. Returns its handle, or 0 if
// there is no room for another.

static uint32_t mixer_add(float *samples, int frames)
{
	if (mixer_sound_count >= MIXER_MAX_SFX) return 0;
	uint32_t handle = mixer_next_handle++;
	mixer_sounds[mixer_sound_count].handle = handle;
	mixer_sounds[mixer_sound_count].samples = samples;
	mixer_sounds[mixer_sound_count].frames = frames;
	mixer_sound_count++;
	return handle;
}

// play a sound, pan is -255 (full left) to 255 (full right), 0 is centre.
// Returns 0, or -1 for a handle that is not a sound.

static int mixer_play(uint32_t handle, int pan)
{
	const MixerSound *sfx = mixer_find(handle);
	if (!sfx) return -1;
	// a free voice, or else the one that has played the longest
	MixerVoice *voice = &mixer_voices[0];
	for (int v = 0; v < MIXER_MAX_VOICES; ++v)
	{
		if (!mixer_voices[v].sfx)
		{
			voice = &mixer_voices[v];
			break;
		}
		if (mixer_voices[v].started < voice->started) voice = &mixer_voices[v];
	}
	if (pan < -255) pan = -255;
	if (pan > 255) pan = 255;
	voice->sfx = sfx;
	voice->position = 0;
	voice->left = (pan > 0 ? 255 - pan : 255) / 255.0f;
	voice->right = (pan < 0 ? 255 + pan : 255) / 255.0f;
	voice->paused = false;
	voice->started = ++mixer_play_count;
	return 0;
}

// every voice that is playing this sound, 1 pause, 0 resume, -1 stop.
// Returns 0, -1 for a handle that is not a sound, -2 if it is not playing.

static int mixer_change(uint32_t handle, int state)
{
	const MixerSound *sfx = mixer_find(handle);
	if (!sfx) return -1;
	int result = -2;
	for (int v = 0; v < MIXER_MAX_VOICES; ++v)
	{
		if (mixer_voices[v].sfx != sfx) continue;
		result = 0;
		if (state == -1) mixer_voices[v].sfx = nullptr;
		else mixer_voices[v].paused = state == 1;
	}
	return result;
}

// stop a sound and forget it. Returns its samples, for the driver to free,
// or 0 for a handle that is not a sound.

static float *mixer_remove(uint32_t handle)
{
	for (int i = 0; i < mixer_sound_count; ++i)
	{
		if (mixer_sounds[i].handle != handle) continue;
		// stop it, then swap with the last element to keep the array packed
		for (int v = 0; v < MIXER_MAX_VOICES; ++v)
		{
			if (mixer_voices[v].sfx == &mixer_sounds[i]) mixer_voices[v].sfx = nullptr;
			else if (mixer_voices[v].sfx == &mixer_sounds[mixer_sound_count - 1]) mixer_voices[v].sfx = &mixer_sounds[i];
		}
		float *samples = mixer_sounds[i].samples;
		mixer_sounds[i] = mixer_sounds[mixer_sound_count - 1];
		mixer_sound_count--;
		return samples;
	}
	return nullptr;
}

// stop everything and forget a sound, any one. Returns its samples, for the
// driver to free, or 0 when there are none left. For when the driver closes.

static float *mixer_take()
{
	memset(mixer_voices, 0, sizeof(mixer_voices));
	if (mixer_sound_count == 0) return nullptr;
	return mixer_sounds[--mixer_sound_count].samples;
}

#endif
