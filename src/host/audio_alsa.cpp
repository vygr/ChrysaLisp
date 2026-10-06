#if defined(_HOST_AUDIO)
#if _HOST_AUDIO == 2

// ALSA AUDIO driver, for a Linux GUI with no SDL under it, the frame
// buffer. ALSA gives the device. The mixing is done by the mixer in
// mixer.h, and a wav file is read by wav.h, neither of which know of ALSA.
// A thread of its own feeds the device, and holds the lock while it mixes.

#include <alsa/asoundlib.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include "mixer.h"
#include "wav.h"

#define FEED_FRAMES 512

static snd_pcm_t *pcm = nullptr;
static pthread_t feed_thread;
static pthread_mutex_t feed_lock = PTHREAD_MUTEX_INITIALIZER;
static volatile bool feeding = false;

// the device is fed 16 bit samples, a write waits till there is room

static void *feed(void *arg)
{
	float mix[FEED_FRAMES * MIXER_CHANNELS];
	int16_t out[FEED_FRAMES * MIXER_CHANNELS];
	while (feeding)
	{
		pthread_mutex_lock(&feed_lock);
		mixer_mix(mix, FEED_FRAMES);
		pthread_mutex_unlock(&feed_lock);
		for (int i = 0; i < FEED_FRAMES * MIXER_CHANNELS; ++i)
		{
			float s = mix[i] * 32767.0f;
			out[i] = (int16_t)(s > 32767.0f ? 32767.0f : (s < -32768.0f ? -32768.0f : s));
		}
		const int16_t *at = out;
		snd_pcm_sframes_t left = FEED_FRAMES;
		while (left > 0 && feeding)
		{
			snd_pcm_sframes_t n = snd_pcm_writei(pcm, at, left);
			// an underrun, or the device was suspended, is got over and tried again
			if (n < 0) n = snd_pcm_recover(pcm, (int)n, 1);
			if (n < 0) break;
			at += n * MIXER_CHANNELS;
			left -= n;
		}
	}
	return nullptr;
}

int host_audio_init()
{
	// the default device, with a 50ms buffer
	if (snd_pcm_open(&pcm, "default", SND_PCM_STREAM_PLAYBACK, 0) < 0)
	{
		pcm = nullptr;
		return -1;
	}
	if (snd_pcm_set_params(pcm, SND_PCM_FORMAT_S16_LE, SND_PCM_ACCESS_RW_INTERLEAVED,
		MIXER_CHANNELS, MIXER_RATE, 1, 50000) < 0)
	{
		snd_pcm_close(pcm);
		pcm = nullptr;
		return -1;
	}
	mixer_reset();
	feeding = true;
	if (pthread_create(&feed_thread, nullptr, feed, nullptr))
	{
		feeding = false;
		snd_pcm_close(pcm);
		pcm = nullptr;
		return -1;
	}
	return 0;
}

int host_audio_deinit()
{
	if (pcm)
	{
		feeding = false;
		pthread_join(feed_thread, nullptr);
		snd_pcm_close(pcm);
		pcm = nullptr;
	}
	while (float *samples = mixer_take()) free(samples);
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
	int frames = 0;
	float *samples = wav_load(filePath, &frames);
	if (!samples)
	{
		fprintf(stderr, "Can not read wav file %s\n", filePath);
		return -1;
	}
	pthread_mutex_lock(&feed_lock);
	uint32_t handle = mixer_add(samples, frames);
	pthread_mutex_unlock(&feed_lock);
	if (!handle)
	{
		free(samples);
		fprintf(stderr, "Maximum number of sound effects reached.\n");
		return -1;
	}
	return handle;
}

int host_audio_play_sfx(uint32_t handle, int pan)
{
	// pan is -255 (full left) to 255 (full right), 0 is centre
	if (!pcm) return -1;
	pthread_mutex_lock(&feed_lock);
	int result = mixer_play(handle, pan);
	pthread_mutex_unlock(&feed_lock);
	if (result == -1) fprintf(stderr, "Invalid handle\n");
	return result;
}

int host_audio_change_sfx(uint32_t handle, int state)
{
	// every voice that is playing this sound, 1 pause, 0 resume, -1 stop
	if (!pcm) return -1;
	if (state < -1 || state > 1)
	{
		fprintf(stderr, "Invalid state\n");
		return 0;
	}
	pthread_mutex_lock(&feed_lock);
	int result = mixer_change(handle, state);
	pthread_mutex_unlock(&feed_lock);
	if (result == -1) fprintf(stderr, "Invalid handle\n");
	return result;
}

int host_audio_remove_sfx(uint32_t handle)
{
	pthread_mutex_lock(&feed_lock);
	float *samples = mixer_remove(handle);
	pthread_mutex_unlock(&feed_lock);
	if (!samples)
	{
		fprintf(stderr, "Invalid handle\n");
		return -1;
	}
	free(samples);
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
