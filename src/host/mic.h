#ifndef HOST_MIC_H
#define HOST_MIC_H

// The microphone of the host AUDIO drivers, what they share. Nothing here
// knows of SDL, or of any other library. What is heard is 16 bit mono at
// MIC_RATE, the rate of a voice, and is read by the Audio service in
// pieces as it comes.
//
// A driver that reads the device on a thread of its own puts what it reads
// in the ring, mic_ring_put, and the service takes it, mic_ring_take, the
// driver holding a lock of its own round both. When the ring is full the
// oldest is let go, a service that has not read for a while hears what is
// now.
//
// A desktop that is never seen, -hidden, the one a test makes, hears no
// device: it hears a tone, MIC_TONE_HZ, made as time goes as a device would
// make it, so a test can know what it should get.

#include <stdint.h>
#include <math.h>

#define MIC_RATE 16000
#define MIC_HOLD (MIC_RATE * 2)
#define MIC_TONE_HZ 440
#define MIC_TONE_LEVEL 8192

static int16_t mic_ring[MIC_HOLD];
static int mic_head = 0, mic_count = 0;

static void mic_ring_reset()
{
	mic_head = 0;
	mic_count = 0;
}

static void mic_ring_put(const int16_t *samples, int frames)
{
	for (int i = 0; i < frames; ++i)
	{
		if (mic_count == MIC_HOLD)
		{
			mic_head = (mic_head + 1) % MIC_HOLD;
			mic_count--;
		}
		mic_ring[(mic_head + mic_count) % MIC_HOLD] = samples[i];
		mic_count++;
	}
}

static int mic_ring_take(int16_t *samples, int max)
{
	int n = mic_count < max ? mic_count : max;
	for (int i = 0; i < n; ++i)
	{
		samples[i] = mic_ring[mic_head];
		mic_head = (mic_head + 1) % MIC_HOLD;
	}
	mic_count -= n;
	return n;
}

// the tone, as many samples as time has made since it was last read, no
// more than max, and no more than the ring would have held. The time is
// the driver's, in microseconds, from its own clock

static uint64_t mic_tone_start = 0;
static uint64_t mic_tone_made = 0;

static void mic_tone_reset(uint64_t now_us)
{
	mic_tone_start = now_us;
	mic_tone_made = 0;
}

static int mic_tone_take(int16_t *samples, int max, uint64_t now_us)
{
	uint64_t due = (now_us - mic_tone_start) * MIC_RATE / 1000000;
	if (due - mic_tone_made > MIC_HOLD) mic_tone_made = due - MIC_HOLD;
	int n = (int)(due - mic_tone_made);
	if (n > max) n = max;
	for (int i = 0; i < n; ++i)
	{
		uint64_t at = mic_tone_made + i;
		samples[i] = (int16_t)(MIC_TONE_LEVEL * sinf(6.2831853f * MIC_TONE_HZ * (float)(at % MIC_RATE) / MIC_RATE));
	}
	mic_tone_made += n;
	return n;
}

#endif
