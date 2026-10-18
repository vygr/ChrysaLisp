#ifndef HOST_AEC_H
#define HOST_AEC_H

// Echo cancelling, for the microphone of the host AUDIO drivers. Nothing
// here knows of SDL, or of any other library.
//
// What the speaker plays is heard again by the microphone, a little later
// and changed by the room: on a call without headphones the far end hears
// itself. What the mixer plays is known here, the reference: the mixer
// gives it as it mixes, aec_far_put, made 16 bit mono at MIC_RATE as the
// microphone is. Each sample the microphone hears has taken from it what
// an adaptive filter of the reference says the echo of it is, aec_near,
// and what is left is the voice of the near end. The filter learns the
// room as it goes, normalised least mean squares, AEC_TAPS long, which is
// how long after it is mixed a sound may come back: the latency of the
// device out, the room, the latency of the device in.
//
// It learns only from what is echo. While the near end talks louder than
// any echo could be, the filter is left as it is, so it does not learn to
// take the near end's voice away too: a Geigel detector, AEC_HANG long.
// While nothing has been played of late it does nothing at all, and costs
// nothing.
//
// The mixer runs on a thread of the device's, the microphone is read on
// another: the reference is a ring one writes and the other reads, the
// count of what is written its only shared state, atomic.
//
// The two ends are tied by counts of samples. As the microphone opens,
// aec_reset, what is read of it and what has been mixed are taken to be
// of the same time, give or take the latency AEC_TAPS covers.

#include <stdint.h>
#include <string.h>
#include <math.h>
#include <atomic>

#ifndef MIC_RATE
#define MIC_RATE 16000
#endif

#define AEC_TAPS 4096
#define AEC_RING (MIC_RATE * 2)
#define AEC_MU 0.25f
#define AEC_HANG (MIC_RATE * 30 / 1000)
#define AEC_QUIET (MIC_RATE / 2)

// the reference, a ring of what was mixed, and how much has been. Each
// sample is kept twice, a ring apart, so the AEC_TAPS before any place are
// one run of memory, and the loops of the filter go straight along it
static float aec_far[AEC_RING * 2];
static std::atomic<uint64_t> aec_far_n(0);

// what has been mixed, at its rate, on its way to MIC_RATE
static double aec_far_step = 0.0, aec_far_at = 0.0, aec_far_sum = 0.0;
static int aec_far_parts = 0;

// the filter, and where the near end is in the reference
static float aec_w[AEC_TAPS];
static int64_t aec_offset = 0;
static uint64_t aec_near_n = 0;
static bool aec_tied = false;
static float aec_peak = 0.0f;
static int aec_hold = 0;
static int64_t aec_heard = -AEC_QUIET;

// the mixer's rate, said once
static void aec_far_rate(int rate)
{
	aec_far_step = (double)MIC_RATE / (double)rate;
}

// what was mixed, float stereo, as the mixer gives it to the device: made
// mono, and an average of the samples of each one at MIC_RATE
static void aec_far_put(const float *mix, int frames)
{
	if (aec_far_step == 0.0) return;
	uint64_t n = aec_far_n.load(std::memory_order_relaxed);
	for (int i = 0; i < frames; ++i)
	{
		aec_far_sum += (mix[i * 2] + mix[i * 2 + 1]) * 0.5f;
		aec_far_parts++;
		aec_far_at += aec_far_step;
		if (aec_far_at >= 1.0)
		{
			aec_far_at -= 1.0;
			float v = (float)(aec_far_sum / aec_far_parts);
			aec_far[n % AEC_RING] = v;
			aec_far[n % AEC_RING + AEC_RING] = v;
			aec_far_sum = 0.0;
			aec_far_parts = 0;
			n++;
		}
	}
	aec_far_n.store(n, std::memory_order_release);
}

// the microphone opened: what is heard from now is tied to what is mixed now
static void aec_reset()
{
	memset(aec_w, 0, sizeof(aec_w));
	aec_tied = false;
	aec_near_n = 0;
	aec_peak = 0.0f;
	aec_hold = 0;
	aec_heard = -AEC_QUIET;
}

static inline float aec_ref(int64_t i)
{
	return (i < 0) ? 0.0f : aec_far[i % AEC_RING];
}

// the reference before and at a place, x[0] that place and x[-k] k before
static inline const float *aec_run(int64_t at)
{
	return aec_far + (at % AEC_RING) + AEC_RING;
}

// the samples the microphone gave, the echo taken from them in place
static void aec_near(int16_t *samples, int n)
{
	if (aec_far_step == 0.0 || n <= 0) return;
	int64_t far_n = (int64_t)aec_far_n.load(std::memory_order_acquire);
	if (!aec_tied)
	{
		// the last of these is of now, as is the last mixed
		aec_offset = far_n - (int64_t)(aec_near_n + n);
		aec_tied = true;
	}
	for (int i = 0; i < n; ++i, ++aec_near_n)
	{
		// the newest of the reference this sample could hear, not newer
		// than what has been mixed, nor older than the ring keeps
		int64_t at = (int64_t)aec_near_n + aec_offset;
		if (at > far_n - 1) at = far_n - 1;
		if (at < AEC_TAPS || at < far_n - AEC_RING + AEC_TAPS) { continue; }
		float x0 = aec_ref(at);
		if (fabsf(x0) > 0.003f) aec_heard = (int64_t)aec_near_n;
		// nothing played of late, nothing to take away
		if ((int64_t)aec_near_n - aec_heard > AEC_QUIET) continue;
		float d = samples[i] / 32768.0f;
		// the estimate of the echo, and the power of the reference it is of
		const float *x = aec_run(at);
		float y = 0.0f, power = 0.0f;
		for (int k = 0; k < AEC_TAPS; ++k)
		{
			y += aec_w[k] * x[-k];
			power += x[-k] * x[-k];
		}
		// the loudest of the reference of late, falling away over the taps
		float ax0 = fabsf(x0);
		aec_peak = ax0 > aec_peak ? ax0 : aec_peak * (1.0f - 1.0f / AEC_TAPS);
		float xmax = aec_peak;
		float e = d - y;
		// the near end louder than an echo could be: talking, learn nothing
		if (fabsf(d) > 0.6f * xmax) aec_hold = AEC_HANG;
		if (aec_hold > 0) aec_hold--;
		else
		{
			float g = AEC_MU * e / (power + 1e-4f);
			for (int k = 0; k < AEC_TAPS; ++k) aec_w[k] += g * x[-k];
		}
		if (e > 1.0f) e = 1.0f;
		if (e < -1.0f) e = -1.0f;
		samples[i] = (int16_t)lrintf(e * 32767.0f);
	}
}

#endif
