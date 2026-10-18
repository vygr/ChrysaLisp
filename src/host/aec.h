#ifndef HOST_AEC_H
#define HOST_AEC_H

// Echo cancelling, for the microphone of the host AUDIO drivers. Nothing
// here knows of SDL, or of any other library.
//
// What the speaker plays is heard again by the microphone, a little later
// and changed by the room: on a call without headphones the far end hears
// itself. What the mixer plays is known here, the reference: the mixer
// gives it as it mixes, aec_far_put, made mono at MIC_RATE as the
// microphone is. What the microphone hears has taken from it what a filter
// of the reference says the echo of it is, aec_near, and what is left is
// the voice of the near end.
//
// The filter is in blocks of AEC_N samples, in the frequency domain, AEC_M
// blocks long, which is how long after it is mixed a sound may come back:
// the latency of the device out, the room, the latency of the device in.
// Each band of frequencies learns at its own rate, so speech, loud in some
// bands and quiet in others, is learnt in a second or two, not tens.
//
// There are two filters. The one in the background learns, the one in the
// foreground takes the echo away. The background one is copied to the
// foreground only when what it leaves has been less for a while, and is
// put back as the foreground one when it has done worse: while the near
// end talks, a voice the filter can never be of, what it learns is wrong,
// and the foreground never takes it.
//
// How fast it learns is from how much of what is left is still echo: the
// leak, the part of the echo the filter misses, found from how what is
// left goes up and down with the echo it took away. While the near end
// talks what is left is mostly not echo, and it learns little.
//
// Last, what echo is still left, the leak of each band, is taken down by a
// gain of that band, not below AEC_FLOOR, in blocks that overlap by half.
// So what is heard comes out two blocks, AEC_N * 2 samples, later.
//
// The mixer runs on a thread of the device's, the microphone is read on
// another: the reference is a ring one writes and the other reads, the
// count of what is written its only shared state, atomic. The two ends are
// tied by counts of samples. As the microphone opens, aec_reset, what is
// read of it and what has been mixed are taken to be of the same time,
// give or take the latency the filter covers.

#include <stdint.h>
#include <string.h>
#include <math.h>
#include <atomic>

#ifndef MIC_RATE
#define MIC_RATE 16000
#endif

// a block, 16ms, and the filter AEC_M of them, 256ms. How fast it learns,
// AEC_SCALE, and how the leak's bands are smoothed, AEC_SPEC, were set on
// recordings of real rooms, an M4 and an Intel MacBook, quiet and with the
// near end talking over the echo. The suppression takes AEC_OVER times
// the echo left from each band, and no band below AEC_FLOOR, -26dB
#define AEC_N 256
#define AEC_FFT (AEC_N * 2)
#define AEC_BINS (AEC_N + 1)
#define AEC_M 16
#define AEC_RING (MIC_RATE * 2)
#define AEC_QUIET (MIC_RATE / 2)
#define AEC_SCALE 2.0f
#define AEC_SPEC 0.35f
#define AEC_OVER 2.0f
#define AEC_FLOOR 0.05f
#define AEC_PI 3.14159265358979323846

struct aec_cplx { float re, im; };

// the reference, a ring of what was mixed, and how much has been
static float aec_far[AEC_RING];
static std::atomic<uint64_t> aec_far_n(0);

// what has been mixed, at its rate, on its way to MIC_RATE
static double aec_far_step = 0.0, aec_far_at = 0.0, aec_far_sum = 0.0;
static int aec_far_parts = 0;

// where the near end is in the reference
static int64_t aec_offset = 0;
static uint64_t aec_near_n = 0;
static bool aec_tied = false;
static int64_t aec_heard = -AEC_QUIET;

// a block coming in, the near end and the reference of each sample; what
// goes out, a block and more behind
static float aec_din[AEC_N], aec_xin[AEC_N];
static int aec_in = 0;
static int16_t aec_out[AEC_N * 3];
static int aec_out_at = 0, aec_out_len = 0;

// the filters, the spectra of the reference of the last AEC_M blocks, newest
// at aec_xhead, and the block of reference before this one
static aec_cplx aec_wf[AEC_M][AEC_BINS], aec_wb[AEC_M][AEC_BINS];
static aec_cplx aec_x[AEC_M][AEC_BINS];
static int aec_xhead = 0;
static float aec_xprev[AEC_N];

// the learning: power of the reference in each band, the leak and what it
// is found from, the two filters compared
static float aec_eh[AEC_BINS], aec_yh[AEC_BINS];
static float aec_pey = 1.0f, aec_pyy = 1.0f, aec_leak = 0.0f;
static float aec_davg1 = 0.0f, aec_davg2 = 0.0f, aec_dvar1 = 0.0f, aec_dvar2 = 0.0f;
static float aec_sum_adapt = 0.0f;
static bool aec_adapted = false;

// the suppression: the last block of what was left and of the echo taken,
// the half of the last block out still to add, the echo left of each band
static float aec_eprev[AEC_N], aec_yprev[AEC_N], aec_ola[AEC_N], aec_rs[AEC_BINS];

// the transform, its turns, its order, its window
static aec_cplx aec_tw[AEC_FFT / 2];
static int aec_rev[AEC_FFT];
static float aec_win[AEC_FFT];
static bool aec_ready = false;

static void aec_fft_init()
{
	for (int i = 0; i < AEC_FFT / 2; ++i)
	{
		double a = -2.0 * AEC_PI * i / AEC_FFT;
		aec_tw[i].re = (float)cos(a);
		aec_tw[i].im = (float)sin(a);
	}
	int bits = 0;
	while ((1 << bits) < AEC_FFT) bits++;
	for (int i = 0; i < AEC_FFT; ++i)
	{
		int r = 0;
		for (int b = 0; b < bits; ++b) if (i & (1 << b)) r |= 1 << (bits - 1 - b);
		aec_rev[i] = r;
	}
	// the square root of a Hann window, twice over, at half overlap, adds
	// to one
	for (int i = 0; i < AEC_FFT; ++i) aec_win[i] = (float)sin(AEC_PI * (i + 0.5) / AEC_FFT);
	aec_ready = true;
}

// the transform in place, the inverse divided by its length
static void aec_fft(aec_cplx *a, bool inverse)
{
	for (int i = 0; i < AEC_FFT; ++i)
	{
		int r = aec_rev[i];
		if (r > i) { aec_cplx t = a[i]; a[i] = a[r]; a[r] = t; }
	}
	for (int len = 2; len <= AEC_FFT; len <<= 1)
	{
		int half = len >> 1, step = AEC_FFT / len;
		for (int i = 0; i < AEC_FFT; i += len)
		{
			for (int j = 0; j < half; ++j)
			{
				aec_cplx w = aec_tw[j * step];
				if (inverse) w.im = -w.im;
				aec_cplx u = a[i + j], v = a[i + j + half];
				aec_cplx t = {v.re * w.re - v.im * w.im, v.re * w.im + v.im * w.re};
				a[i + j].re = u.re + t.re; a[i + j].im = u.im + t.im;
				a[i + j + half].re = u.re - t.re; a[i + j + half].im = u.im - t.im;
			}
		}
	}
	if (inverse)
	{
		float s = 1.0f / AEC_FFT;
		for (int i = 0; i < AEC_FFT; ++i) { a[i].re *= s; a[i].im *= s; }
	}
}

// the bands of a real block, the first half of what the transform gives
static void aec_fwd(const float *in, aec_cplx *out)
{
	aec_cplx t[AEC_FFT];
	for (int i = 0; i < AEC_FFT; ++i) { t[i].re = in[i]; t[i].im = 0.0f; }
	aec_fft(t, false);
	memcpy(out, t, sizeof(aec_cplx) * AEC_BINS);
}

// a real block of the bands, the rest made as a real block's are
static void aec_inv(const aec_cplx *in, float *out)
{
	aec_cplx t[AEC_FFT];
	memcpy(t, in, sizeof(aec_cplx) * AEC_BINS);
	for (int i = AEC_BINS; i < AEC_FFT; ++i) { t[i].re = in[AEC_FFT - i].re; t[i].im = -in[AEC_FFT - i].im; }
	aec_fft(t, true);
	for (int i = 0; i < AEC_FFT; ++i) out[i] = t[i].re;
}

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
			aec_far[n % AEC_RING] = (float)(aec_far_sum / aec_far_parts);
			aec_far_sum = 0.0;
			aec_far_parts = 0;
			n++;
		}
	}
	aec_far_n.store(n, std::memory_order_release);
}

// the microphone opened: what is heard from now is tied to what is mixed
// now, and the filters learn again
static void aec_reset()
{
	if (!aec_ready) aec_fft_init();
	aec_tied = false;
	aec_near_n = 0;
	aec_heard = -AEC_QUIET;
	aec_in = 0;
	// what goes out first is the two blocks it is behind, silence
	memset(aec_out, 0, sizeof(aec_out));
	aec_out_at = 0;
	aec_out_len = AEC_N;
	memset(aec_wf, 0, sizeof(aec_wf));
	memset(aec_wb, 0, sizeof(aec_wb));
	memset(aec_x, 0, sizeof(aec_x));
	aec_xhead = 0;
	memset(aec_xprev, 0, sizeof(aec_xprev));
	memset(aec_eh, 0, sizeof(aec_eh));
	memset(aec_yh, 0, sizeof(aec_yh));
	aec_pey = aec_pyy = 1.0f;
	aec_leak = 0.0f;
	aec_davg1 = aec_davg2 = aec_dvar1 = aec_dvar2 = 0.0f;
	aec_sum_adapt = 0.0f;
	aec_adapted = false;
	memset(aec_eprev, 0, sizeof(aec_eprev));
	memset(aec_yprev, 0, sizeof(aec_yprev));
	memset(aec_ola, 0, sizeof(aec_ola));
	memset(aec_rs, 0, sizeof(aec_rs));
}

// the echo a filter makes of the reference, a block of it
static void aec_echo(aec_cplx (*w)[AEC_BINS], float *y)
{
	aec_cplx sum[AEC_BINS];
	memset(sum, 0, sizeof(sum));
	for (int m = 0; m < AEC_M; ++m)
	{
		aec_cplx *x = aec_x[(aec_xhead + AEC_M - m) % AEC_M];
		for (int i = 0; i < AEC_BINS; ++i)
		{
			sum[i].re += w[m][i].re * x[i].re - w[m][i].im * x[i].im;
			sum[i].im += w[m][i].re * x[i].im + w[m][i].im * x[i].re;
		}
	}
	float t[AEC_FFT];
	aec_inv(sum, t);
	memcpy(y, t + AEC_N, sizeof(float) * AEC_N);
}

// a block of what the microphone heard, d, and the reference tied to it, x,
// in the scale of 16 bit samples: what is left of d, out
static void aec_block(const float *d, const float *x, float *out)
{
	float t[AEC_FFT];
	// the reference's bands, this block after the one before
	aec_xhead = (aec_xhead + 1) % AEC_M;
	memcpy(t, aec_xprev, sizeof(float) * AEC_N);
	memcpy(t + AEC_N, x, sizeof(float) * AEC_N);
	aec_fwd(t, aec_x[aec_xhead]);
	memcpy(aec_xprev, x, sizeof(aec_xprev));

	float yf[AEC_N], yb[AEC_N], ef[AEC_N], eb[AEC_N];
	bool live = (int64_t)aec_near_n - aec_heard <= AEC_QUIET;
	if (!live)
	{
		// nothing played of late, nothing to take away
		memset(yf, 0, sizeof(yf));
		memcpy(ef, d, sizeof(ef));
	}
	else
	{
		aec_echo(aec_wf, yf);
		aec_echo(aec_wb, yb);
		float sff = 0.0f, see = 0.0f, dbf = 0.0f, sxx = 0.0f, syy = 0.0f;
		for (int i = 0; i < AEC_N; ++i)
		{
			ef[i] = d[i] - yf[i];
			eb[i] = d[i] - yb[i];
			sff += ef[i] * ef[i];
			see += eb[i] * eb[i];
			dbf += (ef[i] - eb[i]) * (ef[i] - eb[i]);
			sxx += x[i] * x[i];
			syy += yf[i] * yf[i];
		}
		dbf += 10.0f * AEC_N;

		// is the background doing better than the foreground, over a short
		// while and a longer: then it takes over. Doing worse, it is put
		// back as the foreground is
		float sdiff = sff - see;
		aec_davg1 = 0.6f * aec_davg1 + 0.4f * sdiff;
		aec_davg2 = 0.85f * aec_davg2 + 0.15f * sdiff;
		aec_dvar1 = 0.36f * aec_dvar1 + 0.16f * sff * dbf;
		aec_dvar2 = 0.7225f * aec_dvar2 + 0.0225f * sff * dbf;
		if (sdiff * fabsf(sdiff) > sff * dbf
			|| aec_davg1 * fabsf(aec_davg1) > 0.5f * aec_dvar1
			|| aec_davg2 * fabsf(aec_davg2) > 0.25f * aec_dvar2)
		{
			memcpy(aec_wf, aec_wb, sizeof(aec_wf));
			memcpy(yf, yb, sizeof(yf));
			memcpy(ef, eb, sizeof(ef));
			sff = see;
			aec_davg1 = aec_davg2 = aec_dvar1 = aec_dvar2 = 0.0f;
		}
		else if (-sdiff * fabsf(sdiff) > 4.0f * sff * dbf
			|| -aec_davg1 * fabsf(aec_davg1) > 4.0f * aec_dvar1
			|| -aec_davg2 * fabsf(aec_davg2) > 4.0f * aec_dvar2)
		{
			memcpy(aec_wb, aec_wf, sizeof(aec_wb));
			memcpy(eb, ef, sizeof(eb));
			see = sff;
			aec_davg1 = aec_davg2 = aec_dvar1 = aec_dvar2 = 0.0f;
		}

		// the leak: how what is left goes up and down with the echo taken
		aec_cplx ff[AEC_BINS], yy[AEC_BINS];
		memset(t, 0, sizeof(float) * AEC_N);
		memcpy(t + AEC_N, ef, sizeof(float) * AEC_N);
		aec_fwd(t, ff);
		memcpy(t + AEC_N, yf, sizeof(float) * AEC_N);
		aec_fwd(t, yy);
		float pey = 0.0f, pyy = 0.0f;
		float rf[AEC_BINS], yp[AEC_BINS];
		for (int i = 0; i < AEC_BINS; ++i)
		{
			rf[i] = ff[i].re * ff[i].re + ff[i].im * ff[i].im;
			yp[i] = yy[i].re * yy[i].re + yy[i].im * yy[i].im;
			float ee = rf[i] - aec_eh[i], ey = yp[i] - aec_yh[i];
			pey += ee * ey;
			pyy += ey * ey;
			aec_eh[i] = (1.0f - AEC_SPEC) * aec_eh[i] + AEC_SPEC * rf[i];
			aec_yh[i] = (1.0f - AEC_SPEC) * aec_yh[i] + AEC_SPEC * yp[i];
		}
		// the louder the echo against what is left the faster this follows;
		// while the near end talks, hardly at all
		float alpha = (2.0f * AEC_N / MIC_RATE) * syy / (see + 1.0f);
		if (alpha > 0.5f * AEC_N / MIC_RATE) alpha = 0.5f * AEC_N / MIC_RATE;
		if (pyy < 0.0f) pyy = 0.0f;
		aec_pey = (1.0f - alpha) * aec_pey + alpha * pey;
		aec_pyy = (1.0f - alpha) * aec_pyy + alpha * pyy;
		if (aec_pyy < 1.0f) aec_pyy = 1.0f;
		if (aec_pey < 0.005f * aec_pyy) aec_pey = 0.005f * aec_pyy;
		if (aec_pey > aec_pyy) aec_pey = aec_pyy;
		aec_leak = aec_pey / aec_pyy;
		if (aec_leak > 0.5f) aec_leak = 0.5f;

		// what is left that is still echo, of all of it
		float rer = (0.0001f * sxx + 3.0f * aec_leak * syy) / (see + 1.0f);
		if (rer < 0.0f) rer = 0.0f;
		if (rer > 0.5f) rer = 0.5f;

		// the power of the reference in each band, over the filter
		float px[AEC_BINS];
		for (int i = 0; i < AEC_BINS; ++i)
		{
			float s = 0.0f;
			for (int m = 0; m < AEC_M; ++m) { aec_cplx c = aec_x[m][i]; s += c.re * c.re + c.im * c.im; }
			px[i] = s;
		}

		// how fast each band learns: at first by how loud the reference is,
		// once it has learnt something by how much of what is left is echo
		float mu[AEC_BINS];
		if (!aec_adapted)
		{
			float rate = 0.0f;
			if (sxx > AEC_N * 1000.0f / 64.0f)
			{
				rate = 0.25f * sxx;
				if (rate > 0.25f * see) rate = 0.25f * see;
				rate /= see + 1.0f;
			}
			for (int i = 0; i < AEC_BINS; ++i) mu[i] = rate / (px[i] / AEC_M + 10.0f * AEC_FFT);
			aec_sum_adapt += rate;
			if (aec_sum_adapt > AEC_M && aec_leak * syy > 0.03f * syy) aec_adapted = true;
		}
		else
		{
			for (int i = 0; i < AEC_BINS; ++i)
			{
				float r = aec_leak * yp[i];
				float e = rf[i] + 1.0f;
				r = 0.7f * r + 0.3f * rer * e;
				if (r > 0.5f * e) r = 0.5f * e;
				mu[i] = r / (e * (px[i] / AEC_M + 10.0f * AEC_FFT));
			}
		}

		// each block of the filter learns by how much of the echo it has, the
		// room's echo is in a few of them
		float prop[AEC_M], pmax = 1.0f, psum = 0.0f;
		for (int m = 0; m < AEC_M; ++m)
		{
			float s = 1.0f;
			for (int i = 0; i < AEC_BINS; ++i) s += aec_wb[m][i].re * aec_wb[m][i].re + aec_wb[m][i].im * aec_wb[m][i].im;
			prop[m] = sqrtf(s);
			if (prop[m] > pmax) pmax = prop[m];
		}
		for (int m = 0; m < AEC_M; ++m) { prop[m] += 0.1f * pmax; psum += prop[m]; }
		for (int m = 0; m < AEC_M; ++m) prop[m] = 0.99f * prop[m] / psum;

		// the background learns, from what it left
		aec_cplx eeb[AEC_BINS];
		memset(t, 0, sizeof(float) * AEC_N);
		memcpy(t + AEC_N, eb, sizeof(float) * AEC_N);
		aec_fwd(t, eeb);
		for (int m = 0; m < AEC_M; ++m)
		{
			aec_cplx *xm = aec_x[(aec_xhead + AEC_M - m) % AEC_M];
			aec_cplx g[AEC_BINS];
			for (int i = 0; i < AEC_BINS; ++i)
			{
				// the reference's conjugate times what was left
				float k = mu[i] * prop[m] * AEC_SCALE;
				g[i].re = k * (xm[i].re * eeb[i].re + xm[i].im * eeb[i].im);
				g[i].im = k * (xm[i].re * eeb[i].im - xm[i].im * eeb[i].re);
			}
			// kept to the first half, a filter of AEC_N taps a block
			aec_inv(g, t);
			memset(t + AEC_N, 0, sizeof(float) * AEC_N);
			aec_fwd(t, g);
			for (int i = 0; i < AEC_BINS; ++i) { aec_wb[m][i].re += g[i].re; aec_wb[m][i].im += g[i].im; }
		}
	}

	// what echo is left of each band taken down, in blocks overlapping by
	// half: what was left and the echo taken, windowed, this block after
	// the one before
	float se[AEC_FFT], sy[AEC_FFT];
	for (int i = 0; i < AEC_N; ++i)
	{
		se[i] = aec_eprev[i] * aec_win[i];
		se[i + AEC_N] = ef[i] * aec_win[i + AEC_N];
		sy[i] = aec_yprev[i] * aec_win[i];
		sy[i + AEC_N] = yf[i] * aec_win[i + AEC_N];
	}
	memcpy(aec_eprev, ef, sizeof(aec_eprev));
	memcpy(aec_yprev, yf, sizeof(aec_yprev));
	aec_cplx es[AEC_BINS], ys[AEC_BINS];
	aec_fwd(se, es);
	aec_fwd(sy, ys);
	float leak = live ? aec_leak : 0.0f;
	for (int i = 0; i < AEC_BINS; ++i)
	{
		float pe = es[i].re * es[i].re + es[i].im * es[i].im;
		float r = leak * (ys[i].re * ys[i].re + ys[i].im * ys[i].im);
		// the room rings on: the echo left falls away slower than it came
		aec_rs[i] = r > aec_rs[i] ? r : 0.7f * aec_rs[i] + 0.3f * r;
		float g = (pe > 0.0f) ? 1.0f - AEC_OVER * aec_rs[i] / pe : 1.0f;
		if (g < AEC_FLOOR) g = AEC_FLOOR;
		es[i].re *= g;
		es[i].im *= g;
	}
	aec_inv(es, se);
	for (int i = 0; i < AEC_N; ++i)
	{
		out[i] = aec_ola[i] + se[i] * aec_win[i];
		aec_ola[i] = se[i + AEC_N] * aec_win[i + AEC_N];
	}
}

static inline float aec_ref(int64_t i)
{
	return aec_far[i % AEC_RING];
}

// the samples the microphone gave, the echo taken from them, as they were
// a block and more before
static void aec_near(int16_t *samples, int n)
{
	if (aec_far_step == 0.0 || n <= 0) return;
	if (!aec_ready) aec_reset();
	int64_t far_n = (int64_t)aec_far_n.load(std::memory_order_acquire);
	if (!aec_tied)
	{
		// the last of these is of now, as is the last mixed
		aec_offset = far_n - (int64_t)(aec_near_n + n);
		aec_tied = true;
	}
	for (int i = 0; i < n; ++i, ++aec_near_n)
	{
		// the reference this sample could hear, not newer than what has
		// been mixed, nor older than the ring keeps
		int64_t at = (int64_t)aec_near_n + aec_offset;
		if (at > far_n - 1) at = far_n - 1;
		float x = (at < 0 || at < far_n - AEC_RING) ? 0.0f : aec_ref(at) * 32768.0f;
		if (fabsf(x) > 100.0f) aec_heard = (int64_t)aec_near_n;
		aec_din[aec_in] = samples[i];
		aec_xin[aec_in] = x;
		if (++aec_in == AEC_N)
		{
			float out[AEC_N];
			aec_block(aec_din, aec_xin, out);
			aec_in = 0;
			for (int k = 0; k < AEC_N; ++k)
			{
				float v = out[k];
				if (v > 32767.0f) v = 32767.0f;
				if (v < -32768.0f) v = -32768.0f;
				aec_out[(aec_out_at + aec_out_len++) % (AEC_N * 3)] = (int16_t)lrintf(v);
			}
		}
		// what goes out in place of what came in
		samples[i] = aec_out[aec_out_at];
		aec_out_at = (aec_out_at + 1) % (AEC_N * 3);
		aec_out_len--;
	}
}

#endif
