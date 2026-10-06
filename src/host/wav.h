#ifndef HOST_WAV_H
#define HOST_WAV_H

// A wav file read into what the mixer works in, float stereo at the mix
// rate. Nothing here knows of SDL, or of any other library, only how to
// read a file. It reads PCM of 8, 16, 24 and 32 bits and 32 bit float, of
// any number of channels and any rate. One channel is put on both sides,
// more than two keep the first two. Another rate is made the mix rate by
// drawing a line between each two samples, which is good enough for a
// sound effect.

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mixer.h"

static uint32_t wav_u32(const uint8_t *p)
{
	return (uint32_t)p[0] | ((uint32_t)p[1] << 8) | ((uint32_t)p[2] << 16) | ((uint32_t)p[3] << 24);
}

static float wav_sample(const uint8_t *p, int format, int bits)
{
	// one sample, as -1.0 to 1.0
	switch (bits)
	{
	case 8: return ((int)p[0] - 128) / 128.0f;
	case 16: return (int16_t)(p[0] | (p[1] << 8)) / 32768.0f;
	case 24: return (int32_t)(((uint32_t)p[0] << 8) | ((uint32_t)p[1] << 16) | ((uint32_t)p[2] << 24)) / 2147483648.0f;
	case 32:
		if (format == 3)
		{
			float f;
			memcpy(&f, p, sizeof(f));
			return f;
		}
		return (int32_t)wav_u32(p) / 2147483648.0f;
	}
	return 0.0f;
}

// the samples of a wav file, for free() to let go of, and how many frames
// there are. 0 if it can not be read, or is not a kind that is known.

static float *wav_load(const char *path, int *frames_out)
{
	*frames_out = 0;
	FILE *f = fopen(path, "rb");
	if (!f) return nullptr;
	fseek(f, 0, SEEK_END);
	long size = ftell(f);
	fseek(f, 0, SEEK_SET);
	if (size < 12)
	{
		fclose(f);
		return nullptr;
	}
	uint8_t *file = (uint8_t*)malloc(size);
	size_t got = file ? fread(file, 1, size, f) : 0;
	fclose(f);
	if (!file || got != (size_t)size || memcmp(file, "RIFF", 4) || memcmp(file + 8, "WAVE", 4))
	{
		free(file);
		return nullptr;
	}
	// the chunks, a name, a length, and that many bytes, on an even boundary
	int format = 0, channels = 0, bits = 0;
	uint32_t rate = 0;
	const uint8_t *data = nullptr;
	uint32_t data_len = 0;
	long at = 12;
	while (at + 8 <= size)
	{
		uint32_t len = wav_u32(file + at + 4);
		const uint8_t *body = file + at + 8;
		if ((long)len > size - at - 8) len = (uint32_t)(size - at - 8);
		if (!memcmp(file + at, "fmt ", 4) && len >= 16)
		{
			format = body[0] | (body[1] << 8);
			channels = body[2] | (body[3] << 8);
			rate = wav_u32(body + 4);
			bits = body[14] | (body[15] << 8);
			// the extensible form has the real format further on
			if (format == 0xfffe && len >= 26) format = body[24] | (body[25] << 8);
		}
		else if (!memcmp(file + at, "data", 4))
		{
			data = body;
			data_len = len;
		}
		at += 8 + len + (len & 1);
	}
	int bytes = bits / 8;
	bool known = (format == 1 && (bits == 8 || bits == 16 || bits == 24 || bits == 32)) || (format == 3 && bits == 32);
	if (!data || !known || channels < 1 || rate < 1)
	{
		free(file);
		return nullptr;
	}
	int src_frames = (int)(data_len / (uint32_t)(bytes * channels));
	int frames = (int)((int64_t)src_frames * MIXER_RATE / rate);
	if (src_frames < 1 || frames < 1)
	{
		free(file);
		return nullptr;
	}
	float *out = (float*)malloc(sizeof(float) * MIXER_CHANNELS * frames);
	if (out)
	{
		int right = channels > 1 ? 1 : 0;
		for (int i = 0; i < frames; ++i)
		{
			// where this frame falls in the file, and how far between two frames
			double pos = (double)i * rate / MIXER_RATE;
			int a = (int)pos;
			int b = a + 1 < src_frames ? a + 1 : a;
			float t = (float)(pos - a);
			const uint8_t *pa = data + (size_t)a * bytes * channels;
			const uint8_t *pb = data + (size_t)b * bytes * channels;
			float la = wav_sample(pa, format, bits), lb = wav_sample(pb, format, bits);
			float ra = wav_sample(pa + right * bytes, format, bits), rb = wav_sample(pb + right * bytes, format, bits);
			out[i * 2] = la + (lb - la) * t;
			out[i * 2 + 1] = ra + (rb - ra) * t;
		}
		*frames_out = frames;
	}
	free(file);
	return out;
}

#endif
