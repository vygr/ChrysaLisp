// video.cpp, the decoders a host has for the films of lib/media, called by
// the GUI's table of the driver this host was built with.
//
// open: a decoder for a codec, its 4 letters high first, from its setup as
// the film has it, avcC of avc1, H.264, hvcC of hvc1, H.265, 0 if this host
// has none for it. decode: one packet of the
// film, in the order they are decoded, and when it is shown, -1 if it could
// not be. The pictures are held till asked for. frame: the picture shown at
// a time, its pixels of 32 bits, blue green red alpha, into the buffer
// given, and that time; -1 if it is not out yet, with B-frames a picture is
// out only once those it was made from are. Those to be shown before it go.
// close: the decoder gone.
//
// macOS: VideoToolbox, the hardware's decoder. Elsewhere none yet, open is 0.

#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#include "video.h"

#ifdef __APPLE__

#include <VideoToolbox/VideoToolbox.h>
#include <vector>
#include <algorithm>

struct Video_Picture
{
	int64_t shown;
	CVPixelBufferRef pixels;
};

struct Video_Decoder
{
	VTDecompressionSessionRef session;
	CMVideoFormatDescriptionRef format;
	std::vector<Video_Picture> held;
};

static void video_out(void *ref, void *frame_ref, OSStatus status, VTDecodeInfoFlags flags,
	CVImageBufferRef image, CMTime shown, CMTime duration)
{
	auto decoder = (Video_Decoder*)ref;
	if (status != noErr || !image) return;
	CVPixelBufferRetain(image);
	decoder->held.push_back({shown.value, image});
}

static bool video_avcc(const uint8_t *setup, uint64_t len, std::vector<const uint8_t*> &sets,
	std::vector<size_t> &sizes, int &nal_length)
{
	//avcC: version, profile, its compatibility, level, the bytes of a
	//length less 1, then its parameter sets, SPS and then PPS, each a
	//length of 2 bytes and its bytes
	if (len < 7 || setup[0] != 1) return false;
	size_t at = 5;
	for (int kind = 0; kind < 2; kind++)
	{
		if (at >= len) return false;
		int count = kind == 0 ? (setup[at] & 31) : setup[at];
		at++;
		for (int i = 0; i < count; i++)
		{
			if (at + 2 > len) return false;
			size_t size = (setup[at] << 8) | setup[at + 1];
			at += 2;
			if (at + size > len) return false;
			sets.push_back(setup + at);
			sizes.push_back(size);
			at += size;
		}
	}
	nal_length = (setup[4] & 3) + 1;
	return sets.size() >= 2;
}

static bool video_hvcc(const uint8_t *setup, uint64_t len, std::vector<const uint8_t*> &sets,
	std::vector<size_t> &sizes, int &nal_length)
{
	//hvcC: 21 bytes of what the stream is, the bytes of a length less 1,
	//then arrays, each the kind of its units, VPS SPS PPS and others, how
	//many, and each a length of 2 bytes and its bytes
	if (len < 23 || setup[0] != 1) return false;
	size_t at = 23;
	for (int array = 0, arrays = setup[22]; array < arrays; array++)
	{
		if (at + 3 > len) return false;
		int kind = setup[at] & 63;
		int count = (setup[at + 1] << 8) | setup[at + 2];
		at += 3;
		for (int i = 0; i < count; i++)
		{
			if (at + 2 > len) return false;
			size_t size = (setup[at] << 8) | setup[at + 1];
			at += 2;
			if (at + size > len) return false;
			//the parameter sets, VPS SPS PPS
			if (kind >= 32 && kind <= 34)
			{
				sets.push_back(setup + at);
				sizes.push_back(size);
			}
			at += size;
		}
	}
	nal_length = (setup[21] & 3) + 1;
	return sets.size() >= 3;
}

uint64_t host_video_open(uint64_t codec, const uint8_t *setup, uint64_t len)
{
	//the parameter sets of the codec's setup, avc1 H.264, hvc1 H.265
	std::vector<const uint8_t*> sets;
	std::vector<size_t> sizes;
	int nal_length = 4;
	bool hevc = codec == 0x68766331;
	if (codec == 0x61766331) { if (!video_avcc(setup, len, sets, sizes, nal_length)) return 0; }
	else if (hevc) { if (!video_hvcc(setup, len, sets, sizes, nal_length)) return 0; }
	else return 0;
	auto decoder = new Video_Decoder();
	OSStatus made = hevc
		? CMVideoFormatDescriptionCreateFromHEVCParameterSets(kCFAllocatorDefault, sets.size(),
			sets.data(), sizes.data(), nal_length, nullptr, &decoder->format)
		: CMVideoFormatDescriptionCreateFromH264ParameterSets(kCFAllocatorDefault, sets.size(),
			sets.data(), sizes.data(), nal_length, &decoder->format);
	if (made != noErr)
	{
		delete decoder;
		return 0;
	}
	//pictures as our pixmaps have them, 32 bits blue green red alpha
	int32_t kind = kCVPixelFormatType_32BGRA;
	auto number = CFNumberCreate(kCFAllocatorDefault, kCFNumberSInt32Type, &kind);
	const void *keys[] = {kCVPixelBufferPixelFormatTypeKey};
	const void *values[] = {number};
	auto attrs = CFDictionaryCreate(kCFAllocatorDefault, keys, values, 1,
		&kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
	VTDecompressionOutputCallbackRecord out = {video_out, decoder};
	auto status = VTDecompressionSessionCreate(kCFAllocatorDefault, decoder->format, nullptr, attrs, &out, &decoder->session);
	CFRelease(attrs);
	CFRelease(number);
	if (status != noErr)
	{
		CFRelease(decoder->format);
		delete decoder;
		return 0;
	}
	return (uint64_t)decoder;
}

int64_t host_video_decode(uint64_t handle, const uint8_t *data, uint64_t len, int64_t shown)
{
	auto decoder = (Video_Decoder*)handle;
	if (!decoder) return -1;
	CMBlockBufferRef block = nullptr;
	if (CMBlockBufferCreateWithMemoryBlock(kCFAllocatorDefault, nullptr, len, kCFAllocatorDefault,
			nullptr, 0, len, kCMBlockBufferAssureMemoryNowFlag, &block) != noErr) return -1;
	CMBlockBufferReplaceDataBytes(data, block, 0, len);
	CMSampleTimingInfo timing = {kCMTimeInvalid, CMTimeMake(shown, 1), kCMTimeInvalid};
	CMSampleBufferRef sample = nullptr;
	size_t size = len;
	auto status = CMSampleBufferCreateReady(kCFAllocatorDefault, block, decoder->format, 1, 1, &timing, 1, &size, &sample);
	CFRelease(block);
	if (status != noErr) return -1;
	//decoded now, not in the background: the picture is held when this returns
	VTDecodeInfoFlags flags = 0;
	status = VTDecompressionSessionDecodeFrame(decoder->session, sample, 0, nullptr, &flags);
	VTDecompressionSessionWaitForAsynchronousFrames(decoder->session);
	CFRelease(sample);
	return status == noErr ? 0 : -1;
}

int64_t host_video_frame(uint64_t handle, uint32_t *pixels, uint64_t w, uint64_t h, uint64_t stride, int64_t shown)
{
	//the picture shown at that time, into the pixels given, if it is out
	//yet; those to be shown before it are gone, a film started again at a
	//key will have them
	auto decoder = (Video_Decoder*)handle;
	if (!decoder) return -1;
	int64_t got = -1;
	for (auto it = decoder->held.begin(); it != decoder->held.end();)
	{
		if (it->shown > shown) { ++it; continue; }
		if (it->shown == shown)
		{
			auto image = it->pixels;
			CVPixelBufferLockBaseAddress(image, kCVPixelBufferLock_ReadOnly);
			auto from = (uint8_t*)CVPixelBufferGetBaseAddress(image);
			size_t from_stride = CVPixelBufferGetBytesPerRow(image);
			size_t rows = std::min((size_t)h, CVPixelBufferGetHeight(image));
			size_t bytes = std::min((size_t)w, CVPixelBufferGetWidth(image)) * 4;
			for (size_t y = 0; y < rows; y++)
				memcpy((uint8_t*)pixels + y * stride, from + y * from_stride, bytes);
			CVPixelBufferUnlockBaseAddress(image, kCVPixelBufferLock_ReadOnly);
			got = shown;
		}
		CVPixelBufferRelease(it->pixels);
		it = decoder->held.erase(it);
	}
	return got;
}

void host_video_close(uint64_t handle)
{
	auto decoder = (Video_Decoder*)handle;
	if (!decoder) return;
	VTDecompressionSessionInvalidate(decoder->session);
	CFRelease(decoder->session);
	CFRelease(decoder->format);
	for (auto &picture : decoder->held) CVPixelBufferRelease(picture.pixels);
	delete decoder;
}

#else

uint64_t host_video_open(uint64_t codec, const uint8_t *setup, uint64_t len) { return 0; }
int64_t host_video_decode(uint64_t handle, const uint8_t *data, uint64_t len, int64_t shown) { return -1; }
int64_t host_video_frame(uint64_t handle, uint32_t *pixels, uint64_t w, uint64_t h, uint64_t stride, int64_t shown) { return -1; }
void host_video_close(uint64_t handle) {}

#endif
