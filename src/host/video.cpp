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
// macOS: VideoToolbox, the hardware's decoder, H.264 and H.265. Linux: the
// stateful decoder of V4L2, /dev/video10, H.264, the Raspberry Pi's.
// Windows: a decoder of Media Foundation, H.264, and H.265 if one is
// installed, not yet tried on Windows itself. Elsewhere none, open is 0.

#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#include "video.h"
#include <vector>
#include <algorithm>

#if defined(__APPLE__) || defined(__linux__) || defined(_WIN64)

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

__attribute__((unused)) static bool video_hvcc(const uint8_t *setup, uint64_t len, std::vector<const uint8_t*> &sets,
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

#endif

#ifdef __APPLE__

#include <VideoToolbox/VideoToolbox.h>

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
	//nothing: the end, and this decodes as it is given, it holds none back
	if (len == 0) return 0;
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

#elif defined(__linux__)

// Linux: the stateful decoder of V4L2, a memory to memory device, the
// Raspberry Pi's bcm2835-codec-decode. Packets are given it as Annex B,
// each NAL after a start code, the parameter sets before the first, and
// the time it is shown as its timestamp; pictures come back with it, in
// YUV 4:2:0, and are made blue green red alpha here.

#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <poll.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <linux/videodev2.h>

struct Video_Picture
{
	int64_t shown;
	std::vector<uint32_t> pixels;
};

struct Video_Buffer
{
	void *at;
	size_t size;
};

struct Video_Decoder
{
	int fd = -1;
	int nal_length = 4;
	std::vector<uint8_t> sets;
	bool first = true;
	bool capturing = false;
	uint32_t width = 0, height = 0, stride = 0, plane = 0;
	std::vector<Video_Buffer> outs, caps;
	std::vector<bool> out_free;
	std::vector<Video_Picture> held;
};

//timestamps are kept 0 or more, a time shown may be less than 0
static const int64_t video_base = (int64_t)1 << 40;

static int video_ioctl(int fd, unsigned long what, void *arg)
{
	int r;
	do r = ioctl(fd, what, arg); while (r == -1 && errno == EINTR);
	return r;
}

static void video_free(Video_Decoder *d)
{
	for (auto &b : d->outs) if (b.at) munmap(b.at, b.size);
	for (auto &b : d->caps) if (b.at) munmap(b.at, b.size);
	if (d->fd >= 0) close(d->fd);
	delete d;
}

static bool video_buffers(Video_Decoder *d, uint32_t type, uint32_t count, std::vector<Video_Buffer> &bufs)
{
	v4l2_requestbuffers req = {};
	req.count = count;
	req.type = type;
	req.memory = V4L2_MEMORY_MMAP;
	if (video_ioctl(d->fd, VIDIOC_REQBUFS, &req) < 0 || req.count == 0) return false;
	for (uint32_t i = 0; i < req.count; i++)
	{
		v4l2_buffer buf = {};
		v4l2_plane planes[1] = {};
		buf.type = type;
		buf.memory = V4L2_MEMORY_MMAP;
		buf.index = i;
		buf.m.planes = planes;
		buf.length = 1;
		if (video_ioctl(d->fd, VIDIOC_QUERYBUF, &buf) < 0) return false;
		void *at = mmap(nullptr, planes[0].length, PROT_READ | PROT_WRITE, MAP_SHARED, d->fd, planes[0].m.mem_offset);
		if (at == MAP_FAILED) return false;
		bufs.push_back({at, planes[0].length});
	}
	return true;
}

static bool video_queue_capture(Video_Decoder *d, uint32_t i)
{
	v4l2_buffer buf = {};
	v4l2_plane planes[1] = {};
	buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE;
	buf.memory = V4L2_MEMORY_MMAP;
	buf.index = i;
	buf.m.planes = planes;
	buf.length = 1;
	return video_ioctl(d->fd, VIDIOC_QBUF, &buf) == 0;
}

static bool video_capture_start(Video_Decoder *d)
{
	//the size of the pictures is known now, the capture side made for it
	v4l2_format fmt = {};
	fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE;
	if (video_ioctl(d->fd, VIDIOC_G_FMT, &fmt) < 0) return false;
	fmt.fmt.pix_mp.pixelformat = V4L2_PIX_FMT_YUV420;
	fmt.fmt.pix_mp.num_planes = 1;
	if (video_ioctl(d->fd, VIDIOC_S_FMT, &fmt) < 0) return false;
	v4l2_selection sel = {};
	sel.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	sel.target = V4L2_SEL_TGT_COMPOSE;
	d->width = fmt.fmt.pix_mp.width;
	d->height = fmt.fmt.pix_mp.height;
	if (video_ioctl(d->fd, VIDIOC_G_SELECTION, &sel) == 0 && sel.r.width && sel.r.height)
	{
		d->width = sel.r.width;
		d->height = sel.r.height;
	}
	d->stride = fmt.fmt.pix_mp.plane_fmt[0].bytesperline;
	d->plane = fmt.fmt.pix_mp.height;
	v4l2_control min = {};
	min.id = V4L2_CID_MIN_BUFFERS_FOR_CAPTURE;
	uint32_t count = video_ioctl(d->fd, VIDIOC_G_CTRL, &min) == 0 ? min.value + 4 : 8;
	if (!video_buffers(d, V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE, count, d->caps)) return false;
	for (uint32_t i = 0; i < d->caps.size(); i++) if (!video_queue_capture(d, i)) return false;
	int type = V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE;
	if (video_ioctl(d->fd, VIDIOC_STREAMON, &type) < 0) return false;
	d->capturing = true;
	return true;
}

static void video_convert(Video_Decoder *d, const uint8_t *yuv, std::vector<uint32_t> &out)
{
	//YUV 4:2:0, a plane of Y, then U and V each a quarter, BT.601 limited
	uint32_t w = d->width, h = d->height, ys = d->stride, cs = d->stride / 2;
	const uint8_t *py = yuv, *pu = yuv + ys * d->plane, *pv = pu + cs * (d->plane / 2);
	out.resize((size_t)w * h);
	for (uint32_t y = 0; y < h; y++)
	{
		for (uint32_t x = 0; x < w; x++)
		{
			int c = 298 * ((int)py[y * ys + x] - 16);
			int u = (int)pu[(y / 2) * cs + x / 2] - 128;
			int v = (int)pv[(y / 2) * cs + x / 2] - 128;
			int r = (c + 409 * v + 128) >> 8;
			int g = (c - 100 * u - 208 * v + 128) >> 8;
			int b = (c + 516 * u + 128) >> 8;
			r = r < 0 ? 0 : r > 255 ? 255 : r;
			g = g < 0 ? 0 : g > 255 ? 255 : g;
			b = b < 0 ? 0 : b > 255 ? 255 : b;
			out[(size_t)y * w + x] = 0xff000000 | (r << 16) | (g << 8) | b;
		}
	}
}

static void video_drain(Video_Decoder *d, int wait)
{
	//what the decoder has to say: a new size, done packets, done pictures
	for (;;)
	{
		pollfd p = {d->fd, POLLIN | POLLOUT | POLLPRI, 0};
		if (poll(&p, 1, wait) <= 0) return;
		wait = 0;
		bool any = false;
		if (p.revents & POLLPRI)
		{
			v4l2_event ev = {};
			while (video_ioctl(d->fd, VIDIOC_DQEVENT, &ev) == 0)
			{
				if (ev.type == V4L2_EVENT_SOURCE_CHANGE && !d->capturing) video_capture_start(d);
				any = true;
			}
		}
		for (;;)
		{
			v4l2_buffer buf = {};
			v4l2_plane planes[1] = {};
			buf.type = V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE;
			buf.memory = V4L2_MEMORY_MMAP;
			buf.m.planes = planes;
			buf.length = 1;
			if (video_ioctl(d->fd, VIDIOC_DQBUF, &buf) < 0) break;
			d->out_free[buf.index] = true;
			any = true;
		}
		while (d->capturing)
		{
			v4l2_buffer buf = {};
			v4l2_plane planes[1] = {};
			buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE;
			buf.memory = V4L2_MEMORY_MMAP;
			buf.m.planes = planes;
			buf.length = 1;
			if (video_ioctl(d->fd, VIDIOC_DQBUF, &buf) < 0) break;
			if (planes[0].bytesused)
			{
				Video_Picture pic;
				pic.shown = (int64_t)buf.timestamp.tv_sec * 1000000 + buf.timestamp.tv_usec - video_base;
				video_convert(d, (const uint8_t*)d->caps[buf.index].at, pic.pixels);
				d->held.push_back(std::move(pic));
			}
			video_queue_capture(d, buf.index);
			any = true;
		}
		if (!any) return;
	}
}

uint64_t host_video_open(uint64_t codec, const uint8_t *setup, uint64_t len)
{
	//H.264 only, avc1, its parameter sets each after a start code
	std::vector<const uint8_t*> sets;
	std::vector<size_t> sizes;
	int nal_length = 4;
	if (codec != 0x61766331 || !video_avcc(setup, len, sets, sizes, nal_length)) return 0;
	auto d = new Video_Decoder();
	d->nal_length = nal_length;
	for (size_t i = 0; i < sets.size(); i++)
	{
		static const uint8_t start[] = {0, 0, 0, 1};
		d->sets.insert(d->sets.end(), start, start + 4);
		d->sets.insert(d->sets.end(), sets[i], sets[i] + sizes[i]);
	}
	d->fd = open("/dev/video10", O_RDWR | O_NONBLOCK);
	if (d->fd < 0) { video_free(d); return 0; }
	v4l2_format fmt = {};
	fmt.type = V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE;
	fmt.fmt.pix_mp.pixelformat = V4L2_PIX_FMT_H264;
	fmt.fmt.pix_mp.num_planes = 1;
	fmt.fmt.pix_mp.plane_fmt[0].sizeimage = 1 << 20;
	if (video_ioctl(d->fd, VIDIOC_S_FMT, &fmt) < 0) { video_free(d); return 0; }
	v4l2_event_subscription sub = {};
	sub.type = V4L2_EVENT_SOURCE_CHANGE;
	video_ioctl(d->fd, VIDIOC_SUBSCRIBE_EVENT, &sub);
	if (!video_buffers(d, V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE, 8, d->outs)) { video_free(d); return 0; }
	d->out_free.assign(d->outs.size(), true);
	int type = V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE;
	if (video_ioctl(d->fd, VIDIOC_STREAMON, &type) < 0) { video_free(d); return 0; }
	return (uint64_t)d;
}

int64_t host_video_decode(uint64_t handle, const uint8_t *data, uint64_t len, int64_t shown)
{
	auto d = (Video_Decoder*)handle;
	if (!d) return -1;
	//nothing: the end, what the decoder holds back given up
	if (len == 0)
	{
		v4l2_decoder_cmd cmd = {};
		cmd.cmd = V4L2_DEC_CMD_STOP;
		video_ioctl(d->fd, VIDIOC_DECODER_CMD, &cmd);
		video_drain(d, 30);
		return 0;
	}
	//a free buffer to give it, waiting a little for one to be done
	int i = -1;
	for (int tries = 0; tries < 50 && i < 0; tries++)
	{
		for (size_t j = 0; j < d->out_free.size(); j++) if (d->out_free[j]) { i = (int)j; break; }
		if (i < 0) video_drain(d, 10);
	}
	if (i < 0) return -1;
	//Annex B: the sets before the first packet, each NAL after a start code
	auto at = (uint8_t*)d->outs[i].at;
	size_t room = d->outs[i].size, used = 0;
	if (d->first)
	{
		if (d->sets.size() > room) return -1;
		memcpy(at, d->sets.data(), d->sets.size());
		used = d->sets.size();
		d->first = false;
	}
	for (uint64_t k = 0; k + d->nal_length <= len;)
	{
		size_t n = 0;
		for (int b = 0; b < d->nal_length; b++) n = (n << 8) | data[k + b];
		k += d->nal_length;
		if (k + n > len || used + 4 + n > room) break;
		at[used++] = 0; at[used++] = 0; at[used++] = 0; at[used++] = 1;
		memcpy(at + used, data + k, n);
		used += n;
		k += n;
	}
	v4l2_buffer buf = {};
	v4l2_plane planes[1] = {};
	buf.type = V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE;
	buf.memory = V4L2_MEMORY_MMAP;
	buf.index = i;
	buf.m.planes = planes;
	buf.length = 1;
	planes[0].bytesused = used;
	int64_t stamp = shown + video_base;
	buf.timestamp.tv_sec = stamp / 1000000;
	buf.timestamp.tv_usec = stamp % 1000000;
	if (video_ioctl(d->fd, VIDIOC_QBUF, &buf) < 0) return -1;
	d->out_free[i] = false;
	video_drain(d, 0);
	return 0;
}

int64_t host_video_frame(uint64_t handle, uint32_t *pixels, uint64_t w, uint64_t h, uint64_t stride, int64_t shown)
{
	//the picture shown at that time, waiting a little for the hardware;
	//those to be shown before it are gone
	auto d = (Video_Decoder*)handle;
	if (!d) return -1;
	for (int round = 0; round < 2; round++)
	{
		int64_t got = -1;
		for (auto it = d->held.begin(); it != d->held.end();)
		{
			if (it->shown > shown) { ++it; continue; }
			if (it->shown == shown)
			{
				size_t rows = std::min((size_t)h, (size_t)d->height);
				size_t cols = std::min((size_t)w, (size_t)d->width);
				for (size_t y = 0; y < rows; y++)
					memcpy((uint8_t*)pixels + y * stride, it->pixels.data() + y * d->width, cols * 4);
				got = shown;
			}
			it = d->held.erase(it);
		}
		if (got >= 0) return got;
		if (round == 0) video_drain(d, 30);
	}
	return -1;
}

void host_video_close(uint64_t handle)
{
	auto d = (Video_Decoder*)handle;
	if (!d) return;
	int type = V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE;
	video_ioctl(d->fd, VIDIOC_STREAMOFF, &type);
	type = V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE;
	video_ioctl(d->fd, VIDIOC_STREAMOFF, &type);
	video_free(d);
}

#elif defined(_WIN64)

// Windows: a Media Foundation transform, the first decoder of the system's
// that takes the codec and works as it is called. Packets are given it as
// Annex B, as on Linux, the time shown as the sample's time; pictures come
// back NV12 and are made blue green red alpha here.

#include <windows.h>
#include <mfapi.h>
#include <mfidl.h>
#include <mftransform.h>
#include <mferror.h>

struct Video_Picture
{
	int64_t shown;
	std::vector<uint32_t> pixels;
};

struct Video_Decoder
{
	IMFTransform *mft = nullptr;
	int nal_length = 4;
	std::vector<uint8_t> sets;
	bool first = true;
	uint32_t width = 0, height = 0, plane = 0;
	DWORD out_size = 0;
	bool provides = false;
	std::vector<Video_Picture> held;
};

//mfplat.dll is found as it is wanted, the host starts without it, as on
//the N editions of Windows that have none
//the host is built with no exceptions, but a vector that grows can still
//throw, and linked statically that brings in libstdc++'s whole machinery
//of exceptions and its demangler, 150KB, and so does a new that fails.
//These end the program as a throw would, and keep it out
namespace std
{
	void __throw_length_error(const char*) { abort(); }
	void __throw_bad_alloc() { abort(); }
	void __throw_bad_array_new_length() { abort(); }
}
void *operator new(size_t n) { void *p = malloc(n ? n : 1); if (!p) abort(); return p; }
void *operator new[](size_t n) { void *p = malloc(n ? n : 1); if (!p) abort(); return p; }
void operator delete(void *p) noexcept { free(p); }
void operator delete[](void *p) noexcept { free(p); }
void operator delete(void *p, size_t) noexcept { free(p); }
void operator delete[](void *p, size_t) noexcept { free(p); }

static bool video_started = false;
static HRESULT (WINAPI *mf_startup)(ULONG, DWORD);
static HRESULT (WINAPI *mf_enum)(GUID, UINT32, const MFT_REGISTER_TYPE_INFO*, const MFT_REGISTER_TYPE_INFO*, IMFActivate***, UINT32*);
static HRESULT (WINAPI *mf_type)(IMFMediaType**);
static HRESULT (WINAPI *mf_sample)(IMFSample**);
static HRESULT (WINAPI *mf_buffer)(DWORD, IMFMediaBuffer**);

static bool video_start()
{
	if (video_started) return true;
	HMODULE mf = LoadLibraryA("mfplat.dll");
	if (!mf) return false;
	mf_startup = (decltype(mf_startup))GetProcAddress(mf, "MFStartup");
	mf_enum = (decltype(mf_enum))GetProcAddress(mf, "MFTEnumEx");
	mf_type = (decltype(mf_type))GetProcAddress(mf, "MFCreateMediaType");
	mf_sample = (decltype(mf_sample))GetProcAddress(mf, "MFCreateSample");
	mf_buffer = (decltype(mf_buffer))GetProcAddress(mf, "MFCreateMemoryBuffer");
	if (!mf_startup || !mf_enum || !mf_type || !mf_sample || !mf_buffer) return false;
	CoInitializeEx(nullptr, COINIT_MULTITHREADED);
	if (FAILED(mf_startup(MF_VERSION, MFSTARTUP_LITE))) return false;
	video_started = true;
	return true;
}

static void video_free(Video_Decoder *d)
{
	if (d->mft) d->mft->Release();
	delete d;
}

static bool video_output_type(Video_Decoder *d)
{
	//NV12 of the types it offers, the size of a picture and of the planes
	for (DWORD i = 0;; i++)
	{
		IMFMediaType *type = nullptr;
		if (FAILED(d->mft->GetOutputAvailableType(0, i, &type))) return false;
		GUID sub = {};
		type->GetGUID(MF_MT_SUBTYPE, &sub);
		if (sub == MFVideoFormat_NV12)
		{
			HRESULT r = d->mft->SetOutputType(0, type, 0);
			UINT32 w = 0, h = 0;
			MFGetAttributeSize(type, MF_MT_FRAME_SIZE, &w, &h);
			d->plane = h;
			d->width = w;
			d->height = h;
			//the frame is a whole number of blocks, what is seen may be less
			MFVideoArea area = {};
			if (SUCCEEDED(type->GetBlob(MF_MT_MINIMUM_DISPLAY_APERTURE, (UINT8*)&area, sizeof(area), nullptr))
				&& area.Area.cx && area.Area.cy)
			{
				d->width = area.Area.cx;
				d->height = area.Area.cy;
			}
			type->Release();
			if (FAILED(r)) return false;
			MFT_OUTPUT_STREAM_INFO info = {};
			if (FAILED(d->mft->GetOutputStreamInfo(0, &info))) return false;
			d->out_size = info.cbSize;
			d->provides = (info.dwFlags & (MFT_OUTPUT_STREAM_PROVIDES_SAMPLES | MFT_OUTPUT_STREAM_CAN_PROVIDE_SAMPLES)) != 0;
			return true;
		}
		type->Release();
	}
}

static void video_convert(Video_Decoder *d, const uint8_t *yuv, LONG pitch, std::vector<uint32_t> &out)
{
	//NV12, a plane of Y, then U and V side by side a quarter, BT.601 limited
	uint32_t w = d->width, h = d->height;
	const uint8_t *py = yuv, *puv = yuv + (size_t)pitch * d->plane;
	out.resize((size_t)w * h);
	for (uint32_t y = 0; y < h; y++)
	{
		for (uint32_t x = 0; x < w; x++)
		{
			int c = 298 * ((int)py[y * pitch + x] - 16);
			int u = (int)puv[(y / 2) * pitch + (x & ~1)] - 128;
			int v = (int)puv[(y / 2) * pitch + (x & ~1) + 1] - 128;
			int r = (c + 409 * v + 128) >> 8;
			int g = (c - 100 * u - 208 * v + 128) >> 8;
			int b = (c + 516 * u + 128) >> 8;
			r = r < 0 ? 0 : r > 255 ? 255 : r;
			g = g < 0 ? 0 : g > 255 ? 255 : g;
			b = b < 0 ? 0 : b > 255 ? 255 : b;
			out[(size_t)y * w + x] = 0xff000000 | (r << 16) | (g << 8) | b;
		}
	}
}

static void video_drain(Video_Decoder *d)
{
	//the pictures it has done, a new size taken as it says so
	for (;;)
	{
		MFT_OUTPUT_DATA_BUFFER out = {};
		IMFSample *sample = nullptr;
		if (!d->provides)
		{
			IMFMediaBuffer *buffer = nullptr;
			if (FAILED(mf_sample(&sample))) return;
			if (FAILED(mf_buffer(d->out_size, &buffer))) { sample->Release(); return; }
			sample->AddBuffer(buffer);
			buffer->Release();
			out.pSample = sample;
		}
		DWORD status = 0;
		HRESULT r = d->mft->ProcessOutput(0, 1, &out, &status);
		if (out.pEvents) out.pEvents->Release();
		if (r == MF_E_TRANSFORM_STREAM_CHANGE)
		{
			if (sample) sample->Release();
			if (out.pSample && out.pSample != sample) out.pSample->Release();
			if (!video_output_type(d)) return;
			continue;
		}
		if (FAILED(r))
		{
			if (sample) sample->Release();
			return;
		}
		IMFSample *got = out.pSample;
		LONGLONG time = 0;
		IMFMediaBuffer *buffer = nullptr;
		if (got && SUCCEEDED(got->GetSampleTime(&time)) && SUCCEEDED(got->ConvertToContiguousBuffer(&buffer)))
		{
			Video_Picture pic;
			pic.shown = time / 10;
			IMF2DBuffer *flat = nullptr;
			BYTE *at = nullptr;
			LONG pitch = 0;
			if (SUCCEEDED(buffer->QueryInterface(IID_IMF2DBuffer, (void**)&flat)) && SUCCEEDED(flat->Lock2D(&at, &pitch)))
			{
				video_convert(d, at, pitch, pic.pixels);
				flat->Unlock2D();
				d->held.push_back(std::move(pic));
			}
			else
			{
				DWORD size = 0;
				if (SUCCEEDED(buffer->Lock(&at, nullptr, &size)))
				{
					//no pitch said: the width of the frame, a whole number of blocks
					video_convert(d, at, (LONG)((d->width + 15) & ~15), pic.pixels);
					buffer->Unlock();
					d->held.push_back(std::move(pic));
				}
			}
			if (flat) flat->Release();
			buffer->Release();
		}
		if (got) got->Release();
		if (sample && sample != got) sample->Release();
	}
}

uint64_t host_video_open(uint64_t codec, const uint8_t *setup, uint64_t len)
{
	//avc1 H.264, hvc1 H.265, the parameter sets each after a start code
	std::vector<const uint8_t*> sets;
	std::vector<size_t> sizes;
	int nal_length = 4;
	GUID kind;
	if (codec == 0x61766331) { if (!video_avcc(setup, len, sets, sizes, nal_length)) return 0; kind = MFVideoFormat_H264; }
	else if (codec == 0x68766331) { if (!video_hvcc(setup, len, sets, sizes, nal_length)) return 0; kind = MFVideoFormat_HEVC; }
	else return 0;
	if (!video_start()) return 0;
	auto d = new Video_Decoder();
	d->nal_length = nal_length;
	for (size_t i = 0; i < sets.size(); i++)
	{
		static const uint8_t start[] = {0, 0, 0, 1};
		d->sets.insert(d->sets.end(), start, start + 4);
		d->sets.insert(d->sets.end(), sets[i], sets[i] + sizes[i]);
	}
	//the decoders that take it, those that work as they are called
	MFT_REGISTER_TYPE_INFO in = {MFMediaType_Video, kind};
	IMFActivate **found = nullptr;
	UINT32 count = 0;
	if (FAILED(mf_enum(MFT_CATEGORY_VIDEO_DECODER, MFT_ENUM_FLAG_SYNCMFT | MFT_ENUM_FLAG_SORTANDFILTER,
			&in, nullptr, &found, &count)) || !count) { video_free(d); return 0; }
	for (UINT32 i = 0; i < count; i++)
	{
		if (!d->mft) found[i]->ActivateObject(IID_IMFTransform, (void**)&d->mft);
		found[i]->Release();
	}
	CoTaskMemFree(found);
	if (!d->mft) { video_free(d); return 0; }
	IMFMediaType *type = nullptr;
	if (FAILED(mf_type(&type))) { video_free(d); return 0; }
	type->SetGUID(MF_MT_MAJOR_TYPE, MFMediaType_Video);
	type->SetGUID(MF_MT_SUBTYPE, kind);
	type->SetUINT32(MF_MT_INTERLACE_MODE, MFVideoInterlace_MixedInterlaceOrProgressive);
	HRESULT r = d->mft->SetInputType(0, type, 0);
	type->Release();
	if (FAILED(r) || !video_output_type(d)) { video_free(d); return 0; }
	d->mft->ProcessMessage(MFT_MESSAGE_NOTIFY_BEGIN_STREAMING, 0);
	d->mft->ProcessMessage(MFT_MESSAGE_NOTIFY_START_OF_STREAM, 0);
	return (uint64_t)d;
}

int64_t host_video_decode(uint64_t handle, const uint8_t *data, uint64_t len, int64_t shown)
{
	auto d = (Video_Decoder*)handle;
	if (!d) return -1;
	//nothing: the end, what the decoder holds back given up
	if (len == 0)
	{
		d->mft->ProcessMessage(MFT_MESSAGE_NOTIFY_END_OF_STREAM, 0);
		d->mft->ProcessMessage(MFT_MESSAGE_COMMAND_DRAIN, 0);
		video_drain(d);
		return 0;
	}
	//Annex B: the sets before the first packet, each NAL after a start code
	std::vector<uint8_t> annexb;
	if (d->first)
	{
		annexb = d->sets;
		d->first = false;
	}
	for (uint64_t k = 0; k + d->nal_length <= len;)
	{
		size_t n = 0;
		for (int b = 0; b < d->nal_length; b++) n = (n << 8) | data[k + b];
		k += d->nal_length;
		if (k + n > len) break;
		static const uint8_t start[] = {0, 0, 0, 1};
		annexb.insert(annexb.end(), start, start + 4);
		annexb.insert(annexb.end(), data + k, data + k + n);
		k += n;
	}
	IMFMediaBuffer *buffer = nullptr;
	IMFSample *sample = nullptr;
	if (FAILED(mf_buffer((DWORD)annexb.size(), &buffer))) return -1;
	BYTE *at = nullptr;
	if (FAILED(buffer->Lock(&at, nullptr, nullptr))) { buffer->Release(); return -1; }
	memcpy(at, annexb.data(), annexb.size());
	buffer->Unlock();
	buffer->SetCurrentLength((DWORD)annexb.size());
	if (FAILED(mf_sample(&sample))) { buffer->Release(); return -1; }
	sample->AddBuffer(buffer);
	buffer->Release();
	sample->SetSampleTime(shown * 10);
	//taken, or not till what it has done is out
	HRESULT r = d->mft->ProcessInput(0, sample, 0);
	if (r == MF_E_NOTACCEPTING)
	{
		video_drain(d);
		r = d->mft->ProcessInput(0, sample, 0);
	}
	sample->Release();
	video_drain(d);
	return SUCCEEDED(r) ? 0 : -1;
}

int64_t host_video_frame(uint64_t handle, uint32_t *pixels, uint64_t w, uint64_t h, uint64_t stride, int64_t shown)
{
	//the picture shown at that time; those to be shown before it are gone
	auto d = (Video_Decoder*)handle;
	if (!d) return -1;
	int64_t got = -1;
	for (auto it = d->held.begin(); it != d->held.end();)
	{
		if (it->shown > shown) { ++it; continue; }
		if (it->shown == shown)
		{
			size_t rows = std::min((size_t)h, (size_t)d->height);
			size_t cols = std::min((size_t)w, (size_t)d->width);
			for (size_t y = 0; y < rows; y++)
				memcpy((uint8_t*)pixels + y * stride, it->pixels.data() + y * d->width, cols * 4);
			got = shown;
		}
		it = d->held.erase(it);
	}
	return got;
}

void host_video_close(uint64_t handle)
{
	auto d = (Video_Decoder*)handle;
	if (!d) return;
	d->mft->ProcessMessage(MFT_MESSAGE_NOTIFY_END_STREAMING, 0);
	video_free(d);
}

#else

uint64_t host_video_open(uint64_t codec, const uint8_t *setup, uint64_t len) { return 0; }
int64_t host_video_decode(uint64_t handle, const uint8_t *data, uint64_t len, int64_t shown) { return -1; }
int64_t host_video_frame(uint64_t handle, uint32_t *pixels, uint64_t w, uint64_t h, uint64_t stride, int64_t shown) { return -1; }
void host_video_close(uint64_t handle) {}

#endif
