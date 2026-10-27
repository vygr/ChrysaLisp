// video.cpp, the decoders a host has for the films of lib/media, called by
// the GUI's table of the driver this host was built with.
//
// open: a decoder for a codec, its 4 letters high first, from its setup as
// the film has it, avcC of avc1, H.264, hvcC of hvc1, H.265, 0 if this host
// has none for it. Of mp4v, MPEG-4 Part 2, and jpeg, Motion JPEG, the setup
// is the picture's width and height, four bytes each low first, and of
// mp4v then its headers, VOS, VO and VOL, as the start of its first frame
// has them: their packets are given as they are. decode: one packet of the
// film, in the order they are decoded, and when it is shown, -1 if it could
// not be. The pictures are held till asked for. frame: the picture shown at
// a time, its pixels of 32 bits, blue green red alpha, into the buffer
// given, and that time; -1 if it is not out yet, with B-frames a picture is
// out only once those it was made from are. Those to be shown before it go.
// close: the decoder gone.
//
// macOS: VideoToolbox, the hardware's decoder, H.264 and H.265 and Motion
// JPEG, and MPEG-4 Part 2 by its own. Linux: VA-API, a PC's graphics, H.264
// and Motion JPEG, if it was built with libva's headers and the machine has
// a driver for them, else the stateful decoder of V4L2, /dev/video10, H.264
// and Motion JPEG, the Raspberry Pi's. Windows: a
// decoder of Media Foundation, H.264, and H.265 if one is installed, MPEG-4
// Part 2 and Motion JPEG, not yet tried on Windows itself. Elsewhere none,
// open is 0.

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

static CFDictionaryRef video_esds(const uint8_t *headers, uint64_t len)
{
	//the esds an mp4 has of MPEG-4 Part 2, of its headers: an ES descriptor,
	//of it a decoder config of MPEG-4 visual and its specific info, the
	//headers, and a sync layer config, each a tag and a length
	if (len > 100000) return nullptr;
	std::vector<uint8_t> esds = {0, 0, 0, 0};
	auto descriptor = [&](uint8_t tag, size_t n)
	{
		esds.push_back(tag);
		esds.push_back(0x80 | ((n >> 21) & 0x7f)); esds.push_back(0x80 | ((n >> 14) & 0x7f));
		esds.push_back(0x80 | ((n >> 7) & 0x7f)); esds.push_back(n & 0x7f);
	};
	descriptor(0x03, 3 + 5 + 13 + 5 + len + 5 + 1);
	esds.insert(esds.end(), {0, 1, 0});
	descriptor(0x04, 13 + 5 + len);
	esds.insert(esds.end(), {0x20, 0x11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0});
	descriptor(0x05, len);
	esds.insert(esds.end(), headers, headers + len);
	descriptor(0x06, 1);
	esds.push_back(2);
	auto data = CFDataCreate(kCFAllocatorDefault, esds.data(), esds.size());
	const void *k1[] = {CFSTR("esds")}, *v1[] = {data};
	auto atoms = CFDictionaryCreate(kCFAllocatorDefault, k1, v1, 1, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
	const void *k2[] = {kCMFormatDescriptionExtension_SampleDescriptionExtensionAtoms}, *v2[] = {atoms};
	auto ext = CFDictionaryCreate(kCFAllocatorDefault, k2, v2, 1, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
	CFRelease(atoms);
	CFRelease(data);
	return ext;
}

uint64_t host_video_open(uint64_t codec, const uint8_t *setup, uint64_t len)
{
	//the parameter sets of the codec's setup, avc1 H.264, hvc1 H.265; of
	//mp4v and jpeg the size, and of mp4v its headers
	std::vector<const uint8_t*> sets;
	std::vector<size_t> sizes;
	int nal_length = 4;
	bool hevc = codec == 0x68766331, mp4v = codec == 0x6d703476, jpeg = codec == 0x6a706567;
	if (codec == 0x61766331) { if (!video_avcc(setup, len, sets, sizes, nal_length)) return 0; }
	else if (hevc) { if (!video_hvcc(setup, len, sets, sizes, nal_length)) return 0; }
	else if ((mp4v || jpeg) && len >= 8) {}
	else return 0;
	auto decoder = new Video_Decoder();
	OSStatus made;
	if (mp4v || jpeg)
	{
		int32_t w = (int32_t)(setup[0] | (setup[1] << 8) | (setup[2] << 16) | ((uint32_t)setup[3] << 24));
		int32_t h = (int32_t)(setup[4] | (setup[5] << 8) | (setup[6] << 16) | ((uint32_t)setup[7] << 24));
		auto ext = mp4v ? video_esds(setup + 8, len - 8) : nullptr;
		made = CMVideoFormatDescriptionCreate(kCFAllocatorDefault,
			mp4v ? kCMVideoCodecType_MPEG4Video : kCMVideoCodecType_JPEG, w, h, ext, &decoder->format);
		if (ext) CFRelease(ext);
	}
	else made = hevc
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

// Linux, first: VA-API, the decoder of a PC's graphics, Intel's or AMD's,
// H.264 and Motion JPEG. It is in every Linux build, no libva-dev needed,
// and libva is loaded as it is wanted, so the host starts on a machine
// without it, and it is used only for a profile its driver lists. Its decoding is
// stateless: the stream's headers are read here, and what each picture is
// made from is kept and told it, as the standard has it. The pictures come
// back NV12, or as a JPEG's sampling has them, and are made blue green red
// alpha as they are asked for. What it can not, and on a Raspberry Pi, goes
// to the V4L2 decoder below.

#include <stdio.h>
#include <dlfcn.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/ioctl.h>
#include <linux/dma-buf.h>

//what is used of VA-API, as libva 2.x has it, libva.so.2 and libva-drm.so.2:
//its types and constants, the buffers of H.264 and of baseline JPEG, the
//DRM PRIME descriptor, and the calls, which are found by dlsym. Declared
//here and not included, so every Linux build has it, with or without
//libva-dev, and the host still starts where there is no libva
typedef void *VADisplay;
typedef int VAStatus;
typedef unsigned int VAGenericID;
typedef VAGenericID VAConfigID;
typedef VAGenericID VAContextID;
typedef VAGenericID VASurfaceID;
typedef VAGenericID VABufferID;
typedef VAGenericID VAImageID;
typedef void (*VAMessageCallback)(void *user_context, const char *message);
struct VASurfaceAttrib;

enum VAProfile
{
	VAProfileNone = -1,
	VAProfileH264Main = 6,
	VAProfileH264High = 7,
	VAProfileJPEGBaseline = 12,
	VAProfileH264ConstrainedBaseline = 13
};
enum VAEntrypoint { VAEntrypointVLD = 1 };
enum VAConfigAttribType { VAConfigAttribRTFormat = 0 };
enum VABufferType
{
	VAPictureParameterBufferType = 0,
	VAIQMatrixBufferType = 1,
	VASliceParameterBufferType = 4,
	VASliceDataBufferType = 5,
	VAHuffmanTableBufferType = 12
};

#define VA_STATUS_SUCCESS 0x00000000
#define VA_INVALID_ID 0xffffffff
#define VA_INVALID_SURFACE VA_INVALID_ID
#define VA_PROGRESSIVE 0x1
#define VA_RT_FORMAT_YUV420 0x00000001
#define VA_RT_FORMAT_YUV422 0x00000002
#define VA_RT_FORMAT_YUV444 0x00000004
#define VA_RT_FORMAT_YUV411 0x00000008
#define VA_RT_FORMAT_YUV400 0x00000010
#define VA_SLICE_DATA_FLAG_ALL 0x00
#define VA_PICTURE_H264_INVALID 0x00000001
#define VA_PICTURE_H264_SHORT_TERM_REFERENCE 0x00000008
#define VA_PICTURE_H264_LONG_TERM_REFERENCE 0x00000010
#define VA_EXPORT_SURFACE_READ_ONLY 0x0001
#define VA_EXPORT_SURFACE_COMPOSED_LAYERS 0x0008
#define VA_SURFACE_ATTRIB_MEM_TYPE_DRM_PRIME_2 0x40000000
#define VA_LSB_FIRST 1
#define VA_FOURCC(ch0, ch1, ch2, ch3) \
	((unsigned long)(unsigned char)(ch0) | ((unsigned long)(unsigned char)(ch1) << 8) | \
	((unsigned long)(unsigned char)(ch2) << 16) | ((unsigned long)(unsigned char)(ch3) << 24))
#define VA_FOURCC_NV12 0x3231564E
#define VA_FOURCC_YUY2 0x32595559
#define VA_FOURCC_UYVY 0x59565955
#define VA_FOURCC_YV12 0x32315659
#define VA_FOURCC_I420 0x30323449
#define VA_FOURCC_IMC3 0x33434D49
#define VA_FOURCC_411P 0x50313134
#define VA_FOURCC_422H 0x48323234
#define VA_FOURCC_422V 0x56323234
#define VA_FOURCC_444P 0x50343434
#define VA_FOURCC_Y800 0x30303859

struct VAConfigAttrib
{
	VAConfigAttribType type;
	uint32_t value;
};

struct VAImageFormat
{
	uint32_t fourcc, byte_order, bits_per_pixel, depth;
	uint32_t red_mask, green_mask, blue_mask, alpha_mask;
	uint32_t va_reserved[4];
};

struct VAImage
{
	VAImageID image_id;
	VAImageFormat format;
	VABufferID buf;
	uint16_t width, height;
	uint32_t data_size, num_planes, pitches[3], offsets[3];
	int32_t num_palette_entries, entry_bytes;
	int8_t component_order[4];
	uint32_t va_reserved[4];
};

struct VAPictureH264
{
	VASurfaceID picture_id;
	uint32_t frame_idx, flags;
	int32_t TopFieldOrderCnt, BottomFieldOrderCnt;
	uint32_t va_reserved[4];
};

struct VAPictureParameterBufferH264
{
	VAPictureH264 CurrPic;
	VAPictureH264 ReferenceFrames[16];
	uint16_t picture_width_in_mbs_minus1, picture_height_in_mbs_minus1;
	uint8_t bit_depth_luma_minus8, bit_depth_chroma_minus8, num_ref_frames;
	union
	{
		struct
		{
			uint32_t chroma_format_idc : 2;
			uint32_t residual_colour_transform_flag : 1;
			uint32_t gaps_in_frame_num_value_allowed_flag : 1;
			uint32_t frame_mbs_only_flag : 1;
			uint32_t mb_adaptive_frame_field_flag : 1;
			uint32_t direct_8x8_inference_flag : 1;
			uint32_t MinLumaBiPredSize8x8 : 1;
			uint32_t log2_max_frame_num_minus4 : 4;
			uint32_t pic_order_cnt_type : 2;
			uint32_t log2_max_pic_order_cnt_lsb_minus4 : 4;
			uint32_t delta_pic_order_always_zero_flag : 1;
		} bits;
		uint32_t value;
	} seq_fields;
	uint8_t num_slice_groups_minus1, slice_group_map_type;
	uint16_t slice_group_change_rate_minus1;
	int8_t pic_init_qp_minus26, pic_init_qs_minus26;
	int8_t chroma_qp_index_offset, second_chroma_qp_index_offset;
	union
	{
		struct
		{
			uint32_t entropy_coding_mode_flag : 1;
			uint32_t weighted_pred_flag : 1;
			uint32_t weighted_bipred_idc : 2;
			uint32_t transform_8x8_mode_flag : 1;
			uint32_t field_pic_flag : 1;
			uint32_t constrained_intra_pred_flag : 1;
			uint32_t pic_order_present_flag : 1;
			uint32_t deblocking_filter_control_present_flag : 1;
			uint32_t redundant_pic_cnt_present_flag : 1;
			uint32_t reference_pic_flag : 1;
		} bits;
		uint32_t value;
	} pic_fields;
	uint16_t frame_num;
	uint32_t va_reserved[8];
};

struct VAIQMatrixBufferH264
{
	uint8_t ScalingList4x4[6][16];
	uint8_t ScalingList8x8[2][64];
	uint32_t va_reserved[4];
};

struct VASliceParameterBufferH264
{
	uint32_t slice_data_size, slice_data_offset, slice_data_flag;
	uint16_t slice_data_bit_offset, first_mb_in_slice;
	uint8_t slice_type, direct_spatial_mv_pred_flag;
	uint8_t num_ref_idx_l0_active_minus1, num_ref_idx_l1_active_minus1;
	uint8_t cabac_init_idc;
	int8_t slice_qp_delta;
	uint8_t disable_deblocking_filter_idc;
	int8_t slice_alpha_c0_offset_div2, slice_beta_offset_div2;
	VAPictureH264 RefPicList0[32];
	VAPictureH264 RefPicList1[32];
	uint8_t luma_log2_weight_denom, chroma_log2_weight_denom;
	uint8_t luma_weight_l0_flag;
	int16_t luma_weight_l0[32], luma_offset_l0[32];
	uint8_t chroma_weight_l0_flag;
	int16_t chroma_weight_l0[32][2], chroma_offset_l0[32][2];
	uint8_t luma_weight_l1_flag;
	int16_t luma_weight_l1[32], luma_offset_l1[32];
	uint8_t chroma_weight_l1_flag;
	int16_t chroma_weight_l1[32][2], chroma_offset_l1[32][2];
	uint32_t va_reserved[4];
};

struct VARectangle
{
	int16_t x, y;
	uint16_t width, height;
};

struct VAPictureParameterBufferJPEGBaseline
{
	uint16_t picture_width, picture_height;
	struct
	{
		uint8_t component_id, h_sampling_factor, v_sampling_factor, quantiser_table_selector;
	} components[255];
	uint8_t num_components, color_space;
	uint32_t rotation;
	VARectangle crop_rectangle;
	uint32_t va_reserved[5];
};

struct VAIQMatrixBufferJPEGBaseline
{
	uint8_t load_quantiser_table[4];
	uint8_t quantiser_table[4][64];
	uint32_t va_reserved[4];
};

struct VAHuffmanTableBufferJPEGBaseline
{
	uint8_t load_huffman_table[2];
	struct
	{
		uint8_t num_dc_codes[16], dc_values[12], num_ac_codes[16], ac_values[162], pad[2];
	} huffman_table[2];
	uint32_t va_reserved[4];
};

struct VASliceParameterBufferJPEGBaseline
{
	uint32_t slice_data_size, slice_data_offset, slice_data_flag;
	uint32_t slice_horizontal_position, slice_vertical_position;
	struct
	{
		uint8_t component_selector, dc_table_selector, ac_table_selector;
	} components[4];
	uint8_t num_components;
	uint16_t restart_interval;
	uint32_t num_mcus;
	uint32_t va_reserved[4];
};

struct VADRMPRIMESurfaceDescriptor
{
	uint32_t fourcc, width, height, num_objects;
	struct
	{
		int fd;
		uint32_t size;
		uint64_t drm_format_modifier;
	} objects[4];
	uint32_t num_layers;
	struct
	{
		uint32_t drm_format, num_planes, object_index[4], offset[4], pitch[4];
	} layers[4];
};

extern "C"
{
	VAStatus vaInitialize(VADisplay dpy, int *major_version, int *minor_version);
	VAMessageCallback vaSetInfoCallback(VADisplay dpy, VAMessageCallback callback, void *user_context);
	VAMessageCallback vaSetErrorCallback(VADisplay dpy, VAMessageCallback callback, void *user_context);
	int vaMaxNumProfiles(VADisplay dpy);
	VAStatus vaQueryConfigProfiles(VADisplay dpy, VAProfile *profile_list, int *num_profiles);
	int vaMaxNumEntrypoints(VADisplay dpy);
	VAStatus vaQueryConfigEntrypoints(VADisplay dpy, VAProfile profile, VAEntrypoint *entrypoint_list, int *num_entrypoints);
	VAStatus vaCreateConfig(VADisplay dpy, VAProfile profile, VAEntrypoint entrypoint, VAConfigAttrib *attrib_list, int num_attribs, VAConfigID *config_id);
	VAStatus vaDestroyConfig(VADisplay dpy, VAConfigID config_id);
	VAStatus vaCreateSurfaces(VADisplay dpy, unsigned int format, unsigned int width, unsigned int height,
		VASurfaceID *surfaces, unsigned int num_surfaces, VASurfaceAttrib *attrib_list, unsigned int num_attribs);
	VAStatus vaDestroySurfaces(VADisplay dpy, VASurfaceID *surfaces, int num_surfaces);
	VAStatus vaCreateContext(VADisplay dpy, VAConfigID config_id, int picture_width, int picture_height, int flag,
		VASurfaceID *render_targets, int num_render_targets, VAContextID *context);
	VAStatus vaDestroyContext(VADisplay dpy, VAContextID context);
	VAStatus vaCreateBuffer(VADisplay dpy, VAContextID context, VABufferType type, unsigned int size,
		unsigned int num_elements, void *data, VABufferID *buf_id);
	VAStatus vaDestroyBuffer(VADisplay dpy, VABufferID buffer_id);
	VAStatus vaBeginPicture(VADisplay dpy, VAContextID context, VASurfaceID render_target);
	VAStatus vaRenderPicture(VADisplay dpy, VAContextID context, VABufferID *buffers, int num_buffers);
	VAStatus vaEndPicture(VADisplay dpy, VAContextID context);
	VAStatus vaSyncSurface(VADisplay dpy, VASurfaceID render_target);
	VAStatus vaCreateImage(VADisplay dpy, VAImageFormat *format, int width, int height, VAImage *image);
	VAStatus vaDestroyImage(VADisplay dpy, VAImageID image);
	VAStatus vaGetImage(VADisplay dpy, VASurfaceID surface, int x, int y, unsigned int width, unsigned int height, VAImageID image);
	VAStatus vaDeriveImage(VADisplay dpy, VASurfaceID surface, VAImage *image);
	VAStatus vaMapBuffer(VADisplay dpy, VABufferID buf_id, void **pbuf);
	VAStatus vaUnmapBuffer(VADisplay dpy, VABufferID buf_id);
	VAStatus vaExportSurfaceHandle(VADisplay dpy, VASurfaceID surface_id, uint32_t mem_type, uint32_t flags, void *descriptor);
	VADisplay vaGetDisplayDRM(int fd);
}

//the calls of libva, found as it is loaded
#define VA_CALLS(X) X(vaInitialize) X(vaSetInfoCallback) X(vaSetErrorCallback) \
	X(vaMaxNumProfiles) X(vaQueryConfigProfiles) X(vaMaxNumEntrypoints) X(vaQueryConfigEntrypoints) \
	X(vaCreateConfig) X(vaDestroyConfig) X(vaCreateSurfaces) X(vaDestroySurfaces) \
	X(vaCreateContext) X(vaDestroyContext) X(vaCreateBuffer) X(vaDestroyBuffer) \
	X(vaBeginPicture) X(vaRenderPicture) X(vaEndPicture) X(vaSyncSurface) \
	X(vaCreateImage) X(vaDestroyImage) X(vaGetImage) X(vaDeriveImage) X(vaMapBuffer) X(vaUnmapBuffer)
namespace va
{
#define VA_POINTER(f) static decltype(&::f) f = nullptr;
	VA_CALLS(VA_POINTER)
#undef VA_POINTER
}

static void va_quiet(void*, const char*) {}

static VADisplay va_display()
{
	//once: libva, its DRM part, a render node and its driver, :nil if any
	//is not there
	static bool tried = false;
	static VADisplay display = nullptr;
	if (tried) return display;
	tried = true;
	void *lib = dlopen("libva.so.2", RTLD_NOW | RTLD_LOCAL);
	void *drm = lib ? dlopen("libva-drm.so.2", RTLD_NOW | RTLD_LOCAL) : nullptr;
	if (!drm) return nullptr;
#define VA_LOAD(f) if (!(va::f = (decltype(va::f))dlsym(lib, #f))) return nullptr;
	VA_CALLS(VA_LOAD)
#undef VA_LOAD
	auto get = (decltype(&vaGetDisplayDRM))dlsym(drm, "vaGetDisplayDRM");
	if (!get) return nullptr;
	for (int node = 128; node < 136 && !display; node++)
	{
		char path[32];
		snprintf(path, sizeof(path), "/dev/dri/renderD%d", node);
		int fd = open(path, O_RDWR | O_CLOEXEC);
		if (fd < 0) continue;
		VADisplay d = get(fd);
		int major, minor;
		//libva says what it does on stderr, the host's own output
		if (d) va::vaSetInfoCallback(d, va_quiet, nullptr), va::vaSetErrorCallback(d, va_quiet, nullptr);
		if (d && va::vaInitialize(d, &major, &minor) == VA_STATUS_SUCCESS) display = d;
		else close(fd);
	}
	return display;
}

static bool va_has(VADisplay dpy, VAProfile profile)
{
	//the driver decodes that profile, VLD, the whole of the decoding
	int n = 0;
	std::vector<VAProfile> profiles(va::vaMaxNumProfiles(dpy));
	if (va::vaQueryConfigProfiles(dpy, profiles.data(), &n) != VA_STATUS_SUCCESS
		|| std::find(profiles.begin(), profiles.begin() + n, profile) == profiles.begin() + n) return false;
	std::vector<VAEntrypoint> points(va::vaMaxNumEntrypoints(dpy));
	if (va::vaQueryConfigEntrypoints(dpy, profile, points.data(), &n) != VA_STATUS_SUCCESS) return false;
	return std::find(points.begin(), points.begin() + n, VAEntrypointVLD) != points.begin() + n;
}

//the bits of a NAL with its emulation prevention bytes taken out, its RBSP
struct Va_Bits
{
	const uint8_t *p;
	size_t n, at;
	uint32_t bit()
	{
		uint32_t b = at < n * 8 ? (p[at >> 3] >> (7 - (at & 7))) & 1 : 0;
		at++;
		return b;
	}
	uint32_t u(int k) { uint32_t v = 0; while (k--) v = (v << 1) | bit(); return v; }
	uint32_t ue()
	{
		int zeros = 0;
		while (!bit()) if (++zeros == 32) return 0;
		return ((1u << zeros) - 1) + u(zeros);
	}
	int32_t se() { uint32_t k = ue(); return (k & 1) ? (int32_t)((k + 1) >> 1) : -(int32_t)(k >> 1); }
	bool over() const { return at > n * 8; }
	bool more() const
	{
		//more before the stop bit, the last bit of 1
		size_t last = n * 8;
		while (last && !((p[(last - 1) >> 3] >> (7 - ((last - 1) & 7))) & 1)) last--;
		return at + 1 < last;
	}
};

static void va_rbsp(const uint8_t *nal, size_t len, std::vector<uint8_t> &out)
{
	out.clear();
	int zeros = 0;
	for (size_t i = 0; i < len; i++)
	{
		if (zeros >= 2 && nal[i] == 3) { zeros = 0; continue; }
		zeros = nal[i] ? 0 : zeros + 1;
		out.push_back(nal[i]);
	}
}

//the scaling lists, zig zag to raster, and the defaults of the standard's
//tables 7-3 and 7-4 in zig zag order, intra and inter
static const uint8_t va_zigzag4[16] = {0, 1, 4, 8, 5, 2, 3, 6, 9, 12, 13, 10, 7, 11, 14, 15};
static const uint8_t va_zigzag8[64] = {
	0, 1, 8, 16, 9, 2, 3, 10, 17, 24, 32, 25, 18, 11, 4, 5,
	12, 19, 26, 33, 40, 48, 41, 34, 27, 20, 13, 6, 7, 14, 21, 28,
	35, 42, 49, 56, 57, 50, 43, 36, 29, 22, 15, 23, 30, 37, 44, 51,
	58, 59, 52, 45, 38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63};
static const uint8_t va_default4[2][16] = {
	{6, 13, 13, 20, 20, 20, 28, 28, 28, 28, 32, 32, 32, 37, 37, 42},
	{10, 14, 14, 20, 20, 20, 24, 24, 24, 24, 27, 27, 27, 30, 30, 34}};
static const uint8_t va_default8[2][64] = {
	{6, 10, 10, 13, 11, 13, 16, 16, 16, 16, 18, 18, 18, 18, 18, 23,
	23, 23, 23, 23, 23, 25, 25, 25, 25, 25, 25, 25, 27, 27, 27, 27,
	27, 27, 27, 27, 29, 29, 29, 29, 29, 29, 29, 31, 31, 31, 31, 31,
	31, 33, 33, 33, 33, 33, 36, 36, 36, 36, 38, 38, 38, 40, 40, 42},
	{9, 13, 13, 15, 13, 15, 17, 17, 17, 17, 19, 19, 19, 19, 19, 21,
	21, 21, 21, 21, 21, 22, 22, 22, 22, 22, 22, 22, 24, 24, 24, 24,
	24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 27, 27, 27, 27, 27,
	27, 28, 28, 28, 28, 28, 30, 30, 30, 30, 32, 32, 32, 33, 33, 35}};

struct Va_Sps
{
	bool valid = false;
	int profile_idc, constraints, level_idc, chroma_format_idc, separate_colour_plane;
	int bit_depth_luma, bit_depth_chroma;
	bool scaling;
	uint8_t lists4[6][16], lists8[6][64];
	int log2_max_frame_num, poc_type, log2_max_poc_lsb, delta_pic_order_always_zero;
	int offset_for_non_ref_pic, offset_for_top_to_bottom_field, cycle;
	int offset_for_ref_frame[256];
	int max_num_ref_frames, gaps, width_mbs, height_mbs, frame_mbs_only, mb_adaptive, direct_8x8;
	int crop_left, crop_right, crop_top, crop_bottom;
	bool full;
};

struct Va_Pps
{
	bool valid = false;
	int sps_id, entropy, bottom_field_pic_order, num_ref_idx_default[2];
	int weighted_pred, weighted_bipred_idc, pic_init_qp, pic_init_qs, chroma_qp_index_offset;
	int deblocking, constrained_intra, redundant, transform_8x8, second_chroma_qp_index_offset;
	bool scaling;
	uint8_t lists4[6][16], lists8[6][64];
};

static void va_default_list(uint8_t *list, int i)
{
	if (i < 6) for (int j = 0; j < 16; j++) list[va_zigzag4[j]] = va_default4[i >= 3][j];
	else for (int j = 0; j < 64; j++) list[va_zigzag8[j]] = va_default8[(i - 6) & 1][j];
}

static void va_scaling(Va_Bits &b, int count, uint8_t lists4[6][16], uint8_t lists8[6][64], const Va_Sps *fall)
{
	//the lists of an SPS or a PPS. One not there is by fall-back rule A,
	//the defaults, or for a PPS of an SPS with lists rule B, the SPS's, and
	//past the first of its kind the one before. One that says so the default
	for (int i = 0; i < count; i++)
	{
		uint8_t *list = i < 6 ? lists4[i] : lists8[i - 6];
		int size = i < 6 ? 16 : 64;
		if (b.u(1))
		{
			const uint8_t *zigzag = size == 16 ? va_zigzag4 : va_zigzag8;
			int last = 8, next = 8;
			for (int j = 0; j < size; j++)
			{
				if (next)
				{
					next = (last + b.se() + 256) % 256;
					if (j == 0 && next == 0) { va_default_list(list, i); break; }
				}
				list[zigzag[j]] = last = next ? next : last;
			}
		}
		else if (i == 0 || i == 3 || i == 6 || i == 7)
		{
			if (fall) memcpy(list, i < 6 ? fall->lists4[i] : fall->lists8[i - 6], size);
			else va_default_list(list, i);
		}
		else memcpy(list, i < 6 ? lists4[i - 1] : lists8[i - 8], size);
	}
}

static bool va_sps(const std::vector<uint8_t> &rbsp, Va_Sps *all)
{
	Va_Bits b = {rbsp.data(), rbsp.size(), 8};
	Va_Sps s;
	s.profile_idc = b.u(8);
	s.constraints = b.u(8);
	s.level_idc = b.u(8);
	uint32_t id = b.ue();
	if (id > 31) return false;
	s.chroma_format_idc = 1;
	s.separate_colour_plane = 0;
	s.bit_depth_luma = s.bit_depth_chroma = 8;
	s.scaling = false;
	memset(s.lists4, 16, sizeof(s.lists4));
	memset(s.lists8, 16, sizeof(s.lists8));
	static const int high[] = {100, 110, 122, 244, 44, 83, 86, 118, 128, 138, 139, 134, 135};
	if (std::find(high, high + 13, s.profile_idc) != high + 13)
	{
		s.chroma_format_idc = b.ue();
		if (s.chroma_format_idc == 3) s.separate_colour_plane = b.u(1);
		s.bit_depth_luma = b.ue() + 8;
		s.bit_depth_chroma = b.ue() + 8;
		b.u(1);
		if ((s.scaling = b.u(1))) va_scaling(b, s.chroma_format_idc != 3 ? 8 : 12, s.lists4, s.lists8, nullptr);
	}
	s.log2_max_frame_num = b.ue() + 4;
	s.poc_type = b.ue();
	s.log2_max_poc_lsb = s.delta_pic_order_always_zero = s.offset_for_non_ref_pic = 0;
	s.offset_for_top_to_bottom_field = s.cycle = 0;
	if (s.poc_type == 0) s.log2_max_poc_lsb = b.ue() + 4;
	else if (s.poc_type == 1)
	{
		s.delta_pic_order_always_zero = b.u(1);
		s.offset_for_non_ref_pic = b.se();
		s.offset_for_top_to_bottom_field = b.se();
		s.cycle = b.ue();
		if (s.cycle > 255) return false;
		for (int i = 0; i < s.cycle; i++) s.offset_for_ref_frame[i] = b.se();
	}
	s.max_num_ref_frames = b.ue();
	s.gaps = b.u(1);
	s.width_mbs = b.ue() + 1;
	int map_units = b.ue() + 1;
	s.frame_mbs_only = b.u(1);
	s.height_mbs = (2 - s.frame_mbs_only) * map_units;
	s.mb_adaptive = s.frame_mbs_only ? 0 : b.u(1);
	s.direct_8x8 = b.u(1);
	s.crop_left = s.crop_right = s.crop_top = s.crop_bottom = 0;
	if (b.u(1))
	{
		//in units of chroma samples, and of fields' rows
		int cx = s.chroma_format_idc == 1 || s.chroma_format_idc == 2 ? 2 : 1;
		int cy = (s.chroma_format_idc == 1 ? 2 : 1) * (2 - s.frame_mbs_only);
		s.crop_left = b.ue() * cx;
		s.crop_right = b.ue() * cx;
		s.crop_top = b.ue() * cy;
		s.crop_bottom = b.ue() * cy;
	}
	//of the VUI only the range, video's 16 to 235 unless it says all
	s.full = false;
	if (b.u(1))
	{
		if (b.u(1) && b.u(8) == 255) b.u(32);
		if (b.u(1)) b.u(1);
		if (b.u(1)) { b.u(3); s.full = b.u(1); }
	}
	if (b.over() || s.max_num_ref_frames > 16 || s.width_mbs * 16 <= s.crop_left + s.crop_right
		|| s.height_mbs * 16 <= s.crop_top + s.crop_bottom) return false;
	s.valid = true;
	all[id] = s;
	return true;
}

static bool va_pps(const std::vector<uint8_t> &rbsp, Va_Pps *all, const Va_Sps *spss)
{
	Va_Bits b = {rbsp.data(), rbsp.size(), 8};
	Va_Pps p;
	uint32_t id = b.ue();
	p.sps_id = b.ue();
	if (id > 255 || p.sps_id > 31) return false;
	p.entropy = b.u(1);
	p.bottom_field_pic_order = b.u(1);
	//slice groups, FMO, are of the Baseline profile, VA-API has none
	if (b.ue()) return false;
	p.num_ref_idx_default[0] = b.ue() + 1;
	p.num_ref_idx_default[1] = b.ue() + 1;
	p.weighted_pred = b.u(1);
	p.weighted_bipred_idc = b.u(2);
	p.pic_init_qp = b.se() + 26;
	p.pic_init_qs = b.se() + 26;
	p.chroma_qp_index_offset = p.second_chroma_qp_index_offset = b.se();
	p.deblocking = b.u(1);
	p.constrained_intra = b.u(1);
	p.redundant = b.u(1);
	p.transform_8x8 = 0;
	p.scaling = false;
	if (b.more())
	{
		const Va_Sps &s = spss[p.sps_id];
		int chroma = s.valid ? s.chroma_format_idc : 1;
		p.transform_8x8 = b.u(1);
		if ((p.scaling = b.u(1)))
			va_scaling(b, 6 + (p.transform_8x8 ? (chroma == 3 ? 6 : 2) : 0), p.lists4, p.lists8,
				s.valid && s.scaling ? &s : nullptr);
		p.second_chroma_qp_index_offset = b.se();
	}
	if (b.over()) return false;
	p.valid = true;
	all[id] = p;
	return true;
}

//a picture kept to be made from, its surface an index of the decoder's
struct Va_Ref
{
	int surface, frame_num, long_idx, top, bottom;
	bool short_ref, long_ref;
};

//a picture done, held till it is asked for: Y, U and V, U and V smaller
//by the shifts of their sampling
struct Va_Held
{
	int64_t shown;
	uint32_t width, height, sx, sy;
	//where Y U and V start in the data, how far apart their rows are, and
	//of U and V their samples, 2 of NV12, which has them side by side
	size_t y_at, u_at, v_at;
	uint32_t y_pitch, c_pitch, step;
	std::vector<uint8_t> data;
};

//a memory management control operation of a slice header, what it does
//and its numbers
struct Va_Mmco
{
	uint32_t op, a, b;
};

struct Va_Slice
{
	int first_mb, type, pps_id, frame_num, idr_pic_id, poc_lsb, delta_poc_bottom, delta_poc[2];
	int redundant_pic_cnt, direct_spatial, num_ref_idx[2], cabac_init_idc, slice_qp_delta;
	int disable_deblocking, alpha, beta, long_term_reference, adaptive;
	//the modifications of the lists, each what it is and its number
	std::vector<std::pair<uint32_t, uint32_t>> mods[2];
	std::vector<Va_Mmco> mmcos;
	size_t header_bits;
};

struct Va_Decoder
{
	bool jpeg = false;
	bool full = false;
	int nal_length = 4;
	VAProfile profile = VAProfileNone, asked = VAProfileNone, chosen = VAProfileNone;
	VAConfigID config = VA_INVALID_ID;
	VAContextID context = VA_INVALID_ID;
	std::vector<VASurfaceID> surfaces;
	uint32_t format = 0, coded_w = 0, coded_h = 0;
	std::vector<Va_Held> held;
	std::vector<uint8_t> rbsp;
	//of H.264, the parameter sets, the pictures kept, and what the next
	//picture's order and numbers follow on from
	Va_Sps sps[32];
	Va_Pps pps[256];
	std::vector<Va_Ref> dpb;
	int prev_poc_msb = 0, prev_poc_lsb = 0, prev_frame_num_offset = 0, prev_frame_num = 0, max_long_idx = -1;
	//the picture being decoded
	bool started = false;
	Va_Ref cur;
	int cur_ref_idc = 0, cur_poc_msb = 0, cur_frame_num_offset = 0;
	bool cur_idr = false;
	Va_Slice first;
	std::vector<VABufferID> buffers;
	//the slices of the picture, each its parameters and its data, given
	//the driver together at its end: Intel's takes more than one no other way
	std::vector<VABufferID> slices;
};

static void va_release(Va_Decoder *d)
{
	VADisplay dpy = va_display();
	if (d->context != VA_INVALID_ID) va::vaDestroyContext(dpy, d->context);
	if (!d->surfaces.empty()) va::vaDestroySurfaces(dpy, d->surfaces.data(), d->surfaces.size());
	if (d->config != VA_INVALID_ID) va::vaDestroyConfig(dpy, d->config);
	d->context = VA_INVALID_ID;
	d->config = VA_INVALID_ID;
	d->surfaces.clear();
	d->dpb.clear();
}

static bool va_setup(Va_Decoder *d, VAProfile profile, uint32_t format, uint32_t w, uint32_t h, int count)
{
	//a config, the surfaces and a context of this size and format, kept
	//while they do
	if (d->context != VA_INVALID_ID && d->profile == profile && d->format == format
		&& d->coded_w == w && d->coded_h == h && (int)d->surfaces.size() >= count) return true;
	va_release(d);
	VADisplay dpy = va_display();
	VAConfigAttrib attr = {VAConfigAttribRTFormat, format};
	if (va::vaCreateConfig(dpy, profile, VAEntrypointVLD, &attr, 1, &d->config) != VA_STATUS_SUCCESS)
	{
		d->config = VA_INVALID_ID;
		return false;
	}
	d->surfaces.resize(count);
	if (va::vaCreateSurfaces(dpy, format, w, h, d->surfaces.data(), count, nullptr, 0) != VA_STATUS_SUCCESS)
	{
		d->surfaces.clear();
		va_release(d);
		return false;
	}
	if (va::vaCreateContext(dpy, d->config, w, h, VA_PROGRESSIVE, d->surfaces.data(), count, &d->context) != VA_STATUS_SUCCESS)
	{
		d->context = VA_INVALID_ID;
		va_release(d);
		return false;
	}
	d->profile = profile;
	d->format = format;
	d->coded_w = w;
	d->coded_h = h;
	return true;
}

static bool va_buffer(Va_Decoder *d, VABufferType type, const void *data, size_t size, VABufferID &id)
{
	if (va::vaCreateBuffer(va_display(), d->context, type, size, 1, (void*)data, &id) != VA_STATUS_SUCCESS) return false;
	d->buffers.push_back(id);
	return true;
}

static bool va_render(Va_Decoder *d, VABufferType type, const void *data, size_t size)
{
	VABufferID id;
	return va_buffer(d, type, data, size, id) && va::vaRenderPicture(va_display(), d->context, &id, 1) == VA_STATUS_SUCCESS;
}

static void va_buffers_free(Va_Decoder *d)
{
	for (auto id : d->buffers) va::vaDestroyBuffer(va_display(), id);
	d->buffers.clear();
}

static bool va_planes(Va_Held &pic, const VAImage &image, const uint8_t *at, uint32_t x, uint32_t y)
{
	//of an image of the driver's, the planes as they are, each a row at a
	//time, its memory read as it must be, uncached, once
	int u_plane = 1, v_plane = 2, u_at = 0, v_at = 0;
	pic.step = 1;
	switch (image.format.fourcc)
	{
	case VA_FOURCC_NV12: pic.sx = 1; pic.sy = 1; v_plane = 1; pic.step = 2; v_at = 1; break;
	case VA_FOURCC_I420: case VA_FOURCC_IMC3: pic.sx = 1; pic.sy = 1; break;
	case VA_FOURCC_YV12: pic.sx = 1; pic.sy = 1; u_plane = 2; v_plane = 1; break;
	case VA_FOURCC_422H: pic.sx = 1; pic.sy = 0; break;
	case VA_FOURCC_422V: pic.sx = 0; pic.sy = 1; break;
	case VA_FOURCC_444P: pic.sx = 0; pic.sy = 0; break;
	case VA_FOURCC_411P: pic.sx = 2; pic.sy = 0; break;
	case VA_FOURCC_Y800: case VA_FOURCC('4', '0', '0', 'P'): pic.sx = 0; pic.sy = 0; u_plane = v_plane = -1; break;
	case VA_FOURCC_YUY2: case VA_FOURCC_UYVY: pic.sx = 1; pic.sy = 0; break;
	default: return false;
	}
	uint32_t cw = (pic.width + (1 << pic.sx) - 1) >> pic.sx, ch = (pic.height + (1 << pic.sy) - 1) >> pic.sy;
	uint32_t cx = x >> pic.sx, cy = y >> pic.sy;
	pic.y_pitch = pic.width;
	pic.c_pitch = cw * pic.step;
	pic.u_at = (size_t)pic.width * pic.height;
	pic.v_at = pic.u_at + (pic.step == 2 ? 1 : (size_t)cw * ch);
	pic.data.resize(pic.u_at + (size_t)cw * ch * 2);
	if (image.format.fourcc == VA_FOURCC_YUY2 || image.format.fourcc == VA_FOURCC_UYVY)
	{
		//two pixels in four bytes, Y U Y V or U Y V Y, a row copied first
		int yo = image.format.fourcc == VA_FOURCC_YUY2 ? 0 : 1, uo = 1 - yo;
		std::vector<uint8_t> row((size_t)cw * 4);
		for (uint32_t r = 0; r < pic.height; r++)
		{
			memcpy(row.data(), at + image.offsets[0] + (size_t)(y + r) * image.pitches[0] + (size_t)cx * 4, row.size());
			for (uint32_t c = 0; c < pic.width; c++) pic.data[(size_t)r * pic.width + c] = row[c * 2 + yo];
			for (uint32_t c = 0; c < cw; c++)
			{
				pic.data[pic.u_at + (size_t)r * cw + c] = row[c * 4 + uo];
				pic.data[pic.v_at + (size_t)r * cw + c] = row[c * 4 + uo + 2];
			}
		}
		return true;
	}
	for (uint32_t r = 0; r < pic.height; r++)
		memcpy(&pic.data[(size_t)r * pic.width], at + image.offsets[0] + (size_t)(y + r) * image.pitches[0] + x, pic.width);
	for (uint32_t r = 0; r < ch; r++)
	{
		uint8_t *u = &pic.data[pic.u_at + (size_t)r * pic.c_pitch];
		if (u_plane < 0) { memset(u, 128, cw); memset(&pic.data[pic.v_at + (size_t)r * cw], 128, cw); continue; }
		memcpy(u, at + image.offsets[u_plane] + (size_t)(cy + r) * image.pitches[u_plane] + (size_t)cx * pic.step + u_at, cw * pic.step);
		if (pic.step == 1)
			memcpy(&pic.data[pic.v_at + (size_t)r * cw], at + image.offsets[v_plane] + (size_t)(cy + r) * image.pitches[v_plane] + cx + v_at, cw);
	}
	return true;
}

#if defined(__x86_64__)
#include <immintrin.h>
//rows of Intel's Y tiles, each 4KB, 8 columns of 16 bytes by 32 rows, read
//in the order they are in memory with streaming loads, the fast way to
//read memory written combined, into rows of the same pitch
__attribute__((target("sse4.1"))) static void va_untile(const uint8_t *from, uint8_t *to, uint32_t pitch, uint32_t rows)
{
	uint32_t across = pitch / 128;
	for (uint32_t ty = 0; ty < (rows + 31) / 32; ty++)
	{
		uint32_t last = std::min(32u, rows - ty * 32);
		for (uint32_t tx = 0; tx < across; tx++)
		{
			const uint8_t *tile = from + ((size_t)ty * across + tx) * 4096;
			for (uint32_t col = 0; col < 8; col++)
			{
				uint8_t *out = to + (size_t)ty * 32 * pitch + tx * 128 + col * 16;
				for (uint32_t r = 0; r < last; r++)
					_mm_storeu_si128((__m128i*)(out + (size_t)r * pitch),
						_mm_stream_load_si128((__m128i*)(tile + col * 512 + r * 16)));
			}
		}
	}
}
#endif

static bool va_export(Va_Decoder *, VASurfaceID id, Va_Held &pic, uint32_t x, uint32_t y)
{
	//an NV12 surface as the driver gave it to the GPU, its memory mapped,
	//linear or of Y tiles: Intel's, faster than an image of it, which is
	//read uncached. Anything else, compressed, AMD's tiles, is not
	static auto exporter = (decltype(&vaExportSurfaceHandle))dlsym(dlopen("libva.so.2", RTLD_NOW | RTLD_NOLOAD), "vaExportSurfaceHandle");
	if (!exporter) return false;
	VADRMPRIMESurfaceDescriptor desc = {};
	if (exporter(va_display(), id, VA_SURFACE_ATTRIB_MEM_TYPE_DRM_PRIME_2,
		VA_EXPORT_SURFACE_READ_ONLY | VA_EXPORT_SURFACE_COMPOSED_LAYERS, &desc) != VA_STATUS_SUCCESS) return false;
	const uint64_t linear = 0;
	uint64_t mod = desc.objects[0].drm_format_modifier;
	uint32_t pitch = desc.layers[0].pitch[0], rows = pitch ? desc.layers[0].offset[1] / pitch : 0;
	bool ok = desc.fourcc == VA_FOURCC_NV12 && desc.num_objects == 1 && desc.num_layers == 1
		&& desc.layers[0].num_planes == 2 && desc.layers[0].pitch[1] == pitch && rows >= y + pic.height
		&& desc.layers[0].offset[0] == 0 && (size_t)pitch * rows * 3 / 2 <= desc.objects[0].size;
#if defined(__x86_64__)
	//Intel's I915_FORMAT_MOD_Y_TILED
	ok = ok && (mod == linear || (mod == ((1ull << 56) | 2) && pitch % 128 == 0 && rows % 32 == 0 && __builtin_cpu_supports("sse4.1")));
#else
	ok = ok && mod == linear;
#endif
	void *map = ok ? mmap(nullptr, desc.objects[0].size, PROT_READ, MAP_SHARED, desc.objects[0].fd, 0) : MAP_FAILED;
	if (map != MAP_FAILED)
	{
		dma_buf_sync sync = {DMA_BUF_SYNC_START | DMA_BUF_SYNC_READ};
		ioctl(desc.objects[0].fd, DMA_BUF_IOCTL_SYNC, &sync);
		pic.data.resize((size_t)pitch * rows * 3 / 2);
		if (mod == linear) memcpy(pic.data.data(), map, pic.data.size());
#if defined(__x86_64__)
		else
		{
			va_untile((const uint8_t*)map, pic.data.data(), pitch, rows);
			va_untile((const uint8_t*)map + desc.layers[0].offset[1], pic.data.data() + (size_t)pitch * rows, pitch, rows / 2);
		}
#endif
		sync.flags = DMA_BUF_SYNC_END | DMA_BUF_SYNC_READ;
		ioctl(desc.objects[0].fd, DMA_BUF_IOCTL_SYNC, &sync);
		munmap(map, desc.objects[0].size);
		//the picture where it is in the surface, a frame cropped
		pic.sx = pic.sy = 1;
		pic.step = 2;
		pic.y_pitch = pic.c_pitch = pitch;
		pic.y_at = (size_t)y * pitch + x;
		pic.u_at = (size_t)pitch * rows + (size_t)(y / 2) * pitch + (x & ~1u);
		pic.v_at = pic.u_at + 1;
	}
	else ok = false;
	for (uint32_t i = 0; i < desc.num_objects; i++) close(desc.objects[i].fd);
	return ok;
}

static bool va_read(Va_Decoder *d, int surface, uint32_t x, uint32_t y, uint32_t w, uint32_t h, int64_t shown)
{
	//the picture of a surface, from x y and w by h of it, held: by its
	//memory, or the image the driver has of it, or one of NV12 it is
	//copied to
	VADisplay dpy = va_display();
	VASurfaceID id = d->surfaces[surface];
	if (va::vaSyncSurface(dpy, id) != VA_STATUS_SUCCESS) return false;
	Va_Held pic;
	pic.shown = shown;
	pic.width = w;
	pic.height = h;
	pic.y_at = 0;
	if (!d->jpeg && va_export(d, id, pic, x, y))
	{
		d->held.push_back(std::move(pic));
		return true;
	}
	VAImage image;
	if (va::vaDeriveImage(dpy, id, &image) != VA_STATUS_SUCCESS)
	{
		VAImageFormat nv12 = {};
		nv12.fourcc = VA_FOURCC_NV12;
		nv12.byte_order = VA_LSB_FIRST;
		nv12.bits_per_pixel = 12;
		if (va::vaCreateImage(dpy, &nv12, d->coded_w, d->coded_h, &image) != VA_STATUS_SUCCESS) return false;
		if (va::vaGetImage(dpy, id, 0, 0, d->coded_w, d->coded_h, image.image_id) != VA_STATUS_SUCCESS)
		{
			va::vaDestroyImage(dpy, image.image_id);
			return false;
		}
	}
	uint8_t *at = nullptr;
	bool ok = va::vaMapBuffer(dpy, image.buf, (void**)&at) == VA_STATUS_SUCCESS && va_planes(pic, image, at, x, y);
	if (at) va::vaUnmapBuffer(dpy, image.buf);
	va::vaDestroyImage(dpy, image.image_id);
	if (ok) d->held.push_back(std::move(pic));
	return ok;
}

//BT.601, limited, or of JPEG and of a stream that says so full, as the
//V4L2 decoder has it: the numbers of Y, of red of V, green of U and V, and
//blue of U, and what is taken from Y
struct Va_Matrix
{
	int ky, oy, kr, kgu, kgv, kb;
};

static inline uint32_t va_pixel(const Va_Matrix &m, int yy, int u, int v)
{
	int c = m.ky * (yy - m.oy);
	int r = (c + m.kr * v + 128) >> 8;
	int g = (c - m.kgu * u - m.kgv * v + 128) >> 8;
	int b = (c + m.kb * u + 128) >> 8;
	r = r < 0 ? 0 : r > 255 ? 255 : r;
	g = g < 0 ? 0 : g > 255 ? 255 : g;
	b = b < 0 ? 0 : b > 255 ? 255 : b;
	return 0xff000000 | (r << 16) | (g << 8) | b;
}

#if defined(__x86_64__)
#include <emmintrin.h>
static uint32_t va_row(const Va_Matrix &m, const uint8_t *py, const uint8_t *pu, const uint8_t *pv, uint32_t step, uint32_t *out, uint32_t n)
{
	//8 pixels at a time, two to a U and V, by SSE2, which every x86_64 has:
	//each sum of two products at 32 bits, as the scalar does, and made 8
	//bits with saturation, its clamp. The pixels done
	const __m128i zero = _mm_setzero_si128(), c128 = _mm_set1_epi16(128), oy = _mm_set1_epi16(m.oy);
	const __m128i kr = _mm_set_epi16(m.kr, m.ky, m.kr, m.ky, m.kr, m.ky, m.kr, m.ky);
	const __m128i kgu = _mm_set_epi16(-m.kgu, m.ky, -m.kgu, m.ky, -m.kgu, m.ky, -m.kgu, m.ky);
	const __m128i kgv = _mm_set_epi16(128, -m.kgv, 128, -m.kgv, 128, -m.kgv, 128, -m.kgv);
	const __m128i kb = _mm_set_epi16(m.kb, m.ky, m.kb, m.ky, m.kb, m.ky, m.kb, m.ky);
	const __m128i round = _mm_set1_epi32(128), one = _mm_set1_epi16(1), alpha = _mm_set1_epi8((char)0xff);
	uint32_t x = 0;
	for (; x + 8 <= n; x += 8)
	{
		__m128i y16 = _mm_sub_epi16(_mm_unpacklo_epi8(_mm_loadl_epi64((const __m128i*)(py + x)), zero), oy);
		//U and V of 4 pairs, as U V U V
		__m128i uv8;
		if (step == 2) uv8 = _mm_loadl_epi64((const __m128i*)(pu + x));
		else
		{
			int32_t u4, v4;
			memcpy(&u4, pu + x / 2, 4);
			memcpy(&v4, pv + x / 2, 4);
			uv8 = _mm_unpacklo_epi8(_mm_cvtsi32_si128(u4), _mm_cvtsi32_si128(v4));
		}
		__m128i uv = _mm_sub_epi16(_mm_unpacklo_epi8(uv8, zero), c128);
		__m128i u = _mm_shufflehi_epi16(_mm_shufflelo_epi16(uv, 0xa0), 0xa0);
		__m128i v = _mm_shufflehi_epi16(_mm_shufflelo_epi16(uv, 0xf5), 0xf5);
		__m128i yv_lo = _mm_unpacklo_epi16(y16, v), yv_hi = _mm_unpackhi_epi16(y16, v);
		__m128i yu_lo = _mm_unpacklo_epi16(y16, u), yu_hi = _mm_unpackhi_epi16(y16, u);
		__m128i v1_lo = _mm_unpacklo_epi16(v, one), v1_hi = _mm_unpackhi_epi16(v, one);
		__m128i r = _mm_packs_epi32(
			_mm_srai_epi32(_mm_add_epi32(_mm_madd_epi16(yv_lo, kr), round), 8),
			_mm_srai_epi32(_mm_add_epi32(_mm_madd_epi16(yv_hi, kr), round), 8));
		__m128i g = _mm_packs_epi32(
			_mm_srai_epi32(_mm_add_epi32(_mm_madd_epi16(yu_lo, kgu), _mm_madd_epi16(v1_lo, kgv)), 8),
			_mm_srai_epi32(_mm_add_epi32(_mm_madd_epi16(yu_hi, kgu), _mm_madd_epi16(v1_hi, kgv)), 8));
		__m128i b = _mm_packs_epi32(
			_mm_srai_epi32(_mm_add_epi32(_mm_madd_epi16(yu_lo, kb), round), 8),
			_mm_srai_epi32(_mm_add_epi32(_mm_madd_epi16(yu_hi, kb), round), 8));
		__m128i bg = _mm_unpacklo_epi8(_mm_packus_epi16(b, b), _mm_packus_epi16(g, g));
		__m128i ra = _mm_unpacklo_epi8(_mm_packus_epi16(r, r), alpha);
		_mm_storeu_si128((__m128i*)(out + x), _mm_unpacklo_epi16(bg, ra));
		_mm_storeu_si128((__m128i*)(out + x + 4), _mm_unpackhi_epi16(bg, ra));
	}
	return x;
}
#endif

static void va_convert(const Va_Held &pic, bool full, uint32_t *pixels, uint64_t w, uint64_t h, uint64_t stride)
{
	//the picture into the pixels given, as much as there is room for
	Va_Matrix m = full ? Va_Matrix{256, 0, 359, 88, 183, 454} : Va_Matrix{298, 16, 409, 100, 208, 516};
	uint32_t cols = std::min((uint64_t)pic.width, w), rows = std::min((uint64_t)pic.height, h);
	for (uint32_t y = 0; y < rows; y++)
	{
		const uint8_t *py = &pic.data[pic.y_at + (size_t)y * pic.y_pitch];
		const uint8_t *pu = &pic.data[pic.u_at + (size_t)(y >> pic.sy) * pic.c_pitch];
		const uint8_t *pv = &pic.data[pic.v_at + (size_t)(y >> pic.sy) * pic.c_pitch];
		auto out = (uint32_t*)((uint8_t*)pixels + y * stride);
		uint32_t x = 0;
#if defined(__x86_64__)
		if (pic.sx == 1) x = va_row(m, py, pu, pv, pic.step, out, cols);
#endif
		for (; x < cols; x++)
			out[x] = va_pixel(m, py[x], (int)pu[(x >> pic.sx) * pic.step] - 128, (int)pv[(x >> pic.sx) * pic.step] - 128);
	}
}

/////////
// H.264
/////////

static bool va_slice(Va_Decoder *d, const std::vector<uint8_t> &rbsp, int type, int ref_idc,
	Va_Slice &s, VASliceParameterBufferH264 &param)
{
	//a slice's header, what VA-API is told of it into its parameters, and
	//its bits counted from the start of the NAL, its header byte
	Va_Bits b = {rbsp.data(), rbsp.size(), 8};
	s.first_mb = b.ue();
	s.type = b.ue() % 5;
	s.pps_id = b.ue();
	if (s.pps_id > 255 || !d->pps[s.pps_id].valid) return false;
	const Va_Pps &p = d->pps[s.pps_id];
	const Va_Sps &sp = d->sps[p.sps_id];
	//of SP and SI, the Extended profile's, VA-API has none
	if (!sp.valid || s.type > 2) return false;
	if (sp.separate_colour_plane) b.u(2);
	s.frame_num = b.u(sp.log2_max_frame_num);
	//fields, each a picture, are not decoded here, a frame of MBAFF is
	if (!sp.frame_mbs_only && b.u(1)) return false;
	s.idr_pic_id = type == 5 ? b.ue() : 0;
	s.poc_lsb = s.delta_poc_bottom = s.delta_poc[0] = s.delta_poc[1] = 0;
	if (sp.poc_type == 0)
	{
		s.poc_lsb = b.u(sp.log2_max_poc_lsb);
		if (p.bottom_field_pic_order) s.delta_poc_bottom = b.se();
	}
	if (sp.poc_type == 1 && !sp.delta_pic_order_always_zero)
	{
		s.delta_poc[0] = b.se();
		if (p.bottom_field_pic_order) s.delta_poc[1] = b.se();
	}
	s.redundant_pic_cnt = p.redundant ? b.ue() : 0;
	bool bs = s.type == 1, ps = s.type == 0;
	s.direct_spatial = bs ? b.u(1) : 0;
	s.num_ref_idx[0] = p.num_ref_idx_default[0];
	s.num_ref_idx[1] = p.num_ref_idx_default[1];
	if ((ps || bs) && b.u(1))
	{
		s.num_ref_idx[0] = b.ue() + 1;
		if (bs) s.num_ref_idx[1] = b.ue() + 1;
	}
	if (!bs) s.num_ref_idx[1] = 0;
	if (!ps && !bs) s.num_ref_idx[0] = 0;
	if (s.num_ref_idx[0] > 32 || s.num_ref_idx[1] > 32) return false;
	for (int list = 0; list < 2; list++)
	{
		s.mods[list].clear();
		if (list < (bs ? 2 : ps ? 1 : 0) && b.u(1))
		{
			for (uint32_t idc; (idc = b.ue()) != 3 && !b.over() && s.mods[list].size() < 33;)
				s.mods[list].push_back({idc, b.ue()});
		}
	}
	//the weights, those not said as if none
	memset(&param, 0, sizeof(param));
	int chroma = sp.separate_colour_plane ? 0 : sp.chroma_format_idc;
	if ((p.weighted_pred && ps) || (p.weighted_bipred_idc == 1 && bs))
	{
		param.luma_log2_weight_denom = b.ue();
		if (chroma) param.chroma_log2_weight_denom = b.ue();
		for (int list = 0; list < (bs ? 2 : 1); list++)
		{
			auto lw = list ? param.luma_weight_l1 : param.luma_weight_l0;
			auto lo = list ? param.luma_offset_l1 : param.luma_offset_l0;
			auto cw = list ? param.chroma_weight_l1 : param.chroma_weight_l0;
			auto co = list ? param.chroma_offset_l1 : param.chroma_offset_l0;
			auto &lf = list ? param.luma_weight_l1_flag : param.luma_weight_l0_flag;
			auto &cf = list ? param.chroma_weight_l1_flag : param.chroma_weight_l0_flag;
			for (int i = 0; i < s.num_ref_idx[list]; i++)
			{
				lw[i] = 1 << param.luma_log2_weight_denom;
				cw[i][0] = cw[i][1] = 1 << param.chroma_log2_weight_denom;
				if (b.u(1))
				{
					lf = 1;
					lw[i] = b.se();
					lo[i] = b.se();
				}
				if (chroma && b.u(1))
				{
					cf = 1;
					for (int j = 0; j < 2; j++)
					{
						cw[i][j] = b.se();
						co[i][j] = b.se();
					}
				}
			}
		}
	}
	s.long_term_reference = s.adaptive = 0;
	s.mmcos.clear();
	if (ref_idc)
	{
		if (type == 5)
		{
			b.u(1);
			s.long_term_reference = b.u(1);
		}
		else if ((s.adaptive = b.u(1)))
		{
			for (uint32_t op; (op = b.ue()) != 0 && !b.over() && s.mmcos.size() < 66;)
			{
				Va_Mmco m = {op, 0, 0};
				if (op == 1 || op == 3) m.a = b.ue();
				if (op == 2) m.a = b.ue();
				if (op == 3 || op == 6) m.b = b.ue();
				if (op == 4) m.a = b.ue();
				if (op == 6) m.a = m.b;
				s.mmcos.push_back(m);
			}
		}
	}
	s.cabac_init_idc = p.entropy && s.type != 2 ? b.ue() : 0;
	s.slice_qp_delta = b.se();
	s.disable_deblocking = s.alpha = s.beta = 0;
	if (p.deblocking)
	{
		s.disable_deblocking = b.ue();
		if (s.disable_deblocking != 1)
		{
			s.alpha = b.se();
			s.beta = b.se();
		}
	}
	s.header_bits = b.at;
	return !b.over();
}

static void va_poc(Va_Decoder *d, const Va_Sps &sp, const Va_Slice &s, bool idr, int ref_idc)
{
	//the order the picture is shown in, of its top field and bottom, by
	//the SPS's kind of counting
	int max_frame_num = 1 << sp.log2_max_frame_num;
	int offset = 0;
	if (sp.poc_type == 0)
	{
		if (idr) d->prev_poc_msb = d->prev_poc_lsb = 0;
		int max = 1 << sp.log2_max_poc_lsb, lsb = s.poc_lsb, msb = d->prev_poc_msb;
		if (lsb < d->prev_poc_lsb && d->prev_poc_lsb - lsb >= max / 2) msb += max;
		else if (lsb > d->prev_poc_lsb && lsb - d->prev_poc_lsb > max / 2) msb -= max;
		d->cur_poc_msb = msb;
		d->cur.top = msb + lsb;
		d->cur.bottom = d->cur.top + s.delta_poc_bottom;
		return;
	}
	if (!idr) offset = d->prev_frame_num_offset + (d->prev_frame_num > s.frame_num ? max_frame_num : 0);
	d->cur_frame_num_offset = offset;
	if (sp.poc_type == 1)
	{
		int abs = sp.cycle ? offset + s.frame_num : 0;
		if (!ref_idc && abs > 0) abs--;
		int expected = 0;
		if (abs > 0)
		{
			int delta = 0;
			for (int i = 0; i < sp.cycle; i++) delta += sp.offset_for_ref_frame[i];
			expected = ((abs - 1) / sp.cycle) * delta;
			for (int i = 0; i <= (abs - 1) % sp.cycle; i++) expected += sp.offset_for_ref_frame[i];
		}
		if (!ref_idc) expected += sp.offset_for_non_ref_pic;
		d->cur.top = expected + s.delta_poc[0];
		d->cur.bottom = d->cur.top + sp.offset_for_top_to_bottom_field + s.delta_poc[1];
		return;
	}
	int poc = idr ? 0 : ref_idc ? 2 * (offset + s.frame_num) : 2 * (offset + s.frame_num) - 1;
	d->cur.top = d->cur.bottom = poc;
}

static void va_pic(const Va_Decoder *d, const Va_Ref &r, VAPictureH264 &pic)
{
	pic.picture_id = d->surfaces[r.surface];
	pic.frame_idx = r.long_ref ? r.long_idx : r.frame_num;
	pic.flags = r.long_ref ? VA_PICTURE_H264_LONG_TERM_REFERENCE : VA_PICTURE_H264_SHORT_TERM_REFERENCE;
	pic.TopFieldOrderCnt = r.top;
	pic.BottomFieldOrderCnt = r.bottom;
}

static void va_none(VAPictureH264 &pic)
{
	pic.picture_id = VA_INVALID_SURFACE;
	pic.frame_idx = 0;
	pic.flags = VA_PICTURE_H264_INVALID;
	pic.TopFieldOrderCnt = pic.BottomFieldOrderCnt = 0;
}

static int va_pic_num(const Va_Decoder *d, const Va_Ref &r, int max_frame_num)
{
	//of a short term reference, its frame_num as it wraps before the current's
	return r.frame_num > d->cur.frame_num ? r.frame_num - max_frame_num : r.frame_num;
}

static void va_lists(Va_Decoder *d, const Va_Sps &sp, const Va_Slice &s, VASliceParameterBufferH264 &param)
{
	//the lists of what the slice is made from: the standard's first order of
	//them, a P slice's by frame_num down, a B slice's by the order shown
	//either side of this one, then those of long term, then what the
	//slice's header changes of them
	int max_frame_num = 1 << sp.log2_max_frame_num;
	int poc = std::min(d->cur.top, d->cur.bottom);
	std::vector<int> shorts, longs;
	for (int i = 0; i < (int)d->dpb.size(); i++)
	{
		if (d->dpb[i].short_ref) shorts.push_back(i);
		else if (d->dpb[i].long_ref) longs.push_back(i);
	}
	std::sort(longs.begin(), longs.end(), [&](int a, int b) { return d->dpb[a].long_idx < d->dpb[b].long_idx; });
	auto ref_poc = [&](int i) { return std::min(d->dpb[i].top, d->dpb[i].bottom); };
	for (int list = 0; list < 2; list++)
	{
		auto out = list ? param.RefPicList1 : param.RefPicList0;
		for (int i = 0; i < 32; i++) va_none(out[i]);
		int active = s.num_ref_idx[list];
		if (!active) continue;
		std::vector<int> order;
		if (s.type == 0)
		{
			order = shorts;
			std::sort(order.begin(), order.end(), [&](int a, int b)
				{ return va_pic_num(d, d->dpb[a], max_frame_num) > va_pic_num(d, d->dpb[b], max_frame_num); });
		}
		else
		{
			std::vector<int> before, after;
			for (int i : shorts) (ref_poc(i) < poc ? before : after).push_back(i);
			std::sort(before.begin(), before.end(), [&](int a, int b) { return ref_poc(a) > ref_poc(b); });
			std::sort(after.begin(), after.end(), [&](int a, int b) { return ref_poc(a) < ref_poc(b); });
			if (list == 0) { order = before; order.insert(order.end(), after.begin(), after.end()); }
			else { order = after; order.insert(order.end(), before.begin(), before.end()); }
		}
		order.insert(order.end(), longs.begin(), longs.end());
		//of B, list 1 the same as list 0 of more than one, its first two swapped
		if (list == 1 && order.size() > 1)
		{
			std::vector<int> zero;
			std::vector<int> b4, af;
			for (int i : shorts) (ref_poc(i) < poc ? b4 : af).push_back(i);
			std::sort(b4.begin(), b4.end(), [&](int a, int b) { return ref_poc(a) > ref_poc(b); });
			std::sort(af.begin(), af.end(), [&](int a, int b) { return ref_poc(a) < ref_poc(b); });
			zero = b4;
			zero.insert(zero.end(), af.begin(), af.end());
			zero.insert(zero.end(), longs.begin(), longs.end());
			if (zero == order) std::swap(order[0], order[1]);
		}
		order.resize(active, -1);
		//the changes, each puts a picture at the next place and takes it from
		//further on
		int pred = d->cur.frame_num, at = 0;
		for (auto &mod : s.mods[list])
		{
			int pick = -1;
			if (mod.first < 2)
			{
				int abs = mod.second + 1;
				int nowrap = mod.first == 0 ? pred - abs : pred + abs;
				if (nowrap < 0) nowrap += max_frame_num;
				if (nowrap >= max_frame_num) nowrap -= max_frame_num;
				pred = nowrap;
				int num = nowrap > d->cur.frame_num ? nowrap - max_frame_num : nowrap;
				for (int i : shorts) if (va_pic_num(d, d->dpb[i], max_frame_num) == num) pick = i;
			}
			else if (mod.first == 2)
			{
				for (int i : longs) if (d->dpb[i].long_idx == (int)mod.second) pick = i;
			}
			else continue;
			if (at >= active) break;
			order.insert(order.begin() + at, pick);
			int keep = ++at;
			for (int i = at; i < (int)order.size(); i++)
				if (order[i] != pick || pick < 0) order[keep++] = order[i];
			order.resize(active, -1);
		}
		//a place with nothing, a picture lost to a seek, given one there is,
		//so the hardware has something to read
		int any = -1;
		for (int i : order) if (i >= 0) { any = i; break; }
		for (int i = 0; i < active; i++)
		{
			if (order[i] >= 0) va_pic(d, d->dpb[order[i]], out[i]);
			else if (any >= 0) va_pic(d, d->dpb[any], out[i]);
			else
			{
				out[i].picture_id = d->surfaces[d->cur.surface];
				out[i].frame_idx = d->cur.frame_num;
				out[i].flags = VA_PICTURE_H264_SHORT_TERM_REFERENCE;
				out[i].TopFieldOrderCnt = d->cur.top;
				out[i].BottomFieldOrderCnt = d->cur.bottom;
			}
		}
	}
}

static bool va_start(Va_Decoder *d, const Va_Slice &s, bool idr, int ref_idc)
{
	//a new picture: the surfaces of its size, its order, a surface not
	//kept, and what VA-API is told of the picture and its scaling
	const Va_Pps &p = d->pps[s.pps_id];
	const Va_Sps &sp = d->sps[p.sps_id];
	if (sp.chroma_format_idc != 1 || sp.bit_depth_luma != 8 || sp.bit_depth_chroma != 8) return false;
	VADisplay dpy = va_display();
	VAProfile profile = sp.profile_idc == 66 ? VAProfileH264ConstrainedBaseline
		: sp.profile_idc == 77 || sp.profile_idc == 88 ? VAProfileH264Main : VAProfileH264High;
	//High, which has all of the others, if the driver has not that one, as
	//it said the first time it was asked
	if (profile != d->asked) d->chosen = va_has(dpy, d->asked = profile) ? profile : VAProfileH264High;
	profile = d->chosen;
	int count = std::max(sp.max_num_ref_frames, 1) + 2;
	if (!va_setup(d, profile, VA_RT_FORMAT_YUV420, sp.width_mbs * 16, sp.height_mbs * 16, count)) return false;
	d->full = sp.full;
	d->cur_idr = idr;
	d->cur_ref_idc = ref_idc;
	d->cur.frame_num = s.frame_num;
	d->cur.short_ref = d->cur.long_ref = false;
	d->cur.long_idx = 0;
	if (idr) d->dpb.clear();
	va_poc(d, sp, s, idr, ref_idc);
	//a surface no kept picture is on, of so many there is one
	d->cur.surface = -1;
	for (int i = 0; i < (int)d->surfaces.size() && d->cur.surface < 0; i++)
	{
		bool used = false;
		for (auto &r : d->dpb) used |= r.surface == i;
		if (!used) d->cur.surface = i;
	}
	if (d->cur.surface < 0) return false;
	VAPictureParameterBufferH264 pic = {};
	pic.CurrPic.picture_id = d->surfaces[d->cur.surface];
	pic.CurrPic.frame_idx = s.frame_num;
	pic.CurrPic.flags = 0;
	pic.CurrPic.TopFieldOrderCnt = d->cur.top;
	pic.CurrPic.BottomFieldOrderCnt = d->cur.bottom;
	for (int i = 0; i < 16; i++)
	{
		if (i < (int)d->dpb.size()) va_pic(d, d->dpb[i], pic.ReferenceFrames[i]);
		else va_none(pic.ReferenceFrames[i]);
	}
	pic.picture_width_in_mbs_minus1 = sp.width_mbs - 1;
	pic.picture_height_in_mbs_minus1 = sp.height_mbs - 1;
	pic.bit_depth_luma_minus8 = sp.bit_depth_luma - 8;
	pic.bit_depth_chroma_minus8 = sp.bit_depth_chroma - 8;
	pic.num_ref_frames = sp.max_num_ref_frames;
	pic.seq_fields.bits.chroma_format_idc = sp.chroma_format_idc;
	pic.seq_fields.bits.residual_colour_transform_flag = sp.separate_colour_plane;
	pic.seq_fields.bits.gaps_in_frame_num_value_allowed_flag = sp.gaps;
	pic.seq_fields.bits.frame_mbs_only_flag = sp.frame_mbs_only;
	pic.seq_fields.bits.mb_adaptive_frame_field_flag = sp.mb_adaptive;
	pic.seq_fields.bits.direct_8x8_inference_flag = sp.direct_8x8;
	pic.seq_fields.bits.MinLumaBiPredSize8x8 = sp.level_idc >= 31;
	pic.seq_fields.bits.log2_max_frame_num_minus4 = sp.log2_max_frame_num - 4;
	pic.seq_fields.bits.pic_order_cnt_type = sp.poc_type;
	pic.seq_fields.bits.log2_max_pic_order_cnt_lsb_minus4 = sp.poc_type == 0 ? sp.log2_max_poc_lsb - 4 : 0;
	pic.seq_fields.bits.delta_pic_order_always_zero_flag = sp.delta_pic_order_always_zero;
	pic.pic_init_qp_minus26 = p.pic_init_qp - 26;
	pic.pic_init_qs_minus26 = p.pic_init_qs - 26;
	pic.chroma_qp_index_offset = p.chroma_qp_index_offset;
	pic.second_chroma_qp_index_offset = p.second_chroma_qp_index_offset;
	pic.pic_fields.bits.entropy_coding_mode_flag = p.entropy;
	pic.pic_fields.bits.weighted_pred_flag = p.weighted_pred;
	pic.pic_fields.bits.weighted_bipred_idc = p.weighted_bipred_idc;
	pic.pic_fields.bits.transform_8x8_mode_flag = p.transform_8x8;
	pic.pic_fields.bits.field_pic_flag = 0;
	pic.pic_fields.bits.constrained_intra_pred_flag = p.constrained_intra;
	pic.pic_fields.bits.pic_order_present_flag = p.bottom_field_pic_order;
	pic.pic_fields.bits.deblocking_filter_control_present_flag = p.deblocking;
	pic.pic_fields.bits.redundant_pic_cnt_present_flag = p.redundant;
	pic.pic_fields.bits.reference_pic_flag = ref_idc != 0;
	pic.frame_num = s.frame_num;
	VAIQMatrixBufferH264 iq = {};
	memcpy(iq.ScalingList4x4, p.scaling ? p.lists4 : sp.lists4, sizeof(iq.ScalingList4x4));
	//of 8x8 only luma's, intra and inter
	memcpy(iq.ScalingList8x8[0], p.scaling ? p.lists8[0] : sp.lists8[0], 64);
	memcpy(iq.ScalingList8x8[1], p.scaling ? p.lists8[1] : sp.lists8[1], 64);
	if (va::vaBeginPicture(dpy, d->context, d->surfaces[d->cur.surface]) != VA_STATUS_SUCCESS) return false;
	d->started = true;
	d->first = s;
	return va_render(d, VAPictureParameterBufferType, &pic, sizeof(pic))
		&& va_render(d, VAIQMatrixBufferType, &iq, sizeof(iq));
}

static void va_mark(Va_Decoder *d, const Va_Sps &sp)
{
	//the decoded picture kept, if it is to be made from, and those it says
	//are not to be any more let go: by its operations, or the oldest of
	//short term when there are as many as the SPS keeps
	const Va_Slice &s = d->first;
	int max_frame_num = 1 << sp.log2_max_frame_num;
	bool mmco5 = false, cur_long = false;
	if (d->cur_ref_idc)
	{
		if (d->cur_idr)
		{
			d->dpb.clear();
			cur_long = s.long_term_reference;
			d->max_long_idx = cur_long ? 0 : -1;
		}
		else if (s.adaptive)
		{
			for (auto &m : s.mmcos)
			{
				int num = d->cur.frame_num - (int)(m.a + 1);
				switch (m.op)
				{
				case 1:
					for (auto &r : d->dpb) if (r.short_ref && va_pic_num(d, r, max_frame_num) == num) r.short_ref = false;
					break;
				case 2:
					for (auto &r : d->dpb) if (r.long_ref && r.long_idx == (int)m.a) r.long_ref = false;
					break;
				case 3:
					for (auto &r : d->dpb) if (r.long_ref && r.long_idx == (int)m.b) r.long_ref = false;
					for (auto &r : d->dpb)
						if (r.short_ref && va_pic_num(d, r, max_frame_num) == num)
						{
							r.short_ref = false;
							r.long_ref = true;
							r.long_idx = m.b;
						}
					break;
				case 4:
					d->max_long_idx = (int)m.a - 1;
					for (auto &r : d->dpb) if (r.long_ref && r.long_idx > d->max_long_idx) r.long_ref = false;
					break;
				case 5:
					for (auto &r : d->dpb) r.short_ref = r.long_ref = false;
					d->max_long_idx = -1;
					mmco5 = true;
					break;
				case 6:
					for (auto &r : d->dpb) if (r.long_ref && r.long_idx == (int)m.a) r.long_ref = false;
					cur_long = true;
					d->cur.long_idx = m.a;
					break;
				}
			}
		}
		auto drop = [&]() { d->dpb.erase(std::remove_if(d->dpb.begin(), d->dpb.end(),
			[](const Va_Ref &r) { return !r.short_ref && !r.long_ref; }), d->dpb.end()); };
		drop();
		//the sliding window, and so as never to keep more than there are
		//surfaces for
		while ((int)d->dpb.size() >= std::max(sp.max_num_ref_frames, 1))
		{
			int oldest = -1;
			for (int i = 0; i < (int)d->dpb.size(); i++)
				if (d->dpb[i].short_ref && (oldest < 0 || va_pic_num(d, d->dpb[i], max_frame_num)
					< va_pic_num(d, d->dpb[oldest], max_frame_num))) oldest = i;
			if (oldest < 0) oldest = 0;
			d->dpb.erase(d->dpb.begin() + oldest);
		}
		Va_Ref r = d->cur;
		r.long_ref = cur_long;
		r.short_ref = !cur_long;
		if (mmco5)
		{
			int least = std::min(r.top, r.bottom);
			r.top -= least;
			r.bottom -= least;
			r.frame_num = 0;
		}
		d->dpb.push_back(r);
		//what the next picture's order follows on from
		if (sp.poc_type == 0)
		{
			d->prev_poc_msb = mmco5 ? 0 : d->cur_poc_msb;
			d->prev_poc_lsb = mmco5 ? r.top : s.poc_lsb;
		}
	}
	d->prev_frame_num_offset = mmco5 ? 0 : d->cur_frame_num_offset;
	d->prev_frame_num = mmco5 ? 0 : s.frame_num;
}

static bool va_finish(Va_Decoder *d, int64_t shown)
{
	//the picture decoded, held to be shown, and kept if it is to be made from
	if (!d->started) return true;
	d->started = false;
	VADisplay dpy = va_display();
	bool ok = !d->slices.empty() && va::vaRenderPicture(dpy, d->context, d->slices.data(), d->slices.size()) == VA_STATUS_SUCCESS;
	ok = va::vaEndPicture(dpy, d->context) == VA_STATUS_SUCCESS && ok;
	d->slices.clear();
	va_buffers_free(d);
	const Va_Sps &sp = d->sps[d->pps[d->first.pps_id].sps_id];
	if (ok) ok = va_read(d, d->cur.surface, sp.crop_left, sp.crop_top,
		sp.width_mbs * 16 - sp.crop_left - sp.crop_right, sp.height_mbs * 16 - sp.crop_top - sp.crop_bottom, shown);
	va_mark(d, sp);
	return ok;
}

static int64_t va_h264_decode(Va_Decoder *d, const uint8_t *data, uint64_t len, int64_t shown)
{
	//each NAL of the packet: parameter sets kept, slices given the hardware
	bool ok = true;
	for (uint64_t k = 0; k + d->nal_length <= len;)
	{
		size_t n = 0;
		for (int i = 0; i < d->nal_length; i++) n = (n << 8) | data[k + i];
		k += d->nal_length;
		if (n == 0 || k + n > len) break;
		const uint8_t *nal = data + k;
		k += n;
		int type = nal[0] & 31, ref_idc = (nal[0] >> 5) & 3;
		if (type == 7 || type == 8)
		{
			va_rbsp(nal, n, d->rbsp);
			if (type == 7) va_sps(d->rbsp, d->sps);
			else va_pps(d->rbsp, d->pps, d->sps);
			continue;
		}
		if (type != 1 && type != 5) continue;
		//the header, all there is to read of it, in the first bytes
		va_rbsp(nal, std::min(n, (size_t)4096), d->rbsp);
		Va_Slice s;
		VASliceParameterBufferH264 param;
		if (!va_slice(d, d->rbsp, type, ref_idc, s, param)) { ok = false; continue; }
		if (s.redundant_pic_cnt) continue;
		if (d->started && s.first_mb == 0) ok &= va_finish(d, shown);
		if (!d->started && !va_start(d, s, type == 5, ref_idc))
		{
			if (d->started) va_finish(d, shown);
			ok = false;
			continue;
		}
		const Va_Sps &sp = d->sps[d->pps[s.pps_id].sps_id];
		va_lists(d, sp, s, param);
		param.slice_data_size = n;
		param.slice_data_offset = 0;
		param.slice_data_flag = VA_SLICE_DATA_FLAG_ALL;
		param.slice_data_bit_offset = s.header_bits;
		param.first_mb_in_slice = s.first_mb;
		param.slice_type = s.type;
		param.direct_spatial_mv_pred_flag = s.direct_spatial;
		param.num_ref_idx_l0_active_minus1 = s.num_ref_idx[0] ? s.num_ref_idx[0] - 1 : 0;
		param.num_ref_idx_l1_active_minus1 = s.num_ref_idx[1] ? s.num_ref_idx[1] - 1 : 0;
		param.cabac_init_idc = s.cabac_init_idc;
		param.slice_qp_delta = s.slice_qp_delta;
		param.disable_deblocking_filter_idc = s.disable_deblocking;
		param.slice_alpha_c0_offset_div2 = s.alpha;
		param.slice_beta_offset_div2 = s.beta;
		VABufferID ids[2];
		if (va_buffer(d, VASliceParameterBufferType, &param, sizeof(param), ids[0])
			&& va_buffer(d, VASliceDataBufferType, nal, n, ids[1])) d->slices.insert(d->slices.end(), ids, ids + 2);
		else ok = false;
	}
	ok &= va_finish(d, shown);
	return ok ? 0 : -1;
}

////////////////
// Motion JPEG
////////////////

//the tables of the standard's K.3, for the frames that have none, as AVI's
//Motion JPEG leaves them out: DC of luma and of chroma, then AC
static const uint8_t va_dc_counts[2][16] = {
	{0, 1, 5, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0},
	{0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0}};
static const uint8_t va_ac_counts[2][16] = {
	{0, 2, 1, 3, 3, 2, 4, 3, 5, 5, 4, 4, 0, 0, 1, 0x7d},
	{0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, 0x77}};
static const uint8_t va_ac_values[2][162] = {
	{0x01, 0x02, 0x03, 0x00, 0x04, 0x11, 0x05, 0x12, 0x21, 0x31, 0x41, 0x06, 0x13, 0x51, 0x61, 0x07,
	0x22, 0x71, 0x14, 0x32, 0x81, 0x91, 0xa1, 0x08, 0x23, 0x42, 0xb1, 0xc1, 0x15, 0x52, 0xd1, 0xf0,
	0x24, 0x33, 0x62, 0x72, 0x82, 0x09, 0x0a, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x25, 0x26, 0x27, 0x28,
	0x29, 0x2a, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
	0x4a, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69,
	0x6a, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89,
	0x8a, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
	0xa8, 0xa9, 0xaa, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xc2, 0xc3, 0xc4, 0xc5,
	0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xe1, 0xe2,
	0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,
	0xf9, 0xfa},
	{0x00, 0x01, 0x02, 0x03, 0x11, 0x04, 0x05, 0x21, 0x31, 0x06, 0x12, 0x41, 0x51, 0x07, 0x61, 0x71,
	0x13, 0x22, 0x32, 0x81, 0x08, 0x14, 0x42, 0x91, 0xa1, 0xb1, 0xc1, 0x09, 0x23, 0x33, 0x52, 0xf0,
	0x15, 0x62, 0x72, 0xd1, 0x0a, 0x16, 0x24, 0x34, 0xe1, 0x25, 0xf1, 0x17, 0x18, 0x19, 0x1a, 0x26,
	0x27, 0x28, 0x29, 0x2a, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
	0x49, 0x4a, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68,
	0x69, 0x6a, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
	0x88, 0x89, 0x8a, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0xa2, 0xa3, 0xa4, 0xa5,
	0xa6, 0xa7, 0xa8, 0xa9, 0xaa, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xc2, 0xc3,
	0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda,
	0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,
	0xf9, 0xfa}};

static int64_t va_jpeg_decode(Va_Decoder *d, const uint8_t *data, uint64_t len, int64_t shown)
{
	//a frame's markers read for its tables, its size and sampling and its
	//scan, and the scan's coded data given the hardware. Baseline or
	//extended sequential, of 8 bits and one scan, as Motion JPEG is
	if (len < 4 || data[0] != 0xff || data[1] != 0xd8) return -1;
	VAPictureParameterBufferJPEGBaseline pic = {};
	VAIQMatrixBufferJPEGBaseline iq = {};
	VAHuffmanTableBufferJPEGBaseline huff = {};
	VASliceParameterBufferJPEGBaseline slice = {};
	for (int t = 0; t < 2; t++)
	{
		huff.load_huffman_table[t] = 1;
		memcpy(huff.huffman_table[t].num_dc_codes, va_dc_counts[t], 16);
		for (int i = 0; i < 12; i++) huff.huffman_table[t].dc_values[i] = i;
		memcpy(huff.huffman_table[t].num_ac_codes, va_ac_counts[t], 16);
		memcpy(huff.huffman_table[t].ac_values, va_ac_values[t], 162);
	}
	bool sof = false;
	size_t at = 2, scan = 0, end = len;
	while (!scan && at + 4 <= len)
	{
		if (data[at] != 0xff) { at++; continue; }
		uint8_t m = data[at + 1];
		if (m == 0xff) { at++; continue; }
		if (m == 0xd8 || m == 0x01 || (m >= 0xd0 && m <= 0xd7)) { at += 2; continue; }
		if (m == 0xd9) break;
		size_t n = (data[at + 2] << 8) | data[at + 3];
		if (n < 2 || at + 2 + n > len) return -1;
		const uint8_t *s = data + at + 4;
		size_t size = n - 2;
		switch (m)
		{
		case 0xdb:
			for (size_t k = 0; k + 65 <= size; k += 65)
			{
				if (s[k] >> 4) return -1;
				iq.load_quantiser_table[s[k] & 3] = 1;
				memcpy(iq.quantiser_table[s[k] & 3], s + k + 1, 64);
			}
			break;
		case 0xc4:
			for (size_t k = 0; k + 17 <= size;)
			{
				int tc = s[k] >> 4, th = s[k] & 15, total = 0;
				for (int i = 0; i < 16; i++) total += s[k + 1 + i];
				if (th > 1 || tc > 1 || total > (tc ? 162 : 12) || k + 17 + total > size) return -1;
				auto &table = huff.huffman_table[th];
				memcpy(tc ? table.num_ac_codes : table.num_dc_codes, s + k + 1, 16);
				memset(tc ? table.ac_values : table.dc_values, 0, tc ? 162 : 12);
				memcpy(tc ? table.ac_values : table.dc_values, s + k + 17, total);
				k += 17 + total;
			}
			break;
		case 0xc0: case 0xc1:
		{
			if (size < 6 || s[0] != 8) return -1;
			pic.picture_height = (s[1] << 8) | s[2];
			pic.picture_width = (s[3] << 8) | s[4];
			pic.num_components = s[5];
			if (pic.num_components < 1 || pic.num_components > 4 || size < 6 + 3u * pic.num_components) return -1;
			for (int i = 0; i < pic.num_components; i++)
			{
				pic.components[i].component_id = s[6 + i * 3];
				pic.components[i].h_sampling_factor = s[7 + i * 3] >> 4;
				pic.components[i].v_sampling_factor = s[7 + i * 3] & 15;
				pic.components[i].quantiser_table_selector = s[8 + i * 3] & 3;
			}
			sof = true;
			break;
		}
		case 0xc2: case 0xc3: case 0xc5: case 0xc6: case 0xc7:
		case 0xc9: case 0xca: case 0xcb: case 0xcd: case 0xce: case 0xcf:
			return -1;
		case 0xdd:
			if (size >= 2) slice.restart_interval = (s[0] << 8) | s[1];
			break;
		case 0xda:
			if (!sof || size < 1 || s[0] < 1 || s[0] > 4 || size < 1 + 2u * s[0] + 3) return -1;
			slice.num_components = s[0];
			for (int i = 0; i < s[0]; i++)
			{
				slice.components[i].component_selector = s[1 + i * 2];
				slice.components[i].dc_table_selector = s[2 + i * 2] >> 4;
				slice.components[i].ac_table_selector = s[2 + i * 2] & 15;
			}
			scan = at + 2 + n;
			break;
		}
		at += 2 + n;
	}
	if (!scan || !pic.picture_width || !pic.picture_height) return -1;
	//to the end of the image, the EOI
	while (end > scan + 2 && !(data[end - 2] == 0xff && data[end - 1] == 0xd9)) end--;
	if (end > scan + 2) end -= 2;
	else end = len;
	//the surface's format by the sampling, luma's to chroma's
	int hmax = 1, vmax = 1;
	for (int i = 0; i < pic.num_components; i++)
	{
		hmax = std::max(hmax, (int)pic.components[i].h_sampling_factor);
		vmax = std::max(vmax, (int)pic.components[i].v_sampling_factor);
	}
	uint32_t format;
	if (pic.num_components == 1) format = VA_RT_FORMAT_YUV400;
	else if (pic.num_components != 3 || pic.components[1].h_sampling_factor != 1 || pic.components[1].v_sampling_factor != 1
		|| pic.components[2].h_sampling_factor != 1 || pic.components[2].v_sampling_factor != 1) return -1;
	else if (hmax == 2 && vmax == 2) format = VA_RT_FORMAT_YUV420;
	else if (hmax == 2 && vmax == 1) format = VA_RT_FORMAT_YUV422;
	else if (hmax == 1 && vmax == 2) format = VA_RT_FORMAT_YUV422;
	else if (hmax == 1 && vmax == 1) format = VA_RT_FORMAT_YUV444;
	else if (hmax == 4 && vmax == 1) format = VA_RT_FORMAT_YUV411;
	else return -1;
	uint32_t w = pic.picture_width, h = pic.picture_height;
	if (!va_setup(d, VAProfileJPEGBaseline, format, (w + 15) & ~15u, (h + 15) & ~15u, 2)) return -1;
	if (slice.num_components == 1)
	{
		int c = 0;
		for (int i = 0; i < pic.num_components; i++)
			if (pic.components[i].component_id == slice.components[0].component_selector) c = i;
		slice.num_mcus = ((w * pic.components[c].h_sampling_factor / hmax + 7) / 8)
			* ((h * pic.components[c].v_sampling_factor / vmax + 7) / 8);
	}
	else slice.num_mcus = ((w + 8 * hmax - 1) / (8 * hmax)) * ((h + 8 * vmax - 1) / (8 * vmax));
	slice.slice_data_size = end - scan;
	slice.slice_data_offset = 0;
	slice.slice_data_flag = VA_SLICE_DATA_FLAG_ALL;
	VADisplay dpy = va_display();
	if (va::vaBeginPicture(dpy, d->context, d->surfaces[0]) != VA_STATUS_SUCCESS) return -1;
	bool ok = va_render(d, VAPictureParameterBufferType, &pic, sizeof(pic))
		&& va_render(d, VAIQMatrixBufferType, &iq, sizeof(iq))
		&& va_render(d, VAHuffmanTableBufferType, &huff, sizeof(huff))
		&& va_render(d, VASliceParameterBufferType, &slice, sizeof(slice))
		&& va_render(d, VASliceDataBufferType, data + scan, end - scan);
	ok = (va::vaEndPicture(dpy, d->context) == VA_STATUS_SUCCESS) && ok;
	va_buffers_free(d);
	d->full = true;
	if (ok) ok = va_read(d, 0, 0, 0, w, h, shown);
	return ok ? 0 : -1;
}

///////////////////
// the four calls
///////////////////

static Va_Decoder *va_open(uint64_t codec, const uint8_t *setup, uint64_t len)
{
	//of H.264 its parameter sets read now, the surfaces made with its first
	//picture; of Motion JPEG, each frame says what it is
	bool jpeg = codec == 0x6a706567;
	std::vector<const uint8_t*> sets;
	std::vector<size_t> sizes;
	int nal_length = 4;
	if (jpeg) { if (len < 8) return nullptr; }
	else if (codec != 0x61766331 || !video_avcc(setup, len, sets, sizes, nal_length)) return nullptr;
	VADisplay dpy = va_display();
	if (!dpy) return nullptr;
	auto d = new Va_Decoder();
	d->jpeg = jpeg;
	d->nal_length = nal_length;
	bool ok = jpeg ? va_has(dpy, VAProfileJPEGBaseline) : false;
	for (size_t i = 0; i < sets.size(); i++)
	{
		va_rbsp(sets[i], sizes[i], d->rbsp);
		if ((sets[i][0] & 31) == 7) va_sps(d->rbsp, d->sps);
		else va_pps(d->rbsp, d->pps, d->sps);
	}
	if (!jpeg)
	{
		//a profile it can, of the first SPS, 4:2:0 of 8 bits
		for (auto &sp : d->sps)
		{
			if (!sp.valid) continue;
			VAProfile profile = sp.profile_idc == 66 ? VAProfileH264ConstrainedBaseline
				: sp.profile_idc == 77 || sp.profile_idc == 88 ? VAProfileH264Main
				: sp.profile_idc == 100 ? VAProfileH264High : VAProfileNone;
			ok = profile != VAProfileNone && sp.chroma_format_idc == 1 && sp.bit_depth_luma == 8
				&& sp.bit_depth_chroma == 8 && (va_has(dpy, profile) || va_has(dpy, VAProfileH264High));
			break;
		}
	}
	if (!ok)
	{
		delete d;
		return nullptr;
	}
	return d;
}

static int64_t va_decode(Va_Decoder *d, const uint8_t *data, uint64_t len, int64_t shown)
{
	//nothing: the end, and this holds none back
	if (len == 0) return 0;
	return d->jpeg ? va_jpeg_decode(d, data, len, shown) : va_h264_decode(d, data, len, shown);
}

static int64_t va_frame(Va_Decoder *d, uint32_t *pixels, uint64_t w, uint64_t h, uint64_t stride, int64_t shown)
{
	//the picture shown at that time; those to be shown before it are gone
	int64_t got = -1;
	for (auto it = d->held.begin(); it != d->held.end();)
	{
		if (it->shown > shown) { ++it; continue; }
		if (it->shown == shown)
		{
			va_convert(*it, d->full, pixels, w, h, stride);
			got = shown;
		}
		it = d->held.erase(it);
	}
	return got;
}

static void va_close(Va_Decoder *d)
{
	if (d->started)
	{
		va::vaEndPicture(va_display(), d->context);
		d->slices.clear();
		va_buffers_free(d);
	}
	va_release(d);
	delete d;
}

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
	//one of VA-API, which does all of what is asked
	Va_Decoder *va = nullptr;
	int fd = -1;
	int nal_length = 4;
	std::vector<uint8_t> sets;
	//of JPEG, YUV of the full range, 0 to 255, not of video's 16 to 235
	bool full = false;
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
	//YUV 4:2:0, a plane of Y, then U and V each a quarter, BT.601 limited,
	//or of JPEG full
	uint32_t w = d->width, h = d->height, ys = d->stride, cs = d->stride / 2;
	const uint8_t *py = yuv, *pu = yuv + ys * d->plane, *pv = pu + cs * (d->plane / 2);
	int ky = d->full ? 256 : 298, oy = d->full ? 0 : 16, kr = d->full ? 359 : 409,
		kgu = d->full ? 88 : 100, kgv = d->full ? 183 : 208, kb = d->full ? 454 : 516;
	out.resize((size_t)w * h);
	for (uint32_t y = 0; y < h; y++)
	{
		for (uint32_t x = 0; x < w; x++)
		{
			int c = ky * ((int)py[y * ys + x] - oy);
			int u = (int)pu[(y / 2) * cs + x / 2] - 128;
			int v = (int)pv[(y / 2) * cs + x / 2] - 128;
			int r = (c + kr * v + 128) >> 8;
			int g = (c - kgu * u - kgv * v + 128) >> 8;
			int b = (c + kb * u + 128) >> 8;
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
	//H.264, avc1, its parameter sets each after a start code, or Motion
	//JPEG, jpeg, its frames as they are, a nal_length of 0
	std::vector<const uint8_t*> sets;
	std::vector<size_t> sizes;
	int nal_length = 4;
	bool jpeg = codec == 0x6a706567;
	if (jpeg) { if (len < 8) return 0; nal_length = 0; }
	else if (codec != 0x61766331 || !video_avcc(setup, len, sets, sizes, nal_length)) return 0;
	if (auto va = va_open(codec, setup, len))
	{
		auto d = new Video_Decoder();
		d->va = va;
		return (uint64_t)d;
	}
	auto d = new Video_Decoder();
	d->nal_length = nal_length;
	d->full = jpeg;
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
	fmt.fmt.pix_mp.pixelformat = jpeg ? V4L2_PIX_FMT_MJPEG : V4L2_PIX_FMT_H264;
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
	if (d->va) return va_decode(d->va, data, len, shown);
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
	//a frame of Motion JPEG as it is
	if (d->nal_length == 0)
	{
		if (used + len > room) return -1;
		memcpy(at + used, data, len);
		used += len;
	}
	else for (uint64_t k = 0; k + d->nal_length <= len;)
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
	if (d->va) return va_frame(d->va, pixels, w, h, stride, shown);
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
	if (d->va)
	{
		va_close(d->va);
		delete d;
		return;
	}
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
	//avc1 H.264, hvc1 H.265, the parameter sets each after a start code;
	//mp4v MPEG-4 Part 2 and jpeg Motion JPEG, their frames as they are, a
	//nal_length of 0, their size, and of mp4v its headers
	std::vector<const uint8_t*> sets;
	std::vector<size_t> sizes;
	int nal_length = 4;
	GUID kind;
	bool sized = (codec == 0x6d703476 || codec == 0x6a706567) && len >= 8;
	if (codec == 0x61766331) { if (!video_avcc(setup, len, sets, sizes, nal_length)) return 0; kind = MFVideoFormat_H264; }
	else if (codec == 0x68766331) { if (!video_hvcc(setup, len, sets, sizes, nal_length)) return 0; kind = MFVideoFormat_HEVC; }
	else if (sized) { nal_length = 0; kind = codec == 0x6d703476 ? MFVideoFormat_MP4V : MFVideoFormat_MJPG; }
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
	if (sized)
	{
		UINT64 w = setup[0] | (setup[1] << 8) | (setup[2] << 16) | ((UINT64)setup[3] << 24);
		UINT64 h = setup[4] | (setup[5] << 8) | (setup[6] << 16) | ((UINT64)setup[7] << 24);
		type->SetUINT64(MF_MT_FRAME_SIZE, (w << 32) | h);
		if (len > 8) type->SetBlob(MF_MT_MPEG_SEQUENCE_HEADER, setup + 8, (UINT32)(len - 8));
	}
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
	//Annex B: the sets before the first packet, each NAL after a start code,
	//or a frame of mp4v or jpeg as it is
	std::vector<uint8_t> annexb;
	if (d->first)
	{
		annexb = d->sets;
		d->first = false;
	}
	if (d->nal_length == 0) annexb.insert(annexb.end(), data, data + len);
	else for (uint64_t k = 0; k + d->nal_length <= len;)
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
