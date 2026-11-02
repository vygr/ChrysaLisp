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
// JPEG, and MPEG-4 Part 2 by its own. Linux: VA-API, a PC's graphics, H.264,
// H.265 and Motion JPEG, if the machine has libva and a driver for them, else
// the stateful decoder of V4L2, /dev/video10, H.264 and Motion JPEG, the
// Raspberry Pi's, and its stateless one of H.265. Windows: a decoder of Media Foundation, H.264, and H.265 if one is installed, MPEG-4
// Part 2 and Motion JPEG, not yet tried on Windows itself. Elsewhere none,
// open is 0. A host built with no GUI driver has no table to call them by,
// and none of this.

#if defined(_HOST_GUI)

#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#include "video.h"
#include <vector>
#include <memory>
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
	std::vector<size_t> &sizes, int &nal_length, bool sei = false)
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
			//the parameter sets, VPS SPS PPS, and prefix SEI if asked for,
			//as that of HDR's light may be kept here
			if ((kind >= 32 && kind <= 34) || (sei && kind == 39))
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
// H.264, H.265 of 8 bits and 10, and Motion JPEG. It is in every Linux build, no libva-dev needed,
// and libva is loaded as it is wanted, so the host starts on a machine
// without it, and it is used only for a profile its driver lists. Its decoding is
// stateless: the stream's headers are read here, and what each picture is
// made from is kept and told it, as the standard has it. The pictures come
// back NV12, P010 of 10 bits, or as a JPEG's sampling has them, and are made blue green red
// alpha as they are asked for. What it can not, and on a Raspberry Pi, goes
// to the V4L2 decoder below.

#include <stdio.h>
#include <dlfcn.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/ioctl.h>
#include <linux/dma-buf.h>
#include <linux/videodev2.h>
#include <linux/media.h>
#include <poll.h>
#include <errno.h>
#include <thread>
#include <math.h>

//what is used of VA-API, as libva 2.x has it, libva.so.2 and libva-drm.so.2:
//its types and constants, the buffers of H.264, H.265 and baseline JPEG, the
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
	VAProfileH264ConstrainedBaseline = 13,
	VAProfileHEVCMain = 17,
	VAProfileHEVCMain10 = 18
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
#define VA_RT_FORMAT_YUV420_10 0x00000100
#define VA_SLICE_DATA_FLAG_ALL 0x00
#define VA_PICTURE_H264_INVALID 0x00000001
#define VA_PICTURE_H264_SHORT_TERM_REFERENCE 0x00000008
#define VA_PICTURE_H264_LONG_TERM_REFERENCE 0x00000010
#define VA_PICTURE_HEVC_INVALID 0x00000001
#define VA_PICTURE_HEVC_LONG_TERM_REFERENCE 0x00000008
#define VA_PICTURE_HEVC_RPS_ST_CURR_BEFORE 0x00000010
#define VA_PICTURE_HEVC_RPS_ST_CURR_AFTER 0x00000020
#define VA_PICTURE_HEVC_RPS_LT_CURR 0x00000040
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
#define VA_FOURCC_P010 0x30313050

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

struct VAPictureHEVC
{
	VASurfaceID picture_id;
	int32_t pic_order_cnt;
	uint32_t flags;
	uint32_t va_reserved[4];
};

struct VAPictureParameterBufferHEVC
{
	VAPictureHEVC CurrPic;
	VAPictureHEVC ReferenceFrames[15];
	uint16_t pic_width_in_luma_samples, pic_height_in_luma_samples;
	union
	{
		struct
		{
			uint32_t chroma_format_idc : 2;
			uint32_t separate_colour_plane_flag : 1;
			uint32_t pcm_enabled_flag : 1;
			uint32_t scaling_list_enabled_flag : 1;
			uint32_t transform_skip_enabled_flag : 1;
			uint32_t amp_enabled_flag : 1;
			uint32_t strong_intra_smoothing_enabled_flag : 1;
			uint32_t sign_data_hiding_enabled_flag : 1;
			uint32_t constrained_intra_pred_flag : 1;
			uint32_t cu_qp_delta_enabled_flag : 1;
			uint32_t weighted_pred_flag : 1;
			uint32_t weighted_bipred_flag : 1;
			uint32_t transquant_bypass_enabled_flag : 1;
			uint32_t tiles_enabled_flag : 1;
			uint32_t entropy_coding_sync_enabled_flag : 1;
			uint32_t pps_loop_filter_across_slices_enabled_flag : 1;
			uint32_t loop_filter_across_tiles_enabled_flag : 1;
			uint32_t pcm_loop_filter_disabled_flag : 1;
			uint32_t NoPicReorderingFlag : 1;
			uint32_t NoBiPredFlag : 1;
			uint32_t ReservedBits : 11;
		} bits;
		uint32_t value;
	} pic_fields;
	uint8_t sps_max_dec_pic_buffering_minus1, bit_depth_luma_minus8, bit_depth_chroma_minus8;
	uint8_t pcm_sample_bit_depth_luma_minus1, pcm_sample_bit_depth_chroma_minus1;
	uint8_t log2_min_luma_coding_block_size_minus3, log2_diff_max_min_luma_coding_block_size;
	uint8_t log2_min_transform_block_size_minus2, log2_diff_max_min_transform_block_size;
	uint8_t log2_min_pcm_luma_coding_block_size_minus3, log2_diff_max_min_pcm_luma_coding_block_size;
	uint8_t max_transform_hierarchy_depth_intra, max_transform_hierarchy_depth_inter;
	int8_t init_qp_minus26;
	uint8_t diff_cu_qp_delta_depth;
	int8_t pps_cb_qp_offset, pps_cr_qp_offset;
	uint8_t log2_parallel_merge_level_minus2, num_tile_columns_minus1, num_tile_rows_minus1;
	uint16_t column_width_minus1[19], row_height_minus1[21];
	union
	{
		struct
		{
			uint32_t lists_modification_present_flag : 1;
			uint32_t long_term_ref_pics_present_flag : 1;
			uint32_t sps_temporal_mvp_enabled_flag : 1;
			uint32_t cabac_init_present_flag : 1;
			uint32_t output_flag_present_flag : 1;
			uint32_t dependent_slice_segments_enabled_flag : 1;
			uint32_t pps_slice_chroma_qp_offsets_present_flag : 1;
			uint32_t sample_adaptive_offset_enabled_flag : 1;
			uint32_t deblocking_filter_override_enabled_flag : 1;
			uint32_t pps_disable_deblocking_filter_flag : 1;
			uint32_t slice_segment_header_extension_present_flag : 1;
			uint32_t RapPicFlag : 1;
			uint32_t IdrPicFlag : 1;
			uint32_t IntraPicFlag : 1;
			uint32_t ReservedBits : 18;
		} bits;
		uint32_t value;
	} slice_parsing_fields;
	uint8_t log2_max_pic_order_cnt_lsb_minus4, num_short_term_ref_pic_sets, num_long_term_ref_pic_sps;
	uint8_t num_ref_idx_l0_default_active_minus1, num_ref_idx_l1_default_active_minus1;
	int8_t pps_beta_offset_div2, pps_tc_offset_div2;
	uint8_t num_extra_slice_header_bits;
	uint32_t st_rps_bits;
	uint32_t va_reserved[8];
};

struct VAIQMatrixBufferHEVC
{
	uint8_t ScalingList4x4[6][16];
	uint8_t ScalingList8x8[6][64];
	uint8_t ScalingList16x16[6][64];
	uint8_t ScalingList32x32[2][64];
	uint8_t ScalingListDC16x16[6];
	uint8_t ScalingListDC32x32[2];
	uint32_t va_reserved[4];
};

struct VASliceParameterBufferHEVC
{
	uint32_t slice_data_size, slice_data_offset, slice_data_flag, slice_data_byte_offset;
	uint32_t slice_segment_address;
	uint8_t RefPicList[2][15];
	union
	{
		struct
		{
			uint32_t LastSliceOfPic : 1;
			uint32_t dependent_slice_segment_flag : 1;
			uint32_t slice_type : 2;
			uint32_t color_plane_id : 2;
			uint32_t slice_sao_luma_flag : 1;
			uint32_t slice_sao_chroma_flag : 1;
			uint32_t mvd_l1_zero_flag : 1;
			uint32_t cabac_init_flag : 1;
			uint32_t slice_temporal_mvp_enabled_flag : 1;
			uint32_t slice_deblocking_filter_disabled_flag : 1;
			uint32_t collocated_from_l0_flag : 1;
			uint32_t slice_loop_filter_across_slices_enabled_flag : 1;
			uint32_t reserved : 18;
		} fields;
		uint32_t value;
	} LongSliceFlags;
	uint8_t collocated_ref_idx, num_ref_idx_l0_active_minus1, num_ref_idx_l1_active_minus1;
	int8_t slice_qp_delta, slice_cb_qp_offset, slice_cr_qp_offset, slice_beta_offset_div2, slice_tc_offset_div2;
	uint8_t luma_log2_weight_denom;
	int8_t delta_chroma_log2_weight_denom;
	int8_t delta_luma_weight_l0[15], luma_offset_l0[15];
	int8_t delta_chroma_weight_l0[15][2], ChromaOffsetL0[15][2];
	int8_t delta_luma_weight_l1[15], luma_offset_l1[15];
	int8_t delta_chroma_weight_l1[15][2], ChromaOffsetL1[15][2];
	uint8_t five_minus_max_num_merge_cand;
	uint16_t num_entry_point_offsets, entry_offset_to_subset_array, slice_data_num_emu_prevn_bytes;
	uint32_t va_reserved[2];
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
	VAStatus vaTerminate(VADisplay dpy);
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
#define VA_CALLS(X) X(vaInitialize) X(vaTerminate) X(vaSetInfoCallback) X(vaSetErrorCallback) \
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
		else
		{
			if (d) va::vaTerminate(d);
			close(fd);
		}
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
		//no field of H.264 or H.265 needs more than 20 zeros before its 1, so a
		//stream with more is broken, its bits over: every value fits an int
		int zeros = 0;
		while (!bit()) if (++zeros > 20) { at = n * 8 + 1; return 0; }
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

static int va_matrix(int coefficients, uint32_t height)
{
	//the colours of a stream's VUI: BT.709 1, BT.601 5 and 6, BT.2020 9 and
	//10; not said, by its size, HD's 709
	if (coefficients == 1) return 1;
	if (coefficients == 5 || coefficients == 6) return 0;
	if (coefficients == 9 || coefficients == 10) return 2;
	return height >= 720 ? 1 : 0;
}

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
	int matrix;
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
		if (s.chroma_format_idc > 3) return false;
		if (s.chroma_format_idc == 3) s.separate_colour_plane = b.u(1);
		s.bit_depth_luma = b.ue() + 8;
		s.bit_depth_chroma = b.ue() + 8;
		b.u(1);
		if ((s.scaling = b.u(1))) va_scaling(b, s.chroma_format_idc != 3 ? 8 : 12, s.lists4, s.lists8, nullptr);
	}
	//of the standard, 4 to 16 bits, and an order of type 0, 1 or 2
	s.log2_max_frame_num = b.ue() + 4;
	s.poc_type = b.ue();
	if (s.log2_max_frame_num > 16 || s.poc_type > 2) return false;
	s.log2_max_poc_lsb = s.delta_pic_order_always_zero = s.offset_for_non_ref_pic = 0;
	s.offset_for_top_to_bottom_field = s.cycle = 0;
	if (s.poc_type == 0)
	{
		s.log2_max_poc_lsb = b.ue() + 4;
		if (s.log2_max_poc_lsb > 16) return false;
	}
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
	//of the VUI only the range and the colours, video's 16 to 235 unless
	//it says all
	s.full = false;
	int coefficients = 2;
	if (b.u(1))
	{
		if (b.u(1) && b.u(8) == 255) b.u(32);
		if (b.u(1)) b.u(1);
		if (b.u(1))
		{
			b.u(3);
			s.full = b.u(1);
			if (b.u(1)) { b.u(8); b.u(8); coefficients = b.u(8); }
		}
	}
	//no bigger than 16384 pixels each way, of any level of the standard
	if (b.over() || s.max_num_ref_frames > 16 || s.width_mbs > 1024 || s.height_mbs > 1024
		|| s.width_mbs * 16 <= s.crop_left + s.crop_right
		|| s.height_mbs * 16 <= s.crop_top + s.crop_bottom) return false;
	s.matrix = va_matrix(coefficients, s.height_mbs * 16 - s.crop_top - s.crop_bottom);
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
	//of U and V their samples, 2 of NV12, which has them side by side, and
	//the bytes of each sample, 2 of P010, its 10 bits high
	size_t y_at, u_at, v_at;
	uint32_t y_pitch, c_pitch, step, bytes = 1;
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

//of H.265, a set of the pictures a picture keeps, by how far each is
//before it in the order shown, then after it, and if it is made from it
struct Va_Rps
{
	int negative = 0, positive = 0;
	int delta[32];
	bool used[32];
};

//the scaling lists of H.265, of 4x4 8x8 16x16 and 32x32, each of 6 kinds,
//in the order the stream has them, and of 16x16 and 32x32 their DC
struct Va_Hlists
{
	uint8_t lists[4][6][64], dc[4][6];
};

struct Va_Hsps
{
	bool valid = false;
	int width, height, crop_left, crop_right, crop_top, crop_bottom;
	int chroma_format_idc, bit_depth_luma, bit_depth_chroma, log2_max_poc_lsb, max_dec_pic_buffering, num_reorder;
	int log2_min_cb, log2_diff_cb, log2_min_tb, log2_diff_tb, depth_inter, depth_intra;
	int scaling, amp, sao, pcm, pcm_depth_luma, pcm_depth_chroma, log2_min_pcm, log2_diff_pcm, pcm_loop_filter_disabled;
	int long_term_present, num_lt, temporal_mvp, strong_intra;
	int lt_poc_lsb[32];
	bool lt_used[32];
	int num_rps;
	Va_Rps rps[64];
	Va_Hlists lists;
	bool full;
	int matrix, primaries, transfer;
};

struct Va_Hpps
{
	bool valid = false;
	int sps_id, dependent_slices, output_flag_present, extra_bits, sign_hiding, cabac_init_present;
	int num_ref_idx_default[2], init_qp, constrained_intra, transform_skip, cu_qp_delta, diff_cu_qp_delta_depth;
	int cb_qp_offset, cr_qp_offset, slice_chroma_qp_offsets, weighted_pred, weighted_bipred, transquant_bypass;
	int tiles, entropy_sync, columns, rows, uniform, loop_filter_across_tiles, loop_filter_across_slices;
	int column_width[20], row_height[22];
	int deblocking_override, deblocking_disabled, beta, tc, lists_modification, log2_parallel_merge_level, header_extension;
	bool scaling;
	Va_Hlists lists;
};

struct Va_Hslice
{
	int first, dependent, pps_id, address, type, output, poc_lsb;
	//its short term set, and the bits it takes if it is the slice's own
	Va_Rps rps;
	int rps_bits;
	//of each long term picture, the low bits of its order or all of
	//them, by its high bits, if it is made from it
	int num_lt;
	int lt_lsb[32], lt_cycle[32];
	bool lt_used[32], lt_msb[32];
	int temporal_mvp, sao_luma, sao_chroma, num_ref_idx[2], mods[2], entries[2][16];
	int mvd_l1_zero, cabac_init, collocated_from_l0, collocated_ref_idx, five_minus_max_merge;
	int qp_delta, cb_qp_offset, cr_qp_offset, deblocking_disabled, beta, tc, loop_filter_across_slices;
	size_t header_bytes;
};

//of a picture of 10 bits of BT.2020's primaries, PQ or HLG of HDR or of
//SDR: its R'G'B' made light as it is to be shown, to the SDR display's
//white at most, BT.2390's tone map from the brightest of the film to
//it, then of BT.709's primaries, and the display's 8 bits. By tables,
//each of 1D: Y'CbCr to R'G'B' of 12 bits, 16.16, its light of 14 bits
//by each, the primaries by a matrix of 14 bits, and 8 bits of the light.
//Of 32 bits each, as AVX2 gathers them
struct Va_Tone
{
	int transfer = -1, peak = -1, full = -1;
	int32_t oy, ky, kr, kgu, kgv, kb;
	int32_t gamut[9];
	int32_t light[4096];
	int32_t out[16384];
};

struct Va_Decoder
{
	bool jpeg = false;
	bool full = false;
	//the colours of its pictures, BT.601 0, BT.709 1, BT.2020 2, and of
	//H.265 their primaries and transfer as the VUI has them, 16 PQ, 18
	//HLG, and the light of the brightest, of its SEI, and what a picture
	//of BT.2020 is made 8 bits by
	int matrix = 0, primaries = 2, transfer = 2, peak = 0;
	std::unique_ptr<Va_Tone> tone;
	int nal_length = 4;
	VAProfile profile = VAProfileNone, asked = VAProfileNone, chosen = VAProfileNone;
	VAConfigID config = VA_INVALID_ID;
	VAContextID context = VA_INVALID_ID;
	std::vector<VASurfaceID> surfaces;
	uint32_t format = 0, coded_w = 0, coded_h = 0;
	std::vector<Va_Held> held;
	//the memory of pictures gone, for those to come: a new one of 4K is
	//as long to be given as to fill
	std::vector<std::vector<uint8_t>> spare;
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
	//the SPS the picture was started with, as it was then
	Va_Sps pic_sps;
	std::vector<VABufferID> buffers;
	//the slices of the picture, each its parameters and its data, given
	//the driver together at its end: Intel's takes more than one no other way
	std::vector<VABufferID> slices;
	//of H.265, its parameter sets; of the picture being decoded, its
	//order and what of those kept it is made from, by their index, and the
	//slice before, the last of the picture once it is ended; and what the
	//next picture's order follows on from
	bool hevc = false;
	std::vector<Va_Hsps> hsps;
	std::vector<Va_Hpps> hpps;
	bool hfirst = true, no_rasl = true, hskip = false, hout = true, hpending = false;
	int prev_tid0_poc = 0, cur_poc = 0, hpic_pps = 0;
	int hbefore[16], hafter[16], hlt[16], nbefore = 0, nafter = 0, nlt = 0;
	Va_Hsps hpic_sps;
	Va_Hslice hlast;
	VASliceParameterBufferHEVC hlast_param, hpend_param;
	VABufferID hpend_data = VA_INVALID_ID;
	//of H.265 by the V4L2 stateless decoder, not VA-API
	struct Hv *hv = nullptr;
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

template <typename F> static void va_rows(uint32_t rows, uint32_t align, F f)
{
	//rows of a picture of 4K and the like each a part of the CPU's threads,
	//as many as it has to 4, each from a whole number of align
	uint32_t n = std::min(4u, std::max(1u, std::thread::hardware_concurrency()));
	if (rows < 1024 || n < 2)
	{
		f(0, rows);
		return;
	}
	std::vector<std::thread> threads;
	auto at = [&](uint32_t i) { return std::min(rows, (rows * i / n + align - 1) / align * align); };
	for (uint32_t i = 1; i < n; i++) threads.emplace_back(f, at(i), at(i + 1));
	f(0, at(1));
	for (auto &t : threads) t.join();
}

static bool va_planes(Va_Held &pic, const VAImage &image, const uint8_t *at, uint32_t x, uint32_t y)
{
	//of an image of the driver's, the planes as they are, each a row at a
	//time, its memory read as it must be, uncached, once
	int u_plane = 1, v_plane = 2, u_at = 0, v_at = 0;
	pic.step = 1;
	if (image.format.fourcc == VA_FOURCC_P010)
	{
		//of 2 bytes a sample, U and V side by side
		uint32_t cw = (pic.width + 1) / 2, ch = (pic.height + 1) / 2;
		pic.sx = pic.sy = 1;
		pic.step = pic.bytes = 2;
		pic.y_pitch = pic.width * 2;
		pic.c_pitch = cw * 4;
		pic.u_at = (size_t)pic.y_pitch * pic.height;
		pic.v_at = pic.u_at + 2;
		pic.data.resize(pic.u_at + (size_t)pic.c_pitch * ch);
		for (uint32_t r = 0; r < pic.height; r++)
			memcpy(&pic.data[(size_t)r * pic.y_pitch], at + image.offsets[0] + (size_t)(y + r) * image.pitches[0] + (size_t)x * 2, pic.y_pitch);
		for (uint32_t r = 0; r < ch; r++)
			memcpy(&pic.data[pic.u_at + (size_t)r * pic.c_pitch], at + image.offsets[1] + (size_t)(y / 2 + r) * image.pitches[1] + (size_t)(x / 2) * 4, pic.c_pitch);
		return true;
	}
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
	//an NV12 or P010 surface as the driver gave it to the GPU, its memory mapped,
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
	uint32_t bytes = desc.fourcc == VA_FOURCC_P010 ? 2 : 1;
	bool ok = (desc.fourcc == VA_FOURCC_NV12 || bytes == 2) && desc.num_objects == 1 && desc.num_layers == 1
		&& desc.layers[0].num_planes == 2 && desc.layers[0].pitch[1] == pitch && rows >= y + pic.height
		&& pitch >= (x + pic.width) * bytes && desc.layers[0].offset[0] == 0 && (size_t)pitch * rows * 3 / 2 <= desc.objects[0].size;
	//Y tiles are read whole, of U and V too, so its rows to a whole tile
	size_t uv_rows = mod == linear ? rows / 2 : (rows / 2 + 31) & ~31u;
	ok = ok && desc.layers[0].offset[1] + (size_t)pitch * uv_rows <= desc.objects[0].size;
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
		pic.data.resize((size_t)pitch * (rows + uv_rows));
		if (mod == linear) memcpy(pic.data.data(), map, (size_t)pitch * rows * 3 / 2);
#if defined(__x86_64__)
		else
		{
			//Y's tiles and U and V's, by rows of tiles from each thread
			auto from = (const uint8_t*)map;
			uint8_t *to = pic.data.data();
			uint32_t uv_at = desc.layers[0].offset[1];
			va_rows(rows + rows / 2, 32, [=](uint32_t r0, uint32_t r1) {
				if (r0 < rows) va_untile(from + (size_t)r0 * pitch, to + (size_t)r0 * pitch, pitch, std::min(r1, rows) - r0);
				if (r1 > rows)
				{
					uint32_t c0 = std::max(r0, rows) - rows, c1 = r1 - rows;
					va_untile(from + uv_at + (size_t)c0 * pitch, to + (size_t)(rows + c0) * pitch, pitch, c1 - c0);
				}
			});
		}
#endif
		sync.flags = DMA_BUF_SYNC_END | DMA_BUF_SYNC_READ;
		ioctl(desc.objects[0].fd, DMA_BUF_IOCTL_SYNC, &sync);
		munmap(map, desc.objects[0].size);
		//the picture where it is in the surface, a frame cropped
		pic.sx = pic.sy = 1;
		pic.step = 2;
		pic.bytes = bytes;
		pic.y_pitch = pic.c_pitch = pitch;
		pic.y_at = (size_t)y * pitch + (size_t)x * bytes;
		pic.u_at = (size_t)pitch * rows + (size_t)(y / 2) * pitch + (size_t)(x & ~1u) * bytes;
		pic.v_at = pic.u_at + bytes;
	}
	else ok = false;
	for (uint32_t i = 0; i < desc.num_objects; i++) close(desc.objects[i].fd);
	return ok;
}

static bool va_read(Va_Decoder *d, int surface, uint32_t x, uint32_t y, uint32_t w, uint32_t h, int64_t shown)
{
	//the picture of a surface, from x y and w by h of it, held: by its
	//memory, or the image the driver has of it, or one of NV12 it is
	//copied to, P010 of 10 bits
	VADisplay dpy = va_display();
	VASurfaceID id = d->surfaces[surface];
	if (va::vaSyncSurface(dpy, id) != VA_STATUS_SUCCESS) return false;
	Va_Held pic;
	if (!d->spare.empty())
	{
		pic.data.swap(d->spare.back());
		d->spare.pop_back();
	}
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
		bool ten = d->format == VA_RT_FORMAT_YUV420_10;
		VAImageFormat copy = {};
		copy.fourcc = ten ? VA_FOURCC_P010 : VA_FOURCC_NV12;
		copy.byte_order = VA_LSB_FIRST;
		copy.bits_per_pixel = ten ? 24 : 12;
		if (va::vaCreateImage(dpy, &copy, d->coded_w, d->coded_h, &image) != VA_STATUS_SUCCESS) return false;
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

//the colours of BT.601, BT.709 and BT.2020, limited, or of JPEG and of a
//stream that says so full, BT.601's as the V4L2 decoder has it: the
//numbers of Y, of red of V, green of U and V, and blue of U, and what is
//taken from Y
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
static uint32_t va_row(const Va_Matrix &m, const uint8_t *py, const uint8_t *pu, const uint8_t *pv, uint32_t step,
	uint32_t bytes, uint32_t *out, uint32_t n)
{
	//8 pixels at a time, two to a U and V, by SSE2, which every x86_64 has:
	//each sum of two products at 32 bits, as the scalar does, and made 8
	//bits with saturation, its clamp. Of P010 the top 8 bits of each. The
	//pixels done
	const __m128i zero = _mm_setzero_si128(), c128 = _mm_set1_epi16(128), oy = _mm_set1_epi16(m.oy);
	const __m128i kr = _mm_set_epi16(m.kr, m.ky, m.kr, m.ky, m.kr, m.ky, m.kr, m.ky);
	const __m128i kgu = _mm_set_epi16(-m.kgu, m.ky, -m.kgu, m.ky, -m.kgu, m.ky, -m.kgu, m.ky);
	const __m128i kgv = _mm_set_epi16(128, -m.kgv, 128, -m.kgv, 128, -m.kgv, 128, -m.kgv);
	const __m128i kb = _mm_set_epi16(m.kb, m.ky, m.kb, m.ky, m.kb, m.ky, m.kb, m.ky);
	const __m128i round = _mm_set1_epi32(128), one = _mm_set1_epi16(1), alpha = _mm_set1_epi8((char)0xff);
	uint32_t x = 0;
	for (; x + 8 <= n; x += 8)
	{
		//Y, and U and V of 4 pairs, as U V U V
		__m128i y16, uv;
		if (bytes == 2)
		{
			y16 = _mm_sub_epi16(_mm_srli_epi16(_mm_loadu_si128((const __m128i*)(py + x * 2)), 8), oy);
			uv = _mm_sub_epi16(_mm_srli_epi16(_mm_loadu_si128((const __m128i*)(pu + x * 2)), 8), c128);
		}
		else
		{
			y16 = _mm_sub_epi16(_mm_unpacklo_epi8(_mm_loadl_epi64((const __m128i*)(py + x)), zero), oy);
			__m128i uv8;
			if (step == 2) uv8 = _mm_loadl_epi64((const __m128i*)(pu + x));
			else
			{
				int32_t u4, v4;
				memcpy(&u4, pu + x / 2, 4);
				memcpy(&v4, pv + x / 2, 4);
				uv8 = _mm_unpacklo_epi8(_mm_cvtsi32_si128(u4), _mm_cvtsi32_si128(v4));
			}
			uv = _mm_sub_epi16(_mm_unpacklo_epi8(uv8, zero), c128);
		}
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

static const Va_Matrix va_matrices[2][3] = {
	{{298, 16, 409, 100, 208, 516}, {298, 16, 459, 55, 136, 541}, {298, 16, 430, 48, 167, 548}},
	{{256, 0, 359, 88, 183, 454}, {256, 0, 403, 48, 120, 475}, {256, 0, 377, 42, 146, 482}}};

static void va_convert(const Va_Held &pic, bool full, int matrix, uint32_t *pixels, uint64_t w, uint64_t h, uint64_t stride)
{
	//the picture into the pixels given, as much as there is room for. Of
	//10 bits, the top 8 of each, the high byte of P010's
	const Va_Matrix &m = va_matrices[full][matrix];
	uint32_t cols = std::min((uint64_t)pic.width, w), rows = std::min((uint64_t)pic.height, h);
	uint32_t b = pic.bytes, hi = b - 1;
	va_rows(rows, 2, [&](uint32_t y0, uint32_t y1) {
		for (uint32_t y = y0; y < y1; y++)
		{
			const uint8_t *py = &pic.data[pic.y_at + (size_t)y * pic.y_pitch];
			const uint8_t *pu = &pic.data[pic.u_at + (size_t)(y >> pic.sy) * pic.c_pitch];
			const uint8_t *pv = &pic.data[pic.v_at + (size_t)(y >> pic.sy) * pic.c_pitch];
			auto out = (uint32_t*)((uint8_t*)pixels + y * stride);
			uint32_t x = 0;
#if defined(__x86_64__)
			if (pic.sx == 1) x = va_row(m, py, pu, pv, pic.step, b, out, cols);
#endif
			for (; x < cols; x++)
				out[x] = va_pixel(m, py[x * b + hi], (int)pu[(x >> pic.sx) * pic.step * b + hi] - 128,
					(int)pv[(x >> pic.sx) * pic.step * b + hi] - 128);
		}
	});
}

//////////////////////
// HDR and BT.2020
//////////////////////

//SMPTE ST 2084, PQ: a signal of 0 to 1 to nits and back
static const double va_m1 = 2610.0 / 16384, va_m2 = 2523.0 / 4096 * 128;
static const double va_c1 = 3424.0 / 4096, va_c2 = 2413.0 / 4096 * 32, va_c3 = 2392.0 / 4096 * 32;

static double va_pq_nits(double e)
{
	double p = pow(e, 1 / va_m2);
	return 10000 * pow(std::max(p - va_c1, 0.0) / (va_c2 - va_c3 * p), 1 / va_m1);
}

static double va_pq_signal(double nits)
{
	double p = pow(std::max(nits, 0.0) / 10000, va_m1);
	return pow((va_c1 + va_c2 * p) / (1 + va_c3 * p), va_m2);
}

static void va_tone_make(Va_Tone &t, int transfer, int peak, bool full)
{
	//SDR's white, BT.2408's of HDR, 203 nits; of HLG its display of 1000
	//nits and the gamma of the system 1.2, of each of R G and B
	const double white = 203;
	t.transfer = transfer;
	t.peak = peak;
	t.full = full;
	double sy = full ? 4095.0 / 1023 : 4095.0 / 876, sc = full ? 4095.0 / 1023 : 4095.0 / 896;
	t.oy = full ? 0 : 64;
	t.ky = lround(sy * 65536);
	t.kr = lround(1.4746 * sc * 65536);
	t.kgu = lround(0.164553 * sc * 65536);
	t.kgv = lround(0.571353 * sc * 65536);
	t.kb = lround(1.8814 * sc * 65536);
	bool hdr = transfer == 16 || transfer == 18;
	double lw = transfer == 18 ? 1000 : std::max(peak, 400);
	double pw = va_pq_signal(lw), most = va_pq_signal(white) / pw, ks = 1.5 * most - 0.5;
	for (int i = 0; i < 4096; i++)
	{
		double e = i / 4095.0, nits;
		if (transfer == 16) nits = va_pq_nits(e);
		else if (transfer == 18)
		{
			double scene = e <= 0.5 ? e * e / 3 : (exp((e - 0.55991073) / 0.17883277) + 0.28466892) / 12;
			nits = 1000 * pow(scene, 1.2);
		}
		else nits = white * pow(e, 2.2);
		if (hdr)
		{
			//BT.2390's EETF, of PQ's signal: as it is to the knee, then a
			//spline to the white at the film's brightest
			double x = std::min(va_pq_signal(nits) / pw, 1.0);
			if (ks < 1 && x > ks)
			{
				double k = (x - ks) / (1 - ks), k2 = k * k, k3 = k2 * k;
				x = (2 * k3 - 3 * k2 + 1) * ks + (k3 - 2 * k2 + k) * (1 - ks) + (-2 * k3 + 3 * k2) * most;
			}
			nits = va_pq_nits(x * pw);
		}
		t.light[i] = lround(std::min(nits / white, 1.0) * 16383);
	}
	//BT.2087's, of BT.2020's primaries to BT.709's, of light
	static const double gamut[9] = {
		1.6605, -0.5876, -0.0728,
		-0.1246, 1.1329, -0.0083,
		-0.0182, -0.1006, 1.1187};
	for (int i = 0; i < 9; i++) t.gamut[i] = lround(gamut[i] * 16384);
	//to the display, a PC's of 2.2, so the light is as it was mapped:
	//BT.1886's 2.4, of a TV in the dark, shows it lighter, washed out
	for (int i = 0; i < 16384; i++) t.out[i] = lround(255 * pow(i / 16383.0, 1 / 2.2));
}

static const Va_Tone &va_tone_of(Va_Decoder *d)
{
	//made again only when what it is made by changes
	if (!d->tone) d->tone = std::make_unique<Va_Tone>();
	Va_Tone &t = *d->tone;
	int peak = d->peak > 0 && d->peak <= 10000 ? d->peak : 1000;
	if (t.transfer != d->transfer || t.peak != peak || t.full != d->full) va_tone_make(t, d->transfer, peak, d->full);
	return t;
}

static inline int va_clamp(int v, int most) { return v < 0 ? 0 : v > most ? most : v; }

#if defined(__x86_64__)
__attribute__((target("avx2"))) static inline __m256i va_code(__m256i sum)
{
	//of 16.16 to R'G'B' of 12 bits
	return _mm256_min_epi32(_mm256_max_epi32(_mm256_srai_epi32(_mm256_add_epi32(sum, _mm256_set1_epi32(32768)), 16),
		_mm256_setzero_si256()), _mm256_set1_epi32(4095));
}

__attribute__((target("avx2"))) static inline __m256i va_mix(const Va_Tone &t, const __m256i *g, __m256i rg, __m256i b1)
{
	//a primary of BT.709, of the light of BT.2020's, to 8 bits: R and G,
	//and B and 1, each two of 16 bits, by theirs and its rounding
	__m256i sum = _mm256_add_epi32(_mm256_madd_epi16(rg, g[0]), _mm256_madd_epi16(b1, g[1]));
	return _mm256_i32gather_epi32(t.out, _mm256_min_epi32(_mm256_max_epi32(_mm256_srai_epi32(sum, 14),
		_mm256_setzero_si256()), _mm256_set1_epi32(16383)), 4);
}

__attribute__((target("avx2"))) static uint32_t va_row_wide(const Va_Tone &t, const uint16_t *py, const uint16_t *pc,
	uint32_t *out, uint32_t n)
{
	//8 pixels at a time, as the scalar does, its tables gathered. The
	//pixels done
	const __m256i oy = _mm256_set1_epi32(t.oy), ky = _mm256_set1_epi32(t.ky), c512 = _mm256_set1_epi32(512);
	const __m256i kr = _mm256_set1_epi32(t.kr), kgu = _mm256_set1_epi32(t.kgu), kgv = _mm256_set1_epi32(t.kgv);
	const __m256i kb = _mm256_set1_epi32(t.kb);
	const __m256i alpha = _mm256_set1_epi32((int)0xff000000);
	const __m256i us = _mm256_setr_epi32(0, 0, 2, 2, 4, 4, 6, 6), vs = _mm256_setr_epi32(1, 1, 3, 3, 5, 5, 7, 7);
	__m256i g[6];
	for (int i = 0; i < 3; i++)
	{
		g[i * 2] = _mm256_set1_epi32((int)(((uint32_t)t.gamut[i * 3] & 0xffff) | ((uint32_t)t.gamut[i * 3 + 1] << 16)));
		g[i * 2 + 1] = _mm256_set1_epi32((int)(((uint32_t)t.gamut[i * 3 + 2] & 0xffff) | (8192u << 16)));
	}
	const __m256i one = _mm256_set1_epi32(1 << 16);
	uint32_t x = 0;
	for (; x + 8 <= n; x += 8)
	{
		__m256i yy = _mm256_srli_epi32(_mm256_cvtepu16_epi32(_mm_loadu_si128((const __m128i*)(py + x))), 6);
		yy = _mm256_mullo_epi32(_mm256_sub_epi32(yy, oy), ky);
		__m256i uv = _mm256_sub_epi32(_mm256_srli_epi32(_mm256_cvtepu16_epi32(_mm_loadu_si128((const __m128i*)(pc + x))), 6), c512);
		__m256i u = _mm256_permutevar8x32_epi32(uv, us), v = _mm256_permutevar8x32_epi32(uv, vs);
		__m256i r = va_code(_mm256_add_epi32(yy, _mm256_mullo_epi32(kr, v)));
		__m256i gg = va_code(_mm256_sub_epi32(_mm256_sub_epi32(yy, _mm256_mullo_epi32(kgu, u)), _mm256_mullo_epi32(kgv, v)));
		__m256i b = va_code(_mm256_add_epi32(yy, _mm256_mullo_epi32(kb, u)));
		r = _mm256_i32gather_epi32(t.light, r, 4);
		gg = _mm256_i32gather_epi32(t.light, gg, 4);
		b = _mm256_i32gather_epi32(t.light, b, 4);
		__m256i rg = _mm256_or_si256(r, _mm256_slli_epi32(gg, 16)), b1 = _mm256_or_si256(b, one);
		__m256i pix = _mm256_or_si256(_mm256_or_si256(alpha, _mm256_slli_epi32(va_mix(t, g, rg, b1), 16)),
			_mm256_or_si256(_mm256_slli_epi32(va_mix(t, g + 2, rg, b1), 8), va_mix(t, g + 4, rg, b1)));
		_mm256_storeu_si256((__m256i*)(out + x), pix);
	}
	return x;
}
#endif

static void va_convert_wide(const Va_Held &pic, const Va_Tone &t, uint32_t *pixels, uint64_t w, uint64_t h, uint64_t stride)
{
	//a picture of P010 into the pixels given, as much as there is room
	//for, each pixel by the tables
	uint32_t cols = std::min((uint64_t)pic.width, w), rows = std::min((uint64_t)pic.height, h);
#if defined(__x86_64__)
	static bool avx2 = __builtin_cpu_supports("avx2");
#endif
	va_rows(rows, 2, [&](uint32_t y0, uint32_t y1) {
		for (uint32_t y = y0; y < y1; y++)
		{
			auto py = (const uint16_t*)&pic.data[pic.y_at + (size_t)y * pic.y_pitch];
			auto pc = (const uint16_t*)&pic.data[pic.u_at + (size_t)(y >> 1) * pic.c_pitch];
			auto out = (uint32_t*)((uint8_t*)pixels + y * stride);
			uint32_t x = 0;
#if defined(__x86_64__)
			if (avx2) x = va_row_wide(t, py, pc, out, cols);
#endif
			for (; x < cols; x += 2)
			{
				//U and V of two pixels, then each its Y
				int u = (pc[x] >> 6) - 512, v = (pc[x + 1] >> 6) - 512;
				int dr = t.kr * v + 32768, dg = 32768 - t.kgu * u - t.kgv * v, db = t.kb * u + 32768;
				for (uint32_t k = x; k < std::min(x + 2, cols); k++)
				{
					int yy = ((py[k] >> 6) - t.oy) * t.ky;
					int r = t.light[va_clamp((yy + dr) >> 16, 4095)];
					int g = t.light[va_clamp((yy + dg) >> 16, 4095)];
					int b = t.light[va_clamp((yy + db) >> 16, 4095)];
					int r2 = va_clamp((t.gamut[0] * r + t.gamut[1] * g + t.gamut[2] * b + 8192) >> 14, 16383);
					int g2 = va_clamp((t.gamut[3] * r + t.gamut[4] * g + t.gamut[5] * b + 8192) >> 14, 16383);
					int b2 = va_clamp((t.gamut[6] * r + t.gamut[7] * g + t.gamut[8] * b + 8192) >> 14, 16383);
					out[k] = 0xff000000 | (t.out[r2] << 16) | (t.out[g2] << 8) | t.out[b2];
				}
			}
		}
	});
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
		//0 to 7, of the standard, each a shift
		uint32_t luma = b.ue(), chroma_denom = chroma ? b.ue() : 0;
		if (luma > 7 || chroma_denom > 7) return false;
		param.luma_log2_weight_denom = luma;
		param.chroma_log2_weight_denom = chroma_denom;
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
	d->pic_sps = sp;
	d->full = sp.full;
	d->matrix = sp.matrix;
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
	const Va_Sps &sp = d->pic_sps;
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
			//a parameter set starts a new access unit, so ends the picture
			//before it, which is not to be read by the new set's size
			if (d->started) ok &= va_finish(d, shown);
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
		//a slice of the picture of another SPS is broken
		if (d->pps[s.pps_id].sps_id != d->pps[d->first.pps_id].sps_id) { ok = false; continue; }
		const Va_Sps &sp = d->pic_sps;
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

/////////
// H.265
/////////

//the default scaling lists of 8x8 and up, the standard's table 7-6, in
//the order the stream has them, intra and inter
static const uint8_t va_hdefault[2][64] = {
	{16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 16, 17, 16, 17, 18,
	17, 18, 18, 17, 18, 21, 19, 20, 21, 20, 19, 21, 24, 22, 22, 24,
	24, 22, 22, 24, 25, 25, 27, 30, 27, 25, 25, 29, 31, 35, 35, 31,
	29, 36, 41, 44, 41, 36, 47, 54, 54, 47, 65, 70, 65, 88, 88, 115},
	{16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17, 18,
	18, 18, 18, 18, 18, 20, 20, 20, 20, 20, 20, 20, 24, 24, 24, 24,
	24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 28, 28, 28, 28, 28,
	28, 33, 33, 33, 33, 33, 41, 41, 41, 41, 54, 54, 54, 71, 71, 91}};

static void va_hdefault_list(Va_Hlists &l, int size, int m)
{
	if (size == 0) memset(l.lists[0][m], 16, 64);
	else memcpy(l.lists[size][m], va_hdefault[m >= 3], 64);
	l.dc[size][m] = 16;
}

static void va_hdefaults(Va_Hlists &l)
{
	for (int size = 0; size < 4; size++)
		for (int m = 0; m < 6; m++) va_hdefault_list(l, size, m);
}

static bool va_hscaling(Va_Bits &b, Va_Hlists &l)
{
	//the lists of an SPS or a PPS, each said, or as one before it, or the
	//default. Of 32x32 only intra's and inter's of luma, of 4:2:0
	va_hdefaults(l);
	for (int size = 0; size < 4; size++)
	{
		int step = size == 3 ? 3 : 1, count = size ? 64 : 16;
		for (int m = 0; m < 6; m += step)
		{
			uint8_t *list = l.lists[size][m];
			if (!b.u(1))
			{
				uint32_t delta = b.ue() * step;
				if (delta > (uint32_t)m) return false;
				if (!delta) va_hdefault_list(l, size, m);
				else
				{
					memcpy(list, l.lists[size][m - delta], 64);
					l.dc[size][m] = l.dc[size][m - delta];
				}
				continue;
			}
			int next = 8;
			if (size > 1)
			{
				int dc = b.se() + 8;
				if (dc < 1 || dc > 255) return false;
				next = l.dc[size][m] = dc;
			}
			for (int i = 0; i < count; i++)
			{
				int delta = b.se();
				if (delta < -128 || delta > 127) return false;
				next = (next + delta + 256) % 256;
				//none of 0, the standard's, so none scales to nothing
				if (!next) return false;
				list[i] = next;
			}
		}
	}
	return !b.over();
}

static void va_hraster(uint8_t *to, const uint8_t *from, int n)
{
	//a list from the order the stream has it, up-right diagonal, the
	//standard's 6.5.3, to rows: as Intel's driver takes them, as
	//GStreamer gives them, and not as va_dec_hevc.h says
	int i = 0;
	for (int line = 0; i < n * n; line++)
		for (int y = line, x = 0; y >= 0; y--, x++)
			if (x < n && y < n) to[y * n + x] = from[i++];
}

static void va_hhrd(Va_Bits &b, int sub_layers)
{
	//hrd_parameters(), passed by
	int nal = b.u(1), vcl = b.u(1), sub_pic = 0;
	if (nal || vcl)
	{
		if ((sub_pic = b.u(1))) { b.u(8); b.u(5); b.u(1); b.u(5); }
		b.u(8);
		if (sub_pic) b.u(4);
		b.u(15);
	}
	for (int i = 0; i <= sub_layers && !b.over(); i++)
	{
		int fixed = b.u(1), low_delay = 0;
		if (!fixed) fixed = b.u(1);
		if (fixed) b.ue();
		else low_delay = b.u(1);
		uint32_t count = low_delay ? 1 : b.ue() + 1;
		if (count > 32) { b.at = b.n * 8 + 1; return; }
		for (int k = 0; k < nal + vcl; k++)
			for (uint32_t j = 0; j < count; j++)
			{
				b.ue();
				b.ue();
				if (sub_pic) { b.ue(); b.ue(); }
				b.u(1);
			}
	}
}

static bool va_hrps(Va_Bits &b, int idx, int num, const Va_Rps *sets, Va_Rps &out)
{
	//a short term set, of the SPS's, idx below num, or of a slice, idx of
	//num: as it is, or as one before it moved by a delta, those of it it
	//keeps, the standard's 7-61 and 7-62
	out.negative = out.positive = 0;
	if (idx && b.u(1))
	{
		uint32_t back = idx == num ? b.ue() + 1 : 1;
		if (back > (uint32_t)idx) return false;
		const Va_Rps &r = sets[idx - back];
		int sign = b.u(1);
		int delta_rps = (1 - 2 * sign) * (int)(b.ue() + 1);
		int count = r.negative + r.positive;
		bool used[33], use[33];
		for (int j = 0; j <= count; j++) use[j] = (used[j] = b.u(1)) ? true : b.u(1);
		int i = 0;
		for (int j = r.positive - 1; j >= 0; j--)
		{
			int poc = r.delta[r.negative + j] + delta_rps;
			if (poc < 0 && use[r.negative + j]) { out.delta[i] = poc; out.used[i++] = used[r.negative + j]; }
		}
		if (delta_rps < 0 && use[count]) { out.delta[i] = delta_rps; out.used[i++] = used[count]; }
		for (int j = 0; j < r.negative; j++)
		{
			int poc = r.delta[j] + delta_rps;
			if (poc < 0 && use[j]) { out.delta[i] = poc; out.used[i++] = used[j]; }
		}
		out.negative = i;
		for (int j = r.negative - 1; j >= 0; j--)
		{
			int poc = r.delta[j] + delta_rps;
			if (poc > 0 && use[j]) { out.delta[i] = poc; out.used[i++] = used[j]; }
		}
		if (delta_rps > 0 && use[count]) { out.delta[i] = delta_rps; out.used[i++] = used[count]; }
		for (int j = 0; j < r.positive; j++)
		{
			int poc = r.delta[r.negative + j] + delta_rps;
			if (poc > 0 && use[r.negative + j]) { out.delta[i] = poc; out.used[i++] = used[r.negative + j]; }
		}
		out.positive = i - out.negative;
	}
	else
	{
		uint32_t negative = b.ue(), positive = b.ue();
		if (negative > 16 || positive > 16 - negative) return false;
		int poc = 0;
		for (uint32_t i = 0; i < negative; i++)
		{
			out.delta[i] = poc -= b.ue() + 1;
			out.used[i] = b.u(1);
		}
		poc = 0;
		for (uint32_t i = 0; i < positive; i++)
		{
			out.delta[negative + i] = poc += b.ue() + 1;
			out.used[negative + i] = b.u(1);
		}
		out.negative = negative;
		out.positive = positive;
	}
	return !b.over() && out.negative + out.positive <= 16;
}

static int va_bits_for(uint32_t n)
{
	//Ceil(Log2(n)), the bits of a number below n
	int bits = 0;
	while (bits < 32 && (1u << bits) < n) bits++;
	return bits;
}

static void va_hsei(Va_Decoder *d, const std::vector<uint8_t> &rbsp)
{
	//of a prefix SEI, the light of HDR: its content's brightest, MaxCLL,
	//or else its mastering display's, in nits
	Va_Bits b = {rbsp.data(), rbsp.size(), 16};
	while (b.more() && !b.over())
	{
		uint32_t type = 0, size = 0, k;
		while ((k = b.u(8)) == 255 && !b.over()) type += 255;
		type += k;
		while ((k = b.u(8)) == 255 && !b.over()) size += 255;
		size += k;
		size_t next = b.at + (size_t)size * 8;
		if (type == 144 && size >= 4)
		{
			int cll = b.u(16);
			if (cll) d->peak = cll;
		}
		else if (type == 137 && size >= 24 && !d->peak)
		{
			for (int i = 0; i < 8; i++) b.u(16);
			d->peak = b.u(32) / 10000;
		}
		b.at = next;
	}
}

static bool va_hsps(const std::vector<uint8_t> &rbsp, std::vector<Va_Hsps> &all)
{
	Va_Bits b = {rbsp.data(), rbsp.size(), 16};
	auto s = std::make_unique<Va_Hsps>();
	b.u(4);
	int sub_layers = b.u(3);
	if (sub_layers > 6) return false;
	b.u(1);
	//profile_tier_level: of the whole stream its profile, Main or Main 10,
	//or Main Still Picture, by its number or that it is compatible with
	//one, or of the range extensions, if it is 4:2:0 of 8 or 10 bits and
	//has none of their tools, as Main Intra and Main 10 Intra
	int space = b.u(2);
	b.u(1);
	int profile = b.u(5);
	uint32_t compatible = b.u(32);
	b.u(32);
	b.u(16);
	b.u(8);
	if (space || ((profile < 1 || profile > 4) && !(compatible & 0x78000000))) return false;
	bool profile_present[8], level_present[8];
	for (int i = 0; i < sub_layers; i++)
	{
		profile_present[i] = b.u(1);
		level_present[i] = b.u(1);
	}
	if (sub_layers) for (int i = sub_layers; i < 8; i++) b.u(2);
	for (int i = 0; i < sub_layers; i++)
	{
		if (profile_present[i]) { b.u(32); b.u(32); b.u(24); }
		if (level_present[i]) b.u(8);
	}
	uint32_t id = b.ue();
	if (id > 15) return false;
	//4:2:0, the planes together, of the same depth, 8 or 10 bits
	s->chroma_format_idc = b.ue();
	if (s->chroma_format_idc != 1) return false;
	s->width = b.ue();
	s->height = b.ue();
	s->crop_left = s->crop_right = s->crop_top = s->crop_bottom = 0;
	if (b.u(1))
	{
		//in chroma samples, 2 each way
		s->crop_left = b.ue() * 2;
		s->crop_right = b.ue() * 2;
		s->crop_top = b.ue() * 2;
		s->crop_bottom = b.ue() * 2;
	}
	s->bit_depth_luma = b.ue() + 8;
	s->bit_depth_chroma = b.ue() + 8;
	if (s->bit_depth_luma != s->bit_depth_chroma || (s->bit_depth_luma != 8 && s->bit_depth_luma != 10)) return false;
	s->log2_max_poc_lsb = b.ue() + 4;
	if (s->log2_max_poc_lsb > 16) return false;
	//the DPB of the highest sub-layer, all of them
	bool ordering = b.u(1);
	for (int i = ordering ? 0 : sub_layers; i <= sub_layers; i++)
	{
		s->max_dec_pic_buffering = b.ue() + 1;
		s->num_reorder = b.ue();
		b.ue();
	}
	if (s->max_dec_pic_buffering > 16 || s->num_reorder >= s->max_dec_pic_buffering) return false;
	s->log2_min_cb = b.ue() + 3;
	s->log2_diff_cb = b.ue();
	s->log2_min_tb = b.ue() + 2;
	s->log2_diff_tb = b.ue();
	s->depth_inter = b.ue();
	s->depth_intra = b.ue();
	//of the standard: a CTB 16 to 64, a transform 4 to 32 and smaller than
	//a CTB, smaller than a coding block at its least, and so deep only
	int ctb = s->log2_min_cb + s->log2_diff_cb, max_tb = s->log2_min_tb + s->log2_diff_tb;
	if (s->log2_min_cb > 6 || ctb < 4 || ctb > 6 || s->log2_min_tb >= s->log2_min_cb || max_tb > std::min(ctb, 5)
		|| s->depth_inter > ctb - s->log2_min_tb || s->depth_intra > ctb - s->log2_min_tb) return false;
	if ((s->scaling = b.u(1)))
	{
		if (b.u(1)) { if (!va_hscaling(b, s->lists)) return false; }
		else va_hdefaults(s->lists);
	}
	s->amp = b.u(1);
	s->sao = b.u(1);
	s->pcm_depth_luma = s->pcm_depth_chroma = s->log2_min_pcm = s->log2_diff_pcm = s->pcm_loop_filter_disabled = 0;
	if ((s->pcm = b.u(1)))
	{
		s->pcm_depth_luma = b.u(4) + 1;
		s->pcm_depth_chroma = b.u(4) + 1;
		s->log2_min_pcm = b.ue() + 3;
		s->log2_diff_pcm = b.ue();
		s->pcm_loop_filter_disabled = b.u(1);
		if (s->pcm_depth_luma > s->bit_depth_luma || s->pcm_depth_chroma > s->bit_depth_chroma
			|| s->log2_min_pcm < std::min(s->log2_min_cb, 5) || s->log2_min_pcm + s->log2_diff_pcm > std::min(ctb, 5)) return false;
	}
	s->num_rps = b.ue();
	if (s->num_rps > 64) return false;
	for (int i = 0; i < s->num_rps; i++) if (!va_hrps(b, i, s->num_rps, s->rps, s->rps[i])) return false;
	s->num_lt = 0;
	if ((s->long_term_present = b.u(1)))
	{
		s->num_lt = b.ue();
		if (s->num_lt > 32) return false;
		for (int i = 0; i < s->num_lt; i++)
		{
			s->lt_poc_lsb[i] = b.u(s->log2_max_poc_lsb);
			s->lt_used[i] = b.u(1);
		}
	}
	s->temporal_mvp = b.u(1);
	s->strong_intra = b.u(1);
	//of the VUI the range and the colours, video's 16 to 235 unless it
	//says all, the rest passed by to the extensions after it
	s->full = false;
	int coefficients = 2, primaries = 2, transfer = 2;
	if (b.u(1))
	{
		if (b.u(1) && b.u(8) == 255) b.u(32);
		if (b.u(1)) b.u(1);
		if (b.u(1))
		{
			b.u(3);
			s->full = b.u(1);
			if (b.u(1)) { primaries = b.u(8); transfer = b.u(8); coefficients = b.u(8); }
		}
		if (b.u(1)) { b.ue(); b.ue(); }
		b.u(3);
		if (b.u(1)) { b.ue(); b.ue(); b.ue(); b.ue(); }
		if (b.u(1))
		{
			b.u(32);
			b.u(32);
			if (b.u(1)) b.ue();
			if (b.u(1)) va_hhrd(b, sub_layers);
		}
		if (b.u(1)) { b.u(3); b.ue(); b.ue(); b.ue(); b.ue(); b.ue(); }
	}
	//of the extensions, the range extensions' with none of their tools
	if (b.u(1))
	{
		int extensions = b.u(8);
		if ((extensions & 0x7f) || ((extensions & 0x80) && b.u(9))) return false;
	}
	//no bigger than 8192 each way, of the levels VA-API has, a whole
	//number of coding blocks, and some of it left once cropped
	int cb = 1 << s->log2_min_cb;
	if (b.over() || s->width <= 0 || s->height <= 0 || s->width > 8192 || s->height > 8192
		|| s->width % cb || s->height % cb || s->width <= s->crop_left + s->crop_right
		|| s->height <= s->crop_top + s->crop_bottom) return false;
	s->matrix = va_matrix(coefficients, s->height - s->crop_top - s->crop_bottom);
	s->primaries = primaries;
	s->transfer = transfer;
	s->valid = true;
	all[id] = *s;
	return true;
}

static bool va_hpps(const std::vector<uint8_t> &rbsp, std::vector<Va_Hpps> &all)
{
	//what can be checked of it alone; what is of its SPS as a picture
	//starts with them
	Va_Bits b = {rbsp.data(), rbsp.size(), 16};
	auto p = std::make_unique<Va_Hpps>();
	uint32_t id = b.ue();
	p->sps_id = b.ue();
	if (id > 63 || p->sps_id > 15) return false;
	p->dependent_slices = b.u(1);
	p->output_flag_present = b.u(1);
	p->extra_bits = b.u(3);
	p->sign_hiding = b.u(1);
	p->cabac_init_present = b.u(1);
	p->num_ref_idx_default[0] = b.ue() + 1;
	p->num_ref_idx_default[1] = b.ue() + 1;
	p->init_qp = 26 + b.se();
	p->constrained_intra = b.u(1);
	p->transform_skip = b.u(1);
	p->diff_cu_qp_delta_depth = (p->cu_qp_delta = b.u(1)) ? b.ue() : 0;
	p->cb_qp_offset = b.se();
	p->cr_qp_offset = b.se();
	p->slice_chroma_qp_offsets = b.u(1);
	p->weighted_pred = b.u(1);
	p->weighted_bipred = b.u(1);
	p->transquant_bypass = b.u(1);
	p->tiles = b.u(1);
	p->entropy_sync = b.u(1);
	p->columns = p->rows = 1;
	p->uniform = 1;
	p->loop_filter_across_tiles = 1;
	if (p->tiles)
	{
		//as many as VA-API is told of
		p->columns = b.ue() + 1;
		p->rows = b.ue() + 1;
		if (p->columns > 19 || p->rows > 21) return false;
		if (!(p->uniform = b.u(1)))
		{
			for (int i = 0; i < p->columns - 1; i++) p->column_width[i] = b.ue() + 1;
			for (int i = 0; i < p->rows - 1; i++) p->row_height[i] = b.ue() + 1;
		}
		p->loop_filter_across_tiles = b.u(1);
	}
	p->loop_filter_across_slices = b.u(1);
	p->deblocking_override = p->deblocking_disabled = p->beta = p->tc = 0;
	if (b.u(1))
	{
		p->deblocking_override = b.u(1);
		if (!(p->deblocking_disabled = b.u(1)))
		{
			p->beta = b.se();
			p->tc = b.se();
		}
	}
	if ((p->scaling = b.u(1)) && !va_hscaling(b, p->lists)) return false;
	p->lists_modification = b.u(1);
	p->log2_parallel_merge_level = b.ue() + 2;
	p->header_extension = b.u(1);
	//of the extensions, the range extensions' with none of their tools
	if (b.u(1))
	{
		int range = b.u(1);
		if (b.u(7)) return false;
		if (range && ((p->transform_skip && b.ue()) || b.u(1) || b.u(1) || b.ue() || b.ue())) return false;
	}
	if (b.over() || p->num_ref_idx_default[0] > 15 || p->num_ref_idx_default[1] > 15
		|| p->init_qp < -12 || p->init_qp > 51 || p->cb_qp_offset < -12 || p->cb_qp_offset > 12
		|| p->cr_qp_offset < -12 || p->cr_qp_offset > 12 || p->beta < -6 || p->beta > 6
		|| p->tc < -6 || p->tc > 6 || p->log2_parallel_merge_level > 6) return false;
	p->valid = true;
	all[id] = *p;
	return true;
}

static bool va_hweights(Va_Bits &b, const Va_Hslice &s, VASliceParameterBufferHEVC &param)
{
	//pred_weight_table: the weights, those not said as if none, and of
	//chroma its offset as the standard works it out, 7-56, of 8 bits
	uint32_t luma = b.ue();
	int chroma = (int)luma + b.se();
	if (luma > 7 || chroma < 0 || chroma > 7) return false;
	param.luma_log2_weight_denom = luma;
	param.delta_chroma_log2_weight_denom = chroma - (int)luma;
	for (int list = 0; list < (s.type == 0 ? 2 : 1); list++)
	{
		auto lw = list ? param.delta_luma_weight_l1 : param.delta_luma_weight_l0;
		auto lo = list ? param.luma_offset_l1 : param.luma_offset_l0;
		auto cw = list ? param.delta_chroma_weight_l1 : param.delta_chroma_weight_l0;
		auto co = list ? param.ChromaOffsetL1 : param.ChromaOffsetL0;
		bool lf[15], cf[15];
		int n = s.num_ref_idx[list];
		for (int i = 0; i < n; i++) lf[i] = b.u(1);
		for (int i = 0; i < n; i++) cf[i] = b.u(1);
		for (int i = 0; i < n; i++)
		{
			if (lf[i])
			{
				int w = b.se(), o = b.se();
				if (w < -128 || w > 127 || o < -128 || o > 127) return false;
				lw[i] = w;
				lo[i] = o;
			}
			if (cf[i]) for (int j = 0; j < 2; j++)
			{
				int w = b.se(), o = b.se();
				if (w < -128 || w > 127 || o < -512 || o > 511) return false;
				int offset = 128 - ((128 * ((1 << chroma) + w)) >> chroma) + o;
				cw[i][j] = w;
				co[i][j] = std::max(-128, std::min(127, offset));
			}
		}
	}
	return !b.over();
}

static bool va_hslice(Va_Decoder *d, const std::vector<uint8_t> &rbsp, int type, Va_Hslice &s,
	VASliceParameterBufferHEVC &param)
{
	//a slice segment's header, what VA-API is told of it into its
	//parameters, and its bytes from the start of the NAL, its header. Of
	//a dependent segment the rest as the one before it has it
	Va_Bits b = {rbsp.data(), rbsp.size(), 16};
	int first = b.u(1);
	if (type >= 16 && type <= 23) b.u(1);
	uint32_t pps_id = b.ue();
	if (pps_id > 63 || !d->hpps[pps_id].valid) return false;
	const Va_Hpps &p = d->hpps[pps_id];
	const Va_Hsps &sp = d->hsps[p.sps_id];
	if (!sp.valid) return false;
	int ctb = sp.log2_min_cb + sp.log2_diff_cb;
	uint32_t ctbs = ((sp.width + (1 << ctb) - 1) >> ctb) * ((sp.height + (1 << ctb) - 1) >> ctb);
	int dependent = 0, address = 0;
	if (!first)
	{
		if (p.dependent_slices) dependent = b.u(1);
		address = b.u(va_bits_for(ctbs));
		if ((uint32_t)address >= ctbs) return false;
	}
	if (dependent)
	{
		if (!d->started || d->hlast.pps_id != (int)pps_id) return false;
		s = d->hlast;
		param = d->hlast_param;
	}
	else
	{
		memset(&param, 0, sizeof(param));
		s.pps_id = pps_id;
		b.u(p.extra_bits);
		s.type = b.ue();
		if (s.type > 2) return false;
		s.output = p.output_flag_present ? b.u(1) : 1;
		s.poc_lsb = s.rps_bits = s.num_lt = s.temporal_mvp = 0;
		s.rps.negative = s.rps.positive = 0;
		if (type != 19 && type != 20)
		{
			s.poc_lsb = b.u(sp.log2_max_poc_lsb);
			if (!b.u(1))
			{
				size_t at = b.at;
				if (!va_hrps(b, sp.num_rps, sp.num_rps, sp.rps, s.rps)) return false;
				s.rps_bits = b.at - at;
			}
			else
			{
				if (!sp.num_rps) return false;
				uint32_t i = b.u(va_bits_for(sp.num_rps));
				if (i >= (uint32_t)sp.num_rps) return false;
				s.rps = sp.rps[i];
			}
			if (sp.long_term_present)
			{
				uint32_t of_sps = sp.num_lt ? b.ue() : 0, own = b.ue();
				if (of_sps > (uint32_t)sp.num_lt || own > 32 || of_sps + own > 32) return false;
				s.num_lt = of_sps + own;
				int cycle = 0;
				for (int i = 0; i < s.num_lt; i++)
				{
					if (i < (int)of_sps)
					{
						uint32_t k = sp.num_lt > 1 ? b.u(va_bits_for(sp.num_lt)) : 0;
						if (k >= (uint32_t)sp.num_lt) return false;
						s.lt_lsb[i] = sp.lt_poc_lsb[k];
						s.lt_used[i] = sp.lt_used[k];
					}
					else
					{
						s.lt_lsb[i] = b.u(sp.log2_max_poc_lsb);
						s.lt_used[i] = b.u(1);
					}
					uint32_t delta = (s.lt_msb[i] = b.u(1)) ? b.ue() : 0;
					//DeltaPocMsbCycleLt, after the first of each kind a sum
					cycle = i == 0 || i == (int)of_sps ? delta : cycle + delta;
					s.lt_cycle[i] = cycle;
				}
			}
			if (s.rps.negative + s.rps.positive + s.num_lt > 16) return false;
			s.temporal_mvp = sp.temporal_mvp ? b.u(1) : 0;
		}
		s.sao_luma = s.sao_chroma = 0;
		if (sp.sao)
		{
			s.sao_luma = b.u(1);
			s.sao_chroma = b.u(1);
		}
		bool ps = s.type == 1, bs = s.type == 0;
		s.num_ref_idx[0] = s.num_ref_idx[1] = s.mods[0] = s.mods[1] = 0;
		s.mvd_l1_zero = s.cabac_init = s.collocated_ref_idx = s.five_minus_max_merge = 0;
		s.collocated_from_l0 = 1;
		if (ps || bs)
		{
			//of those it keeps, how many it is made from
			int total = 0;
			for (int i = 0; i < s.rps.negative + s.rps.positive; i++) total += s.rps.used[i];
			for (int i = 0; i < s.num_lt; i++) total += s.lt_used[i];
			s.num_ref_idx[0] = p.num_ref_idx_default[0];
			s.num_ref_idx[1] = bs ? p.num_ref_idx_default[1] : 0;
			if (b.u(1))
			{
				s.num_ref_idx[0] = b.ue() + 1;
				if (bs) s.num_ref_idx[1] = b.ue() + 1;
			}
			if (s.num_ref_idx[0] > 15 || s.num_ref_idx[1] > 15 || !total) return false;
			if (p.lists_modification && total > 1)
			{
				for (int list = 0; list < (bs ? 2 : 1); list++)
				{
					if (!(s.mods[list] = b.u(1))) continue;
					for (int i = 0; i < s.num_ref_idx[list]; i++)
					{
						s.entries[list][i] = b.u(va_bits_for(total));
						if (s.entries[list][i] >= total) return false;
					}
				}
			}
			if (bs) s.mvd_l1_zero = b.u(1);
			if (p.cabac_init_present) s.cabac_init = b.u(1);
			if (s.temporal_mvp)
			{
				if (bs) s.collocated_from_l0 = b.u(1);
				int n = s.num_ref_idx[s.collocated_from_l0 ? 0 : 1];
				if (n > 1) s.collocated_ref_idx = b.ue();
				if (s.collocated_ref_idx >= n) return false;
			}
			if (((p.weighted_pred && ps) || (p.weighted_bipred && bs)) && !va_hweights(b, s, param)) return false;
			s.five_minus_max_merge = b.ue();
			if (s.five_minus_max_merge > 4) return false;
		}
		s.qp_delta = b.se();
		int qp = p.init_qp + s.qp_delta;
		if (qp < -6 * (sp.bit_depth_luma - 8) || qp > 51) return false;
		s.cb_qp_offset = s.cr_qp_offset = 0;
		if (p.slice_chroma_qp_offsets)
		{
			s.cb_qp_offset = b.se();
			s.cr_qp_offset = b.se();
			if (s.cb_qp_offset < -12 || s.cb_qp_offset > 12 || s.cr_qp_offset < -12 || s.cr_qp_offset > 12
				|| p.cb_qp_offset + s.cb_qp_offset < -12 || p.cb_qp_offset + s.cb_qp_offset > 12
				|| p.cr_qp_offset + s.cr_qp_offset < -12 || p.cr_qp_offset + s.cr_qp_offset > 12) return false;
		}
		s.deblocking_disabled = p.deblocking_disabled;
		s.beta = p.beta;
		s.tc = p.tc;
		if (p.deblocking_override && b.u(1) && !(s.deblocking_disabled = b.u(1)))
		{
			s.beta = b.se();
			s.tc = b.se();
			if (s.beta < -6 || s.beta > 6 || s.tc < -6 || s.tc > 6) return false;
		}
		s.loop_filter_across_slices = p.loop_filter_across_slices;
		if (p.loop_filter_across_slices && (s.sao_luma || s.sao_chroma || !s.deblocking_disabled))
			s.loop_filter_across_slices = b.u(1);
	}
	s.first = first;
	s.dependent = dependent;
	s.address = address;
	//where the tiles and rows of WPP start, which the hardware finds itself
	if (p.tiles || p.entropy_sync)
	{
		uint32_t n = b.ue();
		if (n > ctbs) return false;
		if (n)
		{
			uint32_t bits = b.ue() + 1;
			if (bits > 32) return false;
			for (uint32_t i = 0; i < n && !b.over(); i++) b.u(bits);
		}
	}
	if (p.header_extension)
	{
		uint32_t n = b.ue();
		if (n > 256) return false;
		for (uint32_t i = 0; i < n; i++) b.u(8);
	}
	//byte_alignment(), a 1 then 0s
	if (!b.u(1)) return false;
	while (b.at & 7) b.u(1);
	s.header_bytes = b.at / 8;
	return !b.over();
}

//////////////////////////////////////////////////////////////////
// H.265 by V4L2's stateless decoder, the Raspberry Pi 4's rpi-hevc-dec
//////////////////////////////////////////////////////////////////

// The hardware reads a picture's slices, and is told all the rest. So what
// is parsed for VA-API is used here, the parameter sets, the slices, the
// pictures kept and their lists, and only what is told differs: the kernel's
// controls, by a media request, a picture at a time and all its slices
// together. A picture comes back in columns 128 bytes wide, made rows here,
// of 8 bits NV12, of 10 as P010. A buffer of the pictures it is told of is
// known by the time it was given, so each is given one of its own.

#ifdef V4L2_CID_STATELESS_HEVC_SPS

//Y/CbCr 4:2:0 in columns 128 bytes wide, two planes, in the Pi's kernel's
//headers only. Of 10 bits, 3 samples in each 32 bits of a column, so 96 of
//them across it
static const uint32_t hv_fourcc = v4l2_fourcc('N', 'c', '1', '2');
static const uint32_t hv_fourcc_10 = v4l2_fourcc('N', 'c', '3', '0');
static const size_t hv_col = 128, hv_col_10 = 96;

struct Hv
{
	int fd = -1, media = -1;
	void *src = nullptr;
	size_t src_size = 0, src_used = 0;
	struct Cap
	{
		void *at[2];
		size_t size[2];
		uint64_t stamp;
	};
	std::vector<Cap> caps;
	uint32_t w = 0, h = 0, line = 0;
	uint64_t given = 0, stamp = 0;
	bool streaming = false, has_lists = false, ten = false;
	std::vector<v4l2_ctrl_hevc_slice_params> slices;
	v4l2_ctrl_hevc_sps sps;
	v4l2_ctrl_hevc_pps pps;
	v4l2_ctrl_hevc_decode_params decode;
	v4l2_ctrl_hevc_scaling_matrix lists;
};

static int hv_ioctl(int fd, unsigned long what, void *arg)
{
	int r;
	do r = ioctl(fd, what, arg); while (r < 0 && errno == EINTR);
	return r;
}

static bool hv_find(Hv *hv)
{
	//the first decoder that takes parsed slices and gives columns, and the
	//media device of its driver
	for (int i = 0; i < 64 && hv->fd < 0; i++)
	{
		char name[32];
		snprintf(name, sizeof(name), "/dev/video%d", i);
		int fd = open(name, O_RDWR | O_NONBLOCK | O_CLOEXEC);
		if (fd < 0) continue;
		bool in = false, out = false;
		v4l2_fmtdesc f = {};
		f.type = V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE;
		for (f.index = 0; hv_ioctl(fd, VIDIOC_ENUM_FMT, &f) == 0; f.index++) in |= f.pixelformat == V4L2_PIX_FMT_HEVC_SLICE;
		f.type = V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE;
		for (f.index = 0; hv_ioctl(fd, VIDIOC_ENUM_FMT, &f) == 0; f.index++) out |= f.pixelformat == hv_fourcc;
		v4l2_capability cap = {};
		if (!in || !out || hv_ioctl(fd, VIDIOC_QUERYCAP, &cap) < 0) { close(fd); continue; }
		for (int m = 0; m < 16 && hv->media < 0; m++)
		{
			snprintf(name, sizeof(name), "/dev/media%d", m);
			int mfd = open(name, O_RDWR | O_NONBLOCK | O_CLOEXEC);
			if (mfd < 0) continue;
			media_device_info info = {};
			if (hv_ioctl(mfd, MEDIA_IOC_DEVICE_INFO, &info) == 0
				&& !strncmp(info.driver, (const char*)cap.driver, sizeof(info.driver))) hv->media = mfd;
			else close(mfd);
		}
		if (hv->media < 0) { close(fd); continue; }
		hv->fd = fd;
	}
	return hv->fd >= 0;
}

static void hv_stop(Hv *hv)
{
	//what is had for one size of picture let go
	int type = V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE;
	if (hv->streaming)
	{
		hv_ioctl(hv->fd, VIDIOC_STREAMOFF, &type);
		type = V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE;
		hv_ioctl(hv->fd, VIDIOC_STREAMOFF, &type);
		hv->streaming = false;
	}
	for (auto &c : hv->caps) for (int i = 0; i < 2; i++) if (c.at[i] && c.at[i] != MAP_FAILED) munmap(c.at[i], c.size[i]);
	hv->caps.clear();
	if (hv->src && hv->src != MAP_FAILED) munmap(hv->src, hv->src_size);
	hv->src = nullptr;
	v4l2_requestbuffers req = {};
	req.memory = V4L2_MEMORY_MMAP;
	req.type = V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE;
	hv_ioctl(hv->fd, VIDIOC_REQBUFS, &req);
	req.type = V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE;
	hv_ioctl(hv->fd, VIDIOC_REQBUFS, &req);
	hv->w = hv->h = 0;
}

static void hv_fill_sps(v4l2_ctrl_hevc_sps &to, const Va_Hsps &sp, int id)
{
	memset(&to, 0, sizeof(to));
	to.seq_parameter_set_id = id;
	to.pic_width_in_luma_samples = sp.width;
	to.pic_height_in_luma_samples = sp.height;
	to.bit_depth_luma_minus8 = sp.bit_depth_luma - 8;
	to.bit_depth_chroma_minus8 = sp.bit_depth_chroma - 8;
	to.log2_max_pic_order_cnt_lsb_minus4 = sp.log2_max_poc_lsb - 4;
	to.sps_max_dec_pic_buffering_minus1 = sp.max_dec_pic_buffering - 1;
	to.sps_max_num_reorder_pics = sp.num_reorder;
	to.log2_min_luma_coding_block_size_minus3 = sp.log2_min_cb - 3;
	to.log2_diff_max_min_luma_coding_block_size = sp.log2_diff_cb;
	to.log2_min_luma_transform_block_size_minus2 = sp.log2_min_tb - 2;
	to.log2_diff_max_min_luma_transform_block_size = sp.log2_diff_tb;
	to.max_transform_hierarchy_depth_inter = sp.depth_inter;
	to.max_transform_hierarchy_depth_intra = sp.depth_intra;
	if (sp.pcm)
	{
		to.pcm_sample_bit_depth_luma_minus1 = sp.pcm_depth_luma - 1;
		to.pcm_sample_bit_depth_chroma_minus1 = sp.pcm_depth_chroma - 1;
		to.log2_min_pcm_luma_coding_block_size_minus3 = sp.log2_min_pcm - 3;
		to.log2_diff_max_min_pcm_luma_coding_block_size = sp.log2_diff_pcm;
	}
	to.num_short_term_ref_pic_sets = sp.num_rps;
	to.num_long_term_ref_pics_sps = sp.num_lt;
	to.chroma_format_idc = sp.chroma_format_idc;
	to.flags = (sp.scaling ? V4L2_HEVC_SPS_FLAG_SCALING_LIST_ENABLED : 0)
		| (sp.amp ? V4L2_HEVC_SPS_FLAG_AMP_ENABLED : 0)
		| (sp.sao ? V4L2_HEVC_SPS_FLAG_SAMPLE_ADAPTIVE_OFFSET : 0)
		| (sp.pcm ? V4L2_HEVC_SPS_FLAG_PCM_ENABLED : 0)
		| (sp.pcm_loop_filter_disabled ? V4L2_HEVC_SPS_FLAG_PCM_LOOP_FILTER_DISABLED : 0)
		| (sp.long_term_present ? V4L2_HEVC_SPS_FLAG_LONG_TERM_REF_PICS_PRESENT : 0)
		| (sp.temporal_mvp ? V4L2_HEVC_SPS_FLAG_SPS_TEMPORAL_MVP_ENABLED : 0)
		| (sp.strong_intra ? V4L2_HEVC_SPS_FLAG_STRONG_INTRA_SMOOTHING_ENABLED : 0);
}

static bool hv_setup(Va_Decoder *d, const Va_Hsps &sp, int sps_id)
{
	//its buffers, for this size of picture, kept while it is. The SPS is
	//told first, the driver has the format of what comes back by it
	Hv *hv = d->hv;
	int count = sp.max_dec_pic_buffering + 2;
	bool ten = sp.bit_depth_luma == 10;
	uint32_t fourcc = ten ? hv_fourcc_10 : hv_fourcc;
	if (hv->w == (uint32_t)sp.width && hv->h == (uint32_t)sp.height && hv->ten == ten && (int)hv->caps.size() >= count) return true;
	hv_stop(hv);
	v4l2_format fmt = {};
	fmt.type = V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE;
	fmt.fmt.pix_mp.pixelformat = V4L2_PIX_FMT_HEVC_SLICE;
	fmt.fmt.pix_mp.width = sp.width;
	fmt.fmt.pix_mp.height = sp.height;
	fmt.fmt.pix_mp.num_planes = 1;
	fmt.fmt.pix_mp.plane_fmt[0].sizeimage = std::max(1u << 21, (uint32_t)sp.width * sp.height);
	if (hv_ioctl(hv->fd, VIDIOC_S_FMT, &fmt) < 0) return false;
	v4l2_requestbuffers req = {};
	req.count = 1;
	req.type = V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE;
	req.memory = V4L2_MEMORY_MMAP;
	if (hv_ioctl(hv->fd, VIDIOC_REQBUFS, &req) < 0 || req.count < 1) return false;
	v4l2_plane planes[2];
	v4l2_buffer buf = {};
	memset(planes, 0, sizeof(planes));
	buf.type = req.type;
	buf.memory = V4L2_MEMORY_MMAP;
	buf.m.planes = planes;
	buf.length = 1;
	if (hv_ioctl(hv->fd, VIDIOC_QUERYBUF, &buf) < 0) return false;
	hv->src_size = planes[0].length;
	hv->src = mmap(nullptr, hv->src_size, PROT_READ | PROT_WRITE, MAP_SHARED, hv->fd, planes[0].m.mem_offset);
	if (hv->src == MAP_FAILED) { hv->src = nullptr; return false; }
	hv_fill_sps(hv->sps, sp, sps_id);
	v4l2_ext_control c = {};
	c.id = V4L2_CID_STATELESS_HEVC_SPS;
	c.size = sizeof(hv->sps);
	c.ptr = &hv->sps;
	v4l2_ext_controls ctrls = {};
	ctrls.count = 1;
	ctrls.controls = &c;
	if (hv_ioctl(hv->fd, VIDIOC_S_EXT_CTRLS, &ctrls) < 0) return false;
	memset(&fmt, 0, sizeof(fmt));
	fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE;
	fmt.fmt.pix_mp.pixelformat = fourcc;
	fmt.fmt.pix_mp.width = sp.width;
	fmt.fmt.pix_mp.height = sp.height;
	fmt.fmt.pix_mp.num_planes = 2;
	if (hv_ioctl(hv->fd, VIDIOC_S_FMT, &fmt) < 0 || fmt.fmt.pix_mp.pixelformat != fourcc
		|| fmt.fmt.pix_mp.num_planes != 2) return false;
	hv->line = fmt.fmt.pix_mp.plane_fmt[0].bytesperline;
	if (!hv->line) return false;
	memset(&req, 0, sizeof(req));
	req.count = count;
	req.type = V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE;
	req.memory = V4L2_MEMORY_MMAP;
	if (hv_ioctl(hv->fd, VIDIOC_REQBUFS, &req) < 0 || (int)req.count < count) return false;
	hv->caps.resize(req.count);
	for (uint32_t i = 0; i < req.count; i++)
	{
		auto &cap = hv->caps[i];
		cap.at[0] = cap.at[1] = nullptr;
		cap.stamp = 0;
		memset(&buf, 0, sizeof(buf));
		memset(planes, 0, sizeof(planes));
		buf.type = req.type;
		buf.memory = V4L2_MEMORY_MMAP;
		buf.index = i;
		buf.m.planes = planes;
		buf.length = 2;
		if (hv_ioctl(hv->fd, VIDIOC_QUERYBUF, &buf) < 0) return false;
		for (int p = 0; p < 2; p++)
		{
			cap.size[p] = planes[p].length;
			cap.at[p] = mmap(nullptr, cap.size[p], PROT_READ | PROT_WRITE, MAP_SHARED, hv->fd, planes[p].m.mem_offset);
			if (cap.at[p] == MAP_FAILED) { cap.at[p] = nullptr; return false; }
		}
	}
	int type = V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE;
	if (hv_ioctl(hv->fd, VIDIOC_STREAMON, &type) < 0) return false;
	type = V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE;
	if (hv_ioctl(hv->fd, VIDIOC_STREAMON, &type) < 0) return false;
	hv->streaming = true;
	hv->ten = ten;
	hv->w = sp.width;
	hv->h = sp.height;
	d->surfaces.assign(hv->caps.size(), VA_INVALID_ID);
	return true;
}

static int hv_begin(Va_Decoder *d, const Va_Hslice &s, const Va_Hsps &sp, const Va_Hpps &p, bool irap, bool idr,
	const int *columns, const int *rows)
{
	//a new picture: what is told of it, its slices to come
	Hv *hv = d->hv;
	if (d->dpb.size() > V4L2_HEVC_DPB_ENTRIES_NUM_MAX) return -1;
	hv_fill_sps(hv->sps, sp, p.sps_id);
	auto &q = hv->pps;
	memset(&q, 0, sizeof(q));
	q.pic_parameter_set_id = s.pps_id;
	q.num_extra_slice_header_bits = p.extra_bits;
	q.num_ref_idx_l0_default_active_minus1 = p.num_ref_idx_default[0] - 1;
	q.num_ref_idx_l1_default_active_minus1 = p.num_ref_idx_default[1] - 1;
	q.init_qp_minus26 = p.init_qp - 26;
	q.diff_cu_qp_delta_depth = p.diff_cu_qp_delta_depth;
	q.pps_cb_qp_offset = p.cb_qp_offset;
	q.pps_cr_qp_offset = p.cr_qp_offset;
	if (p.tiles)
	{
		q.num_tile_columns_minus1 = p.columns - 1;
		q.num_tile_rows_minus1 = p.rows - 1;
		for (int i = 0; i < p.columns; i++) q.column_width_minus1[i] = columns[i] - 1;
		for (int i = 0; i < p.rows; i++) q.row_height_minus1[i] = rows[i] - 1;
	}
	q.pps_beta_offset_div2 = p.beta;
	q.pps_tc_offset_div2 = p.tc;
	q.log2_parallel_merge_level_minus2 = p.log2_parallel_merge_level - 2;
	q.flags = (p.dependent_slices ? V4L2_HEVC_PPS_FLAG_DEPENDENT_SLICE_SEGMENT_ENABLED : 0)
		| (p.output_flag_present ? V4L2_HEVC_PPS_FLAG_OUTPUT_FLAG_PRESENT : 0)
		| (p.sign_hiding ? V4L2_HEVC_PPS_FLAG_SIGN_DATA_HIDING_ENABLED : 0)
		| (p.cabac_init_present ? V4L2_HEVC_PPS_FLAG_CABAC_INIT_PRESENT : 0)
		| (p.constrained_intra ? V4L2_HEVC_PPS_FLAG_CONSTRAINED_INTRA_PRED : 0)
		| (p.transform_skip ? V4L2_HEVC_PPS_FLAG_TRANSFORM_SKIP_ENABLED : 0)
		| (p.cu_qp_delta ? V4L2_HEVC_PPS_FLAG_CU_QP_DELTA_ENABLED : 0)
		| (p.slice_chroma_qp_offsets ? V4L2_HEVC_PPS_FLAG_PPS_SLICE_CHROMA_QP_OFFSETS_PRESENT : 0)
		| (p.weighted_pred ? V4L2_HEVC_PPS_FLAG_WEIGHTED_PRED : 0)
		| (p.weighted_bipred ? V4L2_HEVC_PPS_FLAG_WEIGHTED_BIPRED : 0)
		| (p.transquant_bypass ? V4L2_HEVC_PPS_FLAG_TRANSQUANT_BYPASS_ENABLED : 0)
		| (p.tiles ? V4L2_HEVC_PPS_FLAG_TILES_ENABLED : 0)
		| (p.entropy_sync ? V4L2_HEVC_PPS_FLAG_ENTROPY_CODING_SYNC_ENABLED : 0)
		| (p.loop_filter_across_tiles ? V4L2_HEVC_PPS_FLAG_LOOP_FILTER_ACROSS_TILES_ENABLED : 0)
		| (p.loop_filter_across_slices ? V4L2_HEVC_PPS_FLAG_PPS_LOOP_FILTER_ACROSS_SLICES_ENABLED : 0)
		| (p.deblocking_override ? V4L2_HEVC_PPS_FLAG_DEBLOCKING_FILTER_OVERRIDE_ENABLED : 0)
		| (p.deblocking_disabled ? V4L2_HEVC_PPS_FLAG_PPS_DISABLE_DEBLOCKING_FILTER : 0)
		| (p.lists_modification ? V4L2_HEVC_PPS_FLAG_LISTS_MODIFICATION_PRESENT : 0)
		| (p.header_extension ? V4L2_HEVC_PPS_FLAG_SLICE_SEGMENT_HEADER_EXTENSION_PRESENT : 0)
		| (p.uniform ? V4L2_HEVC_PPS_FLAG_UNIFORM_SPACING : 0);
	auto &k = hv->decode;
	memset(&k, 0, sizeof(k));
	k.pic_order_cnt_val = d->cur_poc;
	k.short_term_ref_pic_set_size = s.rps_bits;
	k.num_active_dpb_entries = d->dpb.size();
	k.num_poc_st_curr_before = d->nbefore;
	k.num_poc_st_curr_after = d->nafter;
	k.num_poc_lt_curr = d->nlt;
	for (int i = 0; i < d->nbefore; i++) k.poc_st_curr_before[i] = d->hbefore[i];
	for (int i = 0; i < d->nafter; i++) k.poc_st_curr_after[i] = d->hafter[i];
	for (int i = 0; i < d->nlt; i++) k.poc_lt_curr[i] = d->hlt[i];
	for (size_t i = 0; i < d->dpb.size(); i++)
	{
		//one of a size of picture before this one is no use
		if (d->dpb[i].surface >= (int)hv->caps.size()) return -1;
		k.dpb[i].timestamp = hv->caps[d->dpb[i].surface].stamp;
		k.dpb[i].flags = d->dpb[i].long_ref ? V4L2_HEVC_DPB_ENTRY_LONG_TERM_REFERENCE : 0;
		k.dpb[i].pic_order_cnt_val = d->dpb[i].top;
	}
	k.flags = (irap ? V4L2_HEVC_DECODE_PARAM_FLAG_IRAP_PIC : 0) | (idr ? V4L2_HEVC_DECODE_PARAM_FLAG_IDR_PIC : 0);
	hv->has_lists = sp.scaling;
	if (sp.scaling)
	{
		const Va_Hlists &l = p.scaling ? p.lists : sp.lists;
		auto &m = hv->lists;
		for (int i = 0; i < 6; i++)
		{
			va_hraster(m.scaling_list_4x4[i], l.lists[0][i], 4);
			va_hraster(m.scaling_list_8x8[i], l.lists[1][i], 8);
			va_hraster(m.scaling_list_16x16[i], l.lists[2][i], 8);
			m.scaling_list_dc_coef_16x16[i] = l.dc[2][i];
		}
		for (int i = 0; i < 2; i++)
		{
			va_hraster(m.scaling_list_32x32[i], l.lists[3][i * 3], 8);
			m.scaling_list_dc_coef_32x32[i] = l.dc[3][i * 3];
		}
	}
	hv->slices.clear();
	hv->src_used = 0;
	//known by this from now on, as what is made from it knows it
	hv->given++;
	hv->stamp = hv->given * 1000;
	hv->caps[d->cur.surface].stamp = hv->stamp;
	d->started = true;
	return 1;
}

static bool hv_slice(Va_Decoder *d, const Va_Hslice &s, const VASliceParameterBufferHEVC &param,
	const uint8_t *nal, size_t n, int type, int tid)
{
	//a slice, its data after those before it, its parameters for the end
	Hv *hv = d->hv;
	if (hv->src_used + n > hv->src_size || hv->slices.size() >= 600) return false;
	memcpy((uint8_t*)hv->src + hv->src_used, nal, n);
	//the header's bytes are of it with the emulation prevention bytes out
	size_t at = 0, rbsp = 0;
	int zeros = 0;
	while (rbsp < s.header_bytes && at < n)
	{
		if (zeros >= 2 && nal[at] == 3) { zeros = 0; at++; continue; }
		zeros = nal[at] ? 0 : zeros + 1;
		at++;
		rbsp++;
	}
	v4l2_ctrl_hevc_slice_params q;
	memset(&q, 0, sizeof(q));
	//its bits are of what follows the offset, its header not among them
	q.bit_size = (n - at) * 8;
	q.data_byte_offset = hv->src_used + at;
	q.nal_unit_type = type;
	q.nuh_temporal_id_plus1 = tid + 1;
	q.slice_type = s.type;
	q.slice_pic_order_cnt = d->cur_poc;
	q.num_ref_idx_l0_active_minus1 = s.num_ref_idx[0] ? s.num_ref_idx[0] - 1 : 0;
	q.num_ref_idx_l1_active_minus1 = s.num_ref_idx[1] ? s.num_ref_idx[1] - 1 : 0;
	q.collocated_ref_idx = s.temporal_mvp ? s.collocated_ref_idx : 0;
	q.five_minus_max_num_merge_cand = s.five_minus_max_merge;
	q.slice_qp_delta = s.qp_delta;
	q.slice_cb_qp_offset = s.cb_qp_offset;
	q.slice_cr_qp_offset = s.cr_qp_offset;
	q.slice_beta_offset_div2 = s.beta;
	q.slice_tc_offset_div2 = s.tc;
	q.slice_segment_addr = s.address;
	for (int i = 0; i < 15; i++)
	{
		q.ref_idx_l0[i] = param.RefPicList[0][i];
		q.ref_idx_l1[i] = param.RefPicList[1][i];
	}
	q.short_term_ref_pic_set_size = s.rps_bits;
	auto &w = q.pred_weight_table;
	w.luma_log2_weight_denom = param.luma_log2_weight_denom;
	w.delta_chroma_log2_weight_denom = param.delta_chroma_log2_weight_denom;
	for (int i = 0; i < 15; i++)
	{
		w.delta_luma_weight_l0[i] = param.delta_luma_weight_l0[i];
		w.luma_offset_l0[i] = param.luma_offset_l0[i];
		w.delta_luma_weight_l1[i] = param.delta_luma_weight_l1[i];
		w.luma_offset_l1[i] = param.luma_offset_l1[i];
		for (int j = 0; j < 2; j++)
		{
			w.delta_chroma_weight_l0[i][j] = param.delta_chroma_weight_l0[i][j];
			w.chroma_offset_l0[i][j] = param.ChromaOffsetL0[i][j];
			w.delta_chroma_weight_l1[i][j] = param.delta_chroma_weight_l1[i][j];
			w.chroma_offset_l1[i][j] = param.ChromaOffsetL1[i][j];
		}
	}
	q.flags = (s.sao_luma ? V4L2_HEVC_SLICE_PARAMS_FLAG_SLICE_SAO_LUMA : 0)
		| (s.sao_chroma ? V4L2_HEVC_SLICE_PARAMS_FLAG_SLICE_SAO_CHROMA : 0)
		| (s.temporal_mvp ? V4L2_HEVC_SLICE_PARAMS_FLAG_SLICE_TEMPORAL_MVP_ENABLED : 0)
		| (s.mvd_l1_zero ? V4L2_HEVC_SLICE_PARAMS_FLAG_MVD_L1_ZERO : 0)
		| (s.cabac_init ? V4L2_HEVC_SLICE_PARAMS_FLAG_CABAC_INIT : 0)
		| (s.collocated_from_l0 ? V4L2_HEVC_SLICE_PARAMS_FLAG_COLLOCATED_FROM_L0 : 0)
		| (s.deblocking_disabled ? V4L2_HEVC_SLICE_PARAMS_FLAG_SLICE_DEBLOCKING_FILTER_DISABLED : 0)
		| (s.loop_filter_across_slices ? V4L2_HEVC_SLICE_PARAMS_FLAG_SLICE_LOOP_FILTER_ACROSS_SLICES_ENABLED : 0)
		| (s.dependent ? V4L2_HEVC_SLICE_PARAMS_FLAG_DEPENDENT_SLICE_SEGMENT : 0);
	hv->src_used += n;
	hv->slices.push_back(q);
	return true;
}

static bool hv_end(Va_Decoder *d)
{
	//the picture given the hardware, all of it by one request, and waited
	//for. No slices of it, nothing to give
	Hv *hv = d->hv;
	if (hv->slices.empty()) return false;
	int req = -1;
	if (hv_ioctl(hv->media, MEDIA_IOC_REQUEST_ALLOC, &req) < 0) return false;
	bool ok = false;
	v4l2_ext_control c[5];
	memset(c, 0, sizeof(c));
	unsigned n = 0;
	c[n].id = V4L2_CID_STATELESS_HEVC_SPS, c[n].size = sizeof(hv->sps), c[n++].ptr = &hv->sps;
	c[n].id = V4L2_CID_STATELESS_HEVC_PPS, c[n].size = sizeof(hv->pps), c[n++].ptr = &hv->pps;
	c[n].id = V4L2_CID_STATELESS_HEVC_DECODE_PARAMS, c[n].size = sizeof(hv->decode), c[n++].ptr = &hv->decode;
	c[n].id = V4L2_CID_STATELESS_HEVC_SLICE_PARAMS;
	c[n].size = hv->slices.size() * sizeof(hv->slices[0]);
	c[n++].ptr = hv->slices.data();
	if (hv->has_lists) c[n].id = V4L2_CID_STATELESS_HEVC_SCALING_MATRIX, c[n].size = sizeof(hv->lists), c[n++].ptr = &hv->lists;
	v4l2_ext_controls ctrls = {};
	ctrls.which = V4L2_CTRL_WHICH_REQUEST_VAL;
	ctrls.request_fd = req;
	ctrls.count = n;
	ctrls.controls = c;
	v4l2_plane planes[2];
	v4l2_buffer cap = {}, src = {};
	memset(planes, 0, sizeof(planes));
	cap.type = V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE;
	cap.memory = V4L2_MEMORY_MMAP;
	cap.index = d->cur.surface;
	cap.m.planes = planes;
	cap.length = 2;
	v4l2_plane in = {};
	in.bytesused = hv->src_used;
	src.type = V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE;
	src.memory = V4L2_MEMORY_MMAP;
	src.index = 0;
	src.m.planes = &in;
	src.length = 1;
	src.flags = V4L2_BUF_FLAG_REQUEST_FD;
	src.request_fd = req;
	src.timestamp.tv_sec = hv->stamp / 1000000000;
	src.timestamp.tv_usec = (hv->stamp % 1000000000) / 1000;
	if (hv_ioctl(hv->fd, VIDIOC_S_EXT_CTRLS, &ctrls) == 0
		&& hv_ioctl(hv->fd, VIDIOC_QBUF, &cap) == 0
		&& hv_ioctl(hv->fd, VIDIOC_QBUF, &src) == 0
		&& hv_ioctl(req, MEDIA_REQUEST_IOC_QUEUE, nullptr) == 0)
	{
		//a stuck decoder is not waited on for ever
		pollfd wait = {req, POLLPRI, 0};
		ok = poll(&wait, 1, 2000) > 0;
		memset(&src, 0, sizeof(src));
		memset(&in, 0, sizeof(in));
		src.type = V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE;
		src.memory = V4L2_MEMORY_MMAP;
		src.m.planes = &in;
		src.length = 1;
		ok = hv_ioctl(hv->fd, VIDIOC_DQBUF, &src) == 0 && ok;
		memset(&cap, 0, sizeof(cap));
		memset(planes, 0, sizeof(planes));
		cap.type = V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE;
		cap.memory = V4L2_MEMORY_MMAP;
		cap.m.planes = planes;
		cap.length = 2;
		ok = hv_ioctl(hv->fd, VIDIOC_DQBUF, &cap) == 0 && !(cap.flags & V4L2_BUF_FLAG_ERROR) && ok;
	}
	close(req);
	//what is still queued of a picture that failed, or the next one fails
	//too. The buffers stay, as do those it is made from
	if (!ok)
	{
		int type = V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE;
		hv_ioctl(hv->fd, VIDIOC_STREAMOFF, &type);
		hv_ioctl(hv->fd, VIDIOC_STREAMON, &type);
		type = V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE;
		hv_ioctl(hv->fd, VIDIOC_STREAMOFF, &type);
		hv_ioctl(hv->fd, VIDIOC_STREAMON, &type);
	}
	return ok;
}

static bool hv_read(Va_Decoder *d, int surface, uint32_t x, uint32_t y, uint32_t w, uint32_t h, int64_t shown)
{
	//the picture of a buffer held, from columns to rows, as NV12, or P010 of
	//10 bits. Each plane is its columns one after another, each as high as
	//its plane is long for the width it has
	Hv *hv = d->hv;
	auto &cap = hv->caps[surface];
	//what is asked for is in the buffers, a crop that says otherwise is broken
	if (!w || !h || x + w > hv->w || y + h > hv->h) return false;
	Va_Held pic;
	if (!d->spare.empty())
	{
		pic.data.swap(d->spare.back());
		d->spare.pop_back();
	}
	uint32_t cw = (w + 1) / 2, ch = (h + 1) / 2, size = hv->ten ? 2 : 1;
	pic.shown = shown;
	pic.width = w;
	pic.height = h;
	pic.sx = pic.sy = 1;
	pic.step = 2;
	pic.bytes = size;
	pic.y_pitch = w * size;
	pic.c_pitch = cw * 2 * size;
	pic.y_at = 0;
	pic.u_at = (size_t)pic.y_pitch * h;
	pic.v_at = pic.u_at + size;
	pic.data.resize(pic.u_at + (size_t)pic.c_pitch * ch);
	for (int p = 0; p < 2; p++)
	{
		uint32_t rows = p ? ch : h, top = p ? y / 2 : y, left = p ? x & ~1u : x, across = p ? cw * 2 : w;
		size_t high = cap.size[p] / hv->line, per = hv->ten ? hv_col_10 : hv_col;
		const uint8_t *from = (const uint8_t*)cap.at[p];
		uint8_t *to = pic.data.data() + (p ? pic.u_at : 0);
		size_t pitch = p ? pic.c_pitch : pic.y_pitch;
		if (top + rows > high || (left + across + per - 1) / per * hv_col * high > cap.size[p]) return false;
		for (uint32_t r = 0; r < rows; r++)
		{
			uint32_t at = left, done = 0;
			uint8_t *row = to + r * pitch;
			while (done < across)
			{
				size_t col = at / per, in = at % per, take = std::min<size_t>(per - in, across - done);
				const uint8_t *src = from + col * hv_col * high + (size_t)(r + top) * hv_col;
				if (!hv->ten) memcpy(row + done, src + in, take);
				else
				{
					//all of it read the once, its memory is not cached
					uint32_t words[hv_col / 4];
					memcpy(words, src, hv_col);
					auto out = (uint16_t*)row + done;
					for (size_t i = 0; i < take; i++)
					{
						size_t n = in + i;
						out[i] = ((words[n / 3] >> (n % 3 * 10)) & 0x3ff) << 6;
					}
				}
				done += take;
				at += take;
			}
		}
	}
	d->held.push_back(std::move(pic));
	return true;
}

static void hv_close(Va_Decoder *d)
{
	Hv *hv = d->hv;
	hv_stop(hv);
	close(hv->fd);
	close(hv->media);
	delete hv;
	d->hv = nullptr;
}

#else

struct Hv {};
static bool hv_find(Hv *hv) { return false; }
static bool hv_setup(Va_Decoder *d, const Va_Hsps &sp, int sps_id) { return false; }
static int hv_begin(Va_Decoder *d, const Va_Hslice &s, const Va_Hsps &sp, const Va_Hpps &p, bool irap, bool idr,
	const int *columns, const int *rows) { return -1; }
static bool hv_slice(Va_Decoder *d, const Va_Hslice &s, const VASliceParameterBufferHEVC &param,
	const uint8_t *nal, size_t n, int type, int tid) { return false; }
static bool hv_end(Va_Decoder *d) { return false; }
static bool hv_read(Va_Decoder *d, int surface, uint32_t x, uint32_t y, uint32_t w, uint32_t h, int64_t shown) { return false; }
static void hv_close(Va_Decoder *d) { delete d->hv; d->hv = nullptr; }

#endif

static int va_hstart(Va_Decoder *d, const Va_Hslice &s, int type, int tid)
{
	//a new picture: 1 if it is started, 0 if it is not to be decoded, -1
	//if it can not be. Its order, those it keeps of the pictures kept, its
	//surface, and what VA-API is told of the picture and its scaling
	const Va_Hpps &p = d->hpps[s.pps_id];
	const Va_Hsps &sp = d->hsps[p.sps_id];
	bool irap = type >= 16 && type <= 23, idr = type == 19 || type == 20;
	//the first picture, or the first after an end of the stream, of IDR
	//BLA or CRA, and the pictures of RASL after it, made from those before
	//it, are not decoded: as after a seek
	if (irap)
	{
		d->no_rasl = idr || type <= 18 || d->hfirst;
		d->hfirst = false;
	}
	else if (d->hfirst) return 0;
	if ((type == 8 || type == 9) && d->no_rasl) return 0;
	//of the PPS, what is of its SPS
	int ctb = sp.log2_min_cb + sp.log2_diff_cb;
	int across = (sp.width + (1 << ctb) - 1) >> ctb, down = (sp.height + (1 << ctb) - 1) >> ctb;
	int columns[20], rows[22], left = across, up = down;
	for (int i = 0; i < p.columns; i++)
		left -= columns[i] = p.uniform ? ((i + 1) * across) / p.columns - (i * across) / p.columns
			: i < p.columns - 1 ? p.column_width[i] : left;
	for (int i = 0; i < p.rows; i++)
		up -= rows[i] = p.uniform ? ((i + 1) * down) / p.rows - (i * down) / p.rows
			: i < p.rows - 1 ? p.row_height[i] : up;
	bool tiles_ok = left == 0 && up == 0;
	for (int i = 0; i < p.columns; i++) tiles_ok &= columns[i] > 0;
	for (int i = 0; i < p.rows; i++) tiles_ok &= rows[i] > 0;
	if (!tiles_ok || p.init_qp < -6 * (sp.bit_depth_luma - 8) || p.diff_cu_qp_delta_depth > sp.log2_diff_cb
		|| p.log2_parallel_merge_level > ctb) return -1;
	VADisplay dpy = d->hv ? nullptr : va_display();
	if (d->hv)
	{
		if (!hv_setup(d, sp, p.sps_id)) return -1;
	}
	else
	{
		VAProfile profile = sp.bit_depth_luma == 10 ? VAProfileHEVCMain10 : VAProfileHEVCMain;
		if (profile != d->asked) d->chosen = va_has(dpy, d->asked = profile) ? profile : VAProfileNone;
		if (d->chosen == VAProfileNone) return -1;
		if (!va_setup(d, profile, sp.bit_depth_luma == 10 ? VA_RT_FORMAT_YUV420_10 : VA_RT_FORMAT_YUV420,
			sp.width, sp.height, sp.max_dec_pic_buffering + 2)) return -1;
	}
	//its order, by the last of the lowest sub-layer that is to be made from
	int max_lsb = 1 << sp.log2_max_poc_lsb, msb = 0;
	if (!irap || !d->no_rasl)
	{
		int prev_lsb = d->prev_tid0_poc & (max_lsb - 1), prev_msb = d->prev_tid0_poc - prev_lsb;
		msb = s.poc_lsb < prev_lsb && prev_lsb - s.poc_lsb >= max_lsb / 2 ? prev_msb + max_lsb
			: s.poc_lsb > prev_lsb && s.poc_lsb - prev_lsb > max_lsb / 2 ? prev_msb - max_lsb : prev_msb;
	}
	d->cur_poc = msb + s.poc_lsb;
	if (tid == 0 && !(type >= 6 && type <= 9) && !(type <= 14 && type % 2 == 0)) d->prev_tid0_poc = d->cur_poc;
	//those it keeps, of long term first, of those before it, and short
	//term; the rest let go. Those it is made from by their index, -1 if
	//not there, and then it can not be decoded
	if (irap && d->no_rasl) d->dpb.clear();
	std::vector<int> kept(d->dpb.size(), 0);
	int before[16], after[16], lt[16], nb = 0, na = 0, nl = 0;
	bool missing = false;
	for (int i = 0; i < s.num_lt; i++)
	{
		int poc = s.lt_lsb[i];
		if (s.lt_msb[i]) poc += d->cur_poc - s.lt_cycle[i] * max_lsb - (d->cur_poc & (max_lsb - 1));
		int found = -1;
		for (int j = 0; j < (int)d->dpb.size() && found < 0; j++)
			if (s.lt_msb[i] ? d->dpb[j].top == poc : (d->dpb[j].top & (max_lsb - 1)) == poc) found = j;
		if (found >= 0)
		{
			kept[found] = 1;
			d->dpb[found].long_ref = true;
			d->dpb[found].short_ref = false;
		}
		if (s.lt_used[i]) found < 0 ? (void)(missing = true) : (void)(lt[nl++] = found);
	}
	for (int i = 0; i < s.rps.negative + s.rps.positive; i++)
	{
		int poc = d->cur_poc + s.rps.delta[i], found = -1;
		for (int j = 0; j < (int)d->dpb.size() && found < 0; j++)
			if (d->dpb[j].short_ref && d->dpb[j].top == poc) found = j;
		if (found >= 0) kept[found] = 1;
		if (!s.rps.used[i]) continue;
		if (found < 0) missing = true;
		else if (i < s.rps.negative) before[nb++] = found;
		else after[na++] = found;
	}
	//those kept, their indexes as they are after the rest go
	std::vector<Va_Ref> dpb;
	for (int j = 0; j < (int)d->dpb.size(); j++)
	{
		if (!kept[j]) continue;
		kept[j] = dpb.size();
		dpb.push_back(d->dpb[j]);
	}
	d->dpb.swap(dpb);
	d->nbefore = nb;
	d->nafter = na;
	d->nlt = nl;
	for (int i = 0; i < nb; i++) d->hbefore[i] = kept[before[i]];
	for (int i = 0; i < na; i++) d->hafter[i] = kept[after[i]];
	for (int i = 0; i < nl; i++) d->hlt[i] = kept[lt[i]];
	if (missing || d->dpb.size() > 15) return -1;
	d->cur.surface = -1;
	for (int i = 0; i < (int)d->surfaces.size() && d->cur.surface < 0; i++)
	{
		bool used = false;
		for (auto &r : d->dpb) used |= r.surface == i;
		if (!used) d->cur.surface = i;
	}
	if (d->cur.surface < 0) return -1;
	d->hpic_sps = sp;
	d->hpic_pps = s.pps_id;
	d->full = sp.full;
	d->matrix = sp.matrix;
	d->hout = s.output;
	d->primaries = sp.primaries;
	d->transfer = sp.transfer;
	if (d->hv) return hv_begin(d, s, sp, p, irap, idr, columns, rows);
	auto pic = std::make_unique<VAPictureParameterBufferHEVC>();
	memset(pic.get(), 0, sizeof(*pic));
	pic->CurrPic.picture_id = d->surfaces[d->cur.surface];
	pic->CurrPic.pic_order_cnt = d->cur_poc;
	for (int i = 0; i < 15; i++)
	{
		auto &f = pic->ReferenceFrames[i];
		if (i >= (int)d->dpb.size())
		{
			f.picture_id = VA_INVALID_SURFACE;
			f.flags = VA_PICTURE_HEVC_INVALID;
			continue;
		}
		f.picture_id = d->surfaces[d->dpb[i].surface];
		f.pic_order_cnt = d->dpb[i].top;
		f.flags = d->dpb[i].long_ref ? VA_PICTURE_HEVC_LONG_TERM_REFERENCE : 0;
		for (int j = 0; j < nb; j++) if (d->hbefore[j] == i) f.flags |= VA_PICTURE_HEVC_RPS_ST_CURR_BEFORE;
		for (int j = 0; j < na; j++) if (d->hafter[j] == i) f.flags |= VA_PICTURE_HEVC_RPS_ST_CURR_AFTER;
		for (int j = 0; j < nl; j++) if (d->hlt[j] == i) f.flags |= VA_PICTURE_HEVC_RPS_LT_CURR;
	}
	pic->pic_width_in_luma_samples = sp.width;
	pic->pic_height_in_luma_samples = sp.height;
	auto &f = pic->pic_fields.bits;
	f.chroma_format_idc = sp.chroma_format_idc;
	f.pcm_enabled_flag = sp.pcm;
	f.scaling_list_enabled_flag = sp.scaling;
	f.transform_skip_enabled_flag = p.transform_skip;
	f.amp_enabled_flag = sp.amp;
	f.strong_intra_smoothing_enabled_flag = sp.strong_intra;
	f.sign_data_hiding_enabled_flag = p.sign_hiding;
	f.constrained_intra_pred_flag = p.constrained_intra;
	f.cu_qp_delta_enabled_flag = p.cu_qp_delta;
	f.weighted_pred_flag = p.weighted_pred;
	f.weighted_bipred_flag = p.weighted_bipred;
	f.transquant_bypass_enabled_flag = p.transquant_bypass;
	f.tiles_enabled_flag = p.tiles;
	f.entropy_coding_sync_enabled_flag = p.entropy_sync;
	f.pps_loop_filter_across_slices_enabled_flag = p.loop_filter_across_slices;
	f.loop_filter_across_tiles_enabled_flag = p.loop_filter_across_tiles;
	f.pcm_loop_filter_disabled_flag = sp.pcm_loop_filter_disabled;
	pic->sps_max_dec_pic_buffering_minus1 = sp.max_dec_pic_buffering - 1;
	pic->bit_depth_luma_minus8 = sp.bit_depth_luma - 8;
	pic->bit_depth_chroma_minus8 = sp.bit_depth_chroma - 8;
	pic->pcm_sample_bit_depth_luma_minus1 = sp.pcm ? sp.pcm_depth_luma - 1 : 0;
	pic->pcm_sample_bit_depth_chroma_minus1 = sp.pcm ? sp.pcm_depth_chroma - 1 : 0;
	pic->log2_min_luma_coding_block_size_minus3 = sp.log2_min_cb - 3;
	pic->log2_diff_max_min_luma_coding_block_size = sp.log2_diff_cb;
	pic->log2_min_transform_block_size_minus2 = sp.log2_min_tb - 2;
	pic->log2_diff_max_min_transform_block_size = sp.log2_diff_tb;
	pic->log2_min_pcm_luma_coding_block_size_minus3 = sp.pcm ? sp.log2_min_pcm - 3 : 0;
	pic->log2_diff_max_min_pcm_luma_coding_block_size = sp.log2_diff_pcm;
	pic->max_transform_hierarchy_depth_intra = sp.depth_intra;
	pic->max_transform_hierarchy_depth_inter = sp.depth_inter;
	pic->init_qp_minus26 = p.init_qp - 26;
	pic->diff_cu_qp_delta_depth = p.diff_cu_qp_delta_depth;
	pic->pps_cb_qp_offset = p.cb_qp_offset;
	pic->pps_cr_qp_offset = p.cr_qp_offset;
	pic->log2_parallel_merge_level_minus2 = p.log2_parallel_merge_level - 2;
	if (p.tiles)
	{
		pic->num_tile_columns_minus1 = p.columns - 1;
		pic->num_tile_rows_minus1 = p.rows - 1;
		for (int i = 0; i < p.columns; i++) pic->column_width_minus1[i] = columns[i] - 1;
		for (int i = 0; i < p.rows; i++) pic->row_height_minus1[i] = rows[i] - 1;
	}
	auto &g = pic->slice_parsing_fields.bits;
	g.lists_modification_present_flag = p.lists_modification;
	g.long_term_ref_pics_present_flag = sp.long_term_present;
	g.sps_temporal_mvp_enabled_flag = sp.temporal_mvp;
	g.cabac_init_present_flag = p.cabac_init_present;
	g.output_flag_present_flag = p.output_flag_present;
	g.dependent_slice_segments_enabled_flag = p.dependent_slices;
	g.pps_slice_chroma_qp_offsets_present_flag = p.slice_chroma_qp_offsets;
	g.sample_adaptive_offset_enabled_flag = sp.sao;
	g.deblocking_filter_override_enabled_flag = p.deblocking_override;
	g.pps_disable_deblocking_filter_flag = p.deblocking_disabled;
	g.slice_segment_header_extension_present_flag = p.header_extension;
	g.RapPicFlag = irap;
	g.IdrPicFlag = idr;
	g.IntraPicFlag = irap;
	pic->log2_max_pic_order_cnt_lsb_minus4 = sp.log2_max_poc_lsb - 4;
	pic->num_short_term_ref_pic_sets = sp.num_rps;
	pic->num_long_term_ref_pic_sps = sp.num_lt;
	pic->num_ref_idx_l0_default_active_minus1 = p.num_ref_idx_default[0] - 1;
	pic->num_ref_idx_l1_default_active_minus1 = p.num_ref_idx_default[1] - 1;
	pic->pps_beta_offset_div2 = p.beta;
	pic->pps_tc_offset_div2 = p.tc;
	pic->num_extra_slice_header_bits = p.extra_bits;
	pic->st_rps_bits = s.rps_bits;
	//the lists, of 32x32 those of luma, intra and inter
	auto iq = std::make_unique<VAIQMatrixBufferHEVC>();
	memset(iq.get(), 0, sizeof(*iq));
	const Va_Hlists &l = p.scaling ? p.lists : sp.lists;
	for (int i = 0; i < 6; i++)
	{
		va_hraster(iq->ScalingList4x4[i], l.lists[0][i], 4);
		va_hraster(iq->ScalingList8x8[i], l.lists[1][i], 8);
		va_hraster(iq->ScalingList16x16[i], l.lists[2][i], 8);
		iq->ScalingListDC16x16[i] = l.dc[2][i];
	}
	for (int i = 0; i < 2; i++)
	{
		va_hraster(iq->ScalingList32x32[i], l.lists[3][i * 3], 8);
		iq->ScalingListDC32x32[i] = l.dc[3][i * 3];
	}
	if (va::vaBeginPicture(dpy, d->context, d->surfaces[d->cur.surface]) != VA_STATUS_SUCCESS) return -1;
	d->started = true;
	d->hpending = false;
	return va_render(d, VAPictureParameterBufferType, pic.get(), sizeof(*pic))
		&& (!sp.scaling || va_render(d, VAIQMatrixBufferType, iq.get(), sizeof(*iq))) ? 1 : -1;
}

static bool va_hlists(Va_Decoder *d, const Va_Hslice &s, VASliceParameterBufferHEVC &param)
{
	//the lists of a slice, each of the index of a picture kept, made by
	//the standard's 8.3.4 from those the picture is made from, in turn till
	//there are enough, and as the slice changes them. Every slice of a
	//picture is made from the same pictures, those its first slice says,
	//so one that says other than that is broken
	memset(param.RefPicList, 0xff, sizeof(param.RefPicList));
	int total = d->nbefore + d->nafter + d->nlt, own = 0;
	for (int i = 0; i < s.rps.negative + s.rps.positive; i++) own += s.rps.used[i];
	for (int i = 0; i < s.num_lt; i++) own += s.lt_used[i];
	if (own != total || (s.type != 2 && !total)) return false;
	for (int list = 0; list < (s.type == 0 ? 2 : s.type == 1 ? 1 : 0); list++)
	{
		const int *a = list ? d->hafter : d->hbefore, *b = list ? d->hbefore : d->hafter;
		int na = list ? d->nafter : d->nbefore, nb = list ? d->nbefore : d->nafter;
		int temp[32], n = 0, want = std::max(s.num_ref_idx[list], total);
		while (n < want)
		{
			for (int i = 0; i < na && n < want; i++) temp[n++] = a[i];
			for (int i = 0; i < nb && n < want; i++) temp[n++] = b[i];
			for (int i = 0; i < d->nlt && n < want; i++) temp[n++] = d->hlt[i];
		}
		for (int i = 0; i < s.num_ref_idx[list]; i++) param.RefPicList[list][i] = temp[s.mods[list] ? s.entries[list][i] : i];
	}
	return true;
}

static bool va_hpend(Va_Decoder *d, bool last)
{
	//the slice before given its parameters, now it is known if it is the
	//last of the picture
	if (!d->hpending) return true;
	d->hpending = false;
	d->hpend_param.LongSliceFlags.fields.LastSliceOfPic = last;
	VABufferID id;
	if (!va_buffer(d, VASliceParameterBufferType, &d->hpend_param, sizeof(d->hpend_param), id)) return false;
	d->slices.push_back(id);
	d->slices.push_back(d->hpend_data);
	return true;
}

static bool va_hfinish(Va_Decoder *d, int64_t shown)
{
	//the picture decoded, held to be shown if it is, and kept: every
	//picture is, till the next says it is not
	if (!d->started) return true;
	d->started = false;
	bool ok;
	if (d->hv) ok = hv_end(d);
	else
	{
		VADisplay dpy = va_display();
		ok = va_hpend(d, true);
		ok = ok && !d->slices.empty() && va::vaRenderPicture(dpy, d->context, d->slices.data(), d->slices.size()) == VA_STATUS_SUCCESS;
		ok = va::vaEndPicture(dpy, d->context) == VA_STATUS_SUCCESS && ok;
		d->slices.clear();
		va_buffers_free(d);
	}
	const Va_Hsps &sp = d->hpic_sps;
	if (ok && d->hout) ok = (d->hv ? hv_read : va_read)(d, d->cur.surface, sp.crop_left, sp.crop_top,
		sp.width - sp.crop_left - sp.crop_right, sp.height - sp.crop_top - sp.crop_bottom, shown);
	Va_Ref r = {};
	r.surface = d->cur.surface;
	r.top = r.bottom = d->cur_poc;
	r.short_ref = true;
	d->dpb.push_back(r);
	return ok;
}

static int64_t va_h265_decode(Va_Decoder *d, const uint8_t *data, uint64_t len, int64_t shown)
{
	//each NAL of the packet, of the base layer: parameter sets kept,
	//slices given the hardware, those of a picture not decoded passed by
	bool ok = true;
	for (uint64_t k = 0; k + d->nal_length <= len;)
	{
		size_t n = 0;
		for (int i = 0; i < d->nal_length; i++) n = (n << 8) | data[k + i];
		k += d->nal_length;
		if (n == 0 || k + n > len) break;
		const uint8_t *nal = data + k;
		k += n;
		if (n < 3) continue;
		int type = (nal[0] >> 1) & 63, layer = ((nal[0] & 1) << 5) | (nal[1] >> 3), tid = (nal[1] & 7) - 1;
		if (layer || tid < 0) continue;
		if (type == 33 || type == 34)
		{
			//a parameter set starts a new access unit, so ends the picture
			//before it, which is not to be read by the new set's size
			if (d->started) ok &= va_hfinish(d, shown);
			va_rbsp(nal, n, d->rbsp);
			if (type == 33) va_hsps(d->rbsp, d->hsps);
			else va_hpps(d->rbsp, d->hpps);
			continue;
		}
		if (type == 36 || type == 37)
		{
			//the end of a stream: the next picture starts again
			if (d->started) ok &= va_hfinish(d, shown);
			d->hfirst = true;
			continue;
		}
		if (type == 39)
		{
			va_rbsp(nal, n, d->rbsp);
			va_hsei(d, d->rbsp);
			continue;
		}
		if (type > 21 || (type > 9 && type < 16)) continue;
		//the header, all there is to read of it, in the first bytes
		va_rbsp(nal, std::min(n, (size_t)8192), d->rbsp);
		if (d->rbsp.size() < 3) continue;
		bool first = d->rbsp[2] & 0x80;
		if (!first && (d->hskip || !d->started))
		{
			ok &= d->hskip;
			continue;
		}
		if (first && d->started) ok &= va_hfinish(d, shown);
		Va_Hslice s;
		VASliceParameterBufferHEVC param;
		if (!va_hslice(d, d->rbsp, type, s, param))
		{
			ok = false;
			if (first) d->hskip = true;
			continue;
		}
		if (first)
		{
			int started = va_hstart(d, s, type, tid);
			d->hskip = started <= 0;
			if (started < 0)
			{
				if (d->started) va_hfinish(d, shown);
				ok = false;
			}
			if (started <= 0) continue;
		}
		//a slice of the picture of another PPS is broken
		else if (s.pps_id != d->hpic_pps) { ok = false; continue; }
		if (!va_hlists(d, s, param))
		{
			//broken, as is the picture of its first slice
			ok = false;
			if (first) d->hskip = true;
			continue;
		}
		d->hlast = s;
		d->hlast_param = param;
		param.slice_data_size = n;
		param.slice_data_offset = 0;
		param.slice_data_flag = VA_SLICE_DATA_FLAG_ALL;
		param.slice_data_byte_offset = s.header_bytes;
		param.slice_segment_address = s.address;
		auto &f = param.LongSliceFlags.fields;
		f.dependent_slice_segment_flag = s.dependent;
		f.slice_type = s.type;
		f.slice_sao_luma_flag = s.sao_luma;
		f.slice_sao_chroma_flag = s.sao_chroma;
		f.mvd_l1_zero_flag = s.mvd_l1_zero;
		f.cabac_init_flag = s.cabac_init;
		f.slice_temporal_mvp_enabled_flag = s.temporal_mvp;
		f.slice_deblocking_filter_disabled_flag = s.deblocking_disabled;
		f.collocated_from_l0_flag = s.collocated_from_l0;
		f.slice_loop_filter_across_slices_enabled_flag = s.loop_filter_across_slices;
		param.collocated_ref_idx = s.temporal_mvp ? s.collocated_ref_idx : 0xff;
		param.num_ref_idx_l0_active_minus1 = s.num_ref_idx[0] ? s.num_ref_idx[0] - 1 : 0;
		param.num_ref_idx_l1_active_minus1 = s.num_ref_idx[1] ? s.num_ref_idx[1] - 1 : 0;
		param.slice_qp_delta = s.qp_delta;
		param.slice_cb_qp_offset = s.cb_qp_offset;
		param.slice_cr_qp_offset = s.cr_qp_offset;
		param.slice_beta_offset_div2 = s.beta;
		param.slice_tc_offset_div2 = s.tc;
		param.five_minus_max_num_merge_cand = s.five_minus_max_merge;
		if (d->hv)
		{
			if (!hv_slice(d, s, param, nal, n, type, tid)) ok = false;
			continue;
		}
		//its data now, its parameters once it is known if it is the last
		VABufferID id;
		if (!va_hpend(d, false) || !va_buffer(d, VASliceDataBufferType, nal, n, id)) { ok = false; continue; }
		d->hpend_param = param;
		d->hpend_data = id;
		d->hpending = true;
	}
	ok &= va_hfinish(d, shown);
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
				//1 to 4 each way, of the standard
				if (pic.components[i].h_sampling_factor < 1 || pic.components[i].h_sampling_factor > 4
					|| pic.components[i].v_sampling_factor < 1 || pic.components[i].v_sampling_factor > 4) return -1;
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
				//of the two tables of each kind there are
				if (slice.components[i].dc_table_selector > 1 || slice.components[i].ac_table_selector > 1) return -1;
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
	//of H.264 and H.265 their parameter sets read now, the surfaces made
	//with the first picture; of Motion JPEG, each frame says what it is
	bool jpeg = codec == 0x6a706567, hevc = codec == 0x68766331;
	std::vector<const uint8_t*> sets;
	std::vector<size_t> sizes;
	int nal_length = 4;
	if (jpeg) { if (len < 8) return nullptr; }
	else if (hevc) { if (!video_hvcc(setup, len, sets, sizes, nal_length, true)) return nullptr; }
	else if (codec != 0x61766331 || !video_avcc(setup, len, sets, sizes, nal_length)) return nullptr;
	VADisplay dpy = va_display();
	if (!dpy) return nullptr;
	auto d = new Va_Decoder();
	d->jpeg = jpeg;
	d->hevc = hevc;
	d->nal_length = nal_length;
	bool ok = jpeg ? va_has(dpy, VAProfileJPEGBaseline) : false;
	if (hevc)
	{
		d->hsps.resize(16);
		d->hpps.resize(64);
	}
	for (size_t i = 0; i < sets.size(); i++)
	{
		if (sizes[i] < 2) continue;
		va_rbsp(sets[i], sizes[i], d->rbsp);
		if (hevc)
		{
			int type = (sets[i][0] >> 1) & 63;
			if (type == 33) va_hsps(d->rbsp, d->hsps);
			else if (type == 34) va_hpps(d->rbsp, d->hpps);
			else if (type == 39) va_hsei(d, d->rbsp);
		}
		else if ((sets[i][0] & 31) == 7) va_sps(d->rbsp, d->sps);
		else va_pps(d->rbsp, d->pps, d->sps);
	}
	if (hevc)
	{
		//of the first SPS, 8 bits Main, 10 Main 10, if the driver has it
		for (auto &sp : d->hsps)
		{
			if (!sp.valid) continue;
			ok = va_has(dpy, sp.bit_depth_luma == 10 ? VAProfileHEVCMain10 : VAProfileHEVCMain);
			break;
		}
	}
	else if (!jpeg)
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
	return d->jpeg ? va_jpeg_decode(d, data, len, shown) : d->hevc ? va_h265_decode(d, data, len, shown)
		: va_h264_decode(d, data, len, shown);
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
			if (it->bytes == 2 && (d->transfer == 16 || d->transfer == 18 || d->primaries == 9))
				va_convert_wide(*it, va_tone_of(d), pixels, w, h, stride);
			else va_convert(*it, d->full, d->matrix, pixels, w, h, stride);
			got = shown;
		}
		if (d->spare.size() < 4) d->spare.push_back(std::move(it->data));
		it = d->held.erase(it);
	}
	return got;
}

static Va_Decoder *hv_open(uint64_t codec, const uint8_t *setup, uint64_t len)
{
	//H.265 of 8 or 10 bits 4:2:0 by the V4L2 stateless decoder, if the machine has
	//one, nullptr if not. Its parameter sets read now, its buffers made with
	//the first picture
	std::vector<const uint8_t*> sets;
	std::vector<size_t> sizes;
	int nal_length = 4;
	if (codec != 0x68766331 || !video_hvcc(setup, len, sets, sizes, nal_length, true)) return nullptr;
	auto hv = new Hv();
	if (!hv_find(hv)) { delete hv; return nullptr; }
	auto d = new Va_Decoder();
	d->hv = hv;
	d->hevc = true;
	d->nal_length = nal_length;
	d->hsps.resize(16);
	d->hpps.resize(64);
	for (size_t i = 0; i < sets.size(); i++)
	{
		if (sizes[i] < 2) continue;
		va_rbsp(sets[i], sizes[i], d->rbsp);
		int type = (sets[i][0] >> 1) & 63;
		if (type == 33) va_hsps(d->rbsp, d->hsps);
		else if (type == 34) va_hpps(d->rbsp, d->hpps);
		else if (type == 39) va_hsei(d, d->rbsp);
	}
	bool ok = false;
	for (auto &sp : d->hsps)
	{
		if (!sp.valid) continue;
		ok = sp.bit_depth_luma == sp.bit_depth_chroma && (sp.bit_depth_luma == 8 || sp.bit_depth_luma == 10)
			&& sp.chroma_format_idc == 1;
		break;
	}
	if (!ok)
	{
		hv_close(d);
		delete d;
		return nullptr;
	}
	return d;
}

static void va_close(Va_Decoder *d)
{
	if (d->hv)
	{
		hv_close(d);
		delete d;
		return;
	}
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
	//JPEG, jpeg, its frames as they are, a nal_length of 0. H.265, hvc1,
	//only by VA-API
	if (codec == 0x68766331)
	{
		//VA-API, a PC's, or the Pi's stateless decoder
		auto va = va_open(codec, setup, len);
		if (!va) va = hv_open(codec, setup, len);
		if (!va) return 0;
		auto d = new Video_Decoder();
		d->va = va;
		return (uint64_t)d;
	}
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

#endif
