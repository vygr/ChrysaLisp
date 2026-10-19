#ifndef VIDEO_H
#define VIDEO_H

// the decoders of films a host has, video.cpp, at the end of every GUI table

#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif
uint64_t host_video_open(uint64_t codec, const uint8_t *setup, uint64_t len);
int64_t host_video_decode(uint64_t handle, const uint8_t *data, uint64_t len, int64_t shown);
int64_t host_video_frame(uint64_t handle, uint32_t *pixels, uint64_t w, uint64_t h, uint64_t stride, int64_t shown);
void host_video_close(uint64_t handle);
#ifdef __cplusplus
}
#endif

#endif
