#ifndef GPU_SDL3_H
#define GPU_SDL3_H

// what the GPU driver, gpu_sdl3.cpp, and the GUI driver, gui_sdl3.cpp, of a
// host on SDL3 have of each other

#include <SDL3/SDL.h>

// the device, made the first time it is asked for, the one device of the
// host: the GUI driver draws with it too
SDL_GPUDevice *host_gpu_device();
void host_gpu_quiet();

// the depth buffer and the samples a pixel a pair of the GPU driver is built
// for, what a target it draws into must have
SDL_GPUTextureFormat host_gpu_depth_format();
SDL_GPUSampleCount host_gpu_sample_count();

// What a GUI driver that draws with the device lends, so triangles can be
// drawn straight into a texture it shows, where it is, with no copy of a
// frame from the GPU and back. The handle is one of the GUI driver's own
// textures, as its table gives them, and is not trusted: one that is not a
// texture to draw into, or has gone, is refused.
struct HostGpuGui
{
	// the texture to draw into, as the GPU has it, its size, and the depth
	// buffer and the samples that go with it, kept with it, samples nullptr
	// if the device has one a pixel. 1, 0 if it is not to be drawn into now,
	// the GPU is busy with the last draw into it or with another's, try
	// again, -1 if it is not a texture to draw into. At 1 what the GUI has
	// drawn so far has been handed to the GPU, the draw goes after it
	int (*target)(void *handle, SDL_GPUTexture **color, SDL_GPUTexture **depth, SDL_GPUTexture **samples,
		Uint32 *w, Uint32 *h);
	// a draw into it has been handed to the GPU, the fence is the GUI's from here
	void (*drawn)(void *handle, SDL_GPUFence *fence);
	// 1 if the GPU is done with the last draw into it, or it has gone
	int (*done)(void *handle);
};

void host_gpu_gui(const HostGpuGui *hooks);

#endif
