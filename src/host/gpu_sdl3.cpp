#if defined(_HOST_GUI)
#if _HOST_GUI == 3

// SDL3 GPU driver, compute: buffers on the GPU, kernels that read and write
// them, and what they hold read back. Any node of a host on SDL3 has it, a
// desktop or not. The device is made the first time it is wanted, with no
// window, and the GUI driver draws with the same one, host_gpu_device(), so
// a buffer and a texture are of the one device.
//
// Each call is made on the stack of the kernel task, one at a time. A kernel
// is built by a thread of its own, as a shader is, a driver's compiler can
// take a long time. Work goes on the device's one queue in the order it is
// given: a write, a dispatch, a read, each after the last with no wait
// between. A dispatch and a read give a ticket, done says when it is over,
// take copies what a read read and lets the ticket go.

#include <SDL3/SDL.h>
#include "sdl3_load.h"
#include <stdint.h>
#include <string.h>

static SDL_GPUDevice *device = nullptr;
static bool device_tried = false;

// a thing SDL finds wrong is said, once, where what the node says goes, and
// the node carries on. A box that stops it till it is answered is no use to
// who is at a desktop, and can not be seen at all with no desktop. The GUI
// driver asks for this too, CL_GPU_DEBUG asks for SDL's own

static SDL_AssertState SDLCALL quiet_assertion(const SDL_AssertData *data, void *userdata)
{
	(void)userdata;
	SDL_Log("SDL assertion, ignored from now: %s, %s (%s:%d)", data->condition, data->function, data->filename, data->linenum);
	return SDL_ASSERTION_ALWAYS_IGNORE;
}

void host_gpu_quiet()
{
	if (!SDL_getenv("CL_GPU_DEBUG")) SDL_SetAssertionHandler(quiet_assertion, nullptr);
}

// the device, made the first time it is asked for. It asks for no more than
// is used: one SDL makes for itself wants depth clamping and the like, which
// a small GPU may not have, the Raspberry Pi 4 has no depth clamp, and SDL
// then picks a software device in its place

SDL_GPUDevice *host_gpu_device()
{
	if (device_tried) return device;
	device_tried = true;
	host_gpu_quiet();
	// SDL makes no device till its video is up. A desktop has it up, a node
	// with none has it up with no window: on a Mac the system's own, Metal
	// wants it, as an app in the background, not one of the Dock, elsewhere
	// the offscreen driver, as a hidden desktop has, unless SDL_VIDEO_DRIVER
	// says which. So a node that is to be a desktop starts its GUI first, as
	// it does, then is given work
	if (!SDL_WasInit(SDL_INIT_VIDEO))
	{
	#if defined(__APPLE__)
		SDL_SetHint(SDL_HINT_MAC_BACKGROUND_APP, "1");
	#else
		if (!SDL_getenv("SDL_VIDEO_DRIVER")) SDL_SetHint(SDL_HINT_VIDEO_DRIVER, "offscreen");
	#endif
		SDL_InitSubSystem(SDL_INIT_VIDEO);
	}
	auto props = SDL_CreateProperties();
	// SDL makes a device that checks every call unless told not to, and what
	// it finds wrong it stops the program for, a box to be answered. That is
	// for work on the drivers, CL_GPU_DEBUG asks for it
	SDL_SetBooleanProperty(props, SDL_PROP_GPU_DEVICE_CREATE_DEBUGMODE_BOOLEAN, SDL_getenv("CL_GPU_DEBUG") != nullptr);
	SDL_SetBooleanProperty(props, SDL_PROP_GPU_DEVICE_CREATE_SHADERS_SPIRV_BOOLEAN, true);
	SDL_SetBooleanProperty(props, SDL_PROP_GPU_DEVICE_CREATE_SHADERS_MSL_BOOLEAN, true);
	SDL_SetBooleanProperty(props, SDL_PROP_GPU_DEVICE_CREATE_FEATURE_CLIP_DISTANCE_BOOLEAN, false);
	SDL_SetBooleanProperty(props, SDL_PROP_GPU_DEVICE_CREATE_FEATURE_DEPTH_CLAMPING_BOOLEAN, false);
	SDL_SetBooleanProperty(props, SDL_PROP_GPU_DEVICE_CREATE_FEATURE_INDIRECT_DRAW_FIRST_INSTANCE_BOOLEAN, false);
	SDL_SetBooleanProperty(props, SDL_PROP_GPU_DEVICE_CREATE_FEATURE_ANISOTROPY_BOOLEAN, false);
	device = SDL_CreateGPUDeviceWithProperties(props);
	SDL_DestroyProperties(props);
	if (!device) SDL_Log("gpu: %s", SDL_GetError());
	return device;
}

// the shading language a kernel is given in, 0 if there is no GPU

uint64_t host_gpu_format()
{
	if (!host_gpu_device()) return 0;
	auto formats = SDL_GetGPUShaderFormats(device);
	if (formats & SDL_GPU_SHADERFORMAT_MSL) return 1;
	if (formats & SDL_GPU_SHADERFORMAT_SPIRV) return 2;
	return 0;
}

// a buffer a kernel reads and writes, of size bytes

struct Buffer
{
	SDL_GPUBuffer *buffer;
	Uint32 size;
};

void *host_gpu_buffer_create(uint64_t size)
{
	if (!host_gpu_device() || !size || size > 0xffffffff) return nullptr;
	SDL_GPUBufferCreateInfo info = {};
	info.usage = SDL_GPU_BUFFERUSAGE_COMPUTE_STORAGE_READ | SDL_GPU_BUFFERUSAGE_COMPUTE_STORAGE_WRITE;
	info.size = (Uint32)size;
	auto buffer = SDL_CreateGPUBuffer(device, &info);
	if (!buffer) return nullptr;
	auto b = (Buffer*)SDL_malloc(sizeof(Buffer));
	b->buffer = buffer;
	b->size = (Uint32)size;
	return b;
}

void host_gpu_buffer_destroy(void *handle)
{
	auto b = (Buffer*)handle;
	if (!b) return;
	SDL_ReleaseGPUBuffer(device, b->buffer);
	SDL_free(b);
}

// len bytes of data into the buffer at offset, 0 if it can not be

uint64_t host_gpu_buffer_write(void *handle, uint64_t offset, const void *data, uint64_t len)
{
	auto b = (Buffer*)handle;
	if (!b || !len || offset > b->size || len > b->size - offset) return 0;
	SDL_GPUTransferBufferCreateInfo ti = {};
	ti.usage = SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD;
	ti.size = (Uint32)len;
	auto transfer = SDL_CreateGPUTransferBuffer(device, &ti);
	auto cmd = transfer ? SDL_AcquireGPUCommandBuffer(device) : nullptr;
	if (!cmd)
	{
		if (transfer) SDL_ReleaseGPUTransferBuffer(device, transfer);
		return 0;
	}
	auto to = SDL_MapGPUTransferBuffer(device, transfer, false);
	SDL_memcpy(to, data, len);
	SDL_UnmapGPUTransferBuffer(device, transfer);
	auto copy = SDL_BeginGPUCopyPass(cmd);
	SDL_GPUTransferBufferLocation from = {transfer, 0};
	SDL_GPUBufferRegion region = {b->buffer, (Uint32)offset, (Uint32)len};
	SDL_UploadToGPUBuffer(copy, &from, &region, false);
	SDL_EndGPUCopyPass(copy);
	SDL_SubmitGPUCommandBuffer(cmd);
	// let go of now, SDL keeps it till the copy is done
	SDL_ReleaseGPUTransferBuffer(device, transfer);
	return 1;
}

// what is waited for, a dispatch, or a read and where what it read is

struct Ticket
{
	SDL_GPUFence *fence;
	SDL_GPUTransferBuffer *transfer;
	Uint32 size;
};

static void *ticket_of(SDL_GPUCommandBuffer *cmd, SDL_GPUTransferBuffer *transfer, Uint32 size)
{
	auto fence = SDL_SubmitGPUCommandBufferAndAcquireFence(cmd);
	if (!fence)
	{
		if (transfer) SDL_ReleaseGPUTransferBuffer(device, transfer);
		return nullptr;
	}
	auto t = (Ticket*)SDL_malloc(sizeof(Ticket));
	t->fence = fence;
	t->transfer = transfer;
	t->size = size;
	return t;
}

// len bytes of the buffer at offset read, a ticket, 0 if it can not be

void *host_gpu_buffer_read(void *handle, uint64_t offset, uint64_t len)
{
	auto b = (Buffer*)handle;
	if (!b || !len || offset > b->size || len > b->size - offset) return nullptr;
	SDL_GPUTransferBufferCreateInfo ti = {};
	ti.usage = SDL_GPU_TRANSFERBUFFERUSAGE_DOWNLOAD;
	ti.size = (Uint32)len;
	auto transfer = SDL_CreateGPUTransferBuffer(device, &ti);
	auto cmd = transfer ? SDL_AcquireGPUCommandBuffer(device) : nullptr;
	if (!cmd)
	{
		if (transfer) SDL_ReleaseGPUTransferBuffer(device, transfer);
		return nullptr;
	}
	auto copy = SDL_BeginGPUCopyPass(cmd);
	SDL_GPUBufferRegion region = {b->buffer, (Uint32)offset, (Uint32)len};
	SDL_GPUTransferBufferLocation to = {transfer, 0};
	SDL_DownloadFromGPUBuffer(copy, &region, &to);
	SDL_EndGPUCopyPass(copy);
	return ticket_of(cmd, transfer, (Uint32)len);
}

// 1 if what the ticket is of is over

uint64_t host_gpu_done(void *handle)
{
	auto t = (Ticket*)handle;
	return !t || SDL_QueryGPUFence(device, t->fence) ? 1 : 0;
}

// the ticket let go of, waited for if it is not over, and of a read what
// it read copied to data, as much as len of it

void host_gpu_take(void *handle, void *data, uint64_t len)
{
	auto t = (Ticket*)handle;
	if (!t) return;
	SDL_WaitForGPUFences(device, true, &t->fence, 1);
	if (t->transfer)
	{
		if (data && len)
		{
			auto from = SDL_MapGPUTransferBuffer(device, t->transfer, false);
			SDL_memcpy(data, from, len < t->size ? len : t->size);
			SDL_UnmapGPUTransferBuffer(device, t->transfer);
		}
		SDL_ReleaseGPUTransferBuffer(device, t->transfer);
	}
	SDL_ReleaseGPUFence(device, t->fence);
	SDL_free(t);
}

// a kernel, built on a thread of its own

enum
{
	kernel_building,
	kernel_ready,
	kernel_failed,
	kernel_dropped,
};

struct Kernel
{
	SDL_GPUComputePipeline *pipeline;
	SDL_AtomicInt state;
	SDL_GPUComputePipelineCreateInfo info;
	Uint8 *code;
};

static int SDLCALL build_kernel(void *data)
{
	auto k = (Kernel*)data;
	auto pipeline = SDL_CreateGPUComputePipeline(device, &k->info);
	if (!pipeline) SDL_Log("kernel: %s", SDL_GetError());
	SDL_free(k->code);
	k->code = nullptr;
	k->pipeline = pipeline;
	if (!SDL_CompareAndSwapAtomicInt(&k->state, kernel_building, pipeline ? kernel_ready : kernel_failed))
	{
		// it was destroyed while it was being built
		if (pipeline) SDL_ReleaseGPUComputePipeline(device, pipeline);
		SDL_free(k);
	}
	return 0;
}

// code of size bytes, MSL text or a SPIR-V module, its function kernel_main.
// The layout, six numbers of 32 bits: how many buffers it reads, how many it
// reads and writes, 1 if it has a block of uniforms, and its threads x y z.
// Given at once, it may not be built yet, and may not build at all, dispatch
// says which

void *host_gpu_kernel_create(const char *code, uint64_t size, const uint32_t *layout)
{
	auto format = host_gpu_format();
	if (!format || !size) return nullptr;
	auto k = (Kernel*)SDL_malloc(sizeof(Kernel));
	SDL_zero(k->info);
	// a byte more, and zero, text is read as far as that by some drivers
	k->code = (Uint8*)SDL_malloc(size + 1);
	SDL_memcpy(k->code, code, size);
	k->code[size] = 0;
	k->pipeline = nullptr;
	k->info.code = k->code;
	k->info.code_size = size;
	k->info.entrypoint = "kernel_main";
	k->info.format = format == 1 ? SDL_GPU_SHADERFORMAT_MSL : SDL_GPU_SHADERFORMAT_SPIRV;
	k->info.num_readonly_storage_buffers = layout[0];
	k->info.num_readwrite_storage_buffers = layout[1];
	k->info.num_uniform_buffers = layout[2];
	k->info.threadcount_x = layout[3];
	k->info.threadcount_y = layout[4];
	k->info.threadcount_z = layout[5];
	SDL_SetAtomicInt(&k->state, kernel_building);
	// a driver's compiler can want a deep stack
	auto props = SDL_CreateProperties();
	SDL_SetPointerProperty(props, SDL_PROP_THREAD_CREATE_ENTRY_FUNCTION_POINTER, (void*)build_kernel);
	SDL_SetStringProperty(props, SDL_PROP_THREAD_CREATE_NAME_STRING, "kernel");
	SDL_SetPointerProperty(props, SDL_PROP_THREAD_CREATE_USERDATA_POINTER, k);
	SDL_SetNumberProperty(props, SDL_PROP_THREAD_CREATE_STACKSIZE_NUMBER, 8 * 1024 * 1024);
	auto thread = SDL_CreateThreadWithProperties(props);
	SDL_DestroyProperties(props);
	if (thread) SDL_DetachThread(thread);
	else build_kernel(k);
	return k;
}

void host_gpu_kernel_destroy(void *handle)
{
	auto k = (Kernel*)handle;
	if (!k) return;
	// still being built, the thread that is building it frees it
	if (SDL_CompareAndSwapAtomicInt(&k->state, kernel_building, kernel_dropped)) return;
	if (k->pipeline) SDL_ReleaseGPUComputePipeline(device, k->pipeline);
	SDL_free(k);
}

// the kernel run over groups of its threads, three numbers of 32 bits, x y
// z, with buffers, those it reads then those it reads and writes, as its
// layout has them, and the uniforms of size bytes if it has a block of them.
// A ticket, 0 while the kernel is still being built, -1 if it did not build
// or can not be run. Five args, a host call has at most six on every CPU

void *host_gpu_dispatch(void *handle, void *const *buffers, const void *uniforms, uint64_t size,
	const uint32_t *groups)
{
	auto k = (Kernel*)handle;
	auto failed = (void*)-1;
	if (!k) return failed;
	auto state = SDL_GetAtomicInt(&k->state);
	if (state == kernel_building) return nullptr;
	if (state != kernel_ready) return failed;
	auto ro = k->info.num_readonly_storage_buffers, rw = k->info.num_readwrite_storage_buffers;
	if (ro > 8 || rw > 8 || (k->info.num_uniform_buffers && !uniforms)) return failed;
	SDL_GPUBuffer *reads[8];
	SDL_GPUStorageBufferReadWriteBinding writes[8] = {};
	for (Uint32 i = 0; i < ro; ++i) reads[i] = ((Buffer*)buffers[i])->buffer;
	for (Uint32 i = 0; i < rw; ++i) writes[i].buffer = ((Buffer*)buffers[ro + i])->buffer;
	auto cmd = SDL_AcquireGPUCommandBuffer(device);
	if (!cmd) return failed;
	if (k->info.num_uniform_buffers) SDL_PushGPUComputeUniformData(cmd, 0, uniforms, (Uint32)size);
	auto pass = SDL_BeginGPUComputePass(cmd, nullptr, 0, writes, rw);
	SDL_BindGPUComputePipeline(pass, k->pipeline);
	if (ro) SDL_BindGPUComputeStorageBuffers(pass, 0, reads, ro);
	SDL_DispatchGPUCompute(pass, groups[0], groups[1], groups[2]);
	SDL_EndGPUComputePass(pass);
	auto t = ticket_of(cmd, nullptr, 0);
	return t ? t : failed;
}

void (*host_gpu_funcs[]) = {
	(void*)host_gpu_format,
	(void*)host_gpu_buffer_create,
	(void*)host_gpu_buffer_destroy,
	(void*)host_gpu_buffer_write,
	(void*)host_gpu_buffer_read,
	(void*)host_gpu_done,
	(void*)host_gpu_take,
	(void*)host_gpu_kernel_create,
	(void*)host_gpu_kernel_destroy,
	(void*)host_gpu_dispatch,
};

#endif
#endif
