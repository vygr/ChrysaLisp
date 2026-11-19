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
#include "gpu_sdl3.h"
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
	// of a draw into a texture of the GUI driver's, that texture: the fence
	// of it is the GUI driver's, which is asked if the draw is over
	void *gui;
};

struct HostGpuGui;
static const HostGpuGui *gui_hooks = nullptr;
static int gui_done(void *handle);

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
	t->gui = nullptr;
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
	if (t && t->gui) return gui_done(t->gui);
	return !t || SDL_QueryGPUFence(device, t->fence) ? 1 : 0;
}

// the ticket let go of, waited for if it is not over, and of a read what
// it read copied to data, as much as len of it

void host_gpu_take(void *handle, void *data, uint64_t len)
{
	auto t = (Ticket*)handle;
	if (!t) return;
	if (t->gui)
	{
		// nothing was read, and nothing is waited for
		SDL_free(t);
		return;
	}
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

// A kernel must not run till the writes given before it are over. The queue
// keeps that order by itself, but for the Vulkan driver of the Raspberry Pi,
// v3dv. Its hardware has a queue for kernels and another for copies, and it
// has a kernel wait for the copies only when one is before it in the same
// command buffer: the barrier a command buffer ends with is not kept for the
// next that is submitted. So a kernel given straight after a write, or after
// SDL moved a buffer to pack its memory, could run first, on what the buffer
// held before, when other programs kept the copies waiting. On Vulkan each
// dispatch so begins with a copy of its own, 4 bytes to a buffer nothing
// reads, and its kernel waits for that, and so for every copy before it

static SDL_GPUBuffer *order_buffer = nullptr;
static SDL_GPUTransferBuffer *order_transfer = nullptr;
static bool order_tried = false;

static void order_copy(SDL_GPUCommandBuffer *cmd)
{
	if (!order_tried)
	{
		order_tried = true;
		if (host_gpu_format() != 2) return;
		SDL_GPUBufferCreateInfo info = {};
		info.usage = SDL_GPU_BUFFERUSAGE_COMPUTE_STORAGE_READ;
		info.size = 4;
		SDL_GPUTransferBufferCreateInfo ti = {};
		ti.usage = SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD;
		ti.size = 4;
		order_buffer = SDL_CreateGPUBuffer(device, &info);
		order_transfer = SDL_CreateGPUTransferBuffer(device, &ti);
	}
	if (!order_buffer || !order_transfer) return;
	auto copy = SDL_BeginGPUCopyPass(cmd);
	SDL_GPUTransferBufferLocation from = {order_transfer, 0};
	SDL_GPUBufferRegion region = {order_buffer, 0, 4};
	SDL_UploadToGPUBuffer(copy, &from, &region, false);
	SDL_EndGPUCopyPass(copy);
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
	order_copy(cmd);
	if (k->info.num_uniform_buffers) SDL_PushGPUComputeUniformData(cmd, 0, uniforms, (Uint32)size);
	auto pass = SDL_BeginGPUComputePass(cmd, nullptr, 0, writes, rw);
	SDL_BindGPUComputePipeline(pass, k->pipeline);
	if (ro) SDL_BindGPUComputeStorageBuffers(pass, 0, reads, ro);
	SDL_DispatchGPUCompute(pass, groups[0], groups[1], groups[2]);
	SDL_EndGPUComputePass(pass);
	auto t = ticket_of(cmd, nullptr, 0);
	return t ? t : failed;
}

// Triangles. A pair, a vertex shader and a pixel shader, draws meshes into a
// target with a depth buffer, a frame of them at a time: what is nearest is
// what is seen, and the target is cleared first. The target is one of this
// driver's own, and what was drawn is read back, for a node whose pixels
// are shown by another. Or, on a node whose GUI draws with this device, it
// is a texture of the GUI's, drawn into where it is and shown from there,
// with nothing read back: the GUI driver lends that, host_gpu_gui().

static const SDL_GPUTextureFormat tris_format = SDL_GPU_TEXTUREFORMAT_B8G8R8A8_UNORM;
static SDL_GPUTextureFormat tris_depth = SDL_GPU_TEXTUREFORMAT_INVALID;
static SDL_GPUSampleCount tris_samples = SDL_GPU_SAMPLECOUNT_1;
static bool tris_samples_found = false;
static SDL_GPUSampler *tris_sampler = nullptr;

void host_gpu_gui(const HostGpuGui *hooks)
{
	gui_hooks = hooks;
}

static int gui_done(void *handle)
{
	return gui_hooks ? gui_hooks->done(handle) : 1;
}

// the format of a depth buffer, the first of these the GPU has

SDL_GPUTextureFormat host_gpu_depth_format()
{
	if (tris_depth != SDL_GPU_TEXTUREFORMAT_INVALID || !host_gpu_device()) return tris_depth;
	const SDL_GPUTextureFormat formats[] = {SDL_GPU_TEXTUREFORMAT_D32_FLOAT,
		SDL_GPU_TEXTUREFORMAT_D24_UNORM, SDL_GPU_TEXTUREFORMAT_D16_UNORM};
	for (auto f : formats)
	{
		if (SDL_GPUTextureSupportsFormat(device, f, SDL_GPU_TEXTURETYPE_2D, SDL_GPU_TEXTUREUSAGE_DEPTH_STENCIL_TARGET))
		{
			tris_depth = f;
			break;
		}
	}
	return tris_depth;
}

// 4 samples a pixel where the device has that, of both the color and the
// depth of a target, else the one

SDL_GPUSampleCount host_gpu_sample_count()
{
	if (tris_samples_found) return tris_samples;
	auto depth = host_gpu_depth_format();
	if (depth != SDL_GPU_TEXTUREFORMAT_INVALID
		&& SDL_GPUTextureSupportsSampleCount(device, tris_format, SDL_GPU_SAMPLECOUNT_4)
		&& SDL_GPUTextureSupportsSampleCount(device, depth, SDL_GPU_SAMPLECOUNT_4))
		tris_samples = SDL_GPU_SAMPLECOUNT_4;
	tris_samples_found = true;
	return tris_samples;
}

// a pair, built on a thread of its own, as a kernel is

enum
{
	pair_building,
	pair_ready,
	pair_failed,
	pair_dropped,
};

// the most pictures the pixel shader of a pair can read from
static const Uint8 max_pair_textures = 8;

struct Pair
{
	SDL_GPUGraphicsPipeline *pipeline;
	SDL_AtomicInt state;
	SDL_GPUShaderFormat format;
	Uint8 *vertex;
	Uint8 *fragment;
	size_t vertex_size;
	size_t fragment_size;
	// how many attrs a vertex has, how many floats each is, and all of them,
	// whether a triangle that faces away is left out, and how many pictures
	// the pixel shader reads from
	Uint8 num_attrs;
	Uint8 attrs[8];
	Uint8 cull;
	Uint32 stride;
	Uint8 num_textures;
	// flat: no depth, a sample a pixel, and what it draws is put where it
	// goes, not over what is there. What a frame that is all a pixel shader
	// is drawn with, and it can be drawn some rows at a time
	Uint8 flat;
};

static SDL_GPUShader *pair_stage(const Uint8 *code, size_t size, SDL_GPUShaderFormat format,
	SDL_GPUShaderStage stage, const char *entry, Uint32 uniforms, Uint32 samplers)
{
	SDL_GPUShaderCreateInfo info = {};
	info.code = code;
	info.code_size = size;
	info.entrypoint = entry;
	info.format = format;
	info.stage = stage;
	info.num_uniform_buffers = uniforms;
	info.num_samplers = samplers;
	return SDL_CreateGPUShader(device, &info);
}

static int SDLCALL build_pair(void *data)
{
	auto pair = (Pair*)data;
	// the vertex stage has the inputs of the vertex shader as its block. The
	// fragment stage has the inputs of the pixel shader, and the size of the
	// target as a second block after
	auto vs = pair_stage(pair->vertex, pair->vertex_size, pair->format,
		SDL_GPU_SHADERSTAGE_VERTEX, "vertex_main", 1, 0);
	auto fs = pair_stage(pair->fragment, pair->fragment_size, pair->format,
		SDL_GPU_SHADERSTAGE_FRAGMENT, "fragment_main", 2, pair->num_textures);
	SDL_GPUGraphicsPipeline *pipeline = nullptr;
	if (vs && fs)
	{
		SDL_GPUColorTargetDescription target = {};
		target.format = tris_format;
		SDL_GPUGraphicsPipelineCreateInfo info = {};
		info.vertex_shader = vs;
		info.fragment_shader = fs;
		info.primitive_type = SDL_GPU_PRIMITIVETYPE_TRIANGLELIST;
		info.rasterizer_state.fill_mode = SDL_GPU_FILLMODE_FILL;
		// depth is clipped, not clamped, the device was not asked for clamping
		info.rasterizer_state.enable_depth_clip = true;
		info.target_info.color_target_descriptions = &target;
		info.target_info.num_color_targets = 1;
		// the attrs of a vertex, floats, one after another in the one buffer
		const SDL_GPUVertexElementFormat formats[] = {SDL_GPU_VERTEXELEMENTFORMAT_FLOAT,
			SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2, SDL_GPU_VERTEXELEMENTFORMAT_FLOAT3,
			SDL_GPU_VERTEXELEMENTFORMAT_FLOAT4};
		SDL_GPUVertexBufferDescription buffer = {};
		SDL_GPUVertexAttribute attrs[8] = {};
		Uint32 offset = 0;
		for (int i = 0; i < pair->num_attrs; ++i)
		{
			attrs[i].location = i;
			attrs[i].buffer_slot = 0;
			attrs[i].format = formats[pair->attrs[i] - 1];
			attrs[i].offset = offset;
			offset += pair->attrs[i] * sizeof(float);
		}
		buffer.slot = 0;
		buffer.pitch = offset;
		buffer.input_rate = SDL_GPU_VERTEXINPUTRATE_VERTEX;
		info.vertex_input_state.vertex_buffer_descriptions = &buffer;
		info.vertex_input_state.num_vertex_buffers = 1;
		info.vertex_input_state.vertex_attributes = attrs;
		info.vertex_input_state.num_vertex_attributes = pair->num_attrs;
		// the front of a triangle is the side it goes round counter
		// clockwise from, and what is nearest is what is seen
		info.rasterizer_state.front_face = SDL_GPU_FRONTFACE_COUNTER_CLOCKWISE;
		info.rasterizer_state.cull_mode = pair->cull == 1 ? SDL_GPU_CULLMODE_BACK
			: pair->cull == 2 ? SDL_GPU_CULLMODE_FRONT : SDL_GPU_CULLMODE_NONE;
		// a pixel comes with its alpha multiplied in, and goes over what is there
		target.blend_state.enable_blend = !pair->flat;
		target.blend_state.src_color_blendfactor = SDL_GPU_BLENDFACTOR_ONE;
		target.blend_state.dst_color_blendfactor = SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA;
		target.blend_state.color_blend_op = SDL_GPU_BLENDOP_ADD;
		target.blend_state.src_alpha_blendfactor = SDL_GPU_BLENDFACTOR_ONE;
		target.blend_state.dst_alpha_blendfactor = SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA;
		target.blend_state.alpha_blend_op = SDL_GPU_BLENDOP_ADD;
		if (!pair->flat)
		{
			info.depth_stencil_state.enable_depth_test = true;
			info.depth_stencil_state.enable_depth_write = true;
			info.depth_stencil_state.compare_op = SDL_GPU_COMPAREOP_LESS;
			info.target_info.has_depth_stencil_target = true;
			info.target_info.depth_stencil_format = tris_depth;
			info.multisample_state.sample_count = tris_samples;
		}
		pipeline = SDL_CreateGPUGraphicsPipeline(device, &info);
	}
	if (!pipeline) SDL_Log("pair: %s", SDL_GetError());
	if (vs) SDL_ReleaseGPUShader(device, vs);
	if (fs) SDL_ReleaseGPUShader(device, fs);
	SDL_free(pair->vertex);
	SDL_free(pair->fragment);
	pair->vertex = pair->fragment = nullptr;
	pair->pipeline = pipeline;
	if (!SDL_CompareAndSwapAtomicInt(&pair->state, pair_building, pipeline ? pair_ready : pair_failed))
	{
		// it was destroyed while it was being built
		if (pipeline) SDL_ReleaseGPUGraphicsPipeline(device, pipeline);
		SDL_free(pair);
	}
	return 0;
}

static Uint8 *pair_code(const char *code, uint64_t size)
{
	// a byte more, and zero, text is read as far as that by some drivers
	auto copy = (Uint8*)SDL_malloc(size + 1);
	SDL_memcpy(copy, code, size);
	copy[size] = 0;
	return copy;
}

// a vertex shader and a pixel shader, MSL text or SPIR-V modules, their
// functions vertex_main and fragment_main. The layout is how many attrs a
// vertex has, the cull, 0 none, 1 those that face away, 2 those that face
// us, and 4 more for a flat pair, then how many floats each attr is, a byte
// each, and then how many pictures the pixel shader reads from, a byte.
// Given at once, it may not be built yet, and may not build at all, a draw
// says which

void *host_gpu_pair_create(const char *vertex, uint64_t vertex_size, const char *fragment, uint64_t fragment_size,
	const uint8_t *layout)
{
	auto format = host_gpu_format();
	if (!format || !vertex_size || !fragment_size
		|| host_gpu_depth_format() == SDL_GPU_TEXTUREFORMAT_INVALID) return nullptr;
	// found here, the pair is built on a thread
	host_gpu_sample_count();
	auto pair = (Pair*)SDL_malloc(sizeof(Pair));
	pair->pipeline = nullptr;
	pair->format = format == 1 ? SDL_GPU_SHADERFORMAT_MSL : SDL_GPU_SHADERFORMAT_SPIRV;
	pair->num_attrs = layout[0] > 8 ? 8 : layout[0];
	pair->cull = layout[1] & 3;
	pair->flat = (layout[1] & 4) ? 1 : 0;
	pair->stride = 0;
	for (int i = 0; i < pair->num_attrs; ++i)
	{
		auto n = layout[2 + i];
		pair->attrs[i] = n < 1 ? 1 : n > 4 ? 4 : n;
		pair->stride += pair->attrs[i];
	}
	auto textures = layout[2 + layout[0]];
	pair->num_textures = textures > max_pair_textures ? max_pair_textures : textures;
	pair->vertex = pair_code(vertex, vertex_size);
	pair->fragment = pair_code(fragment, fragment_size);
	pair->vertex_size = vertex_size;
	pair->fragment_size = fragment_size;
	SDL_SetAtomicInt(&pair->state, pair_building);
	// a driver's compiler can want a deep stack
	auto props = SDL_CreateProperties();
	SDL_SetPointerProperty(props, SDL_PROP_THREAD_CREATE_ENTRY_FUNCTION_POINTER, (void*)build_pair);
	SDL_SetStringProperty(props, SDL_PROP_THREAD_CREATE_NAME_STRING, "pair");
	SDL_SetPointerProperty(props, SDL_PROP_THREAD_CREATE_USERDATA_POINTER, pair);
	SDL_SetNumberProperty(props, SDL_PROP_THREAD_CREATE_STACKSIZE_NUMBER, 8 * 1024 * 1024);
	auto thread = SDL_CreateThreadWithProperties(props);
	SDL_DestroyProperties(props);
	if (thread) SDL_DetachThread(thread);
	else build_pair(pair);
	return pair;
}

void host_gpu_pair_destroy(void *handle)
{
	auto pair = (Pair*)handle;
	if (!pair) return;
	// still being built, the thread that is building it frees it
	if (SDL_CompareAndSwapAtomicInt(&pair->state, pair_building, pair_dropped)) return;
	if (pair->pipeline) SDL_ReleaseGPUGraphicsPipeline(device, pair->pipeline);
	SDL_free(pair);
}

// the vertices of a mesh, kept on the GPU. They come as a double for each
// number, as the nodes have them, and are kept as floats

struct Mesh
{
	SDL_GPUBuffer *buffer;
	Uint32 floats;
};

void *host_gpu_mesh_create(const void *verts, uint64_t size)
{
	Uint32 floats = (Uint32)(size / 8);
	if (!host_gpu_device() || !floats) return nullptr;
	SDL_GPUBufferCreateInfo bi = {};
	bi.usage = SDL_GPU_BUFFERUSAGE_VERTEX;
	bi.size = floats * (Uint32)sizeof(float);
	auto buffer = SDL_CreateGPUBuffer(device, &bi);
	SDL_GPUTransferBufferCreateInfo ti = {};
	ti.usage = SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD;
	ti.size = bi.size;
	auto transfer = SDL_CreateGPUTransferBuffer(device, &ti);
	auto cmd = buffer && transfer ? SDL_AcquireGPUCommandBuffer(device) : nullptr;
	if (!cmd)
	{
		if (buffer) SDL_ReleaseGPUBuffer(device, buffer);
		if (transfer) SDL_ReleaseGPUTransferBuffer(device, transfer);
		return nullptr;
	}
	auto out = (float*)SDL_MapGPUTransferBuffer(device, transfer, false);
	for (Uint32 i = 0; i < floats; ++i)
	{
		double v;
		SDL_memcpy(&v, (const Uint8*)verts + (size_t)i * 8, 8);
		out[i] = (float)v;
	}
	SDL_UnmapGPUTransferBuffer(device, transfer);
	auto copy = SDL_BeginGPUCopyPass(cmd);
	SDL_GPUTransferBufferLocation from = {transfer, 0};
	SDL_GPUBufferRegion to = {buffer, 0, bi.size};
	SDL_UploadToGPUBuffer(copy, &from, &to, false);
	SDL_EndGPUCopyPass(copy);
	SDL_SubmitGPUCommandBuffer(cmd);
	SDL_ReleaseGPUTransferBuffer(device, transfer);
	auto mesh = (Mesh*)SDL_malloc(sizeof(Mesh));
	mesh->buffer = buffer;
	mesh->floats = floats;
	return mesh;
}

void host_gpu_mesh_destroy(void *handle)
{
	auto mesh = (Mesh*)handle;
	if (!mesh) return;
	SDL_ReleaseGPUBuffer(device, mesh->buffer);
	SDL_free(mesh);
}

// a picture a pixel shader reads from, w by h pixels of 32 bits, blue green
// red alpha, the alpha multiplied in, as a pixmap has them, the top row
// first. It is a copy, what is done to the pixels after is not seen

struct Picture
{
	SDL_GPUTexture *texture;
};

void *host_gpu_picture_create(const void *pixels, uint64_t w, uint64_t h)
{
	if (!host_gpu_device() || !pixels || !w || !h || w > 16384 || h > 16384) return nullptr;
	SDL_GPUTextureCreateInfo info = {};
	info.type = SDL_GPU_TEXTURETYPE_2D;
	info.format = tris_format;
	info.usage = SDL_GPU_TEXTUREUSAGE_SAMPLER;
	info.width = (Uint32)w;
	info.height = (Uint32)h;
	info.layer_count_or_depth = 1;
	info.num_levels = 1;
	auto texture = SDL_CreateGPUTexture(device, &info);
	SDL_GPUTransferBufferCreateInfo ti = {};
	ti.usage = SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD;
	ti.size = (Uint32)(w * h * 4);
	auto transfer = SDL_CreateGPUTransferBuffer(device, &ti);
	auto cmd = texture && transfer ? SDL_AcquireGPUCommandBuffer(device) : nullptr;
	if (!cmd)
	{
		if (texture) SDL_ReleaseGPUTexture(device, texture);
		if (transfer) SDL_ReleaseGPUTransferBuffer(device, transfer);
		return nullptr;
	}
	SDL_memcpy(SDL_MapGPUTransferBuffer(device, transfer, false), pixels, ti.size);
	SDL_UnmapGPUTransferBuffer(device, transfer);
	auto copy = SDL_BeginGPUCopyPass(cmd);
	SDL_GPUTextureTransferInfo from = {};
	from.transfer_buffer = transfer;
	from.pixels_per_row = (Uint32)w;
	from.rows_per_layer = (Uint32)h;
	SDL_GPUTextureRegion to = {};
	to.texture = texture;
	to.w = (Uint32)w;
	to.h = (Uint32)h;
	to.d = 1;
	SDL_UploadToGPUTexture(copy, &from, &to, false);
	SDL_EndGPUCopyPass(copy);
	SDL_SubmitGPUCommandBuffer(cmd);
	SDL_ReleaseGPUTransferBuffer(device, transfer);
	auto picture = (Picture*)SDL_malloc(sizeof(Picture));
	picture->texture = texture;
	return picture;
}

void host_gpu_picture_destroy(void *handle)
{
	auto picture = (Picture*)handle;
	if (!picture) return;
	SDL_ReleaseGPUTexture(device, picture->texture);
	SDL_free(picture);
}

// a target of this driver's own, w by h, with its depth buffer, and the
// samples it is drawn with if the device has more than one a pixel

struct Target
{
	SDL_GPUTexture *color;
	SDL_GPUTexture *depth;
	SDL_GPUTexture *samples;
	Uint32 w, h;
};

void host_gpu_target_destroy(void *handle)
{
	auto target = (Target*)handle;
	if (!target) return;
	if (target->color) SDL_ReleaseGPUTexture(device, target->color);
	if (target->depth) SDL_ReleaseGPUTexture(device, target->depth);
	if (target->samples) SDL_ReleaseGPUTexture(device, target->samples);
	SDL_free(target);
}

void *host_gpu_target_create(uint64_t w, uint64_t h)
{
	if (!host_gpu_device() || !w || !h || w > 16384 || h > 16384
		|| host_gpu_depth_format() == SDL_GPU_TEXTUREFORMAT_INVALID) return nullptr;
	auto target = (Target*)SDL_malloc(sizeof(Target));
	target->w = (Uint32)w;
	target->h = (Uint32)h;
	target->samples = nullptr;
	SDL_GPUTextureCreateInfo info = {};
	info.type = SDL_GPU_TEXTURETYPE_2D;
	info.format = tris_format;
	info.usage = SDL_GPU_TEXTUREUSAGE_COLOR_TARGET;
	info.width = target->w;
	info.height = target->h;
	info.layer_count_or_depth = 1;
	info.num_levels = 1;
	target->color = SDL_CreateGPUTexture(device, &info);
	info.sample_count = host_gpu_sample_count();
	if (info.sample_count != SDL_GPU_SAMPLECOUNT_1) target->samples = SDL_CreateGPUTexture(device, &info);
	info.format = tris_depth;
	info.usage = SDL_GPU_TEXTUREUSAGE_DEPTH_STENCIL_TARGET;
	target->depth = SDL_CreateGPUTexture(device, &info);
	if (!target->color || !target->depth || (info.sample_count != SDL_GPU_SAMPLECOUNT_1 && !target->samples))
	{
		host_gpu_target_destroy(target);
		return nullptr;
	}
	return target;
}

// As a kernel must wait for the writes before it, order_copy() above, a draw
// must wait for the meshes and the pictures put on the GPU before it, and
// the Raspberry Pi's driver has that wait only for a copy in the draw's own
// command buffer. So on Vulkan each draw begins with a copy of its own too,
// 4 bytes to a buffer of vertices nothing draws

static SDL_GPUBuffer *draw_order_buffer = nullptr;
static SDL_GPUTransferBuffer *draw_order_transfer = nullptr;
static bool draw_order_tried = false;

static void order_draw(SDL_GPUCommandBuffer *cmd)
{
	if (!draw_order_tried)
	{
		draw_order_tried = true;
		if (host_gpu_format() != 2) return;
		SDL_GPUBufferCreateInfo info = {};
		info.usage = SDL_GPU_BUFFERUSAGE_VERTEX;
		info.size = 4;
		SDL_GPUTransferBufferCreateInfo ti = {};
		ti.usage = SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD;
		ti.size = 4;
		draw_order_buffer = SDL_CreateGPUBuffer(device, &info);
		draw_order_transfer = SDL_CreateGPUTransferBuffer(device, &ti);
	}
	if (!draw_order_buffer || !draw_order_transfer) return;
	auto copy = SDL_BeginGPUCopyPass(cmd);
	SDL_GPUTransferBufferLocation from = {draw_order_transfer, 0};
	SDL_GPUBufferRegion region = {draw_order_buffer, 0, 4};
	SDL_UploadToGPUBuffer(copy, &from, &region, false);
	SDL_EndGPUCopyPass(copy);
}

// A frame of triangles drawn into a target. The frame is a count, 8 bytes,
// then for each thing drawn, 8 bytes each, the pair, the mesh, the length
// of the vertex shader's block and of the pixel shader's block, then the
// two blocks, each made up to a whole 8 bytes, then the pictures the pair's
// pixel shader reads from, a handle of 8 bytes each, as many as the pair
// was made to have.
//
// With gui 0 the target is one of this driver's, and what is given is a
// ticket: take copies the pixels drawn, w by h of 32 bits, blue green red
// alpha, the top row first, and lets it go. With gui 1 the target is a
// texture of the GUI driver's, drawn into where it is, nothing is read
// back, and its ticket says only when the GPU is done. Either way 0 is nothing
// drawn, a pair is still being built, or the GUI's texture is not to be
// drawn into yet, try again; and -1 is a frame that can not be drawn, a pair
// that did not build, or a target that is not one
//
// A frame whose pairs are flat is drawn with no depth buffer and a sample a
// pixel. And with y1 more than y only the rows y to y1 of it are drawn, the
// rest of the target left as it was: a GPU can not be stopped part way
// through a draw, and the GUI is drawn by the same one, so a frame that
// takes a slow GPU long is given to it some rows at a time, and the GUI has
// its turn between

void *host_gpu_draw(void *handle, const void *frame, uint64_t size, uint64_t gui, uint64_t y, uint64_t y1)
{
	auto failed = (void*)-1;
	if (!host_gpu_device() || !handle || size < 8 || (gui && !gui_hooks)
		|| host_gpu_depth_format() == SDL_GPU_TEXTUREFORMAT_INVALID) return failed;
	struct Draw { Pair *pair; Mesh *mesh; uint64_t vlen, plen; const Uint8 *vblock, *pblock, *textures; };
	auto data = (const Uint8*)frame;
	uint64_t count;
	SDL_memcpy(&count, data, 8);
	if (count > (size - 8) / 32) return failed;
	auto draws = (Draw*)SDL_malloc(sizeof(Draw) * (count ? count : 1));
	const Uint8 *at = data + 8, *end = data + size;
	uint64_t good = 0;
	void *result = nullptr;
	bool building = false;
	uint64_t flats = 0;
	for (; good < count && at + 32 <= end; ++good)
	{
		auto d = &draws[good];
		SDL_memcpy(d, at, 32);
		at += 32;
		auto vpad = (d->vlen + 7) & ~(uint64_t)7, ppad = (d->plen + 7) & ~(uint64_t)7;
		if (vpad > (uint64_t)(end - at) || ppad > (uint64_t)(end - at) - vpad) { result = failed; break; }
		d->vblock = at;
		d->pblock = at + vpad;
		at += vpad + ppad;
		// every pair of the frame has to be built before any of it is drawn
		auto state = d->pair ? SDL_GetAtomicInt(&d->pair->state) : pair_failed;
		if (state != pair_building && state != pair_ready) { result = failed; break; }
		if (state == pair_building) building = true;
		if (d->pair->flat) ++flats;
		d->textures = at;
		if ((uint64_t)(end - at) < 8 * (uint64_t)d->pair->num_textures) { result = failed; break; }
		at += 8 * (uint64_t)d->pair->num_textures;
	}
	if (result != failed && good < count) result = failed;
	// flat or not, all of a frame, and only a flat one a part at a time
	bool flat = flats != 0, band = y1 > y;
	if (result != failed && ((flat && flats != good) || (band && !flat))) result = failed;
	// the target, this driver's or the GUI's, and what goes with it
	SDL_GPUTexture *color = nullptr, *depth = nullptr, *samples = nullptr;
	Uint32 w = 0, h = 0;
	bool ready = result != failed && !building;
	if (ready && gui)
	{
		auto can = gui_hooks->target(handle, &color, flat ? nullptr : &depth, &samples, &w, &h);
		if (can < 0) result = failed;
		if (can <= 0) ready = false;
	}
	else if (ready)
	{
		auto target = (Target*)handle;
		color = target->color;
		if (!flat)
		{
			depth = target->depth;
			samples = target->samples;
		}
		w = target->w;
		h = target->h;
	}
	if (ready && band && (y >= h || y1 > h)) { result = failed; ready = false; }
	if (ready && !tris_sampler)
	{
		// every picture is read the one way: the one pixel a place is in, and
		// round again outside 0 to 1, as the code the nodes run with no GPU
		// reads them
		SDL_GPUSamplerCreateInfo info = {};
		info.min_filter = SDL_GPU_FILTER_NEAREST;
		info.mag_filter = SDL_GPU_FILTER_NEAREST;
		info.mipmap_mode = SDL_GPU_SAMPLERMIPMAPMODE_NEAREST;
		info.address_mode_u = SDL_GPU_SAMPLERADDRESSMODE_REPEAT;
		info.address_mode_v = SDL_GPU_SAMPLERADDRESSMODE_REPEAT;
		info.address_mode_w = SDL_GPU_SAMPLERADDRESSMODE_REPEAT;
		tris_sampler = SDL_CreateGPUSampler(device, &info);
		if (!tris_sampler) { result = failed; ready = false; }
	}
	// what is read back of a target of this driver's
	SDL_GPUTransferBuffer *transfer = nullptr;
	if (ready && !gui)
	{
		SDL_GPUTransferBufferCreateInfo ti = {};
		ti.usage = SDL_GPU_TRANSFERBUFFERUSAGE_DOWNLOAD;
		ti.size = w * h * 4;
		transfer = SDL_CreateGPUTransferBuffer(device, &ti);
		if (!transfer) { result = failed; ready = false; }
	}
	auto cmd = ready ? SDL_AcquireGPUCommandBuffer(device) : nullptr;
	if (ready && !cmd)
	{
		if (transfer) SDL_ReleaseGPUTransferBuffer(device, transfer);
		result = failed;
	}
	if (cmd)
	{
		order_draw(cmd);
		SDL_GPUColorTargetInfo color_info = {};
		color_info.texture = color;
		// a part leaves the rest of the target as it was
		color_info.load_op = band ? SDL_GPU_LOADOP_LOAD : SDL_GPU_LOADOP_CLEAR;
		color_info.store_op = SDL_GPU_STOREOP_STORE;
		if (samples)
		{
			// drawn with the samples, and the target gets what they come to
			color_info.texture = samples;
			color_info.resolve_texture = color;
			color_info.store_op = SDL_GPU_STOREOP_RESOLVE;
		}
		SDL_GPUDepthStencilTargetInfo depth_info = {};
		depth_info.texture = depth;
		depth_info.clear_depth = 1.0f;
		depth_info.load_op = SDL_GPU_LOADOP_CLEAR;
		depth_info.store_op = SDL_GPU_STOREOP_DONT_CARE;
		depth_info.stencil_load_op = SDL_GPU_LOADOP_DONT_CARE;
		depth_info.stencil_store_op = SDL_GPU_STOREOP_DONT_CARE;
		float target_size[4] = {(float)w, (float)h, 0.0f, 0.0f};
		auto pass = SDL_BeginGPURenderPass(cmd, &color_info, 1, flat ? nullptr : &depth_info);
		if (band)
		{
			SDL_Rect rows = {0, (int)y, (int)w, (int)(y1 - y)};
			SDL_SetGPUScissor(pass, &rows);
		}
		for (uint64_t i = 0; i < good; ++i)
		{
			auto d = &draws[i];
			if (!d->mesh || !d->pair->stride) continue;
			SDL_GPUTextureSamplerBinding bindings[max_pair_textures];
			bool have = true;
			for (int j = 0; have && j < d->pair->num_textures; ++j)
			{
				// a picture that is not there, and the thing is not drawn
				Picture *picture;
				SDL_memcpy(&picture, d->textures + 8 * j, 8);
				if (!picture) have = false;
				else bindings[j].texture = picture->texture;
				bindings[j].sampler = tris_sampler;
			}
			if (!have) continue;
			SDL_BindGPUGraphicsPipeline(pass, d->pair->pipeline);
			SDL_GPUBufferBinding binding = {d->mesh->buffer, 0};
			SDL_BindGPUVertexBuffers(pass, 0, &binding, 1);
			SDL_PushGPUVertexUniformData(cmd, 0, d->vblock, (Uint32)d->vlen);
			SDL_PushGPUFragmentUniformData(cmd, 0, d->pblock, (Uint32)d->plen);
			SDL_PushGPUFragmentUniformData(cmd, 1, target_size, sizeof(target_size));
			if (d->pair->num_textures) SDL_BindGPUFragmentSamplers(pass, 0, bindings, d->pair->num_textures);
			SDL_DrawGPUPrimitives(pass, d->mesh->floats / d->pair->stride, 1, 0, 0);
		}
		SDL_EndGPURenderPass(pass);
		if (gui)
		{
			gui_hooks->drawn(handle, SDL_SubmitGPUCommandBufferAndAcquireFence(cmd));
			// a ticket of it too, done says when the GPU is over it
			auto t = (Ticket*)SDL_malloc(sizeof(Ticket));
			t->fence = nullptr;
			t->transfer = nullptr;
			t->size = 0;
			t->gui = handle;
			result = t;
		}
		else
		{
			// read back in the draw's own command buffer, so it waits for it
			auto copy = SDL_BeginGPUCopyPass(cmd);
			SDL_GPUTextureRegion from = {};
			from.texture = color;
			from.w = w;
			from.h = h;
			from.d = 1;
			SDL_GPUTextureTransferInfo to = {};
			to.transfer_buffer = transfer;
			to.pixels_per_row = w;
			to.rows_per_layer = h;
			SDL_DownloadFromGPUTexture(copy, &from, &to);
			SDL_EndGPUCopyPass(copy);
			auto t = ticket_of(cmd, transfer, w * h * 4);
			result = t ? t : failed;
		}
	}
	SDL_free(draws);
	return result;
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
	(void*)host_gpu_pair_create,
	(void*)host_gpu_pair_destroy,
	(void*)host_gpu_mesh_create,
	(void*)host_gpu_mesh_destroy,
	(void*)host_gpu_picture_create,
	(void*)host_gpu_picture_destroy,
	(void*)host_gpu_target_create,
	(void*)host_gpu_target_destroy,
	(void*)host_gpu_draw,
};

#endif
#endif
