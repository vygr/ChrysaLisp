#include "pii.h"
#include <stdint.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <iostream>

#ifdef _WIN64
#include <io.h>
#else
#include <unistd.h>
#endif

#define VP64_STACK_SIZE 8192
extern int vp64(uint8_t* data, int64_t *stack, int64_t *argv, int64_t *host_os_funcs, int64_t *host_gui_funcs);
extern bool run_emu;

extern struct stat fs;
extern int64_t pii_open(const char *path, uint64_t mode);
extern uint64_t pii_close(uint64_t fd);
extern void *pii_mmap(size_t len, int64_t fd, uint64_t mode);
extern int64_t pii_munmap(void *addr, size_t len, uint64_t mode);
extern int64_t pii_mprotect(void *addr, size_t len, uint64_t mode);
extern void *pii_flush_icache(void* addr, size_t len);
extern void (*host_os_funcs[]);

#ifdef _HOST_GUI
extern void (*host_gui_funcs[]);
#endif

int main(int argc, char *argv[])
{
	int ret_val = 0;
	if (argc > 1)
	{
		//check for -e option
		for (int i = 0; i < argc; ++i)
		{
			if (!strcmp(argv[i], "-e"))
			{
				//override boot image to emu image
				run_emu = true;
				argv[1] = (char*)"obj/vp64/VP64/sys/boot_image";
				break;
			}
		}

		int64_t fd = pii_open(argv[1], file_open_read);
		if (fd != -1)
		{
			stat(argv[1], &fs);
			size_t data_size = fs.st_size;
			uint16_t *data = (uint16_t*)pii_mmap(data_size, -1, mmap_data);
			if (data != (uint16_t*)-1)
			{
				if (read((int)fd, data, data_size) == data_size)
				{
					pii_close((int)fd);
				#ifndef _WIN64
					fcntl(0, F_SETFL, fcntl(0, F_GETFL, 0) | O_NONBLOCK);
				#endif
					if (run_emu)
					{
						int64_t* stack = (int64_t*)pii_mmap(VP64_STACK_SIZE, -1, mmap_data);
						if (stack)
						{
						#ifdef _HOST_GUI
							ret_val = vp64((uint8_t*)data, (int64_t*)((char*)stack + VP64_STACK_SIZE), (int64_t*)argv, (int64_t*)host_os_funcs, (int64_t*)host_gui_funcs);
						#else
							ret_val = vp64((uint8_t*)data, (int64_t*)((char*)stack + VP64_STACK_SIZE), (int64_t*)argv, (int64_t*)host_os_funcs, (int64_t*)nullptr);
						#endif
							pii_munmap(stack, VP64_STACK_SIZE, mmap_data);
						}
					}
					else
					{
						//swap to RX
						pii_flush_icache(data, data_size);
						pii_mprotect(data, data_size, mmap_exec);
					#ifdef _HOST_GUI
						ret_val = ((int(*)(char* [], void* [], void* []))((char*)data + data[5]))(argv, host_os_funcs, host_gui_funcs);
					#else
						ret_val = ((int(*)(char* [], void* [], void* []))((char*)data + data[5]))(argv, host_os_funcs, nullptr);
					#endif
					}
					pii_munmap(data, data_size, mmap_exec);
				}
				else
				{
					pii_close((int)fd);
					std::cout << "Error, failed reading boot_image!" << std::endl;
				}
			}
			else
			{
				pii_close((int)fd);
				std::cout << "Error, READ/WRITE/EXEC pages failed!" << std::endl;
			}
		}
		else std::cout << "Error, boot_image not found!" << std::endl;
	}
	else std::cout << "Error, no boot_image arg!" << std::endl;
	return ret_val;
}
