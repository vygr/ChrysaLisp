#pragma once

/*
 *  IMS Open Corpus Workbench (CWB)
 *  Copyright (C) 1993-2006 by IMS, University of Stuttgart
 *  Copyright (C) 2007-     by the respective contributers (see file AUTHORS)
 *
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2, or (at your option) any later
 *  version.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
 *  Public License for more details (in the file "COPYING", or available via
 *  WWW at http://www.gnu.org/copyleft/gpl.html).
 */
 /*
  * Windows/Unicode-compatibility extensions to CWB in this file
  *  Copyright (C) 2010      by ANR TextomÃ©trie, ENS de Lyon
  */


#ifndef __windows_mmap_h
#define __windows_mmap_h

#include <windows.h>
#include <sys/stat.h>
#include <stdint.h>
#include <stdio.h>
#include <io.h>


  /* macro definitions extracted from git/git-compat-util.h */
#define PROT_READ  1
#define PROT_WRITE 2
#define PROT_EXEC 4
#define MAP_FAILED ((void*)-1)

/* macro definitions extracted from /usr/include/bits/mman.h */
#define MAP_SHARED	0x01		/* Share changes.  */
#define MAP_PRIVATE	0x02		/* Changes are private.  */

#define MAP_ANON 0 // mgb

void *mmap(void *start, size_t length, int prot, int flags, int fd, off_t offset);
int munmap(void *start, size_t length);
void *mgbmmap(void *start, size_t length, int prot, int flags, int fd, off_t offset);

#include <stdint.h> // portable: uint64_t   MSVC: __int64 

// MSVC defines this in winsock2.h!?
typedef struct _timeval {
    long tv_sec;
    long tv_usec;
} timeval;

int gettimeofday(struct timeval * tp, struct timezone * tzp);


#endif