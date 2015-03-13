/* jconfig.vc --- Adapted from bcc version and generated mingw version. */
/* see jconfig.doc for explanations */

#define HAVE_PROTOTYPES
#define HAVE_UNSIGNED_CHAR
#define HAVE_UNSIGNED_SHORT

/* #define void char */
/* #define const */
#undef CHAR_IS_UNSIGNED
#define HAVE_STDDEF_H
#define HAVE_STDLIB_H
#undef NEED_BSD_STRINGS
#undef NEED_SYS_TYPES_H
#undef NEED_FAR_POINTERS	/* we presume a 32-bit flat memory model */
#undef NEED_SHORT_EXTERNAL_NAMES
#undef INCOMPLETE_TYPES_BROKEN
#define NO_GETENV     /* disable use of environment variables (and scanf) */

/* Define "boolean" as unsigned char, not int, per Windows custom */
#if defined(WIN32) && !defined(__MINGW32__)
#ifndef __RPCNDR_H__		/* don't conflict if rpcndr.h already read */
typedef unsigned char boolean;
#endif
#define HAVE_BOOLEAN		/* prevent jmorecfg.h from redefining it */
#endif

#ifdef JPEG_INTERNALS

#undef RIGHT_SHIFT_IS_UNSIGNED

#if defined(__MINGW32__)
#define INLINE __inline__
/* These are for configuring the JPEG memory manager. */
#undef DEFAULT_MAX_MEM
#undef NO_MKTEMP
#endif

#endif /* JPEG_INTERNALS */

#ifdef JPEG_CJPEG_DJPEG

#if defined(__BORLANDC__)
#undef BMP_SUPPORTED		/* BMP image file format */
#undef GIF_SUPPORTED		/* GIF image file format */
#undef PPM_SUPPORTED		/* PBMPLUS PPM/PGM image file format */
#undef RLE_SUPPORTED		/* Utah RLE image file format */
#undef TARGA_SUPPORTED		/* Targa image file format */

#define TWO_FILE_COMMANDLINE	/* optional */

#define USE_SETMODE		/* Microsoft has setmode() */
#endif

#if defined(__MINGW32__)
#define BMP_SUPPORTED		/* BMP image file format */
#define GIF_SUPPORTED		/* GIF image file format */
#define PPM_SUPPORTED		/* PBMPLUS PPM/PGM image file format */
#undef RLE_SUPPORTED		/* Utah RLE image file format */
#define TARGA_SUPPORTED		/* Targa image file format */

#undef TWO_FILE_COMMANDLINE
#endif


#undef NEED_SIGNAL_CATCHER
#undef DONT_USE_B_MODE

// GraphicEx special: want progress report.
#define PROGRESS_REPORT		/* optional */

#endif /* JPEG_CJPEG_DJPEG */
