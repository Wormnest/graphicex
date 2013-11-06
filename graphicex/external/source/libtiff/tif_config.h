/* jb We need to add some extra defines that would normally be defined by configure. */

/* Delphi/Object Pascal specific for use in tif_ojpeg.c (see explanation there) */
#define LIBJPEG_ENCAP_EXTERNAL

/*
 * On windows you should define USE_WIN32_FILEIO if you are using tif_win32.c
 * or AVOID_WIN32_FILEIO if you are using something else (like tif_unix.c).
 *
 * By default tif_unix.c is assumed.
 */
#define AVOID_WIN32_FILEIO 1

/* Support ISO JBIG compression (requires JBIG-KIT library) */
#undef JBIG_SUPPORT // jb Maybe look into supporting this in the future.

/* Support JPEG compression (requires IJG JPEG library) */
#define JPEG_SUPPORT 1

/* Support Old JPEG compresson (read-only) */
#define OJPEG_SUPPORT 1

/* Support LogLuv high dynamic range encoding */
#define LOGLUV_SUPPORT 1

/* Support LZW algorithm */
#define LZW_SUPPORT 1

/* Support NeXT 2-bit RLE algorithm */
#define NEXT_SUPPORT 1

/* Support Macintosh PackBits algorithm */
#define PACKBITS_SUPPORT 1

/* Support Pixar log-format algorithm (requires Zlib) */
#define PIXARLOG_SUPPORT 1

/* Support ThunderScan 4-bit RLE algorithm */
#define THUNDER_SUPPORT 1

/* Support CCITT Group 3 & 4 algorithms */
#define CCITT_SUPPORT 1

/* Support Deflate compression */
#define ZIP_SUPPORT 1

/* jb End of added defines. */ 
/* --------------------------------------------------------------------------------- */

/* Define to 1 if you have the <assert.h> header file. */
#define HAVE_ASSERT_H 1

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define as 0 or 1 according to the floating point format suported by the
   machine */
#define HAVE_IEEEFP 1

/* Define to 1 if you have the `jbg_newlen' function. */
#define HAVE_JBG_NEWLEN 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <io.h> header file. */
#define HAVE_IO_H 1

/* Define to 1 if you have the <search.h> header file. */
#define HAVE_SEARCH_H 1

/* Define to 1 if you have the `setmode' function. */
#define HAVE_SETMODE 1

/* The size of a `int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of a `long', as computed by sizeof. */
#define SIZEOF_LONG 4

/* Signed 64-bit type */
#define TIFF_INT64_T signed __int64

/* Unsigned 64-bit type */
#define TIFF_UINT64_T unsigned __int64

/* Set the native cpu bit order */
#define HOST_FILLORDER FILLORDER_LSB2MSB

/* Define to 1 if your processor stores words with the most significant byte
   first (like Motorola and SPARC, unlike Intel and VAX). */
/* #undef WORDS_BIGENDIAN */

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
# ifndef inline
#  define inline __inline
# endif
#endif

#define lfind _lfind
/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 8
 * fill-column: 78
 * End:
 */
