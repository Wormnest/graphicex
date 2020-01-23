@echo off

echo Delete all obj files
cd obj64
del /Q *.obj
cd ..

echo Add Microsoft cl.exe path
setlocal
:This will make sure that path to cl.exe and other necessary stuff for x64 is set
call "d:\Programs\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvarsall.bat" x86_amd64
set


:echo Microsoft cl.exe commandline - Compiling with 
: /c Compile without linking
: /O2 Compile for speed
: /TC All source files are C files
: /GS- Turn off buffer overrun detections since it leads to unresolved externals when linking in Delphi
: /EHa The /EHa compiler option is used to support asynchronous structured exception handling (SEH) ...

: OLD STUFF
: -u- no underscores
: -RT- no runtime type info
: delphizlib uses also flags (probably for newer BCC) -Ve -X -pr -b -d -k -vi -tWM
: Check to see if we should add some of those...

:zlib
echo Compiling zlib...
cd zlib
:del zlib_errors.txt
cl /O2 /GS- /EHar /D_NO_CRT_STDIO_INLINE /Fo..\obj64\ /TC *.c /c >..\zlib_errors.txt
:echo Errorlevel = %errorlevel%
if errorlevel 1 goto zliberror
cd ..

:libjpeg
echo Compiling LibJpeg...
:del libjpeg_errors.txt
cd libjpeg
cl /O2 /GS- /EHar /D_NO_CRT_STDIO_INLINE /Fo..\obj64\ /TC jutils.c jcapimin.c jcapistd.c jccoefct.c jccolor.c jcdctmgr.c jchuff.c jcinit.c jcmainct.c jcmarker.c jcmaster.c jcomapi.c jcparam.c jcphuff.c jcprepct.c jcsample.c jctrans.c jdapimin.c jdapistd.c jdatadst.c jdatasrc.c jdcoefct.c jdcolor.c jddctmgr.c jdhuff.c jdinput.c jdmainct.c jdmarker.c jdmaster.c jdmerge.c jdphuff.c jdpostct.c jdsample.c jdtrans.c jerror.c jfdctflt.c jfdctfst.c jfdctint.c jidctflt.c jidctfst.c jidctint.c jidctred.c jmemmgr.c jmemnobs.c jquant1.c jquant2.c /c >..\libjpeg_errors.txt
:echo Errorlevel = %errorlevel%
if errorlevel 1 goto libjpegerror
cd ..

:libtiff
echo Compiling LibTiff 4...
:del libtiff_errors.txt
cd libtiff4
cl /O2 /GS- /EHar /D_NO_CRT_STDIO_INLINE /I..\zlib /I..\libjpeg /Fo..\obj64\ /TC tif_aux.c tif_close.c tif_codec.c tif_color.c tif_compress.c tif_dir.c tif_dirinfo.c tif_dirread.c tif_dirwrite.c tif_dumpmode.c tif_error.c tif_extension.c tif_fax3.c tif_fax3sm.c tif_flush.c tif_getimage.c tif_jpeg.c tif_luv.c tif_lzw.c tif_next.c tif_ojpeg.c tif_open.c tif_packbits.c tif_pixarlog.c tif_predict.c tif_print.c tif_read.c tif_strip.c tif_swab.c tif_thunder.c tif_tile.c tif_version.c tif_warning.c tif_write.c tif_zip.c /c >..\libtiff_errors.txt
:echo Errorlevel = %errorlevel%
if errorlevel 1 goto libtifferror
cd ..

:no errors: then copy obj files to destination
:echo Copying obj files...

: Auto copying of the obj files disabled since we don't want to overwrite
: in case we didn't have our patches applied!

:copy obj64\*.obj ..\obj64

echo Done.
pause
del /Q *_errors.txt
goto end2

:zliberror
cd ..
echo There was an error compiling zlib

goto end

:libjpegerror
cd ..
echo There was an error compiling LibJpeg

goto end

:libtifferror
cd ..
echo There was an error compiling LibTiff

goto end

:end
pause

:end2
