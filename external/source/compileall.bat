@echo off

echo Delete all obj files
cd obj
del /Q *.obj
cd ..

echo Add BCC55 to path
setlocal
set PATH=e:\Borland\BCC55\Bin;%PATH%

echo Compiling with O2 = optimized for speed [O1 = optimized for size]

:zlib
echo Compiling zlib...
cd zlib
:del zlib_errors.txt
bcc32 -6 -u- -RT- -d- -O2 -c -n..\obj\ *.c >..\zlib_errors.txt
:echo Errorlevel = %errorlevel%
if errorlevel 1 goto zliberror
cd ..

:libjpeg
echo Compiling LibJpeg...
:del libjpeg_errors.txt
cd libjpeg
bcc32 -6 -u- -RT- -d- -O2 -c -n..\obj\ jutils.c jcapimin.c jcapistd.c jccoefct.c jccolor.c jcdctmgr.c jchuff.c jcinit.c jcmainct.c jcmarker.c jcmaster.c jcomapi.c jcparam.c jcphuff.c jcprepct.c jcsample.c jctrans.c jdapimin.c jdapistd.c jdatadst.c jdatasrc.c jdcoefct.c jdcolor.c jddctmgr.c jdhuff.c jdinput.c jdmainct.c jdmarker.c jdmaster.c jdmerge.c jdphuff.c jdpostct.c jdsample.c jdtrans.c jerror.c jfdctflt.c jfdctfst.c jfdctint.c jidctflt.c jidctfst.c jidctint.c jidctred.c jmemmgr.c jmemnobs.c jquant1.c jquant2.c >..\libjpeg_errors.txt
:echo Errorlevel = %errorlevel%
if errorlevel 1 goto libjpegerror
cd ..

:libtiff
echo Compiling LibTiff...
:del libtiff_errors.txt
cd libtiff
bcc32 -6 -u- -RT- -d- -O2 -c -I..\zlib;..\libjpeg -n..\obj\ tif_aux.c tif_close.c tif_codec.c tif_color.c tif_compress.c tif_dir.c tif_dirinfo.c tif_dirread.c tif_dirwrite.c tif_dumpmode.c tif_error.c tif_extension.c tif_fax3.c tif_fax3sm.c tif_flush.c tif_getimage.c tif_jpeg.c tif_luv.c tif_lzw.c tif_next.c tif_ojpeg.c tif_open.c tif_packbits.c tif_pixarlog.c tif_predict.c tif_print.c tif_read.c tif_strip.c tif_swab.c tif_thunder.c tif_tile.c tif_version.c tif_warning.c tif_write.c tif_zip.c >..\libtiff_errors.txt
:echo Errorlevel = %errorlevel%
if errorlevel 1 goto libtifferror
cd ..

:no errors: then copy obj files to destination
echo Copying obj files...
:copy obj\*.obj ..\obj\

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
