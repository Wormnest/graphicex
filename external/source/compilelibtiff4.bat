@echo off

echo Add BCC55 to path
setlocal
set PATH=e:\Borland\BCC55\Bin;%PATH%

echo Compiling with O2 = optimized for speed [O1 = optimized for size]

:libtiff4
echo Compiling LibTiff 4...
:del libtiff4_errors.txt
cd libtiff4
bcc32 -6 -a8 -u- -RT- -d- -O2 -c -I..\zlib;..\libjpeg -n..\obj\libtiff4 tif_aux.c tif_close.c tif_codec.c tif_color.c tif_compress.c tif_dir.c tif_dirinfo.c tif_dirread.c tif_dirwrite.c tif_dumpmode.c tif_error.c tif_extension.c tif_fax3.c tif_fax3sm.c tif_flush.c tif_getimage.c tif_jpeg.c tif_luv.c tif_lzw.c tif_next.c tif_ojpeg.c tif_open.c tif_packbits.c tif_pixarlog.c tif_predict.c tif_print.c tif_read.c tif_strip.c tif_swab.c tif_thunder.c tif_tile.c tif_version.c tif_warning.c tif_write.c tif_zip.c >..\libtiff4_errors.txt
:echo Errorlevel = %errorlevel%
if errorlevel 1 goto libtifferror
cd ..

echo Done.
pause
:del /Q *_errors.txt
goto end2

:libtifferror
cd ..
echo There was an error compiling LibTiff

goto end

:end
pause

:end2
