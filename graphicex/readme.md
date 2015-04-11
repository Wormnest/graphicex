GraphicEx library
=================

GraphicEx is an addendum to Graphics.pas to enable your
application to load many common image formats. This library is
primarily designed to load images as background (buttons, forms,
toolbars) and textures (DirectX, OpenGL) or for image browsing
and editing purposes as long as you don't need to save images.
Currently only TTargaGraphic also supports saving an image.
GraphicEx is open source under the Mozilla Public License (MPL).

Supported image formats
-----------------------

* **TIFF images (.tif; .tiff)**
    + 1..64 bits per sample, including 16, 24, 32 and 64 bits floating point
    + indexed(A), grayscale(A), RGB(A), CMYK(A), L*a*b*, ...
    + uncompressed, packed bits, LZW, CCITT T.4, Thunderscan, Deflate, new and old style JPEG
    + uses libtiff version 3.9.7
* **Photoshop images (.psd, .pdd)**
    + 1, 8, 16 bits per sample
    + indexed, RGB, CMYK, CIE L*a*b*
    + uncompressed and packed bits
* **Paintshop Pro images (.psp)**
    + 1, 4, 8 bits per sample
    + indexed, grayscale, RGB
    + uncompressed, RLE and LZ77
* **Gimp XCF images (.xcf)**
    + 1, 8 bits per sample
    + indexed(A), grayscale(A), RGB(A)
    + uncompressed, RLE
* **Portable network graphic images (.png)**
    + 1, 2, 4, 8, 16 bits per sample
    + indexed(A), grayscale(A), RGB(A)
	+ LZ77 compressed
* **Gif images (.gif)**
    + 1, 4, 8 bits per sample
	+ indexed
	+ LZW compressed
* **Truevision images (.tga; .vst; .icb; .vda; .win)**, write support included
    + 5 and 8 bits per sample
    + grayscale, indexed, 15 bits RGB (555), 24 bits RGB(A)(888)
    + uncompressed, RLE
* **Kodak Photo-CD images (.pcd)**
    + 8 bits per sample in YCbCr in any resolution (192 x 128 up to 6144 x 4096)
* **Portable pixel/gray map images (.ppm, .pgm, .pbm)**
    + 1 and 8 bits per sample
    + grayscale, indexed, RGB
	+ uncompressed
* **ZSoft Paintbrush images (.pcx, .pcc; .scr)**
    + 1..8 bits per sample
    + grayscale, indexed, RGB
    + uncompressed, RLE
* **GFI fax images (.fax)**
	+ uses the Tiff image reading class
* **EPS images (.eps)**
	+ only .eps images that have embedded pixel graphics in TIF format.
* **SGI images (.bw, .rgb, .rgba, .sgi)**
    + 1..16 bits per sample
    + indexed, grayscale, RGB(A)
    + uncompressed, RLE
* **SGI Alias/Wavefront images (.rla, .rpf)**
    + 8 bits per sample
	+ RGB(A)
	+ RLE compressed
* **Dr. Halo images (.cut, .pal)**
    + 8 bits per sample
	+ indexed
	+ RLE compressed
* **Autodesk images files (.cel; .pic)**, old style only    
    _Should be considered deprecated since we don't have any sample images to
    test with nor any documentation of this format._
    + 8 bits per sample
	+ indexed
	+ uncompressed
* **Arts & Letters images (.ged)**    
    _Should be considered deprecated since we don't have any sample images to
    test with nor any documentation of this format._
    + indexed, RGB(A)

Image formats that can be used via a wrapper class
--------------------------------------------------

* **Windows bitmap images (.bmp, .rle, .dib)**
    + grayscale, indexed, RGB(A)
* **Jpeg images (.jpeg, .jpg, .jpe, .jfif)**
    + grayscale, RGB

License:
--------
GraphicEx is released under the Mozilla Public License 1.1 (MPL 1.1).
Some parts, added by Jacob Boerema, have a dual license: MPL 1.1 and
LGPL 2.1 with linking exception (the "FPC modified LGPL License").

This library was written by Mike Lischke.
This fork is currently maintained, updated and extended by Jacob Boerema.

Bug reports
-----------
Please report bugs in the issuetracker using the link below. Bugs concerning
problems loading a certain image should add the problematic image.
Without an example image it will be most likely impossible to find
what's wrong.    
Link: [issuetracker](https://bitbucket.org/jacobb/jgb-thirdparty/issues?status=new&status=open)

Current implementation
----------------------
* GraphicEx was first updated to the last svn version I found except for parts
that I needed to change to make it working with the .obj files I had.
* I have not copied the translated strings. I consider it better to use a
different translation strategy, I recommend using dxgettext.
* I have incorporated all bugfixes mentioned in the softgems forums and those
found elsewhere on the internet.
* A lot of other bugs have been fixed by me, a few extra security checks and 
extra functionality added by myself.
* I added a Gimp XCF image loader.
* Unicode aware fixes have been added, however it needs testing by someone
that has a Unicode version of Delphi.
* LibTiff updated to version 3.9.7, libjpeg 6b, zlib 1.2.8. The C source code
and necessary MQ patches have also been added to the repository.

Additions March/April 2015
--------------------------
* Changes to make it compatible with Fpc/Lazarus on Win32.
* Fixes to gamma correction.
* Several fixes and improvements to the ColorManager and reading image format
* Added a few more DUnit tests and made the tests compatible with FpcUnit.
* Improvements to the ImageViewer example and made it compatible with Lazarus.

Todo
----
* Make 64 bit safe and add pure pascal alternatives to asm functions.
* Integrate jpeg handling more into graphicex.
* Add more unit tests.
* Move all code for setting target options from GraphicEx to the ColorManager and
  make that part overrideable. This will make it easier to override how we want
  a certain format converted and to select a target format.

Folder structure
----------------
\external = Folder with units that interface with external libraries.    
\graphicex    
- \examples = Folder with examples.    
- \obj = Folder with compiled .obj files for static linked libraries.    
- \src = Folder with GraphicEx source files.    
\packages = Contains the Lazarus packages.

Note that the .obj files needed for Delphi are not available in the hg repository itself.
They need to be downloaded from [bitbucket](https://bitbucket.org/jacobb/jgb-thirdparty/downloads/obj_libtiff_397.zip)

The .a lib files needed for Fpc/Lazarus you will have to make yourself although
libz and libcrtdll can be copied/used from your MingW installation.
