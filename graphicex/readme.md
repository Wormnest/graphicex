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
    + RGB(A), Grayscale(A), Indexed(A), CMYK(A), L*a*b*(A), YCbCr, ICCLab, ITULab, CIELog2L, CIELog2Luv
    + 1..64 bits per sample, including 16, 24, 32 and 64 bits floating point
    + Uncompressed, packbits, LZW, CCITT T.4, Thunderscan, Deflate, new and old style JPEG, etc.
    + Uses libtiff version 4.0.7
* **Photoshop images (.psd, .pdd)**
    + RGB(A), Indexed(A), Grayscale(A), CMYK(A), CIE L*a*b*
    + 1, 8, 16 bits per sample integer, 32 bits per sample float
    + Uncompressed, packbits
	+ Reads the combined image
	+ color profiles can be read and used except for indexed
* **Paintshop Pro images (.psp)**
    + RGB(A), Indexed, Grayscale
    + 1, 4, 8, 16 bits per sample
    + Uncompressed, RLE and LZ77
* **Gimp XCF images (.xcf)**
    + RGB(A), Indexed(A), Grayscale(A)
    + 1, 8 bits per sample
    + Uncompressed, RLE
* **Jpeg images (.jpeg, .jpg, .jpe, .jfif)**
    + RGB, Grayscale, CMYK
	+ 8 bits per sample
	+ Uses libjpeg
* **Portable network graphic images (.png)**
    + RGB(A), Indexed(A), Grayscale(A)
    + 1, 2, 4, 8, 16 bits per sample
	+ LZ77 compressed
	+ Color profiles can be read and used except for interlaced images
* **Gif images (.gif)**
	+ Indexed
    + 1, 4, 8 bits per sample
	+ LZW compressed
	+ All image frames can be read (but not animated)
* **Truevision images (.tga; .vst; .icb; .vda; .win)**, write support included
    + 24 bits RGB(A)(888), 15 bits RGB (555), Grayscale, Indexed
    + 5 and 8 bits per sample
    + Uncompressed, RLE
* **Kodak Photo-CD images (.pcd)**
    + 8 bits per sample in YCbCr in any resolution (192 x 128 up to 6144 x 4096)
* **Portable pixel/gray/bw map images (.ppm, .pgm, .pbm)**
    + RGB, Grayscale, B/W
    + 1..16 bits per sample
	+ Uncompressed
* **ZSoft Paintbrush images (.pcx, .pcc)**
    + RGB, Indexed (including CGA palette images), Grayscale
    + 1..8 bits per sample
    + Uncompressed, RLE
	+ Also reads obsolete Word for Dos screen capture images that are very similar to pcx (.scr)
* **GFI fax images (.fax)**
	+ Uses the Tiff image reading class
* **EPS images (.eps)**
	+ Only .eps images that have embedded pixel graphics in TIF format.
* **SGI images (.bw, .rgb, .rgba, .sgi)**
    + RGB(A), Grayscale(A)
    + 8, 16 bits per sample
    + RLE, uncompressed
* **SGI Alias/Wavefront images (.rla, .rpf)**
	+ RGB(A), Grayscale(A)
    + 1..16 bits per sample integer, 32 bits per sample float
	+ RLE compressed, Uncompressed (for float only)
* **Maya images (.iff)**
	+ RGB(A)
    + 8 bits per sample
	+ RLE, Uncompressed
* **Amiga ilbm and pbm images (.ilbm, .lbm, .pbm, .iff)**
	+ RGB(A), Indexed(A), Ham, Extra HalfBrite, Sham, Ctbl, Rgb8, Rgbn
    + 1-8 bits per sample; 1-8, 24, 32 planes
	+ RLE, RGBN RLE, Uncompressed
* **Dr. Halo images (.cut, .pal)**
	+ Indexed
    + 8 bits per sample
	+ RLE compressed
* **Autodesk Animator images files (.cel; .pic)**, old style only    
	+ Indexed
    + 8 bits per sample
	+ Uncompressed
* **Arts & Letters images (.ged)**
    + Only the embedded thumbnail images can be loaded.
    + RGB, Indexed

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
problems loading a certain image should add an example of a problematic image.
Without an example image it will be most likely impossible to find
what's wrong.    
Link: [issuetracker](https://bitbucket.org/jacobb/graphicex/issues?status=new&status=open)

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
* Unicode aware fixes have been added, tested in Delphi 10.1 Berlin.
* LibTiff updated to version 4.0.7, libjpeg 6b, zlib 1.2.8. The C source code
and necessary MQ patches have also been added to the repository.
* Delphi 32 bits and Lazarus/Free Pascal 32 and 64 bits Windows compatible.
* A start has been made with adding color profile support.

Additions March/April 2015
--------------------------
* Changes to make it compatible with Fpc/Lazarus on Win32.
* Fixes to gamma correction.
* Several fixes and improvements to the ColorManager and reading image format
* Added a few more DUnit tests and made the tests compatible with FpcUnit.
* Improvements to the ImageViewer example and made it compatible with Lazarus.

Additions July/August 2015
--------------------------
* Support for LibTiff 4.x.
* Lazarus Win64 compatibility.
* Added image reader tests.

Additions September 2015
------------------------
* Jpeg reader class using LibJpeg or LibJpeg-Turbo.
* Fix TIFF and PSD CIELab with alpha images.
* Fix PSD 16 bit CMYK images.
* Minor improvements to RLA/RPF, CEL/PIC.

Additions October 2015
----------------------
* Maya IFF and Amiga ILBM/PBM image reader classes added.
* Better detection of overflow errors in Packbits and Targa RLE decoders.
* ImageViewer example: show also images marked as hidden.

Additions December 2016 - January 2017
--------------------------------------
* Make it run on Delphi Unicode versions.
* Add Delphi Berlin packages (only 32 bits supported)

Additions February 2017
-----------------------
* 32 bit PSD support.
* Optional handling of included ICC color profiles for most PSD and PNG images.
  However this does need the lcms2.dll.

Additions March 2017
--------------------
* All compression decoders now have safety checks against buffer overflows
  and decoder tests were added to the test suite.
* Improvements to the GIF, PCX, RLA and SGI image readers.
* Update libtiff to version 4.0.7.

Todo
----
* Make it work for Delphi Windows 64 bit target. (Should only need small changes
  since it is already working for Lazarus 64 bits, however I don't have a Delphi
  version that can make 64 bit builds.)
* Make 64 bit version of the .obj files.
* Add pure pascal alternatives to asm functions.
* Add more unit tests.
* Move all code for setting target options from GraphicEx to the ColorManager and
  make that part overrideable. This will make it easier to override how we want
  a certain format converted and to select a target format.
* Move info about additions to a separate changelog.
* It would be nice if we could also choose using external libraries instead of
  linking in .obj files.

Folder structure
----------------
\external = Folder with units that interface with external libraries.    
\graphicex    
- \examples = Folder with examples.    
- \obj = Folder with compiled .obj files for static linked libraries.    
- \src = Folder with GraphicEx source files.    
\packages = Contains the Lazarus packages.

Note that the .obj files needed for Delphi are not available in the hg repository itself.
Only the 32 bits object files are available here as a separate download. The zip file includes
zlib 1.2.8, libjpeg 6b and libtiff 4.0.7.    

Downloads (32 bits only):    

* Object files: [bitbucket](https://bitbucket.org/jacobb/graphicex/downloads/obj\_libtiff\_407.zip)    

* lcms2, version 2.8:[bitbucket](https://bitbucket.org/jacobb/graphicex/downloads/lcms2%20version%202.8,%2032bits.zip)    

The .a lib files needed for Fpc/Lazarus you will have to make yourself although
libz and libcrtdll can be copied/used from your MingW installation.
