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
    + indexed(A), grayscale(A), RGB(A), CMYK(A), L*a*b*(A), ...
    + uncompressed, packed bits, LZW, CCITT T.4, Thunderscan, Deflate, new and old style JPEG
    + uses libtiff version 4.0.4 or 3.9.7
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
* **Jpeg images (.jpeg, .jpg, .jpe, .jfif)**
	+ 8 bits per sample
    + grayscale, RGB, CMYK
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
* **Maya images (.iff)**
    + 8 bits per sample
	+ RGB(A)
	+ RLE, uncompressed
* **Amiga ilbm and pbm images (.ilbm, .lbm, .pbm, .iff)**
    + 1-8 bits per sample; 1-8, 24, 32 planes
	+ RGB(A), Indexed(A), Ham, Extra HalfBrite, Sham, Ctbl
	+ RLE, uncompressed
* **Dr. Halo images (.cut, .pal)**
    + 8 bits per sample
	+ indexed
	+ RLE compressed
* **Autodesk Animator images files (.cel; .pic)**, old style only    
    + 8 bits per sample
	+ indexed
	+ uncompressed
* **Arts & Letters images (.ged)**
    + only the embedded thumbnail images can be loaded.
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
* LibTiff updated to version 4.0.4, libjpeg 6b, zlib 1.2.8. The C source code
and necessary MQ patches have also been added to the repository.
* Delphi 32 bits and Lazarus/Free Pascal 32 and 64 bits Windows compatible.

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
Currently there are two versions. The first includes zlib, libjpeg and libtiff 3.9.7;
and the second includes zlib, libjpeg and libtiff 4.0.4.    

Downloads (32 bits only):    

* Version one: [bitbucket](https://bitbucket.org/jacobb/graphicex/downloads/obj\_libtiff\_397.zip)    
* Version two: [bitbucket](https://bitbucket.org/jacobb/graphicex/downloads/obj\_libtiff\_404.zip)    

The .a lib files needed for Fpc/Lazarus you will have to make yourself although
libz and libcrtdll can be copied/used from your MingW installation.
