GraphicEx library
=================

GraphicEx is an addendum to Delphi's Graphics.pas to enable your
application to load many common image formats. This library is
primarily designed to load images as background (buttons, forms,
toolbars) and textures (DirectX, OpenGL) or for image browsing
and editing purposes as long as you don't need to save images.
Currently only TTargaGraphic also supports saving an image.
GraphicEx is open source under the Mozilla Public License (MPL).
Take a look below to see the image formats which can be used in
connection with this software.
 

* **TIFF images (*.tif; *.tiff)**, extended base line implementation
    + 1..16 bits per sample
    + indexed, grayscale, RGB(A), CMYK, L*a*b*
    + uncompressed, packed bits, LZW, CCITT T.4, Thunderscan, Deflate, new style JPEG
* **GFI fax images (*.fax)**, uses TTIFFGraphic to read
* **SGI images (*.bw, *.rgb, *.rgba, *.sgi)**
    + 1..16 bits per sample
    + indexed, grayscale, RGB(A)
    + uncompressed, RLE
* **Autodesk images files (*.cel; *.pic)**, old style only
    + 8 bits per sample, indexed and uncompressed
* **Truevision images (*.tga; *.vst; *.icb; *.vda; *.win)**, write support included
    + 5 and 8 bits per sample
    + grayscale, indexed, 15 bits RGB (555), 24 bits RGB(A)(888)
    + uncompressed, RLE
* **ZSoft Paintbrush images (*.pcx, *.pcc; *.scr)**
    + 1..8 bits per sample
    + grayscale, indexed, RGB
    + uncompressed, RLE
* **Kodak Photo-CD images (*.pcd)**
    + 8 bits per sample in YCbCr in any resolution (192 x 128 up to 6144 x 4096)
* **Portable pixel/gray map images (*.ppm, *.pgm, *.pbm)**
    + 1 and 8 bits per sample
    + grayscale, indexed, RGB uncompressed
* **Dr. Halo images (*.cut, *.pal)**
    + 8 bits per sample indexed, RLE compressed
* **CompuServe images (*.gif)**
    + 1, 4, 8 bits per sample indexed, LZW compressed
* **SGI Alias/Wavefront images (*.rla, *.rpf)**
    + 8 bits per sample RGB(A), RLE compressed
* **Standard Windows bitmap images (*.bmp, *.rle, *.dib)**
* **Photoshop images (*.psd, *.pdd)**
    + 1, 8, 16 bits per sample
    + indexed, RGB, CMYK, CIE L*a*b*
    + uncompressed and packed bits
* **Paintshop Pro images (*.psp)**
    + 1, 4, 8 bits per sample
    + indexed, grayscale, RGB
    + uncompressed, RLE and LZ77
    + single-layered files only!
* **Portable network graphic images (*.png)**
    + 1, 2, 4, 8, 16 bits per sample
    + indexed, grayscale alpha, RGB(A), LZ77 compressd

License:
--------
GraphicEx is released under the Mozilla Public License 1.1 (MPL 1.1).

This library was written by Mike Lischke.
This fork is currently maintained and updated by Jacob Boerema.

Current implementation
----------------------
* GraphicEx was first updated to the last svn version I found except for parts
that I needed to change to make it working with the .obj files I had.
* I have not copied the translated strings. I consider it better to use a
different translation strategy, I recommend using dxgettext.
* I have incorporated all bugfixes mentioned in the softgems forums and those
found elsewhere on the internet.
* Several other bugs fixed, a few extra security checks and some extra 
functionality added by myself.

Todo
----
* Make Unicode safe. Unicode aware fixes have not been added yet.
* Make 64 bit safe and add pure pascal alternatives to asm functions.
* Update libtif version (currently using an old libtif 3.5.7) and probably
* at the same time change to using LibTifDelphi.
* Integrate jpeg handling more into graphicex.
* Support more different tiff image types.
* Add unit tests.

Folder structure
----------------
graphicex
- examples  Folder with examples.
- external  Folder with units that interface with external libraries.
- obj       Folder with compiled .obj files for static linked libraries.
- src       Folder with GraphicEx source files.

Note that the .obj files are not available in the hg repository itself.
They need to be downloaded from:
https://bitbucket.org/jacobb/jgb-thirdparty/downloads/obj.zip
