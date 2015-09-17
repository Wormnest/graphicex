GraphicEx repository
====================

Overview of parts
-----------------

1. graphicex
------------
This is my fork of the latest version of graphicex that I could find
(inside the VirtualTreeView repository) with all bugfixes that I found
online applied and a lot of others fixed by me.
Besides that many changes and enhancements to the code have been made,
especially in PSP and TIFF loading.

I have also added support for loading Gimp XCF files, jpeg images, an ImageViewer example, some unit tests, and Fpc/Lazarus Win32/Win64 compatibility.
For more details see the [graphicex](/graphicex/readme.md) folder.

2. external
-----------
Interface code for linking with external C libraries and C rtl replacement
functions. Used by grapicex but can also be used separately.
Currently available here are zlib, libtiff and libjpeg.    

The .obj files needed by Delphi for zlib, JPG and TIFF support are available
as a separate download since versioning them in hg doesn't work well.    
Downloads:    
https://bitbucket.org/jacobb/graphicex/downloads/obj\_libtiff\_397.zip    
https://bitbucket.org/jacobb/graphicex/downloads/obj\_libtiff\_404.zip    

GraphicEx is now also working with Fpc/Lazarus Win32 and Win64. However, you
will have to compile the libraries yourself for now.


August 2013 - September 2015
Jacob Boerema
