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

I have also added support for loading Gimp XCF files, jpeg images,
Maya IFF images, Amiga ilbm/pbm images, an ImageViewer example, 
some unit tests, and Fpc/Lazarus Win32/Win64 compatibility.    
Recently I have started adding color management support using LCMS.    
For more details see the [graphicex](/graphicex/readme.md) folder.

2. external
-----------
Interface code for linking with external C libraries and C rtl replacement
functions. Used by grapicex but can also be used separately.
Currently available here are zlib, libtiff and libjpeg.    

The .obj files needed by Delphi 32 bits for zlib, JPG and TIFF support are
available as a separate download since versioning them in hg doesn't work well.    
Downloads (32 bits versions only):    
https://bitbucket.org/jacobb/graphicex/downloads/obj\_libtiff\_407.zip    

Also available is the 32 bits version of lcms2.dll to be used when color
profiling using lcms is turned on.    
https://bitbucket.org/jacobb/graphicex/downloads/lcms2%20version%202.8,%2032bits.zip    


GraphicEx is now also working with Fpc/Lazarus Win32 and Win64. However, you
will have to compile the libraries yourself for now.

3.include
---------
Contains compilers.inc used both in graphicex and external.

4. packages
-----------
Contains Delphi Berlin and Lazarus packages including packages used for the
ImageViewer example.

5. tests
--------
Contains project that has partial tests for the GraphicEx library.
There are also tests that can read whole folders of images but
you will need to adapt some paths to make it work on your system.    
In the dev folder is a project to make default test expectations
for all images under a root path.    
Publishing some of the test images is still on my todo list.


August 2013 - March 2017
Jacob Boerema
