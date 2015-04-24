Opensource thirdparty components
================================

This repository contains my forks of several open source components that
I'm using.
Compared to the originals they have been changed to my needs and where
needed bugs have been fixed.
Other bug fixes, comments and patches with new features are welcome.
However be aware that this is a hobby project of mine and I'm
still using Delphi 6. To be able to integrate your patches they will
need to be made compatible with Delphi 6.
Work has also started on making graphicex compatible with Fpc/Lazarus.
Currently only Win32 is supported.

Overview of components
----------------------

1. graphicex
------------
This is my fork of the latest version of graphicex that I could find
with all bugfixes that I found online applied and a lot of others fixed
by me.
Besides that many changes and enhancements to the code have been made,
especially in PSP and TIFF loading.
I have also added support for loading Gimp XCF files, an ImageViewer example,
and some unit tests.

2. external
-----------
Interface code for linking with external C libraries and C rtl replacement
functions. Used by grapicex but can also be used separately.
Currently available here are zlib, libtiff and libjpeg.
The .obj files needed by Delphi for zlib, JPG and TIFF support are available as a
separate download since versioning them in hg doesn't work well.
https://bitbucket.org/jacobb/jgb-thirdparty/downloads/obj_libtiff_397.zip

3. ghostscript
--------------
A version of gsapi.pas which is an interface to the Ghostscript API
adapted to my needs.

4. twain-wilson
---------------
A twain api interface made by Colin Wilson with bugfixes by me and
further adapted to my needs.

5. ruler-zijlstra
-----------------
A ruler component made by Pieter Zijlstra. We use version 1.73 since
that version is working fine for our needs. However newer versions
are available from his website.

August 2013 - April 2015
Jacob Boerema
