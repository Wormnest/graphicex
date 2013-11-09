Opensource thirdparty components
================================

This repository contains my forks of several opensource components that
I'm using.
Compared to the originals they have been changed to my needs and where
needed bugs have been fixed.
Other bug fixes, comments and patches with new features are welcome.
However be aware that this is a hobby project of mine and I'm
still using Delphi 6. To be able to integrate your patches they will
need to be made compatible with Delphi 6.

Overview of components
----------------------

1. graphicex
------------
This is my fork of the latest version of graphicex that I could find
with all bugfixes that I found online applied and a lot of others fixed
by me.
Besides that some changes and enhancements to the code have been made,
especially in PSP and TIFF loading.
I have also added support for loading Gimp XCF files.
The .obj files needed for JPG and TIFF support are available as a
separate download since versioning them in hg doesn't work well.
https://bitbucket.org/jacobb/jgb-thirdparty/downloads/obj_libtiff_397.zip

2. ghostscript
--------------
A version of gsapi.pas which is an interface to the Ghostscript API
adapted to my needs.

3. twain-wilson
---------------
A twain api interface made by Colin Wilson with bugfixes by me and
further adapted to my needs.

August-November 2013
Jacob Boerema
