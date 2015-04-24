                          TRuler v1.73 14 may 2005
                          ========================

                    Copyright © 2003-2005 Pieter Zijlstra

Note(s):
========
1.40 - property LeftInset is replaced by a property called Inset.
       This will give a warning when you open an existing project
       with the previous version of the Ruler component. You can
       ignore this warning and set Inset to the value you had before.

1.73 - XP themes.
       - for D4-6 when using Mike Lischke's ThemeManager the ruler will
         paint its background using the parent when ParentColor := True.
       - for D7 set ParentBackGround to True so that the ruler use the
         parent's theme background to draw its own background.


Contains
========
\RichView       Demo project using TRichViewEdit (*)
\Simple         Demo project using standard TRichEdit
CompVers.inc
Ruler.pas       Core unit containing TRuler, TVRuler
RulerReg.dcr
RulerReg.pas    Use this unit to register/install the component.

RERuler         TRERuler example component specialized for TRichEdit
RVRuler.pas     TRVRuler (*)

(*) Requires RichView package from Sergey Tkachenko. This is
not included but you can download a trial version from...
http://www.trichview.com/download/


Installation
============
· D2
Select menu "Component" -> "Install...".
Press the "Add" button and select the unit RulerReg.pas.

· D3/4/5/6/7
Select menu "Component" -> "Install Component...".
Select if you want to add the Ruler component to a new or
existing package.
Press the "Browse" button and select the unit RulerReg.pas.

· Note: before installing TRVRuler make sure that you first
        install the RichView package and the TRuler component.
        TRVRuler can then be installed in the same way as TRuler.


p.zylstra@hccnet.nl
http://home.hccnet.nl/p.zylstra/
