{ gexdefines.inc Some defines to make life easier.
  Dual License: MPL 1.1 or LGPL 2.1 with linking exception (the "FPC modified LGPL License")
  Portions Created by Jacob Boerema are Copyright (C) 2019 Jacob Boerema.
  All Rights Reserved.
  This fork of GraphicEx can be found at https://github.com/Wormnest/graphicex
}

{$IFDEF FPC}
  {$mode delphi}
  {$DEFINE USEINLINING}
  {$ASMMODE INTEL}            // Needed for 64 bit assembler
  {$DEFINE SUPPORTS_INLINE}   // Compiler supports the inline directive (D9+/FPC)
{$ELSE}
  {$DEFINE DELPHI}
  {$IFDEF MSWINDOWS}
    {$DEFINE WINDOWS}
  {$ENDIF}
  {$IFDEF CPUX64}
    {$DEFINE CPU64}           // Only use one x64 define across Delphi/FPC
  {$ENDIF}
{$ENDIF}
