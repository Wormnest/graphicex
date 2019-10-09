// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// Portions created by Jacob Boerema are
// Copyright (C) 2013-2015 Jacob Boerema. All Rights Reserved.
// This fork of GraphicEx can be found at https://bitbucket.org/jacobb/graphicex
//
// GraphicEx Version constants

unit gexVersion;

interface

{.$DEFINE GEX_RELEASE}
{$DEFINE GEX_BETA}

{$I gexdefines.inc}

const
  GraphicExBase   = 'GraphicEx';  // GraphicEx identifier
  GraphicExFork   = 'Fork: jb';   // Identifier of this fork

  // Current code status: beta or release
{$IFDEF GEX_BETA}
  GraphicExStatus = 'beta';
{$ENDIF}
{$IFDEF GEX_RELEASE}
  GraphicExStatus = 'release';
{$ENDIF}

  // Since the last version by Mike Lischke I've seen was II.1.17 we start with version 3.
  GraphicExMajorVersion = 3;
  GraphicExMinorVersion = 0;

  GraphicExString = GraphicExBase + ' ' + GraphicExFork + ' '+ GraphicExStatus;
  GraphicExVersion = (GraphicExMajorVersion shl 8) or GraphicExMinorVersion;

var
  GraphicExVersionString: string;

implementation

uses SysUtils;

initialization
  GraphicExVersionString := GraphicExString + ' ' +
    IntToStr(GraphicExMajorVersion) + '.' +
    IntToStr(GraphicExMinorVersion);
end.
