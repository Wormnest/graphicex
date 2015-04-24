{******************************************************************************}
{                                    Ruler                                     }
{                                    -----                                     }
{                                                                              }
{                     Copyright © 2003-2005 Pieter Zijlstra                    }
{                                                                              }
{ A horizontal ruler component simular like the one used in Word(Pad).         }
{ - Adjustable left and right margins.                                         }
{ - Adjustable first, hanging, left and right indent markers.                  }
{ - Tabs can be added/removed at runtime and designtime.                       }
{ - Tabs support left, center, right, decimal and wordbar alignment when       }
{   toAdvancedTabs is set in TabsSettings.Options.                             }
{                                                                              }
{ E-mail: p.zylstra@hccnet.nl                                                  }
{ Website: http://home.hccnet.nl/p.zylstra/                                    }
{==============================================================================}
{                           This software is FREEWARE                          }
{                           -------------------------                          }
{                                                                              }
{ You may freely use it in any software, including commercial software, but    }
{ be-aware that the code is provided as-is, with no implied warranty.          }
{==============================================================================}
{                                                                              }
{ Notes:                                                                       }
{ - This component uses different names for the same indentations:             }
{   Indent-Left is for the visible part (eg Hint) called "hanging indent" and  }
{   to make it more confusing, Indent-Both is called "left indent" (ala Word). }
{                                                                              }
{ - XP themes.                                                                 }
{   - for D4-6 when using Mike Lischkes ThemeManager the ruler will paint its  }
{     background using the parent when ParentColor := True.                    }
{   - for D7 set ParentBackGround to True so that the ruler use the parent's   }
{     theme background to draw its own background.                             }
{                                                                              }
{                                                                              }
{ Version history:                                                             }
{ 0.9.9.0 11 feb 2003 - First public release (BETA)                            }
{ 0.9.9.1 27 may 2003 - Added property UnitsDisplay.                           }
{                     - Based on comments of Andrew Fiddian-Green:             }
{                       - Removed RulerAdj (4/3) 'kludge' .                    }
{                       - Property Pixels of TTab is now actual returning the  }
{                         number of pixels instead of points it was doing before }
{                         whith the above mentioned (removed) RulerAdj 'kludge'. }
{                       - Added new property TTab.Points.                      }
{                       - Improved 3D look of the outline of the ruler.        }
{ 1.0.0.0 14 jun 2003 - Draw default tab stops as little dot markers at the    }
{                       bottom of the ruler.                                   }
{                     - SnapToRuler for Tabs, Indents and Margins.             }
{                     - Adjustable margins (by dragging them).                 }
{                     - Indent and Tab hints can be modified from the OI.      }
{ 1.0.1.0 31 jul 2003 - Based on bug report by John Bennett:                   }
{                       - SetFirstIndent sets the LeftIndent back to the       }
{                         original position when the FirstIndent is moved      }
{                         outside the margins and kept within margins by code. }
{ 1.0.2.0 29 oct 2003 - BugFix:                                                }
{                       - Left and RightMargin were not stored when they were  }
{                         set to zero in the OI. This default action (for some }
{                         of the types) of Delphi is now overruled by using    }
{                         DefineProperties and separate readers and writers.   }
{ 1.1.0.0  1 nov 2003 - New IndentOption, ioKeepWithinMargins. This one is     }
{                       set to True by default because this was (roughly) the  }
{                       default behaviour of the previous versions. BTW setting}
{                       this option to False is not very usefull when using a  }
{                       standard TRichEdit.                                    }
{                     - Indents and Margins can no longer be dragged outside   }
{                       the paper-area.                                        }
{                     - The First/Left/Both idents can no longer be dragged    }
{                       closer then a 1/8 inch towards the RightIndent (and    }
{                       vice versa).                                           }
{                     - The LeftMargin can no longer be dragged closer then    }
{                       a 1/8 inch towards the RightMargin (and vice versa).   }
{                     - Added following procedures for use with TRVRuler       }
{                         DoRulerItemSelect(...);                              }
{                         DoRulerItemMove(...)                                 }
{                         DoRulerItemRelease;                                  }
{                     - Made it compatible with D2 and D3 (and hopefully D4).  }
{ 1.2.0.0  5 nov 2003 - Added new ruler units ruMillimeters, ruPicas, ruPixels }
{                       and ruPoints.                                          }
{                     - Removed previous added DefineProperties (see v1.0.2.0) }
{                       LeftMargin and RightMargin are now only set in the     }
{                       constructor when the Ruler is dropped on a component   }
{                       from within the IDE.                                   }
{                     - Improved ioKeepWithinMargins so that you can't drag the}
{                       indents beyond the margins when this is option is set. }
{                     - Improved handling of indents when dragging. The        }
{                       First/Left/Both indents are now kept separated from the}
{                       Right indent.                                          }
{                     - Added MarginSettings.GripColor. It will only be painted}
{                       when the color is not the same as the MarginColor or the}
{                       RulerColor.                                            }
{                     - Added "DoubleBuffered" for Delphi versions below D4.   }
{                     - BugFix: Arrggh, forgot to set the Font of the Canvas.  }
{ 1.2.1.0  6 nov 2003 - Changed compiler directives and the file-order of some }
{                       record definitions for C++ Builder compatibility.      }
{ 1.3.0.0 19 feb 2004 - BugFix: in SetUnitsProgram the conversion for ruPicas  }
{                               to other ruler units was missing.              }
{                     - New options to enable displaying of the last position  }
{                       of an item (only Indents) while it's being dragged.    }
{                     - When SnapToRuler is True and a tab is placed on the    }
{                       ruler by clicking on the empty ruler it will also be   }
{                       directly 'snapped' into the right place.               }
{                     - Register moved to RulersReg.pas                        }
{                     ~ New TableEditor for TRichView (under construction).    }
{                     - RTL (beta)                                             }
{ 1.3.0.1 20 feb 2004 - Left and RightMargin handling changed for RTL.         }
{                       Tabs are changed from LeftAligned to RightAligned when }
{                       the BiDiMode has changed.                              }
{ 1.3.1.0 21 feb 2004 - Corrected RTL drawing of OutLine.                      }
{                     - Corrected handling of Tabs in RTL mode.                }
{                     - Implemented ItemSeparation for RTL mode.               }
{                     - Also Tabs can display their last position while it's   }
{                       being dragged (set roItemsShowLastPos of Ruler.Options).}
{                     - Added property BiDiModeRuler that can be used to force }
{                       the ruler to use LTR or RTL independent of Delphi's    }
{                       build-in BiDiMode handling.                            }
{ 1.4.0.0 22 feb 2004 - Ruler goes vertical:                                   }
{                       - Added property RulerType                             }
{                       - Added property TopMargin                             }
{                       - Added property BottomMargin                          }
{                       - Added 'specialised' component TVRuler which just sets}
{                         the defaults for using the Ruler in vertical mode.   }
{                     - New property Flat                                      }
{                     - Property LeftInset is replaced by property Inset.      }
{ 1.4.1.0 28 feb 2004 - Cleaning up (double) code.                             }
{                     - Added property Zoom (in percentage, default is 100%).  }
{ 1.4.2.0 29 feb 2004 - Scale will be drawn relative to the margin when        }
{                       roScaleRelativeToMargin is set.                        }
{ 1.5.0.0 23 mar 2004 - TableEditor for TRichView.                             }
{                     - Made property ScreenRes writable (for TRichView)       }
{                     - Improved handling of the default printer/paper         }
{                       dimensions. In case the printer is not available       }
{                       (network disconnected) paper dimensions will use       }
{                       default "Letter" settings.                             }
{ 1.5.0.1 02 apr 2004 - Added property DefaultTabWidth.                        }
{ 1.5.0.2 04 apr 2004 - BugFix: Forgot to update DefaultTabWidth               }
{                               when UnitsProgram is changed.                  }
{                     - Drawing of Tabs (and DefaultTabs) within table cells.  }
{                     - Limitted drag of columns/borders to neighbouring       }
{                       columns/borders.                                       }
{ 1.6.0.0 12 apr 2004 - Added TableEditor.DraggedDelta.                        }
{                     - Changed drawing of the margins a little when Flat is   }
{                       True. Also the Table graphics will be drawn flat now.  }
{                     - Table: Drag/Shift when VK_SHIFT is pressed.            }
{                     - BugFix: KeepDragWithinPageArea did not function        }
{                               correctly in RTL mode.                         }
{                     - BugFix: KeepColumnsSeparated did not function          }
{                               correctly in RTL mode.                         }
{ 1.6.1.0 13 apr 2004 - BugFix: When DefaultTabWidth did not have a valid      }
{                               value (it should be >0) it would cause the     }
{                               drawing routines to go into an endless loop.   }
{ 1.7.0.0 19 dec 2004 - Added the basics for Bullets & Numbering for TRichView.}
{ 1.7.1.0 21 apr 2005 - Improved drawing for themed applications, background   }
{                       did not show correctly on for instance PageControls.   }
{                       (thanks to Alexander Halser for fixing this).          }
{ 1.7.2.0 01 may 2005 - BugFix: The first time the position was calculated for }
{                               a tab when adding tabs did not take the margin }
{                               into account.                                  }
{                     - Default tabs are no longer drawn by default before the }
{                       LeftIndent. If you want them back you can turn off     }
{                       toDontShowDefaultTabsBeforeLeftIndent of               }
{                       TabSettings.Options.                                   }
{ 1.7.3.0 14 may 2005 - BugFix: The Font could be modified by themed drawing   }
{                               The Canvas is now forced to (re)create its     }
{                               drawing objects (Brush, Font, Pen).            }
{******************************************************************************}
unit Ruler;

{$B-}

interface

{$I CompVers.inc}

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, ExtCtrls, Menus;

type
  TRulerType = (
    rtHorizontal,
    rtVertical
  );

  TTabAlign = (
    taLeftAlign,
    taCenterAlign,
    taRightAlign,
    taDecimalAlign,
    taWordBarAlign
  );

  TIndentGraphic = (
    igNone,
    igUp,
    igDown,
    igRectangle,
    igLevelDec1,
    igLevelDec2,
    igLevelInc1,
    igLevelInc2
  );

  TIndentType = (
    itBoth,        // just to move both first and left indent sliders.
    itFirst,       // relative to left margin
    itLeft,        // relative to first indent
    itRight
  );

  TLevelType = (
    ltLevelDec,
    ltLevelInc
  );

  TMarginType = (
    mtTop,
    mtBottom,
    mtLeft,
    mtRight
  );

  TBiDiModeRuler = (
    bmUseBiDiMode,
    bmLeftToRight,
    bmRightToLeft
  );

  TRulerUnits = (
    ruInches,
    ruCentimeters,
    ruMillimeters,
    ruPicas,
    ruPixels,
    ruPoints
  );

  THitItem = (
    hiNone,
    hiOnTab,
    hiOnBothIndent,
    hiOnFirstIndent,
    hiOnLeftIndent,
    hiOnRightIndent,
    hiOnTopMargin,
    hiOnBottomMargin,
    hiOnLeftMargin,
    hiOnRightMargin,
    hiOnColumn,
    hiOnLeftBorder,
    hiOnRightBorder,
    hiOnLevelDec,
    hiOnLevelInc
  );
  THitItems = set of THitItem;

  TDragItem = (
    diNone,
    diBorder,
    diColumn,
    diIndent,
    diMargin,
    diTab
  );

  TTableGraphic = (
    tgNone,
    tgBorder,
    tgColumn
  );

  TBorderType = (
    btLeft,
    btRight
  );

  TTablePosition = (
    tpAbsolute,
    tpFromFirstIndent,
    tpFromLeftIndent,
    tpFromMargin
  );

  TIndentOption = (
    ioExtraShadow,
    ioKeepWithinMargins,
    ioShowFirstIndent,
    ioShowHangingIndent,
    ioShowLeftIndent,
    ioShowRightIndent,
    ioSnapToRuler
  );
  TIndentOptions = set of TIndentOption;

  TLevelGraphic = (
    lgType1,
    lgType2
  );

  TListEditorOption = (
    leoAdjustable,
    leoLevelAdjustable
  );
  TListEditorOptions = set of TListEditorOption;

  TMarginOption = (
    moAdjustable,
    moSnapToRuler
  );
  TMarginOptions = set of TMarginOption;

  TRulerOption = (
    roAutoUpdatePrinterWidth,
    roItemsShowLastPos,
    roUseDefaultPrinterWidth,
    roScaleRelativeToMargin
  );
  TRulerOptions = set of TRulerOption;

  TTabOption = (
    toAdvancedTabs,
    toShowDefaultTabs,
    toSnapToRuler,
    toDontShowDefaultTabsBeforeLeftIndent
  );
  TTabOptions = set of TTabOption;

  TTableEditorOption = (
    teoAdjustable,
    teoSnapToRuler
  );
  TTableEditorOptions = set of TTableEditorOption;

  TTableEditorVisible = (
    tevNever,
    tevOnlyWhenActive
  );


  TZoomRange = 1..10000;  // 1% - 10000% 

const
  AllBorders: THitItems = [hiOnLeftBorder..hiOnRightBorder];
  AllIndents: THitItems = [hiOnBothIndent..hiOnRightIndent];
  AllLevels: THitItems = [hiOnLevelDec..hiOnLevelInc];
  AllMargins: THitItems = [hiOnTopMargin..hiOnRightMargin];
  AllTable: THitItems = [hiOnColumn, hiOnLeftBorder..hiOnRightBorder];
  AllTabs: THitItems = [hiOnTab];

  DefaultIndentOptions = [ioKeepWithinMargins,
                          ioShowFirstIndent, ioShowHangingIndent,
                          ioShowLeftIndent, ioShowRightIndent];
  DefaultListOptions = [leoAdjustable, leoLevelAdjustable];
  DefaultMarginOptions = [moAdjustable];
  DefaultRulerOptions = [roUseDefaultPrinterWidth];
  DefaultTableOptions = [teoAdjustable];
  DefaultTabOptions = [toShowDefaultTabs, toDontShowDefaultTabsBeforeLeftIndent];

type
  TDragInfo = record
    Item: TDragItem;
    Index: Integer;
    Offset: Integer;
  end;

  THitInfo = record
    Index: Integer;
    HitItems: THitItems;
  end;

  TIndent = record
    Graphic: TIndentGraphic;
    Left: Integer;
    Position: Extended;
    Top: Integer;
  end;

  TMargin = record
    Grip: Integer;
    Position: Extended;
  end;

  TTableBorder = record
    Left: Integer;
    Position: Extended;
  end;

  TIndentArray = array[TIndentType] of TIndent;

  TCustomRuler = class;

  TTab = class(TCollectionItem)
  private
    FAlign: TTabAlign;
    FColor: TColor;
    FDeleting: Boolean;
    FLeft: Integer;
    FPosition: Extended;
    FTop: Integer;
    function GetPixels: Extended;
    function GetPoints: Extended;
    function GetRTLAlign: TTabAlign;
    procedure SetAlign(const Value: TTabAlign);
    procedure SetColor(const Value: TColor);
    procedure SetPosition(const Value: Extended);
    procedure SetPixels(const Value: Extended);
    procedure SetPoints(const Value: Extended);
  protected
    property Pixels: Extended read GetPixels write SetPixels;
    property RTLAlign: TTabAlign read GetRTLAlign;
  public
    constructor Create(Collection: TCollection); override;
    procedure AssignTo(Dest: TPersistent); override;
    property Left: Integer read FLeft write FLeft;
    property Points: Extended read GetPoints write SetPoints;
    property Top: Integer read FTop write FTop;
  published
    property Align: TTabAlign read FAlign write SetAlign;
    property Color: TColor read FColor write SetColor;
    property Position: Extended read FPosition write SetPosition;
  end;

  TTabs = class(TCollection)
  private
    FBlockDoTabChanged: Boolean;
    FRuler: TCustomRuler;
    function GetTab(Index: Integer): TTab;
    procedure SetTab(Index: Integer; const Value: TTab);
  protected
{$IFDEF COMPILER35_UP}
    function GetOwner: TPersistent; override;
{$ELSE}
    function GetOwner: TPersistent;
{$ENDIF}
    procedure SortTabs;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Ruler: TCustomRuler);
    function Add: TTab;
    property Items[Index: Integer]: TTab read GetTab write SetTab; default;
    property Ruler: TCustomRuler read FRuler;
  end;

  TRulerCell = class(TCollectionItem)
  private
    FCellWidth: Extended;
    FDragBoundary: Extended;
    FFirstIndent: Extended;
    FLeft: Integer;
    FLeftIndent: Extended;
    FRightIndent: Extended;
    function GetDragLimit: Integer;
    function GetPosition: Extended;
    procedure SetCellWidth(const Value: Extended);
    procedure SetDragBoundary(const Value: Extended);
    procedure SetLeft(const Value: Integer);
    procedure SetPosition(const Value: Extended);
  protected
    property DragLimit: Integer read GetDragLimit;
    property FirstIndent: Extended read FFirstIndent write FFirstIndent;
    property Left: Integer read FLeft write SetLeft;
    property LeftIndent: Extended read FLeftIndent write FLeftIndent;
    property Position: Extended read GetPosition write SetPosition;
    property RightIndent: Extended read FRightIndent write FRightIndent;
  public
    constructor Create(Collection: TCollection); override;
    property DragBoundary: Extended read FDragBoundary write SetDragBoundary;
  published
    property CellWidth: Extended read FCellWidth write SetCellWidth;
  end;

  TRulerCells = class(TCollection)
  private
    FRuler: TCustomRuler;
    function GetCell(Index: Integer): TRulerCell;
    procedure SetCell(Index: Integer; const Value: TRulerCell);
  protected
{$IFDEF COMPILER35_UP}
    function GetOwner: TPersistent; override;
{$ELSE}
    function GetOwner: TPersistent;
{$ENDIF}
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TCustomRuler);
    function Add: TRulerCell;
    property Items[Index: Integer]: TRulerCell read GetCell write SetCell; default;
    property Ruler: TCustomRuler read FRuler;
  end;

  TRulerListEditor = class(TPersistent)
  private
    FActive: Boolean;
    FLevelGraphic: TLevelGraphic;
    FListLevel: Integer;
    FOptions: TListEditorOptions;
    FOwner: TCustomRuler;
    procedure SetOptions(const Value: TListEditorOptions);
    procedure SetActive(const Value: Boolean);
    procedure SetLevelGraphic(const Value: TLevelGraphic);
  protected
{$IFDEF COMPILER35_UP}
    function GetOwner: TPersistent; override;
{$ELSE}
    function GetOwner: TPersistent;
{$ENDIF}
    procedure Invalidate;
  public
    constructor Create(AOwner: TCustomRuler); virtual;
    property Active: Boolean read FActive write SetActive;
    property ListLevel: Integer read FListLevel write FListLevel;
    property Ruler: TCustomRuler read FOwner;
  published
    property LevelGraphic: TLevelGraphic read FLevelGraphic write SetLevelGraphic default lgType1;
    property Options: TListEditorOptions read FOptions write SetOptions default DefaultListOptions;
  end;

  TRulerTableEditor = class(TPersistent)
  private
    FActive: Boolean;
    FBackGroundColor: TColor;
    FBorders: array[TBorderType] of TTableBorder;
    FBorderHSpacing: Extended;
    FBorderWidth: Extended;
    FCellPadding: Extended;
    FCellBorderWidth: Extended;
    FCellHSpacing: Extended;
    FCellIndex: Integer;
    FDragCursor: TCursor;
    FDraggedColumn: Integer;
    FDraggedDelta: Extended;
    FDraggedWithShift: Boolean;
    FDragLast: Integer;
    FDragStart: Integer;
    FFirstIndent: Extended;
    FForeGroundColor: TColor;
    FLeftIndent: Extended;
    FOptions: TTableEditorOptions;
    FOwner: TCustomRuler;
    FRightIndent: Extended;
    FRulerCells: TRulerCells;
    FRulerIndents: TIndentArray; // Used to store a temporary copy.
    FTablePosition: TTablePosition;
    FUseDragBoundaries: Boolean;
    FVisible: TTableEditorVisible;
    function GetBorderRect(const BorderType: TBorderType): TRect;
    function GetCellRect(const Index: Integer): TRect;
    function GetColumnIndexAt(const X, Y: Integer): Integer;
    function GetNextValidCell(const Index: Integer): Integer;
    function GetOffset: Integer;
    function GetPrevValidCell(const Index: Integer): Integer;
    function GetTableLeft: Extended;
    function GetTableWidth: Extended;
    function GetTotalCellSpacing(const FromBorder, FromLeft: Boolean): Integer;
    function KeepColumnsSeparated(const Index, Left: Integer): Integer;
    function KeepWithinCurrentCell(const IT: TIndentType; const X: Integer): Integer;
    function LastValidCellIndex: Integer;
    function RTLAdjust(X, Offset: Integer): Integer;
    procedure SetActive(const Value: Boolean);
    procedure SetBackGroundColor(const Value: TColor);
    procedure SetBorderHSpacing(const Value: Extended);
    procedure SetBorderWidth(const Value: Extended);
    procedure SetCellBorderWidth(const Value: Extended);
    procedure SetCellHSpacing(const Value: Extended);
    procedure SetCellIndex(const Value: Integer);
    procedure SetCellPading(const Value: Extended);
    procedure SetDragCursor(const Value: TCursor);
    procedure SetFirstIndent(const Value: Extended);
    procedure SetForeGroundColor(const Value: TColor);
    procedure SetLeftIndent(const Value: Extended);
    procedure SetOptions(const Value: TTableEditorOptions);
    procedure SetRightIndent(const Value: Extended);
    procedure SetRulerCells(const Value: TRulerCells);
    procedure SetTableLeft(const Value: Extended);
    procedure SetTableWidth(const Value: Extended);
    procedure SetTablePosition(const Value: TTablePosition);
    procedure SetVisible(const Value: TTableEditorVisible);
    procedure UpdateIndentPosition(const IT: TIndentType; const XPos: Integer);
  protected
    function CalculateCurrentIndentPosition(const IT: TIndentType): Integer;
{$IFDEF COMPILER35_UP}
    function GetOwner: TPersistent; override;
{$ELSE}
    function GetOwner: TPersistent;
{$ENDIF}
    procedure Invalidate;
    property Offset: Integer read GetOffset;
  public
    constructor Create(AOwner: TCustomRuler); virtual;
    destructor Destroy; override;
    property DraggedColumn: Integer read FDraggedColumn;
    property DraggedDelta: Extended read FDraggedDelta;
    property DraggedWithShift: Boolean read FDraggedWithShift;
    property Ruler: TCustomRuler read FOwner;
    property UseDragBoundaries: Boolean read FUseDragBoundaries write FUseDragBoundaries;
  published
    property Active: Boolean read FActive write SetActive;
    property BackGroundColor: TColor read FBackGroundColor write SetBackGroundColor default clBtnFace;
    property BorderHSpacing: Extended read FBorderHSpacing write SetBorderHSpacing;
    property BorderWidth: Extended read FBorderWidth write SetBorderWidth;
    property CellBorderWidth: Extended read FCellBorderWidth write SetCellBorderWidth;
    property CellHSpacing: Extended read FCellHSpacing write SetCellHSpacing;
    property CellIndex: Integer read FCellIndex write SetCellIndex;
    property CellPadding: Extended read FCellPadding write SetCellPading;
    property Cells: TRulerCells read FRulerCells write SetRulerCells;
    property DragCursor: TCursor read FDragCursor write SetDragCursor default crHSplit;
    property FirstIndent: Extended read FFirstIndent write SetFirstIndent;
    property ForeGroundColor: TColor read FForeGroundColor write SetForeGroundColor default clBtnShadow;
    property LeftIndent: Extended read FLeftIndent write SetLeftIndent;
    property Options: TTableEditorOptions read FOptions write SetOptions default DefaultTableOptions;
    property RightIndent: Extended read FRightIndent write SetRightIndent;
    property TableLeft: Extended read GetTableLeft write SetTableLeft;
    property TablePosition: TTablePosition read FTablePosition write SetTablePosition default tpFromMargin;
    property TableWidth: Extended read GetTableWidth write SetTableWidth;
    property Visible: TTableEditorVisible read FVisible write SetVisible default tevOnlyWhenActive;
  end;

  TIndentSettings = class(TPersistent)
  private
    FDragCursor: TCursor;
    FOptions: TIndentOptions;
    FOwner: TCustomRuler;
    procedure SetDragCursor(const Value: TCursor);
    procedure SetOptions(const Value: TIndentOptions);
  public
    constructor Create(AOwner: TCustomRuler); virtual;
    procedure AssignTo(Dest: TPersistent); override;
    property Owner: TCustomRuler read FOwner;
  published
    property DragCursor: TCursor read FDragCursor write SetDragCursor default crDrag;
    property Options: TIndentOptions read FOptions write SetOptions default DefaultIndentOptions;
  end;

  TMarginSettings = class(TPersistent)
  private
    FDragCursor: TCursor;
    FGripColor: TColor;
    FOptions: TMarginOptions;
    FOwner: TCustomRuler;
    procedure SetDragCursor(const Value: TCursor);
    procedure SetOptions(const Value: TMarginOptions);
    procedure SetGripColor(const Value: TColor);
  public
    constructor Create(AOwner: TCustomRuler); virtual;
    procedure AssignTo(Dest: TPersistent); override;
    property Owner: TCustomRuler read FOwner;
  published
    property DragCursor: TCursor read FDragCursor write SetDragCursor default crSizeWE;
    property GripColor: TColor read FGripColor write SetGripColor default clBtnShadow;
    property Options: TMarginOptions read FOptions write SetOptions default DefaultMarginOptions;
  end;

  TTabSettings = class(TPersistent)
  private
    FDeleteCursor: TCursor;
    FDragCursor: TCursor;
    FOptions: TTabOptions;
    FOwner: TCustomRuler;
    procedure SetDeleteCursor(const Value: TCursor);
    procedure SetDragCursor(const Value: TCursor);
    procedure SetOptions(const Value: TTabOptions);
  public
    constructor Create(AOwner: TCustomRuler); virtual;
    procedure AssignTo(Dest: TPersistent); override;
    property Owner: TCustomRuler read FOwner;
  published
    property DeleteCursor: TCursor read FDeleteCursor write SetDeleteCursor default crNone;
    property DragCursor: TCursor read FDragCursor write SetDragCursor default crDrag;
    property Options: TTabOptions read FOptions write SetOptions default DefaultTabOptions;
  end;

  TRulerTexts = class(TPersistent)
  private
    FHintColumnMove: string;
    FHintIndentFirst: string;
    FHintIndentLeft: string;
    FHintIndentHanging: string;
    FHintIndentRight: string;
    FHintLevelDec: string;
    FHintLevelInc: string;
    FHintTabCenter: string;
    FHintTabDecimal: string;
    FHintTabLeft: string;
    FHintTabRight: string;
    FHintTabWordBar: string;
    FMenuTabCenter: string;
    FMenuTabDecimal: string;
    FMenuTabLeft: string;
    FMenuTabRight: string;
    FMenuTabWordBar: string;
    FRuler: TCustomRuler;
    procedure SetHintColumnMove(const Value: string);
    procedure SetHintIndentFirst(const Value: string);
    procedure SetHintIndentHanging(const Value: string);
    procedure SetHintIndentLeft(const Value: string);
    procedure SetHintIndentRight(const Value: string);
    procedure SetHintLevelDec(const Value: string);
    procedure SetHintLevelInc(const Value: string);
    procedure SetHintTabCenter(const Value: string);
    procedure SetHintTabDecimal(const Value: string);
    procedure SetHintTabLeft(const Value: string);
    procedure SetHintTabRight(const Value: string);
    procedure SetHintTabWordBar(const Value: string);
    procedure SetMenuTabCenter(const Value: string);
    procedure SetMenuTabDecimal(const Value: string);
    procedure SetMenuTabLeft(const Value: string);
    procedure SetMenuTabRight(const Value: string);
    procedure SetMenuTabWordBar(const Value: string);
  public
    constructor Create(AOwner: TCustomRuler); virtual;
  published
    property HintColumnMove: string read FHintColumnMove write SetHintColumnMove;
    property HintIndentFirst: string read FHintIndentFirst write SetHintIndentFirst;
    property HintIndentLeft: string read FHintIndentLeft write SetHintIndentLeft;
    property HintIndentHanging: string read FHintIndentHanging write SetHintIndentHanging;
    property HintIndentRight: string read FHintIndentRight write SetHintIndentRight;
    property HintLevelDec: string read FHintLevelDec write SetHintLevelDec;
    property HintLevelInc: string read FHintLevelInc write SetHintLevelInc;
    property HintTabCenter: string read FHintTabCenter write SetHintTabCenter;
    property HintTabDecimal: string read FHintTabDecimal write SetHintTabDecimal;
    property HintTabLeft: string read FHintTabLeft write SetHintTabLeft;
    property HintTabRight: string read FHintTabRight write SetHintTabRight;
    property HintTabWordBar: string read FHintTabWordBar write SetHintTabWordBar;
    property MenuTabCenter: string read FMenuTabCenter write SetMenuTabCenter;
    property MenuTabDecimal: string read FMenuTabDecimal write SetMenuTabDecimal;
    property MenuTabLeft: string read FMenuTabLeft write SetMenuTabLeft;
    property MenuTabRight: string read FMenuTabRight write SetMenuTabRight;
    property MenuTabWordBar: string read FMenuTabWordBar write SetMenuTabWordBar;
  end;

  TRulerItemClickEvent = procedure(Sender: TObject; X: Integer) of object;
  TRulerItemMoveEvent = procedure(Sender: TObject; X: Integer; Removing: Boolean) of object;

  TCustomRuler = class(TCustomControl)
  private
    FBiDiModeRuler: TBiDiModeRuler;
    FDefaultTabWidth: Extended;
    FDiff: Integer;
    FDragInfo: TDragInfo;
    FFlat: Boolean;
    FIndents: TIndentArray;
    FIndentSettings: TIndentSettings;
    FIndentTraces: TIndentArray;
    FInset: Integer;
    FItemSeparation: Integer;
    FListEditor: TRulerListEditor;
    FMargins: array[TMarginType] of TMargin;
    FMarginSettings: TMarginSettings;
    FMarginColor: TColor;
    FMaxTabs: Integer;
    FMultiPixels: Extended;
    FMultiPoints: Extended;
{$IFDEF COMPILER7_UP}
    FOldParentBackground: Boolean;
{$ENDIF}
    FOldShowHint: Boolean;
    FOnBiDiModeChanged: TNotifyEvent;
    FOnIndentChanged: TNotifyEvent;
    FOnMarginChanged: TNotifyEvent;
    FOnRulerItemMove: TRulerItemMoveEvent;
    FOnRulerItemRelease: TNotifyEvent;
    FOnRulerItemSelect: TRulerItemClickEvent;
    FOnTabChanged: TNotifyEvent;
    FOnTableColumnChanged: TNotifyEvent;
    FOrgHint: string;
    FOvrHint: Boolean;
    FPageHeight: Extended;
    FPageWidth: Extended;
    FPopupMenu: TPopupMenu;
    FPrinterHeight: Extended;
    FPrinterWidth: Extended;
    FRulerColor: TColor;
    FRulerOptions: TRulerOptions;
    FRulerTexts: TRulerTexts;
    FRulerType: TRulerType;
    FScreenRes: Integer;
    FTableEditor: TRulerTableEditor;
    FTabs: TTabs;
    FTabSettings: TTabSettings;
    FTabTrace: TTab;
    FTimer: TTimer;
    FUnitsDisplay: TRulerUnits;
    FUnitsProgram: TRulerUnits;
    FZoom: TZoomRange;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    function GetBottomMargin: Extended;
    function GetDefaultTabWidth: Extended;
    function GetEndMargin: Extended;
    function GetEndMarginGrip: Integer;
    function GetFirstIndent: Extended;
    function GetIndentRect(IndentType: TIndentType): TRect;
    function GetLeftIndent: Extended;
    function GetLeftMargin: Extended;
    function GetLevelRect(LevelType: TLevelType): TRect;
    function GetMarginGripRect(MarginType: TMarginType): TRect;
    function GetMarginRect(MarginType: TMarginType): TRect;
    function GetRightIndent: Extended;
    function GetRightMargin: Extended;
    function GetStartMargin: Extended;
    function GetStartMarginGrip: Integer;
    function GetTabIndexAt(X, Y: Integer): Integer;
    function GetTopMargin: Extended;
    function HitItemsToIndex(HitItems: THitItems): Integer;
    procedure PaintHDefaultTabs(Canvas: TCanvas; R: TRect);
    procedure PaintHIndent(Canvas: TCanvas; Graphic: TIndentGraphic; X, Y: Integer; HighLight, ExtraShadow, Dimmed: Boolean);
    procedure PaintHIndents(Canvas: TCanvas; R: TRect);
    procedure PaintHMargins(Canvas: TCanvas; R: TRect);
    procedure PaintHMarkers(Canvas: TCanvas; R: TRect);
    procedure PaintHOutline(Canvas: TCanvas; R: TRect);
    procedure PaintHTab(Canvas: TCanvas; Graphic: TTabAlign; X, Y: Integer);
    procedure PaintHTableGraphic(Canvas: TCanvas; Graphic: TTableGraphic; R: TRect);
    procedure PaintHTableGraphics(Canvas: TCanvas; R: TRect);
    procedure PaintHTabs(Canvas: TCanvas; R: TRect);
    procedure PaintVMargins(Canvas: TCanvas; R: TRect);
    procedure PaintVMarkers(Canvas: TCanvas; R: TRect);
    procedure PaintVOutline(Canvas: TCanvas; R: TRect);
    procedure ProcessParentBackground(B: Boolean);
    function RTLAdjust(X, Offset: Integer): Integer;
    function RTLAdjustRect(const R: TRect): TRect;
    procedure SetBiDiModeRuler(const Value: TBiDiModeRuler);
    procedure SetBottomMargin(Value: Extended);
    procedure SetDefaultTabWidth(const Value: Extended);
    procedure SetFirstIndent(Value: Extended);
    procedure SetFlat(const Value: Boolean);
    procedure SetIndentSettings(const Value: TIndentSettings);
    procedure SetInset(const Value: Integer);
    procedure SetItemSeparation(const Value: Integer);
    procedure SetLeftIndent(Value: Extended);
    procedure SetLeftMargin(Value: Extended);
    procedure SetListEditor(const Value: TRulerListEditor);
    procedure SetMarginColor(const Value: TColor);
    procedure SetMarginSettings(const Value: TMarginSettings);
    procedure SetMaxTabs(const Value: Integer);
    procedure SetPageHeight(const Value: Extended);
    procedure SetPageWidth(const Value: Extended);
    procedure SetRightIndent(Value: Extended);
    procedure SetRightMargin(Value: Extended);
    procedure SetRulerColor(const Value: TColor);
    procedure SetRulerOptions(const Value: TRulerOptions);
    procedure SetRulerTexts(const Value: TRulerTexts);
    procedure SetRulerType(const Value: TRulerType);
    procedure SetScreenRes(const Value: Integer);
    procedure SetTableEditor(const Value: TRulerTableEditor);
    procedure SetTabs(const Value: TTabs);
    procedure SetTabSettings(const Value: TTabSettings);
    procedure SetTopMargin(Value: Extended);
    procedure SetUnitsDisplay(const Value: TRulerUnits);
    procedure SetUnitsProgram(const Value: TRulerUnits);
    procedure SetZoom(const Value: TZoomRange);
    procedure TimerProc(Sender: TObject);
  protected
    procedure DoBiDiModeChanged; dynamic;
    procedure DoIndentChanged; dynamic;
    procedure DoLevelButtonDown(Direction: Integer); dynamic;
    procedure DoLevelButtonUp(Direction: Integer); dynamic;
    procedure DoMarginChanged; dynamic;
    procedure DoRulerItemMove(X: Integer; Removing: Boolean); dynamic;
    procedure DoRulerItemRelease; dynamic;
    procedure DoRulerItemSelect(X: Integer); dynamic;
    procedure DoTabChanged; dynamic;
    procedure DoTableColumnChanged; dynamic;
    function GetClosestOnRuler(X: Integer; FromMargin: Boolean): Integer;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure OverrideHint(NewHint: string);
    procedure Paint; override;
    procedure PaintHorizontalRuler(Canvas: TCanvas);
    procedure PaintVerticalRuler(Canvas: TCanvas);
    function PointsToUnits(const Value: Extended): Extended;
    procedure PopupClick(Sender: TObject);
    procedure RestoreHint;
    function UnitsPerInch(Units: TRulerUnits): Extended;
    function UnitsToPoints(const Value: Extended): Extended;
    procedure UpdatePageDimensions; virtual;
    function UseRTL: Boolean; virtual;
    function ZoomAndRound(const Value: Extended): Integer;
    function ZPixsToUnits(const Value: Extended): Extended;
    function ZUnitsToPixs(const Value: Extended): Extended;
    property BiDiModeRuler: TBiDiModeRuler read FBiDiModeRuler write SetBiDiModeRuler default bmUseBiDiMode;
    property BottomMargin: Extended read GetBottomMargin write SetBottomMargin;
    property DefaultTabWidth: Extended read GetDefaultTabWidth write SetDefaultTabWidth;
    property EndMargin: Extended read GetEndMargin;
    property EndMarginGrip: Integer read GetEndMarginGrip;
    property FirstIndent: Extended read GetFirstIndent write SetFirstIndent;
    property Flat: Boolean read FFlat write SetFlat;
    property IndentSettings: TIndentSettings read FIndentSettings write SetIndentSettings;
    property Inset: Integer read FInset write SetInset default 10;
    property ItemSeparation: Integer read FItemSeparation write SetItemSeparation;
    property LeftIndent: Extended read GetLeftIndent write SetLeftIndent;
    property LeftMargin: Extended read GetLeftMargin write SetLeftMargin;
    property ListEditor: TRulerListEditor read FListEditor write SetListEditor;
    property MarginColor: TColor read FMarginColor write SetMarginColor default clInfoBk;
    property MarginSettings: TMarginSettings read FMarginSettings write SetMarginSettings;
    property MaxTabs: Integer read FMaxTabs write SetMaxTabs default 32;
    property OnBiDiModeChanged: TNotifyEvent read FOnBiDiModeChanged write FOnBiDiModeChanged;
    property OnIndentChanged: TNotifyEvent read FOnIndentChanged write FOnIndentChanged;
    property OnMarginChanged: TNotifyEvent read FOnMarginChanged write FOnMarginChanged;
    property OnRulerItemMove: TRulerItemMoveEvent read FOnRulerItemMove write FOnRulerItemMove;
    property OnRulerItemRelease: TNotifyEvent read FOnRulerItemRelease write FOnRulerItemRelease;
    property OnRulerItemSelect: TRulerItemClickEvent read FOnRulerItemSelect write FOnRulerItemSelect;
    property OnTabChanged: TNotifyEvent read FOnTabChanged write FOnTabChanged;
    property OnTableColumnChanged: TNotifyEvent read FOnTableColumnChanged write FOnTableColumnChanged;
    property Options: TRulerOptions read FRulerOptions write SetRulerOptions default DefaultRulerOptions;
    property RightIndent: Extended read GetRightIndent write SetRightIndent;
    property RightMargin: Extended read GetRightMargin write SetRightMargin;
    property RulerColor: TColor read FRulerColor write SetRulerColor default clWindow;
    property RulerTexts: TRulerTexts read FRulerTexts write SetRulerTexts;
    property RulerType: TRulerType read FRulerType write SetRulerType default rtHorizontal;
    property StartMargin: Extended read GetStartMargin;
    property StartMarginGrip: Integer read GetStartMarginGrip;
    property TableEditor: TRulerTableEditor read FTableEditor write SetTableEditor;
    property Tabs: TTabs read FTabs write SetTabs;
    property TabSettings: TTabSettings read FTabSettings write SetTabSettings;
    property TopMargin: Extended read GetTopMargin write SetTopMargin;
    property UnitsDisplay: TRulerUnits read FUnitsDisplay write SetUnitsDisplay default ruCentimeters;
    property UnitsProgram: TRulerUnits read FUnitsProgram write SetUnitsProgram default ruCentimeters;
    property Zoom: TZoomRange read FZoom write SetZoom default 100;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddRichEdit98Tab(const Value: Extended; Alignment: TTabAlign);
    function GetHitTestInfoAt(X, Y: Integer): THitInfo;
    property MultiPixels: Extended read FMultiPixels;
    property MultiPoints: Extended read FMultiPoints;
    property PageHeight: Extended read FPageHeight write SetPageHeight;
    property PageWidth: Extended read FPageWidth write SetPageWidth;
    function PixsToUnits(const Value: Extended): Extended;
    function UnitsToPixs(const Value: Extended): Extended;
    property ScreenRes: Integer read FScreenRes write SetScreenRes;
  end;

  TRuler = class(TCustomRuler)
  published
    property Align;
{$IFDEF COMPILER4_UP}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
{$ENDIF}
    property BiDiModeRuler;
    property BottomMargin;
    property Color;
    property DefaultTabWidth;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FirstIndent;
    property Flat;
    property Font;
    property Hint;
    property IndentSettings;
    property Inset;
    property LeftIndent;
    property LeftMargin;
    property MarginColor;
    property MarginSettings;
    property MaxTabs;
    property OnBiDiModeChanged;
    property OnClick;
{$IFDEF COMPILER5_UP}
    property OnContextPopup;
{$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
{$IFDEF COMPILER4_UP}
    property OnEndDock;
{$ENDIF}
    property OnEndDrag;
    property OnIndentChanged;
    property OnMarginChanged;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnRulerItemMove;
    property OnRulerItemRelease;
    property OnRulerItemSelect;
{$IFDEF COMPILER4_UP}
    property OnStartDock;
{$ENDIF}
    property OnStartDrag;
    property OnTabChanged;
    property OnTableColumnChanged;
    property Options;
{$IFDEF COMPILER7_UP}
    property ParentBackground;
{$ENDIF}
{$IFDEF COMPILER4_UP}
    property ParentBiDiMode;
{$ENDIF}
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RightIndent;
    property RightMargin;
    property RulerColor;
    property RulerTexts;
    property RulerType;
    property ShowHint;
    property Tabs;
    property TabSettings;
    property TopMargin;
    property Visible;
    property UnitsDisplay;
    property UnitsProgram;
    property Zoom;
  end;

  TVRuler = class(TCustomRuler)
  public
    constructor Create(AOwner: TComponent); override;
    property MaxTabs default 0;
    property RulerType default rtVertical;
  published
    property Align;
{$IFDEF COMPILER4_UP}
    property Anchors;
    property Constraints;
    property DragKind;
{$ENDIF}
    property BottomMargin;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Flat;
    property Font;
    property Hint;
    property Inset;
    property MarginColor;
    property MarginSettings;
    property OnBiDiModeChanged;
    property OnClick;
{$IFDEF COMPILER5_UP}
    property OnContextPopup;
{$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
{$IFDEF COMPILER4_UP}
    property OnEndDock;
{$ENDIF}
    property OnEndDrag;
    property OnMarginChanged;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnRulerItemMove;
    property OnRulerItemRelease;
    property OnRulerItemSelect;
{$IFDEF COMPILER4_UP}
    property OnStartDock;
{$ENDIF}
    property OnStartDrag;
    property Options;
{$IFDEF COMPILER7_UP}
    property ParentBackground;
{$ENDIF}
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RulerColor;
    property ShowHint;
    property TopMargin;
    property Visible;
    property UnitsDisplay;
    property UnitsProgram;
    property Zoom;
  end;

implementation

uses
  Printers;

const
  cmPerInch = 2.54;
  mmPerInch = 25.4;
  PicasPerInch = 6;
  PointsPerInch = 72;

  // Determines the increments in which the numbers should be displayed.
  NumberIncrements: array[TRulerUnits] of Integer = (1, 1, 20, 6, 100, 36);
  // Determines the increments in which the markers should be displayed.
  MarkerIncrements: array[TRulerUnits] of Extended = (1/8, 0.25, 2.5, 1, 10, 6);
  // Determines the number of dot markers between Numbers and Markers.
  DotMarkers: array[TRulerUnits] of Integer = (3, 1, 3, 2, 4, 2);
  // Used for "SnapToRuler"
  GridSteps: array[TRulerUnits] of Extended = (1/16, 0.25, 2.5, 0.5, 5, 6);

  BorderOffset: Integer = 4;
  ColumnOffset: Integer = 4;
  IndentOffset: Integer = 4;
  TabOffset: array[TTabAlign] of Integer = (0, 3, 4, 3, 3);

function IMax(A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function IMin(A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

{ TTab }

procedure TTab.AssignTo(Dest: TPersistent);
begin
  if Dest is TTab then
  begin
    with Dest as TTab do
    begin
      FAlign := Self.Align;
      FLeft := Self.Left;
      FTop := Self.Top;
    end;
  end
  else
    inherited;
end;

constructor TTab.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FAlign := taLeftAlign;
  FColor := clBlack;
end;

function TTab.GetPixels: Extended;
begin
  with TTabs(Collection).Ruler do
    Result := UnitsToPixs(Position);
end;

function TTab.GetPoints: Extended;
begin
  with TTabs(Collection).Ruler do
    Result := UnitsToPoints(Position);
end;

function TTab.GetRTLAlign: TTabAlign;
begin
  Result := FAlign;
  if Assigned(Collection) then
    if TTabs(Collection).Ruler.UseRTL then
      if FAlign = taLeftAlign then
        Result := taRightAlign
      else
        if FAlign = taRightAlign then
          Result := taLeftAlign;
end;

procedure TTab.SetAlign(const Value: TTabAlign);
var
  HotSpot: Integer;
begin
  if Value <> FAlign then
  begin
    case FAlign of
      taLeftAlign:  HotSpot := Left;
      taRightAlign: HotSpot := Left + 5;
    else            HotSpot := Left + 3;
    end;
    // Setup new alignment and the rest of the stuff.
    FAlign := Value;
    case FAlign of
      taLeftAlign:    Left := HotSpot;
      taCenterAlign:  Left := HotSpot - 3;
      taRightAlign:   Left := HotSpot - 5;
      taDecimalAlign: Left := HotSpot - 3;
      taWordBarAlign: Left := HotSpot - 3;
    end;
    Changed(False);
  end;
end;

procedure TTab.SetColor(const Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    TCustomRuler(TTabs(Collection).GetOwner).Invalidate;
  end;
end;

procedure TTab.SetPixels(const Value: Extended);
begin
  with TTabs(Collection).Ruler do
    Position := PixsToUnits(Value);
end;

procedure TTab.SetPoints(const Value: Extended);
begin
  with TTabs(Collection).Ruler do
    Position := PointsToUnits(Value);
end;

procedure TTab.SetPosition(const Value: Extended);
begin
  FPosition := Value;
  with TTabs(Collection) do
  begin
    BeginUpdate;
    try
      SortTabs;
    finally
      EndUpdate;
    end;
  end;
end;

{ TTabs }

function CompareTabs(Item1, Item2: Pointer): Integer;
begin
  if TTab(Item1).Position > TTab(Item2).Position then
    Result := +1
  else if TTab(Item1).Position < TTab(Item2).Position then
    Result := -1
  else
    Result := 0;
end;

function TTabs.Add: TTab;
begin
  Result := TTab(inherited Add);
end;

constructor TTabs.Create(Ruler: TCustomRuler);
begin
  inherited Create(TTab);
  FRuler := Ruler;
end;

function TTabs.GetOwner: TPersistent;
begin
  Result := FRuler;
end;

function TTabs.GetTab(Index: Integer): TTab;
begin
  Result := TTab(inherited Items[Index]);
end;

procedure TTabs.SetTab(Index: Integer; const Value: TTab);
begin
  inherited SetItem(Index, Value);
  FRuler.Invalidate;
end;

procedure TTabs.SortTabs;
var
  I: Integer;
  List: TList;
begin
  List := TList.Create;
  try
    for I := 0 to Count - 1 do
      List.Add(Items[I]);
    List.Sort(CompareTabs);
    for I := 0 to List.Count - 1 do
      TTab(List.Items[I]).Index := I
  finally
    List.Free;
  end;
end;

procedure TTabs.Update(Item: TCollectionItem);
begin
  inherited;
  with FRuler do
  begin
    if csLoading in ComponentState then
      Exit;
    if not FBlockDoTabChanged then
      DoTabChanged;
    Invalidate;
  end;
end;

{ TIndentSettings }

procedure TIndentSettings.AssignTo(Dest: TPersistent);
begin
  if Dest is TIndentSettings then
  begin
    with Dest as TIndentSettings do
    begin
      DragCursor := Self.DragCursor;
      Options := Self.Options;
    end;
  end
  else
    inherited;
end;

constructor TIndentSettings.Create(AOwner: TCustomRuler);
begin
  FOwner := AOwner;
  FDragCursor := crDrag;
  FOptions := DefaultIndentOptions;
end;

procedure TIndentSettings.SetDragCursor(const Value: TCursor);
begin
  if FDragCursor <> Value then
  begin
    FDragCursor := Value;
    FOwner.Invalidate;
  end;
end;

procedure TIndentSettings.SetOptions(const Value: TIndentOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    if ioShowLeftIndent in FOptions then
      Include(FOptions, ioShowHangingIndent);
    if not (ioShowHangingIndent in FOptions) then
      Exclude(FOptions, ioShowLeftIndent);
    FOwner.Invalidate;
  end;
end;

{ TMarginSettings }

procedure TMarginSettings.AssignTo(Dest: TPersistent);
begin
  if Dest is TMarginSettings then
  begin
    with Dest as TMarginSettings do
    begin
      DragCursor := Self.DragCursor;
      GripColor := Self.GripColor;
      Options := Self.Options;
    end;
  end
  else
    inherited;
end;

constructor TMarginSettings.Create(AOwner: TCustomRuler);
begin
  FOwner := AOwner;
  FDragCursor := crSizeWE;
  FGripColor := clBtnShadow;
  FOptions := DefaultMarginOptions;
end;

procedure TMarginSettings.SetDragCursor(const Value: TCursor);
begin
  if FDragCursor <> Value then
  begin
    FDragCursor := Value;
    FOwner.Invalidate;
  end;
end;

procedure TMarginSettings.SetGripColor(const Value: TColor);
begin
  if FGripColor <> Value then
  begin
    FGripColor := Value;
    FOwner.Invalidate;
  end;
end;

procedure TMarginSettings.SetOptions(const Value: TMarginOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    FOwner.Invalidate;
  end;
end;

{ TTabSettings }

procedure TTabSettings.AssignTo(Dest: TPersistent);
begin
  if Dest is TTabSettings then
  begin
    with Dest as TTabSettings do
    begin
      DeleteCursor := Self.DeleteCursor;
      DragCursor := Self.DragCursor;
      Options := Self.Options;
    end;
  end
  else
    inherited;
end;

constructor TTabSettings.Create(AOwner: TCustomRuler);
begin
  FOwner := AOwner;
  FDeleteCursor := crDrag;
  FDragCursor := crDrag;
  FOptions := DefaultTabOptions;
end;

procedure TTabSettings.SetDeleteCursor(const Value: TCursor);
begin
  if FDeleteCursor <> Value then
  begin
    FDeleteCursor := Value;
    FOwner.Invalidate;
  end;
end;

procedure TTabSettings.SetDragCursor(const Value: TCursor);
begin
  if FDragCursor <> Value then
  begin
    FDragCursor := Value;
    FOwner.Invalidate;
  end;
end;

procedure TTabSettings.SetOptions(const Value: TTabOptions);
var
  I: Integer;
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    if not (toAdvancedTabs in Value) then
      for I := 0 to FOwner.Tabs.Count - 1 do
        FOwner.Tabs[I].Align := taLeftAlign;
    FOwner.Invalidate;
  end;
end;

{ TRulerTexts }

constructor TRulerTexts.Create(AOwner: TCustomRuler);
begin
  FRuler := AOwner;
  FHintColumnMove := 'Move Table Column';
  FHintIndentFirst := 'First Indent';
  FHintIndentLeft := 'Left Indent';
  FHintIndentHanging := 'Hanging Indent';
  FHintIndentRight := 'Right Indent';
  FHintLevelDec := 'Decrease level';
  FHintLevelInc := 'Increase level';
  FHintTabCenter := 'Center aligned tab';
  FHintTabDecimal := 'Decimal aligned tab';
  FHintTabLeft := 'Left aligned tab';
  FHintTabRight := 'Right aligned tab';
  FHintTabWordBar := 'Word Bar aligned tab';
  FMenuTabCenter := 'Center align';
  FMenuTabDecimal := 'Decimal align';
  FMenuTabLeft := 'Left align';
  FMenuTabRight := 'Right align';
  FMenuTabWordBar := 'Word Bar align';
end;

procedure TRulerTexts.SetHintColumnMove(const Value: string);
begin
  FHintColumnMove := Value;
end;

procedure TRulerTexts.SetHintIndentFirst(const Value: string);
begin
  FHintIndentFirst := Value;
end;

procedure TRulerTexts.SetHintIndentHanging(const Value: string);
begin
  FHintIndentHanging := Value;
end;

procedure TRulerTexts.SetHintIndentLeft(const Value: string);
begin
  FHintIndentLeft := Value;
end;

procedure TRulerTexts.SetHintIndentRight(const Value: string);
begin
  FHintIndentRight := Value;
end;

procedure TRulerTexts.SetHintLevelDec(const Value: string);
begin
  FHintLevelDec := Value;
end;

procedure TRulerTexts.SetHintLevelInc(const Value: string);
begin
  FHintLevelInc := Value;
end;

procedure TRulerTexts.SetHintTabCenter(const Value: string);
begin
  FHintTabCenter := Value;
end;

procedure TRulerTexts.SetHintTabDecimal(const Value: string);
begin
  FHintTabDecimal := Value;
end;

procedure TRulerTexts.SetHintTabLeft(const Value: string);
begin
  FHintTabLeft := Value;
end;

procedure TRulerTexts.SetHintTabRight(const Value: string);
begin
  FHintTabRight := Value;
end;

procedure TRulerTexts.SetHintTabWordBar(const Value: string);
begin
  FHintTabWordBar := Value;
end;

procedure TRulerTexts.SetMenuTabCenter(const Value: string);
begin
  FMenuTabCenter := Value;
end;

procedure TRulerTexts.SetMenuTabDecimal(const Value: string);
begin
  FMenuTabDecimal := Value;
end;

procedure TRulerTexts.SetMenuTabLeft(const Value: string);
begin
  FMenuTabLeft := Value;
end;

procedure TRulerTexts.SetMenuTabRight(const Value: string);
begin
  FMenuTabRight := Value;
end;

procedure TRulerTexts.SetMenuTabWordBar(const Value: string);
begin
  FMenuTabWordBar := Value;
end;

{ TCustomRuler }

// --- Specialised procedure to directly add tabs from a TRichEdit98 control
procedure TCustomRuler.AddRichEdit98Tab(const Value: Extended; Alignment: TTabAlign);
begin
  with Tabs do
  begin
    BeginUpdate;
    try
      with Add do
      begin
        Points := Value;
        Align := Alignment;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TCustomRuler.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

constructor TCustomRuler.Create(AOwner: TComponent);
var
  DC: HDC;
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];

{$IFDEF COMPILER4_UP}
  DoubleBuffered := True;
{$ELSE}
  ControlStyle   := ControlStyle + [csOpaque];
{$ENDIF}
  Height         := 25;
  Width          := 600;
  FIndentSettings:= TIndentSettings.Create(Self);
  FInset         := 10;
  FListEditor    := TRulerListEditor.Create(Self);
  FMarginColor   := clInfoBk;
  FMarginSettings:= TMarginSettings.Create(Self);
  FMaxTabs       := 32;
  FRulerColor    := clWindow;
  FRulerOptions  := DefaultRulerOptions;
  FRulerTexts    := TRulerTexts.Create(Self);
  FTableEditor   := TRulerTableEditor.Create(Self);
  FTabs          := TTabs.Create(Self);
  FTabSettings   := TTabSettings.Create(Self);
  FUnitsDisplay  := ruCentimeters;
  FUnitsProgram  := ruCentimeters;
  PageHeight     := cmPerInch * 11;  // Standard Letter
  PageWidth      := cmPerInch * 8.5; // Standard Letter
  FRulerType     := rtHorizontal;
  FZoom          := 100;

  if (Owner = nil) or
     (([csReading, csDesigning] * Owner.ComponentState) = [csDesigning]) then
  begin
    // Component is being dropped on a form, give it some nice defaults.
    TopMargin    := 2.54;
    BottomMargin := 2.54;
    LeftMargin   := 2.54;
    RightMargin  := 2.54;
  end;

  DC := GetDC(0);
  try
    FScreenRes := GetDeviceCaps(DC, LOGPIXELSX);  // Pixels per inch
    FMultiPixels := FScreenRes / cmPerInch;
  finally
    ReleaseDC(0, DC);
  end;

  FMultiPoints := PointsPerInch / cmPerInch;

  FItemSeparation := Round(UnitsToPixs(2.54 / 8));

  FIndents[itBoth].Graphic := igRectangle;
  FIndents[itFirst].Graphic := igDown;
  FIndents[itLeft].Graphic := igUp;
  FIndents[itRight].Graphic := igUp;

  FTimer := TTimer.Create(Self);
  with FTimer do
  begin
    Interval := 500;
    OnTimer := TimerProc;
    Enabled := False;
  end;
end;

destructor TCustomRuler.Destroy;
begin
  FIndentSettings.Free;
  FListEditor.Free;
  FMarginSettings.Free;
  FPopupMenu.Free;
  FRulerTexts.Free;
  FTableEditor.Free;
  FTabs.Free;
  FTabSettings.Free;
  inherited Destroy;
end;

procedure TCustomRuler.DoBiDiModeChanged;
begin
  if Assigned(FOnBiDiModeChanged) then
    FOnBiDiModeChanged(Self);
end;

procedure TCustomRuler.DoIndentChanged;
begin
  if Assigned(FOnIndentChanged) then
    FOnIndentChanged(Self);
end;

procedure TCustomRuler.DoLevelButtonDown(Direction: Integer);
begin
end;

procedure TCustomRuler.DoLevelButtonUp(Direction: Integer);
begin
end;

procedure TCustomRuler.DoMarginChanged;
begin
  if Assigned(FOnMarginChanged) then
    FOnMarginChanged(Self);
end;

procedure TCustomRuler.DoRulerItemMove(X: Integer; Removing: Boolean);
begin
  if Assigned(FOnRulerItemMove) then
    FOnRulerItemMove(Self, X, Removing);
end;

procedure TCustomRuler.DoRulerItemRelease;
begin
  if Assigned(FOnRulerItemRelease) then
    FOnRulerItemRelease(Self);
end;

procedure TCustomRuler.DoRulerItemSelect(X: Integer);
begin
  if Assigned(FOnRulerItemSelect) then
    FOnRulerItemSelect(Self, X);
end;

procedure TCustomRuler.DoTabChanged;
begin
  if Assigned(FOnTabChanged) then
    FOnTabChanged(Self);
end;

procedure TCustomRuler.DoTableColumnChanged;
begin
  if Assigned(FOnTableColumnChanged) then
    FOnTableColumnChanged(Self);
end;

function TCustomRuler.GetBottomMargin: Extended;
begin
  Result := FMargins[mtBottom].Position;
end;

function TCustomRuler.GetClosestOnRuler(X: Integer; FromMargin: Boolean): Integer;
var
  Grid: Extended;
  XPos: Extended;
begin
  // Setup display grid steps.
  Grid := GridSteps[UnitsDisplay];
  // Convert to UnitsProgram since this is what the rest is based on.
  Grid := Grid * UnitsPerInch(UnitsProgram) / UnitsPerInch(UnitsDisplay);

  if FromMargin then
  begin
    XPos := ZPixsToUnits(RTLAdjust(X, Inset)) - StartMargin;
    Result := RTLAdjust(Inset + Round(ZUnitsToPixs(StartMargin + Grid * Round(XPos / Grid))), 0);
  end
  else
  begin
    XPos := ZPixsToUnits(RTLAdjust(X, Inset));
    Result := RTLAdjust(Inset + Round(ZUnitsToPixs(Grid * Round(XPos / Grid))), 0);
  end;
end;

function TCustomRuler.GetDefaultTabWidth: Extended;
begin
  if FDefaultTabWidth <= 0 then
    FDefaultTabWidth := 0.5 * UnitsPerInch(UnitsProgram);
  Result := FDefaultTabWidth;
end;

function TCustomRuler.GetEndMargin: Extended;
begin
  if UseRTL then
    Result := LeftMargin
  else
    Result := RightMargin;
end;

function TCustomRuler.GetEndMarginGrip: Integer;
begin
  if UseRTL then
    Result := FMargins[mtLeft].Grip
  else
    Result := FMargins[mtRight].Grip;
end;

function TCustomRuler.GetFirstIndent: Extended;
begin
  Result := FIndents[itFirst].Position;
end;

function TCustomRuler.GetHitTestInfoAt(X, Y: Integer): THitInfo;
var
  BT: TBorderType;
  IT: TIndentType;
  MT: TMarginType;
  P: TPoint;
begin
  Result.HitItems := [];

  P := Point(X, Y);

  if RulerType = rtHorizontal then
  begin
    // First check if it is on one of the indent sliders
    for IT := Low(TIndentType) to High(TIndentType) do
      if PtInRect(GetIndentRect(IT), P) then
        case IT of
          itBoth: Include(Result.HitItems, hiOnBothIndent);
          itFirst: Include(Result.HitItems, hiOnFirstIndent);
          itLeft: Include(Result.HitItems, hiOnLeftIndent);
          itRight: Include(Result.HitItems, hiOnRightIndent);
        end;
    if ListEditor.Active and (leoLevelAdjustable in ListEditor.Options) then
    begin
      if PtInRect(GetLevelRect(ltLevelDec), P) then
        if UseRTL then
          Include(Result.HitItems, hiOnLevelInc)
        else
          Include(Result.HitItems, hiOnLevelDec);
      if PtInRect(GetLevelRect(ltLevelInc), P) then
        if UseRTL then
          Include(Result.HitItems, hiOnLevelDec)
        else  
          Include(Result.HitItems, hiOnLevelInc);
    end;
    if Result.HitItems <> [] then
      Exit;

    // Second, check if it is on one of the tabs.
    Result.Index := GetTabIndexAt(X, Y);
    if Result.Index <> -1 then
    begin
      Result.HitItems := Result.HitItems + [hiOnTab];
      Exit;
    end;

    if TableEditor.Active and (teoAdjustable in TableEditor.Options) then
    begin
      // Third, check if it is on a Table Column.
      Result.Index := TableEditor.GetColumnIndexAt(X, Y);
      if Result.Index <> -1 then
      begin
        Result.HitItems := Result.HitItems + [hiOnColumn];
        Exit;
      end;

      // Fourth, check if it is on a Table Border.
      for BT := Low(TBorderType) to High(TBorderType) do
        if PtInRect(TableEditor.GetBorderRect(BT), P) then
          case BT of
// !!!      btLeft:  Include(Result.HitItems, hiOnLeftBorder);
            btRight: Include(Result.HitItems, hiOnRightBorder);
          end;
      if Result.HitItems <> [] then
        Exit;
    end;

    // Fifth, check if it is on a margin
    for MT := mtLeft to mtRight do
      if PtInRect(GetMarginGripRect(MT), P) then
        case MT of
          mtLeft: Include(Result.HitItems, hiOnLeftMargin);
          mtRight: Include(Result.HitItems, hiOnRightMargin);
        end;
  end
  else
  begin
    // For vertical ruler only check Top and BottomMargin
    for MT := mtTop to mtBottom do
      if PtInRect(GetMarginGripRect(MT), P) then
        case MT of
          mtTop: Include(Result.HitItems, hiOnTopMargin);
          mtBottom: Include(Result.HitItems, hiOnBottomMargin);
        end;
  end;
end;

function TCustomRuler.GetIndentRect(IndentType: TIndentType): TRect;
var
  H: Integer;
begin
  with FIndents[IndentType] do
  begin
    if Graphic = igRectangle then
      H := 7
    else
      H := 8;
    Result := Rect(Left, Top, Left + 9, Top + H);
  end;
end;

function TCustomRuler.GetLeftIndent: Extended;
begin
  Result := FIndents[itLeft].Position;
end;

function TCustomRuler.GetLeftMargin: Extended;
begin
  Result := FMargins[mtLeft].Position;
end;

function TCustomRuler.GetLevelRect(LevelType: TLevelType): TRect;
begin
  Result := GetIndentRect(itBoth);
  case LevelType of
    ltLevelDec:
      begin
        Result.Left := Result.Left - 8;
        Result.Right := Result.Right - 8;
      end;
    ltLevelInc:
      begin
        Result.Left := Result.Left + 8;
        Result.Right := Result.Right + 8;
      end;
  end;
end;

function TCustomRuler.GetMarginGripRect(MarginType: TMarginType): TRect;
var
  R: TRect;
begin
  R := GetMarginRect(MarginType);
  case MarginType of
    mtTop: Result := Rect(R.Left, R.Bottom - 2, R.Right, R.Bottom + 2);
    mtBottom: Result := Rect(R.Left, R.Top - 2, R.Right, R.Top + 2);
    mtLeft: Result := Rect(R.Right - 2, R.Top, R.Right + 2, R.Bottom);
    mtRight: Result := Rect(R.Left - 2, R.Top, R.Left + 2, R.Bottom);
  end;
end;

function TCustomRuler.GetMarginRect(MarginType: TMarginType): TRect;
var
  B, L, R, T: Integer;
begin
  if RulerType = rtHorizontal then
  begin
    T := 4;
    B := Height - 10;
    if (MarginType = mtLeft) xor UseRTL then
    begin
      L := Inset + 1;
      R := Inset + Round(ZUnitsToPixs(StartMargin));
      if UseRTL then Dec(L);
      Result := RTLAdjustRect(Rect(L, T, R, B));
    end
    else
    begin
      L := Inset + Round(ZUnitsToPixs(PageWidth - EndMargin));
      R := Inset + Round(ZUnitsToPixs(PageWidth));
      if UseRTL then Dec(R);
      Result := RTLAdjustRect(Rect(L, T, R, B));
    end;
  end
  else
  begin
    L := 4;
    R := Width - 3;
    if MarginType = mtTop then
    begin
      T := Inset + 1;
      B := Inset + Round(ZUnitsToPixs(TopMargin));
      Result := Rect(L, T, R, B);
    end
    else
    begin
      T := Inset + Round(ZUnitsToPixs(PageHeight - BottomMargin));
      B := Inset + Round(ZUnitsToPixs(PageHeight));
      Result := Rect(L, T, R, B);
    end;
  end;
end;

function TCustomRuler.GetRightIndent: Extended;
begin
  Result := FIndents[itRight].Position;
end;

function TCustomRuler.GetRightMargin: Extended;
begin
  Result := FMargins[mtRight].Position;
end;

function TCustomRuler.GetStartMargin: Extended;
begin
  if UseRTL then
    Result := RightMargin
  else
    Result := LeftMargin;
end;

function TCustomRuler.GetStartMarginGrip: Integer;
begin
  if UseRTL then
    Result := FMargins[mtRight].Grip
  else
    Result := FMargins[mtLeft].Grip;
end;

function TCustomRuler.GetTabIndexAt(X, Y: Integer): Integer;
var
  I: Integer;
  W: Integer;
begin
  Result := -1;
  for I := 0 to Tabs.Count - 1 do
    with Tabs[I] do
    begin
      case Align of
        taLeftAlign,
        taRightAlign: W := 6;
      else            W := 8;
      end;
      if (X >= Left) and (X < Left + W) and
         (Y >= Top)  and (Y < Top + 6) then
        Result := I;
    end;
end;

function TCustomRuler.GetTopMargin: Extended;
begin
  Result := FMargins[mtTop].Position;
end;

function TCustomRuler.HitItemsToIndex(HitItems: THitItems): Integer;
begin
  Result := -1;
  if hiOnBothIndent in HitItems then
    Result := Integer(itBoth)
  else if hiOnFirstIndent in HitItems then
    Result := Integer(itFirst)
  else if hiOnLeftIndent in HitItems then
    Result := Integer(itLeft)
  else if hiOnRightIndent in HitItems then
    Result := Integer(itRight)
  else if hiOnLeftBorder in HitItems then
    Result := Integer(btLeft)
  else if hiOnRightBorder in HitItems then
    Result := Integer(btRight)
  else if hiOnTopMargin in HitItems then
    Result := Integer(mtTop)
  else if hiOnBottomMargin in HitItems then
    Result := Integer(mtBottom)
  else if hiOnLeftMargin in HitItems then
    Result := Integer(mtLeft)
  else if hiOnRightMargin in HitItems then
    Result := Integer(mtRight);
end;

procedure TCustomRuler.Loaded;
begin
  inherited;
  UpdatePageDimensions;
end;

procedure TCustomRuler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  BT: TBorderType;
  HitInfo: THitInfo;
  IT: TIndentType;
  NewTab: TTab;
  P: TPoint;
begin
  // Turn off hints while dragging. It can obscure the RichEdit when
  // the user uses vertical marker lines within the RichEdit.
  FOldShowHint := ShowHint;
  ShowHint := False;

  HitInfo := GetHitTestInfoAt(X, Y);
  if not (moAdjustable in MarginSettings.Options) then
    HitInfo.HitItems := HitInfo.HitItems - AllMargins;

  if Button = mbLeft then
  begin
    if (RulerType = rtHorizontal) and (HitInfo.HitItems = []) then
    begin
      // Add a new tab when the user clicked on a 'blank' part of the ruler.
      if Tabs.Count < MaxTabs then
        with Tabs do
        begin
          BeginUpdate;
          try
            NewTab := Add;
            with NewTab do
              if toSnapToRuler in TabSettings.Options then
                X := GetClosestOnRuler(X, True) + TabOffset[RTLAlign];
            if TableEditor.Active then
              NewTab.Position := ZPixsToUnits(TableEditor.RTLAdjust(X, 0) -
                TableEditor.GetCellRect(TableEditor.CellIndex).Left)
            else
              NewTab.Position := ZPixsToUnits(RTLAdjust(X, Inset)) - StartMargin;
          finally
            FBlockDoTabChanged := True;
            try
              EndUpdate;
            finally
              FBlockDoTabChanged := False;
            end;  
          end;
          FDragInfo.Item := diTab;
          FDragInfo.Index := NewTab.Index;
          FDragInfo.Offset := TabOffset[NewTab.RTLAlign];
          ProcessParentBackGround(True);
          Tabs[FDragInfo.Index].Left := X - TabOffset[NewTab.RTLAlign];
          DoRulerItemSelect(X);
        end;
    end
    else
    begin
      if (AllIndents * HitInfo.HitItems) <> [] then
      begin
        Screen.Cursor := IndentSettings.DragCursor;
        FDragInfo.Item := diIndent;
        FDragInfo.Index := HitItemsToIndex(HitInfo.HitItems);
        IT := TIndentType(FDragInfo.Index);
        if roItemsShowLastPos in Options then
          FIndentTraces := FIndents;
        if hiOnBothIndent in HitInfo.HitItems then
          FDiff := FIndents[itFirst].Left - FIndents[itLeft].Left
        else
          FDiff := 0;
        if Screen.Cursor = crHSplit then
          FDragInfo.Offset := IndentOffset
        else
          FDragInfo.Offset := X - FIndents[IT].Left;

        X := FIndents[IT].Left + IndentOffset;
        if IndentSettings.DragCursor = crHSplit then
        begin
          P := ClientToScreen(Point(X, Y));
          Windows.SetCursorPos(P.X, P.Y)
        end;
        ProcessParentBackGround(True);
        DoRulerItemSelect(X);
      end
      else if (AllLevels * HitInfo.HitItems) <> [] then
      begin
        if hiOnLevelDec in HitInfo.HitItems then
          DoLevelButtonDown(-1);
        if hiOnLevelInc in HitInfo.HitItems  then
          DoLevelButtonDown(+1);
      end
      else if hiOnTab in HitInfo.HitItems then
      begin
        Screen.Cursor := TabSettings.DragCursor;
        FDragInfo.Item := diTab;
        FDragInfo.Index := HitInfo.Index;
        FDragInfo.Offset := X - Tabs[FDragInfo.Index].Left;
        if roItemsShowLastPos in Options then
        begin
          FTabTrace := TTab.Create(nil);
          FTabTrace.Assign(Tabs[FDragInfo.Index]);
          FTabTrace.FAlign := Tabs[FDragInfo.Index].RTLAlign;
        end;
        ProcessParentBackGround(True);
        with Tabs[FDragInfo.Index] do
          X := Left + TabOffset[RTLAlign];
        DoRulerItemSelect(X);
      end
      else if hiOnColumn in HitInfo.HitItems then
      begin
        TableEditor.FDraggedWithShift := ssShift in Shift;
        TableEditor.FDragStart := X;
        Screen.Cursor := TableEditor.DragCursor;
        FDragInfo.Item := diColumn;
        FDragInfo.Index := HitInfo.Index;
        if Screen.Cursor = crHSplit then
          FDragInfo.Offset := ColumnOffset
        else
          FDragInfo.Offset := X - TableEditor.Cells[FDragInfo.Index].Left;
        ProcessParentBackGround(True);
        with TableEditor.Cells[FDragInfo.Index] do
          X := Left + ColumnOffset;
        DoRulerItemSelect(X);
      end
      else if (AllBorders * HitInfo.HitItems) <> [] then
      begin
        TableEditor.FDraggedWithShift := False;
        Screen.Cursor := TableEditor.DragCursor;
        FDragInfo.Item := diBorder;
        FDragInfo.Index := HitItemsToIndex(HitInfo.HitItems);
        BT := TBorderType(FDragInfo.Index);
        if Screen.Cursor = crHSplit then
          FDragInfo.Offset := BorderOffset
        else
          FDragInfo.Offset := X - TableEditor.FBorders[BT].Left;
        ProcessParentBackGround(True);
        with TableEditor.FBorders[BT] do
          X := Left + BorderOffset;
        DoRulerItemSelect(X);
      end
      else if (AllMargins * HitInfo.HitItems) <> [] then
      begin
        if RulerType = rtHorizontal then
        begin
          Screen.Cursor := MarginSettings.DragCursor;
          FDragInfo.Item := diMargin;
          FDragInfo.Index := HitItemsToIndex(HitInfo.HitItems);
          FDragInfo.Offset := X - FMargins[TMarginType(FDragInfo.Index)].Grip;
          ProcessParentBackGround(True);
          X := FMargins[TMarginType(FDragInfo.Index)].Grip;
          DoRulerItemSelect(X);
        end
        else
        begin
          Screen.Cursor := MarginSettings.DragCursor;
          FDragInfo.Item := diMargin;
          FDragInfo.Index := HitItemsToIndex(HitInfo.HitItems);
          FDragInfo.Offset := Y - FMargins[TMarginType(FDragInfo.Index)].Grip;
          ProcessParentBackGround(True);
          Y := FMargins[TMarginType(FDragInfo.Index)].Grip;
          DoRulerItemSelect(Y);
        end;
      end;
    end;
  end;
  inherited;
end;

procedure TCustomRuler.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  BT: TBorderType;
  HitInfo: THitInfo;
  I: Integer;
  IT: TIndentType;
  MT: TMarginType;
  PageWidthInPixels: Integer;

  function GetTabHint(TA: TTabAlign): string;
  begin
    case TA of
      taCenterAlign:  Result := RulerTexts.HintTabCenter;
      taDecimalAlign: Result := RulerTexts.HintTabDecimal;
      taRightAlign:   Result := RulerTexts.HintTabRight;
      taWordBarAlign: Result := RulerTexts.HintTabWordBar;
    else
      Result := RulerTexts.HintTabLeft;
    end;
  end;

  function KeepDragWithinPageArea(X, Offset: Integer): Integer;
  begin
    Result := X;
    if UseRTL then
    begin
      Result := IMin(Result, RTLAdjust(Inset, Offset));
      Result := IMax(Result, RTLAdjust(Inset + PageWidthInPixels, Offset));
    end
    else
    begin
      Result := IMax(Result, Inset - Offset);
      Result := IMin(Result, Inset - Offset + PageWidthInPixels);
    end;
  end;

begin
  if FDragInfo.Item = diNone then
  begin
    HitInfo := GetHitTestInfoAt(X, Y);
    if (TableEditor.Active and
        ((hiOnColumn in HitInfo.HitItems) or
         (hiOnRightBorder in HitInfo.HitItems)) and
        (teoAdjustable in TableEditor.Options)) or
       ((moAdjustable in MarginSettings.Options) and
        ((HitInfo.HitItems - AllMargins) = []) and
        ((HitInfo.HitItems * AllMargins) <> [])) then
      Windows.SetCursor(Screen.Cursors[MarginSettings.DragCursor])
    else
      Windows.SetCursor(Screen.Cursors[Cursor]);

    if hiOnTab in HitInfo.HitItems then
      OverrideHint(GetTabHint(Tabs[HitInfo.Index].Align))
    else if hiOnBothIndent in HitInfo.HitItems then
      OverrideHint(RulerTexts.HintIndentLeft)
    else if hiOnFirstIndent in HitInfo.HitItems then
      OverrideHint(RulerTexts.HintIndentFirst)
    else if hiOnLeftIndent in HitInfo.HitItems then
      OverrideHint(RulerTexts.HintIndentHanging)
    else if hiOnRightIndent in HitInfo.HitItems then
      OverrideHint(RulerTexts.HintIndentRight)
    else if hiOnLevelDec in HitInfo.HitItems then
      OverrideHint(RulerTexts.HintLevelDec)
    else if hiOnLevelInc in HitInfo.HitItems then
      OverrideHint(RulerTexts.HintLevelInc)
    else if hiOnColumn in HitInfo.HitItems then
      OverrideHint(RulerTexts.HintColumnMove)
    else if hiOnLeftBorder in HitInfo.HitItems then
      OverrideHint(RulerTexts.HintColumnMove)
    else if hiOnRightBorder in HitInfo.HitItems then
      OverrideHint(RulerTexts.HintColumnMove)
    else
      RestoreHint;
  end;

  PageWidthInPixels := Round(ZUnitsToPixs(PageWidth));

  if FDragInfo.Item = diIndent then
  begin
    IT := TIndentType(FDragInfo.Index);
    with FIndents[IT] do
    begin
      if ioSnapToRuler in IndentSettings.Options then
        Left := GetClosestOnRuler(X, True) - IndentOffset
      else
        Left := X - FDragInfo.Offset;

      // Keep the First/Left/Both indents separated from the right indent
      if not UseRTL then
      begin
        // Keep the indent within the page-area
        if FDiff < 0 then Left := Left + FDiff;
        Left := KeepDragWithinPageArea(Left, IndentOffset);
        if FDiff < 0 then Left := Left - FDiff;

        if IT in [itFirst, itLeft] then
        begin
          Left := IMin(Left, FIndents[itRight].Left - ItemSeparation);
          if ioKeepWithinMargins in IndentSettings.Options then
            Left := IMax(Left, StartMarginGrip - IndentOffset);
          if TableEditor.Active then
            Left := TableEditor.KeepWithinCurrentCell(IT, Left);
        end;
        if IT = itBoth then
        begin
          if FDiff > 0 then Left := Left + FDiff;
          Left := IMin(Left, FIndents[itRight].Left - ItemSeparation);
          if FDiff > 0 then Left := Left - FDiff;
          if ioKeepWithinMargins in IndentSettings.Options then
          begin
            if FDiff < 0 then Left := Left + FDiff;
            Left := IMax(Left, StartMarginGrip - IndentOffset);
            if FDiff < 0 then Left := Left - FDiff;
          end;
          if TableEditor.Active then
          begin
            if FDiff < 0 then Left := Left + FDiff;
            Left := TableEditor.KeepWithinCurrentCell(IT, Left);
            if FDiff < 0 then Left := Left - FDiff;
          end;
        end;
        if IT = itRight then
        begin
          Left := IMax(Left, FIndents[itFirst].Left + ItemSeparation);
          Left := IMax(Left, FIndents[itLeft].Left + ItemSeparation);
          if ioKeepWithinMargins in IndentSettings.Options then
            Left := IMin(Left, EndMarginGrip - IndentOffset);
          if TableEditor.Active then
            Left := TableEditor.KeepWithinCurrentCell(IT, Left);
        end;
      end
      else
      begin
        // Keep the indent within the page-area
        if FDiff > 0 then Left := Left + FDiff;
        Left := KeepDragWithinPageArea(Left, IndentOffset);
        if FDiff > 0 then Left := Left - FDiff;

        if IT in [itFirst, itLeft] then
        begin
          Left := IMax(Left, FIndents[itRight].Left + ItemSeparation);
          if ioKeepWithinMargins in IndentSettings.Options then
            Left := IMin(Left, StartMarginGrip - IndentOffset);
          if TableEditor.Active then
            Left := TableEditor.KeepWithinCurrentCell(IT, Left);
        end;
        if IT = itBoth then
        begin
          if FDiff < 0 then Left := Left + FDiff;
          Left := IMax(Left, FIndents[itRight].Left + ItemSeparation);
          if FDiff < 0 then Left := Left - FDiff;
          if ioKeepWithinMargins in IndentSettings.Options then
          begin
            if FDiff > 0 then Left := Left + FDiff;
            Left := IMin(Left, StartMarginGrip - IndentOffset);
            if FDiff > 0 then Left := Left - FDiff;
          end;
          if TableEditor.Active then
          begin
            if FDiff > 0 then Left := Left + FDiff;
            Left := TableEditor.KeepWithinCurrentCell(IT, Left);
            if FDiff > 0 then Left := Left - FDiff;
          end;
        end;
        if IT = itRight then
        begin
          Left := IMin(Left, FIndents[itFirst].Left - ItemSeparation);
          Left := IMin(Left, FIndents[itLeft].Left - ItemSeparation);
          if ioKeepWithinMargins in IndentSettings.Options then
            Left := IMax(Left, EndMarginGrip - IndentOffset);
          if TableEditor.Active then
            Left := TableEditor.KeepWithinCurrentCell(IT, Left);
        end;
      end;
    end;

    // Keep Idents 'synchronized' when needed.
    if IT = itLeft then
    begin
      FIndents[itBoth].Left := FIndents[itLeft].Left;
    end;
    if IT = itBoth then
    begin
      FIndents[itFirst].Left := FIndents[itBoth].Left + FDiff;
      FIndents[itLeft].Left := FIndents[itBoth].Left;
    end;

    X := FIndents[IT].Left + IndentOffset;
    DoRulerItemMove(X, False);
    Invalidate;
  end;

  if FDragInfo.Item = diTab then
  begin
    with Tabs[FDragInfo.Index] do
    begin
      FDeleting := (Y < 0) or (Y > Self.Height);
      if toSnapToRuler in TabSettings.Options then
        Left := GetClosestOnRuler(X, True) - TabOffset[RTLAlign]
      else
        Left := X - FDragInfo.Offset;

      // Keep the tabs within the page-area
      Left := KeepDragWithinPageArea(Left, TabOffset[RTLAlign]);

      if FDeleting then
        Screen.Cursor := TabSettings.DeleteCursor
      else
        Screen.Cursor := TabSettings.DragCursor;

      X := Left + TabOffset[RTLAlign];
    end;
    DoRulerItemMove(X, False);
    Invalidate;
  end;

  if FDragInfo.Item = diColumn then
  begin
    with TableEditor.Cells[FDragInfo.Index] do
    begin
      if teoSnapToRuler in TableEditor.Options then
        Left := GetClosestOnRuler(X, True) - ColumnOffset
      else
        Left := X - FDragInfo.Offset;

      Left := KeepDragWithinPageArea(Left, ColumnOffset);
      Left := TableEditor.KeepColumnsSeparated(FDragInfo.Index, Left);

      X := Left + ColumnOffset;
      TableEditor.FDragLast := X;
    end;
    DoRulerItemMove(X, False);
    Invalidate;
  end;

  if FDragInfo.Item = diBorder then
  begin
    BT := TBorderType(FDragInfo.Index);
    with TableEditor.FBorders[BT] do
    begin
      if teoSnapToRuler in TableEditor.Options then
        Left := GetClosestOnRuler(X, True) - BorderOffset
      else
        Left := X - FDragInfo.Offset;

      Left := KeepDragWithinPageArea(Left, BorderOffset);
      if BT = btLeft then
        Left := TableEditor.KeepColumnsSeparated(-1, Left)
      else
        Left := TableEditor.KeepColumnsSeparated(TableEditor.Cells.Count-1, Left);

      X := Left + BorderOffset;
    end;
    DoRulerItemMove(X, False);
    Invalidate;
  end;

  if FDragInfo.Item = diMargin then
  begin
    MT := TMarginType(FDragInfo.Index);
    if RulerType = rtHorizontal then
    begin
      with FMargins[MT] do
      begin
        if moSnapToRuler in MarginSettings.Options then
          Grip := GetClosestOnRuler(X, False)
        else
          Grip := X - FDragInfo.Offset;

        // Keep the margins within the page-area
        Grip := KeepDragWithinPageArea(Grip, 0);

        // Keep the Left and Right margins separated.
        if MT = mtLeft then
          Grip := IMin(Grip, FMargins[mtRight].Grip - ItemSeparation)
        else
          Grip := IMax(Grip, FMargins[mtLeft].Grip + ItemSeparation);

        // Keep the indents separated
        if not UseRTL then
        begin
          if MT = mtLeft then
          begin
            I := Round(ZUnitsToPixs(FirstIndent));
            if FIndents[itLeft].Left > FIndents[itFirst].Left then
              I := I + FIndents[itLeft].Left - FIndents[itFirst].Left;
            Grip := IMin(Grip, FIndents[itRight].Left + IndentOffset - I - ItemSeparation);
          end
          else
          begin
            I := Round(ZUnitsToPixs(RightIndent));
            Grip := IMax(Grip, FIndents[itFirst].Left + IndentOffset + I + ItemSeparation);
            Grip := IMax(Grip, FIndents[itLeft].Left + IndentOffset + I + ItemSeparation);
          end;
        end
        else
        begin
          if MT = mtLeft then
          begin
            I := Round(ZUnitsToPixs(RightIndent));
            Grip := IMin(Grip, FIndents[itFirst].Left + IndentOffset - I - ItemSeparation);
            Grip := IMin(Grip, FIndents[itLeft].Left + IndentOffset - I - ItemSeparation);
          end
          else
          begin
            I := Round(ZUnitsToPixs(FirstIndent));
            if FIndents[itLeft].Left < FIndents[itFirst].Left then
              I := I - FIndents[itLeft].Left + FIndents[itFirst].Left;
            Grip := IMax(Grip, FIndents[itRight].Left + IndentOffset + I + ItemSeparation);
          end;
        end;
      end;
      X := FMargins[MT].Grip;
      DoRulerItemMove(X, False);
      Invalidate;
    end
    else
    begin
      with FMargins[MT] do
      begin
        if moSnapToRuler in MarginSettings.Options then
          Grip := GetClosestOnRuler(Y, False)
        else
          Grip := Y - FDragInfo.Offset;

        // Keep the margins within the page-area
        Grip := IMax(Grip, Inset);
        Grip := IMin(Grip, Inset + Round(ZUnitsToPixs(PageHeight)));

        // Keep the Top and Bottom margins separated.
        if MT = mtTop then
          Grip := IMin(Grip, FMargins[mtBottom].Grip - ItemSeparation)
        else
          Grip := IMax(Grip, FMargins[mtTop].Grip + ItemSeparation);
      end;
      Y := FMargins[MT].Grip;
      DoRulerItemMove(Y, False);
      Invalidate;
    end;
  end;

  inherited;
end;

procedure TCustomRuler.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  BT: TBorderType;
  Delta: Extended;
  HitInfo: THitInfo;
  IT: TIndentType;
  MT: TMarginType;
  NewWidth: Extended;
  P: TPoint;
  XPos: Integer;
  YPos: Integer;

  function NewMenuItem(ACaption: string; AOnClick: TNotifyEvent; ATag: TTabAlign): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Caption := ACaption;
    Result.OnClick := AOnClick;
    Result.Tag := Integer(ATag);
  end;

begin
  if FDragInfo.Item = diIndent then
  begin
    try
      for IT := Low(TIndentType) to High(TIndentType) do
        FIndentTraces[IT].Graphic := igNone;

      IT := TIndentType(FDragInfo.Index);
      XPos := FIndents[IT].Left + IndentOffset;

      if TableEditor.Active then
      begin
        XPos := TableEditor.RTLAdjust(XPos, 0);
        TableEditor.UpdateIndentPosition(IT, XPos)
      end
      else
      begin
        XPos := RTLAdjust(XPos, Inset);
        if IT = itFirst then
        begin
          FIndents[itLeft].Position := StartMargin + FirstIndent + LeftIndent - ZPixsToUnits(XPos);
          FirstIndent := ZPixsToUnits(XPos) - StartMargin;
        end
        else if IT = itLeft then
          LeftIndent := ZPixsToUnits(XPos) - FirstIndent - StartMargin
        else if IT = itBoth then
          FirstIndent := ZPixsToUnits(XPos) - LeftIndent - StartMargin
        else
          RightIndent := PageWidth - EndMargin - ZPixsToUnits(XPos);
      end;

      DoRulerItemRelease;
      ProcessParentBackGround(False);
    finally
      FDragInfo.Item := diNone;
      Screen.Cursor := crDefault;
    end;
  end;

  if FDragInfo.Item = diTab then
  begin
    FTabTrace.Free;
    FTabTrace := nil;
    // When finished dragging a Tab, calculate the new Position
    // or remove the Tab when it was dragged outside the ruler.
    try
      with Tabs[FDragInfo.Index] do
        if FDeleting then
          Tabs.Items[FDragInfo.Index].Free
        else
        begin
          XPos := Left + TabOffset[RTLAlign];
          if TableEditor.Active then
          begin
            XPos := TableEditor.RTLAdjust(XPos, 0);
            Position := ZPixsToUnits(XPos -
              TableEditor.GetCellRect(TableEditor.CellIndex).Left);
          end
          else
          begin
            XPos := RTLAdjust(XPos, Inset);
            Position := ZPixsToUnits(XPos) - StartMargin;
          end;
        end;

      DoRulerItemRelease;
      ProcessParentBackGround(False);
    finally
      FDragInfo.Item := diNone;
      Screen.Cursor := crDefault;
    end;
  end;

  if FDragInfo.Item = diColumn then
  begin
    try
      TableEditor.FDragLast := TableEditor.FDragStart;
      if TableEditor.DraggedWithShift then
        TableEditor.TableWidth := ZPixsToUnits(TableEditor.FBorders[btRight].Left -
          TableEditor.FBorders[btLeft].Left);
      with TableEditor.Cells[FDragInfo.Index] do
        Position := ZPixsToUnits(Left + ColumnOffset - TableEditor.GetOffset) -
           TableEditor.TableLeft;

      DoRulerItemRelease;
      ProcessParentBackGround(False);
    finally
      FDragInfo.Item := diNone;
      Screen.Cursor := crDefault;
    end;
  end;

  if FDragInfo.Item = diBorder then
  begin
    try
      BT := TBorderType(FDragInfo.Index);
      XPos := TableEditor.FBorders[BT].Left + BorderOffset;

      if BT = btLeft then
        TableEditor.TableLeft := ZPixsToUnits(XPos - TableEditor.GetOffset)
      else
      begin
        NewWidth := ZPixsToUnits(XPos - TableEditor.GetOffset) - TableEditor.TableLeft;
        Delta := NewWidth - TableEditor.TableWidth;
        TableEditor.TableWidth := NewWidth;
        with TableEditor do
          if Cells.Count > 0 then
            with Cells[Cells.Count-1] do
              Position := Position + Delta;
      end;
      DoRulerItemRelease;
      ProcessParentBackGround(False);
    finally
      FDragInfo.Item := diNone;
      Screen.Cursor := crDefault;
    end;
  end;

  if FDragInfo.Item = diMargin then
  begin
    try
      MT := TMarginType(FDragInfo.Index);
      if RulerType = rtHorizontal then
      begin
        XPos := FMargins[MT].Grip;
        XPos := RTLAdjust(XPos, Inset);

        if MT = mtLeft then
        begin
          if UseRTL then
            LeftMargin := PageWidth - ZPixsToUnits(XPos)
          else
            LeftMargin := ZPixsToUnits(XPos)
        end
        else
        begin
          if UseRTL then
            RightMargin := ZPixsToUnits(XPos)
          else
            RightMargin := PageWidth - ZPixsToUnits(XPos);
        end;
      end
      else
      begin
        YPos := FMargins[MT].Grip - Inset;
        if MT = mtTop then
          TopMargin := ZPixsToUnits(YPos)
        else
          BottomMargin := PageHeight - ZPixsToUnits(YPos);
      end;
      DoRulerItemRelease;
      ProcessParentBackGround(False);
    finally
      FDragInfo.Item := diNone;
      Screen.Cursor := crDefault;
    end;
  end;

  HitInfo := GetHitTestInfoAt(X, Y);
  if (AllLevels * HitInfo.HitItems) <> [] then
  begin
    if hiOnLevelDec in HitInfo.HitItems then
      DoLevelButtonUp(-1);
    if hiOnLevelInc in HitInfo.HitItems then
      DoLevelButtonUp(+1);
  end;

  // Create and show a popupmenu when right-clicked on a Tab.
  if (toAdvancedTabs in TabSettings.Options) and (Button = mbRight) then
  begin
    HitInfo := GetHitTestInfoAt(X, Y);
    if (hiOnTab in HitInfo.HitItems) and (HitInfo.Index >= 0) then
    begin
      if not Assigned(FPopupMenu) then
      begin
        FPopupmenu := TPopupMenu.Create(Self);
        with FPopupMenu, RulerTexts do
        begin
          Items.Add(NewMenuItem(MenuTabLeft, PopupClick, taLeftAlign));
          Items.Add(NewMenuItem(MenuTabCenter, PopupClick, taCenterAlign));
          Items.Add(NewMenuItem(MenuTabRight, PopupClick, taRightAlign));
          Items.Add(NewMenuItem(MenuTabDecimal, PopupClick, taDecimalAlign));
          Items.Add(NewMenuItem(MenuTabWordBar, PopupClick, taWordBarAlign));
        end;
      end;

      with FPopupMenu do
      begin
        Tag := HitInfo.Index;
        P := Self.ClientToScreen(Point(X, Y));
        Popup(P.X, P.Y);
      end;
    end;
  end;

  ShowHint := FOldShowHint;
  inherited;
end;

procedure TCustomRuler.OverrideHint(NewHint: string);
var
  P: TPoint;
{$IFNDEF COMPILER5_UP}
  M: TWMMouse;
{$ENDIF}
begin
  if not FOvrHint then
    FOrgHint := Hint;
  Hint := NewHint;
  if not FOvrHint then
  begin
    GetCursorPos(P);
{$IFDEF COMPILER5_UP}
    Application.ActivateHint(P);
{$ELSE}
    M.Pos.X := -1;
    M.Pos.Y := -1;
    Application.HintMouseMessage(Self, TMessage(M));
    M.Pos.X := P.X;
    M.Pos.Y := P.Y;
    Application.HintMouseMessage(Self, TMessage(M));
{$ENDIF}
  end;
  FOvrHint := True;
end;

procedure TCustomRuler.Paint;
var
  ACanvas: TCanvas;
{$IFNDEF COMPILER4_UP}
  ABitmap: TBitmap;
  R: TRect;
{$ELSE}
  DC: THandle;
  LastOrigin: TPoint;
{$ENDIF}
begin
{$IFNDEF COMPILER4_UP}
  ABitmap := TBitmap.Create;
  try
    // Buffer data in local bitmap
    ABitmap.Width  := Width;
    ABitmap.Height := Height;
    ACanvas := ABitmap.Canvas;
    ACanvas.Brush.Color := Self.Color;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.FillRect(ClientRect);
{$ELSE}
    // Use original canvas
    ACanvas := Canvas;

  {$IFDEF COMPILER7_UP}
    if FOldParentBackground then
    begin
  {$ELSE}
    if ParentColor or ((Parent <> nil) and (Parent.Brush.Color = Color)) then
    begin
  {$ENDIF}
      DC := ACanvas.Handle;
      GetWindowOrgEx(DC, LastOrigin);
      SetWindowOrgEx(DC, LastOrigin.X + Left, LastOrigin.Y + Top, nil);
      Self.Parent.Perform(WM_ERASEBKGND, Integer(DC), Integer(DC));
      SetWindowOrgEx(DC, LastOrigin.X, LastOrigin.Y, nil);

      // Above code might change font settings for DC without TCanvas knowing
      // about it. Next lines are there to force TCanvas to reset its internal
      // State and to (re)create the drawing objects when needed.
      ACanvas.Handle := 0;
      ACanvas.Handle := DC;
    end;
{$ENDIF}

    if RulerType = rtHorizontal then
      PaintHorizontalRuler(ACanvas)
    else
      PaintVerticalRuler(ACanvas);

{$IFNDEF COMPILER4_UP}
    R := ClientRect;
    Canvas.CopyMode := cmSrcCopy;
    Canvas.Draw(R.Left, R.Top, ABitmap);
  finally
    ABitmap.Free
  end;
{$ENDIF}
end;

procedure TCustomRuler.PaintHorizontalRuler(Canvas: TCanvas);
var
  R: TRect;
begin
  R := Rect(0, 0, Width, Height - 7);
  PaintHOutline(Canvas, R);
  PaintHMargins(Canvas, R);
  PaintHMarkers(Canvas, R);
  PaintHTableGraphics(Canvas, R);
  PaintHDefaultTabs(Canvas, R);
  PaintHTabs(Canvas, R);
  PaintHIndents(Canvas, R);
end;

procedure TCustomRuler.PaintHDefaultTabs(Canvas: TCanvas; R: TRect);
var
  N: Integer;
  X: Integer;
  XAdj: Integer;
  XInd: Integer;
  XMax: Integer;
  XMin: Integer;
  XTab: Integer;
begin
  if toShowDefaultTabs in TabSettings.Options then
  begin
    if TableEditor.Active then
      with TableEditor do
      begin
        XMin := GetCellRect(CellIndex).Left;
        XMax := GetCellRect(CellIndex).Right;
      end
    else
    begin
      XMin := RTLAdjust(StartMarginGrip, 0);
      XMax := RTLAdjust(EndMarginGrip, 0);
    end;

    // Don't display default tabs before the first actual tab.
    if Tabs.Count > 0 then
      XTab := XMin + Round(ZUnitsToPixs(Tabs[Tabs.Count-1].Position))
    else
      XTab := XMin;

    if toDontShowDefaultTabsBeforeLeftIndent in TabSettings.Options then
    begin
      if TableEditor.Active then
        XInd := TableEditor.CalculateCurrentIndentPosition(itLeft)
      else
        XInd := XMin + Round(ZUnitsToPixs(FirstIndent + LeftIndent));
      if XInd > XTab then
        XTab := XInd;
    end;

    N := 0;
    with Canvas do
    begin
      Pen.Color := clBtnShadow;
      repeat
        Inc(N);
        X := XMin + ZoomAndRound(N * UnitsToPixs(DefaultTabWidth));
        if (X > XTab) and (X < XMax) then
        begin
          if TableEditor.Active then
            XAdj := TableEditor.RTLAdjust(X, 0)
          else
            XAdj := RTLAdjust(X, 0);
          if Flat then
          begin
            MoveTo(XAdj, R.Bottom);
            LineTo(XAdj, R.Bottom + 2);
          end
          else
          begin
            MoveTo(XAdj, R.Bottom - 2);
            LineTo(XAdj, R.Bottom);
          end;
        end;
      until X >= XMax;
    end;
  end;
end;

procedure TCustomRuler.PaintHIndent(Canvas: TCanvas; Graphic: TIndentGraphic;
  X, Y: Integer; HighLight, ExtraShadow, Dimmed: Boolean);
var
  Points: array[0..5] of TPoint;
begin
  if Dimmed then
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.Pen.Color := clBtnShadow;
    HighLight := False;
    ExtraShadow := False;
  end
  else
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.Pen.Color := clBlack;
  end;

  // Using polygons instead of normal line drawing and floodfill.
  // Floodfill does not always fill the entire area. This especially
  // happens when scrolling the stuff in and out of a view area.
  with Canvas do
    case Graphic of
      igUp:
        begin
          Points[0] := Point(X+4, Y+0);
          Points[1] := Point(X+8, Y+4);
          Points[2] := Point(X+8, Y+7);
          Points[3] := Point(X+0, Y+7);
          Points[4] := Point(X+0, Y+4);
          Points[5] := Point(X+0, Y+4);
          Polygon(Points);
          if HighLight then
          begin
            Pen.Color := clWhite;
            MoveTo(X+4, Y+1);
            LineTo(X+1, Y+4);
            LineTo(X+1, Y+6);
          end;
          if ExtraShadow then
          begin
            Pen.Color := clBtnShadow;
            LineTo(X+7, Y+6);
            LineTo(X+7, Y+4);
            LineTo(X+3, Y);
          end;
        end;
      igDown:
        begin
          Points[0] := Point(X+0, Y+0);
          Points[1] := Point(X+8, Y+0);
          Points[2] := Point(X+8, Y+3);
          Points[3] := Point(X+4, Y+7);
          Points[4] := Point(X+0, Y+3);
          Points[5] := Point(X+0, Y+3);
          Polygon(Points);
          if HighLight then
          begin
            Pen.Color := clWhite;
            MoveTo(X+4, Y+6);
            LineTo(X+1, Y+3);
            LineTo(X+1, Y+1);
            LineTo(X+7, Y+1);
          end;
          if ExtraShadow then
          begin
            Pen.Color := clBtnShadow;
            LineTo(X+7, Y+3);
            LineTo(X+3, Y+7);
          end;
        end;
      igRectangle,
      igLevelDec2,
      igLevelInc2:
        begin
          Points[0] := Point(X+0, Y+0);
          Points[1] := Point(X+8, Y+0);
          Points[2] := Point(X+8, Y+6);
          Points[3] := Point(X+0, Y+6);
          Points[4] := Point(X+0, Y+0);
          Points[5] := Point(X+0, Y+0);
          Polygon(Points);
          if HighLight then
          begin
            Pen.Color := clWhite;
            MoveTo(X+1, Y+4);
            LineTo(X+1, Y+1);
            LineTo(X+7, Y+1);
          end;
          if ExtraShadow then
          begin
            Pen.Color := clBtnShadow;
            LineTo(X+7, Y+5);
            LineTo(X+0, Y+5);
          end;
          case Graphic of
            igLevelDec2:
              begin
                Pen.Color := clBlack;
                MoveTo(X+6, Y+3);
                LineTo(X+1, Y+3);
                MoveTo(X+4, Y+1);
                LineTo(X+4, Y+6);
                MoveTo(X+3, Y+2);
                LineTo(X+3, Y+5);
              end;
            igLevelInc2:
              begin
                Pen.Color := clBlack;
                MoveTo(X+2, Y+3);
                LineTo(X+7, Y+3);
                MoveTo(X+4, Y+1);
                LineTo(X+4, Y+6);
                MoveTo(X+5, Y+2);
                LineTo(X+5, Y+5);
              end;
          end;
        end;
      igLevelDec1:
        begin
          Points[0] := Point(X+8, Y+0);
          Points[1] := Point(X+5, Y+0);
          Points[2] := Point(X+2, Y+3);
          Points[3] := Point(X+5, Y+6);
          Points[4] := Point(X+8, Y+6);
          Points[5] := Point(X+8, Y+0);
          Polygon(Points);
          if HighLight then
          begin
            Pen.Color := clWhite;
            MoveTo(X+3, Y+3);
            LineTo(X+5, Y+1);
            LineTo(X+7, Y+1);
          end;
          if ExtraShadow then
          begin
            Pen.Color := clBtnShadow;
            LineTo(X+7, Y+5);
            LineTo(X+5, Y+5);
            LineTo(X+2, Y+2);
          end;
        end;
      igLevelInc1:
        begin
          Points[0] := Point(X+0, Y+0);
          Points[1] := Point(X+3, Y+0);
          Points[2] := Point(X+6, Y+3);
          Points[3] := Point(X+3, Y+6);
          Points[4] := Point(X+0, Y+6);
          Points[5] := Point(X+0, Y+0);
          Polygon(Points);
          if HighLight then
          begin
            Pen.Color := clWhite;
            MoveTo(X+1, Y+4);
            LineTo(X+1, Y+1);
            LineTo(X+3, Y+1);
          end;
          if ExtraShadow then
          begin
            Pen.Color := clBtnShadow;
            LineTo(X+5, Y+3);
            LineTo(X+3, Y+5);
            LineTo(X+0, Y+5);
          end;
        end;
    end;
end;

procedure TCustomRuler.PaintHIndents(Canvas: TCanvas; R: TRect);
var
  I: TIndentType;
begin
  if roItemsShowLastPos in Options then
  begin
    for I := High(TIndentType) downto Low(TIndentType) do
      with FIndentTraces[I] do
      begin
        PaintHIndent(Canvas, Graphic, Left, Top, False, False, True);
        if (I = itBoth) and (Graphic <> igNone) and
           ListEditor.Active and (leoLevelAdjustable in ListEditor.Options) then
        begin
          if ListEditor.LevelGraphic = lgType1 then
          begin
            PaintHIndent(Canvas, igLevelDec1, Left - 8, Top, False, False, True);
            PaintHIndent(Canvas, igLevelInc1, Left + 8, Top, False, False, True);
          end
          else
          begin
            PaintHIndent(Canvas, igLevelDec2, Left - 8, Top, False, False, True);
            PaintHIndent(Canvas, igLevelInc2, Left + 8, Top, False, False, True);
          end;
        end;
      end;
  end;

  for I := High(TIndentType) downto Low(TIndentType) do
    with FIndents[I] do
    begin
      case I of
        itBoth:
          begin
            Top := R.Bottom;
            if FDragInfo.Item <> diIndent then
            begin
              if TableEditor.Active then
              begin
                Left := TableEditor.CalculateCurrentIndentPosition(I);
                Left := TableEditor.RTLAdjust(Left, IndentOffset);
              end
              else
              begin
                Left := RTLAdjust(StartMarginGrip, 0) +
                  Round(ZUnitsToPixs(FirstIndent + LeftIndent));
                Left := RTLAdjust(Left, IndentOffset)
              end;
            end;
            if not (ioShowLeftIndent in IndentSettings.Options) then Continue;
          end;
        itFirst:
          begin
            Top := 0;
            if FDragInfo.Item <> diIndent then
            begin
              if TableEditor.Active then
              begin
                Left := TableEditor.CalculateCurrentIndentPosition(I);
                Left := TableEditor.RTLAdjust(Left, IndentOffset);
              end
              else
              begin
                Left := RTLAdjust(StartMarginGrip, 0) +
                  Round(ZUnitsToPixs(FirstIndent));
                Left := RTLAdjust(Left, IndentOffset)
              end;
            end;
            if not (ioShowFirstIndent in IndentSettings.Options) then Continue;
          end;
        itLeft:
          begin
            Top := R.Bottom - 7;
            if FDragInfo.Item <> diIndent then
            begin
              if TableEditor.Active then
              begin
                Left := TableEditor.CalculateCurrentIndentPosition(I);
                Left := TableEditor.RTLAdjust(Left, IndentOffset);
              end
              else
              begin
                Left := RTLAdjust(StartMarginGrip, 0) +
                  Round(ZUnitsToPixs(FirstIndent + LeftIndent));
                Left := RTLAdjust(Left, IndentOffset)
              end;
            end;
            if not (ioShowHangingIndent in IndentSettings.Options) then Continue;
          end;
        itRight:
          begin
            Top := R.Bottom - 7;
            if FDragInfo.Item <> diIndent then
            begin
              if TableEditor.Active then
              begin
                Left := TableEditor.CalculateCurrentIndentPosition(I);
                Left := TableEditor.RTLAdjust(Left, IndentOffset);
              end
              else
              begin
                Left := RTLAdjust(EndMarginGrip, 0) -
                  Round(ZUnitsToPixs(RightIndent));
                Left := RTLAdjust(Left, IndentOffset)
              end;
            end;
            if not (ioShowRightIndent in IndentSettings.Options) then Continue;
          end;
       end;

      PaintHIndent(Canvas, Graphic, Left, Top, True,
        ioExtraShadow in IndentSettings.Options, False);
      if (I = itBoth) and (Graphic <> igNone) and
         ListEditor.Active and (leoLevelAdjustable in ListEditor.Options) then
      begin
        if ListEditor.LevelGraphic = lgType1 then
        begin
          PaintHIndent(Canvas, igLevelDec1, Left - 8, Top, True,
            ioExtraShadow in IndentSettings.Options, False);
          PaintHIndent(Canvas, igLevelInc1, Left + 8, Top, True,
            ioExtraShadow in IndentSettings.Options, False);
        end
        else
        begin
          PaintHIndent(Canvas, igLevelDec2, Left - 8, Top, True,
            ioExtraShadow in IndentSettings.Options, False);
          PaintHIndent(Canvas, igLevelInc2, Left + 8, Top, True,
            ioExtraShadow in IndentSettings.Options, False);
        end;
      end;  
    end;
end;

procedure TCustomRuler.PaintHMargins(Canvas: TCanvas; R: TRect);
var
  MT: TMarginType;
  MR: TRect;
begin
  for MT := mtLeft to mtRight do
    with FMargins[MT], Canvas do
    begin
      MR := GetMarginRect(MT);
      if MT = mtLeft then
      begin
        if FDragInfo.Item <> diMargin then
          Grip := MR.Right;
        MR.Right := Grip;
      end
      else
      begin
        if FDragInfo.Item <> diMargin then
          Grip := MR.Left;
        MR.Left := Grip + 1;
      end;
      if MR.Left < 2 then
        MR.Left := 2;
      if MR.Right > (Width - 2) then
        MR.Right := (Width - 2);
      if MR.Left > MR.Right then
        Continue;
      if Flat then
      begin
        Dec(MR.Top, 2);
        Inc(MR.Bottom, 2);
        if MT = mtLeft then
          Dec(MR.Left)
        else
          Inc(MR.Right);
      end;
      Brush.Color := MarginColor;
      FillRect(MR);

      if (MarginSettings.GripColor <> MarginColor) and
         (MarginSettings.GripColor <> RulerColor) then
      begin
        if MT = mtLeft then
        begin
          MR.Left := IMax(MR.Left, MR.Right - 2);
          MR.Right := MR.Right + 3;
        end
        else
        begin
          MR.Right := IMin(MR.Right, MR.Left + 2);
          MR.Left := MR.Left - 3;
        end;
        if MR.Left > MR.Right then Continue;

        Brush.Color := MarginSettings.GripColor;
        FillRect(MR);
      end;
    end;
end;

procedure TCustomRuler.PaintHMarkers(Canvas: TCanvas; R: TRect);
var
  ClipR: HRGN;
  D: Integer;
  DotH: Integer;
  DotX: Integer;
  DotY: Integer;
  I: Integer;
  MarkH: Integer;
  MarkX: Integer;
  MarkY: Integer;
  N: Integer;
  M: Extended;
  MG: Integer;
  Number: Integer;
  NumbX: Integer;
  NumbY: Integer;
  S: string;
  TH: Integer;
  TW: Integer;
  TR: TRect;
  Z: Integer;
begin
  Canvas.Font := Self.Font;
  TH    := Canvas.TextHeight('0123456789');
  DotH  := TH div 5;                        // Height of the dot markers
  DotY  := ((R.Bottom - 2 - DotH) div 2) + 2;
  MarkH := TH div 3;                        // Height of the halfway markers
  MarkY := ((R.Bottom - 2 - MarkH) div 2) + 2;

  N := NumberIncrements[UnitsDisplay];
  M := MarkerIncrements[UnitsDisplay];
  D := DotMarkers[UnitsDisplay];

  Z := IMax(1, Round(100 / Zoom));
  N := Z * N;
  M := Z * M;

  if roScaleRelativeToMargin in Options then
  begin
    MG := RTLAdjust(StartMarginGrip, Inset);
    Number := Pred(Trunc(ZPixsToUnits(-MG) * UnitsPerInch(UnitsDisplay) / UnitsPerInch(UnitsProgram)));
  end
  else
  begin
    MG := 0;
    Number := 0;
  end;
  with Canvas do
  begin
    TR := ClientRect;
    TR.Left := TR.Left + Inset;
    TR := RTLAdjustRect(TR);
    InflateRect(TR, -3, -3);

    // Prevent the markers to paint over the right edge.
    ClipR := CreateRectRgn(TR.Left, TR.Top, TR.Right, TR.Bottom);
    SelectClipRgn(Handle, ClipR);
    DeleteObject(ClipR);

    Pen.Color := clBlack;
    Brush.Style := bsClear;
    repeat
      Inc(Number);
      NumbX := Number * ScreenRes;       // X offset of center of number markers (inch based)
      MarkX := NumbX - ScreenRes div 2;  // X offset of center of halfway markers

      NumbX := ZoomAndRound(N * NumbX / UnitsPerInch(UnitsDisplay));
      MarkX := ZoomAndRound(N * MarkX / UnitsPerInch(UnitsDisplay));

      NumbX := Inset + NumbX;            // X position of number markers
      MarkX := Inset + MarkX;            // X position of halfway markers

      NumbX := MG + NumbX;               // Adjust for possible relative display
      MarkX := MG + MarkX;

      MarkX := RTLAdjust(MarkX, 0);
      NumbX := RTLAdjust(NumbX, 0);

      S := IntToStr(Abs(N * Number));
      TW := TextWidth(S);
      NumbX := NumbX - (TW div 2);       // Center number markers
      NumbY := ((R.Bottom - 2 - TH) div 2) + 2;
      TextRect(TR, NumbX, NumbY, S);

      // Draw halfway markers
      if UnitsDisplay in [ruPicas, ruPoints] then
      begin
        MoveTo(MarkX, DotY);
        LineTo(MarkX, DotY + DotH);
      end
      else
      begin
        MoveTo(MarkX, MarkY);
        LineTo(MarkX, MarkY + MarkH);
      end;

      // Draw dot markers
      for I := 1 to D do
      begin
        Z := ZoomAndRound(I * M * ScreenRes / UnitsPerInch(UnitsDisplay));
        DotX := MarkX + Z;
        MoveTo(DotX, DotY);
        LineTo(DotX, DotY + DotH);
        DotX := MarkX - Z;
        MoveTo(DotX, DotY);
        LineTo(DotX, DotY + DotH);
      end;

      NumbX := RTLAdjust(NumbX, 0);
    until NumbX > Width;

    SelectClipRgn(Canvas.Handle, 0);
  end;
end;

procedure TCustomRuler.PaintHOutline(Canvas: TCanvas; R: TRect);
var
  R2: TRect;

  function LocalRTLAdjustRect(const R: TRect): TRect;
  begin
    if UseRTL then
      Result := Rect(Width - R.Right, R.Top, Width - R.Left, R.Bottom)
    else
      Result := R;
  end;

begin
  with Canvas do
  begin
    Brush.Color := RulerColor;
    Brush.Style := bsSolid;
    R2 := Rect(Inset, 2, R.Right, R.Bottom - 1);
    R2 := LocalRTLAdjustRect(R2);
    if R2.Left < 1 then
      R2.Left := 0;
    if R2.Right > Width then
      R2.Right := Width;
    if R2.Left < R2.Right then
    begin
      FillRect(R2);
      if UseRTL then
      begin
        Dec(R2.Left);
        Inc(R2.Right);
      end
      else
      begin
        Dec(R2.Left);
        Inc(R2.Right);
      end;
      if R2.Left < 1 then
        R2.Left := 0;
      if R2.Right > Width then
        R2.Right := Width;
      if not Flat then
        Windows.DrawEdge(Canvas.Handle, R2, EDGE_SUNKEN, BF_RECT);
    end;

    // Area to the right of the paper
    Brush.Color := clBtnFace;
    R2.Left := Inset + Round(ZUnitsToPixs(PageWidth)) + 1;
    R2 := Rect(R2.Left, 2, R.Right, R.Bottom - 1);
    R2 := LocalRTLAdjustRect(R2);
    if R2.Left < R2.Right then
    begin
      FillRect(R2);
      if UseRTL then
        Inc(R2.Right, 2)
      else
        Dec(R2.Left);
      if not Flat then
        Windows.DrawEdge(Canvas.Handle, R2, EDGE_ETCHED, BF_RECT);
    end;
  end;
end;

procedure TCustomRuler.PaintHTableGraphic(Canvas: TCanvas; Graphic: TTableGraphic;
  R: TRect);
var
  I: Integer;
  X, Y, B: Integer;
begin
  X := R.Left;
  Y := R.Top;
  B := R.Bottom;
  with Canvas do
  begin
    if Flat then
    begin
      Pen.Color := TableEditor.BackGroundColor;
      Brush.Color := TableEditor.BackGroundColor;
      Rectangle(R.Left, R.Top+2, R.Right, R.Bottom-2);
    end
    else
    begin
      Pen.Color := clBlack;
      Brush.Color := TableEditor.BackGroundColor;
      Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      Pen.Color := clWhite;
      MoveTo(X+7, Y+1);
      LineTo(X+1, Y+1);
      LineTo(X+1, B-2);
    end;

    case Graphic of
      tgBorder:
        begin
          Pen.Color := TableEditor.ForeGroundColor;
          I := Y + 3;
          while I < B - 3 do
          begin
            MoveTo(X+2, I);
            LineTo(X+3, I);
            MoveTo(X+4, I);
            LineTo(X+5, I);
            MoveTo(X+6, I);
            LineTo(X+7, I);
            I := I + 2;
          end;
        end;
      tgColumn:
        begin
          Pen.Color := TableEditor.ForeGroundColor;
          MoveTo(X+2, Y+5);
          LineTo(X+3, Y+5);
          LineTo(X+3, B-6);
          LineTo(X+1, B-6);
          MoveTo(X+6, Y+5);
          LineTo(X+5, Y+5);
          LineTo(X+5, B-6);
          LineTo(X+7, B-6);
        end;
    end;
  end;
end;

procedure TCustomRuler.PaintHTableGraphics(Canvas: TCanvas; R: TRect);
var
  BT: TBorderType;
  I: Integer;
  RG: TRect;
begin
  if not TableEditor.Active or (TableEditor.Cells.Count < 1) then
    Exit;

  for BT := Low(TBorderType) to High(TBorderType) do
    with TableEditor, TableEditor.FBorders[BT] do
    begin
      if FDragInfo.Item <> diBorder then
      begin
        Left := TableEditor.Offset + Round(ZUnitsToPixs(Position)) - BorderOffset;
        if DraggedWithShift and (BT = btRight) then
          Left := Left + FDragLast - FDragStart;
      end
      else
        if (Integer(btLeft) = FDragInfo.Index) and (BT = btRight) then
          Left := TableEditor.FBorders[btLeft].Left +
            Round(ZUnitsToPixs(TableEditor.TableWidth));
      RG := Rect(Left, 0, Left + 9, R.Bottom + 1);
      PaintHTableGraphic(Canvas, tgBorder, RG)
    end;

  // Never paint the last ColumnGraphic.
  for I := 0 to TableEditor.LastValidCellIndex - 1 do
    with TableEditor, TableEditor.Cells[I] do
      if CellWidth >= 0 then
      begin
        if (FDragInfo.Item <> diColumn) then
          Left := FBorders[btLeft].Left + Trunc(ZUnitsToPixs(Position))
        else
          if (DraggedWithShift and (I > FDragInfo.Index)) then
            Left := FBorders[btLeft].Left + FDragLast - FDragStart +
              Trunc(ZUnitsToPixs(Position));
        RG := Rect(Left, 0, Left + 9, R.Bottom + 1);
        PaintHTableGraphic(Canvas, tgColumn, RG);
      end;
end;

procedure TCustomRuler.PaintHTab(Canvas: TCanvas; Graphic: TTabAlign;
  X, Y: Integer);
begin
  if UseRTL then
    X := Pred(X);
  with Canvas do
    case Graphic of
      taLeftAlign:
        begin
          MoveTo(X+0, Y+0);
          LineTo(X+0, Y+5);
          LineTo(X+6, Y+5);
          MoveTo(X+1, Y+0);
          LineTo(X+1, Y+4);
          LineTo(X+6, Y+4);
        end;
      taCenterAlign:
        begin
          MoveTo(X+0, Y+5);
          LineTo(X+8, Y+5);
          MoveTo(X+0, Y+4);
          LineTo(X+8, Y+4);
          MoveTo(X+3, Y+0);
          LineTo(X+3, Y+4);
          MoveTo(X+4, Y+0);
          LineTo(X+4, Y+4);
        end;
      taRightAlign:
        begin
          MoveTo(X+5, Y+0);
          LineTo(X+5, Y+5);
          LineTo(X-1, Y+5);
          MoveTo(X+4, Y+0);
          LineTo(X+4, Y+4);
          LineTo(X-1, Y+4);
        end;
      taDecimalAlign:
        begin
          MoveTo(X+0, Y+5);
          LineTo(X+8, Y+5);
          MoveTo(X+0, Y+4);
          LineTo(X+8, Y+4);
          MoveTo(X+3, Y+0);
          LineTo(X+3, Y+4);
          MoveTo(X+4, Y+0);
          LineTo(X+4, Y+4);
          MoveTo(X+6, Y+1);
          LineTo(X+8, Y+1);
          MoveTo(X+6, Y+2);
          LineTo(X+8, Y+2);
        end;
      taWordBarAlign:
        begin
          MoveTo(X+3, Y+0);
          LineTo(X+3, Y+6);
          MoveTo(X+4, Y+0);
          LineTo(X+4, Y+6);
        end;
    end;
end;

procedure TCustomRuler.PaintHTabs(Canvas: TCanvas; R: TRect);
var
  I: Integer;
begin
  if roItemsShowLastPos in Options then
    if Assigned(FTabTrace) then
    begin
      Canvas.Pen.Color := clBtnShadow;
      with FTabTrace do
        PaintHTab(Canvas, RTLAlign, Left, Top);
    end;

  for I := 0 to Tabs.Count - 1 do
  begin
    with Tabs[I] do
    begin
      if FDeleting then
        Canvas.Pen.Color := clBtnFace
      else
        Canvas.Pen.Color := Color;
      if FDragInfo.Item <> diTab then
        if TableEditor.Active then
        begin
          Left := TableEditor.GetCellRect(TableEditor.CellIndex).Left;
          Left := Left + Round(ZUnitsToPixs(Position));
          Left := TableEditor.RTLAdjust(Left, TabOffset[RTLAlign]);
        end
        else
        begin
          Left := RTLAdjust(StartMarginGrip, 0);
          Left := Left + Round(ZUnitsToPixs(Position));
          Left := RTLAdjust(Left, TabOffset[RTLAlign]);
        end;
      Top := R.Bottom - 8;
      PaintHTab(Canvas, RTLAlign, Left, Top);
    end;
  end;
end;

procedure TCustomRuler.PaintVerticalRuler(Canvas: TCanvas);
var
  R: TRect;
begin
  R := Rect(0, 0, Width, Height);
  PaintVOutline(Canvas, R);
  PaintVMargins(Canvas, R);
  PaintVMarkers(Canvas, R);
end;

procedure TCustomRuler.PaintVMargins(Canvas: TCanvas; R: TRect);
var
  MT: TMarginType;
  MR: TRect;
begin
  for MT := mtTop to mtBottom do
    with FMargins[MT], Canvas do
    begin
      MR := GetMarginRect(MT);
      if MT = mtTop then
      begin
        if FDragInfo.Item <> diMargin then
          Grip := MR.Bottom;
        MR.Bottom := Grip;
      end
      else
      begin
        if FDragInfo.Item <> diMargin then
          Grip := MR.Top;
        MR.Top := Grip + 1;
      end;
      if MR.Top < 2 then
        MR.Top := 2;
      if MR.Bottom > (Height - 2) then
        MR.Bottom := (Height - 2);
      if MR.Top > MR.Bottom then
        Continue;
      if Flat then
      begin
        Dec(MR.Left, 2);
        Inc(MR.Right, 2);
        if MT = mtTop then
          Dec(MR.Top)
        else
          Inc(MR.Bottom);
      end;
      Brush.Color := MarginColor;
      FillRect(MR);

      if (MarginSettings.GripColor <> MarginColor) and
         (MarginSettings.GripColor <> RulerColor) then
      begin
        if MT = mtTop then
        begin
          MR.Top := IMax(MR.Top, MR.Bottom - 2);
          MR.Bottom := MR.Bottom + 3;
        end
        else
        begin
          MR.Bottom := IMin(MR.Bottom, MR.Top + 2);
          MR.Top := MR.Top - 3;
        end;
        if MR.Top > MR.Bottom then Continue;

        Brush.Color := MarginSettings.GripColor;
        FillRect(MR);
      end;
    end;
end;

procedure TCustomRuler.PaintVMarkers(Canvas: TCanvas; R: TRect);
var
  ClipR: HRGN;
  D: Integer;
  DotX: Integer;
  DotY: Integer;
  DotW: Integer;
  I: Integer;
  MarkX: Integer;
  MarkY: Integer;
  MarkW: Integer;
  N: Integer;
  M: Extended;
  MG: Integer;
  Number: Integer;
  NumbX: Integer;
  NumbY: Integer;
  S: string;
  TH: Integer;
  TW: Integer;
  TR: TRect;
  Z: Integer;
  lf: TLogFont;
  tf: TFont;
begin
  // Get 90 degrees rotated font
  Canvas.Font := Self.Font;
  with Canvas do
  begin
    tf := TFont.Create;
    tf.Assign(Font);
    GetObject(tf.Handle, sizeof(lf), @lf);
    lf.lfEscapement  := 900;  // angle * 10
    lf.lfOrientation := 900;
    tf.Handle := CreateFontIndirect(lf);
    Font.Assign(tf);
    tf.Free;
  end;

  TH    := Canvas.TextHeight('0123456789');
  DotW  := TH div 5;                     // Width of the dot markers
  DotX  := ((R.Right - 2 - DotW) div 2) + 2;
  MarkW := TH div 3;                     // Width of the halfway markers
  MarkX := ((R.Right - 2 - MarkW) div 2) + 2;

  N := NumberIncrements[UnitsDisplay];
  M := MarkerIncrements[UnitsDisplay];
  D := DotMarkers[UnitsDisplay];

  Z := IMax(1, Round(100 / Zoom));
  N := Z * N;
  M := Z * M;

  if roScaleRelativeToMargin in Options then
  begin
    MG := FMargins[mtTop].Grip - Inset;
    Number := Pred(Trunc(ZPixsToUnits(-MG) * UnitsPerInch(UnitsDisplay) / UnitsPerInch(UnitsProgram)));
  end
  else
  begin
    MG := 0;
    Number := 0;
  end;
  with Canvas do
  begin
    TR := ClientRect;
    TR.Top := TR.Top + Inset;
    InflateRect(TR, -3, -3);

    // Prevent the markers to paint over the right edge.
    ClipR := CreateRectRgn(TR.Left, TR.Top, TR.Right, TR.Bottom);
    SelectClipRgn(Handle, ClipR);
    DeleteObject(ClipR);

    Pen.Color := clBlack;
    Brush.Style := bsClear;
    repeat
      Inc(Number);
      NumbY := Number * ScreenRes;       // Y offset of center of number markers (inch based)
      MarkY := NumbY - ScreenRes div 2;  // Y offset of center of half-inch markers

      NumbY := ZoomAndRound(N * NumbY / UnitsPerInch(UnitsDisplay));
      MarkY := ZoomAndRound(N * MarkY / UnitsPerInch(UnitsDisplay));

      NumbY := Inset + NumbY;            // Y position of number markers
      MarkY := Inset + MarkY;            // Y position of halfway markers

      NumbY := MG + NumbY;               // Adjust for possible relative display
      MarkY := MG + MarkY;

      S := IntToStr(Abs(N * Number));
      TW := TextWidth(S);
      NumbY := NumbY + (TW div 2);          // Center number markers
      NumbX := ((R.Right - 2 - TH) div 2) + 2;
      TextRect(TR, NumbX, NumbY, S);

      // Draw halfway markers
      if UnitsDisplay in [ruPicas, ruPoints] then
      begin
        MoveTo(DotX, MarkY);
        LineTo(DotX + DotW, MarkY);
      end
      else
      begin
        MoveTo(MarkX, MarkY);
        LineTo(MarkX + MarkW, MarkY);
      end;

      // Draw dot markers
      for I := 1 to D do
      begin
        Z := ZoomAndRound(I * M * ScreenRes / UnitsPerInch(UnitsDisplay));
        DotY := MarkY + Z;
        MoveTo(DotX, DotY);
        LineTo(DotX + DotW, DotY);
        DotY := MarkY - Z;
        MoveTo(DotX, DotY);
        LineTo(DotX + DotW, DotY);
      end;
    until NumbY > Height;

    SelectClipRgn(Canvas.Handle, 0);
  end;
end;

procedure TCustomRuler.PaintVOutline(Canvas: TCanvas; R: TRect);
var
  R2: TRect;
begin
  with Canvas do
  begin
    Brush.Color := RulerColor;
    Brush.Style := bsSolid;
    R2 := Rect(2, Inset, R.Right - 1, R.Bottom - 1);
    if R2.Top < 1 then
      R2.Top := 0;
    if R2.Top < R2.Bottom then
    begin
      FillRect(R2);
      Dec(R2.Top);
      Inc(R2.Bottom);
      if R2.Top < 1 then
        R2.Top := 0;
      if not Flat then
        Windows.DrawEdge(Canvas.Handle, R2, EDGE_SUNKEN, BF_RECT);
    end;

    // Area at the bottom of the paper
    Brush.Color := clBtnFace;
    R2.Top := Inset + Round(ZUnitsToPixs(PageHeight)) + 1;
    R2 := Rect(2, R2.Top, R.Right - 1, R.Bottom - 1);
    if R2.Top < R2.Bottom then
    begin
      FillRect(R2);
      Dec(R2.Top);
      Inc(R2.Bottom);
      if not Flat then
        Windows.DrawEdge(Canvas.Handle, R2, EDGE_ETCHED, BF_RECT);
    end;
  end;
end;

function TCustomRuler.PixsToUnits(const Value: Extended): Extended;
begin
  Result := Value / MultiPixels;
end;

function TCustomRuler.PointsToUnits(const Value: Extended): Extended;
begin
  Result := Value / MultiPoints;
end;

procedure TCustomRuler.PopupClick(Sender: TObject);
var
  I: Integer;
begin
  // The Popupmenu itself holds the TabIndex in its Tag property
  I := (Sender as TMenuItem).GetParentComponent.Tag;
  Tabs[I].Align := TTabAlign(TMenuItem(Sender).Tag);
end;

procedure TCustomRuler.ProcessParentBackground(B: Boolean);
begin
{$IFDEF COMPILER7_UP}
  if B then
  begin
    FOldParentBackground := ParentBackground;
    if FOldParentBackground then
      ParentBackground := False;
  end
  else
    if FOldParentBackground then
      ParentBackground := True;
{$ENDIF}
end;

procedure TCustomRuler.RestoreHint;
var
  P: TPoint;
{$IFNDEF COMPILER5_UP}
  M: TWMMouse;
{$ENDIF}
begin
  if FOvrHint then
  begin
    FOvrHint := False;
    Hint := FOrgHint;
    GetCursorPos(P);
{$IFDEF COMPILER5_UP}
    Application.ActivateHint(P);
{$ELSE}
    M.Pos.X := -1;
    M.Pos.Y := -1;
    Application.HintMouseMessage(Self, TMessage(M));
    M.Pos.X := P.X;
    M.Pos.Y := P.Y;
    Application.HintMouseMessage(Self, TMessage(M));
{$ENDIF}
  end;
end;

function TCustomRuler.RTLAdjust(X, Offset: Integer): Integer;
begin
  if UseRTL then
    Result := Pred(Width - (X + Offset))
  else
    Result := X - Offset;
end;

function TCustomRuler.RTLAdjustRect(const R: TRect): TRect;
begin
  if UseRTL then
    Result := Rect(Pred(Width - R.Right), R.Top, Pred(Width - R.Left), R.Bottom)
  else
    Result := R;
end;

procedure TCustomRuler.SetBiDiModeRuler(const Value: TBiDiModeRuler);
begin
  if FBiDiModeRuler <> Value then
  begin
    FBiDiModeRuler := Value;
    DoBiDiModeChanged;
    Invalidate;
  end;
end;

procedure TCustomRuler.SetBottomMargin(Value: Extended);
begin
  if Value <> BottomMargin then
  begin
    if not (csLoading in ComponentState) then
      if Value < 0 then
        Value := 0
      else
        if TopMargin + Value > PageHeight then
          Value := PageHeight - TopMargin;
    FMargins[mtBottom].Position := Value;
    DoMarginChanged;
    Invalidate;
  end;
end;

procedure TCustomRuler.SetDefaultTabWidth(const Value: Extended);
begin
  if FDefaultTabWidth <> Value then
  begin
    if Value > 0 then
      FDefaultTabWidth := Value;
    Invalidate;
  end;
end;

procedure TCustomRuler.SetFirstIndent(Value: Extended);
var
  MinValue: Extended;
  WorkWidth: Extended;
begin
  if Value <> FirstIndent then
  begin
    if not (csLoading in ComponentState) then
    begin
      if ioKeepWithinMargins in IndentSettings.Options then
        MinValue := 0
      else
        MinValue := -StartMargin;
      if Value < MinValue then
      begin
        FIndents[itLeft].Position := FIndents[itLeft].Position + Value;
        Value := MinValue
      end
      else
      begin
        WorkWidth := PageWidth - RightMargin - LeftMargin;
        if Value > WorkWidth then
        begin
          FIndents[itLeft].Position := FIndents[itLeft].Position + Value - WorkWidth;
          Value := WorkWidth;
        end;
      end;
    end;
    FIndents[itFirst].Position := Value;
    DoIndentChanged;
    Invalidate;
  end;
end;

procedure TCustomRuler.SetFlat(const Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TCustomRuler.SetIndentSettings(const Value: TIndentSettings);
begin
  FIndentSettings.Assign(Value);
end;

procedure TCustomRuler.SetInset(const Value: Integer);
begin
  if Value <> FInset then
  begin
    FInset := Value;
    Invalidate;
  end;
end;

procedure TCustomRuler.SetItemSeparation(const Value: Integer);
begin
  FItemSeparation := Value;
end;

procedure TCustomRuler.SetLeftIndent(Value: Extended);
var
  MinValue: Extended;
  WorkWidth: Extended;
begin
  if Value <> LeftIndent then
  begin
    if not (csLoading in ComponentState) then
    begin
      if ioKeepWithinMargins in IndentSettings.Options then
        MinValue := 0
      else
        MinValue := -StartMargin;
      if FirstIndent + Value < MinValue then
        Value := -FirstIndent
      else
      begin
        WorkWidth := PageWidth - RightMargin - LeftMargin;
        if FirstIndent + Value >= WorkWidth - RightIndent then
          Value := WorkWidth - RightIndent - FirstIndent
      end;
    end;
    FIndents[itLeft].Position := Value;
    DoIndentChanged;
    Invalidate;
  end;
end;

procedure TCustomRuler.SetLeftMargin(Value: Extended);
begin
  if Value <> LeftMargin then
  begin
    if not (csLoading in ComponentState) then
      if Value < 0 then
        Value := 0
      else
        if RightMargin + Value > PageWidth then
          Value := PageWidth - RightMargin;
    FMargins[mtLeft].Position := Value;
    DoMarginChanged;
    Invalidate;
  end;
end;

procedure TCustomRuler.SetListEditor(const Value: TRulerListEditor);
begin
  FListEditor := Value;
end;

procedure TCustomRuler.SetMarginColor(const Value: TColor);
begin
  if Value <> FMarginColor then
  begin
    FMarginColor := Value;
    Invalidate;
  end;
end;

procedure TCustomRuler.SetMarginSettings(const Value: TMarginSettings);
begin
  FMarginSettings.Assign(Value);
end;

procedure TCustomRuler.SetMaxTabs(const Value: Integer);
begin
  if Value <> FMaxTabs then
  begin
    FMaxTabs := Value;
    while Tabs.Count > FMaxTabs do
      Tabs.Items[Tabs.Count-1].Free;
  end;
end;

procedure TCustomRuler.SetPageHeight(const Value: Extended);
begin
  if FPageHeight <> Value then
  begin
    FPageHeight := Value;
    Invalidate;
  end;
end;

procedure TCustomRuler.SetPageWidth(const Value: Extended);
begin
  if FPageWidth <> Value then
  begin
    FPageWidth := Value;
    Invalidate;
  end;
end;

procedure TCustomRuler.SetRightIndent(Value: Extended);
var
  MinValue: Extended;
begin
  if Value <> RightIndent then
  begin
    if not (csLoading in ComponentState) then
    begin
      if ioKeepWithinMargins in IndentSettings.Options then
        MinValue := 0
      else
        MinValue := -EndMargin;
      if Value < MinValue then
        Value := MinValue
      else
        if PageWidth - EndMargin - Value <= StartMargin + FirstIndent then
          Value := StartMargin + FirstIndent - PageWidth + EndMargin;
    end;
    FIndents[itRight].Position := Value;
    DoIndentChanged;
    Invalidate;
  end;
end;

procedure TCustomRuler.SetRightMargin(Value: Extended);
begin
  if Value <> RightMargin then
  begin
    if not (csLoading in ComponentState) then
      if Value < 0 then
        Value := 0
      else
        if LeftMargin + Value > PageWidth then
          Value := PageWidth - LeftMargin;
    FMargins[mtRight].Position := Value;
    DoMarginChanged;
    Invalidate;
  end;
end;

procedure TCustomRuler.SetRulerColor(const Value: TColor);
begin
  if Value <> FRulerColor then
  begin
    FRulerColor := Value;
    Invalidate;
  end;
end;

procedure TCustomRuler.SetRulerOptions(const Value: TRulerOptions);
begin
  if FRulerOptions <> Value then
  begin
    FRulerOptions := Value;
    FTimer.Enabled := roAutoUpdatePrinterWidth in FRulerOptions;
    Invalidate;
  end;
end;

procedure TCustomRuler.SetRulerTexts(const Value: TRulerTexts);
begin
  FRulerTexts := Value;
end;

procedure TCustomRuler.SetRulerType(const Value: TRulerType);
var
  DC: HDC;
begin
  if FRulerType <> Value then
  begin
    FRulerType := Value;

    DC := GetDC(0);
    try
      if FRulerType = rtHorizontal then
        ScreenRes := GetDeviceCaps(DC, LOGPIXELSX)  // Pixels per inch
      else
        ScreenRes := GetDeviceCaps(DC, LOGPIXELSY);
    finally
      ReleaseDC(0, DC);
    end;
    Invalidate;

    // Rotate ruler
    if not (csReading in ComponentState) then
      SetBounds(Left, Top, Height, Width);
  end;
end;

procedure TCustomRuler.SetScreenRes(const Value: Integer);
begin
  if FScreenRes <> Value then
  begin
    FScreenRes := Value;
    SetUnitsProgram(UnitsProgram);
    Invalidate;
  end;
end;

procedure TCustomRuler.SetTableEditor(const Value: TRulerTableEditor);
begin
  FTableEditor := Value;
end;

procedure TCustomRuler.SetTabs(const Value: TTabs);
begin
  FTabs := Value;
end;

procedure TCustomRuler.SetTabSettings(const Value: TTabSettings);
begin
  FTabSettings.Assign(Value);
end;

procedure TCustomRuler.SetTopMargin(Value: Extended);
begin
  if Value <> TopMargin then
  begin
    if not (csLoading in ComponentState) then
      if Value < 0 then
        Value := 0
      else
        if BottomMargin + Value > PageHeight then
          Value := PageHeight - BottomMargin;
    FMargins[mtTop].Position := Value;
    DoMarginChanged;
    Invalidate;
  end;
end;

procedure TCustomRuler.SetUnitsDisplay(const Value: TRulerUnits);
begin
  if FUnitsDisplay <> Value then
  begin
    FUnitsDisplay := Value;
    Invalidate;
  end;
end;

procedure TCustomRuler.SetUnitsProgram(const Value: TRulerUnits);
var
  M: Extended;
  I: Integer;
begin
  if Value <> FUnitsProgram then
  begin
    M := UnitsPerInch(Value) / UnitsPerInch(FUnitsProgram);
    FUnitsProgram := Value;

    PageHeight := M * PageHeight;
    PageWidth := M * PageWidth;
    if not (csLoading in ComponentState) then
    begin
      FDefaultTabWidth           := M * DefaultTabWidth;
      FIndents[itFirst].Position := M * FirstIndent;
      FIndents[itLeft].Position  := M * LeftIndent;
      FIndents[itRight].Position := M * RightIndent;
      FMargins[mtTop].Position   := M * TopMargin;
      FMargins[mtBottom].Position:= M * BottomMargin;
      FMargins[mtLeft].Position  := M * LeftMargin;
      FMargins[mtRight].Position := M * RightMargin;
      for I := 0 to FTabs.Count - 1 do
        FTabs[I].Position := M * FTabs[I].Position;

      with TableEditor do
      begin
        FBorderHSpacing := M * BorderHSpacing;
        FBorderWidth    := M * BorderWidth;
        FCellBorderWidth:= M * CellBorderWidth;
        FCellHSpacing   := M * CellHSpacing;
        FCellPadding    := M * CellPadding;
        FFirstIndent    := M * FirstIndent;
        FLeftIndent     := M * LeftIndent;
        FRightIndent    := M * RightIndent;
        FBorders[btLeft].Position  := M * FBorders[btLeft].Position;
        FBorders[btRight].Position := M * FBorders[btRight].Position;
        for I := 0 to Cells.Count - 1 do
          if Cells[I].CellWidth >= 0 then
            Cells[I].CellWidth := M * Cells[I].CellWidth;
      end;
      Invalidate;
    end;
  end;

  // FMultiPixels can be used to convert Units to pixels.
  // 1 / FMultiPixels can of course be used for the reversed effect.
  FMultiPixels := UnitsPerInch(ruPixels) / UnitsPerInch(FUnitsProgram);

  // FMultiPoints can be used to convert Units to Points
  // 1 / FMultiPoints can of course be used for the reversed effect.
  FMultiPoints := UnitsPerInch(ruPoints) / UnitsPerInch(FUnitsProgram);
end;

procedure TCustomRuler.SetZoom(const Value: TZoomRange);
begin
  if FZoom <> Value then
  begin
    FZoom := Value;
    Invalidate;
  end;
end;

procedure TCustomRuler.TimerProc(Sender: TObject);
begin
  UpdatePageDimensions;
end;

function TCustomRuler.UnitsPerInch(Units: TRulerUnits): Extended;
begin
  case Units of
    ruCentimeters: Result := cmPerInch;
    ruMillimeters: Result := mmPerInch;
    ruPicas:  Result := PicasPerInch;
    ruPixels: Result := ScreenRes;
    ruPoints: Result := PointsPerInch;
  else
    Result := 1;
  end;
end;

function TCustomRuler.UnitsToPixs(const Value: Extended): Extended;
begin
  Result := Value * MultiPixels;
end;

function TCustomRuler.UnitsToPoints(const Value: Extended): Extended;
begin
  Result := Value * MultiPoints;
end;

procedure TCustomRuler.UpdatePageDimensions;
var
  InchHeight: Extended;
  InchWidth: Extended;
  PixsInchX: Integer;
  PixsInchY: Integer;
  PhysHeight: Integer;
  PhysWidth: Integer;
  PrinterHandle: HDC;
begin
  PrinterHandle := 0;
  try
    if ((roUseDefaultPrinterWidth in FRulerOptions) or
        (roAutoUpdatePrinterWidth in FRulerOptions)) and
       (Printer.Printers.Count > 0) then
         PrinterHandle := Printer.Handle;
  except
    // Eat errors in case the default printer is not available.
  end;

  if PrinterHandle <> 0 then
  begin
    PixsInchX := GetDeviceCaps(PrinterHandle, LOGPIXELSX);
    PixsInchY := GetDeviceCaps(PrinterHandle, LOGPIXELSY);
    PhysWidth := GetDeviceCaps(PrinterHandle, PHYSICALWIDTH);
    InchWidth := PhysWidth / PixsInchX;
    PhysHeight := GetDeviceCaps(PrinterHandle, PHYSICALHEIGHT);
    InchHeight := PhysHeight / PixsInchY;
  end
  else
  begin
    InchWidth := 8.5;
    InchHeight := 11;
  end;

  if InchWidth <> FPrinterWidth then
  begin
    FPrinterWidth := InchWidth;
    PageWidth := InchWidth * UnitsPerInch(UnitsProgram);
  end;
  if InchHeight <> FPrinterHeight then
  begin
    FPrinterHeight := InchHeight;
    PageHeight := InchHeight * UnitsPerInch(UnitsProgram);
  end;
end;

function TCustomRuler.UseRTL: Boolean;
begin
  Result := False;
  case BiDiModeRuler of
    bmRightToLeft: Result := True;
    bmUseBiDiMode:
      {$IFDEF COMPILER4_UP}
        Result := UseRightToLeftReading;
      {$ELSE}
        Result := False;
      {$ENDIF}
  end;
end;

function TCustomRuler.ZoomAndRound(const Value: Extended): Integer;
begin
  Result := Round(Value * Zoom / 100);
end;

function TCustomRuler.ZPixsToUnits(const Value: Extended): Extended;
begin
  Result := PixsToUnits(Value) * 100 / Zoom;
end;

function TCustomRuler.ZUnitsToPixs(const Value: Extended): Extended;
begin
  Result := UnitsToPixs(Value) * Zoom / 100;
end;


{ TVRuler }

constructor TVRuler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := 600;
  Width  := 21;
  Font.Name := 'Arial';
  FIndentSettings.Options := [];
  FMarginSettings.DragCursor := crSizeNS;
  FMaxTabs := 0;
  FTabSettings.Options := [];
  FRulerType := rtVertical;
end;


{ TRulerCell }

constructor TRulerCell.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FDragBoundary := -1;
end;

function TRulerCell.GetDragLimit: Integer;
var
  I: Integer;
  X: Extended;
begin
  with TCustomRuler(TRulerCells(Collection).GetOwner).TableEditor do
  begin
    X := BorderHSpacing;
    if Index < Collection.Count then
    begin
      for I := 0 to Index do
        if Cells[I].DragBoundary >= 0 then
        begin
          X := X + CellBorderWidth;
          X := X + Cells[I].DragBoundary;
          X := X + CellBorderWidth;
          X := X + CellHSpacing;
        end;
      X := X - 0.5 * CellHSpacing;
    end;
    Result := FBorders[btLeft].Left + Trunc(Ruler.ZUnitsToPixs(X));
  end;
end;

function TRulerCell.GetPosition: Extended;
var
  I: Integer;
begin
  with TCustomRuler(TRulerCells(Collection).GetOwner).TableEditor do
  begin
    Result := BorderHSpacing;
    if Index < Collection.Count then
    begin
      for I := 0 to Index do
        if Cells[I].CellWidth >= 0 then
        begin
          Result := Result + CellBorderWidth;
          Result := Result + Cells[I].CellWidth;
          Result := Result + CellBorderWidth;
          Result := Result + CellHSpacing;
        end;
      Result := Result - 0.5 * CellHSpacing;
    end;
  end;
end;

procedure TRulerCell.SetCellWidth(const Value: Extended);
begin
  if FCellWidth <> Value then
  begin
    FCellWidth := Value;
    TCustomRuler(TRulerCells(Collection).GetOwner).Invalidate;
  end;
end;

procedure TRulerCell.SetDragBoundary(const Value: Extended);
begin
  FDragBoundary := Value;
end;

procedure TRulerCell.SetLeft(const Value: Integer);
begin
  FLeft := Value;
end;

procedure TRulerCell.SetPosition(const Value: Extended);
var
  I: Integer;
begin
  with TCustomRuler(TRulerCells(Collection).GetOwner).TableEditor do
  begin
    FDraggedDelta := Value - Position;
    Collection.BeginUpdate;
    try
      CellWidth := CellWidth + FDraggedDelta;

      I := GetNextValidCell(Index);
      if I <= Collection.Count - 1 then
        Cells[I].CellWidth := Cells[I].CellWidth - FDraggedDelta;

      FDraggedColumn := Index;
      for I := Index + 1 to Cells.Count - 1 do
        if Cells[I].CellWidth < 0 then
          Inc(FDraggedColumn)
        else
          Break;
    finally
      Collection.EndUpdate;
    end;
  end;
end;

{ TRulerCells }

function TRulerCells.Add: TRulerCell;
begin
  Result := TRulerCell(inherited Add);
end;

constructor TRulerCells.Create(AOwner: TCustomRuler);
begin
  inherited Create(TRulerCell);
  FRuler := AOwner;
end;

function TRulerCells.GetCell(Index: Integer): TRulerCell;
begin
  Result := TRulerCell(inherited Items[Index]);
end;

function TRulerCells.GetOwner: TPersistent;
begin
  Result := FRuler;
end;

procedure TRulerCells.SetCell(Index: Integer; const Value: TRulerCell);
begin
  inherited SetItem(Index, Value);
  FRuler.Invalidate;
end;

procedure TRulerCells.Update(Item: TCollectionItem);
begin
  inherited;
  with FRuler do
  begin
    if csLoading in ComponentState then
      Exit;
    DoTableColumnChanged;
    Invalidate;
  end;
end;

{ TRulerListEditor }

constructor TRulerListEditor.Create(AOwner: TCustomRuler);
begin
  FOwner := AOwner;
  FOptions := DefaultListOptions;
end;

function TRulerListEditor.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TRulerListEditor.Invalidate;
begin
  if Active then
    Ruler.Invalidate;
end;

procedure TRulerListEditor.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    Ruler.Invalidate;
  end;
end;

procedure TRulerListEditor.SetLevelGraphic(const Value: TLevelGraphic);
begin
  if FLevelGraphic <> Value then
  begin
    FLevelGraphic := Value;
    Invalidate;
  end;
end;

procedure TRulerListEditor.SetOptions(const Value: TListEditorOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    Invalidate;
  end;
end;

{ TRulerTableEditor }

function TRulerTableEditor.CalculateCurrentIndentPosition(
  const IT: TIndentType): Integer;
var
  R: TRect;
begin
  R := GetCellRect(CellIndex);
  if IT = itRight then
    Result := R.Right
  else
    Result := R.Left;

  case IT of
    itFirst:
      Result := Result + Round(Ruler.ZUnitsToPixs(FirstIndent));
    itLeft,
    itBoth:
      Result := Result + Round(Ruler.ZUnitsToPixs(FirstIndent + LeftIndent));
    itRight:
      Result := Result - Round(Ruler.ZUnitsToPixs(RightIndent));
  end;
end;

constructor TRulerTableEditor.Create(AOwner: TCustomRuler);
begin
  FOwner := AOwner;
  FDragCursor := crHSplit;
  FBackGroundColor := clBtnFace;
  FForeGroundColor := clBtnShadow;
  FOptions := DefaultTableOptions;
  FRulerCells := TRulerCells.Create(FOwner);
  FTablePosition := tpFromMargin;
  FVisible := tevOnlyWhenActive;
end;

destructor TRulerTableEditor.Destroy;
begin
  FRulerCells.Free;
  inherited;
end;

function TRulerTableEditor.GetBorderRect(const BorderType: TBorderType): TRect;
begin
  with FBorders[BorderType] do
    Result := Rect(Left, 0, Left + 9, Ruler.Height - 6);
end;

function TRulerTableEditor.GetCellRect(const Index: Integer): TRect;
var
  I: Integer;
begin
  if (Index < 0) or (Index >= Cells.Count) then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;

  Result.Top := 0;
  Result.Bottom := Ruler.Height - 6;
  with Ruler do
  begin
    if Index = LastValidCellIndex then
      Result.Right := FBorders[btRight].Left + BorderOffset - GetTotalCellSpacing(True, False)
    else
      Result.Right := Cells[Index].Left + ColumnOffset - GetTotalCellSpacing(False, False);

    if Index = 0 then
      Result.Left := FBorders[btLeft].Left + BorderOffset + GetTotalCellSpacing(True, True)
    else
    begin
      I := GetPrevValidCell(Index);
      Result.Left := Cells[I].Left + ColumnOffset + GetTotalCellSpacing(False, True);
    end;
  end;
end;

function TRulerTableEditor.GetColumnIndexAt(const X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if not Active then
    Exit;

  if (Y > 0) and (Y < Ruler.Height - 7) then
    for I := 0 to Cells.Count - 1 do
      with Cells[I] do
        if (CellWidth >= 0) and (Left > 0) and (X >= Left) and (X < Left + 9) then
          Result := I;
end;

function TRulerTableEditor.GetNextValidCell(const Index: Integer): Integer;
begin
  Result := Index + 1;
  while (Result < Cells.Count - 1) and (Cells[Result].CellWidth < 0) do
    Inc(Result);
end;

function TRulerTableEditor.GetOffset: Integer;
begin
  Result := 0;
  case TablePosition of
    tpAbsolute: Result := Ruler.Inset;
    tpFromFirstIndent: Result := FRulerIndents[itFirst].Left + IndentOffset;
    tpFromLeftIndent: Result := FRulerIndents[itLeft].Left + IndentOffset;
    tpFromMargin: Result := Ruler.FMargins[mtLeft].Grip;
  end;
end;

function TRulerTableEditor.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TRulerTableEditor.GetPrevValidCell(const Index: Integer): Integer;
begin
  Result := Index - 1;
  while (Result > 0) and (Cells[Result].CellWidth < 0) do
    Dec(Result);
end;

function TRulerTableEditor.GetTableLeft: Extended;
begin
  Result := FBorders[btLeft].Position;
end;

function TRulerTableEditor.GetTableWidth: Extended;
begin
  Result := FBorders[btRight].Position - FBorders[btLeft].Position;
end;

function TRulerTableEditor.GetTotalCellSpacing(const FromBorder,
  FromLeft: Boolean): Integer;
begin
  if FromBorder then
    Result := Round(Ruler.ZUnitsToPixs(BorderHSpacing))
  else
    if FromLeft then
      Result := Trunc(Ruler.ZUnitsToPixs(0.5 * CellHSpacing) + 0.5)
    else
      Result := Trunc(Ruler.ZUnitsToPixs(0.5 * CellHSpacing) - 0.5);
  Result := Result + Round(Ruler.ZUnitsToPixs(CellBorderWidth + CellPadding));
end;

procedure TRulerTableEditor.Invalidate;
begin
  if Active then
    Ruler.Invalidate;
end;

function TRulerTableEditor.KeepColumnsSeparated(const Index,
  Left: Integer): Integer;
var
  DI: Integer;
  I: Integer;
  Spacing: Integer;
  BLSpacing: Integer;
  BRSpacing: Integer;
  CLSpacing: Integer;
  CRSpacing: Integer;

  function KeepFromLeftBorder(X: Integer): Integer;
  begin
    Result := IMax(X, FBorders[btLeft].Left + BLSpacing + Spacing + CRSpacing);
  end;

  function KeepFromRightBorder(X: Integer): Integer;
  begin
    Result := IMin(X, FBorders[btRight].Left - CLSpacing - Spacing - BRSpacing);
  end;

  function KeepFromPrevCell(Index, X: Integer): Integer;
  begin
    I := GetPrevValidCell(Index);
    if I < 0 then
      Result := KeepFromLeftBorder(X)
    else
      Result := IMax(X, Cells[I].Left + CLSpacing + Spacing + CRSpacing);
  end;

  function KeepFromNextCell(Index, X: Integer): Integer;
  begin
    I := GetNextValidCell(Index);
    if I = LastValidCellIndex then
      Result := KeepFromRightBorder(X)
    else
      Result := IMin(X, Cells[I].Left - CLSpacing - Spacing - CRSpacing);
  end;

  function KeepFromPrevDragLimit(Index, X: Integer): Integer;
  begin
    I := Index - 1;
    if I < 0 then
      Result := KeepFromLeftBorder(X)
    else
      Result := IMax(X, Cells[I].DragLimit + CLSpacing + Spacing + CRSpacing);
  end;

  function KeepFromNextDragLimit(Index, X: Integer): Integer;
  begin
    I := Index + 1;
    if I >= Cells.Count - 1 then
      Result := KeepFromRightBorder(X)
    else
      Result := IMin(X, Cells[I].DragLimit - CLSpacing - Spacing - CRSpacing);
  end;

begin
  Spacing := Ruler.ItemSeparation;
  BLSpacing := GetTotalCellSpacing(True, True);   // Border Left spacing
  BRSpacing := GetTotalCellSpacing(True, False);  // Border Right spacing
  CLSpacing := GetTotalCellSpacing(False, True);  // Cell Left spacing
  CRSpacing := GetTotalCellSpacing(False, False); // Cell Right spacing

  Result := Left;
  if UseDragBoundaries then
  begin
    DI := Index;
    for I := Index + 1 to Cells.Count - 1 do
      if Cells[I].CellWidth < 0 then
        Inc(DI)
      else
        Break;

    if Index = -1 then                              // Left border being dragged
      Result := KeepFromNextDragLimit(DI, Result)
    else
      if Index = (Cells.Count - 1) then             // Right border being dragged
        Result := KeepFromPrevDragLimit(DI, Result)
      else                                          // Columns being dragged
      begin
        Result := KeepFromPrevDragLimit(DI, Result);
        Result := KeepFromNextDragLimit(DI, Result);
      end;
  end
  else
  begin
    if Index = -1 then                              // Left border being dragged
      Result := KeepFromNextCell(Index, Result)
    else
      if Index = (Cells.Count - 1) then             // Right border being dragged
        Result := KeepFromPrevCell(LastValidCellIndex, Result)
      else                                          // Columns being dragged
      begin
        Result := KeepFromPrevCell(Index, Result);
        Result := KeepFromNextCell(Index, Result);
      end;
  end;

  // For the current editing cell the Indent information is available.
  // Use it to keep the Indents separated when the column is being dragged.
  with Ruler do
    if GetNextValidCell(Index) = CellIndex then
    begin
      if CellIndex = 0 then
        I := BorderOffset + BLSpacing
      else
        I := ColumnOffset + CLSpacing;
      if not UseRTL then
      begin
        I := I + Round(ZUnitsToPixs(FirstIndent));
        if FIndents[itLeft].Left > FIndents[itFirst].Left then
          I := I + FIndents[itLeft].Left - FIndents[itFirst].Left;
        Result := IMin(Result, FIndents[itRight].Left + IndentOffset - I - Spacing);
      end
      else
      begin
        I := I + Round(ZUnitsToPixs(RightIndent));
        Result := IMin(Result, FIndents[itFirst].Left + IndentOffset - I - Spacing);
        Result := IMin(Result, FIndents[itLeft].Left + IndentOffset - I - Spacing);
      end;
    end
    else if GetPrevValidCell(Index + 1) = CellIndex then
    begin
      if CellIndex = LastValidCellIndex then
        I := -BorderOffset + BRSpacing
      else
        I := -ColumnOffset + CRSpacing;
      if not UseRTL then
      begin
        I := I + Round(ZUnitsToPixs(RightIndent));
        Result := IMax(Result, FIndents[itFirst].Left + IndentOffset + I + Spacing);
        Result := IMax(Result, FIndents[itLeft].Left + IndentOffset + I + Spacing);
      end
      else
      begin
        I := I + Round(ZUnitsToPixs(FirstIndent));
        if FIndents[itLeft].Left < FIndents[itFirst].Left then
          I := I - FIndents[itLeft].Left + FIndents[itFirst].Left;
        Result := IMax(Result, FIndents[itRight].Left + IndentOffset + I + Spacing);
      end;
    end;
end;

function TRulerTableEditor.KeepWithinCurrentCell(const IT: TIndentType;
  const X: Integer): Integer;
var
  R: TRect;
begin
  R := GetCellRect(CellIndex);
  if (IT = itRight) xor Ruler.UseRTL then
    Result := IMin(X, R.Right - IndentOffset)
  else
    Result := IMax(X, R.Left - IndentOffset);
end;

function TRulerTableEditor.LastValidCellIndex: Integer;
begin
  Result := Cells.Count - 1;
  while (Result > 0) and (Cells[Result].CellWidth < 0) do
    Dec(Result);
end;

function TRulerTableEditor.RTLAdjust(X, Offset: Integer): Integer;
var
  R: TRect;
begin
  R := GetCellRect(CellIndex);
  if Ruler.UseRTL then
    Result := R.Left + R.Right - (X + Offset)
  else
    Result := X - Offset;
end;

procedure TRulerTableEditor.SetActive(const Value: Boolean);
begin
  if Value and (Visible = tevNever) then Exit;

  if FActive <> Value then
  begin
    if not (csDesigning in Ruler.ComponentState) then
    begin
      if Value then
        FRulerIndents := Ruler.FIndents  // Save Ruler Indents
      else
        Ruler.FIndents := FRulerIndents; // Restore Ruler Indents
    end;
    FActive := Value;
    Ruler.Invalidate;
  end;
end;

procedure TRulerTableEditor.SetBackGroundColor(const Value: TColor);
begin
  if FBackGroundColor <> Value then
  begin
    FBackGroundColor := Value;
    Invalidate;
  end;
end;

procedure TRulerTableEditor.SetBorderHSpacing(const Value: Extended);
begin
  if FBorderHSpacing <> Value then
  begin
    FBorderHSpacing := Value;
    Invalidate;
  end;
end;

procedure TRulerTableEditor.SetBorderWidth(const Value: Extended);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    Invalidate;
  end;
end;

procedure TRulerTableEditor.SetCellBorderWidth(const Value: Extended);
begin
  if FCellBorderWidth <> Value then
  begin
    FCellBorderWidth := Value;
    Invalidate;
  end;
end;

procedure TRulerTableEditor.SetCellHSpacing(const Value: Extended);
begin
  if FCellHSpacing <> Value then
  begin
    FCellHSpacing := Value;
    Invalidate;
  end;
end;

procedure TRulerTableEditor.SetCellIndex(const Value: Integer);
begin
  if FCellIndex <> Value then
  begin
    FCellIndex := Value;
    Invalidate;
  end;
end;

procedure TRulerTableEditor.SetCellPading(const Value: Extended);
begin
  if FCellPadding <> Value then
  begin
    FCellPadding := Value;
    Invalidate;
  end;
end;

procedure TRulerTableEditor.SetDragCursor(const Value: TCursor);
begin
  if FDragCursor <> Value then
  begin
    FDragCursor := Value;
    Invalidate;
  end;
end;

procedure TRulerTableEditor.SetFirstIndent(const Value: Extended);
begin
  Ruler.FirstIndent := Value;
  if FFirstIndent <> Value then
  begin
    FFirstIndent := Value;
    Invalidate;
  end;
end;

procedure TRulerTableEditor.SetForeGroundColor(const Value: TColor);
begin
  if FForeGroundColor <> Value then
  begin
    FForeGroundColor := Value;
    Invalidate;
  end;
end;

procedure TRulerTableEditor.SetLeftIndent(const Value: Extended);
begin
  Ruler.LeftIndent := Value;
  if FLeftIndent <> Value then
  begin
    FLeftIndent := Value;
    Invalidate;
  end;
end;

procedure TRulerTableEditor.SetOptions(const Value: TTableEditorOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    Invalidate;
  end;
end;

procedure TRulerTableEditor.SetRightIndent(const Value: Extended);
begin
  Ruler.RightIndent := Value;
  if FRightIndent <> Value then
  begin
    FRightIndent := Value;
    Invalidate;
  end;
end;

procedure TRulerTableEditor.SetRulerCells(const Value: TRulerCells);
begin
  FRulerCells.Assign(Value);
end;

procedure TRulerTableEditor.SetTableLeft(const Value: Extended);
begin
  if FBorders[btLeft].Position <> Value then
  begin
    FBorders[btLeft].Position := Value;
    FBorders[btRight].Position := Value + TableWidth;
    Invalidate;
  end;
end;

procedure TRulerTableEditor.SetTablePosition(const Value: TTablePosition);
begin
  if FTablePosition <> Value then
  begin
    FTablePosition := Value;
    Invalidate;
  end;
end;

procedure TRulerTableEditor.SetTableWidth(const Value: Extended);
var
  NewRight: Extended;
begin
  NewRight := TableLeft + Value;
  if FBorders[btRight].Position <> NewRight then
  begin
    FBorders[btRight].Position := NewRight;
    Invalidate;
  end;
end;

procedure TRulerTableEditor.SetVisible(const Value: TTableEditorVisible);
begin
  if FVisible <> Value then
  begin
    if Value = tevNever then
      Active := False;
    FVisible := Value;
    Invalidate;
  end;
end;

procedure TRulerTableEditor.UpdateIndentPosition(const IT: TIndentType;
  const XPos: Integer);
var
  R: TRect;
  Start: Integer;
begin
  R := GetCellRect(CellIndex);
  if IT = itRight then
    Start := R.Right
  else
    Start := R.Left;

  with Ruler do
  begin
    if IT = itFirst then
    begin
      FIndents[itLeft].Position :=
        FirstIndent + LeftIndent - ZPixsToUnits(XPos - Start);
      FirstIndent := ZPixsToUnits(XPos - Start);
    end
    else if IT = itLeft then
      LeftIndent := ZPixsToUnits(XPos - Start) - FirstIndent
    else if IT = itBoth then
      FirstIndent := ZPixsToUnits(XPos - Start) - LeftIndent
    else
      RightIndent := ZPixsToUnits(Start - XPos);
  end;
end;

end.

