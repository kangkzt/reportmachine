{**************************************************}
{                                                  }
{                   llPDFLib                       }
{            Version  3.6,  09.01.2007             }
{      Copyright (c) 2002-2007  llionsoft          }
{             All rights reserved                  }
{            mailto:einfo@llion.net                }
{                                                  }
{**************************************************}

{$I pdf.inc}


unit pdfttfcodes;
interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  PDFMisc,
  Math;

type

  FIXED = packed record
    Hi, Lo: Word;
  end;

  PTTFTreeNode = ^TTTFTreeNode;

  LongByteRec = record
    b1,
      b2,
      b3,
      b4: byte;
  end;

  TTTFTreeNode = record
    Code: Word;
    ID: Word;
    Width: Word;
    Used: Boolean;
  end;

  Hhea_type = packed record
    TablVer: LongInt;
    Ascender: Word;
    Descender: Word;
    LineGap: Word;
    advanceWidthMax: Word;
    minLeftSideBearing: Word;
    minRightSideBearing: Word;
    xMaxExtent: Word;
    caretSlopeRise: SmallInt;
    caretSlopeRun: SmallInt;
    caretOffset: SmallInt;
    res1: SmallInt;
    res2: SmallInt;
    res3: SmallInt;
    res4: SmallInt;
    metricDataFormat: SmallInt;
    numberOfHMetrics: Word;
  end;

  PHHEa = ^Hhea_type;

  FileHeaderRec = packed record
    Version: FIXED;
    NumTables: Word;
    SearchRange: Word;
    EntrySelector: Word;
    RangeShift: Word;
  end;

  TableInfoRec = packed record
    Tag: LongInt;
    Checksum: LongInt;
    Offset: LongInt;
    Length: LongInt;
  end;

  THead_type = packed record
    version_number: FIXED;
    fontRevision: FIXED;
    checkSum: LongInt;
    magicNumber: LongInt;
    flags: Word;
    unitsPerEm: Word;
    two_date: array [ 1..16 ] of byte;
    xMin: SmallInt;
    yMin: SmallInt;
    xMax: SmallInt;
    yMax: SmallInt;
    macStyle: Word;
    lowestRec: Word;
    fontDirection: SmallInt;
    indexToLocFormat: SmallInt;
    glyphDataFormat: SmallInt
  end;

  CmapHeaderRec = packed record
    Version: Word;
    NumTables: Word;
  end;

  CmapTableInfoRec = packed record
    Platform_ID: Word;
    Encoding_ID: Word;
    Offset: LongInt;
  end;

  TCMap4HEAD = record
    Format: Word;
    length: Word;
    version: Word;
  end;

  TCMap4Segment = packed record
    endCount: array of Word;
    zero_byte: Word;
    startCount: array of Word;
    idDelta: array of SmallInt;
    idRangeOffset: array of Word;
  end;

  Maxp_type = packed record
    version: fixed;
    numGlyphs: Word;
    maxPoints: Word;
    maxContours: Word;
    maxCompositePoints: Word;
    maxCompositeContours: Word;
    maxZones: Word;
    maxTwilightPoints: Word;
    maxStorage: Word;
    maxFunctionDefs: Word;
    maxInstructionDefs: Word;
    maxStackElements: Word;
    maxSizeOfInstructions: Word;
    maxComponentElements: Word;
    maxComponentDepth: Word;
  end;

  PGlyf_type = ^TGlyf_type;

  TGlyf_type = packed record
    num: integer;
    size: LongInt;
    Glyf_: PAnsiChar;
    next: PGlyf_type;
    tag: boolean;
  end;

  TCMap4 = record
    segCountX2: Word;
    searchRange: Word;
    entrySelector: Word;
    rangeShift: Word;
  end;

  PLongWord = ^Longint;
  PLongint = ^Longint;
  TDirectory = packed array [ 0..2047 ] of TableInfoRec;
  PDirectory = ^TDirectory;

  TTTFData = class
  private
    CMap4: TCMap4;
    FMono: boolean;
    Hhea: Hhea_type;
    FUnitsPerEm: Word;
    FHead: THead_type;
    FEmbedded: Boolean;
    CodeTable: array of TTTFTreeNode;
    FLocaSize: LongWord;
    FLoca_big: PByteArray;
    FLoca_small: PByteArray;
    cmap: array of byte;
    Prep: array of byte;
    fpgm: array of byte;
    FmaxpSize: LongWord;
    FheadSize: LongWord;
    FhheaSize: LongWord;
    cvt: array of byte;
    Hmtx: array of Word;
    FHmtxIsOK: Boolean;
    FGlyf: PGlyf_type;
    FGlyfCount: Integer;
    FMaxp_type: Maxp_type;
    NumOfHMetrics: Word;
    Cmap4_HEAD: TCMap4HEAD;
    segments: TCMap4Segment;
    CmapHeader: CmapHeaderRec;
    g_array_Id: array of SmallInt;
    CmapTableInfo: CmapTableInfoRec;
    TTFFontName: TFontname;
    TTFFontStyle: TFontStyles;
    GlyfIndex: Word;
    procedure BuildTheTable;
    procedure TTFException ( TypeOfException: byte );
    function DSwap ( L: Longint ): Longint;
    function GetSymbWidth ( ID: Word ): LongWord;
    function GetSymbID ( Unicode: Word; i: integer ): Word;
    function GetCode ( Unicode: Word; BeginOfTable: PTTFTreeNode; TableLength: Word ): PTTFTreeNode;
    function CalculateChecksum ( Buffer: Pointer; Size: LongInt ): LongInt;
    procedure LoadTables ( FontName: TFontname; FontStyle: TFontStyles; IsEmbedded: Boolean );
    procedure GetGlyf ( Unicode: Word );
    procedure GetGlyfs;
    procedure SaveFont ( Stream: TStream );
    procedure CutGlyfs;
    function SizeOfGlyf: LongInt;
    procedure Composit_glyf ( Glyf: PGlyf_type );
  public
    constructor Create ( FontName: TFontname; FontStyle: TFontStyles; IsEmbedded: Boolean );
    destructor Destroy; override;
    procedure GetFontCodes ( Characters: PWord; Len: Integer; Buff: Pword );
    procedure GetFontWidths ( Characters: PWord; Len: Integer; Buff: Pword );
    procedure GetFontSubset ( OutStream: TStream );
    function GetNodeByID(ID: Word): PTTFTreeNode;
    function GetWideString: string;
    procedure GetToUnicodeStream ( Alias: string; Stream: TStream );
    function GetCharWidth ( Character: Word ): Integer;
    function GetCharID ( Character: Word ): Integer;
    procedure SetCharIDUsed ( var ID: Word );
    property GlyfCount: Integer read FGlyfCount write FGlyfCount default 0;
    property Embedded: Boolean read FEmbedded write FEmbedded default false;
    property UnitsPerEm: Word read FUnitsPerEm write FUnitsPerEm;
  end;

implementation

const
  UnknownSymbol: Word = 32;

  InTables: array [ 1..5 ] of DWORD = ( $636D6170, $686D7478, $6D617870, $68656164, $68686561 );
  AddTables: array [ 1..6 ] of DWORD = ( $63767420, $6670676D, $70726570, $6C6F6361, $676C7966, $636D6170 );
  Empty: array [ 1..2 ] of byte = ( 0, 0 );
  OutHeader: array [ 1..12 ] of byte = ( 0, 1, 0, 0, 0, $A, 0, $80, 0, 3, 0, $10 );
  PostTable: array [ 1..52 ] of byte = ( 00, 02, 00, 00, 00, 00, 00, 00, $FF, $27, 00, $96, 00, 00, 00, 00,
    00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    00, 04, 00, 00, 00, 00, 01, 02, 00, $44, 09, $61, $66, $69, $69, $31, $30, $30, $36, $35 );
  ColNames = 10;

procedure TTTFData.GetGlyfs;
var
  f, i, j: integer;
  size: longint;
  Glyf_next, Glyf_tmp: PGlyf_type;
  x: LongInt;
  x1: Word;
  DC: HDC;
  FZ: Cardinal;
  LF: TLogFont;
  Obj: THandle;
  TablBegin: PByte;
  FontStream: TMemoryStream;

begin
  new ( FGlyf );
  FGlyf^.next := nil;
  FGlyf^.Glyf_ := nil;
  TTFFontName := TTFFontName;
  TTFFontStyle := TTFFontStyle;
  GlyfIndex := 4;
  DC := CreateCompatibleDC ( 0 );
  try
    FillChar ( LF, SizeOf ( LF ), 0 );
    with LF do
    begin
      lfHeight := -10;
      lfWidth := 0;
      lfEscapement := 0;
      lfOrientation := 0;
      if fsBold in TTFFontStyle then
        lfWeight := FW_BOLD
      else
        lfWeight := FW_NORMAL;
      lfItalic := Byte ( fsItalic in TTFFontStyle );
      lfUnderline := Byte ( fsUnderline in TTFFontStyle );
      lfStrikeOut := Byte ( fsStrikeOut in TTFFontStyle );
      lfCharSet := DEFAULT_CHARSET;
      StrPCopy ( lfFaceName, TTFFontName );
      lfQuality := DEFAULT_QUALITY;
      lfOutPrecision := OUT_DEFAULT_PRECIS;
      lfClipPrecision := CLIP_DEFAULT_PRECIS;
      lfPitchAndFamily := DEFAULT_PITCH;
    end;
    obj := CreateFontIndirect ( LF );
    try
      SelectObject ( DC, Obj );
      FontStream := TMemoryStream.Create;
      try
        for f := 1 to 5 do
        begin
          FontStream.Position := 0;
          FZ := GetFontData ( DC, DSwap ( AddTables [ f ] ), 0, nil, 0 );
          if FZ = GDI_ERROR then
            TTFException ( 1 );
          FontStream.SetSize ( FZ );
          if GetFontData ( DC, DSwap ( AddTables [ f ] ), 0, FontStream.Memory, FZ ) = GDI_ERROR then
            TTFException ( 1 );
          FontStream.Position := 0;

          case f of
            1:
              begin //cvt
                SetLength ( cvt, FZ );
                TablBegin := @cvt [ 0 ];
                FontStream.Read ( TablBegin^, FZ );
              end;
            2:
              begin //fpgm
                SetLength ( Fpgm, FZ );
                TablBegin := @Fpgm [ 0 ];
                FontStream.Read ( TablBegin^, FZ );
              end;
            3:
              begin //prep
                SetLength ( Prep, FZ );
                TablBegin := @Prep [ 0 ];
                FontStream.Read ( TablBegin^, FZ );
              end;

            4:
              begin
                if swap ( FHead.indexToLocFormat ) = 1 then
                begin
                  getmem ( FLoca_big, FZ );
                  FontStream.Read ( FLoca_big^, FZ );
                end
                else
                begin
                  getmem ( FLoca_small, FZ );
                  FontStream.Read ( FLoca_small^, FZ );
                end;
                FLocaSize := FZ;
              end;
            5:
              begin
                FontStream.Position := 0;
                Glyf_next := FGlyf;
                j := swap ( FMaxp_type.numGlyphs );
                for i := 1 to j do
                begin
                  if swap ( FHead.indexToLocFormat ) = 1 then
                  begin
                    x := byte ( FLoca_big [ i * 4 ] ) shl 24 + byte ( FLoca_big [ i * 4 + 1 ] ) shl 16 +
                      byte ( FLoca_big [ i * 4 + 2 ] ) shl 8 + byte ( FLoca_big [ i * 4 + 3 ] );
                    size := x - byte ( FLoca_big [ ( i - 1 ) * 4 ] ) shl 24 - byte ( FLoca_big [ ( i - 1 ) * 4 + 1 ] ) shl 16 -
                    byte ( FLoca_big [ ( i - 1 ) * 4 + 2 ] ) shl 8 - byte ( FLoca_big [ ( i - 1 ) * 4 + 3 ] );
                  end
                  else
                  begin
                    size := 2 * ( byte ( FLoca_small [ i * 2 ] ) shl 8 + byte ( FLoca_small [ i * 2 + 1 ] ) -
                      byte ( FLoca_small [ ( i - 1 ) * 2 ] ) shl 8 - byte ( FLoca_small [ ( i - 1 ) * 2 + 1 ] ) );
                  end;
                  if size <> 0 then
                    getmem ( Glyf_next.Glyf_, size )
                  else
                    Glyf_next.Glyf_ := nil;
                  Glyf_next.num := i;
                  Glyf_next.size := size;
                  Glyf_next.tag := false;
                  if swap ( FHead.indexToLocFormat ) = 1 then
                  begin
                    x := byte ( FLoca_big [ ( i - 1 ) * 4 ] ) shl 24 + byte ( FLoca_big [ ( i - 1 ) * 4 + 1 ] ) shl 16 +
                    byte ( FLoca_big [ ( i - 1 ) * 4 + 2 ] ) shl 8 + byte ( FLoca_big [ ( i - 1 ) * 4 + 3 ] );
                    FontStream.Seek ( x, soFromBeginning )
                  end
                  else
                  begin
                    x1 := byte ( FLoca_small [ ( i - 1 ) * 2 ] ) shl 8 + byte ( FLoca_small [ ( i - 1 ) * 2 + 1 ] );
                    FontStream.Seek ( 2 * x1, soFromBeginning );
                  end;
                  FontStream.Read ( Glyf_next.Glyf_^, size );
                  if i <> j then
                  begin
                    new ( glyf_tmp );
                    glyf_tmp.next := nil;
                    glyf_tmp.Glyf_ := nil;
                    Glyf_next.next := glyf_tmp;
                    Glyf_next := glyf_tmp;
                  end
                  else
                    Glyf_next := nil;
                end;
              end;
          end;
        end;
      finally
        FontStream.SetSize ( 0 );
        FontStream.free;
      end;
    finally
      DeleteObject ( obj );
    end;
  finally
    DeleteDC ( DC );
  end;
end;

procedure TTTFData.LoadTables ( FontName: TFontname; FontStyle: TFontStyles; IsEmbedded: Boolean );
var
  DC: HDC;
  K: Integer;
  FZ: Cardinal;
  LF: TLogFont;
  Obj: THandle;
  M: TTextMetric;
  TablBegin: PByte;
  f, i, j: Integer;
  Short_read: SmallInt;
  UShort_read: Word;
  FontStream: TMemoryStream;
  off: longint;
begin
  TTFFontName := Fontname;
  TTFFontStyle := FontStyle;
  GlyfIndex := 4;
  DC := CreateCompatibleDC ( 0 );
  try
    FillChar ( LF, SizeOf ( LF ), 0 );
    with LF do
    begin
      lfHeight := -10;
      lfWidth := 0;
      lfEscapement := 0;
      lfOrientation := 0;
      if fsBold in FontStyle then
        lfWeight := FW_BOLD
      else
        lfWeight := FW_NORMAL;
      lfItalic := Byte ( fsItalic in FontStyle );
      lfUnderline := Byte ( fsUnderline in FontStyle );
      lfStrikeOut := Byte ( fsStrikeOut in FontStyle );
      lfCharSet := DEFAULT_CHARSET;
      StrPCopy ( lfFaceName, FontName );
      lfQuality := DEFAULT_QUALITY;
      lfOutPrecision := OUT_DEFAULT_PRECIS;
      lfClipPrecision := CLIP_DEFAULT_PRECIS;
      lfPitchAndFamily := DEFAULT_PITCH;
    end;
    obj := CreateFontIndirect ( LF );
    try
      SelectObject ( DC, Obj );
      GetTextMetrics ( DC, M );
      FMono := ( M.tmPitchAndFamily and TMPF_FIXED_PITCH ) = 0;
      FontStream := TMemoryStream.Create;
      try
        for i := 1 to Length ( InTables ) do
        begin
          FontStream.Position := 0;
          FZ := GetFontData ( DC, DSwap ( InTables [ i ] ), 0, nil, 0 );
          if FZ = GDI_ERROR then
            TTFException ( 1 );
          FontStream.SetSize ( FZ );
          if GetFontData ( DC, DSwap ( InTables [ i ] ), 0, FontStream.Memory, FZ ) = GDI_ERROR then
            TTFException ( 1 );
          FontStream.Position := 0;
          case i of
            1:
              begin
                SetLength ( cmap, FZ );
                TablBegin := @cmap [ 0 ];
                FontStream.Read ( TablBegin^, FZ );
                FontStream.Position := 0;
                FontStream.Read ( CmapHeader, sizeof ( CmapHeader ) );
                j := Swap ( CmapHeader.NumTables );
                off := 0;
                for f := 1 to j do
                begin
                  FontStream.Read ( CmapTableInfo, sizeof ( CmapTableInfo ) );
                  if ( swap ( CmapTableInfo.Platform_ID ) = 3 ) and ( swap ( CmapTableInfo.Encoding_ID ) = 1 ) then
                  begin
                    off := Dswap ( CmapTableInfo.Offset );
                    break;
                  end;
                  if ( swap ( CmapTableInfo.Platform_ID ) = 3 ) and ( swap ( CmapTableInfo.Encoding_ID ) = 0 ) then
                  begin
                    off := Dswap ( CmapTableInfo.Offset );
                  end;
                end;
                if FontStream.Seek ( off, soFromBeginning ) <> off then
                  TTFException ( 0 );
                FontStream.Read ( Cmap4_HEAD, sizeof ( Cmap4_HEAD ) );
                FontStream.Read ( CMap4.segCountX2, 2 );
                FontStream.Read ( CMap4.searchRange, 2 );
                FontStream.Read ( CMap4.entrySelector, 2 );
                FontStream.Read ( CMap4.rangeShift, 2 );
                K := Trunc ( swap ( CMap4.segCountX2 ) / 2 );
                SetLength ( segments.endCount, K );
                for f := 0 to K - 1 do
                begin
                  FontStream.Read ( UShort_read, 2 );
                  j := swap ( UShort_read );
                  segments.endCount [ f ] := j;
                end;
                FontStream.Read ( UShort_read, 2 );
                segments.zero_byte := swap ( UShort_read );
                SetLength ( segments.startCount, K );
                for f := 0 to K - 1 do
                begin
                  FontStream.Read ( UShort_read, 2 );
                  segments.startCount [ f ] := swap ( UShort_read );
                end;
                SetLength ( segments.idDelta, K );
                for f := 0 to K - 1 do
                begin
                  FontStream.read ( Short_read, 2 );
                  segments.idDelta [ f ] := swap ( Short_read );
                end;
                SetLength ( segments.idRangeOffset, K );
                for f := 0 to K - 1 do
                begin
                  FontStream.Read ( UShort_read, 2 );
                  segments.idRangeOffset [ f ] := swap ( UShort_read );
                end;
                j := trunc ( swap ( Cmap4_HEAD.length ) shr 1 ) - 8 - 4 * K;
                SetLength ( g_array_Id, j + 1 );
                for f := 0 to j do
                begin
                  FontStream.Read ( UShort_read, 2 );
                  g_array_Id [ f ] := swap ( UShort_read );
                end;
              end;
            2:
              begin //hmtx
                SetLength ( Hmtx, FZ );
                TablBegin := @Hmtx [ 0 ];
                FontStream.Read ( TablBegin^, FZ );
              end;
            3:
              begin //maxp
                FontStream.Read ( FMaxp_type, sizeof ( Maxp_type ) );
                FGlyfCount := swap ( FMaxp_type.numGlyphs );
                FmaxpSize := FZ;
              end;
            4:
              begin //head
                FontStream.Read ( FHead, sizeof ( THead_type ) );
                FUnitsPerEm := swap ( FHead.unitsPerEm );
                FheadSize := FZ;
              end;
            5:
              begin //hhea
                FontStream.Read ( Hhea, sizeof ( hhea ) );
                NumOfHMetrics := swap ( Hhea.numberOfHMetrics );
                FhheaSize := FZ;
              end;
          end;
        end;
      finally
        FontStream.SetSize ( 0 );
        FontStream.free;
      end;
    finally
      DeleteObject ( obj );
    end;
    BuildTheTable;
  finally
    DeleteDC ( DC );
  end;
end;

constructor TTTFData.Create ( FontName: TFontname; FontStyle: TFontStyles; IsEmbedded: Boolean );
begin
  TTFFontName := FontName;
  TTFFontStyle := FontStyle;
  FEmbedded := IsEmbedded;
  LoadTables ( FontName, FontStyle, IsEmbedded );
  BuildTheTable;
end;

function TTTFData.CalculateChecksum ( Buffer: Pointer; Size: LongInt ): LongInt;
var
  Check, t: LongInt;
  HBuf: Pointer;

  function AddCheck ( Check, Plus: LongInt ): LongInt;
  var
    h1, h2, h3: LongByteRec;
  begin
    h1 := LongByteRec ( Check );
    h2 := LongByteRec ( Plus );
    h3.b4 := ( h1.b4 + h2.b4 ) mod 256;
    h3.b3 := ( h1.b3 + h2.b3 + ( ( h1.b4 + h2.b4 ) div 256 ) ) mod 256;
    h3.b2 := ( h1.b2 + h2.b2 + ( ( h1.b3 + h2.b3 ) div 256 ) ) mod 256;
    h3.b1 := ( h1.b1 + h2.b1 + ( ( h1.b2 + h2.b2 ) div 256 ) ) mod 256;
    result := LongInt ( h3 );
  end;
begin
  if Size = 0
    then
    result := 0
  else
  begin
    Check := 0;
    HBuf := Buffer;
    for t := 0 to ( Size div 4 ) - 1 do
    begin
      Check := AddCheck ( Check, LongInt ( HBuf^ ) );
      HBuf := pointer ( PAnsiChar( HBuf ) + 4 );
    end;
    result := Check;
  end;
end;

procedure TTTFData.BuildTheTable;
var
  SerialNumber, f, i: Word;

begin
  SerialNumber := 0;
  for i := 0 to trunc ( swap ( CMap4.segCountX2 ) / 2 ) - 2 do
    for f := segments.startCount [ i ] to segments.endCount [ i ] do
    begin
      SetLength ( CodeTable, SerialNumber + 1 );
      CodeTable [ SerialNumber ].Code := f;
      CodeTable [ SerialNumber ].ID := GetSymbID ( f, i );
      CodeTable [ SerialNumber ].Width := GetSymbWidth ( CodeTable [ SerialNumber ].ID );
      CodeTable [ SerialNumber ].Used := False;
      Inc ( SerialNumber );
    end;
end;

procedure TTTFData.GetFontCodes ( Characters: PWord; Len: Integer; Buff: Pword );
var
  CharCode: PTTFTreeNode;
  i: LongWord;
begin
  if Len > 0 then
    for i := 1 to Len do
    begin
      if Characters^ <> 160 then
      begin
        CharCode := GetCode ( Characters^, @CodeTable [ 0 ], Length ( CodeTable ) );
        Buff^ := CharCode.ID;
        Inc ( Characters );
        Inc ( Buff );
      end;
    end;
end;

procedure TTTFData.GetFontWidths ( Characters: PWord; Len: Integer; Buff: Pword );
var
  CharCode: PTTFTreeNode;
  i: LongWord;
begin
  if Len > 0 then
  begin
    for i := 1 to Len do
    begin
      CharCode := GetCode ( Characters^, @CodeTable [ 0 ], Length ( CodeTable ) );
      Buff^ := CharCode.Width;
      Inc ( Characters );
      Inc ( Buff );
    end;
  end;
end;

function TTTFData.DSwap ( L: Longint ): Longint;
begin
  asm
        mov EAX,L
        bswap EAX
        mov @result,EAX
  end;
end;

function TTTFData.GetCode ( Unicode: Word; BeginOfTable: PTTFTreeNode; TableLength: Word ): PTTFTreeNode;
const
  opa: TTTFTreeNode = ( Code: 32; ID: 0; Used: True );
var
  AproxCenter: PTTFTreeNode;
  HalfLength: Word;
begin
  if TableLength > 1 then
  begin
    HalfLength := TableLength div 2;
    AproxCenter := BeginOfTable;
    Inc ( AproxCenter, HalfLength );
    if Unicode > AproxCenter.Code then
      Result := GetCode ( Unicode, AproxCenter, ( TableLength - HalfLength ) )
    else if Unicode < AproxCenter.Code then
      Result := GetCode ( Unicode, BeginOfTable, HalfLength )
    else
    begin
      Result := AproxCenter;
    end;
  end
  else if Unicode = BeginOfTable.Code then
    Result := BeginOfTable
  else
    Result := @(CodeTable [ 0 ]) ;
end;

function TTTFData.GetSymbID ( Unicode: Word; i: integer ): Word;
var
  off: longint;
  unicode_glyf: Word;
begin
  Result := 0;
  if ( unicode <= segments.endCount [ i ] ) and ( unicode >= segments.startCount [ i ] ) then
  begin
    if segments.idRangeOffset [ i ] = 0 then
    begin
      unicode_glyf := unicode + segments.idDelta [ i ];
      Result := unicode_glyf;
    end
    else
    begin
      off := i + trunc ( segments.idRangeOffset [ i ] shr 1 ) - trunc ( swap ( CMap4.segCountX2 ) shr 1 ) + unicode - segments.startCount [ i ];
      unicode_glyf := Word ( g_array_Id [ off ] ) + segments.idDelta [ i ];
      Result := unicode_glyf;
    end;
  end;
end;

procedure TTTFData.getglyf ( Unicode: Word );
var
  i: integer;
  off: longint;
  unicode_glyf: Word;
  Glyf_next: PGlyf_type;
begin
  unicode_glyf := 0;
  for i := 0 to trunc ( swap ( CMap4.segCountX2 ) / 2 ) - 1 do
    if ( unicode <= segments.endCount [ i ] ) and ( unicode >= segments.startCount [ i ] ) then
    begin
      if segments.idRangeOffset [ i ] = 0 then
      begin
        unicode_glyf := unicode + segments.idDelta [ i ];
        Break;
      end
      else
      begin
        off := i + trunc ( segments.idRangeOffset [ i ] / 2 ) - trunc ( swap ( CMap4.segCountX2 ) / 2 ) + unicode - segments.startCount [ i ];
        unicode_glyf := Word ( g_array_Id [ off ] ) + segments.idDelta [ i ];
        Break;
      end;
    end
    else
      unicode_glyf := 0;
  Glyf_next := FGlyf;
  for i := 1 to swap ( FMaxp_type.numGlyphs ) do
  begin
    if ( i = unicode_glyf + 1 ) then
    begin
      Glyf_next.tag := true; break;
    end;
    Glyf_next := Glyf_next.next;
  end;
end;


function TTTFData.SizeOfGlyf: LongInt;
var
  i: longint;
  size: LongInt;
  Glyf_next: PGlyf_type;
begin
  Glyf_next := Fglyf; size := 0;
  for i := 1 to swap ( FMaxp_type.numGlyphs ) do
  begin
    size := size + Glyf_next.size;
    Glyf_next := Glyf_next.next;
  end;
  result := size;
end;


procedure TTTFData.SaveFont ( Stream: TStream );
var
  Sign, InsTable: PLongInt;
  PrevOff, PrevSize: Integer;
  AuxGlyf1, AuxGlyf2: PGlyf_type;

  procedure CalcStep ( Block: Pointer; BlockSize: LongInt );
  begin
    Inc ( Sign );
    Sign^ := CalculateChecksum ( Block, BlockSize );
    Inc ( Sign );
    Sign^ := DSwap ( PrevOff );
    Inc ( Sign );
    Sign^ := DSwap ( PrevSize );
    Dec ( Sign, 3 );
  end;

  procedure FoldInfo ( TablBlock: Pointer; Index: Integer; Size: LongWord; MainTable: boolean );
  var
    TableName: LongWord;
  begin
    if MainTable then
      TableName := InTables [ Index ]
    else
      TableName := AddTables [ Index ];
    Sign^ := DSwap ( TableName );
    Inc ( Sign );
    Sign^ := CalculateChecksum ( TablBlock, Size );
    Inc ( Sign );
    PrevOff := PrevOff + PrevSize;
    PrevOff := ( PrevOff + 3 ) and not 3;
    Sign^ := DSwap ( PrevOff );
    Inc ( Sign );
    PrevSize := Size;
    Sign^ := DSwap ( Size );
    Dec ( Sign, 3 );
    Stream.Write ( Sign^, 16 );
  end;

  function getSize ( Size: LongWord ): LongWord;
  begin
    Result := ( Size + 3 ) and not 3;
  end;
var
  i, j: integer;
  off: longint;
  Glyf_next: PGlyf_type;
  TableBuff: PByte;

begin
  CutGlyfs;
  GetMem ( Sign, 16 );
  try
    TableBuff := @OutHeader [ 1 ];
    Stream.Write ( TableBuff^, 12 );
    off := 12 + 160;
    PrevOff := 0;
    PrevSize := off;
    FoldInfo ( @hhea, 5, FhheaSize, true );
    if swap ( Fhead.indexToLocFormat ) = 1 then
      FoldInfo ( FLoca_big, 4, FLocaSize, false )
    else
      FoldInfo ( FLoca_small, 4, FLocaSize, false );
    FoldInfo ( @cmap [ 0 ], 6, Length ( cmap ), false );
    Sign^ := DSwap ( InTables [ 4 ] );
    Inc ( Sign );
    Sign^ := CalculateChecksum ( @FHead, Sizeof ( THead_type ) );
    Inc ( Sign );
    PrevOff := PrevOff + PrevSize;
    PrevOff := ( PrevOff + 3 ) and not 3;
    Sign^ := DSwap ( PrevOff );
    Inc ( Sign );
    PrevSize := FHeadSize;
    Sign^ := DSwap ( PrevSize );
    Dec ( Sign, 3 );
    Stream.Write ( Sign^, 16 );
    FoldInfo ( @FMaxp_type, 3, FmaxpSize, true );
    FoldInfo ( @cvt [ 0 ], 1, Length ( cvt ), false );
    Sign^ := DSwap ( AddTables [ 3 ] );
    Inc ( Sign );
    Sign^ := CalculateChecksum ( @prep [ 0 ], Length ( prep ) );
    Inc ( Sign );
    PrevOff := PrevOff + PrevSize;
    PrevOff := ( PrevOff + 3 ) and not 3;
    Sign^ := DSwap ( PrevOff );
    Inc ( Sign );
    PrevSize := Length ( prep );
    Sign^ := DSwap ( Length ( prep ) );
    Dec ( Sign, 3 );
    Stream.Write ( Sign^, 16 );
    Sign^ := DSwap ( AddTables [ 5 ] );
    Inc ( Sign );
    Sign^ := CalculateChecksum ( @prep [ 0 ], Length ( prep ) );
    Inc ( Sign );
    PrevOff := PrevOff + PrevSize;
    PrevOff := ( PrevOff + 3 ) and not 3;
    Sign^ := DSwap ( PrevOff );
    Inc ( Sign );
    PrevSize := SizeOfGlyf;
    Sign^ := DSwap ( PrevSize );
    Dec ( Sign, 3 );
    Stream.Write ( Sign^, 16 );
    FoldInfo ( @hmtx [ 0 ], 2, Length ( Hmtx ), true );
    FoldInfo ( @fpgm [ 0 ], 2, Length ( fpgm ), false );
    Stream.Write ( hhea, getSize ( FhheaSize ) );
    if swap ( Fhead.indexToLocFormat ) = 1 then
      Stream.Write ( FLoca_big^, 4 + 4 * swap ( FMaxp_type.numGlyphs ) )
    else
      Stream.Write ( FLoca_small^, 2 + 2 * swap ( FMaxp_type.numGlyphs ) );
    off := trunc ( FLocaSize / 4 ) * 4;
    if Cardinal ( off ) <> FlocaSize then
      Stream.Write ( FLoca_small [ swap ( FMaxp_type.numGlyphs ) ], ( Cardinal ( off ) + 4 - FLocaSize ) );
    InsTable := @cmap [ 0 ];
    Stream.Write ( InsTable^, getSize ( Length ( cmap ) ) );
    Stream.Write ( FHead, getSize ( FheadSize ) );
    Stream.Write ( FMaxp_type, getSize ( FmaxpSize ) );
    InsTable := @cvt [ 0 ];
    Stream.Write ( InsTable^, getSize ( Length ( cvt ) ) );
    InsTable := @Prep [ 0 ];
    Stream.Write ( InsTable^, getSize ( Length ( Prep ) ) );
    Glyf_next := Fglyf;
    for i := 1 to swap ( FMaxp_type.numGlyphs ) do
    begin
      Stream.Write ( Glyf_next.Glyf_^, Glyf_next.size );
      Glyf_next := Glyf_next.next;
    end;
    off := ( ( SizeOfGlyf + 3 ) and not 3 );
    if SizeOfGlyf < off then
      Stream.Write ( j, off - SizeOfGlyf );
    InsTable := @Hmtx [ 0 ];
    Stream.Write ( InsTable^, getSize ( Length ( Hmtx ) ) );
    InsTable := @Fpgm [ 0 ];
    Stream.Write ( InsTable^, getSize ( Length ( Fpgm ) ) );
  finally
    FreeMem ( Sign );
  end;
  AuxGlyf1 := Fglyf;
  for i := 1 to swap ( FMaxp_type.numGlyphs ) do
  begin
    AuxGlyf2 := AuxGlyf1.next;
    if AuxGlyf1.Glyf_ <> nil then
      freemem ( AuxGlyf1.Glyf_ );
    dispose ( AuxGlyf1 );
    AuxGlyf1 := AuxGlyf2;
  end;
  if swap ( Fhead.indexToLocFormat ) = 1 then
    FreeMem ( FLoca_big )
  else
    FreeMem ( FLoca_small );
end;

procedure TTTFData.Composit_glyf ( Glyf: PGlyf_type );
var
  j, x: integer;
  Glyf_next: PGlyf_type;
  ind: integer;
  all: boolean;
begin
  Glyf_next := Fglyf;
  ind := 11;
  all := false;
  j := byte ( Glyf.Glyf_ [ ind + 1 ] ) shl 8 + byte ( Glyf.Glyf_ [ ind + 2 ] );
  for x := 1 to j do
    Glyf_next := Glyf_next.next;
  Glyf_next.tag := true;
  if ( byte ( Glyf_next.Glyf_ [ 0 ] ) = 255 ) and ( byte ( Glyf_next.Glyf_ [ 1 ] ) = 255 ) then
    composit_glyf ( Glyf_next );
  repeat
    if ( byte ( Glyf.Glyf_ [ ind ] ) ) and ( 33 ) = 33 then
      ind := ind + 8
    else if ( byte ( Glyf.Glyf_ [ ind ] ) ) and ( 32 ) = 32 then
      ind := ind + 6
    else
      all := true;
    j := byte ( Glyf.Glyf_ [ ind + 1 ] ) shl 8 + byte ( Glyf.Glyf_ [ ind + 2 ] );
    Glyf_next := Fglyf;
    for x := 1 to j do
      Glyf_next := Glyf_next.next;
    Glyf_next.tag := true;
    if ( byte ( Glyf_next.Glyf_ [ 0 ] ) = 255 ) and ( byte ( Glyf_next.Glyf_ [ 1 ] ) = 255 ) then
      composit_glyf ( Glyf_next );
  until all = true;
end;


procedure TTTFData.CutGlyfs;
var
  i: integer;
  off, x: longint;
  Glyf_next: PGlyf_type;
  x1: Word;
  mod_loca: PAnsiChar;

begin
  Glyf_next := Fglyf;
  for i := 1 to swap ( FMaxp_type.numGlyphs ) do
  begin
    if ( Glyf_next.tag = true ) and ( Glyf_next.size <> 0 ) then
      if ( byte ( Glyf_next.Glyf_ [ 0 ] ) = 255 ) and ( byte ( Glyf_next.Glyf_ [ 1 ] ) = 255 )
        then
        composit_glyf ( Glyf_next );
    Glyf_next := Glyf_next.next;
  end;
  off := 0; Glyf_next := Fglyf;
  for i := 1 to swap ( FMaxp_type.numGlyphs ) do
  begin
    if Glyf_next.tag = false then
      Glyf_next.size := 0;
    if swap ( Fhead.indexToLocFormat ) = 1 then
    begin
      x := DSwap ( off );
      mod_loca := @x;
      FLoca_big [ ( i - 1 ) * 4 ] := byte ( mod_loca [ 0 ] );
      FLoca_big [ ( i - 1 ) * 4 + 1 ] := byte ( mod_loca [ 1 ] );
      FLoca_big [ ( i - 1 ) * 4 + 2 ] := byte ( mod_loca [ 2 ] );
      FLoca_big [ ( i - 1 ) * 4 + 3 ] := byte ( mod_loca [ 3 ] );
      off := off + Glyf_next.size;
    end
    else
    begin
      x1 := swap ( trunc ( off / 2 ) );
      mod_loca := @x1;
      FLoca_small [ ( i - 1 ) * 2 ] := byte ( mod_loca [ 0 ] );
      FLoca_small [ ( i - 1 ) * 2 + 1 ] := byte ( mod_loca [ 1 ] );
      off := off + Glyf_next.size;
    end;
    Glyf_next := Glyf_next.next;
  end;
  if swap ( Fhead.indexToLocFormat ) = 1 then
  begin
    x := DSwap ( off );
    mod_loca := @x;
    FLoca_big [ swap ( FMaxp_type.numGlyphs ) * 4 ] := byte ( mod_loca [ 0 ] );
    FLoca_big [ swap ( FMaxp_type.numGlyphs ) * 4 + 1 ] := byte ( mod_loca [ 1 ] );
    FLoca_big [ swap ( FMaxp_type.numGlyphs ) * 4 + 2 ] := byte ( mod_loca [ 2 ] );
    FLoca_big [ swap ( FMaxp_type.numGlyphs ) * 4 + 3 ] := byte ( mod_loca [ 3 ] );
  end
  else
  begin
    x1 := swap ( trunc ( off / 2 ) );
    mod_loca := @x1;
    FLoca_small [ swap ( FMaxp_type.numGlyphs ) * 2 ] := byte ( mod_loca [ 0 ] );
    FLoca_small [ swap ( FMaxp_type.numGlyphs ) * 2 + 1 ] := byte ( mod_loca [ 1 ] );
  end;
end;

function TTTFData.GetSymbWidth ( ID: Word ): LongWord;
var
  KEm, FloatWith: Real;
begin
  if FMono then
    FloatWith := Swap ( Hmtx [ 0 ] )
  else
    FloatWith := Swap ( Hmtx [ ID * 2 ] );
  KEm := FUnitsPerEm / 1000;
  FloatWith := ( FloatWith / KEm );
  Result := Trunc ( FloatWith );
end;

procedure TTTFData.TTFException ( TypeOfException: byte );
begin
  case TypeOfException of
    0: raise Exception.Create ( 'Incorrect or unknown font format' );
    1: raise Exception.Create ( 'Cannot recieve data from font' );
  end;
end;

destructor TTTFData.Destroy;
begin
  CodeTable := nil;
  if FHmtxIsOK then
    Hmtx := nil;
end;

function TTTFData.GetCharWidth ( Character: Word ): Integer;
begin
  Result := GetCode ( Character, @CodeTable [ 0 ], Length ( CodeTable ) ).Width;
end;

function TTTFData.GetCharID ( Character: Word ): Integer;
var
  CC: PTTFTreeNode;
begin
  CC := GetCode ( Character, @CodeTable [ 0 ], Length ( CodeTable ) );
  Result := CC.ID;
  CC.Used := True;
end;

procedure TTTFData.GetToUnicodeStream ( Alias: string; Stream: TStream );
var
  SS: TStringStream;
  I, K: Integer;
  First, Last: Integer;
  US: Integer;

begin
  First := $FFFF;
  Last := 0;
  US := 0;
  for i := 0 to Length ( CodeTable ) - 1 do
    if CodeTable [ i ].Used then
    begin
      Inc ( US );
      if Last < CodeTable [ i ].ID then
        Last := CodeTable [ i ].ID;
      if First > CodeTable [ I ].ID then
        First := CodeTable [ i ].ID;
    end;
  ss := TStringStream.Create ( '' );
  try
    ss.WriteString ('/CIDInit /ProcSet findresource begin 12 dict begin begincmap /CIDSystemInfo << '#13);
    ss.WriteString ('/Registry (' + Alias + '+0) /Ordering (T42UV) /Supplement 0 >> def'#13);
    ss.WriteString ('/CMapName /' + Alias + '+0 def'#13);
    ss.WriteString ('/CMapType 2 def'#13);
    ss.WriteString ('1 begincodespacerange <' + WordToHex ( First ) + '> <' + WordToHex ( Last ) + '> endcodespacerange'#13);
    I := 0;
    while US > 100 do
    begin
      ss.WriteString ( '100 beginbfchar'#13 );
      K := 0;
      while K < 100 do
      begin
        if CodeTable [ I ].Used then
        begin
          ss.WriteString ( '<' + WordToHex ( Codetable [ i ].ID ) + '> <' + WordToHex ( Codetable [ i ].Code ) + '>'#13 );
          Inc ( K );
        end;
        Inc ( I );
      end;
      dec ( US, 100 );
      ss.WriteString ( 'endbfchar'#13 );
    end;
    K := 0;
    ss.WriteString ( Inttostr ( US ) + ' beginbfchar'#13 );
    while K < US do
    begin
      if CodeTable [ I ].Used then
      begin
        ss.WriteString ( '<' + WordToHex ( Codetable [ i ].ID ) + '> <' + WordToHex ( Codetable [ i ].Code ) + '>'#13 );
        Inc ( K );
      end;
      Inc ( I );
    end;
    ss.WriteString ( 'endbfchar'#13 );
    ss.WriteString ( 'endcmap CMapName currentdict /CMap defineresource pop end end' );
    ss.Position := 0;
    Stream.CopyFrom ( SS, ss.Size );
  finally
    ss.Free;
  end;
end;

procedure TTTFData.GetFontSubset ( OutStream: TStream );
var
  i: Integer;
  F: Integer;
begin
  f := 0;
  for I:= 0 to Length ( CodeTable ) - 1 do
    if CodeTable[I].Used then 
      Inc(f);
  if ( f < 2 )  then
  begin
      CodeTable[1].Code := 34;
      CodeTable[1].Used := true;
      CodeTable[2].Used := true;
  end;
  GetGlyfs;
  for i := 0 to Length ( CodeTable ) - 1 do
    if ( CodeTable [ i ].Used ) then
      getglyf ( CodeTable [ i ].Code );
  SaveFont ( OutStream );
end;

function TTTFData.GetWideString: string;
var
  S: string;
  I: Integer;
begin
  S := '';
  for i := 0 to Length ( CodeTable ) - 1 do
  begin
    if CodeTable [ i ].Used then
      s := s + Inttostr ( CodeTable [ i ].ID ) + '[' + Inttostr ( CodeTable [ I ].Width ) + ']';
  end;
  result := '[' + s + ']';
end;

procedure TTTFData.SetCharIDUsed ( var ID: Word );
var
  i: integer;
begin
  for i := 0 to Length ( CodeTable ) - 1 do
    if CodeTable [ i ].ID = ID then
    begin
      CodeTable [ i ].Used := true;
      Exit;
    end;
  CodeTable [ 0 ].Used := True;
  ID := CodeTable [ 0 ].ID;
end;

function TTTFData.GetNodeByID ( ID: Word ):PTTFTreeNode;
var
  i: integer;
begin
  for i := 0 to Length ( CodeTable ) - 1 do
    if CodeTable [ i ].ID = ID then
    begin
      Result := @CodeTable [ i ];
      Exit;
    end;
  Result := @CodeTable [ 0 ];
end;


end.

