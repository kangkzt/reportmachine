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
unit pdfttf;

interface

uses
    Windows,
    SysUtils,
    Classes,
    Graphics,
    Math;

procedure GetFontSubset(FontName: TFontname; FontStyle: TFontStyles; OutStream: TStream; CharsetBuffer: PWord; CharBufferLen: Word);

implementation

type
    USHORT = Word;
    SHORT = Smallint;
    ULONG = Longint;
    LONG = Longint;
    FWORD = WORD;
    UFWORD = WORD;

    GlyphCodes = record
        CODE: Word;
        UNICODE: Word;
        GlyphID: Word;
        SerialNumber: Word;
    end;

    FIXED = packed record
        Hi, Lo: USHORT;
    end;

    Segment = record
        StartCode: Word;
        EndCode: Word;
        idRange: Word;
    end;

    FileHeaderRec = packed record
        Version: FIXED;
        NumTables: USHORT;
        SearchRange: USHORT;
        EntrySelector: USHORT;
        RangeShift: USHORT;
    end;

    LongByteRec = record
        b1,
            b2,
            b3,
            b4: byte;
    end;

    GlyfRec = record
        Index: Integer;
        Glyf: Pointer;
        Size: LongInt;
        Links: array of word;
    end;

    TableInfoRec = packed record
        Tag: ULONG;
        Checksum: ULONG;
        Offset: ULONG;
        Length: ULONG;
    end;

    Maxp_type = packed record
        version: fixed;
        numGlyphs: USHORT;
        maxPoints: USHORT;
        maxContours: USHORT;
        maxCompositePoints: USHORT;
        maxCompositeContours: USHORT;
        maxZones: USHORT;
        maxTwilightPoints: USHORT;
        maxStorage: USHORT;
        maxFunctionDefs: USHORT;
        maxInstructionDefs: USHORT;
        maxStackElements: USHORT;
        maxSizeOfInstructions: USHORT;
        maxComponentElements: USHORT;
        maxComponentDepth: USHORT;
    end;

    PGlyf_type = ^TGlyf_type;

    TGlyf_type = packed record
        num: integer;
        size: long;
        Glyf_: Pchar;
        next: PGlyf_type;
        tag: boolean;
    end;

    THead_type = packed record
        version_number: FIXED;
        fontRevision: FIXED;
        checkSum: ULONG;
        magicNumber: ULONG;
        flags: USHORT;
        unitsPerEm: USHORT;
        two_date: array[1..16] of byte;
        xMin: SHORT;
        yMin: SHORT;
        xMax: SHORT;
        yMax: SHORT;
        macStyle: USHORT;
        lowestRec: USHORT;
        fontDirection: SHORT;
        indexToLocFormat: SHORT;
        glyphDataFormat: SHORT
    end;

    CmapHeaderRec = packed record
        Version: USHORT;
        NumTables: USHORT;
    end;

    CmapTableInfoRec = packed record
        Platform_ID: USHORT;
        Encoding_ID: USHORT;
        Offset: ULONG;
    end;

    TCMap4HEAD = packed record
        Format: USHORT;
        length: USHORT;
        version: USHORT;
    end;

    TCMap4Segment = record
        endCount: array of UShort;
        zero_byte: UShort;
        startCount: array of UShort;
        idDelta: array of Short;
        idRangeOffset: array of UShort;
    end;

    Hhea_type = packed record
        TablVer: LongInt;
        Ascender: FWORD;
        Descender: FWORD;
        LineGap: FWORD;
        advanceWidthMax: UFWORD;
        minLeftSideBearing: FWORD;
        minRightSideBearing: FWORD;
        xMaxExtent: FWORD;
        caretSlopeRise: SHORT;
        caretSlopeRun: SHORT;
        caretOffset: SHORT;
        res1: SHORT;
        res2: SHORT;
        res3: SHORT;
        res4: SHORT;
        metricDataFormat: SHORT;
        numberOfHMetrics: USHORT;
    end;

    TCMap4 = record
        segCountX2: UShort;
        searchRange: UShort;
        entrySelector: UShort;
        rangeShift: UShort;
    end;

    PLongint = ^Longint;
    TDirectory = packed array[0..2047] of TableInfoRec;
    PDirectory = ^TDirectory;

    TAnsiFontSubset = class
    private
        IsFPGMPresent: boolean;
        IsCVTPresent: Boolean;
        IsPREPPresent: Boolean;
        NoBreakSpacePres: boolean;
        TTFDirectory: PLongInt;
        FLocaSize: LongWord;
        FTableCount: Integer;
        FHead: THead_type;
        Prep: array of byte;
        hmtx: array of Word;
        fpgm: array of byte;
        cvt: array of byte;
            name: array of byte;
        hhea: Hhea_type;
        FMaxp_type: Maxp_type;
        FLoca_big: PByteArray;
        FLoca_small: PByteArray;
        g_array_Id: array of SmallInt;
        FGlyf: PGlyf_type;
        CmapHeader: CmapHeaderRec;
        CMapSize: LongWord;
        CmapTableInfo: CmapTableInfoRec;
        Cmap4_HEAD: TCMap4HEAD;
        CMap4: TCMap4;
        Segments: TCMap4Segment;
        GlyfCount: Integer;
        MonoSpace: boolean;
        FFontName: TFontname;
        FFontStyle: TFontStyles;
        function DSwap(L: LongWord): LongWord;
        function CalculateChecksum(Buffer: Pointer; Size: ULONG): ULONG;
        procedure CutGlyfs;
        function GetGlyf(Unicode: UShort): boolean;
        procedure GetFont(FontName: TFontname; FontStyle: TFontStyles; OutStream: Tstream; CharsetBuffer: PWord; CharBufferLen: Word);
        function GetUnicode(GyphID: word): word;
        procedure QuickSort(var A: array of GlyphCodes; iLo, iHi: Integer; FromUnicode: boolean);
        procedure LoadTables;
        procedure TTFException(TypeOfException: byte);
        procedure SaveFont(Stream: TStream);
        function CheckGlyf(Unicode: UShort): boolean;
    end;

const
    InTables: array[1..12] of LongWord = ($636D6170, $63767420, $6670676D, $676C7966, $68656164, $68686561, $686D7478, $6C6F6361, $6D617870, $6E616D65, $70726570, $706F7374);
    OutHeader: array[1..16] of byte = (0, 1, 0, 0, 0, $C, 0, $80, 0, 3, 0, $10,
        $63, $6D, $61, $70);
    CMHeader: array[1..59] of byte = (0, 0, 0, 2, 0, 1, 0, 0, 0, 0, 0, $14, 0, 3, 0, 1,
        0, 0, 1, $1A, 0, 0, 1, 6, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2);
    PostTable: array[1..52] of byte = (00, 02, 00, 00, 00, 00, 00, 00, $FF, $27, 00, $96, 00, 00, 00, 00,
        00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
        00, 04, 00, 00, 00, 00, 01, 02, 00, $44, 09, $61, $66, $69, $69, $31, $30, $30, $36, $35);
    ColNames = 12;
    NameTable: array[0..259] of byte = ($00, $00, $00, $0A,
        $00, $7E, $00, $01, $00, $00, $00, $00, $00, $01, $00, $05, $00, $00, $00, $01,
        $00, $00, $00, $00, $00, $02, $00, $07, $00, $05, $00, $01, $00, $00, $00, $00,
        $00, $04, $00, $05, $00, $0C, $00, $01, $00, $00, $00, $00, $00, $06, $00, $07,
        $00, $11, $00, $01, $00, $00, $00, $00, $00, $03, $00, $1F, $00, $18, $00, $03,
        $00, $01, $04, $09, $00, $01, $00, $0A, $00, $37, $00, $03, $00, $01, $04, $09,
        $00, $02, $00, $0E, $00, $41, $00, $03, $00, $01, $04, $09, $00, $04, $00, $0A,
        $00, $4F, $00, $03, $00, $01, $04, $09, $00, $06, $00, $0E, $00, $59, $00, $03,
        $00, $01, $04, $09, $00, $03, $00, $1E, $00, $67, $41, $72, $69, $61, $6C, $52,
        $65, $67, $75, $6C, $61, $72, $41, $72, $69, $61, $6C, $41, $72, $69, $61, $6C,
        $4D, $54, $6C, $6C, $50, $44, $46, $6C, $69, $62, $3A, $20, $41, $72, $69, $61,
        $6C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
        $00, $00, $41, $00, $72, $00, $69, $00, $61, $00, $6C, $00, $52, $00, $65, $00,
        $67, $00, $75, $00, $6C, $00, $61, $00, $72, $00, $41, $00, $72, $00, $69, $00,
        $61, $00, $6C, $00, $41, $00, $72, $00, $69, $00, $61, $00, $6C, $00, $4D, $00,
        $54, $00, $6C, $00, $6C, $00, $50, $00, $44, $00, $46, $00, $6C, $00, $69, $00,
        $62, $00, $3A, $00, $20, $00, $41, $00, $72, $00, $69, $00, $61, $00, $6C, $00);

function TAnsiFontSubset.DSwap(L: Longword): Longword; assembler;
asm
    mov EAX,L
    bswap EAX
    mov @result,EAX
end;

function TAnsiFontSubset.GetGlyf(Unicode: UShort): boolean;
var
    i: integer;
    off: longint;
    unicode_glyf: UShort;
    Glyf_next: PGlyf_type;
begin
    unicode_glyf := 0;
    if Unicode = 160 then
        NoBreakSpacePres := true;
    for i := 0 to trunc(swap(CMap4.segCountX2) / 2) - 1 do
        if (unicode <= segments.endCount[i]) and (unicode >= segments.startCount[i]) then
        begin
            if segments.idRangeOffset[i] = 0 then
            begin
                unicode_glyf := unicode + segments.idDelta[i];
                Break;
            end
            else
            begin
                off := i + trunc(segments.idRangeOffset[i] / 2) - trunc(swap(CMap4.segCountX2) shr 1) + unicode - segments.startCount[i];
                unicode_glyf := UShort(g_array_Id[off]) + segments.idDelta[i];
                Break;
            end;
        end
        else
            unicode_glyf := 0;
    result := false;
    Glyf_next := FGlyf;
    for i := 1 to swap(FMaxp_type.numGlyphs) do
    begin
        if (i = unicode_glyf + 1) then
        begin
            Glyf_next.tag := true;
            break;
        end;
        Glyf_next := Glyf_next.next;
    end;
end;

function TAnsiFontSubset.CheckGlyf(Unicode: UShort): boolean;
var
    i: integer;
begin
    result := false;
    for i := 0 to trunc(swap(CMap4.segCountX2) / 2) - 1 do
        if (unicode <= segments.endCount[i]) and (unicode >= segments.startCount[i]) then
        begin
            result := true;
            Exit;
        end;
end;

function TAnsiFontSubset.CalculateChecksum(Buffer: Pointer; Size: ULONG): ULONG;
var
    Check, t: ULONG;
    HBuf: Pointer;

    function AddCheck(Check, Plus: ULONG): ULONG;
    var
        h1, h2, h3: LongByteRec;
    begin
        h1 := LongByteRec(Check);
        h2 := LongByteRec(Plus);
        h3.b4 := (h1.b4 + h2.b4) mod 256;
        h3.b3 := (h1.b3 + h2.b3 + ((h1.b4 + h2.b4) div 256)) mod 256;
        h3.b2 := (h1.b2 + h2.b2 + ((h1.b3 + h2.b3) div 256)) mod 256;
        h3.b1 := (h1.b1 + h2.b1 + ((h1.b2 + h2.b2) div 256)) mod 256;
        result := ULONG(h3);
    end;
begin
    if Size = 0
        then
        result := 0
    else
    begin
        Check := 0;
        HBuf := Buffer;
        for t := 0 to (Size div 4) - 1 do
        begin
            Check := AddCheck(Check, ULONG(HBuf^));
            HBuf := pointer(pchar(HBuf) + 4);
        end;
        result := Check;
    end;
end;

procedure TAnsiFontSubset.TTFException(TypeOfException: byte);
begin
    case TypeOfException of
        0: raise Exception.Create('Incorrect or unknown font format');
        1: raise Exception.Create('Cannot recieve data from font');
    end;
end;

procedure TAnsiFontSubset.LoadTables;
var
    i, j, K: integer;
    fix_pitch, off, size: longint;
    UShort_read: UShort;
    p_pitch: PLongint;
    Glyf_next, Glyf_tmp: PGlyf_type;
    TablBegin: PByte;
    x: Ulong;
    x1: Ushort;
    DC: HDC;
    FZ: Cardinal;
    LF: TLogFont;
    Obj: THandle;
    M: TTextMetric;
    f: Integer;
    FontStream: TMemoryStream;
begin
    new(FGlyf);
    IsFPGMPresent := True;
    IsCVTPresent := True;
    IsPREPPresent := True;
    FGlyf^.next := nil;
    FGlyf^.Glyf_ := nil;
    off := 0;
    DC := CreateCompatibleDC(0);
    try
        FillChar(LF, SizeOf(LF), 0);
        with LF do
        begin
            lfHeight := -10;
            lfWidth := 0;
            lfEscapement := 0;
            lfOrientation := 0;
            if fsBold in FFontStyle then
                lfWeight := FW_BOLD
            else
                lfWeight := FW_NORMAL;
            lfItalic := Byte(fsItalic in FFontStyle);
            lfUnderline := Byte(fsUnderline in FFontStyle);
            lfStrikeOut := Byte(fsStrikeOut in FFontStyle);
            lfCharSet := ANSI_CHARSET;
            StrPCopy(lfFaceName, FFontName);
            lfQuality := DEFAULT_QUALITY;
            lfOutPrecision := OUT_DEFAULT_PRECIS;
            lfClipPrecision := CLIP_DEFAULT_PRECIS;
            lfPitchAndFamily := DEFAULT_PITCH;
        end;
        obj := CreateFontIndirect(LF);
        try
            SelectObject(DC, Obj);
            GetTextMetrics(DC, M);
            FontStream := TMemoryStream.Create;
            try
                for f := 1 to Length(InTables) do
                begin
                    FontStream.Position := 0;
                    FZ := GetFontData(DC, DSwap(InTables[f]), 0, nil, 0);
                    if FZ = GDI_ERROR then
                        if f = 2 then
                        begin
                            IsCVTPresent := false;
                            continue;
                        end
                        else if f = 3 then
                        begin
                            IsFPGMPresent := false;
                            continue;
                        end
                        else if f = 11 then
                        begin
                            IsPREPPresent := false;
                            continue;
                        end

                        else
                            TTFException(1);
                    FontStream.SetSize(FZ);
                    if GetFontData(DC, DSwap(InTables[f]), 0, FontStream.Memory, FZ) = GDI_ERROR then
                        TTFException(1);
                    FontStream.Position := 0;
                    case f of
                        1: //CMAP
                            begin
                                FontStream.Read(CmapHeader, sizeof(CmapHeader));
                                j := Swap(CmapHeader.NumTables);
                                for i := 1 to j do
                                begin
                                    FontStream.Read(CmapTableInfo, sizeof(CmapTableInfo));
                                    if (swap(CmapTableInfo.Platform_ID) = 3) and (swap(CmapTableInfo.Encoding_ID) = 1) then
                                    begin
                                        off := Dswap(CmapTableInfo.Offset);
                                        break;
                                    end;
                                    if (swap(CmapTableInfo.Platform_ID) = 3) and (swap(CmapTableInfo.Encoding_ID) = 0) then
                                    begin
                                        off := Dswap(CmapTableInfo.Offset);
                                    end;
                                end;
                                if FontStream.Seek(off, soFromBeginning) <> (off) then
                                    raise exception.Create('Offset Error');
                                FontStream.Read(Cmap4_HEAD, sizeof(Cmap4_HEAD));
                                FontStream.Read(CMap4.segCountX2, 2);
                                FontStream.Read(CMap4.searchRange, 2);
                                FontStream.Read(CMap4.entrySelector, 2);
                                FontStream.Read(CMap4.rangeShift, 2);
                                K := swap(CMap4.segCountX2) shr 1;
                                SetLength(segments.endCount, k);
                                SetLength(segments.startCount, K);
                                SetLength(segments.idRangeOffset, K);
                                SetLength(segments.idDelta, K);
                                for i := 0 to K - 1 do
                                begin
                                    FontStream.Read(UShort_read, 2);
                                    j := swap(UShort_read);
                                    segments.endCount[i] := j;
                                end;
                                FontStream.Read(UShort_read, 2);
                                segments.zero_byte := swap(UShort_read);
                                for i := 0 to K - 1 do
                                begin
                                    FontStream.Read(UShort_read, 2);
                                    segments.startCount[i] := swap(UShort_read);
                                end;
                                for i := 0 to K - 1 do
                                begin
                                    FontStream.Read(UShort_read, 2);
                                    X1 := swap(UShort_read);
                                    segments.idDelta[i] := X1;
                                end;
                                for i := 0 to K - 1 do
                                begin
                                    FontStream.Read(UShort_read, 2);
                                    segments.idRangeOffset[i] := swap(UShort_read);
                                end;
                                j := (swap(Cmap4_HEAD.length) shr 1);
                                j := j - 8 - 4 * K;
                                SetLength(g_array_Id, j + 1);
                                for i := 0 to j do
                                begin
                                    FontStream.Read(UShort_read, 2);
                                    g_array_Id[i] := swap(UShort_read);
                                end;
                            end;
                        2: //CVT
                            begin
                                SetLength(cvt, FZ);
                                TablBegin := @cvt[0];
                                FontStream.Read(TablBegin^, FZ);
                            end;
                        3: //fpgm
                            begin
                                SetLength(Fpgm, FZ);
                                TablBegin := @Fpgm[0];
                                FontStream.Read(TablBegin^, FZ);
                            end;
                        5: //HEAD
                            FontStream.Read(FHead, sizeof(THead_type));
                        6: //HHEA
                            FontStream.Read(hhea, sizeof(hhea));
                        7: //HMTX
                            begin
                                SetLength(Hmtx, trunc(FZ / 2 + 0.5));
                                TablBegin := @Hmtx[0];
                                FontStream.Read(TablBegin^, FZ);
                            end;
                        8: //loca
                            begin
                                if swap(FHead.indexToLocFormat) = 1 then
                                begin
                                    getmem(FLoca_big, FZ);
                                    FontStream.Read(FLoca_big^, FZ);
                                end
                                else
                                begin
                                    getmem(FLoca_small, FZ);
                                    FontStream.Read(FLoca_small^, FZ);
                                end;
                            end;
                        9: //maxp
                            FontStream.Read(FMaxp_type, sizeof(Maxp_type));
                        10: //name
                            begin
                                SetLength(Name, FZ);
                                TablBegin := @Name[0];
                                FontStream.Read(TablBegin^, FZ);
                            end;
                        11: //prep
                            begin
                                SetLength(Prep, FZ);
                                TablBegin := @Prep[0];
                                FontStream.Read(TablBegin^, FZ);
                            end;
                        12: //post
                            begin
                                FontStream.Position := FontStream.Position + 12;
                                p_pitch := @fix_pitch;
                                FontStream.Read(p_pitch^, 4);
                                MonoSpace := (fix_pitch <> 0);
                            end;
                    end;
                end;
                FontStream.Position := 0;
                FZ := GetFontData(DC, DSwap(InTables[4]), 0, nil, 0);
                if FZ = GDI_ERROR then
                    TTFException(1);
                FontStream.SetSize(FZ);
                if GetFontData(DC, DSwap(InTables[4]), 0, FontStream.Memory, FZ) = GDI_ERROR then
                    TTFException(1);
                FontStream.Position := 0;
                Glyf_next := FGlyf;
                j := swap(FMaxp_type.numGlyphs);
                for i := 1 to j do
                begin
                    if swap(FHead.indexToLocFormat) = 1 then
                    begin
                        x := byte(FLoca_big[i * 4]) shl 24 + byte(FLoca_big[i * 4 + 1]) shl 16 +
                            byte(FLoca_big[i * 4 + 2]) shl 8 + byte(FLoca_big[i * 4 + 3]);
                        size := x - byte(FLoca_big[(i - 1) * 4]) shl 24 - byte(FLoca_big[(i - 1) * 4 + 1]) shl 16 -
                        byte(FLoca_big[(i - 1) * 4 + 2]) shl 8 - byte(FLoca_big[(i - 1) * 4 + 3]);
                    end
                    else
                    begin
                        size := 2 * (byte(FLoca_small[i * 2]) shl 8 + byte(FLoca_small[i * 2 + 1]) -
                            byte(FLoca_small[(i - 1) * 2]) shl 8 - byte(FLoca_small[(i - 1) * 2 + 1]));
                    end;
                    if size <> 0 then
                        getmem(Glyf_next.Glyf_, size)
                    else
                        Glyf_next.Glyf_ := nil;
                    Glyf_next.num := i;
                    Glyf_next.size := size;
                    Glyf_next.tag := false;
                    if swap(FHead.indexToLocFormat) = 1 then
                    begin
                        x := byte(FLoca_big[(i - 1) * 4]) shl 24 + byte(FLoca_big[(i - 1) * 4 + 1]) shl 16 +
                        byte(FLoca_big[(i - 1) * 4 + 2]) shl 8 + byte(FLoca_big[(i - 1) * 4 + 3]);
                        FontStream.Seek(x, soFromBeginning)
                    end
                    else
                    begin
                        x1 := byte(FLoca_small[(i - 1) * 2]) shl 8 + byte(FLoca_small[(i - 1) * 2 + 1]);
                        FontStream.Seek(2 * x1, soFromBeginning);
                    end;
                    FontStream.Read(Glyf_next.Glyf_^, size);
                    if i <> j then
                    begin
                        new(glyf_tmp);
                        glyf_tmp.next := nil;
                        glyf_tmp.Glyf_ := nil;
                        Glyf_next.next := glyf_tmp;
                        Glyf_next := glyf_tmp;
                    end
                    else
                        Glyf_next := nil;
                end;
            finally
                FontStream.SetSize(0);
                FontStream.free;
            end;
        finally
            DeleteObject(obj);
        end;
    finally
        DeleteDC(DC);
    end;
end;

procedure TAnsiFontSubset.SaveFont(Stream: TStream);
var
    CmapInc, PrevCode, i, x: integer;
    CHchew: SmallInt;
    CharCode: Word;
    off, size: longWord;
    Glyf_next: PGlyf_type;
    OutCmap: array of Byte;
    OutHmtx: Pointer;
    InclStream, TempStream: TMemoryStream;
    Sign, InsTable: PLongInt;
    PrevOff, PrevSize: Integer;
    GlyfBuff, TableBuff: PByte;
    OutLoca: PLongWord;
    CurrentCode: Word;

    procedure CalcStep(Block: Pointer; BlockSize: LongInt);
    begin
        Inc(Sign);
        Sign^ := CalculateChecksum(Block, BlockSize);
        Inc(Sign);
        Sign^ := DSwap(PrevOff);
        Inc(Sign);
        Sign^ := DSwap(PrevSize);
        Dec(Sign, 3);
    end;

    procedure FoldInfo(TablBlock: Pointer; Index: Integer; Size: LongWord);
    var
        TableName: LongWord;
    begin
        TableName := InTables[Index];
        Sign^ := DSwap(TableName);
        Inc(Sign);
        Sign^ := CalculateChecksum(TablBlock, Size);
        Inc(Sign);
        PrevOff := PrevOff + PrevSize;
        Sign^ := DSwap(PrevOff);
        Inc(Sign);
        PrevSize := Size;
        Sign^ := DSwap(Size);
        Dec(Sign, 3);
        Stream.Write(Sign^, 16);
    end;

    function GetInverselyCode(AnsiCode: word): Word;
    begin
        if (Ansicode < 128) or ((AnsiCODE < 255) and (AnsiCODE > 159)) then
            Result := Ansicode
        else if ANSICODE = 128 then
            Result := 8364
        else if ANSICODE = 129 then
            Result := 129
        else if ANSICODE = 130 then
            Result := 8218
        else if ANSICODE = 131 then
            Result := 402
        else if ANSICODE = 132 then
            Result := 8222
        else if ANSICODE = 133 then
            Result := 8230
        else if ANSICODE = 134 then
            Result := 8224
        else if ANSICODE = 135 then
            Result := 8225
        else if ANSICODE = 136 then
            Result := 710
        else if ANSICODE = 137 then
            Result := 8240
        else if ANSICODE = 138 then
            Result := 352
        else if ANSICODE = 139 then
            Result := 8249
        else if ANSICODE = 140 then
            Result := 338
        else if ANSICODE = 141 then
            Result := 141
        else if ANSICODE = 142 then
            Result := 381
        else if ANSICODE = 143 then
            Result := 143
        else if ANSICODE = 144 then
            Result := 144
        else if ANSICODE = 145 then
            Result := 8216
        else if ANSICODE = 146 then
            Result := 8217
        else if ANSICODE = 147 then
            Result := 8220
        else if ANSICODE = 148 then
            Result := 8221
        else if ANSICODE = 149 then
            Result := 8226
        else if ANSICODE = 150 then
            Result := 8211
        else if ANSICODE = 151 then
            Result := 8212
        else if ANSICODE = 152 then
            Result := 732
        else if ANSICODE = 153 then
            Result := 8482
        else if ANSICODE = 154 then
            Result := 353
        else if ANSICODE = 155 then
            Result := 8250
        else if ANSICODE = 156 then
            Result := 339
        else if ANSICODE = 157 then
            Result := 157
        else if ANSICODE = 158 then
            Result := 382
        else if ANSICODE = 159 then
            Result := 376
        else
            Result := 0;
    end;

    function GetCode(Unicode: Word): Word;
    begin
        if (Unicode < 128) or ((UNICODE < 255) and (UNICODE > 159)) then
            Result := Byte(Unicode)
        else if UNICODE = 8364 then
            Result := 128
        else if UNICODE = 129 then
            Result := 129
        else if UNICODE = 8218 then
            Result := 130
        else if UNICODE = 402 then
            Result := 131
        else if UNICODE = 8222 then
            Result := 132
        else if UNICODE = 8230 then
            Result := 133
        else if UNICODE = 8224 then
            Result := 134
        else if UNICODE = 8225 then
            Result := 135
        else if UNICODE = 710 then
            Result := 136
        else if UNICODE = 8240 then
            Result := 137
        else if UNICODE = 352 then
            Result := 138
        else if UNICODE = 8249 then
            Result := 139
        else if UNICODE = 338 then
            Result := 140
        else if UNICODE = 141 then
            Result := 141
        else if UNICODE = 381 then
            Result := 142
        else if UNICODE = 143 then
            Result := 143
        else if UNICODE = 144 then
            Result := 144
        else if UNICODE = 8216 then
            Result := 145
        else if UNICODE = 8217 then
            Result := 146
        else if UNICODE = 8220 then
            Result := 147
        else if UNICODE = 8221 then
            Result := 148
        else if UNICODE = 8226 then
            Result := 149
        else if UNICODE = 8211 then
            Result := 150
        else if UNICODE = 8212 then
            Result := 151
        else if UNICODE = 732 then
            Result := 152
        else if UNICODE = 8482 then
            Result := 153
        else if UNICODE = 353 then
            Result := 154
        else if UNICODE = 8250 then
            Result := 155
        else if UNICODE = 339 then
            Result := 156
        else if UNICODE = 157 then
            Result := 157
        else if UNICODE = 382 then
            Result := 158
        else if UNICODE = 376 then
            Result := 159
        else
            Result := 0;
    end;

var
    CmapSegments: array of Segment;
    SearchRange, Selector: LongWord;
    GlyfTable: array of GlyphCodes;
    Code, Jump: Word;
    TempW: word;
    hmtx_count: Integer;

begin
    CutGlyfs;
    TempStream := TMemoryStream.Create;
    try
        GetMem(Sign, 16);
        try
            GlyfCount := 0;
            Glyf_next := Fglyf;
            for i := 1 to swap(FMaxp_type.numGlyphs) do
            begin
                if (Glyf_next.size <> 0) then
                begin
                    inc(GlyfCount);
                    SetLength(GlyfTable, GlyfCount);
                    GlyfTable[GlyfCount - 1].GlyphID := i;
                    Jump := GetUnicode(i - 1);
                    Code := GetCode(Jump);
                    if Code = 0 then
                    begin
                        GlyfTable[GlyfCount - 1].UNICODE := 0;
                        GlyfTable[GlyfCount - 1].CODE := 0;
                    end
                    else
                    begin
                        GlyfTable[GlyfCount - 1].UNICODE := GetInverselyCode(Code);
                        GlyfTable[GlyfCount - 1].CODE := Code;
                    end;
                    GlyfTable[GlyfCount - 1].SerialNumber := GlyfCount + 3;
                end;
                Glyf_next := Glyf_next.next;
            end;
            QuickSort(GlyfTable, 0, length(GlyfTable) - 1, false);
            TableBuff := @CMHeader[1];
            TempStream.Write(TableBuff^, 59);
            PrevCode := 32;
            CmapInc := 2;
            i := 1;
            while i <= Length(GlyfTable) do
            begin
                if GlyfTable[i - 1].CODE <> 0 then
                begin
                    CurrentCode := GlyfTable[i - 1].CODE;
                    TableBuff := @CMHeader[1];
                    for x := 1 to (CurrentCode - PrevCode - 1) do
                    begin
                        TempStream.Write(TableBuff^, 1);
                    end;
                    PrevCode := CurrentCode;
                    Inc(CmapInc);
                    TableBuff := @CmapInc;
                    TempStream.Write(TableBuff^, 1);
                end;
                Inc(i);
            end;
            i := 1;
            PrevCode := 32;
            QuickSort(GlyfTable, 0, length(GlyfTable) - 1, true);
            SetLength(CmapSegments, 1);
            CmapSegments[0].StartCode := 32;
            CmapSegments[0].idRange := (2 - CmapSegments[0].StartCode);
            while i <= Length(GlyfTable) do
            begin
                if GlyfTable[i - 1].CODE <> 0 then
                begin
                    CurrentCode := GlyfTable[i - 1].UNICODE;
                    if (((CurrentCode - PrevCode) >= 2) or (GlyfTable[i - 1].GlyphID < GlyfTable[i - 2].GlyphID) or ((GlyfTable[i - 1].GlyphID - GlyfTable[i - 2].GlyphID) >= 2)) then
                    begin
                        TempW := PrevCode;
                        CmapSegments[Length(CmapSegments) - 1].EndCode := TempW;
                        SetLength(CmapSegments, Length(CmapSegments) + 1);
                        TempW := CurrentCode;
                        CmapSegments[Length(CmapSegments) - 1].StartCode := TempW;
                        CmapSegments[Length(CmapSegments) - 1].idRange := GlyfTable[i - 1].SerialNumber - GlyfTable[i - 1].UNICODE - 1;
                    end;
                    PrevCode := CurrentCode;
                end;
                Inc(i);
            end;
            TempStream.Position := 0;
            CmapSegments[Length(CmapSegments) - 1].EndCode := PrevCode;
            SetLength(CmapSegments, Length(CmapSegments) + 1);
            CmapSegments[Length(CmapSegments) - 1].EndCode := $FFFF;
            CmapSegments[Length(CmapSegments) - 1].StartCode := $FFFF;
            CharCode := TempStream.Size;
            TempStream.Seek(0, soFromEnd);
            TableBuff := @CMHeader[1];
            while CharCode < 283 do
            begin
                TempStream.Write(TableBuff^, 1);
                Inc(CharCode);
            end;
            CharCode := 4;
            TableBuff := @CharCode;
            TempStream.Write(TableBuff^, 1);
            InclStream := TMemoryStream.Create;
            try
                CharCode := 0;
                TableBuff := @CharCode;
                InclStream.Write(TableBuff^, 2);
                CharCode := Swap(Length(CmapSegments) * 2);
                TableBuff := @CharCode;
                InclStream.Write(TableBuff^, 2);
                SearchRange := (2 * trunc(power(2, log2(Length(CmapSegments)))));
                CharCode := SearchRange;
                CharCode := Swap(CharCode);
                TableBuff := @CharCode;
                InclStream.Write(TableBuff^, 2);
                Selector := Round(Log2(SearchRange) / 2);
                CharCode := Selector;
                CharCode := Swap(CharCode);
                TableBuff := @CharCode;
                InclStream.Write(TableBuff^, 2);
                CharCode := Cardinal(2 * Length(CmapSegments)) - SearchRange;
                CharCode := Swap(CharCode);
                TableBuff := @CharCode;
                InclStream.Write(TableBuff^, 2);
                for i := 0 to Length(CmapSegments) - 1 do
                begin
                    CharCode := Swap(CmapSegments[i].EndCode);
                    TableBuff := @CharCode;
                    InclStream.Write(TableBuff^, 2);
                end;
                CharCode := 0;
                TableBuff := @CharCode;
                InclStream.Write(TableBuff^, 2);
                for i := 0 to Length(CmapSegments) - 1 do
                begin
                    CharCode := Swap(CmapSegments[i].StartCode);
                    TableBuff := @CharCode;
                    InclStream.Write(TableBuff^, 2);
                end;
                for i := 0 to Length(CmapSegments) - 2 do
                begin
                    CHchew := Swap(CmapSegments[i].idRange);
                    TableBuff := @CHchew;
                    InclStream.Write(TableBuff^, 2);
                end;
                CharCode := Swap(1);
                TableBuff := @CharCode;
                InclStream.Write(TableBuff^, 2);
                CharCode := 0;
                TableBuff := @CharCode;
                for i := 0 to Length(CmapSegments) - 1 do
                begin
                    InclStream.Write(TableBuff^, 2);
                end;
                CharCode := 2;
                CharCode := Swap(CharCode);
                TableBuff := @CharCode;
                InclStream.Write(TableBuff^, 2);
                CmapInc := 2;
                i := 1;
                CmapSegments[0].StartCode := 32;
                while i <= Length(GlyfTable) do
                begin
                    if GlyfTable[i].CODE <> 0 then
                    begin
                        Inc(CmapInc);
                        CmapInc := Swap(CmapInc);
                        TableBuff := @CmapInc;
                        InclStream.Write(TableBuff^, 2);
                        CmapInc := Swap(CmapInc);
                    end;
                    Inc(i);
                end;
                InclStream.Position := 0;
                CharCode := InclStream.Size;
                CharCode := Swap(CharCode + 2);
                TableBuff := @CharCode;
                TempStream.Write(TableBuff^, 2);
                TempStream.CopyFrom(InclStream, Swap(CharCode) - 2);
            finally
                InclStream.Free;
            end;
            TempStream.Position := 0;
            CMapSize := TempStream.Size;
            SetLength(OutCmap, CMapSize);
            TableBuff := @OutCmap[0];
            TempStream.Read(TableBuff^, CMapSize);
            TempStream.Position := 0;
            TempStream.Clear;
            Sign^ := InTables[2];
            Inc(Sign);
            Sign^ := CalculateChecksum(TableBuff, CMapSize);
            Inc(Sign);
            PrevOff := ColNames;
            if not IsFPGMPresent then
                Dec(PrevOff);
            if not IsCVTPresent then
                Dec(PrevOff);
            if not IsPREPPresent then
                Dec(PrevOff);
            PrevOff := 12 + (SizeOf(TableInfoRec) * PrevOff);
            Sign^ := DSwap(PrevOff);
            Inc(Sign);
            PrevSize := CMapSize;
            Sign^ := DSwap(PrevSize);
            Dec(Sign, 2);
            TableBuff := @OutHeader[1];
            Stream.Write(TableBuff^, 16);
            Stream.Write(Sign^, 12);
            Dec(Sign);
            if IsCVTPresent then
                FoldInfo(@cvt[0], 2, Length(cvt));
            if IsFPGMPresent then
                FoldInfo(@fpgm[0], 3, Length(fpgm));
            PrevOff := PrevOff + PrevSize;
            PrevSize := 0;
            GlyfCount := 0;
            Glyf_next := Fglyf;
            for i := 1 to swap(FMaxp_type.numGlyphs) do
            begin
                if Glyf_next.size <> 0 then
                begin
                    TempStream.Write(Glyf_next.Glyf_^, Glyf_next.size);
                    Inc(PrevSize, Glyf_next.size);
                    inc(GlyfCount);
                end;
                Glyf_next := Glyf_next.next;
            end;
            TempStream.Position := 0;
            Size := PrevSize;
            GetMem(GlyfBuff, PrevSize);
            try
                TempStream.Read(GlyfBuff^, PrevSize);
                TempStream.Clear;
                Sign^ := DSwap(InTables[4]);
                CalcStep(GlyfBuff, PrevSize);
                Stream.Write(Sign^, 16);
                x := FHead.indexToLocFormat;
                FHead.indexToLocFormat := 0;
                Sign^ := DSwap(InTables[5]);
                Inc(Sign);
                Sign^ := CalculateChecksum(@FHead, Sizeof(THead_type));
                Inc(Sign);
                PrevOff := PrevOff + PrevSize;
                Sign^ := DSwap(PrevOff);
                Inc(Sign);
                Sign^ := DSwap(Sizeof(THead_type));
                PrevSize := Sizeof(THead_type);
                Dec(Sign, 3);
                Stream.Write(Sign^, 16);
                FHead.indexToLocFormat := x;
                FoldInfo(@hhea, 6, 36);
                x := 0;
                TempStream.SetSize((GlyfCount + 3) * 4);
                for i := 0 to 1 do
                    TempStream.Write(x, 4);
                Glyf_next := Fglyf;
                TempStream.Write(Hmtx[2], 4);
                hhea.numberOfHMetrics := swap(hhea.numberOfHMetrics);
                hmtx_count := ((Length(Hmtx) * 2) div hhea.numberOfHMetrics);
                for i := 1 to swap(FMaxp_type.numGlyphs) do
                begin
                    if Glyf_next.size > 0 then
                    begin
                        if ((MonoSpace) and (hmtx_count <> 4)) then
                        begin
                            TempStream.Write(Hmtx[0], 2);
                            TempStream.Write(Hmtx[i + 2], 2);
                        end
                        else
                        begin
                            TempStream.Write(Hmtx[(i * 2) - 2], 2);
                            TempStream.Write(Hmtx[(i * 2) - 1], 2);
                        end;
                    end;
                    Glyf_next := Glyf_next.next;
                end;
                TempStream.Position := 0;
                OutHmtx := AllocMem((GlyfCount + 3) * 4);
                try
                    TempStream.Read(OutHmtx^, TempStream.Size);
                    TempStream.Clear;
                    Sign^ := DSwap(InTables[7]);
                    Inc(Sign);
                    Sign^ := CalculateChecksum(@OutHmtx, PrevSize);
                    Inc(Sign);
                    PrevOff := PrevOff + PrevSize;
                    Sign^ := DSwap(PrevOff);
                    Inc(Sign);
                    PrevSize := ((GlyfCount + 3) * 4);
                    Sign^ := DSwap(PrevSize);
                    Dec(Sign, 3);
                    Stream.Write(Sign^, 16);
                    TempStream.SetSize((GlyfCount + 4) * 2);
                    for i := 0 to 3 do
                    begin
                        TempStream.Write(CMHeader[1], 2);
                    end;
                    off := 0;
                    Glyf_next := Fglyf;
                    for i := 1 to swap(FMaxp_type.numGlyphs) do
                    begin
                        if Glyf_next.size > 0 then
                        begin
                            Inc(off, Glyf_next.size);
                            x := (Swap(off shr 1));
                            TempStream.Write(x, 2);
                        end;
                        Glyf_next := Glyf_next.next;
                    end;
                    TempStream.Position := 0;
                    FLocaSize := TempStream.Size;
                    GetMem(OutLoca, ((GlyfCount + 4) * 2));
                    try
                        TempStream.Read(OutLoca^, FLocaSize);
                        TempStream.Clear;
                        FoldInfo(OutLoca, 8, FLocaSize);
                        x := FMaxp_type.numGlyphs;
                        FoldInfo(@FMaxp_type, 9, SizeOf(Maxp_type));
                        FMaxp_type.numGlyphs := x;
                        Sign^ := DSwap($6E616D65);
                        Inc(Sign);
                        Sign^ := CalculateChecksum(@NameTable[0], Length(NameTable));
                        Inc(Sign);
                        PrevOff := PrevOff + PrevSize;
                        Sign^ := DSwap(PrevOff);
                        Inc(Sign);
                        PrevSize := Length(NameTable);
                        Sign^ := DSwap(PrevSize);
                        Dec(Sign, 3);
                        Stream.Write(Sign^, 16);
                        Sign^ := DSwap($706F7374);
                        Inc(Sign);
                        Sign^ := CalculateChecksum(@PostTable[1], Length(PostTable));
                        Inc(Sign);
                        PrevOff := PrevOff + PrevSize;
                        Sign^ := DSwap(PrevOff);
                        Inc(Sign);
                        PrevSize := Length(PostTable);
                        Sign^ := DSwap(PrevSize);
                        Dec(Sign, 3);
                        Stream.Write(Sign^, 16);
                        if IsPREPPresent then
                            FoldInfo(@prep[0], 11, Length(prep));
                        InsTable := @OutCmap[0];
                        Stream.Write(InsTable^, CMapSize);
                        if IsCVTPresent then
                        begin
                            InsTable := @cvt[0];
                            Stream.Write(InsTable^, Length(cvt));
                        end;
                        if IsFPGMPresent then
                        begin
                            InsTable := @Fpgm[0];
                            Stream.Write(InsTable^, Length(Fpgm));
                        end;
                        Stream.Write(GlyfBuff^, Size);
                        x := FHead.indexToLocFormat;
                        FHead.indexToLocFormat := 0;
                        Stream.Write(FHead, Sizeof(THead_type));
                        FHead.indexToLocFormat := x;
                        hhea.numberOfHMetrics := swap(GlyfCount + 3);
                        Stream.Write(hhea, sizeof(hhea));
                        Stream.Write(OutHmtx^, ((GlyfCount + 3) * 4));
                        Stream.Write(OutLoca^, ((GlyfCount + 4) * 2));
                        x := FMaxp_type.numGlyphs;
                        FMaxp_type.numGlyphs := swap(GlyfCount + 3);
                        Stream.Write(FMaxp_type, SizeOf(Maxp_type));
                        FMaxp_type.numGlyphs := x;
                        InsTable := @NameTable[0];
                        Stream.Write(InsTable^, Length(NameTable));
                        InsTable := @PostTable[1];
                        Stream.Write(InsTable^, Length(PostTable));
                        if IsPREPPresent then
                        begin
                            InsTable := @Prep[0];
                            Stream.Write(InsTable^, Length(Prep));
                        end;
                    finally
                        FreeMem(OutLoca);
                    end;
                finally
                    FreeMem(OutHmtx);
                end;
            finally
                FreeMem(GlyfBuff);
            end;
        finally
            FreeMem(Sign);
        end;
    finally
        TempStream.Free;
    end;
end;

procedure TAnsiFontSubset.CutGlyfs;
var
    GlyfLinks: array of integer;

    procedure EditLinks(Glyf: PGlyf_type);
    var
        j, x: integer;
        Glyf_next: PGlyf_type;
        ind: integer;
        GlyfNewInd: PWord;
        GNI: word;

        function LinkCompression: word;
        begin
            Result := GlyfLinks[j] + 3;
            if NoBreakSpacePres then
                Dec(Result);
        end;
    begin
        Glyf_next := Fglyf;
        ind := 11;
        j := byte(Glyf.Glyf_[ind + 1]) shl 8 + byte(Glyf.Glyf_[ind + 2]);
        GlyfNewInd := PWord(Glyf.Glyf_ + ind + 1);
        GNI := LinkCompression;
        GlyfNewInd^ := Swap(GNI);
        for x := 1 to j do
            Glyf_next := Glyf_next.next;
        if (byte(Glyf_next.Glyf_[0]) = 255) and (byte(Glyf_next.Glyf_[1]) = 255) then
            EditLinks(Glyf_next);
        repeat
            if (byte(Glyf.Glyf_[ind])) and (33) = 33 then
                ind := ind + 8
            else if (byte(Glyf.Glyf_[ind])) and (32) = 32 then
                ind := ind + 6
            else
                break;
            j := byte(Glyf.Glyf_[ind + 1]) shl 8 + byte(Glyf.Glyf_[ind + 2]);
            GlyfNewInd := PWord(Glyf.Glyf_ + ind + 1);
            GNI := LinkCompression;
            GlyfNewInd^ := Swap(GNI);
            Glyf_next := Fglyf;
            for x := 1 to j do
                Glyf_next := Glyf_next.next;
            if (byte(Glyf_next.Glyf_[0]) = 255) and (byte(Glyf_next.Glyf_[1]) = 255) then
                EditLinks(Glyf_next);
        until false;
    end;

    procedure composit_glyf(Glyf: PGlyf_type);
    var
        j, x: integer;
        Glyf_next: PGlyf_type;
        ind: integer;
        all: boolean;
    begin
        Glyf_next := Fglyf;
        ind := 11;
        all := false;
        j := byte(Glyf.Glyf_[ind + 1]) shl 8 + byte(Glyf.Glyf_[ind + 2]);
        for x := 1 to j do
            Glyf_next := Glyf_next.next;
        Glyf_next.tag := true;
        if (byte(Glyf_next.Glyf_[0]) = 255) and (byte(Glyf_next.Glyf_[1]) = 255) then
            composit_glyf(Glyf_next);
        repeat
            if (byte(Glyf.Glyf_[ind])) and (33) = 33 then
                ind := ind + 8
            else if (byte(Glyf.Glyf_[ind])) and (32) = 32 then
                ind := ind + 6
            else
                all := true;
            j := byte(Glyf.Glyf_[ind + 1]) shl 8 + byte(Glyf.Glyf_[ind + 2]);
            Glyf_next := Fglyf;
            for x := 1 to j do
                Glyf_next := Glyf_next.next;
            Glyf_next.tag := true;
            if (byte(Glyf_next.Glyf_[0]) = 255) and (byte(Glyf_next.Glyf_[1]) = 255) then
                composit_glyf(Glyf_next);
        until all = true;
    end;
var
    i: integer;
    Glyf_next: PGlyf_type;
begin
    GlyfCount := 0;
    Glyf_next := Fglyf;
    for i := 1 to swap(FMaxp_type.numGlyphs) do
    begin
        if (Glyf_next.tag = true) and (Glyf_next.size <> 0) then
        begin
            if (byte(Glyf_next.Glyf_[0]) = 255) and (byte(Glyf_next.Glyf_[1]) = 255) then
            begin
                composit_glyf(Glyf_next);
            end;
        end;
        Glyf_next := Glyf_next.next;
    end;
    SetLength(GlyfLinks, swap(FMaxp_type.numGlyphs));
    Glyf_next := Fglyf;
    for i := 1 to swap(FMaxp_type.numGlyphs) do
    begin
        if Glyf_next.tag = true then
        begin
            GlyfLinks[i - 1] := GlyfCount;
            inc(GlyfCount);
        end
        else
            Glyf_next.size := 0;
        Glyf_next := Glyf_next.next;
    end;
    Glyf_next := Fglyf;
    for i := 1 to swap(FMaxp_type.numGlyphs) do
    begin
        if (Glyf_next.tag = true) and (Glyf_next.size <> 0) then
        begin
            if (byte(Glyf_next.Glyf_[0]) = 255) and (byte(Glyf_next.Glyf_[1]) = 255) then
                EditLinks(Glyf_next);
        end;
        Glyf_next := Glyf_next.next;
    end;
end;

function TAnsiFontSubset.GetUnicode(GyphID: word): word;
var
    i, h: integer;
    off: longint;
    unicode_glyf: UShort;
begin
    result := 0;
    for i := 0 to trunc(swap(CMap4.segCountX2) / 2) - 1 do
    begin
        for h := segments.startCount[i] to segments.endCount[i] do
        begin
            if segments.idRangeOffset[i] = 0 then
                unicode_glyf := h + segments.idDelta[i]
            else
            begin
                off := i + trunc(segments.idRangeOffset[i] / 2) - trunc(swap(CMap4.segCountX2) shr 1) + h - segments.startCount[i];
                unicode_glyf := UShort(g_array_Id[off]) + segments.idDelta[i];
            end;
            if GyphID = unicode_glyf then
            begin
                result := h;
                Exit;
            end;
        end;
    end;
end;

procedure TAnsiFontSubset.QuickSort(var A: array of GlyphCodes; iLo, iHi: Integer; FromUnicode: boolean);
var
    Lo, Hi, Mid: Integer;
    T: GlyphCodes;
begin
    if Length(a) > 0 then
    begin
        Lo := iLo;
        Hi := iHi;
        if FromUnicode then
            Mid := A[(Lo + Hi) div 2].UNICODE
        else
            Mid := A[(Lo + Hi) div 2].CODE;
        repeat
            if FromUnicode then
                while A[Lo].UNICODE < Mid do
                    Inc(Lo)
            else
                while A[Lo].CODE < Mid do
                    Inc(Lo);
            if FromUnicode then
                while A[Hi].UNICODE > Mid do
                    Dec(Hi)
            else
                while A[Hi].CODE > Mid do
                    Dec(Hi);
            if Lo <= Hi then
            begin
                T := A[Lo];
                A[Lo] := A[Hi];
                A[Hi] := T;
                Inc(Lo);
                Dec(Hi);
            end;
        until Lo > Hi;
        if Hi > iLo then
            QuickSort(A, iLo, Hi, FromUnicode);
        if Lo < iHi then
            QuickSort(A, Lo, iHi, FromUnicode);
    end;
end;

procedure TAnsiFontSubset.GetFont(FontName: TFontname; FontStyle: TFontStyles; OutStream: Tstream; CharsetBuffer: PWord; CharBufferLen: Word);
var
    i: Integer;
    AuxGlyf1, AuxGlyf2: PGlyf_type;
    Code: Word;
    CharSubset: array of word;
    DC: HDC;
    FZ: Cardinal;
    LF: TLogFont;
    Obj: THandle;
    M: TTextMetric;
    FontStream: TMemoryStream;
begin
    if CharBufferLen = 0 then
        Exit;
    FFontName := FontName;
    FFontStyle := FontStyle;
    if (UpperCase(FFontName) = 'MARLETT') then
    begin
        DC := CreateCompatibleDC(0);
        try
            FillChar(LF, SizeOf(LF), 0);
            with LF do
            begin
                lfHeight := -10;
                lfWidth := 0;
                lfEscapement := 0;
                lfOrientation := 0;
                if fsBold in FFontStyle then
                    lfWeight := FW_BOLD
                else
                    lfWeight := FW_NORMAL;
                lfItalic := Byte(fsItalic in FFontStyle);
                lfUnderline := Byte(fsUnderline in FFontStyle);
                lfStrikeOut := Byte(fsStrikeOut in FFontStyle);
                lfCharSet := DEFAULT_CHARSET;
                StrPCopy(lfFaceName, FFontName);
                lfQuality := DEFAULT_QUALITY;
                lfOutPrecision := OUT_DEFAULT_PRECIS;
                lfClipPrecision := CLIP_DEFAULT_PRECIS;
                lfPitchAndFamily := DEFAULT_PITCH;
            end;
            obj := CreateFontIndirect(LF);
            try
                SelectObject(DC, Obj);
                GetTextMetrics(DC, M);
                FZ := GetFontData(DC, 0, 0, nil, 0);
                FontStream := TMemoryStream.Create;
                try
                    FontStream.SetSize(FZ);
                    GetFontData(DC, 0, 0, FontStream.Memory, FZ);
                    FontStream.Position := 0;
                    OutStream.CopyFrom(FontStream, FontStream.Size );
                finally
                    FontStream.Free;
                end;
            finally
                DeleteObject(obj);
            end;
        finally
            DeleteDC(DC);
        end;
        Exit;
    end;
    LoadTables;
    for i := 1 to CharBufferLen do
    begin
        Code := CharsetBuffer^;
        if CheckGlyf(Code) then
        begin
            SetLength(CharSubset, Length(CharSubset) + 1);
            CharSubset[Length(CharSubset) - 1] := Code;
        end;
        Inc(CharsetBuffer);
    end;
  if ( CharBufferLen = 1 ) then
    begin
        SetLength(CharSubset, 2);
    if CharSubset [ 0 ] < 34 then
       CharSubset [ 1 ] := 34
    else
    begin
        CharSubset [ 1 ] := CharSubset [ 0 ];
        CharSubset [ 0 ] := 34;        
    end;
    end;
    if (CharBufferLen = 2) and (CharSubset[1] = 160) then
    begin
        SetLength(CharSubset, 3);
        CharSubset[2] := CharSubset[1];
        CharSubset[1] := 34;
    end;
    for i := 0 to (Length(CharSubset) - 1) do
    begin
        Code := CharSubset[i];
        if (Code > 32) then
        begin
            GetGlyf(Code);
        end;
    end;
    SaveFont(OutStream);
    AuxGlyf1 := Fglyf;
    for i := 1 to swap(FMaxp_type.numGlyphs) do
    begin
        AuxGlyf2 := AuxGlyf1.next;
        if AuxGlyf1.Glyf_ <> nil then
            freemem(AuxGlyf1.Glyf_);
        dispose(AuxGlyf1);
        AuxGlyf1 := AuxGlyf2;
    end;
    if swap(Fhead.indexToLocFormat) = 1 then
        FreeMem(FLoca_big)
    else
        FreeMem(FLoca_small);
    FreeMem(TTFDirectory, FTableCount * sizeof(TableInfoRec));
    TTFDirectory := nil;
    FLoca_big := nil;
    FLoca_small := nil;
    Prep := nil;
    hmtx := nil;
    fpgm := nil;
    g_array_Id := nil;
end;

procedure GetFontSubset(FontName: TFontname; FontStyle: TFontStyles; OutStream: TStream; CharsetBuffer: PWord; CharBufferLen: Word);
var
    AnsiFontSubset: TAnsiFontSubset;
begin
    AnsiFontSubset := TAnsiFontSubset.Create;
    try
        AnsiFontSubset.GetFont(FontName, FontStyle, OutStream, CharsetBuffer, CharBufferLen);
    finally
        AnsiFontSubset.Free;
    end;
end;

end.

