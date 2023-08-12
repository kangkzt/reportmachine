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

unit pdfMisc;

interface
uses windows, sysutils, graphics, classes;
type
  PByteArray = ^TByteArray;
  TByteArray = array [ 0..MaxInt - 1 ] of Byte;

  TTextCTM = record
    a, b, c, d, x, y: Extended;
  end;
  PExt = ^Single;

  TSingleArray = array [ 0..$FFFF ] of Single;
  PSingleArray = ^TSingleArray;


  TWidthArray = array [ 0..255 ] of Word;
  TLargeWidthArray = array [ 0..255 ] of Cardinal;

  TPDFJSFunction = class
  private
    FID: Integer;
    FBody: string;
    FName: string;
    FParams: string;
    procedure SetBody ( const Value: string );
    procedure SetID ( const Value: Integer );
    procedure SetName ( const Value: string );
    procedure SetParams ( const Value: string );
  public
    property Name: string read FName write SetName;
    property Body: string read FBody write SetBody;
    property Params: string read FParams write SetParams;
    property ID: Integer read FID write SetID;
  end;

  TPDFJSFList = class
  private
    FList: TList;
    function GetItems ( Index: Integer ): TPDFJSFunction;
    procedure SetItems ( Index: Integer; const Value: TPDFJSFunction );
  public
    constructor Create;
    destructor Destroy; override;
    function Add ( Name, Params, Body: string ): Integer;
    function Count: Integer;
    procedure Clear;
    property Items [ Index: Integer ]: TPDFJSFunction read GetItems write SetItems; default;
  end;


procedure swp ( var A, B: Integer ); overload;
procedure swp ( var A, B: Extended ); overload;
function IsTrueType ( FontName: string; Charset: Byte ): Boolean;
function GetFontByCharset ( Charset: Byte ): string;
function ByteToHex ( B: Byte ): string;
function WordToHex ( W: Word ): string;
function FormatFloat ( Value: Extended ): string;
procedure MultiplyCTM ( var T: TTextCTM; const S: TTextCTM );
function StrToOctet ( st: string ): string;
function ByteToOct ( B: Byte ): string;

function iis ( Comp: Boolean; First, Second: Integer ): Integer; Overload;
function iis ( Comp: Boolean; First, Second: Extended ): Extended; Overload;
function iis ( Comp: Boolean; First, Second: Boolean ): Boolean; Overload;
function iis ( Comp: Boolean; First, Second: string ): string; Overload;

function IsANSICode ( UNICODE: Word ): Boolean;
function ANSIUNICODEToByte ( Unicode: Word ): Byte;
function ByteToANSIUNICODE ( B: Byte ): Word;

procedure QuickSortArray ( var Arr: array of Word );
function PosText ( const FindString, SourceString: string; StartPos: Integer ): Integer;

function FontTest ( var FontName: string; var FontStyle: TFontStyles ): Boolean;

implementation

function FontTest ( var FontName: string; var FontStyle: TFontStyles ): Boolean;
type
  TFontInfo = record
    FontName: TFontName;
    Style: TFontStyles;
    Error: Boolean;
    DefItalic: Boolean;
    DefBold: Boolean;
    Step: Integer;
  end;

  function Back ( const Enum: ENUMLOGFONTEX; const PFD: TNEWTEXTMETRICEXA; FT: DWORD; var FI: TFontInfo ): Integer; stdcall;
  var
    Bold, Italic: Boolean;
    Er: Boolean;
  begin
    if FT <> TRUETYPE_FONTTYPE then
    begin
      Result := 1;
      Exit;
    end;
    if fi.FontName <> Enum.elfLogFont.lfFaceName then
      fi.FontName := Enum.elfLogFont.lfFaceName;
    Bold := Enum.elfLogFont.lfWeight >= 600;
    Italic := Enum.elfLogFont.lfItalic <> 0;
    if FI.Step = 0 then
    begin
      FI.DefItalic := Italic;
      FI.DefBold := Bold;
    end;
    Inc ( FI.Step );
    Er := False;
    if ( fsbold in FI.Style ) <> Bold then
      Er := True;
    if ( fsItalic in FI.Style ) <> Italic then
      Er := True;
    FI.Error := Er;
    if Er then
      Result := 1
    else
      Result := 0;
  end;
var
  LogFont: TLogFont;
  DC: HDC;
  ST: TFontStyles;
  FI: TFontInfo;
begin
  FI.FontName := FontName;
  FI.Style := FontStyle;
  FillChar ( LogFont, SizeOf ( LogFont ), 0 );
  LogFont.lfCharSet := DEFAULT_CHARSET;
  move ( FI.FontName [ 1 ], LogFont.lfFaceName, Length ( FI.FontName ) * SizeOf(Char) );
  FI.Step := 0;
  FI.Error := True;
  ST := FI.Style;
  DC := GetDC ( 0 );
  try
    EnumFontFamiliesEx ( DC, LogFont, @Back, Integer ( @FI ), 0 );
    if FI.Step <> 0 then
      if FI.Error then
      begin
        if fsItalic in FI.Style then
        begin
          FI.Style := FontStyle - [ fsItalic ];
          EnumFontFamiliesEx ( DC, LogFont, @Back, Integer ( @FI ), 0 );
        end;
        if FI.Error then
          if fsBold in FontStyle then
          begin
            FI.Style := FI.Style - [ fsBold ];
            EnumFontFamiliesEx ( DC, LogFont, @Back, Integer ( @FI ), 0 );
          end;
        if FI.Error then
        begin
          FI.Style := [ ];
          EnumFontFamiliesEx ( DC, LogFont, @Back, Integer ( @FI ), 0 );
        end;
        if FI.Error then
        begin
          FI.Style := [ ];
          if FI.DefItalic then
            FI.Style := FI.Style + [ fsItalic ];
          if FI.DefBold then
            FI.Style := FI.Style + [ fsBold ];
          EnumFontFamiliesEx ( DC, LogFont, @Back, Integer ( @FI ), 0 );
        end;
      end;
  finally
    ReleaseDC ( 0, DC );
  end;
  Result := not FI.Error;
  if FI.FontName <> FontName then
    FontName := FI.FontName;
  if not FI.Error then
    FontStyle := FI.Style;
end;



function PosText ( const FindString, SourceString: string; StartPos: Integer ): Integer;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        PUSH    EDX
        TEST    EAX,EAX
        JE      @@qt
        TEST    EDX,EDX
        JE      @@qt0
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EAX,[EAX-4]
        MOV     EDX,[EDX-4]
        DEC     EAX
        SUB     EDX,EAX
        DEC     ECX
        SUB     EDX,ECX
        JNG     @@qt0
        XCHG    EAX,EDX
        ADD     EDI,ECX
        MOV     ECX,EAX
        JMP     @@nx
@@fr:   INC     EDI
        DEC     ECX
        JE      @@qt0
@@nx:   MOV     EBX,EDX
        MOV     AL,BYTE PTR [ESI]
@@lp1:  CMP     AL,BYTE PTR [EDI]
        JE      @@uu
        INC     EDI
        DEC     ECX
        JE      @@qt0
        CMP     AL,BYTE PTR [EDI]
        JE      @@uu
        INC     EDI
        DEC     ECX
        JE      @@qt0
        CMP     AL,BYTE PTR [EDI]
        JE      @@uu
        INC     EDI
        DEC     ECX
        JE      @@qt0
        CMP     AL,BYTE PTR [EDI]
        JE      @@uu
        INC     EDI
        DEC     ECX
        JNE     @@lp1
@@qt0:  XOR     EAX,EAX
@@qt:   POP     ECX
        POP     EBX
        POP     EDI
        POP     ESI
        RET
@@uu:   TEST    EDX,EDX
        JE      @@fd
@@lp2:  MOV     AL,BYTE PTR [ESI+EBX]
        CMP     AL,BYTE PTR [EDI+EBX]
        JNE     @@fr
        DEC     EBX
        JE      @@fd
        MOV     AL,BYTE PTR [ESI+EBX]
        CMP     AL,BYTE PTR [EDI+EBX]
        JNE     @@fr
        DEC     EBX
        JE      @@fd
        MOV     AL,BYTE PTR [ESI+EBX]
        CMP     AL,BYTE PTR [EDI+EBX]
        JNE     @@fr
        DEC     EBX
        JE      @@fd
        MOV     AL,BYTE PTR [ESI+EBX]
        CMP     AL,BYTE PTR [EDI+EBX]
        JNE     @@fr
        DEC     EBX
        JNE     @@lp2
@@fd:   LEA     EAX,[EDI+1]
        SUB     EAX,[ESP]
        POP     ECX
        POP     EBX
        POP     EDI
        POP     ESI
end;


procedure QuickSortArray ( var Arr: array of Word );

  procedure QuickSort ( var A: array of Word; iLo, iHi: Integer );
  var
    Lo, Hi, Mid, T: Integer;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := A [ ( Lo + Hi ) div 2 ];
    repeat
      while A [ Lo ] < Mid do
        Inc ( Lo );
      while A [ Hi ] > Mid do
        Dec ( Hi );
      if Lo <= Hi then
      begin
        T := A [ Lo ];
        A [ Lo ] := A [ Hi ];
        A [ Hi ] := T;
        Inc ( Lo );
        Dec ( Hi );
      end;
    until Lo > Hi;
    if Hi > iLo then
      QuickSort ( A, iLo, Hi );
    if Lo < iHi then
      QuickSort ( A, Lo, iHi );
  end;
begin
  QuickSort ( Arr, Low ( Arr ), High ( Arr ) );
end;

function IsANSICode ( UNICODE: Word ): Boolean;
begin
  Result := False; Exit;
  if UNICODE < 128 then
    Result := True
  else if ( UNICODE < 255 ) and ( UNICODE > 159 ) then
    Result := True
  else if ( UNICODE = 129 ) or ( UNICODE = 8218 ) or ( UNICODE = 402 )

    or ( UNICODE = 710 ) or ( UNICODE = 8240 ) or ( UNICODE = 352 )
    or ( UNICODE = 338 ) or ( UNICODE = 141 ) or ( UNICODE = 381 ) or ( UNICODE = 143 )
    or ( UNICODE = 144 )

    or ( UNICODE = 732 )  or ( UNICODE = 353 )
    or ( UNICODE = 339 ) or ( UNICODE = 157 ) or ( UNICODE = 382 ) or ( UNICODE = 376 )
{  else if ( UNICODE = 8364 ) or ( UNICODE = 129 ) or ( UNICODE = 8218 ) or ( UNICODE = 402 )
    or ( UNICODE = 8222 ) or ( UNICODE = 8230 ) or ( UNICODE = 8224 ) or ( UNICODE = 8225 )
    or ( UNICODE = 710 ) or ( UNICODE = 8240 ) or ( UNICODE = 352 ) or ( UNICODE = 8249 )
    or ( UNICODE = 338 ) or ( UNICODE = 141 ) or ( UNICODE = 381 ) or ( UNICODE = 143 )
    or ( UNICODE = 144 ) or ( UNICODE = 8216 ) or ( UNICODE = 8217 ) or ( UNICODE = 8220 )
    or ( UNICODE = 8221 ) or ( UNICODE = 8226 ) or ( UNICODE = 8211 ) or ( UNICODE = 8212 )
    or ( UNICODE = 732 ) or ( UNICODE = 8482 ) or ( UNICODE = 353 ) or ( UNICODE = 8250 )
    or ( UNICODE = 339 ) or ( UNICODE = 157 ) or ( UNICODE = 382 ) or ( UNICODE = 376 )}
    then
    Result := True
  else
    Result := False;
end;

function ANSIUNICODEToByte ( Unicode: Word ): Byte;
begin
  if ( Unicode < 128 ) or ( ( UNICODE < 255 ) and ( UNICODE > 159 ) ) then
    Result := Byte ( Unicode )
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

function ByteToANSIUNICODE ( B: Byte ): Word;
const
  UVAL: array [ 128..159 ] of word =
  ( 8364, 129, 8218, 402, 8222, 8230, 8224, 8225, 710, 8240, 352, 8249, 338, 141, 381, 143,
    144, 8216, 8217, 8220, 8221, 8226, 8211, 8212, 732, 8482, 353, 8250, 339, 157, 382, 376 );
begin
  if ( B < 128 ) or ( B > 159 ) then
    Result := B
  else
    Result := UVAL [ B ];
end;

function iis ( Comp: Boolean; First, Second: Integer ): Integer;
begin
  if Comp then
    Result := First
  else
    Result := Second;
end;

function iis ( Comp: Boolean; First, Second: Extended ): Extended;
begin
  if Comp then
    Result := First
  else
    Result := Second;
end;

function iis ( Comp: Boolean; First, Second: Boolean ): Boolean;
begin
  if Comp then
    Result := First
  else
    Result := Second;
end;

function iis ( Comp: Boolean; First, Second: string ): string;
begin
  if Comp then
    Result := First
  else
    Result := Second;
end;


function ByteToOct ( B: Byte ): string;
begin
  Result := '';
  while B > 7 do
  begin
    Result := IntToStr ( B mod 8 ) + Result;
    b := b div 8;
  end;
  Result := IntToStr ( b ) + Result;
end;

function StrToOctet ( st: string ): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length ( st ) do
    Result := Result + '\' + ByteToOct ( Ord ( st [ i ] ) );
end;

procedure MultiplyCTM ( var T: TTextCTM; const S: TTextCTM );
var
  T1: TTextCTM;
begin
  Move ( T, T1, SizeOf ( T ) );
  T.a := S.a * T1.a + S.b * T1.c;
  T.b := S.a * T1.b + S.b * T1.d;
  T.c := S.c * T1.a + S.d * T1.c;
  T.d := S.c * T1.b + S.d * T1.d;
  T.x := S.x * T1.a + S.y * T1.c + T1.x;
  T.y := S.x * T1.b + S.y * T1.d + T1.y;
end;

function ByteToHex ( B: Byte ): string;
const
  H: AnsiString = '0123456789ABCDEF';
begin
  Result := String(H [ 1 + b shr 4 ] + H [ 1 + b and $F ]);
end;

function WordToHex ( W: Word ): string;
begin
  Result := ByteToHex ( Hi ( W ) ) + ByteToHex ( Lo ( W ) )
end;

function FormatFloat ( Value: Extended ): string;
var
  c: Char;
begin
//  c := DecimalSeparator;
 // DecimalSeparator := '.';
  Result := SysUtils.FormatFloat ( '0.####', Value );
 // DecimalSeparator := c;
end;

function GetFontByCharset ( Charset: Byte ): string;
type
  FND = record
    TT: Boolean;
    FontName: array [ 0..LF_FACESIZE - 1 ] of char;
  end;
  Z = record
    Charset: Byte;
    Valid: boolean;
  end;
var
  LF: TLogFont;
  DC: HDC;
  F: FND;

  function Check1 ( const Enum: ENUMLOGFONTEX; const PFD: TNEWTEXTMETRICEXA; FT: DWORD; var F: Z ): Integer; stdcall;
  begin
    if Enum.elfLogFont.lfCharSet = F.Charset then
    begin
      f.Valid := true;
      Result := 0;
    end
    else
      result := 1;
  end;

  function Check ( const Enum: ENUMLOGFONTEX; const PFD: TNEWTEXTMETRICEXA; FT: DWORD; var F: FND ): Integer; stdcall;
  var
    LF: TLogFont;
    DC: HDC;
    ZZ: Z;
  begin
    if FT = TRUETYPE_FONTTYPE then
    begin
      ZZ.Valid := False;
      ZZ.Charset := Enum.elfLogFont.lfCharSet;
      FillChar ( LF, SizeOf ( LF ), 0 );
      LF.lfCharSet := 1;
      LF.lfFaceName := Enum.elfLogFont.lfFaceName;
      ZZ.CharSet := Enum.elfLogFont.lfCharSet;
      DC := GetDC ( 0 );
      try
        EnumFontFamiliesEx ( DC, LF, @Check1, Integer ( @ZZ ), 0 );
      finally
        ReleaseDC ( 0, DC );
      end;
      if ZZ.Valid then
      begin
        Move ( Enum.elfLogFont.lfFaceName, F.FontName, LF_FACESIZE - 1 );
        F.TT := True;
        Result := 0;
      end
      else
        result := 1;
    end
    else
      result := 1;
  end;
begin
  if Charset = 1 then
    Charset := GetDefFontCharSet;
  FillChar ( LF, SizeOf ( LF ), 0 );
  FillChar ( F, SizeOf ( F ), 0 );
  F.TT := False;
  LF.lfCharSet := Charset;
  DC := GetDC ( 0 );
  try
    EnumFontFamiliesEx ( DC, LF, @Check, Integer ( @F ), 0 );
  finally
    ReleaseDC ( 0, DC );
  end;
  if F.TT then
    Result := F.FontName
  else
    Result := '';
end;


function IsTrueType ( FontName: string; Charset: Byte ): Boolean;
var
  LF: TLogFont;
  TT: Boolean;
  DC: HDC;

  function Check ( const Enum: ENUMLOGFONTEX; const PFD: TNEWTEXTMETRICEXA; FT: DWORD; var TT: Boolean ): Integer; stdcall;
  begin
    TT := ( FT = TRUETYPE_FONTTYPE );
    Result := 0;
  end;
begin
  if FontName = '' then
  begin
    Result := False;
    Exit;
  end;
  FillChar ( LF, SizeOf ( LF ), 0 );
  LF.lfCharSet := CHARSET;
  Move ( FontName [ 1 ], LF.lfFaceName, Length ( FontName ) );
  DC := GetDC ( 0 );
  try
    EnumFontFamiliesEx ( DC, LF, @Check, Integer ( @TT ), 0 );
  finally
    ReleaseDC ( 0, DC );
  end;
  Result := tt;
end;


procedure swp ( var A, B: Integer ); overload;
var
  C: Integer;
begin
  C := A;
  A := B;
  B := C;
end;

procedure swp ( var A, B: Extended ); overload;
var
  C: Extended;
begin
  C := A;
  A := B;
  B := C;
end;


{ TPDFJSFunction }

procedure TPDFJSFunction.SetBody ( const Value: string );
begin
  FBody := Value;
end;

procedure TPDFJSFunction.SetID ( const Value: Integer );
begin
  FID := Value;
end;

procedure TPDFJSFunction.SetName ( const Value: string );
begin
  FName := Value;
end;

procedure TPDFJSFunction.SetParams ( const Value: string );
begin
  FParams := Value;
end;

{ TPDFJSFList }

function TPDFJSFList.Add ( Name, Params, Body: string ): Integer;
var
  F: TPDFJSFunction;
begin
  F := TPDFJSFunction.Create;
  F.FBody := Body;
  F.FName := Name;
  F.FParams := Params;
  Result := FList.Add ( Pointer ( F ) );
end;

procedure TPDFJSFList.Clear;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    TPDFJSFunction ( FList [ i ] ).Free;
  FList.Clear;
end;

function TPDFJSFList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TPDFJSFList.Create;
begin
  FList := TList.Create;
end;

destructor TPDFJSFList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TPDFJSFList.GetItems ( Index: Integer ): TPDFJSFunction;
begin
  Result := TPDFJSFunction ( FList [ Index ] );
end;

procedure TPDFJSFList.SetItems ( Index: Integer;
  const Value: TPDFJSFunction );
begin
  TPDFJSFunction ( FList [ Index ] ).FID := Value.FID;
  TPDFJSFunction ( FList [ Index ] ).FBody := Value.FBody;
  TPDFJSFunction ( FList [ Index ] ).FName := Value.FName;
  TPDFJSFunction ( FList [ Index ] ).FParams := Value.FParams;
end;

end.

