{**************************************************}
{                                                  }
{                   llPDFLib                       }
{            Version  3.6,  09.01.2007             }
{      Copyright (c) 2002-2007  llionsoft          }
{             All rights reserved                  }
{            mailto:einfo@llion.net                }
{                                                  }
{**************************************************}

{$i pdf.inc}

unit pdfMD5;

interface
uses windows;

type
  TMD5Byte64 = array[0..63] of Byte;
  TMD5Byte16 = array[0..15] of Byte;
  TMD5Int16 = array[0..15] of DWORD;
  TMD5Int4 = array[0..3] of DWORD;
  TMD5Int2 = array[0..1] of DWORD;
  TMD5Ctx = record
    State: TMD5Int4;
    Count: TMD5Int2;
    Buffer: TMD5Byte64;
    BLen: DWORD;
  end;
  TLongWordBuf = Array[0..15] Of LongWord;
  TByteBuf = Array[0..63] Of Byte;
  TDigestLongWord = Array[0..3] Of Longword;

function md5result(st: AnsiString): AnsiString;
procedure xCalcMD5(Input: Pointer; InputLen: Integer; var Digest: TMD5Byte16);
procedure MD5Init(var Context: TMD5ctx);
procedure MD5Update(var Context: TMD5ctx; const ChkBuf; Len: DWORD);
procedure MD5Final(var Digest: TMD5Byte16; var Context: TMD5ctx);

implementation

procedure Transform(var Accu; const Buf);
Var
  a, b, c, d: LongWord;
  lBuf: TLongWordBuf Absolute Buf;
  lAccu: TDigestLongWord Absolute Accu;

  Function ROL (x: LongWord; n: LongWord): LongWord;
  Begin Result:= (x Shl n) Or (x Shr (32-n)) End;

  Function FF (a,b,c,d,x,s,ac: LongWord): LongWord;
  Begin Result:= ROL (a+x+ac + (b And c Or Not b And d), s) + b End;

  Function GG (a,b,c,d,x,s,ac: LongWord): LongWord;
  Begin Result:= ROL (a+x+ac + (b And d Or c And Not d), s) + b End;

  Function HH (a,b,c,d,x,s,ac: LongWord): LongWord;
  Begin Result:= ROL (a+x+ac + (b Xor c Xor d), s) + b End;

  Function II (a,b,c,d,x,s,ac: LongWord): LongWord;
  Begin Result:= ROL (a+x+ac + (c Xor (b Or Not d)), s) + b End;

Begin
  a:= lAccu[0];
  b:= lAccu[1];
  c:= lAccu[2];
  d:= lAccu[3];

  a:= FF(a,b,c,d, lBuf[ 0],  7, $d76aa478); 
  d:= FF(d,a,b,c, lBuf[ 1], 12, $e8c7b756); 
  c:= FF(c,d,a,b, lBuf[ 2], 17, $242070db); 
  b:= FF(b,c,d,a, lBuf[ 3], 22, $c1bdceee); 
  a:= FF(a,b,c,d, lBuf[ 4],  7, $f57c0faf); 
  d:= FF(d,a,b,c, lBuf[ 5], 12, $4787c62a); 
  c:= FF(c,d,a,b, lBuf[ 6], 17, $a8304613); 
  b:= FF(b,c,d,a, lBuf[ 7], 22, $fd469501); 
  a:= FF(a,b,c,d, lBuf[ 8],  7, $698098d8); 
  d:= FF(d,a,b,c, lBuf[ 9], 12, $8b44f7af); 
  c:= FF(c,d,a,b, lBuf[10], 17, $ffff5bb1); 
  b:= FF(b,c,d,a, lBuf[11], 22, $895cd7be); 
  a:= FF(a,b,c,d, lBuf[12],  7, $6b901122);
  d:= FF(d,a,b,c, lBuf[13], 12, $fd987193); 
  c:= FF(c,d,a,b, lBuf[14], 17, $a679438e); 
  b:= FF(b,c,d,a, lBuf[15], 22, $49b40821); 

  a:= GG(a,b,c,d, lBuf[ 1],  5, $f61e2562);
  d:= GG(d,a,b,c, lBuf[ 6],  9, $c040b340); 
  c:= GG(c,d,a,b, lBuf[11], 14, $265e5a51); 
  b:= GG(b,c,d,a, lBuf[ 0], 20, $e9b6c7aa); 
  a:= GG(a,b,c,d, lBuf[ 5],  5, $d62f105d); 
  d:= GG(d,a,b,c, lBuf[10],  9, $02441453); 
  c:= GG(c,d,a,b, lBuf[15], 14, $d8a1e681); 
  b:= GG(b,c,d,a, lBuf[ 4], 20, $e7d3fbc8); 
  a:= GG(a,b,c,d, lBuf[ 9],  5, $21e1cde6); 
  d:= GG(d,a,b,c, lBuf[14],  9, $c33707d6); 
  c:= GG(c,d,a,b, lBuf[ 3], 14, $f4d50d87); 
  b:= GG(b,c,d,a, lBuf[ 8], 20, $455a14ed); 
  a:= GG(a,b,c,d, lBuf[13],  5, $a9e3e905); 
  d:= GG(d,a,b,c, lBuf[ 2],  9, $fcefa3f8); 
  c:= GG(c,d,a,b, lBuf[ 7], 14, $676f02d9); 
  b:= GG(b,c,d,a, lBuf[12], 20, $8d2a4c8a); 

  a:= HH(a,b,c,d, lBuf[ 5],  4, $fffa3942); 
  d:= HH(d,a,b,c, lBuf[ 8], 11, $8771f681); 
  c:= HH(c,d,a,b, lBuf[11], 16, $6d9d6122); 
  b:= HH(b,c,d,a, lBuf[14], 23, $fde5380c); 
  a:= HH(a,b,c,d, lBuf[ 1],  4, $a4beea44); 
  d:= HH(d,a,b,c, lBuf[ 4], 11, $4bdecfa9); 
  c:= HH(c,d,a,b, lBuf[ 7], 16, $f6bb4b60); 
  b:= HH(b,c,d,a, lBuf[10], 23, $bebfbc70);
  a:= HH(a,b,c,d, lBuf[13],  4, $289b7ec6); 
  d:= HH(d,a,b,c, lBuf[ 0], 11, $eaa127fa); 
  c:= HH(c,d,a,b, lBuf[ 3], 16, $d4ef3085); 
  b:= HH(b,c,d,a, lBuf[ 6], 23, $04881d05); 
  a:= HH(a,b,c,d, lBuf[ 9],  4, $d9d4d039); 
  d:= HH(d,a,b,c, lBuf[12], 11, $e6db99e5); 
  c:= HH(c,d,a,b, lBuf[15], 16, $1fa27cf8); 
  b:= HH(b,c,d,a, lBuf[ 2], 23, $c4ac5665);

  a:= II(a,b,c,d, lBuf[ 0],  6, $f4292244); 
  d:= II(d,a,b,c, lBuf[ 7], 10, $432aff97); 
  c:= II(c,d,a,b, lBuf[14], 15, $ab9423a7); 
  b:= II(b,c,d,a, lBuf[ 5], 21, $fc93a039);
  a:= II(a,b,c,d, lBuf[12],  6, $655b59c3); 
  d:= II(d,a,b,c, lBuf[ 3], 10, $8f0ccc92); 
  c:= II(c,d,a,b, lBuf[10], 15, $ffeff47d); 
  b:= II(b,c,d,a, lBuf[ 1], 21, $85845dd1); 
  a:= II(a,b,c,d, lBuf[ 8],  6, $6fa87e4f); 
  d:= II(d,a,b,c, lBuf[15], 10, $fe2ce6e0); 
  c:= II(c,d,a,b, lBuf[ 6], 15, $a3014314); 
  b:= II(b,c,d,a, lBuf[13], 21, $4e0811a1); 
  a:= II(a,b,c,d, lBuf[ 4],  6, $f7537e82); 
  d:= II(d,a,b,c, lBuf[11], 10, $bd3af235);
  c:= II(c,d,a,b, lBuf[ 2], 15, $2ad7d2bb);
  b:= II(b,c,d,a, lBuf[ 9], 21, $eb86d391);

  Inc(lAccu[0], a);
  Inc(lAccu[1], b);
  Inc(lAccu[2], c);
  Inc(lAccu[3], d);
end;

procedure MD5Init(var Context: TMD5ctx);
begin
  Context.BLen := 0;
  Context.Count[0] := 0;
  Context.Count[1] := 0;
  Context.State[0] := $67452301;
  Context.State[1] := $EFCDAB89;
  Context.State[2] := $98BADCFE;
  Context.State[3] := $10325476;
end;

procedure MD5Update(var Context: TMD5ctx; const ChkBuf; Len: DWORD);
var
  BufPtr: ^Byte;
  Left: Cardinal;
begin
  if Context.Count[0] + DWORD(Integer(Len) shl 3) < Context.Count[0] then Inc(Context.Count[1]);
  Inc(Context.Count[0], Integer(Len) shl 3);
  Inc(Context.Count[1], Integer(Len) shr 29);

  BufPtr := @ChkBuf;
  if Context.bLen > 0 then
  begin
    Left := 64 - Context.bLen; if Left > Len then Left := Len;
    Move(BufPtr^, Context.Buffer[Context.bLen], Left);
    Inc(Context.bLen, Left); Inc(BufPtr, Left);
    if Context.bLen < 64 then Exit;
    Transform(Context.State, Context.Buffer);
    Context.bLen := 0;
    Dec(Len, Left)
  end;
  while Len >= 64 do
  begin
    Transform(Context.State, BufPtr^);
    Inc(BufPtr, 64);
    Dec(Len, 64)
  end;
  if Len > 0 then begin
    Context.bLen := Len;
    Move(BufPtr^, Context.Buffer[0], Context.bLen)
  end
end;

procedure MD5Final(var Digest: TMD5Byte16; var Context: TMD5ctx);
var
  WorkBuf: TMD5Byte64;
  WorkLen: Cardinal;
begin
  Digest := TMD5Byte16(Context.State);
  Move(Context.Buffer, WorkBuf, Context.bLen); {make copy of buffer}
  {pad out to block of form (0..55, BitLo, BitHi)}
  WorkBuf[Context.bLen] := $80;
  WorkLen := Context.bLen + 1;
  if WorkLen > 56 then begin
    FillChar(WorkBuf[WorkLen], 64 - WorkLen, 0);
    TransForm(Digest, WorkBuf);
    WorkLen := 0
  end;
  FillChar(WorkBuf[WorkLen], 56 - WorkLen, 0);
  TMD5Int16(WorkBuf)[14] := Context.Count[0];
  TMD5Int16(WorkBuf)[15] := Context.Count[1];
  Transform(Digest, WorkBuf);
  FillChar(Context, SizeOf(Context), 0);
end;

procedure xCalcMD5(Input: Pointer; InputLen: Integer; var Digest: TMD5Byte16);
var
  Context: TMD5CTX;
begin
  MD5Init(Context);
  MD5Update(Context, Input^, InputLen);
  MD5Final(Digest, Context);
end;

function DigestToStr(const D: TMD5Byte16): AnsiString;
const
  HexChars: AnsiString = '0123456789abcdef';
var
  I: Integer;
begin
  SetLength(Result, 32);
  for I := 0 to 15 do
  begin
    Result[1 + I * 2] := HexChars[1 + D[I] shr 4];
    Result[2 + I * 2] := HexChars[1 + D[I] and 15];
  end;
end;

function md5result(st: AnsiString): AnsiString;
var
  Digest: TMD5Byte16;
begin
  xCalcMD5(@st[1], length(st), digest);
  result := digesttostr(digest);
end;


end.
