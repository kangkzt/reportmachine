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

unit pdfRC4;

interface
uses windows, classes, sysutils, pdfResources;

type
  TRC4Data = record
    Key: array[0..255] of byte; { current key }
    OrgKey: array[0..255] of byte; { original key }
  end;
procedure RC4Init(var Data: TRC4Data; Key: pointer; Len: integer);
procedure RC4Burn(var Data: TRC4Data);
procedure RC4Crypt(var Data: TRC4Data; InData, OutData: pointer; Len: integer);
procedure RC4Reset(var Data: TRC4Data);

implementation


procedure RC4Init(var Data: TRC4Data; Key: pointer; Len: integer);
var
  xKey: array[0..255] of byte;
  i, j: integer;
  t: byte;
begin
  if (Len <= 0) or (Len > 256) then
    raise Exception.Create(SRC4InvalidKeyLength);
  for i := 0 to 255 do
  begin
    Data.Key[i] := i;
    xKey[i] := PByte(integer(Key) + (i mod Len))^;
  end;
  j := 0;
  for i := 0 to 255 do
  begin
    j := (j + Data.Key[i] + xKey[i]) and $FF;
    t := Data.Key[i];
    Data.Key[i] := Data.Key[j];
    Data.Key[j] := t;
  end;
  Move(Data.Key, Data.OrgKey, 256);
end;

procedure RC4Burn(var Data: TRC4Data);
begin
  FillChar(Data, Sizeof(Data), $FF);
end;

procedure RC4Crypt(var Data: TRC4Data; InData, OutData: pointer; Len: integer);
var
  t, i, j: byte;
  k: integer;
  Ind, Ou: PByteArray;
begin
  ind := InData;
  Ou := OutData;
  i := 0;
  j := 0;
  for k := 0 to Len - 1 do
  begin
    i := Byte(i + 1);
    j := Byte(j + Data.Key[i]);
    t := Data.Key[i];
    Data.Key[i] := Data.Key[j];
    Data.Key[j] := t;
    t := Byte(Data.Key[i] + Data.Key[j]);
    Ou[k] := Ind[k] xor Data.Key[t];
  end;
end;

procedure RC4Reset(var Data: TRC4Data);
begin
  Move(Data.OrgKey, Data.Key, 256);
end;

end.
