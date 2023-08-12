unit psCodeStudio;

interface

uses  Windows, Classes, Math, SysUtils, VCL.Graphics,
      psTypes, psCodeFNLite, psCode2D, psCodeExceptions, psCodeGS1;

{$I psBarcode.inc}

const constDataMatrixDefaultECI = -1;

type


  TpsDataMatrixSpec = record
    No        : integer;        // Number of data regions
    DW        : integer;        // total data codewords
    EW        : integer;        // total error correction codewords
    Inter     : integer;        // Interleaved blocks
    Size      : TpsSize2D;      // Sizes in pixels
    Capacity  : TpsCapacity2D;  // Data capacity
  end;

  TdmMatrixParams = record
    CellX, CellY          : Integer; // number of cells in X and Y direction
    CellSizeX, CellSizeY  : Integer; // cell size in pixels (include control pixels)
    TotalMatrixW,
    TotalMatrixH          : Integer; // matrix size (width control pixels)
    TotalDW               : Integer; // max. datawords
    TotalEW               : Integer; // count of error correction words
    Capacity              : TpsCapacity2D;
    Inter                 : Integer;
  end;

  TpsDataMatrixEncoding = ( dmeAutomatic, dmeASCII, dmeBinary, dmeEDIFACT,
      dmeText, dmeX12, dmeC40);

  TpsDataMatrixVersion = (
      psDMAutomatic,
      psDM10x10,  psDM12x12, psDM14x14, psDM16x16,   psDM18x18,   psDM20x20,
      psDM22x22,  psDM24x24, psDM26x26, psDM32x32,   psDM36x36,   psDM40x40,
      psDM44x44,  psDM48x48, psDM52x52, psDM64x64,   psDM72x72,   psDM80x80,
      psDM88x88,  psDM96x96, ps104x104, psDM120x120, psDM132x132, psDM144x144,
      psDM8x18,   psDM8x32,  psDM12x26, psDM12x36,   psDM16x36,   psDm16x48
  );

  TpsDataMatrixParams=class(TPersistent)
  private
      FBarcode : TComponent;
      FEncoding: TpsDataMatrixEncoding;
      FVersion : TpsDataMatrixVersion;
      procedure UpdateBarcode;
      procedure SetEncoding(const Value: TpsDataMatrixEncoding);
      procedure SetVersion(const Value: TpsDataMatrixVersion);
  public
      constructor CreateBarcode(AOwner:TComponent);
      procedure   Assign(Source:TPersistent); override;
  published
      property  Encoding : TpsDataMatrixEncoding read FEncoding write SetEncoding;
      property  Version  : TpsDataMatrixVersion read FVersion write SetVersion;
  end;

  TpsQrEccLevel= (QrEccLevelL, QrEccLevelM, QrEccLevelQ, QrEccLevelH);
  TpsQrMode    = (QrAutomatic, QrNumeric, QrAlphanumeric, QrBytes, QrKanji);


    TpsQRParams = class(TPersistent)
    private
      FBarcode : TComponent;
      FMode: TpsQrMode;
      FMicroQR: Boolean;
      FEccLevel: TpsQrEccLevel;
      FVersion: Integer;
      FMask: Integer;
      FFileName     : String;
      FCheckSum     : Integer;
      {$ifdef PSOFT_SETUP}
          FDrawData: Boolean;
          FDrawMask: Boolean;
      {$endif}
      procedure UpdateBarcode;
      procedure SetEccLevel(const Value: TpsQrEccLevel);
      procedure SetMicroQR(const Value: Boolean);
      procedure SetMode(const Value: TpsQrMode);
      procedure SetVersion(const Value: Integer);
      procedure SetMask(const Value: Integer);
      procedure SetFileName(const Value: String);
      procedure SetChecksum(const Value: Integer);
    protected
    public
      FUsedVersion  : Integer;
      FUsedMask     : Integer;
      FUsedMicroQR  : Boolean;
      FUsedMode     : TpsQRMode;
      FUsedEccLevel : TpsQrEccLevel;

      constructor CreateBarcode(ParentEAN:TComponent);
      procedure   Assign(Source:TPersistent); override;
    published
      property EccLevel:TpsQrEccLevel read FEccLevel write SetEccLevel;
      property Mode:TpsQrMode read FMode write SetMode;
      property MicroQR:Boolean read FMicroQR write SetMicroQR;
      property Version:Integer read FVersion write SetVersion;
      property Mask:Integer read FMask write SetMask;
      property FileName:String read FFileName write SetFileName;
      property Checksum:Integer read FChecksum write SetChecksum;
      {$ifdef PSOFT_SETUP}
          property DrawData:Boolean read FDrawData write FDrawData stored False;
          property DrawMask:Boolean read FDrawMask write FDrawMask Stored False;
      {$endif}
      property UsedVersion:Integer read FUsedVersion stored False;
      property UsedMask:Integer read FUsedMask stored False;
      property UsedMicroQR:Boolean read FUsedMicroQR stored False;
      property UsedMode:TpsQRMode read FUsedMode stored False;
      property UsedECCLevel:TpsQrEccLevel read FUsedEccLevel stored False;
    end;

    TpsAztecVersion = Integer;
    TpsAztecECC     = (azECC5, azECC10, azECC15, azECC20, azECC23, azECC25,
        azECC30, azECC35, azECC40, azECC45, azECC50, azECC55, azECC60, azECC65,
        azECC70, azECC75, azECC80, azECC85, azECC90);

    TpsAztecParams = class(TPersistent)
    private
      FBarcode : TComponent;
      FCompact: Boolean;
      FVersion: TpsAztecVersion;
      FErrorLevel: TpsAztecECC;
      procedure UpdateBarcode;
      procedure SetCompact(const Value: Boolean);
      procedure SetVersion(const Value: TpsAztecVersion);
      procedure SetErrorLevel(const Value: TpsAztecECC);
    public
      constructor CreateBarcode(ParentEAN:TComponent);
      procedure   Assign(Source:TPersistent); override;
    published
      property Compact : Boolean read FCompact write SetCompact default False;
      property Version : TpsAztecVersion read FVersion write SetVersion;
      property ErrorLevel:TpsAztecECC read FErrorLevel write SetErrorLevel default azECC23;
    end;


  function  dmGetMatrixParams(version:TpsDataMatrixVersion;
      TotalEncoded:integer):TdmMatrixParams;

  // toto potom prehodit do implementation sekcie
  function  dmMapToFinal(dmParams:TdmMatrixParams; x,y:Integer):TPoint;

  //  function  psPaintDataMatrix(Value:String; bc:IpsBarcodeInterface; C:TCanvas; R:TRect):Integer;
  function  psDataMatrixGetPixels(BarcodeObject:TComponent):Integer;


  // --------------------------------------------------------------------------
  // main function for QR symbology/Micro QR ...
  // fills FBars variable of TpsCustomBarcode, for internal use only
  function  psQRCodeGetPixels(Barcode:TComponent):Integer;

  // convert ECC level to human readable text
  function  qrECCDescription(ecc:TpsQrEccLevel):string;
  function  qrModeDescription(ecc:TpsQrMode):string;





implementation

uses psBarcodeComp;

type

  TpsByteArray = array of byte;

  TpsDataMatrixRSItem = record
    ErrCount : Integer;
    Coef     : Array[1..68] of Integer;
  end;


type
  TpsReedSolomon = class
  private
    gfPoly  : Integer;
    symsize : Integer;
    logmod  : Integer;
    rlen    : Integer;
    _index  : Integer;
    log,
    alog,
    rspoly  : array of integer;
  protected
  public
    constructor Create(poly:integer);
    procedure   GenerateGF(poly:integer);
    procedure   GeneratePoly(nsym,index:integer);
    procedure   Encode(const data:array of byte; InLen:Integer;
  var outData:array of byte; OutLen:integer);  overload;
    procedure   Encode(const data:array of byte;
        var outData:array of byte);  overload;
  end;

var dmReedSolomon:TpsReedSolomon;
    idx_color : Integer;

{ TpsDataMatrix }

type

  TpsDataMatrix = class(TPersistent)
  private
    FEncoding           : TpsDataMatrixEncoding;
    FVersion            : TpsDataMatrixVersion;
    FECI                : Integer;

    FCurrentEncoding    : TpsDataMatrixEncoding;
    DataSize,ErrorSize  : Integer;
    FSegmentIndex       : Integer;
    FSegmentCount       : Integer;
    FFileID             : String;
    FNC1                : TpsFNC1Type;

    procedure SetEncoding(const Value: TpsDataMatrixEncoding);
    procedure SetVersion(const Value: TpsDataMatrixVersion);
    procedure SwitchToASCII;
    procedure EncodeAutomatic(s: String);
    procedure EncodeText(Enc:TpsDataMatrixEncoding; const s: String);
    procedure Encode256(const s:String; EndOfSymbol:Boolean);
    procedure EncodeASCII(const s: String);
    procedure EncodeEdiFact(const s: String);
    procedure AddPad(addFrom, totalLen:Integer);

    function  AppendCheck(id:integer): Boolean;

    procedure ECCInterleave(Bytes, Blocks, RSBlock: Integer);
    procedure SetECI(const Value: Integer);
    procedure SetSegmentIndex(const Value: Integer);
    procedure SetSegmentCount(const Value: Integer);
    procedure SetFileID(const Value: String);
  protected
    procedure EncodeECI(eciValue:Integer);
    procedure AppendEncode;
  public
    mp        : TdmMatrixParams;
    dw, err   : TpsArray;

    function  Encode(s:String):String;
    function  CreateMatrix(s:String):boolean;
    procedure dmGrid(d2: TpsMatrix);
    property  Encoding : TpsDataMatrixEncoding read FEncoding write SetEncoding;
    property  Version  : TpsDataMatrixVersion read FVersion write SetVersion;
    property  ECI      : Integer  Read FECI write SetECI;
    property  SegmentIndex:Integer read FSegmentIndex write SetSegmentIndex;
    property  SegmentCount:Integer read FSegmentCount write SetSegmentCount;
    property  FileID      : String read FFileID write SetFileID;
  published
  end;

const
  // ord0      : Integer=Ord('0');
  tblC40    : String = #0#0#0+' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  tblText   : String = #0#0#0+' 0123456789abcdefghijklmnopqrstuvwxyz';
  tblShift1 : String = #0#1#2#3#4#5#6#7#8#9#10#11#12#13#14#15#16#17#18#19#20
    + #21#22#23#24#24#26#27#28#29#30#31;
  tblShift2 : String = #33#34#35#36#37#38#39#40#41#42#43#44#45#46#47#59#59#60#61#62#63#64#91#92#93#94#95;
  tblShift3C40 : String = #96+'abcdefghijklmnopqrstuvwxyz'#123#124#125#126#127;
  tblShift3Text : String = #96+'ABCDEFGHIJKLMNOPQRSTUVWXYZ'#123#124#125#126#127;
  tblX12    : String = #13'*> 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';

  dm_PAD          = 129;
  dm_NumberPairs  = 130;
  dm_LatchC40     = 230;
  dm_Latch256     = 231;
  dm_FNC1         = 232;
  dm_Append       = 233;
  dm_ReaderPrg    = 234;
  dm_Extended     = 235;
  dm_Macro05      = 236;
  dm_Macro_06     = 237;
  dm_LatchX12     = 238;
  dm_LatchText    = 239;
  dm_LatchEdiFact = 240;
  dm_ECI          = 241;

  dm_ToASCII        = 254;
  dm_ToASCIIFromEDI = $7C;

  tblDataMatrixSpecs:array[TpsDataMatrixVersion] of TpsDataMatrixSpec=(
    { psDMAutomatic} (No: 0; DW:0;    EW:0;  Inter: 0;
        Size    : (DataW : 0; DataH : 0;  MatrixW : 0; MatrixH : 0);
        Capacity:( Numeric:0;    Alphanumeric: 0;   Binary:0)),
    { psDM10x10    } (No: 1; DW:3;    EW:5;  Inter :1;
        Size    : (DataW : 8; DataH : 8;  MatrixW : 8; MatrixH : 8);
        Capacity:( Numeric:6;    Alphanumeric: 3;   Binary:1)),
    { psDM12x12    } (No: 1; DW:5;    EW:7;  Inter :1;
        Size    : (DataW : 10; DataH : 10;  MatrixW : 10; MatrixH : 10);
        Capacity:( Numeric:10;   Alphanumeric:6;    Binary:3)),
    { psDM14x14    } (No: 1; DW:8;    EW:10; Inter :1;
        Size    : (DataW : 12; DataH : 12;  MatrixW : 12; MatrixH : 12);
        Capacity:( Numeric:16;   Alphanumeric:10;   Binary:6)),
    { psDM16x16    } (No: 1; DW:12;   EW:12; Inter :1;
        Size    : (DataW : 14; DataH : 14;  MatrixW : 14; MatrixH : 14);
        Capacity:( Numeric:24;   Alphanumeric:16;   Binary:10)),
    { psDM18x18    } (No: 1; DW:18;   EW:14; Inter :1;
        Size    : (DataW : 16; DataH : 16;  MatrixW : 16; MatrixH : 16);
        Capacity:( Numeric:36;   Alphanumeric:25;   Binary:16)),
    { psDM20x20    } (No: 1; DW:22;   EW:18; Inter :1;
        Size    : (DataW : 18; DataH : 18;  MatrixW : 18; MatrixH : 18);
        Capacity:( Numeric:44;   Alphanumeric:31;   Binary:20)),
    { psDM22x22    } (No: 1; DW:30;   EW:20; Inter :1;
        Size    : (DataW : 20; DataH : 20;  MatrixW : 20; MatrixH : 20);
        Capacity:( Numeric:60;   Alphanumeric:43;   Binary:28)),
    { psDM24x24    } (No: 1; DW:36;   EW:24; Inter :1;
        Size    : (DataW : 22; DataH : 22;  MatrixW : 22; MatrixH : 22);
        Capacity:( Numeric:72;   Alphanumeric:52;   Binary:34)),
    { psDM26x26    } (No: 1; DW:44;   EW:28; Inter :1;
        Size    : (DataW : 24; DataH : 24;  MatrixW : 24; MatrixH : 24);
        Capacity:( Numeric:88;   Alphanumeric:64;   Binary:42)),
    { psDM32x32    } (No: 4; DW:62;   EW:36; Inter :1;
        Size    : (DataW : 14; DataH : 14;  MatrixW : 28; MatrixH : 28);
        Capacity:( Numeric:124;  Alphanumeric:91;   Binary:60)),
    { psDM36x36    } (No: 4; DW:86;   EW:42; Inter :1;
        Size    : (DataW : 16; DataH : 16;  MatrixW : 32; MatrixH : 32);
        Capacity:( Numeric:172;  Alphanumeric:127;  Binary:84)),
    { psDM40x40    } (No: 4; DW:114;  EW:48; Inter :1;
        Size    : (DataW : 18; DataH : 18;  MatrixW : 36; MatrixH : 36);
        Capacity:( Numeric:228;  Alphanumeric:169;  Binary:112)),
    { psDM44x44    } (No: 4; DW:144;  EW:56; Inter :1;
        Size    : (DataW : 20; DataH : 20;  MatrixW : 40; MatrixH : 40);
        Capacity:( Numeric:288;  Alphanumeric:214;  Binary:142)),
    { psDM48x48    } (No: 4; DW:174;  EW:68; Inter :1;
        Size    : (DataW : 22; DataH : 22;  MatrixW : 44; MatrixH : 44);
        Capacity:( Numeric:348;  Alphanumeric:259;  Binary:172)),
    { psDM52x52    } (No: 4; DW:204;  EW:84; Inter :2;
        Size    : (DataW : 24; DataH : 24;  MatrixW : 48; MatrixH : 48);
        Capacity:( Numeric:408;  Alphanumeric:304;  Binary:202)),
    { psDM64x64    } (No:16; DW:280;  EW:112; Inter :2;
        Size    : (DataW : 14; DataH : 14;  MatrixW : 56; MatrixH : 56);
        Capacity:( Numeric:560;  Alphanumeric:418;  Binary:277)),
    { psDM72x72    } (No:16; DW:368;  EW:144; Inter :4;
        Size    : (DataW : 16; DataH : 16;  MatrixW : 64; MatrixH : 64);
        Capacity:( Numeric:736;  Alphanumeric:550;  Binary:365)),
    { psDM80x80    } (No:16; DW:456;  EW:192; Inter :4;
        Size    : (DataW : 18; DataH : 18;  MatrixW : 72; MatrixH : 72);
        Capacity:( Numeric:912;  Alphanumeric:682;  Binary:453)),
    { psdDM88x88   } (No:16; DW:576;  EW:224; Inter :4;
        Size    : (DataW : 20; DataH : 20;  MatrixW : 80; MatrixH : 80);
        Capacity:( Numeric:1152; Alphanumeric:862;  Binary:573)),
    { psDM96x96    } (No:16; DW:696;  EW:272; Inter :4;
        Size    : (DataW : 22; DataH : 22;  MatrixW : 88; MatrixH : 88);
        Capacity:( Numeric:1392; Alphanumeric:1042; Binary:693)),
    { ps104x104    } (No:16; DW:816;  EW:336; Inter :6;
        Size    : (DataW : 24; DataH : 24;  MatrixW : 96; MatrixH : 96);
        Capacity:( Numeric:1632; Alphanumeric:1222; Binary:813)),
    { psDM120x120  } (No:36; DW:1050; EW:408; Inter :6;
        Size    : (DataW : 18; DataH : 18;  MatrixW : 108; MatrixH : 108);
        Capacity:( Numeric:2100; Alphanumeric:1573; Binary:1047)),
    { psDM132x132  } (No:36; DW:1304; EW:496; Inter :8;
        Size    : (DataW : 20; DataH : 20;  MatrixW : 120; MatrixH : 120);
        Capacity:( Numeric:2608; Alphanumeric:1954; Binary:1301)),
    { psDM144x144  } (No:36; DW:1558; EW:620; Inter :10;
        Size    : (DataW : 22; DataH : 22;  MatrixW : 132; MatrixH : 132);
        Capacity:( Numeric:3116; Alphanumeric:2335; Binary:1556)),

    { psDM8x18     } (No:1;  DW:5;    EW:5;   Inter :1;
        Size    : (DataW : 16; DataH : 6;  MatrixW : 16; MatrixH : 6);
        Capacity:( Numeric:10;   Alphanumeric:6;   Binary:3)),
    { psDM8x32     } (No:2;  DW:10;   EW:11;  Inter :1;
        Size    : (DataW : 14; DataH : 6;  MatrixW :28; MatrixH : 6);
        Capacity:( Numeric:20;   Alphanumeric:13;  Binary:8)),
    { psDM12x26    } (No:1;  DW:16;   EW:14;  Inter :1;
        Size    : (DataW : 24; DataH : 10;  MatrixW : 24; MatrixH : 10);
        Capacity:( Numeric:32;   Alphanumeric:22;  Binary:14)),
    { psDM12x36    } (No:2;  DW:22;   EW:18;  Inter :1;
        Size    : (DataW : 16; DataH : 10;  MatrixW : 32; MatrixH : 10);
        Capacity:( Numeric:44;   Alphanumeric:31;  Binary:20)),
    { psDM16x36    } (No:2;  DW:32;   EW:24;  Inter :1;
        Size    : (DataW : 16; DataH : 14;  MatrixW : 32; MatrixH : 14);
        Capacity:( Numeric:64;   Alphanumeric:46;  Binary:30)),
    { psDm16x48    } (No:2;  DW:49;   EW:28;  Inter :1;
        Size    : (DataW : 22; DataH : 14;  MatrixW : 44; MatrixH : 14);
        Capacity:( Numeric:98;   Alphanumeric:72;  Binary:47))
  );


procedure TpsDataMatrix.dmGrid(d2:TpsMatrix);
var row,col   : integer;
    bits      : array[0..7] of Integer;
    idx       : integer;
    outMatrix : array of Integer;
    NR,NC     : Integer;

  procedure initBits;
  var x,value:integer;
  begin
    value:=dw.Data[idx];
    for x:=0 to 7 do begin
      bits[x] := value and $1;
      value   := value shr 1;
    end;
    Inc(idx);
  end;

  function  getPixel(const row,col:Integer):integer;
  begin
    result:=outMatrix[row*NC+col];
  end;

  procedure setPixel(const row,col,value:Integer);
  begin
    outMatrix[row*NC+col] := value;
  end;

  procedure dmBit(row,col:Integer; value:Integer); overload;
  begin
    if row<0 then begin
      Inc(row,NR);
      Inc(col, 4-((NR+4) mod 8));
    end;
    if col<0 then begin
      Inc(col, NC);
      Inc(row, 4-((NC+4) mod 8));
    end;
    if idx=idx_color then Inc(Value,2);
    setPixel(row, col, value);
  end;

  procedure dmBit(row,col:Integer; value:Boolean); overload;
  begin
    if Value then dmBit(row,col,1)
    else          dmBit(row,col,0);
  end;

  procedure dmBlock;
  begin
    initBits;
    dmBit(row-2, col-2, bits[7]);
    dmBit(row-2, col-1, bits[6]);
    dmBit(row-1, col-2, bits[5]);
    dmBit(row-1, col-1, bits[4]);
    dmBit(row-1, col-0, bits[3]);
    dmBit(row-0, col-2, bits[2]);
    dmBit(row-0, col-1, bits[1]);
    dmBit(row-0, col-0, bits[0]);
  end;

  procedure dmCornerA;
  begin
    initBits;
    dmBit(NR-1,     0,  bits[7]);
    dmBit(NR-1,     1,  bits[6]);
    dmBit(NR-1,     2,  bits[5]);
    dmBit(   0, NC -2,  bits[4]);
    dmBit(   0, NC -1,  bits[3]);
    dmBit(   1, NC -1,  bits[2]);
    dmBit(   2, NC -1,  bits[1]);
    dmBit(   3, NC -1,  bits[0]);
  end;

  procedure dmCornerB;
  begin
    initBits;
    dmBit(NR-3,     0,  bits[7]);
    dmBit(NR-2,     0,  bits[6]);
    dmBit(NR-1,     0,  bits[5]);
    dmBit(   0, NC -4,  bits[4]);
    dmBit(   0, NC -3,  bits[3]);
    dmBit(   0, NC -2,  bits[2]);
    dmBit(   0, NC -1,  bits[1]);
    dmBit(   1, NC -1,  bits[0]);
  end;

  procedure dmCornerC;
  begin
    initBits;
    dmBit(NR-3,     0,  bits[7]);
    dmBit(NR-2,     0,  bits[6]);
    dmBit(NR-1,     0,  bits[5]);
    dmBit(   0, NC -2,  bits[4]);
    dmBit(   0, NC -1,  bits[3]);
    dmBit(   1, NC -1,  bits[2]);
    dmBit(   2, NC -1,  bits[1]);
    dmBit(   3, NC -1,  bits[0]);
  end;

  procedure dmCornerD;
  begin
    initBits;
    dmBit(NR-1,     0,  bits[7]);
    dmBit(NR-1, NC -1,  bits[6]);
    dmBit(   0, NC -3,  bits[5]);
    dmBit(   0, NC -2,  bits[4]);
    dmBit(   0, NC -1,  bits[3]);
    dmBit(   1, NC -3,  bits[2]);
    dmBit(   1, NC -2,  bits[1]);
    dmBit(   1, NC -1,  bits[0]);
  end;

  procedure dmBuildFinalMatrix;
  var idxIn, x,y  : Integer;
  begin
    idxIn := 0;
    d2.Clear;
    for y:=0 to mp.TotalMatrixH-1 do begin
      for x:=0 to mp.TotalMatrixW-1 do begin
        // left or bottom full line
        if ((x mod mp.CellSizeX)=0) or ((y mod mp.CellSizeY)=mp.CellSizeY-1) then
          d2.PutChar('1')
        else
        // right or top dotted line
        if ((x mod mp.CellSizeX)=mp.CellSizeX-1) or ((y mod mp.CellSizeY)=0) then begin
          if ((x mod 2)=0) or ((y mod 2)=1) then d2.PutChar('1')
          else                                   d2.PutChar('0')
        end else begin
          d2.PutChar(Chr(Ord0+OutMatrix[idxIn]));
          Inc(idxIn);
        end;
      end;
    end;
  end;

begin
  d2.InitializeWH(mp.TotalMatrixW, mp.TotalMatrixH);
  NC := mp.TotalMatrixW-2*mp.CellX;
  NR := mp.TotalMatrixH-2*mp.CellY;

  // SetLength(OutMatrix, NC*NR);
  SetLength(OutMatrix, mp.TotalMatrixW*mp.TotalMatrixH);
  for idx:=0 to Length(outMatrix)-1 {NC*NR-1} do outMatrix[idx]:=-1;

  // starting point
  idx := 0;
  row := 4;
  col := 0;

  repeat
    if (row=NR  ) and (col=0) then dmCornerA;
    if (row=NR-2) and (col=0) and ((NC mod 4)<>0) then dmCornerB;
    if (row=NR-2) and (col=0) and ((NC mod 8)=4)  then dmCornerC;
    if (row=NR+4) and (col=2) and ((NC mod 8)=0)  then dmCornerD;

    // move up right
    repeat
      if (row<NR) and (col>=0) and (getPixel(row,col)=-1) then
          dmBlock;
      Dec(row,2);
      Inc(col,2);
    until not ((row>=0) and (col<NC));

    // right down
    Inc(row,1);
    Inc(col,3);

    // down left
    repeat
      if (row>=0) and (col<NC) and (getPixel(row,col)=-1) then
          dmBlock;
      Inc(row,2);
      Dec(col,2);
    until not((row<NR) and (col>=0));

    Inc(row,3);
    Inc(col,1);
  until not ((row<NR) or (col<NC));

  if getPixel(NC-1,NR-1)=-1 then begin
       setPixel(NC-1, NR-1, 1);
       setPixel(NC-2, NR-2, 1);
       setPixel(NC-2, NR-1, 0);
       setPixel(NC-1, NR-2, 0);
  end;

  dmBuildFinalMatrix;

end;

function  dmGetMatrixParams(version:TpsDataMatrixVersion;
  TotalEncoded:integer):TdmMatrixParams;
var Spec  : TpsDataMatrixSpec;
    i     : TpsDataMatrixVersion;
begin
    if version=psDMAutomatic then
      for i:=Low(TpsDataMatrixVersion) to High(TpsDataMatrixVersion) do
        if tblDataMatrixSpecs[i].DW >= TotalEncoded then begin
          version := i;
          break;
        end;

    Spec  := tblDataMatrixSpecs[version];
    if Spec.Size.DataW>0 then
      Result.CellX := Spec.Size.MatrixW div (Spec.Size.DataW);
    if Spec.Size.DataH>0 then
      Result.CellY := Spec.Size.MatrixH div (Spec.Size.DataH);

    Result.Inter        := Spec.Inter;
    Result.CellSizeX    := Spec.Size.DataW + 2;
    Result.CellSizeY    := Spec.Size.DataH + 2;
    Result.TotalMatrixW := Result.CellX * Result.CellSizeX;
    Result.TotalMatrixH := Result.CellY * Result.CellSizeY;
    Result.TotalDW      := Spec.DW;
    Result.TotalEW      := Spec.EW;
    Result.Capacity     := Spec.Capacity;
end;

procedure TpsDataMatrix.EncodeASCII(const s:String);
var i,len, val : integer;
    curr       : Integer;
    w          : Char;
begin
  len     := length(s);

  SwitchToASCII;
  i:=1;
  while (i<=len) do begin
    w    := s[i];
    curr := Ord(w);
    // Pair of numerical value
    if IsDigit(w) and (i<len) then
      if IsDigit(s[i+1]) then begin
        val:=10*(curr-Ord0)+Ord(s[i+1])-Ord0;
        psDataPut(dw, 130+val);
        Inc(i,2);
        Continue;
      end;
    // ASCII
    if curr<=127 then
      psDataPut(dw, curr+1)
    else begin
      //Extended ASCII
      psDataPut(dw, dm_Extended);
      psDataPut(dw, curr-127);
    end;
    Inc(i);
  end;
end;

procedure TpsDataMatrix.EncodeText(Enc:TpsDataMatrixEncoding; const s:String);
var i, idx, len   : integer;
    basicSet,Shift1,Shift2,Shift3 : String;
    buf           : array[0..2] of byte;
    buf_idx       : integer;
    currvalue     : Char;

    procedure StoreText(val:byte);
    var tmp:integer;
    begin
      buf[buf_idx] := val;
      if buf_idx=2 then begin
        if FCurrentEncoding<>Enc then begin
          if FCurrentEncoding=dmeEdiFact then
            psDataPut(dw, dm_ToASCIIFromEDI)
          else
            psDataPut(dw, dm_ToASCII);
          case Enc of
            dmeC40 : psDataPut(dw, dm_LatchC40);
            dmeText: psDataPut(dw, dm_LatchText);
            dmeX12 : psDataPut(dw, dm_LatchX12);
          end;
          FCurrentEncoding := Enc;
        end;

        tmp     := 1600*buf[0]+40*buf[1]+buf[2]+1;
        psDataPut(dw, tmp shr 8);
        psDataPut(dw, tmp and $FF);
        buf_idx := 0;
      end else
        Inc(buf_idx);
    end;

begin

  buf_idx:= 0;

  case enc of
    dmeC40 : begin
        basicSet := tblC40;
        Shift1   := tblShift1;
        Shift2   := tblShift2;
        Shift3   := tblShift3C40;
      end;
    dmeText: begin
        basicSet := tblText;
        Shift1   := tblShift1;
        Shift2   := tblShift2;
        Shift3   := tblShift3Text;
      end;
    dmeX12 : begin
        basicSet := tblX12;
        Shift1   := '';
        Shift2   := '';
        Shift3   := '';
      end;
  end;

  idx:=1;
  len := Length(s);
  while len>0 do begin
    currvalue := s[idx];
    Inc(idx);
    Dec(len);
    if currvalue>=#128 then begin
      if FCurrentEncoding=dmeX12 then begin
        Exit;
      end;
      StoreText(1);
      StoreText(30);
    end;

    i:=Pos(CurrValue,BasicSet);
    if i>0 then
      StoreText(i-1)
    else begin
      if currValue<#32 then begin
        StoreText(0);       // Shift1
        StoreText(Byte(currValue));
      end else begin
        i:=Pos(currValue, Shift2);
        if i>0 then begin
          StoreText(1);     // Shift2
          StoreText(i-1);
        end else begin
          i:=Pos(currValue, Shift3);
          if i>0 then begin
            StoreText(2);     // Shift3
            StoreText(i-1);
          end else begin
            Exit;
          end;
        end;
      end;
    end;
  end; // end while
  if buf_idx<>1 then StoreText(0);
  if buf_idx<>1 then StoreText(0);
end;

procedure TpsDataMatrix.EncodeEdiFact(const s:String);
var i,x,val     : integer;
    total       : integer;
    cw1,cw2,cw3 : integer;
begin
  if FCurrentEncoding<>dmeEdiFact then begin
    FCurrentEncoding := dmeEdiFact;
    psDataPut(dw, dm_LatchEdiFact);
  end;

  x:=1;
  while Length(s)-x+1>=4  do begin
    total := 0;
    for i:=0 to 3 do begin
      val := Ord(s[x+i]);
      case val of
        64..94 : Dec(val,64);
        // 31 : ; { TODO : ??? Co tu mabyt ? }
        32..63 : ;
        else  begin
          Break;
        end;
      end;
      total := (total shl 6) + val;
    end;

    cw1   := total div 65536;
    total := total mod 65536;
    cw2   := total div 256;
    cw3   := total mod 256;
    psDataPut(dw, cw1);
    psDataPut(dw, cw2);
    psDataPut(dw, cw3);
    Inc(x,4);
  end;

  if x<=Length(s) then
      EncodeASCII(Copy(s,x, Length(s)-x-1));
end;


procedure TpsDataMatrix.Encode256(const s:String ; EndOfSymbol:Boolean);
var i,r,tmp, len:integer;
begin
  len := Length(s);
  psDataPut(dw, dm_Latch256); // latch to binary mode
  if EndOfSymbol then
      psDataPut(dw,0)
  else begin
    if len<250 then
        psDataPut(dw, len)
    else begin
        psDataPut(dw, (len div 250)+249);
        psDataPut(dw, len mod 250);
    end;
  end;

  for i:=1 to len do begin
      r:=((149*( dw.position + 1)) mod 255)+1;
      tmp := Ord(s[i]) + r;
      psDataPut(dw, tmp mod 256);
  end;
  FCurrentEncoding := dmeASCII;
end;


procedure TpsDataMatrix.AddPad(addFrom, totalLen:Integer);
var i,x:integer;
begin
  psDataPut(dw, dm_PAD);
  for i:=addFrom to totalLen - 1 do begin
    x := (((149*(i+1)) mod 253) + 1 + 129) ;
    if x>=254 then
        Dec(x,254);
    psDataPut(dw, x);
  end;
end;

function  dmMapToFinal(dmParams:TdmMatrixParams; x,y:Integer):TPoint;
begin
  { TODO -opsoft :  }
  //  toto by malo previest datamatrix mriezku po prevode DW do zakladnej mriezky
  //  a potom nasledne este aplikovat posun do mriezky s riadiacimi pixelmi
  Result.X := x+1+(x div dmParams.CellSizeX) shr 1;
  Result.Y := y+1+(y div dmParams.CellSizeY) shr 1;
end;


// function  dmECC200Optimize(s:string; var outData:array of integer):integer;
procedure TpsDataMatrix.EncodeAutomatic(s:String);
var {idxOut,} idxIn {,cnt}  : integer;
    {CurrentEncoding,} NewEncoding       : TpsDataMatrixEncoding;
    res               : string;
    c                 : Integer;

    procedure SwitchTo(nc:TpsDataMatrixEncoding);
    begin
      if FCurrentEncoding<>nc then Exit;
      FCurrentEncoding:=nc;
      { TODO : dorobit prepinanie encoding }
    end;

    procedure putCW(const value:Integer);
    begin
      res := res+Chr(value);
    end;

    function  GetNewMode:TpsDataMatrixEncoding;
    var E       : array [TpsDataMatrixEncoding] of Double;
        idxE    : TpsDataMatrixEncoding;
        idxTest : Integer;
        value   : double;
        c       : Char;
    begin
      idxTest := idxIn;
      {J.1}
      if FCurrentEncoding = dmeASCII then begin
          E[dmeASCII]  := 0;
          E[dmeC40]    := 1;
          E[dmeText]   := 1;
          E[dmeX12]    := 1;
          E[dmeEDIFACT]:= 1;
          E[dmeBinary] := 1.25;
      end else begin
          E[dmeASCII]  := 1;
          E[dmeC40]    := 2;
          E[dmeText]   := 2;
          E[dmeX12]    := 2;
          E[dmeEDIFACT]:= 2;
          E[dmeBinary] := 2.25;
      end;

      {J.2 - J.6}
      case FCurrentEncoding of
        dmeC40      : E[dmeC40]    := 0;
        dmeText     : E[dmeText]   := 0;
        dmeX12      : E[dmeX12]    := 0;
        dmeEDIFACT  : E[dmeEdiFact]:= 0;
        dmeBinary   : E[dmeBinary] := 0;
      end;

      while True do begin
        {K}
        if idxIn>Length(s) then begin
          {K.1}
          for idxE:=Low(TpsDataMatrixEncoding) to High(TpsDataMatrixEncoding) do
            E[idxE] := Round(E[idxE]+0.5);
          {K.2 - K.7}
          Result := dmeC40;
          value:=9999999;  // used for find minimum value of E
          for idxE:=Low(TpsDataMatrixEncoding) to High(TpsDataMatrixEncoding) do
            if E[idxE]<value then begin
              value  := E[idxE];
              Result := idxE;
            end;
          Exit;
        end;

        c := s[idxTest];
        Inc(idxTest);
        case FCurrentEncoding of
          {L}
          dmeASCII : if IsDigit(c)      then E[dmeASCII]:=E[dmeASCII]+0.5
                     else if c>#127     then E[dmeASCII]:=Round(E[dmeASCII]+0.5)+2
                     else                    E[dmeASCII]:=Round(E[dmeASCII]+0.5)+1;
          {M}
          dmeC40   : if Pos(c,tblC40)>0 then E[dmeC40]:=E[dmeC40]+2/3
                     else if c>#127     then E[dmeC40]:=E[dmeC40]+8/3
                     else                    E[dmeC40]:=E[dmeC40]+4/3;
          {N}
          dmeText  : if Pos(c,tblText)>0 then E[dmeText]:=E[dmeText]+2/3
                     else if c>#127      then E[dmeText]:=E[dmeText]+8/3
                     else                     E[dmeText]:=E[dmeText]+4/3;
          {O}
          dmeX12   : if Pos(c,tblX12)>0  then E[dmeX12]:=E[dmeX12]+ 2/3
                     else if c>#127      then E[dmeX12]:=E[dmeX12]+13/3
                     else                     E[dmeX12]:=E[dmeX12]+10/3;
          {P}
          dmeEdiFact:if (c>=#32) and (c<=#94)   then E[dmeEdiFact]:=E[dmeEdiFact]+ 3/4
                     else if c>#127             then E[dmeEdiFact]:=E[dmeEdiFact]+17/4
                     else                            E[dmeEdiFact]:=E[dmeEdiFact]+13/4;
          {Q}
          dmeBinary:
            {Q.1}
              { TODO : dopracovat FNC1, StructuredAppen, Reader program, Code page}
            {Q.2}
              E[dmeBinary]:=E[dmeBinary]+1;
        end;

        {R}
        if Length(s)-idxIn<=4 then begin
          Result := dmeC40;
          value  := 9999999;  // used for find minimum value of E
          for idxE:=Low(TpsDataMatrixEncoding) to High(TpsDataMatrixEncoding) do
            if E[idxE]+1<value then begin
              value  := E[idxE];
              Result := idxE;
              {K.6.i.}{ TODO : dorobit }
              // if E[dmeC40]<E[dmeX12] then Result := dmeX12;
            end;
            Exit;
        end;
      end; // end while
    end;

begin

  // now not supported automatic encoding
  EncodeASCII(s);
  Exit;

  // idxOut := 0;    // output data - dynamic integer array, start with index 0
  idxIn  := 1;    // input data begin from index 1

  // --------------------------------------------------------------------------
  // algoritm used from aimglobal.org, ECC200 symbology specification,
  // pages 91-92
  // --------------------------------------------------------------------------
  // Annexe S

  {Annexe S}
  {A}
  // newEncoding      := dmeASCII;
  FCurrentEncoding := dmeASCII;

  while idxIn<=Length(s) do begin
    c:=Ord(s[idxIn]);
    case FCurrentEncoding of
      {B}
      dmeASCII : begin
        {B.1}
          if idxIn+1<=Length(s) then
            if isDigit(s[idxIn]) and isDigit(s[idxIn+1]) then begin
              putCW( 10*(Ord(s[idxIn])-Ord0) + Ord(s[idxIn+1])- Ord0 + dm_NumberPairs);
              Inc(idxIn,2);
              Continue;
            end;


        {B.2}
        newEncoding:=GetNewMode;
        if newEncoding<>FCurrentEncoding then  begin
          { TODO : ak encBinary ... dorobit }
          SwitchTo(newEncoding);
        end;

        {B.3}
        if c>127 then begin
          putCW(dm_Extended);
          putCW(c-127);
          Inc(idxIn);
          Continue;
        end;

        {B.4}
        putCW(c+1);
        Inc(idxIn);
      end;

    {C}
      dmeC40 : begin
        {C.2}

      end;

      {D}
      dmeText: begin
      end;
      {E}
      dmeX12 :begin

      end;

      {F}
      dmeEdiFact :begin
      end;

      {G}
      dmeBinary : begin
      end;
    end; // end case
  end;   // end of main loop
end;


procedure TpsDataMatrix.EncodeECI(eciValue:Integer);
begin
  if eciValue<0 then
      Exit;

  case eciValue of
    000000 .. 000126 : begin
        psDataPut(dw, dm_ECI);
        psDataPut(dw, eciValue+1);
    end;
    000127 .. 016382 : begin
        psDataPut(dw, dm_ECI);
        psDataPut(dw, ((eciValue-127) div 254) + 128 );
        psDataPut(dw, ((eciValue-127) mod 254) + 1);
    end;
    016383 .. 999999 : begin
        psDataPut(dw, dm_ECI);
        psDataPut(dw, ((eciValue-16383) div 64516) + 192);
        psDataPut(dw, (((eciValue-16383) div 254) mod 254) + 1);
        psDataPut(dw, ((eciValue-16383) mod 254) + 1 );
    end;
  end;
end;


function  TpsDataMatrix.Encode(s:String):String;
begin

//  if FECI<>constDataMatrixDefaultECI then
  EncodeECI(FECI);
  AppendEncode;

  if FNC1 in [fnc1FirstPosition, fnc1GS1] then
        psDataPut(dw, dm_FNC1);

  FCurrentEncoding := dmeASCII;
  case Encoding of
    dmeAutomatic  : EncodeAutomatic(s);
    dmeASCII      : EncodeASCII(s);
    dmeTEXT, //       : Result := EncodeText(s,idx);
    dmeX12, //        : Result := EncodeX12(s,idx);
    dmeC40        : EncodeText(Encoding, s);
    dmeEDIFACT    : EncodeEdiFact(s);
    dmeBinary     : Encode256(s, True);
  end;
  // EncodeASCII('A');//
  //psDataPut(dw, dm_ToASCII);
end;

procedure TpsDataMatrix.ECCInterleave(Bytes, Blocks, RSBlock:Integer);
var n, p, b   : integer;
    buf, ecc  : array [0..10000] of byte;
    EccCount,DataCount  : integer;
begin
  EccCount   := RSBlock div blocks;
  DataCount  := Bytes div blocks;

  dmReedSolomon.GeneratePoly(EccCount,1);

  for b:=0 to blocks-1 do begin
    n:=b;
    for p:=0 to DataCount-1 do begin
      buf[p]:=dw.data[n];
      Inc(n,Blocks);
    end;

    p:=DataCount;
    if (b in [8,9]) and (RSBlock=620) then Dec(p);

    dmReedSolomon.Encode(buf, p, ecc, EccCount);

    p:=eccCount-1;
    n:=b;
    while n<rsblock{*blocks} do begin
      dw.data[bytes+n]:=ecc[p];
      Dec(p);
      Inc(n, blocks);
    end;
  end;
end;




procedure TpsDataMatrix.SetECI(const Value: Integer);
begin
  FECI := Value;
end;

procedure TpsDataMatrix.SetEncoding(const Value: TpsDataMatrixEncoding);
begin
    FEncoding := Value;
end;

procedure TpsDataMatrix.SetFileID(const Value: String);
begin
  FFileID := Value;
end;

procedure TpsDataMatrix.SetSegmentCount(const Value: Integer);
begin
  FSegmentCount := Value;
end;

procedure TpsDataMatrix.SetSegmentIndex(const Value: Integer);
begin
  FSegmentIndex := Value;
end;

procedure TpsDataMatrix.SetVersion(const Value: TpsDataMatrixVersion);
begin
    FVersion := Value;
end;


function TpsDataMatrix.CreateMatrix(s: String): boolean;
var encLength : Integer;
begin
  Result    := False;
  psArrayInitialize(dw, 5000);

  { TODO : Toto preverit }
  {AppendEncode + EncodeECI(Eci) + }
  Encode(s);

  encLength := dw.count;
  if encLength=0 then
      Exit;

  mp  := dmGetMatrixParams(Version, encLength);

  { TODO : toto prerobit na exception }
  if not ((mp.TotalDW>0) and (mp.TotalDW>=encLength)) then begin
    // ShowMessage('Maximum data capacity !!!');
    Exit;
  end;

  ErrorSize := mp.TotalEW;
  DataSize  := mp.TotalDW;

  if (DataSize>encLength) and (FCurrentEncoding<>dmeASCII) then
    SwitchToASCII;

  encLength := dw.Count;

  if DataSize<>encLength then
      AddPad(encLength, DataSize);

  ECCInterleave(DataSize, mp.Inter, ErrorSize);

  Result := True;
end;

procedure TpsDataMatrix.SwitchToASCII;
begin
  if FCurrentEncoding<>dmeASCII then begin
    if FCurrentEncoding in [dmeC40, dmeText, dmeX12] then
      psDataPut(dw, dm_ToASCII)
    else
      psDataPut(dw,dm_ToASCIIFromEDI);
    FCurrentEncoding := dmeASCII;
  end;
end;

{
function TpsDataMatrix.EncodeC40(const s: String;
  var idx: Integer): string;
begin

end;

function TpsDataMatrix.EncodeX12(const s: String; var idx: Integer): string;
var i,value : Integer;
begin
  Result:= '';
  if idx>Length(s) then Exit;

  if FCurrentEncoding<>dmeX12 then begin
    FCurrentEncoding := dmeX12;
    Result:=Result + Char(dm_LatchX12);
  end;

  for i:=idx to Length(s) do begin
      // value:=Ord(s[i]);
      value := Pos(tblX12, s[i]);
      if value>0 then
        Result := Result + Chr(Value-1)
      else begin
            Result:='';
            Exit;
      end;
  end;
end;
}

procedure TpsDataMatrix.AppendEncode;
var x1,x2,x3,id:integer;
begin
  id := StrToIntDef(FFileID,-1);

  if (id>0) and AppendCheck(id) then begin
    x1 := ( (SegmentIndex and $0f) shl 4 ) + (17-SegmentCount);
    x2 := (id div 254)+1;
    x3 := (id mod 254)+1;
    psDataPut(dw, dm_Append);
    psDataPut(dw, x1);
    psDataPut(dw, x2);
    psDataPut(dw, x3);
  end;
end;


function TpsDataMatrix.AppendCheck(id:integer): Boolean;
begin
  Result := (SegmentCount in [2..16]) and (SegmentIndex in [0..SegmentCount-1])
      and (id>=0) and (id<=64516);
end;


function  psDataMatrixGetPixels(BarcodeObject:TComponent):Integer;
var dm : TpsDataMatrix;
    bc : TpsBarcodeComponent;
    s  : String;
begin
  Result := 1;
  bc:=BarcodeObject as TpsBarcodeComponent;
  if Length(bc.Barcode)=0 then Exit;

  dm := TpsDataMatrix.Create;
  try
    with bc.Params do begin
      dm.Encoding     := DataMatrix.Encoding;
      dm.Version      := DataMatrix.Version;
      dm.ECI          := ECI;
      dm.SegmentCount := SegmentCount;
      dm.SegmentIndex := SegmentIndex;
      dm.FileID       := qrCode.FileName;
      dm.FNC1         := bc.Params.GS1.FNC1Type;
    end;

    // if GS1/FNC1 enabled, apply input mode
    // if no, return TpsBarcode.Barcode value
    s:=bc.SolveGS1;
    s:=ConvertUnicodeToUTF8(s);

    if dm.CreateMatrix(s) then
        dm.dmGrid(bc.Pixels);
  finally
    dm.Free;
  end;
end;

{
function  psPaintDataMatrix(Value:String; bc:IpsBarcodeInterface; C:TCanvas; R:TRect):Integer;
var L  : TpsMatrix;
    dm : TpsDataMatrix;
begin
  Result := 1;
  if Length(Value)=0 then Exit;

  dm := TpsDataMatrix.Create;
  try
    with bc.Params do begin
      dm.Encoding     := DataMatrix.Encoding;
      dm.Version      := DataMatrix.Version;
      dm.ECI          := ECI;
      dm.SegmentCount := SegmentCount;
      dm.SegmentIndex := SegmentIndex;
      dm.FileID       := FileName;
    end;

    if dm.CreateMatrix(Value) then begin
      L:=TpsMatrix.Create;
      try
        dm.dmGrid(L);
        L.Paint(C, R, bc);
      finally
        L.Free;
      end;
    end;
  finally
    dm.Free;
  end;
end;
}



//--------------------------------------------------------------------------
// -- QR CODE
//--------------------------------------------------------------------------

// ------------------------------------------------------------------------
// WARNING : PLEASE, NOT CHANGE THESE TABLES , IF YOU MAKE HERE BAD CHANGES
// QR Code don't work
// ------------------------------------------------------------------------
// some table : index 1..4 is used for Micro QR, 5..44 for QR, version 1-40

const

    Default_QR_ECI     = 3;
    constDataArraySize = 256;

    qrECCDescriptions : array[TpsQrEccLevel] of Char=('L','M','Q','H');
    qrModeDescriptions: array[TpsQRMode] of String=(
      'Automatic', 'Numeric','Alphanumeric','Byte(ASCII 256)', 'Kanji');

    QR_AlignTable : array[1..40,1..7] of byte =
    ( (0,0,0,0,0,0,0),   (6,18,0,0,0,0,0), (6,22,0,0,0,0,0),
      (6,26,0,0,0,0,0),  (6,30,0,0,0,0,0),  (6,34,0,0,0,0,0),
      (6,22,38,0,0,0,0), (6,24,42,0,0,0,0), (6,26,46,0,0,0,0),
      (6,28,50,0,0,0,0), (6,30,54,0,0,0,0), (6,32,58,0,0,0,0),
      (6,34,62,0,0,0,0), (6,26,46,66,0,0,0), (6,26,48,70,0,0,0),
      (6,26,50,74,0,0,0), (6,30,54,78,0,0,0), (6,30,56,82,0,0,0),
      (6,30,58,86,0,0,0),(6,34,62,90,0,0,0), (6,28,50,72,94,0,0),
      (6,26,50,74,98,0,0),(6,30,54,78,102,0,0),(6,28,54,80,106,0,0),
      (6,32,58,84,110,0,0),(6,30,58,86,114,0,0),(6,34,62,90,118,0,0),
      (6,26,50,74,98,122,0), (6,30,54,78,102,126,0),(6,26,52,78,104,130,0),
      (6,30,56,82,108,134,0),(6,34,60,86,112,138,0), (6,30,58,86,114,142,0),
      (6,34,62,90,118,146,0),(6,30,54,78,102,126,150), (6,24,50,76,102,128,154),
      (6,28,54,80,106,132,158),(6,32,58,84,110,136,162),
      (6,26,54,82,110,138,166),(6,30,58,86,114,142,170) );

    QR_VersionInfo : array[7..40] of Integer=
      ( $07C94, $085BC, $09A99, $0A4D3, $0BBF6, $0C762, $0D847, $0E60D, $0F928,
        $10B78, $1145D, $12A17, $13532, $149A6, $15683, $168C9, $177EC, $18EC4,
        $191E1, $1AFAB, $1B08E, $1CC1A, $1D33F, $1ED75, $1F250, $209D5, $216F0,
        $228BA, $2379F, $24B0B, $2542E, $26A64, $27541, $28C69);

    QR_FormatInfo :  array[0..31] of Integer=
      ( $5412,  $5125,  $5E7C,  $5B4B,  $45F9,  $40CE,  $4F97,  $4AA0,  $77C4,
        $72F3,  $7DAA,  $789D,  $662F,  $6318,  $6C41,  $6976,  $1689,  $13BE,
        $1CE7,  $19D0,  $0762,  $0255,  $0D0C,  $083B,  $355F,  $3068,  $3F31,
        $3A06,  $24B4,  $2183,  $2EDA,  $2BED );

    QR_FormatInfoMicro: array[0..31] of Integer=
      ( $4445,  $4172,  $4E2B,  $4B1C,  $55AE,  $5099,  $5FC0,  $5AF7,  $6793,
        $62A4,  $6DFD,  $68CA,  $7678,  $734F,  $7C16,  $7921,  $06DE,  $03E9,
        $0CB0,  $0987,  $1735,  $1202,  $1D5B,  $186C,  $2508,  $203F,  $2F66,
        $2A51,  $34E3,  $31D4,  $3E8D,  $3BBA );

    QR_AlphaTable ='0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./';

    QR_Capacity_TotalCW_Micro : array[1..4] of Integer = ( 5 ,10, 17, 24 );
    QR_Capacity_TotalCW : array[1..40] of Integer = (
      26,   44,   70,   100,  134,  172,  196,  242,
      292,  346,  404,  466,  532,  581,  655,  733,
      815,  901,  991,  1085, 1156, 1258, 1364, 1474,
      1588, 1706, 1828, 1921, 2051, 2185, 2323, 2465,
      2611, 2761, 2876, 3034, 3196, 3362, 3532, 3706
      );

    QR_Capacity_DataCW_Micro : array[1..4, TpsQREccLevel] of Integer = (
      //MicroQR
      (3,0,0,0),    (5,4,0,0),    (11,9,0,0),     (16,14,10, 0));

    QR_Micro_Bits : array[1..4, TpsQREccLevel] of Integer = (
       (20, 0, 0, 0), (40,32,0,0), (84, 68, 0,0), (128, 112, 80, 0) );

    // QR Code
    QR_Capacity_DataCW       : array[1..40, TpsQREccLevel] of Integer = (
      (19,16,13,9),           {1}
      (34,28,22,6),           {2}
      (55,44,34,26),          {3}
      (80,64,48,36),          {4}


      (108, 86, 62, 46),      (136,108,76,60),        (156,124,88,66),
      (194,154,110,86),       (232,182,132,100),      (274,216,154,122),
      (324,254,180,140),      (370,290,206,158),      (428,334,244,180),
      (461,365,261,197),      (523,415,295,223),      (589,453,325,253),
      (647,507,367,283),      (721,563,397,313),      (795,627,445,341),
      (861,669,485,385),      (932,714,512,406),      (1006,782,568,442),
      (1094,860,614,464),     (1174,914,664,514),     (1276,1000,718,538),
      (1370,1062,754,596),    (1468,1128,808,628),    (1531,1193,871,661),
      (1631,1267,911,701),    (1735,1373,985,745),    (1843,1455,1033,793),
      (1955,1541,1115,845),   (2071,1631,1171,901),   (2191,1725,1231,961),
      (2306,1812,1286,986),   (2434,1914,1354,1054),  (2566,1992,1426,1096),
      (2702,2102,1502,1142),  (2812,2216,1582,1222),  (2956,2334,1666,1276)
      );


    // 1.->ECC_COUNT, 2.->ECC_BLOCKS, 3.-5. C,K,R
    QR_ECC : array[1..44,1..8, 1..5] of Integer =(
      // M1
      ((2,1,5,3,0), (0,0,0,0,0), (0,0,0,0,0), (0,0,0,0,0),
      (0,0,0,0,0), (0,0,0,0,0), (0,0,0,0,0), (0,0,0,0,0)),
      // M2
      ((5,1,10,5,1),(0,0,0,0,0), (6,1,10,4,2),(0,0,0,0,0),
      (0,0,0,0,0), (0,0,0,0,0), (0,0,0,0,0),(0,0,0,0,0)),
      //M3
      ((6,1,17,11,2),(0,0,0,0,0),(8,1,17,9,4),(0,0,0,0,0),
      (0,0,0,0,0),(0,0,0,0,0),(0,0,0,0,0),(0,0,0,0,0)),
      //M4
      ((8,1,24,16,3), (0,0,0,0,0), (10,1,24,14,5),(0,0,0,0,0),
      (14,1,24,10,7),(0,0,0,0,0),(0,0,0,0,0),(0,0,0,0,0)),
      //1
      ((7,1,26,19,2), (0,0,0,0,0),(10,1,26,16,4),(0,0,0,0,0),
      (13,1,26,13,6),(0,0,0,0,0),(17,1,26,9,8),(0,0,0,0,0)),
      //2
      ((10,1,44,34,4),(0,0,0,0,0),(16,1,44,28,8),(0,0,0,0,0),
      (22,1,44,22,11),(0,0,0,0,0),(28,1,44,16,14),(0,0,0,0,0)),
      //3
      ((15,1,70,55,7),(0,0,0,0,0),(26,1,70,44,13),(0,0,0,0,0),
      (36,2,35,17,9),(0,0,0,0,0),(44,2,35,13,11),(0,0,0,0,0)),
      //4
      ((20,1,100,80,10),(0,0,0,0,0),(36,2,50,32,9),(0,0,0,0,0),
      (52,2,50,24,13),(0,0,0,0,0),(64,4,25,9,8),(0,0,0,0,0)),
      //5
      ((26,1,134,108,13),(0,0,0,0,0),(48,2,67,43,12),(0,0,0,0,0),
      (72,2,33,15,9),(72,2,34,16,9),(88,2,33,11,11),(88,2,34,12,11)),
      //6
      ((36,2,86,68,9),(0,0,0,0,0),(64,4,43,27,8),(0,0,0,0,0),
      (96,4,43,19,12),(0,0,0,0,0),(112,4,43,15,14),(0,0,0,0,0)),
      //7
      ((40,2,98,78,10),(0,0,0,0,0),(72,4,49,31,9),(0,0,0,0,0),
      (108,2,32,14,9),(108,4,33,15,9),(130,4,39,13,13),(130,1,40,14,13)),
      //8
      ((48,2,121,97,12),(0,0,0,0,0),(88,2,60,38,11),(88,2,61,39,11),
      (132,4,40,18,11),(132,2,41,19,11),(156,4,40,14,13),(156,2,41,15,13)),
      //9
      ((60,2,146,116,15),(0,0,0,0,0),(110,3,58,36,11),(110,2,59,37,11),
      (160,4,36,16,10),(160,4,37,17,10),(192,4,36,12,12),(192,4,37,13,12)),
      //10
      ((72,2,86,68,9),(72,2,87,69,9),(130,4,69,43,13),(130,1,70,44,13),
      (192,6,43,19,12),(192,2,44,20,12),(224,6,43,15,14),(224,2,44,16,14)),
      //11
      ((80,4,101,81,10),(0,0,0,0,0),(150,1,80,50,15),(150,4,81,51,15),
      (224,4,50,22,14),(224,4,51,23,14),(264,3,36,12,12),(264,8,37,13,12)),
      //12
      ((96,2,118,92,12),(96,2,117,93,12),(176,6,58,36,11),(176,2,59,37,12),
      (260,4,46,20,13),(260,6,47,21,13),(308,7,42,14,14),(308,4,43,15,14)),
      //13
      ((104,4,133,107,13),(0,0,0,0,0),(198,8,59,37,11),(198,1,60,38,11),
      (288,8,44,20,12),(288,4,45,21,12),(352,12,33,11,11),(352,4,34,12,11)),
      //14
      ((120,3,145,115,15),(120,1,146,116,15),(216,4,64,40,12),(216,5,65,41,12),
      (320,11,36,16,10),(320,5,37,17,10),(384,11,36,12,12),(384,5,37,13,12)),
      //15
      ((132,5,109,87,11),(132,1,110,88,11),(240,5,65,41,12),(240,5,66,42,12),
      (360,5,54,24,15),(360,7,55,25,15),(432,11,36,12,12),(432,7,37,13,12)),
      //16
      ((144,5,122,98,12),(144,1,123,99,12),(280,7,73,45,14),(280,3,74,46,14),
      (408,15,43,19,12),(408,2,44,20,12),(480,3,45,15,15),(480,13,46,16,15)),
      //17
      ((168,1,135,107,14),(168,5,136,108,14),(308,10,74,46,14),(308,1,75,47,14),
      (448,1,50,22,14),(448,15,51,23,14),(532,2,42,14,14),(532,17,43,15,14)),
      //18
      ((180,5,150,120,15),(180,1,151,121,15),(338,9,69,43,13),(338,4,70,44,13),
      (504,17,50,22,14),(504,1,51,23,14),(588,2,42,14,14),(588,19,43,15,14)),
      //19
      ((196,3,141,113,14),(196,4,142,114,14),(364,3,70,44,13),(364,11,71,45,13),
      (546,17,47,21,13),(546,4,48,22,13),(650,9,39,13,13),(650,16,40,14,13)),
      //20
      ((224,3,135,107,14),(224,5,136,108,14),(416,3,67,41,13),(416,13,68,42,13),
      (600,15,54,24,15),(600,5,55,25,15),(700,15,43,15,14),(700,10,44,16,14)),
      //21
      ((224,4,144,116,14),(224,4,145,117,14),(442,17,68,42,13),(0,0,0,0,0),
      (644,17,50,22,14),(644,6,51,23,14),(750,19,46,16,15),(750,6,47,17,15)),
      //22
      ((252,2,139,111,14),(252,7,140,112,14),(476,17,74,46,14),(0,0,0,0,0),
      (690,7,54,24,15),(690,16,55,25,15),(816,34,37,13,12),(0,0,0,0,0)),
      //23
      ((270,4,151,121,15),(270,5,152,122,15),(504,4,75,47,14),(504,14,76,48,14),
      (750,11,54,24,15),(750,14,55,25,15),(900,16,45,15,15),(900,14,46,16,15)),
      //24
      ((300,6,147,117,15),(300,4,148,118,15),(560,6,73,45,14),(560,14,74,46,14),
      (810,11,54,24,15),(810,16,55,25,15),(960,30,46,16,15),(960,2,47,17,15)),
      //25
      ((312,8,132,106,13),(312,4,133,107,13),(588,8,75,47,14),(588,13,76,48,14),
      (870,7,54,24,15),(870,22,55,25,15),(1050,22,45,15,15),(1050,13,46,16,15)),
      //26
      ((336,10,142,114,14),(336,2,143,115,14),(644,19,74,46,14),(644,4,75,47,14),
      (952,28,50,22,14),(952,6,51,23,14),(1110,33,46,16,15),(1110,4,47,17,15)),
      //27
      ((360,8,152,122,15),(360,4,153,123,15),(700,22,73,45,14),(700,3,74,46,14),
      (1020,8,53,23,15),(1020,26,54,24,15),(1200,12,45,15,15),(1200,28,46,16,15)),
      //28
      ((390,3,147,117,15),(390,10,148,118,15),(728,3,73,45,14),(728,23,74,46,14),
      (1050,4,54,24,15),(1050,31,55,25,15),(1260,11,45,15,15),(1260,31,46,16,15)),
      //29
      ((420,7,146,116,15),(420,7,147,117,15),(784,21,73,45,14),(784,7,74,46,14),
      (1140,1,53,23,15),(1140,37,54,24,15),(1350,19,45,15,15),(1350,26,46,16,15)),
      //30
      ((450,5,145,115,15),(450,10,146,116,15),(812,19,75,47,14),(812,10,76,48,14),
      (1200,15,54,24,15),(1200,25,55,25,15),(1440,23,45,15,15),(1440,25,46,16,15)),
      //31
      ((480,13,145,115,15),(480,3,146,116,15),(868,2,74,46,14),(868,29,75,47,14),
      (1290,42,54,24,15),(1290,1,55,25,15),(1530,23,45,15,15),(1530,28,46,16,15)),
      //32
      ((510,17,145,115,15),(0,0,0,0,0),(924,10,74,46,14),(924,23,75,47,14),
      (1350,10,54,24,15),(1350,35,55,25,15),(1620,19,45,15,15),(1620,35,46,16,15)),
      //33
      ((540,17,145,115,15),(540,1,146,116,15),(980,14,74,46,14),(980,21,75,47,14),
      (1440,29,54,24,15),(1440,19,55,25,15),(1710,11,45,15,15),(1710,46,46,16,15)),
      //34
      ((570,13,145,115,15),(570,6,146,116,15),(1036,14,74,46,14),(1036,23,75,47,14),
      (1530,44,54,24,15),(1530,7,55,25,15),(1800,59,46,16,15),(1800,1,47,17,15)),
      //35
      ((570,12,151,121,15),(570,7,152,122,15),(1064,12,75,47,14),(1064,26,76,48,14),
      (1590,39,54,24,15),(1590,14,55,25,15),(1890,22,45,15,15),(1890,41,46,16,15)),
      //36
      ((600,6,151,121,15),(600,14,152,122,15),(1120,6,75,47,14),(1120,34,76,48,14),
      (1680,46,54,24,15),(1680,10,55,25,15),(1980,2,45,15,15),(1980,64,46,16,15)),
      //37
      ((630,17,152,122,15),(630,4,153,123,15),(1204,29,74,46,14),(1204,14,75,47,14),
      (1770,49,54,24,15),(1770,10,55,25,15),(2100,24,45,15,15),(2100,46,46,16,15)),
      //38
      ((660,4,152,122,15),(660,18,153,123,15),(1260,13,74,46,14),(1260,32,75,47,14),
      (1860,48,54,24,15),(1860,14,55,25,15),(2220,42,45,15,15),(2220,32,46,16,15)),
      //39
      ((720,20,147,117,15),(720,4,148,118,15),(1316,40,75,47,14),(1316,7,76,48,14),
      (1950,43,54,24,15),(1950,22,55,25,15),(2310,10,45,15,15),(2310,67,46,16,15)),
      //40
      ((750,19,148,118,15),(750,6,149,119,15),(1372,18,75,47,14),(1372,31,76,48,14),
      (2040,34,54,24,15),(2040,34,55,25,15),(2430,20,45,15,15),(2430,61,46,16,15))
    );

   indicatorECI           = 1;
   indicatorNumeric       = 2;
   indicatorAplhanumeric  = 3;
   indicatorBinary        = 4;
   indicatorKanji         = 5;
   indicatorAppend        = 6;
   indicatorFNC1st        = 7;
   indicatorFNC2nd        = 8;

   ModeIndicators: array[1..8,1..5] of String = (
    ('0111','','','',''),         // ECI
    ('0001','','0','00','000'),   // Numeric
    ('0010','','1','01','001'),   // Alphanumeric
    ('0100','','' ,'10','010'),   // Binary/Byte
    ('1000','','' ,'11','011'),   // Kanji
    ('0011','','' ,'',''),        // Structuted append
    ('0101','','' ,'',''),        // FNC1 1st position
    ('1001','','' ,'','')         // FNC1 2nd position
  );

type

  TpsByterArray=array{[0..constDataArraySize-1]} of byte;

  TpsQRCode = class(TObject)
  private
    Params            : TpsParams;
    FMicroQR          : Boolean;
    FUsedVersion      : Integer;
    FUsedMode         : TpsQrMode;
    FUsedECC          : TpsQrEccLevel;
    FUsedMask         : Integer;
    fUsedMicroQR      : Boolean;
    FNC1              : TpsFNC1Type;
    MatrixW, MatrixH  : Integer;
    EncodedData       : String;

    err               : TpsErrRecord;

    function  GetFormatInfo(FullData:Boolean):String;
    function  GetNumberOfBitsIndicatorSize(mqr:Boolean; currMode:TpsQrMode; ver:Integer): Integer;
    function  GetModeIndicator(Micro:boolean; currMode:TpsQrMode; ver:Integer): String;
    procedure AddTerminator(var s:String);

    procedure DrawFinder(d2:TpsMatrix; x,y:Integer);
    procedure DrawAlign(d2:TpsMatrix; x,y:Integer);
    procedure DrawInfos(d2:TpsMatrix; FullData:Boolean);
    procedure DrawAligns(d2:TpsMatrix);
    procedure DrawMask(d2:TpsMatrix; MaskID:Integer);
    procedure DrawCW(d2: TpsMatrix);

    procedure Draw(dm:TpsMatrix);

    function  OptimizeMask(d2: TpsMatrix):Integer;
    function  EvaluateMask(d2: TpsMatrix): Integer;

    function  EncodePart(const ToEncode: String; currMode: TpsQRMode): String;
    function  EncodeECI(value:Integer):String;
    function  EncodeAppend(idx, total, checksum:Integer):String;
    function  EncodeAutomaticSimple(const ToEncode: String): String;

    procedure BarcodeRaise(err:TpsErrRecord);
  protected
    procedure CreatePlan(s:string; var P:TpsEncodingPlan);
    function  EncodeAutomaticExtended(const ToEncode: String): String;
  public
    class function GetDataCodeWords(ver: Integer; Micro: Boolean;
      ecc: TpsQREccLevel): Integer;
    class function GetTotalCodeWords(ver: Integer; Micro: Boolean): Integer;
    class function CheckVersion(Micro:Boolean; ver:Integer):Boolean;
    class function MaxVersion(Micro:Boolean):Integer;
    class function RemainderBits(Micro:Boolean; Ver:Integer):Integer;

    function TotalDataBits:Integer;
    function CalcStreamLength(const s:string; FMicroTmp:Boolean;
        FVersion:Integer; FMode:TpsQRMode):Integer;
    function  SizeInModules:Integer;

    function  FNC1First:Boolean;

    function  EncodeData(ToEncode:String): String;
    function  EncodeECC(const data:String): String;

    function  Encode(const ToEncode:String):Boolean;
  end;


// -------------------------------------------------------------------------
// ----- constats for ReedSolomon error correction algorithm            ----
// -------------------------------------------------------------------------
const
  mm=8;
  nn=mm*mm-1;
  pp:array[0..mm] of integer = (1,0,0,0,1,1,1,0,1);

  alpha_to:array[0..255] of integer=(
        1,2,4,8,16,32,64,128,29,58,116,232,205,135,19,38,76,
        152,45,90,180,117,234,201,143,3,6,12,24,48,96,192,
        157,39,78,156,37,74,148,53,106,212,181,119,238,193,159,35,70,140,5,10,20,40,80,
        160,93,186,105,210,185,111,222,161,95,190,97,194,153,47,94,188,101,
        202,137,15,30,60,120,240,253,231,211,187,107,214,177,127,254,225,223,
        163,91,182,113,226,217,175,67,134,17,34,68,136,13,26,52,104,208,
        189,103,206,129,31,62,124,248,237,199,147,59,118,236,197,151,51,102,204,
        133,23,46,92,184,109,218,169,79,158,33,66,132,21,42,84,168,
        77,154,41,82,164,85,170,73,146,57,114,228,213,183,115,230,209,191,
        99,198,145,63,126,252,229,215,179,123,246,241,255,227,219,171,75,150,
        49,98,196,149,55,110,220,165,87,174,65,130,25,50,100,200,141,7,14,28,
        56,112,224,221,167,83,166,81,162,89,178,121,242,249,239,195,
        155,43,86,172,69,138,9,18,36,72,144,61,122,244,245,247,243,251,235,
        203,139,11,22,44,88,176,125,250,233,207,131,27,54,108,216,173,71,142,1
      );

      index_of:array[0..255] of integer=(
        0,0,1,25,2,50,26,198,3,223,51,238,27,104,199,75,4,100,224,14,52,141,239,
        129,28,193,105,248,200,8,76,113,5,138,101,47,225,36,15,33,53,147,142,
        218,240,18,130,69,29,181,194,125,106,39,249,185,201,154,9,120,77,228,
        114,166,6,191,139,98,102,221,48,253,226,152,37,179,16,145,34,136,54,
        208,148,206,143,150,219,189,241,210,19,92,131,56,70,64,30,66,182,
        163,195,72,126,110,107,58,40,84,250,133,186,61,202,94,155,159,10,
        21,121,43,78,212,229,172,115,243,167,87,7,112,192,247,140,128,99,
        13,103,74,222,237,49,197,254,24,227,165,153,119,38,184,180,124,17,68,
        146,217,35,32,137,46,55,63,209,91,149,188,207,205,144,135,151,178,
        220,252,190,97,242,86,211,171,20,42,93,158,132,60,57,83,71,109,65,162,
        31,45,67,216,183,123,164,118,196,23,73,236,127,12,111,246,108,161,
        59,82,41,157,85,170,251,96,134,177,187,204,62,90,203,89,95,176,156,
        169,160,81,11,245,22,235,122,117,44,215,79,174,213,233,230,231,173,
        232,116,214,244,234,168,80,88,175
      );


// var
    QR_ReedSolomon : array[1..36 , 0..68] of byte = (
      ( 2,  25,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      ( 5,  113,164,166,119,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      ( 6,  116,0,134,5,176,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      ( 7,  87,229,146,149,238,102,21,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      ( 8,  175,238,208,249,215,252,196,28,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (10,  251,67,46,61,118,70,64,94,32,45,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (13,  74,152,176,100,86,100,106,104,130,218,206,140,78,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (14,  199,249,155,48,190,124,218,137,216,87,207,59,22,91,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (15,  8,183,61,91,202,37,51,58,58,237,140,124,5,99,105,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (16,  120,104,107,109,102,161,76,3,91,191,147,169,182,194,225,120,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (17,  43,139,206,78,43,239,123,206,214,147,24,99,150,39,243,163,136,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (18,  215,234,158,94,184,97,118,170,79,187,152,148,252,179,5,98,96,153,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (20,  17,60,79,50,61,163,26,187,202,180,221,225,83,239,156,164,212,212,188,190,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (22,  210,171,247,242,93,230,14,109,221,53,200,74,8,172,98,80,219,134,160,105,165,231,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (24,  229,121,135,48,211,117,251,126,159,180,169,152,192,226,228,218,111,0,117,232,87,96,227,21,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (26,  173,125,158,2,103,182,118,17,145,201,111,28,165,53,161,21,245,142,13,102,48,227,153,145,218,70,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (28,  168,223,200,104,224,234,108,180,110,190,195,147,205,27,232,201,21,43,245,87,42,195,212,119,242,37,9,123,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (30,  41,173,145,152,216,31,179,182,50,48,110,86,239,96,222,125,42,173,226,193,224,130,156,37,251,216,238,40,192,180,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (32,  10,6,106,190,249,167,4,67,209,138,138,32,242,123,89,27,120,185,80,156,38,69,171,60,28,222,80,52,254,185,220,241,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (34,  111,77,146,94,26,21,108,19,105,94,113,193,86,140,163,125,58,158,229,239,218,103,56,70,114,61,183,129,167,13,98,62,129,51,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (36,  200,183,98,16,172,31,246,234,60,152,115,0,167,152,113,248,238,107,18,63,218,37,87,210,105,177,120,74,121,196,117,251,113,233,30,120,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (40,  59,116,79,161,252,98,128,205,128,161,247,57,163,56,235,106,53,26,187,174,226,104,170,7,175,35,181,114,88,41,47,163,125,134,72,20,232,53,35,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (42,  250,103,221,230,25,18,137,231,0,3,58,242,221,191,110,84,230,8,188,106,96,147,15,131,139,34,101,223,39,101,213,199,237,254,201,123,171,162,194,117,50,96,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (44,  190,7,61,121,71,246,69,55,168,188,89,243,191,25,72,123,9,145,14,247,1,238,44,78,143,62,224,126,118,114,68,163,52,194,217,147,204,169,37,130,113,102,73,181,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (46,  112,94,88,112,253,224,202,115,187,99,89,5,54,113,129,44,58,16,135,216,169,211,36,1,4,96,60,241,73,104,234,8,249,245,119,174,52,25,157,224,43,202,223,19,82,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (48,  228,25,196,130,211,146,60,24,251,90,39,102,240,61,178,63,46,123,115,18,221,111,135,160,182,205,107,206,95,150,120,184,91,21,247,156,140,238,191,11,94,227,84,50,163,39,34,108,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (50,  232,125,157,161,164,9,118,46,209,99,203,193,35,3,209,111,195,242,203,225,46,13,32,160,126,209,130,160,242,215,242,75,77,42,189,32,113,65,124,69,228,114,235,175,124,170,215,232,133,205,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (52,  116,50,86,186,50,220,251,89,192,46,86,127,124,19,184,233,151,215,22,14,59,145,37,242,203,134,254,89,190,94,59,65,124,113,100,233,235,121,22,76,86,97,39,242,200,220,101,33,239,254,116,51,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (54,  183,26,201,87,210,221,113,21,46,65,45,50,238,184,249,225,102,58,209,218,109,165,26,95,184,192,52,245,35,254,238,175,172,79,123,25,122,43,120,108,215,80,128,201,235,8,153,59,101,31,198,76,31,156,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (56,  106,120,107,157,164,216,112,116,2,91,248,163,36,201,202,229,6,144,254,155,135,208,170,209,12,139,127,142,182,249,177,174,190,28,10,85,239,184,101,124,152,206,96,23,163,61,27,196,247,151,154,202,207,20,61,10,0,0,0,0,0,0,0,0,0,0,0,0),
      (58,  82,116,26,247,66,27,62,107,252,182,200,185,235,55,251,242,210,144,154,237,176,141,192,248,152,249,206,85,253,142,65,165,125,23,24,30,122,240,214,6,219,218,29,145,127,134,206,245,117,29,41,63,159,142,233,125,148,123,0,0,0,0,0,0,0,0,0,0),
      (60,  107,140,26,12,9,141,243,197,226,197,219,45,211,101,219,120,28,181,127,6,100,247,2,205,198,57,115,219,101,109,160,82,37,38,238,49,160,209,121,86,11,124,30,181,84,25,194,87,65,102,190,220,70,27,209,16,89,7,33,240,0,0,0,0,0,0,0,0),
      (62,  65,202,113,98,71,223,248,118,214,94,0,122,37,23,2,228,58,121,7,105,135,78,243,118,70,76,223,89,72,50,70,111,194,17,212,126,181,35,221,117,235,11,229,149,147,123,213,40,115,6,200,100,26,246,182,218,127,215,36,186,110,106,0,0,0,0,0,0),
      (64,  45,51,175,9,7,158,159,49,68,119,92,123,177,204,187,254,200,78,141,149,119,26,127,53,160,93,199,212,29,24,145,156,208,150,218,209,4,216,91,47,184,146,47,140,195,195,125,242,238,63,99,108,140,230,242,31,204,11,178,243,217,156,213,231,0,0,0,0),
      (66,  5,118,222,180,136,136,162,51,46,117,13,215,81,17,139,247,197,171,95,173,65,137,178,68,111,95,101,41,72,214,169,197,95,7,44,154,77,111,236,40,121,143,63,87,80,253,240,126,217,77,34,232,106,50,168,82,76,146,67,106,171,25,132,93,45,105,0,0),
      (68,  247,159,223,33,224,93,77,70,90,160,32,254,43,150,84,101,190,205,133,52,60,202,165,220,203,151,93,84,15,84,253,173,160,89,227,52,199,97,95,231,52,177,41,125,137,241,166,225,118,2,54,32,82,215,175,198,43,238,235,27,101,184,127,3,5,8,163,238)
    );

function ReedSolomonBytes(const DataArr:TpsByteArray; DataCount:Integer;
      var EccArray:TpsByteArray; EcwCount:Integer;
      const RSCoef:TpsByteArray):Integer;
var i,j  : Integer;
    lead : Integer;
    tmp  : byte;

    procedure ShiftData;
    var a:Integer;
    begin
      for a := 0 to constDataArraySize- 2 do
        DataArr[a]:=DataArr[a+1];
      DataArr[constDataArraySize-1] := 0;
    end;
begin
  for i:=0 to DataCount-1 do begin
    if DataArr[0]=0 then begin
      ShiftData;
      Continue;
    end;

    Lead := index_of[DataArr[0]];
    ShiftData;

    for j:=0 to EcwCount-1 do begin
      tmp        := alpha_to[(RSCoef[j]+lead) mod 255];
      DataArr[j] := DataArr[j] xor tmp;
    end;
  end;
  Result := 1;
end;

function qr_ReedSolomonCalc(Source:String; Binary:Boolean; errCount:integer):String;
const DataSize=256;
var i,j,k,idx           : integer;
    DataCnt             : integer;
    data, err, qr_coef  : TpsByteArray;
begin
  Result  := '';
  SetLength(data,    constDataArraySize);
  SetLength(err ,    constDataArraySize);
  SetLength(qr_coef, constDataArraySize);

  for i:=0 to constDataArraySize-1 do begin
      Data[i]    := 0;
      Err[i]     := 0;
      qr_coef[i] := 0;
  end;

  // fill data
  DataCnt:=0;
  if Binary then begin
    // konverzia, ak je vstup zadany ako 100101101 ...
    i:=0;
    j:=1;
    while j<=Length(Source) do begin
      data[i] := BinaryStrToInt(Source,j,8);
      Inc(DataCnt);
      Inc(i);
      Inc(j,8);
    end;
  end else begin
    for i:=1 to Length(Source) do
      data[i-1]:=Ord(Source[i]);
    Inc(DataCnt, Length(Source));
  end;


  idx:=-1;
  for i:=1 {Low(QR_ReedSolomon)} to 36 {High(QR_ReedSolomon)} do
    if QR_ReedSolomon[i][0]=errCount then begin
       for k := 1 to errCount do
          qr_coef[k-1] :=QR_ReedSolomon[i][k];
       idx   := i;
       Break;
    end;

  if idx=-1 then
    raise psBarcodeException.Create('Error : No RS koeficients found for ECW count '+IntToStr(errCount));

  ReedSolomonBytes(Data, DataCnt, Err, ErrCount, qr_coef);

  // calculate return string
  Result := '';
  for i:=0 to errCount-1 do begin
    k:=data[i];
    if not Binary then
        Result := Result+Char(k)
    else
        Result := Result + ByteAsBinary(k);
  end;
end;

function  psQRSubset(s:String; fnc1:TpsFNC1Type):TpsQRMode;
var x1,x2 : integer;
begin
    Result := QrBytes;
    if Length(s)=0 then
        Exit;
    x1:=Ord(s[1]);
    if Length(s)>=2 then
        x2:= Ord(s[2])
    else
        x2 := -1;
    if (Fnc1<>fnc1None) and (x1=$1D) then begin
        Result := QrAlphanumeric;
        Exit;
    end;

    case x1 of
      $30..$39             : Result := QrNumeric;
      $20, $24, $25, $2A, $2B, $2D, $2E, $2F, $3A,
      $41 .. $5A           : Result := QrAlphanumeric;
      $81 .. $9F, $E0..$EA : if x2 in [$40..$7E, $80..$FC] then
                Result := QrKanji;
      $EB : if x2 in [$40..$7E, $80..$BF] then
                Result := QrKanji;
    end;
end;

function  IsExclusive(s:String; Subset:TpsQrMode; fnc1:TpsFNC1Type):Boolean;
begin
    Result := psQRSubset(s, fnc1)=Subset;
end;

function psQRExclusiveSubsetCount(const s:string; idx:Integer; Subset:TpsQRMode):Integer;
var i:Integer;
begin
    Result := 0;
    i:=idx;
    while i<=Length(s) do
      if True then
         ;

end;

function psQRGetSubset(const s:String; fnc1:TpsFNC1Type):TpsQRMode;
var i:Integer;
begin
  result := QRNumeric;
  for  i:=1 to Length(s) do
    case psQRSubset(Copy(s,i,1), fnc1) of
      qrNumeric      : ;
      qrAlphanumeric : Result := qrAlphanumeric;
      qrBytes        : begin
            Result := qrBytes;
            Exit;
          end;
      end;
end;

function psQRCheckSubSet(s: String; Mode: TpsQRMode; fnc1:TpsFNC1Type): Integer;
var i:Integer;
begin
    Result:=0;
    for I := 1 to Length(s) do
      if psQRSubSet(Copy(s,i,1), fnc1)>Mode then begin
          Result := i;
          Exit;
      end;
end;


function psQRPrepare(const s:string; var Data:TpsEncodingPlan; fnc1:TpsFNC1Type):boolean;
var i, idx:Integer;
begin
  Result := True;
  idx := 0;
  i   := 1;
  while i<=Length(s) do begin
      Data.Items[idx].Mode := Integer(psQRSubset(s[i], fnc1));
      //Data.Items[idx].Data := s[i];
      Inc(idx);
      Inc(i);
  end;
end;

function  psQRCodeGetPixels(Barcode:TComponent):Integer;
const ansi_len=10000;
var qr    : TpsQRCode;
    bc    : TpsBarcodeComponent;
    s     : string;
begin
  Result := 1;
  bc := Barcode as TpsBarcodeComponent;
  s  := bc.Barcode;
  if Length(s)=0 then Exit;

  qr := TpsQRCode.Create;
  try
    with bc.Params do begin
      qr.Params       := bc.Params;
      qr.FUsedVersion := QRCode.Version;
      qr.FUsedMode    := QRCode.Mode;
      qr.FMicroQR     := QRCode.MicroQR;
      qr.FUsedMask    := QRCode.Mask;
      qr.FUsedECC     := QRCode.EccLevel;
      qr.FUsedMicroQR := QRCode.MicroQr;
      qr.FNC1         := GS1.FNC1Type;
    end;

    s:=bc.SolveGS1;
    s:=ConvertUnicodeToUTF8(s);

    if qr.Encode(s) then begin
      with bc.Params do begin
        QRCode.FUsedVersion := qr.FUsedVersion;
        QRCode.FUsedECCLevel:= qr.FUsedECC;
        QRCode.FUsedMicroQR := qr.FUsedMicroQR;
        QRCode.FUsedMode    := qr.FUsedMode;
      end;
      qr.Draw((bc as TpsBarcodeComponent).Pixels);
      bc.Params.QRCode.FUsedMask := qr.FUsedMask;
    end;
  finally
    qr.Free;
  end;
end;


function  psPaintQRCode(Value:String; BarcodeObject:TComponent; C:TCanvas; R:TRect):Integer;
var L  : TpsMatrix;
    qr : TpsQRCode;
    bc : TpsBarcodeComponent;
begin
  Result := 1;
  bc := BarcodeObject as TpsBarcodeComponent;
  if Length(bc.Barcode)=0 then Exit;

  qr := TpsQRCode.Create;
  try
    with bc.Params do begin
      qr.Params       := bc.Params;
      qr.FUsedVersion := QRCode.Version;
      qr.FUsedMode    := QRCode.Mode;
      qr.FMicroQR     := QRCode.MicroQR;
      qr.FUsedMask    := QRCode.Mask;
      qr.FUsedECC     := QRCode.EccLevel;
      qr.FUsedMicroQR := QRCode.MicroQr;
    end;

    if qr.Encode(bc.Barcode) then begin
      with bc.Params do begin
        QRCode.FUsedVersion := qr.FUsedVersion;
        QRCode.FUsedEccLevel:= qr.FUsedECC;
        QRCode.FUsedMicroQR := qr.FUsedMicroQR;
        QRCode.FUsedMode    := qr.FUsedMode;
      end;

      L:=TpsMatrix.Create;
      try
        qr.Draw(L);
        bc.Params.QRCode.FUsedMask := qr.FUsedMask;
        L.Paint(C, R, CentreOf(R), bc);
      finally
        L.Free;
      end;
    end;
  finally
    qr.Free;
  end;
end;


{ TQRCode }

procedure TpsQRCode.BarcodeRaise(err: TpsErrRecord);
begin
    TpsBarcodeComponent(Params.BarcodeObject).BarcodeRaise(err);
end;

function TpsQRCode.CalcStreamLength(const s: string; FMicroTmp: Boolean;
  FVersion: Integer; FMode:TpsQRMode): Integer;
var d:Integer;
begin
  d := length(s);
  if FMicroTmp then
      Result := FVersion
  else
      Result := 4;

  Result:=Result + GetNumberOfBitsIndicatorSize(FMicroTmp, FMode, FVersion);
  case FMode of
    QrNumeric: begin
            Inc(Result,10*(d div 3));
            case d mod 3 of
                0 : ;
                1 : Inc(Result, 4);
                2 : Inc(Result, 7);
            end;
       end;
    QrAlphanumeric: Inc(Result, 11*(d div 2) + 6*(d mod 2) );
    QrBytes: Inc(Result, 8*d);
    QrKanji: Inc(Result, 13*(d div 2));
  end;
end;

class function TpsQRCode.CheckVersion(Micro:Boolean; ver:Integer): Boolean;
begin
  Result:= (Micro and (ver in [1..4])) or
  (not Micro and (ver in [1..40]));
end;


procedure TpsQRCode.CreatePlan(s: string; var P: TpsEncodingPlan);
//var i, planIdx:Integer;
begin
{  i := 1;
  PlanIdx := 0 ;
  while i<=Length(s) do begin
  end;
  }
end;

procedure TpsQRCode.Draw(dm: TpsMatrix);
var i : Integer;
begin
  MatrixW := SizeInModules;
  MatrixH := MatrixW;
  dm.InitializeWH(MatrixW, MatrixH);

  // draw finders
  DrawFinder(dm, 3,3);
  if not FMicroQR then begin
      DrawFinder(dm, 3,         MatrixH - 4);
      DrawFinder(dm, MatrixW-4, 3);
  end;

// draw timing pattern
  with dm do
  if FMicroQR then begin
      for i:=8 to MatrixW do
        if i mod 2 = 0 then Point2D(i-1,0,'2')
        else                Point2D(i-1,0,'3');
      for i:=8 to MatrixH do
        if i mod 2 = 0 then Point2D(0,i-1,'2')
        else                Point2D(0,i-1,'3');
  end else begin
    for i:=8 to MatrixW-7 do
        if i mod 2 = 0 then Point2D(i-1,6,'2')
        else                Point2D(i-1,6,'3');
    for i:=8 to MatrixH-7 do
        if i mod 2 = 0 then Point2D(6,i-1,'2')
        else                Point2D(6,i-1,'3');
  end;

  DrawAligns(dm);
  DrawInfos(dm, False);  // with any mask value now
  {$ifdef PSOFT_SETUP}
      if Params.QRCode.DrawData then
  {$endif}
      DrawCW(dm);

  if not (FUsedMask in [0..7]) then
      FUsedMask := OptimizeMask(dm);
  {$ifdef PSOFT_SETUP}
      if Params.QRCode.DrawMask then
  {$endif}
      DrawMask(dm, FUsedMask);

  DrawInfos(dm, True);  // with updated mask value
end;

procedure TpsQRCode.DrawAlign(d2: TpsMatrix; x, y: Integer);
begin
  if (x<10) and (y<10) then exit;        // warning, left top
  if (x<10) and (y>MatrixW-10) then exit; // left bottom
  if (x>MatrixH-10) and (y<10) then exit; // right top

  d2.Point2D(x,y,'3');
  d2.Rect2D(x-1,y-1,x+1,y+1,'2');
  d2.Rect2D(x-2,y-2,x+2,y+2,'3');
end;

procedure TpsQRCode.DrawAligns(d2: TpsMatrix);
var i,j : integer;
begin
  // aligns not used for Micro QR symbology
  if FMicroQR then Exit;
  if not (FUsedVersion in [1..40]) then Exit;

  for i:=1 to 7 do
    for j:=1 to 7 do begin
      if (i=1) and (j=1) then Continue;
      if (QR_AlignTable[FUsedVersion,i]=0) or (QR_AlignTable[FUsedVersion,j]=0) then
          Continue;

      DrawAlign(d2, QR_AlignTable[FUsedVersion,i], QR_AlignTable[FUsedVersion,j]);
    end;
end;

procedure TpsQRCode.DrawFinder(d2: TpsMatrix; x, y: Integer);
var c:char;
begin
  c:='3';
  d2.Rect2D(x-3,y-3,x+3,y+3,c);
  d2.Rect2D(x-1,y-1,x+1,y+1,c);
  d2.Point2D(x,y,c);
  c:='2';
  d2.Rect2D(x-2,y-2,x+2,y+2,c);
  d2.Rect2D(x-4,y-4,x+4,y+4,c);
end;

procedure TpsQRCode.DrawInfos(d2: TpsMatrix; FullData:Boolean);
var s:string;
    c:Char;
    i,x1,x2,y1,y2:Integer;
begin
  // version info, only for QR code version 7 or above
  if not FMicroQR and (FUsedVersion in [7..40]) then begin
    s :=IntegerToBits(iif(FullData, QR_VersionInfo[FUsedVersion], 0) , 18);
    for i:=0 to 17 do begin
      c:=Chr(Ord(s[18-i])+4); // change 0,1 to 4,5  ->4,5 is control chars
      x1 := i div 3;
      y1 := (MatrixH-11) + i mod 3;
      x2 := (MatrixW-11)+i mod 3; y2 := i div 3;
      d2.Point2D(x1,y1, c); // left bottom version info
      d2.Point2D(x2,y2, c); // right top version info
    end;
  end;

  // format info .. 15 bit as string 0110010...
  s := GetFormatInfo(FullData);

  // Dark point for QR Code
  if not FMicroQR then
      d2.Point2D(8,MatrixH-8,'7');

  // draw version info
  for i:=0 to 14 do begin
    c:=Chr(Ord(s[15-i])+6);
    if FMicroQR then
      if i<7 then
          d2.Point2D(8, i+1, c)
      else
          d2.Point2D(15-i, 8, c)
    else begin
      if i<=7 then  begin
          x1:=8;
          x2:=MatrixW-i-1;
          y1:=i;
          if i>=6 then
              Inc(y1,1);
          y2:=8;
        end
      else
        begin
          x1:= iif(i=8, 7, 14-i);
          x2:=8;
          y1:=8;
          y2:=MatrixH-(15-i);
        end;
      d2.Point2D(x1,y1,c);
      d2.Point2D(x2,y2,c);
    end;
  end;
end;

procedure TpsQRCode.DrawCW(d2: TpsMatrix);
var i,x,y   : integer;
    move_up : boolean;
    LeftFromTiming:Integer;
    s:string;

    function GetNextPoint:boolean;
    begin
      Result:=True;
      if move_up then begin
        if x mod 2 = LeftFromTiming then
          Dec(x)
        else begin
          Inc(x);
          Dec(y);
        end;
      end else begin
        if x mod 2 = 1-LeftFromTiming then begin
          Inc(x);
          Inc(y);
        end else begin
          Dec(x);
        end;
      end;

      if (y<0) or (y>=MatrixH) then begin
        move_up := not move_up;
        Dec(x,2);
        if (x=6) and not FMicroQR then begin
          //Dec(x);
          LeftFromTiming:=1-LeftFromTiming;
          move_up := not move_up;
        end;
        if y<0 then y:=0
        else        y:=MatrixH-1;
      end;
    end;
begin
  s:=EncodedData;
  s:=StringReplace(s,' ','',[rfReplaceAll]);
  x:=MatrixW-1;
  y:=MatrixH-1;
  move_up:=true;

  i:=1;
  LeftFromTiming:=0;
  while i<=length(s) do begin
    if (d2.GetPoint2D(x,y)='0') then begin
        d2.Point2D(x,y,s[i]);
        Inc(i);
    end;
    GetNextPoint;
    if x<0 then Break;
  end;
end;



procedure TpsQRCode.DrawMask(d2: TpsMatrix; MaskID: Integer);
var x,y : integer;
    d   : Boolean;
begin
 if not (MaskID in [0..7]) then
      Exit;

  for x:=0 to MatrixW-1 do
    for y:=0 to MatrixH-1 do begin
      if d2.IsFunction(x,y) then Continue;

      d:=False;
      case MaskID of
        0 : d:= ((x+y) mod 2)=0;
        1 : d:= (y mod 2)=0;
        2 : d:= (x mod 3)=0;
        3 : d:= ((x+y) mod 3)=0;
        4 : d:= (((y div 2)+(x div 3)) mod 2)=0;
        5 : d:= ((x*y) mod 2)+ ((x*y) mod 3)=0;
        6 : d:= (((x*y) mod 2)+ ((x*y) mod 3)) mod 2=0;
        7 : d:= (((x+y) mod 2)+ ((x*y) mod 3)) mod 2=0;
      end;
      if d then
        if d2.IsBlack(x,y) then
          d2.Point2D(x,y,'8')
        else
          d2.Point2D(x,y,'9');
          // d2.Point2D(x,y,'9');
    end;
end;

function TpsQRCode.Encode(const ToEncode: String): Boolean;
var dw    : String;
    i,len : Integer;
    good  : boolean;
begin
  Result      := False;
  EncodedData := '';

  if Params.QRCode.Version>0 then begin
      FUsedVersion := Params.QRCode.Version ;
      dw  := EncodeData(ToEncode);
      len := Length(dw) div 8;
      if len=0 then
            Exit;
      // test QRif data capacity is good for this version
      if Len>GetDataCodeWords(FUsedVersion, FMicroQR, FUsedECC) then
          TpsBarcodeComponent(Params.BarcodeObject).BarcodeRaise(erOutOfMaxCapacity);
  end else begin
    // for automatic mode selection, select optimal version
    FUsedVersion := -1;
    Good         := False;
    for i := 1 to MaxVersion(FMicroQR) do begin
      FUsedVersion := i;
      dw  := EncodeData(ToEncode);
      if (Length(dw)=0) or (Length(dw)>TotalDataBits) then
            Continue
      else begin
        good          := True;
        FUsedVersion  := i;
        Break;
      end;
    end;
    if not good then
      // not found, means max. capacity exceeded
      raise psBarcodeException.Create('Max. QR code capacity exceded.');
  end;

  EncodedData := EncodeECC(dw);
  i := RemainderBits(FMicroQR, FUsedVersion);
  EncodedData := EncodedData + StringOfChar('0', i);

  Result := (EncodedData<>'');
end;

function TpsQRCode.EncodeAppend(idx, total, checksum: Integer): String;
var s1:String;
begin
  Result:='';
  if FMicroQR then Exit;
  if (Total in [2..16]) and (idx>=1) and (idx<=Total) then
    s1 := ModeIndicators[indicatorAppend, 1]
        + IntegerToBits( 16*(idx-1)+(Total-1) , 8)
        + IntegerToBits( checksum mod 256 , 8);
end;

function TpsQRCode.EncodePart(const ToEncode:String; currMode: TpsQRMode):String;
var i,j,k  : Integer;
    s,s1   : String;
    k1,k2  : Integer;
    c      : Char;
begin
  i := 1;

  s := GetModeIndicator(FMicroQR, currMode, FUsedVersion);
  s := s+IntegerToBits(Length(ToEncode),
    GetNumberOfBitsIndicatorSize(FMicroQR, currMode, FUsedVersion));

  case currMode of
    QrNumeric     :
        while i<=Length(ToEncode) do begin
            s1:=Copy(ToEncode,i,3);
            k:=0;
            for j:=1 to Length(s1) do
                k:=10*k+Ord(s1[j])-Ord0;
            case Length(s1) of
                1: s:=s+IntegerToBits(k,4);
                2: s:=s+IntegerToBits(k,7);
                3: s:=s+IntegerToBits(k,10);
            end;
            Inc(i,3);
        end;
    QrAlphanumeric:
        while i<=Length(ToEncode) do begin
            c := ToEncode[i];
            if (fnc1<>fnc1None) and (Char(c)=GS1_GroupSeparator) then
                c:='%';
            k1:=System.Pos(c,QR_AlphaTable)-1;
            if k1=-1 then begin
              s:='';
              Exit;
            end;

            Inc(i);
            if i<=Length(ToEncode) then begin
                c:=ToEncode[i];
                if (fnc1<>fnc1None) and (Char(c)=GS1_GroupSeparator) then
                    c:='%';
                k2:=Pos(c,QR_AlphaTable)-1;
                if k2=-1 then begin
                  s:='';
                  Exit;
                end;
                s:=s+IntegerToBits(45*k1+k2, 11);
            end
            else
                s:=s+IntegerToBits(k1,6);
            Inc(i);
        end;
    QrBytes       :
        for i:=1 to Length(ToEncode) do
            s:=s+IntegerToBits(Ord(ToEncode[i]),8);
    QrKanji       : ;
  end;
  Result := s;
end;

function TpsQRCode.EncodeAutomaticExtended(const ToEncode: String): String;
var i:Integer;
begin
  Result :='';
  for I := 1 to Length(ToEncode) do begin

  end;

end;

function TpsQRCode.EncodeAutomaticSimple(const ToEncode:String):String;
var M:TpsQRMode;
begin
  M         := psQRGetSubset(ToEncode, fnc1);
  FUsedMode := M;
  Result    := EncodePart(ToEncode, M);
end;

function TpsQRCode.EncodeData(ToEncode: String): String;
const qrPAD : array[0..1] of String =('00010001', '11101100' );
var s             : String;
    i,padding_cw  : Integer;
//    ErrRecord     : TpsErrRecord;
begin
  Result := '';
  s      := '';
//  InitializeErrorRec(Err);

  // ToEncode := psReplaceHexa(ToEncode);

  if FUsedMode=QrAutomatic then
      s:=s+EncodeAutomaticSimple(ToEncode)
  else begin
        err.Position := psQRCheckSubSet(ToEncode,FUsedMode, FNC1);
        if err.Position>0 then begin
          err.Err := erCharOutOfSet;
          BarcodeRaise(err);
        end;
    s:=EncodePart(ToEncode, FUsedMode);
  end;

  if s='' then Exit;

  // encode ECI if dirrefent than default 000003
  if Params.ECI<>Default_QR_ECI then
      s:=EncodeECI(Params.ECI)+s;
  // add FNC1 if needed
  if Params.GS1.FNC1Type in [fnc1FirstPosition, fnc1GS1] then
      s:=ModeIndicators[indicatorFNC1st,1]+s;
  if Params.GS1.FNC1Type = fnc1SecondPosition then
      s:=ModeIndicators[indicatorFNC2nd,1]+s;

  if Params.SegmentCount>1 then
      s:=EncodeAppend(Params.SegmentIndex, Params.SegmentCount, Params.QRCode.Checksum)+s;

  // add terminator  & align to byte
  AddTerminator(s);

  // add padding codewords
  padding_cw := GetDataCodeWords(FUsedVersion,FMicroQR,FUsedEcc)-(Length(s) div 8);
  if padding_cw<0 then begin
      Result := '';
      Exit;
  end;

  // M1 and M3 last part is only 4 bits
  if FMicroQR and (FUsedVersion in[1,3]) then
    dec(padding_cw);

  // add padding
  for i:=1 to padding_cw do
    s:=s+qrPAD[i mod 2];
  // for M3 add last bits
  if FMicroQR then
      s:=s+StringOfChar('0', TotalDataBits - Length(s));
  Result :=s;
end;

function TpsQRCode.EncodeECC(const data: String): String;
var idx, i, j, r1,d1,e1, r2,d2,e2, _cols : Integer;
    dw, ew, s : String;
    L1,L2 : TStringList;

begin
  i:=FUsedVersion;
  j:=1;
  if not FMicroQR then
    Inc(i,4);

  case FUsedECC of
    QrEccLevelL : j:=1;
    QrEccLevelM : j:=3;
    QrEccLevelQ : j:=5;
    QrEccLevelH : j:=7;
  end;

{ ri = block count, d1 = data codewords in clock, e1 = error codewords
 r2,d2,e2 = second part ...}
  r1 := QR_ECC[i][j][2];
  d1 := QR_ECC[i][j][4];
  e1 := QR_ECC[i][j][3]-d1;

  r2 := QR_ECC[i][j+1][2];
  d2 := QR_ECC[i][j+1][4];
  e2 := QR_ECC[i][j+1][3]-d2;

  ew  := '';
  idx := 1;

  L1:=tStringList.Create;
  L2:=tStringList.Create;
  try
    for i:=1 to r1 do begin
      s:=Copy(data,idx,d1*8);
      L1.Add(s);
      L2.Add(qr_ReedSolomonCalc(s,True,e1));
      Inc(idx,d1*8);
    end;
    for i:=1 to r2 do begin
      s:=Copy(data,idx,d2*8);
      L1.Add(s);
      L2.Add(qr_ReedSolomonCalc(s,True, e2));
      Inc(idx,d2*8);
    end;

    _cols := psMax(d1,d2);
    dw    := '';
    for i := 1 to _cols do
      for j := 0 to L1.Count - 1 do
         dw:=dw+String(Copy(L1.Strings[j], i*8 - 7, 8));

    ew := '';
    _cols := psMax(e1,e2);
    for i := 1 to _cols do
      for j := 0 to L2.Count - 1 do
         ew:=ew+String(Copy(L2.Strings[j],i*8 - 7, 8));
  finally
    L1.Free;
    L2.Free;
  end;

  // here in dw and ew are data codewords and error codewords
  // now we finalize complete message
  {$ifdef PSOFT_DEBUG}
      psStringToFile(psCRLF+parsedString(ew,8,psCRLF),'d:\ew.txt');
  {$endif}
  // add data codewords to final message
//  dw := AddToFinalQR(data,r1,d1,r2,d2);
//  ew := AddToFinalQR(ew,r1,e1,r2,e2);
  {$ifdef PSOFT_DEBUG}
      psStringToFile(psCRLF+parsedString(dw,8,psCRLF),'d:\dw_final.txt');
      psStringToFile(psCRLF+parsedString(ew,8,psCRLF),'d:\ew_final.txt');
  {$endif}
  Result := dw+ew;
end;

function TpsQRCode.EncodeECI(value: Integer): String;
var s1:String;
begin
  Result:='';
  if FMicroQR or (Value<0) or (Value>999999) then
      Exit;
  s1 := ModeIndicators[indicatorECI, 1];
  if Value<=127 then
      Result:= s1 + '0'+ IntegerToBits(Value, 7)
  else
    if Value<=16383  then
        Result:= s1 + '10'+ IntegerToBits(Value, 14)
    else
      if Value<=999999 then
        Result := s1 + '110'+ IntegerToBits(Value, 21);
end;


function TpsQRCode.EvaluateMask(d2: TpsMatrix):Integer;
var x,y:Integer;
    cnt:Integer;
    k1,k2:Integer;
begin
  Result  := 0;
  k1      := 0;
  k2      := 0;

  if FMicroQR then begin
      for x := 1 to MatrixW - 1 do
        if d2.IsBlack(x,MatrixH-1) then
          Inc(k1);
      for y := 1 to MatrixH - 1 do
        if d2.IsBlack(MatrixW-1,y) then
          Inc(k2);
      if k1>k2 then
        Result := k2 + 16*k1
      else
        Result := k1 + 16*k2;
      Result := -Result;
      Exit;
  end;;

  // step 1 horizontally
  for x := 0 to MatrixW- 1 do begin
    cnt := 0;
    for y := 0 to MatrixH - 1 do begin
        if (y>0) and (d2.IsBlack(x,y)=d2.IsBlack(x,y-1)) then
            Inc(cnt)
        else begin
            if cnt>=5 then
                Inc(Result, 3 + cnt-5);
            cnt := 1;
        end;
    end;
    if cnt>=5 then
        Inc(Result, 3 + cnt-5);
  end;

  // step 1 vertically
  for x := 0 to MatrixW- 1 do begin
    cnt := 0;
    for y := 0 to MatrixH - 1 do begin
        if (y>0) and (d2.IsBlack(y,x)=d2.IsBlack(y-1,x)) then
            Inc(cnt)
        else begin
            if cnt>=5 then
                Inc(Result, 3 + cnt-5);
            cnt := 1;
        end;
    end;
    if cnt>=5 then
        Inc(Result, 3 + cnt-5);
  end;

  // step 2 count squares 2x2
  for x := 0 to MatrixW - 2 do
    for y := 0 to MatrixH - 2 do begin
       if (d2.IsBlack(x,y)=d2.IsBlack(x+1,y))
          and (d2.IsBlack(x,y)=d2.IsBlack(x,y+1))
          and (d2.IsBlack(x,y)=d2.IsBlack(x+1,y+1))
          then
              Inc(Result,3);
    end;

  // step 3
  // mask dot, stace, dot,dot,dot, space, dot vertically
  for x := 0 to MatrixW - 1 do
    for y := 0 to MatrixH - 7 do begin
      if ((y=0) or not d2.IsBlack(x,y-1)) and
        (d2.IsBlack(x,y+0)=True) and
        (d2.IsBlack(x,y+1)=False) and
        (d2.IsBlack(x,y+2)=True) and
        (d2.IsBlack(x,y+3)=True) and
        (d2.IsBlack(x,y+4)=True) and
        (d2.IsBlack(x,y+5)=False) and
        (d2.IsBlack(x,y+6)=True) and
        ( (y=MatrixH-7) or not d2.IsBlack(x,y+7) )
          then
            Inc(Result, 40);
    end;

  // step 3
  // mask dot, stace, dot,dot,dot, space, dot horizontally
  for x := 0 to MatrixW - 1 do
    for y := 0 to MatrixH - 7 do begin
      if ((y=0) or not d2.IsBlack(y-1,x)) and
        (d2.IsBlack(y+0,x)=True) and
        (d2.IsBlack(y+1,x)=False) and
        (d2.IsBlack(y+2,x)=True) and
        (d2.IsBlack(y+3,x)=True) and
        (d2.IsBlack(y+4,x)=True) and
        (d2.IsBlack(y+5,x)=False) and
        (d2.IsBlack(y+6,x)=True) and
        ( (y=MatrixH-7) or not d2.IsBlack(y+7,x) )
          then
            Inc(Result, 40);
    end;

  // step 4
  k1 := MatrixH*MatrixW;
  k2 := 0;
  for x := 0 to MatrixW - 1 do
    for y := 0 to MatrixH - 1 do
      if d2.IsBlack(x,y) then
        Inc(k2);

  Inc(Result ,  10*Trunc( (Abs(MulDiv(k2,100,k1)-50) div 5) ) );
end;

function TpsQRCode.FNC1First: Boolean;
begin
    //Result := (not FMicroQR) and (Params.FNC1_Type);
    Result := False;
end;

function TpsQRCode.GetModeIndicator(micro:boolean; currMode:TpsQrMode; ver:Integer): String;
var x,y : integer;
begin
  x := iif(micro, 1+ver, 1);
  case currMode of
    QrNumeric       : y:=2;
    QrAlphanumeric  : y:=3;
    QrBytes         : y:=4;
    QrKanji         : y:=5;
    else              y:=4;
  end;

  Result := ModeIndicators[y,x];
end;


class function TpsQRCode.GetTotalCodeWords(ver:Integer; Micro:Boolean): Integer;
var idx:integer;
begin
  idx:=ver;
  if not Micro then Inc(idx,4);
  Result := QR_Capacity_TotalCW[idx];
end;

class function TpsQRCode.MaxVersion(Micro: Boolean): Integer;
begin
  Result := iif(Micro, 4, 40);
end;

function TpsQRCode.OptimizeMask(d2: TpsMatrix):Integer;
var i: Integer;
    BestMask, BestMaskScore, MaskScore : Integer;
begin
  BestMask      := -1;
  BestMaskScore := 0;
  for i:=0 to 7 do begin
      if FMicroQR and not (i in [1,4,6,7]) then
        Continue;

      d2.Backup;
      DrawMask(d2, i);

      MaskScore := EvaluateMask(d2);
      d2.Restore;

      if (BestMask=-1) or (MaskScore<BestMaskScore) then begin
          BestMaskScore := MaskScore;
          BestMask      := i;
      end;
  end;
  Result := BestMask;
end;


class function TpsQRCode.RemainderBits(Micro: Boolean; Ver: Integer): Integer;
begin
    Result := 0;
    if not Micro then
      case ver of
         2.. 6 : Result := 7;
        14..20 : Result := 3;
        21..27 : Result := 4;
        28..34 : Result := 3;
      end;
end;

class function TpsQRCode.GetDataCodeWords(ver:Integer; Micro:Boolean; ecc:TpsQREccLevel): Integer;
begin
  if Micro then
    Result := QR_Capacity_DataCW_Micro[ver, ecc]
  else
    Result := QR_Capacity_DataCW[ver,ecc];
end;

function TpsQRCode.GetFormatInfo(FullData:Boolean): string;
var idx:integer;
begin
  if not FullData then begin
    Result := IntegerToBits(0,15);
    Exit;
  end;

  idx:=1;
  if FMicroQR then begin
    case FUsedVersion of
      1 : idx:=0;
      2 : idx:=iif(FUsedECC=qrEccLevelL,1,2);
      3 : idx:=iif(FUsedECC=qrEccLevelL,3,4);
      { TODO : toto preverit bo je to debilina }
      4 : if FUsedECC=qrEccLevelL then
            idx:=5
          else
            if FUsedECC=qrEccLevelM then
              idx:=6
          else
              idx:=7;
    end;
    Idx := idx shl 2;
    case FUsedMask of
      1 : ;
      4 : Inc(idx,1);
      6 : Inc(idx,2);
      7 : Inc(idx,3);
    end;
    Result := IntegerToBits(QR_FormatInfoMicro[idx],15);
  end else begin
    case FUsedECC of
      QrEccLevelL : idx:=1;
      QrEccLevelM : idx:=0;
      QrEccLevelQ : idx:=3;
      QrEccLevelH : idx:=2;
    end;
    if FUsedMask<0 then
        FUsedMask := 0;
    idx := (idx shl 3)+FUsedMask;
    Result := IntegerToBits(QR_FormatInfo[idx],15);
  end;
end;




function TpsQRCode.SizeInModules: Integer;
begin
  Result := -1;
  if not CheckVersion(FMicroQR, FUsedVersion) then
    Exit;

  if FMicroQR then
    Result := 9+2*FUsedVersion
  else
    Result := 17 + 4*FUsedVersion;
end;

function TpsQRCode.TotalDataBits: Integer;
begin
    if FMicroQR then
      Result := QR_Micro_Bits[FUsedVersion, FUsedECC]
    else
      Result := 8*GetDataCodeWords(FUsedVersion, FMicroQR, FUsedECC);
end;

function TpsQRCode.GetNumberOfBitsIndicatorSize(mqr:Boolean; currMode:TpsQrMode; ver:Integer): Integer;
const bisTable: array[1..7,QrNumeric..QrKanji] of integer =
      ( (3,0,0,0),  (4,3,0,0),  (5,4,4,3), (6,5,5,4),
        (10,9,8,8), (12,11,16,10), (14,13,16,12) );
begin
  Result := 0;
  if mqr then begin
    if ver in[1..4] then
      Result := bisTable[ver, CurrMode];
  end
  else
    case ver of
       1..9 : result:=bisTable[5,currMode];
      10..26: result:=bisTable[6,currMode];
      27..40: result:=bisTable[7,currMode];
  end;
end;

procedure TpsQRCode.AddTerminator(var s:String);
var terminator      : string;
    bits,remainBits : Integer;
begin
  if FMicroQR then
    Terminator := '0'+StringOfChar('0',2*FUsedVersion)
  else
    Terminator := '0000';

  bits := Length(s)+Length(Terminator);
  if bits<=TotalDataBits then
    s:=s+Terminator;
  // byte align
  RemainBits := 8-(Length(s) mod 8);
  if RemainBits=8 then
      RemainBits :=0;
  if (RemainBits>0) then begin
      if FMicroQR then begin
          case FUsedVersion of
             // no padding cw possible
             1 : s:=s+StringOfChar('0',TotalDataBits-Length(s));
             2 : s:=s+StringOfChar('0',RemainBits);
              // some PAD can be added
             3 : if TotalDataBits-Length(s)>=12 then
                     s:=s+StringOfChar('0',RemainBits)
                 else
                    // no PAD, finalize message
                    s:=s+StringOfChar('0',TotalDataBits-Length(s));
             4 : s:=s+StringOfChar('0',RemainBits);
          end;
      end
      else
          s:=s+StringOfChar('0',RemainBits);
  end;
end;



procedure TpsQRParams.Assign(Source: TPersistent);
var v:TpsQRParams;
begin
  if Source is TpsQRParams then begin
      v         := TpsQRParams(Source);
      FEccLevel := v.EccLevel;
      FMode     := v.Mode;
      FMicroQR  := v.MicroQR;
      FVersion  := v.Version;
      FMask     := v.Mask;
      FFileName     := V.FileName;
      FChecksum     := V.Checksum;
      UpdateBarcode;
  end;
end;

constructor TpsQRParams.CreateBarcode(ParentEAN: TComponent);
begin
  inherited Create;
  FBarcode  := ParentEAN;
  FVersion  := 0;
  FECCLevel := QrEccLevelM;
  FMode     := QrAutomatic;
  FMicroQR  := False;
end;


procedure TpsQRParams.SetChecksum(const Value: Integer);
begin
    if FCheckSum<>Value then begin
        FCheckSum:=Value;
        UpdateBarcode;
    end;
end;

procedure TpsQRParams.SetEccLevel(const Value: TpsQrEccLevel);
begin
  if FEccLevel<>Value then begin
    FEccLevel := Value;
    UpdateBarcode;
  end;
end;

procedure TpsQRParams.SetFileName(const Value: String);
begin
    if FFileName<>Value then begin
        FFileName:=Value;
        UpdateBarcode;
    end;
end;

procedure TpsQRParams.SetMask(const Value: Integer);
begin
  if FMask<>Value then begin
    if (Value=-1) or (FMicroQR and (Value in [1,4,6,7])) or ((not FMicroQR) and (Value in [0..7])) then
    begin
      FMask := Value;
      UpdateBarcode;
    end else
      raise psBarcodeException.Create('QR code bad mask value');
  end;
end;

procedure TpsQRParams.SetMicroQR(const Value: Boolean);
begin
  if FMicroQR<>Value then begin
    FMicroQR := Value;
    if FVersion>4 then
        FVersion := 4;
    UpdateBarcode;
  end;
end;

procedure TpsQRParams.SetMode(const Value: TpsQrMode);
begin
  if FMode<>Value then begin
    FMode := Value;
    UpdateBarcode;
  end;
end;

procedure TpsQRParams.SetVersion(const Value: Integer);
var ok:boolean;
begin
  if FVersion<>Value then begin
    ok := (FMicroQR and (Value in [0..4])) or (not FMicroQR and (Value in [0..40]));
    if ok then begin
        FVersion := Value;
        UpdateBarcode;
    end else
      ;
  end;
end;

procedure TpsQRParams.UpdateBarcode;
begin
    psUpdateBarcode(FBarcode);
end;


function  qrECCDescription(ecc:TpsQrEccLevel):string;
begin
  Result := qrECCDescriptions[ecc];
end;

function  qrModeDescription(ecc:TpsQrMode):string;
begin
  Result := qrModeDescriptions[ecc];
end;


procedure TpsDataMatrixParams.Assign(Source: TPersistent);
var v:TpsDataMatrixParams;
begin
  if Source is TpsDataMatrixParams then begin
      v := TpsDataMatrixParams(Source);
      FEncoding := v.Encoding;
      FVersion  := v.Version;
      UpdateBarcode;
  end;
end;

constructor TpsDataMatrixParams.CreateBarcode(AOwner: TComponent);
begin
  inherited Create;
  FBarcode:=AOwner;
end;

procedure TpsDataMatrixParams.SetEncoding(
  const Value: TpsDataMatrixEncoding);
begin
  if FEncoding<>Value then begin
    FEncoding := Value;
    UpdateBarcode;
  end;
end;

procedure TpsDataMatrixParams.SetVersion(
  const Value: TpsDataMatrixVersion);
begin
  if FVersion<>Value then begin
    FVersion:= Value;
    UpdateBarcode;
  end;
end;

procedure TpsDataMatrixParams.UpdateBarcode;
begin
    psUpdateBarcode(FBarcode);
end;


var psReedSolomon : TpsReedSolomon = nil;  // internal use

procedure TpsReedSolomon.GenerateGF(poly:integer);
var m,b,p,v:integer;
begin
  b:=1;
  m:=0;
  while (b<=poly) do begin
    Inc(m);
    b:=b shl 1;
  end;
  b:=b shr 1;
  Dec(m);

  gfpoly  := poly;
  symsize := m;
  logmod  := (1 shl m)-1;

  SetLength(log,  logmod+1);
  SetLength(alog, logmod);

  p:=1;
  for v:=0 to logmod-1 do begin
    alog[v] := p;
    log[p]  := v;
    p:= p shl 1;
    if (p and b)<>0 then
        p:=p xor poly;
  end;
end;


procedure TpsReedSolomon.GeneratePoly(nsym,index:integer);
var i,k:integer;
begin
  if nsym=symsize then Exit;

  SetLength(rspoly, nsym+1);
  rlen   := nsym;
  _index := index;
  rspoly[0]:=1;
  for i:=1 to nsym do begin
    rspoly[i]:=1;
    for k:=i-1 downto 1 do begin
      if rspoly[k]<>0 then
        rspoly[k]:=alog[(log[rspoly[k]]+index) mod logmod];
      rspoly[k] := rspoly[k] xor rspoly[k-1];
    end;
    rspoly[0]:=alog[(log[rspoly[0]]+index) mod logmod];
    Inc(index);
  end;
end;

procedure TpsReedSolomon.Encode(const data:array of byte; InLen:Integer;
  var outData:array of byte; OutLen:integer);
var i,k,m:integer;
begin
  // reset result array
  // ZeroMemory(OutData, SizeOf(Byte)*OutLen);
  for i:=0 to OutLen-1 do
    outData[i]:=0;

  for i:=0 to InLen-1 do begin
    m:=outData[OutLen-1] xor data[i];
    for k:=OutLen-1 downto 1 do begin
      if (m<>0) and (rspoly[k]<>0) then
        outData[k]:= byte ( outData[k-1] xor
          alog[(log[m]+log[rspoly[k]]) mod logmod])
      else
        outData[k]:=outData[k-1];
    end;
    if (m<>0) and (rspoly[0]<>0) then
      outData[0]:=byte (alog[(log[m]+log[rspoly[0]]) mod logmod])
    else
      outData[0] := 0;
  end;
end;

procedure TpsReedSolomon.Encode(const data:array of byte;
        var outData:array of byte);
begin
    Encode(data, Length(data), outData, Length(outdata));

end;

constructor TpsReedSolomon.Create(poly: integer);
begin
  inherited Create;
  gfpoly  := -1;
  rlen    := -1;
  _index  := -1;
  symsize := -1;
//   Poly := 285;        { magic value for QR CODE };
  GenerateGF(poly);
end;

procedure EncodeRS(poly, nsym, index:integer);
begin
  if not Assigned(psReedSolomon) then
    psReedSolomon:=TpsReedSolomon.Create(poly);
  if (psReedSolomon.gfpoly<>poly) or (psReedSolomon._index<>index)then
    psReedSolomon.GenerateGF(poly);
  if (psReedSolomon.rlen<>nsym) or (psReedSolomon._index<>index)then
    psReedSolomon.GeneratePoly(nsym, index);
end;


procedure TpsAztecParams.Assign(Source: TPersistent);
var v:TpsAztecParams;
begin
    if Source is TpsAztecParams then begin
        v:=TpsAztecParams(Source);
        FCompact    := v.Compact;
        FVersion    := v.Version;
        FErrorLevel := v.ErrorLevel;
        UpdateBarcode;
    end;
end;

constructor TpsAztecParams.CreateBarcode(ParentEAN: TComponent);
begin
  inherited Create;
  FBarcode    := ParentEAN;
  FCompact    := False;
  FErrorLevel := azECC23;
end;

procedure TpsAztecParams.SetCompact(const Value: Boolean);
begin
  if FCompact<>Value then begin
    FCompact := Value;
    FVersion := 0;
    UpdateBarcode;
  end;
end;

procedure TpsAztecParams.SetErrorLevel(const Value: TpsAztecECC);
begin
  if FErrorLevel<>Value then begin
    FErrorLevel := Value;
    UpdateBarcode;
  end;
end;

procedure TpsAztecParams.SetVersion(const Value: TpsAztecVersion);
begin
  if FVersion<>Value then begin
    if not (Value in [0..12]) then begin
      raise psBarcodeException.Create('Aztec barcode version must be in 0..36');
      // Exit;
    end;

    if FCompact and not (Value in [0..4]) then begin
      raise psBarcodeException.Create('Compact Aztec barcode version must be in 0..4');
      //exit;
    end;

    FVersion := Value;
    UpdateBarcode;
  end;
end;

procedure TpsAztecParams.UpdateBarcode;
begin
  psUpdateBarcode(FBarcode);
end;




initialization

  // create and initialize ReedSolomon object
  dmReedSolomon := TpsReedSolomon.Create(301);
  dmReedSolomon.GeneratePoly(5,1);

finalization

  FreeAndNil(dmReedSolomon);

end.
