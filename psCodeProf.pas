unit psCodeProf;

interface

{$I psBarcode.inc}

uses Classes, Windows, VCL.Graphics, SysUtils, DateUtils,
  psCodeRes, psTypes, psCodeFNLite, psCodeExceptions;

type
        longword4 = array[0..3] of longword;

        TInteger5 = array[0..4] of Integer;
        TByte6    = array[0..5] of Byte;

{ TODO : dopracovat ak pdf417 ma cols i rows=0 }

// ****************************************************************************
// *** procedured/functions for work with PDF417
// ****************************************************************************

    TpsPDF417Mode = (psPDF417Alphanumeric, psPDF417Binary, psPDF417BinaryHex, psPDF417Numeric, psPDF417AutoCode);

    TpsPDF417ErrorCorrection = (psPDF417AutoEC,
        psPDF417Error0, psPDF417Error1, psPDF417Error2,
        psPDF417Error3, psPDF417Error4, psPDF417Error5,
        psPDF417Error6, psPDF417Error7, psPDF417Error8 );
    TpsPDF417Kind = (pkStandard, pkTruncated, pkMicro);

    TpsPDF417Params=class(TPersistent)
    private
      FBarcode        : TComponent;
      FMode           : TpsPDF417Mode;
      FRows           : Integer;
      FCols           : Integer;
      FSecurityLevel  : TpsPDF417ErrorCorrection;
      FFileSize       : Integer;
      FSender         : String;
      FAddresse       : string;
      FTimeStamp      : TDateTime;
      FKind           : TpsPDF417Kind;
      FUseMacro       : Boolean;
      FFileName       : String;
      FCheckSum       : Integer;
      FUsedRows       : Integer;
      FUsedCols       : Integer;
      FUsedECL        : TpsPDF417ErrorCorrection;
      procedure       UpdateBarcode;
    protected
      procedure SetMode(Value:TpsPDF417Mode);
      procedure SetSecurityLevel(Value:TpsPDF417ErrorCorrection);
      procedure SetRows(Value:Integer);
      procedure SetCols(Value:Integer);
      procedure SetAddresse(const Value: string);
      procedure SetFileSize(const Value: Integer);
      procedure SetSender(const Value: String);
      procedure SetTimeStamp(const Value: TDateTime);
      procedure SetKind(Value:TpsPDF417Kind);
      procedure SetFileName(const Value: String);
      procedure SetChecksum(const Value: Integer);
      procedure SetUseMacro(Value: Boolean);
    public
      constructor CreateBarcode(AOwner:TComponent);
      procedure   Assign(Source: TPersistent); override;

      function    BarcodeComponent:TComponent;
      //function  OptionalFieldCount:Integer;
    published
      property  Mode:TpsPDF417Mode Read FMode Write SetMode;
      property  Rows:Integer Read FRows Write SetRows Default 0;
      property  Cols:Integer Read FCols Write SetCols Default 10;
      property  SecurityLevel: TpsPDF417ErrorCorrection Read FSecurityLevel Write SetSecurityLevel;
      property  TimeStamp:TDateTime read FTimeStamp write SetTimeStamp;
      property  Sender:String read FSender write SetSender;
      property  Addresse:string read FAddresse write SetAddresse;
      property  FileSize:Integer read FFileSize write SetFileSize;
      property  Kind:TpsPDF417Kind read FKind write SetKind;
      property  FileName:String read FFileName write SetFileName;
      property  Checksum:Integer read FChecksum write SetChecksum;
      property  UseMacro:Boolean read FUseMacro write SetUseMacro;
      property  UsedCols:Integer read FUsedCols stored False;
      property  UsedRows:Integer read FUsedRows stored False;
      property  UsedECL :TpsPDF417ErrorCorrection read FUsedECL stored False;
    end;

    procedure psPDF417GetLines(bc:TComponent; L:TStringList);

//    procedure psPDF417GetPixels(S:String; var Cols, Rows:Integer;
//          BarcodeObject:TComponent);

// ****************************************************************************
// *** HugeInt functions - 128 bit arithmetic - need in IntelligentMailbarcode
// *** used in PDF417 for increase spped ...
// ****************************************************************************
type
    Hugeint = packed array [0..3] of longword;

    function LongWordToHuge(const x: Longword): Hugeint; overload;
    function Int64ToHuge(const x: Int64): Hugeint; overload;
    function LongIntToHuge(const x: Integer): Hugeint; overload;
    function HugeAdd(x: Hugeint; y: Hugeint): Hugeint;
    function MultiplyWithCarry(x: Longword; y: Longword; var Carry: Longword): LongWord;
    function HugeMulInt(x: Hugeint; y: Longword): Hugeint;
    function HugeGetBit(h:HugeInt; bit:Integer):Integer;
    function DivideWithRemainder(x: Longword; y: Longword; var Remainder: Longword): Longword;
    function HugeDivInt(x: Hugeint; y: Longword): Hugeint;
    function HugeModInt(dividend: Hugeint; divisor: Longint): Longint;

// **************************************************************************
// *** Telepen barcode
// **************************************************************************
    function Telepen(ck:String):string;
    function TelepenNumeric(ck:String):string;

// **************************************************************************
// *** IntelligentMail
// **************************************************************************
    function IntelligentMailGetLines(ToEncode:String):String;
    function IntelligentMailCheck(ToEncode:String; BC:TObject):Boolean;

// **************************************************************************
// *** Australia Post barcode
// **************************************************************************
    function AustraliaPostGetLines(var ToEncode:String; ShowChecksum:Boolean):String;
    function AustraliaPostCheck(ToEncode:String; BC:TObject):Boolean;

// **************************************************************************
// *** Postbar , CPC 4-state
// **************************************************************************
    function  CPC4State(ck:string):string;
    function  CPC4StateCheck(var ck:string):Boolean;

// **************************************************************************
// **** Used for encoding Code16K                                         ***
// **************************************************************************



// **************************************************************************
// **** bcChromoCode - color barcode symbology                            ***
// **************************************************************************
    function  EncodeChromocode(E:TComponent):Boolean;


//    function pdf417MicroComplete(L:TStrings; cols,rows:Integer):Boolean;


procedure PaintStacked(C:TCanvas; R:TRect; bc:TComponent);


implementation

uses Math, psBarcodeComp;


procedure DrawLines(C:TCanvas; R:TRect; const s:String);
var i,x1,x2:Integer;
begin
        i  := 1;
        x1 := R.Left;
        x2 := R.Left;
        while i<=Length(s) do begin
                while (i<=Length(s)) and (s[i]<>'0') do begin
                        Inc(x2);
                        Inc(i);
                end;
                C.Rectangle(x1, R.Top, x2, R.Bottom);

                x1:=x2;
                while (i<=Length(s)) and (s[i]='0') do begin
                        Inc(x1);
                        Inc(i);
                end;
                x2:=x1;
        end;
end;

procedure PaintStacked(C:TCanvas; R:TRect; bc:TComponent);
var E                  : TpsBarcodeComponent;
    L                  : TStringList;
    s                  : String;
    R1                 : TRect;
    i, rows            : Integer;
    w, counter,LastTop : Integer;
begin
    E := bc as TpsBarcodeComponent;
    L := E.Bars;

    if L.Count>0 then
        if Length(L[0])>WidthOf(R) then
              if not (boPaintIfSmall in E.Options) then
                  E.BarcodeRaise(erSmallPaintBox);

    with C do begin
        with C.Pen do begin
            Color := E.LinesColor;
            Width := 1;
            Mode  := pmCopy;
            Style := psSolid;
        end;
        Brush.Style := bsSolid;
        Brush.Color := E.LinesColor;
    end;

    counter := 0;
    LastTop := R.Top;
    Rows    := L.Count;

    for i:=0 to L.Count-1 do begin
        // this is "height zoom" algorithm
        Dec(Counter, R.Bottom-R.Top);
        w:=0;
        while Counter<0 do begin
            { TODO : dat kontrolu ci rows moze byt 0 }
            Inc(Counter, rows);
            Inc(w);
        end;
        s:=E.Zoom(L[i], WidthOf(R), boSecurity in E.Options);
        R1.Left  := R.Left;
        R1.Right := R.Right;
        R1.Top   := LastTop;
        R1.Bottom:= LastTop+w;
        LastTop:=R1.Bottom;
        DrawLines(C,R1,s);
    end;
end;




const

// --------------------------------------------------------------
// for Australia post bar code symbology
// --------------------------------------------------------------
       MAX_TABLE      = 64;
       MAX_GEN        =  5;
       FCC_37_CUST    = '11';
       FCC_37_ROUT    = '87';
       FCC_37_REPL    = '45';
       FCC_37_REDI    = '92';
       FCC_52_FF_MET  = '59';
       FCC_67_FF_MET  = '62';
       FCC_67_FF_MAN  = '44';
       BC_START_SYM   = '13';
       BC_STOP_SYM    = '13';



// --------------------------------------------------------------
// for PDF417 and Micro PDF417 bar code symbology
// --------------------------------------------------------------
	Pdf417_Start	= '11111111010101000';
	Pdf417_Stop   = '111111101000101001';

  Pdf417Bits : array[0..2,0..928] of Word =
            ( ( $d5c0, $eaf0, $f57c, $d4e0, $ea78, $f53e, $a8c0, $d470,
                $a860, $5040, $a830, $5020, $adc0, $d6f0, $eb7c, $ace0,
                $d678, $eb3e, $58c0, $ac70, $5860, $5dc0, $aef0, $d77c,
                $5ce0, $ae78, $d73e, $5c70, $ae3c, $5ef0, $af7c, $5e78,
                $af3e, $5f7c, $f5fa, $d2e0, $e978, $f4be, $a4c0, $d270,
                $e93c, $a460, $d238, $4840, $a430, $d21c, $4820, $a418,
                $4810, $a6e0, $d378, $e9be, $4cc0, $a670, $d33c, $4c60,
                $a638, $d31e, $4c30, $a61c, $4ee0, $a778, $d3be, $4e70,
                $a73c, $4e38, $a71e, $4f78, $a7be, $4f3c, $4f1e, $a2c0,
                $d170, $e8bc, $a260, $d138, $e89e, $4440, $a230, $d11c,
                $4420, $a218, $4410, $4408, $46c0, $a370, $d1bc, $4660,
                $a338, $d19e, $4630, $a31c, $4618, $460c, $4770, $a3bc,
                $4738, $a39e, $471c, $47bc, $a160, $d0b8, $e85e, $4240,
                $a130, $d09c, $4220, $a118, $d08e, $4210, $a10c, $4208,
                $a106, $4360, $a1b8, $d0de, $4330, $a19c, $4318, $a18e,
                $430c, $4306, $a1de, $438e, $4140, $a0b0, $d05c, $4120,
                $a098, $d04e, $4110, $a08c, $4108, $a086, $4104, $41b0,
                $4198, $418c, $40a0, $d02e, $a04c, $a046, $4082, $cae0,
                $e578, $f2be, $94c0, $ca70, $e53c, $9460, $ca38, $e51e,
                $2840, $9430, $2820, $96e0, $cb78, $e5be, $2cc0, $9670,
                $cb3c, $2c60, $9638, $2c30, $2c18, $2ee0, $9778, $cbbe,
                $2e70, $973c, $2e38, $2e1c, $2f78, $97be, $2f3c, $2fbe,
                $dac0, $ed70, $f6bc, $da60, $ed38, $f69e, $b440, $da30,
                $ed1c, $b420, $da18, $ed0e, $b410, $da0c, $92c0, $c970,
                $e4bc, $b6c0, $9260, $c938, $e49e, $b660, $db38, $ed9e,
                $6c40, $2420, $9218, $c90e, $6c20, $b618, $6c10, $26c0,
                $9370, $c9bc, $6ec0, $2660, $9338, $c99e, $6e60, $b738,
                $db9e, $6e30, $2618, $6e18, $2770, $93bc, $6f70, $2738,
                $939e, $6f38, $b79e, $6f1c, $27bc, $6fbc, $279e, $6f9e,
                $d960, $ecb8, $f65e, $b240, $d930, $ec9c, $b220, $d918,
                $ec8e, $b210, $d90c, $b208, $b204, $9160, $c8b8, $e45e,
                $b360, $9130, $c89c, $6640, $2220, $d99c, $c88e, $6620,
                $2210, $910c, $6610, $b30c, $9106, $2204, $2360, $91b8,
                $c8de, $6760, $2330, $919c, $6730, $b39c, $918e, $6718,
                $230c, $2306, $23b8, $91de, $67b8, $239c, $679c, $238e,
                $678e, $67de, $b140, $d8b0, $ec5c, $b120, $d898, $ec4e,
                $b110, $d88c, $b108, $d886, $b104, $b102, $2140, $90b0,
                $c85c, $6340, $2120, $9098, $c84e, $6320, $b198, $d8ce,
                $6310, $2108, $9086, $6308, $b186, $6304, $21b0, $90dc,
                $63b0, $2198, $90ce, $6398, $b1ce, $638c, $2186, $6386,
                $63dc, $63ce, $b0a0, $d858, $ec2e, $b090, $d84c, $b088,
                $d846, $b084, $b082, $20a0, $9058, $c82e, $61a0, $2090,
                $904c, $6190, $b0cc, $9046, $6188, $2084, $6184, $2082,
                $20d8, $61d8, $61cc, $61c6, $d82c, $d826, $b042, $902c,
                $2048, $60c8, $60c4, $60c2, $8ac0, $c570, $e2bc, $8a60,
                $c538, $1440, $8a30, $c51c, $1420, $8a18, $1410, $1408,
                $16c0, $8b70, $c5bc, $1660, $8b38, $c59e, $1630, $8b1c,
                $1618, $160c, $1770, $8bbc, $1738, $8b9e, $171c, $17bc,
                $179e, $cd60, $e6b8, $f35e, $9a40, $cd30, $e69c, $9a20,
                $cd18, $e68e, $9a10, $cd0c, $9a08, $cd06, $8960, $c4b8,
                $e25e, $9b60, $8930, $c49c, $3640, $1220, $cd9c, $c48e,
                $3620, $9b18, $890c, $3610, $1208, $3608, $1360, $89b8,
                $c4de, $3760, $1330, $cdde, $3730, $9b9c, $898e, $3718,
                $130c, $370c, $13b8, $89de, $37b8, $139c, $379c, $138e,
                $13de, $37de, $dd40, $eeb0, $f75c, $dd20, $ee98, $f74e,
                $dd10, $ee8c, $dd08, $ee86, $dd04, $9940, $ccb0, $e65c,
                $bb40, $9920, $eedc, $e64e, $bb20, $dd98, $eece, $bb10,
                $9908, $cc86, $bb08, $dd86, $9902, $1140, $88b0, $c45c,
                $3340, $1120, $8898, $c44e, $7740, $3320, $9998, $ccce,
                $7720, $bb98, $ddce, $8886, $7710, $3308, $9986, $7708,
                $1102, $11b0, $88dc, $33b0, $1198, $88ce, $77b0, $3398,
                $99ce, $7798, $bbce, $1186, $3386, $11dc, $33dc, $11ce,
                $77dc, $33ce, $dca0, $ee58, $f72e, $dc90, $ee4c, $dc88,
                $ee46, $dc84, $dc82, $98a0, $cc58, $e62e, $b9a0, $9890,
                $ee6e, $b990, $dccc, $cc46, $b988, $9884, $b984, $9882,
                $b982, $10a0, $8858, $c42e, $31a0, $1090, $884c, $73a0,
                $3190, $98cc, $8846, $7390, $b9cc, $1084, $7388, $3184,
                $1082, $3182, $10d8, $886e, $31d8, $10cc, $73d8, $31cc,
                $10c6, $73cc, $31c6, $10ee, $73ee, $dc50, $ee2c, $dc48,
                $ee26, $dc44, $dc42, $9850, $cc2c, $b8d0, $9848, $cc26,
                $b8c8, $dc66, $b8c4, $9842, $b8c2, $1050, $882c, $30d0,
                $1048, $8826, $71d0, $30c8, $9866, $71c8, $b8e6, $1042,
                $71c4, $30c2, $71c2, $30ec, $71ec, $71e6, $ee16, $dc22,
                $cc16, $9824, $9822, $1028, $3068, $70e8, $1022, $3062,
                $8560, $0a40, $8530, $0a20, $8518, $c28e, $0a10, $850c,
                $0a08, $8506, $0b60, $85b8, $c2de, $0b30, $859c, $0b18,
                $858e, $0b0c, $0b06, $0bb8, $85de, $0b9c, $0b8e, $0bde,
                $8d40, $c6b0, $e35c, $8d20, $c698, $8d10, $c68c, $8d08,
                $c686, $8d04, $0940, $84b0, $c25c, $1b40, $0920, $c6dc,
                $c24e, $1b20, $8d98, $c6ce, $1b10, $0908, $8486, $1b08,
                $8d86, $0902, $09b0, $84dc, $1bb0, $0998, $84ce, $1b98,
                $8dce, $1b8c, $0986, $09dc, $1bdc, $09ce, $1bce, $cea0,
                $e758, $f3ae, $ce90, $e74c, $ce88, $e746, $ce84, $ce82,
                $8ca0, $c658, $9da0, $8c90, $c64c, $9d90, $cecc, $c646,
                $9d88, $8c84, $9d84, $8c82, $9d82, $08a0, $8458, $19a0,
                $0890, $c66e, $3ba0, $1990, $8ccc, $8446, $3b90, $9dcc,
                $0884, $3b88, $1984, $0882, $1982, $08d8, $846e, $19d8,
                $08cc, $3bd8, $19cc, $08c6, $3bcc, $19c6, $08ee, $19ee,
                $3bee, $ef50, $f7ac, $ef48, $f7a6, $ef44, $ef42, $ce50,
                $e72c, $ded0, $ef6c, $e726, $dec8, $ef66, $dec4, $ce42,
                $dec2, $8c50, $c62c, $9cd0, $8c48, $c626, $bdd0, $9cc8,
                $ce66, $bdc8, $dee6, $8c42, $bdc4, $9cc2, $bdc2, $0850,
                $842c, $18d0, $0848, $8426, $39d0, $18c8, $8c66, $7bd0,
                $39c8, $9ce6, $0842, $7bc8, $bde6, $18c2, $7bc4, $086c,
                $18ec, $0866, $39ec, $18e6, $7bec, $39e6, $7be6, $ef28,
                $f796, $ef24, $ef22, $ce28, $e716, $de68, $ef36, $de64,
                $ce22, $de62, $8c28, $c616, $9c68, $8c24, $bce8, $9c64,
                $8c22, $bce4, $9c62, $bce2, $0828, $8416, $1868, $8c36,
                $38e8, $1864, $0822, $79e8, $38e4, $1862, $79e4, $38e2,
                $79e2, $1876, $79f6, $ef12, $de34, $de32, $9c34, $bc74,
                $bc72, $1834, $3874, $78f4, $78f2, $0540, $0520, $8298,
                $0510, $0508, $0504, $05b0, $0598, $058c, $0586, $05dc,
                $05ce, $86a0, $8690, $c34c, $8688, $c346, $8684, $8682,
                $04a0, $8258, $0da0, $86d8, $824c, $0d90, $86cc, $0d88,
                $86c6, $0d84, $0482, $0d82, $04d8, $826e, $0dd8, $86ee,
                $0dcc, $04c6, $0dc6, $04ee, $0dee, $c750, $c748, $c744,
                $c742, $8650, $8ed0, $c76c, $c326, $8ec8, $c766, $8ec4,
                $8642, $8ec2, $0450, $0cd0, $0448, $8226, $1dd0, $0cc8,
                $0444, $1dc8, $0cc4, $0442, $1dc4, $0cc2, $046c, $0cec,
                $0466, $1dec, $0ce6, $1de6, $e7a8, $e7a4, $e7a2, $c728,
                $cf68, $e7b6, $cf64, $c722, $cf62, $8628, $c316, $8e68,
                $c736, $9ee8, $8e64, $8622, $9ee4, $8e62, $9ee2, $0428,
                $8216, $0c68, $8636, $1ce8, $0c64, $0422, $3de8, $1ce4,
                $0c62, $3de4, $1ce2, $0436, $0c76, $1cf6, $3df6, $f7d4,
                $f7d2, $e794, $efb4, $e792, $efb2, $c714, $cf34, $c712,
                $df74, $cf32, $df72, $8614, $8e34, $8612, $9e74, $8e32,
                $bef4 ),

               ($f560, $fab8, $ea40, $f530, $fa9c, $ea20, $f518, $fa8e,
                $ea10, $f50c, $ea08, $f506, $ea04, $eb60, $f5b8, $fade,
                $d640, $eb30, $f59c, $d620, $eb18, $f58e, $d610, $eb0c,
                $d608, $eb06, $d604, $d760, $ebb8, $f5de, $ae40, $d730,
                $eb9c, $ae20, $d718, $eb8e, $ae10, $d70c, $ae08, $d706,
                $ae04, $af60, $d7b8, $ebde, $5e40, $af30, $d79c, $5e20,
                $af18, $d78e, $5e10, $af0c, $5e08, $af06, $5f60, $afb8,
                $d7de, $5f30, $af9c, $5f18, $af8e, $5f0c, $5fb8, $afde,
                $5f9c, $5f8e, $e940, $f4b0, $fa5c, $e920, $f498, $fa4e,
                $e910, $f48c, $e908, $f486, $e904, $e902, $d340, $e9b0,
                $f4dc, $d320, $e998, $f4ce, $d310, $e98c, $d308, $e986,
                $d304, $d302, $a740, $d3b0, $e9dc, $a720, $d398, $e9ce,
                $a710, $d38c, $a708, $d386, $a704, $a702, $4f40, $a7b0,
                $d3dc, $4f20, $a798, $d3ce, $4f10, $a78c, $4f08, $a786,
                $4f04, $4fb0, $a7dc, $4f98, $a7ce, $4f8c, $4f86, $4fdc,
                $4fce, $e8a0, $f458, $fa2e, $e890, $f44c, $e888, $f446,
                $e884, $e882, $d1a0, $e8d8, $f46e, $d190, $e8cc, $d188,
                $e8c6, $d184, $d182, $a3a0, $d1d8, $e8ee, $a390, $d1cc,
                $a388, $d1c6, $a384, $a382, $47a0, $a3d8, $d1ee, $4790,
                $a3cc, $4788, $a3c6, $4784, $4782, $47d8, $a3ee, $47cc,
                $47c6, $47ee, $e850, $f42c, $e848, $f426, $e844, $e842,
                $d0d0, $e86c, $d0c8, $e866, $d0c4, $d0c2, $a1d0, $d0ec,
                $a1c8, $d0e6, $a1c4, $a1c2, $43d0, $a1ec, $43c8, $a1e6,
                $43c4, $43c2, $43ec, $43e6, $e828, $f416, $e824, $e822,
                $d068, $e836, $d064, $d062, $a0e8, $d076, $a0e4, $a0e2,
                $41e8, $a0f6, $41e4, $41e2, $e814, $e812, $d034, $d032,
                $a074, $a072, $e540, $f2b0, $f95c, $e520, $f298, $f94e,
                $e510, $f28c, $e508, $f286, $e504, $e502, $cb40, $e5b0,
                $f2dc, $cb20, $e598, $f2ce, $cb10, $e58c, $cb08, $e586,
                $cb04, $cb02, $9740, $cbb0, $e5dc, $9720, $cb98, $e5ce,
                $9710, $cb8c, $9708, $cb86, $9704, $9702, $2f40, $97b0,
                $cbdc, $2f20, $9798, $cbce, $2f10, $978c, $2f08, $9786,
                $2f04, $2fb0, $97dc, $2f98, $97ce, $2f8c, $2f86, $2fdc,
                $2fce, $f6a0, $fb58, $6bf0, $f690, $fb4c, $69f8, $f688,
                $fb46, $68fc, $f684, $f682, $e4a0, $f258, $f92e, $eda0,
                $e490, $fb6e, $ed90, $f6cc, $f246, $ed88, $e484, $ed84,
                $e482, $ed82, $c9a0, $e4d8, $f26e, $dba0, $c990, $e4cc,
                $db90, $edcc, $e4c6, $db88, $c984, $db84, $c982, $db82,
                $93a0, $c9d8, $e4ee, $b7a0, $9390, $c9cc, $b790, $dbcc,
                $c9c6, $b788, $9384, $b784, $9382, $b782, $27a0, $93d8,
                $c9ee, $6fa0, $2790, $93cc, $6f90, $b7cc, $93c6, $6f88,
                $2784, $6f84, $2782, $27d8, $93ee, $6fd8, $27cc, $6fcc,
                $27c6, $6fc6, $27ee, $f650, $fb2c, $65f8, $f648, $fb26,
                $64fc, $f644, $647e, $f642, $e450, $f22c, $ecd0, $e448,
                $f226, $ecc8, $f666, $ecc4, $e442, $ecc2, $c8d0, $e46c,
                $d9d0, $c8c8, $e466, $d9c8, $ece6, $d9c4, $c8c2, $d9c2,
                $91d0, $c8ec, $b3d0, $91c8, $c8e6, $b3c8, $d9e6, $b3c4,
                $91c2, $b3c2, $23d0, $91ec, $67d0, $23c8, $91e6, $67c8,
                $b3e6, $67c4, $23c2, $67c2, $23ec, $67ec, $23e6, $67e6,
                $f628, $fb16, $62fc, $f624, $627e, $f622, $e428, $f216,
                $ec68, $f636, $ec64, $e422, $ec62, $c868, $e436, $d8e8,
                $c864, $d8e4, $c862, $d8e2, $90e8, $c876, $b1e8, $d8f6,
                $b1e4, $90e2, $b1e2, $21e8, $90f6, $63e8, $21e4, $63e4,
                $21e2, $63e2, $21f6, $63f6, $f614, $617e, $f612, $e414,
                $ec34, $e412, $ec32, $c834, $d874, $c832, $d872, $9074,
                $b0f4, $9072, $b0f2, $20f4, $61f4, $20f2, $61f2, $f60a,
                $e40a, $ec1a, $c81a, $d83a, $903a, $b07a, $e2a0, $f158,
                $f8ae, $e290, $f14c, $e288, $f146, $e284, $e282, $c5a0,
                $e2d8, $f16e, $c590, $e2cc, $c588, $e2c6, $c584, $c582,
                $8ba0, $c5d8, $e2ee, $8b90, $c5cc, $8b88, $c5c6, $8b84,
                $8b82, $17a0, $8bd8, $c5ee, $1790, $8bcc, $1788, $8bc6,
                $1784, $1782, $17d8, $8bee, $17cc, $17c6, $17ee, $f350,
                $f9ac, $35f8, $f348, $f9a6, $34fc, $f344, $347e, $f342,
                $e250, $f12c, $e6d0, $e248, $f126, $e6c8, $f366, $e6c4,
                $e242, $e6c2, $c4d0, $e26c, $cdd0, $c4c8, $e266, $cdc8,
                $e6e6, $cdc4, $c4c2, $cdc2, $89d0, $c4ec, $9bd0, $89c8,
                $c4e6, $9bc8, $cde6, $9bc4, $89c2, $9bc2, $13d0, $89ec,
                $37d0, $13c8, $89e6, $37c8, $9be6, $37c4, $13c2, $37c2,
                $13ec, $37ec, $13e6, $37e6, $fba8, $75f0, $bafc, $fba4,
                $74f8, $ba7e, $fba2, $747c, $743e, $f328, $f996, $32fc,
                $f768, $fbb6, $76fc, $327e, $f764, $f322, $767e, $f762,
                $e228, $f116, $e668, $e224, $eee8, $f776, $e222, $eee4,
                $e662, $eee2, $c468, $e236, $cce8, $c464, $dde8, $cce4,
                $c462, $dde4, $cce2, $dde2, $88e8, $c476, $99e8, $88e4,
                $bbe8, $99e4, $88e2, $bbe4, $99e2, $bbe2, $11e8, $88f6,
                $33e8, $11e4, $77e8, $33e4, $11e2, $77e4, $33e2, $77e2,
                $11f6, $33f6, $fb94, $72f8, $b97e, $fb92, $727c, $723e,
                $f314, $317e, $f734, $f312, $737e, $f732, $e214, $e634,
                $e212, $ee74, $e632, $ee72, $c434, $cc74, $c432, $dcf4,
                $cc72, $dcf2, $8874, $98f4, $8872, $b9f4, $98f2, $b9f2,
                $10f4, $31f4, $10f2, $73f4, $31f2, $73f2, $fb8a, $717c,
                $713e, $f30a, $f71a, $e20a, $e61a, $ee3a, $c41a, $cc3a,
                $dc7a, $883a, $987a, $b8fa, $107a, $30fa, $71fa, $70be,
                $e150, $f0ac, $e148, $f0a6, $e144, $e142, $c2d0, $e16c,
                $c2c8, $e166, $c2c4, $c2c2, $85d0, $c2ec, $85c8, $c2e6,
                $85c4, $85c2, $0bd0, $85ec, $0bc8, $85e6, $0bc4, $0bc2,
                $0bec, $0be6, $f1a8, $f8d6, $1afc, $f1a4, $1a7e, $f1a2,
                $e128, $f096, $e368, $e124, $e364, $e122, $e362, $c268,
                $e136, $c6e8, $c264, $c6e4, $c262, $c6e2, $84e8, $c276,
                $8de8, $84e4, $8de4, $84e2, $8de2, $09e8, $84f6, $1be8,
                $09e4, $1be4, $09e2, $1be2, $09f6, $1bf6, $f9d4, $3af8,
                $9d7e, $f9d2, $3a7c, $3a3e, $f194, $197e, $f3b4, $f192,
                $3b7e, $f3b2, $e114, $e334, $e112, $e774, $e332, $e772,
                $c234, $c674, $c232, $cef4, $c672, $cef2, $8474, $8cf4,
                $8472, $9df4, $8cf2, $9df2, $08f4, $19f4, $08f2, $3bf4,
                $19f2, $3bf2, $7af0, $bd7c, $7a78, $bd3e, $7a3c, $7a1e,
                $f9ca, $397c, $fbda, $7b7c, $393e, $7b3e, $f18a, $f39a,
                $f7ba, $e10a, $e31a, $e73a, $ef7a, $c21a, $c63a, $ce7a,
                $defa, $843a, $8c7a, $9cfa, $bdfa, $087a, $18fa, $39fa,
                $7978, $bcbe, $793c, $791e, $38be, $79be, $78bc, $789e,
                $785e, $e0a8, $e0a4, $e0a2, $c168, $e0b6, $c164, $c162,
                $82e8, $c176, $82e4, $82e2, $05e8, $82f6, $05e4, $05e2,
                $05f6, $f0d4, $0d7e, $f0d2, $e094, $e1b4, $e092, $e1b2,
                $c134, $c374, $c132, $c372, $8274, $86f4, $8272, $86f2,
                $04f4, $0df4, $04f2, $0df2, $f8ea, $1d7c, $1d3e, $f0ca,
                $f1da, $e08a, $e19a, $e3ba, $c11a, $c33a, $c77a, $823a,
                $867a, $8efa, $047a, $0cfa, $1dfa, $3d78, $9ebe, $3d3c,
                $3d1e, $1cbe, $3dbe, $7d70, $bebc, $7d38, $be9e, $7d1c,
                $7d0e, $3cbc, $7dbc, $3c9e, $7d9e, $7cb8, $be5e, $7c9c,
                $7c8e, $3c5e, $7cde, $7c5c, $7c4e, $7c2e, $c0b4, $c0b2,
                $8174, $8172, $02f4, $02f2, $e0da, $c09a, $c1ba, $813a,
                $837a, $027a, $06fa, $0ebe, $1ebc, $1e9e, $3eb8, $9f5e,
                $3e9c, $3e8e, $1e5e, $3ede, $7eb0, $bf5c, $7e98, $bf4e,
                $7e8c, $7e86, $3e5c, $7edc, $3e4e, $7ece, $7e58, $bf2e,
                $7e4c, $7e46, $3e2e, $7e6e, $7e2c, $7e26, $0f5e, $1f5c,
                $1f4e, $3f58, $9fae, $3f4c, $3f46, $1f2e, $3f6e, $3f2c,
                $3f26 ),
               ($abe0, $d5f8, $53c0, $a9f0, $d4fc, $51e0, $a8f8, $d47e,
                $50f0, $a87c, $5078, $fad0, $5be0, $adf8, $fac8, $59f0,
                $acfc, $fac4, $58f8, $ac7e, $fac2, $587c, $f5d0, $faec,
                $5df8, $f5c8, $fae6, $5cfc, $f5c4, $5c7e, $f5c2, $ebd0,
                $f5ec, $ebc8, $f5e6, $ebc4, $ebc2, $d7d0, $ebec, $d7c8,
                $ebe6, $d7c4, $d7c2, $afd0, $d7ec, $afc8, $d7e6, $afc4,
                $4bc0, $a5f0, $d2fc, $49e0, $a4f8, $d27e, $48f0, $a47c,
                $4878, $a43e, $483c, $fa68, $4df0, $a6fc, $fa64, $4cf8,
                $a67e, $fa62, $4c7c, $4c3e, $f4e8, $fa76, $4efc, $f4e4,
                $4e7e, $f4e2, $e9e8, $f4f6, $e9e4, $e9e2, $d3e8, $e9f6,
                $d3e4, $d3e2, $a7e8, $d3f6, $a7e4, $a7e2, $45e0, $a2f8,
                $d17e, $44f0, $a27c, $4478, $a23e, $443c, $441e, $fa34,
                $46f8, $a37e, $fa32, $467c, $463e, $f474, $477e, $f472,
                $e8f4, $e8f2, $d1f4, $d1f2, $a3f4, $a3f2, $42f0, $a17c,
                $4278, $a13e, $423c, $421e, $fa1a, $437c, $433e, $f43a,
                $e87a, $d0fa, $4178, $a0be, $413c, $411e, $41be, $40bc,
                $409e, $2bc0, $95f0, $cafc, $29e0, $94f8, $ca7e, $28f0,
                $947c, $2878, $943e, $283c, $f968, $2df0, $96fc, $f964,
                $2cf8, $967e, $f962, $2c7c, $2c3e, $f2e8, $f976, $2efc,
                $f2e4, $2e7e, $f2e2, $e5e8, $f2f6, $e5e4, $e5e2, $cbe8,
                $e5f6, $cbe4, $cbe2, $97e8, $cbf6, $97e4, $97e2, $b5e0,
                $daf8, $ed7e, $69c0, $b4f0, $da7c, $68e0, $b478, $da3e,
                $6870, $b43c, $6838, $b41e, $681c, $25e0, $92f8, $c97e,
                $6de0, $24f0, $927c, $6cf0, $b67c, $923e, $6c78, $243c,
                $6c3c, $241e, $6c1e, $f934, $26f8, $937e, $fb74, $f932,
                $6ef8, $267c, $fb72, $6e7c, $263e, $6e3e, $f274, $277e,
                $f6f4, $f272, $6f7e, $f6f2, $e4f4, $edf4, $e4f2, $edf2,
                $c9f4, $dbf4, $c9f2, $dbf2, $93f4, $93f2, $65c0, $b2f0,
                $d97c, $64e0, $b278, $d93e, $6470, $b23c, $6438, $b21e,
                $641c, $640e, $22f0, $917c, $66f0, $2278, $913e, $6678,
                $b33e, $663c, $221e, $661e, $f91a, $237c, $fb3a, $677c,
                $233e, $673e, $f23a, $f67a, $e47a, $ecfa, $c8fa, $d9fa,
                $91fa, $62e0, $b178, $d8be, $6270, $b13c, $6238, $b11e,
                $621c, $620e, $2178, $90be, $6378, $213c, $633c, $211e,
                $631e, $21be, $63be, $6170, $b0bc, $6138, $b09e, $611c,
                $610e, $20bc, $61bc, $209e, $619e, $60b8, $b05e, $609c,
                $608e, $205e, $60de, $605c, $604e, $15e0, $8af8, $c57e,
                $14f0, $8a7c, $1478, $8a3e, $143c, $141e, $f8b4, $16f8,
                $8b7e, $f8b2, $167c, $163e, $f174, $177e, $f172, $e2f4,
                $e2f2, $c5f4, $c5f2, $8bf4, $8bf2, $35c0, $9af0, $cd7c,
                $34e0, $9a78, $cd3e, $3470, $9a3c, $3438, $9a1e, $341c,
                $340e, $12f0, $897c, $36f0, $1278, $893e, $3678, $9b3e,
                $363c, $121e, $361e, $f89a, $137c, $f9ba, $377c, $133e,
                $373e, $f13a, $f37a, $e27a, $e6fa, $c4fa, $cdfa, $89fa,
                $bae0, $dd78, $eebe, $74c0, $ba70, $dd3c, $7460, $ba38,
                $dd1e, $7430, $ba1c, $7418, $ba0e, $740c, $32e0, $9978,
                $ccbe, $76e0, $3270, $993c, $7670, $bb3c, $991e, $7638,
                $321c, $761c, $320e, $760e, $1178, $88be, $3378, $113c,
                $7778, $333c, $111e, $773c, $331e, $771e, $11be, $33be,
                $77be, $72c0, $b970, $dcbc, $7260, $b938, $dc9e, $7230,
                $b91c, $7218, $b90e, $720c, $7206, $3170, $98bc, $7370,
                $3138, $989e, $7338, $b99e, $731c, $310e, $730e, $10bc,
                $31bc, $109e, $73bc, $319e, $739e, $7160, $b8b8, $dc5e,
                $7130, $b89c, $7118, $b88e, $710c, $7106, $30b8, $985e,
                $71b8, $309c, $719c, $308e, $718e, $105e, $30de, $71de,
                $70b0, $b85c, $7098, $b84e, $708c, $7086, $305c, $70dc,
                $304e, $70ce, $7058, $b82e, $704c, $7046, $302e, $706e,
                $702c, $7026, $0af0, $857c, $0a78, $853e, $0a3c, $0a1e,
                $0b7c, $0b3e, $f0ba, $e17a, $c2fa, $85fa, $1ae0, $8d78,
                $c6be, $1a70, $8d3c, $1a38, $8d1e, $1a1c, $1a0e, $0978,
                $84be, $1b78, $093c, $1b3c, $091e, $1b1e, $09be, $1bbe,
                $3ac0, $9d70, $cebc, $3a60, $9d38, $ce9e, $3a30, $9d1c,
                $3a18, $9d0e, $3a0c, $3a06, $1970, $8cbc, $3b70, $1938,
                $8c9e, $3b38, $191c, $3b1c, $190e, $3b0e, $08bc, $19bc,
                $089e, $3bbc, $199e, $3b9e, $bd60, $deb8, $ef5e, $7a40,
                $bd30, $de9c, $7a20, $bd18, $de8e, $7a10, $bd0c, $7a08,
                $bd06, $7a04, $3960, $9cb8, $ce5e, $7b60, $3930, $9c9c,
                $7b30, $bd9c, $9c8e, $7b18, $390c, $7b0c, $3906, $7b06,
                $18b8, $8c5e, $39b8, $189c, $7bb8, $399c, $188e, $7b9c,
                $398e, $7b8e, $085e, $18de, $39de, $7bde, $7940, $bcb0,
                $de5c, $7920, $bc98, $de4e, $7910, $bc8c, $7908, $bc86,
                $7904, $7902, $38b0, $9c5c, $79b0, $3898, $9c4e, $7998,
                $bcce, $798c, $3886, $7986, $185c, $38dc, $184e, $79dc,
                $38ce, $79ce, $78a0, $bc58, $de2e, $7890, $bc4c, $7888,
                $bc46, $7884, $7882, $3858, $9c2e, $78d8, $384c, $78cc,
                $3846, $78c6, $182e, $386e, $78ee, $7850, $bc2c, $7848,
                $bc26, $7844, $7842, $382c, $786c, $3826, $7866, $7828,
                $bc16, $7824, $7822, $3816, $7836, $0578, $82be, $053c,
                $051e, $05be, $0d70, $86bc, $0d38, $869e, $0d1c, $0d0e,
                $04bc, $0dbc, $049e, $0d9e, $1d60, $8eb8, $c75e, $1d30,
                $8e9c, $1d18, $8e8e, $1d0c, $1d06, $0cb8, $865e, $1db8,
                $0c9c, $1d9c, $0c8e, $1d8e, $045e, $0cde, $1dde, $3d40,
                $9eb0, $cf5c, $3d20, $9e98, $cf4e, $3d10, $9e8c, $3d08,
                $9e86, $3d04, $3d02, $1cb0, $8e5c, $3db0, $1c98, $8e4e,
                $3d98, $9ece, $3d8c, $1c86, $3d86, $0c5c, $1cdc, $0c4e,
                $3ddc, $1cce, $3dce, $bea0, $df58, $efae, $be90, $df4c,
                $be88, $df46, $be84, $be82, $3ca0, $9e58, $cf2e, $7da0,
                $3c90, $9e4c, $7d90, $becc, $9e46, $7d88, $3c84, $7d84,
                $3c82, $7d82, $1c58, $8e2e, $3cd8, $1c4c, $7dd8, $3ccc,
                $1c46, $7dcc, $3cc6, $7dc6, $0c2e, $1c6e, $3cee, $7dee,
                $be50, $df2c, $be48, $df26, $be44, $be42, $3c50, $9e2c,
                $7cd0, $3c48, $9e26, $7cc8, $be66, $7cc4, $3c42, $7cc2,
                $1c2c, $3c6c, $1c26, $7cec, $3c66, $7ce6, $be28, $df16,
                $be24, $be22, $3c28, $9e16, $7c68, $3c24, $7c64, $3c22,
                $7c62, $1c16, $3c36, $7c76, $be14, $be12, $3c14, $7c34,
                $3c12, $7c32, $02bc, $029e, $06b8, $835e, $069c, $068e,
                $025e, $06de, $0eb0, $875c, $0e98, $874e, $0e8c, $0e86,
                $065c, $0edc, $064e, $0ece, $1ea0, $8f58, $c7ae, $1e90,
                $8f4c, $1e88, $8f46, $1e84, $1e82, $0e58, $872e, $1ed8,
                $8f6e, $1ecc, $0e46, $1ec6, $062e, $0e6e, $1eee, $9f50,
                $cfac, $9f48, $cfa6, $9f44, $9f42, $1e50, $8f2c, $3ed0,
                $9f6c, $8f26, $3ec8, $1e44, $3ec4, $1e42, $3ec2, $0e2c,
                $1e6c, $0e26, $3eec, $1e66, $3ee6, $dfa8, $efd6, $dfa4,
                $dfa2, $9f28, $cf96, $bf68, $9f24, $bf64, $9f22, $bf62,
                $1e28, $8f16, $3e68, $1e24, $7ee8, $3e64, $1e22, $7ee4,
                $3e62, $7ee2, $0e16, $1e36, $3e76, $7ef6, $df94, $df92,
                $9f14, $bf34, $9f12, $bf32, $1e14, $3e34, $1e12, $7e74,
                $3e32, $7e72, $df8a, $9f0a, $bf1a, $1e0a, $3e1a, $7e3a,
                $035c, $034e, $0758, $83ae, $074c, $0746, $032e, $076e,
                $0f50, $87ac, $0f48, $87a6, $0f44, $0f42, $072c, $0f6c,
                $0726, $0f66, $8fa8, $c7d6, $8fa4, $8fa2, $0f28, $8796,
                $1f68, $8fb6, $1f64, $0f22, $1f62, $0716, $0f36, $1f76,
                $cfd4, $cfd2, $8f94, $9fb4, $8f92, $9fb2, $0f14, $1f34,
                $0f12, $3f74, $1f32, $3f72, $cfca, $8f8a, $9f9a, $0f0a,
                $1f1a, $3f3a, $03ac, $03a6, $07a8, $83d6, $07a4, $07a2,
                $0396, $07b6, $87d4, $87d2, $0794, $0fb4, $0792, $0fb2,
                $c7ea ));


        ErrorTable2 : array [0..1] of word =(27,917);
        ErrorTable3 : array [0..2] of word =(200,351,890);
        ErrorTable4 : array [0..3] of word =(522,568,723,809);
        ErrorTable5 : array [0..4] of word =(427,919,460,155,566);
        ErrorTable6 : array [0..5] of word =(861,285,19,803,17,766);
        ErrorTable7 : array [0..6] of word =(76,925,537,597,784,691,437);
        ErrorTable8 : array [0..7] of word =(237,308,436,284,646,653,428,379);
        ErrorTable9 : array [0..8] of word =(567,527,622,257,289,362,501,
              441,205);
        ErrorTable10: array [0..9] of word =(377,457,64,244,826,841,818,
              691,266,612);
        ErrorTable11: array [0..10] of word =(462,45,565,708,825,213,15,68,
              327,602,904);
        ErrorTable12: array [0..11] of word =(597,864,757,201,646,684,347,127,
              388,7,69,851);
        ErrorTable13: array [0..12] of word =(764,713,342,384,606,583,322,592,
              678,204,184,394,692);
        ErrorTable14: array [0..13] of word =(669,677,154,187,241,286,274,354,
              478,915,691,833,105,215);
        ErrorTable15: array [0..14] of word =(460,829,476,109,904,664,230,5,
              80,74,550,575,147,868,642);
        ErrorTable16 : array [0..15] of word = (274,562,232,755,599,524,801,
              132,295,116,442,428,295, 42,176, 65);
        ErrorTable18 : array [0..17] of word =(279,577,315,624,37,855,275,739,
              120,297,312,202,560,321,233,756,760,573);
        ErrorTable21 : array [0..20] of word =(108,519,781,534,129,425,681,553,
              422,716,763,693,624,610,310,691,347,165,193,259,568);
        ErrorTable26 : array [0..25] of word =(443,284,887,544,788,93,477,760,
              331,608,269,121,159,830,446,893,699,245,441,454,325,858,131,847,
              764,169);
        ErrorTable32 : array [0..31] of word = (361,575,922,525,176,586,640,
              321,536,742,677,742,687,284,193,517,273,494,263,147,593,800,571,
              320,803,133,231,390,685,330, 63,410);
        ErrorTable38 : array [0..37] of word =(234,228,438,848,133,703,529,721,
              788,322,280,159,738,586,388,684,445,680,245,595,614,233,812,32,
              284,658,745,229,95,689,920,771,554,289,231,125,117,518);
        ErrorTable44 : array [0..43] of word =(476,36,659,848,678,64,764,840,
              157,915,470,876,109,25,632,405,417,436,714,60,376,97,413,706,446,
              21,3,773,569,267,272,213,31,560,231,758,103,271,572,436,339,730,
              82,285);
        ErrorTable50 : array [0..49] of word =(923,797,576,875,156,706,63,81,
              257,874,411,416,778,50,205,303,188,535,909,155,637,230,534,96,
              575,102,264,233,919,593,865,26,579,623,766,146,10,739,246,127,
              71,244,211,477,920,876,427,820,718,435);
        ErrorTable64 : array [0..63]  of word =
                (539,422,  6, 93,862,771,453,106,610,287,107,505,733,877,381,612,
                 723,476,462,172,430,609,858,822,543,376,511,400,672,762,283,184,
                 440, 35,519, 31,460,594,225,535,517,352,605,158,651,201,488,502,
                 648,733,717, 83,404, 97,280,771,840,629,  4,381,843,623,264,543
                );
        ErrorTable128 : array [0..127] of word =
                (521,310, 864, 547, 858, 580, 296, 379,  53, 779, 897, 444, 400, 925, 749, 415,
                 822, 93, 217, 208, 928, 244, 583, 620, 246, 148, 447, 631, 292, 908, 490, 704,
                 516,258, 457, 907, 594, 723, 674, 292, 272,  96, 684, 432, 686, 606, 860, 569,
                 193,219, 129, 186, 236, 287, 192, 775, 278, 173,  40, 379, 712, 463, 646, 776,
                 171,491, 297, 763, 156, 732,  95, 270, 447,  90, 507,  48, 228, 821, 808, 898,
                 784,663, 627, 378, 382, 262, 380, 602, 754, 336,  89, 614,  87, 432, 670, 616,
                 157,374, 242, 726, 600, 269, 375, 898, 845, 454, 354, 130, 814, 587, 804,  34,
                 211,330, 539, 297, 827, 865,  37, 517, 834, 315, 550,  86, 801,   4, 108, 539
                );
        ErrorTable256 : array [0..255] of word =
                (524, 894,  75, 766, 882, 857,  74, 204,  82, 586, 708, 250, 905, 786, 138, 720,
                 858, 194, 311, 913, 275, 190, 375, 850, 438, 733, 194, 280, 201, 280, 828, 757,
                 710, 814, 919,  89,  68, 569,  11, 204, 796, 605, 540, 913, 801, 700, 799, 137,
                 439, 418, 592, 668, 353, 859, 370, 694, 325, 240, 216, 257, 284, 549, 209, 884,
                 315,  70, 329, 793, 490, 274, 877, 162, 749, 812, 684, 461, 334, 376, 849, 521,
                 307, 291, 803, 712,  19, 358, 399, 908, 103, 511,  51,   8, 517, 225, 289, 470,
                 637, 731,  66, 255, 917, 269, 463, 830, 730, 433, 848, 585, 136, 538, 906,  90,
                   2, 290, 743, 199, 655, 903, 329,  49, 802, 580, 355, 588, 188, 462,  10, 134,
                 628, 320, 479, 130, 739,  71, 263, 318, 374, 601, 192, 605, 142, 673, 687, 234,
                 722, 384, 177, 752, 607, 640, 455, 193, 689, 707, 805, 641,  48,  60, 732, 621,
                 895, 544, 261, 852, 655, 309, 697, 755, 756,  60, 231, 773, 434, 421, 726, 528,
                 503, 118,  49, 795,  32, 144, 500, 238, 836, 394, 280, 566, 319,   9, 647, 550,
                  73, 914, 342, 126,  32, 681, 331, 792, 620,  60, 609, 441, 180, 791, 893, 754,
                 605, 383, 228, 749, 760, 213,  54, 297, 134,  54, 834, 299, 922, 191, 910, 532,
                 609, 829, 189,  20, 167,  29, 872, 449,  83, 402,  41, 656, 505, 579, 481, 173,
                 404, 251, 688,  95, 497, 555, 642, 543, 307, 159, 924, 558, 648,  55, 497,  10
                );
        ErrorTable512 : array [0..511] of word =
                (352,  77, 373, 504,  35, 599, 428, 207, 409, 574, 118, 498, 285, 380, 350, 492,
                 197, 265, 920, 155, 914, 299, 229, 643, 294, 871, 306,  88,  87, 193, 352, 781,
                 846,  75, 327, 520, 435, 543, 203, 666, 249, 346, 781, 621, 640, 268, 794, 534,
                 539, 781, 408, 390, 644, 102, 476, 499, 290, 632, 545,  37, 858, 916, 552,  41,
                 542, 289, 122, 272, 383, 800, 485,  98, 752, 472, 761, 107, 784, 860, 658, 741,
                 290, 204, 681, 407, 855,  85,  99,  62, 482, 180,  20, 297, 451, 593, 913, 142,
                 808, 684, 287, 536, 561,  76, 653, 899, 729, 567, 744, 390, 513, 192, 516, 258,
                 240, 518, 794, 395, 768, 848,  51, 610, 384, 168, 190, 826, 328, 596, 786, 303,
                 570, 381, 415, 641, 156, 237, 151, 429, 531, 207, 676, 710,  89, 168, 304, 402,
                  40, 708, 575, 162, 864, 229,  65, 861, 841, 512, 164, 477, 221,  92, 358, 785,
                 288, 357, 850, 836, 827, 736, 707,  94,   8, 494, 114, 521,   2, 499, 851, 543,
                 152, 729, 771,  95, 248, 361, 578, 323, 856, 797, 289,  51, 684, 466, 533, 820,
                 669,  45, 902, 452, 167, 342, 244, 173,  35, 463, 651,  51, 699, 591, 452, 578,
                  37, 124, 298, 332, 552,  43, 427, 119, 662, 777, 475, 850, 764, 364, 578, 911,
                 283, 711, 472, 420, 245, 288, 594, 394, 511, 327, 589, 777, 699, 688,  43, 408,
                 842, 383, 721, 521, 560, 644, 714, 559,  62, 145, 873, 663, 713, 159, 672, 729,
                 624,  59, 193, 417, 158, 209, 563, 564, 343, 693, 109, 608, 563, 365, 181, 772,
                 677, 310, 248, 353, 708, 410, 579, 870, 617, 841, 632, 860, 289, 536,  35, 777,
                 618, 586, 424, 833,  77, 597, 346, 269, 757, 632, 695, 751, 331, 247, 184,  45,
                 787, 680,  18,  66, 407, 369,  54, 492, 228, 613, 830, 922, 437, 519, 644, 905,
                 789, 420, 305, 441, 207, 300, 892, 827, 141, 537, 381, 662, 513,  56, 252, 341,
                 242, 797, 838, 837, 720, 224, 307, 631,  61,  87, 560, 310, 756, 665, 397, 808,
                 851, 309, 473, 795, 378,  31, 647, 915, 459, 806, 590, 731, 425, 216, 548, 249,
                 321, 881, 699, 535, 673, 782, 210, 815, 905, 303, 843, 922, 281,  73, 469, 791,
                 660, 162, 498, 308, 155, 422, 907, 817, 187,  62,  16, 425, 535, 336, 286, 437,
                 375, 273, 610, 296, 183, 923, 116, 667, 751, 353,  62, 366, 691, 379, 687, 842,
                  37, 357, 720, 742, 330,   5,  39, 923, 311, 424, 242, 749, 321,  54, 669, 316,
                 342, 299, 534, 105, 667, 488, 640, 672, 576, 540, 316, 486, 721, 610,  46, 656,
                 447, 171, 616, 464, 190, 531, 297, 321, 762, 752, 533, 175, 134,  14, 381, 433,
                 717,  45, 111,  20, 596, 284, 736, 138, 646, 411, 877, 669, 141, 919,  45, 780,
                 407, 164, 332, 899, 165, 726, 600, 325, 498, 655, 357, 752, 768, 223, 849, 647,
                  63, 310, 863, 251, 366, 304, 282, 738, 675, 410, 389, 244,  31, 121, 303, 263
                );

    //  1. Number of data columns (c)
    //  2. Number of rows         (r)
    //  3. Total CWs in Data region
    //  4. Number of EC CWs       (k)
    //  5. % of CWs for EC
    //  6. Number of non-EC CWs
    //  7. Number of CWs for data
    //  8. Max. Data Bytes
    //  9. Max. Aplha chars
    //  10.Max.Digits
    //  11.Symbol width in X
    //  12.Symbol height in X
    //  13.Offset Left RAP
    //  14.Offset Center RAP
    //  15.Offset Right RAP
{  type

    TpsPDF417Micro = record
      cols, rows, TotalCW, EC:word;
      Perc : word;
    end;
}
  const

  pdf417MicroVersionsCount = 6+7+10+11;
  tblPDF417MicroVersions : array[1..pdf417MicroVersionsCount, 1..15] of Integer=(
    (1, 11, 11,  7, 64,  4,  3,  3,  6,  8, 40, 24,  0,  0,  8),
    (1, 14, 14,  7, 50,  7,  6,  7, 12, 17, 40, 30,  7,  0,  7),
    (1, 17, 17,  7, 41, 10,  9, 10, 18, 26, 40, 36, 35,  0, 35),
    (1, 20, 20,  8, 40, 12, 11, 13, 22, 32, 40, 42, 18,  0, 18),
    (1, 24, 24,  8, 33, 16, 15, 18, 30, 44, 40, 50,  8,  0, 16),
    (1, 28, 28,  8, 29, 20, 19, 22, 38, 55, 40, 58, 24,  0, 32),

    (2,  8, 16,  8, 50,  8,  7,  8, 14, 20, 57, 18,  0,  0,  0),
    (2, 11, 22,  9, 41, 13, 12, 14, 24, 35, 57, 24,  0,  0,  8),
    (2, 14, 28,  9, 32, 19, 18, 21, 36, 52, 57, 30,  7,  0,  7),
    (2, 17, 34, 10, 29, 24, 23, 27, 46, 67, 57, 36, 35,  0, 35),
    (2, 20, 40, 11, 28, 29, 28, 33, 56, 82, 57, 42, 18,  0, 18),
    (2, 23, 46, 13, 28, 33, 32, 38, 64, 93, 57, 48,  8,  0, 16),
    (2, 26, 52, 15, 29, 37, 36, 43, 72,105, 57, 54, 26,  0, 34),

    (3,  6, 18, 12, 67,  6,  5,  6, 10, 14, 84, 14,  0,  0,  0),
    (3,  8, 24, 14, 58, 10,  9, 10, 18, 26, 84, 18,  6,  6,  6),
    (3, 10, 30, 16, 53, 14, 13, 15, 26, 38, 84, 22, 14, 14, 14),
    (3, 12, 36, 18, 50, 18, 17, 20, 34, 49, 84, 26, 24, 24, 24),
    (3, 15, 45, 21, 47, 24, 23, 27, 46, 67, 84, 32, 36, 36, 36),
    (3, 20, 60, 26, 43, 34, 33, 39, 66, 96, 84, 42,  0, 16, 32),
    (3, 26, 78, 32, 41, 46, 45, 54, 90,132, 84, 54,  0,  8, 16),
    (3, 32, 96, 38, 40, 58, 57, 68,114,167, 84, 66, 20, 28, 36),
    (3, 38,114, 44, 39, 70, 69, 82,138,202, 84, 78, 14, 30, 46),
    (3, 44,132, 50, 38, 82, 81, 97,162,237, 84, 90,  0, 24, 48),

    (4,  4, 16,  8, 50,  8,  7,  8, 14, 20,101, 10, 46, 18, 42),
    (4,  6, 24, 12, 50, 12, 11, 13, 22, 32,101, 14,  0,  0,  0),
    (4,  8, 32, 14, 44, 18, 17, 20, 34, 49,101, 18,  6,  6,  6),
    (4, 10, 40, 16, 40, 24, 23, 27, 46, 67,101, 22, 14, 14, 14),
    (4, 12, 48, 18, 38, 30, 29, 34, 58, 85,101, 26, 14, 24, 24),
    (4, 15, 60, 21, 35, 39, 38, 45, 76,111,101, 32, 36, 36, 36),
    (4, 20, 80, 26, 33, 54, 53, 63,106,155,101, 42,  0, 16, 32),
    (4, 26,104, 32, 31, 72, 71, 85,142,208,101, 54,  0,  8, 16),
    (4, 32,128, 38, 30, 90, 89,106,178,261,101, 66, 20, 28, 36),
    (4, 38,152, 44, 29,108,107,128,214,313,101, 78, 14, 30, 46),
    (4, 44,176, 50, 28,126,125,150,250,366,101, 90,  0, 24, 48)
  );


  pdfLatchToText                = 900;
  pdfLatchToBytePart            = 901;
  pdfLatchToNumeric             = 902;
  pdfECCEan128Latch900          = 903;
  pdfECCEan128Latch902          = 904;
  pdfECCEan128Latch01           = 905;
  pdfLinkedECCEan128Latch900    = 906;
  pdfLinkedECCEan128Latch902    = 907;
  pdfCode128Emulation_1         = 908;
  pdfCode128Emulation_2         = 909;
  pdfCode128Emulation_3         = 910;
  pdfCode128Emulation_4         = 911;
  pdfLinkedUCCEAN128            = 912;
  pdfShiftToByteCompaction      = 913;
  pdfLinkedEANCUU128_AI10       = 914;
  pdfLinkedEANCUU128_AI21       = 915;
  pdfMacro05                    = 916;
  pdfMacro06                    = 917;
  pdfEANUCC_Composite           = 918;
  pdfReserved                   = 919;
  pdfLinearComposite            = 920;
  pdfReaderInitialisation       = 921;
  pdfStructuredAppendTerminator = 922;
  pdfOptionalFields             = 923;
  pdfLatchToByteFull            = 924;
  pdfECI_925                    = 925;
  pdfECI_926                    = 926;
  pdfECI_927                    = 927;
  pdfBeginStructuredAppend      = 928;

  latch_to_num                  = #28;
  switch_to_punct               = #29;
  latch_to_punct                = #25;
  latch_to_lower                = #27;
  switch_to_alpha               = #27;
  latch_to_alpha_num            = #28;
  latch_fr_mixed_to_alpha       = #28;
  latch_to_alpha_punct          = #29;


//---------------------------------------------------------------------------
// implementation of TpsPDF417Params class
//---------------------------------------------------------------------------
procedure TpsPDF417Params.SetFileName(const Value: String);
begin
  if FFileName<>Value then begin
    FFileName := Value;
    UpdateBarcode;
  end;
end;


procedure TpsPDF417Params.Assign(Source: TPersistent);
var v:TpsPDF417Params;
begin
  // inherited;
  if Source is TpsPDF417Params then begin
      v             := TpsPDf417Params(Source);
      FMode         := v.Mode;
      FRows         := v.Rows;
      FCols         := v.FCols;
      FSecurityLevel:= v.SecurityLevel;
      FTimeStamp    := v.TimeStamp;
      FSender       := v.Sender;
      FAddresse     := v.Addresse;
      FFileSize     := v.FileSize;
      FKind         := v.Kind;
      FUseMacro     := v.UseMacro;
      FFileName     := V.FileName;
      FChecksum     := V.Checksum;
      UpdateBarcode;
  end;
end;

function    TpsPDF417Params.BarcodeComponent:TComponent;
begin
      Result := FBarcode;
end;

constructor TpsPDF417Params.CreateBarcode(AOwner:TComponent);
begin
    inherited Create;
    FBarcode := AOwner;
    FRows    := 0;
    FCols    := 10;
    FMode    := psPDF417Alphanumeric;
end;

{
function TpsPDF417Params.OptionalFieldCount: Integer;
begin
  Result:=0;
  if FFileName<>'' then
    Inc(Result);
  if FTimeStamp>0 then
    Inc(Result);
  if FSender<>'' then
    Inc(Result);
  if FAddresse<>'' then
    Inc(Result);
  if FFileSize>0 then
    Inc(Result);
  if FSender<>'' then
    Inc(Result);
  if FCheckSum>0 then
    Inc(Result);
end;
}

procedure TpsPDF417Params.UpdateBarcode;
begin
    psUpdateBarcode(FBarcode);
end;

procedure TpsPDF417Params.SetMode(Value:TpsPDF417Mode);
begin
  if FMode<>Value then begin
      FMode:=Value;
      UpdateBarcode;
  end;
end;

procedure TpsPDF417Params.SetSecurityLevel(Value:TpsPDF417ErrorCorrection);
begin
  if FSecurityLevel<>Value then begin
    FSecurityLevel:=Value;
    UpdateBarcode;
  end;
end;

procedure TpsPDF417Params.SetRows(Value:Integer);
begin
  if FRows<>Value then begin
    FRows:=Value;
    UpdateBarcode;
  end;
end;

procedure TpsPDF417Params.SetCols(Value:Integer);
begin
  if FCols<>Value then begin
    FCols:=Value;
    UpdateBarcode;
  end;
end;

procedure TpsPDF417Params.SetAddresse(const Value: string);
begin
  if FAddresse<>Value then begin
    FAddresse := Value;
    UpdateBarcode;
  end;
end;

procedure TpsPDF417Params.SetChecksum(const Value: Integer);
begin
  if FCheckSum<>Value then begin
    FChecksum := Value;
    UpdateBarcode;
  end;
end;

procedure TpsPDF417Params.SetFileSize(const Value: Integer);
begin
  if FFileSize<>Value then begin
    FFileSize := Value;
    UpdateBarcode;
  end;
end;

procedure TpsPDF417Params.SetSender(const Value: String);
begin
  if FSender<>Value then begin
    FSender := Value;
    UpdateBarcode;
  end;
end;

procedure TpsPDF417Params.SetTimeStamp(const Value: TDateTime);
begin
  if FTimeStamp<>Value then begin
    FTimeStamp := Value;
    UpdateBarcode;
  end;
end;

procedure TpsPDF417Params.SetKind(Value:TpsPDF417Kind);
begin
  if FKind<>Value then begin
      FKind:= Value;
      UpdateBarcode;
  end;
end;

procedure TpsPDF417Params.SetUseMacro(Value:Boolean);
begin
  if FUseMAcro<>Value then begin
    FUseMacro:= Value;
    UpdateBarcode;
  end;
end;


function Pdf417CodewordToBars(cw:Integer; row:Integer):String;
var w:word;

    i:Integer;

begin

    w      := Pdf417Bits[row mod 3][cw];

    Result := StringOfChar('1',17);

    for i:=17 downto 2 do begin

        if (w and $1)=0 then

                Result[i]:='0';

        w := w shr 1;

    end;

end;


function Base256ToBase900(B6:TByte6):TInteger5;
var i:Integer;
    a, b: Array[0..4] of LongInt;
    carry, rem, temp : LongInt;
begin
    for i:=0 to 4 do begin
        a[i] := 0;
        b[i] := 0;
    end;

    // bytes 0-2
    // rem := 65536*B6[0]+256*B6[1]+B6[2];
    rem := B6[0] shl 16;
    Inc(rem, B6[1] shl 8);
    Inc(rem, B6[2]);
    i:=0;
    while rem>899 do begin
          // DivMod(rem, 900, rem, a[i]);
          a[i] := rem mod 900;
          rem  := rem div 900;
          Inc(i);
    end;
    a[i] := rem;

    // bytes 3-5
    rem := 65536*B6[3]+256*B6[4]+B6[5];
    i:=0;
    while rem>899 do begin
          b[i] := rem mod 900;
          rem      := rem div 900;
          Inc(i);
    end;
    b[i] := rem;

    Temp      := 316*a[0]+b[0];
    Result[4] := temp mod 900;
    carry     := temp div 900;

    temp      := (b[1]+641*a[0]+316*a[1]+carry);
    Result[3] := temp mod 900;
    carry     := temp div 900;

    temp      := (b[2]+20*a[0]+641*a[1]+316*a[2]+carry);
    Result[2] := temp mod 900;
    carry     := temp div 900;

    temp      := (b[3]+20*a[1]+641*a[2]+carry);
    Result[1] := temp mod 900;
    carry     := temp div 900;

    temp      := (20*a[2]+carry);
    Result[0] := temp mod 900;
end;

procedure GetValueMode(c:Char; var code:char; var mode:Integer);
var v:byte;
begin
    v:=0;
    case c of
        'A'..'Z' : begin v:=Ord(C)-OrdA; mode:=8; end;
        'a'..'z' : begin v:=Ord(C)-Ord('a'); mode:=4; end;
        '0'..'9' : begin v:=Ord(C)-Ord0; mode:=2; end;
        '&'      : begin v:=10; mode:=2; end;
        #13      : begin v:=11; mode:=3; end;
        #9       : begin v:=12; mode:=3; end;
        ','      : begin v:=13; mode:=3; end;
        ':'      : begin v:=14; mode:=3; end;
        '#'      : begin v:=15; mode:=2; end;
        '-'      : begin v:=16; mode:=3; end;
        '.'      : begin v:=17; mode:=3; end;
        '$'      : begin v:=18; mode:=3; end;
        '/'      : begin v:=19; mode:=3; end;
        '+'      : begin v:=20; mode:=2; end;
        '%'      : begin v:=21; mode:=2; end;
        '*'      : begin v:=22; mode:=3; end;
        '='      : begin v:=23; mode:=2; end;
        '^'      : begin v:=24; mode:=2; end;
        ' '      : begin v:=26; mode:=14; end;

        ';'      : begin v:= 0; mode:=1; end;
        '<'      : begin v:= 1; mode:=1; end;
        '>'      : begin v:= 2; mode:=1; end;
        '@'      : begin v:= 3; mode:=1; end;
        '['      : begin v:= 4; mode:=1; end;
        '\'      : begin v:= 5; mode:=1; end;
        ']'      : begin v:= 6; mode:=1; end;
        '_'      : begin v:= 7; mode:=1; end;
        '`'      : begin v:= 8; mode:=1; end;
        '~'      : begin v:= 9; mode:=1; end;
        '!'      : begin v:=10; mode:=1; end;
        #10      : begin v:=15; mode:=1; end;
        '"'      : begin v:=20; mode:=1; end;
        '|'      : begin v:=21; mode:=1; end;
        '('      : begin v:=23; mode:=1; end;
        ')'      : begin v:=24; mode:=1; end;
        '?'      : begin v:=25; mode:=1; end;
        '{'      : begin v:=26; mode:=1; end;
        '}'      : begin v:=27; mode:=1; end;
        ''''     : begin v:=28; mode:=1; end;
        else     mode:=-1;
    end;
    Code:=Char(v);
end;

procedure pdfEncodeText(s:String; var R:TpsArray; Latch:Boolean);
var code, code_temp : char;
    last_mode, mode, mode_temp, mode_prev : Integer;
    temp        : String;
    i           : Integer;
    single_capital, Num : Boolean;
begin
    if Length(s)=0 then
        Exit;

    if Latch then
        psDataPut(R, pdfLatchToText);

    temp      := '';
    i         := 1;
    Last_Mode := 8;

    while i<=Length(s) do begin
        GetValueMode(s[i], code, mode);
        Inc(i);

        if (Last_Mode and Mode)<>0 then begin
            temp:=temp+code;
            if (Mode=8) or (Mode=4) or (Mode=2) or (Mode=1) then
                Last_Mode := Mode;
        end else begin
            mode_prev := Last_Mode;
            // change subtype
            if (Mode_Prev and 8)<>0 then begin
                Num := False;
                if (Mode and 2)<>0 then begin
                    Num:=True;
                    temp := temp + latch_to_num + code;
                    Last_Mode := 2;
                end;
                if (not Num) and ((Mode and 1)<>0) then begin
                    if i<=Length(s) then
                        GetValueMode(s[i], code_temp, mode_temp)
                    else
                        mode_temp := 0;

                    if (mode_temp and 1)=0 then begin
                        temp:=temp+switch_to_punct+code;
                    end else begin
                        temp:=temp+latch_to_num+latch_to_punct+code;
                        last_mode := 1;
                    end;
                end;

                if (Mode and 4)<>0 then begin
                    temp:=temp+latch_to_lower+code;
                    last_mode:=mode;
                end;
            end;


            if (Mode_Prev and 4)<>0 then begin
                num:=False;
                if (Mode and 8)<>0 then begin
                    if i<=Length(s) then
                        GetValueMode(s[i], code_temp, mode_temp)
                    else
                        mode_temp := 0;

                    single_capital := (mode_temp and 8)=0;
                    if single_capital then
                        temp:=temp+switch_to_alpha+code
                    else begin
                        temp:=temp+latch_to_num+latch_to_alpha_num+code;
                        Last_Mode := 8;
                    end;
                end else
                if (Mode and 2)<>0 then begin
                    // num:=True;
                    temp:=temp+latch_to_num+code;
                    Last_Mode := 2;
                end else
                if (not Num) and ((Mode and 1)<>0) then begin
                    if i<=Length(s) then
                        GetValueMode(s[i], code_temp, mode_temp)
                    else
                        mode_temp := 0;

                    if (mode_temp and 1)=0 then begin
                        temp:=temp+switch_to_punct+code;
                    end else begin
                        temp:=temp+latch_to_num+latch_to_punct+code;
                        last_mode := 1;
                    end;
                end;
            end;


            if (Mode_Prev and 2)<>0 then begin
                if (Mode and 8)<>0 then begin
                    temp:=temp+latch_fr_mixed_to_alpha+Code;
                    Last_Mode := 8;
                end else
                if (Mode and 4)<>0 then begin
                    temp:=temp+latch_to_lower+code;
                    Last_Mode := 4;
                end else
                if (Mode and 1)<>0 then begin
                    if i<=Length(s) then
                        GetValueMode(s[i], code_temp, mode_temp)
                    else
                        mode_temp := 0;

                    if (mode_temp and 1)=0 then begin
                        temp:=temp+switch_to_punct+code;
                    end else begin
                        temp:=temp+latch_to_punct+code;
                        last_mode := 1;
                    end;
                end;
            end;


            if ((Mode_Prev and 1)<>0) and ((Mode_Prev and 2)=0) then begin
                if (Mode and 8)<>0 then begin
                    temp:=temp+latch_to_alpha_punct+code;
                    Last_Mode := 8;
                end else
                if (Mode and 4)<>0 then begin
                    temp:=temp+latch_to_alpha_punct+latch_to_lower+code;
                    Last_Mode := 4;
                end else
                if (Mode and 2)<>0 then begin
                    temp:=temp+latch_to_alpha_punct+latch_to_num+code;
                    Last_Mode := 2;
                end
            end;
        end;
    end;


    if (Length(temp) mod 2)<>0 then temp:=temp+#29;

    i:=1;
    while i<Length(temp) do begin
        psDataPut(R, 30*Ord(temp[i])+Ord(temp[i+1]) );
        Inc(i,2);
    end;
end;

procedure pdfEncodeNumeric(s:String; var R:TpsArray; Latch:Boolean);
    procedure PDF417NumericPart(ss:String; var R:TpsArray);
    var x,i:Integer;
        ss1:string;
        v:array [1..30] of Integer;
        v1:Integer;
    begin
        for i:=1 to 30 do
            v[i]:=-1;
        v1:=0;
        i :=1;
        while (ss<>'') and (ss<>'0') do begin
            ss1:='';
            x:=0;
            while (x<900) and (i<=Length(ss)) do begin
                x:=10*x+psNumericToInteger(ss[i]);
                Inc(i);
            end;
            while (i<=Length(ss)+1) do begin
                    ss1:=ss1+Char(Ord0+x div 900);
                    if i<=Length(ss) then begin
                        x:=10*(x mod 900)+psNumericToInteger(ss[i]);
                        Inc(i);
                    end else Break;
            end;
            Inc(v1);
            v[v1]:=x mod 900;
            ss := ss1;
            i  := 1;

        end;
        for i:=30 downto 1 do
            if v[i]>=0 then
                psDataPut(R, v[i]);
    end;
begin
    s:='1'+s;

    // swith to numeric mode
    if Latch then
        psDataPut(R, pdfLatchToNumeric);

    while Length(s)>44 do begin
        PDF417NumericPart(Copy(s,1,44), R);
        s:=Copy(s,45,Length(s)-44);
    end;
    if s<>'' then
        PDF417NumericPart(s, R);
end;

procedure pdfEncodeBinary(s:String; var R:TpsArray; Latch:Boolean);
var i,j:Integer;
    B  :TByte6;
    V  :TInteger5;
begin
    i:=1;
    if Length(s) mod 6 = 0 then
        psDataPut(R, pdfLatchToByteFull)
    else
        psDataPut(R, pdfLatchToBytePart);

    while Length(s)-i>=5 do begin
        for j:=0 to 5 do
            B[j]:=Ord(s[i+j]);
        V := Base256ToBase900(B);

        for j:=0 to 4 do
                psDataPut(R, V[j]);
        Inc(i,6);
    end;

    while i<=Length(s) do begin
        psDataPut(R, Ord(s[i]) );
        Inc(i);
    end;
end;


procedure PdfBinaryHexToCodeWords(s:String; var R:TpsArray);
var temp:String;
    i:Integer;
    j:Integer;
begin
     if (Length(s) mod 2)<>0 then
        s:='0'+s;
     temp := '';
     i    := 1;
     while i<Length(s) do begin
           j:=16*psHexDigit(s[i])+psHexDigit(s[i]);
           temp:=temp+Char(j);
           Inc(i,2);
     end;

     pdfEncodeBinary(temp, R, True);
end;

function Pdf417LeftRowIndicator(RowNumber,NumberOfRows,SecurityLevel,ColumnCount:Integer):Integer;
begin
        Result := 0;
        case (RowNumber mod 3) of
                0 : Result := 30*(RowNumber div 3)+(NumberOfRows-1) div 3;
                1 : Result := 30*(RowNumber div 3)+(SecurityLevel*3) + (NumberOfRows-1) mod 3;
                2 : Result := 30*(RowNumber div 3)+(ColumnCount-1);
        end;
end;

function Pdf417RightRowIndicator(RowNumber,NumberOfRows,SecurityLevel,ColumnCount:Integer):Integer;
begin
        Result := 0;
        case (RowNumber mod 3) of
                0 : Result := 30*(RowNumber div 3)+(ColumnCount-1);
                1 : Result := 30*(RowNumber div 3)+(NumberOfRows-1) div 3;
                2 : Result := 30*(RowNumber div 3)+(SecurityLevel*3) + (NumberOfRows-1) mod 3;
        end;
end;



function Pdf417OneRowToBars(row,RowCount,SecurityLevel:Integer; CodeWords:TpsArray):String;
var li,ri:word;
    i : Integer;
begin
        li := Pdf417LeftRowIndicator(Row,RowCount,SecurityLevel,CodeWords.Count);
        ri := Pdf417RightRowIndicator(Row,RowCount,SecurityLevel,CodeWords.Count);

        for i:=0 to CodeWords.Count-1 do
                Result := Result + Pdf417CodewordToBars(CodeWords.Data[i], row);

        Result := PDF417_Start + Pdf417CodewordToBars(li, row);
        Result := Result + Pdf417CodewordToBars(ri, row) + PDF417_Stop;
end;


function PdfCalcErrorCodes(D:TpsArray; ErrCount:Integer):TpsArray;
var i,j     : Integer;
    t1,t2,t3: Integer;
    aj,a0   : Integer;
    ck_m1   : Integer;
    tbl     : array of word;
begin
  Result.Count:=ErrCount;

  case ErrCount of
    2   : tbl:= @ErrorTable2;
    3   : tbl:= @ErrorTable3;
    4   : tbl:= @ErrorTable4;
    5   : tbl:= @ErrorTable5;
    6   : tbl:= @ErrorTable6;
    7   : tbl:= @ErrorTable7;
    8   : tbl:= @ErrorTable8;
    9   : tbl:= @ErrorTable9;
    10  : tbl:= @ErrorTable10;
    11  : tbl:= @ErrorTable11;
    12  : tbl:= @ErrorTable12;
    13  : tbl:= @ErrorTable13;
    14  : tbl:= @ErrorTable14;
    15  : tbl:= @ErrorTable15;
    16  : tbl:= @ErrorTable16;
    18  : tbl:= @ErrorTable18;
    21  : tbl:= @ErrorTable21;
    26  : tbl:= @ErrorTable26;
    32  : tbl:= @ErrorTable32;
    38  : tbl:= @ErrorTable38;
    44  : tbl:= @ErrorTable44;
    50  : tbl:= @ErrorTable50;
    64  : tbl:= @ErrorTable64;
    128 : tbl:= @ErrorTable128;
    256 : tbl:= @ErrorTable256;
    512 : tbl:= @ErrorTable512;
    else
      raise psBarcodeException.Create('PDF417 Error correction : bad count of ECW');
  end;

  for i:=0 to D.Count-1 do begin
    ck_m1:= Result.Data[Result.Count-1];
    t1 := (D.Data[i]+ck_m1) mod 929;

    for j:=Result.Count-1 downto 1 do begin
      aj := tbl[j];
      t2 := (t1*aj) mod 929;
      t3 := 929 - t2;
      Result.Data[j] := (Result.Data[j-1]+t3) mod 929;
    end;
    a0 := tbl[0];
    t2 := (t1*a0) mod 929;
    t3 := 929 - t2;
    Result.Data[0] := t3 mod 929;
  end;

  for j:=0 to Result.Count-1 do
    if Result.Data[j]<>0 then
      Result.Data[j]:= 929-Result.Data[j];
end;


procedure PDF417Check(s:String; CodingScheme:TpsPDF417Mode);
var i:Integer;
    ok:Boolean;
    c :Char;
begin
     ok:=True;
     i:=0;
     case CodingScheme of
          psPDF417Alphanumeric  :
               for i:=1 to Length(s) do begin
                   c:=s[i];
                   if not( IsUpper(c) or IsLower(c) or IsDigit(c)
                    or (Pos(c,'&'#9#13',:#-.$/+%*=^ ;<>@[\]_`~!'#10'"|()?{}''')>0)) then begin
                      ok:=False;
                      Break;
                   end;
               end;
          psPDF417BinaryHex     :
               for i:=1 to Length(s) do begin
                   c:=s[i];
                   if not( IsDigit(c) or ((c>='A') and (c<='F')) or ((c>='a') and (c<='f'))) then begin
                      ok:=False;
                      Break;
                   end;
               end;
          psPDF417Numeric       :
               for i:=1 to Length(s) do
                   if not IsDigit(s[i]) then begin
                      ok:=False;
                      Break;
                   end;
     end;

     if not OK then
        Raise psBarcodeException.Create(Format(rsBadCharInPDF417,[i,s[i]]));
end;

procedure PdfEncodeAutomatic(const S:String; var D:TpsArray);
var idx : Integer;
    enc : TpsPDF417Mode;
    N   : Integer;
    T   : Integer;
    B   : Integer;
    i   : Integer;
begin
    // pdfEncodeBinary(S, D, True);
    idx := 1;
    enc := psPDF417Binary;

    while idx<=Length(s) do begin
        // 3.
        N:=DigitsCount(s, idx);
        if N>=13 then begin
            PdfEncodeNumeric(Copy(s, idx, N), D, True);
            Inc(idx, N);
            Continue;
        end;

        //10.
        T := 0;
        i := idx;
        while i<=Length(s) do begin
             if Ord(s[i]) in [32..126] then begin
                if IsDigit(s[i]) then begin
                    N := DigitsCount(s, i);
                    if N>=13 then
                      Break;
                end;
                Inc(T);
             end
                else
                    Break;
             Inc(i);
        end;

        // 11.
        if T>=5 then begin
            pdfEncodeText(Copy(s,idx, T), D, True);
            Inc(idx, T);
            enc := psPDF417Alphanumeric;
            Continue;
        end;

        T := 0;
        B := 0;
        i := idx;
        while i<=Length(s) do begin
             if Ord(s[i]) in [32..126] then begin
                if IsDigit(s[i]) then begin
                    N := DigitsCount(s, i);
                    if N>=13 then
                        Break;
                end;
                Inc(T);
                if T>=5 then
                    Break;
             end else begin
                T:=0;
             end;
             Inc(B);
             Inc(i);
        end;

        if (B=1) and (enc=psPDF417Alphanumeric) then begin
            psDataPut(d, pdfShiftToByteCompaction);
            psDataPut(d, Ord(s[idx]));
            Inc(idx, B);
            Continue;
        end;

        pdfEncodeBinary(Copy(s,idx,B), D, True);

        Inc(idx, B);
    end;
end;


procedure PDF417Encode(const s:string; Mode:TpsPDF417Mode; var D:TpsArray);
begin
  // D.Count:=1;
  // psDataPut(D, 0);

  case Mode of
    psPDF417Alphanumeric  : pdfEncodeText(S, D, True);
    psPDF417Binary        : pdfEncodeBinary(S, D, True);
    psPDF417BinaryHex     : PdfBinaryHexToCodeWords(S, D);
    psPDF417Numeric       : pdfEncodeNumeric(S, D, True);
    psPDF417AutoCode      : PdfEncodeAutomatic(S, D);
  end;
end;

function  pdf417EncodeMacroBlock(par:TpsParams; var M:TpsArray):Integer;
var s:string;
    i,k:Integer;
    d:TDateTime;
begin
//  Result := 0;
  psArrayInitialize(M, 1000);

  M.Count:=0;

  if par.PDF417.UseMacro or (par.SegmentCount>1) then begin
      psDataPut(M, pdfBeginStructuredAppend);
      s:=psIntToStr(par.SegmentIndex,5,'0');
      pdfEncodeNumeric(s, M, False);

      // encode FileID
      i:=1;
      s:=par.FileID;
      while i<=Length(s) do begin
          k:=StrToIntDef(Copy(s,i,3), -1);
          if (k<0) or (k>899) then
              k:=0;
          Inc(i,3);
          psDataPut(M, k);
      end;

      // encode SegmentCount
      psDataPut(M, pdfOptionalFields);
      psDataPut(M, 1);
      s:=psIntToStr(par.SegmentCount, 5, '0');
      pdfEncodeNumeric(s, M, False);
  end;

  // encode other optional fields
  if par.PDF417.UseMacro then begin
    if par.PDF417.FileName<>'' then begin
          psDataPut(M, pdfOptionalFields);
          psDataPut(M, 0);
          pdfEncodeText(par.PDF417.FileName, M, False);
    end;

    if par.pdf417.TimeStamp>0 then begin
          psDataPut(M, pdfOptionalFields);
          psDataPut(M, 2);
          d := EncodeDateTime(1970, 1, 1, 0, 0, 0, 0);
          i := Trunc( (par.PDF417.TimeStamp - d)*24*60*60 );
          s := psIntToStr(i, 10, '0');
          pdfEncodeNumeric(s, M, False);
    end;

    if par.PDF417.Sender<>'' then begin
          psDataPut(M, pdfOptionalFields);
          psDataPut(M, 3);
          pdfEncodeText(par.PDF417.Sender, M, False);
    end;

    if par.PDF417.Addresse<>'' then begin
          psDataPut(M, pdfOptionalFields);
          psDataPut(M, 4);
          pdfEncodeText(par.PDF417.Addresse, M, False);
    end;

    if par.pdf417.FileSize>0 then begin
          psDataPut(M, pdfOptionalFields);
          psDataPut(M, 5);
          s:=IntToStr(par.PDF417.FileSize);
          pdfEncodeNumeric(s, M, False);
    end;

    if par.pdf417.CheckSum>0 then begin
          psDataPut(M, pdfOptionalFields);
          psDataPut(M, 6);
          s:=psIntToStr(par.Pdf417.CheckSum, 5, '0');
          pdfEncodeNumeric(s, M, False);
    end;
  end;

  if par.SegmentIndex=par.SegmentCount-1 then
      psDataPut(M, pdfStructuredAppendTerminator);

  Result :=M.Count;
end;

procedure psPDF417CalcSize(var cols, rows:Integer; TotalCount:Integer);
const pdf417Ratio       = 3;
      pdf417MaxCapacity = 90 * 30;
  procedure pdf417MaxCapacityError;
  begin
      raise psBarcodeException.Create('PDF417 Max data error');
  end;
var x,y:Integer;
begin
    if TotalCount>929 then
        pdf417MaxCapacityError;

    if TotalCount>pdf417MaxCapacity then
        pdf417MaxCapacityError;

    // solve when not selected cols/rows...
    if (Cols<=0) and (Rows<=0) then begin
        y    := Round(sqrt(TotalCount/pdf417Ratio));
        x    := y * 3;
        if x*y<TotalCount then
            Inc(x,1);

        if x*y<TotalCount then
            Inc(y,1);
        if x>90 then
            pdf417MaxCapacityError;
        if y>30 then
            pdf417MaxCapacityError;
        Cols := x;
        Rows := y;
        Exit;
    end;

    // if rows selected....
    if Rows>0 then begin
        if rows<3 then rows :=3;

        Cols := Round(TotalCount/Rows)+1;
        if Cols>30 then
            pdf417MaxCapacityError
        else
            Exit;
    end;

    if Cols>0 then begin
        if Cols<3 then
            Cols:=3;
        Rows := Round(TotalCount/Cols)+1;
        if Rows>90 then
            pdf417MaxCapacityError
        else
            Exit;
    end;
end;

procedure PDF417EncodeECI(ECIValue:Integer; AtStart:Boolean; var D:TpsArray);
begin
    if ECIValue<0 then
        Exit;

    if AtStart and ((ECIValue>=900) and (ECIValue<=1799)) then begin
        psDataPut(D, ECIValue Mod 900);
        Exit;
    end;

    if ECIValue<=899 then begin
        psDataPut(D, pdfECI_927);
        psDataPut(D, ECIValue);
    end else
    if ECIValue<=810899 then begin
        psDataPut(D, pdfECI_926);
        psDataPut(D, (ECIValue div 900) - 1);
        psDataPut(D, ECIValue mod 900);
    end else
    if ECIValue<811799 then begin
        psDataPut(D, pdfECI_925);
        psDataPut(D, ECIValue - 810900);
    end;
end;

procedure PDF417PrepareCodeWords(S:String; var Cols, Rows:Integer;
    bc:TpsBarcodeComponent; var D:TpsArray; var ErrCount, SecurityLevel:Integer;
    var SecLevel   : TpsPDF417ErrorCorrection);
var
    M, E       : TpsArray;
    PDF        : TpsPDF417Params;
    DataCount  : Integer;
    MacroCount : Integer;
    PADCount   : Integer;
    TotalCount : Integer;
    i          : Integer;
begin
    PDF :=  bc.Params.PDF417;
    PDF417Check(s, PDF.Mode);
    PDF417EncodeECI(bc.Params.ECI, True, D);

    psDataPut(D,900);
    PDF417Encode(S, PDF.Mode, D);

    DataCount := D.count;

    MacroCount := 0;
    if PDF.UseMacro then begin
        psArrayInitialize(M,1000);
        pdf417EncodeMacroBlock(bc.Params, M);
        MacroCount := M.count;
    end;

    SecLevel := PDF.SecurityLevel;
    if SecLevel=psPDF417AutoEC then
        case D.Count + MacroCount of
             1.. 40 : SecLevel := psPDF417Error2;
            41..160 : SecLevel := psPDF417Error3;
           161..320 : SecLevel := psPDF417Error4;
           321..863 : SecLevel := psPDF417Error5;
           else       SecLevel := psPDF417Error6;
        end;

    SecurityLevel := Integer(SecLevel)-1;
    ErrCount      := 1 shl (SecurityLevel+1);

    TotalCount := DataCount + MacroCount + ErrCount;

    psPDF417CalcSize(Cols,Rows, TotalCount);

    // add pad codewords here
    PADCount := Cols*Rows-TotalCount;
    for i:=1 to PADCount do
          psDataPut(D, pdfLatchToText);
    // add macro block
    if PDF.UseMacro then
        psDataAdd(D, M);

    // first codeword assign total codewords in symbol
    // not used for MicroPDF417
    psDataSet(D, D.Count, 0);

    psArrayInitialize(E,5000);
    E:=PdfCalcErrorCodes(D, ErrCount);

    // psDataAdd(D, E);
    for i:=0 to E.Count-1 do
        psDataPut(D, E.data[E.Count-1-i]);
end;

procedure PDF417MicroPrepareCodeWords(S:String; var Cols, Rows:Integer;
    bc:TpsBarcodeComponent; var D:TpsArray; var version:Integer);
var
    M, E       : TpsArray;
    PDF        : TpsPDF417Params;
    DataCount  : Integer;
    MacroCount : Integer;
    PADCount   : Integer;
    TotalCount : Integer;
    i          : Integer;
    errCount   : Integer;
begin
    if Cols>4 then
        Cols := 4;
    PDF :=  bc.Params.PDF417;
    PDF417Check(s, PDF.Mode);
    PDF417EncodeECI(bc.Params.ECI, True, D);
    PDF417Encode(S, PDF.Mode, D);

    DataCount := D.count;

    MacroCount := 0;
    if PDF.UseMacro then begin
        psArrayInitialize(M,1000);
        pdf417EncodeMacroBlock(bc.Params, M);
        MacroCount := M.count;
    end;

    version  := -1;
    ErrCount := 0;
    for i := 1 to pdf417MicroVersionsCount do begin
            if (Cols>0) and (Cols<>tblPDF417MicroVersions[i,1]) then
                Continue;
            if (Rows>0) and (Rows<>tblPDF417MicroVersions[i,2]) then
                Continue;
        if tblPDF417MicroVersions[i,6]>=DataCount+MacroCount then begin

            version  := i;
            Cols     := tblPDF417MicroVersions[i,1];
            Rows     := tblPDF417MicroVersions[i,2];
            ErrCount := tblPDF417MicroVersions[i,4];
            break;
        end;
    end;

    if version=-1 then
        raise psBarcodeException.Create('PDF417 max. capacity error.');

    TotalCount := DataCount + MacroCount + ErrCount;

    // add pad codewords here
    PADCount := Cols*Rows-TotalCount;
    for i:=1 to PADCount do
          psDataPut(D, pdfLatchToText);
    // add macro block
    if PDF.UseMacro then
        psDataAdd(D, M);

    // psDataSet(D, pdfLatchToText {D.Count}, 0);

    // D.data[0]:=913;
    // D.data[1]:=66;
    psArrayInitialize(E,5000);
    E:=PdfCalcErrorCodes(D, ErrCount);

    // psDataAdd(D, E);
    for i:=0 to E.Count-1 do
        psDataPut(D, E.data[E.Count-1-i]);
end;


// ----------------------------------------------------------------------------
// tables for MicroPDF417
// ----------------------------------------------------------------------------

{
type  pdf417def=record
        cols:Integer;
        rows:integer;
        cw  :integer;
        ew  :integer;
        MaxData,MaxAlpha,MaxDigits:Integer;
      end;

const tblMicroPDF417sizes:array[1..34] of pdf417def=(
      (cols: 1; rows: 11; cw :11; ew: 7; MaxData:3;  MaxAlpha:6;  MaxDigits:8),
      (cols: 1; rows: 14; cw :14; ew: 7; MaxData:7;  MaxAlpha:12; MaxDigits:17),
      (cols: 1; rows: 17; cw :17; ew: 7; MaxData:10; MaxAlpha:18; MaxDigits:26),
      (cols: 1; rows: 20; cw :20; ew: 8; MaxData:13; MaxAlpha:22; MaxDigits:32),
      (cols: 1; rows: 24; cw :24; ew: 8; MaxData:18; MaxAlpha:30; MaxDigits:44),
      (cols: 1; rows: 28; cw :28; ew: 8; MaxData:22; MaxAlpha:38; MaxDigits:55),

      (cols: 2; rows:  8; cw : 16; ew: 8; MaxData:8;  MaxAlpha:14; MaxDigits:20),
      (cols: 2; rows: 11; cw : 22; ew: 9; MaxData:14; MaxAlpha:24; MaxDigits:35),
      (cols: 2; rows: 14; cw : 28; ew: 9; MaxData:21; MaxAlpha:36; MaxDigits:52),
      (cols: 2; rows: 17; cw : 34; ew: 10;MaxData:27; MaxAlpha:46; MaxDigits:67),
      (cols: 2; rows: 20; cw : 40; ew: 11;MaxData:33; MaxAlpha:56; MaxDigits:82),
      (cols: 2; rows: 23; cw : 46; ew: 13;MaxData:38; MaxAlpha:64; MaxDigits:93),
      (cols: 2; rows: 26; cw : 52; ew: 15;MaxData:43; MaxAlpha:72; MaxDigits:105),

      (cols: 3; rows:  6; cw : 18; ew: 12; MaxData: 6; MaxAlpha: 10; MaxDigits:14),
      (cols: 3; rows:  8; cw : 24; ew: 14; MaxData:10; MaxAlpha: 18; MaxDigits:26),
      (cols: 3; rows: 10; cw : 30; ew: 16; MaxData:15; MaxAlpha: 26; MaxDigits:38),
      (cols: 3; rows: 12; cw : 36; ew: 18; MaxData:20; MaxAlpha: 34; MaxDigits:49),
      (cols: 3; rows: 15; cw : 45; ew: 21; MaxData:27; MaxAlpha: 46; MaxDigits:67),
      (cols: 3; rows: 20; cw : 60; ew: 26; MaxData:39; MaxAlpha: 66; MaxDigits:96),
      (cols: 3; rows: 26; cw : 78; ew: 32; MaxData:54; MaxAlpha: 90; MaxDigits:132),
      (cols: 3; rows: 32; cw : 96; ew: 38; MaxData:68; MaxAlpha:114; MaxDigits:167),
      (cols: 3; rows: 38; cw :114; ew: 44; MaxData:82; MaxAlpha:138; MaxDigits:202),
      (cols: 3; rows: 44; cw :132; ew: 50; MaxData:97; MaxAlpha:162; MaxDigits:237),

      (cols: 4; rows:  4; cw : 16; ew:  8; MaxData:  8; MaxAlpha: 14; MaxDigits: 20),
      (cols: 4; rows:  6; cw : 24; ew: 12; MaxData: 13; MaxAlpha: 22; MaxDigits: 32),
      (cols: 4; rows:  8; cw : 32; ew: 14; MaxData: 20; MaxAlpha: 34; MaxDigits: 49),
      (cols: 4; rows: 10; cw : 40; ew: 16; MaxData: 27; MaxAlpha: 46; MaxDigits: 67),
      (cols: 4; rows: 12; cw : 48; ew: 18; MaxData: 34; MaxAlpha: 58; MaxDigits: 85),
      (cols: 4; rows: 15; cw : 60; ew: 21; MaxData: 45; MaxAlpha: 76; MaxDigits:111),
      (cols: 4; rows: 20; cw : 80; ew: 26; MaxData: 63; MaxAlpha:106; MaxDigits:155),
      (cols: 4; rows: 26; cw :104; ew: 32; MaxData: 85; MaxAlpha:142; MaxDigits:208),
      (cols: 4; rows: 32; cw :128; ew: 38; MaxData:106; MaxAlpha:178; MaxDigits:261),
      (cols: 4; rows: 38; cw :152; ew: 44; MaxData:128; MaxAlpha:214; MaxDigits:313),
      (cols: 4; rows: 44; cw :176; ew: 50; MaxData:150; MaxAlpha:250; MaxDigits:366)
    );
}

type
    MicroPDF417rowPattern = record
      lr : string; // string[10];
      c  : string; // string[10];
    end;

const
    MicroPDF417rowPatterns: array [1..52] of MicroPDF417rowPattern=(
      { 1} (lr:'1100100010'; c:'1011001110'),
      { 2} (lr:'1110100010'; c:'1001001110'),
      { 3} (lr:'1110110010'; c:'1001101110'),
      { 4} (lr:'1100110010'; c:'1000101110'),
      { 5} (lr:'1101110010'; c:'1000100110'),
      { 6} (lr:'1101111010'; c:'1000110110'),
      { 7} (lr:'1100111010'; c:'1000010110'),
      { 8} (lr:'1110111010'; c:'1000010010'),
      { 9} (lr:'1110011010'; c:'1000011010'),
      {10} (lr:'1111011010'; c:'1000111010'),
      {11} (lr:'1111001010'; c:'1000110010'),
      {12} (lr:'1110001010'; c:'1000100010'),
      {13} (lr:'1100001010'; c:'1001100010'),
      {14} (lr:'1100011010'; c:'1001110010'),
      {15} (lr:'1100010010'; c:'1001111010'),
      {16} (lr:'1110010010'; c:'1011111010'),
      {17} (lr:'1111010010'; c:'1011110010'),
      {18} (lr:'1111010110'; c:'1011110110'),
      {19} (lr:'1111010100'; c:'1001110110'),
      {20} (lr:'1110010100'; c:'1001110100'),
      {21} (lr:'1110110100'; c:'1001100100'),
      {22} (lr:'1110100100'; c:'1001100110'),
      {23} (lr:'1110100110'; c:'1001000110'),
      {24} (lr:'1110101110'; c:'1001000010'),
      {25} (lr:'1110101100'; c:'1011000010'),
      {26} (lr:'1110101000'; c:'1011100010'),
      {27} (lr:'1100101000'; c:'1011100110'),
      {28} (lr:'1100101100'; c:'1011100100'),
      {29} (lr:'1100101110'; c:'1011101100'),
      {30} (lr:'1100100110'; c:'1001101100'),
      {31} (lr:'1100110110'; c:'1000101100'),
      {32} (lr:'1110110110'; c:'1000101000'),
      {33} (lr:'1110010110'; c:'1001101000'),
      {34} (lr:'1100010110'; c:'1011101000'),
      {35} (lr:'1100010100'; c:'1011001000'),
      {36} (lr:'1100110100'; c:'1011001100'),
      {37} (lr:'1101110100'; c:'1011000100'),
      {38} (lr:'1101100100'; c:'1011000110'),
      {39} (lr:'1101100110'; c:'1010000110'),
      {40} (lr:'1101101110'; c:'1010001110'),
      {41} (lr:'1101101100'; c:'1010001100'),
      {42} (lr:'1101101000'; c:'1010011100'),
      {43} (lr:'1101001000'; c:'1010011000'),
      {44} (lr:'1101011000'; c:'1010111000'),
      {45} (lr:'1101011100'; c:'1010110000'),
      {46} (lr:'1101011110'; c:'1010010000'),
      {47} (lr:'1101001110'; c:'1011010000'),
      {48} (lr:'1101001100'; c:'1001010000'),
      {49} (lr:'1101000100'; c:'1001011000'),
      {50} (lr:'1101000110'; c:'1001011100'),
      {51} (lr:'1101000010'; c:'1011011100'),
      {52} (lr:'1101100010'; c:'1011011110')
    );

procedure CompleteMicroPDF417(L:TStringList; const D:TpsArray; Version:Integer);
var y,x : Integer;
    s   : String;
    tmp : Integer;
    lr, cr, rr, cluster : Integer;
    cols, rows          : Integer;
begin
  cols := tblPDF417MicroVersions[Version,1];
  rows := tblPDF417MicroVersions[Version,2];
  lr   := tblPDF417MicroVersions[Version,13];
  cr   := tblPDF417MicroVersions[Version,14];
  rr   := tblPDF417MicroVersions[Version,15];
  for y := 0 to Rows - 1 do begin
    cluster := (lr + y);
    s:=MicroPDF417rowPatterns[((lr + y) MOD 52) + 1].lr;
    for x := 0 to Cols - 1 do  begin
      if ((cols=3) and (x=1))  or ((cols=4) and (x=2)) then
          s:=s+MicroPDF417rowPatterns[((cr + y) mod 52) + 1].c;
      tmp := D.Data[y*cols+x];
      s   := s + Pdf417CodewordToBars(tmp, cluster);
    end;
    s:=s+MicroPDF417rowPatterns[((rr + y) mod 52) + 1].lr+'1';
    L.Add(s);
  end;
end;


procedure psPDF417GetLines(bc:TComponent; L:TStringList);
const ansi_len=10000;
var s : String;
    D:TpsArray;
    ErrCount:Integer;
    i, j, CodeWordsInRow, tmp :Integer;
    one_line:String;
    SecurityLevel : Integer;
    Barcode       : TpsBarcodeComponent;
    SecLevel      : TpsPDF417ErrorCorrection;
    MicroVersion  : Integer;
    Cols, Rows    : Integer;
    PDF417        : TpsPDF417Params;
begin
    Barcode := bc as TpsBarcodeComponent;
    L.Clear;
    s := ConvertUnicodeToUTF8(Barcode.Barcode);
    PDF417:=Barcode.Params.PDF417;

    Cols := PDF417.Cols;
    Rows := PDF417.Rows;

    psArrayInitialize(D,5000);
    if PDF417.Kind=pkMicro then begin
        PDF417MicroPrepareCodeWords(S, Cols, Rows, Barcode, D, microVersion);
        if D.count>125 then
            raise psBarcodeException.Create('MicroPDF417 max. capacity is 125 codewords');

        PDF417.FUsedRows := Rows;
        PDF417.FUsedCols := Cols;
        PDF417.FUsedECL  := psPDF417AutoEC;
        CompleteMicroPDF417(L, D, MicroVersion);
    end else begin
        PDF417PrepareCodeWords(S, Cols, Rows, Barcode, D, ErrCount,
            SecurityLevel, SecLevel);
        if D.count>929 then
            raise psBarcodeException.Create('PDF417 max. capacity is 929 codewords');


        PDF417.FUsedRows := Rows;
        PDF417.FUsedCols := Cols;
        PDF417.FUsedECL  := SecLevel;

        CodeWordsInRow := D.Count div Rows;
        for i:=0 to Rows-1 do begin
            one_line := Pdf417_Start;
            tmp := Pdf417LeftRowIndicator(i,Rows,SecurityLevel,CodeWordsInRow);
            one_line := one_line + Pdf417CodewordToBars(tmp, i);

            for j:=0 to CodeWordsInRow-1 do begin
                tmp      := D.Data[i*CodeWordsInRow+j];
                one_line := one_line + Pdf417CodewordToBars(tmp, i);
            end;

            if PDF417.Kind = pkTruncated then
                one_line:=one_line+'1'
            else begin
                tmp := Pdf417RightRowIndicator(i,Rows,SecurityLevel,CodeWordsInRow);
                one_line := one_line + Pdf417CodewordToBars(tmp, i);
                one_line := one_line + Pdf417_Stop;
            end;
            L.Add(one_line);
        end;
    end;
end;


procedure psPDF417GetPixels(S:String; var Cols, Rows:Integer; Barcode:TComponent);
{var D,E:TpsIntegerArray;
    ErrCount:Integer;
    i, j, CodeWordsInRow, tmp :Integer;
    one_line:String;
    SecurityLevel : Integer;
    bc : TpsCustomBarcode;
}begin
{    bc:=BarcodeObject as TpsCustomBarcode;

    PDF417PrepareCodeWords(S, Cols, Rows, PDF, D, ErrCount,SecurityLevel);
    E:=PdfCalcErrorCodes(D, SecurityLevel, ErrCount);
    for i:=0 to E.Count-1 do
        PutCodeWord(D, E.CodeWords[E.Count-1-i]);

    if PDF.Kind=pkMicro then begin
            CompleteMicroPDF417(L, D, Cols, Rows);
    end else begin
        CodeWordsInRow := D.Count div Rows;
        for i:=0 to Rows-1 do begin
            one_line := Pdf417_Start;
            tmp := Pdf417LeftRowIndicator(i,Rows,SecurityLevel,CodeWordsInRow);
            one_line := one_line + Pdf417CodewordToBars(tmp, i);

            for j:=0 to CodeWordsInRow-1 do begin
                tmp      := D.CodeWords[i*CodeWordsInRow+j];
                one_line := one_line + Pdf417CodewordToBars(tmp, i);
            end;

            if PDF.Kind = pkTruncated then
                one_line:=one_line+'1'
            else begin
                tmp := Pdf417RightRowIndicator(i,Rows,SecurityLevel,CodeWordsInRow);
                one_line := one_line + Pdf417CodewordToBars(tmp, i);
                one_line := one_line + Pdf417_Stop;
            end;
            L.Add(one_line);
        end;
    end;
}

end;




// ******************************************************************************
// *** IntelligentMail
// ******************************************************************************

const _0_ = 0;
      _1_ = 4;
      _2_ = 8;
      _3_ = 12;

      GenPoly  = $F35;
      GenFrame = $7FF;

      IntelligentMailTbl1 : Array [0..1286] of word =(
        1    ,7936 ,77   ,7808 ,55   ,7552 ,59   ,7040 ,61   ,6016 ,
        62   ,3968 ,79   ,7744 ,87   ,7488 ,91   ,6976 ,93   ,5952 ,
        94   ,3904 ,103  ,7360 ,107  ,6848 ,109  ,5824 ,110  ,
        3776 ,115  ,6592 ,117  ,5568 ,118  ,3520 ,121  ,5056 ,122  ,
        3008 ,124  ,1984 ,143  ,7712 ,151  ,7456 ,155  ,6944 ,
        157  ,5920 ,158  ,3872 ,167  ,7328 ,171  ,6816 ,173  ,5792 ,
        174  ,3744 ,179  ,6560 ,181  ,5536 ,182  ,3488 ,185  ,5024 ,
        186  ,2976 ,188  ,1952 ,199  ,7264 ,203  ,6752 ,205  ,
        5728 ,206  ,3680 ,211  ,6496 ,213  ,5472 ,214  ,3424 ,217  ,
        4960 ,218  ,2912 ,220  ,1888 ,227  ,6368 ,229  ,5344 ,230  ,
        3296 ,233  ,4832 ,234  ,2784 ,236  ,1760 ,241  ,4576 ,242  ,
        2528 ,244  ,1504 ,248  ,992  ,271  ,7696 ,279  ,7440 ,283  ,
        6928 ,285  ,5904 ,286  ,3856 ,295  ,7312 ,299  ,6800 ,301  ,
        5776 ,302  ,3728 ,307  ,6544 ,309  ,5520 ,310  ,3472 ,313  ,
        5008 ,314  ,2960 ,316  ,1936 ,327  ,7248 ,331  ,6736 ,333  ,
        5712 ,334  ,3664 ,339  ,6480 ,341  ,5456 ,342  ,3408 ,345  ,
        4944 ,346  ,2896 ,348  ,1872 ,355  ,6352 ,357  ,5328 ,358  ,
        3280 ,361  ,4816 ,362  ,2768 ,364  ,1744 ,369  ,4560 ,370  ,
        2512 ,372  ,1488 ,376  ,976  ,391  ,7216 ,395  ,6704 ,397  ,
        5680 ,398  ,3632 ,403  ,6448 ,405  ,5424 ,406  ,3376 ,409  ,
        4912 ,410  ,2864 ,412  ,1840 ,419  ,6320 ,421  ,5296 ,422  ,
        3248 ,425  ,4784 ,426  ,2736 ,428  ,1712 ,433  ,4528 ,434  ,
        2480 ,436  ,1456 ,440  ,944  ,451  ,6256 ,453  ,5232 ,454  ,
        3184 ,457  ,4720 ,458  ,2672 ,460  ,1648 ,465  ,4464 ,466  ,
        2416 ,468  ,1392 ,472  ,880  ,481  ,4336 ,482  ,2288 ,484  ,
        1264 ,488  ,752  ,527  ,7688 ,535  ,7432 ,539  ,6920 ,541  ,
        5896 ,542  ,3848 ,551  ,7304 ,555  ,6792 ,557  ,5768 ,558  ,
        3720 ,563  ,6536 ,565  ,5512 ,566  ,3464 ,569  ,5000 ,570  ,
        2952 ,572  ,1928 ,583  ,7240 ,587  ,6728 ,589  ,5704 ,590  ,
        3656 ,595  ,6472 ,597  ,5448 ,598  ,3400 ,601  ,4936 ,602  ,
        2888 ,604  ,1864 ,611  ,6344 ,613  ,5320 ,614  ,3272 ,617  ,
        4808 ,618  ,2760 ,620  ,1736 ,625  ,4552 ,626  ,2504 ,628  ,
        1480 ,632  ,968  ,647  ,7208 ,651  ,6696 ,653  ,5672 ,654  ,
        3624 ,659  ,6440 ,661  ,5416 ,662  ,3368 ,665  ,4904 ,666  ,
        2856 ,668  ,1832 ,675  ,6312 ,677  ,5288 ,678  ,3240 ,681  ,
        4776 ,682  ,2728 ,684  ,1704 ,689  ,4520 ,690  ,2472 ,692  ,
        1448 ,696  ,936  ,707  ,6248 ,709  ,5224 ,710  ,3176 ,713  ,
        4712 ,714  ,2664 ,716  ,1640 ,721  ,4456 ,722  ,2408 ,724  ,
        1384 ,728  ,872  ,737  ,4328 ,738  ,2280 ,740  ,1256 ,775  ,
        7192 ,779  ,6680 ,781  ,5656 ,782  ,3608 ,787  ,6424 ,789  ,
        5400 ,790  ,3352 ,793  ,4888 ,794  ,2840 ,796  ,1816 ,803  ,
        6296 ,805  ,5272 ,806  ,3224 ,809  ,4760 ,810  ,2712 ,812  ,
        1688 ,817  ,4504 ,818  ,2456 ,820  ,1432 ,824  ,920  ,835  ,
        6232 ,837  ,5208 ,838  ,3160 ,841  ,4696 ,842  ,2648 ,844  ,
        1624 ,849  ,4440 ,850  ,2392 ,852  ,1368 ,865  ,4312 ,866  ,
        2264 ,868  ,1240 ,899  ,6200 ,901  ,5176 ,902  ,3128 ,905  ,
        4664 ,906  ,2616 ,908  ,1592 ,913  ,4408 ,914  ,2360 ,916  ,
        1336 ,929  ,4280 ,930  ,2232 ,932  ,1208 ,961  ,4216 ,962  ,
        2168 ,964  ,1144 ,1039 ,7684 ,1047 ,7428 ,1051 ,6916 ,1053 ,
        5892 ,1054 ,3844 ,1063 ,7300 ,1067 ,6788 ,1069 ,5764 ,1070 ,
        3716 ,1075 ,6532 ,1077 ,5508 ,1078 ,3460 ,1081 ,4996 ,1082 ,
        2948 ,1084 ,1924 ,1095 ,7236 ,1099 ,6724 ,1101 ,5700 ,1102 ,
        3652 ,1107 ,6468 ,1109 ,5444 ,1110 ,3396 ,1113 ,4932 ,1114 ,
        2884 ,1116 ,1860 ,1123 ,6340 ,1125 ,5316 ,1126 ,3268 ,1129 ,
        4804 ,1130 ,2756 ,1132 ,1732 ,1137 ,4548 ,1138 ,2500 ,1140 ,
        1476 ,1159 ,7204 ,1163 ,6692 ,1165 ,5668 ,1166 ,3620 ,1171 ,
        6436 ,1173 ,5412 ,1174 ,3364 ,1177 ,4900 ,1178 ,2852 ,1180 ,
        1828 ,1187 ,6308 ,1189 ,5284 ,1190 ,3236 ,1193 ,4772 ,1194 ,
        2724 ,1196 ,1700 ,1201 ,4516 ,1202 ,2468 ,1204 ,1444 ,1219 ,
        6244 ,1221 ,5220 ,1222 ,3172 ,1225 ,4708 ,1226 ,2660 ,1228 ,
        1636 ,1233 ,4452 ,1234 ,2404 ,1236 ,1380 ,1249 ,4324 ,1250 ,
        2276 ,1287 ,7188 ,1291 ,6676 ,1293 ,5652 ,1294 ,3604 ,1299 ,
        6420 ,1301 ,5396 ,1302 ,3348 ,1305 ,4884 ,1306 ,2836 ,1308 ,
        1812 ,1315 ,6292 ,1317 ,5268 ,1318 ,3220 ,1321 ,4756 ,1322 ,
        2708 ,1324 ,1684 ,1329 ,4500 ,1330 ,2452 ,1332 ,1428 ,1347 ,
        6228 ,1349 ,5204 ,1350 ,3156 ,1353 ,4692 ,1354 ,2644 ,1356 ,
        1620 ,1361 ,4436 ,1362 ,2388 ,1377 ,4308 ,1378 ,2260 ,1411 ,
        6196 ,1413 ,5172 ,1414 ,3124 ,1417 ,4660 ,1418 ,2612 ,1420 ,
        1588 ,1425 ,4404 ,1426 ,2356 ,1441 ,4276 ,1442 ,2228 ,1473 ,
        4212 ,1474 ,2164 ,1543 ,7180 ,1547 ,6668 ,1549 ,5644 ,1550 ,
        3596 ,1555 ,6412 ,1557 ,5388 ,1558 ,3340 ,1561 ,4876 ,1562 ,
        2828 ,1564 ,1804 ,1571 ,6284 ,1573 ,5260 ,1574 ,3212 ,1577 ,
        4748 ,1578 ,2700 ,1580 ,1676 ,1585 ,4492 ,1586 ,2444 ,1603 ,
        6220 ,1605 ,5196 ,1606 ,3148 ,1609 ,4684 ,1610 ,2636 ,1617 ,
        4428 ,1618 ,2380 ,1633 ,4300 ,1634 ,2252 ,1667 ,6188 ,1669 ,
        5164 ,1670 ,3116 ,1673 ,4652 ,1674 ,2604 ,1681 ,4396 ,1682 ,
        2348 ,1697 ,4268 ,1698 ,2220 ,1729 ,4204 ,1730 ,2156 ,1795 ,
        6172 ,1797 ,5148 ,1798 ,3100 ,1801 ,4636 ,1802 ,2588 ,1809 ,
        4380 ,1810 ,2332 ,1825 ,4252 ,1826 ,2204 ,1857 ,4188 ,1858 ,
        2140 ,1921 ,4156 ,1922 ,2108 ,2063 ,7682 ,2071 ,7426 ,2075 ,
        6914 ,2077 ,5890 ,2078 ,3842 ,2087 ,7298 ,2091 ,6786 ,2093 ,
        5762 ,2094 ,3714 ,2099 ,6530 ,2101 ,5506 ,2102 ,3458 ,2105 ,
        4994 ,2106 ,2946 ,2119 ,7234 ,2123 ,6722 ,2125 ,5698 ,2126 ,
        3650 ,2131 ,6466 ,2133 ,5442 ,2134 ,3394 ,2137 ,4930 ,2138 ,
        2882 ,2147 ,6338 ,2149 ,5314 ,2150 ,3266 ,2153 ,4802 ,2154 ,
        2754 ,2161 ,4546 ,2162 ,2498 ,2183 ,7202 ,2187 ,6690 ,2189 ,
        5666 ,2190 ,3618 ,2195 ,6434 ,2197 ,5410 ,2198 ,3362 ,2201 ,
        4898 ,2202 ,2850 ,2211 ,6306 ,2213 ,5282 ,2214 ,3234 ,2217 ,
        4770 ,2218 ,2722 ,2225 ,4514 ,2226 ,2466 ,2243 ,6242 ,2245 ,
        5218 ,2246 ,3170 ,2249 ,4706 ,2250 ,2658 ,2257 ,4450 ,2258 ,
        2402 ,2273 ,4322 ,2311 ,7186 ,2315 ,6674 ,2317 ,5650 ,2318 ,
        3602 ,2323 ,6418 ,2325 ,5394 ,2326 ,3346 ,2329 ,4882 ,2330 ,
        2834 ,2339 ,6290 ,2341 ,5266 ,2342 ,3218 ,2345 ,4754 ,2346 ,
        2706 ,2353 ,4498 ,2354 ,2450 ,2371 ,6226 ,2373 ,5202 ,2374 ,
        3154 ,2377 ,4690 ,2378 ,2642 ,2385 ,4434 ,2401 ,4306 ,2435 ,
        6194 ,2437 ,5170 ,2438 ,3122 ,2441 ,4658 ,2442 ,2610 ,2449 ,
        4402 ,2465 ,4274 ,2497 ,4210 ,2567 ,7178 ,2571 ,6666 ,2573 ,
        5642 ,2574 ,3594 ,2579 ,6410 ,2581 ,5386 ,2582 ,3338 ,2585 ,
        4874 ,2586 ,2826 ,2595 ,6282 ,2597 ,5258 ,2598 ,3210 ,2601 ,
        4746 ,2602 ,2698 ,2609 ,4490 ,2627 ,6218 ,2629 ,5194 ,2630 ,
        3146 ,2633 ,4682 ,2641 ,4426 ,2657 ,4298 ,2691 ,6186 ,2693 ,
        5162 ,2694 ,3114 ,2697 ,4650 ,2705 ,4394 ,2721 ,4266 ,2753 ,
        4202 ,2819 ,6170 ,2821 ,5146 ,2822 ,3098 ,2825 ,4634 ,2833 ,
        4378 ,2849 ,4250 ,2881 ,4186 ,2945 ,4154 ,3079 ,7174 ,3083 ,
        6662 ,3085 ,5638 ,3086 ,3590 ,3091 ,6406 ,3093 ,5382 ,3094 ,
        3334 ,3097 ,4870 ,3107 ,6278 ,3109 ,5254 ,3110 ,3206 ,3113 ,
        4742 ,3121 ,4486 ,3139 ,6214 ,3141 ,5190 ,3145 ,4678 ,3153 ,
        4422 ,3169 ,4294 ,3203 ,6182 ,3205 ,5158 ,3209 ,4646 ,3217 ,
        4390 ,3233 ,4262 ,3265 ,4198 ,3331 ,6166 ,3333 ,5142 ,3337 ,
        4630 ,3345 ,4374 ,3361 ,4246 ,3393 ,4182 ,3457 ,4150 ,3587 ,
        6158 ,3589 ,5134 ,3593 ,4622 ,3601 ,4366 ,3617 ,4238 ,3649 ,
        4174 ,3713 ,4142 ,3841 ,4126 ,4111 ,7681 ,4119 ,7425 ,4123 ,
        6913 ,4125 ,5889 ,4135 ,7297 ,4139 ,6785 ,4141 ,5761 ,4147 ,
        6529 ,4149 ,5505 ,4153 ,4993 ,4167 ,7233 ,4171 ,6721 ,4173 ,
        5697 ,4179 ,6465 ,4181 ,5441 ,4185 ,4929 ,4195 ,6337 ,4197 ,
        5313 ,4201 ,4801 ,4209 ,4545 ,4231 ,7201 ,4235 ,6689 ,4237 ,
        5665 ,4243 ,6433 ,4245 ,5409 ,4249 ,4897 ,4259 ,6305 ,4261 ,
        5281 ,4265 ,4769 ,4273 ,4513 ,4291 ,6241 ,4293 ,5217 ,4297 ,
        4705 ,4305 ,4449 ,4359 ,7185 ,4363 ,6673 ,4365 ,5649 ,4371 ,
        6417 ,4373 ,5393 ,4377 ,4881 ,4387 ,6289 ,4389 ,5265 ,4393 ,
        4753 ,4401 ,4497 ,4419 ,6225 ,4421 ,5201 ,4425 ,4689 ,4483 ,
        6193 ,4485 ,5169 ,4489 ,4657 ,4615 ,7177 ,4619 ,6665 ,4621 ,
        5641 ,4627 ,6409 ,4629 ,5385 ,4633 ,4873 ,4643 ,6281 ,4645 ,
        5257 ,4649 ,4745 ,4675 ,6217 ,4677 ,5193 ,4739 ,6185 ,4741 ,
        5161 ,4867 ,6169 ,4869 ,5145 ,5127 ,7173 ,5131 ,6661 ,5133 ,
        5637 ,5139 ,6405 ,5141 ,5381 ,5155 ,6277 ,5157 ,5253 ,5187 ,
        6213 ,5251 ,6181 ,5379 ,6165 ,5635 ,6157 ,6151 ,7171 ,6155 ,
        6659 ,6163 ,6403 ,6179 ,6275 ,6211 ,5189 ,4681 ,4433 ,4321 ,
        3142 ,2634 ,2386 ,2274 ,1612 ,1364 ,1252 ,856  ,744  ,496 );

      IntelligentMailTbl2 : Array [0..77] of word =(
        3    ,6144 ,5    ,5120 ,6    ,3072 ,9    ,4608 ,10   ,2560 ,
        12   ,1536 ,17   ,4352 ,18   ,2304 ,20   ,1280 ,24   ,768  ,
        33   ,4224 ,34   ,2176 ,36   ,1152 ,40   ,640  ,48   ,384  ,
        65   ,4160 ,66   ,2112 ,68   ,1088 ,72   ,576  ,80   ,320  ,
        96   ,192  ,129  ,4128 ,130  ,2080 ,132  ,1056 ,136  ,544  ,
        144  ,288  ,257  ,4112 ,258  ,2064 ,260  ,1040 ,264  ,528  ,
        513  ,4104 ,514  ,2056 ,516  ,1032 ,1025 ,4100 ,1026 ,2052 ,
        2049 ,4098 ,4097 ,2050 ,1028 ,520  ,272  ,160  );


function  psFourToDec(s:string):Integer;
begin
      Result:=16*Ord(s[1])+4*Ord(s[2])+Ord(s[3])-21*Ord0;
end;

function  psDecToFour(x:integer):string;
var tmp1,tmp2,tmp3:integer;
begin
  tmp1:= x div 16;
  tmp2:=(x-16*tmp1) div 4;
  tmp3:= x-16*tmp1-4*tmp2;
  result:=Char(tmp1+Ord0)+Char(tmp2+Ord0)+Char(tmp3+Ord0);
end;


// ************************************************************************
// *** HugeInt functions
// ************************************************************************

function LongWordToHuge(const x: Longword): Hugeint; overload;
asm
    xor ecx, ecx
    mov [edx+_0_], eax
    mov [edx+_1_], ecx
    mov [edx+_2_], ecx
    mov [edx+_3_], ecx
end;

function Int64ToHuge(const x: Int64): Hugeint; overload;
asm
    mov edx, dword[x+_0_]
    mov ecx, dword[x+_1_]
    mov [eax+_0_], edx
    mov [eax+_1_], ecx
    sar ecx, 31
    mov [eax+_2_], ecx
    mov [eax+_3_], ecx
end;

function LongIntToHuge(const x: Integer): Hugeint; overload;
  asm
    or  eax, eax
    mov ecx, 0
    jns @@not_negative
    dec ecx
  @@not_negative:
    mov [edx+_0_], eax
    mov [edx+_1_], ecx
    mov [edx+_2_], ecx
    mov [edx+_3_], ecx
end;

function HugeAdd(x: Hugeint; y: Hugeint): Hugeint;
  asm
    push esi
    mov esi, [eax+_0_]
    add esi, [edx+_0_]
    mov [ecx+_0_], esi
    mov esi, [eax+_1_]
    adc esi, [edx+_1_]
    mov [ecx+_1_], esi
    mov esi, [eax+_2_]
    adc esi, [edx+_2_]
    mov [ecx+_2_], esi
    mov esi, [eax+_3_]
    adc esi, [edx+_3_]
    mov [ecx+_3_], esi
    pop esi
end;


function IntelligentMailCheck(ToEncode:string; BC:TObject):Boolean;
var tmp:string;
begin
  tmp:=OnlyDigits(ToEncode);
  Result:=Length(tmp) in [20,25,29,31];
  if Result then begin
    Result:=Ord(tmp[2]) in [Ord('0')..Ord('4')];
    if not Result then begin

      //TCustomEAN(BC).BarcodeRaise(erMustBe, 2, '"0".."4"');
    end;
  end;
end;

function MultiplyWithCarry(x: Longword; y: Longword; var Carry: Longword): LongWord;
asm
    mul edx
    add eax, [ecx]
    adc edx, 0
    mov [ecx], edx;
end;


function HugeMulInt(x: Hugeint; y: Longword): Hugeint;
var Carry: Longword;
begin
    Carry := 0;
    Result[0] := MultiplyWithCarry(x[0], y, Carry);
    Result[1] := MultiplyWithCarry(x[1], y, Carry);
    Result[2] := MultiplyWithCarry(x[2], y, Carry);
    Result[3] := MultiplyWithCarry(x[3], y, Carry);
end;

function HugeGetBit(h:HugeInt; bit:Integer):Integer;
var x,y:LongWord;
begin
  x      := h[bit div 32];
  y      := bit mod 32;
  result := (x shr y) and 1;
end;

function DivideWithRemainder(x: Longword; y: Longword; var Remainder: Longword): Longword;
asm
    push esi
    mov esi, edx
    mov edx, [ecx]
    div esi
    mov [ecx], edx;
    pop esi
end;

function HugeDivInt(x: Hugeint; y: Longword): Hugeint;
var Remainder: Longword;
begin
    Remainder := 0;
    Result[3] := DivideWithRemainder(x[3], y, Remainder);
    Result[2] := DivideWithRemainder(x[2], y, Remainder);
    Result[1] := DivideWithRemainder(x[1], y, Remainder);
    Result[0] := DivideWithRemainder(x[0], y, Remainder);
    asm
      mov edx, Remainder
    end;
end;


function HugeModInt(dividend: Hugeint; divisor: Longint): Longint;
asm
    sub esp, TYPE(Hugeint)
    mov ecx, esp
    call HugeDivInt
    add esp, TYPE(Hugeint)
    mov eax, edx
end;

function IntelligentMailGetLines(ToEncode:String):String;
var _tmp, _tmp1 : HugeInt;
    i,crc,data  : Integer;
    s,TrackingCode,RoutingCode : String;
    c : char;
    cw:array ['A'..'J'] of word;
    procedure GetOneBar(c1:Char; Bit1:Integer;c2:Char; Bit2:Integer);
    var a,d:Boolean;
        idx:Integer;
    begin
      d:=((cw[c1] shr Bit1) and 1)=1;
      a:=((cw[c2] shr Bit2) and 1)=1;
      idx := 2*i-1;
      if a then
        if d then s[idx] := 'F'
        else      s[idx]  := 'A'
      else
        if d then s[idx]  := 'D'
        else      s[idx]  := 'T'
    end;
begin
  ToEncode:=OnlyDigits(ToEncode);
//  if not CheckIntelligentMail(ToEncode) then
//    Raise psBarcodeException.Create('Bad string to encode ...');

  TrackingCode := Copy(ToEncode,1,20);
  RoutingCode  := Copy(ToEncode,21,11);

  // step 1. Conversion of Data Fields into Binary Data - tested - OK
  case Length(RoutingCode) of
     0 : _tmp:=Int64ToHuge(0);
     5 : _tmp:=Int64ToHuge(StrToInt64(RoutingCode)+ 1);
     9 : _tmp:=Int64ToHuge(StrToInt64(RoutingCode)+ 100000 + 1);
    11 : _tmp:=Int64ToHuge(StrToInt64(RoutingCode)+ 1000000000 + 100000 + 1);
  end;


  _tmp1:=LongIntToHuge(Ord(TrackingCode[1])-Ord0);
  _tmp:=HugeAdd(HugeMulInt(_tmp,10),_tmp1);

  _tmp1:=LongIntToHuge(Ord(TrackingCode[2])-Ord0);
  _tmp:=HugeAdd(HugeMulInt(_tmp,5),_tmp1);


  for i:=3 to 20 do begin
    _tmp1:=LongIntToHuge(Ord(TrackingCode[i])-Ord0);
    _tmp:=HugeAdd(HugeMulInt(_tmp,10),_tmp1);
  end;

  // Step 2 Generation of 11-Bit CRC on Binary Data - verified - OK
  crc:=GenFrame;
  for i:=101 downto 0 do begin
    data :=((HugeGetBit(_tmp,i) shl 10) xor crc) and $400;
    if data<>0 then
      crc := (crc shl 1) xor GenPoly
    else crc := crc shl 1;
    crc := crc and GenFrame;
  end;

  // Step 3 Conversion from Binary Data to Codewords - verified - OK
  cw['J']:= HugeModInt(_tmp,636);
  _tmp  := HugeDivInt(_tmp,636);
  for c:='I' downto 'B' do begin
    cw[c]:= HugeModInt(_tmp,1365);
    _tmp := HugeDivInt(_tmp,1365);
  end;
  cw['A']:=_tmp[0];

  // Step 4 Inserting Additional Information into Codewords - OK
  cw['J']:=cw['J'] shl 1;
  if (crc and $400)<>0 then
      cw['A']:=cw['A'] + 659;

  // Step 5 Conversion from Codewords to Characters - OK
  for c:='A' to 'J'  do begin
    if cw[c]>=1287 then cw[c]:=IntelligentMailTbl2[cw[c]-1287]
    else                cw[c]:=IntelligentMailTbl1[cw[c]];
    if (crc mod 2) = 1 then
      cw[c] := (not cw[c]) and $1fff;
    crc := crc div 2;
  end;

  // Step 6 Conversion from Characters to the Intelligent Mail Barcode
  s:=StringOfChar('0',130);
  for i:=1 to 65 do
    case i of
      1 : GetOneBar('H',  2, 'E',  3);
      2 : GetOneBar('B', 10, 'A',  0);
      3 : GetOneBar('J', 12, 'C',  8);
      4 : GetOneBar('F',  5, 'G', 11);
      5 : GetOneBar('I',  9, 'D',  1);
      6 : GetOneBar('A',  1, 'F', 12);
      7 : GetOneBar('C',  5, 'B',  8);
      8 : GetOneBar('E',  4, 'J', 11);
      9 : GetOneBar('G',  3, 'I', 10);
     10 : GetOneBar('D',  9, 'H',  6);
     11 : GetOneBar('F', 11, 'B',  4);
     12 : GetOneBar('I',  5, 'C', 12);
     13 : GetOneBar('J', 10, 'A',  2);
     14 : GetOneBar('H',  1, 'G',  7);
     15 : GetOneBar('D',  6, 'E',  9);
     16 : GetOneBar('A',  3, 'I',  6);
     17 : GetOneBar('G',  4, 'C',  7);
     18 : GetOneBar('B',  1, 'J',  9);
     19 : GetOneBar('H', 10, 'F',  2);
     20 : GetOneBar('E',  0, 'D',  8);
     21 : GetOneBar('G',  2, 'A',  4);
     22 : GetOneBar('I', 11, 'B',  0);
     23 : GetOneBar('J',  8, 'D', 12);
     24 : GetOneBar('C',  6, 'H',  7);
     25 : GetOneBar('F',  1, 'E', 10);
     26 : GetOneBar('B', 12, 'G',  9);
     27 : GetOneBar('H',  3, 'I',  0);
     28 : GetOneBar('F',  8, 'J',  7);
     29 : GetOneBar('E',  6, 'C', 10);
     30 : GetOneBar('D',  4, 'A',  5);
     31 : GetOneBar('I',  4, 'F',  7);
     32 : GetOneBar('H', 11, 'B',  9);
     33 : GetOneBar('G',  0, 'J',  6);
     34 : GetOneBar('A',  6, 'E',  8);
     35 : GetOneBar('C',  1, 'D',  2);
     36 : GetOneBar('F',  9, 'I', 12);
     37 : GetOneBar('E', 11, 'G',  1);
     38 : GetOneBar('J',  5, 'H',  4);
     39 : GetOneBar('D',  3, 'B',  2);
     40 : GetOneBar('A',  7, 'C',  0);
     41 : GetOneBar('B',  3, 'E',  1);
     42 : GetOneBar('G', 10, 'D',  5);
     43 : GetOneBar('I',  7, 'J',  4);
     44 : GetOneBar('C', 11, 'F',  6);
     45 : GetOneBar('A',  8, 'H', 12);
     46 : GetOneBar('E',  2, 'I',  1);
     47 : GetOneBar('F', 10, 'D',  0);
     48 : GetOneBar('J',  3, 'A',  9);
     49 : GetOneBar('G',  5, 'C',  4);
     50 : GetOneBar('H',  8, 'B',  7);
     51 : GetOneBar('F',  0, 'E',  5);
     52 : GetOneBar('C',  3, 'A', 10);
     53 : GetOneBar('G', 12, 'J',  2);
     54 : GetOneBar('D', 11, 'B',  6);
     55 : GetOneBar('I',  8, 'H',  9);
     56 : GetOneBar('F',  4, 'A', 11);
     57 : GetOneBar('B',  5, 'C',  2);
     58 : GetOneBar('J',  1, 'E', 12);
     59 : GetOneBar('I',  3, 'G',  6);
     60 : GetOneBar('H',  0, 'D',  7);
     61 : GetOneBar('E',  7, 'H',  5);
     62 : GetOneBar('A', 12, 'B', 11);
     63 : GetOneBar('C',  9, 'J',  0);
     64 : GetOneBar('G',  8, 'F',  3);
     65 : GetOneBar('D', 10, 'I',  2);
    end;

    Result:=Copy(s,1,130);
end;

// **************************************************************************
// *** Telepn barcode symbology implementation
// **************************************************************************

function Telepen(ck:String):string;
var s,s1,s2  : string;
    i,j,m,state,ones,bit: integer;
    k        : byte;
begin
    {checksum calculation}
    j:=0;
    for i:=1 to Length(ck) do
        Inc(j,Ord(ck[i]) and 127);

    j:=not (j mod 127);
    ck:=ck+Char(j);

    {add s-tart and stop chars}
    ck:='_'+ck+'z';

    {encode ck to LSB with even parity }
    s:='';
    for i:=1 to length(ck) do begin
        k    := Ord(ck[i]);
        ones := 0;
        for j:=0 to 6 do begin
            bit := (k shr j) and 1;
            Inc(ones, bit);
            if bit=1 then s:=s+'1'
            else          s:=s+'0';
        end;
        if ones=1 then s:=s+'1'
        else           s:=s+'0';
    end;

  s1   :='';
  state:=0;
  m    :=1;
  while m<Length(s) do begin
    s2   := Copy(s,m,8);
    Inc(m,8);
    i    :=1;
    while (i<=Length(s2)) do begin
        if (Copy(s2,i,3)='010') and (state=0) then begin
                s1:=s1+'111000';
                Inc(i,3);
        end
        else if (Copy(s2,i,2)='00') and (state=0) then begin
                s1:=s1+'1110';
                Inc(i,2);
        end
        else if (Copy(s2,i,2)='01') and (state=0) then begin
                s1:=s1+'1000';
                Inc(i,2);
                State :=1;
        end
        else if (Copy(s2,i,2)='10') and (state=1) then begin
                s1:=s1+'1000';
                Inc(i,2);
                State:=0;
        end
        else begin
                s1:=s1+'10';
                Inc(i,1);
        end;
    end;
  end;
  Result := s1;
end;

function TelepenNumeric(ck:String):string;
var s:String;
    i,znak:Integer;
begin
    if (Length(ck) mod 2) = 1 then ck:='0'+ck;

    i:=1;
    s:='';
    while i<Length(ck) do begin
      znak:=27 + 10*(Ord(ck[i])-Ord0) + Ord(ck[i+1])-Ord0;
      Inc(i,2);

{      obrat := 0;
      for j:=0 to 7 do begin
        obrat := obrat shl 1;
        if ((znak shr j) and 1)=1 then Inc(obrat);
      end;
}      // ShowMessage(intToStr(obrat));
      s:=s+Char(Znak);
    end;
    Result := Telepen(s);
end;

// ***************************************************************
// ***** Australia Post barcode
// ***************************************************************

const
  BarTable : array[0..63] of String= (
    '000', '001', '002', '003', '010', '011', '012', '013', '020', '021', '022',
    '023', '030', '031', '032', '033', '100', '101', '102', '103', '110', '111',
    '112', '113', '120', '121', '122', '123', '130', '131', '132', '133', '200',
    '201', '202', '203', '210', '211', '212', '213', '220', '221', '222', '223',
    '230', '231', '232', '233', '300', '301', '302', '303', '310', '311', '312',
    '313', '320', '321', '322', '323', '330', '331', '332', '333');

  NTable : array['0'..'9'] of string =
      ('00', '01', '02', '10', '11', '12', '20', '21', '22', '30');

  CNTable: Array['0'..'9'] of String =
     ('222', '300', '301', '302', '310', '311', '312', '320','321', '322');

  // uppercase A-Z
  ZTable : array['A'..'Z'] of string = (
    '000', '001', '002', '010', '011', '012', '020', '021', '022', '100', '101',
    '102', '110', '111', '112', '120', '121', '122', '200', '201', '202', '210',
    '211', '212', '220', '221');

  // lowercase a-z, space and #
  CTable : array['a'..'z'] of string = (
   '023', '030', '031', '032', '033', '103', '113', '123', '130', '131', '132',
   '133', '203', '213', '223', '230', '231', '232', '233', '303', '313', '323',
   '330', '331', '332', '333');
  SpaceTable = '003';
  Mriezka    = '013';

  bSet = '0123';
  nSet = constDigits;
  zSet = constUpperAZ;
  cSet = constLowerAZ+' #';


// *******************************************************
// ReedSolomon rountines
// *******************************************************
var mult : array[0..MAX_TABLE-1,0..MAX_TABLE-1] of longword;
    gen  : array[0..MAX_GEN-1] of integer;

procedure InitializeReedSolomon(TblSize:Integer);
var primpoly, test, prev, next : longword;
    i,j:integer;
begin
        primpoly := 67;
        test     := 64;
        for i := 0 to 63 do begin
          mult[0][i] := 0;
          mult[1][i] := i;
        end;

        prev := 1;
        for i := 1 to 63 do begin
          next := prev shl 1;
          if (next and test)<>0 then  next := next xor primpoly;
          for j := 0 to 63 do begin
             mult[next][j] := mult[prev][j] shl 1;
             if (mult[next][j] and test)<>0 then
              mult[next][j] := mult[next][j] xor primpoly;
          end;
          prev := next;
        end;

        gen[0] := 48;
        gen[1] := 17;
        gen[2] := 29;
        gen[3] := 30;
        gen[4] := 1;
end;

procedure InitializeGen(g:Array of Integer);
var i:Integer;
begin
  if Length(g)>MAX_GEN then
    raise psBarcodeException.Create(rsReedSolomonOverFlow);

  for i:=0 to Length(g)-1 do
    gen[i]:=g[i];
end;

function ReedSolomonEncode(count:Integer; symbols:array of longword):longword4;
var i,j,k,n:Integer;
    tmp:array[0..30] of longword;
begin
    if gen[0]=0 then InitializeReedSolomon(MAX_TABLE);

	  k := Count;
    n := k+4;
    for i := 0 to 3 do tmp[i] := 0;
    for i:=4 to n-1 do tmp[i] := symbols[i - 4];

    for i:=k-1 downto 0 do
      for j:=0 to 4 do
        tmp[i + j]:=tmp[i + j] xor (mult[gen[j]][tmp[4 + i]]);

    for i:=0 to 3 do result[i] := tmp[i];
end;

procedure psStringReplace(var Source:String; iPar:String; iFrom:Integer);
var i,j:Integer;
begin
    j:=Length(Source);
    for i:=0 to Length(iPar)-1 do
        if iFrom+i<=j then
           Source[iFrom+i] := iPar[i+1];
end;

function ParityAdd(var sBarcode:String; sLen:Integer; var paritySymbols:longword4):Integer;
var iSymbols,iNumInfoSymbols,i,j:Integer;
    // sBarGroup : String;
    iCodeWord {,iTempCodeWord} :array[0..31] of LongWord;
begin
        iSymbols        := (sLen - 4) div 3;
        iNumInfoSymbols := iSymbols - 4;

        //scan if all 3character group are in BarTable
        for i:=0 to iNumInfoSymbols-1 do
            iCodeWord[iNumInfoSymbols-1-i] := psFourToDec(Copy(sBarcode,3+i*3,3));

        // all is OK
        for i:=iNumInfoSymbols to iSymbols-1 do
            iCodeWord[i] := 0;

        // Call RSEncode to get the parity symbols
        paritysymbols := ReedSolomonEncode(iNumInfoSymbols, iCodeWord);

        // Add the RS Parity Symbols to the end of the code word
        // Convert the symbol order from 1,2,3.. to ..3,2,1

        //for i:=0 to 3 do
        //        iCodeWord[i+iNumInfoSymbols] := paritysymbols[3-i];
        // Now we need to grab the 4 parity symbols returned
        // append them to our bar code string
        j:=3*(iNumInfoSymbols+1);
        for i:=iNumInfoSymbols to iSymbols-1 do begin
          psStringReplace(sBarcode,psDecToFour(paritySymbols[3-i+iNumInfoSymbols]),j);
          Inc(j,3);
        end;
        result := 0;
end;

{
function ParityAdd(var sBarcode:String; sLen:Integer; var paritySymbols:longword4):Integer;
var iSymbols,iNumInfoSymbols,i,j:Integer;
    bcFound   : Boolean;
    sBarGroup : String;
    iCodeWord,iTempCodeWord:array[0..31] of LongWord;
begin
        iSymbols        := (sLen - 4) div 3;
        iNumInfoSymbols := iSymbols - 4;

        //scan if all 3character group are in BarTable
        for i:=0 to iNumInfoSymbols-1 do begin
            sBarGroup    :=Copy(sBarcode,3+i*3,3);
            bcfound      := False;

            for j:=0 to MAX_TABLE-1 do begin
                if BarTable[j]=sBarGroup then begin
                  iCodeWord[i] := j;
                  bcfound      := true;
                  break;
                end;
            end;
            if not bcfound then begin
                    iCodeWord[i] := 0;
                    raise psBarcodeException.Create(rsAustraliaInvalidEncode);
            end;
        end;

        // all is OK
        for i:=iNumInfoSymbols to iSymbols-1 do
            iCodeWord[i] := 0;

        // Now we must convert the symbol order from 1,2,3.. to ..3,2,1
        j := iNumInfoSymbols - 1;
        for i:=0 to iNumInfoSymbols-1 do begin
            iTempCodeWord[i] := iCodeWord[j];
            Dec(j);
        end;

        // Call RSEncode to get the parity symbols
        paritysymbols := ReedSolomonEncode(iNumInfoSymbols, iTempCodeWord);

        // Add the RS Parity Symbols to the end of the code word
        // Convert the symbol order from 1,2,3.. to ..3,2,1

        for i:=0 to 3 do
                iCodeWord[i+iNumInfoSymbols] := paritysymbols[3-i];
        // Now we need to grab the 4 parity symbols returned
        // append them to our bar code string
        j:=3*(iNumInfoSymbols+1);
        for i:=iNumInfoSymbols to iSymbols-1 do begin
          // sBarcode:=sBarcode+BarTable[iCodeWord[i]];
          psStringReplace(sBarcode,BarTable[iCodeWord[i]],j);
          Inc(j,3);
        end;
        result := 0;
end;

}



function ConvertN (var sBarcode:String; iStart,iEnd:Integer; inString:string):Integer;
var  i,j:Integer;
     c:String;
begin
  j:=iStart;
  Result := 0;
  for i:=1 to Length(inString) do begin
    if IsDigit(inString[i]) then
        c:=NTable[inString[i]]
    else begin
        c:='  ';
        Result := -1;
    end;
    psStringReplace(sBarcode,c,j);
    Inc(j,Length(c));
  end;
end;

function ConvertZ(var  sBarcode:string; iStart,iEnd:Integer;
        inString:String):Integer;
var i,j    :Integer;
     c     :string;
begin
  j:=iStart;
  Result := 0;
  for i:=1 to Length(inString) do begin
    if Pos(inString[i], zSet)>0 then
        c:=ZTable[inString[i]]
    else begin
        c:='   ';
        Result := -1;
    end;
    psStringReplace(sBarcode,c,j);
    Inc(j,Length(c));
  end;
end;


function ConvertC(var  sBarcode:string; iStart,iEnd:Integer;
        inString:String):Integer;
var i,ipos,bcStat:Integer;
     barval:string;
     c:Char;
begin
  bcStat := 0;
  ipos   := istart;

  for i:=1 to Length(inString) do begin
      c := InString[i];
      case c of
        'A'..'Z' : barval := ZTable[c];
        '0'..'9' : barval := CNTable[c];
        'a'..'z' : barval := CTable[c];
        ' '      : barval := SpaceTable;
        '#'      : barval := Mriezka;
      else
        BarVal := '   ';
        bcStat := -1;
      end;
      psStringReplace(sBarcode,barval,iPos);
      Inc(iPos,Length(barval));
  end;

  Result := bcstat;
end;

function BuildBarcode37(var sBarcode:string; sFCC,sSortingCode:String):Integer;
const iLen=37;
var bcstat:Integer;
begin
  bcStat:=ConvertN (sBarcode, 03, 06, sFCC);
  if bcStat<>0 then
        raise psBarcodeException.Create(rsAustraliaInvalidFCC);

  bcstat := ConvertN (sBarcode, 07, 22, sSortingCode);
  if bcStat<>0 then
        raise psBarcodeException.Create(rsAustraliaInvalidSortCode);
  Result := bcstat;
end;

procedure BuildBarcode52(var sBarcode:String;
      sFCC, sSortingCode,sCustInfo:String);
const iLen=52;
begin
    if ConvertN (sBarcode, 03, 06, sFCC)<>0 then
      raise psBarcodeException.Create(rsAustraliaInvalidFCC);

    if ConvertN (sBarcode, 07, 22, sSortingCode) <>0 then
      raise psBarcodeException.Create(rsAustraliaInvalidSortCode);

    if ConvertC (sBarcode, 23, 38, Copy(sCustInfo,1,5))<>0 then
      raise psBarcodeException.Create(rsAustraliaInvalidCustomerCode);
end;

procedure BuildBarcode67(var sBarcode:String;
      sFCC, sSortingCode,sCustInfo:String);
const iLen=67;
begin
  if ConvertN (sBarcode, 03, 06, sFCC)<>0 then
      raise psBarcodeException.Create(rsAustraliaInvalidFCC);

  if ConvertN (sBarcode, 07, 22, sSortingCode)<>0 then
      raise psBarcodeException.Create(rsAustraliaInvalidSortCode);

  if IsNumeric(sCustInfo) then begin
      if ConvertN (sBarcode, 23, 53, Copy(sCustInfo,1,15))<>0 then
      raise psBarcodeException.Create(rsAustraliaInvalidCustomerInfo);
  end else
    if ConvertC (sBarcode,        23, 53, Copy(sCustInfo,1,10))<>0 then
      raise psBarcodeException.Create(rsAustraliaInvalidCustomerInfo);
end;

function  TranslateAndExpand(s:String):String;
const trans:array['0'..'3'] of char=('F','A','D','T');
var idx,i,iLen:Integer;
begin
    iLen  := Length(s);
    Result:= StringOfChar('0',2*iLen-1);
    idx:=1;
    for i:=1 to iLen do begin
        Result[idx] := trans[s[i]];
        Inc(idx,2);
    end;
end;


procedure BuildBarcode(sFCC,sSortingCode,sCustInfo:String;
                var sBarcode:String; var checksum:longword4);
var sTemp      : string;
    iLen       : Integer;
begin
    // initialize max size for barcode
    sTemp  := StringOfChar('3',67);
    psStringReplace(sTemp,BC_START_SYM,1);      // start char

    if (sFCC=FCC_37_CUST) or (sFCC=FCC_37_ROUT) or (sFCC=FCC_37_REPL)
      or (sFCC=FCC_37_REDI) then begin
 		      iLen := 37;
          BuildBarcode37(sTemp, sFcc, sSortingCode);
    end  else if sFCC=FCC_52_FF_MET then begin
  		    iLen := 52;
          BuildBarcode52(sTemp, sFcc, sSortingCode,sCustInfo);
    end else if (sFCC=FCC_67_FF_MET) or (sFCC=FCC_67_FF_MAN) then begin
		      iLen := 67;
          BuildBarcode67(sTemp, sFcc, sSortingCode, sCustInfo);
    end else
      raise psBarcodeException.Create(rsAustraliaInvalidFCC);

    ParityAdd(sTemp, iLen, checksum );

    psStringReplace(sTemp,BC_STOP_SYM,iLen-1);  // stop char
    sTemp:=Copy(sTemp,1,iLen);
    sBarcode:=TranslateAndExpand(sTemp);
end;


function AustraliaPostParse(ToEncode:String; var sFFC,sSort,sCust:string):Boolean;
begin
    sFFC := Copy(ToEncode,1,2);
    sSort:= Copy(ToEncode,3,8);
    sCust:= Copy(ToEncode,11,100);
    Result :=True;
end;


function AustraliaPostCheck(ToEncode:String; BC:TObject):Boolean;
var sFCC,sSort,sCust : string;
//    i                : Integer;
    version          : Integer;
//    EAN              : TCustomEAN;
begin
//  EAN := BC as TCustomEAN;

  AustraliaPostParse(ToEncode,sFCC,sSort,sCust);

  // version test
  Version:=0;
  if (sFCC=FCC_37_CUST) or (sFCC=FCC_37_ROUT)   or (sFCC=FCC_37_REPL)
         or (sFCC=FCC_37_REDI) then
              Version:=37
  else if sFCC=FCC_52_FF_MET then
              Version:=52
  else if (sFCC=FCC_67_FF_MET) or (sFCC=FCC_67_FF_MAN) then
              Version:=67;

  Result:=Version<>0;
  if not Result then
        raise psBarcodeException.Create(rsAustraliaInvalidFCC);

  // test Customer info
  Result := (Length(sSort)=8) and isNumeric(sSort);
  if not Result then
        raise psBarcodeException.Create(rsAustraliaInvalidSortCode);

  // length test
  case Version of
      37 : Result := Length(sCust)=0;
      52 : Result := (Length(sCust)<=5)  or ((Length(sCust)<=8)  and isNumeric(sCust));
      67 : Result := (Length(sCust)<=10) or ((Length(sCust)<=15) and isNumeric(sCust));
  end;

  if not Result then
        raise psBarcodeException.Create(rsAustraliaInvalidCustomerCode);
end;

function AustraliaPostGetLines(var ToEncode:String; ShowChecksum:Boolean):String;
var tmp,sFCC,sSort,sCust:string;
    ch: longword4;
begin
    InitializeGen([48,17,29,30,1]);
    InitializeReedSolomon(64);
    AustraliaPostParse(ToEncode,sFCC,sSort,sCust);
    BuildBarcode(sFCC,sSort,sCust,tmp, ch );

    if ShowChecksum then
      ToEncode:=Format('%2s %8s %s (%2d %2d %2d %2d)',
          [sFCC,sSort,sCust, ch[3], ch[2], ch[1], ch[0]])
    else
      ToEncode:=Format('%2s %8s %s', [sFCC,sSort,sCust]);

    Result:=tmp;
end;

// ***************************************************************************
// W=White, Y=Yellow, B=Blue, R=Red, G=Green, C=Black,
const ChromocodeTableAZ : Array ['A'..'Z'] of string= (

      // {!} asi chyba, X a Y maju rovnaky kod
      {A} 'WWWW', {B} 'WYWY', {C} 'YWYW', {D} 'WBWB', {E} 'BWBW', {F} 'WRWR',
      {G} 'RWRW', {H} 'WGWG', {I} 'GWGW', {J} 'WCWC', {K} 'CWCW', {L} 'YYYY',
      {M} 'YBYB', {N} 'BYBY', {O} 'YRYR', {P} 'RYRY', {Q} 'YGYG', {R} 'GYGY',
      {S} 'YCYC', {T} 'CYCY', {U} 'BBBB', {V} 'BRBR', {W} 'RBRB', {X} 'BGBG',
      {Y} 'GBGB', {Z}  'BCBC');

      ChromocodeTable09 : Array ['0'..'9'] of string =(
        {0} 'CCCC', {1} 'GCGC', {2} 'CGCG', {3} 'GGGG', {4} 'CRCR',
        {5} 'RCRC', {6} 'GRGR', {7} 'RGRG', {8} 'RRRR', {9} 'CBCB'
      );

function  EncodeChromocode(E:TComponent):Boolean;
var i         : Integer;
    s1,s2,tmp : String;
    barcode   : string;
    c         : Char;
    BC        : TpsBarcodeComponent;
begin
  s1:='';
  s2:='';
  BC      := E as TpsBarcodeComponent;
  Barcode := BC.Barcode;
  Result  := True;

  for i:=1 to Length(Barcode) do begin
      c:=Barcode[i];
      if IsUpper(c)       then tmp := ChromoCodeTableAZ[c]
      else  if IsDigit(c) then tmp := ChromoCodeTable09[c]
      else  raise psBarcodeException.Create(ErrUnavailableChar);
      s1:=s1+Copy(tmp,1,2);
      s2:=s2+Copy(tmp,3,2);
  end;

  {!!! dorobit }
{  BC.Bars.Clear;
  BC.Bars.Add('BR'+s1+'BR');
  BC.Bars.Add('GY'+s2+'GY');
}
end;

//--------------------------------------------------------------------------
//------- Postbar, CPC 4-state
//--------------------------------------------------------------------------

function  CPC4State(ck:string):string;
const pbTablAN_N:array['0'..'9'] of string=
        ('FF','FA','FD','AF','AA','AD','DF','DA','DD','TF');
      pbTablAN_A:array['A'..'Z'] of string=
        ('FFD','FAF','FAA','FAD','FDF','FDA','FDD','FFA',
         'AFA','AFD','AAF','AAA','FFF','ADF',
         'ADA','ADD','DFF','DFA','DFD','DAF',
         'DAA','DAD','DDF','DDA','DDD','AFF');
      pbTablZ_N:array['0'..'9'] of string=
        ('DDD','TFF','TFA','TFD','TAF','TAA','TAD','TDF','TDA','TDD');
      pbTablZ_A:array['A'..'Z'] of string=
        ('FFF','FFA','FFD','FAF','FAA','FAD','FDF','FDA',
         'FDD','AFF','AFA','AFD','AAF','AAA',
         'AAD','ADF','ADA','ADD','DFF','DFA',
         'DFD','DAF','DAA','DAD','DDF','DDA');
      pbStart = 'AT';
      pbStop  = 'AT';
      pbSpace = 'FFT';


var tmp,znak,mask:string;
    i:Integer;
    c:char;
    checksum:longword4;
begin
    Result := '';
    ck  := UpperCase(StringReplace(ck,' ','',[rfReplaceAll]));
    mask :='';
    case ck[1] of
      'A'..'L' : case Length(ck) of
                    7 : mask := 'ZANANAN';
                   11 : begin
                          ck   := ck+'#';
                          mask := 'ZANANANZZZZ#';
                        end;
                   12 : mask := 'ZANANANZZZZ#';
                   22 : mask := 'ZANANANZZZZZZZZZZZ';
                 end;
      '1'..'9' : case Length(ck) of
                   12 : mask := 'ZNNNZZZZZZZZ';
                   22 : mask := 'ZNNNZZZZZZZZZZZZZZZZZZ';
                 end;
      'M'..'U' : case Length(ck) of
                    6 : mask := 'ZZZZZZ';
                   11 : mask := 'ZZZZZZZZZZZ';
                   21 : mask := 'ZZZZZZZZZZZZZZZZZZZZZ';
                 end;
      'V'..'Z' : case Length(ck) of
                    10 : mask := 'ZANANAN3BBBB';
                 end;
    end;

    if mask='' then Exit;

    //tmp := pbStart;
    tmp:=pbStart;
    for i:=1 to Length(ck) do begin
      c:=ck[i];
      if c='#' then znak:=pbSpace
      else if IsDigit(c) then begin
                if mask[i]='Z' then znak:=pbTablZ_N[c]
                else                znak:=pbTablAN_N[c];
           end else begin
                if mask[i]='Z' then znak:=pbTablZ_A[c]
                else                znak:=pbTablAN_A[c];
           end;
      tmp:=tmp+znak;
    end;

    tmp:=tmp+StringOfChar(' ',12)+pbStop;
    tmp:=StringReplace(tmp,'F','0',[rfReplaceAll]);
    tmp:=StringReplace(tmp,'A','1',[rfReplaceAll]);
    tmp:=StringReplace(tmp,'D','2',[rfReplaceAll]);
    tmp:=StringReplace(tmp,'T','3',[rfReplaceAll]);

    // calculate Reed Solomon ECC

    InitializeGen([48,17,29,30,1]);
    InitializeReedSolomon(64);
    ParityAdd(Tmp, Length(tmp), checksum );

    Result:=TranslateAndExpand(Tmp);
    Result:=ExpandCode(tmp);
end;


function  CPC4StateCheck(var ck:string):Boolean;
begin
  Result:=True;
end;




// ------------------------------------------------------------------------
// Code 16K implementation
// ------------------------------------------------------------------------
procedure PaintCode16K(C:TCanvas; R:TRect; E:TpsBarcodeComponent);
const
  Start16KCodes:array[0..7] of string=(
    '111001', '110011', '110110', '100001', '101110', '100111', '101000', '111010');
  Start16KIdx:array[0..15] of Integer=(0,1,2,3,4,5,6,7,0,1,2,3,4,5,6,7);
  Stop16KIdx :array[0..15] of Integer=(0,1,2,3,4,5,6,7,4,5,6,7,0,1,2,3);

  RowPixelSize=11+8+55+7;
//var s,ck:String;
//    R1:TRect;
//    i,j,cols,rows,NumbersOfChar:Integer;
//    mode, c1, c2, dy   : integer;
//    StepY, StartSymbol : integer;
begin
//    s :=E.Zoom(StringOfChar('1',RowPixelSize), R.Right-R.Left,False);
//    dy:=(Length(s) div RowPixelSize) div 3;
//    if dy<1 then dy:=1;
//
//    ck           :=compress128(E.Barcode);
//    ck           :=copy(ck,2,Length(ck)-1);
//    cols         := 5;
//    rows         := ((Length(ck)+1{StartSymbol}+2{Checksum} -1) div 5)+1 ;
//
//    // minimum two rows for this symbology
//    if rows<2 then rows:=2;
//
//
//    mode         := 1;
//    NumbersOfChar:= Rows*Cols;
//    startSymbol  := 7*(rows-2)+mode;
//
//    {Calc checksum here}
//    c1:=0;
//    c2:=0;
//    for i:=1 to Length(ck) do begin
//        c1:=c1+(i+1)*Ord(ck[i]);
//        c2:=c2+    i*Ord(ck[i]);
//    end;
//    c1:=c1 mod 107;
//    c2:=c2 mod 107;
//
//    { PAD symbol count }
//    i :=NumbersOfChar-(Length(ck)-1{StartSymbol}-2{checksum});
//    ck:=Char(StartSymbol)+ck+StringOfChar(Code128_PAD ,i)+Char(c1)+Char(c2);
//
//    // ShowMessage(Format('%d %d',[c1,c2]));
//    stepY := (R.Bottom-R.Top) div rows;
//
//    with C do begin
//           Pen.Color := E.LinesColor;
//           Pen.Width := dy;
//           Pen.Mode  := pmCopy;
//           Pen.Style := psSolid;
//           Brush.Style := bsSolid;
//           Brush.Color := E.LinesColor;
//    end;
//
//    { high horizontal line}
//    C.Rectangle(R.Left,R.Top,R.Right,R.Top+2);
//    { bottom horizontal line}
//    C.Rectangle(R.Left,R.Bottom-2,R.Right,R.Bottom);
//
//    R1.Top    := R.Top   +dy;
//    R1.Bottom := R.Bottom-dy;
//
//    with C do
//        for i:=1 to rows do begin
//                s:='';
//                for j:=1 to cols do
//                        s:=s+Code128Table[Ord(ck[(i-1)*cols+j])]+'0';
//
//                {Change bars and spaces}
//                for j:=1 to length(s) do
//                        if s[j]='1' then s[j]:='0'
//                        else             s[j]:='1';
//
//
//                {one single line }
//                s:=StringOfChar('0',11)+Start16KCodes[i]+'01'
//                  +s+Stop16KCodes[i]+'10';
//                s:=E.Zoom(s, R.Right-R.Left, boSecurity in E.Options);
//                R1.Bottom:=R1.Top+StepY;
//                DrawLines(C,R1,s);
//
//                { middle horinontal line }
//                s:=StringOfChar('0',11)+StringOfChar('1',RowPixelSize-11)+'0';
//                R1.Top:=R1.Bottom-dy;
//                s:=E.Zoom(s, R.Right-R.Left, boSecurity in E.Options);
//                DrawLines(C,R1,s);
//
//                R1.Top   :=R1.Bottom+1;
//         end;
end;

procedure CodablockF(C:TCanvas; R:TRect; E:TpsBarcodeComponent);
begin
end;





procedure Test;
var bc:TpsBarcodeComponent;
    M:TpsArray;
    i:Integer;
begin
 bc := TpsBarcodeComponent.Create(nil);
 bc.BarcodeSymbology        := bcPDF417;
 bc.Params.SegmentIndex     := 0;
 bc.Params.SegmentCount     := 4;
 bc.Params.FileID           := '023083';
 bc.Params.PDF417.FileName  := '123';
 bc.Params.PDF417.FileSize  := 400;
 bc.Params.PDF417.UseMacro  := True;
 bc.Params.PDF417.Sender    := 'CEN BE';
 bc.Params.PDF417.Addresse  := 'ISO CH';
 i  := pdf417EncodeMacroBlock(bc.Params, M);


 write(i);
 bc.Free;
end;

procedure Test2;
var D,E:TpsArray;
begin
  psArrayInitialize(D, 1000);
  psArrayInitialize(E, 1000);

  psDataPut(D, 5);
  psDataPut(D, 453);
  psDataPut(D, 178);
  psDataPut(D, 121);
  psDataPut(D, 239);

  E:=PdfCalcErrorCodes(D, 4);

end;

initialization

//  Test2;

end.








