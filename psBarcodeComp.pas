Unit psBarcodeComp;

{
Library name  ...................: Barcode studio 2011
Classes implemented..............: TpsBarcodeComponent ....
Version..........................: 2011
Status...........................: Final
Author...........................: Peter CIRIP , PSOFT , Slovak republic
Comments, bugs, suggestions to...: peter@psoft.sk, peter@psoft.sk
Authors homepage.................: http://www.psoft.sk
.................................: http://barcode-software.eu
Language.........................: English
Platform.........................: Windows 95/98/NT/2000/Vista/Windows7
Requires.........................: Borland Delphi 6.0 or above
                                   or Borland C++ Builder 5
}

{$I psBarcode.inc}

{.$define PSOFT_DEMO}

interface

uses
  Windows, VCL.Graphics, Classes, SysUtils,

  {$ifdef PSOFT_PROF}   psCode2D, psCodeProf, psCodeGS1,  {$endif}
  {$ifdef PSOFT_STUDIO} psCodeStudio,                     {$endif}

  psCodeExceptions,
  psTypes,
  psCodeSpecs,
  psCodeFNlite,
  psCodeRes;

type
    TpsBarcodeComponent = class;

    TpsParams = class(TPersistent)
    private
      FParentBarcode: TpsBarcodeComponent;
      FECI          : Integer;
      FSegmentIndex : Integer;
      FSegmentCount : Integer;
//      FFNC1Type     : TpsFNC1Type;
//      FFNC1InputType: TpsFNC1InputType;
      FRatio        : TpsRatio;
      {$ifdef PSOFT_PROF}
          FPDF417 : TpsPDF417Params;
          FGS1    : TpsGS1;
          FFileID : String;
      {$endif}
      {$ifdef PSOFT_STUDIO}
          FdmParams     : TpsDataMatrixParams;
          FQrParams     : TpsQRParams;
          FAztecParams  : TpsAztecParams;
      {$endif}
      procedure UpdateBarcode;
      procedure SetECI(const Value: Integer);
      procedure SetSegmentCount(const Value: Integer);
      procedure SetSegmentIndex(const Value: Integer);
      procedure SetRatio(const Value: TpsRatio);
    public
      constructor Create(ParentEAN:TpsBarcodeComponent);
      destructor  Destroy;  override;
      procedure   Assign(Source:TPersistent); override;
      function    BarcodeObject:TpsBarcodeComponent;
      {$ifdef PSOFT_PROF}
          procedure SetFileID(const Value:String);
      {$endif}
      {$ifdef PSOFT_STUDIO}
          property Aztec:TpsAztecParams read FAztecParams write FAztecParams;
      {$endif}
    published
//      property FNC1_Type:TpsFNC1Type read FFNC1Type write SetFNC1Type default fnc1None;
//      property FNC1_InputType:TpsFNC1InputType read fFNC1InputType write SetFNC1InputType;
      property SegmentCount:Integer read FSegmentCount write SetSegmentCount default 1;
      property SegmentIndex:Integer read FSegmentIndex write SetSegmentIndex default 0;
      property ECI:Integer read FECI write SetECI default -1;
      property Ratio:TpsRatio read FRatio write SetRatio default ra20;
      {$ifdef PSOFT_PROF}
          property GS1    : TpsGS1          Read FGS1 write FGS1;
          property PDF417 : TpsPDF417Params Read FPDF417 write FPDF417;
          property FileID : String          read FFileID write SetFileID;
      {$endif}

      {$ifdef PSOFT_STUDIO}
          property DataMatrix:TpsDataMatrixParams read FdmParams write FdmParams;
          property QRCode:TpsQRParams read FqrParams write FqrParams;
      {$endif}
    end;

    TpsCaptionPosition = (cpUpper, cpHuman, cpBottom);

    TpsBarcodeCaption = class(TPersistent)
    private
       FParentBarcode: TpsBarcodeComponent;
       FVisible       : Boolean;
       FText          : TStrings;
       FFont          : TFont;
       FAutoSize      : Boolean;
       FAlignment     : TAlignment;
       FAutoCaption   : Boolean;
       FMaxHeight     : Integer;
       FParentFont    : Boolean;
       FLineSpacing   : Integer;
       FBgColor       : TColor;
       FPosition      : TpsCaptionPosition;
       procedure FontChanged(Sender: TObject);
       procedure UpdateBarcode;
    protected
       function  GetVisible:Boolean;
       procedure SetVisible(const Value:Boolean);
       function  GetText:TStrings;
       procedure SetText(const Value:TStrings);
       function  GetFont:TFont;
       procedure SetFont(const Value:TFont);
       function  GetAutoSize:Boolean;
       procedure SetAutoSize(const Value:Boolean);
       function  GetAlignment:TAlignment;
       procedure SetAlignment(const Value:TAlignment);
       function  GetAutoCaption:Boolean;
       procedure SetAutoCaption(const Value:Boolean);
       function  GetMaxHeight:Integer;
       procedure SetMaxHeight(const Value:Integer);
       function  GetParentFont:Boolean;
       procedure SetParentFont(const Value:Boolean);
       function  GetLineSpacing:Integer;
       procedure SetLineSpacing(const Value:Integer);
       function  GetBgColor:TColor;
       procedure SetBgColor(const Value:TColor);

       procedure   Paint(C:TCanvas; var R:TRect; RC:TPoint; wi:Integer); virtual;
    public
       constructor CreateEAN(AOwner:TpsBarcodeComponent; Pos:TpsCaptionPosition);
       destructor  Destroy; override;
       procedure   Assign(Source: TPersistent); override;
       procedure   UpdateCaption;
    published
       property Visible     : Boolean     Read GetVisible     Write SetVisible;
       property Text        : TStrings    Read GetText        Write SetText;
       property Font        : TFont       Read GetFont        Write SetFont;
       property AutoSize    : Boolean     Read GetAutoSize    Write SetAutoSize;
       property Alignment   : TAlignment  Read GetAlignment   Write SetAlignment;
       property AutoCaption : Boolean     Read GetAutoCaption Write SetAutoCaption;
       property MaxHeight   : Integer     Read GetMaxHeight   Write SetMaxHeight;
       property ParentFont  : Boolean     Read GetParentFont  Write SetParentFont;
       property LineSpacing : Integer     Read GetLineSpacing Write SetLineSpacing;
       property BgColor     : TColor      Read GetBgColor     Write SetBgColor;
    end;


    TpsQuietZoneIndicatorStyle = (qzNone, qzLines, qzTriangel, qzRectangle);

    TpsQuietZone = class(TPersistent)
    private
        FIndicatorSize  : Integer;
        FStyle          : TpsQuietZoneIndicatorStyle;
        FPen            : TPen;
        FSpace          : array[0..3] of Integer;
        FParentBarcode  : TpsBarcodeComponent;
        FVisible        : Boolean;
        procedure SetIndicatorSize(const Value: Integer);
        procedure SetStyle(const Value: TpsQuietZoneIndicatorStyle);
        procedure SetPen(const Value: TPen);
        function  GetSpace(Index: Integer): Integer;
        procedure SetSpace(Index: Integer; Value: Integer);
        procedure SetVisible(const Value: Boolean);
    protected
        procedure   Clear;
        procedure   UpdateBarcode;
    public
        constructor CreateEan(Owner: TpsBarcodeComponent);
        destructor  Destroy; override;
        procedure   Paint(C: TCanvas; var R: TRect);
        procedure   Assign(Source: TPersistent); override;
        procedure   Change(Sender: TObject);
        procedure   SetZone(ValueLeft, ValueTop, ValueWidth, ValueHeight: Integer);
        procedure   ClearZone;
    published
        property Pen: TPen read FPen write SetPen;
        property Style: TpsQuietZoneIndicatorStyle read FStyle write SetStyle default qzLines;
        property IndicatorSize: Integer read FIndicatorSize write SetIndicatorSize default 5;
        property Left: Integer index 0 read GetSpace write SetSpace default 0;
        property Top: Integer index 1 read GetSpace write SetSpace default 0;
        property Right: Integer index 2 read GetSpace write SetSpace default 0;
        property Bottom: Integer index 3 read GetSpace write SetSpace default 0;
        property Visible:Boolean read FVisible write SetVisible default False;
    end;

  TpsHorzLines = class(TPen)
  private
      FBarcodeComp: TpsBarcodeComponent;
      FLinesCount : Integer;
      procedure SetLinesCount(Value:Integer);
  public
      constructor CreateEAN(AOwner:TpsBarcodeComponent);
      procedure   Assign(Source:TPersistent); override;
  published
      property LinesCount : Integer Read FLinesCount Write SetLinesCount Default 0;
  end;


  IpsBarcodeInterface = interface
    ['{872CF3D9-9900-4A50-ABFC-0D546AE71E5E}']

    function  GetAbout:String;

    function  BarcodeComponent:TpsBarcodeComponent;

    function  GetOptions:TpsBarcodeOptions;
    procedure SetOptions(const Value:TpsBarcodeOptions);

    function  GetBackgroundColor: TColor;
    procedure SetBackgroundColor(const Value:TColor);

    function  GetAngle: Integer;
    procedure SetAngle(const Value:Integer);

    function  GetBarcode:String;
    procedure SetBarcode(const Value:String);

    function  GetBarcodeSymbology:TpsBarcodeSymbology;
    procedure SetBarcodeSymbology(const value :TpsBarcodeSymbology);

    function  GetLinesColor:TColor;
    procedure SetLinesColor(const Value:TColor);

    function  GetQuietZone:TpsQuietZone;
    procedure SetQuietZone(const Value:TpsQuietZone);

    function  GetHorzLines:TpsHorzLines;
    procedure SetHorzLines(const Value:TpsHorzLines);

    function  GetParams:TpsParams;
    procedure SetParams(const Value:TpsParams);

    function  GetCaptionUpper:TpsBarcodeCaption;
    procedure SetCaptionUpper(const Value:TpsBarcodeCaption);

    function  GetCaptionBottom:TpsBarcodeCaption;
    procedure SetCaptionBottom(const Value:TpsBarcodeCaption);

    function  GetCaptionHuman:TpsBarcodeCaption;
    procedure SetCaptionHuman(const Value:TpsBarcodeCaption);

    function  GetErrorInfo:TpsBarcodeError;
    procedure SetErrorInfo(Value : TpsBarcodeError);
    // ----------------------------------------------------------------------
    // psBarcode properties
    // ----------------------------------------------------------------------

    property  CaptionHuman:TpsBarcodeCaption read GetCaptionHuman write SetCaptionHuman;
    property  CaptionBottom:TpsBarcodeCaption read GetCaptionBottom write SetCaptionBottom;
    property  CaptionUpper:TpsBarcodeCaption read GetCaptionUpper write SetCaptionUpper;
    property  Params:TpsParams read GetParams write SetParams;
    property  HorzLines:TpsHorzLines read GetHorzLines write SetHorzLines;
    property  QuietZone:TpsQuietZone read GetQuietZone write SetQuietZone;
    property  LinesColor:TColor read GetLinesColor write SetLinesColor;
    property  BarcodeSymbology:TpsBarcodeSymbology read GetBarcodeSymbology write SetBarcodeSymbology;
    property  Barcode:String read GetBarcode write SetBarcode;
    property  Options:TpsBarcodeOptions read GetOptions write SetOptions;
    property  BackgroundColor:TColor read GetBackgroundColor write SetBackgroundColor;
    property  Angle:Integer read GetAngle write SetAngle;
    property  About:String  read GetAbout;
    property  ErrorInfo:TpsBarcodeError read GetErrorInfo write SetErrorInfo;
  end;

  // TOnUpdateBarcodeParent = procedure (Sender:TObject) of object;

  TpsBarcodeComponent = class(TComponent, IpsBarcodeInterface)
  private
         // H1,Ofs[2],Ofs[3],Ofs[4],Ofs[5],Ofs[6]    : Integer;
         // FParentControl  : TComponent;
         FBarCode        : string;
         FBarcodeSymbology     : TpsBarcodeSymbology;
         FLinesColor     : TColor;
         FFontAutoSize   : Boolean;
         FBackgroundColor: TColor;
         //FLabelMask      : String;
         FAngle          : Integer;
         FOnChangeTypCode: TNotifyEvent;
         FOnChangeBarCode: TNotifyEvent;
         FCaptionUpper   : TpsBarcodeCaption;
         FCaptionBottom  : TpsBarcodeCaption;
         FCaptionHuman   : TpsBarcodeCaption;

         FBars           : TStringList;
         FHorzLines      : TpsHorzLines;

         FQuietZone      : TpsQuietZone;

         FParams         : TpsParams;
         FOptions        : TpsBarcodeOptions;

         {$ifdef PSOFT_STUDIO}
              FPixels         : TpsMatrix;
         {$endif}
         FErrorInfo      : TpsBarcodeError;
         FOnUpdate       : TNotifyEvent;

         FOnUpdateBarcodeParent: TNotifyEvent; //TOnUpdateBarcodeParent;

         function    GetAbout:String;
         function    GetBackgroundColor: TColor;
         procedure   SetBackgroundColor(const Value:TColor);

         function    GetBarCode:String;
         procedure   SetBarCode(const Value:String);

         function    GetBarCodeSymbology:TpsBarCodeSymbology;
         procedure   SetBarCodeSymbology(const Value:TpsBarCodeSymbology);

         function    GetOptions:TpsBarcodeOptions;
         procedure   SetOptions(const Value: TpsBarcodeOptions);

         function    GetAngle: Integer;
         procedure   SetAngle(const Value:Integer);

         function    GetLinesColor:TColor;
         procedure   SetLinesColor(const Value:TColor);

         function    GetQuietZone:TpsQuietZone;
         procedure   SetQuietZone(const Value: TpsQuietZone);

         function    GetHorzLines: TpsHorzLines;
         procedure   SetHorzLines(const Value: TpsHorzLines);

         function    GetParams: TpsParams;
         procedure   SetParams(const Value: TpsParams);

         function    GetCaptionUpper:TpsBarcodeCaption;
         procedure   SetCaptionUpper(const Value:TpsBarcodeCaption);

         function    GetCaptionBottom:TpsBarcodeCaption;
         procedure   SetCaptionBottom(const Value:TpsBarcodeCaption);

         function    GetCaptionHuman:TpsBarcodeCaption;
         procedure   SetCaptionHuman(const Value:TpsBarcodeCaption);

         function    GetErrorInfo:TpsBarcodeError;
         procedure   SetErrorInfo(Value : TpsBarcodeError);

         // procedure   SetLabelMask(Value:String);

         function    Codabar(ck:string):String;
         function    AbcCodabar(const ck:string):string;
         function    Code39Standard(const ck:string):string;
         function    Code39Full(const ck:string):string;
         function    Code93Standard(ck:string):string;
         function    Code93Full(ck:string):string;
         function    Code128(const ck:string):string;
         function    UccEan128(const ck:string):string;
         function    EanAddOn(const ck:String; le:Integer):String;
         function    Ean8(const ck:string):string;
         function    Ean13(const ck:string):string;
         function    UPCA(const ck:String):String;
         function    UPCE(const ck:String):String;
         // function    ITF14(ck:string):string;
         function    Plessey(ck:String):String;
         function    Datalogic(const ck:string):String;
         function    Matrix(const ck:string):String;
         function    Industrial(ck:string):String;
         function    Interleaved(ck:string):String;
         function    IATA(ck:string):String;
         function    Invert(ck:string):string;
         function    Coop(ck:string):String;
         function    Code11(ck:string):String;
         function    PostNet(ck:string):string;
         function    PZN(ck:String):String;

         function    CheckEAN8 (var s:String):Boolean;
         function    CheckEAN13(var s:String):Boolean;
         function    CheckUPCA (var s:String):Boolean;
         function    TestCharsInSet(s:String):Boolean;
         procedure   HorzLinesChange(Sender:TObject);

         function    RoyalMail(ck: string): string;
         function    SingaporeCode(ck: string): string;
         function    PlanetCode(ck: string): string;
         function    Monarch(ck: String): String;
         function    PostBar(ck: string): string;
         function    SwissPostal(ck: string): string;
         function    Dutch4StatePostal(ck: string): string;
         function    State4(ck: string): string;
         function    Fim(ck: string): string;

         procedure   UpdateBarcode;
         procedure   DoUpdate;
         procedure   PaintBarCode2(C: TCanvas; R: TRect);
         function    GetLastError: TpsBarcodeError;
         procedure   SetlastError(Value: TpsBarcodeError);
  protected
         function    GetBarCodeLines(BC:String):String;
  public
         constructor Create(AOwner:TComponent); override;
         constructor CreateFromControl(AOwner:TComponent;
              UpdateProc:TNotifyEvent); // TOnUpdateBarcodeParent);
         destructor  Destroy;                   override;

         procedure   ChangeSymbology(dir: TpsDirection; OnlySupported: Boolean);

         function    BarcodeHint:String;

         function    Bars:TStringList;
         function    Zoom(ABarCode:string; AWidth:integer; ASecurity:Boolean):string;

         function    Compress128(s: String): string;
         function    SolveGS1:String;

         procedure   BarcodeRaise; overload;
         procedure   BarcodeRaiseStr(err:TpsBarcodeErrorCode;
              str:String=''; hlp:Integer=0);
         procedure   BarcodeRaise(err:TpsBarcodeErrorCode; idx:Integer=0;
              str:String=''; hlp:Integer=0); overload;
         procedure   BarcodeRaise(err:TpsBarcodeErrorCode; P1,P2:Integer ); overload;
         procedure   BarcodeRaise(err:TpsErrRecord); overload;

         {$ifdef PSOFT_STUDIO}
           function    Pixels:TpsMatrix;
         {$endif}
         procedure   BarcodeComplete;
         procedure   PaintBarCode(C: TCanvas; R: TRect);
         function    GetHint: String;

         function    MinWidth  : Integer;
         function    MinHeight(DefHeightPercent:Integer=40) : Integer;
         function    GetSetOfChars:string;
         function    GetSetOfCharsVisible:String;
         function    CheckBarCode(S:String):Boolean;

         function    BI:TpsBarCodeInfo;

//         function    LastPaintErrorText:String;
         function    GetBarcodeInfo:TpsBarCodeInfo;
         procedure   Assign(Source:TPersistent); override;
         procedure   AssignOnlyBarcode(Source:TpsBarcodeComponent); virtual;
         function    BarcodeTypeName:String;

         function    BarcodeComponent:TpsBarcodeComponent;
  published
         property About             : String         Read GetAbout Stored False;
         property BackgroundColor   : TColor         Read GetBackgroundColor Write SetBackgroundColor;
         property Options           : TpsBarcodeOptions read FOptions write SetOptions;
         property BarCode           : string         Read GetBarCode         Write SetBarCode;
         property BarcodeSymbology  : TpsBarcodeSymbology    Read GetBarcodeSymbology Write SetBarcodeSymbology;
         property QuietZone         : TpsQuietZone read GetQuietZone write SetQuietZone;
         property HorzLines         : TpsHorzLines Read GetHorzLines      Write SetHorzLines;
         property ErrorInfo         : TpsBarcodeError read GetlastError write SetLastError;
         property LinesColor        : TColor         Read GetLinesColor     Write SetLinesColor;

         // property LabelMask         : string         Read FLabelMask       Write SetLabelMask;
         property Angle             : Integer        Read GetAngle         Write SetAngle             Default 0;
         property CaptionUpper      : TpsBarcodeCaption Read GetCaptionUpper   Write SetCaptionUpper  Stored True;
         property CaptionBottom     : TpsBarcodeCaption Read GetCaptionBottom  Write SetCaptionBottom Stored True;
         property CaptionHuman      : TpsBarcodeCaption Read GetCaptionHuman   Write SetCaptionHuman  Stored True;

         property Params:TpsParams read GetParams write SetParams;

         property OnChangeTypCode:TNotifyEvent
              Read FOnChangeTypCode Write FOnChangeTypCode;
         property OnChangeBarCode:TNotifyEvent
              Read FOnChangeBarCode Write FOnChangeBarCode;
         property OnUpdate:TNotifyEvent
              Read FOnUpdate write FOnUpdate;
  end;

  procedure psUpdateBarcode(bc:TComponent);

  procedure PaintBarCode(C:TCanvas; R:TRect; E:TpsBarcodeComponent);
  procedure PaintBarCodeHandle(HWnd:THandle; R:TRect; E:TpsBarcodeComponent);
  function  IAmDemo:Boolean;

  function  IsBookland(ISBN:String):Boolean;
  function  BooklandToEAN(s:String; T:TpsBarcodeSymbology):String;
  function  CalcEan8CheckDigit(s:String):Char;
  function  CalcEan13CheckDigit(s:String):Char;
  function  CalcISBNCheckDigit(s:String):Char;


var ofs : TpsOffsets;
                              //    psBarcodeFmt ,
implementation

uses TypInfo, Math
     {$ifdef PSOFT_EDITORS} ,  psBoxes {$endif}
     {$ifdef PSOFT_JPEG} , VCL.Imaging.JPeg {$endif}
     ;

{$R psBarcode.res}

const
    // OEEOEO
    //           *0*     *1*     *2*     *3*     *4*    *5*     *6*      *7*     *8*      *9*
    ean_sa = '0001101 0011001 0010011 0111101 0100011 0110001 0101111 0111011 0110111 0001011';
    ean_sb = '0100111 0110011 0011011 0100001 0011101 0111001 0000101 0010001 0001001 0010111';
    ean_sc = '1110010 1100110 1101100 1000010 1011100 1001110 1010000 1000100 1001000 1110100';
    ean_sm = 'AAAAAA AABABB AABBAB AABBBA ABAABB ABBAAB ABBBAA ABABAB ABABBA ABBABA';

    c93_r_dolar='100100110';
    c93_r_perc ='111011010';
    c93_r_lom  ='111010110';
    c93_r_plus ='100110010';
    c93_start  ='202022220';
    c93_0      ='100010100';
    c93_1      ='101001000';
    c93_2      ='101000100';
    c93_3      ='101000010';
    c93_4      ='100101000';
    c93_5      ='100100100';
    c93_6      ='100100010';
    c93_7      ='101010000';
    c93_8      ='100010010';
    c93_9      ='100001010';
    c93_A      ='110101000';
    c93_B      ='110100100';
    c93_C      ='110100010';
    c93_D      ='110010100';
    c93_E      ='110010010';
    c93_F      ='110001010';
    c93_G      ='101101000';
    c93_H      ='101100100';
    c93_I      ='101100010';
    c93_J      ='100110100';
    c93_K      ='100011010';
    c93_L      ='101011000';
    c93_M      ='101001100';
    c93_N      ='101000110';
    c93_O      ='100101100';
    c93_P      ='100010110';
    c93_Q      ='110110100';
    c93_R      ='110110010';
    c93_S      ='110101100';
    c93_T      ='110100110';
    c93_U      ='110010110';
    c93_V      ='110011010';
    c93_W      ='101101100';
    c93_X      ='101100110';
    c93_Y      ='100110110';
    c93_Z      ='100111010';
    c93_minus  ='100101110';
    c93_bodka  ='111010100';
    c93_space  ='111010010';
    c93_dolar  ='111001010';
    c93_lom    ='101101110';
    c93_plus   ='101110110';
    c93_perc   ='110101110';
    c93_sp1    ='100100110';
    c93_sp2    ='111011010';
    c93_sp3    ='111010110';
    c93_sp4    ='100110010';
    c93_stop   ='2020222202';


    co_start ='20220020020';
    co_1     ='1010110010';
    co_2     ='1010010110';
    co_3     ='1100101010';
    co_4     ='1011010010';
    co_5     ='1101010010';
    co_6     ='1001010110';
    co_7     ='1001011010';
    co_8     ='1001101010';
    co_9     ='1101001010';
    co_0     ='1010100110';
    co_minus ='1010011010';
    co_dolar ='1011001010';
    co_dvojb ='11010110110';
    co_lom   ='11011010110';
    co_bodka ='11011011010';
    co_plus  ='10110110110';
    co_A     ='10110010010';
    co_B     ='10010010110';
    co_C     ='10100100110';
    co_D     ='10100110010';
    co_stop  ='2022002002';

    PrefixISSN = '977';
    PrefixISBN = '978';
    PrefixISMN = '979';


const
    Code128Table: array[0..106] of string = (
        '1101100110', '1100110110', '1100110011', '1001001100',
        '1001000110', '1000100110', '1001100100', '1001100010',
        '1000110010', '1100100100', '1100100010', '1100010010',
        '1011001110', '1001101110', '1001100111', '1011100110',
        '1001110110', '1001110011', '1100111001', '1100101110',
        '1100100111', '1101110010', '1100111010', '1110110111',
        '1110100110', '1110010110', '1110010011', '1110110010',
        '1110011010', '1110011001', '1101101100', '1101100011',
        '1100011011', '1010001100', '1000101100', '1000100011',
        '1011000100', '1000110100', '1000110001', '1101000100',
        '1100010100', '1100010001', '1011011100', '1011000111',
        '1000110111', '1011101100', '1011100011', '1000111011',
        '1110111011', '1101000111', '1100010111', '1101110100',
        '1101110001', '1101110111', '1110101100', '1110100011',
        '1110001011', '1110110100', '1110110001', '1110001101',
        '1110111101', '1100100001', '1111000101', '1010011000',
        '1010000110', '1001011000', '1001000011', '1000010110',
        '1000010011', '1011001000', '1011000010', '1001101000',
        '1001100001', '1000011010', '1000011001', '1100001001',
        '1100101000', '1111011101', '1100001010', '1000111101',
        '1010011110', '1001011110', '1001001111', '1011110010',
        '1001111010', '1001111001', '1111010010', '1111001010',
        '1111001001', '1101101111', '1101111011', '1111011011',
        '1010111100', '1010001111', '1000101111', '1011110100',
        '1011110001', '1111010100', '1111010001', '1011101111',
        '1011110111', '1110101111', '1111010111',
         // START_A      START_B       START_C
         // PAD          2SA            2SC           2SB
        '2202000020', '2202002000', '2202002220', '1100011100');

       CODE128_STOP_ALL = '2200022202022';
       Code128StartA    = #103;
       Code128StartB    = #104;
       Code128StartC    = #105;
       Code128CodeA     = #101;
       Code128CodeB     = #100;
       Code128CodeC     = #99;
       Code128Shift     = #98;
       Code128FNC1      = #102;
       Code128_PAD      = #103;


procedure psUpdateBarcode(bc:TComponent);
begin
  if bc is TpsBarcodeComponent then
      TpsBarcodeComponent(bc).UpdateBarcode;
end;


// ----------------------------------------------------------------------------
// TpsBarcodeCaptionInt implementation
// ----------------------------------------------------------------------------
  function  TpsBarcodeCaption.GetVisible:Boolean;
  begin
     Result := FVisible;
  end;
  procedure TpsBarcodeCaption.SetVisible(const Value:Boolean);
  begin
    if Value<>FVisible then begin
      FVisible := Value;
      UpdateBarcode;
    end;
  end;

  function  TpsBarcodeCaption.GetText:TStrings;
  begin
    Result := FText;
  end;
  procedure TpsBarcodeCaption.SetText(const Value:TStrings);
  begin
      FText.Assign(Value);
      UpdateBarcode;
  end;

  function  TpsBarcodeCaption.GetFont:TFont;
  begin
      Result:=FFont;
  end;
  procedure TpsBarcodeCaption.SetFont(const Value:TFont);
  begin
      FFont.Assign(Value);
      UpdateBarcode;
  end;

  function  TpsBarcodeCaption.GetAutoSize:Boolean;
  begin
      Result := FAutoSize;
  end;
  procedure TpsBarcodeCaption.SetAutoSize(const Value:Boolean);
  begin
    if Value<>FAutoSize then begin
      FAutoSize := Value;
      UpdateBarcode;
    end;
  end;

  function  TpsBarcodeCaption.GetAlignment:TAlignment;
  begin
    Result := FAlignment;
  end;
  procedure TpsBarcodeCaption.SetAlignment(const Value:TAlignment);
  begin
    if Value<>FAlignment then begin
      FAlignment := Value;
      UpdateBarcode;
    end;
  end;

  function  TpsBarcodeCaption.GetAutoCaption:Boolean;
  begin
    Result := FAutoCaption;
  end;
  procedure TpsBarcodeCaption.SetAutoCaption(const Value:Boolean);
  begin
    if Value<>FAutoCaption then begin
      FAutoCaption := Value;
      UpdateBarcode;
    end;
  end;

  function  TpsBarcodeCaption.GetMaxHeight:Integer;
  begin
    Result := FMaxHeight;
  end;
  procedure TpsBarcodeCaption.SetMaxHeight(const Value:Integer);
  begin
    if Value<>FMaxHeight then begin
      FMaxHeight := Value;
      UpdateBarcode;
    end;
  end;

  function  TpsBarcodeCaption.GetParentFont:Boolean;
  begin
    Result := FParentFont;
  end;
  procedure TpsBarcodeCaption.SetParentFont(const Value:Boolean);
  begin
    if Value<>FParentFont then begin
      FParentFont := Value;
      UpdateBarcode;
    end;
  end;

  function  TpsBarcodeCaption.GetLineSpacing:Integer;
  begin
    Result := FLineSpacing;
  end;
  procedure TpsBarcodeCaption.SetLineSpacing(const Value:Integer);
  begin
    if Value<>FLineSpacing then begin
      FLineSpacing := Value;
      UpdateBarcode;
    end;
  end;

  function  TpsBarcodeCaption.GetBgColor:TColor;
  begin
    Result := FBgColor;
  end;
  procedure TpsBarcodeCaption.SetBgColor(const Value:TColor);
  begin
    if Value<>FBgColor then begin
      FBgColor := Value;
      UpdateBarcode;
    end;
  end;

  constructor TpsBarcodeCaption.CreateEAN(AOwner:TpsBarcodeComponent;
        Pos:TpsCaptionPosition);
  begin
     Create;
     FParentBarcode := AOwner;
     FPosition      := Pos;
     FText          := TStringList.Create;
     // FText.Add(' ');
     FVisible       := True;
     FAutoSize      := True;
     FAutoCaption   := False;
     FMaxHeight     := 25;
     FBgColor       := clNone;

     FFont          := TFont.Create;
     FFont.Name     := 'Arial';
     FFont.Size     := 10;
     FFont.OnChange := FontChanged;
  end;
  destructor TpsBarcodeCaption.Destroy;
  begin
     FFont.Free;
     FText.Free;
     inherited Destroy;
  end;

  procedure TpsBarcodeCaption.Assign(Source: TPersistent);
  var s : TpsBarcodeCaption;
  begin
      if Source is TpsBarcodeCaption then begin
        s            := TpsBarcodeCaption(Source);
        FVisible     := S.Visible;
        FText.Text   := S.Text.Text;
        FAutoSize    := S.AutoSize;
        FAutoCaption := S.AutoCaption;
        FAlignment   := S.Alignment;
        FFont.Assign(S.Font);
        FMaxHeight   := S.MaxHeight;
        FParentFont  := S.ParentFont;
        FLineSpacing := S.LineSpacing;
        FBgColor     := S.BgColor;
      end
        else
          inherited Assign(Source);
  end;


  procedure TpsBarcodeCaption.UpdateBarcode;
  begin
      psUpdateBarcode(FParentBarcode);
  end;

  procedure TpsBarcodeCaption.UpdateCaption;
  begin
    {}
  end;

procedure TpsBarcodeCaption.FontChanged(Sender: TObject);
begin
     UpdateBarcode;
end;

procedure   TpsBarcodeCaption.Paint(C:TCanvas; var R:TRect; RC:TPoint; wi:Integer);
var R1          : TRect;
    x,i,j, he   : Integer;
    LineHeight  : Integer;
    TotalHeight : Integer;
    bc          : TpsBarcodeComponent;
    w           : Integer;
begin

    if (not Visible) or (Length(Text.Text)=0) then
        Exit;

    bc := TpsBarcodeComponent(FParentBarcode);

    C.Brush.Color := BgColor;
    R1            := R;
    R1.Right      := R1.Left + wi;

    if not ParentFont then
        C.Font.Assign(Font);

    if FMaxHeight>0 then begin
        LineHeight  := MulDiv(C.TextHeight('A'), 100+FLineSpacing, 100);
        TotalHeight := LineHeight*Text.Count;
        he:= MulDiv(100,TotalHeight,R1.Bottom-R1.Top);
        if he>FMaxHeight then
            C.Font.Size := MulDiv(C.Font.Size, FMaxHeight, he);
    end;

    w := 0;
    if AutoSize then begin
        w:=psMaxLineTextWidth(C, Text);
        if w>0 then
            w:= WidthOf(R) div w;
    end;

    RotateFont(C.Font, FParentBarcode.Angle, w);

    LineHeight  := MulDiv(C.TextHeight('A'), 100+FLineSpacing, 100);
    TotalHeight := LineHeight*Text.Count;

    if FPosition=cpUpper then begin
      R1.Bottom := R1.Top  + TotalHeight; //Round(1.2*Abs(C.Font.Height));
      R.Top := R1.Bottom+1;
    end else begin
      R1.Top   := R1.Bottom - TotalHeight; //Round(1.2*Abs(C.Font.Height));
      R.Bottom := R1.Top-1;
    end;

    if boReflectanceReversal in bc.Options then begin
        C.Font.Color  := clWhite;
        C.Brush.Color := bc.LinesColor;
    end else begin
        C.Brush.Color := BgColor;
    end;

    for x := 0 to Text.Count - 1 do begin
      i:=C.TextWidth(Text.Strings[x]);
      j:=R1.Left;
      case Alignment of
        taLeftJustify   : j := R1.Left;
        taCenter        : j := R1.Left  + (R1.Right-R1.Left-i) div 2;
        taRightJustify  : j := R1.Right - i;
      end;
      DrawRotatedText(C, j, R1.Top, rc, FParentBarcode.Angle, Text.Strings[x]);
      Inc(R1.Top, LineHeight);
    end;
end;


//------ TpsBarcodeCaption end --------------------------------------



//--------------------------------------------------------------------
// - TpsQuietZone implementation
// -------------------------------------------------------------------
function TpsQuietZone.GetSpace(Index: Integer): Integer;
begin
    if (Index >= 0) and (Index <= 3) then
        Result := FSpace[Index]
    else
        Result := 0;
end;

procedure TpsQuietZone.SetSpace(Index: Integer; Value: Integer);
begin
    if (Index >= 0) and (Index <= 3) then
        if FSpace[Index] <> Value then
            if (Value >= 0) and (Value <= 100) then begin
                FSpace[Index] := Value;
                Change(self);
            end;
end;


procedure TpsQuietZone.Clear;
var i: Integer;
begin
    for i := 0 to 3 do FSpace[i] := 0;
end;


procedure TpsQuietZone.Assign(Source: TPersistent);
var qz:TpsQuietZone;
begin
    if source is TpsQuietZone then begin
        qz            :=TpsQuietZone(Source);
        Pen.Assign(qz.Pen);
        Style         := qz.Style;
        IndicatorSize := qz.IndicatorSize;
        Left          := qz.Left;
        Top           := qz.Top;
        Right         := qz.Right;
        Bottom        := qz.Bottom;
        Visible       := qz.Visible;
    end;
end;

constructor TpsQuietZone.CreateEan(Owner: TpsBarcodeComponent);
begin
    inherited Create;

    FIndicatorSize  := 5;
    FStyle          := qzLines;

    FParentBarcode  := Owner;
    FPen            := TPen.Create;
    FPen.Style      := psSolid;
    FPen.Color      := clBlack;
    FPen.OnChange   := Self.Change;
end;

destructor TpsQuietZone.Destroy;
begin
    FPen.Free;
    inherited Destroy;
end;

procedure TpsQuietZone.Paint(C: TCanvas; var R: TRect);
var R2: TRect;
    w, h: Integer;
begin
    if not Visible then Exit;

    w := R.Right - R.Left;
    h := R.Bottom - R.Top;

    if Left>0   then Inc(R.Left, MulDiv(w,Left,100) );
    if Right>0  then Dec(R.Right, MulDiv(w,Right,100) );
    if Top>0    then Inc(R.Top, MulDiv(h,Top,100) );
    if Bottom>0 then Dec(R.Bottom, MulDiv(h,Bottom,100) );

    R2 := R;
    Inc(R2.Left, MulDiv(IndicatorSize, w, 100));
    Inc(R2.Top, MulDiv(IndicatorSize, h, 100));
    Dec(R2.Right, MulDiv(IndicatorSize, w, 100));
    Dec(R2.Bottom, MulDiv(IndicatorSize, h, 100));

    // if used indicators, display this
    if (IndicatorSize > 0) and (Style <> qzNone) then begin
        C.Pen.Assign(FPen);
        with C do begin
            Brush.Style := bsSolid;
            Brush.Color := clBlack;

            case Style of
                qzNone: ;
                qzLines: begin
                        Brush.Style := bsClear;
                        MoveTo(R2.Left, R.Top); LineTo(R2.Left, R2.Top); LineTo(R.Left, R2.Top);
                        MoveTo(R2.Left, R.Bottom); LineTo(R2.Left, R2.Bottom); LineTo(R.Left, R2.Bottom);
                        MoveTo(R2.Right, R.Bottom); LineTo(R2.Right, R2.Bottom); LineTo(R.Right, R2.Bottom);
                        MoveTo(R2.Right, R.Top); LineTo(R2.Right, R2.Top); LineTo(R.Right, R2.Top);
                    end;
                qzTriangel: begin
                        Polygon([Point(R2.Left, R.Top), Point(R2.Left, R2.Top), Point(R.Left, R2.Top)]);
                        Polygon([Point(R2.Left, R.Bottom), Point(R2.Left, R2.Bottom), Point(R.Left, R2.Bottom)]);
                        Polygon([Point(R2.Right, R.Bottom), Point(R2.Right, R2.Bottom), Point(R.Right, R2.Bottom)]);
                        Polygon([Point(R2.Right, R.Top), Point(R2.Right, R2.Top), Point(R.Right, R2.Top)]);
                    end;
                qzRectangle: begin
                        Rectangle(R.Left, R.Top, R2.Left, R2.Top);
                        Rectangle(R.Left, R2.Bottom, R2.Left, R.Bottom);
                        Rectangle(R2.Right, R2.Bottom, R.Right, R.Bottom);
                        Rectangle(R2.Right, R.Top, R.Right, R2.Top);

                    end;
            end;
        end;
    end;
    R := R2;
end;

procedure TpsQuietZone.SetIndicatorSize(const Value: Integer);
begin
    if Value <> FIndicatorSize then begin
        FIndicatorSize := Value;
        UpdateBarcode;
    end;
end;

procedure TpsQuietZone.SetPen(const Value: TPen);
begin
    FPen.Assign(Value);
    UpdateBarcode;
end;

procedure TpsQuietZone.SetStyle(const Value: TpsQuietZoneIndicatorStyle);
begin
    if FStyle <> Value then begin
        FStyle := Value;
        UpdateBarcode;
    end;
end;

procedure TpsQuietZone.Change(Sender: TObject);
begin
       UpdateBarcode;
end;

procedure TpsQuietZone.SetZone(ValueLeft, ValueTop, ValueWidth,
    ValueHeight: Integer);
begin
    FSpace[0] := ValueLeft;
    FSpace[1] := ValueTop;
    FSpace[2] := ValueWidth;
    FSpace[3] := ValueHeight;
    Change(Self);
end;

procedure TpsQuietZone.UpdateBarcode;
begin
    psUpdateBarcode(FParentBarcode);
end;

procedure TpsQuietZone.ClearZone;
begin
    Clear;
    Change(Self);
end;

procedure TpsQuietZone.SetVisible(const Value: Boolean);
begin
  if FVisible<>Value then begin
    FVisible := Value;
    Change(Self);
  end;
end;


// ---------------------------------------------------------------------
// --- TpsHorzLInes implementation
// ---------------------------------------------------------------------
constructor TpsHorzLines.CreateEAN(AOwner:TpsBarcodeComponent);
begin
  Create;
  FBarcodeComp := AOwner;
end;

procedure TpsHorzLines.Assign(Source: TPersistent);
begin
    if Source is TpsHorzLines then begin
      FLinesCount := TpsHorzLines(Source).LinesCount;
    end
    else
      inherited;
end;

procedure TpsHorzLines.SetLinesCount(Value:Integer);
begin
     if Value<>FLinesCount then begin
        FLinesCount:=Value;
        if Assigned(OnChange) then OnChange(Self);
     end;
end;





// ---------------------------------------------------------------------
// --- TpsParams implementation
// ---------------------------------------------------------------------

{ TpsParams }

procedure TpsParams.Assign(Source: TPersistent);
var V:TpsParams;
begin
  // inherited;
  if Source is TpsParams then begin
      V:=TpsParams(Source);

      FSegmentCount := V.SegmentCount;
      FSegmentIndex := V.SegmentIndex;
      FECI          := V.ECI;
//      FFNC1Type     := V.FNC1_Type;
//      FFNC1InputType:= V.FNC1_InputType;

      {$ifdef PSOFT_PROF}
          GS1.Assign(V.GS1);
          FPDF417.Assign(V.PDF417);
      {$endif}

      {$ifdef PSOFT_STUDIO}
          FdmParams.Assign(V.DataMatrix);
          FqrParams.Assign(V.QRCode);
          FAztecParams.Assign(V.Aztec);
      {$endif}

      UpdateBarcode;
  end;

end;

function TpsParams.BarcodeObject: TpsBarcodeComponent;
begin
    Result := FParentBarcode;
end;

constructor TpsParams.Create(ParentEAN:TpsBarcodeComponent);
begin
  inherited Create;
  FParentBarcode:= ParentEAN;
  FECI          :=  -1;
  FSegmentCount :=   1;
  FSegmentIndex :=   0;
  FGS1          := TpsGS1.CreateBarcode(ParentEAN);
//  FFNC1Type     := fnc1None;
//  FFNC1InputType:= gs1Classic;
  FRatio        := ra20;
  {$ifdef PSOFT_PROF}
     FPDF417    := TpsPDF417Params.CreateBarcode(ParentEAN);
  {$endif}

  {$ifdef PSOFT_STUDIO}
      FdmParams  := TpsDataMatrixParams.CreateBarcode(ParentEAN);
      FQrParams  := TpsQRParams.CreateBarcode(ParentEAN);
      FAztecParams := TpsAztecParams.CreateBarcode(ParentEAN);
  {$endif}

end;

destructor TpsParams.Destroy;
begin
  {$ifdef PSOFT_STUDIO}
      FAztecParams.Free;
      FdmParams.Free;
      FQrParams.Free;
  {$endif}
  {$ifdef PSOFT_PROF}
      FGS1.Free;
      FPDF417.Free;
  {$endif}
  inherited;
end;


{$ifdef PSOFT_PROF}
  procedure TpsParams.SetFileID(const Value:String);
  begin
      if FFileID<>Value then begin
          FFileID:=Value;
          UpdateBarcode;
      end;
  end;
{$endif}

procedure TpsParams.SetECI(const Value: Integer);
begin
  if FECI<>Value then begin
    FECI := Value;
    UpdateBarcode;
  end;
end;


procedure TpsParams.SetRatio(const Value: TpsRatio);
begin
  if FRatio<>Value then begin
    FRatio := Value;
    UpdateBarcode;
  end;
end;

procedure TpsParams.SetSegmentCount(const Value: Integer);
begin
  if FSegmentCount<>Value then begin
    FSegmentCount := Value;
    UpdateBarcode;
  end;
end;

procedure TpsParams.SetSegmentIndex(const Value: Integer);
begin
  if FSegmentIndex <> Value then begin
    FSegmentIndex := Value;
    UpdateBarcode;
  end;
end;

procedure TpsParams.UpdateBarcode;
begin
    psUpdateBarcode(FParentBarcode);
end;







{ ---------------------------------------------------------------------------}


function TpsBarcodeComponent.TestCharsInSet(s:String):Boolean;
var i:Integer;
    mn:String;
begin
    Result:=True;
    mn:=GetSetOfChars;
    if mn='' then Exit;

    for i:=1 to Length(s) do
        if Pos(s[i],mn)<=0 then begin
           Result := False;
           BarcodeRaise(erCharOutOfSet, i, s[i]);
        end;
end;


function TpsBarcodeComponent.CheckEAN13(var s:String):Boolean;
begin
     if Length(s) in [12,13,16,19] then begin
        if Length(s)=12 then s:=s+' ';
        S[13]  := CalcEan13CheckDigit(Copy(s,1,12));
        Result := TestCharsInSet(Copy(S,1,13)+Copy(S,15,5));

        if (Length(s)>13) and Result then begin
           Result:= s[14]=' ';
           if Not Result then
              BarcodeRaise(erCharMustBeSpace, 14);
        end;
     end else begin
        Result          := False;
        BarcodeRaise(erBarcodeLengthInvalid);
     end;
end;

function TpsBarcodeComponent.CheckEan8(var s:String):Boolean;
begin
     if Length(s) in [7,8,11,14] then begin
        if Length(s)=7 then s:=s+' ';
        S[8]  := CalcEan8CheckDigit(Copy(s,1,7));
        Result := TestCharsInSet(Copy(S,1,8)+Copy(S,10,5));
        if (Length(s)>8) and Result then begin
           Result:= s[9]=' ';
           if Not Result then
              BarcodeRaise(erCharMustBeSpace, 9);
        end;
     end else begin
        Result          := False;
        BarcodeRaise(erBarcodeLengthInvalid);
     end;
end;

function TpsBarcodeComponent.CheckUPCA(var s:String):Boolean;
begin
     if Length(s) in [11,12,15,18] then begin
        if Length(s)=11 then s:=s+' ';
        S[12]  := CalcEan13CheckDigit(Copy(s,1,11));
        Result := TestCharsInSet(Copy(S,1,12)+Copy(S,14,5));
        if (Length(s)>12) and Result then begin
           Result:= s[13]=' ';
           if Not Result then
              BarcodeRaise(erCharMustBeSpace,13);
        end;
     end else begin
        Result          := False;
        BarcodeRaise(erBarcodeLengthInvalid);
     end;
end;

function TpsBarcodeComponent.GetSetOfChars:string;
begin
     result:=BarcodeInfo(FBarcodeSymbology,Barcode).Chars;
end;

function    TpsBarcodeComponent.GetSetOfCharsVisible:String;
var s:String;
    i:Integer;
begin
     s:=GetSetOfChars;
     Result:='';
     for i:=1 to Length(s) do
         if s[i]<' ' then Result:=Result+'^'+Char(Ord(s[i])+Ord('A'))+' '
         else             Result:=Result+s[i]+' ';
     s:=Result;
end;

function TpsBarcodeComponent.CheckBarCode(S:String):Boolean;
var pom : string;
    i   : Integer;
    c1,c2 : Char;
begin
     result := (s<>'');
     if s='' then
        BarcodeRaise(erEmptyCode);

     case FBarcodeSymbology of
          bcEan8,bcJAN8   : Result:=CheckEan8(s);
          bcEan13,bcJAN13 : Result:=CheckEan13(s);
          bcUPCA          : Result:=CheckUPCA(s);
          bcITF   :
                begin
                      Result := (Length(s) mod 2 =0);
                      if (not Result) and (boAddLeadingZero in Options) then
                          Result:= True;
                end;
          bcISSN,bcISBN,bcISMN : begin
                        pom:='';
                        for i:=1 to Length(S) do
                              if S[i]<>'-' then pom:=pom+s[i];
                        if Length(pom)>10 then begin
                           Result:=(pom[11]=' ');
                        end;

                        if Result then begin
                           Result:= (Length(pom) in [10,13,16]);
                        end;

                        if Result then
                           Result:=TestCharsInSet(Copy(pom,1,9)+Copy(pom,12,5));
                   end;
           bc25Interleaved : begin
                 Result:= TestCharsInSet(s);
                 if Result then begin
                    Result := (Length(s) mod 2 = 0);
                 end;
             end;
           bcUpcE0,bcUpcE1 : begin
                        Result:= (Length(s) in [8,11,14]);
                        //if not Result then
                        //      FLastPaintError := erBarcodeLengthInvalid;
                        if Result then
                              Result := TestCharsInSet(Copy(s,1,8)+Copy(s,10,5));
                        if Result then begin
                           Result:=(s[1]='0');
                           //if not Result then
                           //   FLastPaintError := erFirstCharMustBeZero;
                        end;
                  end;
           bcUPCShipping : begin
                        Result:= (Length(s) in [14,21]);
                        // if not Result then
                        //      FLastPaintError := erBarcodeLengthInvalid;
                        if Result then
                              Result := TestCharsInSet(Copy(s,1,14)+Copy(s,16,6));
                  end;
           bcPostNet : begin
                            Result:= (Length(s) in [5,9,11]);
                            //if not Result then
                            //  FLastPaintError := erBarcodeLengthInvalid;
                            if Result then
                              Result := TestCharsInSet(s);
                       end;

           bcSingapore4StatePostalCode: begin
                Result:=TestCharsInSet(s);
                if Result then
                        Result:=(Length(s)=6);
           end;

           bcPlanet : begin
                        Result := TestCharsInSet(s);
                        if Result then Result:= (Length(s) in [11,13]);
                      end;

           bcRoyalMail,
           bcSwissPostal,
           bcPostBar,
           bcDutch4StatePostal,
           bc4State   : Result := TestCharsInSet(s);
           bcOPC     : begin
                            Result:= (Length(s)=10);
                            //if not Result then
                            //  FLastPaintError := erBarcodeLengthInvalid;
                            if Result then
                              Result := TestCharsInSet(s);
                       end;
           { bcMSI     : Result := TestCharsInSet(s);}
           { bcPlessey, bcADSPlessey : ;}
           bcAbcCodabar    : begin
                               Result := TestCharsInSet(s);
                               c1:=UpCase(s[1]);
                               c2:=UpCase(s[Length(s)]);
                               if Result then
                                  Result := (c1>='A') and (c1<='D')
                                   and (c2>='A') and (c2<'D');
                          end;
           bcMSIPlessey : begin
                            Result:= (Length(s)<=13);
                            if Result then
                              Result := TestCharsInSet(s);
                       end;
           bcPZN : begin
                        Result := TestCharsInSet(s);
                        if Result then
                           Result := Length(s) in [6,7];

              end;
          {$ifdef PSOFT_PROF}
              bcTelepen          : Result := TestCharsInSet(s);
              bcIntelligentMail  : Result := IntelligentMailCheck(s, Self);
              bcAustraliaPost    : Result := AustraliaPostCheck(s,Self);
              bcPostbarCPC4State : Result := CPC4StateCheck(s);

              // bcPostbarCPC4State : Result := CPC4StateCheck(s);
          {$endif}
          else    Result := TestCharsInSet(s);
     end;
end;

function TpsBarcodeComponent.EanAddOn(const ck:String; le:Integer):String;
var i,j :Integer;
    s,sp:String;
begin
     // s     := '       ';
     s     := '000000000';
     Ofs[5]    := Length(s)+le+1;
     s     := s+'X1011';

     { two characters additional code}
     if Length(ck)=2 then begin
        j:=StrToInt(ck);
        case j mod 4 of
             0 : sp:='AA';
             1 : sp:='AB';
             2 : sp:='BA';
             3 : sp:='BB';
        end;
        case sp[1] of
          'A': S:=S+Copy(ean_sa,8*(Ord(ck[1])-Ord0)+1,7);
          'B': S:=S+Copy(ean_sb,8*(Ord(ck[1])-Ord0)+1,7);
        end;
        s:=s+'01';
        case sp[2] of
          'A': S:=S+Copy(ean_sa,8*(Ord(ck[2])-Ord0)+1,7);
          'B': S:=S+Copy(ean_sb,8*(Ord(ck[2])-Ord0)+1,7);
        end;
     end;

     { five characters additional code}
     if Length(ck)=5 then begin
        j:=3*(Ord(ck[1])+Ord(ck[3])+Ord(ck[5]) - 3* Ord0)
          +9*(Ord(ck[2])+Ord(ck[4])            - 2* Ord0);
        j:=j mod 10;

        case j of
             0 : sp:='BBAAA';
             1 : sp:='BABAA';
             2 : sp:='BAABA';
             3 : sp:='BAAAB';
             4 : sp:='ABBAA';
             5 : sp:='AABBA';
             6 : sp:='AAABB';
             7 : sp:='ABABA';
             8 : sp:='ABAAB';
             9 : sp:='AABAB';
        end;

        for i:=1 to Length(ck) do begin
            if i>1 then s:=s+'01';
            case sp[i] of
                'A': S:=S+Copy(ean_sa,8*(Ord(ck[i])-Ord0)+1,7);
                'B': S:=S+Copy(ean_sb,8*(Ord(ck[i])-Ord0)+1,7);
                'C': S:=S+Copy(ean_sc,8*(Ord(ck[i])-Ord0)+1,7);
            end;
        end;
     end;

     Result := s;
end;

function TpsBarcodeComponent.Ean8(const ck:string):string;
var i,ma : integer;
    s    : string;
begin
   ma        := Length(ck);
   Result := '';
   if ma<8 then Exit;

   s:='202';
   Ofs[1]:=Length(s)+1;
   for i:=1 to 4 do S:=S+Copy(ean_sa,8*(Ord(ck[i])-Ord0)+1,7);
   Ofs[2]:=Length(s)-1;
   s:=s+'02020';
   Ofs[3]:=Length(s)+1;
   for i:=5 to 8 do
         S:=S+Copy(ean_sc,8*(Ord(ck[i])-Ord0)+1,7);
   Ofs[4]:=Length(s)-1;
   s:=s+'202';

   Result:=s;

   if length(ck)>8 then
      Result:=Result+EanAddOn(Copy(ck,10,5), Length(Result));
end;

function TpsBarcodeComponent.Ean13(const ck:string):String;
var i             : integer;
    s1            : char;
    s,sp          : string;
begin

   Result    := '';

   sp := ' '+Copy(ean_sm,7*(Ord(ck[1])-Ord0)+1,6);
   s:='202';
   Ofs[1]:=Length(s)+1;
   for i:=2 to 7 do begin
       s1:=sp[i];
       case s1 of
         'A': S:=S+Copy(ean_sa,8*(Ord(ck[i])-Ord0)+1,7);
         'B': S:=S+Copy(ean_sb,8*(Ord(ck[i])-Ord0)+1,7);
         'C': S:=S+Copy(ean_sc,8*(Ord(ck[i])-Ord0)+1,7);
       end;
   end;

   Ofs[2]:=Length(s)-1;
   s:=s+'02020';
   Ofs[3]:=Length(s)+1;
   for i:=8 to 13 do S:=S+Copy(ean_sc,8*(Ord(ck[i])-Ord0)+1,7);
   Ofs[4]:=Length(s)-1;
   s:=s+'202';

   Result:=s;
   if length(ck)>13 then
      Result:=Result+EanAddOn(Copy(ck,15,5), Length(Result));
end;

function TpsBarcodeComponent.UccEan128(const ck: string): string;
var i   : integer;
    tmp : string;
begin
     result := '';

     tmp    := Compress128(ck);
     if tmp ='' then
          Exit;

     if boAutoCheckDigit in Options then
        tmp:=tmp+Mod103CheckDigit(tmp);

     for i:=1 to Length(tmp) do
         result:=result+Code128Table[Ord(tmp[i])]+'0';
     result:=result+CODE128_STOP_ALL;

     Ofs[1]:=12;
     Ofs[2]:=11*Length(tmp)-1;
end;

function TpsBarcodeComponent.UPCA(const ck:String):String;
var i:Integer;
    s:String;
begin
     { start sequence}
     s := '202';
     Ofs[1]:=Length(s)+8;
     { left 6 digits  uses ean code table part A}
     for i:=1 to 6 do
         S:=S+Copy(ean_sa,8*(Ord(ck[i])-Ord0)+1,7);

     Ofs[2]:=Length(s)-1;
     { middle sequence}
     s :=s+'02020';
     Ofs[3]:=Length(s)+1;

     { right 6 digits uses ean code table part C}
     for i:=7 to 12 do
         S:=S+Copy(ean_sc,8*(Ord(ck[i])-Ord0)+1,7);

     { stop sequence }
     Ofs[4]:=Length(s)-8;
     s :=s+'202';
     Ofs[6]:=Length(s)+1;

     for i:=4 to 11 do if s[i]='1' then s[i]:='2';
     for i:=Length(s)-9  to Length(s)-2 do
         if s[i]='1' then s[i]:='2';

     if CaptionHuman.Visible and (Length(ck)<=12) then s:=s+'         ';

     Result := s;

     { UPC with extension ?}
     if length(ck)>12 then
        Result:=Result+EanAddOn(Copy(ck,14,5), Length(Result));

end;

function TpsBarcodeComponent.UPCE(const ck:String):String;
const UpcESystem0 : array [0..9] of String[6]=
          ('EEEOOO',  'EEOEOO',  'EEOOEO',  'EEOOOE',  'EOEEOO',
           'EOOEEO',  'EOOOEE',  'EOEOEO',  'EOEOOE',  'EOOEOE');
      UpcESystem1 : array [0..9] of String[6]=
          ('OOOEEE',  'OOEOEE',  'OOEEOE',  'OOEEEO',  'OEOOEE',
           'OEEOOE',  'OEEEOO',  'OEOEOE',  'OEOEEO',  'OEEOEO');
var s:String;
    i :Integer;
    p : String[1];
    cd: Integer;
begin
     cd := Ord(ck[8])-Ord0;
     { start sequence}
     s := '202';
     Ofs[1]:=Length(s)+1;
     for i:=2 to 7 do begin
         If FBarcodeSymbology=bcUPCE0 then P:=UpcESystem0[cd][i-1]
         else                        P:=UpcESystem1[cd][i-1];
         if P='O' then S:=S+Copy(ean_sa,8*(Ord(ck[i])-Ord0)+1,7)
         else          S:=S+Copy(ean_sb,8*(Ord(ck[i])-Ord0)+1,7);
     end;
     Ofs[2] := Length(s)-1;
     s  := s+'020202';
     Ofs[6] := Length(s)+1;

     If CaptionHuman.Visible and (Length(ck)<=8) then s:=s+'     ';
     Result := s;
     { UPC with extension ?}
     if length(ck)>8 then
        Result:=Result+EanAddOn(Copy(ck,10,5), Length(Result));
end;

function    TpsBarcodeComponent.Plessey(ck:String):String;
const _n:array['0'..'9'] of string=(
             '0000', '1000', '0100', '1100',
             '0010', '1010', '0110', '1110',
             '0001', '1001');
      _c:Array['A'..'F'] of String=(
             '0101', '1101', '0011', '1011', '0111', '1111');
var s,pom,pom1  : String;
    i,j,su      : Integer;
    c           : Char;
    function MSICheck(ck:String):Char;
    var su_e,i : Integer;
        pom    : String;
    begin
         su := 0;
         pom:='';
         su_e:=0;
         if Length(ck)>=2 then begin
            for i:=1 to Length(ck) do
                if i mod 2 =0 then pom:=pom+ck[i]
                else               Inc(su_e, Ord(ck[i])-Ord0);
            pom:=IntToStr(2*StrToInt(pom));
            for i:=1 to Length(pom) do
                Inc(su_e, Ord(pom[i])-Ord0);
            su_e := 10 - (su_e mod 10);
            if su_e=10 then su_e:=0;
            Result :=Char(Ord0+su_e);
         end
            else Result:=' ';
    end;
begin
     case FBarcodeSymbology of
//          bcPlessey    : s      := '1101';
//          bcMSI        : s      := '1';
//          bcADSPlessey : s      := '22';
          bcMSIPlessey : begin
                       s      := '1';
                       { adding MSI check char}
                       if Length(ck)>=2 then begin
                          ck := ck+MSICheck(ck);
                          if Length(ck)=14 then
                             ck := ck+MSICheck(ck);
                       end;
               end;
     end;


     for i:=1 to Length(ck) do begin
         c := ck[i];
         if IsDigit(c) then pom:=_n[c];
         if (c>='A') and (c<='F') then pom:=_c[c];
         if FBarcodeSymbology in [{bcMSI,}bcMSIPlessey] then begin
              pom1:='';
              for j:=1 to Length(pom) do
                  pom1:=Pom[j]+Pom1;
              pom:=Pom1;
         end;
         s:=s+pom;
     end;

     case FBarcodeSymbology of
//          bcPlessey    : s      := s+'110011';
//          bcMSI        : s      := s+'00';
          bcMSIPlessey : s      := s+'00';
//          bcADSPlessey : s      := s+'22';
     end;

     pom:=s;
     s:='';
     for i:=1 to Length(pom) do begin
         case pom[i] of
              '0' : s:=s+'10000';
              '1' : s:=s+'11100';
              '2' : s:=s+'22200';
              '3' : s:=s+'20000';
         end;
     end;
     Result := s;
     Ofs[1]     := Length(s) div 4;
     Ofs[2]     := Length(s) - Ofs[1];
end;

function CorrectHight(s:string):string;
var i:Integer;
begin
     for i:=1 to Length(s) do begin
         if s[i]='0' then Result:=Result+'M0'
         else             Result:=Result+'10';
     end;
end;

const PostnetTable : Array ['0'..'9'] of String = (
         '11000','00011','00101','00110','01001',
         '01010','01100','10001','10010','10100');


function  TpsBarcodeComponent.PostNet(ck:string):string;
var p  : String;
    i:Integer;
begin
     p :='1';
     for i:=1 to Length(ck) do
         p :=p+PostnetTable[ck[i]];

     if boAutoCheckDigit in Options then
        p:=p+PostnetTable[Modulo10(ck)];

     p :=p+'1';
     Result:=CorrectHight(p);
end;

const RoyalTable:array['0'..'9'] of string=
        ('SSVV','SDHV','SDVH','DSHV','DSVH','DDHH','SHDV','SVSV','SVDH','DHSV');
      RoyalTableChars: array ['A'..'Z'] of string=
        ('DHDH','DVSH','SHVD','SVHD','SVVS','DHHD','DHVS','DVHS','HSDV',
        {J}
        'HDSV','HDDH','VSSV','VSDH','VDSH','HSVD','HDHD','HDVS','VSHD','VSVS',
        {T}
        'VDHS','HHDD','HVSD','HVDS','VHSD','VHDS','VVSS');
      RoyalCheckSums:array[0..5,0..5] of char =(
        ('Z','U','V','W','X','Y'),
        ('5','0','1','2','3','4'),
        ('B','6','7','8','9','A'),
        ('H','C','D','E','F','G'),
        ('N','I','J','K','L','M'),
        ('T','O','P','Q','R','S')
        );

function CalcRoyalCheckSum(s:string):string;
var i,cs1,cs2:integer;
    cs:char;
    tmp : char;
begin
      cs1:=0;
      cs2:=0;
      for i:=1 to length(s) do begin
        tmp := s[i];
        case (i mod 4) of
                1 : begin if (tmp='V') or (tmp='H') then Inc(cs1,4);
                          if (tmp='V') or (tmp='D') then Inc(cs2,4);
                    end;
                2 : begin if (tmp='V') or (tmp='H') then Inc(cs1,2);
                          if (tmp='V') or (tmp='D') then Inc(cs2,2);
                    end;
                3 : begin if (tmp='V') or (tmp='H') then Inc(cs1,1);
                          if (tmp='V') or (tmp='D') then Inc(cs2,1);
                    end;
                4 : ;
        end;
      end;
        cs1:=cs1 mod 6;
        cs2:=cs2 mod 6;

      cs:=RoyalCheckSums[cs1,cs2];
      if IsUpper(cs) then
                result:=RoyalTableChars[cs];
      if IsDigit(cs) then
                result:=RoyalTable[cs];
end;


FUNCTION RoyalExpand(s:string):string;
var i:Integer;
begin
      for i:=1 to Length(s) do
        result:=result+s[i]+'0';
end;

function  CalcRoyal(ck:string):string;
var s:string;
    i:Integer;
    c:char;
begin
      result:='';
      s  :='';

      for i:=1 to Length(ck) do begin
        c := ck[i];
        if IsUpper(c) then
                s:=s+RoyalTableChars[c];
        if IsDigit(c) then
                s:=s+RoyalTable[c];
      end;
      Result:=s;
end;

function  TpsBarcodeComponent.Royalmail(ck:string):string;
var s:string;
begin
        s:=CalcRoyal(ck);
        {checksum}
        s:=s+CalcRoyalCheckSum(s);

        Result:='H0'+RoyalExpand(s)+'V';
end;

function  TpsBarcodeComponent.State4(ck:string):string;
var s:String;
begin
        s:=CalcRoyal(ck);
        Result:='H0'+RoyalExpand(s)+'V';
end;


function  TpsBarcodeComponent.SingaporeCode(ck:string):string;
var s:string;
begin
        s:=CalcRoyal(ck);
        {checksum}
        s:=s+CalcRoyalCheckSum(s);
        Result:='H0'+RoyalExpand(s)+{H}'V';
end;

function TpsBarcodeComponent.SolveGS1: String;
var err:TpsErrRecord;
begin
  {$ifdef PSOFT_PROF}
      err.Err := erOK;
      Result  := Params.GS1.SolveGS1(Barcode, err);
      if err.Err<>erOK then
            BarcodeRaise(err);
  {$else}
      Result := Barcode;
  {$endif}
end;

function  TpsBarcodeComponent.Dutch4StatePostal(ck:string):string;
begin
        Result:=RoyalExpand(CalcRoyal(ck));
end;

function  TpsBarcodeComponent.PlanetCode(ck:string):string;
var i:integer;
begin
        ck:=ck+Modulo10(ck);
        Result:='0';
        for i:=1 to length(ck) do
                result:=result+PostNetTable[ck[i]];
        Result:=Result+'0';
        for i:=1 to Length(Result) do
                if Result[i]='1' then Result[i]:='0'
                else                  Result[i]:='1';
        Result:=CorrectHight(Result);
end;



function  TpsBarcodeComponent.PostBar(ck:string):string;
begin

end;

function  TpsBarcodeComponent.SwissPostal(ck:string):string;
begin

end;




function TpsBarcodeComponent.Codabar(ck:string):String;
var s   :string;
    i   :integer;
    suma:integer;
    mn  :string;
begin
     mn:=GetSetOfChars;

     if boAutoCheckDigit in Options then begin
        suma:=0;
        for i:=1 to Length(ck) do
           suma:=suma+Pos(ck[i],mn)-1;
        ck:=ck+mn[(suma mod 16)+1];
     end;


     Result:=co_start;
     Ofs[1]:=Length(result)+1;
     for i:=1 to Length(ck) do begin
         case ck[i] of
            '0': s:=co_0;
            '1': s:=co_1;
            '2': s:=co_2;
            '3': s:=co_3;
            '4': s:=co_4;
            '5': s:=co_5;
            '6': s:=co_6;
            '7': s:=co_7;
            '8': s:=co_8;
            '9': s:=co_9;
            '-': s:=co_minus;
            '$': s:=co_dolar;
            ':': s:=co_dvojb;
            '/': s:=co_lom;
            '.': s:=co_bodka;
            '+': s:=co_plus;
            'A': s:=co_A;
            'B': s:=co_B;
            'C': s:=co_C;
            'D': s:=co_D;
         end;
         Result:=Result+s;
     end;
     Ofs[2]:=Length(result)-1;
     Result:=Result+co_stop;
end;


function TpsBarcodeComponent.Code39Standard(const ck:string):string;
var s:string;
    i:integer;
begin
     Result:='2002022022020';
     Ofs[1]:=Length(result)+1;

     for i:=1 to Length(ck) do begin
         case ck[i] of
             '0': s:='101001101101';
             '1': s:='110100101011';
             '2': s:='101100101011';
             '3': s:='110110010101';
             '4': s:='101001101011';
             '5': s:='110100110101';
             '6': s:='101100110101';
             '7': s:='101001011011';
             '8': s:='110100101101';
             '9': s:='101100101101';
             'A': s:='110101001011';
             'B': s:='101101001011';
             'C': s:='110110100101';
             'D': s:='101011001011';
             'E': s:='110101100101';
             'F': s:='101101100101';
             'G': s:='101010011011';
             'H': s:='110101001101';
             'I': s:='101101001101';
             'J': s:='101011001101';
             'K': s:='110101010011';
             'L': s:='101101010011';
             'M': s:='110110101001';
             'N': s:='101011010011';
             'O': s:='110101101001';
             'P': s:='101101101001';
             'Q': s:='101010110011';
             'R': s:='110101011001';
             'S': s:='101101011001';
             'T': s:='101011011001';
             'U': s:='110010101011';
             'V': s:='100110101011';
             'W': s:='110011010101';
             'X': s:='100101101011';
             'Y': s:='110010110101';
             'Z': s:='100110110101';
             '-': s:='100101011011';
             '.': s:='110010101101';
             ' ': s:='100110101101';
             '*': s:='100101101101';
             '$': s:='100100100101';
             '/': s:='100100101001';
             '+': s:='100101001001';
             '%': s:='101001001001';
         end;
         Result:=Result+s+'0';
     end;
     Ofs[2]:=Length(result)-1;
     Result:=Result+'200202202202';
end;

function TpsBarcodeComponent.Code39Full(const ck:string):string;
var s:string;
    I:INTEGER;
begin
     s:='*+$*';
     for i:=1 to Length(ck) do
         case ck[i] of
              #0 : s:=s+'%U';
              #1 : s:=s+'$A';
              #2 : s:=s+'$B';
              #3 : s:=s+'$C';
              #4 : s:=s+'$D';
              #5 : s:=s+'$E';
              #6 : s:=s+'$F';
              #7 : s:=s+'$G';
              #8 : s:=s+'$H';
              #9 : s:=s+'$I';
              #10: s:=s+'$J';
              #11: s:=s+'$K';
              #12: s:=s+'$L';
              #13: s:=s+'$M';
              #14: s:=s+'$N';
              #15: s:=s+'$O';
              #16: s:=s+'$P';
              #17: s:=s+'$Q';
              #18: s:=s+'$R';
              #19: s:=s+'$S';
              #20: s:=s+'$T';
              #21: s:=s+'$U';
              #22: s:=s+'$V';
              #23: s:=s+'$W';
              #24: s:=s+'$X';
              #25: s:=s+'$Y';
              #26: s:=s+'$Z';
              #27: s:=s+'%A';
              #28: s:=s+'%B';
              #29: s:=s+'%C';
              #30: s:=s+'%D';
              #31: s:=s+'%E';
              ' ': s:=s+' ';
              '!': s:=s+'/A';
              '"': s:=s+'/B';
              '#': s:=s+'/C';
              '$': s:=s+'/D';
              '%': s:=s+'/E';
              '&': s:=s+'/F';
              '''': s:=s+'/G';
              '(': s:=s+'/H';
              ')': s:=s+'/I';
              '*': s:=s+'/J';
              '+': s:=s+'/K';
              ',': s:=s+'/L';
              '-': s:=s+'-';
              '.': s:=s+'.';
              '/': s:=s+'/O';
              '0': s:=s+'0';
              '1': s:=s+'1';
              '2': s:=s+'2';
              '3': s:=s+'3';
              '4': s:=s+'4';
              '5': s:=s+'5';
              '6': s:=s+'6';
              '7': s:=s+'7';
              '8': s:=s+'8';
              '9': s:=s+'9';
              ':': s:=s+'/';
              ';': s:=s+'%F';
              '<': s:=s+'%G';
              '=': s:=s+'%H';
              '>': s:=s+'%I';
              '?': s:=s+'%J';
              '@': s:=s+'%V';
              'A'..'Z' : S:=S+CK[I];
              '[': s:=s+'%K';
              '\': s:=s+'%L';
              ']': s:=s+'%M';
              '^': s:=s+'%N';
              '_': s:=s+'%O';
              '`': s:=s+'%W';
              'a'..'z': s:=s+ '+' +UpCase(ck[i]);
              '{': s:=s+'%P';
              '|': s:=s+'%Q';
              '}': s:=s+'%R';
              '~': s:=s+'%S';
              #128:s:=s+'%T';
         end;
     result:=Code39Standard(s);
end;

type
    tblNumeric = array['0'..'9'] of string;

function MakeSimpleBarcode(const start, body, stop:string;
    tbl:tblNumeric; var h:TpsOffsets):String;
var i:Integer;
begin
  Result := start;
  H[1]     := Length(result)+1;

  for i:=1 to Length(body) do
    Result:=Result+tbl[body[i]];

  H[2]     := Length(result)-1;
  Result := Result + Stop;
end;


function TpsBarcodeComponent.Datalogic(const ck:string):String;
const tblDatalogic : tblNumeric=(
  '10110010', '11010110', '10010110', '11001010', '10110110',
  '11011010', '10011010', '10100110', '11010010', '10010010');
begin
    Result := MakeSimpleBarcode('2020', ck, '2202', tblDatalogic, Ofs);
end;

function TpsBarcodeComponent.Matrix(const ck:string):String;
const tblMatrix : tblNumeric=(
  '10110010', '11010110', '10010110', '11001010', '10110110',
  '11011010', '10011010', '10100110', '11010010', '10010010');
begin
    Result := MakeSimpleBarcode('22202020', ck, '2220202', tblMatrix, Ofs);
end;

function TpsBarcodeComponent.Coop(ck:string):String;
const tblCoop : tblNumeric=(
  '11001010', '10100110', '10110110', '10110010', '10010110',
  '10010010', '10011010', '11010110', '11010010', '11011010');
begin
    Result := MakeSimpleBarcode('220220', ck, '20022', tblCoop, Ofs);
end;

function TpsBarcodeComponent.Industrial(ck:string):String;
const tblIndustrial : tblNumeric=(
  '101011011010', '110101010110', '101101010110', '110110101010', '101011010110',
  '110101101010', '101101101010', '101010110110', '110101011010', '101101011010');
begin
    Result := MakeSimpleBarcode('22022020', ck, '2202022', tblIndustrial, Ofs);
end;


function TpsBarcodeComponent.Code11(ck:string):String;
var s,m   : string;
    i,j,k : integer;
begin

     m:=GetSetOfChars;

     // first check digit
     s:=ck;
     j:=0;
     k:=Length(s);
     for i:=1 to k do
         Inc(j, (k-i+1)*(Pos(s[i],m)-1));
     s:=s+m[(j mod 11)+1];

     // second check digit
     k:=Length(s);
     j:=0;
     for i:=1 to k do
         Inc(j, (k-i+1)*(Pos(s[i],m)-1));
     s:=s+m[(j mod 11)+1];

     Result:='20220020';
     Ofs[1]:=Length(result)+1;
     for i:=1 to Length(s) do begin
         case s[i] of
             '0': s:='1010110';
             '1': s:='11010110';
             '2': s:='10010110';
             '3': s:='11001010';
             '4': s:='10110110';
             '5': s:='11011010';
             '6': s:='10011010';
             '7': s:='10100110';
             '8': s:='11010010';
             '9': s:='1101010';
             '-': s:='1011010';
         end;
         Result:=Result+s;
     end;
     Ofs[2]:=Length(Result)-1;
     Result:=Result+'2022002';
end;



function TpsBarcodeComponent.Interleaved(ck:string):String;
const def   : string='00110 10001 01001 11000 00101 10100 01100 00011 10010 01010';
      _space: String='          ';
var s     : string;
    i,j   : integer;
    s1,s2 : Byte;
    n1,n2 : byte;
begin
     s      := '';
     result := '';

     if (Length(ck) mod 2)<>0 then begin
       if not (boAddLeadingZero in Options) then
          Exit;
       ck := '0' + ck;
     end;

     for i:=1 to (Length(ck)div 2) do begin
       s1:=Ord(ck[2*i-1]);
       s2:=Ord(ck[2*i]);
       for j:=1 to 5 do begin
           n1:=1+Ord(def[6*(Ord(s1)-Ord0)+j])-Ord0;
           n2:=1+Ord(def[6*(Ord(s2)-Ord0)+j])-Ord0;

           if n1=1 then s:=s+'1'
           else         s:=s+'11';

           if n2=1 then s:=s+'0'
           else         s:=s+'00';
       end;
       Result:=_space+'1010';
       Ofs[1]:=Length(result)+1;
       Result:=Result+s;
       Ofs[2]:=Length(result)-1;
       Result:=Result+'1101'+_space;
    end;
end;


function TpsBarcodeComponent.Code93Standard(ck:string):string;
var i,j,k : integer;
    s     : String;
    suma  : integer;
    mn    : string;
    procedure AddOneCheckDigit;
    begin
        i:=Length(ck);
        j:=1;
        suma:=0;
        while i>0 do begin
           k:=Pos(ck[i],mn)-1;
           suma:=suma+j*k;
           Inc(j);
           if j>20 then j:=1;
           Dec(i);
        end;
        ck:=ck+mn[(suma mod 47) +1];
    end;
begin
     Result:=c93_start;
     Ofs[1]:=Length(result)+1;
     mn:=GetSetOfChars;

     if boAutoCheckDigit in Options then begin
        AddOneCheckDigit;
     end;

     j:=1;
     i:=Length(ck);
     suma:=0;
     while i>0 do begin
           k:=Pos(ck[i],mn)-1;
           suma:=suma+j*k;
           Inc(j);
           if j>15 then j:=1;
           Dec(i);
     end;
     ck:=ck+mn[(suma mod 47)+1];

     for i:=1 to Length(ck) do begin
         case ck[i] of
             '0': s:=c93_0;
             '1': s:=c93_1;
             '2': s:=c93_2;
             '3': s:=c93_3;
             '4': s:=c93_4;
             '5': s:=c93_5;
             '6': s:=c93_6;
             '7': s:=c93_7;
             '8': s:=c93_8;
             '9': s:=c93_9;
             'A': s:=c93_A;
             'B': s:=c93_B;
             'C': s:=c93_C;
             'D': s:=c93_D;
             'E': s:=c93_E;
             'F': s:=c93_F;
             'G': s:=c93_G;
             'H': s:=c93_H;
             'I': s:=c93_I;
             'J': s:=c93_J;
             'K': s:=c93_K;
             'L': s:=c93_L;
             'M': s:=c93_M;
             'N': s:=c93_N;
             'O': s:=c93_O;
             'P': s:=c93_P;
             'Q': s:=c93_Q;
             'R': s:=c93_R;
             'S': s:=c93_S;
             'T': s:=c93_T;
             'U': s:=c93_U;
             'V': s:=c93_V;
             'W': s:=c93_W;
             'X': s:=c93_X;
             'Y': s:=c93_Y;
             'Z': s:=c93_Z;
             '-': s:=c93_minus;
             '.': s:=c93_Bodka;
             ' ': s:=c93_space;
             '$': s:=c93_dolar;
             '/': s:=c93_lom;
             '+': s:=c93_plus;
             '%': s:=c93_perc;
             '&': s:=c93_sp1;
             '"': s:=c93_sp2;
             '(': s:=c93_sp3;
             ')': s:=c93_sp4;
         end;
         Result:=Result+s;
     end;
     Ofs[2]:=Length(result)-1;
     Result:=Result+c93_stop;
end;

function TpsBarcodeComponent.Code93Full(ck:string):string;
var i:integer;
    s:String;
begin
     Result:=c93_start;
     Ofs[1]:=Length(result)+1;

     for i:=1 to Length(ck) do begin
         case ck[i] of
             ' ': s:=c93_space;
             '!': s:=c93_r_lom+c93_A;
             '"': s:=c93_r_lom+c93_B;
             '#': s:=c93_r_lom+c93_C;
             '$': s:=c93_dolar;
             '%': s:=c93_perc;
             '&': s:=c93_r_lom+c93_F;
             '''': s:=c93_r_lom+c93_G;
             '(': s:=c93_r_lom+c93_H;
             ')': s:=c93_r_lom+c93_I;
             '*': s:=c93_r_lom+c93_J;
             '+': s:=c93_plus;
             ',': s:=c93_r_lom+c93_L;
             '-': s:=c93_minus;
             '.': s:=c93_Bodka;
             '/': s:=c93_lom;

             '0': s:=c93_0;
             '1': s:=c93_1;
             '2': s:=c93_2;
             '3': s:=c93_3;
             '4': s:=c93_4;
             '5': s:=c93_5;
             '6': s:=c93_6;
             '7': s:=c93_7;
             '8': s:=c93_8;
             '9': s:=c93_9;

             ':': s:=c93_r_lom +c93_Z;
             ';': s:=c93_r_perc+c93_F;
             '<': s:=c93_r_perc+c93_G;
             '=': s:=c93_r_perc+c93_H;
             '>': s:=c93_r_perc+c93_I;
             '?': s:=c93_r_perc+c93_J;
             '@': s:=c93_r_perc+c93_V;

             'A': s:=c93_A;
             'B': s:=c93_B;
             'C': s:=c93_C;
             'D': s:=c93_D;
             'E': s:=c93_E;
             'F': s:=c93_F;
             'G': s:=c93_G;
             'H': s:=c93_H;
             'I': s:=c93_I;
             'J': s:=c93_J;
             'K': s:=c93_K;
             'L': s:=c93_L;
             'M': s:=c93_M;
             'N': s:=c93_N;
             'O': s:=c93_O;
             'P': s:=c93_P;
             'Q': s:=c93_Q;
             'R': s:=c93_R;
             'S': s:=c93_S;
             'T': s:=c93_T;
             'U': s:=c93_U;
             'V': s:=c93_V;
             'W': s:=c93_W;
             'X': s:=c93_X;
             'Y': s:=c93_Y;
             'Z': s:=c93_Z;

             '[': s:=c93_r_perc+c93_K;
             '\': s:=c93_r_perc+c93_L;
             ']': s:=c93_r_perc+c93_M;
             '^': s:=c93_r_perc+c93_N;
             '_': s:=c93_r_perc+c93_O;
             '`': s:=c93_r_perc+c93_W;

             'a': s:=c93_r_plus+c93_A;
             'b': s:=c93_r_plus+c93_B;
             'c': s:=c93_r_plus+c93_C;
             'd': s:=c93_r_plus+c93_D;
             'e': s:=c93_r_plus+c93_E;
             'f': s:=c93_r_plus+c93_F;
             'g': s:=c93_r_plus+c93_G;
             'h': s:=c93_r_plus+c93_H;
             'i': s:=c93_r_plus+c93_I;
             'j': s:=c93_r_plus+c93_J;
             'k': s:=c93_r_plus+c93_K;
             'l': s:=c93_r_plus+c93_L;
             'm': s:=c93_r_plus+c93_M;
             'n': s:=c93_r_plus+c93_N;
             'o': s:=c93_r_plus+c93_O;
             'p': s:=c93_r_plus+c93_P;
             'q': s:=c93_r_plus+c93_Q;
             'r': s:=c93_r_plus+c93_R;
             's': s:=c93_r_plus+c93_S;
             't': s:=c93_r_plus+c93_T;
             'u': s:=c93_r_plus+c93_U;
             'v': s:=c93_r_plus+c93_V;
             'w': s:=c93_r_plus+c93_W;
             'x': s:=c93_r_plus+c93_X;
             'y': s:=c93_r_plus+c93_Y;
             'z': s:=c93_r_plus+c93_Z;

             '{': s:=c93_r_perc+c93_P;
             '|': s:=c93_r_perc+c93_Q;
             '}': s:=c93_r_perc+c93_R;
             '~': s:=c93_r_perc+c93_S;

         end;
         Result:=Result+s;
     end;
     Ofs[2]:=Length(result)-1;
     Result:=Result+c93_stop;
end;


        function Have4OrMoreDigits(const s:string; index:integer):integer;
        var i:integer;
        begin
             result := 0;
             i      := index;
             while ((i<=Length(s)) and IsDigit(s[i]) ) do begin
                   Inc(Result);
                   Inc(i);
             end;
             if result<4 then Result := 0;
        end;

        function LowerAfterControl(const s:String; index:integer):Boolean;
        begin
             Result:=False;
             if index<Length(s) then
                Result := IsControl(s[index]) and IsLower(s[index+1]);
        end;

        function ControlLowerControl(const s:String; index:integer):Boolean;
        begin
             Result:=False;
             if index<=Length(s)-2 then
                Result := IsControl(s[index]) and
                  IsLOwer(s[index+1]) and
                    IsControl(s[index+2]);
        end;


function TpsBarcodeComponent.Compress128(s:String):string;
var i,j:integer;
AktTyp:Char;

    function SwitchToCharSet(c:Char):string;
    begin
        result:='';
        case c of
            #0..#31 :
                case AktTyp of
                  'A' : ;
                  'B' : result := Code128Shift;
                  'C' : begin
                      result := Code128CodeB+Code128Shift;
                      AktTyp := 'B';
                    end;
                end;
            ' '..'_' :
                case AktTyp of
                    'A' : ;
                    'B' : ;
                    'C' : begin
                        AktTyp :='B';
                        result   := Code128CodeB;
                    end;
                end;
            '`' .. '~' :
                case AktTyp of
                    'A','C' : begin
                        AktTyp :='B';
                        result := Code128CodeB;
                      end;
                    'B' : ;
                end;
            end;
    end;  // end of SwitchToCharSet

    function ConvertChar(c:Char):Char;
    begin
        if Ord(c) in [0..31] then Result := Chr(64+Ord(c))
        else                      Result := Chr(Ord(c)-32);
    end;

begin
    Result:='';

    // s:='A'#9'b'#9'C';
    if s='' then Exit;
    i:=1;

    // step 1.
    j:=Have4OrMoreDigits(s,1);
    if j>=4 then begin
        Result := Code128StartC;
        AktTyp := 'C';

        while j>1 do begin
            Result:=result+Chr( StrToInt(Copy(s,i,2)) );
            Inc(i,2);
            Dec(j,2);
        end;
        if j=1 then begin
            Result:=Result+SwitchToCharSet(s[i])+ConvertChar(s[i]);
            Inc(i);
        end;
    end else
      if LowerAfterControl(s,1) then begin
          Result := Code128StartA+ConvertChar(s[1])+Code128CodeB+ConvertChar(s[2]);
          AktTyp := 'B';
          Inc(i,2);
      end else begin
          Result := Code128StartB;
          AktTyp := 'B';
      end;

      // main loop
      while i<=Length(s) do begin
          j:=Have4OrMoreDigits(s,i);

          // switch to code C and compress
          if j>=4 then begin
              // step 3.
              if AktTyp<>'C' then begin
                  if (j mod 2)=0 then
                    Result:=Result+Code128CodeC
                  else begin
                    Result:=Result+SwitchToCharSet(s[i])+ConvertChar(s[i])+Code128CodeC;
                  Inc(i);
                  Dec(j);
              end;
              AktTyp:='C';
          end;

          while j>0 do begin
              Result:=result+Chr( StrToInt(Copy(s,i,2)) );
              Inc(i,2);
              Dec(j,2);
          end;
      end
      else
          // step 4.
          if (AktTyp='B') and (i<=Length(s)) and (IsControl(s[i]))  then begin
              if ControlLowerControl(s,i) then begin
                  Result := Result + Code128Shift + ConvertChar(s[i])
                                   + ConvertChar(s[i+1])
                                   + Code128Shift + ConvertChar(s[i+2]);
                  Inc(i,3);
              end else begin
                  Result := Result + Code128CodeA + ConvertChar(s[i]);
                  Inc(i,1);
                  AktTyp := 'A';
              end;
          end
          else
            // step 5.
              if (AktTyp='A') and (i<=Length(s)) and IsLower(s[i]) then begin
                  if (i+2<=Length(s)) and (IsControl(s[i+1])) and IsLower(s[i+2]) then begin
                      Result := Result + Code128Shift + ConvertChar(s[i])
                              + ConvertChar(s[i+1])
                              + Code128Shift + ConvertChar(s[i+2]);
                      Inc(i,3);
                  end else begin
                      Result := Result + Code128Shift + ConvertChar(s[i]);
                      Inc(i,1);
                  end;
              end
              else
                 // step 6.
                 if (AktTyp='C') and (i<=Length(s)) and (not IsDigit(s[i])) then begin
                    if LowerAfterControl(s,i) then begin
                       AktTyp := 'A';
                       Result := Result+Code128CodeA+ConvertChar(s[i])+Code128Shift + ConvertChar(s[i]);
                    end else begin
                       AktTyp := 'B';
                       Result := Result+Code128CodeB;
                    end;
                 end
            else begin
                if i<=Length(s) then begin
                    Result:=Result+SwitchToCharSet(s[i])+ConvertChar(s[i]);
                    Inc(i);
                end;
            end;
      end;
end;

function TpsBarcodeComponent.Code128(const ck:string):string;
var i   : integer;
    tmp : string;
begin
     result := '';
     tmp    := Compress128(ck);
     if tmp ='' then
          Exit;
     if boAutoCheckDigit in Options then
        tmp:=tmp+Mod103CheckDigit(tmp);
     for i:=1 to Length(tmp) do
         result:=result+Code128Table[Ord(tmp[i])]+'0';
     result:=result+CODE128_STOP_ALL;
     Ofs[1]:=12;
     Ofs[2]:=11*Length(tmp)-1;
end;




function TpsBarcodeComponent.AbcCodabar(const ck:string):string;
var s:string;
    i:integer;
begin
     for i:=1 to Length(ck) do begin
         case ck[i] of
            ' ': s:='000000000000';
            '0': s:=co_0;
            '1': s:=co_1;
            '2': s:=co_2;
            '3': s:=co_3;
            '4': s:=co_4;
            '5': s:=co_5;
            '6': s:=co_6;
            '7': s:=co_7;
            '8': s:=co_8;
            '9': s:=co_9;
            '-': s:=co_minus;
            '$': s:=co_dolar;
            ':': s:=co_dvojb;
            '/': s:=co_lom;
            '.': s:=co_bodka;
            '+': s:=co_plus;
            'A': s:=co_A;
            'B': s:=co_B;
            'C': s:=co_C;
            'D': s:=co_D;
         end;
         Ofs[1]:=1;
         Result:=Result+s;
         Ofs[2]:=Length(result)-1;
     end;
end;

function TpsBarcodeComponent.Iata(ck:string):string;
var s:string;
    i:integer;
begin
     Result:='2020';
     Ofs[1]:=Length(result)+1;

     for i:=1 to Length(ck) do begin
         case ck[i] of
           '0': s:='10101110111010';
           '1': s:='11101010101110';
           '2': s:='10111010101110';
           '3': s:='11101110101010';
           '4': s:='10101110101110';
           '5': s:='11101011101010';
           '6': s:='10111011101010';
           '7': s:='10101011101110';
           '8': s:='11101010111010';
           '9': s:='10111010111010';
         end;
         Result:=Result+s;
     end;
     Ofs[2]:=Length(result)-1;
     Result:=Result+'22202';
end;


function TpsBarcodeComponent.Invert(ck:string):string;
var s:string;
    i:integer;
begin
     Result:='20002000202';
     Ofs[1]:=Length(result)+1;

     for i:=1 to Length(ck) do begin
         case ck[i] of
           '0': s:='01010001000101';
           '1': s:='00010101010001';
           '2': s:='01000101010001';
           '3': s:='00010001010101';
           '4': s:='01010001010001';
           '5': s:='00010100010101';
           '6': s:='01000100010101';
           '7': s:='01010100010001';
           '8': s:='00010101000101';
           '9': s:='01000101000101';
         end;
         Result:=Result+s;
     end;
     Ofs[2]:=Length(result)-1;
     Result:=Result+'0002020002';
end;

function  TpsBarcodeComponent.PZN(ck:String):String;
var s:String;
begin
    s:='-'+ck;
    Result := Code39Standard(s);
end;


function  TpsBarcodeComponent.Monarch(ck:String):String;
const table:array [0..23] of string=(
    {0} '101010011',
    {1} '101011001',
    {2} '101001011',
    {3} '110010101',
    {4} '101101001',
    {5} '110101001',
    {6} '100101011',
    {7} '100101101',
    {8} '100110101',
    {9} '110100101',
    {-} '101001101',
    { $} '101100101',
    {:} '1101011011',
    {/} '1101101011',
    {.} '1101101101',
    {+} '1011011011',
    {a} '1011001001',
    {b} '1001001011',
    {c} '1010010011',
    {d} '1010011001',
    {t} '1011001001',
    {n} '1001001011',
    {*} '1010010011',
    {e} '1010011001');
var i:integer;
    s:string;
begin
        result:='';
        for i:=1 to length(ck) do begin
                case ck[i] of
                        '0'..'9':s:=table[Ord(ck[i])-Ord0];
                        '-'     :s:=table[10];
                        '$'     :s:=table[11];
                        ':'     :s:=table[12];
                        '/'     :s:=table[13];
                        '.'     :s:=table[14];
                        '+'     :s:=table[15];
                        'a','A' :s:=table[16];
                        'b','B' :s:=table[17];
                        'c','C' :s:=table[18];
                        'd','D' :s:=table[19];
                        't','T' :s:=table[20];
                        'n','N' :s:=table[21];
                        '*'     :s:=table[22];
                        'e','E' :s:=table[23];
                end;
                result:=result+s+'0';
        end;
end;


function TpsBarcodeComponent.GetBarCodeLines(BC:String):String;
var s,pr : string;
    i    : Integer;
    {$ifdef PSOFT_PROF}
      outBarcode  : string;
    {$endif}
    procedure MoveRight;
    var k :Integer;
    begin
         if CaptionHuman.Visible then begin
            s:=pr+s;
            for k := 1 to 6 do
               Inc(Ofs[k],Length(pr));
         end;
    end;
begin
     s:='';
     pr:='        ';
     for i := 1 to 6 do
        Ofs[i]:=0;

     // BC := psReplaceHexa(BC);
     // ShowMessagePos(BC,0,0);
     FBars.Clear;
     if not CheckBarCode(BC) then Exit;

     { FBarCode:=BC; }

     case FBarcodeSymbology of
        bcEan8,bcJAN8      : s:=Ean8(BarCode);
        bcEan13,bcISBN, bcISSN, bcISMN, bcJAN13 : begin
                                 if FBarcodeSymbology<>bcEAN13 then
                                    s:=Ean13(BooklandToEAN(BC,FBarcodeSymbology))
                                 else
                                    s:=Ean13(BC);
                                 MoveRight;
                           end;
        bcCodabar        : s:=Codabar(BarCode);
        bcCode39Standard : s:=Code39Standard(BarCode);
        bcCode39Full     : s:=Code39Full(BarCode);
        bcCode93Standard : s:=Code93Standard(BarCode);
        bcCode93Full     : s:=Code93Full(BarCode);
        bcCode128        : s:=Code128(BarCode);
        bcUccEan128      : s:=UccEan128(BarCode);
        bcAbcCodabar     : s:=AbcCodabar(BarCode);
        bc25Datalogic    : s:=Datalogic(BarCode);
        bc25Interleaved  : s:=Interleaved(BarCode);
        bc25Matrix       : s:=Matrix(BarCode);
        bc25Industrial   : s:=Industrial(BarCode);
        bc25Iata         : s:=Iata(BarCode);
        bc25Invert       : s:=Invert(BarCode);
        bcITF            : s:=Interleaved(BarCode);
        bcUPCA           : begin
                                s:=UPCA(BarCode);
                                MoveRight;
                           end;
        bcUPCE0,bcUPCE1  : begin
                                s:=UPCE(BarCode);
                                MoveRight;
                           end;
        bcUPCShipping    : begin
                         i := 0;
                         if Copy(Barcode,16,6)<>'' then begin
                            s := Interleaved(Copy(Barcode,16,6));
                            s[1]:='X';
                            for i:=1 to Length(s) do
                                if s[i]='2' then s[i]:='1';
                            i:=Length(s);
                            s:='         '+s;
                         end;

                         s  := Interleaved(Copy(Barcode,1,14)) + s;
                         Ofs[5] := Length(s) - i + 20;
                    end;
        {bcPlessey,bcMSI,bcADSPlessey,}
        bcMSIPlessey                    : s:=Plessey(BarCode);
        bcPostNet                       : s:=PostNet(BarCode);

        bcRoyalMail                     : s:=RoyalMail(BarCode);
        bcDutch4StatePostal             : s:=Dutch4StatePostal(BarCode);
        bc4State                        : s:=State4(BarCode);
        bcSingapore4StatePostalCode     : s:=SingaporeCode(Barcode);
        bcPlanet                        : s:=PlanetCode(Barcode);

        bcSwissPostal                   : s:=SwissPostal(Barcode);
        bcPostBar                       : s:=Postbar(Barcode);
        bcOPC                           : s:=Interleaved(Barcode);
        bc25Coop                        : s:=Coop(BarCode);
        bcCode11                        : s:=Code11(BarCode);
        bcPZN                           : s:=PZN(Barcode);
        bcCodabarMonarch                : s:= Monarch(Barcode);
        bcFim                           : s:= Fim(UpperCase(Copy(Barcode,1,1)));

        {$ifdef PSOFT_PROF}
              // bcPostbarCPC4State  : s:= CPC4State(BC);
              bcTelepen           : s:= Telepen(BC);
              //bcTelepenNumeric  : s:= TelepenNumeric(BC);
              bcIntelligentMail   : s:= IntelligentMailGetLines(BC);
              bcPostbarCPC4State  : s:= CPC4State(BC);

              bcAustraliaPost     : begin
                 outBarcode:=BC;
                 s:=AustraliaPostGetLines(outBarcode, True);
                 if CaptionUpper.AutoCaption then
                       CaptionUpper.Text.Text:= outBarcode;
                 BC:= outBarcode;
               end;

              // bcPDF417   ,
              // bcCode16K,
              // bcCodablockA,bcCodablockF,bcCodablock256 : Code2D.Encode;
              // bcChromoCode : EncodeChromocode(Self);
        {$endif}

        {$ifdef PSOFT_STUDIO}
            bcQRCode      : psQRCodeGetPixels(Self);
            bcDataMatrix  : psDataMatrixGetPixels(Self);
        {$endif}
     end;

     if (FParams.Ratio<>ra20) and BI.VariableRatio then
        s:=psExpandRatio(s,FParams.Ratio);
     FBars.Add(s);
     if Ofs[2]<=0 then
        Ofs[2] := Length(s)-1;
     result:=s;
end;


function TpsBarcodeComponent.GetBarCodeSymbology: TpsBarCodeSymbology;
begin
  Result := FBarcodeSymbology;
end;

function TpsBarcodeComponent.GetCaptionUpper: TpsBarcodeCaption;
begin
  Result :=FCaptionUpper;
end;

function TpsBarcodeComponent.GetErrorInfo: TpsBarcodeError;
begin
    Result := FErrorInfo;
end;

function TpsBarcodeComponent.GetCaptionHuman: TpsBarcodeCaption;
begin
  Result :=FCaptionHuman;
end;

function TpsBarcodeComponent.GetCaptionBottom: TpsBarcodeCaption;
begin
  Result :=FCaptionBottom;
end;


function TpsBarcodeComponent.GetHorzLines: TpsHorzLines;
begin
  Result := FHorzLines;
end;

function TpsBarcodeComponent.GetLastError: TpsBarcodeError;
begin
  Result := FErrorInfo;
end;

function TpsBarcodeComponent.GetLinesColor: TColor;
begin
  Result := FLinesColor;
end;

function TpsBarcodeComponent.GetOptions: TpsBarcodeOptions;
begin
  Result := FOptions;
end;

function TpsBarcodeComponent.GetParams: TpsParams;
begin
  Result := FParams;
end;

function TpsBarcodeComponent.GetQuietZone: TpsQuietZone;
begin
  Result:=FQuietZone;
end;


{
procedure TpsBarcodeComponent.Next;
var s   : string;
    mn  : string;
    i,j : integer;
    pz  : Char;
    ai_from,ai_to : Integer;
begin
    mn := GetSetOfChars;
    if mn='' then Exit;
    s  := BarCode;
    pz := mn[Length(mn)];

    ai_from:=FAutoIncFrom; if ai_from=0 then ai_from:=1;
    ai_to:=FAutoIncTo;
    if ai_to=0 then
       case BarcodeSymbology of
         bcEan8  : ai_to:=7;
         bcEan13 : ai_to:=12;
         else      ai_to:=Length(BarCode);
       end;

    s:=Copy(s,ai_from,ai_to-ai_from+1);

    i:=Length(s);
    repeat
          if s[i]=pz then begin
             s[i]:=mn[1];
             Dec(i);
          end else begin
             j:=Pos(s[i],mn);
             if j>0 then
                s[i]:=mn[j+1];
             Break;
          end;
    until False;

    s:=Copy(BarCode,1,ai_from-1)
      +s
      +Copy(BarCode,ai_to+1,1000);

    case BarcodeSymbology of
         bcEan8  : s:=Copy(s,1,7);
         bcEan13 : s:=Copy(s,1,12);
    end;

    if CheckBarCode(s) then BarCode:=s;
end;
}


procedure   TpsBarcodeComponent.SetBarCode(const Value:String);
begin
     if FBarCode<>Value then begin
        FBarCode:=Value;
        BarCodeComplete;
        FCaptionUpper.UpdateCaption;
        UpdateBarcode;
        if Assigned(FOnChangeBarCode) then
              FOnChangeBarCode(Self);
     end;
end;

//procedure   TpsBarcodeComponent.SetLabelMask(Value:String);
//begin
//     if FLabelMask<>Value then begin
//        FLabelMask:=Value;
//        UpdateBarcode;
//     end;
//end;


procedure   TpsBarcodeComponent.SetBarcodeSymbology(const Value:TpsBarcodeSymbology);
var BI:TpsBarCodeInfo;
begin
     if Value in [bcCode16k,bcCodablockF] then begin
        Exit; // Value:=bcEan13;
     end;

     if FBarcodeSymbology<>Value then begin
        FBarcodeSymbology := Value;
        BI          := BarcodeInfo(FBarcodeSymbology,Barcode);
        if not (boNoUpdateAfterSymbologyChange in Options) then begin
            BarCode     := BI.InitValue;
            // Caption.FAutoCaption := BI.AutoCaption;
        end;
        if Assigned(FOnChangeTypCode) then
            FOnChangeTypCode(Self);
        UpdateBarcode;
     end;
end;


procedure   TpsBarcodeComponent.SetLinesColor(const Value:TColor);
begin
     if FLinesColor<>Value then begin
        FLinesColor:=Value;
        UpdateBarcode;
     end;
end;

procedure   TpsBarcodeComponent.SetBackgroundColor(const Value:TColor);
begin
     if FBackgroundColor<>Value then begin
        FBackgroundColor:=Value;
        UpdateBarcode;
     end;
end;

procedure TpsBarcodeComponent.SetHorzLines(const Value: TpsHorzLines);
begin
  FHorzLines.Assign(Value);
end;

procedure TpsBarcodeComponent.SetlastError(Value: TpsBarcodeError);
begin
  FErrorInfo := Value;
end;

constructor TpsBarcodeComponent.Create(AOwner:TComponent);
begin
     inherited Create(AOwner);

     FOnUpdateBarcodeParent := nil;

     FErrorInfo          := TpsBarcodeError.CreateBC(Self);
     FBars               := TStringList.Create;
     {$ifdef PSOFT_STUDIO}
        FPixels             := TpsMatrix.Create;
     {$endif}
     FCaptionUpper       := TpsBarcodeCaption.CreateEAN(Self, cpUpper);
     FCaptionBottom      := TpsBarcodeCaption.CreateEAN(Self, cpBottom);
     FCaptionHuman       := TpsBarcodeCaption.CreateEAN(Self, cpHuman);
     FHorzLines          := TpsHorzLines.Create;
     FHorzLines.OnChange := HorzLinesChange;
     FQuietZone          := TpsQuietZone.CreateEan(Self);
     FParams             := TpsParams.Create(Self);

     FOnChangeTypCode := nil;
     FOnChangeBarCode := nil;
     FOptions         := psGlobalBarcodeOptions;
     FbackGroundColor := clWhite;
     FBarCode         := '9771210107001';
     FBarcodeSymbology:= bcEan13;
     FLinesColor      := clBlack;
     FFontAutoSize    := True;
     FAngle           := 0;
end;

constructor TpsBarcodeComponent.CreateFromControl(AOwner:TComponent;
      UpdateProc:TNotifyEvent);
begin
    Create(AOwner);
    FOnUpdateBarcodeParent := UpdateProc;
end;

destructor TpsBarcodeComponent.Destroy;
begin
     FParams.Free;
     FHorzLines.Free;
     FCaptionHuman.Free;
     FCaptionUpper.Free;
     FCaptionBottom.Free;
     FQuietZone.Free;
     FBars.Free;
     {$ifdef PSOFT_STUDIO}
       FPixels.Free;
     {$endif}
     FErrorInfo.Free;

     Inherited Destroy;
end;

procedure TpsBarcodeComponent.DoUpdate;
begin
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

procedure TpsBarcodeComponent.HorzLinesChange(Sender:TObject);
begin
     UpdateBarcode;
end;


function TpsBarcodeComponent.Zoom(ABarCode:string; AWidth:integer; ASecurity:Boolean):string;
var WidthCount     : integer;
    LineWidth      : integer;
    i,j            : integer;
    x1,x2,x3,x4,x5,x6 : integer;
begin
      result      := '';
      if ABarCode  = '' then Exit;

      if ASecurity then begin
         result:='';
         LineWidth := AWidth div Length(ABarCode);
         if LineWidth=0 then begin
            BarcodeRaise(erOutOfSecurityBox);
            Exit;
         end;
         for i:=1 to Length(ABarCode) do begin
            for j:=1 to LineWidth do
                result:=result+ABarCode[i];
         end;
         Ofs[1]:=Ofs[1]*LineWidth;
         Ofs[2]:=Ofs[2]*LineWidth;
         Ofs[3]:=Ofs[3]*LineWidth;
         Ofs[4]:=Ofs[4]*LineWidth;
         Ofs[5]:=Ofs[5]*LineWidth;
         Ofs[6]:=Ofs[6]*LineWidth;
      end else BEGIN
         WidthCount  := 0;
         LineWidth   := Length(ABarCode);
         x1:=Ofs[1]; x2:=Ofs[2]; x3:=Ofs[3]; x4:=Ofs[4]; x5:=Ofs[5]; x6:=Ofs[6];

         for i:=1 to LineWidth   do begin
            Dec(WidthCount,AWidth);
            if i=x1 then Ofs[1]:=Length(Result);
            if i=x2 then Ofs[2]:=Length(Result);
            if i=x3 then Ofs[3]:=Length(Result);
            if i=x4 then Ofs[4]:=Length(Result);
            if i=x5 then Ofs[5]:=Length(Result);
            if i=x6 then Ofs[6]:=Length(Result);
            while(WidthCount<0) do begin
                Inc(WidthCount,LineWidth);
                result:=result+ABarCode[i];
            end;
         end;
      end;
end;


function TpsBarcodeComponent.MinWidth:Integer;
var s:string;
begin
     Result := 100;
     s      := FBarcode;
     GetBarcodeLines(s);
     if FBars.Count>0 then
          Result:=Length(FBars.Strings[0]);
end;

function TpsBarcodeComponent.MinHeight(DefHeightPercent:Integer=40): Integer;
var s  : string;
    BI : TpsBarcodeInfo;
begin
//     Result := 1;
     s      := FBarcode;
     GetBarcodeLines(s);
     case FBars.Count of
        0 : Result := 100;
        1 : Result := MulDiv(Length(FBars.Strings[0]), DefHeightPercent, 100) ;
        else begin
            BI:=BarcodeInfo(BarcodeSymbology);
            if BI.SymbolType=stMatrix then
                  Result := FBars.Count
            else
                  Result := 5*FBars.Count;
        end;
     end;
end;

function IAmDemo:Boolean;
begin
  {$ifdef PSOFT_DEMO}
      Result := True;
  {$else}
      Result := False;
  {$endif}
end;


procedure DrawDemo(C:TCanvas; R:TRect; rc:TPoint; A:Integer; bg:TColor);
var rp:TPoint;
begin

    Exit;

    C.Font.Color := clRed;
    RotateFont(C.Font, A);
    C.Brush.Color := bg;
    C.Brush.Style := bsSolid;

    rp := RotatePoint( Point((R.Left+R.Right) div 2, (R.Top+R.Bottom) div 2), rc,A);
    SetTextAlign(C.Handle, TA_CENTER);
    SetBkMode(C.Handle,OPAQUE);
    C.TextOut(rp.X,rp.Y,'>>Demo version<<');
end;


procedure PaintBarCode1(C:TCanvas; R:TRect; E:TpsBarcodeComponent; ABarCode:String; RC:TPoint);
const itf_width = 4;
var Y,i,j      : integer;
    S          : string;
    LinesCount : integer;
    dy1,dy2    : integer;
    FontHeight : integer;
    BarCodeLabel : String;
    W,_lc        : Integer;
    P            : TPen;
    LastChar     : Char;
    OfsBottom,h  : Integer;
    he           : Double;
    AddOn        : Boolean;

    procedure DrawEanText(T1,T2,T3:String);
    var i:Integer;
    begin
      with E do begin
         C.Brush.Style := bsClear;
         C.Brush.Color := clNone;
         i:=Ofs[2]-Ofs[1]-C.TextWidth(T1);
         DrawRotatedText(C, R.Left+Ofs[1]+ (i div 2), Y, rc,E.Angle, T1);
         i:=Ofs[4]-Ofs[3]-C.TextWidth(T2);
         DrawRotatedText(C, R.Left+Ofs[3]+ (i div 2),Y, rc,E.Angle, T2);
         if Length(T3)>=0 then begin
            if boAddOnUp in E.Options then DrawRotatedText(C, R.Left+Ofs[5],R.Top, rc,E.Angle,T3)
            else                 DrawRotatedText(C, R.Left+Ofs[5],Y, rc,E.Angle, T3);
         end;
      end;
    end;
begin
     LinesCount         := 1;

     s := E.FBars.Strings[0];

     if E.BarcodeSymbology in [bcISBN, bcISSN, bcISMN] then
        ABarCode:=BooklandToEAN(ABarCode,E.BarcodeSymbology);
     W   := WidthOf(R);
     _lc := 0;

     if (Length(s)>W) and not (boPaintIfSmall in E.Options) then
          E.BarcodeRaise(erSmallPaintBox);

     BarCodeLabel:=ABarcode;

     { TODO : dopracovat auto update }
     case E.BarcodeSymbology of
           bcPZN : BarcodeLabel := 'PZN - '+BarcodeLabel;
     end;

     // BarcodeLabel := RemoveControlChars(BarcodeLabel);

     { zoom to regular rectangle }
     if Length(s)>0 then
        s:=E.Zoom(s,W, boSecurity in E.Options);

     with C do begin
          // Font.Assign(E.Font);

          Pen.Width   := 1;
          if boReflectanceReversal in E.Options then
              Pen.Color   := E.BackgroundColor
          else
              Pen.Color   := E.LinesColor;

          Brush.Color := Pen.Color;
          Brush.Style := bsSolid;

          { Change font size, if needed }
          if E.CaptionHuman.AutoSize then begin
             case E.BarcodeSymbology of
                bcEan8,bcJAN8                   : i:=TextWidth(Copy(ABarCode,1,4));
                bcEan13, bcISBN, bcISSN, bcISMN,
                bcUpcE0,bcUpcE1,bcJAN13         : i:=TextWidth(Copy(ABarCode,2,6));
                bcUPCA                          : i:=TextWidth(Copy(ABarCode,2,5));
                bcUPCShipping                   : i:=TextWidth(ABarCode+'     ');
                bcOPC                           : i:=TextWidth(ABarCode+'--');
                bcPZN                           : i:=TextWidth(BarcodeLabel);
                else
                    i:=TextWidth(ABarCode);
             end;
             if (i>0) then
                  Font.Size:=Round(Int(0.95*Font.Size*(Ofs[2]-Ofs[1])/i));
          end;

          he := 100*C.Font.Size/(R.Bottom-R.Top);
          if (E.CaptionHuman.MaxHeight>0) and (he>E.CaptionHuman.MaxHeight) then
                C.Font.Size := Round(0.9*C.Font.Size*E.CaptionHuman.MaxHeight/he);
          FontHeight := C.TextHeight(ABarCode);

          dy1:=0; dy2:=0;
          OfsBottom := iif(E.CaptionHuman.Visible, FontHeight);

          LastChar:='0';
          if Length(s)>0 then begin
             s:=s+'0';
             h:=HeightOf(R)-OfsBottom;
             AddOn := False;
             for i:=1 to Length(s) do begin
                if not AddOn then
                      AddOn:=(LastChar='X');
                if (Pos(s[i],'X01 MVFSTHAD')>0) AND (LinesCount>0) then begin
                      case E.BarcodeSymbology of
                        bcPostnet,bcPlanet,bcRoyalMail,bcDutch4StatePostal,
                        bcSingapore4StatePostalCode, bc4State
                        {$ifdef PSOFT_PROF}
                          , bcIntelligentMail, bcAustraliaPost
                        {$endif}
                        // ,bcPostbarCPC4State
                          : begin
                            dy1:=0;
                            dy2:=0;
                            case LastChar of
                                'M'     : dy1 := h div 2;
                                'V','F' : ;
                                'H','A' : dy2 := h div 3;
                                'D'     : dy1 := h div 3;
                                'S','T':begin
                                        dy2 := h div 3;
                                        dy1 := h div 3;
                                    end;
                            end;
                            Inc(dy2,OfsBottom);
                        end
                      else begin
                        if (LastChar='2') and (boStartStopLines in E.Options) then
                          DY2 := iif(E.BarcodeSymbology in [bcUPCA,bcUpcE0, bcUpcE1], FontHeight div 2)
                        else
                          DY2 := iif(E.CaptionHuman.Visible or (LastChar='3'), FontHeight);

                        if AddOn then begin
                          if boAddOnUp in E.Options then
                            dy1 := FontHeight;
                        end;
                      end;
                 end;

                 if LastChar<>'0' then
                   DrawRotatedRect( C,
                      Rect(R.Left+i-LinesCount-1,R.Top+dy1,R.Left+i-1,R.Bottom-dy2),
                      RC,
                      E.Angle);
                 if _lc=0 then _lc := LinesCount;
                 LinesCount:=0;
                end;

                if Pos(s[i],'123MHDVSATF')>0 then
                    Inc(LinesCount);
                LastChar:=s[i];
             end;
          end;

          i:=Length(s);
          if E.BarcodeSymbology in [bcITF] then begin

             // horizontal lines
             DrawRotatedRect(C, Rect(R.Left,R.Top,       R.Left+i-1,R.Top+itf_width*_lc),
                             RC,
                             E.Angle);
             DrawRotatedRect(C, Rect(R.Left,R.Bottom-dy2-itf_width*_lc,R.Left+i-1,R.Bottom-dy2),
                             RC,
                             E.Angle);
             // svertical lines
             DrawRotatedRect(C, Rect(R.Left,R.Top,       R.Left+itf_width*_lc,R.Bottom-dy2),
                             RC,
                             E.Angle);
             DrawRotatedRect(C, Rect(R.Left+i-1-itf_width*_lc, R.Top, R.Left+i-1, R.Bottom-dy2),
                             RC,
                             E.Angle);

          end;

          C.Brush.Color:=E.LinesColor;

          if E.CaptionHuman.Visible then begin
             C.Font.Assign(E.CaptionHuman.Font);
             if E.CaptionUpper.AutoSize then begin
                C.Font.Height := FontHeight;
             end;

             RotateFont(C.Font, E.Angle);

             { mask characters}

             {s:=E.LabelMask;
             for i:=1 to Length(BarCodeLabel) do
                 if (Copy(s,i,1)='') or (Copy(s,i,1)='_') then begin end
                 else BarCodeLabel[i]:=s[i];
             }

             Y := R.Bottom - FontHeight;

             C.Brush.Style := bsClear;
             C.Brush.Color := clNone;

             s:=BarCodeLabel;
             case E.BarcodeSymbology of
                      bcEan8,bcJAN8  : DrawEanText(Copy(s,1,4),
                                                   Copy(s,5,4),
                                                   Copy(s,10,5));
                      bcEan13,bcISBN, bcISSN, bcISMN,bcJAN13 : begin
                                     DrawRotatedText(C, R.Left, Y, rc,E.Angle, Copy(s,1,1));
                                     DrawEanText(Copy(s,2,6),
                                                 Copy(s,8,6),
                                                 Copy(s,15,5));
                                end;
                      bcUPCA : begin
                                     DrawRotatedText(C, R.Left, Y, rc,E.Angle, Copy(s,1,1));
                                     DrawEanText(Copy(s,2,5),
                                                 Copy(s,7,5),
                                                 Copy(s,14,5));
                                     DrawRotatedText(C, R.Left+Ofs[6], Y, rc,E.Angle, Copy(s,12,1));
                                end;
                      bcUPCE0,bcUPCE1 : begin
                                     DrawEanText(Copy(s,2,6),'',Copy(s,10,5));
                                     C.Font.Size := Round (C.Font.Size *0.75 );
                                     RotateFont(C.Font, E.Angle);
                                     DrawRotatedText(C, R.Left,    R.Bottom-C.TextHeight('1'), rc,E.Angle, Copy(s,1,1));
                                     DrawRotatedText(C, R.Left+Ofs[6], R.Bottom-C.TextHeight('1'), rc,E.Angle, Copy(s,8,1));
                                end;
                      bcUPCShipping : DrawEanText(Copy(s,1,1)
                                               +'  '+Copy(s, 2,2)
                                               +'  '+Copy(s, 4,5)
                                               +'  '+Copy(s, 9,5)
                                               +'  '+Copy(s,14,1),
                                               '',
                                               Copy(s,16,6));
                      bcOPC : begin
                                       s:=Copy(s, 1,5)+'-'
                                         +Copy(s, 6,4)+'-'
                                         +Copy(s,10,1);
                                       i:=Ofs[2]-Ofs[1]-C.TextWidth(s);
                                       DrawRotatedText(C, R.Left+Ofs[1]+(i div 2), Y, rc,E.Angle,s);
                               end;
                      {bcPostnet,bcPlanet,
                      bcSingapore4StatePostalCode,bcRoyalMail,
                      bcDutch4StatePostal,bc4State, bcIntelligentMail, bcAustraliaPost : ;
                      }
                      else begin
                         if Ofs[2]=0 then Ofs[2]:=R.Right-R.Left;
                         i:=Ofs[2]-Ofs[1]-C.TextWidth(s);
                         DrawRotatedText(C, R.Left+Ofs[1]+(i div 2), Y, rc,E.Angle,s);
                      end;
            end;
          end;

          // draw horizontal lines, if nedded
          if E.HorzLines.LinesCount>0 then begin
             P:=TPen.Create;
             with C do
             try
                P.Assign(Pen);
                Pen.Color := E.HorzLines.Color;
                Pen.Style := E.HorzLines.Style;
                Pen.Mode  := E.HorzLines.Mode;
                Pen.Width := E.HorzLines.Width;

                j:=(R.Bottom-R.Top) div (E.HorzLines.LinesCount + 1);
                for i:=1 TO E.HorzLines.LinesCount do begin
                        MoveTo(R.Left,R.Top+i*j);
                        LineTo(R.Right,R.Top+i*j);
                end;
             finally
                Pen.Assign(P);
                P.Free;
             end;
       end;
    end;
end;

procedure PaintBarCode(C:TCanvas; R:TRect; E:TpsBarcodeComponent);
begin
    if Assigned(E) then
        E.PaintBarcode(C,R);
end;

procedure PaintBarCodeHandle(HWnd:THandle; R:TRect; E:TpsBarcodeComponent);
var C:TCanvas;
begin
  C:=TCanvas.Create;
  try
    C.Handle:= HWnd;
    PaintBarcode(C,R,E);
  finally
    C.Free;
  end;
end;


procedure TpsBarcodeComponent.PaintBarCode(C: TCanvas; R: TRect);
var b_rec : TBrushRecall;
    p_rec : TPenRecall;
    f_rec : TFontRecall;
begin
    FErrorInfo.Init(C,R);
    b_rec := TBrushRecall.Create(C.Brush);
    f_rec := TFontRecall.Create(C.Font);
    p_rec := TPenRecall.Create(C.Pen);
    try
      try
        PaintBarcode2(C,R);
      except
        on psBarcodeException do
            FErrorInfo.HandleException(C,R);
        else
          raise;
      end;
    finally
      p_rec.Free;
      f_rec.Free;
      b_rec.Free;
    end;
end;

procedure TpsBarcodeComponent.PaintBarCode2(C:TCanvas; R:TRect);
var R1,R2: TRect;   { rotated rect - size}
    RC   : TPoint;  { Rotation center;}
    W,H  : Integer;
    A,B  : Integer;
    X1,X2,X3,X4,ta : Extended;
    i : Integer;
    s : String;
begin
     if Barcode='' then
        BarcodeRaise(erEmptyCode);

     if not BI.Supported then
        BarcodeRaise(erNotSupported);

     R2 := R;

     // draw background
     if not(boTransparent in Options) then
     begin
       if boReflectanceReversal in Options then
         C.Brush.Color := LinesColor
       else
         C.Brush.Color := BackgroundColor;

       C.Brush.Style := bsSolid;
       C.Pen.Color := C.Brush.Color;
       DrawRotatedRect(C, R, RC, Angle);
       SetBkMode(C.Handle, OPAQUE);
     end
     else
       SetBkMode(C.Handle, TRANSPARENT);

     RC.X := (R.Left+R.Right)  div 2;
     RC.Y := (R.Top +R.Bottom) div 2;

     R1 := R;
     W  := WidthOf(R);
     H  := HeightOf(R);
     ta := Tan(DegToRad(Angle));
     X2 := ( W-H*ta)/(1-sqr(ta));
     X4 :=  ta*X2;
     A  := Round(sqrt(sqr(X2)+sqr(X4)));
     X1 := W-X2;
     X3 := H-X4;
     B  := Round(sqrt(sqr(X1)+sqr(X3)));

     R1.Left   := RC.X - A div 2;
     R1.Right  := RC.X + A div 2;
     R1.Top    := RC.Y - B div 2;
     R1.Bottom := RC.Y + B div 2;

     GetBarCodeLines(BarCode);

     if FBars.Count=0 then
        BarcodeRaise(erErrorInBarCode);

     s:=FBars.Strings[0];
     i:=1;
     while (i<=Length(s)) and (s[i]=' ') do begin
        Inc(R1.Left);
        Inc(i);
     end;

     { draw label top and bottom }
     FCaptionUpper.Paint(c,R1,RC,WidthOf(r1){Length(s)});
     FCaptionBottom.Paint(c,R1,RC,WidthOf(r1){Length(s)});

     // paint quiet zone here
     QuietZone.Paint(C, R1);

     case BarcodeSymbology of
        {$ifdef PSOFT_PROF}
                bcPDF417  :
                    begin
                        psPDF417GetLines(Self, FBars);
                        PaintStacked(C,R1,Self);
                    end;
                // bcCode16K : PaintCode16K(C,R1,E);
        {$endif}
        {$ifdef PSOFT_STUDIO}
                bcDataMatrix,
                bcQRCode          : Pixels.Paint(C, R1, rc, Self);
        {$endif}
                bcNone : ;
        else
                PaintBarCode1(C,R1,Self,BarCode, RC);
     end;

//     if Assigned(FOnPaint) then
//        OnPaint(Self,R,BarCode);

     if IAmDemo then begin
          C.Font.Assign(CaptionHuman.Font);
          DrawDemo(C,R2, RC, Angle, BackgroundColor);
     end;

end;

{$ifdef PSOFT_STUDIO}
  function TpsBarcodeComponent.Pixels: TpsMatrix;
  begin
    Result := FPixels;
  end;
{$endif}

procedure TpsBarcodeComponent.SetAngle(const Value:Integer);
begin
     if FAngle<>Value then begin
       FAngle := 90*(Value div 90) mod 360;
       UpdateBarcode;
     end;
end;

procedure   TpsBarcodeComponent.SetCaptionUpper(const Value:TpsBarcodeCaption);
begin
     FCaptionUpper.Assign(Value);
end;

procedure TpsBarcodeComponent.SetErrorInfo(Value: TpsBarcodeError);
begin
    FErrorInfo := Value;
end;

procedure   TpsBarcodeComponent.SetCaptionHuman(const Value:TpsBarcodeCaption);
begin
     FCaptionHuman.Assign(Value);
end;

procedure   TpsBarcodeComponent.SetCaptionBottom(const Value:TpsBarcodeCaption);
begin
     FCaptionBottom.Assign(Value);
end;

function    CalcEan13CheckDigit(s:String):Char;
var i,j,c:Integer;
begin
        j:=0;
        for i:=1 to Length(s) do begin
            c:=Ord(S[i])-Ord0;
            if (i mod 2)=0 then j:=j+3*c
            else                j:=j+c;
        end;
        j:=j mod 10;
        if j>0 then j:=10-j;
        Result:=Char(j+Ord0);
end;

function    CalcEan8CheckDigit(s:String):Char;
var i,j,c:Integer;
begin
        j:=0;
        for i:=1 to Length(s) do begin
            c:=Ord(S[i])-Ord0;
            if (i mod 2)=0 then j:=j+c
            else                j:=j+3*c;
        end;
        j:=j mod 10;
        if j>0 then j:=10-j;
        Result:=Char(j+Ord0);
end;

function    CalcISBNCheckDigit(s:String):Char;
var i,j:Integer;
    pom:String;
begin
    pom:='';
    for i:=1 to Length(s) do
        if s[i]<>'-' then pom:=pom+s[i];

    j:=0;
    for i:=1 to 9 do
         j:=j+i*(Ord(pom[i])-Ord0);

    j:=j mod 11;

    if j<10 then Result:=Char(j+Ord0)
    else         Result:='X';
end;

function    BooklandToEAN(s:String; T:TpsBarcodeSymbology):String;
var i:Integer;
begin
     case T of
          bcISSN : Result := PrefixISSN;
          bcISBN : Result := PrefixISBN;
          bcISMN : Result := PrefixISMN;
     end;
     for i:=1 to Length(s) do
         if s[i]<>'-' then Result:=Result+s[i];

     if Length(Result)=12 then
        Result:=Result+' ';

     if Length(Result)>=13 then
        Result[13]:=CalcEan13CheckDigit(Copy(Result,1,12));
end;


function  CheckDigit212Mod10(s:String):Char;
var i,su,na :Integer;
begin
    su:=0;
    for i:=1 to Length(s) do begin
        na := Ord(s[i])-Ord0;
        if (i mod 2)=1 then na:=2*na;
        if na<10 then Inc(su,na)
        else begin
             Inc(su,1);
             Inc(su,na-10);
        end;
    end;

    su:=10 - (su mod 10);
    if su=10 then su:=0;

    Result := Char(su+Ord0);
end;

procedure   TpsBarcodeComponent.BarcodeComplete;
var s,p:String;
    i,j:Integer;
begin
     s:=FBarCode;
     case FBarcodeSymbology of
          bcEan8,bcJAN8  : begin
                         if FBarcodeSymbology=bcJAN8 then
                            s:='49'+Copy(s,3,20);
                         if Length(s)=7 then s:=s+' ';
                         if Length(s)>=8 then
                            s[8] := CalcEan8CheckDigit(Copy(s,1,7));
                    end;
          bcEan13,bcJAN13 : begin
                         if FBarcodeSymbology=bcJAN13 then
                            s:='49'+Copy(s,3,20);
                         if Length(s)=12 then s:=s+' ';
                         if Length(s)>=13 then
                            s[13] := CalcEan13CheckDigit(Copy(s,1,12));
                    end;
          bcUPCA : begin
                         if Length(s)=11 then s:=s+' ';
                         if Length(s)>=12 then
                            s[12] := CalcEan8CheckDigit(Copy(s,1,11));
                    end;
          bcITF  : begin
                         if Length(s) in [5,13,15] then s:=s+' ';
                         case Length(s) of
                              6  : s[ 6] := CalcEan8CheckDigit(Copy(s,1, 5));
                              14 : s[14] := CalcEan8CheckDigit(Copy(s,1,13));
                              16 : s[16] := CalcEan8CheckDigit(Copy(s,1,15));
                         end;
                    end;
          bcISBN, bcISSN, bcISMN  : begin
                         if Length(s)=11 then s:=s+'- ';
                         if Length(s)=12 then s:=s+' ';
                         if Length(s)>=13 then
                            s[13] := CalcISBNCheckDigit(Copy(s,1,12));
                    end;
          bcUPCE0,bcUPCE1 : begin
                         if Length(s)=6 then s:='0'+s;
                         if Length(s)=7 then s:=s+' ';
                         if Length(s)>=8 then begin
                            i:=Ord(s[7])-Ord0;
                            case i of
                                 0,1,2 : p:=Copy(s,1,3)+s[7]+'0000'+Copy(s,4,3);
                                 3     : p:=Copy(s,1,4)+'00000'+Copy(s,5,2);
                                 4     : p:=Copy(s,1,5)+'00000'+Copy(s,6,1);
                                 5 ..9 : p:=Copy(s,1,6)+'0000' +s[7];
                            end;
                            s[8] := CalcEan8CheckDigit(p);
                         end;
                    end;
          bcUPCShipping : begin
                               if Length(s)=13 then s:=s+' ';
                               if Length(s)>=14 then
                                  s[14] := CalcEan8CheckDigit(Copy(s,1,13));
                          end;
          bcOPC         : begin
                               if Length(s)=9 then s:=s+' ';
                               if Length(s)>=10 then
                                  s[10] := CheckDigit212Mod10(Copy(s,1,9));
                          end;
          bcPZN        : begin
                               if Length(s) in [6,7] then begin
                                  if Length(s)=6 then s:=s+' ';
                                  j := 0;
                                  for i:=1 to 6 do
                                     j := j + (i+1)*(Ord(s[i])-Ord0);
                                  s[7] := Char(Ord0 + j mod 11);
                               end;
                         end;
     end;
     if s<>FBarCode then FBarCode:=s;
end;

function TpsBarcodeComponent.BarcodeComponent: TpsBarcodeComponent;
begin
  Result := Self;
end;

function TpsBarcodeComponent.BarcodeHint: String;
var s:string;
begin
    if not (boAutoHint in Options) then Exit;
    s:=Format(rsHintTemplate,
      [ BarcodeInfoItem(BI, itName), BarcodeInfoItem(BI, itEnumName),
        BarcodeInfoItem(BI, itSymbolType),
        Barcode, BarcodeInfoItem(BI, itChars)]);
    if s<>'' then
      s:=s+psCRLF+psCRLF+s;
    Result := s;
end;

function TpsBarcodeComponent.GetAbout: String;
begin
  Result := constAboutProduct;
end;


function TpsBarcodeComponent.GetAngle: Integer;
begin
  Result := FAngle;
end;

function TpsBarcodeComponent.GetBackgroundColor: TColor;
begin
  Result:=FBackgroundColor;
end;

function TpsBarcodeComponent.GetBarCode: String;
begin
  Result := FBarcode;
end;

function TpsBarcodeComponent.GetBarcodeInfo:TpsBarCodeInfo;
begin
     Result:=BarcodeInfo(FBarcodeSymbology,Barcode);
end;


procedure   TpsBarcodeComponent.Assign(Source:TPersistent);
var V:TpsBarcodeComponent;
begin
    if  Source is TpsBarcodeComponent then begin
         V:=TpsBarcodeComponent(Source);

         //FAutoInc           := V.AutoInc;
         //FAutoIncFrom       := V.AutoIncFrom;
         //FAutoIncTo         := V.AutoIncTo;
         FBackgroundColor   := V.BackgroundColor;
         FOptions           := V.Options;

         FBarcodeSymbology  := V.BarcodeSymbology;
         FLinesColor        := V.Linescolor;
         // OnPaint            := V.OnPaint;
         // OnChangeTypCode    := V.OnChangeTypCode;
         // OnChangeBarCode    := V.OnChangeBarCode;
         FBarCode           := V.Barcode;
         //FLabelMask         := V.LabelMask;
         FAngle             := V.Angle;
         FCaptionUpper.Assign(V.CaptionUpper);
         FCaptionBottom.Assign(V.CaptionBottom);
         FCaptionHuman.Assign(V.CaptionHuman);
         FErrorInfo.Assign(V.ErrorInfo);
         FHorzLines.Assign(V.HorzLines);
         FQuietZone.Assign(V.QuietZone);
         FErrorInfo.Assign(V.ErrorInfo);
         FParams.Assign(V.Params);

         UpdateBarcode;
    end
      else
          inherited Assign(Source);
end;

procedure TpsBarcodeComponent.AssignOnlyBarcode(Source: TpsBarcodeComponent);
var V:TpsBarcodeComponent;
begin
    if Source is TpsBarcodeComponent then begin
         V := TpsBarcodeComponent(Source);

         FBarcodeSymbology        := V.BarcodeSymbology;
         FBarCode           := V.Barcode;
         FAngle             := V.Angle;
         FBackgroundColor   := V.BackgroundColor;
         FLinesColor        := V.Linescolor;
         FOptions           := V.Options;
         FCaptionUpper.Assign(V.CaptionUpper);
         FCaptionBottom.Assign(V.CaptionBottom);
         FCaptionHuman.Assign(V.CaptionHuman);
         FQuietZone.Assign(V.QuietZone);
         FParams.Assign(V.Params);
         // Font.Assign(V.Font);
         UpdateBarcode;
    end;
end;

function    TpsBarcodeComponent.BarcodeTypeName:String;
var P:PPropInfo;
begin
     P:= GetPropInfo(Self.ClassInfo,'TypBarCode');
     Result := GetEnumName(P^.PropType^,Integer(BarcodeSymbology));
end;





function TpsBarcodeComponent.Bars: TStringList;
begin
    Result := FBars;
end;

function IsBookland(ISBN: String): Boolean;
var Number, CheckDigit        : String;
    CheckValue, CheckSum, Err : Integer;
    i, Cnt                    : Word;
begin
  {Get check digit}
  CheckDigit := Copy(ISBN, Length(ISBN), 1);

  {Get rest of ISBN, minus check digit and its hyphen}
  Number := Copy(ISBN, 1, Length(ISBN) - 2);
  {Length of ISBN remainder must be 11 and check digit between 9 and 9 or
   X}
  if (Length(Number) = 11) and (Pos(CheckDigit, '0123456789X') > 0) then
    begin

    {Get numeric value for check digit}
    if (CheckDigit = 'X') then
      CheckSum := 10
    else
      Val(CheckDigit, CheckSum, Err);
    {Iterate through ISBN remainder, applying decode algorithm}
    Cnt := 1;
    for i := 1 to 12 do begin
      {Act only if current character is between "0" and "9" to exclude
       hyphens}
      if (Pos(Number[i], '0123456789') > 0) then begin
        Val(Number[i], CheckValue, Err);
        {Algorithm for each character in ISBN remainder, Cnt is the nth

        character so processed}
        CheckSum := CheckSum + CheckValue * (11 - Cnt);
        Inc(Cnt);
      end;
    end;
    {Verify final value is evenly divisible by 11}
    Result := (CheckSum MOD 11 = 0);
  end
  else
    Result := False;
end;

function TpsBarcodeComponent.Fim(ck: string): string;
var x:integer;
begin
  Result:='';
  if Length(ck)=0 then Exit;
  x:=Ord(UpCase(ck[1]));
  case x of
    Ord('A') : Result := '1010001000101';
    Ord('B') : Result := '10010100101001';
    Ord('C') : Result := '10100100100101';
    Ord('D') : Result := '101010010010101';
  end;
end;

procedure TpsBarcodeComponent.SetQuietZone(const Value: TpsQuietZone);
begin
    FQuietZone.Assign(Value);
end;


procedure TpsBarcodeComponent.SetOptions(const Value: TpsBarcodeOptions);
begin
  if FOptions<>Value then begin
    FOptions := Value;
    UpdateBarcode;
  end;
end;

procedure TpsBarcodeComponent.SetParams(const Value: TpsParams);
begin
  FParams.Assign(Value);
end;

function TpsBarcodeComponent.BI: TpsBarCodeInfo;
begin
  Result:=BarcodeInfo(BarcodeSymbology,Barcode);
end;

procedure TpsBarcodeComponent.UpdateBarcode;
begin
    DoUpdate;
    if Assigned(FOnUpdateBarcodeParent) then
        FOnUpdateBarcodeParent(Self);
end;

procedure TpsBarcodeComponent.BarcodeRaise;
begin
    FErrorInfo.Execute;
end;

procedure   TpsBarcodeComponent.BarcodeRaiseStr(err:TpsBarcodeErrorCode;
    str:String=''; hlp:Integer=0);
begin
  FErrorInfo.ExecuteStr(err, str, hlp);
end;

procedure   TpsBarcodeComponent.BarcodeRaise(err:TpsBarcodeErrorCode; idx:Integer=0;
  str:String=''; hlp:Integer=0);
begin
  FErrorInfo.Execute(err, idx, str, hlp);
end;

procedure   TpsBarcodeComponent.BarcodeRaise(err:TpsBarcodeErrorCode; P1,P2:Integer );
begin
  FErrorInfo.Execute(err, P1, P2);
end;

procedure  TpsBarcodeComponent.BarcodeRaise(err:TpsErrRecord);
begin
  FErrorInfo.Execute(err);
end;

function TpsBarcodeComponent.GetHint: String;
begin
    result := Format(rsHintTemplate,
      [ BarcodeInfoItem(BI, itName), BarcodeInfoItem(BI, itEnumName),
        BarcodeInfoItem(BI, itSymbolType),
        Barcode, BarcodeInfoItem(BI, itChars)]);
end;

procedure TpsBarcodeComponent.ChangeSymbology(dir: TpsDirection;
  OnlySupported: Boolean);
var bs:TpsBarcodeSymbology;
    BI:TpsBarCodeInfo;
begin
  bs := BarcodeSymbology;
  BI := BarcodeInfo(bs);

  while True do begin
    case dir of
      psDirFirst : begin
            bs  := Low(TpsBarcodeSymbology);
            dir := psDirNext;
          end;
      psDirPrevious : begin
            if bs=Low(TpsBarcodeSymbology) then
                bs:=High(TpsBarcodeSymbology)
            else
                bs:=Pred(bs);
         end;
      psDirNone : ;
      psDirNext : begin
            if bs=High(TpsBarcodeSymbology) then
                bs:=Low(TpsBarcodeSymbology)
            else
                bs:=Succ(bs);
         end;
      psDirLast : begin
            bs  := High(TpsBarcodeSymbology);
            dir := psDirPrevious;
          end;
    end;

    BI:=BarcodeInfo(bs);
    if OnlySupported and (BI.Supported=False) then begin

      Continue;
    end
    else Break;
  end;
  BarcodeSymbology := bs;
end;



end.
