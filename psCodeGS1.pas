unit psCodeGS1;

interface

{$I psBarcode.inc}

uses Classes, SysUtils, StrUtils, psTypes, psCodeFNLite, psCodeExceptions;

// ****************************************************************************
// *** Used for locate and work with special symbol/command in barcode
// *** suchg as FNC1, ECI, GLI, ...
// ****************************************************************************

const
    GS1_AI_START        = '(';
    GS1_AI_END          = ')';
    GS1_PERC            = '%';
    GS1_TILDE           = '~';
    GS1_BRACKET         = '[';
    GS1_GroupSeparator  : Char = #$1D;

type

    TpsFNC1InputType = (gs1Separators, gs1Percent, gs1Classic, gs1Tilde);
    TpsFNC1Type      = (fnc1None, fnc1FirstPosition, fnc1SecondPosition, fnc1GS1);

    TpsCode128 = ( psCodeA, psCodeB, psCodeC, psCodeAorB);

    TpsSpecialSymbol  = (scNone, scUnknown, scASCII, scOriginalValue,
        scFNC1, scFNC2, scFNC3, scFNC4, scECI, scGLI0, scGLI1,
        scSegmentIndex, scSegmentCount,
        // for Macro block PDF417
        scFileName, scTimeStamp,  scSender, scAddress, scFileSize, scCheckSum,
        // for Code128
        scStartA,   scStartB,     scStartC, scShift );

    TpsSpecialParamType = (stNone, stInteger, stString, stDate, stTimeStamp1970);

    TpsCheckSum = (chUndefined, chNone, chGS, chMod10, chMod43, chMod11,
      chMod11_10, chMod37_2, chReedSolomon, chCRC,
      chGS1_Standard, chGS1_4DigitPrice, chGS1_5DigitPrice );

    TpsCheckDigitMode = (cdNone, cdAdd, cdReplace, cdReplaceWarning);

    TpsSpecialResult   = record
      Symbol        : TpsSpecialSymbol;
      Cmd           : String;
      ValueInteger  : Integer;
      ValueString   : String;
      NextIndex     : Integer;
      Error         : Boolean;
    end;

    TpsSpecialRec = record
      Text    : String;
      Desc    : String;
      Symbol  : TpsSpecialSymbol;
      Params  : TpsSpecialParamType;
      Value   : Integer;
    end;

    TpsGS1 = class(TPersistent)
    private
      FBarcodeComponent:TComponent;
      FFNC1Type   : TpsFNC1Type;
      FFNC1Input  : TpsFNC1InputType;
      procedure SetFNC1Input(const Value: TpsFNC1InputType);
      procedure SetFNC1Type(const Value: TpsFNC1Type);

      procedure UpdateBarcode;
    public
      constructor CreateBarcode(AOwner:TComponent);
      procedure Assign(Source:TPersistent); override;

      function  SolveGS1(const Barcode:String; var err:TpsErrRecord):String;
      // check if value is correct for this AI
      class function LocateAI(const ai: String): Integer; 
      class function VerifyAI(const ai, value:String; var err:TpsErrRecord): Boolean; overload;
      class function VerifyAI(ai_idx:Integer; const value:String; var err:TpsErrRecord): Boolean; overload;
      class function Verify(s:String; var ErrRecord:TpsErrRecord ):Integer;
      class function idxOK(idx:Integer): Boolean;
      class function GetElement(const s:String; idx:Integer; var ai, value:String): Integer;
      class function GetElementVisual(idx:Integer; const ai,value:String;
            var errRecord:TpsErrRecord):String;
      class function GetDescription(const s:String): String;
      class function DecodeDateString(const s:String): String;
      class function IsSpecial(const s:String; var index:Integer; var SpecialRec:TpsSpecialResult): Boolean;
      class function RemoveCommands(const s:String):String;
      class function CalcCheckDigit(var s:String; totalLen:Integer; version:TpsChecksum): Boolean;
     // convert from classic () syntax to FS syntax
      class function Prepare(const s:String; var ErrRecord:TpsErrRecord):String;
      class function PrepareTilde(const s:String; var ErrRecord:TpsErrRecord):String;
      // convert from % syntax to FS syntax
      class function PreparePerc(const s:String; var ErrRecord:TpsErrRecord):String;
    published
        property FNC1InputType  : TpsFNC1InputType read FFNC1Input write SetFNC1Input;
        property FNC1Type       : TpsFNC1Type read FFNC1Type write SetFNC1Type;
        // property AutoDetect;
        //property SubType ;
    end;

    TpsAIType=(aiInteger=0, aiFloat=1, aiString=2, aiDate=3,
        aiDateTimeMinutes=4, aiDateTimeSeconds=5,
        aiISO_Country=6,     aiISO_CountryMulti=7,  aiISO_Currency=8);

//     TpsGS1Checksum = (chNone, chStandard, ch4DigitPrice, ch5DigitPrice);

     // list of available AI's, their title, description, type, length,
     TpsGS1Item = record
        AI                  : String;
        FNC1                : Boolean;
        Title               : String;
        Description         : String;
        AIType              : TpsAIType;
        LenMin, LenMax      : Integer;
        CheckSum            : TpsCheckSum;

        SecUsed             : Boolean;
        SecTitle            : String;
        SecDescription      : String;
        SecAIType           : TpsAIType;
        SecLenMin,SecLenMax : Integer;
     end;

    // invalid AI pairs, not can be combined in one barcode symbol
    TpsGS1_InvalidPair = record
      AI1 : string;
      AI2 : string;
      N9  : Boolean;
      Description : String;
    end;

    TpsISOCountry = record
        Country : String;
        A2      : String[2];
        A3      : String[3];
        ISOCode : Integer;
    end;

    TpsISOCurrency = record
      ISOCountry     : Integer;
      CurrencyName   : String;
      AlphabeticCode : String[3];
      NumericCode    : Integer;
    end;

implementation

uses psBarcodeComp;

//---------------------------------------------------------------------------
//--- All about GS1
//---------------------------------------------------------------------------

//type


  // TpsCheckDigitStatus=(cdCanBe, cdAlways, cdWithout);



const constSprecialStart      : Char = '[';
      constSprecialStop       : Char = ']';

      tblSpecialSymbols : Array [0..59] of TpsSpecialRec =(
        (Text:'NUL';      Desc:'Null';                  Symbol:scASCII; Value:0),
        (Text:'SOH';      Desc:'Start of heading';      Symbol:scASCII; Value:1),
        (Text:'STX';      Desc:'Start of text';         Symbol:scASCII; Value:2),
        (Text:'ETX';      Desc:'End of Text';           Symbol:scASCII; Value:3),
        (Text:'EOT';      Desc:'End of transmit';       Symbol:scASCII; Value:4),
        (Text:'ENQ';      Desc:'Enquiry';               Symbol:scASCII; Value:5),
        (Text:'ACK';      Desc:'Acknowledge';           Symbol:scASCII; Value:6),
        (Text:'BEL';      Desc:'Audible bell';          Symbol:scASCII; Value:7),
        (Text:'BS';       Desc:'Backspace';             Symbol:scASCII; Value:8),
        (Text:'HT';       Desc:'Horizontal tab';        Symbol:scASCII; Value:9),
        (Text:'LF';       Desc:'Line feed';             Symbol:scASCII; Value:10),
        (Text:'VT';       Desc:'Vertical tab';          Symbol:scASCII; Value:11),
        (Text:'FF';       Desc:'Form feed';             Symbol:scASCII; Value:12),
        (Text:'CR';       Desc:'Cariage return';        Symbol:scASCII; Value:13),
        (Text:'SO';       Desc:'Shift out';             Symbol:scASCII; Value:14),
        (Text:'SI';       Desc:'Shift in';              Symbol:scASCII; Value:15),
        (Text:'DLE';      Desc:'Data link escape';      Symbol:scASCII; Value:16),
        (Text:'DC1';      Desc:'Device control 1';      Symbol:scASCII; Value:17),
        (Text:'DC2';      Desc:'Device control 2';      Symbol:scASCII; Value:18),
        (Text:'DC3';      Desc:'Device control 3';      Symbol:scASCII; Value:19),
        (Text:'DC4';      Desc:'Device control 4';      Symbol:scASCII; Value:20),
        (Text:'NAK';      Desc:'Negative acknowledge';  Symbol:scASCII; Value:21),
        (Text:'SYN';      Desc:'Synchronous idle';      Symbol:scASCII; Value:22),
        (Text:'ETB';      Desc:'End trasmit block';     Symbol:scASCII; Value:23),
        (Text:'CAN';      Desc:'Cancel';                Symbol:scASCII; Value:24),
        (Text:'EM';       Desc:'End of medium';         Symbol:scASCII; Value:25),
        (Text:'SUB';      Desc:'Substitution';          Symbol:scASCII; Value:26),
        (Text:'ESC';      Desc:'Escape';                Symbol:scASCII; Value:27),
        (Text:'FS';       Desc:'File separator';        Symbol:scASCII; Value:28),
        (Text:'GS';       Desc:'Group separator';       Symbol:scASCII; Value:29),
        (Text:'RS';       Desc:'Record separator';      Symbol:scASCII; Value:30),
        (Text:'US';       Desc:'Unit separator';        Symbol:scASCII; Value:31),
        (Text:'DEL';                                    Symbol:scASCII; Value:$7F),

        (Text:'Copy';     Desc:'Copyright sign';      Symbol:scASCII; Value:169),
        (Text:'TM';       Desc:'Trademark sign';      Symbol:scASCII; Value:174),
        (Text:'1/4';      Desc:'One quarter';         Symbol:scASCII; Value:188),
        (Text:'1/2';      Desc:'One half';            Symbol:scASCII; Value:189),
        (Text:'3/4';      Desc:'Three quarters';      Symbol:scASCII; Value:190),
        (Text:'CENT';     Desc:'Cent sign';           Symbol:scASCII; Value:162),
        (Text:'POUND';    Desc:'Pound sign';          Symbol:scASCII; Value:163),
        (Text:'YEN';      Desc:'Yen sign';            Symbol:scASCII; Value:165),

        (Text:'FNC1';     Symbol:scFNC1),
        (Text:'FNC2';     Symbol:scFNC2),
        (Text:'FNC3';     Symbol:scFNC3),
        (Text:'FNC4';     Symbol:scFNC4),
        (Text:'ECI';      Desc:'Extended Channel Interpretation'; Symbol:scECI; Params:stInteger),
        (Text:'GLI0';     Symbol:scGLI0),
        (Text:'GLI1';     Symbol:scGLI1),

        (Text:'SegmentCount';       Desc:'Total count of segments'; Symbol:scSegmentCount;  Params:stInteger),
        (Text:'SegmentIndex';       Desc:'Segment Index'; Symbol:scSegmentIndex;  Params:stInteger),

        (Text:'Filename';           Symbol:scFileName;      Params:stString),
        (Text:'TimeStamp';          Symbol:scTimeStamp;     Params:stTimeStamp1970),
        (Text:'Sender';             Symbol:scSender;        Params:stString),
        (Text:'Address';            Symbol:scAddress;       Params:stString),
        (Text:'Filesize';           Symbol:scFileSize;      Params:stString),
        (Text:'Checksum';           Symbol:scChecksum;      Params:stString),

        (Text:'StartA';             Symbol:scStartA),
        (Text:'StartB';             Symbol:scStartB),
        (Text:'StartC';             Symbol:scStartC),
        (Text:'Shift';              Symbol:scShift)
        );


    GS1_COUNT     = 115;
    GS1_AI : array[0..GS1_COUNT-1] of TpsGS1Item = (
{ 1}
    (AI:'00'; Title:'SSCC';     Description: 'SSCC (Serial Shipping Container Code)';
        AIType:aiInteger; LenMin:18; LenMax: 18; Checksum: chGS1_Standard; SecUsed:False),
    (AI:'01'; Title:'GTIN';     Description: 'GTIN (Global Trade Item Number)';
        AITYpe:aiInteger; LenMin:14; LenMax:14;  Checksum: chGS1_Standard; SecUsed:False),
    (AI:'02'; Title:'CONTENT';  Description: 'GTIN of Contained Trade Items';
        AIType:aiInteger; LenMin:14; LenMax:14; Checksum: chGS1_Standard; SecUsed:False),
    (AI:'10'; Title:'BATCH/LOT'; Description: 'Batch or Lot Number';
        AIType:aiString; LenMin:1; LenMax:20; Checksum: chNone; SecUsed:False),
    (AI:'11'; Title:'PROD.DATE'; Description: 'Production Date';
        AIType:aiDate; LenMin:6; LenMax:6; Checksum: chNone; SecUsed:False),
    (AI:'12'; Title:'DUE DATE'; Description: 'Due Date';
        AIType:aiDate; LenMin:6; LenMax:6; Checksum: chNone; SecUsed:False),
    (AI:'13'; Title:'PACK DATE'; Description: 'Packaging Date';
        AIType:aiDate; LenMin:6; LenMax:6; Checksum: chNone; SecUsed:False),
    (AI:'15'; Title:'BEST BEFORE'; Description: 'Best Before Date';
        AIType:aiDate; LenMin:6; LenMax:6; Checksum: chNone; SecUsed:False),
    (AI:'17'; Title:'EXPIRY'; Description: 'Expiration Date';
        AIType:aiDate; LenMin:6; LenMax:6; Checksum: chNone; SecUsed:False),
    (AI:'20'; Title:'VARIANT'; Description: 'Variant Number';
        AIType:aiInteger; LenMin:2; LenMax:2; Checksum: chNone; SecUsed:False),
{11}(AI:'21'; Title:'SERIAL'; Description: 'Serial Number';
        AIType:aiString; LenMin:1; LenMax:20; Checksum: chNone; SecUsed:False),
    (AI:'22'; Title:'QTY/DATE/BATCH'; Description: 'Secondary Data Fields';
        AIType:aiString; LenMin:1; LenMax:29; Checksum: chNone; SecUsed:False),
    (AI:'240'; Title:'ADDITIONAL ID'; Description: 'Additional Item Identification';
        AIType:aiString; LenMin:1; LenMax:30; Checksum: chNone; SecUsed:False),
    (AI:'241'; Title:'CUST.PART NO.'; Description: 'Customer Part Number';
        AIType:aiString; LenMin:1; LenMax:30; Checksum: chNone; SecUsed:False),
    (AI:'242'; Title:'MTO VARIANT'; Description: 'Made-to-Order Variation Number';
        AIType:aiInteger; LenMin:1; LenMax:6; Checksum: chNone; SecUsed:False),
    (AI:'250'; Title:'SECONDARY SERIAL'; Description: 'Secondary Serial Number';
        AIType:aiString; LenMin:1; LenMax:30; Checksum: chNone; SecUsed:False),
    (AI:'251'; Title:'REF.ToolPalettePageBarcode SOURCE'; Description: 'Reference to Source Entity';
        AIType:aiString; LenMin:1; LenMax:30; Checksum: chNone; SecUsed:False),
    (AI:'253'; Title:'DOC.ID'; Description: 'Global Document Type Identifier (GDTI)';
        AIType:aiInteger; LenMin:13; LenMax:13; Checksum: chGS1_Standard;
        SecUsed:True; SecAIType:aiInteger; SecLenMin:0; SecLenMax:17),
    (AI:'254'; Title:'GLN EXTENSION'; Description: 'GLN Extension Component';
        AIType:aiString; LenMin:1; LenMax:20; SecUsed:False),
    (AI:'30';  Title:'VAR.COUNT'; Description: 'Count of Items (Variable Measure Trade Item)';
        AIType:aiInteger; LenMin:1; LenMax:8; SecUsed:False),
{21}(AI:'310X'; Title:'NET WEIGHT(kg)'; Description: 'Net weight, kilograms';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'311X'; Title:'LENGTH(m)'; Description: 'Length of first dimension, metres';
        AIType:aiFloat; LenMin:1; LenMax:6; SecUsed:False),
    (AI:'312X'; Title:'WIDTH(m)'; Description: 'Width, diametre, or second dimension, metres';
        AIType:aiFloat; LenMin:1; LenMax:6; SecUsed:False),
    (AI:'313X'; Title:'HEIGHT(m)'; Description: 'Depth, thickness, height, or third dimension, metres';
        AIType:aiFloat; LenMin:1; LenMax:6; SecUsed:False),
    (AI:'314X'; Title:'AREA(m)'; Description: 'Area, square metres';
        AIType:aiFloat; LenMin:1; LenMax:6; SecUsed:False),
    (AI:'315X'; Title:'NET VOLUME(l)'; Description: 'Net volume, litres';
        AIType:aiFloat; LenMin:1; LenMax:6; SecUsed:False),
    (AI:'316X'; Title:'NET VOLUME(m)'; Description: 'Net volume, cubic metres';
        AIType:aiFloat; LenMin:1; LenMax:6; SecUsed:False),
    (AI:'320X'; Title:'NET WEIGHT(lb)'; Description: 'Net weight, pounds';
        AIType:aiFloat; LenMin:1; LenMax:6; SecUsed:False),
    (AI:'321X'; Title:'LENGTH(i)'; Description: 'Length or first dimension, inches';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'322X'; Title:'LENGTH(f)'; Description: 'Length or first dimension, feet';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
{31}
    (AI:'323X'; Title:'LENGTH(y)'; Description: 'Length or first dimension, yards';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'324X'; Title:'WIDTH(i)'; Description: 'Width, diametre or second dimension, inches';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'325X'; Title:'WIDTH(f)'; Description: 'Width, diametre or second dimension, feet';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'326X'; Title:'WIDTH(y)'; Description: 'Width, diametre or second dimension, yards';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'327X'; Title:'HEIGHT(i)'; Description: 'Depth, thickness, height, or third dimension, inches';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'328X'; Title:'HEIGHT(f)'; Description: 'Depth, thickness, height, or third dimension, feet';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'329X'; Title:'HEIGHT(y)'; Description: 'Depth, thickness, height, or third dimension, yards';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'330X'; Title:'GROSS WEIGHT(kg)'; Description: 'Logistic weight, kilograms';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'331X'; Title:'LENGTH(m),log'; Description: 'Length or first dimension, metres';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'332X'; Title:'WIDTH(m),log'; Description: 'Width, diameter, or second dimension, metres';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
{41}
    (AI:'333X'; Title:'HEIGHT(m),log'; Description: 'Depth, thickness, height, or third dimension, metres';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'334X'; Title:'AREA(m),log'; Description: 'Area, square metres';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'335X'; Title:'VOLUME(l),log'; Description: 'Logistic volume, litres';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'336X'; Title:'VOLUME(m),log'; Description: 'Logistic volume, cubic litres';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'337X'; Title:'KG PER m2'; Description: 'Kilograms per square metre';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'340X'; Title:'GROSS WEIGHT(lb)'; Description: 'Logistic weight, pounds';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'341X'; Title:'LENGTH(i),log'; Description: 'Length or first dimension, inches';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'342X'; Title:'LENGTH(f),log'; Description: 'Length or first dimension, feet';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'343X'; Title:'LENGTH(y),log'; Description: 'Length or first dimension, yards';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'344X'; Title:'WIDTH(i),log'; Description: 'Width, diametre, or second dimension';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
{51}
    (AI:'345X'; Title:'WIDTH(f),log'; Description: 'Width, diametre, or second dimension';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'346X'; Title:'WIDTH(y),log'; Description: 'Width, diametre, or second dimension';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'347X'; Title:'HEIGHT(i),log'; Description: 'Depth, thickness, height, or third dimension';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'348X'; Title:'HEIGHT(f),log'; Description: 'Depth, thickness, height, or third dimension';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'349X'; Title:'HEIGHT(y),log'; Description: 'Depth, thickness, height, or third dimension';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'350X'; Title:'AREA(i)'; Description: 'Area, square inches';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'351X'; Title:'AREA(f)'; Description: 'Area, square feet';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'352X'; Title:'AREA(y)'; Description: 'Area, square yards';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'353X'; Title:'AREA(i),log'; Description: 'Area, square inches';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'354X'; Title:'AREA(f),log'; Description: 'Area, square feet';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
{62}(AI:'355X'; Title:'AREA(y),log'; Description: 'Area, square yards';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'356X'; Title:'NET WEIGHT(t)'; Description: 'Net weight, troy ounces';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'357X'; Title:'NET VOLUME(oz)'; Description: 'Net weight (or volume), ounces';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'360X'; Title:'NET VOLUME(q)'; Description: 'Net volume, quarts';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'361X'; Title:'NET VOLUME(g)'; Description: 'Net volume, gallons U.S.';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'362X'; Title:'VOLUME(q),log'; Description: 'Logistic volume, quarts';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'363X'; Title:'VOLUME(g),log'; Description: 'Logistic volume, gallons U.S.';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'364X'; Title:'VOLUME(i),log'; Description: 'Net volume, cubic inches';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'365X'; Title:'VOLUME(f),log'; Description: 'Net volume, cubic feet';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'366X'; Title:'VOLUME(y),log'; Description: 'Net volume, cubic yards';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
{71}
    (AI:'367X'; Title:'VOLUME(q),log'; Description: 'Logistic volume, cubic inches';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'368X'; Title:'VOLUME(g),log'; Description: 'Logistic volume, cubic feet';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'369X'; Title:'VOLUME(y),log'; Description: 'Logistic volume, cubic yards';
        AIType:aiFloat; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'37';   Title:'COUNT'; Description: 'Count of Trade Items';
        AIType:aiInteger; LenMin:1; LenMax:8; SecUsed:False),
    (AI:'390X'; Title:'AMOUNT'; Description: 'Applicable Amount Payable, local currency';
        AIType:aiFloat; LenMin:1; LenMax:15; SecUsed:False),
    (AI:'391X'; Title:'AMOUNT'; Description: 'Applicable Amount Payable with ISO Currency Code';
          AIType:aiISO_Currency; LenMin:3; LenMax:3;
          SecUsed:True; SecAIType:aiInteger; SecLenMin:0; SecLenMax:15),
    (AI:'392X'; Title:'PRICE'; Description: 'Applicable Amount Payable, single monetary area';
          AIType:aiFloat; LenMin:1; LenMax:15; SecUsed:False),
    (AI:'393X'; Title:'PRICE'; Description: 'Applicable Amount Payable with ISO Currency Code';
          AIType:aiISO_Currency; LenMin:3; LenMax:3;
          SecUsed:True; SecAIType:aiInteger; SecLenMin:0; SecLenMax:15),
    (AI:'400';  Title:'ORDER NUMBER'; Description: 'Customer''s Purchase OrderNumber';
          AIType:aiString; LenMin:1; LenMax:30; SecUsed:False),
    (AI:'401';  Title:'CONSIGNMENT'; Description: 'Consignment NUmber';
          AIType:aiString; LenMin:1; LenMax:30; SecUsed:False),
{81}(AI:'402'; Title:'SHIPMENT NO.'; Description: 'Shipment Identification number';
          AIType:aiInteger; LenMin:17; LenMax:17; Checksum: chGS1_Standard; SecUsed:False),
    (AI:'403'; Title:'ROUTE'; Description: 'Routing Code';
          AIType:aiString; LenMin:1; LenMax:30; SecUsed:False),
    (AI:'410'; Title:'SHIP TO LOC'; Description: 'Ship to - Deliver to Global Location Number';
          AIType:aiInteger; LenMin:13; LenMax:13; Checksum: chGS1_Standard; SecUsed:False),
    (AI:'411'; Title:'BILL TO'; Description: 'Bill to - Invoice to Global Location Number';
          AIType:aiInteger; LenMin:13; LenMax:13; Checksum: chGS1_Standard; SecUsed:False),
    (AI:'412'; Title:'PURCHASE FROM'; Description: 'Purchased from Global Location Number';
          AIType:aiInteger; LenMin:13; LenMax:13; Checksum: chGS1_Standard; SecUsed:False),
    (AI:'413'; Title:'SHIP FOR LOC'; Description: 'Ship for - Deliver for - Forward to Global Location Number';
          AIType:aiInteger; LenMin:13; LenMax:13; Checksum: chGS1_Standard; SecUsed:False),
    (AI:'414'; Title:'LOC No'; Description: 'Identification of Physical Location- Global Location Number';
          AIType:aiInteger; LenMin:13; LenMax:13; Checksum: chGS1_Standard; SecUsed:False),
    (AI:'415'; Title:'PAY TO'; Description: 'Global Location Number of the Invoicing Party';
          AIType:aiInteger; LenMin:13; LenMax:13; Checksum: chGS1_Standard; SecUsed:False),
    (AI:'420'; Title:'SHIP TO POST'; Description: 'Ship to - Deliver to Postal Code Within a Single Postal Authority';
          AIType:aiString; LenMin:1; LenMax:20; SecUsed:False),
    (AI:'421'; Title:'SHIP TO POST'; Description: 'Ship to - Deliver to Postal Code with ISO Country Code';
          AIType:aiISO_Country; LenMin:3; LenMax:3;
          SecUsed:True; SecAIType:aiString; SecLenMin:0; SecLenMax:12),
{91}
    (AI:'422'; Title:'ORIGIN'; Description: 'Country of Origin of the Trade Item';
          AIType:aiISO_Country; LenMin:3; LenMax:3; SecUsed:False),
    (AI:'423'; Title:'COUNTRY-INITIAL PROCESS'; Description: 'Country of Initial Processing';
          AIType:aiISO_CountryMulti; LenMin:3; LenMax:15;
          SecUsed:True; SecAIType:aiInteger; SecLenMin:0; SecLenMax:12),
    (AI:'424'; Title:'COUNTRY-PROCESS'; Description: 'Country of Processing';
        AIType:aiISO_Country; LenMin:3; LenMax:3; SecUsed:False),
    (AI:'425'; Title:'COUNTRY-DISASSEMBLY'; Description: 'Country of Disassembly';
        AIType:aiISO_Country; LenMin:3; LenMax:3; SecUsed:False),
    (AI:'426'; Title:'COUNTRY-FULL PROCESS'; Description: 'Country Covering full Process Chin';
        AIType:aiISO_Country; LenMin:3; LenMax:3; SecUsed:False),
    (AI:'7001'; Title:'NSN'; Description: 'NATO Stock Number (NSN)';
        AIType:aiInteger; LenMin:13; LenMax:13; SecUsed:False),
    (AI:'7002'; Title:'MEAT CUT'; Description: 'UN/ECE Meat Carcasses and Cut Classification';
        AIType:aiString; LenMin:1; LenMax:30; SecUsed:False),
    (AI:'7003'; Title:'EXPIRY TIME'; Description: 'Expiration Date and Time';
        AIType:aiDateTimeMinutes; LenMin:12; LenMax:12; SecUsed:False),
    (AI:'703X'; Title:'PROCESSOR #'; Description: 'Approval Numberof Processor with ISO country Code';
          AIType:aiISO_Country; LenMin:3; LenMax:3;
          SecUsed:True; SecAIType:aiString; SecLenMin:0; SecLenMax:27),
    (AI:'8001'; Title:'DIMENSIONS'; Description: 'Roll Products (Width, Length, Core Diameter, Direction, Splices)';
        AIType:aiInteger; LenMin:14; LenMax:14; SecUsed:False),
{101}
    (AI:'8002'; Title:'CMT No'; Description: 'Cellular Mobile Telephone Identifier';
        AIType:aiString; LenMin:1; LenMax:20; SecUsed:False),
    (AI:'8003'; Title:'GRAI'; Description: 'Global Returnable Asset Identifier (GRAI)';
          AIType:aiInteger; LenMin:14; LenMax:14; Checksum: chGS1_Standard;
          SecUsed:True; SecAIType:aiString; SecLenMin:0; SecLenMax:16),
    (AI:'8004'; Title:'GIAI'; Description: 'Global Individual Asset Identifier (GRAI)';
          AIType:aiString; LenMin:1; LenMax:30; SecUsed:False),
    (AI:'8005'; Title:'PRICE PER UNIT'; Description: 'Price per Unit of Measure';
          AIType:aiInteger; LenMin:6; LenMax:6; SecUsed:False),
    (AI:'8006'; Title:'GCTIN'; Description: 'Identification of the Components of a Trade Item';
          AIType:aiInteger; LenMin:18; LenMax:18; SecUsed:False),
    (AI:'8007'; Title:'IBAN'; Description: 'International Bank Account Number (IBAN)';
          AIType:aiString; LenMin:1; LenMax:30; SecUsed:False),
    (AI:'8008'; Title:'PROD TIME'; Description: 'Date and Time of Production';
          AIType:aiDateTimeSeconds; LenMin:8; LenMax:8;
          SecUsed:True; SecAIType:aiInteger; SecLenMin:0; SecLenMax:4),
    (AI:'8018'; Title:'GSRN'; Description: 'Global Service Relation Number (GSRN)';
          AIType:aiInteger; LenMin:18; LenMax:18; Checksum: chGS1_Standard; SecUsed:False),
    (AI:'8020'; Title:'REF No.'; Description: 'PAyment Slip Reference Number';
          AIType:aiString; LenMin:1; LenMax:25; SecUsed:False),
    (AI:'8100'; Title:'-'; Description: 'GS1-128 Coupon Extended Code';
          AIType:aiInteger; LenMin:6; LenMax:6; SecUsed:False),
{111}
    (AI:'8101'; Title:'-'; Description: 'GS1-128 Coupon Extended Code';
          AIType:aiInteger; LenMin:10; LenMax:10; SecUsed:False),
    (AI:'8102'; Title:'-'; Description: 'GS1-128 Coupon Extended Code';
          AIType:aiInteger; LenMin:2; LenMax:2; SecUsed:False),
    (AI:'8110'; Title:'-'; Description: 'Coupon Code Identification for Use in North America';
          AIType:aiString; LenMin:1; LenMax:30; SecUsed:False),
    (AI:'90'; Title:'INTERNAL'; Description: 'Information Mutually Agren Between Trading Partners';
          AIType:aiString; LenMin:1; LenMax:30; SecUsed:False),
    (AI:'9X'; Title:'INTERNAL'; Description: 'Company internal Information';
          AIType:aiString; LenMin:1; LenMax:30; SecUsed:False)
  );


  GS1_IvalidPairs : Array[0..16] of TpsGS1_InvalidPair = (
    (AI1 : '01';    AI2: '01';    N9:False;   Description: 'Duplicate Global Trade Item Numbers (GTINs) with different values'),
    (AI1 : '01';    AI2: '02';    N9:False;   Description: 'AI (02) must not be used for the identification of trade items contained in a trade item'),
    (AI1 : '01';    AI2: '37';    N9:False;   Description: 'The count of units contained would duplicate the master data of the GTIN. AI (37) may only be used with AI (02)'),
    (AI1 : '22';    AI2: '30';    N9:False;   Description: 'Duplicate counts with different values'),
    (AI1 : '22';    AI2: '10';    N9:False;   Description: 'Duplicate lot numbers with different values'),
    (AI1 : '22';    AI2: '17';    N9:False;   Description: 'Duplicate lot numbers with different values'),
    (AI1 : '22';    AI2: '21';    N9:False;   Description: 'Duplicate lot numbers with different values'),
    (AI1 : '241';   AI2: '01';    N9:True;    Description: 'Made-to-Order Variation can only be used with GTIN-14, Indicator digit 9. This represents a Custom Industrial Supply Item'),
    (AI1 : '241';   AI2: '02';    N9:True;    Description: 'Made-to-Order Variation can only be used with GTIN-14, Indicator digit 9. This represents a Custom Industrial Supply Item'),
    (AI1 : '420';   AI2: '421';   N9:False;   Description: 'Only one ship to postal code may be applied to an item'),
    (AI1 : '422';   AI2: '426';   N9:False;   Description: 'Duplication of country of origin of a trade item (covered by country of full processing)'),
    (AI1 : '423';   AI2: '426';   N9:False;   Description: 'Duplication of country of origin of a trade item (covered by country of full processing)'),
    (AI1 : '424';   AI2: '426';   N9:False;   Description: 'Duplication of country of origin of a trade item (covered by country of full processing)'),
    (AI1 : '425';   AI2: '426';   N9:False;   Description: 'Duplication of country of origin of a trade item (covered by country of full processing)'),
    (AI1 : '390X';  AI2: '391X';  N9:False;   Description: 'Only one amount payable Element String may be applied on a payment slip'),
    (AI1 : '392X';  AI2: '393X';  N9:False;   Description: 'Only one amount payable Element String may be applied on a Variable Measure Trade Item'),
    (AI1 : '8006';  AI2: '01';    N9:False;   Description: 'Other GTIN cannot be used with AI (8006). The trade item is identified by a GTIN contained in the AI (8006)')
  );


  psISOCountries : array [1..240] of TpsISOCountry = (
{1} ( Country : 'AALAND ISLANDS';       A2 : 'AX';  A3 : 'ALA';    ISOCode:248),
    ( Country : 'AFGHANISTAN';          A2 : 'AF';  A3 : 'AFG';    ISOCode:004),
    ( Country : 'ALBANIA';              A2 : 'AL';  A3 : 'ALB';    ISOCode:008),
    ( Country : 'ALGERIA';              A2 : 'DZ';  A3 : 'DZA';    ISOCode:012),
    ( Country : 'AMERICAN SAMOA';       A2 : 'AS';  A3 : 'ASM';    ISOCode:016),
    ( Country : 'ANDORRA';              A2 : 'AD';  A3 : 'AND';    ISOCode:020),
    ( Country : 'ANGOLA';               A2 : 'AO';  A3 : 'AGO';    ISOCode:024),
    ( Country : 'ANGUILLA';             A2 : 'AI';  A3 : 'AIA';    ISOCode:660),
    ( Country : 'ANTARCTICA';           A2 : 'AQ';  A3 : 'ATA';    ISOCode:010),
{10}( Country : 'ANTIGUA AND BARBUDA';  A2 : 'AG';  A3 : 'ATG';    ISOCode:028),
    ( Country : 'ARGENTINA';            A2 : 'AR';  A3 : 'ARG';    ISOCode:032),
    ( Country : 'ARMENIA';              A2 : 'AM';  A3 : 'ARM';    ISOCode:051),
    ( Country : 'ARUBA';                A2 : 'AW';  A3 : 'ABW';    ISOCode:533),
    ( Country : 'AUSTRALIA';            A2 : 'AU';  A3 : 'AUS';    ISOCode:036),
    ( Country : 'AUSTRIA';              A2 : 'AT';  A3 : 'AUT';    ISOCode:040),
    ( Country : 'AZERBAIJAN';           A2 : 'AZ';  A3 : 'AZE';    ISOCode:031),
    ( Country : 'BAHAMAS';              A2 : 'BS';  A3 : 'BHS';    ISOCode:044),
    ( Country : 'BAHRAIN';              A2 : 'BH';  A3 : 'BHR';    ISOCode:048),
    ( Country : 'BANGLADESH';           A2 : 'BD';  A3 : 'BGD';    ISOCode:050),
{20}( Country : 'BARBADOS';             A2 : 'BB';  A3 : 'BRB';    ISOCode:052),
    ( Country : 'BELARUS';              A2 : 'BY';  A3 : 'BLR';    ISOCode:112),
    ( Country : 'BELGIUM';              A2 : 'BE';  A3 : 'BEL';    ISOCode:056),
    ( Country : 'BELIZE';               A2 : 'BZ';  A3 : 'BLZ';    ISOCode:084),
    ( Country : 'BENIN';                A2 : 'BJ';  A3 : 'BEN';    ISOCode:204),
    ( Country : 'BERMUDA';              A2 : 'BM';  A3 : 'BMU';    ISOCode:060),
    ( Country : 'BHUTAN';               A2 : 'BT';  A3 : 'BTN';    ISOCode:064),
    ( Country : 'BOLIVIA';              A2 : 'BO';  A3 : 'BOL';    ISOCode:068),
    ( Country : 'BOSNIA AND HERZEGOWINA';A2: 'BA';  A3 : 'BIH';    ISOCode:070),
    ( Country : 'BOTSWANA';             A2 : 'BW';  A3 : 'BWA';    ISOCode:072),
{30}( Country : 'BOUVET ISLAND';        A2 : 'BV';  A3 : 'BVT';    ISOCode:074),
    ( Country : 'BRAZIL';               A2 : 'BR';  A3 : 'BRA';    ISOCode:076),
    ( Country : 'BRITISH INDIAN OCEAN TERRITORY';
                                        A2 : 'IO';  A3 : 'IOT';    ISOCode:086),
    ( Country : 'BRUNEI DARUSSALAM';    A2 : 'BN';  A3 : 'BRN';    ISOCode:096),
    ( Country : 'BULGARIA';             A2 : 'BG';  A3 : 'BGR';    ISOCode:100),
    ( Country : 'BURKINA FASO';         A2 : 'BF';  A3 : 'BFA';    ISOCode:854),
    ( Country : 'BURUNDI';              A2 : 'BI';  A3 : 'BDI';    ISOCode:108),
    ( Country : 'CAMBODIA';             A2 : 'KH';  A3 : 'KHM';    ISOCode:116),
    ( Country : 'CAMEROON';             A2 : 'CM';  A3 : 'CMR';    ISOCode:120),
    ( Country : 'CANADA';               A2 : 'CA';  A3 : 'CAN';    ISOCode:124),
{40}( Country : 'CAPE VERDE';           A2 : 'CV';  A3 : 'CPV';    ISOCode:132),
    ( Country : 'CAYMAN ISLANDS';       A2 : 'KY';  A3 : 'CYM';    ISOCode:136),
    ( Country : 'CENTRAL AFRICAN REPUBLIC';
                                        A2 : 'CF';  A3 : 'CAF';    ISOCode:140),
    ( Country : 'CHAD';                 A2 : 'TD';  A3 : 'TCD';    ISOCode:148),
    ( Country : 'CHILE';                A2 : 'CL';  A3 : 'CHL';    ISOCode:152),
    ( Country : 'CHINA';                A2 : 'CN';  A3 : 'CHN';    ISOCode:156),
    ( Country : 'CHRISTMAS ISLAND';     A2 : 'CX';  A3 : 'CXR';    ISOCode:162),
    ( Country : 'COCOS (KEELING) ISLANDS';
                                        A2 : 'CC';  A3 : 'CCK';    ISOCode:166),
    ( Country : 'COLOMBIA';             A2 : 'CO';  A3 : 'COL';    ISOCode:170),
    ( Country : 'COMOROS';              A2 : 'KM';  A3 : 'COM';    ISOCode:174),
{50}( Country : 'CONGO, Democratic Republic of (was Zaire)';
                                        A2 : 'CD';  A3 : 'COD';    ISOCode:180),
    ( Country : 'CONGO, Republic of';   A2 : 'CG';  A3 : 'COG';    ISOCode:178),
    ( Country : 'COOK ISLANDS';         A2 : 'CK';  A3 : 'COK';    ISOCode:184),
    ( Country : 'COSTA RICA';           A2 : 'CR';  A3 : 'CRI';    ISOCode:188),
    ( Country : 'COTE D''IVOIRE';       A2 : 'CI';  A3 : 'CIV';    ISOCode:384),
    ( Country : 'CROATIA (local name: Hrvatska)';
                                        A2 : 'HR';  A3 : 'HRV';    ISOCode:191),
    ( Country : 'CUBA';                 A2 : 'CU';  A3 : 'CUB';    ISOCode:192),
    ( Country : 'CYPRUS';               A2 : 'CY';  A3 : 'CYP';    ISOCode:196),
    ( Country : 'CZECH REPUBLIC';       A2 : 'CZ';  A3 : 'CZE';    ISOCode:203),
    ( Country : 'DENMARK';              A2 : 'DK';  A3 : 'DNK';    ISOCode:208),
{60}( Country : 'DJIBOUTI';             A2 : 'DJ';  A3 : 'DJI';    ISOCode:262),
    ( Country : 'DOMINICA';             A2 : 'DM';  A3 : 'DMA';    ISOCode:212),
    ( Country : 'DOMINICAN REPUBLIC';   A2 : 'DO';  A3 : 'DOM';    ISOCode:214),
    ( Country : 'ECUADOR';              A2 : 'EC';  A3 : 'ECU';    ISOCode:218),
    ( Country : 'EGYPT';                A2 : 'EG';  A3 : 'EGY';    ISOCode:818),
    ( Country : 'EL SALVADOR';          A2 : 'SV';  A3 : 'SLV';    ISOCode:222),
    ( Country : 'EQUATORIAL GUINEA';    A2 : 'GQ';  A3 : 'GNQ';    ISOCode:226),
    ( Country : 'ERITREA';              A2 : 'ER';  A3 : 'ERI';    ISOCode:232),
    ( Country : 'ESTONIA';              A2 : 'EE';  A3 : 'EST';    ISOCode:233),
    ( Country : 'ETHIOPIA';             A2 : 'ET';  A3 : 'ETH';    ISOCode:231),
{70}( Country : 'FALKLAND ISLANDS (MALVINAS)';
                                        A2 : 'FK';  A3 : 'FLK';    ISOCode:238),
    ( Country : 'FAROE ISLANDS';        A2 : 'FO';  A3 : 'FRO';    ISOCode:234),
    ( Country : 'FIJI';                 A2 : 'FJ';  A3 : 'FJI';    ISOCode:242),
    ( Country : 'FINLAND';              A2 : 'FI';  A3 : 'FIN';    ISOCode:246),
    ( Country : 'FRANCE';               A2 : 'FR';  A3 : 'FRA';    ISOCode:250),
    ( Country : 'FRENCH GUIANA';        A2 : 'GF';  A3 : 'GUF';    ISOCode:254),
    ( Country : 'FRENCH POLYNESIA';     A2 : 'PF';  A3 : 'PYF';    ISOCode:258),
    ( Country : 'FRENCH SOUTHERN TERRITORIES';
                                        A2 : 'TF';  A3 : 'ATF';    ISOCode:260),
    ( Country : 'GABON';                A2 : 'GA';  A3 : 'GAB';    ISOCode:266),
    ( Country : 'GAMBIA';               A2 : 'GM';  A3 : 'GMB';    ISOCode:270),
{80}( Country : 'GEORGIA';              A2 : 'GE';  A3 : 'GEO';    ISOCode:268),
    ( Country : 'GERMANY';              A2 : 'DE';  A3 : 'DEU';    ISOCode:276),
    ( Country : 'GHANA';                A2 : 'GH';  A3 : 'GHA';    ISOCode:288),
    ( Country : 'GIBRALTAR';            A2 : 'GI';  A3 : 'GIB';    ISOCode:292),
    ( Country : 'GREECE';               A2 : 'GR';  A3 : 'GRC';    ISOCode:300),
    ( Country : 'GREENLAND';            A2 : 'GL';  A3 : 'GRL';    ISOCode:304),
    ( Country : 'GRENADA';              A2 : 'GD';  A3 : 'GRD';    ISOCode:308),
    ( Country : 'GUADELOUPE';           A2 : 'GP';  A3 : 'GLP';    ISOCode:312),
    ( Country : 'GUAM';                 A2 : 'GU';  A3 : 'GUM';    ISOCode:316),
    ( Country : 'GUATEMALA';            A2 : 'GT';  A3 : 'GTM';    ISOCode:320),
{90}( Country : 'GUINEA';               A2 : 'GN';  A3 : 'GIN';    ISOCode:324),
    ( Country : 'GUINEA-BISSAU';        A2 : 'GW';  A3 : 'GNB';    ISOCode:624),
    ( Country : 'GUYANA';               A2 : 'GY';  A3 : 'GUY';    ISOCode:328),
    ( Country : 'HAITI';                A2 : 'HT';  A3 : 'HTI';    ISOCode:332),
    ( Country : 'HEARD AND MC DONALD ISLANDS';
                                        A2 : 'HM';  A3 : 'HMD';    ISOCode:334),
    ( Country : 'HONDURAS';             A2 : 'HN';  A3 : 'HND';    ISOCode:340),
    ( Country : 'HONG KONG';            A2 : 'HK';  A3 : 'HKG';    ISOCode:344),
    ( Country : 'HUNGARY';              A2 : 'HU';  A3 : 'HUN';    ISOCode:348),
    ( Country : 'ICELAND';              A2 : 'IS';  A3 : 'ISL';    ISOCode:352),
    ( Country : 'INDIA';                A2 : 'IN';  A3 : 'IND';    ISOCode:356),
{100}
    ( Country : 'INDONESIA';            A2 : 'ID';  A3 : 'IDN';    ISOCode:360),
    ( Country : 'IRAN (ISLAMIC REPUBLIC OF)';
                                        A2 : 'IR';  A3 : 'IRN';    ISOCode:364),
    ( Country : 'IRAQ';                 A2 : 'IQ';  A3 : 'IRQ';    ISOCode:368),
    ( Country : 'IRELAND';              A2 : 'IE';  A3 : 'IRL';    ISOCode:372),
    ( Country : 'ISRAEL';               A2 : 'IL';  A3 : 'ISR';    ISOCode:376),
    ( Country : 'ITALY';                A2 : 'IT';  A3 : 'ITA';    ISOCode:380),
    ( Country : 'JAMAICA';              A2 : 'JM';  A3 : 'JAM';    ISOCode:388),
    ( Country : 'JAPAN';                A2 : 'JP';  A3 : 'JPN';    ISOCode:392),
    ( Country : 'JORDAN';               A2 : 'JO';  A3 : 'JOR';    ISOCode:400),
    ( Country : 'KAZAKHSTAN';           A2 : 'KZ';  A3 : 'KAZ';    ISOCode:398),
{110}
    ( Country : 'KENYA';                A2 : 'KE';  A3 : 'KEN';    ISOCode:404),
    ( Country : 'KIRIBATI';             A2 : 'KI';  A3 : 'KIR';    ISOCode:296),
    ( Country : 'KOREA, DEMOCRATIC PEOPLE''S REPUBLIC OF';
                                        A2 : 'KP';  A3 : 'PRK';    ISOCode:408),
    ( Country : 'KOREA, REPUBLIC OF';   A2 : 'KR';  A3 : 'KOR';    ISOCode:410),
    ( Country : 'KUWAIT';               A2 : 'KW';  A3 : 'KWT';    ISOCode:414),
    ( Country : 'KYRGYZSTAN';           A2 : 'KG';  A3 : 'KGZ';    ISOCode:417),
    ( Country : 'LAO PEOPLE'' DEMOCRATIC REPUBLIC';
                                        A2 : 'LA';  A3 : 'LAO';    ISOCode:418),
    ( Country : 'LATVIA';               A2 : 'LV';  A3 : 'LVA';    ISOCode:428),
    ( Country : 'LEBANON';              A2 : 'LB';  A3 : 'LBN';    ISOCode:422),
    ( Country : 'LESOTHO';              A2 : 'LS';  A3 : 'LSO';    ISOCode:426),
{120}
    ( Country : 'LIBERIA';              A2 : 'LR';  A3 : 'LBR';    ISOCode:430),
    ( Country : 'LIBYAN ARAB JAMAHIRIYA';
                                        A2 : 'LY';  A3 : 'LBY';    ISOCode:434),
    ( Country : 'LIECHTENSTEIN';        A2 : 'LI';  A3 : 'LIE';    ISOCode:438),
    ( Country : 'LITHUANIA';            A2 : 'LT';  A3 : 'LTU';    ISOCode:440),
    ( Country : 'LUXEMBOURG';           A2 : 'LU';  A3 : 'LUX';    ISOCode:442),
    ( Country : 'MACAU';                A2 : 'MO';  A3 : 'MAC';    ISOCode:446),
    ( Country : 'MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF';
                                        A2 : 'MK';  A3 : 'MKD';    ISOCode:807),
    ( Country : 'MADAGASCAR';           A2 : 'MG';  A3 : 'MDG';    ISOCode:450),
    ( Country : 'MALAWI';               A2 : 'MW';  A3 : 'MWI';    ISOCode:454),
    ( Country : 'MALAYSIA';             A2 : 'MY';  A3 : 'MYS';    ISOCode:458),
{130}
    ( Country : 'MALDIVES';             A2 : 'MV';  A3 : 'MDV';    ISOCode:462),
    ( Country : 'MALI';                 A2 : 'ML';  A3 : 'MLI';    ISOCode:466),
    ( Country : 'MALTA';                A2 : 'MT';  A3 : 'MLT';    ISOCode:470),
    ( Country : 'MARSHALL ISLANDS';     A2 : 'MH';  A3 : 'MHL';    ISOCode:584),
    ( Country : 'MARTINIQUE';           A2 : 'MQ';  A3 : 'MTQ';    ISOCode:474),
    ( Country : 'MAURITANIA';           A2 : 'MR';  A3 : 'MRT';    ISOCode:478),
    ( Country : 'MAURITIUS';            A2 : 'MU';  A3 : 'MUS';    ISOCode:480),
    ( Country : 'MAYOTTE';              A2 : 'YT';  A3 : 'MYT';    ISOCode:175),
    ( Country : 'MEXICO';               A2 : 'MX';  A3 : 'MEX';    ISOCode:484),
    ( Country : 'MICRONESIA, FEDERATED STATES OF';
                                        A2 : 'FM';  A3 : 'FSM';    ISOCode:583),
{140}
    ( Country : 'MOLDOVA, REPUBLIC OF'; A2 : 'MD';  A3 : 'MDA';    ISOCode:498),
    ( Country : 'MONACO';               A2 : 'MC';  A3 : 'MCO';    ISOCode:492),
    ( Country : 'MONGOLIA';             A2 : 'MN';  A3 : 'MNG';    ISOCode:496),
    ( Country : 'MONTSERRAT';           A2 : 'MS';  A3 : 'MSR';    ISOCode:500),
    ( Country : 'MOROCCO';              A2 : 'MA';  A3 : 'MAR';    ISOCode:504),
    ( Country : 'MOZAMBIQUE';           A2 : 'MZ';  A3 : 'MOZ';    ISOCode:508),
    ( Country : 'MYANMAR';              A2 : 'MM';  A3 : 'MMR';    ISOCode:104),
    ( Country : 'NAMIBIA';              A2 : 'NA';  A3 : 'NAM';    ISOCode:516),
    ( Country : 'NAURU';                A2 : 'NR';  A3 : 'NRU';    ISOCode:520),
    ( Country : 'NEPAL';                A2 : 'NP';  A3 : 'NPL';    ISOCode:524),
{150}
    ( Country : 'NETHERLANDS';          A2 : 'NL';  A3 : 'NLD';    ISOCode:528),
    ( Country : 'NETHERLANDS ANTILLES'; A2 : 'AN';  A3 : 'ANT';    ISOCode:530),
    ( Country : 'NEW CALEDONIA';        A2 : 'NC';  A3 : 'NCL';    ISOCode:540),
    ( Country : 'NEW ZEALAND';          A2 : 'NZ';  A3 : 'NZL';    ISOCode:554),
    ( Country : 'NICARAGUA';            A2 : 'NI';  A3 : 'NIC';    ISOCode:558),
    ( Country : 'NIGER';                A2 : 'NE';  A3 : 'NER';    ISOCode:562),
    ( Country : 'NIGERIA';              A2 : 'NG';  A3 : 'NGA';    ISOCode:566),
    ( Country : 'NIUE';                 A2 : 'NU';  A3 : 'NIU';    ISOCode:570),
    ( Country : 'NORFOLK ISLAND';       A2 : 'NF';  A3 : 'NFK';    ISOCode:574),
    ( Country : 'NORTHERN MARIANA ISLANDS';
                                        A2 : 'MP';  A3 : 'MNP';    ISOCode:580),
{160}
    ( Country : 'NORWAY';               A2 : 'NO';  A3 : 'NOR';    ISOCode:578),
    ( Country : 'OMAN';                 A2 : 'OM';  A3 : 'OMN';    ISOCode:512),
    ( Country : 'PAKISTAN';             A2 : 'PK';  A3 : 'PAK';    ISOCode:586),
    ( Country : 'PALAU';                A2 : 'PW';  A3 : 'PLW';    ISOCode:585),
    ( Country : 'PALESTINIAN TERRITORY, Occupied';
                                        A2 : 'PS';  A3 : 'PSE';    ISOCode:     275),
    ( Country : 'PANAMA';               A2 : 'PA';  A3 : 'PAN';    ISOCode:     591),
    ( Country : 'PAPUA NEW GUINEA';     A2 : 'PG';  A3 : 'PNG';    ISOCode:     598),
    ( Country : 'PARAGUAY';             A2 : 'PY';  A3 : 'PRY';    ISOCode:     600),
    ( Country : 'PERU';                 A2 : 'PE';  A3 : 'PER';    ISOCode:     604),
    ( Country : 'PHILIPPINES';          A2 : 'PH';  A3 : 'PHL';    ISOCode:     608),
{170}
    ( Country : 'PITCAIRN';             A2 : 'PN';  A3 : 'PCN';    ISOCode:     612),
    ( Country : 'POLAND';               A2 : 'PL';  A3 : 'POL';    ISOCode:     616),
    ( Country : 'PORTUGAL';             A2 : 'PT';  A3 : 'PRT';    ISOCode:     620),
    ( Country : 'PUERTO RICO';          A2 : 'PR';  A3 : 'PRI';    ISOCode:     630),
    ( Country : 'QATAR';                A2 : 'QA';  A3 : 'QAT';    ISOCode:     634),
    ( Country : 'REUNION';              A2 : 'RE';  A3 : 'REU';    ISOCode:     638),
    ( Country : 'ROMANIA';              A2 : 'RO';  A3 : 'ROU';    ISOCode:     642),
    ( Country : 'RUSSIAN FEDERATION';   A2 : 'RU';  A3 : 'RUS';    ISOCode:     643),
    ( Country : 'RWANDA';               A2 : 'RW';  A3 : 'RWA';    ISOCode:     646),
    ( Country : 'SAINT HELENA';         A2 : 'SH';  A3 : 'SHN';    ISOCode:     654),
{180}
    ( Country : 'SAINT KITTS AND NEVIS';A2 : 'KN';  A3 : 'KNA';    ISOCode:     659),
    ( Country : 'SAINT LUCIA';          A2 : 'LC';  A3 : 'LCA';    ISOCode:     662),
    ( Country : 'SAINT PIERRE AND MIQUELON';
                                        A2 : 'PM';  A3 : 'SPM';    ISOCode:     666),
    ( Country : 'SAINT VINCENT AND THE GRENADINES';
                                        A2 : 'VC';  A3 : 'VCT';    ISOCode:     670),
    ( Country : 'SAMOA';                A2 : 'WS';  A3 : 'WSM';    ISOCode:     882),
    ( Country : 'SAN MARINO';           A2 : 'SM';  A3 : 'SMR';    ISOCode:     674),
    ( Country : 'SAO TOME AND PRINCIPE';A2 : 'ST';  A3 : 'STP';    ISOCode:     678),
    ( Country : 'SAUDI ARABIA';         A2 : 'SA';  A3 : 'SAU';    ISOCode:     682),
    ( Country : 'SENEGAL';              A2 : 'SN';  A3 : 'SEN';    ISOCode:     686),
    ( Country : 'SERBIA AND MONTENEGRO';A2 : 'CS';  A3 : 'SCG';    ISOCode:     891),
{190}
    ( Country : 'SEYCHELLES';           A2 : 'SC';  A3 : 'SYC';    ISOCode:     690),
    ( Country : 'SIERRA LEONE';         A2 : 'SL';  A3 : 'SLE';    ISOCode:     694),
    ( Country : 'SINGAPORE';            A2 : 'SG';  A3 : 'SGP';    ISOCode:     702),
    ( Country : 'SLOVAKIA';             A2 : 'SK';  A3 : 'SVK';    ISOCode:     703),
    ( Country : 'SLOVENIA';             A2 : 'SI';  A3 : 'SVN';    ISOCode:     705),
    ( Country : 'SOLOMON ISLANDS';      A2 : 'SB';  A3 : 'SLB';    ISOCode:     090),
    ( Country : 'SOMALIA';              A2 : 'SO';  A3 : 'SOM';    ISOCode:     706),
    ( Country : 'SOUTH AFRICA';         A2 : 'ZA';  A3 : 'ZAF';    ISOCode:     710),
    ( Country : 'SOUTH GEORGIA AND THE SOUTH SANDWICH ISLANDS';
                                        A2 : 'GS';  A3 : 'SGS';    ISOCode:     239),
    ( Country : 'SPAIN';                A2 : 'ES';  A3 : 'ESP';    ISOCode:     724),
{200}
    ( Country : 'SRI LANKA';            A2 : 'LK';  A3 : 'LKA';    ISOCode:     144),
    ( Country : 'SUDAN';                A2 : 'SD';  A3 : 'SDN';    ISOCode:     736),
    ( Country : 'SURINAME';             A2 : 'SR';  A3 : 'SUR';    ISOCode:     740),
    ( Country : 'SVALBARD AND JAN MAYEN ISLANDS';
                                        A2 : 'SJ';  A3 : 'SJM';    ISOCode:     744),
    ( Country : 'SWAZILAND';            A2 : 'SZ';  A3 : 'SWZ';    ISOCode:     748),
    ( Country : 'SWEDEN';               A2 : 'SE';  A3 : 'SWE';    ISOCode:     752),
    ( Country : 'SWITZERLAND';          A2 : 'CH';  A3 : 'CHE';    ISOCode:     756),
    ( Country : 'SYRIAN ARAB REPUBLIC'; A2 : 'SY';  A3 : 'SYR';    ISOCode:     760),
    ( Country : 'TAIWAN';               A2 : 'TW';  A3 : 'TWN';    ISOCode:     158),
    ( Country : 'TAJIKISTAN';           A2 : 'TJ';  A3 : 'TJK';    ISOCode:     762),
{210}
    ( Country : 'TANZANIA, UNITED REPUBLIC OF';
                                        A2 : 'TZ';  A3 : 'TZA';    ISOCode:     834),
    ( Country : 'THAILAND';             A2 : 'TH';  A3 : 'THA';    ISOCode:     764),
    ( Country : 'TIMOR-LESTE';          A2 : 'TL';  A3 : 'TLS';    ISOCode:     626),
    ( Country : 'TOGO';                 A2 : 'TG';  A3 : 'TGO';    ISOCode:     768),
    ( Country : 'TOKELAU';              A2 : 'TK';  A3 : 'TKL';    ISOCode:     772),
    ( Country : 'TONGA';                A2 : 'TO';  A3 : 'TON';    ISOCode:     776),
    ( Country : 'TRINIDAD AND TOBAGO';  A2 : 'TT';  A3 : 'TTO';    ISOCode:     780),
    ( Country : 'TUNISIA';              A2 : 'TN';  A3 : 'TUN';    ISOCode:     788),
    ( Country : 'TURKEY';               A2 : 'TR';  A3 : 'TUR';    ISOCode:     792),
    ( Country : 'TURKMENISTAN';         A2 : 'TM';  A3 : 'TKM';    ISOCode:     795),
{220}
    ( Country : 'TURKS AND CAICOS ISLANDS';
                                        A2 : 'TC';  A3 : 'TCA';    ISOCode:     796),
    ( Country : 'TUVALU';               A2 : 'TV';  A3 : 'TUV';    ISOCode:     798),
    ( Country : 'UGANDA';               A2 : 'UG';  A3 : 'UGA';    ISOCode:     800),
    ( Country : 'UKRAINE';              A2 : 'UA';  A3 : 'UKR';    ISOCode:     804),
    ( Country : 'UNITED ARAB EMIRATES'; A2 : 'AE';  A3 : 'ARE';    ISOCode:     784),
    ( Country : 'UNITED KINGDOM';       A2 : 'GB';  A3 : 'GBR';    ISOCode:     826),
    ( Country : 'UNITED STATES';        A2 : 'US';  A3 : 'USA';    ISOCode:     840),
    ( Country : 'UNITED STATES MINOR OUTLYING ISLANDS';
                                        A2 : 'UM';  A3 : 'UMI';    ISOCode:     581),
    ( Country : 'URUGUAY';              A2 : 'UY';  A3 : 'URY';    ISOCode:     858),
    ( Country : 'UZBEKISTAN';           A2 : 'UZ';  A3 : 'UZB';    ISOCode:     860),
{230}
    ( Country : 'VANUATU';              A2 : 'VU';  A3 : 'VUT';    ISOCode:     548),
    ( Country : 'VATICAN CITY STATE (HOLY SEE)';
                                        A2 : 'VA';  A3 : 'VAT';    ISOCode:     336),
    ( Country : 'VENEZUELA';           A2 : 'VE';  A3 : 'VEN';    ISOCode:     862),
    ( Country : 'VIET NAM';             A2 : 'VN';  A3 : 'VNM';    ISOCode:     704),
    ( Country : 'VIRGIN ISLANDS (BRITISH)';
                                        A2 : 'VG';  A3 : 'VGB';    ISOCode:     092),
    ( Country : 'VIRGIN ISLANDS (U.S.)';A2 : 'VI';  A3 : 'VIR';    ISOCode:     850),
    ( Country : 'WALLIS AND FUTUNA ISLANDS';
                                        A2 : 'WF';  A3 : 'WLF';    ISOCode:     876),
    ( Country : 'WESTERN SAHARA';       A2 : 'EH';  A3 : 'ESH';    ISOCode:     732),
    ( Country : 'YEMEN';                A2 : 'YE';  A3 : 'YEM';    ISOCode:     887),
    ( Country : 'ZAMBIA';               A2 : 'ZM';  A3 : 'ZMB';    ISOCode:     894),
{240}
    ( Country : 'ZIMBABWE';             A2 : 'ZW';  A3 : 'ZWE';    ISOCode:     716)
  );


  psICOCurrency : array [1..272] of TpsISOCurrency =(
//      ISOCountry     : Integer;
//      CurrencyName   : String;
//      AlphabeticCode : String[3];
//      NumericCode    : Integer;
    (CurrencyName : 'Afghani';  	                        AlphabeticCode :'AFN'; 	NumericCode: 971),
    (CurrencyName : 'AALAND ISLANDS';  	                  AlphabeticCode :'EUR'; 	NumericCode: 978),
    (CurrencyName : 'ALBANIA 	Lek';  	                    AlphabeticCode :'ALL'; 	NumericCode: 008),
    (CurrencyName : 'ALGERIA 	Algerian Dinar';            AlphabeticCode :'DZD'; 	NumericCode: 012),
    (CurrencyName : 'AMERICAN SAMOA US Dollar';           AlphabeticCode :'USD'; 	NumericCode: 840),
    (CurrencyName : 'ANDORRA 	Euro';                      AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'ANGOLA 	Kwanza';                    AlphabeticCode :'AOA'; 	NumericCode: 973),
    (CurrencyName : 'ANGUILLA 	East Caribbean Dollar';   AlphabeticCode :'XCD'; 	NumericCode: 951),
    (CurrencyName : 'ANTIGUA AND BARBUDA 	East Caribbean Dollar'; AlphabeticCode :'XCD'; 	NumericCode: 951),
    (CurrencyName : 'ARGENTINA 	Argentine Peso';          AlphabeticCode :'ARS'; 	NumericCode: 032),
    (CurrencyName : 'ARMENIA 	Armenian Dram';             AlphabeticCode :'AMD'; 	NumericCode: 051),
    (CurrencyName : 'ARUBA 	Aruban Guilder';              AlphabeticCode :'AWG'; 	NumericCode: 533),
    (CurrencyName : 'AUSTRALIA 	Australian Dollar';       AlphabeticCode :'AUD'; 	NumericCode: 036),
    (CurrencyName : 'AUSTRIA 	Euro';                      AlphabeticCode :'EUR'; 	NumericCode: 978),
    (CurrencyName : 'AZERBAIJAN 	Azerbaijanian Manat';   AlphabeticCode :'AZN'; 	NumericCode: 944),
    (CurrencyName : 'BAHAMAS 	Bahamian Dollar';           AlphabeticCode :'BSD'; 	NumericCode: 044),
    (CurrencyName : 'BAHRAIN 	Bahraini Dinar';            AlphabeticCode :'BHD'; 	NumericCode: 048),
    (CurrencyName : 'BANGLADESH 	Taka';                  AlphabeticCode :'BDT'; 	NumericCode: 050),
    (CurrencyName : 'BARBADOS 	Barbados Dollar';         AlphabeticCode :'BBD'; 	NumericCode: 052),
    (CurrencyName : 'BELARUS 	Belarussian Ruble';         AlphabeticCode :'BYR'; 	NumericCode: 974),
    (CurrencyName : 'BELGIUM 	Euro';                      AlphabeticCode :'EUR'; 	NumericCode: 978),
    (CurrencyName : 'BELIZE 	Belize Dollar';             AlphabeticCode :'BZD'; 	NumericCode: 084),
    (CurrencyName : 'BENIN 	CFA Franc BCEAO';             AlphabeticCode :'XOF'; 	NumericCode: 952),
    (CurrencyName : 'BERMUDA 	Bermudian Dollar (customarily known as Bermuda Dollar)'; AlphabeticCode :'BMD'; 	NumericCode: 060),
    (CurrencyName : 'BHUTAN 	Indian Rupee ';                     AlphabeticCode :'INR'; 	NumericCode: 356),
    (CurrencyName : 'BHUTAN 	Ngultrum';                          AlphabeticCode :'BTN'; 	NumericCode: 064),
    (CurrencyName : 'BOLIVIA 	Boliviano';                         AlphabeticCode :'BOB';  NumericCode: 068),
    (CurrencyName : 'BOLIVIA 	Mvdol';                             AlphabeticCode :'BOV';  NumericCode: 984),
    (CurrencyName : 'BOSNIA AND HERZEGOVINA 	Convertible Marks'; AlphabeticCode :'BAM';  NumericCode: 977),
    (CurrencyName : 'BOTSWANA 	Pula';                            AlphabeticCode :'BWP';  NumericCode: 072),
    (CurrencyName : 'BOUVET ISLAND 	Norwegian Krone';             AlphabeticCode :'NOK';  NumericCode: 578),
    (CurrencyName : 'BRAZIL 	Brazilian Real';                    AlphabeticCode :'BRL';  NumericCode: 986),
    (CurrencyName : 'BRITISH INDIAN OCEAN TERRITORY 	US Dollar'; AlphabeticCode :'USD';  NumericCode: 840),
    (CurrencyName : 'BRUNEI DARUSSALAM 	Brunei Dollar';           AlphabeticCode :'BND';  NumericCode: 096),
    (CurrencyName : 'BULGARIA 	Bulgarian Lev';                   AlphabeticCode :'BGN';  NumericCode: 975),
    (CurrencyName : 'BURKINA FASO 	CFA Franc BCEAO';             AlphabeticCode :'XOF';  NumericCode: 952),
    (CurrencyName : 'BURUNDI 	Burundi Franc';                     AlphabeticCode :'BIF';  NumericCode: 108),
    (CurrencyName : 'CAMBODIA 	Riel';                            AlphabeticCode :'KHR';  NumericCode: 116),
    (CurrencyName : 'CAMEROON 	CFA Franc BEAC';                  AlphabeticCode :'XAF';  NumericCode: 950),
    (CurrencyName : 'CANADA 	Canadian Dollar';                   AlphabeticCode :'CAD';  NumericCode: 124),
    (CurrencyName : 'CAPE VERDE 	Cape Verde Escudo';             AlphabeticCode :'CVE';  NumericCode: 132),
    (CurrencyName : 'CAYMAN ISLANDS 	Cayman Islands Dollar';     AlphabeticCode :'KYD';  NumericCode: 136),
    (CurrencyName : 'CENTRAL AFRICAN REPUBLIC 	CFA Franc BEAC';  AlphabeticCode :'XAF';  NumericCode: 950),
    (CurrencyName : 'CHAD 	CFA Franc BEAC';                      AlphabeticCode :'XAF';  NumericCode: 950),
    (CurrencyName : 'CHILE 	Chilean Peso ';                       AlphabeticCode :'CLP';  NumericCode: 152),
    (CurrencyName : 'CHILE 	Unidades de fomento';                 AlphabeticCode :'CLF';  NumericCode: 990),
    (CurrencyName : 'CHINA 	Yuan Renminbi';                       AlphabeticCode :'CNY';  NumericCode: 156),
    (CurrencyName : 'CHRISTMAS ISLAND 	Australian Dollar';       AlphabeticCode :'AUD';  NumericCode: 036),
    (CurrencyName : 'COCOS (KEELING) ISLANDS 	Australian Dollar'; AlphabeticCode :'AUD';  NumericCode: 036),
    (CurrencyName : 'COLOMBIA 	Colombian Peso';                  AlphabeticCode :'COP';  NumericCode: 170),
    (CurrencyName : 'COLOMBIA 	Unidad de Valor Real';            AlphabeticCode :'COU';  NumericCode: 970),
    (CurrencyName : 'COMOROS 	Comoro Franc';                      AlphabeticCode :'KMF';  NumericCode: 174),
    (CurrencyName : 'CONGO 	CFA Franc BEAC';                      AlphabeticCode :'XAF';  NumericCode: 950),
    (CurrencyName : 'CONGO, THE DEMOCRATIC REPUBLIC OF 	Congolese Franc';AlphabeticCode :'CDF';  NumericCode: 976),
    (CurrencyName : 'COOK ISLANDS 	New Zealand Dollar';          AlphabeticCode :'NZD';  NumericCode: 554),
    (CurrencyName : 'COSTA RICA 	Costa Rican Colon';             AlphabeticCode :'CRC';  NumericCode: 188),
    (CurrencyName : 'CÔTE D''IVOIRE 	CFA Franc BCEAO';           AlphabeticCode :'XOF';  NumericCode: 952),
    (CurrencyName : 'CROATIA 	Croatian Kuna';                     AlphabeticCode :'HRK';  NumericCode: 191),
    (CurrencyName : 'CUBA 	Cuban Peso';                          AlphabeticCode :'CUP';  NumericCode: 192),
    (CurrencyName : 'CUBA 	Peso Convertible';                    AlphabeticCode :'CUC';  NumericCode: 931),
    (CurrencyName : 'CYPRUS 	Euro';                              AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'CZECH REPUBLIC 	Czech Koruna';              AlphabeticCode :'CZK';  NumericCode: 203),
    (CurrencyName : 'DENMARK 	Danish Krone';                      AlphabeticCode :'DKK';  NumericCode: 208),
    (CurrencyName : 'DJIBOUTI 	Djibouti Franc';                  AlphabeticCode :'DJF';  NumericCode: 262),
    (CurrencyName : 'DOMINICA 	East Caribbean Dollar';           AlphabeticCode :'XCD';  NumericCode: 951),
    (CurrencyName : 'DOMINICAN REPUBLIC 	Dominican Peso';        AlphabeticCode :'DOP';  NumericCode: 214),
    (CurrencyName : 'ECUADOR 	US Dollar';                         AlphabeticCode :'USD';  NumericCode: 840),
    (CurrencyName : 'EGYPT 	Egyptian Pound';                      AlphabeticCode :'EGP';  NumericCode: 818),
    (CurrencyName : 'EL SALVADOR 	El Salvador Colon';             AlphabeticCode :'SVC';  NumericCode: 222),
    (CurrencyName : 'EL SALVADOR 	US Dollar';                    	AlphabeticCode :'USD';  NumericCode: 840),
    (CurrencyName : 'EQUATORIAL GUINEA 	CFA Franc BEAC';          AlphabeticCode :'XAF';  NumericCode: 950),
    (CurrencyName : 'ERITREA 	Nakfa';                    	        AlphabeticCode :'ERN';  NumericCode: 232),
    (CurrencyName : 'ESTONIA 	Kroon';                    	        AlphabeticCode :'EEK';  NumericCode: 233),
    (CurrencyName : 'ETHIOPIA 	Ethiopian Birr';                  AlphabeticCode :'ETB';  NumericCode: 230),
    (CurrencyName : 'FALKLAND ISLANDS (MALVINAS) 	Falkland Islands Pound'; AlphabeticCode :'FKP';  NumericCode: 238),
    (CurrencyName : 'FAROE ISLANDS 	Danish Krone';                AlphabeticCode :'DKK';  NumericCode: 208),
    (CurrencyName : 'FIJI 	Fiji Dollar';                    	    AlphabeticCode :'FJD';  NumericCode: 242),
    (CurrencyName : 'FINLAND 	Euro';                    	        AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'FRANCE 	Euro';                    	        AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'FRENCH GUIANA 	Euro';                    	  AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'FRENCH POLYNESIA 	CFP Franc';               AlphabeticCode :'XPF';  NumericCode: 953),
    (CurrencyName : 'FRENCH SOUTHERN TERRITORIES 	Euro';          AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'GABON 	CFA Franc BEAC';                    	AlphabeticCode :'XAF';  NumericCode: 950),
    (CurrencyName : 'GAMBIA 	Dalasi';                    	      AlphabeticCode :'GMD';  NumericCode: 270),
    (CurrencyName : 'GEORGIA 	Lari';                    	        AlphabeticCode :'GEL';  NumericCode: 981),
    (CurrencyName : 'GERMANY 	Euro';                    	        AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'GHANA 	Cedi';                    	          AlphabeticCode :'GHS';  NumericCode: 936),
    (CurrencyName : 'GIBRALTAR 	Gibraltar Pound';                 AlphabeticCode :'GIP';  NumericCode: 292),
    (CurrencyName : 'GREECE 	Euro';                    	        AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'GREENLAND 	Danish Krone';                    AlphabeticCode :'DKK';  NumericCode: 208),
    (CurrencyName : 'GRENADA 	East Caribbean Dollar';             AlphabeticCode :'XCD';  NumericCode: 951),
    (CurrencyName : 'GUADELOUPE 	Euro';                    	    AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'GUAM 	US Dollar';                    	      AlphabeticCode :'USD';  NumericCode: 840),
    (CurrencyName : 'GUATEMALA 	Quetzal';                    	    AlphabeticCode :'GTQ';  NumericCode: 320),
    (CurrencyName : 'GUERNSEY 	Pound Sterling';                  AlphabeticCode :'GBP';  NumericCode: 826),
    (CurrencyName : 'GUINEA 	Guinea Franc';                    	AlphabeticCode :'GNF';  NumericCode: 324),
    (CurrencyName : 'GUINEA-BISSAU 	CFA Franc BCEAO';             AlphabeticCode :'XOF';  NumericCode: 952),
    (CurrencyName : 'GUYANA 	Guyana Dollar';                    	AlphabeticCode :'GYD';  NumericCode: 328),
    (CurrencyName : 'HAITI 	Gourde';                              AlphabeticCode :'HTG';  NumericCode: 332),
    (CurrencyName : 'HAITI 	US Dollar';                           AlphabeticCode :'USD';  NumericCode: 840),
    (CurrencyName : 'HEARD ISLAND AND MCDONALD ISLANDS 	Australian Dollar'; AlphabeticCode :'AUD';  NumericCode: 036),
    (CurrencyName : 'HOLY SEE (VATICAN CITY STATE) 	Euro';        AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'HONDURAS 	Lempira';                         AlphabeticCode :'HNL';  NumericCode: 340),
    (CurrencyName : 'HONG KONG 	Hong Kong Dollar';                AlphabeticCode :'HKD';  NumericCode: 344),
    (CurrencyName : 'HUNGARY 	Forint';                            AlphabeticCode :'HUF';  NumericCode: 348),
    (CurrencyName : 'ICELAND 	Iceland Krona';                     AlphabeticCode :'ISK';  NumericCode: 352),
    (CurrencyName : 'INDIA 	Indian Rupee';                        AlphabeticCode :'INR';  NumericCode: 356),
    (CurrencyName : 'INDONESIA 	Rupiah';                          AlphabeticCode :'IDR';  NumericCode: 360),
    (CurrencyName : 'IRAN, ISLAMIC REPUBLIC OF 	Iranian Rial';    AlphabeticCode :'IRR';  NumericCode: 364),
    (CurrencyName : 'IRAQ 	Iraqi Dinar';                         AlphabeticCode :'IQD';  NumericCode: 368),
    (CurrencyName : 'IRELAND 	Euro';                              AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'ISLE OF MAN 	Pound Sterling';                AlphabeticCode :'GBP';  NumericCode: 826),
    (CurrencyName : 'ISRAEL 	New Israeli Sheqel';                AlphabeticCode :'ILS';  NumericCode: 376),
    (CurrencyName : 'ITALY 	Euro';                                AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'JAMAICA 	Jamaican Dollar';                   AlphabeticCode :'JMD';  NumericCode: 388),
    (CurrencyName : 'JAPAN 	Yen';                                 AlphabeticCode :'JPY';  NumericCode: 392),
    (CurrencyName : 'JERSEY 	Pound Sterling';                    AlphabeticCode :'GBP';  NumericCode: 826),
    (CurrencyName : 'JORDAN 	Jordanian Dinar';                   AlphabeticCode :'JOD';  NumericCode: 400),
    (CurrencyName : 'KAZAKHSTAN 	Tenge';                         AlphabeticCode :'KZT';  NumericCode: 398),
    (CurrencyName : 'KENYA 	Kenyan Shilling';                     AlphabeticCode :'KES';  NumericCode: 404),
    (CurrencyName : 'KIRIBATI 	Australian Dollar';               AlphabeticCode :'AUD';  NumericCode: 036),
    (CurrencyName : 'KOREA, DEMOCRATIC PEOPLE''S REPUBLIC OF 	North Korean Won'; AlphabeticCode :'KPW';  NumericCode: 408),
    (CurrencyName : 'KOREA, REPUBLIC OF 	Won';                   AlphabeticCode :'KRW';  NumericCode: 410),
    (CurrencyName : 'KUWAIT 	Kuwaiti Dinar';                     AlphabeticCode :'KWD';  NumericCode: 414),
    (CurrencyName : 'KYRGYZSTAN 	Som';                           AlphabeticCode :'KGS';  NumericCode: 417),
    (CurrencyName : 'LAO PEOPLE''S DEMOCRATIC REPUBLIC 	Kip';     AlphabeticCode :'LAK';  NumericCode: 418),
    (CurrencyName : 'LATVIA 	Latvian Lats';                      AlphabeticCode :'LVL';  NumericCode: 428),
    (CurrencyName : 'LEBANON 	Lebanese Pound';                    AlphabeticCode :'LBP';  NumericCode: 422),
    (CurrencyName : 'LESOTHO 	Rand';                              AlphabeticCode :'ZAR';  NumericCode: 710),
    (CurrencyName : 'LESOTHO 	Loti';                              AlphabeticCode :'LSL';  NumericCode: 426),
    (CurrencyName : 'LIBERIA 	Liberian Dollar';                   AlphabeticCode :'LRD';  NumericCode: 430),
    (CurrencyName : 'LIBYAN ARAB JAMAHIRIYA 	Libyan Dinar';      AlphabeticCode :'LYD';  NumericCode: 434),
    (CurrencyName : 'LIECHTENSTEIN 	Swiss Franc';                 AlphabeticCode :'CHF';  NumericCode: 756),
    (CurrencyName : 'LITHUANIA 	Lithuanian Litas';                AlphabeticCode :'LTL';  NumericCode: 440),
    (CurrencyName : 'LUXEMBOURG 	Euro';                          AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'MACAO 	Pataca';                              AlphabeticCode :'MOP';  NumericCode: 446),
    (CurrencyName : 'MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF 	Denar'; AlphabeticCode :'MKD';  NumericCode: 807),
    (CurrencyName : 'MADAGASCAR 	Malagasy Ariary';               AlphabeticCode :'MGA';  NumericCode: 969),
    (CurrencyName : 'MALAWI 	Kwacha';                            AlphabeticCode :'MWK';  NumericCode: 454),
    (CurrencyName : 'MALAYSIA 	Malaysian Ringgit';               AlphabeticCode :'MYR';  NumericCode: 458),
    (CurrencyName : 'MALDIVES 	Rufiyaa';                         AlphabeticCode :'MVR';  NumericCode: 462),
    (CurrencyName : 'MALI 	CFA Franc BCEAO';                     AlphabeticCode :'XOF';  NumericCode: 952),
    (CurrencyName : 'MALTA 	Euro';                                AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'MARSHALL ISLANDS 	US Dollar';               AlphabeticCode :'USD';  NumericCode: 840),
    (CurrencyName : 'MARTINIQUE 	Euro';                          AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'MAURITANIA 	Ouguiya';                       AlphabeticCode :'MRO';  NumericCode: 478),
    (CurrencyName : 'MAURITIUS 	Mauritius Rupee';                 AlphabeticCode :'MUR';  NumericCode: 480),
    (CurrencyName : 'MAYOTTE 	Euro';                              AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'MEXICO 	Mexican Peso';                      AlphabeticCode :'MXN';  NumericCode: 848),
    (CurrencyName : 'Mexican Unidad de Inversion (UDI) 	MXN';     AlphabeticCode :'MXV';  NumericCode: 979),
    (CurrencyName : 'MICRONESIA, FEDERATED STATES OF 	US Dollar'; AlphabeticCode :'USD';  NumericCode: 840),
    (CurrencyName : 'MOLDOVA, REPUBLIC OF 	Moldovan Leu';        AlphabeticCode :'MDL';  NumericCode: 498),
    (CurrencyName : 'MONACO 	Euro';                              AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'MONGOLIA 	Tugrik';                          AlphabeticCode :'MNT';  NumericCode: 496),
    (CurrencyName : 'MONTENEGRO 	Euro';                          AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'MONTSERRAT 	East Caribbean Dollar';         AlphabeticCode :'XCD';  NumericCode: 951),
    (CurrencyName : 'MOROCCO 	Moroccan Dirham';                   AlphabeticCode :'MAD';  NumericCode: 504),
    (CurrencyName : 'MOZAMBIQUE 	Metical';                       AlphabeticCode :'MZN';  NumericCode: 943),
    (CurrencyName : 'MYANMAR 	Kyat';                              AlphabeticCode :'MMK';  NumericCode: 104),
    (CurrencyName : 'NAMIBIA 	Rand';                              AlphabeticCode :'ZAR';  NumericCode: 710),
    (CurrencyName : 'Namibia Dollar';                             AlphabeticCode :'NAD';  NumericCode: 516),
    (CurrencyName : 'NAURU 	Australian Dollar'; 	                AlphabeticCode :'AUD';  NumericCode: 036),
    (CurrencyName : 'NEPAL 	Nepalese Rupee';                     	AlphabeticCode :'NPR';  NumericCode: 524),
    (CurrencyName : 'NETHERLANDS 	Euro';                          AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'NETHERLANDS ANTILLES 	Netherlands Antillian Guilder'; AlphabeticCode :'ANG';  NumericCode: 532),
    (CurrencyName : 'NEW CALEDONIA 	CFP Franc';                   AlphabeticCode :'XPF';  NumericCode: 953),
    (CurrencyName : 'NEW ZEALAND 	New Zealand Dollar';            AlphabeticCode :'NZD';  NumericCode: 554),
    (CurrencyName : 'NICARAGUA 	Cordoba Oro';                     AlphabeticCode :'NIO';  NumericCode: 558),
    (CurrencyName : 'NIGER 	CFA Franc BCEAO';                     AlphabeticCode :'XOF';  NumericCode: 952),
    (CurrencyName : 'NIGERIA 	Naira';                             AlphabeticCode :'NGN';  NumericCode: 566),
    (CurrencyName : 'NIUE 	New Zealand Dollar';                  AlphabeticCode :'NZD';  NumericCode: 554),
    (CurrencyName : 'NORFOLK ISLAND 	Australian Dollar';         AlphabeticCode :'AUD';  NumericCode: 036),
    (CurrencyName : 'NORTHERN MARIANA ISLANDS 	US Dollar';       AlphabeticCode :'USD';  NumericCode: 840),
    (CurrencyName : 'NORWAY 	Norwegian Krone';                   AlphabeticCode :'NOK';  NumericCode: 578),
    (CurrencyName : 'OMAN 	Rial Omani';                          AlphabeticCode :'OMR';  NumericCode: 512),
    (CurrencyName : 'PAKISTAN 	Pakistan Rupee';                  AlphabeticCode :'PKR';  NumericCode: 586),
    (CurrencyName : 'PALAU 	US Dollar';                           AlphabeticCode :'USD';  NumericCode: 840),
    (CurrencyName : 'PANAMA 	Balboa';                            AlphabeticCode :'PAB';  NumericCode: 590),
    (CurrencyName : 'PANAMA 	US Dollar';                         AlphabeticCode :'USD';  NumericCode: 840),
    (CurrencyName : 'PAPUA NEW GUINEA 	Kina';                    AlphabeticCode :'PGK';  NumericCode: 598),
    (CurrencyName : 'PARAGUAY 	Guarani';                         AlphabeticCode :'PYG';  NumericCode: 600),
    (CurrencyName : 'PERU 	Nuevo Sol';                           AlphabeticCode :'PEN';  NumericCode: 604),
    (CurrencyName : 'PHILIPPINES 	Philippine Peso';               AlphabeticCode :'PHP';  NumericCode: 608),
    (CurrencyName : 'PITCAIRN 	New Zealand Dollar';              AlphabeticCode :'NZD';  NumericCode: 554),
    (CurrencyName : 'POLAND 	Zloty';                             AlphabeticCode :'PLN';  NumericCode: 985),
    (CurrencyName : 'PORTUGAL 	Euro';                            AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'PUERTO RICO 	US Dollar';                     AlphabeticCode :'USD';  NumericCode: 840),
    (CurrencyName : 'QATAR 	Qatari Rial';                         AlphabeticCode :'QAR';  NumericCode: 634),
    (CurrencyName : 'RÉUNION 	Euro';                              AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'ROMANIA 	New Leu';                           AlphabeticCode :'RON';  NumericCode: 946),
    (CurrencyName : 'RUSSIAN FEDERATION 	Russian Ruble';         AlphabeticCode :'RUB';  NumericCode: 643),
    (CurrencyName : 'RWANDA 	Rwanda Franc';                      AlphabeticCode :'RWF';  NumericCode: 646),
    (CurrencyName : 'SAINT-BARTHÉLEMY 	Euro';                    AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'SAINT HELENA, ASCENSION AND TRISTAN DA CUNHA Saint Helena Pound'; AlphabeticCode :'SHP';  NumericCode:	654),
    (CurrencyName : 'SAINT KITTS AND NEVIS 	East Caribbean Dollar';AlphabeticCode :'XCD'; NumericCode: 951),
    (CurrencyName : 'SAINT LUCIA 	East Caribbean Dollar';         AlphabeticCode :'XCD';  NumericCode: 951),
    (CurrencyName : 'SAINT MARTIN 	Euro';                        AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'SAINT PIERRE AND MIQUELON 	Euro';            AlphabeticCode :'EUR';  NumericCode: 978),
    (CurrencyName : 'SAINT VINCENT AND THE GRENADINES 	East Caribbean Dollar';AlphabeticCode :'XCD';  NumericCode:951),
    (CurrencyName : 'SAMOA 	Tala';                                AlphabeticCode :'WST';  NumericCode:882),
    (CurrencyName : 'SAN MARINO 	Euro';                          AlphabeticCode :'EUR';  NumericCode:978),
    (CurrencyName : 'SÃO TOME AND PRINCIPE 	Dobra';               AlphabeticCode :'STD';  NumericCode:678),
    (CurrencyName : 'SAUDI ARABIA 	Saudi Riyal';                 AlphabeticCode :'SAR';  NumericCode:682),
    (CurrencyName : 'SENEGAL 	CFA Franc BCEAO';                   AlphabeticCode :'XOF';  NumericCode:952),
    (CurrencyName : 'SERBIA 	Serbian Dinar';                     AlphabeticCode :'RSD';  NumericCode:941),
    (CurrencyName : 'SEYCHELLES 	Seychelles Rupee';              AlphabeticCode :'SCR';  NumericCode:690),
    (CurrencyName : 'SIERRA LEONE 	Leone';                       AlphabeticCode :'SLL';  NumericCode:694),
    (CurrencyName : 'SINGAPORE 	Singapore Dollar';                AlphabeticCode :'SGD';  NumericCode:702),
    (CurrencyName : 'SLOVAKIA 	Euro';                            AlphabeticCode :'EUR';  NumericCode:978),
    (CurrencyName : 'SLOVENIA 	Euro';                            AlphabeticCode :'EUR';  NumericCode:978),
    (CurrencyName : 'SOLOMON ISLANDS 	Solomon Islands Dollar';    AlphabeticCode :'SBD';  NumericCode:090),
    (CurrencyName : 'SOMALIA 	Somali Shilling';                   AlphabeticCode :'SOS';  NumericCode:706),
    (CurrencyName : 'SOUTH AFRICA 	Rand';                        AlphabeticCode :'ZAR';  NumericCode:710),
    (CurrencyName : 'SPAIN 	Euro';                                AlphabeticCode :'EUR';  NumericCode:978),
    (CurrencyName : 'SRI LANKA 	Sri Lanka Rupee';                 AlphabeticCode :'LKR';  NumericCode:144),
    (CurrencyName : 'SUDAN 	Sudanese Pound';                      AlphabeticCode :'SDG';  NumericCode:938),
    (CurrencyName : 'SURINAME 	Surinam Dollar';                  AlphabeticCode :'SRD';  NumericCode:968),
    (CurrencyName : 'SVALBARD AND JAN MAYEN 	Norwegian Krone';   AlphabeticCode :'NOK';  NumericCode:578),
    (CurrencyName : 'SWAZILAND 	Lilangeni';                       AlphabeticCode :'SZL';  NumericCode:748),
    (CurrencyName : 'SWEDEN 	Swedish Krona';                     AlphabeticCode :'SEK';  NumericCode:752),
    (CurrencyName : 'SWITZERLAND 	Swiss Franc';                   AlphabeticCode :'CHF';  NumericCode:756),
    (CurrencyName : 'SWITZERLAND 	WIR Franc';                     AlphabeticCode :'CHW';  NumericCode:948),
    (CurrencyName : 'SWITZERLAND 	WIR Euro';                      AlphabeticCode :'CHE';  NumericCode:947),
    (CurrencyName : 'SYRIAN ARAB REPUBLIC 	Syrian Pound';        AlphabeticCode :'SYP';  NumericCode:760),
    (CurrencyName : 'TAIWAN, PROVINCE OF CHINA 	New Taiwan Dollar';AlphabeticCode :'TWD';  NumericCode:901),
    (CurrencyName : 'TAJIKISTAN 	Somoni';                        AlphabeticCode :'TJS';  NumericCode:972),
    (CurrencyName : 'TANZANIA, UNITED REPUBLIC OF 	Tanzanian Shilling';AlphabeticCode :'TZS';  NumericCode:834),
    (CurrencyName : 'THAILAND 	Baht';                            AlphabeticCode :'THB';  NumericCode:764),
    (CurrencyName : 'TIMOR-LESTE 	US Dollar';                     AlphabeticCode :'USD';  NumericCode:840),
    (CurrencyName : 'TOGO 	CFA Franc BCEAO';                     AlphabeticCode :'XOF';  NumericCode:952),
    (CurrencyName : 'TOKELAU 	New Zealand Dollar';                AlphabeticCode :'NZD';  NumericCode:554),
    (CurrencyName : 'TONGA 	Pa''anga';                            AlphabeticCode :'TOP';  NumericCode:776),
    (CurrencyName : 'TRINIDAD AND TOBAGO 	Trinidad and Tobago Dollar';AlphabeticCode :'TTD';  NumericCode:780),
    (CurrencyName : 'TUNISIA 	Tunisian Dinar';                    AlphabeticCode :'TND';  NumericCode:788),
    (CurrencyName : 'TURKEY 	Turkish Lira';                      AlphabeticCode :'TRY';  NumericCode:949),
    (CurrencyName : 'TURKMENISTAN 	Manat';                       AlphabeticCode :'TMT';  NumericCode:934),
    (CurrencyName : 'TURKS AND CAICOS ISLANDS 	US Dollar';       AlphabeticCode :'USD';  NumericCode:840),
    (CurrencyName : 'TUVALU 	Australian Dollar';                 AlphabeticCode :'AUD';  NumericCode:036),
    (CurrencyName : 'UGANDA 	Uganda Shilling';                   AlphabeticCode :'UGX';  NumericCode:800),
    (CurrencyName : 'UKRAINE 	Hryvnia';                           AlphabeticCode :'UAH';  NumericCode:980),
    (CurrencyName : 'UNITED ARAB EMIRATES 	UAE Dirham';          AlphabeticCode :'AED';  NumericCode:784),
    (CurrencyName : 'UNITED KINGDOM 	Pound Sterling';            AlphabeticCode :'GBP';  NumericCode:826),
    (CurrencyName : 'UNITED STATES 	US Dollar';                   AlphabeticCode :'USD';  NumericCode:840),
    (CurrencyName : 'UNITED STATES  US Dollar (Same day)';        AlphabeticCode :'USS';  NumericCode:998),
    (CurrencyName : 'UNITED STATES US Dollar (Next day)'; 	      AlphabeticCode :'USN';  NumericCode:997),
    (CurrencyName : 'UNITED STATES MINOR OUTLYING ISLANDS 	US Dollar'; AlphabeticCode :'USD';  NumericCode:840),
    (CurrencyName : 'URUGUAY 	Peso Uruguyo';                      AlphabeticCode :'UYU';  NumericCode:858),
    (CurrencyName : 'Uruguay Peso en Unidades Indexadas';         AlphabeticCode :'UYI';  NumericCode:940),
    (CurrencyName : 'UZBEKISTAN 	Uzbekistan Sum';                AlphabeticCode :'UZS';  NumericCode:860),
    (CurrencyName : 'VANUATU 	Vatu';                              AlphabeticCode :'VUV';  NumericCode:548),
    (CurrencyName : 'VATICAN CITY STATE (Holy See) 	Euro';        AlphabeticCode :'EUR';  NumericCode:978),
    (CurrencyName : 'VENEZUELA 	Bolivar Fuerte';                  AlphabeticCode :'VEF';  NumericCode:937),
    (CurrencyName : 'VIET NAM 	Dong';                            AlphabeticCode :'VND';  NumericCode:704),
    (CurrencyName : 'VIRGIN ISLANDS (BRITISH) 	US Dollar';       AlphabeticCode :'USD';  NumericCode:840),
    (CurrencyName : 'VIRGIN ISLANDS (U.S.) 	US Dollar';           AlphabeticCode :'USD';  NumericCode:840),
    (CurrencyName : 'WALLIS AND FUTUNA 	CFP Franc';               AlphabeticCode :'XPF';  NumericCode:953),
    (CurrencyName : 'WESTERN SAHARA 	Moroccan Dirham';           AlphabeticCode :'MAD';  NumericCode:504),
    (CurrencyName : 'YEMEN 	Yemeni Rial';                         AlphabeticCode :'YER';  NumericCode:886),
    (CurrencyName : 'ZAMBIA 	Zambian Kwacha';                    AlphabeticCode :'ZMK';  NumericCode:894),
    (CurrencyName : 'ZIMBABWE 	Zimbabwe Dollar';                 AlphabeticCode :'ZWL';  NumericCode:932),
    (CurrencyName : 'Gold';                                       AlphabeticCode :'XAU';  NumericCode:959),
    (CurrencyName : 'Bond Markets Units European Composite Unit (EURCO)'; AlphabeticCode :'XBA';  NumericCode:955),
    (CurrencyName : 'European Monetary Unit (E.M.U.-6)';          AlphabeticCode :'XBB';  NumericCode:956),
    (CurrencyName : 'European Unit of Account 9(E.U.A.-9)';       AlphabeticCode :'XBC';  NumericCode:957),
    (CurrencyName : 'European Unit of Account 17(E.U.A.-17)';     AlphabeticCode :'XBD';  NumericCode:958),
    (CurrencyName : 'INTERNATIONAL MONETARY FUND (I.M.F) 	SDR';   AlphabeticCode :'XDR';  NumericCode:960),
    (CurrencyName : 'Palladium';                                  AlphabeticCode :'XPD';  NumericCode:964),
    (CurrencyName : 'Platinum';                                   AlphabeticCode :'XPT';  NumericCode:962),
    (CurrencyName : 'Silver';                                     AlphabeticCode :'XAG';  NumericCode:961),
    (CurrencyName : 'UIC-Franc';                                  AlphabeticCode :'XFU';  NumericCode:0),
    (CurrencyName : 'Codes specifically reserved for testing purposes'; AlphabeticCode :'XTS';  NumericCode:963),
    (CurrencyName : 'The codes assigned for transactions where no currency is involved are:';AlphabeticCode :'XXX';  NumericCode:999)
  );


procedure TpsGS1.Assign(Source: TPersistent);
begin
  if Source is TpsGS1 then begin
      FFNC1Type   := TpsGS1(Source).FFNC1Type;
      FFNC1Input  := TpsGS1(Source).FFNC1Input;
  end
    else
      inherited;
end;

class function TpsGS1.CalcCheckDigit(var s: String; totalLen: Integer;
  version: TpsChecksum): Boolean;
begin
  Result := False;
end;

constructor TpsGS1.CreateBarcode(AOwner: TComponent);
begin
    inherited Create;
    FBarcodeComponent := AOwner;
end;

class function TpsGS1.idxOK(idx:Integer): Boolean;
begin
  Result:= (idx>=Low(GS1_AI)) and (idx<=High(GS1_AI));
end;

class function TpsGS1.LocateAI(const ai:String): Integer;
var idx, len : Integer;
begin
    Result := -1;
    len    := Length(ai);
    for idx  := Low(GS1_AI) to High(GS1_AI) do
          if Length(GS1_AI[idx].AI)=len then
            if (GS1_AI[idx].AI=ai)
              or ( (Copy(GS1_AI[idx].AI,len,1)='X') and (Copy(GS1_AI[idx].AI,1,len-1)=Copy(ai,1,len-1)) ) then begin
                Result := idx;
                Break;
          end;
end;

class function TpsGS1.VerifyAI(const ai, value:String; var err:TpsErrRecord): Boolean;
var idx:Integer;
begin
    idx := LocateAI(ai);
    if idx<0 then Result:=False
    else
      Result:=VerifyAI(idx, Value, err);
end;

class function TpsGS1.VerifyAI(ai_idx:Integer; const value:String;
    var err:TpsErrRecord): Boolean;
var len:Integer;
begin
    Result := False;
    len := Length(Value);
    if (len<GS1_AI[ai_idx].LenMin) or (len>GS1_AI[ai_idx].LenMax) then begin
            Err.Err       := erGS1_BadValueLength;
            Err.StrPar    := String(value);
            Err.Par1      := GS1_AI[ai_idx].LenMin;
            Err.Par2      := GS1_AI[ai_idx].LenMax;
            Exit;
    end;

    if (GS1_AI[ai_idx].AIType in [aiInteger, aiDate]) and not IsNumeric(value) then begin
            Err.Err       := erGS1_ValueMustBeInteger;
            Err.StrPar    := value;
            Err.Par1      := 0;
            Err.Par2      := 0;
            Exit;
    end;
    Result := True;
end;

class function TpsGS1.Verify(s:String; var ErrRecord:TpsErrRecord ):Integer;
var i,j,k,idx : Integer;
    ai,value  : String;
begin
  ErrRecord.Err := erOK;
  i             := 1;
  Result        := 0;

  while i<=Length(s) do begin
    if s[i]<>GS1_AI_START then begin
        ErrRecord.Err       := erGS1;
        ErrRecord.Position  := i;
        Break;
    end;
    j:=PosEx(GS1_AI_END,s,i);
    if j<=0 then begin
        ErrRecord.Err       := erGS1;
        ErrRecord.Position  := i;
        Break;
    end;

    ai   := Copy(s,i+1,j-i-1);
    if not isNumeric(ai) then begin
        ErrRecord.Err       := erGS1_AI_MustBeInteger;
        ErrRecord.Position  := i;
        ErrRecord.StrPar    := ai;
        Break;
    end;

    // k    := Pos('(',Copy(s,j+1,Length(s)-j-1));
    k := PosEx(GS1_AI_START, s, j+1);
    if k>0 then begin
      i     := k;
      value := Copy(s,j+1,k-j-1);
    end else begin
      i     := Length(s)+1;
      Value := Copy(s,j+1, Length(s)-j);
    end;

    idx := LocateAI(ai);
    if idx<0 then begin
      ErrRecord.Err       := erGS1_AI_NotFound;
      ErrRecord.Position  := i;
      ErrRecord.StrPar    := ai;
      Break;
    end;

    if not VerifyAI(idx, Value, ErrRecord) then begin
      ErrRecord.Position  := j+1;
      Break;
    end;

    Inc(Result);
  end;
end;

class function TpsGS1.GetElement(const s:String; idx:Integer; var ai, value:String): Integer;
var i,j:Integer;
begin
  Result:= -1;
  i     := 1;
  ai    := '';
  value := '';
  while i<=Length(s) do begin
    if Copy(s,i,1)=GS1_AI_START then begin
      Dec(idx);
      if idx=0 then Break;
    end;
    Inc(i);
  end;
  if idx>0 then Exit;
  j:=PosEx(GS1_AI_END,s,i+1);
  if j<=0 then Exit;
  ai:=Copy(s,i+1,j-i-1);
  i:=PosEx(GS1_AI_START,s,j+1);
  if i>0 then Value := Copy(s,j+1,i-j-1)
  else        Value := Copy(s,j+1,Length(s)-j);
  Result := LocateAI(ai);
end;

class function TpsGS1.GetElementVisual(idx:Integer; const ai,value:String;
        var errRecord:TpsErrRecord):String;
var DecPos:Integer;
begin
  Result:=value;
  if not idxOK(idx) then Exit;
  case GS1_AI[idx].AIType of
    aiFloat : begin
          DecPos:=Ord(ai[Length(ai)])-Ord0;
          if DecPos>0 then
              Result:=Copy(Value,1,Length(Value)-DecPos)+'.'+Copy(Value,Length(Value)-DecPos+1,DecPos);
        end;
    aiDate : Result := DecodeDateString(Value);
  end;
end;

class function TpsGS1.GetDescription(const s:String): String;
var i,cnt,idx:Integer;
    ai,value:String;
    err : TpsErrRecord;
begin
  Result := '';
  cnt    := Verify(s,err);

  for I := 1 to cnt do begin
    idx   := GetElement(s,i,ai,value);
    value := GetElementVisual(idx, ai, value, err);
    if idxOK(idx) then begin
      if i>1 then
        Result:=Result+psCRLF;
      Result:=Result+GS1_AI[idx].Title+':'+Value;
    end else
      Result:=Result+ai+':???';
  end;
end;

class function TpsGS1.Prepare(const s:String; var ErrRecord:TpsErrRecord):String;
var i,cnt,idx : Integer;
    ai,value  : String;
begin
  Result := '';
  cnt    := Verify(s,ErrRecord);
  if ErrRecord.Err=erOK then begin
    for i := 1 to cnt do begin
      idx:=GetElement(s,i,ai,value);
      if idxOK(idx)  then
        Result:=Result+ai+value;
        if (i<>cnt) and (Length(value)<>GS1_AI[idx].LenMax) then
          Result := Result + Char(GS1_GroupSeparator);
    end;
  end;
end;

class function TpsGS1.PrepareTilde(const s:String; var ErrRecord:TpsErrRecord):String;
  function IsSeparator(c:Char):Boolean;
  begin
    Result := (c=GS1_TILDE) or (c=GS1_BRACKET);
  end;
var i:Integer;
begin
  Result := '';
  i      :=  1;
  while i<=Length(s) do begin
    if (i>1) and IsSeparator(s[i]) then begin
        if i=Length(s) then begin
            // Result := Result + s[i]
            // if last char is separator, ignore
        end else begin
          if IsSeparator(s[i+1]) then begin
            Result := Result + s[i];
            Inc(i);
          end else
            Result := Result + GS1_GroupSeparator;
        end;
    end else
        Result := Result + s[i];
    Inc(i);
  end;
end;

// replace %% with %, and signle % with GS1_GroupSeparator
class function TpsGS1.PreparePerc (const s:String; var ErrRecord:TpsErrRecord):String;
var i,dx:Integer;
    c   :Char;
begin
    ErrRecord.Err := erOK;
    Result    := '';
    i         := 1;
    while i<=Length(s) do begin
        c  := s[i];
        dx := 1;
        if c=GS1_Perc then begin
            c:=GS1_GroupSeparator;
            if i<Length(s) then
              if s[i+1]=GS1_Perc then begin
                  c  := GS1_Perc;
                  dx := 2;
              end;
        end;
        Result:=Result+c;
        Inc(i,dx);
    end;
end;


class function TpsGS1.DecodeDateString(const s:String): String;
var d,m,y:Integer;
begin
  m:=StrToInt(copy(s,3,2));
  d:=StrToInt(copy(s,5,2));
  y:=StrToInt(copy(s,1,2));
  if y>50 then Inc(y,1900)
  else         Inc(y,2000);
  Result := IntToStr(d)+'.'+IntToStr(m)+'.'+IntToStr(y);
end;


function IsSpecial(const s:String; var index:Integer; var SpecialRec:TpsSpecialResult): Boolean;
var i,j,k   : integer;
    cmd,val : string;
begin
    Result  := False;
    SpecialRec.Symbol := scASCII;
    SpecialRec.Error  := False;
    if Index>Length(s) then begin
        SpecialRec.Error  := True;
        Exit;
    end;

    if Copy(s,index,1)<>constSprecialStart then begin
        SpecialRec.Symbol       := scASCII;
        SpecialRec.ValueString  := Copy(s,index,1);
        SpecialRec.ValueInteger := Ord(s[index]);
        SpecialRec.NextIndex    := Index+1;
        Exit;
    end;

    if Copy(s,index,2)=constSprecialStart+constSprecialStart then begin
      SpecialRec.Symbol       := scASCII;
      SpecialRec.ValueInteger := Ord(constSprecialStart);
      SpecialRec.ValueString  := constSprecialStart;
      SpecialRec.NextIndex    := Index+2;
      Exit;
    end;

    j:=PosEx(constSprecialStop, s, index+1);
    if j<=0 then begin
      SpecialRec.Error := True;
      Exit;
    end;

    SpecialRec.NextIndex := j+1;
    cmd := Copy(s,index+1, j-index-1);
    k   := Pos('=',cmd);
    if k>0 then begin
            val:=Copy(cmd,k+1,Length(cmd)-k);
            cmd:=Copy(cmd,1,k-1);
    end else
            val:='';

    for i:=Low(tblSpecialSymbols) to High(tblSpecialSymbols) do begin
      if CompareText(tblSpecialSymbols[i].Text, cmd) = 0 then begin
        SpecialRec.Symbol      := tblSpecialSymbols[i].Symbol;
        SpecialRec.Cmd         := cmd;
        if SpecialRec.Symbol=scASCII then begin
            SpecialRec.ValueInteger := tblSpecialSymbols[i].Value;
            SpecialRec.ValueString  := Char(SpecialRec.ValueInteger);
        end else begin
            SpecialRec.ValueString := val;
            if tblSpecialSymbols[i].Params = stInteger then
                SpecialRec.ValueInteger := StrToIntDef(val,-1);
        end;

        SpecialRec.Error  := False;
        Result            := True;
        Break;
      end;
    end;
end;

function RemoveCommands(const s:String):String;
var idx : integer;
    SR  : TpsSpecialResult;
    //std : Boolean;
begin
    idx:=1;
    Result:='';
    while idx<=Length(s) do begin
        //std := IsSpecial(s, idx, SR);
        if SR.Symbol=scASCII then
            Result:=Result+SR.ValueString;
        idx:=SR.NextIndex;
    end;
end;

function GS1_CalcCheckDigit(var s:String; totalLen:Integer; version:TpsChecksum): Boolean;
const
  GS1_check2      : array [0..9] of integer = (0,2,4,6,8,9,1,3,5,7);
  GS1_check3      : array [0..9] of integer = (0,3,6,9,2,5,8,1,4,7);
  GS1_check5Plus  : array [0..9] of integer = (0,5,1,6,2,7,3,8,4,9);
  GS1_check5Minus : array [0..9] of integer = (0,5,9,4,8,3,7,2,6,1);
var i, ver, last, CheckSum : Integer;
begin
  CheckSum  := 0;
  Result    := True;
  last      := Length(s);
  i         := 0;
  case version of
    chNone        : Exit;
    chGS1_Standard    : begin
        ver  := 3;
        i:=Last-1;
        while i>=1 do begin
            Inc(CheckSum, ver*AsDigit(s[i]));
            if ver=1 then
                ver := 3
            else
                ver := 1;
            Dec(i);
        end;
        CheckSum := 10-(CheckSum mod 10);
      end;
    chGS1_4DigitPrice : begin
        if Last<>5 then begin
          Result := False;
          Exit;
        end;
        CheckSum:=  GS1_Check2[AsDigit(s[1])]
                +   GS1_Check2[AsDigit(s[2])]
                +   GS1_Check3[AsDigit(s[3])]
                +   GS1_Check5Minus[AsDigit(s[4])];
        CheckSum := (3*CheckSum) mod 10;
      end;
    chGS1_5DigitPrice : begin
        if Last<>6 then begin
          Result := False;
          Exit;
        end;
        CheckSum:=  GS1_Check5Plus[AsDigit(s[1])]
                +   GS1_Check2[AsDigit(s[2])]
                +   GS1_Check5Minus[AsDigit(s[3])]
                +   GS1_Check5Plus[AsDigit(s[4])]
                +   GS1_Check2[AsDigit(s[5])];
        CheckSum := (3*CheckSum) mod 10;
      end;
  end;
  if CheckSum=10 then
      s[Last] := AsChar(0)
  else
      s[Last] := AsChar(i);
end;


class function TpsGS1.IsSpecial(const s: String; var index: Integer;
  var SpecialRec: TpsSpecialResult): Boolean;
begin
  Result := False;
end;


class function TpsGS1.RemoveCommands(const s: String): String;
begin
  Result := s;
end;

procedure TpsGS1.SetFNC1Input(const Value: TpsFNC1InputType);
begin
  if FFNC1Input<>Value then begin
      FFNC1Input := Value;
      UpdateBarcode;
  end;
end;

procedure TpsGS1.SetFNC1Type(const Value: TpsFNC1Type);
begin
  if FFNC1Type<>value then begin
    FFNC1Type := Value;
    UpdateBarcode;
  end;
end;

function TpsGS1.SolveGS1(const Barcode: String; var err:TpsErrRecord): String;
var s:string;
begin
    if FNC1Type=fnc1None then
        s:=Barcode
    else
        case FNC1InputType of
          gs1Separators : s := Barcode;
          gs1Percent    : s := PreparePerc(Barcode, err);
          gs1Classic    : s := Prepare(Barcode, err);
          gs1Tilde      : s := PrepareTilde(Barcode, err);
        end;
    Result := s;
end;

procedure TpsGS1.UpdateBarcode;
begin
    psUpdateBarcode(FBarcodeComponent);
end;


function psCode128EncodeA(C:Char):Integer;
begin
  if Ord(c) in [] then

  else
    Result :=-1;
end;

function psCode128EncodeB(C:Char):Integer;
begin
  Result := 0;
end;

//function psCode128EncodeCA(C1, C2:Char):Integer;
//begin
//
//end;


//function psCode128GetCharset(C1,C2:Char):TpsCode128;
//begin
//  Result := '';
//  if IsDigit(C1) and IsDigit(C2)then begin
//    Result := psCodeC;
//    Exit;
//  end;
// end;


end.
