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
unit PDF;

interface

uses Windows, SysUtils, Classes, Graphics, Math, {$IFDEF VER350}Vcl.Imaging.jpeg{$ELSE}jpeg{$ENDIF}, ShellApi, pdfMisc,
  pdfttfCodes;

type
  TDoublePoint = record
    x, y: Extended;
  end;

  TNamedDestRecord = record
    Dest: string;
    Page: Integer;
    TopOffset: Integer;
    ID: Integer;
  end;

  TNamedDestRecordArray = array of TNamedDestRecord;

  TEmbedFileRecord = record
    Title: string;
    FileName: string;
    ID:Integer;
  end;

  TEmbedFileRecordArray = array of TEmbedFileRecord;


  TPDFPageOrientation = ( poPagePortrait, poPageLandscape );
  TPDFVersion = ( v13, v14, v15, v16 );
  TPDFPageType = ( ptPage, ptXForm, ptAnnotation );
  TPDFLineJoin = ( ljMiter, ljRound, ljBevel );
  TPDFLineCap = ( lcButtEnd, lcRound, lcProjectingSquare );
  TPDFSubmitType = ( stGet, stPost, stFDF );

  TViewerPreference = ( vpHideToolBar, vpHideMenuBar, vpHideWindowUI,
    vpFitWindow, vpCenterWindow );
  TViewerPreferences = set of TViewerPreference;

  TAnnotationFlag = ( afInvisible, afHidden, afPrint, afNoZoom, afNoRotate, afNoView, afReadOnly );

  TAnnotationFlags = set of TAnnotationFlag;
  TEncripKey = array [ 1..16 ] of Byte;

  THorJust = ( hjLeft, hjCenter, hjRight );
  TVertJust = ( vjUp, vjCenter, vjDown );

  TCompressionType = ( ctNone, ctFlate );
  TImageCompressionType = ( itcFlate, itcJpeg, itcCCITT3, itcCCITT32d, itcCCITT4 );
  TPDFCriptoOption = ( coPrint, coModifyStructure, coCopyInformation,
    coModifyAnnotation, coPrintHi, coFillAnnotation,
    coExtractInfo, coAssemble );
  TPDFCtiptoOptions = set of TPDFCriptoOption;
  TPDFKeyLength = ( kl40, kl128 );

  TPageLayout = ( plSinglePage, plOneColumn, plTwoColumnLeft, plTwoColumnRight );

  TPageMode = ( pmUseNone, pmUseOutlines, pmUseThumbs, pmFullScreen );
  TPDFPageRotate = ( pr0, pr90, pr180, pr270 );

  TPDFANSICharacterWidth = array [ 0..255 ] of Integer;
  PPDFANSICharacterWidth = ^TPDFANSICharacterWidth;

  TPDFException = class ( Exception );

  TPDFDocument = class;
  TPDFFonts = class;
  TPDFImages = class;
  TPDFImage = class;
  TPDFPage = class;
  TPDFFont = class;

  TPDFControlHint = class ( TObject )
  private
    FCaption: string;
    FCharset: TFontCharset;
  public
    property Caption: string read FCaption write FCaption;
    property Charset: TFontCharset read FCharset write FCharset;
  end;

  TPDFControlFont = class
  private
    FSize: Integer;
    FName: string;
    FColor: TColor;
    FStyle: TFontStyles;
    procedure SetSize ( const Value: Integer );
  public
    constructor Create;
    property Name: string read FName write FName;
    property Style: TFontStyles read FStyle write FStyle;
    property Size: Integer read FSize write SetSize;
    property Color: TColor read FColor write FColor;
  end;

  TPDFPageSize = ( psUserDefined, psLetter, psA4, psA3, psLegal, psB5, psC5, ps8x11, psB4,
    psA5, psFolio, psExecutive, psEnvB4, psEnvB5, psEnvC6, psEnvDL, psEnvMonarch,
    psEnv9, psEnv10, psEnv11 );

  TPDFDocInfo = class ( TPersistent )
  private
    FProducer: string;
    ID: Integer;
    FKeywords: string;
    FTitle: string;
    FCreator: string;
    FAuthor: string;
    FSubject: string;
    FCreationDate: TDateTime;
    procedure SetAuthor ( const Value: string );
    procedure SetCreationDate ( const Value: TDateTime );
    procedure SetCreator ( const Value: string );
    procedure SetKeywords ( const Value: string );
    procedure SetSubject ( const Value: string );
    procedure SetTitle ( const Value: string );
  public
    property CreationDate: TDateTime read FCreationDate write SetCreationDate;
    property Producer: string read FProducer write FProducer;
  published
    property Author: string read FAuthor write SetAuthor;
    property Creator: string read FCreator write SetCreator;
    property Keywords: string read FKeywords write SetKeywords;
    property Subject: string read FSubject write SetSubject;
    property Title: string read FTitle write SetTitle;
  end;

  TTextAnnotationIcon = ( taiComment, taiKey, taiNote, taiHelp, taiNewParagraph, taiParagraph, taiInsert );
  TPDFControl = class;

  TPDFControls = class
  private
    FControls: TList;
    function GetCount: Integer;
    function GetControl ( Index: Integer ): TPDFControl;
  public
    constructor Create;
    destructor Destroy; override;
    function Add ( Control: TPDFControl ): Integer;
    function IndexOf ( Control: TPDFControl ): Integer;
    procedure Delete ( Control: TPDFControl );
    procedure Clear;
    property Count: Integer read GetCount;
    property Items [ Index: Integer ]: TPDFControl read GetControl; default;
  end;

  TPDFAction = class ( TObject )
  private
    FOwner: TPDFDocument;
    ActionID: Integer;
    FNext: TPDFAction;
    procedure Prepare;
    procedure Save; virtual; abstract;
  public
    constructor Create; virtual;
  end;

  TPDFActionClass = class of TPDFAction;

  TPDFURLAction = class ( TPDFAction )
  private
    FURL: string;
    procedure Save; override;
    procedure SetURL ( const Value: string );
  public
    property URL: string read FURL write SetURL;
  end;

  TPDFJavaScriptAction = class ( TPDFAction )
  private
    FJavaScript: string;
    procedure Save; override;
    procedure SetJavaScript ( const Value: string );
  public
    property JavaScript: string read FJavaScript write SetJavaScript;
  end;

  TPDFGoToPageAction = class ( TPDFAction )
  private
    FTopOffset: Integer;
    FPageIndex: Integer;
    FChange: Boolean;
    procedure Save; override;
    procedure SetPageIndex ( const Value: Integer );
    procedure SetTopOffset ( const Value: Integer );
  public
    property PageIndex: Integer read FPageIndex write SetPageIndex;
    property TopOffset: Integer read FTopOffset write SetTopOffset;
    property NoChangeZoom: Boolean read FChange write FChange;
  end;

  TPDFNamedDestinationAction = class ( TPDFAction )
  private
    FDocument: string;
    FDestination: string;
    FNewWindow: Boolean;
    procedure Save; override;
    procedure SetDestination ( const Value: string );
    procedure SetDocument ( const Value: string );
    procedure SetNewWindow ( const Value: Boolean );
  public
    property Destination: string read FDestination write SetDestination;
    property Document: string read FDocument write SetDocument;
    property NewWindow: Boolean read FNewWindow write SetNewWindow;
  end;

  TPDFGoToRemoteAction = class ( TPDFAction )
  private
    FInNewWindow: Boolean;
    FPageIndex: Integer;
    FDocument: string;
    procedure Save; override;
    procedure SetDocument ( const Value: string );
    procedure SetInNewWindow ( const Value: Boolean );
    procedure SetPageIndex ( const Value: Integer );
  public
    property Document: string read FDocument write SetDocument;
    property PageIndex: Integer read FPageIndex write SetPageIndex;
    property InNewWindow: Boolean read FInNewWindow write SetInNewWindow;
  end;

  TPDFVisibeControlAction = class ( TPDFAction )
  private
    FVisible: Boolean;
    FControls: TPDFControls;
    procedure Save; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Visible: Boolean read FVisible write FVisible;
    property Controls: TPDFControls read FControls;
  end;

  TPDFSubmitFormAction = class ( TPDFAction )
  private
    FFields: TPDFControls;
    FRG: TList;
    FIncludeEmptyFields: Boolean;
    FURL: string;
    FSubmitType: TPDFSubmitType;
    procedure Save; override;
    procedure SetURL ( const Value: string );
  public
    constructor Create; override;
    destructor Destroy; override;
    property IncludeFields: TPDFControls read FFields;
    property URL: string read FURL write SetURL;
    property SendEmpty: Boolean read FIncludeEmptyFields write FIncludeEmptyFields;
    property SubmitType: TPDFSubmitType read FSubmitType write FSubmitType;
  end;

  TPDFResetFormAction = class ( TPDFAction )
  private
    FFields: TPDFControls;
    FRG: TList;
    procedure Save; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property ResetFields: TPDFControls read FFields;
  end;

  TPDFImportDataAction = class ( TPDFAction )
  private
    FFileName: string;
    procedure Save; override;
    procedure SetFileName ( const Value: string );
  public
    property FileName: string read FFileName write SetFileName;
  end;

  TPDFCustomAnnotation = class ( TObject )
  private
    FOwner: TPDFPage;
    AnnotID: Integer;
    FLeft, FTop, FRight, FBottom: Integer;
    FFlags: TAnnotationFlags;
    FBorderStyle: string;
    FBorderColor: TColor;
    function CalcFlags: Integer;
    procedure Save; virtual; abstract;
    function GetBox: TRect;
    procedure SetBox ( const Value: TRect );
  public
    constructor Create ( AOwner: TPDFPage );
    property Box: TRect read GetBox write SetBox;
    property BorderStyle: string read FBorderStyle write FBorderStyle;
    property Flags: TAnnotationFlags read FFlags write FFlags;
    property BorderColor: TColor read FBorderColor write FBorderColor;
  end;


  TPDFControl = class ( TPDFCustomAnnotation )
  private
    FRequired: Boolean;
    FReadOnly: Boolean;
    FName: string;
    FOnMouseDown: TPDFAction;
    FOnMouseExit: TPDFAction;
    FOnMouseEnter: TPDFAction;
    FOnMouseUp: TPDFAction;
    FOnLostFocus: TPDFAction;
    FOnSetFocus: TPDFAction;
    FColor: TColor;
    FFont: TPDFControlFont;
    FFN: string;
    FHint: TPDFControlHint;
    procedure SetName ( const Value: string );
    procedure SetOnMouseDown ( const Value: TPDFAction );
    procedure SetOnMouseEnter ( const Value: TPDFAction );
    procedure SetOnMouseExit ( const Value: TPDFAction );
    procedure SetOnMouseUp ( const Value: TPDFAction );
    procedure SetOnLostFocus ( const Value: TPDFAction );
    procedure SetOnSetFocus ( const Value: TPDFAction );
    procedure SetColor ( const Value: TColor );
    function CalcActions: string;
  public
    constructor Create ( AOwner: TPDFPage; AName: string ); virtual;
    destructor Destroy; override;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Required: Boolean read FRequired write FRequired;
    property Name: string read FName write SetName;
    property Color: TColor read FColor write SetColor;
    property Font: TPDFControlFont read FFont;
    property Hint: TPDFControlHint read FHint;
    property OnMouseUp: TPDFAction read FOnMouseUp write SetOnMouseUp;
    property OnMouseDown: TPDFAction read FOnMouseDown write SetOnMouseDown;
    property OnMouseEnter: TPDFAction read FOnMouseEnter write SetOnMouseEnter;
    property OnMouseExit: TPDFAction read FOnMouseExit write SetOnMouseExit;
    property OnSetFocus: TPDFAction read FOnSetFocus write SetOnSetFocus;
    property OnLostFocus: TPDFAction read FOnLostFocus write SetOnLostFocus;
  end;

  TPDFControlClass = class of TPDFControl;

  TPDFRadioGroup = class
  private
    FButtons: TPDFControls;
    FOwner: TPDFDocument;
    GroupID: Integer;
    FName: string;
    procedure Save;
  public
    constructor Create ( AOwner: TPDFDocument; Name: string );
    destructor Destroy; override;
  end;


  TPDFButton = class ( TPDFControl )
  private
    FCaption: string;
    procedure Save; override;
  protected
    FUp: TPDFPage;
    FDown: TPDFPage;
    procedure Paint; virtual;
  public
    constructor Create ( AOwner: TPDFPage; AName: string ); override;
    property Caption: string read FCaption write FCaption;
  end;

  TPDFInputControl = class ( TPDFControl )
  private
    FOnOtherControlChanged: TPDFJavaScriptAction;
    FOnKeyPress: TPDFJavaScriptAction;
    FOnBeforeFormatting: TPDFJavaScriptAction;
    FOnChange: TPDFJavaScriptAction;
    procedure SetOnOtherControlChanged ( const Value: TPDFJavaScriptAction );
    procedure SetOnKeyPress ( const Value: TPDFJavaScriptAction );
    procedure SetOnBeforeFormatting ( const Value: TPDFJavaScriptAction );
    procedure SetOnChange ( const Value: TPDFJavaScriptAction );
    function CalcActions: string;
  public
    property OnKeyPress: TPDFJavaScriptAction read FOnKeyPress write SetOnKeyPress;
    property OnBeforeFormatting: TPDFJavaScriptAction read FOnBeforeFormatting write SetOnBeforeFormatting;
    property OnChange: TPDFJavaScriptAction read FOnChange write SetOnChange;
    property OnOtherControlChanged: TPDFJavaScriptAction read FOnOtherCOntrolChanged write SetOnOtherCOntrolChanged;
  end;

  TPDFCheckBox = class ( TPDFInputControl )
  private
    FChecked: Boolean;
    FCaption: string;
    procedure Save; override;
  protected
    FCheck: TPDFPage;
    FUnCheck: TPDFPage;
    procedure Paint; virtual;
  public
    constructor Create ( AOwner: TPDFPage; AName: string ); override;
    property Checked: Boolean read FChecked write FChecked;
    property Caption: string read FCaption write FCaption;
  end;

  TPDFRadioButton = class ( TPDFInputControl )
  private
    FRG: TPDFRadioGroup;
    FChecked: Boolean;
    FExportValue: string;
    procedure Save; override;
    procedure SetExportValue ( const Value: string );
  protected
    FCheck: TPDFPage;
    FUnCheck: TPDFPage;
    procedure Paint; virtual;
  public
    constructor Create ( AOwner: TPDFPage; AName: string ); override;
    property ExportValue: string read FExportValue write SetExportValue;
    property Checked: Boolean read FChecked write FChecked;
  end;


  TPDFEdit = class ( TPDFInputControl )
  private
    FText: string;
    FIsPassword: Boolean;
    FShowBorder: Boolean;
    FMultiline: Boolean;
    FMaxLength: Integer;
    FJustification: THorJust;
    procedure Save; override;
    procedure SetMaxLength ( const Value: Integer );
  protected
    FShow: TPDFPage;
    procedure Paint; virtual;
  public
    constructor Create ( AOwner: TPDFPage; AName: string ); override;
    property Text: string read FText write FText;
    property Multiline: Boolean read FMultiline write FMultiline;
    property IsPassword: Boolean read FIsPassword write FIsPassword;
    property ShowBorder: Boolean read FShowBorder write FShowBorder;
    property MaxLength: Integer read FMaxLength write SetMaxLength;
    property Justification: THorJust read FJustification write FJustification;
  end;

  TPDFComboBox = class ( TPDFInputControl )
  private
    FEditEnabled: Boolean;
    FItems: TStringList;
    FText: string;
    procedure Save; override;
  protected
    FShow: TPDFPage;
    procedure Paint; virtual;
  public
    constructor Create ( AOwner: TPDFPage; AName: string ); override;
    destructor Destroy; override;
    property Items: TStringList read FItems;
    property EditEnabled: Boolean read FEditEnabled write FEditEnabled;
    property Text: string read FText write FText;
  end;

  TPDFTextAnnotation = class ( TPDFCustomAnnotation )
  private
    FText: string;
    FCaption: string;
    FTextAnnotationIcon: TTextAnnotationIcon;
    FOpened: Boolean;
    FCharset: TFontCharset;
    procedure Save; override;
  public
    property Caption: string read FCaption write FCaption;
    property Text: string read FText write FText;
    property TextAnnotationIcon: TTextAnnotationIcon read FTextAnnotationIcon write FTextAnnotationIcon;
    property Opened: Boolean read FOpened write FOpened;
    property Charset: TFontCharset read FCharset write FCharset;
  end;

  TPDFActionAnnotation = class ( TPDFCustomAnnotation )
  private
    FAction: TPDFAction;
    procedure Save; override;
    procedure SetAction ( const Value: TPDFAction );
  public
    property Action: TPDFAction read FAction write SetAction;
  end;

  TPDFActions = class ( TObject )
  private
    FOwner: TPDFDocument;
    FActions: TList;
    function GetCount: Integer;
    function GetAction ( Index: Integer ): TPDFAction;
    procedure Clear;
  public
    constructor Create ( AOwner: TPDFDocument );
    destructor Destroy; override;
    function Add ( Action: TPDFAction ): Integer;
    function IndexOf ( Action: TPDFAction ): Integer;
    procedure Delete ( Action: TPDFAction );
    property Count: Integer read GetCount;
    property Items [ Index: Integer ]: TPDFAction read GetAction; default;
  end;

  TPDFAcroForm = class ( TObject )
  private
    AcroID: Integer;
    FOwner: TPDFDocument;
    FFields: TPDFControls;
    FFonts: TList;
    FRadioGroups: TList;
    procedure Save;
    function GetEmpty: Boolean;
  public
    constructor Create ( AOwner: TPDFDocument );
    procedure Clear;
    destructor Destroy; override;
    property Empty: Boolean read GetEmpty;
  end;

  TPDFPages = class;

  TPDFPage = class ( TObject )
  public
    FContent: TStringList;
  private
    FOwner: TPDFDocument;
    FAnnot: TList;
    FMatrix: TTextCTM;
    FForms: TPDFPages;
    FIsForm: Boolean;
    FRes: Integer;
    D2P: Extended;
    // ProcSet Options
    FTextUsed: Boolean;
    FGrayUsed: Boolean;
    FColorUsed: Boolean;
    FAskCanvas: Boolean;
    FHeight: Integer;
    FWidth: Integer;
    FPageSize: TPDFPageSize;
    FX: Extended;
    FY: Extended;
    FRotate: TPDFPageRotate;
    FLinkedFont: array of TPDFFont;
    FLinkedImages: TList;
    ResourceID: Integer;
    PageID: Integer;
    ContentID: Integer;
//    FCTM: TTextCTM;
    TP: TDoublePoint;
    FF: TTextCTM;
    FCurrentFontSize: Extended;
    FCurrentFont: TPDFFont;
    FCurrentDash: string;
    FHorizontalScaling: Extended;
    FCharSpace: Extended;
    FWordSpace: Extended;
    FRise: Extended;
    FRender: Integer;
    FTextInited: Boolean;
    FTextLeading: Extended;
    FSaveCount: Integer;
    FRealAngle: Extended;
    FOrientation: TPDFPageOrientation;
    Factions: Boolean;
    FMF: TMetafile;
    FCanvas: TCanvas;
    FWaterMark: Integer;
    FThumbnail: Integer;
    FRemoveCR: Boolean;
    FEmulationEnabled: Boolean;
    FCanvasOver: Boolean;
    FBaseLine: Boolean;
    FCharset: TFontCharset;
    FCurrentFontName: TFontName;
    FCurrentFontStyle: TFontStyles;
    FFontIsChanged: Boolean;
    FUnicodeUse: Boolean;
    FCurrentFontIndex: Integer;
    FPathInited: Boolean;
    procedure SetHeight ( const Value: Integer );
    procedure SetWidth ( const Value: Integer );
    procedure SetPageSize ( Value: TPDFPageSize );
    procedure Save;
    function IntToExtX ( AX: Extended ): Extended;
    function IntToExtY ( AY: Extended ): Extended;
    function ExtToIntX ( AX: Extended ): Extended;
    function ExtToIntY ( AY: Extended ): Extended;
    procedure DrawArcWithBezier ( CenterX, CenterY, RadiusX, RadiusY, StartAngle, SweepRange: Extended; UseMoveTo: Boolean = True );
    procedure SetRotate ( const Value: TPDFPageRotate );
    procedure RawSetTextPosition ( X, Y, Orientation: Extended );
    procedure RawTextOut ( X, Y, Orientation: Extended; TextStr: string );
    procedure RawLineTo ( X, Y: Extended );
    procedure RawMoveTo ( X, Y: Extended );
    procedure RawRect ( X, Y, W, H: Extended );
    function RawArc ( X1, Y1, x2, y2, x3, y3, x4, y4: Extended ): TDoublePoint; overload;
    function RawArc ( X1, Y1, x2, y2, BegAngle, EndAngle: Extended ): TDoublePoint; overload;
    function RawPie ( X1, Y1, x2, y2, x3, y3, x4, y4: Extended ): TDoublePoint; overload;
    function RawPie ( X1, Y1, x2, y2, BegAngle, EndAngle: Extended ): TDoublePoint; overload;
    procedure RawCircle ( X, Y, R: Extended );
    procedure RawEllipse ( x1, y1, x2, y2: Extended );
    procedure RawRectRotated ( X, Y, W, H, Angle: Extended );
    procedure RawShowImage ( ImageIndex: Integer; x, y, w, h, angle: Extended );
    procedure RawConcat ( A, B, C, D, E, F: Extended );
    procedure RawTranslate ( XT, YT: Extended );
    procedure DeleteAllAnnotations;
    procedure RawCurveto ( X1, Y1, X2, Y2, X3, Y3: Extended );
    procedure SetOrientation ( const Value: TPDFPageOrientation );
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure CreateCanvas;
    procedure DeleteCanvas;
    procedure CloseCanvas;

    procedure SetWaterMark ( const Value: Integer );
    procedure SetThumbnail ( const Value: Integer );
    procedure PrepareID;

    //Internal Text use
    procedure BeginText;
    procedure EndText;
    procedure TextShow ( TextStr: AnsiString );
    procedure UnicodeTextShow ( Text: PWord; Len: Integer );
    procedure SetCurrentFont ( Unicode: Boolean );
    function RawGetTextWidth ( Text: string ): Extended;
    function RawGetUnicodeWidth ( Text: PWord; Len: Integer ): Extended;
    procedure RawUnicodeTextOut ( X, Y, Orientation: Extended; Text: PWord;
      Len: Integer );
    procedure RawExtTextOut ( X, Y, Orientation: Extended; TextStr: string;
      Dx: PExt );
    procedure RawExtUnicodeTextOut ( X, Y, Orientation: Extended;
      Text: PWord; Len: Integer; DX: PExt );
    procedure RawExtGlyphTextOut ( X, Y, Orientation: Extended;
      Text: PWord; Len: Integer; DX: PExt );
    procedure ExtTextShow ( TextStr: string; Dx: PExt );
    procedure ExtUnicodeTextShow ( Text: PWord; Len: Integer; Dx: PExt );
    procedure ExtGlyphTextShow ( Text: PWord; Len: Integer; Dx: PExt );
    function GetRawTextHeight: Extended;
    procedure PaintTextLines ( Width: Extended );
    procedure SetIntCharacterSpacing(Spacing: Extended);
  public
    constructor Create ( AOwner: TPDFDocument );
    destructor Destroy; override;

    // Annotation
    function SetAnnotation ( ARect: TRect; Title, Text: string; Color: TColor; Flags: TAnnotationFlags; Opened: Boolean; Charset: TFontCharset = ANSI_CHARSET ): TPDFCustomAnnotation;
    function SetUrl ( ARect: TRect; URL: string ): TPDFCustomAnnotation;
    function SetAction ( ARect: TRect; Action: TPDFAction ): TPDFCustomAnnotation;
    function SetLinkToPage ( ARect: TRect; PageIndex: Integer; TopOffset: Integer ): TPDFCustomAnnotation;
    function CreateControl ( CClass: TPDFControlClass; ControlName: string; Box: TRect ): TPDFControl;

    // Text operation
    procedure SetActiveFont ( FontName: string; FontStyle: TFontStyles; FontSize: Extended; FontCharset: TFontCharset = ANSI_CHARSET );
    procedure SetCharacterSpacing ( Spacing: Extended );
    procedure SetHorizontalScaling ( Scale: Extended );
    procedure SetTextRenderingMode ( Mode: integer );
    procedure SetWordSpacing ( Spacing: Extended );
{$IFDEF CB}
    procedure TextOutput ( X, Y, Orientation: Extended; TextStr: string );
{$ELSE}
    procedure TextOut ( X, Y, Orientation: Extended; TextStr: string );
{$ENDIF}
    procedure UnicodeTextOut ( X, Y, Orientation: Extended; Text: PWord; Len: Integer );
    procedure ExtTextOut ( X, Y, Orientation: Extended; TextStr: string; Dx: PExt );
    procedure ExtUnicodeTextOut ( X, Y, Orientation: Extended; Text: PWord; Len: Integer; DX: PExt );
    procedure ExtGlyphTextOut ( X, Y, Orientation: Extended; Text: PWord; Len: Integer; DX: PExt );
    function TextOutBox ( LTCornX, LTCornY, Interval, BoxWidth, BoxHeight: integer; TextStr: Ansistring ): integer;
    function GetTextRowCount ( BoxWidth: Integer; TextStr: string ): Integer;
    function UnicodeTextOutBox ( LTCornX, LTCornY, Interval, BoxWidth, BoxHeight: integer; Text: PWord; Len: Integer ): integer;
    function GetUnicodeTextRowCount ( BoxWidth: Integer; Text: PWord; Len: Integer ): Integer;
    procedure TextFromBaseLine ( BaseLine: Boolean );
    function GetTextWidth ( Text: string ): Extended;
    function GetUnicodeWidth ( Text: PWord; Len: Integer ): Extended;
    procedure TextBox ( Rect: TRect; Text: string; Hor: THorJust; Vert: TVertJust );
    // Path operation
    procedure Clip;
    procedure EoClip;
    procedure EoFill;
    procedure EoFillAndStroke;
    procedure Fill;
    procedure FillAndStroke;
    procedure NewPath;
    procedure ClosePath;
    procedure Stroke;
    // Graphic primitive operation
    procedure LineTo ( X, Y: Extended );
    procedure MoveTo ( X, Y: Extended );
    procedure Curveto ( X1, Y1, X2, Y2, X3, Y3: Extended );
    procedure Rectangle ( X1, Y1, X2, Y2: Extended );
    // Advanced graphic operation
    procedure Circle ( X, Y, R: Extended );
    procedure Ellipse ( X1, Y1, X2, Y2: Extended );
    function Arc ( X1, Y1, x2, y2, x3, y3, x4, y4: Extended ): TDoublePoint; overload;
    function Arc ( X1, Y1, x2, y2, BegAngle, EndAngle: Extended ): TDoublePoint; overload;
    procedure Pie ( X1, Y1, x2, y2, x3, y3, x4, y4: Extended ); overload;
    procedure Pie ( X1, Y1, x2, y2, BegAngle, EndAngle: Extended ); overload;
    procedure RectRotated ( X, Y, W, H, Angle: Extended );
    procedure RoundRect ( X1, Y1, X2, Y2, X3, Y3: Integer );
    // Graphic state operation
    procedure GStateRestore;
    procedure GStateSave;
    procedure NoDash;
    procedure SetCMYKColor ( C, M, Y, K: Extended );
    procedure SetCMYKColorFill ( C, M, Y, K: Extended );
    procedure SetCMYKColorStroke ( C, M, Y, K: Extended );
    procedure SetRGBColor ( R, G, B: Extended );
    procedure SetRGBColorFill ( R, G, B: Extended );
    procedure SetRGBColorStroke ( R, G, B: Extended );
    procedure SetDash ( DashSpec: string );
    procedure SetFlat ( FlatNess: integer );
    procedure SetGray ( Gray: Extended );
    procedure SetGrayFill ( Gray: Extended );
    procedure SetGrayStroke ( Gray: Extended );
    procedure SetLineWidth ( lw: Extended );

    // Transform operation
    procedure Scale ( SX, SY: Extended );
    procedure Rotate ( Angle: Extended );
    procedure Translate ( XT, YT: Extended );
    // Picture
    procedure ShowImage ( ImageIndex: Integer; x, y, w, h, angle: Extended );

    function GetCurrentFontSize: Extended;
    // Other

    procedure SetLineCap ( LineCap: TPDFLineCap );
    procedure SetLineJoin ( LineJoin: TPDFLineJoin );
    procedure SetMiterLimit ( MiterLimit: Extended );

    procedure Comment ( st: string );
    procedure AppendAction ( Action: string );
    procedure PlayMetaFile ( MF: TMetaFile ); overload;
    procedure PlayMetaFile ( MF: TMetafile; x, y, XScale, YScale: Extended ); overload;
    property Orientation: TPDFPageOrientation read FOrientation write SetOrientation;
    property Size: TPDFPageSize read FPageSize write SetPageSize;
    property Height: Integer read GetHeight write SetHeight;
    property Width: Integer read GetWidth write SetWidth;
    property PageRotate: TPDFPageRotate read FRotate write SetRotate;
    property WaterMark: Integer read FWaterMark write SetWaterMark;
    property Thumbnail: Integer read FThumbnail write SetThumbnail;
    property TextInited: Boolean read FTextInited;
    property CanvasOver: Boolean read FCanvasOver write FCanvasOver;
    property Owner: TPDFDocument read FOwner;
  end;

  TPDFPages = class ( TObject )
  private
    IsWaterMarks: Boolean;
    FOwner: TPDFDocument;
    FPages: TList;
    function GetCount: Integer;
    function GetPage ( Index: Integer ): TPDFPage;
    procedure Clear;
  public
    constructor Create ( AOwner: TPDFDocument; WM: Boolean );
    destructor Destroy; override;
    function Add: TPDFPage;
    function IndexOf ( Page: TPDFPage ): Integer;
    procedure Delete ( Page: TPDFPage );
    property Count: Integer read GetCount;
    property Items [ Index: Integer ]: TPDFPage read GetPage; default;
  end;

  TPDFOutlines = class;

  TPDFOutlineNode = class ( TObject )
  private
    OutlineNodeID: Integer;
    FChild: TList;
    FOwner: TPDFOutlines;
    FParent: TPDFOutlineNode;
    FPrev: TPDFOutlineNode;
    FNext: TPDFOutlineNode;
    FTitle: string;
    FExpanded: Boolean;
    FCharset: TFontCharset;
    FAction: TPDFAction;
    FColor: TColor;
    FStyle: TFontStyles;
    procedure SetTitle ( const Value: string );
    function GetCount: Integer;
    function GetHasChildren: Boolean;
    function GetItem ( Index: Integer ): TPDFOutlineNode;
    procedure Save;
    procedure SetExpanded ( const Value: Boolean );
    procedure SetCharset ( const Value: TFontCharset );
    procedure SetAction ( const Value: TPDFAction );
  public
    procedure Delete;
    procedure DeleteChildren;
    constructor Create ( AOwner: TPDFOutlines );
    destructor Destroy; override;
    function GetFirstChild: TPDFOutlineNode;
    function GetLastChild: TPDFOutlineNode;
    function GetNext: TPDFOutlineNode;
    function GetNextChild ( Node: TPDFOutlineNode ): TPDFOutlineNode;
    function GetNextSibling: TPDFOutlineNode;
    function GetPrev: TPDFOutlineNode;
    function GetPrevChild ( Node: TPDFOutlineNode ): TPDFOutlineNode;
    function GetPrevSibling: TPDFOutlineNode;
    property Action: TPDFAction read FAction write SetAction;
    property Title: string read FTitle write SetTitle;
    property Color: TColor read FColor write FColor;
    property Style: TFontStyles read FStyle write FStyle;
    property Count: Integer read GetCount;
    property HasChildren: Boolean read GetHasChildren;
    property Item [ Index: Integer ]: TPDFOutlineNode read GetItem;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property Charset: TFontCharset read FCharset write SetCharset;
    property Parent: TPDFOutlineNode read FParent;

  end;

  TPDFOutlines = class ( TObject )
  private
    OutlinesID: Integer;
    FList: TList;
    FOwner: TPDFDocument;
    function GetCount: Integer;
    function GetItem ( Index: Integer ): TPDFOutlineNode;
    function Add ( Node: TPDFOutlineNode ): TPDFOutlineNode; overload;
    function AddChild ( Node: TPDFOutlineNode ): TPDFOutlineNode; overload;
    function AddChildFirst ( Node: TPDFOutlineNode ): TPDFOutlineNode; overload;
    function AddFirst ( Node: TPDFOutlineNode ): TPDFOutlineNode; overload;
    function Insert ( Node: TPDFOutlineNode ): TPDFOutlineNode; overload;
  public
    procedure Clear;
    procedure Delete ( Node: TPDFOutlineNode );
    function GetFirstNode: TPDFOutlineNode;

    function Add ( Node: TPDFOutlineNode; Title: string; Action: TPDFAction; Charset: TFontCharset = ANSI_CHARSET ): TPDFOutlineNode; overload;
    function AddChild ( Node: TPDFOutlineNode; Title: string; Action: TPDFAction; Charset: TFontCharset = ANSI_CHARSET ): TPDFOutlineNode; overload;
    function AddChildFirst ( Node: TPDFOutlineNode; Title: string; Action: TPDFAction; Charset: TFontCharset = ANSI_CHARSET ): TPDFOutlineNode; overload;
    function AddFirst ( Node: TPDFOutlineNode; Title: string; Action: TPDFAction; Charset: TFontCharset = ANSI_CHARSET ): TPDFOutlineNode; overload;
    function Insert ( Node: TPDFOutlineNode; Title: string; Action: TPDFAction; Charset: TFontCharset = ANSI_CHARSET ): TPDFOutlineNode; overload;

    constructor Create ( AOwner: TPDFDocument );
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Item [ Index: Integer ]: TPDFOutlineNode read GetItem; default;
  end;

  TPDFFont = class ( TObject )
  private
    FontID: Integer;
    FFontName: string;
    FFontStyle: TFontStyles;
    FUnicode: Boolean;
    Standart: Boolean;
    AliasName: string;
    StdID: Byte;
    FWidthArray: PPDFANSICharacterWidth;
    WidthLoaded: Boolean;
    FAbscent: Integer;
    FDescent: Integer;
    FEbd: Boolean;
    OTM: OUTLINETEXTMETRIC;
    FUsed: array [ 0..255 ] of Boolean;
    FFirstChar, FLastChar: Byte;
    FFontUsed: Boolean;
    TTFInfo: TTTFData;
    IsWing: Boolean;
    FMono: Boolean;
    procedure Save ( Doc: TPDFDocument );
    function GetWidth ( Index: Integer ): Word;
    function GetAbscent: Integer;
    function GetDescent: Integer;
    procedure FillInfo;
    procedure FillUsed ( st: string );
    procedure UsedChar ( Ch: Byte );
    procedure SetAllANSI;
  public
    constructor Create;
    destructor Destroy; override;
    property Descent: Integer read GetDescent;
    property Abscent: Integer read GetAbscent;
    property FontName: string read FFontName;
    property FontStyle: TFontStyles read FFontStyle;
    property Unicode: Boolean read FUnicode;
  end;

  TPDFFonts = class ( TObject )
  private
    FOwner: TPDFDocument;
    FFonts: TList;
    function GetCount: Integer;
    function GetFont ( Index: Integer ): TPDFFont;
  public
    constructor Create ( AOwner: TPDFDocument );
    destructor Destroy; override;
    procedure Clear;
    function GetFontByInfo ( FontName: TFontName; FontStyle: TFontStyles; IsUnicode: Boolean ): TPDFFont;
    property Count: Integer read GetCount;
    property Items [ Index: Integer ]: TPDFFont read GetFont; default;
  end;

  TPDFImage = class ( TObject )
  private
    FDoc: TPDFDocument;
    FCT: TImageCompressionType;
    ImageName: string;
    PictureID: Integer;
    FHeight: Integer;
    FWidth: Integer;
    Data: TMemoryStream;
    FBitPerPixel: Integer;
    GrayScale: Boolean;
    Saved: Boolean;
    FIsMask: Boolean;
    FMaskIndex: Integer;
    BWInvert: Boolean;
    procedure Save ( Doc: TPDFDocument );
  public
    constructor Create ( Image: TGraphic; Compression: TImageCompressionType; Doc: TPDFDocument; MaskIndex: Integer = -1; IsMask: Boolean = False; TransparentColor: TColor = -1 ); overload;
    constructor Create ( FileName: TFileName; Compression: TImageCompressionType; Doc: TPDFDocument; MaskIndex: Integer = -1; IsMask: Boolean = False; TransparentColor: TColor = -1 ); overload;
    destructor Destroy; override;
    property Height: Integer read FHeight;
    property Width: Integer read FWidth;
    property BitPerPixel: Integer read FBitPerPixel;
  end;

  TPDFImages = class ( TObject )
  private
    FOwner: TPDFDocument;
    FImages: TList;
    function GetCount: Integer;
    function GetImage ( Index: Integer ): TPDFImage;
  public
    constructor Create ( AOwner: TPDFDocument );
    destructor Destroy; override;
    procedure Delete ( Index: Integer );
    procedure Clear;
    function Add ( Image: TGraphic; Compression: TImageCompressionType; MaskIndex: Integer = -1; IsMask: Boolean = False; TransparentColor: TColor = -1 ): TPDFImage; overload;
    function Add ( FileName: TFileName; Compression: TImageCompressionType; MaskIndex: Integer = -1; IsMask: Boolean = False; TransparentColor: TColor = -1 ): TPDFImage; overload;
    property Count: Integer read GetCount;
    property Items [ Index: Integer ]: TPDFImage read GetImage; default;
  end;

  TPDFDocument = class ( TComponent )
  private
    FNamedDest: TNamedDestRecordArray;
    FEmbedFiles: TEmbedFileRecordArray;
    IDOffset: array of Integer;
    CurID: Integer;
    EncriptID: Integer;
    PagesID: Integer;
    CatalogID: Integer;
    XREFOffset: Integer;
    FAcroForm: TPDFAcroForm;
    FJSF: TPDFJSFList;
    FFileName: TFileName;
    FPages: TPDFPages;
    FWaterMarks: TPDFPages;
    FActions: TPDFActions;
    FFonts: TPDFFonts;
    FImages: TPDFImages;
    FRadioGroups: TList;
    FStream: TStream;
    FCompression: TCompressionType;
    FDocumentInfo: TPDFDocInfo;
    FPageLayout: TPageLayout;
    FPageMode: TPageMode;
    FCurrentPage: TPDFPage;
    FOutlines: TPDFOutlines;
    FProtectionEnabled: Boolean;
    FUserPassword: string;
    FOwnerPassword: string;
    FOCP: string;
    FProtectionOptions: TPDFCtiptoOptions;
    FKey: TEncripKey;
    FPFlags: Integer;
    FileID: string;
    FNotEmbeddedFont: TStringList;
    FOutputStream: TStream;
    FJPEGQuality: Integer;
    FPrinting: Boolean;
    FAborted: Boolean;
    FAutoLaunch: Boolean;
    FResolution: Integer;
    FViewerPreferences: TViewerPreferences;
    FOpenDocumentAction: TPDFAction;
    FACURL: Boolean;
    FOnePass: Boolean;
    FVersion: TPDFVersion;
    FEMFImageLostQuality: Boolean;
    FPKL: TPDFKeyLength;
    FUseScreen: Boolean;
    FDC: HDC;
    FEmulateStd: Boolean;
    FReDraw: Boolean;
    procedure CalcKey;
    procedure SaveXREF;
    procedure SaveTrailer;
    procedure SaveDocumentInfo;
    procedure SetFileName ( const Value: TFileName );
    procedure SetCompression ( const Value: TCompressionType );
    procedure SetDocumentInfo ( const Value: TPDFDocInfo );
    procedure SaveToStream ( st: string; CR: Boolean = True );
    function GetNextID: Integer;
    procedure SetPageLayout ( const Value: TPageLayout );
    procedure SetPageMode ( const Value: TPageMode );
    procedure SetOwnerPassword ( const Value: string );
    procedure SetProtectionEnabled ( const Value: Boolean );
    procedure SetProtectionOptions ( const Value: TPDFCtiptoOptions );
    procedure SetUserPassword ( const Value: string );
    procedure GenFileID;
    procedure CalcOwnerPassword;
    function CalcUserPassword: string;
    procedure SetNotEmbeddedFont ( const Value: TStringList );
    function GetPageCount: Integer;
    function GetPage ( Index: Integer ): TPDFPage;
    procedure SetOutputStream ( const Value: TStream );
    procedure SetJPEGQuality ( const Value: Integer );
    procedure SetAutoLaunch ( const Value: Boolean );
    function GetCanvas: TCanvas;
    function GetPageHeight: Integer;
    function GetPageNumber: Integer;
    function GetPageWidth: Integer;
    function GetWaterMark ( Index: Integer ): TPDFPage;
    procedure AppendAction ( const Action: TPDFAction );
    procedure SetOpenDocumentAction ( const Value: TPDFAction );
    procedure StartObj ( var ID: Integer );
    procedure CloseHObj;
    procedure CloseObj;
    procedure StartStream;
    procedure CloseStream;
    procedure SetOnePass ( const Value: Boolean );
    procedure ClearAll;
    procedure DeleteAllRadioGroups;
    function CreateRadioGroup ( Name: string ): TPDFRadioGroup;
    procedure SetVersion ( const Value: TPDFVersion );
    procedure StoreDocument;
    function StoreNamedDests: Integer;
    function StoreEmbeddedFiles: Integer;
    procedure SetProtectionKeyLength ( const Value: TPDFKeyLength );
    procedure SetEmulateStd ( const Value: Boolean );
    procedure SetUsedDC(const Value: HDC);
  public
    constructor Create ( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure Abort;
    procedure BeginDoc;
    procedure EndDoc;
    procedure NewPage;
    procedure SetCurrentPage ( Index: Integer );
    function GetCurrentPageIndex: Integer;
    function AddImage ( Image: TGraphic; Compression: TImageCompressionType; MaskIndex: Integer = -1; IsMask: Boolean = False; TransparentColor: TColor = -1 ): Integer; overload;
    function AddImage ( FileName: TFileName; Compression: TImageCompressionType; MaskIndex: Integer = -1; IsMask: Boolean = False; TransparentColor: TColor = -1 ): Integer; overload;
    function CreateWaterMark: Integer;
    function CreateAction ( CClass: TPDFActionClass ): TPDFAction;
    procedure AddJSFunction ( AName, AParams, ABody: string );
    procedure AddNameDestination ( Dest: string; Page: Integer; TopOffset: Integer );
    procedure AddEmbedFile ( FileName: string; Title:string );
    property OpenDocumentAction: TPDFAction read FOpenDocumentAction write SetOpenDocumentAction;
    property Outlines: TPDFOutlines read FOutlines;
    property CurrentPage: TPDFPage read FCurrentPage;
    property PageCount: Integer read GetPageCount;
    property Page [ Index: Integer ]: TPDFPage read GetPage; default;
    property WaterMark [ Index: Integer ]: TPDFPage read GetWaterMark;
    property Printing: Boolean read FPrinting;
    property Aborted: Boolean read FAborted;
    property Canvas: TCanvas read GetCanvas;
    property OutputStream: TStream read FOutputStream write SetOutputStream;
    property PageHeight: Integer read GetPageHeight;
    property PageWidth: Integer read GetPageWidth;
    property PageNumber: Integer read GetPageNumber;
    property UsedDC: HDC read FDC write SetUsedDC;
  published
    property AutoLaunch: Boolean read FAutoLaunch write SetAutoLaunch;
    property FileName: TFileName read FFileName write SetFileName;
    property Compression: TCompressionType read FCompression write SetCompression;
    property DocumentInfo: TPDFDocInfo read FDocumentInfo write SetDocumentInfo;
    property PageLayout: TPageLayout read FPageLayout write SetPageLayout;
    property PageMode: TPageMode read FPageMode write SetPageMode;
    property ProtectionEnabled: Boolean read FProtectionEnabled write SetProtectionEnabled;
    property ProtectionOptions: TPDFCtiptoOptions read FProtectionOptions write SetProtectionOptions;
    property OwnerPassword: string read FOwnerPassword write SetOwnerPassword;
    property UserPassword: string read FUserPassword write SetUserPassword;
    property NonEmbeddedFont: TStringList read FNotEmbeddedFont write SetNotEmbeddedFont;
     property JPEGQuality: Integer read FJPEGQuality write SetJPEGQuality;
    property Resolution: Integer read FResolution write FResolution;
    property ViewerPreferences: TViewerPreferences read FViewerPreferences write FViewerPreferences;
    property AutoCreateURL: Boolean read FACURL write FACURL;
    property OnePass: Boolean read FOnePass write SetOnePass;
    property Version: TPDFVersion read FVersion write SetVersion;
    property EMFImageAsJpeg: Boolean read FEMFImageLostQuality write FEMFImageLostQuality;
    property ProtectionKeyLength: TPDFKeyLength read FPKL write SetProtectionKeyLength;
    property EmulateStandardFont: Boolean read FEmulateStd write SetEmulateStd;
    property ReDrawEMFFile: Boolean read FReDraw write FReDraw;
  end;


implementation

uses pdfRC4, pdfResources, pdfGliphNames, pdfCCITT, pdfWMF, pdfZLib,
  pdfMD5, pdfttf;

const
  PassKey: array [ 1..32 ] of Byte = ( $28, $BF, $4E, $5E, $4E, $75, $8A, $41, $64, $00, $4E,
    $56, $FF, $FA, $01, $08, $2E, $2E, $00, $B6, $D0, $68,
    $3E, $80, $2F, $0C, $A9, $FE, $64, $53, $69, $7A );

  PDFStandardFontNames: array [ 0..13 ] of string
  = ( 'Helvetica', 'Helvetica-Bold', 'Helvetica-Oblique', 'Helvetica-BoldOblique',
    'Times-Roman', 'Times-Bold', 'Times-Italic', 'Times-BoldItalic',
    'Courier', 'Courier-Bold', 'Courier-Oblique', 'Courier-BoldOblique',
    'Symbol', 'ZapfDingbats' );
  URLDetectStrings: array [ 0..2 ] of string = ( 'http://', 'ftp://', 'mailto:' );

{$IFDEF LLPDFEVAL}
var
  s1, s2, s3: string;
  KeyData: TRC4Data;
const
{$I SOP.inc}
  ComponentName: string = 'TPDFDocument';
{$ENDIF}


function CharSetToCodePage ( Charset: Byte ): Integer;
begin
  if Charset = Default_Charset then
    Charset := GetDefFontCharSet;
  case Charset of
    ANSI_CHARSET: Result := 1252;
    RUSSIAN_CHARSET: Result := 1251;
    TURKISH_CHARSET: Result := 1254;
    SHIFTJIS_CHARSET: Result := 932;
    HANGEUL_CHARSET: Result := 949;
    CHINESEBIG5_CHARSET: Result := 950;
    GB2312_CHARSET: Result := 936;
    JOHAB_CHARSET: Result := 1361;
    HEBREW_CHARSET: Result := 1255;
    ARABIC_CHARSET: Result := 1256;
    GREEK_CHARSET: Result := 1253;
    THAI_CHARSET: Result := 874;
    EASTEUROPE_CHARSET: Result := 1250;
    MAC_CHARSET: Result := 10000;
    BALTIC_CHARSET: Result := 1257;
    VIETNAMESE_CHARSET: Result := 1258;
  else
    Result := 1252;
  end;
end;


function EnCodeString ( Encoding: Boolean; Key: TEncripKey; KeyLength: TPDFKeyLength; ID: Integer; Source: string ): string;
var
  FullKey: array [ 1..21 ] of Byte;
  Digest: TMD5Byte16;
  AKey: TRC4Data;
  S: string;
begin
  if ( Source = '' ) or ( not Encoding ) then
  begin
    Result := Source;
    Exit;
  end;
  S := Source;
  FillChar ( FullKey, 21, 0 );
  if KeyLength = kl40 then
  begin
    Move ( Key, FullKey, 5 );
    Move ( ID, FullKey [ 6 ], 3 );
    xCalcMD5 ( @FullKey, 10, Digest );
    RC4Init ( AKey, @Digest, 10 );
  end
  else
  begin
    Move ( Key, FullKey, 16 );
    Move ( ID, FullKey [ 17 ], 3 );
    xCalcMD5 ( @FullKey, 21, Digest );
    RC4Init ( AKey, @Digest, 16 );
  end;
  RC4Crypt ( AKey, @s [ 1 ], @S [ 1 ], Length ( S ) );
  Result := S;
end;

function EnCodeHexString ( Encoding: Boolean; Key: TEncripKey; KeyLength: TPDFKeyLength; ID: Integer; Source: string ): string;
var
  FullKey: array [ 1..21 ] of Byte;
  Digest: TMD5Byte16;
  AKey: TRC4Data;
  a: array of Byte;
  S: string;
  i: Integer;
begin
  if ( Source = '' ) or ( not Encoding ) then
  begin
    Result := Source;
    Exit;
  end;
  S := '';
  FillChar ( FullKey, 21, 0 );
  if KeyLength = kl40 then
  begin
    Move ( Key, FullKey, 5 );
    Move ( ID, FullKey [ 6 ], 3 );
    xCalcMD5 ( @FullKey, 10, Digest );
    RC4Init ( AKey, @Digest, 10 );
  end
  else
  begin
    Move ( Key, FullKey, 16 );
    Move ( ID, FullKey [ 17 ], 3 );
    xCalcMD5 ( @FullKey, 21, Digest );
    RC4Init ( AKey, @Digest, 16 );
  end;
  SetLength ( a, Length ( Source ) div 2 );
  for i := 1 to Length ( Source ) div 2 do
    a [ i - 1 ] := StrToInt ( '$' + Source [ i shl 1 - 1 ] + Source [ i shl 1 ] );
  RC4Crypt ( AKey, @a [ 0 ], @a [ 0 ], Length ( Source ) div 2 );
  for i := 1 to Length ( Source ) div 2 do
    S := S + ByteToHex ( a [ i - 1 ] );
  Result := S;
end;


procedure EnCodeStream ( Encoding: Boolean; Key: TEncripKey; KeyLength: TPDFKeyLength; ID: Integer; Source: TMemoryStream );
var
  FullKey: array [ 1..21 ] of Byte;
  Digest: TMD5Byte16;
  AKey: TRC4Data;
begin
  if ( not Encoding ) or ( Source.Size = 0 ) then
    Exit;
  FillChar ( FullKey, 21, 0 );
  if KeyLength = kl40 then
  begin
    Move ( Key, FullKey, 5 );
    Move ( ID, FullKey [ 6 ], 3 );
    xCalcMD5 ( @FullKey, 10, Digest );
    RC4Init ( AKey, @Digest, 10 );
  end
  else
  begin
    Move ( Key, FullKey, 16 );
    Move ( ID, FullKey [ 17 ], 3 );
    xCalcMD5 ( @FullKey, 21, Digest );
    RC4Init ( AKey, @Digest, 16 );
  end;
  RC4Crypt ( AKey, Source.Memory, Source.Memory, Source.Size );
end;

function UnicodeChar ( Text: AnsiString; Charset: Integer ): string;
var
  A: array of Word;
  i: Integer;
  W: PWideChar;
  CodePage: Integer;
  OS: Integer;
begin
  Result := '';
  case Charset of
    EASTEUROPE_CHARSET: CodePage := 1250;
    RUSSIAN_CHARSET: CodePage := 1251;
    GREEK_CHARSET: CodePage := 1253;
    TURKISH_CHARSET: CodePage := 1254;
    BALTIC_CHARSET: CodePage := 1257;
    SHIFTJIS_CHARSET: CodePage := 932;
    129: CodePage := 949;
    CHINESEBIG5_CHARSET: CodePage := 950;
    GB2312_CHARSET: CodePage := 936;
  else
    CodePage := 1252;
  end;
  OS := MultiByteToWideChar ( CodePage, 0, PAnsiChar ( Text ), Length ( Text ), nil, 0 );
  if OS = 0 then
    Exit;
  SetLength ( A, OS );
  W := @a [ 0 ];
  if MultiByteToWideChar ( CodePage, 0, PAnsiChar ( Text ), Length ( Text ), W, OS ) <> 0 then
  begin
    Result := 'FEFF';
    for i := 0 to OS - 1 do
      Result := Result + WordToHex ( A [ i ] );
  end;
end;

function ReplStr ( Source: string; Ch: Char; Sub: string ): string;
var
  I: Integer;
begin
  Result := '';
  for i := 1 to Length ( Source ) do
    if Source [ I ] <> Ch then
      Result := Result + Source [ i ]
    else
      Result := Result + Sub;
end;

function PrepareFileSpec ( FileName: TFileName ): string;
var
  S: string;
  WS: string;
  FF: string;

  function RepS ( Source, Rep, RP: string ): string;
  var
    I, L: Integer;
    RS, S: string;
  begin
    S := Source;
    RS := '';
    L := Length ( Rep );
    I := Pos ( Rep, S );
    while I <> 0 do
    begin
      RS := RS + Copy ( S, 1, I - 1 ) + RP;
      Delete ( S, 1, L + I - 1 );
      I := Pos ( Rep, S );
    end; ;
    RS := RS + S;
    Result := RS;
  end;
begin
  WS := ExtractFilePath ( FileName );
  if WS = '' then
  begin
    Result := FileName;
    Exit;
  end;
  FF := ExtractFileDrive ( FileName );
  if FF = '' then
  begin
    S := RepS ( FileName, '\', '/' );
    if S [ 1 ] = '/' then
      S := '/' + S;
    Result := S;
    Exit;
  end;
  S := RepS ( FileName, '\\', '/' );
  S := RepS ( S, ':\', '/' );
  S := RepS ( S, '\', '/' );
  S := RepS ( S, ':', '/' );
  if S [ 1 ] <> '/' then
    S := '/' + S;
  Result := S;
end;

function EscapeSpecialChar ( TextStr: string ): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length ( TextStr ) do
    case TextStr [ I ] of
      '(': Result := Result + '\(';
      ')': Result := Result + '\)';
      '\': Result := Result + '\\';
      #13: Result := result + '\r';
      #10: Result := result + '\n';
    else
      Result := Result + chr ( Ord ( textstr [ i ] ) );
    end;
end;


function IntToStrWithZero ( ID, Count: Integer ): string;
var
  s, d: string;
  I: Integer;
begin
  s := IntToStr ( ID );
  I := Count - Length ( s );
  d := '';
  for I := 0 to I - 1 do
    d := d + '0';
  Result := d + s;
end;

procedure NormalizeRect ( var Rect: TRect ); overload;
begin
  if Rect.Left > Rect.Right then
    swp ( Rect.Left, Rect.Right );
  if Rect.Top > Rect.Bottom then
    swp ( Rect.Top, Rect.Bottom );
end;

procedure NormalizeRect ( var x1, y1, x2, y2: integer ); overload;
begin
  if x1 > x2 then
    swp ( x2, x1 );
  if y1 > y2 then
    swp ( y2, y1 );
end;

procedure NormalizeRect ( var x1, y1, x2, y2: Extended ); overload;
begin
  if x1 > x2 then
    swp ( x2, x1 );
  if y1 > y2 then
    swp ( y2, y1 );
end;


function DPoint ( x, y: Extended ): TDoublePoint;
begin
  Result.x := x;
  Result.y := y;
end;

procedure RotateCoordinate ( X, Y, Angle: Extended; var XO, YO: Extended );
var
  rcos, rsin: Extended;
begin
  Angle := Angle * ( PI / 180 );
  rcos := cos ( angle );
  rsin := sin ( angle );
  XO := rcos * x - rsin * y;
  YO := rsin * x + rcos * y;
end;




{ TPDFDocument }

procedure TPDFDocument.Abort;
begin
  if not FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileNotActivated );
  ClearAll;
  if FOutputStream = nil then
    DeleteFile ( FileName );
  FAborted := True;
  FPrinting := False;
end;

procedure TPDFDocument.BeginDoc;
var
  S:String;
begin
  if FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileInProgress );
  ClearAll;
  GenFileID;
  if FPKL = kl40 then
  begin
    FPFlags := $7FFFFFE0;
    FPFlags := FPFlags shl 1;
    if coPrint in ProtectionOptions then
      FPFlags := FPFlags or 4;
    if coModifyStructure in ProtectionOptions then
      FPFlags := FPFlags or 8;
    if coCopyInformation in ProtectionOptions then
      FPFlags := FPFlags or 16;
    if coModifyAnnotation in ProtectionOptions then
      FPFlags := FPFlags or 32;
  end
  else
  begin
    FPFlags := $7FFFF860;
    FPFlags := FPFlags shl 1;
    if coPrint in ProtectionOptions then
      FPFlags := FPFlags or 4;
    if coModifyStructure in ProtectionOptions then
      FPFlags := FPFlags or 8;
    if coCopyInformation in ProtectionOptions then
      FPFlags := FPFlags or 16;
    if coModifyAnnotation in ProtectionOptions then
      FPFlags := FPFlags or 32;
    if coPrintHi in ProtectionOptions then
      FPFlags := FPFlags or 2048;
    if coFillAnnotation in ProtectionOptions then
      FPFlags := FPFlags or 256;
    if coAssemble in ProtectionOptions then
      FPFlags := FPFlags or 1024;
    if coExtractInfo in ProtectionOptions then
      FPFlags := FPFlags or 512;
  end;
  if ProtectionEnabled then
    CalcKey;
  if FOutputStream = nil then
    FStream := TFileStream.Create ( FileName, fmCreate )
  else
    FStream := TMemoryStream.Create;
  case Version of
    v13: S:= '%PDF-1.3';
    v14: S:= '%PDF-1.4';
    v15: S:= '%PDF-1.5';
    v16: S:= '%PDF-1.6';
  end;
  SaveToStream ( S );
  SaveToStream ( '%“Â—Ú' );
  FPrinting := True;
  FAborted := False;
  PagesID := GetNextID;
  FCurrentPage := FPages.Add;
  FCurrentPage.CreateCanvas;
end;

procedure TPDFDocument.CalcOwnerPassword;

var
  Key: TRC4Data;
  Pass: array [ 1..32 ] of byte;
  I, J: Byte;
  Digest, DG1: TMD5Byte16;
begin
  if FOwnerPassword <> '' then
    Move ( FOwnerPassword [ 1 ], Pass, 32 );
  for I := 1 to 32 - Length ( FOwnerPassword ) do
    Pass [ I + Length ( FOwnerPassword ) ] := passkey [ I ];
  xCalcMD5 ( @Pass [ 1 ], 32, Digest );
  if FPKL = kl128 then
  begin
    for I := 1 to 50 do
      xCalcMD5 ( @Digest, 16, Digest );
    RC4Init ( Key, @Digest, 16 );
  end
  else
    RC4Init ( Key, @Digest, 5 );
  if FUserPassword <> '' then
    Move ( FUserPassword [ 1 ], Pass, 32 );
  for I := 1 to 32 - Length ( FUserPassword ) do
    Pass [ I + Length ( FUserPassword ) ] := passkey [ I ];
  SetLength ( FOCP, 32 );
  RC4Crypt ( Key, @Pass, @FOCP [ 1 ], 32 );
  if FPKL = kl128 then
    for i := 1 to 19 do
    begin
      for J := 0 to 15 do
        DG1 [ j ] := Digest [ j ] xor I;
      RC4Init ( Key, @DG1, 16 );
      RC4Crypt ( Key, @FOCP [ 1 ], @FOCP [ 1 ], 32 );
    end;
end;

function TPDFDocument.CalcUserPassword: string;
var
  I, J: Byte;
  Op: array [ 1..32 ] of Byte;
  AKey: TRC4Data;
  Context: TMD5Ctx;
  Digest: TMD5Byte16;
  K2: TEncripKey;
  C: string;
begin
  if FPKL = kl40 then
  begin
    RC4Init ( AKey, @FKey, 5 );
    RC4Crypt ( AKey, @PassKey, @op, 32 );
    Result := '';
    for I := 1 to 32 do
      Result := Result + chr ( op [ I ] );
  end
  else
  begin
    MD5Init ( Context );
    MD5Update ( Context, PassKey, 32 );
    C := '';
    for i := 1 to 16 do
      C := C + chr ( StrToInt ( '$' + FileID [ i shl 1 - 1 ] + FileID [ i shl 1 ] ) );
    MD5Update ( Context, C [ 1 ], 16 );
    MD5Final ( Digest, Context );
    for I := 0 to 19 do
    begin
      for J := 1 to 16 do
        K2 [ j ] := FKey [ j ] xor i;
      RC4Init ( AKey, @k2, 16 );
      RC4Crypt ( AKey, @DIgest, @Digest, 16 );
    end;
    SetLength ( Result, 32 );
    Move ( Digest, Result [ 1 ], 16 );
    Randomize;
    for I := 17 to 32 do
      Result [ i ] := char ( Random ( 200 ) + 32 );
  end;
end;

constructor TPDFDocument.Create ( AOwner: TComponent );
begin
  inherited Create ( AOwner );
  FRadioGroups := TList.Create;
  FPages := TPDFPages.Create ( Self, False );
  FActions := TPDFActions.Create ( Self );
  FWaterMarks := TPDFPages.Create ( Self, True );
  FFonts := TPDFFonts.Create ( Self );
  FNotEmbeddedFont := TStringList.Create;
  FImages := TPDFImages.Create ( Self );
  FOutlines := TPDFOutlines.Create ( Self );
  FDocumentInfo := TPDFDocInfo.Create;
  FEmbedFiles := nil;
  FAcroForm := TPDFAcroForm.Create ( Self );
  FDocumentInfo.Creator := 'llPDFLib program';
  FDocumentInfo.CreationDate := Now;
  FDocumentInfo.Producer := 'llPDFLib 3.x';
  FDocumentInfo.Author := 'Windows 9x/NT/2000/XP User';
  FDocumentInfo.Title := 'No Title';
  FDocumentInfo.Subject := 'None';
  FDocumentInfo.Keywords := 'llPDFLib';
  FJSF := TPDFJSFList.Create;
  FCurrentPage := nil;
  FOutputStream := nil;
  FNamedDest := nil;
  FJPEGQuality := 80;
  FAborted := False;
  FPrinting := False;
  FResolution := 72;
  FOpenDocumentAction := nil;
  FOnePass := False;
  EMFImageAsJpeg := False;
  UsedDC := 0;
  FACURL := True;
  FReDraw := True;
end;

destructor TPDFDocument.Destroy;
begin
  FJSF.Free;
  FRadioGroups.Free;
  FDocumentInfo.Free;
  FOutlines.Free;
  FImages.Free;
  FFonts.Free;
  FPages.Free;
  FActions.Free;
  FWaterMarks.Free;
  FNotEmbeddedFont.Free;
  FEmbedFiles := nil;
  FAcroForm.Free;
  FNamedDest := nil;
  DeleteDC ( FDC );
  inherited;
end;


procedure TPDFDocument.EndDoc;
begin
  try
    StoreDocument;
  except
    on Exception do
    begin
      Abort;
      raise;
    end;
  end;
  if FOutputStream <> nil then
  begin
    FStream.Position := 0;
    FOutputStream.CopyFrom ( FStream, FStream.Size );
  end;
  ClearAll;
  FPrinting := False;
  FAborted := False;
  if ( FOutputStream = nil ) and ( AutoLaunch ) then
  try
    ShellExecute ( GetActiveWindow, 'open', PChar ( FFileName ), nil, nil, SW_NORMAL );
  except
  end;
end;

procedure TPDFDocument.NewPage;
begin
  if not FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileNotActivated );
  FCurrentPage.CloseCanvas;
  if OnePass then
    FCurrentPage.Save;
  FCurrentPage := FPages.Add;
  FCurrentPage.CreateCanvas;
end;

procedure TPDFDocument.SetFileName ( const Value: TFileName );
begin
  if FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileInProgress );
  FFileName := Value;
end;

procedure TPDFDocument.SetCompression ( const Value: TCompressionType );
begin
  FCompression := Value;
end;

function TPDFDocument.GetNextID: Integer;
begin
  Inc ( CurID );
  SetLength ( IDOffset, CurID );
  Result := CurID;
end;

procedure TPDFDocument.SaveTrailer;
begin
  SaveToStream ( 'trailer' );
  SaveToStream ( '<<' );
  SaveToStream ( '/Size ' + IntToStr ( CurID + 1 ) );
  SaveToStream ( '/Root ' + IntToStr ( CatalogID ) + ' 0 R' );
  SaveToStream ( '/Info ' + IntToStr ( FDocumentInfo.ID ) + ' 0 R' );
  if FProtectionEnabled then
    SaveToStream ( '/Encrypt ' + IntToStr ( EncriptID ) + ' 0 R' );
  SaveToStream ( '/ID [<' + FileID + '><' + FileID + '>]' );
  SaveToStream ( '>>' );
end;

procedure TPDFDocument.SaveXREF;
var
  I: Integer;
begin
  XREFOffset := FStream.Position;
  SaveToStream ( 'xref' );
  SaveToStream ( '0 ' + IntToStr ( CurID + 1 ) );
  SaveToStream ( IntToStrWithZero ( 0, 10 ) + ' ' + IntToStr ( $FFFF ) + ' f' );
  for I := 0 to CurID - 1 do
    SaveToStream ( IntToStrWithZero ( idoffset [ I ], 10 ) + ' ' + IntToStrWithZero ( 0, 5 ) + ' n' );
end;

procedure TPDFDocument.SaveDocumentInfo;
begin
  StartObj ( FDocumentInfo.ID );
  SaveToStream ( '/Creator (' + EscapeSpecialChar ( EnCodeString ( FProtectionEnabled, FKey, FPKL, FDocumentInfo.ID, FDocumentInfo.Creator ) ) + ')' );
  SaveToStream ( '/CreationDate (' + EscapeSpecialChar ( EnCodeString ( FProtectionEnabled, FKey, FPKL, FDocumentInfo.ID, 'D:' + FormatDateTime ( 'yyyymmddhhnnss', FDocumentInfo.CreationDate ) ) ) + ')' );
  SaveToStream ( '/Producer (' + EscapeSpecialChar ( EnCodeString ( FProtectionEnabled, FKey, FPKL, FDocumentInfo.ID, FDocumentInfo.Producer ) ) + ')' );
  SaveToStream ( '/Author (' + EscapeSpecialChar ( EnCodeString ( FProtectionEnabled, FKey, FPKL, FDocumentInfo.ID, FDocumentInfo.Author ) ) + ')' );
  SaveToStream ( '/Title (' + EscapeSpecialChar ( EnCodeString ( FProtectionEnabled, FKey, FPKL, FDocumentInfo.ID, FDocumentInfo.Title ) ) + ')' );
  SaveToStream ( '/Subject (' + EscapeSpecialChar ( EnCodeString ( FProtectionEnabled, FKey, FPKL, FDocumentInfo.ID, FDocumentInfo.Subject ) ) + ')' );
  SaveToStream ( '/Keywords (' + EscapeSpecialChar ( EnCodeString ( FProtectionEnabled, FKey, FPKL, FDocumentInfo.ID, FDocumentInfo.Keywords ) ) + ')' );
  CloseHObj;
  CloseObj;
end;

procedure TPDFDocument.SetDocumentInfo ( const Value: TPDFDocInfo );
begin
  FDocumentInfo.Creator := Value.Creator;
  FDocumentInfo.CreationDate := Value.CreationDate;
  FDocumentInfo.Author := Value.Author;
  FDocumentInfo.Title := Value.Title;
  FDocumentInfo.Subject := Value.Subject;
  FDocumentInfo.Keywords := Value.Keywords;
end;

procedure TPDFDocument.SaveToStream ( st: string; CR: Boolean );
var
  WS: Ansistring;
  Ad: Pointer;
begin
  WS := AnsiString(st);
  if CR then
    WS := WS + #13#10;
  Ad := @WS [ 1 ];
  FStream.Write ( ad^, Length ( WS ) );
end;

procedure TPDFDocument.SetPageLayout ( const Value: TPageLayout );
begin
  FPageLayout := Value;
end;

procedure TPDFDocument.SetPageMode ( const Value: TPageMode );
begin
  FPageMode := Value;
end;

procedure TPDFDocument.SetCurrentPage ( Index: Integer );
begin
  if not FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileNotActivated );
  if OnePass then
    raise TPDFException.Create ( SCannotChangePageInOnePassMode );
  FCurrentPage.CloseCanvas;
  FCurrentPage := FPages [ Index ];
  FCurrentPage.CreateCanvas;
end;

procedure TPDFDocument.SetOwnerPassword ( const Value: string );
begin
  if FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileInProgress );
  FOwnerPassword := Value;
end;

procedure TPDFDocument.SetProtectionEnabled ( const Value: Boolean );
begin
  if FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileInProgress );
  FProtectionEnabled := Value;
end;

procedure TPDFDocument.SetProtectionOptions (
  const Value: TPDFCtiptoOptions );
begin
  if FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileInProgress );
  FProtectionOptions := Value;
end;

procedure TPDFDocument.SetUserPassword ( const Value: string );
begin
  if FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileInProgress );
  FUserPassword := Value;
end;

procedure TPDFDocument.CalcKey;
var
  Digest: TMD5Byte16;
  W, Z, C: string;
  Cont: TMD5Ctx;
  i: Integer;
begin

  CalcOwnerPassword;
  z := FOCP;
  W := Copy ( FUserPassword, 1, 32 );
  SetLength ( W, 32 );
  if Length ( FUserPassword ) < 32 then
    Move ( PassKey, W [ Length ( FUserPassword ) + 1 ], 32 - Length ( FUserPassword ) );
  C := '';
  for i := 1 to 16 do
    C := C + chr ( StrToInt ( '$' + FileID [ i shl 1 - 1 ] + FileID [ i shl 1 ] ) );
  MD5Init ( Cont );
  MD5Update ( Cont, w [ 1 ], 32 );
  MD5Update ( Cont, z [ 1 ], 32 );
  MD5Update ( Cont, FPFlags, 4 );
  MD5Update ( Cont, C [ 1 ], 16 );
  MD5Final ( Digest, Cont );
  if FPKL = kl128 then
  begin
    for i := 1 to 50 do
      xCalcMD5 ( @Digest, 16, Digest );
    Move ( Digest, FKey, 16 );
  end
  else
    Move ( Digest, FKey, 5 );
end;

procedure TPDFDocument.GenFileID;
var
  s: string;
begin
  s := FileName + FormatDateTime ( 'ddd dd-mm-yyyy hh:nn:ss.zzz', Now );
  FileID := LowerCase ( String(md5result ( AnsiString(s) )) );
end;

procedure TPDFDocument.SetNotEmbeddedFont ( const Value: TStringList );
begin
  FNotEmbeddedFont.Assign ( Value );
end;

function TPDFDocument.GetCurrentPageIndex: Integer;
begin
  if not FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileNotActivated );
  Result := FPages.FPages.IndexOf ( Pointer ( FCurrentPage ) );
end;

function TPDFDocument.AddImage ( Image: TGraphic; Compression: TImageCompressionType; MaskIndex: Integer = -1;
  IsMask: Boolean = False; TransparentColor: TColor = -1 ): Integer;
begin
  if not FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileNotActivated );
  Result := FImages.FImages.IndexOf ( Pointer ( FImages.Add ( Image, Compression, MaskIndex, IsMask, TransparentColor ) ) );
end;

function TPDFDocument.AddImage ( FileName: TFileName; Compression: TImageCompressionType;
  MaskIndex: Integer = -1; IsMask: Boolean = False; TransparentColor: TColor = -1 ): Integer;
begin
  if not FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileNotActivated );
  Result := FImages.FImages.IndexOf ( Pointer ( FImages.Add ( FileName, Compression, MaskIndex, IsMask, TransparentColor ) ) );
end;

function TPDFDocument.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

function TPDFDocument.GetPage ( Index: Integer ): TPDFPage;
begin
  if not FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileNotActivated );
  if OnePass then
    raise TPDFException.Create ( SCannotAccessToPageInOnePassMode );
  if ( Index < 0 ) or ( Index > FPages.Count - 1 ) then
    raise TPDFException.Create ( SOutOfRange );
  Result := FPages [ Index ];
end;

procedure TPDFDocument.SetOutputStream ( const Value: TStream );
begin
  if FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileInProgress );
  FOutputStream := Value;
end;

procedure TPDFDocument.SetJPEGQuality ( const Value: Integer );
begin
  FJPEGQuality := Value;
end;

procedure TPDFDocument.SetAutoLaunch ( const Value: Boolean );
begin
  FAutoLaunch := Value;
end;


function TPDFDocument.GetCanvas: TCanvas;
begin
  if CurrentPage = nil then
    Result := nil
  else
  begin
    Result := CurrentPage.FCanvas;
    CurrentPage.FAskCanvas := True;
  end;
end;

function TPDFDocument.GetPageHeight: Integer;
var
  I: Integer;
begin
  if not FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileNotActivated );
  I := GetDeviceCaps ( UsedDC, LOGPIXELSX );
  Result := MulDiv ( CurrentPage.FHeight, I, 72 );
end;

function TPDFDocument.GetPageNumber: Integer;
begin
  if not FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileNotActivated );
  Result := GetCurrentPageIndex + 1;
end;

function TPDFDocument.GetPageWidth: Integer;
var
  I: Integer;
begin
  if not FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileNotActivated );
  I := GetDeviceCaps ( UsedDC, LOGPIXELSX );
  Result := MulDiv ( CurrentPage.FWidth, I, 72 );
end;

function TPDFDocument.CreateWaterMark: Integer;
var
  P: TPDFPage;
begin
  if not FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileNotActivated );
  P := FWaterMarks.Add;
  Result := FWaterMarks.IndexOf ( P );
end;

function TPDFDocument.GetWaterMark ( Index: Integer ): TPDFPage;
begin
  if not FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileNotActivated );
  Result := FWaterMarks.GetPage ( Index );
end;

procedure TPDFDocument.AppendAction ( const Action: TPDFAction );
var
  i: Integer;
begin
  if not FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileNotActivated );
  if FActions.IndexOf ( Action ) < 0 then
  begin
    i := FActions.Add ( Action );
    if FActions [ i ].ActionID < 1 then
      FActions [ i ].ActionID := GetNextID;
  end;
end;

procedure TPDFDocument.SetOpenDocumentAction ( const Value: TPDFAction );
begin
  if not FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileNotActivated );
  if FOpenDocumentAction = nil then
    FOpenDocumentAction := Value
  else
  begin
    Value.FNext := FOpenDocumentAction;
    FOpenDocumentAction := Value;
  end;
  AppendAction ( Value );
end;

procedure TPDFDocument.CloseHObj;
begin
  SaveToStream ( '>>' );
end;

procedure TPDFDocument.CloseObj;
begin
  SaveToStream ( 'endobj' );
end;

procedure TPDFDocument.StartObj ( var ID: Integer );
var
  Offset: Integer;
begin
  if ID <= 0 then
    ID := GetNextID;
  Offset := FStream.Position;
  if ID > CurID then
    raise TPDFException.Create ( SOutOfRange );
  IDOffset [ ID - 1 ] := Offset;
  SaveToStream ( IntToStr ( ID ) + ' 0 obj' );
  SaveToStream ( '<<', False );
end;

procedure TPDFDocument.CloseStream;
begin
  SaveToStream ( 'endstream' );
end;

procedure TPDFDocument.StartStream;
begin
  SaveToStream ( 'stream' );
end;

procedure TPDFDocument.SetOnePass ( const Value: Boolean );
begin
  if FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileInProgress );
  FOnePass := Value;
end;

procedure TPDFDocument.ClearAll;
begin
  CurID := 0;
  FDocumentInfo.ID := 0;
  PagesID := 0;
  CatalogID := 0;
  EncriptID := 0;
  IDOffset := nil;
  DeleteAllRadioGroups;
  FImages.Clear;
  FFonts.Clear;
  FPages.Clear;
  FWaterMarks.Clear;
  FActions.Clear;
  FOutlines.Clear;
  FCurrentPage := nil;
  FAcroForm.Clear;
  FJSF.Clear;
  FEmbedFiles := nil;
  FNamedDest := nil;
  FOpenDocumentAction := nil;
  if FStream <> nil then
  begin
    FStream.Free;
    FStream := nil;
  end;
end;

procedure TPDFDocument.DeleteAllRadioGroups;
var
  I: Integer;
begin
  for I := 0 to FRadioGroups.Count - 1 do
    TPDFRadioGroup ( FRadioGroups [ I ] ).Free;
  FRadioGroups.Clear;
end;

function TPDFDocument.CreateAction ( CClass: TPDFActionClass ): TPDFAction;
var
  SO: TPDFAction;
begin
  SO := CClass.Create;
  AppendAction ( SO );
  Result := SO;
end;

function TPDFDocument.CreateRadioGroup ( Name: string ): TPDFRadioGroup;
var
  RG: TPDFRadioGroup;
begin
  RG := TPDFRadioGroup.Create ( Self, Name );
  RG.GroupID := GetNextID;
  FRadioGroups.Add ( RG );
  Result := RG;
end;

procedure TPDFDocument.SetVersion ( const Value: TPDFVersion );
begin
  FVersion := Value;
  if Value = v13 then
    FPKL := kl40;
end;

procedure TPDFDocument.StoreDocument;
var
  i, K: Integer;
  NID: Integer;
  NamedDestID: Integer;
  EmbeddedFilesID: Integer;
  NamesUsed: Boolean;
begin
  if not FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileNotActivated );
  FCurrentPage.CloseCanvas;
  if OnePass then
    FCurrentPage.Save;
{$IFDEF LLPDFEVAL}
{$I eval.inc}
{$ENDIF}
  SaveDocumentInfo;
  i := 0;
  while i < FActions.Count do
  begin
    FActions [ i ].Prepare;
    Inc ( i );
  end;
  for i := 0 to FWaterMarks.Count - 1 do
    FWaterMarks [ i ].CloseCanvas;
  for i := 0 to FWaterMarks.Count - 1 do
    FWaterMarks [ i ].Save;

  StartObj ( PagesID );
  SaveToStream ( '/Type /Pages' );
  SaveToStream ( '/Kids [' );
  for i := 0 to FPages.Count - 1 do
    SaveToStream ( IntToStr ( FPages [ i ].PageID ) + ' 0 R' );
  SaveToStream ( ']' );
  SaveToStream ( '/Count ' + IntToStr ( FPages.Count ) );
  CloseHObj;
  CloseObj;
  if not OnePass then
    for i := 0 to FPages.Count - 1 do
      FPages [ i ].Save;
  for i := 0 to FActions.Count - 1 do
    FActions [ i ].Save;
  if Outlines.Count <> 0 then
  begin
    FOutlines.OutlinesID := GetNextID;
    for i := 0 to FOutlines.Count - 1 do
      FOutlines [ i ].OutlineNodeID := GetNextID;
    for i := 0 to FOutlines.Count - 1 do
      FOutlines [ i ].Save;
    StartObj ( FOutlines.OutlinesID );
    SaveToStream ( '/Type /Outlines' );
    SaveToStream ( '/Count ' + IntToStr ( FOutlines.Count ) );
    for i := 0 to FOutlines.Count - 1 do
    begin
      if ( FOutlines [ i ].FParent = nil ) and ( FOutlines [ i ].FPrev = nil ) then
        SaveToStream ( '/First ' + IntToStr ( FOutlines [ i ].OutlineNodeID ) + ' 0 R' );
      if ( FOutlines [ i ].FParent = nil ) and ( FOutlines [ i ].FNext = nil ) then
        SaveToStream ( '/Last ' + IntToStr ( FOutlines [ i ].OutlineNodeID ) + ' 0 R' );
    end;
    CloseHObj;
    CloseObj;
  end;
  for i := 0 to FRadioGroups.Count - 1 do
    TPDFRadioGroup ( FRadioGroups [ i ] ).Save;
  if not FAcroForm.Empty then
    FAcroForm.Save;
  NamedDestID := StoreNamedDests;
  EmbeddedFilesID := StoreEmbeddedFiles;
  StartObj ( CatalogID );
  SaveToStream ( '/Type /Catalog' );
  SaveToStream ( ' /Pages ' + IntToStr ( PagesID ) + ' 0 R' );
  case PageLayout of
    plSinglePage: SaveToStream ( '/PageLayout /SinglePage' );
    plOneColumn: SaveToStream ( '/PageLayout /OneColumn' );
    plTwoColumnLeft: SaveToStream ( '/Pagelayout /TwoColumnLeft' );
    plTwoColumnRight: SaveToStream ( '/PageLayout /TwoColumnRight' );
  end;
  if ViewerPreferences <> [ ] then
  begin
    SaveToStream ( '/ViewerPreferences <<' );
    if vpHideToolBar in ViewerPreferences then
      SaveToStream ( '/HideToolbar true' );
    if vpHideMenuBar in ViewerPreferences then
      SaveToStream ( '/HideMenubar true' );
    if vpHideWindowUI in ViewerPreferences then
      SaveToStream ( '/HideWindowUI true' );
    if vpFitWindow in ViewerPreferences then
      SaveToStream ( '/FitWindow true' );
    if vpCenterWindow in ViewerPreferences then
      SaveToStream ( '/CenterWindow true' );
    SaveToStream ( '>>' );
  end;
  case PageMode of
    pmUseNone: SaveToStream ( '/PageMode /UseNone' );
    pmUseOutlines: SaveToStream ( '/PageMode /UseOutlines' );
    pmUseThumbs: SaveToStream ( '/PageMode /UseThumbs' );
    pmFullScreen: SaveToStream ( '/PageMode /FullScreen' );
  end;
  if FOpenDocumentAction <> nil then
    SaveToStream ( '/OpenAction ' + IntToStr ( FOpenDocumentAction.ActionID ) + ' 0 R' );
  if FOutlines.Count <> 0 then
    SaveToStream ( '/Outlines ' + IntToStr ( FOutlines.OutlinesID ) + ' 0 R' );
  if not FAcroForm.Empty then
    SaveToStream ( '/AcroForm ' + IntToStr ( FAcroForm.AcroID ) + ' 0 R' );
  NamesUsed := (( FJSF.Count <> 0 ) or ( NamedDestID <> 0 ) or (EmbeddedFilesID <> 0) );
  if  NamesUsed then
  begin
    NID := GetNextID;
    SaveToStream ( '/Names ' + IntToStr ( NID ) + ' 0 R' );
  end;
  CloseHObj;
  CloseObj;
  if NamesUsed then
  begin
    StartObj ( NID );
    if NamedDestID <> 0 then
      SaveToStream ( '/Dests ' + IntToStr ( NamedDestID ) + ' 0 R' );
    if ( FJSF.Count <> 0 ) then
    begin
      NID := GetNextID;
      SaveToStream ( '/JavaScript ' + IntToStr ( NID ) + ' 0 R' );
    end;
    if EmbeddedFilesID <> 0  then
      SaveToStream ( '/EmbeddedFiles ' + IntToStr ( EmbeddedFilesID ) + ' 0 R' );
    CloseHObj;
    CloseObj;
    if FJSF.Count <> 0 then
    begin
      for i := 0 to FJSF.Count - 1 do
        FJSF [ i ].ID := GetNextID;
      StartObj ( NID );
      SaveToStream ( '/Names [', False );
      for i := 0 to FJSF.Count - 1 do
        SaveToStream ( ' (' + EscapeSpecialChar ( EnCodeString ( FProtectionEnabled, FKey, FPKL, NID, FJSF [ i ].Name ) ) + ') ' + IntToStr ( FJSF [ i ].ID ) + ' 0 R', False );
      SaveToStream ( ']' );
      CloseHObj;
      CloseObj;
      for i := 0 to FJSF.Count - 1 do
      begin
        K := FJSF [ i ].ID;
        StartObj ( K );
        SaveToStream ( '/S /JavaScript /JS (' + EscapeSpecialChar ( EnCodeString ( FProtectionEnabled, FKey, FPKL, FJSF [ i ].ID,
          'function ' + FJSF [ i ].Name + '(' + FJSF [ i ].Params + ')' + '{' + FJSF [ i ].Body + '}' ) ) + ') ' );
        CloseHObj;
        CloseObj;
      end;
    end;
  end;
  if FProtectionEnabled then
  begin
    StartObj ( EncriptID );
    SaveToStream ( '/Filter /Standard' );
    if FPKL = kl40 then
    begin
      SaveToStream ( '/V 1' );
      SaveToStream ( '/R 2' );
    end
    else
    begin
      SaveToStream ( '/V 2' );
      SaveToStream ( '/R 3' );
      SaveToStream ( '/Length 128' );
    end;
    SaveToStream ( '/P ' + IntToStr ( FPFlags ) );
    SaveToStream ( '/O (' + EscapeSpecialChar ( FOCP ) + ')' );
    SaveToStream ( '/U (' + EscapeSpecialChar ( CalcUserPassword ) + ')' );
    CloseHObj;
    CloseObj;
  end;
  for i := 0 to FFonts.Count - 1 do
    FFonts [ i ].Save ( Self );
  if not FOnePass then
    for i := 0 to FImages.Count - 1 do
      FImages [ i ].Save ( Self );
  SaveXREF;
  SaveTrailer;
  SaveToStream ( 'startxref' );
  SaveToStream ( IntToStr ( XREFOffset ) );
  SaveToStream ( '%%EOF' );
end;


procedure TPDFDocument.SetProtectionKeyLength ( const Value: TPDFKeyLength );
begin
  if FPKL <> Value then
  begin
    if Printing then
      raise TPDFException.Create ( 'Cannot change this value' );
    FPKL := Value;
    if ( Value = kl128 ) and ( FVersion = v13) then
      FVersion := v14 ;
  end;
end;

procedure TPDFDocument.AddJSFunction ( AName, AParams, ABody: string );
begin
  FJSF.Add ( AName, AParams, ABody );
end;


procedure TPDFDocument.SetEmulateStd ( const Value: Boolean );
begin
  if FEmulateStd = Value then
    Exit;
  if Printing then
    raise TPDFException.Create ( 'Cannot change this value' );
  FEmulateStd := Value;
end;

procedure TPDFDocument.AddNameDestination ( Dest: string; Page,
  TopOffset: Integer );
var
  i: Integer;
begin
  if ( Page < 0 ) or ( Page >= FPages.GetCount ) then
    raise TPDFException.Create ( SOutOfRange );
  if TopOffset < 0 then
    raise TPDFException.Create ( STopOffsetCannotBeNegative );
  i := Length ( FnamedDest );
  SetLength ( FnamedDest, i + 1 );
  FNamedDest [ i ].Dest := Dest;
  FNamedDest [ i ].Page := Page;
  FNamedDest [ i ].TopOffset := TopOffset;
end;

function TPDFDocument.StoreNamedDests: Integer;
var
  i, k: Integer;

  procedure QuickSort ( var A: TNamedDestRecordArray; iLo, iHi: Integer );
  var
    Lo, Hi: Integer;
    ap: TNamedDestRecord;
    Mid: string;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := A [ ( Lo + Hi ) div 2 ].Dest;
    repeat
      while A [ Lo ].Dest < Mid do
        Inc ( Lo );
      while A [ Hi ].Dest > Mid do
        Dec ( Hi );
      if Lo <= Hi then
      begin
        ap := A [ Lo ];
        A [ Lo ] := A [ Hi ];
        A [ Hi ] := ap;
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
  k := Length ( FNamedDest );
  if k = 0 then
  begin
    Result := 0;
    Exit;
  end;
  if k > 1 then
    QuickSort ( FnamedDest, low ( FNamedDest ), High ( FNamedDest ) );
  Result := GetNextID;
  StartObj ( Result );
  SaveToStream ( '/Limits [', False );
  SaveToStream ( ' (' + EscapeSpecialChar ( EnCodeString ( FProtectionEnabled, FKey, FPKL, Result, FNamedDest [ 0 ].Dest ) ) + ') ' );
  SaveToStream ( ' (' + EscapeSpecialChar ( EnCodeString ( FProtectionEnabled, FKey, FPKL, Result, FNamedDest [ k - 1 ].Dest ) ) + ') ' );
  SaveToStream ( ']' );
  SaveToStream ( '/Names [', False );
  for i := 0 to k - 1 do
  begin
    FNamedDest [ i ].ID := GetNextID;
    SaveToStream ( ' (' + EscapeSpecialChar ( EnCodeString ( FProtectionEnabled, FKey, FPKL, Result, FNamedDest [ i ].Dest ) ) + ') ' + IntToStr ( FNamedDest [ i ].ID ) + ' 0 R', False );
  end;
  SaveToStream ( ']' );
  CloseHObj;
  CloseObj;
  for i := 0 to k - 1 do
  begin
    StartObj ( FNamedDest [ i ].ID );
    SaveToStream ( '/D [' + IntToStr ( FPages [ FNamedDest [ i ].Page ].PageID ) +
      ' 0 R /FitH ' + IntToStr ( Round ( FPages [ FNamedDest [ i ].Page ].ExtToIntY ( FNamedDest [ i ].TopOffset ) ) ) + ']' );
    CloseHObj;
    CloseObj;
  end;

end;

procedure TPDFDocument.AddEmbedFile(FileName: string; Title:string);
var
  i:Integer;
begin
  i := Length ( FEmbedFiles );
  SetLength ( FEmbedFiles, i+1);
  FEmbedFiles[i].Title := Title;
  FEmbedFiles[i].FileName := FileName;
end;

function TPDFDocument.StoreEmbeddedFiles:Integer;
var
  i, Len:Integer;
  MS:TMemoryStream;
  CS:TCompressionStream;
  FS:TFileStream;
  TID, RS:Integer;
  S:string;
  procedure QuickSort ( var A: TEmbedFileRecordArray; iLo, iHi: Integer );
  var
    Lo, Hi: Integer;
    ap: TEmbedFileRecord;
    Mid: string;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := A [ ( Lo + Hi ) div 2 ].Title;
    repeat
      while A [ Lo ].Title < Mid do
        Inc ( Lo );
      while A [ Hi ].Title > Mid do
        Dec ( Hi );
      if Lo <= Hi then
      begin
        ap := A [ Lo ];
        A [ Lo ] := A [ Hi ];
        A [ Hi ] := ap;
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
  Len := Length( FEmbedFiles );
  if  Len = 0 then
  begin
    Result:= 0;
    Exit;
  end;
  if Len > 1 then
    QuickSort ( FEmbedFiles, low ( FEmbedFiles ), High ( FEmbedFiles ) );
  Result := GetNextID;
  StartObj ( Result );
  SaveToStream ( '/Names [', False );
  for i := 0 to Len - 1 do
  begin
    FEmbedFiles [ i ].ID := GetNextID;
    SaveToStream ( ' (' + EscapeSpecialChar ( EnCodeString ( FProtectionEnabled, FKey, FPKL, Result, FEmbedFiles [ i ].Title ) ) + ') ' + IntToStr ( FEmbedFiles [ i ].ID ) + ' 0 R', False );
  end;
  SaveToStream ( ']' );
  CloseHObj;
  CloseObj;
  for i := 0 to Len - 1 do
  begin
    FS := TFileStream.Create(FEmbedFiles[i].FileName, fmOpenRead )	;
    try
      RS := FS.Size;
      FS.Position := 0;
      MS := TMemoryStream.Create;
      try
        CS := TCompressionStream.Create ( clMax, MS );
        try
          CS.CopyFrom ( FS, FS.Size );
        finally
          CS.Free;
        end;
        TID := GetNextID;
        StartObj ( TID );
        SaveToStream ( '/Filter /FlateDecode /Length ' + IntToStr ( MS.Size ) );
        SaveToStream ( '/Params <</Size '+IntToStr ( RS )+'>>');
        CloseHObj;
        StartStream;
        ms.Position := 0;
        EnCodeStream ( FProtectionEnabled, FKey, FPKL, TID, MS );
        FStream.CopyFrom ( MS, MS.Size );
        SaveToStream ( '' );
        CloseStream;
        CloseObj;
      finally
        MS.Free;
      end;
    finally
      FS.Free;
    end;
    StartObj ( FEmbedFiles [ i ].ID );
    SaveToStream ( '/Type /Filespec /F ', False );
    S := ExtractFileName(FEmbedFiles [ i ].FileName );
    SaveToStream ( '('+ EscapeSpecialChar ( EnCodeString ( FProtectionEnabled, FKey, FPKL, FEmbedFiles [ i ].ID, S ) ) + ')' );
    SaveToStream ( '/EF <</F '+IntToStr( TID )+' 0 R >>' );
    CloseHObj;
    CloseObj;
  end;
end;

procedure TPDFDocument.SetUsedDC(const Value: HDC);
var
 DC:HDC;
begin
  if FPrinting then
    raise TPDFException.Create ( SGenerationPDFFileInProgress );
  DeleteDC (FDC);
  if Value = 0 then
  begin
    DC := GetDC ( 0 );
    FDC := CreateCompatibleDC ( DC );
    ReleaseDC ( 0, DC );
  end else FDC := CreateCompatibleDC ( Value );
  FUseScreen := (Value = 0);
end;

{ TPDFPage }

procedure TPDFPage.AppendAction ( Action: string );
begin
  FContent.Add ( Action );
end;

function TPDFPage.Arc ( X1, Y1, x2, y2, BegAngle,
  EndAngle: Extended ): TDoublePoint;
var
  d: TDoublePoint;
begin
  FPathInited := True;
  D := RawArc ( ExtToIntX ( X1 ), ExtToIntY ( Y1 ), ExtToIntX ( X2 ), ExtToIntY ( Y2 ), -EndAngle, -BegAngle );
  Result := DPoint ( IntToExtX ( d.x ), IntToExtY ( d.y ) );
end;

function TPDFPage.Arc ( X1, Y1, x2, y2, x3, y3, x4, y4: Extended ): TDoublePoint;
var
  d: TDoublePoint;
begin
  FPathInited := True;
  D := RawArc ( ExtToIntX ( X1 ), ExtToIntY ( Y1 ), ExtToIntX ( X2 ), ExtToIntY ( Y2 ),
    ExtToIntX ( X4 ), ExtToIntY ( Y4 ), ExtToIntX ( X3 ), ExtToIntY ( y3 ) );
  Result := DPoint ( IntToExtX ( d.x ), IntToExtY ( d.y ) );
end;

procedure TPDFPage.BeginText;
begin
  if FTextInited then
    Exit;
  AppendAction ( 'BT' );
  TP.x := 0;
  TP.y := 0;
  FTextInited := True;
end;

procedure TPDFPage.Circle ( X, Y, R: Extended );
begin
  FPathInited := True;
  RawCircle ( ExtToIntX ( X ), ExtToIntY ( Y ), ExtToIntX ( R ) );
end;

procedure TPDFPage.Clip;
begin
  EndText;
  if FPathInited then
    AppendAction ( 'W' );
end;

procedure TPDFPage.ClosePath;
begin
  EndText;
  AppendAction ( 'h' );
end;

procedure TPDFPage.Comment ( st: string );
begin
  AppendAction ( '% ' + st );
end;

constructor TPDFPage.Create ( AOwner: TPDFDocument );
begin
  FOwner := AOwner;
  PageID := FOwner.GetNextID;
  FAnnot := TList.Create;
  FCharset := 0;
  FCurrentFontName := 'Helvetica';
  FCurrentFontSize := 8;
  FCurrentFontStyle := [ ];
  FFontIsChanged := True;
  FUnicodeUse := False;
  FCurrentDash := '[] 0';
  FContent := TStringList.Create;
  FLinkedFont := nil;
  FLinkedImages := TList.Create;
  FForms := TPDFPages.Create ( FOwner, True );
  FMF := nil;
  FBaseLine := False;
  FRes := FOwner.FResolution;
  D2P := FRes / 72;
  Size := psA4;
  PageRotate := pr0;
  FCharSpace := 0;
  FWordSpace := 0;
  FRemoveCR := False;
  FEmulationEnabled := True;
  FHorizontalScaling := 100;
  FTextLeading := 0;
  FRender := 0;
  FRise := 0;
  FTextInited := False;
  FSaveCount := 0;
  GStateSave;
  Factions := False;
  FPathInited :=False;
  FWaterMark := -1;
  FThumbnail := -1;
  FMatrix.a := 1;
  FMatrix.b := 0;
  FMatrix.c := 0;
  FMatrix.d := 1;
  FMatrix.x := 0;
  FMatrix.y := 0;
  FF := FMatrix;
  FTextUsed := False;
  FColorUsed := False;
  FGrayUsed := False;
  FCanvasOver := True;
end;

procedure TPDFPage.Curveto ( X1, Y1, X2, Y2, X3, Y3: Extended );
begin
  FPathInited := true;
  RawCurveto ( ExtToIntX ( x1 ), ExtToIntY ( y1 ), ExtToIntX ( x2 ), ExtToIntY ( y2 ), ExtToIntX ( x3 ), ExtToIntY ( y3 ) );
end;

destructor TPDFPage.Destroy;
begin
  EndText;
  DeleteCanvas;
  DeleteAllAnnotations;
  FAnnot.Free;
  FLinkedImages.Free;
  FLinkedFont := nil;
  FContent.Free;
  FForms.Free;
  inherited;
end;

procedure TPDFPage.DrawArcWithBezier ( CenterX, CenterY, RadiusX, RadiusY,
  StartAngle, SweepRange: Extended; UseMoveTo: Boolean );
var
  Coord, C2: array [ 0..3 ] of TDoublePoint;
  a, b, c, x, y: Extended;
  ss, cc: Double;
  i: Integer;
begin
  if SweepRange = 0 then
  begin
    if UseMoveTo then
      RawMoveTo ( CenterX + RadiusX * cos ( StartAngle ),
        CenterY - RadiusY * sin ( StartAngle ) );
    RawLineTo ( CenterX + RadiusX * cos ( StartAngle ),
      CenterY - RadiusY * sin ( StartAngle ) );
    Exit;
  end;
  b := sin ( SweepRange / 2 );
  c := cos ( SweepRange / 2 );
  a := 1 - c;
  x := a * 4 / 3;
  y := b - x * c / b;
  ss := sin ( StartAngle + SweepRange / 2 );
  cc := cos ( StartAngle + SweepRange / 2 );
  Coord [ 0 ] := DPoint ( c, b );
  Coord [ 1 ] := DPoint ( c + x, y );
  Coord [ 2 ] := DPoint ( c + x, -y );
  Coord [ 3 ] := DPoint ( c, -b );
  for i := 0 to 3 do
  begin
    C2 [ i ].x := CenterX + RadiusX * ( Coord [ i ].x * cc + Coord [ i ].y * ss ) - 0.0001;
    C2 [ i ].y := CenterY + RadiusY * ( -Coord [ i ].x * ss + Coord [ i ].y * cc ) - 0.0001;
  end;
  if UseMoveTo then
    RawMoveTo ( C2 [ 0 ].x, C2 [ 0 ].y );
  RawCurveto ( C2 [ 1 ].x, C2 [ 1 ].y, C2 [ 2 ].x, C2 [ 2 ].y, C2 [ 3 ].x, C2 [ 3 ].y );
end;

procedure TPDFPage.Ellipse ( X1, Y1, X2, Y2: Extended );
begin
  FPathInited := True;
  RawEllipse ( ExtToIntX ( X1 ), ExtToIntY ( Y1 ), ExtToIntX ( X2 ), ExtToIntY ( Y2 ) );
end;

procedure TPDFPage.EndText;
begin
  if not FTextInited then
    Exit;
  AppendAction ( 'ET' );
  FTextInited := False;
end;

procedure TPDFPage.EoClip;
begin
  EndText;
  if FPathInited then
    AppendAction ( 'W*' );
end;

procedure TPDFPage.EoFill;
begin
  EndText;
  FPathInited := False;
  AppendAction ( 'f*' );
end;

procedure TPDFPage.EoFillAndStroke;
begin
  EndText;
  FPathInited := False;
  AppendAction ( 'B*' );
end;

function TPDFPage.ExtToIntX ( AX: Extended ): Extended;
begin
  Result := AX / D2P;
  Factions := True;
end;

function TPDFPage.ExtToIntY ( AY: Extended ): Extended;
begin
  Result := FHeight - AY / D2P;
  Factions := True;
end;

procedure TPDFPage.Fill;
begin
  EndText;
  FPathInited := False;
  AppendAction ( 'f' );
end;

procedure TPDFPage.FillAndStroke;
begin
  EndText;
  FPathInited := False;
  AppendAction ( 'B' );
end;

procedure TPDFPage.GStateRestore;
begin
  if FSaveCount <> 1 then
  begin
    EndText;
    AppendAction ( 'Q' );
    Dec ( FSaveCount );
    if FCurrentFontIndex > FSaveCount then
      FFontIsChanged := True;
  end;
end;

procedure TPDFPage.GStateSave;
begin
  EndText;
  Inc ( FSaveCount );
  AppendAction ( 'q' );
end;

function TPDFPage.IntToExtX ( AX: Extended ): Extended;
begin
  Result := AX * D2P;
  Factions := True;
end;

function TPDFPage.IntToExtY ( AY: Extended ): Extended;
begin
  Result := ( FHeight - AY ) * D2P;
  Factions := True;
end;

procedure TPDFPage.LineTo ( X, Y: Extended );
begin
  FPathInited := True;
  RawLineTo ( ExttointX ( X ), ExtToIntY ( Y ) );
end;

procedure TPDFPage.MoveTo ( X, Y: Extended );
begin
  FPathInited := True;
  RawMoveTo ( ExttointX ( X ), ExtToIntY ( Y ) );
end;

procedure TPDFPage.NewPath;
begin
  EndText;
  AppendAction ( 'n' );
end;

procedure TPDFPage.NoDash;
begin
  EndText;
  SetDash ( '[] 0' );
end;

procedure TPDFPage.Pie ( X1, Y1, x2, y2, BegAngle, EndAngle: Extended );
begin
  RawPie ( ExtToIntX ( X1 ), ExtToIntY ( Y1 ), ExtToIntX ( X2 ), ExtToIntY ( Y2 ), -EndAngle, -BegAngle );
end;

procedure TPDFPage.Pie ( X1, Y1, x2, y2, x3, y3, x4, y4: Extended );
begin
  RawPie ( ExtToIntX ( X1 ), ExtToIntY ( Y1 ), ExtToIntX ( X2 ),
    ExtToIntY ( Y2 ), ExtToIntX ( X4 ), ExtToIntY ( Y4 ), ExtToIntX ( X3 ), ExtToIntY ( y3 ) );
end;

function TPDFPage.RawArc ( X1, Y1, x2, y2, x3, y3, x4,
  y4: Extended ): TDoublePoint;
var
  CenterX, CenterY: Extended;
  RadiusX, RadiusY: Extended;
  StartAngle,
    EndAngle,
    SweepRange: Extended;
  UseMoveTo: Boolean;
begin
  CenterX := ( x1 + x2 ) / 2;
  CenterY := ( y1 + y2 ) / 2;
  RadiusX := ( abs ( x1 - x2 ) - 1 ) / 2;
  RadiusY := ( abs ( y1 - y2 ) - 1 ) / 2;
  if RadiusX < 0 then
    RadiusX := 0;
  if RadiusY < 0 then
    RadiusY := 0;

  StartAngle := ArcTan2 ( - ( y3 - CenterY ) * RadiusX,
    ( x3 - CenterX ) * RadiusY );
  EndAngle := ArcTan2 ( - ( y4 - CenterY ) * RadiusX,
    ( x4 - CenterX ) * RadiusY );
  SweepRange := EndAngle - StartAngle;

  if SweepRange <= 0 then
    SweepRange := SweepRange + 2 * PI;

  Result := DPoint ( CenterX + RadiusX * cos ( StartAngle ),
    CenterY - RadiusY * sin ( StartAngle ) );

  UseMoveTo := True;
  while SweepRange > PI / 2 do
  begin
    DrawArcWithBezier ( CenterX, CenterY, RadiusX, RadiusY,
      StartAngle, PI / 2, UseMoveTo );
    SweepRange := SweepRange - PI / 2;
    StartAngle := StartAngle + PI / 2;
    UseMoveTo := False;
  end;
  if SweepRange >= 0 then
    DrawArcWithBezier ( CenterX, CenterY, RadiusX, RadiusY,
      StartAngle, SweepRange, UseMoveTo );
end;


function TPDFPage.RawArc ( X1, Y1, x2, y2, BegAngle,
  EndAngle: Extended ): TDoublePoint;
var
  CenterX, CenterY: Extended;
  RadiusX, RadiusY: Extended;
  StartAngle,
    EndsAngle,
    SweepRange: Extended;
  UseMoveTo: Boolean;
begin
  CenterX := ( x1 + x2 ) / 2;
  CenterY := ( y1 + y2 ) / 2;
  RadiusX := ( abs ( x1 - x2 ) - 1 ) / 2;
  RadiusY := ( abs ( y1 - y2 ) - 1 ) / 2;
  if RadiusX < 0 then
    RadiusX := 0;
  if RadiusY < 0 then
    RadiusY := 0;

  StartAngle := BegAngle * pi / 180;
  EndsAngle := EndAngle * pi / 180;
  SweepRange := EndsAngle - StartAngle;

  if SweepRange < 0 then
    SweepRange := SweepRange + 2 * PI;

  Result := DPoint ( CenterX + RadiusX * cos ( StartAngle ),
    CenterY - RadiusY * sin ( StartAngle ) );
  UseMoveTo := True;
  while SweepRange > PI / 2 do
  begin
    DrawArcWithBezier ( CenterX, CenterY, RadiusX, RadiusY,
      StartAngle, PI / 2, UseMoveTo );
    SweepRange := SweepRange - PI / 2;
    StartAngle := StartAngle + PI / 2;
    UseMoveTo := False;
  end;
  if SweepRange >= 0 then
    DrawArcWithBezier ( CenterX, CenterY, RadiusX, RadiusY,
      StartAngle, SweepRange, UseMoveTo );
end;

procedure TPDFPage.RawCircle ( X, Y, R: Extended );
const
  b: Extended = 0.5522847498;
begin
  RawMoveto ( X + R, Y );
  RawCurveto ( X + R, Y + b * R, X + b * R, Y + R, X, Y + R );
  RawCurveto ( X - b * R, Y + R, X - R, Y + b * R, X - R, Y );
  RawCurveto ( X - R, Y - b * R, X - b * R, Y - R, X, Y - R );
  RawCurveto ( X + b * R, Y - R, X + R, Y - b * R, X + R, Y );
end;

procedure TPDFPage.RawConcat ( A, B, C, D, E, F: Extended );
begin
  EndText;
  FF.a := A;
  FF.b := B;
  FF.c := C;
  FF.d := D;
  FF.x := E;
  FF.y := F;
  AppendAction ( FormatFloat ( A ) + ' ' + FormatFloat ( B ) + ' ' +
    FormatFloat ( C ) + ' ' + FormatFloat ( D ) + ' ' +
    FormatFloat ( E ) + ' ' + FormatFloat ( F ) + ' cm' );
end;

procedure TPDFPage.RawCurveto ( X1, Y1, X2, Y2, X3, Y3: Extended );
begin
  EndText;
  AppendAction ( FormatFloat ( x1 ) + ' ' + FormatFloat ( y1 ) + ' ' + FormatFloat ( x2 ) + ' ' +
    FormatFloat ( y2 ) + ' ' + FormatFloat ( x3 ) + ' ' + FormatFloat ( y3 ) + ' c' );
  FX := X3;
  FY := Y3;
end;

procedure TPDFPage.RawEllipse ( x1, y1, x2, y2: Extended );
const
  b = 0.5522847498;
var
  RX, RY, X, Y: Extended;
begin
  Rx := ( x2 - x1 ) / 2;
  Ry := ( y2 - y1 ) / 2;
  X := x1 + Rx;
  Y := y1 + Ry;
  RawMoveto ( X + Rx, Y );
  RawCurveto ( X + RX, Y + b * RY, X + b * RX, Y + RY, X, Y + RY );
  RawCurveto ( X - b * RX, Y + RY, X - RX, Y + b * RY, X - RX, Y );
  RawCurveto ( X - RX, Y - b * RY, X - b * RX, Y - RY, X, Y - RY );
  RawCurveto ( X + b * RX, Y - RY, X + RX, Y - b * RY, X + RX, Y );
end;

procedure TPDFPage.RawLineTo ( X, Y: Extended );
begin
  EndText;
  AppendAction ( FormatFloat ( X ) + ' ' + FormatFloat ( Y ) + ' l' );
  FX := X;
  FY := Y;
end;

procedure TPDFPage.RawMoveTo ( X, Y: Extended );
begin
  EndText;
  AppendAction ( FormatFloat ( X ) + ' ' + FormatFloat ( Y ) + ' m' );
  FX := X;
  FY := Y;
end;


function TPDFPage.RawPie ( X1, Y1, x2, y2, BegAngle,
  EndAngle: Extended ): TDoublePoint;
var
  CX, CY: Extended;
  dp: TDoublePoint;
begin
  dp := RawArc ( X1, Y1, x2, y2, BegAngle, EndAngle );
  CX := X1 + ( x2 - X1 ) / 2;
  CY := Y1 + ( Y2 - Y1 ) / 2;
  RawLineTo ( CX, CY );
  RawMoveTo ( dp.x, dp.y );
  RawLineTo ( CX, CY );
end;

function TPDFPage.RawPie ( X1, Y1, x2, y2, x3, y3, x4,
  y4: Extended ): TDoublePoint;
var
  CX, CY: Extended;
  dp: TDoublePoint;
begin
  dp := RawArc ( X1, Y1, x2, y2, x3, y3, x4, y4 );
  CX := X1 + ( x2 - X1 ) / 2;
  CY := Y1 + ( Y2 - Y1 ) / 2;
  RawLineTo ( CX, CY );
  RawMoveTo ( dp.x, dp.y );
  RawLineTo ( CX, CY );
end;

procedure TPDFPage.RawRect ( X, Y, W, H: Extended );
begin
  EndText;
  AppendAction ( FormatFloat ( x ) + ' ' + FormatFloat ( y ) + ' ' +
    FormatFloat ( w ) + ' ' + FormatFloat ( h ) + ' re' );
  FX := X;
  FY := Y;
end;

procedure TPDFPage.RawRectRotated ( X, Y, W, H, Angle: Extended );
var
  xo, yo: Extended;
begin
  RawMoveto ( x, y );
  RotateCoordinate ( w, 0, angle, xo, yo );
  RawLineto ( x + xo, y + yo );
  RotateCoordinate ( w, h, angle, xo, yo );
  RawLineto ( x + xo, y + yo );
  RotateCoordinate ( 0, h, angle, xo, yo );
  RawLineto ( x + xo, y + yo );
  RawLineto ( x, y );
end;

procedure TPDFPage.RawSetTextPosition ( X, Y, Orientation: Extended );
var
  c, s, g: Extended;
begin
  BeginText;
  g := PI * Orientation / 180.0;
  c := cos ( g ) * FCurrentFontSize;
  s := sin ( g ) * FCurrentFontSize;
  AppendAction ( FormatFloat ( c ) + ' ' + FormatFloat ( s ) + ' ' + FormatFloat ( -s ) + ' ' +
    FormatFloat ( c ) + ' ' + FormatFloat ( x ) + ' ' + FormatFloat ( y ) + ' Tm' );
  FRealAngle := Orientation;
  TP.x := X;
  TP.y := Y;
end;

procedure TPDFPage.RawTextOut ( X, Y, Orientation: Extended;
  TextStr: string );
begin
  RawSetTextPosition ( X, Y, Orientation );
  TextShow ( TextStr );
end;

procedure TPDFPage.RawUnicodeTextOut ( X, Y, Orientation: Extended;
  Text: PWord; Len: Integer );
begin
  RawSetTextPosition ( X, Y, Orientation );
  UnicodeTextShow ( Text, Len );
end;


procedure TPDFPage.RawTranslate ( XT, YT: Extended );
begin
  RawConcat ( 1, 0, 0, 1, xt, yt );
end;

procedure TPDFPage.Rectangle ( X1, Y1, X2, Y2: Extended );
var
  convw, convh: Extended;
begin
  NormalizeRect ( x1, y1, x2, y2 );
  FPathInited := True;
  convw := ExtToIntX ( X2 ) - ExtToIntX ( X1 );
  convh := ExtToIntY ( Y1 ) - ExtToIntY ( Y2 );
  RawRect ( ExtToIntX ( X1 ), ExtToIntY ( y2 ), convw, convh );
end;

procedure TPDFPage.RectRotated ( X, Y, W, H, Angle: Extended );
var
  convw, convh: Extended;
begin
  convw := ExtToIntX ( X + W ) - ExtToIntX ( X );
  convh := ExtToIntY ( Y + H ) - ExtToIntY ( Y );
  RawRectRotated ( ExtToIntX ( X ), ExtToIntY ( y ), convw, convh, Angle );
end;

procedure TPDFPage.Rotate ( Angle: Extended );
var
  vsin, vcos: Extended;
begin
  Angle := Angle * ( PI / 180 );
  vsin := sin ( angle );
  vcos := cos ( angle );
  RawConcat ( vcos, vsin, -vsin, vcos, 0, 0 );
end;

procedure TPDFPage.Save;
var
  I: Integer;
  MS: TMemoryStream;
  CS: TCompressionStream;
  S, O: string;
begin
  for I := FSaveCount downto 1 do
    GStateRestore;
{$IFDEF LLPDFEVAL}
  if not FIsForm then
  begin
    NoDash;
    BeginText;
    SetLineWidth ( 1 );
    SetRGBColorFill ( 0.5, 0.5, 0.5 );
    SetRGBColorStroke ( 1, 0, 0 );
    SetTextRenderingMode ( 2 );
    SetActiveFont ( 'Helvetica', [ ], 40 );
    SetHorizontalScaling ( 50 );
{$IFNDEF CB}
    TextOut ( ( Width - GetTextWidth ( s1 ) ) / 2, ( Height - 40 ) shr 1 - 40, 0, s1 );
    TextOut ( ( Width - GetTextWidth ( s2 ) ) / 2, ( Height - 40 ) shr 1, 0, s2 );
    TextOut ( ( Width - GetTextWidth ( s3 ) ) / 2, ( Height - 40 ) shr 1 + 40, 0, s3 );
{$ELSE}
    TextOutput ( ( Width - GetTextWidth ( s1 ) ) / 2, ( Height - 40 ) shr 1 - 40, 0, s1 );
    TextOutput ( ( Width - GetTextWidth ( s2 ) ) / 2, ( Height - 40 ) shr 1, 0, s2 );
    TextOutput ( ( Width - GetTextWidth ( s3 ) ) / 2, ( Height - 40 ) shr 1 + 40, 0, s3 );
{$ENDIF}
    SetUrl ( Rect ( round ( ( Width - GetTextWidth ( s2 ) ) / 2 ), ( Height - 40 ) shr 1 + 50, round ( ( Width + GetTextWidth ( s2 ) ) / 2 ),
      ( Height - 40 ) shr 1 + 90 ), s3 );
    EndText;
  end;
{$ENDIF}
  PrepareID;
  for I := 0 to FForms.Count - 1 do
    FForms [ I ].Save;
  ResourceID := FOwner.GetNextID;
  if not FIsForm then
    ContentID := FOwner.GetNextID;
  FSaveCount := 2;
  GStateRestore;
  if FRemoveCR then
  begin
    S := FContent.Text;
    O := '';
    I := Pos ( #13#10, S );
    while I <> 0 do
    begin
      O := O + ' ' + Copy ( S, 1, I - 1 );
      Delete ( S, 1, I + 1 );
      I := Pos ( #13#10, S );
    end;
    O := O + ' ' + S;
    FContent.Text := O;
  end;
  for I := 0 to FAnnot.Count - 1 do
  begin
    TPDFCustomAnnotation ( FAnnot [ I ] ).Save;
  end;
  FOwner.StartObj ( ResourceID );
  FOwner.SaveToStream ( ' /ProcSet [/PDF ', False );
  if FTextUsed then
    FOwner.SaveToStream ( '/Text ', False );
  if FGrayUsed then
    FOwner.SaveToStream ( '/ImageB ', False );
  if FColorUsed then
    FOwner.SaveToStream ( '/ImageC ', False );
  FOwner.SaveToStream ( ']' );
  if Length ( FLinkedFont ) > 0 then
  begin
    FOwner.SaveToStream ( '/Font <<' );
    for I := 0 to Length ( FLinkedFont ) - 1 do
      if FLinkedFont [ I ].FFontUsed then
        FOwner.SaveToStream ( '/' + FLinkedFont [ I ].AliasName + ' ' + IntToStr ( FLinkedFont [ I ].FontID ) + ' 0 R' );
    FOwner.SaveToStream ( '>>' );
  end;
  if ( FLinkedImages.Count > 0 ) or ( FWaterMark >= 0 ) or ( FForms.Count > 0 ) then
  begin
    FOwner.SaveToStream ( ' /XObject <<' );
    for I := 0 to FLinkedImages.Count - 1 do
      FOwner.SaveToStream ( '/' + TPDFImage ( FLinkedImages [ I ] ).ImageName + ' ' + IntToStr ( TPDFImage ( FLinkedImages [ I ] ).PictureID ) + ' 0 R' );
    if FWaterMark >= 0 then
      FOwner.SaveToStream ( '/Form' + IntToStr ( FWaterMark ) + ' ' + IntToStr ( FOwner.FWaterMarks [ FWaterMark ].PageID ) + ' 0 R' );
    for I := 0 to FForms.Count - 1 do
      FOwner.SaveToStream ( '/IF' + IntToStr ( FForms.IndexOf ( FForms [ I ] ) ) + ' ' + IntToStr ( FForms [ I ].PageID ) + ' 0 R' );
    FOwner.SaveToStream ( '>>' );
  end;
  FOwner.CloseHObj;
  FOwner.CloseObj;
  if not FIsForm then
  begin
    if FWaterMark >= 0 then
      FContent.Insert ( 0, 'q /Form' + IntToStr ( FWaterMark ) + ' Do   Q' );
    FOwner.StartObj ( ContentID );
    if FOwner.Compression = ctFlate then
    begin
      MS := TMemoryStream.Create;
      try
        CS := TCompressionStream.Create ( clDefault, MS );
        try
          FContent.SaveToStream ( CS );
        finally
          CS.Free;
        end;
        FOwner.SaveToStream ( '/Length ' + IntToStr ( MS.size ) );
        FOwner.SaveToStream ( '/Filter /FlateDecode' );
        FOwner.CloseHObj;
        FOwner.StartStream;
        MS.Position := 0;
        EnCodeStream ( FOwner.FProtectionEnabled, FOwner.FKey, FOwner.FPKL, ContentID, MS );
        FOwner.FStream.CopyFrom ( MS, MS.Size );
        FOwner.SaveToStream ( '' );
      finally
        MS.Free;
      end;
    end
    else
    begin
      FOwner.SaveToStream ( '/Length ' + IntToStr ( Length ( FContent.Text ) ) );
      FOwner.CloseHObj;
      FOwner.StartStream;
      FOwner.SaveToStream ( EnCodeString ( FOwner.FProtectionEnabled, FOwner.FKey, FOwner.FPKL, ContentID, FContent.Text ) );
    end;
    FContent.Text := '';
    FOwner.CloseStream;
    FOwner.CloseObj;
    FOwner.StartObj ( PageID );
    FOwner.SaveToStream ( '/Type /Page' );
    FOwner.SaveToStream ( '/Parent ' + IntToStr ( FOwner.PagesID ) + ' 0 R' );
    FOwner.SaveToStream ( '/Resources ' + IntToStr ( ResourceID ) + ' 0 R' );
    FOwner.SaveToStream ( '/Contents [' + IntToStr ( ContentID ) + ' 0 R]' );
    if FThumbnail >= 0 then
      FOwner.SaveToStream ( '/Thumb ' + IntToStr ( FOwner.FImages [ FThumbnail ].PictureID ) + ' 0 R' );
    FOwner.SaveToStream ( '/MediaBox [0 0 ' + IntToStr ( FWidth ) + ' ' + IntToStr ( FHeight ) + ']' );
    case FRotate of
      pr90: FOwner.SaveToStream ( '/Rotate 90' );
      pr180: FOwner.SaveToStream ( '/Rotate 180' );
      pr270: FOwner.SaveToStream ( '/Rotate 270' );
    end;
    if FAnnot.Count <> 0 then
    begin
      FOwner.SaveToStream ( '/Annots [' );
      for I := 0 to FAnnot.Count - 1 do
        FOwner.SaveToStream ( IntToStr ( TPDFCustomAnnotation ( FAnnot [ I ] ).AnnotID ) + ' 0 R' );
      FOwner.SaveToStream ( ']' );
    end;
    FOwner.CloseHObj;
    FOwner.CloseObj;
  end
  else
  begin
    FOwner.StartObj ( PageID );
    FOwner.SaveToStream ( '/Type /XObject' );
    FOwner.SaveToStream ( '/Subtype /Form' );
    FOwner.SaveToStream ( '/Resources ' + IntToStr ( ResourceID ) + ' 0 R' );
    FOwner.SaveToStream ( '/Matrix [' + FormatFloat ( FMatrix.a ) + ' ' + FormatFloat ( FMatrix.b ) + ' ' + FormatFloat ( FMatrix.c ) + ' ' +
      FormatFloat ( FMatrix.d ) + ' ' + FormatFloat ( FMatrix.x ) + ' ' + FormatFloat ( FMatrix.y ) + ' ]' );
 //   FOwner.SaveToStream ( '/BBox [0 0 ' + IntToStr ( Round ( FWidth * FMatrix.a ) ) +
//  ' ' + IntToStr ( Round ( FHeight * FMatrix.d ) ) + ']' );
    FOwner.SaveToStream ( '/BBox [0 0 ' + IntToStr ( FWidth ) +
      ' ' + IntToStr ( FHeight ) + ']' );

    if FOwner.Compression = ctFlate then
    begin
      MS := TMemoryStream.Create;
      try
        CS := TCompressionStream.Create ( clDefault, MS );
        try
          FContent.SaveToStream ( CS );
        finally
          CS.Free;
        end;
        FOwner.SaveToStream ( '/Length ' + IntToStr ( MS.size ) );
        FOwner.SaveToStream ( '/Filter /FlateDecode' );
        FOwner.CloseHObj;
        FOwner.StartStream;
        MS.Position := 0;
        EnCodeStream ( FOwner.FProtectionEnabled, FOwner.FKey, FOwner.FPKL, PageID, MS );
        FOwner.FStream.CopyFrom ( MS, MS.Size );
        FOwner.SaveToStream ( '' );
      finally
        MS.Free;
      end;
    end
    else
    begin
      FOwner.SaveToStream ( '/Length ' + IntToStr ( Length ( FContent.Text ) ) );
      FOwner.CloseHObj;
      FOwner.StartStream;
      FOwner.SaveToStream ( EnCodeString ( FOwner.FProtectionEnabled, FOwner.FKey, FOwner.FPKL, PageID, FContent.Text ) );
    end;
    FContent.Text := '';
    FOwner.CloseStream;
    FOwner.CloseObj;
  end;
end;

procedure TPDFPage.Scale ( SX, SY: Extended );
begin
  RawConcat ( sx, 0, 0, sy, 0, 0 );
end;

procedure TPDFPage.SetActiveFont ( FontName: string; FontStyle: TFontStyles; FontSize: Extended; FontCharset: TFontCharset = ANSI_CHARSET );
begin
  if ( FCurrentFontName <> FontName ) or ( FCurrentFontStyle <> FontStyle ) or ( FontSize <> FCurrentFontSize ) then
  begin
    FCurrentFontName := FontName;
    FCurrentFontSize := FontSize;
    FCurrentFontStyle := FontStyle;
    FFontIsChanged := True;
    FUnicodeUse := False;
  end;
  if FontCharset = DEFAULT_CHARSET then
    FCharset := GetDefFontCharSet
  else
    FCharset := FontCharset;
end;

procedure TPDFPage.SetIntCharacterSpacing ( Spacing: Extended );
begin
  if FCharSpace = Spacing then
    exit;
  BeginText;
  AppendAction ( FormatFloat ( Spacing ) + ' Tc' );
  FCharSpace := Spacing;
end;

procedure TPDFPage.SetCharacterSpacing ( Spacing: Extended );
begin
  Spacing := Spacing / D2P;
  if FCharSpace = Spacing then
    exit;
  BeginText;
  AppendAction ( FormatFloat ( Spacing ) + ' Tc' );
  FCharSpace := Spacing;
end;

procedure TPDFPage.SetCMYKColor ( C, M, Y, K: Extended );
begin
  SetCMYKColorFill ( C, M, Y, K );
  SetCMYKColorStroke ( C, M, Y, K );
end;

procedure TPDFPage.SetCMYKColorFill ( C, M, Y, K: Extended );
begin
  AppendAction ( FormatFloat ( C ) + ' ' + FormatFloat ( M ) + ' ' + FormatFloat ( Y ) + ' ' + FormatFloat ( K ) + ' k' );
end;

procedure TPDFPage.SetCMYKColorStroke ( C, M, Y, K: Extended );
begin
  AppendAction ( FormatFloat ( C ) + ' ' + FormatFloat ( M ) + ' ' + FormatFloat ( Y ) + ' ' + FormatFloat ( K ) + ' K' );
end;

procedure TPDFPage.SetDash ( DashSpec: string );
begin
  EndText;
  if FCurrentDash <> DashSpec then
  begin
    AppendAction ( DashSpec + ' d' );
    FCurrentDash := DashSpec;
  end;
end;

procedure TPDFPage.SetFlat ( FlatNess: integer );
begin
  EndText;
  AppendAction ( Format ( '%d i', [ FlatNess ] ) );
end;

procedure TPDFPage.SetGray ( Gray: Extended );
begin
  SetGrayFill ( Gray );
  SetGrayStroke ( Gray );
end;

procedure TPDFPage.SetGrayFill ( Gray: Extended );
begin
  AppendAction ( FormatFloat ( Gray ) + ' g' );
end;

procedure TPDFPage.SetGrayStroke ( Gray: Extended );
begin
  AppendAction ( FormatFloat ( Gray ) + ' G' );
end;

procedure TPDFPage.SetHeight ( const Value: Integer );
var
  I: Integer;
begin
  if Factions then
    raise TPDFException.Create ( SPageInProgress );
  if FHeight <> Value then
  begin
    FHeight := round ( Value / D2P );
    FPageSize := psUserDefined;
    if FMF <> nil then
      if not FAskCanvas then
      begin
        I := GetDeviceCaps ( FOwner.FDC, LOGPIXELSY );
        FMF.Height := MulDiv ( FHeight, I, 72 );
        FCanvas.Free;
        FCanvas := TMetafileCanvas.Create ( FMF, FOwner.FDC );
      end;
  end;
end;

procedure TPDFPage.SetHorizontalScaling ( Scale: Extended );
begin
  if FHorizontalScaling = Scale then
    exit;
  BeginText;
  AppendAction ( FormatFloat ( Scale ) + ' Tz' );
  FHorizontalScaling := Scale;
end;

procedure TPDFPage.SetLineCap ( LineCap: TPDFLineCap );
begin
  EndText;
  AppendAction ( Format ( '%d J', [ Ord ( LineCap ) ] ) );
end;

procedure TPDFPage.SetLineJoin ( LineJoin: TPDFLineJoin );
begin
  EndText;
  AppendAction ( Format ( '%d j', [ Ord ( LineJoin ) ] ) );
end;

procedure TPDFPage.SetLineWidth ( lw: Extended );
begin
  EndText;
  AppendAction ( FormatFloat ( lw / D2P ) + ' w' );
end;

procedure TPDFPage.SetMiterLimit ( MiterLimit: Extended );
begin
  EndText;
  MiterLimit := MiterLimit / D2P;
  AppendAction ( FormatFloat ( MiterLimit ) + ' M' );
end;

procedure TPDFPage.SetPageSize ( Value: TPDFPageSize );
var
  I, J: Integer;
begin
  if Factions then
    raise TPDFException.Create ( SPageInProgress );
  FPageSize := Value;
  case Value of
    psLetter:
      begin
        FHeight := 792;
        FWidth := 612;
      end;
    psA4:
      begin
        FHeight := 842;
        FWidth := 595;
      end;
    psA3:
      begin
        FHeight := 1190;
        FWidth := 842;
      end;
    psLegal:
      begin
        FHeight := 1008;
        FWidth := 612;
      end;
    psB5:
      begin
        FHeight := 728;
        FWidth := 516;
      end;
    psC5:
      begin
        FHeight := 649;
        FWidth := 459;
      end;
    ps8x11:
      begin
        FHeight := 792;
        FWidth := 595;
      end;
    psB4:
      begin
        FHeight := 1031;
        FWidth := 728;
      end;
    psA5:
      begin
        FHeight := 595;
        FWidth := 419;
      end;
    psFolio:
      begin
        FHeight := 936;
        FWidth := 612;
      end;
    psExecutive:
      begin
        FHeight := 756;
        FWidth := 522;
      end;
    psEnvB4:
      begin
        FHeight := 1031;
        FWidth := 728;
      end;
    psEnvB5:
      begin
        FHeight := 708;
        FWidth := 499;
      end;
    psEnvC6:
      begin
        FHeight := 459;
        FWidth := 323;
      end;
    psEnvDL:
      begin
        FHeight := 623;
        FWidth := 312;
      end;
    psEnvMonarch:
      begin
        FHeight := 540;
        FWidth := 279;
      end;
    psEnv9:
      begin
        FHeight := 639;
        FWidth := 279;
      end;
    psEnv10:
      begin
        FHeight := 684;
        FWidth := 297;
      end;
    psEnv11:
      begin
        FHeight := 747;
        FWidth := 324;
      end;
  end;
  SetOrientation ( FOrientation );
  if FMF <> nil then
    if not FAskCanvas then
    begin
      I := GetDeviceCaps ( FOwner.FDC, LOGPIXELSX );
      J := GetDeviceCaps ( FOwner.FDC, LOGPIXELSY );
      FMF.Height := MulDiv ( FHeight, J, 72 );
      FMF.Width := MulDiv ( FWidth, I, 72 );
      FCanvas.Free;
      FCanvas := TMetafileCanvas.Create ( FMF, FOwner.FDC );
    end;
end;


procedure TPDFPage.SetRGBColor ( R, G, B: Extended );
begin
  SetRGBcolorFill ( R, G, B );
  SetRGBcolorStroke ( R, G, B );
end;

procedure TPDFPage.SetRGBColorFill ( R, G, B: Extended );
begin
  AppendAction ( FormatFloat ( R ) + ' ' + FormatFloat ( G ) + ' ' + FormatFloat ( B ) + ' rg' );
end;

procedure TPDFPage.SetRGBColorStroke ( R, G, B: Extended );
begin
  AppendAction ( FormatFloat ( R ) + ' ' + FormatFloat ( G ) + ' ' + FormatFloat ( B ) + ' RG' );
end;

procedure TPDFPage.SetRotate ( const Value: TPDFPageRotate );
begin
  FRotate := Value;
end;


procedure TPDFPage.SetTextRenderingMode ( Mode: integer );
begin
  BeginText;
  AppendAction ( Format ( '%d Tr', [ mode ] ) );
  FRender := Mode;
end;

procedure TPDFPage.SetWidth ( const Value: Integer );
var
  I: Integer;
begin
  if Factions then
    raise TPDFException.Create ( SPageInProgress );
  if FWidth <> Value then
  begin
    FWidth := round ( Value / D2P );
    FPageSize := psUserDefined;
    if FMF <> nil then
      if not FAskCanvas then
      begin
        I := GetDeviceCaps ( FOwner.Fdc, LOGPIXELSX );
        FMF.Width := MulDiv ( FWidth, I, 72 );
        FCanvas.Free;
        FCanvas := TMetafileCanvas.Create ( FMF, FOwner.FDC );
      end;
  end;
end;


procedure TPDFPage.SetWordSpacing ( Spacing: Extended );
begin
  Spacing := Spacing / D2P;
  if FWordSpace = Spacing then
    exit;
  BeginText;
  AppendAction ( FormatFloat ( Spacing ) + ' Tw' );
  FWordSpace := Spacing;
end;

procedure TPDFPage.TextFromBaseLine ( BaseLine: Boolean );
begin
  FBaseLine := BaseLine;
end;

procedure TPDFPage.Stroke;
begin
  EndText;
  FPathInited := False;
  AppendAction ( 'S' );
end;

function TPDFPage.TextOutBox ( LTCornX, LTCornY, Interval, BoxWidth, BoxHeight: integer; TextStr: Ansistring ): integer;
var
  i, Count, Len: integer;
  StrLine, OutTxtLine: string;
  Ch: Ansichar;
  CWidth, StrWidth, OutWidth: Extended;
  CNT, CodePage: Integer;
  Mem: PWord;
begin
  if TextStr = '' then
  begin
    Result := 0;
    Exit;
  end;
  Len := Length ( TextStr );
  Result := 0; //
  if FCharset <> 0 then
  begin
    CodePage := CharSetToCodePage ( FCharset );
    CNT := MultiByteToWideChar ( CodePage, 0, PAnsiChar ( TextStr ), Len, nil, 0 );
    if CNT = 0 then
      raise TPDFException.Create ( 'Cannot convert text to unicode' );
    GetMem ( Mem, CNT * 2 );
    try
      CNT := MultiByteToWideChar ( CodePage, 0, PAnsiChar ( TextStr ), Len, PWideChar ( Mem ), CNT );
      if CNT = 0 then
        raise TPDFException.Create ( 'Cannot convert text to unicode' );
      Result := UnicodeTextOutBox ( LTCornX, LTCornY, Interval, BoxWidth, BoxHeight, Mem, CNT );
    finally
      FreeMem ( Mem );
    end;
    Exit;
  end;
  BeginText;
  i := 1;
  Count := 0;
  StrWidth := 0;
  OutWidth := 0;
  OutTxtLine := '';
  StrLine := '';
  SetCurrentFont ( False );
  while i <= Len do
  begin
    ch := TextStr [ i ];
    if ( ch = #13 ) and ( i < Len ) then
    begin
        if ( TextStr [ i + 1 ] = #10 ) then
          Inc ( i );
{$IFNDEF CB}
        TextOut ( LTCornX, LTCornY + Count * Interval, 0, {Trim} ( OutTxtLine + StrLine ) );
{$ELSE}
        TextOutput ( LTCornX, LTCornY + Count * Interval, 0, {Trim} ( OutTxtLine + StrLine ) );
{$ENDIF}
        Inc ( i );
        OutTxtLine := '';
        OutWidth := 0;
        Inc ( Count );
        StrLine := '';
        Continue;
    end;
    CWidth := FCurrentFont.GetWidth ( Ord ( ch ) ) * FCurrentFontSize / 1000;
    if FHorizontalScaling <> 100 then
      CWidth := CWidth * FHorizontalScaling / 100;
    if CWidth > 0 then
      CWidth := CWidth + FCharSpace
    else
      CWidth := 0;
    if ( ch = ' ' ) and ( FWordSpace > 0 ) and ( i <> Length ( TextStr ) ) then
      CWidth := CWidth + FWordSpace;
    if ( ( OutWidth + StrWidth + CWidth ) < BoxWidth ) and ( i < Len ) and ( not ( Ch in [ #10, #13 ] ) ) then
    begin
      StrWidth := StrWidth + CWidth;
      StrLine := StrLine + String(ch);
      if ch = ' ' then
      begin
        OutTxtLine := OutTxtLine + StrLine;
        OutWidth := OutWidth + StrWidth;
        StrWidth := 0;
        StrLine := '';
      end;
    end
    else
    begin

      if i = Len then
      begin
        StrWidth := StrWidth + CWidth;
        StrLine := StrLine + String(ch);
        OutWidth := 0;
      end;
      if ( OutWidth = 0 ) {or (StrLine <> '')} then
      begin
{$IFNDEF CB}
        TextOut ( LTCornX, LTCornY + Count * Interval, 0, {Trim} ( OutTxtLine + StrLine ) );
{$ELSE}
        TextOutput ( LTCornX, LTCornY + Count * Interval, 0, {Trim} ( OutTxtLine + StrLine ) );
{$ENDIF}
        StrLine := String(ch);
        StrWidth := CWidth;
      end
      else
      begin
{$IFNDEF CB}
        TextOut ( LTCornX, LTCornY + Count * Interval, 0, OutTxtLine );
{$ELSE}
        TextOutput ( LTCornX, LTCornY + Count * Interval, 0, OutTxtLine );
{$ENDIF}
        StrLine := StrLine + String(ch);
        StrWidth := StrWidth + CWidth;
      end;
      OutTxtLine := '';
      OutWidth := 0;
      Inc ( Count );
      if ( Count * Interval ) + FCurrentFontSize > BoxHeight then
      begin
        Result := i;
        exit;
      end;
    end;
    Inc ( i );
  end;
  Result:= i;
end;

function TPDFPage.GetCurrentFontSize: Extended;
begin
  Result := FCurrentFontSize;
end;

{$IFDEF CB}

procedure TPDFPage.TextOutput(X, Y, Orientation: Extended; TextStr: string);
{$ELSE}


procedure TPDFPage.TextOut ( X, Y, Orientation: Extended; TextStr: string );
{$ENDIF}
var
  O: Extended;
  ws, URL: string;
  I: Integer;
  K: Integer;
  Off: Integer;
  offUrl, LenUrl: Extended;
  fnd: Boolean;
begin
  if TextStr = '' then
    Exit;
  O := GetRawTextHeight;
  if Orientation = 0 then
  begin
    RawTextOut ( ExtToIntX ( X ), ExtToIntY ( Y ) - O, Orientation, TextStr );
    if FOwner.FACURL then
      if not FIsForm then
      begin
        ws := LowerCase ( TextStr );
        fnd := False;
        for I := 0 to 2 do
          if PosText ( URLDetectStrings [ i ], ws, 1 ) <> 0 then
            fnd := True;
        if fnd then
        begin
          for I := 0 to 2 do
          begin
            K := PosText ( URLDetectStrings [ i ], ws, 1 );
            while K <> 0 do
            begin
              if K <> 1 then
                if ws [ K - 1 ] <> #32 then
                begin
                  OFF := K + Length ( URLDetectStrings [ i ] );
                  K := PosText ( URLDetectStrings [ i ], ws, OFF );
                  Continue;
                end;
              OFF := PosText ( ' ', ws, K + 1 );
              if OFF = 0 then
              begin
                off := Length ( ws );
                URL := Copy ( TextStr, K, OFF - K + 1 );
              end
              else
                URL := Copy ( TextStr, K, OFF - K );
              offUrl := GetTextWidth ( Copy ( TextStr, 1, K - 1 ) );
              LenUrl := GetTextWidth ( URL );
              if FBaseLine then
                SetUrl ( Rect ( Trunc ( X + offUrl ), Trunc ( Y ), Trunc ( X + offURL + LenUrl ), Trunc ( Y - FCurrentFontSize * d2p ) ), URL )
              else
                SetUrl ( Rect ( Trunc ( X + offUrl ), trunc ( Y ), Trunc ( X + offURL + LenUrl ), Trunc ( Y + O * d2p ) ), URL );
              K := PosText ( URLDetectStrings [ i ], ws, OFF );
            end;
          end;
        end;
      end;

  end
  else
    RawTextOut ( ExtToIntX ( X ) + o * sin ( Orientation * Pi / 180 ),
      ExtToIntY ( Y ) - O * cos ( Orientation * Pi / 180 ), Orientation, TextStr );
end;


procedure TPDFPage.TextShow ( TextStr: AnsiString );
var
  s: string;
  TL: Integer;
  CodePage: Integer;
  Len: Integer;
  Mem: PWord;
  i: Integer;
  TextWidth: Extended;
begin
  FTextUsed := True;
  for i := 1 to Length ( TextStr ) do
    if TextStr [ i ] < #32 then
      TextStr [ i ] := #32;
  if FCharset = 0 then
  begin
    SetCurrentFont ( False );  
    if not FCurrentFont.Standart then
      for I := 1 to Length ( TextStr ) do
        if TextStr[I] = #160 then
          TextStr[I] := #32;
    FCurrentFont.FillUsed ( TextStr );
    s := EscapeSpecialChar ( TextStr );
    AppendAction ( '(' + s + ') Tj' );
    if ( ( not ( fsUnderLine in FCurrentFontStyle ) ) and ( not ( fsStrikeOut in FCurrentFontStyle ) ) ) then
      Exit;
    TextWidth := RawGetTextWidth ( TextStr );
    PaintTextLines ( TextWidth );
  end
  else
  begin
    TL := Length ( TextStr );
    CodePage := CharSetToCodePage ( FCharset );
    Len := MultiByteToWideChar ( CodePage, 0, PAnsiChar ( TextStr ), TL, nil, 0 );
    if Len = 0 then
      raise TPDFException.Create ( 'Cannot convert text to unicode' );
    GetMem ( Mem, Len * 2 );
    try
      Len := MultiByteToWideChar ( CodePage, 0, PAnsiChar ( TextStr ), TL, PWideChar ( Mem ), Len );
      if Len = 0 then
        raise TPDFException.Create ( 'Cannot convert text to unicode' );
      UnicodeTextShow ( Mem, Len );
    finally
      FreeMem ( Mem );
    end;
  end;
end;

procedure TPDFPage.Translate ( XT, YT: Extended );
begin
  RawTranslate ( ExtToIntX ( XT ), ExtToIntY ( YT ) );
end;

procedure TPDFPage.RawShowImage ( ImageIndex: Integer; x, y, w, h, angle: Extended );
begin
  if ( ImageIndex < 0 ) or ( ImageIndex > FOwner.Fimages.Count - 1 ) then
    raise TPDFException.Create ( SOutOfRange );
  if ( FOwner.Fimages [ ImageIndex ].FBitPerPixel = 1 ) or ( FOwner.Fimages [ ImageIndex ].GrayScale ) then
    FGrayUsed := True
  else
    FColorUsed := True;
  GStateSave;
  RawTranslate ( x, y );
  if Abs ( angle ) > 0.001 then
    Rotate ( angle );
  RawConcat ( w, 0, 0, h, 0, 0 );
  AppendAction ( '/' + FOwner.Fimages [ ImageIndex ].ImageName + ' Do' );
  GStateRestore;
  if FLinkedImages.IndexOf ( Pointer ( FOwner.Fimages [ ImageIndex ] ) ) = -1 then
    FLinkedImages.Add ( Pointer ( FOwner.Fimages [ ImageIndex ] ) );
end;

procedure TPDFPage.ShowImage ( ImageIndex: Integer; x, y, w, h, angle: Extended );
begin
  if ( w = 0 ) or ( h = 0 ) then Exit;
  RawShowImage ( ImageIndex, ExtToIntX ( X ), ExtToIntY ( y ) - h / d2p, w / d2p, h / d2p, angle );
end;

procedure TPDFPage.DeleteAllAnnotations;
var
  i: Integer;
begin
  for i := 0 to FAnnot.Count - 1 do
    TPDFCustomAnnotation ( FAnnot [ i ] ).Free;
  FAnnot.Clear;
end;

function TPDFPage.GetTextWidth ( Text: string ): Extended;
begin
  Result := RawGetTextWidth ( Text ) * D2P;
end;

function TPDFPage.SetAnnotation ( ARect: TRect; Title, Text: string;
  Color: TColor; Flags: TAnnotationFlags; Opened: Boolean; Charset: TFontCharset = ANSI_CHARSET ): TPDFCustomAnnotation;
var
  A: TPDFTextAnnotation;
begin
  if FIsForm then
    raise TPDFException.Create ( SCannotCreateAnnotationToWatermark );
  A := TPDFTextAnnotation.Create ( Self );
  A.Caption := Title;
  A.Text := Text;
  A.BorderColor := Color;
  A.Box := Rect ( ARect.Left, ARect.Top, ARect.Right, ARect.Bottom );
  A.Flags := Flags;
  A.Charset := Charset;
  A.Opened := Opened;
  Result := A;
end;


function TPDFPage.SetLinkToPage ( ARect: TRect; PageIndex,
  TopOffset: Integer ): TPDFCustomAnnotation;
var
  A: TPDFActionAnnotation;
begin
  if FIsForm then
    raise TPDFException.Create ( SCannotCreateLinkToPageToWatermark );
  A := TPDFActionAnnotation.Create ( Self );
  A.Box := Rect ( ARect.Left, ARect.Top, ARect.Right, ARect.Bottom );
  A.Action := TPDFGoToPageAction.Create;
  TPDFGoToPageAction ( A.Action ).PageIndex := PageIndex;
  TPDFGoToPageAction ( A.Action ).TopOffset := TopOffset;
  A.BorderStyle := '[]';
  Result := A;
end;

function TPDFPage.SetUrl ( ARect: TRect; URL: string ): TPDFCustomAnnotation;
var
  A: TPDFActionAnnotation;
begin
  if FIsForm then
    raise TPDFException.Create ( SCannotCreateURLToWatermark );
  A := TPDFActionAnnotation.Create ( Self );
  A.Box := Rect ( ARect.Left, ARect.Top, ARect.Right, ARect.Bottom );
  A.Action := TPDFURLAction.Create;
  TPDFURLAction ( A.Action ).URL := URL;
  A.BorderStyle := '[]';
  Result := A;
end;

procedure TPDFPage.RoundRect ( X1, Y1, X2, Y2, X3, Y3: Integer );
const
  b = 0.5522847498;
var
  RX, RY: Extended;
begin
  NormalizeRect ( x1, y1, x2, y2 );
  Rx := x3 / 2;
  Ry := y3 / 2;
  MoveTo ( X1 + RX, Y1 );
  LineTo ( X2 - RX, Y1 );
  Curveto ( X2 - RX + b * RX, Y1, X2, Y1 + RY - b * RY, X2, Y1 + ry );
  LineTo ( X2, Y2 - RY );
  Curveto ( X2, Y2 - RY + b * RY, X2 - RX + b * RX, Y2, X2 - RX, Y2 );
  LineTo ( X1 + RX, Y2 );
  Curveto ( X1 + RX - b * RX, Y2, X1, Y2 - RY + b * RY, X1, Y2 - RY );
  LineTo ( X1, Y1 + RY );
  Curveto ( X1, Y1 + RY - b * RY, X1 + RX - b * RX, Y1, X1 + RX, Y1 );
  ClosePath;
end;

procedure TPDFPage.TextBox ( Rect: TRect; Text: string; Hor: THorJust; Vert: TVertJust );
var
  x, y: Extended;
begin
  NormalizeRect ( Rect );
  Y := Rect.Top;
  x := Rect.Left;
  case Hor of
    hjLeft: x := Rect.Left;
    hjRight: x := Rect.Right - GetTextWidth ( Text );
    hjCenter: x := Rect.Left + ( Rect.Right - Rect.Left - GetTextWidth ( Text ) ) / 2;
  end;
  case Vert of
    vjUp: y := Rect.Top;
    vjDown: y := Rect.Bottom - FCurrentFontSize;
    vjCenter: y := Rect.Top + ( Rect.Bottom - Rect.Top - FCurrentFontSize ) / 2;
  end;
{$IFDEF CB}
  TextOutput ( x, y, 0, Text );
{$ELSE}
  TextOut ( x, y, 0, Text );
{$ENDIF}
end;

procedure TPDFPage.SetOrientation ( const Value: TPDFPageOrientation );
begin
  if Factions then
    raise TPDFException.Create ( SPageInProgress );
  FOrientation := Value;
  if Value = poPagePortrait then
    if FWidth > FHeight then
      swp ( FWidth, FHeight );
  if Value = poPageLandScape then
    if FWidth < FHeight then
      swp ( FWidth, FHeight );
end;

function TPDFPage.GetHeight: Integer;
begin
  Result := Round ( FHeight * D2P );
end;

function TPDFPage.GetWidth: Integer;
begin
  Result := Round ( FWidth * D2P );
end;

procedure TPDFPage.CloseCanvas;
var
  s: string;
  Pars: TEMWParser;
  SZ: TSize;
  Z: Boolean;
begin
  if FMF = nil then
    Exit;
  FCanvas.Free;
  Z := False;
  Pars := TEMWParser.Create ( Self );
  try
    FMF.Enhanced := True;
    Pars.LoadMetaFile ( FMF );
    SZ := Pars.GetMax;
    if ( SZ.cx <= 0 ) or ( SZ.cy <= 0 ) then
      Z := True;
    if not Z then
    begin
      EndText;
      s := FContent.Text;
      FContent.Clear;
      FFontIsChanged := True;
      Pars.Execute;
      EndText;
      if FCanvasOver then
        FContent.Text := FContent.Text + #13 + s
      else
        FContent.Text := s + #13 + FContent.Text;
    end;
  finally
    Pars.Free;
  end;
  FMF.Free;
  FMF := nil;
  FAskCanvas := False;
end;

procedure TPDFPage.PlayMetaFile ( MF: TMetaFile );
begin
  PlayMetaFile ( MF, 0, 0, 1, 1 );
end;

procedure TPDFPage.PlayMetaFile ( MF: TMetafile; x, y, XScale, YScale: Extended );
var
  P: TPDFPage;
  Pars: TEMWParser;
  S: TSize;
  Z: Boolean;
  NMF: TMetafile;
  MFC: TMetafileCanvas;
  AX: Extended;
  XS, YS: Integer;
  W, H: Integer;
begin
  Z := False;
  AX := 1;
  P := FForms.Add;
  if P.FCanvas <> nil then
    P.DeleteCanvas;
  P.FWidth := FWidth;
  P.FHeight := FHeight;
  P.FRes := FRes;
  P.D2P := D2P;
  if FOwner.FReDraw then
  begin
    NMF := TMetafile.Create;
    try
      if FOwner.FUseScreen then
      begin
        XS := GetDeviceCaps ( FOwner.FDC, HORZRES );
        YS := GetDeviceCaps ( FOwner.FDC, VERTRES );
        if ( MF.Height > YS ) or ( MF.Width > XS ) then
        begin
          AX := Min ( YS / MF.Height, XS / MF.Width );
          NMF.Height := Round ( MF.Height * AX );
          NMF.Width := Round ( MF.Width * AX );
        end
        else
        begin
          NMF.Height := MF.Height;
          NMF.Width := MF.Width;
        end;
      end
      else
      begin
        NMF.Height := MF.Height;
        NMF.Width := MF.Width;
      end;
      W := NMF.Width;
      H := NMF.Height;
      MFC := TMetafileCanvas.Create ( NMF, FOwner.FDC );
      try
        if AX = 1 then
          MFC.Draw ( 0, 0, MF )
        else
          MFC.StretchDraw ( Rect ( 0, 0, W - 1, H - 1 ), MF );
      finally
        MFC.Free;
      end;
      Pars := TEMWParser.Create ( P );
      try
        MF.Enhanced := True;
        Pars.LoadMetaFile ( NMF );
        S := Pars.GetMax;
        if ( S.cx = 0 ) or ( S.cy = 0 ) then
          Z := True;
        if not Z then
        begin
          P.Width := abs ( S.cx );
          P.Height := abs ( S.cy );
          Pars.Execute;
        end
        else                                                      
          FForms.Delete ( P );
      finally
        Pars.Free;
      end;
    finally
      NMF.Free;
    end;
  end
  else
  begin
    Pars := TEMWParser.Create ( P );
    try
      MF.Enhanced := True;
      Pars.LoadMetaFile ( MF );
      S := Pars.GetMax;
      if ( S.cx = 0 ) or ( S.cy = 0 ) then
        Z := True;
      if not Z then
      begin
        P.Width := abs ( S.cx );
        P.Height := abs ( S.cy );
        Pars.Execute;
      end
      else
        FForms.Delete ( P );
    finally
      Pars.Free;
    end;
  end;
  if not Z then
  begin
    EndText;
    AppendAction ( 'q /IF' + IntToStr ( FForms.IndexOf ( P ) ) + ' Do Q' );
    P.FMatrix.x := x / D2P;
    P.FMatrix.y := ( Height - P.Height * YScale / AX - y ) / D2P;
    P.FMatrix.a := XScale / AX;
    P.FMatrix.d := YScale / AX;
  end;
end;

procedure TPDFPage.CreateCanvas;
var
  I, J: Integer;
begin
  if FMF = nil then
  begin
    FMF := TMetafile.Create;
    FAskCanvas := False;
    I := GetDeviceCaps ( FOwner.Fdc, LOGPIXELSX );
    J := GetDeviceCaps ( FOwner.Fdc, LOGPIXELSY );
    FMF.Height := MulDiv ( FHeight, J, 72 );
    FMF.Width := MulDiv ( FWidth, I, 72 );
    FCanvas := TMetafileCanvas.Create ( FMF, FOwner.FDC );
  end
  else
    raise TPDFException.Create ( SCanvasForThisPageAlreadyCreated );
end;

procedure TPDFPage.SetWaterMark ( const Value: Integer );
begin
  if Value < 0 then
  begin
    FWaterMark := Value;
    Exit;
  end;
  if FIsForm then
    raise TPDFException.Create ( SWatermarkCannotHaveWatermark );
  if Value >= FOwner.FWaterMarks.Count then
    raise TPDFException.Create ( SWaterMarkWithSuchIndexNotCreatedYetForThisDocument );
  FWaterMark := Value;
end;

procedure TPDFPage.DeleteCanvas;
begin
  if FMF <> nil then
  begin
    FCanvas.Free;
    FMF.Free;
    FMF := nil;
    FCanvas := nil;
    FAskCanvas := False;
  end;
end;

function TPDFPage.SetAction ( ARect: TRect; Action: TPDFAction ): TPDFCustomAnnotation;
var
  A: TPDFActionAnnotation;
begin
  if FIsForm then
    raise TPDFException.Create ( SCannotCreateURLToWatermark );
  A := TPDFActionAnnotation.Create ( Self );
//  FAnnot.Add(Pointer(A));
  A.Box := Rect ( ARect.Left, ARect.Top, ARect.Right, ARect.Bottom );
  A.Action := Action;
  A.BorderStyle := '[]';
  Result := A;
end;

procedure TPDFPage.SetThumbnail ( const Value: Integer );
begin
  if Value < 0 then
  begin
    FThumbnail := Value;
    Exit;
  end;
  if FIsForm then
    raise TPDFException.Create ( SWatermarkCannotHaveThumbnail );
  if Value >= FOwner.FImages.Count then
    raise TPDFException.Create ( SImageWithSuchIndexNotCreatedYetForThisDocument );
  FThumbnail := Value;
end;

procedure TPDFPage.PrepareID;
var
  I: Integer;
begin
  for I := 0 to Length ( FLinkedFont ) - 1 do
    if ( FLinkedFont [ I ].FontID = -1 ) and ( FLinkedFont [ I ].FFontUsed ) then
      FLinkedFont [ I ].FontID := FOwner.GetNextID;
  for I := 0 to FLinkedImages.Count - 1 do
    if TPDFImage ( FLinkedImages [ I ] ).PictureID = -1 then
      TPDFImage ( FLinkedImages [ I ] ).Save ( FOwner );
  for I := 0 to FAnnot.Count - 1 do
    if TPDFCustomAnnotation ( FAnnot [ I ] ).AnnotID = -1 then
      TPDFCustomAnnotation ( FAnnot [ I ] ).AnnotID := FOwner.GetNextID;
  if FThumbnail >= 0 then
    if FOwner.FImages [ FThumbnail ].PictureID = -1 then
      FOwner.FImages [ FThumbnail ].Save ( FOwner );
end;

function TPDFPage.CreateControl ( CClass: TPDFControlClass; ControlName: string;
  Box: TRect ): TPDFControl;
var
  SO: TPDFControl;
begin
  SO := CClass.Create ( Self, ControlName );
  SO.Box := Box;
  Result := SO;
end;

function TPDFPage.GetTextRowCount ( BoxWidth: Integer; TextStr: string ): integer;
var
  i, Len: integer;
  Ch: char;
  CWidth, StrWidth, OutWidth: Extended;
  CF: TPDFFont;
begin
  Result := 0;
  StrWidth := 0;
  OutWidth := 0;
  i := 1;
  CF := FOwner.FFonts.GetFontByInfo ( FCurrentFontName, FCurrentFontStyle, false );
  Len := Length ( TextStr );
  while i <= Len do
  begin
    ch := TextStr [ i ];
    CWidth := CF.GetWidth ( Ord ( ch ) )
      * FCurrentFontSize / 1000;
    if FHorizontalScaling <> 100 then
      CWidth := CWidth * FHorizontalScaling / 100;
    if CWidth > 0 then
      CWidth := CWidth + FCharSpace
    else
      CWidth := 0;
    if ( ch = ' ' ) and ( FWordSpace > 0 ) and ( i <> Len ) then
      CWidth := CWidth + FWordSpace;
    if ( ( OutWidth + StrWidth + CWidth ) < BoxWidth ) and
      ( i < Length ( TextStr ) ) and ( not ( Ch in [ #10, #13 ] ) ) then
    begin
      StrWidth := StrWidth + CWidth;
      if ch = ' ' then
      begin
        OutWidth := OutWidth + StrWidth;
        StrWidth := 0;
      end;
    end
    else
    begin
      if ( ch = #13 ) and ( i < Len ) then
        if ( TextStr [ i + 1 ] = #10 ) then
          Inc ( i );
      if i = Len then
        StrWidth := CWidth
      else
        StrWidth := StrWidth + CWidth;
      OutWidth := 0;
      Inc ( Result );
    end;
    Inc ( i );
  end;
end;


procedure TPDFPage.SetCurrentFont ( Unicode: Boolean );
var
  I: Integer;
  fnd: Boolean;
begin
  if FFontIsChanged or ( FUnicodeUse <> Unicode ) then
  begin
    BeginText;
    FTextUsed := True;
    FCurrentFont := FOwner.FFonts.GetFontByInfo ( FCurrentFontName, FCurrentFontStyle, Unicode );
    fnd := False;
    for I := 0 to Length ( FLinkedFont ) - 1 do
      if FCurrentFont = FLinkedFont [ I ] then
      begin
        fnd := true;
        Break;
      end;
    if not fnd then
    begin
      I := Length ( FLinkedFont );
      SetLength ( FLinkedFont, I + 1 );
      FLinkedFont [ I ] := FCurrentFont;
    end;
    AppendAction ( '/' + FCurrentFont.AliasName + ' 1 Tf' );
    FCurrentFont.FFontUsed := True;
    FFontIsChanged := False;
    FUnicodeUse := Unicode;
    FCurrentFontIndex := FSaveCount;
  end;
end;

function TPDFPage.GetUnicodeWidth ( Text: PWord; Len: Integer ): Extended;
begin
  Result := IntToExtX ( RawGetUnicodeWidth ( Text, Len ) );
end;

procedure TPDFPage.UnicodeTextShow ( Text: PWord; Len: Integer );
var
  S: string;
  W: PWord;
  I: Integer;
  U: Boolean;
  Cr, Sp: Integer;
  L: Integer;
  TL: Extended;
  Calc: Boolean;
  B: Byte;
begin
  if Len = 0 then
    Exit;

  Calc := ( fsUnderline in FCurrentFontStyle ) or ( fsStrikeOut in FCurrentFontStyle );
  TL := 0;
  Cr := 0;
  Sp := 0;
  W := Text;
  for I := 0 to Len -1  do
  begin
    if W^ = 160 then
      W^ := 32;
    Inc ( W )
  end;
  W := Text;
  FTextUsed := True;
  SetCurrentFont ( not IsANSICode ( W^ ) );
  if FUnicodeUse then
  begin
    S := WordToHex ( FCurrentFont.TTFInfo.GetCharID ( W^ ) );
    if Calc then
    begin
      L := FCurrentFont.TTFInfo.GetCharWidth ( W^ );
      if ( L <> 0 ) or ( len <> 1 ) then
        Inc ( Cr );
      TL := L;
    end;
  end
  else
  begin
    B := ANSIUNICODEToByte ( W^ );
    S := Chr ( B );
    if Calc then
    begin
      if ( B = 32 ) and ( Len <> 1 ) then
        Inc ( Sp );
      if FCurrentFont.Standart then
        L := StWidth [ FCurrentFont.StdID, B ]
      else
        L := FCurrentFont.FWidthArray^ [ B ];
      if ( L <> 0 ) or ( len <> 1 ) then
        Inc ( Cr );
      TL := L;
    end;
  end;
  Inc ( W );
  for I := 2 to Len do
  begin
    U := not IsANSICode ( W^ );
    if U <> FUnicodeUse then
    begin
      if FUnicodeUse then
        S := '<' + S + '> Tj'
      else
      begin
        FCurrentFont.FillUsed ( S );
        S := '(' + EscapeSpecialChar ( S ) + ') Tj';
      end;
      AppendAction ( S );
      SetCurrentFont ( U );
      if FUnicodeUse then
      begin
        S := WordToHex ( FCurrentFont.TTFInfo.GetCharID ( W^ ) );
        if Calc then
        begin
          L := FCurrentFont.TTFInfo.GetCharWidth ( W^ );
          if ( L <> 0 ) or ( len <> I ) then
            Inc ( Cr );
          TL := TL + L;
        end;
      end
      else
      begin
        B := ANSIUNICODEToByte ( W^ );
        S := Chr ( B );
        if Calc then
        begin
          if ( B = 32 ) and ( Len <> I ) then
            Inc ( Sp );
          if FCurrentFont.Standart then
            L := StWidth [ FCurrentFont.StdID, B ]
          else
            L := FCurrentFont.FWidthArray^ [ B ];
          if ( L <> 0 ) or ( len <> I ) then
            Inc ( Cr );
          TL := TL + L;
        end;
      end;
    end
    else if FUnicodeUse then
    begin
      S := S + WordToHex ( FCurrentFont.TTFInfo.GetCharID ( W^ ) );
      if Calc then
      begin
        L := FCurrentFont.TTFInfo.GetCharWidth ( W^ );
        if ( L <> 0 ) or ( len <> I ) then
          Inc ( Cr );
        TL := TL + L;
      end;
    end
    else
    begin
      B := ANSIUNICODEToByte ( W^ );
      S := S + Chr ( B );
      if Calc then
      begin
        if ( B = 32 ) and ( Len <> I ) then
          Inc ( Sp );
        if FCurrentFont.Standart then
          L := StWidth [ FCurrentFont.StdID, B ]
        else
          L := FCurrentFont.FWidthArray^ [ B ];
        if ( L <> 0 ) or ( len <> I ) then
          Inc ( Cr );
        TL := TL + L;
      end;
    end;
    Inc ( W );
  end;
  if FUnicodeUse then
    S := '<' + S + '> Tj'
  else
  begin
    FCurrentFont.FillUsed ( S );
    S := '(' + EscapeSpecialChar ( S ) + ') Tj';
  end;
  AppendAction ( S );
  if not Calc then
    Exit;
  TL := TL * FCurrentFontSize / 1000;
  if FHorizontalScaling <> 100 then
    TL := TL * FHorizontalScaling / 100;
  if FWordSpace > 0 then
    TL := TL + Sp * FWordSpace;
  if FCharSpace > 0 then
    TL := TL + Cr * FCharSpace;
  PaintTextLines ( TL );
end;

function TPDFPage.RawGetTextWidth ( Text: string ): Extended;
var
  i, L: integer;
  CF: TPDFFont;
  TL: Integer;
  Cr, Sp: Integer;
  B: Byte;
  CodePage: Integer;
  Len: Integer;
  Mem: PWord;
begin
  Result := 0;
  TL := Length ( Text );
  if ( FHorizontalScaling <= 0 ) or ( TL = 0 ) then
    Exit;
  if FCharset = 0 then
  begin
    if FFontIsChanged or FUnicodeUse then
      CF := FOwner.FFonts.GetFontByInfo ( FCurrentFontName, FCurrentFontStyle, False )
    else
      CF := FCurrentFont;
    Cr := 0;
    Sp := 0;
    for i := 1 to TL do
    begin
      B := Byte ( Text [ i ] );
      if ( B = 32 ) and ( I <> TL ) then
        Inc ( Sp );
      if CF.Standart then
        L := StWidth [ CF.StdID, B ]
      else
        L := CF.FWidthArray^ [ B ];
      if ( L <> 0 ) or ( i <> TL ) then
        Inc ( Cr );
      Result := Result + L;
    end;
    Result := Result * FCurrentFontSize / 1000;
    if FHorizontalScaling <> 100 then
      Result := Result * FHorizontalScaling / 100;
    if FWordSpace > 0 then
      Result := Result + Sp * FWordSpace;
    if FCharSpace > 0 then
      Result := Result + Cr * FCharSpace;
  end
  else
  begin
    CodePage := CharSetToCodePage ( FCharset );
    Len := MultiByteToWideChar ( CodePage, 0, PAnsiChar ( Text ), TL, nil, 0 );
    if Len = 0 then
      raise TPDFException.Create ( 'Cannot convert text to unicode' );
    GetMem ( Mem, Len * 2 );
    try
      Len := MultiByteToWideChar ( CodePage, 0, PAnsiChar ( Text ), TL, PWideChar ( Mem ), Len );
      if Len = 0 then
        raise TPDFException.Create ( 'Cannot convert text to unicode' );
      Result := RawGetUnicodeWidth ( Mem, Len );
    finally
      FreeMem ( Mem );
    end;
  end;
end;

function TPDFPage.RawGetUnicodeWidth ( Text: PWord; Len: Integer ): Extended;
var
  CF, CFU: TPDFFont;
  L, I: Integer;
  W: PWord;
  Cr: Integer;
  Sp: Integer;
  B: Byte;
begin
  Result := 0;
  if Len = 0 then
    Exit;
  Sp := 0;
  Cr := 0;
  W := Text;
  CF := nil;
  CFU := nil;
  for I := 1 to Len do
  begin
    if IsANSICode ( W^ ) then
    begin
      if CF = nil then
        if FFontIsChanged or FUnicodeUse then
          CF := FOwner.FFonts.GetFontByInfo ( FCurrentFontName, FCurrentFontStyle, False )
        else
          CF := FCurrentFont;
      B := ANSIUNICODEToByte ( W^ );
      if ( W^ = 32 ) and ( I <> Len ) then
        Inc ( Sp );
      if CF.Standart then
        L := StWidth [ CF.StdID, B ]
      else
        L := CF.FWidthArray^ [ B ];
    end
    else
    begin
      if CFU = nil then
        if FFontIsChanged or ( not FUnicodeUse ) then
          CFU := FOwner.FFonts.GetFontByInfo ( FCurrentFontName, FCurrentFontStyle, True )
        else
          CFU := FCurrentFont;
      L := CFU.TTFInfo.GetCharWidth ( W^ );
    end;
    if ( L <> 0 ) or ( i <> Len ) then
      Inc ( Cr );
    Result := Result + L;
    Inc ( W );
  end;
  Result := Result * FCurrentFontSize / 1000;
  if FHorizontalScaling <> 100 then
    Result := Result * FHorizontalScaling / 100;
  if FWordSpace > 0 then
    Result := Result + Sp * FWordSpace;
  if FCharSpace > 0 then
    Result := Result + Cr * FCharSpace;
end;

procedure TPDFPage.UnicodeTextOut ( X, Y, Orientation: Extended; Text: PWord;
  Len: Integer );
var
  O: Extended;
  ws, URL: string;
  I, J: Integer;
  K: Integer;
  Off: Integer;
  offUrl, LenUrl: Extended;
  fnd: Boolean;
  T: PWord;
begin
  if Len = 0 then
    Exit;
  O := GetRawTextHeight;
  if Orientation = 0 then
  begin
    RawUnicodeTextOut ( ExtToIntX ( X ), ExtToIntY ( Y ) - o, Orientation, Text, Len );
    if FOwner.FACURL then
      if not FIsForm then
      begin
        ws := '';
        T := Text;
        for I := 1 to Len do
        begin
          ws := ws + chr ( Byte ( T^ ) );
          Inc ( T );
        end;
        ws := LowerCase ( ws );
        fnd := False;
        for I := 0 to 2 do
          if PosText ( URLDetectStrings [ i ], ws, 1 ) <> 0 then
            fnd := True;
        if fnd then
        begin
          for I := 0 to 2 do
          begin
            OFF := 1;
            K := PosText ( URLDetectStrings [ i ], ws, 1 );
            while K <> 0 do
            begin
              if K <> 1 then
                if ws [ K - 1 ] <> #32 then
                begin
                  OFF := PosText ( ' ', ws, OFF ) + 1;
                  K := PosText ( URLDetectStrings [ i ], ws, OFF );
                  Continue;
                end;
              OFF := PosText ( ' ', ws, K + 1 );
              T := Text;
              Inc ( T, K - 1 );
              URL := '';
              if OFF = 0 then
              begin
                off := Length ( ws );
                for J := 1 to OFF - K + 1 do
                begin
                  URL := URL + chr ( Byte ( T^ ) );
                  Inc ( T );
                end;
              end
              else
                for J := 1 to OFF - K do
                begin
                  URL := URL + chr ( Byte ( T^ ) );
                  Inc ( T );
                end;
              offUrl := GetUnicodeWidth ( Text, K - 1 );
              LenUrl := GetTextWidth ( URL );
              if FBaseLine then
                SetUrl ( Rect ( Trunc ( X + offUrl ), Trunc ( Y ), Trunc ( X + offURL + LenUrl ), Trunc ( Y - FCurrentFontSize * d2p ) ), URL )
              else
                SetUrl ( Rect ( Trunc ( X + offUrl ), trunc ( Y ), Trunc ( X + offURL + LenUrl ), Trunc ( Y + O * d2p ) ), URL );
              K := PosText ( URLDetectStrings [ i ], ws, OFF );
            end;
          end;
        end;
      end;
  end
  else
    RawUnicodeTextOut ( ExtToIntX ( X ) + O * sin ( Orientation * Pi / 180 ),
      ExtToIntY ( Y ) - O * cos ( Orientation * Pi / 180 ), Orientation, Text, Len );
end;

procedure TPDFPage.ExtTextShow ( TextStr: string; Dx: PExt );
var
  CodePage: Integer;
  Len: Integer;
  Mem: PWord;
  S: string;
  TL: Integer;
  I: Integer;
  P: PExt;
  SZ: Extended;
  B: Byte;
begin
  if TextStr = '' then
    Exit;
   FTextUsed := True;
  SetHorizontalScaling ( 100 );
  SetWordSpacing ( 0 );
  SetCharacterSpacing ( 0 );
  P := Dx;
  if FCharset = 0 then
  begin
    SetCurrentFont ( False );
    if not FCurrentFont.Standart then
      for I := 1 to Length ( TextStr ) do
        if TextStr[I] = #160 then
          TextStr[I] := #32;
    FCurrentFont.FillUsed ( TextStr );
    for I := 1 to Length ( TextStr ) do
    begin
      B := Byte ( TextStr [ i ] );
      S := '(' + EscapeSpecialChar ( Chr ( B ) ) + ')';
      AppendAction ( S + ' Tj ' + FormatFloat ( p^ ) + ' 0 Td' );
      Inc ( P );
    end;
    P := Dx;
    SZ := 0;
    if ( ( not ( fsUnderLine in FCurrentFontStyle ) ) and ( not ( fsStrikeOut in FCurrentFontStyle ) ) ) then
      Exit;
    for I := 1 to Length ( TextStr ) do
    begin
      SZ := SZ + P^;
      Inc ( P );
    end;
    PaintTextLines ( SZ );
  end
  else
  begin
    TL := Length ( TextStr );
    CodePage := CharSetToCodePage ( FCharset );
    Len := MultiByteToWideChar ( CodePage, 0, PAnsiChar ( TextStr ), TL, nil, 0 );
    if Len = 0 then
      raise TPDFException.Create ( 'Cannot convert text to unicode' );
    GetMem ( Mem, Len * 2 );
    try
      Len := MultiByteToWideChar ( CodePage, 0, PAnsiChar ( TextStr ), TL, PWideChar ( Mem ), Len );
      if Len = 0 then
        raise TPDFException.Create ( 'Cannot convert text to unicode' );
      ExtUnicodeTextShow ( Mem, Len, Dx );
    finally
      FreeMem ( Mem );
    end;
  end;
end;

procedure TPDFPage.ExtUnicodeTextShow ( Text: PWord; Len: Integer; Dx: PExt );
var
  I: Integer;
  ANSI: Boolean;
  OS: String;
  A: Integer;
  RS: Extended;
  CS: Extended;
  D: Extended;
  Ar: PWordArray;
  SZ: PSingleArray;
  B: Byte;
  ID: WORD;
  K: Integer;
begin
  if Len = 0 then
    Exit;
  AR := PWORDArray ( Text );
  SZ := PSingleArray ( Dx );
  A := 0;
  SetWordSpacing ( 0 );
  for I := 0 to Len -1  do
      if Ar[I] = 160 then
        Ar[I] := 32;
  while ( True ) do
  begin
    RS := 0;
    CS := 0;
    K := 0;
    ANSI := IsANSICode ( Ar^ [ A ] );
    OS := '';
    SetCurrentFont ( not ANSI );
    while ( True ) do
    begin
      if A >= Len then
      begin
        if k > 0 then
          SetIntCharacterSpacing ( ( CS - RS ) / k );
        Break;
      end;
      if ANSI <> IsANSICode ( Ar^ [ A ] ) then
      begin
        if k > 0 then
          SetIntCharacterSpacing ( ( CS - RS ) / k );
        break;
      end;
      if ANSI then
      begin
        B := ANSIUNICODEToByte ( Ar^ [ A ] );
        if FCurrentFont.Standart then
          D := StWidth [ FCurrentFont.StdID, B ] / 1000
        else
          D := FCurrentFont.FWidthArray^ [ B ] / 1000;
        if B < 32 then
          B := 32;
        OS := OS + chr ( b );
        FCurrentFont.UsedChar ( B );
      end
      else
      begin
        D := FCurrentFont.TTFInfo.GetCharWidth ( Ar^ [ A ] ) / 1000;
        ID := FCurrentFont.TTFInfo.GetCharID ( Ar^ [ A ] );
        OS := OS + WordToHex ( ID );
      end;
      if FHorizontalScaling <> 100 then
        D := D * FHorizontalScaling / 100;
      if abs ( D - SZ^ [ A ] ) > 0.1 then
      begin
        if k > 0 then
          SetIntCharacterSpacing ( ( CS - RS ) / k );
        CS := CS + SZ^ [ A ];
        inc ( A );
        Inc ( k );
        break;
      end;
      RS := RS + D;
      CS := CS + SZ^ [ A ];
      inc ( A );
      Inc ( k );
    end;
    if K <> 0 then
    begin
      if ANSI then
        AppendAction ( '(' + EscapeSpecialChar ( OS ) + ') Tj' )
      else
        AppendAction ( '<' + OS + '> Tj' );
    end;
    if A < Len then
      AppendAction ( FormatFloat ( CS ) + ' 0 Td' )
    else
      Break;
  end;
  if ( ( not ( fsUnderLine in FCurrentFontStyle ) ) and ( not ( fsStrikeOut in FCurrentFontStyle ) ) ) then
    Exit;
  RS := 0;
  for I := 0 to Len - 1 do
    RS := RS + sz^ [ i ];
  RS := RS * FCurrentFontSize;
  PaintTextLines ( RS );
end;


procedure TPDFPage.RawExtTextOut ( X, Y, Orientation: Extended; TextStr: string;
  Dx: PExt );
begin
  RawSetTextPosition ( X, Y, Orientation );
  ExtTextShow ( TextStr, Dx );
end;

procedure TPDFPage.RawExtUnicodeTextOut ( X, Y, Orientation: Extended;
  Text: PWord; Len: Integer; DX: PExt );
begin
  RawSetTextPosition ( X, Y, Orientation );
  ExtUnicodeTextShow ( Text, Len, Dx );
end;

procedure TPDFPage.ExtTextOut ( X, Y, Orientation: Extended; TextStr: string;
  Dx: PExt );
var
  I, J: Integer;
  Ext: array of Single;
  P: PExt;
  K: Integer;
  O: Extended;
  ws, URL: string;
  Off: Integer;
  offUrl, LenUrl: Extended;
  fnd: Boolean;
begin
  if TextStr = '' then
    Exit;
  if Dx = nil then
  begin
{$IFDEF CB}
    TextOutput ( X, Y, Orientation, TextStr );
{$ELSE}
    TextOut ( X, Y, Orientation, TextStr );
{$ENDIF}
    Exit;
  end;
  k := length ( TextStr );
  SetLength ( Ext, K );
  P := DX;
  for I := 0 to k - 1 do
  begin
    Ext [ i ] := p^ / D2P / FCurrentFontSize;
    inc ( P );
  end;
  O := GetRawTextHeight;
  if Orientation = 0 then
  begin
    RawExtTextOut ( ExtToIntX ( X ), ExtToIntY ( Y ) - O, Orientation, TextStr, @Ext [ 0 ] );
    if FOwner.FACURL then
      if not FIsForm then
      begin
        ws := LowerCase ( TextStr );
        fnd := False;
        for I := 0 to 2 do
          if PosText ( URLDetectStrings [ i ], ws, 1 ) <> 0 then
            fnd := True;
        if fnd then
        begin
          for I := 0 to 2 do
          begin
            OFF := 1;
            K := PosText ( URLDetectStrings [ i ], ws, 1 );
            while K <> 0 do
            begin
              if K <> 1 then
                if ws [ K - 1 ] <> #32 then
                begin
                  OFF := PosText ( ' ', ws, OFF ) + 1;
                  K := PosText ( URLDetectStrings [ i ], ws, OFF );
                  Continue;
                end;
              OFF := PosText ( ' ', ws, K + 1 );
              if OFF = 0 then
              begin
                off := Length ( ws );
                URL := Copy ( TextStr, K, OFF - K + 1 );
              end
              else
                URL := Copy ( TextStr, K, OFF - K );
              P := Dx;
              offUrl := 0;
              LenUrl := 0;
              for J := 1 to K - 1 do
              begin
                offUrl := offUrl + p^;
                Inc ( P );
              end;
              for j := 1 to Length ( URL ) do
              begin
                LenUrl := LenUrl + p^;
                Inc ( P );
              end;
              if FBaseLine then
                SetUrl ( Rect ( Trunc ( X + offUrl ), Trunc ( Y ), Trunc ( X + offURL + LenUrl ), Trunc ( Y - FCurrentFontSize * d2p ) ), URL )
              else
                SetUrl ( Rect ( Trunc ( X + offUrl ), trunc ( Y ), Trunc ( X + offURL + LenUrl ), Trunc ( Y + O * d2p ) ), URL );
              K := PosText ( URLDetectStrings [ i ], ws, OFF );
            end;
          end;
        end;
      end;
  end
  else
    RawExtTextOut ( ExtToIntX ( X ) + o * sin ( Orientation * Pi / 180 ), ExtToIntY ( Y ) - o * cos ( Orientation * Pi / 180 ),
      Orientation, TextStr, @Ext [ 0 ] );
  Ext := nil;
end;

procedure TPDFPage.ExtUnicodeTextOut ( X, Y, Orientation: Extended;
  Text: PWord; Len: Integer; DX: PExt );
var
  I: Integer;
  Ext: array of Single;
  P: PExt;
  o: Extended;
  ws, URL: string;
  J: Integer;
  K: Integer;
  Off: Integer;
  offUrl, LenUrl: Extended;
  fnd: Boolean;
  T: PWord;
begin
  if Len = 0 then
    Exit;
  if DX = nil then
  begin
    UnicodeTextOut ( X, Y, Orientation, Text, Len );
    Exit;
  end;
  SetLength ( Ext, Len );
  P := DX;
  for I := 0 to Len - 1 do
  begin
    Ext [ i ] := p^ / D2P / FCurrentFontSize;
    inc ( P );
  end;
  o := GetRawTextHeight;
  if Orientation = 0 then
  begin
    RawExtUnicodeTextOut ( ExtToIntX ( X ), ExtToIntY ( Y ) - O, Orientation, Text, Len, @Ext [ 0 ] );
    if FOwner.FACURL then
      if not FIsForm then
      begin
        ws := '';
        T := Text;
        for I := 1 to Len do
        begin
          ws := ws + chr ( Byte ( T^ ) );
          Inc ( T );
        end;
        ws := LowerCase ( ws );
        fnd := False;
        for I := 0 to 2 do
          if PosText ( URLDetectStrings [ i ], ws, 1 ) <> 0 then
            fnd := True;
        if fnd then
        begin
          for I := 0 to 2 do
          begin
            OFF := 1;
            K := PosText ( URLDetectStrings [ i ], ws, 1 );
            while K <> 0 do
            begin
              if K <> 1 then
                if ws [ K - 1 ] <> #32 then
                begin
                  OFF := PosText ( ' ', ws, OFF ) + 1;
                  if off = 1 then
                    break;
                  K := PosText ( URLDetectStrings [ i ], ws, OFF );
                  Continue;
                end;
              OFF := PosText ( ' ', ws, K + 1 );
              T := Text;
              Inc ( T, K - 1 );
              URL := '';
              if OFF = 0 then
              begin
                off := Length ( ws );
                for J := 1 to OFF - K + 1 do
                begin
                  URL := URL + chr ( Byte ( T^ ) );
                  Inc ( T );
                end;
              end
              else
                for J := 1 to OFF - K do
                begin
                  URL := URL + chr ( Byte ( T^ ) );
                  Inc ( T );
                end;
              P := Dx;
              offUrl := 0;
              LenUrl := 0;
              for J := 1 to K - 1 do
              begin
                offUrl := offUrl + p^;
                Inc ( P );
              end;
              for j := 1 to Length ( URL ) do
              begin
                LenUrl := LenUrl + p^;
                Inc ( P );
              end;
              if FBaseLine then
                SetUrl ( Rect ( Trunc ( X + offUrl ), Trunc ( Y ), Trunc ( X + offURL + LenUrl ), Trunc ( Y - FCurrentFontSize * d2p ) ), URL )
              else
                SetUrl ( Rect ( Trunc ( X + offUrl ), trunc ( Y ), Trunc ( X + offURL + LenUrl ), Trunc ( Y + O * d2p ) ), URL );
              K := PosText ( URLDetectStrings [ i ], ws, OFF );
            end;
          end;
        end;
      end;
  end
  else
    RawExtUnicodeTextOut ( ExtToIntX ( X ) + o * sin ( Orientation * Pi / 180 ),
      ExtToIntY ( Y ) - o * cos ( Orientation * Pi / 180 ), Orientation, Text, Len, @Ext [ 0 ] );
  Ext := nil;
end;

function TPDFPage.GetRawTextHeight: Extended;
var
  CF: TPDFFont;
begin
  if FBaseLine then
    Result := 0
  else if FFontIsChanged then
  begin
    CF := FOwner.FFonts.GetFontByInfo ( FCurrentFontName, FCurrentFontStyle, False );
    Result := FCurrentFontSize * ( CF.Abscent ) / 1000;
  end
  else
    Result := FCurrentFontSize * ( FCurrentFont.Abscent ) / 1000;
end;

function TPDFPage.GetUnicodeTextRowCount ( BoxWidth: Integer; Text: PWord;
  Len: Integer ): Integer;
var
  BeforeLastWordWidth, LastWordWidth: Extended;
  i, OutLen: integer;
  CWidth, StrWidth: Extended;
  PW: PWord;
  CU: Word;
  T: PWord;
  CF, CFU: TPDFFont;
  LastWord: PWord;
  LineCount: Integer;
  BeforeLastWordChar: Integer;
  NX: Boolean;
begin
  if Len = 0 then
  begin
    Result := 0;
    Exit;
  end;
  CF := nil;
  CFU := nil;
  i := 1;
  PW := Text;
  OutLen := 0;
  BeforeLastWordWidth := 0;
  LastWordWidth := 0;
  LineCount := 0;
  LastWord := nil;
  StrWidth := BoxWidth * D2P;
  BeforeLastWordChar := 0;
  while i <= Len do
  begin
    CU := PW^;
    if IsANSICode ( CU ) then
    begin
      if CF = nil then
        CF := FOwner.FFonts.GetFontByInfo ( FCurrentFontName, FCurrentFontStyle, False );
      if CF.Standart then
        CWidth := StWidth [ CF.StdID, ANSIUNICODEToByte ( CU ) ]
      else
        CWidth := CF.GetWidth ( ANSIUNICODEToByte ( CU ) );
    end
    else
    begin
      if CFU = nil then
        CFU := FOwner.FFonts.GetFontByInfo ( FCurrentFontName, FCurrentFontStyle, True );
      CWidth := CFU.TTFInfo.GetCharWidth ( CU );
    end;
    CWidth := CWidth * FCurrentFontSize / 1000;
    if FHorizontalScaling <> 100 then
      CWidth := CWidth * FHorizontalScaling / 100;
    if CWidth > 0 then
      CWidth := CWidth + FCharSpace
    else
      CWidth := 0;
    if ( cu = 32 ) and ( FWordSpace > 0 ) and ( i <> Len ) then
      CWidth := CWidth + FWordSpace;
    if ( BeforeLastWordWidth + LastWordWidth + CWidth < StrWidth ) and ( not ( ( CU = 13 ) or ( CU = 10 ) ) ) and ( i <> Len ) then
    begin
      Inc ( OutLen );
      if CU = 32 then
      begin
        BeforeLastWordWidth := BeforeLastWordWidth + LastWordWidth + CWidth;
        LastWordWidth := 0;
        BeforeLastWordChar := OutLen;
        LastWord := PW;
        Inc ( LastWord );
      end
      else
        LastWordWidth := CWidth + LastWordWidth;
    end
    else
    begin
      if ( CU = 13 ) and ( i <> Len ) then
      begin
        T := PW;
        Inc ( T );
        if T^ = 10 then
        begin
          Inc ( I );
          Inc ( PW );
        end;
      end;
      if ( CU = 10 ) or ( CU = 13 ) or ( CU = 32 ) or ( i = Len ) then
      begin
        NX := ( BeforeLastWordWidth + LastWordWidth + CWidth > StrWidth ) and ( i = Len );
        Inc ( LineCount );
        if NX then
          OutLen := 1
        else
          OutLen := 0;
        BeforeLastWordChar := 0;
        BeforeLastWordWidth := 0;
        LastWordWidth := 0;
        LastWord := nil;
      end
      else
      begin
        if LastWord <> nil then
        begin
          LastWordWidth := LastWordWidth + CWidth;
          OutLen := OutLen - BeforeLastWordChar + 1;
          BeforeLastWordChar := 0;
          BeforeLastWordWidth := 0;
          LastWord := nil;
          Inc ( LineCount );
        end
        else
        begin
          Inc ( LineCount );
          OutLen := 1;
          BeforeLastWordChar := 0;
          BeforeLastWordWidth := 0;
          LastWordWidth := CWidth;
          LastWord := nil;
        end;
      end;
    end;
    Inc ( i );
    Inc ( PW );
  end;
  if OutLen <> 0 then
    Inc ( LineCount );
  Result := LineCount;
end;

function TPDFPage.UnicodeTextOutBox ( LTCornX, LTCornY, Interval, BoxWidth,
  BoxHeight: integer; Text: PWord; Len: Integer ): integer;
var
  BeforeLastWordWidth, LastWordWidth: Extended;
  i, OutLen: integer;
  CWidth, StrWidth: Extended;
  PW: PWord;
  CU: Word;
  StartLine, T: PWord;
  CF, CFU: TPDFFont;
  LastWord: PWord;
  LineCount: Integer;
  BeforeLastWordChar: Integer;
  Base: Boolean;
  NX: Boolean;
  MaxLine: Integer;
begin
  if Len = 0 then
  begin
    Result := 0;
    Exit;
  end;
  Result := 0; //
  CF := nil;
  CFU := nil;
  Base := FBaseLine;
  i := 1;
  PW := Text;
  StartLine := Text;
  OutLen := 0;
  BeforeLastWordWidth := 0;
  LastWordWidth := 0;
  LineCount := 0;
  LastWord := nil;
  StrWidth := BoxWidth / d2p;
  BeforeLastWordChar := 0;
  if BoxHeight < FCurrentFontSize then
    Exit;
  MaxLine := trunc ( ( BoxHeight - FCurrentFontSize ) / Interval );
  while i <= Len do
  begin
    CU := PW^;
    if IsANSICode ( CU ) then
    begin
      if CF = nil then
        CF := FOwner.FFonts.GetFontByInfo ( FCurrentFontName, FCurrentFontStyle, False );
      if CF.Standart then
        CWidth := StWidth [ CF.StdID, ANSIUNICODEToByte ( CU ) ]
      else
        CWidth := CF.GetWidth ( ANSIUNICODEToByte ( CU ) );
    end
    else
    begin
      if CFU = nil then
        CFU := FOwner.FFonts.GetFontByInfo ( FCurrentFontName, FCurrentFontStyle, True );
      CWidth := CFU.TTFInfo.GetCharWidth ( CU );
    end;
    CWidth := CWidth * FCurrentFontSize / 1000;
    if FHorizontalScaling <> 100 then
      CWidth := CWidth * FHorizontalScaling / 100;
    if CWidth > 0 then
      CWidth := CWidth + FCharSpace
    else
      CWidth := 0;
    if ( cu = 32 ) and ( FWordSpace > 0 ) and ( i <> Len ) then
      CWidth := CWidth + FWordSpace;
    if ( BeforeLastWordWidth + LastWordWidth + CWidth < StrWidth ) and ( not ( ( CU = 13 ) or ( CU = 10 ) ) ) and ( i <> Len ) then
    begin
      Inc ( OutLen );
      if CU = 32 then
      begin
        BeforeLastWordWidth := BeforeLastWordWidth + LastWordWidth + CWidth;
        LastWordWidth := 0;
        BeforeLastWordChar := OutLen;
        LastWord := PW;
        Inc ( LastWord );
      end
      else
        LastWordWidth := CWidth + LastWordWidth;
    end
    else
    begin
      if ( CU = 13 ) and ( i <> Len ) then
      begin
        T := PW;
        Inc ( T );
        if T^ = 10 then
        begin
          Inc ( I );
          Inc ( PW );
        end;
      end;
      if ( CU = 10 ) or ( CU = 13 ) or ( CU = 32 ) or ( i = Len ) then
      begin
        NX := ( BeforeLastWordWidth + LastWordWidth + CWidth > StrWidth ) and ( i = Len );
        if ( i = Len ) and not ( ( CU = 10 ) or ( CU = 13 ) or ( CU = 32 ) or NX ) then
          Inc ( OutLen );
        if OutLen <> 0 then
          UnicodeTextOut ( LTCornX, LTCornY + Interval * LineCount, 0, StartLine, OutLen );
        Inc ( LineCount );
        if NX then
          OutLen := 1
        else
          OutLen := 0;
        BeforeLastWordChar := 0;
        BeforeLastWordWidth := 0;
        LastWordWidth := 0;
        LastWord := nil;
        StartLine := PW;
        if not NX then
          Inc ( StartLine );
      end
      else
      begin
        if LastWord <> nil then
        begin
          UnicodeTextOut ( LTCornX, LTCornY + Interval * LineCount, 0, StartLine, BeforeLastWordChar - 1 );
          StartLine := LastWord;
          LastWordWidth := LastWordWidth + CWidth;
          OutLen := OutLen - BeforeLastWordChar + 1;
          BeforeLastWordChar := 0;
          BeforeLastWordWidth := 0;
          LastWord := nil;
          Inc ( LineCount );
        end
        else
        begin
          if OutLen <> 0 then
            UnicodeTextOut ( LTCornX, LTCornY + Interval * LineCount, 0, StartLine, OutLen );
          Inc ( LineCount );
          OutLen := 1;
          BeforeLastWordChar := 0;
          BeforeLastWordWidth := 0;
          LastWordWidth := CWidth;
          LastWord := nil;
          StartLine := PW;
        end;
      end;
    end;
    if MaxLine < LineCount then
    begin
      Result := i;
      FBaseLine := Base;
      Exit;
    end;
    Inc ( i );
    Inc ( PW );
  end;
  if OutLen <> 0 then
    UnicodeTextOut ( LTCornX, LTCornY + Interval * LineCount, 0, StartLine, OutLen );
  Result := i;
  FBaseLine := Base;
end;

procedure TPDFPage.ExtGlyphTextOut ( X, Y, Orientation: Extended;
  Text: PWord; Len: Integer; DX: PExt );
var
  I: Integer;
  Ext: array of Single;
  P: PExt;
  o: Extended;
begin
  if Len = 0 then
    Exit;
  if DX <> nil then
  begin
    SetLength ( Ext, Len );
    P := DX;
    for I := 0 to Len - 1 do
    begin
      Ext [ i ] := p^ / D2P / FCurrentFontSize;
      inc ( P );
    end;
  end
  else
    Ext := nil;
  o := GetRawTextHeight;
  if Orientation = 0 then
    RawExtGlyphTextOut ( ExtToIntX ( X ), ExtToIntY ( Y ) - O, Orientation, Text, Len, @Ext [ 0 ] )
  else
    RawExtGlyphTextOut ( ExtToIntX ( X ) + o * sin ( Orientation * Pi / 180 ),
      ExtToIntY ( Y ) - o * cos ( Orientation * Pi / 180 ), Orientation, Text, Len, @Ext [ 0 ] );
  Ext := nil;
end;

procedure TPDFPage.ExtGlyphTextShow ( Text: PWord; Len: Integer; Dx: PExt );
var
  ID: Word;
  S: string;
  P: PWord;
  i: Integer;
  Width: Extended;
  Node: PTTFTreeNode;
  CalcWidth: Boolean;
  CharCount, SpaceCount: Integer;
  TextArray: PWordArray;
  SizeArray: PSingleArray;
  A, K: Integer;
  RealSize: Extended;
  CalculateSize: Extended;
  CharWidth: Extended;
  OutString: string;
begin
  SetCurrentFont ( True );
  if FCurrentFont.Standart then
    Exit;
  Width := 0;
  CharCount := 0;
  SpaceCount := 0;
  CalcWidth := ( fsUnderline in FCurrentFontStyle ) or ( fsStrikeOut in FCurrentFontStyle );
  if Dx = nil then
  begin
    P := Text;
    S := '<';
    for i := 0 to Len - 1 do
    begin
      ID := P^;
      Node := FCurrentFont.TTFInfo.GetNodeByID ( ID );
      Node^.Used := True;
      if CalcWidth then
      begin
        if Node^.Code = 32 then
        begin
          Inc ( SpaceCount );
        end;
        Inc ( CharCount );
        Width := Width + Node^.Width;
      end;
      S := S + WordToHex ( ID );
      Inc ( P );
    end;
    S := S + '> Tj';
    AppendAction ( S );
    if CalcWidth then
    begin
      Width := Width * FCurrentFontSize / 1000;
      if FHorizontalScaling <> 100 then
        Width := Width * FHorizontalScaling / 100;
      if FWordSpace > 0 then
        Width := Width + SpaceCount * FWordSpace;
      if FCharSpace > 0 then
        Width := Width + CharCount * FCharSpace;
    end;
  end
  else
  begin
    A := 0;
    SetWordSpacing ( 0 );
    TextArray := PWordArray ( Text );
    SizeArray := PSingleArray ( Dx );
    while ( True ) do
    begin
      RealSize := 0;
      CalculateSize := 0;
      K := 0;
      OutString := '';
      while ( True ) do
      begin
        if A >= Len then
        begin
          if k > 0 then
            SetIntCharacterSpacing ( ( CalculateSize - RealSize ) / k );
          Break;
        end;
        ID := TextArray^ [ A ];
        Node := FCurrentFont.TTFInfo.GetNodeByID ( ID );
        Node^.Used := True;
        CharWidth := Node^.Width / 1000;
        OutString := OutString + WordToHex ( ID );
        if FHorizontalScaling <> 100 then
          CharWidth := CharWidth * FHorizontalScaling / 100;
        if abs ( CharWidth - SizeArray^ [ A ] ) > 0.1 then
        begin
          if k > 0 then
            SetIntCharacterSpacing ( ( CalculateSize - RealSize ) / k );
          CalculateSize := CalculateSize + SizeArray^ [ A ];
          inc ( A );
          Inc ( k );
          break;
        end;
        RealSize := RealSize + CharWidth;
        CalculateSize := CalculateSize + SizeArray^ [ A ];
        inc ( A );
        Inc ( k );
      end;
      if K <> 0 then
      begin
        AppendAction ( '<' + OutString + '> Tj' );
      end;
      if A < Len then
        AppendAction ( FormatFloat ( CalculateSize ) + ' 0 Td' )
      else
        Break;
    end;
    if ( ( not ( fsUnderLine in FCurrentFontStyle ) ) and ( not ( fsStrikeOut in FCurrentFontStyle ) ) ) then
      Exit;
    for I := 0 to Len - 1 do
      Width := Width + SizeArray^ [ i ];
    Width := Width * FCurrentFontSize;
  end;
  if CalcWidth then
  begin
    PaintTextLines ( Width );
  end;
end;

procedure TPDFPage.RawExtGlyphTextOut ( X, Y, Orientation: Extended;
  Text: PWord; Len: Integer; DX: PExt );
begin
  RawSetTextPosition ( X, Y, Orientation );
  ExtGlyphTextShow ( Text, Len, Dx );
end;


procedure TPDFPage.PaintTextLines ( Width: Extended );
begin
  if ( ( not ( fsUnderLine in FCurrentFontStyle ) ) and ( not ( fsStrikeOut in FCurrentFontStyle ) ) ) then
    Exit;
  if fsUnderLine in FCurrentFontStyle then
  begin
    if FRealAngle <> 0 then
      RawRectRotated ( TP.x + 3 * sin ( ( PI / 180 ) * FRealAngle ), TP.y - 3 * cos ( FRealAngle * ( PI / 180 ) ),
        Width, -FCurrentFontSize * 0.05, FRealAngle )
    else
      RawRect ( TP.x, TP.y - FCurrentFontSize * 0.05, Width, -FCurrentFontSize * 0.05 );
  end;
  if fsStrikeOut in FCurrentFontStyle then
  begin
    if FRealAngle <> 0 then
      RawRectRotated ( TP.x - FCurrentFontSize / 4 * sin ( ( PI / 180 ) * FRealAngle ), TP.y + FCurrentFontSize / 4 * cos ( FRealAngle * ( PI / 180 ) ),
        Width, FCurrentFontSize * 0.05, FRealAngle )
    else
      RawRect ( TP.x, TP.y + FCurrentFontSize / 4, Width, FCurrentFontSize * 0.05 );
  end;
  case FRender of
    0: Fill;
    1: Stroke;
    2: FillAndStroke;
  else
    Fill;
  end;
end;

{ TPDFPages }

function TPDFPages.Add: TPDFPage;
begin
  Result := TPDFPage.Create ( FOwner );
  FPages.Add ( Pointer ( Result ) );
  Result.FIsForm := IsWaterMarks;
end;

procedure TPDFPages.Clear;
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do
    TPDFPage ( FPages [ I ] ).Free;
  FPages.Clear;
end;

constructor TPDFPages.Create ( AOwner: TPDFDocument; WM: Boolean );
begin
  FOwner := AOwner;
  FPages := TList.Create;
  IsWaterMarks := WM;
end;

procedure TPDFPages.Delete ( Page: TPDFPage );
var
  I: Integer;
begin
  I := IndexOf ( Page );
  if I <> -1 then
  begin
    Page.Free;
    FPages.Delete ( I );
  end;
end;

destructor TPDFPages.Destroy;
begin
  Clear;
  FPages.Free;
end;

function TPDFPages.GetCount: Integer;
begin
  Result := FPages.Count;
end;

function TPDFPages.GetPage ( Index: Integer ): TPDFPage;
begin
  Result := TPDFPage ( FPages [ Index ] );
end;

function TPDFPages.IndexOf ( Page: TPDFPage ): Integer;
begin
  Result := FPages.IndexOf ( Pointer ( Page ) );
end;

{ TPDFFonts }

procedure TPDFFonts.Clear;
var
  I: Integer;
begin
  for I := 0 to FFonts.Count - 1 do
    TPDFFont ( FFonts [ I ] ).Free;
  FFonts.Clear;
end;

constructor TPDFFonts.Create ( AOwner: TPDFDocument );
begin
  FFonts := TList.Create;
  FOwner := AOwner;
end;


destructor TPDFFonts.Destroy;
begin
  Clear;
  FFonts.Free;
  inherited;
end;


function TPDFFonts.GetCount: Integer;
begin
  Result := FFonts.Count;
end;

function TPDFFonts.GetFont ( Index: Integer ): TPDFFont;
begin
  Result := TPDFFont ( FFonts [ Index ] );
end;


function TPDFFonts.GetFontByInfo ( FontName: TFontName; FontStyle: TFontStyles;
  IsUnicode: Boolean ): TPDFFont;
var
  FA: string;
  I: Integer;
  ID: Byte;
  FS: TFontStyles;
  K: Integer;
  FND: Boolean;
  FN: string;
begin
  ID := 255;
  FA := UpperCase ( FontName );
  FontStyle := FontStyle - [ fsUnderLine, fsStrikeOut ];
// Check to standard Adobe fonts
  if not IsUnicode then
    if FA = 'HELVETICA' then
      if ( fsBold in FontStyle ) and ( fsItalic in FontStyle ) then
        ID := 3
      else if fsBold in FontStyle then
        ID := 1
      else if fsItalic in FontStyle then
        ID := 2
      else
        ID := 0
    else if FA = 'TIMES' then
      if ( fsBold in FontStyle ) and ( fsItalic in FontStyle ) then
        ID := 7
      else if fsBold in FontStyle then
        ID := 5
      else if fsItalic in FontStyle then
        ID := 6
      else
        ID := 4
    else if FA = 'COURIER' then
      if ( fsBold in FontStyle ) and ( fsItalic in FontStyle ) then
        ID := 11
      else if fsBold in FontStyle then
        ID := 9
      else if fsItalic in FontStyle then
        ID := 10
      else
        ID := 8
    else if FA = 'SYMBOL' then
      ID := 12
    else if FA = 'ZAPFDINGBATS' then
      ID := 13;

  if ID <> 255 then
  begin
    for I := 0 to FFonts.Count - 1 do
      if ( TPDFFont ( FFonts [ I ] ).Standart ) and ( TPDFFont ( FFonts [ I ] ).StdID = ID ) then
      begin
        Result := TPDFFont ( FFonts [ I ] );
        Exit;
      end;
    Result := TPDFFont.Create;
    Result.Standart := True;
    Result.StdID := ID;
    Result.AliasName := 'F' + IntToStr ( ID );
    FFonts.Add ( Result );
//    Result.FontID := FOwner.GetNextID;
    Exit;
  end;
// Check to emulation by standard fonts
  if FOwner.EmulateStandardFont and ( not IsUnicode ) then
    if FA = 'ARIAL' then
    begin
      Result := GetFontByInfo ( 'Helvetica', FontStyle, False );
      Exit;
    end
    else if FA = 'TIMES NEW ROMAN' then
    begin
      Result := GetFontByInfo ( 'Times', FontStyle, False );
      Exit;
    end
    else if FA = 'COURIER NEW' then
    begin
      Result := GetFontByInfo ( 'Courier', FontStyle, False );
      Exit;
    end;
  for I := 0 to FFonts.Count - 1 do
    if ( not TPDFFont ( FFonts [ i ] ).Standart ) and ( UpperCase ( TPDFFont ( FFonts [ i ] ).FontName ) = FA ) and
      ( TPDFFont ( FFonts [ i ] ).FontStyle = FontStyle ) and ( TPDFFont ( FFonts [ i ] ).Unicode = IsUnicode ) then
    begin
      Result := TPDFFont ( FFonts [ i ] );
      Exit;
    end;
  FS := FontStyle;
  FN := FontName;
  if not FontTest ( FN, FS ) then
  begin
    if IsUnicode then
      Result := GetFontByInfo ( 'Arial', FontStyle, True )
    else
      Result := GetFontByInfo ( 'Helvetica', FontStyle, False );
    Exit;
  end;
  if FN <> FontName then
    FontName := FN;
  if FS <> FontStyle then
    for I := 0 to FFonts.Count - 1 do
      if ( not TPDFFont ( FFonts [ i ] ).Standart ) and ( UpperCase ( TPDFFont ( FFonts [ i ] ).FontName ) = FA ) and
        ( TPDFFont ( FFonts [ i ] ).FontStyle = FS ) and ( TPDFFont ( FFonts [ i ] ).Unicode = IsUnicode ) then
      begin
        Result := TPDFFont ( FFonts [ i ] );
        Exit;
      end;
  Result := TPDFFont.Create;
  Result.Standart := False;
  Result.FUnicode := IsUnicode;
  Result.FFontName := FontName;
  Result.FFontStyle := FS;
  if FA = 'WINGDINGS' then
    Result.IsWing := True;
//  Result.FontID := FOwner.GetNextID;
  FND := False;
  for I := 0 to FOwner.NonEmbeddedFont.Count - 1 do
    if UpperCase ( FOwner.NonEmbeddedFont [ I ] ) = FA then
    begin
      FND := True;
      Result.FEbd := False;
      Break;
    end;
  if not FND then
    Result.FEBD := True;
  Result.FillInfo;
  FFonts.Add ( Result );
  k := 0;
  for i := 0 to FFonts.Count - 1 do
    if not TPDFFont ( FFonts [ i ] ).Standart then
      Inc ( k );
  Result.AliasName := 'TT' + IntToStr ( k );

end;



{ TPDFImages }

function TPDFImages.Add ( Image: TGraphic; Compression: TImageCompressionType; MaskIndex: Integer = -1; IsMask: Boolean = False; TransparentColor: TColor = -1 ): TPDFImage;
begin
  Result := TPDFImage.Create ( Image, Compression, FOwner, MaskIndex, IsMask, TransparentColor );
  FImages.Add ( Pointer ( Result ) );
  Result.ImageName := 'Im' + IntToStr ( Count - 1 );
end;

function TPDFImages.Add ( FileName: TFileName; Compression: TImageCompressionType; MaskIndex: Integer = -1; IsMask: Boolean = False; TransparentColor: TColor = -1 ): TPDFImage;
begin
  Result := TPDFImage.Create ( FileName, Compression, FOwner, MaskIndex, IsMask, TransparentColor );
  FImages.Add ( Pointer ( Result ) );
  Result.ImageName := 'Im' + IntToStr ( Count - 1 );
end;

procedure TPDFImages.Clear;
var
  I: Integer;
begin
  for I := 0 to FImages.Count - 1 do
    TPDFImage ( FImages [ I ] ).Free;
  FImages.Clear;
end;

constructor TPDFImages.Create ( AOwner: TPDFDocument );
begin
  FOwner := AOwner;
  FImages := TList.Create;
end;

procedure TPDFImages.Delete ( Index: Integer );
begin
  TPDFImage ( FImages [ Index ] ).Free;
  FImages.Delete ( Index );
end;

destructor TPDFImages.Destroy;
begin
  Clear;
  FImages.Free;
  inherited;
end;

function TPDFImages.GetCount: Integer;
begin
  Result := FImages.Count;
end;

function TPDFImages.GetImage ( Index: Integer ): TPDFImage;
begin
  Result := TPDFImage ( FImages [ Index ] );
end;

{ TPDFFont }

constructor TPDFFont.Create;
begin
  WidthLoaded := False;
  FontID := -1;
  FWidthArray := nil;
  FLastChar := 0;
  FFirstChar := 255;
  FFontUsed := False;
  TTFInfo := nil;
  IsWing := False;
end;

destructor TPDFFont.Destroy;
begin
  if TTFInfo <> nil then
    TTFInfo.Free;
  if FWidthArray <> nil then
    Dispose ( FWidthArray );
  inherited;
end;

procedure TPDFFont.FillInfo;
type
  TABCANSIArray = array [ 0..255 ] of ABC;
  PABCANSIArray = ^TABCANSIArray;
var
  I: Integer;
  DC: HDC;
  LF: TLogFont;
  obj: THandle;
  P: PABCANSIArray;
  M: TTextMetric;
begin
  DC := CreateCompatibleDC ( 0 );
  try
    FillChar ( LF, SizeOf ( LF ), 0 );
    with LF do
    begin
      lfHeight := -1000;
      lfWidth := 0;
      lfEscapement := 0;
      lfOrientation := 0;
      if fsBold in FontStyle then
        lfWeight := FW_BOLD
      else
        lfWeight := FW_NORMAL;
      lfItalic := Byte ( fsItalic in FontStyle );
      lfCharSet := ANSI_CHARSET;
      StrPCopy ( lfFaceName, FontName );
      lfQuality := DEFAULT_QUALITY;
      lfOutPrecision := OUT_DEFAULT_PRECIS;
      lfClipPrecision := CLIP_DEFAULT_PRECIS;
      lfPitchAndFamily := DEFAULT_PITCH;
      obj := CreateFontIndirect ( LF );
    end;
    SelectObject ( DC, Obj );
    GetTextMetrics ( DC, M );
    FDescent := M.tmDescent;
    FAbscent := M.tmAscent;
    FMono := ( M.tmPitchAndFamily and TMPF_FIXED_PITCH ) = 0;
    FillChar ( OTM, SizeOf ( OTM ), 0 );
    OTM.otmSize := SizeOf ( OTM );
    GetOutlineTextMetrics ( DC, SizeOf ( OTM ), @OTM );
    if not Unicode then
    begin
      New ( FWidthArray );
      New ( P );
      try
        GetCharABCWidths ( DC, 0, 255, p^ );
        for I := 0 to 255 do
          if not Fmono then
            FWidthArray^ [ I ] := P^ [ I ].abcA + Integer ( P^ [ I ].abcB ) + P^ [ I ].abcC
          else
            FWidthArray^ [ I ] := P^ [ 0 ].abcA + Integer ( P^ [ 0 ].abcB ) + P^ [ 0 ].abcC;
      finally
        Dispose ( P );
      end;
    end
    else
    begin
      TTFInfo := TTTFData.Create ( FontName, FontStyle, FEbd );
    end;
    DeleteObject ( obj );
  finally
    DeleteDC ( DC );
  end;
end;

procedure TPDFFont.FillUsed ( st: string );
var
  I: Integer;
  B: Byte;
begin
  if Standart or Unicode then
    Exit;
  for I := 1 to Length ( st ) do
  begin
    b := Ord ( st [ i ] );
    FUsed [ B ] := True;
    if B > FLastChar then
      FLastChar := B;
    if B < FFirstChar then
      FFirstChar := B;
  end;
end;

function TPDFFont.GetAbscent: Integer;
begin
  if Standart then
    Result := StdFontAbscent [ StdID ]
  else
    Result := FAbscent;
end;

function TPDFFont.GetDescent: Integer;
begin
  if Standart then
    Result := StdFontDescent [ StdID ]
  else
    Result := FDescent;
end;

function TPDFFont.GetWidth ( Index: Integer ): Word;
begin
  if Standart then
  begin
    Result := stWidth [ StdID, Index ];
    Exit;
  end;
  Result := FWidthArray^ [ Index ];
end;



procedure TPDFFont.Save ( Doc: TPDFDocument );
var
  I, L, K: Integer;
  Wid: string;
  MS, MS1: TMemoryStream;
  CS: TCompressionStream;
  RS: Integer;
  FFN: string;
  SUBSTR: string;
  IDA, UnicodeID: Integer;
  FontDescriptorID: Integer;
  FontFileID: Integer;
  USD: array of Word;
  Len: DWORD;
  DC: HDC;
  LF: TLogFont;
  obj: THandle;
begin
  if not FFontUsed then
    Exit;
  if not Standart then
  begin
    FontDescriptorID := -1;
    FontFileID := -1;
    if FEbd and ( not Unicode ) then
    begin
      USD := nil;
      L := 0;
      SUBSTR := '';
      for I := FFirstChar to FLastChar do
        if FUsed [ I ] then
        begin
          K := Length ( USD );
          SetLength ( USD, K + 1 );
          USD [ K ] := ByteToANSIUNICODE ( I );
          Inc ( L );
        end;
      if L <> 0 then
      begin
        MS := TMemoryStream.Create;
        try
          QuickSortArray ( USD );
          GetFontSubset ( FontName, FontStyle, MS, @USD [ 0 ], L );
          USD := nil;
          RS := MS.Size;
          MS.Position := 0;
          MS1 := TMemoryStream.Create;
          try
            CS := TCompressionStream.Create ( clMax, MS1 );
            try
              CS.CopyFrom ( MS, MS.Size );
            finally
              CS.Free;
            end;
            MS.Clear;
            Doc.StartObj ( FontFileID );
            Doc.SaveToStream ( '/Filter /FlateDecode /Length ' + IntToStr ( MS1.Size ) + ' /Length1 ' + IntToStr ( RS ) );
            Doc.CloseHObj;
            Doc.StartStream;
            ms1.Position := 0;
            EnCodeStream ( Doc.FProtectionEnabled, Doc.FKey, Doc.FPKL, FontFileID, MS1 );
            Doc.FStream.CopyFrom ( MS1, MS1.Size );
            Doc.SaveToStream ( '' );
            Doc.CloseStream;
            Doc.CloseObj;
          finally
            MS1.Free;
          end;
        finally
          MS.Free;
        end;
      end;
    end;
    if Unicode and FEbd then
    begin
      MS := TMemoryStream.Create;
      try
        if pos ( '.TMP', UpperCase( FontName ) ) = 0 then
        begin
          TTFInfo.GetFontSubset ( MS );
        end else
        begin
          DC := CreateCompatibleDC ( 0 );
          try
            FillChar ( LF, SizeOf ( LF ), 0 );
            with LF do
            begin
              lfHeight := -1000;
              lfWidth := 0;
              lfEscapement := 0;
              lfOrientation := 0;
              if fsBold in FontStyle then
                lfWeight := FW_BOLD
              else
                lfWeight := FW_NORMAL;
              lfItalic := Byte ( fsItalic in FontStyle );
              lfCharSet := DEFAULT_CHARSET;
              StrPCopy ( lfFaceName, FontName );
              lfQuality := DEFAULT_QUALITY;
              lfOutPrecision := OUT_DEFAULT_PRECIS;
              lfClipPrecision := CLIP_DEFAULT_PRECIS;
              lfPitchAndFamily := DEFAULT_PITCH;
              obj := CreateFontIndirect ( LF );
            end;
            SelectObject ( DC, Obj );
            Len := GetFontData( DC,0,0, nil,0);
            MS.SetSize( Len );
            GetFontData ( DC, 0,0, MS.Memory, Len );
            DeleteObject ( obj );
          finally
            DeleteDC ( DC );
          end;
        end;
        RS := MS.Size;
        MS.Position := 0;
        MS1 := TMemoryStream.Create;
        try
          CS := TCompressionStream.Create ( clMax, MS1 );
          try
            CS.CopyFrom ( MS, MS.Size );
          finally
            CS.Free;
          end;
          MS.Clear;
          Doc.StartObj ( FontFileID );
          Doc.SaveToStream ( '/Filter /FlateDecode /Length ' + IntToStr ( MS1.Size ) + ' /Length1 ' + IntToStr ( RS ) );
          Doc.CloseHObj;
          Doc.StartStream;
          ms1.Position := 0;
          EnCodeStream ( Doc.FProtectionEnabled, Doc.FKey, Doc.FPKL, FontFileID, MS1 );
          Doc.FStream.CopyFrom ( MS1, MS1.Size );
          Doc.SaveToStream ( '' );
          Doc.CloseStream;
          Doc.CloseObj;
        finally
          MS1.Free;
        end;
      finally
        MS.Free;
      end;
    end;
    if not Unicode then
    begin
      Wid := '';
      for I := FFirstChar to FLastChar do
      begin
        if FUsed [ I ] then
          Wid := Wid + IntToStr ( FWidthArray^ [ I ] ) + ' '
        else
          Wid := Wid + '0 ';
        if I mod 16 = 0 then
          Wid := Wid + #13#10;
      end;
      FFN := ReplStr ( FFontName, ' ', '#20' );
      if fsBold in FFontStyle then
        FFN := FFN + '#20Bold';
      if fsItalic in FFontStyle then
        FFN := FFN + '#20Italic';
      Doc.StartObj ( FontDescriptorID );
      Doc.SaveToStream ( '/Type /FontDescriptor' );
      Doc.SaveToStream ( '/Ascent ' + IntToStr ( OTM.otmAscent ) );
      Doc.SaveToStream ( '/CapHeight 666' );
      Doc.SaveToStream ( '/Descent ' + IntToStr ( OTM.otmDescent ) );
      Doc.SaveToStream ( '/Flags 32' );
      Doc.SaveToStream ( '/FontBBox [' + IntToStr ( OTM.otmrcFontBox.Left ) + ' ' + IntToStr ( OTM.otmrcFontBox.Bottom ) + ' ' + IntToStr ( OTM.otmrcFontBox.Right ) + ' ' + IntToStr ( OTM.otmrcFontBox.Top ) + ']' );
      Doc.SaveToStream ( '/FontName /' + FFN );
      Doc.SaveToStream ( '/ItalicAngle ' + IntToStr ( OTM.otmItalicAngle ) );
      Doc.SaveToStream ( '/StemV 87' );
      if FontFileID <> -1 then
        Doc.SaveToStream ( '/FontFile2 ' + IntToStr ( FontFileID ) + ' 0 R' );
      Doc.CloseHObj;
      Doc.CloseObj;
      Doc.StartObj ( FontID );
      Doc.SaveToStream ( '/Type /Font' );
      Doc.SaveToStream ( '/Subtype /TrueType' );
      Doc.SaveToStream ( '/BaseFont /' + FFN );
      Doc.SaveToStream ( '/FirstChar ' + IntToStr ( FFirstChar ) );
      Doc.SaveToStream ( '/LastChar ' + IntToStr ( FLastChar ) );
      if not IsWing then
        Doc.SaveToStream ( '/Encoding /WinAnsiEncoding' )
      else
        Doc.SaveToStream ( '/Encoding /MacRomanEncoding' );
      Doc.SaveToStream ( '/FontDescriptor ' + IntToStr ( FontDescriptorID ) + ' 0 R' );
      Doc.SaveToStream ( '/Widths [' + Wid + ']' );
      Doc.CloseHObj;
      Doc.CloseObj;
    end
    else
    begin
      Randomize;
      SUBSTR := '';
      IDA := -1;
      UnicodeID := -1;
      for I := 1 to 6 do
        SUBSTR := SUBSTR + Chr ( Random ( 25 ) + 65 );
      FFN := ReplStr ( FFontName, ' ', '#20' );
      if fsBold in FFontStyle then
        FFN := FFN + '#20Bold';
      if fsItalic in FFontStyle then
        FFN := FFN + '#20Italic';
   //   FFN := SUBSTR +'+'+ FFN;
      Doc.StartObj ( FontDescriptorID );
      Doc.SaveToStream ( '/Type /FontDescriptor' );
      Doc.SaveToStream ( '/Ascent ' + IntToStr ( OTM.otmAscent ) );
      Doc.SaveToStream ( '/CapHeight 666' );
      Doc.SaveToStream ( '/Descent ' + IntToStr ( OTM.otmDescent ) );
      Doc.SaveToStream ( '/Flags 32' );
      Doc.SaveToStream ( '/FontBBox [' + IntToStr ( OTM.otmrcFontBox.Left ) + ' ' + IntToStr ( OTM.otmrcFontBox.Bottom ) + ' ' + IntToStr ( OTM.otmrcFontBox.Right ) + ' ' + IntToStr ( OTM.otmrcFontBox.Top ) + ']' );
      Doc.SaveToStream ( '/FontName /' + FFN );
      Doc.SaveToStream ( '/ItalicAngle ' + IntToStr ( OTM.otmItalicAngle ) );
      Doc.SaveToStream ( '/StemV 87' );
      if FontFileID <> -1 then
        Doc.SaveToStream ( '/FontFile2 ' + IntToStr ( FontFileID ) + ' 0 R' );
      Doc.CloseHObj;
      Doc.CloseObj;
      Doc.StartObj ( IDA );
      Doc.SaveToStream ( '/Type /Font' );
      Doc.SaveToStream ( '/BaseFont /' + FFN );
      Doc.SaveToStream ( '/Subtype /CIDFontType2' );
      Doc.SaveToStream ( '/CIDSystemInfo <</Ordering(Identity)/Registry(Adobe)/Supplement 0>>' );
      Doc.SaveToStream ( '/DW 1000' );
      Doc.SaveToStream ( '/W' + TTFInfo.GetWideString );
      Doc.SaveToStream ( '/FontDescriptor ' + IntToStr ( FontDescriptorID ) + ' 0 R' );
      Doc.CloseHObj;
      Doc.CloseObj;
      Doc.StartObj ( UnicodeID );
      MS1 := TMemoryStream.Create;
      try
        CS := TCompressionStream.Create ( clDefault, MS1 );
        try
          TTFInfo.GetToUnicodeStream ( AliasName, CS );
        finally
          CS.Free;
        end;
        Doc.SaveToStream ( '/Filter /FlateDecode /Length ' + IntToStr ( MS1.Size ) );
        Doc.CloseHObj;
        Doc.StartStream;
        ms1.Position := 0;
        EnCodeStream ( Doc.FProtectionEnabled, Doc.FKey, Doc.FPKL, UnicodeID, MS1 );
        Doc.FStream.CopyFrom ( MS1, MS1.Size );
        Doc.SaveToStream ( '' );
        Doc.CloseStream;
      finally
        MS1.Free;
      end;
      Doc.CloseObj;
      Doc.StartObj ( FontID );
      Doc.SaveToStream ( '/Type /Font' );
      Doc.SaveToStream ( '/Subtype /Type0' );
      Doc.SaveToStream ( '/BaseFont /' + FFN );
      Doc.SaveToStream ( '/Encoding /Identity-H' );
      Doc.SaveToStream ( '/DescendantFonts [' + IntToStr ( IDA ) + ' 0 R]' );
      Doc.SaveToStream ( '/ToUnicode ' + IntToStr ( UnicodeID ) + ' 0 R' );
      Doc.CloseHObj;
      Doc.CloseObj;
    end;
  end
  else
  begin
    Doc.StartObj ( FontID );
    Doc.SaveToStream ( '/Type /Font' );
    Doc.SaveToStream ( '/Subtype /Type1' );
    Doc.SaveToStream ( '/BaseFont /' + PDFStandardFontNames [ StdID ] );
    if StdID < 12 then
      Doc.SaveToStream ( '/Encoding /WinAnsiEncoding' );
    Doc.SaveToStream ( '/FirstChar 32' );
    Doc.SaveToStream ( '/LastChar 255' );
    Doc.CloseHObj;
    Doc.CloseObj;
  end;
end;

procedure TPDFFont.SetAllANSI;
var
  I: Integer;
begin
  FFirstChar := 32;
  FLastChar := 255;
  for I := 32 to 255 do
    FUsed [ i ] := True;
end;

procedure TPDFFont.UsedChar ( Ch: Byte );
begin
  if Standart or Unicode then
    Exit;
  FUsed [ Ch ] := True;
  if Ch > FLastChar then
    FLastChar := Ch;
  if Ch < FFirstChar then
    FFirstChar := Ch;
end;

{ TPDFImage }

constructor TPDFImage.Create ( Image: TGraphic;
  Compression: TImageCompressionType; Doc: TPDFDocument;
  MaskIndex: Integer = -1; IsMask: Boolean = False; TransparentColor: TColor = -1 );
var
  J: TJPEGImage;
  B: TBitmap;
  CS: TCompressionStream;
  pb: PByteArray;
  bb: Byte;
  x, y: Integer;
  DIB: DIBSECTION;
begin
  PictureID := -1;
  if ( Image is TBitmap ) and ( TBitmap ( Image ).PixelFormat = pf1bit ) and ( Compression <> itcJpeg )
    then
  begin
    pb := TBitmap ( Image ).ScanLine [ 0 ];
    bb := pb [ 0 ] shr 7;
    BWInvert := iis ( TBitmap ( Image ).Canvas.Pixels [ 0, 0 ] > 0, 1, 0 ) <> bb;
  end
  else
    BWInvert := False;
  if MaskIndex >= 0 then
  begin
    if IsMask then
      raise TPDFException.Create ( SMaskImageCannotHaveMask );
    if ( MaskIndex >= Doc.FImages.GetCount ) then
      raise TPDFException.Create ( SUnknowMaskImageOutOfBound );
    if not TPDFImage ( Doc.FImages.FImages [ MaskIndex ] ).FIsMask then
      raise TPDFException.Create ( SMaskImageNotMarkedAsMaskImage );
  end;
  if IsMask and ( not ( Image is TBitmap ) ) then
    raise TPDFException.Create ( SCreateMaskAvailableOnlyForBitmapImages );
  FMaskIndex := MaskIndex;
  FIsMask := IsMask;
  Saved := False;
  GrayScale := False;
  FCT := Compression;
  FDoc := Doc;
  FWidth := Image.Width;
  FHeight := Image.Height;
  if not ( ( Image is TJPEGImage ) or ( Image is TBitmap ) ) then
    raise TPDFException.Create ( SNotValidImage );
  Data := TMemoryStream.Create;
  if not IsMask then
  begin
    if FCT = ITCjpeg then
    begin
      J := TJPEGImage.Create;
      try
        J.Assign ( Image );
        J.ProgressiveEncoding := False;
        if Doc <> nil then
          J.CompressionQuality := Doc.FJPEGQuality
        else
          J.CompressionQuality := 80;
        J.SaveToStream ( Data );
        Data.Position := 0;
        if J.Grayscale then
          Grayscale := True;
        FBitPerPixel := 8;
      finally
        J.Free;
      end;
    end;
    if FCT = itcFlate then
    begin
      B := TBitmap.Create;
      try
        B.Assign ( Image );
        b.PixelFormat := pf24bit;
        CS := TCompressionStream.Create ( clDefault, Data );
        try
          for y := 0 to B.Height - 1 do
          begin
            pb := B.ScanLine [ y ];
            x := 0;
            while x <= ( b.Width - 1 ) * 3 do
            begin
              bb := pb [ x ];
              pb [ x ] := pb [ x + 2 ];
              pb [ x + 2 ] := bb;
              x := x + 3;
            end;
            CS.Write ( pb^, b.Width * 3 );
          end;
        finally
          CS.Free;
        end;
        FBitPerPixel := 8;
        Data.Position := 0;
      finally
        B.Free;
      end;
    end;
    if FCT in [ itcCCITT3, itcCCITT32d, itcCCITT4 ] then
    begin
      if not ( Image is TBitmap ) then
      begin
        Data.Free;
        raise TPDFException.Create ( SCCITTCompressionWorkOnlyForBitmap );
      end;
      B := TBitmap ( Image );
      if B.PixelFormat <> pf1bit then
      begin
        if B.PixelFormat = pfDevice then
        begin
          DIB.dsBmih.biSize := 0;
          GetObject( B.Handle, sizeof (DIB), @DIB);
          if DIB.dsBm.bmBitsPixel = 1 then
          begin
            B.PixelFormat := pf1bit;
          end else
          begin
            Data.Free;
            raise Exception.Create ( SCannotCompressNotMonochromeImageViaCCITT );
          end;
        end else
        begin
          Data.Free;
          raise Exception.Create ( SCannotCompressNotMonochromeImageViaCCITT );
        end;
      end;
      FBitPerPixel := 1;
      case FCT of
        itcCCITT3: SaveBMPtoCCITT ( B, Data, CCITT31D );
        itcCCITT32d: SaveBMPtoCCITT ( B, Data, CCITT32D );
        itcCCITT4: SaveBMPtoCCITT ( B, Data, CCITT42D );
      end;
      Data.Position := 0;
    end;
  end
  else
  begin
    B := TBitmap.Create;
    try
      B.Assign ( Image );
      B.PixelFormat := pf24bit;
      if TransparentColor = -1 then
        TransparentColor := B.TransparentColor;
      B.Mask ( TransparentColor );
      GrayScale := True;
      FCT := itcCCITT4;
      FBitPerPixel := 1;
      B.Monochrome := True;
      B.PixelFormat := pf1bit;
      SaveBMPtoCCITT ( B, Data, CCITT42D );
      Data.Position := 0;
    finally
      B.Free;
    end;
  end;
end;

constructor TPDFImage.Create ( FileName: TFileName;
  Compression: TImageCompressionType; Doc: TPDFDocument; MaskIndex: Integer = -1;
  IsMask: Boolean = False; TransparentColor: TColor = -1 );
var
  Gr: TGraphic;
  MS: TMemoryStream;
  BMSig: Word;
begin
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile ( FileName );
    MS.Position := 0;
    MS.Read ( BMSig, 2 );
    MS.Position := 0;
    if BMSig = 19778 then
      GR := TBitmap.Create
    else
      GR := TJPEGImage.Create;
    try
      Gr.LoadFromStream ( MS );
      Create ( Gr, Compression, Doc, MaskIndex, IsMask, TransparentColor );
    finally
      Gr.Free;
    end;
  finally
    MS.Free;
  end;
end;

destructor TPDFImage.Destroy;
begin
  Data.Free;
  inherited;
end;

procedure TPDFImage.Save ( Doc: TPDFDocument );
begin
  if Saved then
    Exit;
  if FMaskIndex <> -1 then
    TPDFImage ( FDoc.FImages.FImages [ FMaskIndex ] ).Save ( Doc );
  Doc.StartObj ( PictureID );
  Doc.SaveToStream ( '/Type /XObject' );
  Doc.SaveToStream ( '/Subtype /Image' );
  if ( FBitPerPixel <> 1 ) and ( not GrayScale ) then
    Doc.SaveToStream ( '/ColorSpace /DeviceRGB' )
  else
    Doc.SaveToStream ( '/ColorSpace /DeviceGray' );
  Doc.SaveToStream ( '/BitsPerComponent ' + IntToStr ( BitPerPixel ) );
  Doc.SaveToStream ( '/Width ' + IntToStr ( FWidth ) );
  Doc.SaveToStream ( '/Height ' + IntToStr ( FHeight ) );
  if FIsMask then
    Doc.SaveToStream ( '/ImageMask true' );
  if FMaskIndex <> -1 then
    Doc.SaveToStream ( '/Mask ' + IntToStr ( TPDFImage ( FDoc.FImages.FImages [ FMaskIndex ] ).PictureID ) + ' 0 R' );
  case FCT of
    itcJpeg: Doc.SaveToStream ( '/Filter /DCTDecode' );
    itcFlate: Doc.SaveToStream ( '/Filter /FlateDecode' );
  else
    Doc.SaveToStream ( '/Filter [/CCITTFaxDecode]' );
  end;
  Doc.SaveToStream ( '/Length ' + IntToStr ( Data.Size ) );
  case FCT of
    itcCCITT3: Doc.SaveToStream ( '/DecodeParms [<</K 0 /Columns ' + Inttostr ( FWidth ) + ' /Rows ' + IntToStr ( FHeight ) + iis ( BWInvert, '/BlackIs1 true', '' ) + '>>]' );
    itcCCITT32d: Doc.SaveToStream ( '/DecodeParms [<</K 1 /Columns ' + Inttostr ( FWidth ) + ' /Rows ' + IntToStr ( FHeight ) + iis ( BWInvert, '/BlackIs1 true', '' ) + '>>]' );
    itcCCITT4: Doc.SaveToStream ( '/DecodeParms [<</K -1 /Columns ' + Inttostr ( FWidth ) + ' /Rows ' + IntToStr ( FHeight ) + iis ( BWInvert, '/BlackIs1 true', '' ) + '>>]' );
  end;
  Doc.CloseHObj;
  Doc.StartStream;
  Data.Position := 0;
  EnCodeStream ( Doc.FProtectionEnabled, Doc.FKey, Doc.FPKL, PictureID, Data );
  Doc.FStream.CopyFrom ( Data, Data.Size );
  Data.Clear;
  Doc.CloseStream;
  Doc.CloseObj;
  Saved := True;
end;

{ TPDFOutlines }

function TPDFOutlines.Add ( Node: TPDFOutlineNode ): TPDFOutlineNode;
var
  N, T, M: TPDFOutlineNode;
  I: Integer;
begin
  N := TPDFOutlineNode.Create ( Self );
  if Node <> nil then
    T := Node.FParent
  else
    T := nil;
  N.FParent := T;
  N.FNext := nil;
  M := nil;
  for I := 0 to FList.Count - 1 do
    if ( TPDFOutlineNode ( FList [ I ] ).FParent = T ) and ( TPDFOutlineNode ( FList [ I ] ).FNext = nil ) then
    begin
      M := TPDFOutlineNode ( FList [ I ] );
      Break;
    end;
  if M <> nil then
    M.FNext := N;
  N.FPrev := M;
  FList.Add ( Pointer ( N ) );
  if T <> nil then
    T.FChild.Add ( Pointer ( N ) );
  Result := N;
end;

function TPDFOutlines.AddChild ( Node: TPDFOutlineNode ): TPDFOutlineNode;
var
  N, T, M: TPDFOutlineNode;
  I: Integer;
begin
  N := TPDFOutlineNode.Create ( Self );
  T := Node;
  N.FParent := T;
  N.FNext := nil;
  M := nil;
  for I := 0 to FList.Count - 1 do
    if ( TPDFOutlineNode ( FList [ I ] ).FParent = T ) and ( TPDFOutlineNode ( FList [ I ] ).FNext = nil ) then
    begin
      M := TPDFOutlineNode ( FList [ I ] );
      Break;
    end;
  if M <> nil then
    M.FNext := N;
  N.FPrev := M;
  FList.Add ( Pointer ( N ) );
  if T <> nil then
    T.FChild.Add ( Pointer ( N ) );
  Result := N;
end;

function TPDFOutlines.AddChildFirst (
  Node: TPDFOutlineNode ): TPDFOutlineNode;
var
  N, T, M: TPDFOutlineNode;
  I: Integer;
begin
  N := TPDFOutlineNode.Create ( Self );
  T := Node;
  N.FParent := T;
  N.FPrev := nil;
  M := nil;
  for I := 0 to FList.Count - 1 do
    if ( TPDFOutlineNode ( FList [ I ] ).FParent = T ) and ( TPDFOutlineNode ( FList [ I ] ).FPrev = nil ) then
    begin
      M := TPDFOutlineNode ( FList [ I ] );
      Break;
    end;
  if M <> nil then
    M.FPrev := N;
  N.FNext := M;
  FList.Add ( Pointer ( N ) );
  if T <> nil then
    T.FChild.Add ( Pointer ( N ) );
  Result := N;
end;

function TPDFOutlines.AddFirst ( Node: TPDFOutlineNode ): TPDFOutlineNode;
var
  N, T, M: TPDFOutlineNode;
  I: Integer;
begin
  N := TPDFOutlineNode.Create ( Self );
  if Node <> nil then
    T := Node.FParent
  else
    T := nil;
  N.FParent := T;
  N.FPrev := nil;
  M := nil;
  for I := 0 to FList.Count - 1 do
    if ( TPDFOutlineNode ( FList [ I ] ).FParent = T ) and ( TPDFOutlineNode ( FList [ I ] ).FPrev = nil ) then
    begin
      M := TPDFOutlineNode ( FList [ I ] );
      Break;
    end;
  if M <> nil then
    M.FPrev := N;
  N.FNext := M;
  FList.Add ( Pointer ( N ) );
  if T <> nil then
    T.FChild.Add ( Pointer ( N ) );
  Result := N;
end;


procedure TPDFOutlines.Clear;
begin
  while FList.Count <> 0 do
    TPDFOutlineNode ( FList [ 0 ] ).Delete;
end;

constructor TPDFOutlines.Create ( AOwner: TPDFDocument );
begin
  FList := TList.Create;
  FOwner := AOwner;
end;

procedure TPDFOutlines.Delete ( Node: TPDFOutlineNode );
begin
  Node.Delete;
end;

destructor TPDFOutlines.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TPDFOutlines.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TPDFOutlines.GetFirstNode: TPDFOutlineNode;
begin
  if FList.Count <> 0 then
    Result := TPDFOutlineNode ( FList [ 0 ] )
  else
    Result := nil;
end;

function TPDFOutlines.GetItem ( Index: Integer ): TPDFOutlineNode;
begin
  Result := TPDFOutlineNode ( FList [ Index ] );
end;

function TPDFOutlines.Insert ( Node: TPDFOutlineNode ): TPDFOutlineNode;
var
  N, Ne: TPDFOutlineNode;
begin
  if Node = nil then
  begin
    Result := Add ( nil );
    Exit;
  end;
  N := TPDFOutlineNode.Create ( Self );
  Ne := Node.FNext;
  N.FParent := Node.FParent;
  N.FPrev := Node;
  N.FNext := Node.FNext;
  Node.FNext := N;
  if Ne <> nil then
    Ne.FPrev := N;
  FList.Add ( Pointer ( N ) );
  if N.FParent <> nil then
    N.FParent.FChild.Add ( Pointer ( N ) );
  Result := N;
end;

function TPDFOutlines.Add ( Node: TPDFOutlineNode; Title: string; Action: TPDFAction; Charset: TFontCharset = ANSI_CHARSET ): TPDFOutlineNode;
begin
  Result := Add ( Node );
  Result.Title := Title;
  Result.Action := Action;
  Result.Charset := Charset;
end;

function TPDFOutlines.AddChild ( Node: TPDFOutlineNode; Title: string;
  Action: TPDFAction; Charset: TFontCharset ): TPDFOutlineNode;
begin
  Result := AddChild ( Node );
  Result.Title := Title;
  Result.Action := Action;
  Result.Charset := Charset;
end;

function TPDFOutlines.AddChildFirst ( Node: TPDFOutlineNode; Title: string;
  Action: TPDFAction; Charset: TFontCharset ): TPDFOutlineNode;
begin
  Result := AddChildFirst ( Node );
  Result.Title := Title;
  Result.Action := Action;
  Result.Charset := Charset;
end;

function TPDFOutlines.AddFirst ( Node: TPDFOutlineNode; Title: string;
  Action: TPDFAction; Charset: TFontCharset ): TPDFOutlineNode;
begin
  Result := Add ( Node );
  Result.Title := Title;
  Result.Action := Action;
  Result.Charset := Charset;
end;

function TPDFOutlines.Insert ( Node: TPDFOutlineNode; Title: string;
  Action: TPDFAction; Charset: TFontCharset ): TPDFOutlineNode;
begin
  Result := Insert ( Node );
  Result.Title := Title;
  Result.Action := Action;
  Result.Charset := Charset;
end;

{ TPDFOutlineNode }

constructor TPDFOutlineNode.Create ( AOwner: TPDFOutlines );
begin
  if AOwner = nil then
    raise TPDFException.Create ( SOutlineNodeMustHaveOwner );
  FOwner := AOwner;
  FChild := TList.Create;
end;

procedure TPDFOutlineNode.Delete;
var
  I: Integer;
  P, N: TPDFOutlineNode;
begin
  DeleteChildren;
  P := GetPrev;
  N := GetNext;
  if P <> nil then
    P.FNext := N;
  if N <> nil then
    N.FPrev := P;
  I := FOwner.FList.IndexOf ( Pointer ( Self ) );
  if I <> -1 then
    FOwner.FList.Delete ( I );
  if FParent <> nil then
  begin
    I := FParent.FChild.IndexOf ( Pointer ( Self ) );
    if I <> -1 then
      FParent.FChild.Delete ( I );
  end;
  Free;
end;

procedure TPDFOutlineNode.DeleteChildren;
begin
  while FChild.Count <> 0 do
    TPDFOutlineNode ( FChild [ 0 ] ).Delete;
end;

destructor TPDFOutlineNode.Destroy;
begin
  FChild.Free;
  inherited;
end;


function TPDFOutlineNode.GetCount: Integer;
begin
  Result := FChild.Count;
end;


function TPDFOutlineNode.GetFirstChild: TPDFOutlineNode;
var
  I: Integer;
begin
  Result := nil;
  if Count = 0 then
    Exit;
  for I := 0 to FChild.Count - 1 do
    if TPDFOutlineNode ( FChild [ I ] ).FPrev = nil then
    begin
      Result := TPDFOutlineNode ( FChild [ I ] );
      Exit;
    end;
end;

function TPDFOutlineNode.GetHasChildren: Boolean;
begin
  Result := Count <> 0;
end;

function TPDFOutlineNode.GetItem ( Index: Integer ): TPDFOutlineNode;
begin
  Result := TPDFOutlineNode ( FChild [ Index ] );
end;

function TPDFOutlineNode.GetLastChild: TPDFOutlineNode;
var
  I: Integer;
begin
  Result := nil;
  if Count = 0 then
    Exit;
  for I := 0 to FChild.Count - 1 do
    if TPDFOutlineNode ( FChild [ I ] ).FNext = nil then
    begin
      Result := TPDFOutlineNode ( FChild [ I ] );
      Exit;
    end;
end;

function TPDFOutlineNode.GetNext: TPDFOutlineNode;
var
  I: Integer;
begin
  I := FOwner.FList.IndexOf ( Self );
  if I <> FOwner.FList.Count - 1 then
    Result := FOwner [ i + 1 ]
  else
    Result := nil;
end;

function TPDFOutlineNode.GetNextChild (
  Node: TPDFOutlineNode ): TPDFOutlineNode;
var
  i: Integer;
begin
  i := FChild.IndexOf ( Pointer ( Node ) );
  if ( i = -1 ) or ( i = FChild.Count - 1 ) then
    Result := nil
  else
    Result := TPDFOutlineNode ( FChild [ i + 1 ] );
end;

function TPDFOutlineNode.GetNextSibling: TPDFOutlineNode;
begin
  Result := FNext;
end;

function TPDFOutlineNode.GetPrev: TPDFOutlineNode;
var
  I: Integer;
begin
  I := FOwner.FList.IndexOf ( Self );
  if I <> 0 then
    Result := FOwner [ i - 1 ]
  else
    Result := nil;
end;

function TPDFOutlineNode.GetPrevChild (
  Node: TPDFOutlineNode ): TPDFOutlineNode;
var
  i: Integer;
begin
  i := FChild.IndexOf ( Pointer ( Node ) );
  if ( i = -1 ) or ( i = 0 ) then
    Result := nil
  else
    Result := TPDFOutlineNode ( FChild [ i - 1 ] );
end;

function TPDFOutlineNode.GetPrevSibling: TPDFOutlineNode;
begin
  Result := FPrev;
end;

procedure TPDFOutlineNode.SetTitle ( const Value: string );
begin
  FTitle := Value;
end;

procedure TPDFOutlineNode.Save;
var
  I: Integer;
begin
  FOwner.FOwner.StartObj ( OutlineNodeID );
  if FCharset = ANSI_charset then
    FOwner.FOwner.SaveToStream ( '/Title (' + EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, OutlineNodeID, Title ) ) + ')' )
  else
    FOwner.FOwner.SaveToStream ( '/Title <' + EnCodeHexString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, OutlineNodeID, UnicodeChar ( FTitle, FCharset ) ) + '>' );
  if FOwner.FOwner.Version = v14 then
  begin
    if Color <> 0 then
      FOwner.FOwner.SaveToStream ( '/C [' + FormatFloat ( GetRValue ( Color ) / 255 ) + ' ' +
        FormatFloat ( GetGValue ( Color ) / 255 ) + ' ' + FormatFloat ( GetBValue ( Color ) / 255 ) + ' ]' );
    I := 0;
    if fsbold in Style then
      I := I or 2;
    if fsItalic in Style then
      I := I or 1;
    if I <> 0 then
      FOwner.FOwner.SaveToStream ( '/F ' + IntToStr ( I ) );
  end;
  if FChild.Count <> 0 then
  begin
    if FExpanded then
      FOwner.FOwner.SaveToStream ( '/Count ' + IntToStr ( FChild.Count ) )
    else
      FOwner.FOwner.SaveToStream ( '/Count -' + IntToStr ( FChild.Count ) );
    FOwner.FOwner.SaveToStream ( '/First ' + IntToStr ( GetFirstChild.OutlineNodeID ) + ' 0 R' );
    FOwner.FOwner.SaveToStream ( '/Last ' + IntToStr ( GetLastChild.OutlineNodeID ) + ' 0 R' );
  end;
  if FParent = nil then
    FOwner.FOwner.SaveToStream ( '/Parent ' + IntToStr ( FOwner.OutlinesID ) + ' 0 R' )
  else
    FOwner.FOwner.SaveToStream ( '/Parent ' + IntToStr ( FParent.OutlineNodeID ) + ' 0 R' );
  if FNext <> nil then
    FOwner.FOwner.SaveToStream ( '/Next ' + IntToStr ( FNext.OutlineNodeID ) + ' 0 R' );
  if FPrev <> nil then
    FOwner.FOwner.SaveToStream ( '/Prev ' + IntToStr ( FPrev.OutlineNodeID ) + ' 0 R' );
  if FAction <> nil then
    FOwner.FOwner.SaveToStream ( '/A ' + IntToStr ( FAction.ActionID ) + ' 0 R' );
  FOwner.FOwner.CloseHObj;
  FOwner.FOwner.CloseObj;
end;

procedure TPDFOutlineNode.SetExpanded ( const Value: Boolean );
begin
  FExpanded := Value;
end;

procedure TPDFOutlineNode.SetCharset ( const Value: TFontCharset );
begin
  FCharset := Value;
end;

procedure TPDFOutlineNode.SetAction ( const Value: TPDFAction );
begin
  if Value <> nil then
  begin
    FOwner.FOwner.AppendAction ( Value );
    FAction := Value;
  end;
end;

{ TPDFDocInfo }

procedure TPDFDocInfo.SetAuthor ( const Value: string );
begin
  FAuthor := Value;
end;

procedure TPDFDocInfo.SetCreationDate ( const Value: TDateTime );
begin
  FCreationDate := Value;
end;

procedure TPDFDocInfo.SetCreator ( const Value: string );
begin
  FCreator := Value;
end;

procedure TPDFDocInfo.SetKeywords ( const Value: string );
begin
  FKeywords := Value;
end;

procedure TPDFDocInfo.SetSubject ( const Value: string );
begin
  FSubject := Value;
end;

procedure TPDFDocInfo.SetTitle ( const Value: string );
begin
  FTitle := Value;
end;

{ TPDFCustomAnnotation }

function TPDFCustomAnnotation.CalcFlags: Integer;
begin
  Result := 0;
  if afInvisible in FFlags then
    Result := Result or 1;
  if afHidden in FFlags then
    Result := Result or 2;
  if afPrint in FFlags then
    Result := Result or 4;
  if afNoZoom in FFlags then
    Result := Result or 8;
  if afNoRotate in FFlags then
    Result := Result or 16;
  if afNoView in FFlags then
    Result := Result or 32;
  if afReadOnly in FFlags then
    Result := Result or 64;
end;

constructor TPDFCustomAnnotation.Create ( AOwner: TPDFPage );
begin
  if AOwner = nil then
    raise TPDFException.Create ( SAnnotationMustHaveTPDFPageAsOwner );
  FOwner := AOwner;
  AnnotID := FOwner.FOwner.GetNextID;
  FBorderStyle := '[0 0 1]';
  FBorderColor := clYellow;
  FOwner.FAnnot.Add ( Pointer ( Self ) );
end;

function TPDFCustomAnnotation.GetBox: TRect;
begin
  Result.Left := Round ( FOwner.IntToExtX ( FLeft ) );
  Result.Top := Round ( FOwner.IntToExtY ( FTop ) );
  Result.Bottom := Round ( FOwner.IntToExtY ( FBottom ) );
  Result.Right := Round ( FOwner.IntToExtX ( FRight ) );
end;


procedure TPDFCustomAnnotation.SetBox ( const Value: TRect );
var
  R: TRect;
begin
  R := Value;
  NormalizeRect ( R.Left, R.Top, R.Right, R.Bottom );
  FLeft := Round ( FOwner.ExtToIntX ( R.Left ) );
  FTop := Round ( FOwner.ExtToIntY ( R.Top ) );
  FBottom := Round ( FOwner.ExtToIntY ( R.Bottom ) );
  FRight := Round ( FOwner.ExtToIntX ( R.Right ) );
end;

{ TPDFTextAnnotation }


procedure TPDFTextAnnotation.Save;
begin
  FOwner.FOwner.StartObj ( AnnotID );
  FOwner.FOwner.SaveToStream ( '/Type /Annot' );
  FOwner.FOwner.SaveToStream ( '/Subtype /Text' );
  FOwner.FOwner.SaveToStream ( '/Border ' + FBorderStyle );
  FOwner.FOwner.SaveToStream ( '/F ' + IntToStr ( CalcFlags ) );
  FOwner.FOwner.SaveToStream ( '/C [' + FormatFloat ( GetRValue ( FBorderColor ) / 255 ) + ' ' +
    FormatFloat ( GetGValue ( FBorderColor ) / 255 ) + ' ' + FormatFloat ( GetBValue ( FBorderColor ) / 255 ) + ' ]' );
  FOwner.FOwner.SaveToStream ( '/Rect [' + IntToStr ( Round ( FLeft ) ) + ' ' + IntToStr ( FBottom ) +
    ' ' + IntToStr ( FRight ) + ' ' + IntToStr ( FTop ) + ']' );
  if FCharset = ANSI_CHARSET then
  begin
    FOwner.FOwner.SaveToStream ( '/T (' + EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, FCaption ) ) + ')' );
    FOwner.FOwner.SaveToStream ( '/Contents (' + EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, FText ) ) + ')' );
  end
  else
  begin
    FOwner.FOwner.SaveToStream ( '/T <' + EnCodeHexString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, UnicodeChar ( FCaption, FCharset ) ) + '>' );
    FOwner.FOwner.SaveToStream ( '/Contents <' + EnCodeHexString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, UnicodeChar ( FText, FCharset ) ) + '>' );
  end;
  case TextAnnotationIcon of
    taiComment: FOwner.FOwner.SaveToStream ( '/Name /Comment' );
    taiKey: FOwner.FOwner.SaveToStream ( '/Name /Key' );
    taiNote: FOwner.FOwner.SaveToStream ( '/Name /Note' );
    taiHelp: FOwner.FOwner.SaveToStream ( '/Name /Help' );
    taiNewParagraph: FOwner.FOwner.SaveToStream ( '/Name /NewParagraph' );
    taiParagraph: FOwner.FOwner.SaveToStream ( '/Name /Paragraph' );
    taiInsert: FOwner.FOwner.SaveToStream ( '/Name /Insert' );
  end;
  if FOpened then
    FOwner.FOwner.SaveToStream ( '/Open true' )
  else
    FOwner.FOwner.SaveToStream ( '/Open false' );
  FOwner.FOwner.CloseHObj;
  FOwner.FOwner.CloseObj;
end;

{ TPDFActionAnotation }

procedure TPDFActionAnnotation.Save;
begin
  FOwner.FOwner.StartObj ( AnnotID );
  FOwner.FOwner.SaveToStream ( '/Type /Annot' );
  FOwner.FOwner.SaveToStream ( '/Subtype /Link' );
  FOwner.FOwner.SaveToStream ( '/Border ' + FBorderStyle );
  FOwner.FOwner.SaveToStream ( '/F ' + IntToStr ( CalcFlags ) );
  FOwner.FOwner.SaveToStream ( '/C [' + FormatFloat ( GetRValue ( FBorderColor ) / 255 ) + ' ' +
    FormatFloat ( GetGValue ( FBorderColor ) / 255 ) + ' ' + FormatFloat ( GetBValue ( FBorderColor ) / 255 ) + ' ]' );
  FOwner.FOwner.SaveToStream ( '/Rect [' + IntToStr ( Round ( FLeft ) ) + ' ' + IntToStr ( FBottom ) +
    ' ' + IntToStr ( FRight ) + ' ' + IntToStr ( FTop ) + ']' );
  FOwner.FOwner.SaveToStream ( '/A ' + IntToStr ( FAction.ActionID ) + ' 0 R' );
  FOwner.FOwner.CloseHObj;
  FOwner.FOwner.CloseObj;
end;

procedure TPDFActionAnnotation.SetAction ( const Value: TPDFAction );
begin
  FOwner.FOwner.AppendAction ( Value );
  FAction := Value;
end;


{ TPDFActions }

function TPDFActions.Add ( Action: TPDFAction ): Integer;
begin
  Result := FActions.Add ( Pointer ( Action ) );
  Action.FOwner := FOwner;
end;

procedure TPDFActions.Clear;
var
  I: Integer;
begin
  for I := 0 to FActions.Count - 1 do
    TPDFAction ( FActions [ I ] ).Free;
  FActions.Clear;
end;

constructor TPDFActions.Create ( AOwner: TPDFDocument );
begin
  FActions := TList.Create;
  FOwner := AOwner;
end;

procedure TPDFActions.Delete ( Action: TPDFAction );
var
  I: Integer;
begin
  I := IndexOf ( Action );
  if I <> -1 then
  begin
    TPDFAction ( FActions [ I ] ).Free;
    FActions.Delete ( I );
  end;
end;

destructor TPDFActions.Destroy;
begin
  Clear;
  FActions.Free;
  inherited;
end;

function TPDFActions.GetAction ( Index: Integer ): TPDFAction;
begin
  Result := TPDFAction ( FActions [ Index ] );
end;

function TPDFActions.GetCount: Integer;
begin
  Result := FActions.Count;
end;

function TPDFActions.IndexOf ( Action: TPDFAction ): Integer;
begin
  Result := FActions.IndexOf ( Pointer ( Action ) );
end;

{ TPDFURLAction }

procedure TPDFURLAction.Save;
begin
  FOwner.StartObj ( ActionID );
  FOwner.SaveToStream ( '/S /URI /URI (' + EscapeSpecialChar ( EnCodeString ( FOwner.FProtectionEnabled, FOwner.FKey, FOwner.FPKL, ActionID, FURL ) ) + ')' );
  if FNext <> nil then
    FOwner.SaveToStream ( '/Next ' + IntToStr ( FNext.ActionID ) + ' 0 R' );
  FOwner.CloseHObj;
  FOwner.CloseObj;
end;

procedure TPDFURLAction.SetURL ( const Value: string );
begin
  if Value = '' then
    raise TPDFException.Create ( SURLCannotBeEmpty );
  FURL := Value;
end;

{ TPDFAction }

constructor TPDFAction.Create;
begin
  FNext := nil;
  ActionID := 0;
end;

procedure TPDFAction.Prepare;
begin
  if ActionID < 1 then
    ActionID := FOwner.GetNextID;
  if FNext <> nil then
    FOwner.AppendAction ( FNext );
end;

{ TPDFJavaScriptAction }

procedure TPDFJavaScriptAction.Save;
begin
  FOwner.StartObj ( ActionID );
  FOwner.SaveToStream ( '/S /JavaScript /JS (' + EscapeSpecialChar ( EnCodeString ( FOwner.FProtectionEnabled, FOwner.FKey, FOwner.FPKL, ActionID, FJavaScript ) ) + ')' );
  if FNext <> nil then
    FOwner.SaveToStream ( '/Next ' + IntToStr ( FNext.ActionID ) + ' 0 R' );
  FOwner.CloseHObj;
  FOwner.CloseObj;
end;

procedure TPDFJavaScriptAction.SetJavaScript ( const Value: string );
begin
  if Value = '' then
    raise TPDFException.Create ( SJavaScriptCannotBeEmpty );
  FJavaScript := Value;
end;

{ TPDFGoToPageAction }

procedure TPDFGoToPageAction.Save;
begin
  FOwner.StartObj ( ActionID );
  if FChange then
  begin
    FOwner.SaveToStream ( '/S /GoTo /D [' + IntToStr ( FOwner.FPages [ FPageIndex ].PageID ) +
      ' 0 R /FitH ' + IntToStr ( Round ( FOwner.FPages [ PageIndex ].ExtToIntY ( FTopOffset ) ) ) + ']' );
  end
  else
  begin
    FOwner.SaveToStream ( '/S /GoTo /D [' + IntToStr ( FOwner.FPages [ FPageIndex ].PageID ) +
      ' 0 R /XYZ null ' + IntToStr ( Round ( FOwner.FPages [ PageIndex ].ExtToIntY ( FTopOffset ) ) ) + ' null ]' );
  end;
  if FNext <> nil then
    FOwner.SaveToStream ( '/Next ' + IntToStr ( FNext.ActionID ) + ' 0 R' );
  FOwner.CloseHObj;
  FOwner.CloseObj;
end;

procedure TPDFGoToPageAction.SetPageIndex ( const Value: Integer );
begin
  if Value < 0 then
    raise TPDFException.Create ( SPageIndexCannotBeNegative );
  FPageIndex := Value;
end;

procedure TPDFGoToPageAction.SetTopOffset ( const Value: Integer );
begin
  if Value < 0 then
    raise TPDFException.Create ( STopOffsetCannotBeNegative );
  FTopOffset := Value;
end;

{ TPDFVisibeControlAction }

constructor TPDFVisibeControlAction.Create;
begin
  FControls := TPDFControls.Create;
  Visible := True;
end;

destructor TPDFVisibeControlAction.Destroy;
begin
  FControls.Free;
end;

procedure TPDFVisibeControlAction.Save;
var
  I: Integer;
begin
  FOwner.StartObj ( ActionID );
  FOwner.SaveToStream ( '/S /Hide /T [', False );
  for I := 0 to FControls.Count - 1 do
    FOwner.SaveToStream ( IntToStr ( FControls [ I ].AnnotID ) + ' 0 R ', False );
  FOwner.SaveToStream ( ']' );
  if FVisible then
    FOwner.SaveToStream ( '/H false' );
  if FNext <> nil then
    FOwner.SaveToStream ( '/Next ' + IntToStr ( FNext.ActionID ) + ' 0 R' );
  FOwner.CloseHObj;
  FOwner.CloseObj;
end;


{ TPDFControls }

function TPDFControls.Add ( Control: TPDFControl ): Integer;
begin
  Result := FControls.Add ( Pointer ( Control ) );
end;

procedure TPDFControls.Clear;
begin
  FControls.Clear;
end;

constructor TPDFControls.Create;
begin
  FControls := TList.Create;
end;

procedure TPDFControls.Delete ( Control: TPDFControl );
begin
  FControls.Delete ( IndexOf ( Control ) );
end;

destructor TPDFControls.Destroy;
begin
  FControls.Free;
end;

function TPDFControls.GetControl ( Index: Integer ): TPDFControl;
begin
  Result := TPDFControl ( FControls [ Index ] );
end;

function TPDFControls.GetCount: Integer;
begin
  Result := FControls.Count;
end;

function TPDFControls.IndexOf ( Control: TPDFControl ): Integer;
begin
  Result := FControls.IndexOf ( Pointer ( Control ) );
end;

{ TPDFSubmitFormAction }

constructor TPDFSubmitFormAction.Create;
begin
  URL := 'http://www.borland.com';
  FFields := TPDFControls.Create;
  FRG := TList.Create
end;

destructor TPDFSubmitFormAction.Destroy;
begin
  FRG.Free;
  FFields.Free;
end;

procedure TPDFSubmitFormAction.Save;
var
  I, Flag: Integer;
  S: string;
begin
  I := 0;
  while I < FFields.Count do
  begin
    if FFields [ I ] is TPDFButton then
      FFields.Delete ( FFields [ I ] )
    else if FFields [ I ] is TPDFRadioButton then
    begin
      if FRG.IndexOf ( TPDFRadioButton ( FFields [ I ] ).FRG ) < 0 then
        FRG.Add ( TPDFRadioButton ( FFields [ I ] ).FRG );
      FFields.Delete ( FFields [ I ] );
    end
    else
      Inc ( I );
  end;
  Flag := 0;
  if SubmitType <> StFDF then
  begin
    if SubmitType <> stPost then
      Flag := Flag or 8;
    Flag := Flag or 4;
    S := '';
  end
  else
    S := '#FDF';
  if FIncludeEmptyFields then
    Flag := Flag or 2;
  FOwner.StartObj ( ActionID );
  FOwner.SaveToStream ( '/S /SubmitForm' );
  FOwner.SaveToStream ( '/F <</FS /URL /F (' + EscapeSpecialChar ( EnCodeString ( FOwner.FProtectionEnabled, FOwner.FKey, FOwner.FPKL, ActionID, FURL + S ) ) + ')>>' );
  if ( FFields.Count > 0 ) or ( FRG.Count > 0 ) then
  begin
    FOwner.SaveToStream ( '/Fields [', False );
    for I := 0 to FFields.Count - 1 do
      FOwner.SaveToStream ( IntToStr ( FFields [ I ].AnnotID ) + ' 0 R ', False );
    for I := 0 to FRG.Count - 1 do
      FOwner.SaveToStream ( IntToStr ( TPDFRadioGroup ( FRG [ I ] ).GroupID ) + ' 0 R ', False );
    FOwner.SaveToStream ( ']' );
  end;
  FOwner.SaveToStream ( '/Flags ' + IntToStr ( Flag ) );
  if FNext <> nil then
    FOwner.SaveToStream ( '/Next ' + IntToStr ( FNext.ActionID ) + ' 0 R' );
  FOwner.CloseHObj;
  FOwner.CloseObj;
end;

procedure TPDFSubmitFormAction.SetURL ( const Value: string );
begin
  if Value = '' then
    raise TPDFException.Create ( SURLCannotBeEmpty );
  FURL := Value;
end;

{ TPDFResetFormAction }

constructor TPDFResetFormAction.Create;
begin
  FFields := TPDFControls.Create;
  FRG := TList.Create;
end;

destructor TPDFResetFormAction.Destroy;
begin
  FRG.Free;
  FFields.Free;
end;

procedure TPDFResetFormAction.Save;
var
  I: Integer;
begin
  I := 0;
  while I < FFields.Count do
  begin
    if FFields [ I ] is TPDFButton then
      FFields.Delete ( FFields [ I ] )
    else if FFields [ I ] is TPDFRadioButton then
    begin
      if FRG.IndexOf ( TPDFRadioButton ( FFields [ I ] ).FRG ) < 0 then
        FRG.Add ( TPDFRadioButton ( FFields [ I ] ).FRG );
      FFields.Delete ( FFields [ I ] );
    end
    else
      Inc ( I );
  end;
  FOwner.StartObj ( ActionID );
  FOwner.SaveToStream ( '/S /ResetForm' );
  if ( FFields.Count > 0 ) or ( FRG.Count > 0 ) then
  begin
    FOwner.SaveToStream ( '/Fields [', False );
    for I := 0 to FFields.Count - 1 do
      FOwner.SaveToStream ( IntToStr ( FFields [ I ].AnnotID ) + ' 0 R ', False );
    for I := 0 to FRG.Count - 1 do
      FOwner.SaveToStream ( IntToStr ( TPDFRadioGroup ( FRG [ I ] ).GroupID ) + ' 0 R ', False );
    FOwner.SaveToStream ( ']' );
  end;
  if FNext <> nil then
    FOwner.SaveToStream ( '/Next ' + IntToStr ( FNext.ActionID ) + ' 0 R' );
  FOwner.CloseHObj;
  FOwner.CloseObj;
end;

{ TPDFImportDataAction }

procedure TPDFImportDataAction.Save;
begin
  FOwner.StartObj ( ActionID );
  FOwner.SaveToStream ( '/S /ImportData' );
  FOwner.SaveToStream ( '/F <</Type /FileSpec /F (' + EscapeSpecialChar ( EnCodeString ( FOwner.FProtectionEnabled, FOwner.FKey, FOwner.FPKL, ActionID, FFileName ) ) + ')>>' );
  if FNext <> nil then
    FOwner.SaveToStream ( '/Next ' + IntToStr ( FNext.ActionID ) + ' 0 R' );
  FOwner.CloseHObj;
  FOwner.CloseObj;
end;

procedure TPDFImportDataAction.SetFileName ( const Value: string );
begin
  if Value = '' then
    raise TPDFException.Create ( SFileNameCannotBeEmpty );
  FFileName := Value;
end;

{ TPDFControl }

function TPDFControl.CalcActions: string;
begin
  Result := '';
  if OnMouseUp <> nil then
    Result := Result + '/A ' + IntToStr ( OnMouseUp.ActionID ) + ' 0 R'#13#10;
  Result := Result + '/AA <<';
  if OnMouseEnter <> nil then
    Result := Result + '/E ' + IntToStr ( OnMouseEnter.ActionID ) + ' 0 R';
  if OnMouseExit <> nil then
    Result := Result + '/X ' + IntToStr ( OnMouseExit.ActionID ) + ' 0 R';
  if OnMouseDown <> nil then
    Result := Result + '/D ' + IntToStr ( OnMouseDown.ActionID ) + ' 0 R';
  if OnLostFocus <> nil then
    Result := Result + '/Bl ' + IntToStr ( OnLostFocus.ActionID ) + ' 0 R';
  if OnSetFocus <> nil then
    Result := Result + '/D ' + IntToStr ( OnSetFocus.ActionID ) + ' 0 R';
end;

constructor TPDFControl.Create ( AOwner: TPDFPage; AName: string );
begin
  inherited Create ( AOwner );
  Flags := [ afPrint ];
  Name := AName;
  FFont := TPDFControlFont.Create;
  FHint := TPDFControlHint.Create;
  FBorderColor := clBlack;
  if not ( ( Self is TPDFButton ) or ( Self is TPDFRadioButton ) ) then
    FOwner.FOwner.FAcroForm.FFields.Add ( Self );
end;

destructor TPDFControl.Destroy;
begin
  FFont.Free;
  FHint.Free;
  inherited;
end;

procedure TPDFControl.SetColor ( const Value: TColor );
begin
  FColor := Value;
end;

procedure TPDFControl.SetName ( const Value: string );
begin
  FName := Value;
end;

procedure TPDFControl.SetOnLostFocus ( const Value: TPDFAction );
begin
  if FOnLostFocus = nil then
    FOnLostFocus := Value
  else
  begin
    Value.FNext := FOnLostFocus;
    FOnLostFocus := Value;
  end;
  FOwner.FOwner.AppendAction ( Value );
end;

procedure TPDFControl.SetOnMouseDown ( const Value: TPDFAction );
begin
  if FOnMouseDown = nil then
    FOnMouseDown := Value
  else
  begin
    Value.FNext := FOnMouseDown;
    FOnMouseDown := Value;
  end;
  FOwner.FOwner.AppendAction ( Value );
end;

procedure TPDFControl.SetOnMouseEnter ( const Value: TPDFAction );
begin
  if FOnMouseEnter = nil then
    FOnMouseEnter := Value
  else
  begin
    Value.FNext := FOnMouseEnter;
    FOnMouseEnter := Value;
  end;
  FOwner.FOwner.AppendAction ( Value );
end;

procedure TPDFControl.SetOnMouseExit ( const Value: TPDFAction );
begin
  if FOnMouseExit = nil then
    FOnMouseExit := Value
  else
  begin
    Value.FNext := FOnMouseExit;
    FOnMouseExit := Value;
  end;
  FOwner.FOwner.AppendAction ( Value );
end;

procedure TPDFControl.SetOnMouseUp ( const Value: TPDFAction );
begin
  if FOnMouseUp = nil then
    FOnMouseUp := Value
  else
  begin
    Value.FNext := FOnMouseUp;
    FOnMouseUp := Value;
  end;
  FOwner.FOwner.AppendAction ( Value );
end;

procedure TPDFControl.SetOnSetFocus ( const Value: TPDFAction );
begin
  if FOnSetFocus = nil then
    FOnSetFocus := Value
  else
  begin
    Value.FNext := FOnSetFocus;
    FOnSetFocus := Value;
  end;
  FOwner.FOwner.AppendAction ( Value );
end;

{ TPDFAcroForm }

procedure TPDFAcroForm.Clear;
begin
  FFields.Clear;
  FFonts.Clear;
  FRadioGroups.Clear;
end;

constructor TPDFAcroForm.Create ( AOwner: TPDFDocument );
begin
  FFields := TPDFControls.Create;
  FFonts := TList.Create;
  FOwner := AOwner;
  FRadioGroups := TList.Create;
end;

destructor TPDFAcroForm.Destroy;
begin
  FRadioGroups.Free;
  FFonts.Free;
  FFields.Free;
end;

function TPDFAcroForm.GetEmpty: Boolean;
begin
  Result := ( FFields.Count = 0 ) and ( FRadioGroups.Count = 0 );
end;

procedure TPDFAcroForm.Save;
var
  I, ResID: Integer;
begin
  if ( FFields.Count = 0 ) and ( FRadioGroups.Count = 0 ) then
    Exit;
  FOwner.StartObj ( ResID );
  FOwner.SaveToStream ( '/Font <<', False );
  for I := 0 to FFonts.Count - 1 do
    FOwner.SaveToStream ( '/' + TPDFFont ( FFonts [ I ] ).AliasName + ' ' + IntToStr ( TPDFFont ( FFonts [ I ] ).FontID ) + ' 0 R ', False );
  FOwner.SaveToStream ( '>>' );
  FOwner.CloseHObj;
  FOwner.CloseObj;
  FOwner.StartObj ( AcroID );
  FOwner.SaveToStream ( '/Fields [', False );
  for I := 0 to FFields.Count - 1 do
    FOwner.SaveToStream ( IntToStr ( FFields [ I ].AnnotID ) + ' 0 R ', False );
  for I := 0 to FRadioGroups.Count - 1 do
    FOwner.SaveToStream ( IntToStr ( TPDFRadioGroup ( FRadioGroups [ I ] ).GroupID ) + ' 0 R ', False );
  FOwner.SaveToStream ( ']', False );
  FOwner.SaveToStream ( '/DR ' + IntToStr ( ResID ) + ' 0 R' );
  FOwner.SaveToStream ( '/CO [', False );
  for I := 0 to FFields.Count - 1 do
    if FFields [ I ] is TPDFInputControl then
      if TPDFInputControl ( FFields [ I ] ).OnOtherCOntrolChanged <> nil then
        FOwner.SaveToStream ( IntToStr ( FFields [ I ].AnnotID ) + ' 0 R ', False );
  FOwner.SaveToStream ( ']', False );
  FOwner.CloseHObj;
  FOwner.CloseObj;
end;

{ TPDFInputControl }

function TPDFInputControl.CalcActions: string;
begin
  Result := inherited CalcActions;
  if OnKeyPress <> nil then
    Result := Result + '/K ' + IntToStr ( OnKeyPress.ActionID ) + ' 0 R';
  if OnBeforeFormatting <> nil then
    Result := Result + '/F ' + IntToStr ( OnBeforeFormatting.ActionID ) + ' 0 R';
  if OnChange <> nil then
    Result := Result + '/V ' + IntToStr ( OnChange.ActionID ) + ' 0 R';
  if OnOtherControlChanged <> nil then
    Result := Result + '/C ' + IntToStr ( OnOtherControlChanged.ActionID ) + ' 0 R';
end;

procedure TPDFInputControl.SetOnBeforeFormatting ( const Value: TPDFJavaScriptAction );
begin
  FOnBeforeFormatting := Value;
  FOwner.FOwner.AppendAction ( Value );
end;

procedure TPDFInputControl.SetOnChange ( const Value: TPDFJavaScriptAction );
begin
  FOnChange := Value;
  FOwner.FOwner.AppendAction ( Value );
end;

procedure TPDFInputControl.SetOnKeyPress ( const Value: TPDFJavaScriptAction );
begin
  FOnKeyPress := Value;
  FOwner.FOwner.AppendAction ( Value );
end;

procedure TPDFInputControl.SetOnOtherCOntrolChanged ( const Value: TPDFJavaScriptAction );
begin
  FOnOtherCOntrolChanged := Value;
  FOwner.FOwner.AppendAction ( Value );
end;

{ TPDFControlFont }

constructor TPDFControlFont.Create;
begin
  FName := 'Arial';
  FColor := clBlack;
  FSize := 8;
  FStyle := [ ];
end;


procedure TPDFControlFont.SetSize ( const Value: Integer );
begin
  if Value < 0 then
    raise TPDFException.Create ( SCannotSetNegativeSize );
  FSize := Value;
end;

{ TPDFButton }

constructor TPDFButton.Create ( AOwner: TPDFPage; AName: string );
begin
  inherited Create ( AOwner, AName );
  FColor := clSilver;
end;

procedure TPDFButton.Paint;
var
  x, y, z: array [ 0..3 ] of Byte;
  i: Integer;
begin
  I := ColorToRGB ( FFont.Color );
  Move ( i, x [ 0 ], 4 );
  i := ColorToRGB ( FColor );
  Move ( i, z [ 0 ], 4 );
  i := ColorToRGB ( FBorderColor );
  Move ( i, y [ 0 ], 4 );
  with FUp do
  begin
    Width := abs ( FRight - FLeft );
    Height := abs ( FBottom - FTop );
    SetRGBColorFill ( z [ 0 ] / 255 * 0.6, z [ 1 ] / 255 * 0.6, z [ 2 ] / 255 * 0.6 );
    MoveTo ( Width - 0.5, 0.5 );
    LineTo ( Width - 0.5, Height - 0.5 );
    LineTo ( 0.5, Height - 0.5 );
    LineTo ( 1.5, Height - 1.5 );
    LineTo ( Width - 1.5, Height - 1.5 );
    LineTo ( Width - 1.5, 1.5 );
    LineTo ( Width - 0.5, 0.5 );
    Fill;
    SetRGBColorFill ( z [ 0 ] / 255 * 1.4, z [ 1 ] / 255 * 1.4, z [ 2 ] / 255 * 1.4 );
    MoveTo ( 0.5, Height - 0.5 );
    LineTo ( 0.5, 0.5 );
    LineTo ( Width - 0.5, 0.5 );
    LineTo ( Width - 1.5, 1.5 );
    LineTo ( 1.5, 1.5 );
    LineTo ( 1.5, Height - 1.5 );
    LineTo ( 0.5, Height - 0.5 );
    Fill;
    SetRGBColorFill ( z [ 0 ] / 255, z [ 1 ] / 255, z [ 2 ] / 255 );
    Rectangle ( 1.5, 1.5, Width - 1.5, Height - 1.5 );
    Fill;
    SetLineWidth ( 1 );
    SetRGBColorStroke ( y [ 0 ] / 255, y [ 1 ] / 255, y [ 2 ] / 255 );
    Rectangle ( 0, 0, Width, Height );
    Stroke;
    BeginText;
    SetRGBColorFill ( x [ 0 ] / 255, X [ 1 ] / 255, X [ 2 ] / 255 );
    SetActiveFont ( FFont.Name, FFont.Style, FFont.Size, 0 );
    SetCurrentFont ( False );
    FCurrentFont.SetAllANSI;
    TextBox ( Rect ( 0, 0, Width, Height ), Caption, hjCenter, vjCenter );
    FFN := FCurrentFont.AliasName;
    EndText;
  end;
  with FDown do
  begin
    Width := abs ( FRight - FLeft );
    Height := abs ( FBottom - FTop );
    SetLineWidth ( 1 );
    SetRGBColorFill ( z [ 0 ] / 255, z [ 1 ] / 255, z [ 2 ] / 255 );
    Rectangle ( 0, 0, Width, Height );
    Fill;
    SetRGBColorStroke ( z [ 0 ] / 255 * 1.4, z [ 1 ] / 255 * 1.4, z [ 2 ] / 255 * 1.4 );
    MoveTo ( Width - 0.5, 1 );
    LineTo ( Width - 0.5, Height - 0.5 );
    LineTo ( 1, Height - 0.5 );
    Stroke;
    SetRGBColorStroke ( z [ 0 ] / 255 * 0.6, z [ 1 ] / 255 * 0.6, z [ 2 ] / 255 * 0.6 );
    MoveTo ( 0.5, Height - 1 );
    LineTo ( 0.5, 0.5 );
    LineTo ( Width - 1, 0.5 );
    Stroke;
    SetRGBColorStroke ( y [ 0 ] / 255, y [ 1 ] / 255, y [ 2 ] / 255 );
    Rectangle ( 0, 0, Width, Height );
    Stroke;
    BeginText;
    SetRGBColorFill ( x [ 0 ] / 255, X [ 1 ] / 255, X [ 2 ] / 255 );
    SetActiveFont ( FFont.Name, FFont.Style, FFont.Size, 0 );
    SetCurrentFont ( False );
    FCurrentFont.SetAllANSI;
    TextBox ( Rect ( 0, 0, Width, Height - 2 ), Caption, hjCenter, vjCenter );
    EndText;
  end;
end;

procedure TPDFButton.Save;
var
  i, j: Integer;
begin
  FUp := TPDFPage.Create ( FOwner.FOwner );
  try
    FUp.FIsForm := True;
    try
      FDown := TPDFPage.Create ( FOwner.FOwner );
      FDown.FIsForm := True;
      Paint;
      FUp.Save;
      FDown.Save;
      for i := 0 to Length ( FUp.FLinkedFont ) - 1 do
        if FOwner.FOwner.FAcroForm.FFonts.IndexOf ( FUp.FLinkedFont [ i ] ) = -1 then
          FOwner.FOwner.FAcroForm.FFonts.Add ( FUp.FLinkedFont [ i ] );
      FOwner.FOwner.StartObj ( AnnotID );
      FOwner.FOwner.SaveToStream ( '/Type /Annot' );
      FOwner.FOwner.SaveToStream ( '/Subtype /Widget' );
      FOwner.FOwner.SaveToStream ( '/Rect [' + IntToStr ( Round ( FLeft ) ) + ' ' + IntToStr ( FBottom ) +
        ' ' + IntToStr ( FRight ) + ' ' + IntToStr ( FTop ) + ']' );
      FOwner.FOwner.SaveToStream ( '/P ' + IntToStr ( FOwner.PageID ) + ' 0 R' );
      FOwner.FOwner.SaveToStream ( '/MK <</CA (' +
        EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, Caption ) ) + ') ', False );
      FOwner.FOwner.SaveToStream ( '/BC [' + FormatFloat ( GetRValue ( FBorderColor ) / 255 ) + ' ' +
        FormatFloat ( GetGValue ( FBorderColor ) / 255 ) + ' ' + FormatFloat ( GetBValue ( FBorderColor ) / 255 ) + ' ]', False );
      FOwner.FOwner.SaveToStream ( '/BG [' + FormatFloat ( GetRValue ( FColor ) / 255 ) + ' ' +
        FormatFloat ( GetGValue ( FColor ) / 255 ) + ' ' + FormatFloat ( GetBValue ( FColor ) / 255 ) + ' ]', False );
      FOwner.FOwner.SaveToStream ( '>>' );
      FOwner.FOwner.SaveToStream ( '/DA (' + EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled,
        FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, '/' + FFN + ' ' + IntToStr ( FFont.Size ) ) ) + ' Tf ' +
        FormatFloat ( GetRValue ( FFont.Color ) / 255 ) + ' ' +
        FormatFloat ( GetGValue ( FFont.Color ) / 255 ) + ' ' + FormatFloat ( GetBValue ( FFont.Color ) / 255 ) + ' rg)' );
      FOwner.FOwner.SaveToStream ( '/BS <</W 1 /S /B>>' );
      FOwner.FOwner.SaveToStream ( '/T (' +
        EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, Name ) ) + ')' );
      FOwner.FOwner.SaveToStream ( '/FT /Btn' );
      if FHint.Caption <> '' then
        if ( FHint.Charset in [ 0..2 ] ) then
          FOwner.FOwner.SaveToStream ( '/TU (' +
            EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, FHint.Caption ) ) + ')' )
        else
          FOwner.FOwner.SaveToStream ( '/TU <' +
            EnCodeHexString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, UnicodeChar ( FHint.Caption, FHint.Charset ) ) + '>' );
      i := 0;
      if FReadOnly then
        i := i or 1;
      if FRequired then
        i := i or 2;
      j := 1 shl 16;
      i := i or j;
      FOwner.FOwner.SaveToStream ( '/F ' + IntToStr ( CalcFlags ) );
      FOwner.FOwner.SaveToStream ( '/Ff ' + IntToStr ( i ) );
      FOwner.FOwner.SaveToStream ( '/H /P' );
      FOwner.FOwner.SaveToStream ( '/AP <</N ' + IntToStr ( FUp.PageID ) + ' 0 R ', False );
      FOwner.FOwner.SaveToStream ( '/D ' + IntToStr ( FDown.PageID ) + ' 0 R', False );
      FOwner.FOwner.SaveToStream ( '>>' );
      FOwner.FOwner.SaveToStream ( CalcActions + '>>' );
      FOwner.FOwner.CloseHObj;
      FOwner.FOwner.CloseObj;
    finally
      FDown.Free;
    end;
  finally
    FUp.Free;
  end;
end;

{ TPDFCheckBox }

constructor TPDFCheckBox.Create ( AOwner: TPDFPage; AName: string );
begin
  inherited Create ( AOwner, AName );
  FColor := clWhite;
end;

procedure TPDFCheckBox.Paint;
var
  x, y, z: array [ 0..3 ] of Byte;
  i: Integer;
begin
  I := ColorToRGB ( FFont.Color );
  Move ( i, x [ 0 ], 4 );
  i := ColorToRGB ( FColor );
  Move ( i, z [ 0 ], 4 );
  i := ColorToRGB ( FBorderColor );
  Move ( i, y [ 0 ], 4 );
  with FCheck do
  begin
    Width := abs ( FRight - FLeft );
    Height := abs ( FBottom - FTop );
    SetLineWidth ( 1 );
    SetRGBColorFill ( z [ 0 ] / 255, z [ 1 ] / 255, z [ 2 ] / 255 );
    Rectangle ( 0, 0, Width, Height );
    Fill;
    SetRGBColor ( y [ 0 ] / 255, y [ 1 ] / 255, y [ 2 ] / 255 );
    Rectangle ( 0.5, 0.5, Height - 0.5, Height - 0.5 );
    Stroke;
    BeginText;
    SetActiveFont ( 'ZapfDingbats', [ ], Height - 2 );
    TextBox ( Rect ( 2, 2, Height - 2, Height - 4 ), '8', hjCenter, vjCenter );
    SetRGBColorFill ( x [ 0 ] / 255, X [ 1 ] / 255, X [ 2 ] / 255 );
    SetActiveFont ( FFont.Name, FFont.Style, FFont.Size, 0 );
    SetCurrentFont ( False );
    TextBox ( Rect ( Height + 4, 0, Width, Height ), Caption, hjLeft, vjCenter );
    EndText;
  end;
  with FUncheck do
  begin
    Width := abs ( FRight - FLeft );
    Height := abs ( FBottom - FTop );
    SetLineWidth ( 1 );
    SetRGBColorFill ( z [ 0 ] / 255, z [ 1 ] / 255, z [ 2 ] / 255 );
    Rectangle ( 0, 0, Width, Height );
    Fill;
    SetRGBColorStroke ( y [ 0 ] / 255, y [ 1 ] / 255, y [ 2 ] / 255 );
    Rectangle ( 0.5, 0.5, Height - 0.5, Height - 0.5 );
    Stroke;
    BeginText;
    SetRGBColorFill ( x [ 0 ] / 255, X [ 1 ] / 255, X [ 2 ] / 255 );
    SetActiveFont ( FFont.Name, FFont.Style, FFont.Size, 0 );
    SetCurrentFont ( False );
    TextBox ( Rect ( Height + 4, 0, Width, Height ), Caption, hjLeft, vjCenter );
    EndText;
  end;
end;

procedure TPDFCheckBox.Save;
var
  i: Integer;
begin
  FCheck := TPDFPage.Create ( FOwner.FOwner );
  try
    FCheck.FIsForm := True;
    FUnCheck := TPDFPage.Create ( FOwner.FOwner );
    try
      FUnCheck.FIsForm := True;
      Paint;
      FCheck.Save;
      FUnCheck.Save;
      FOwner.FOwner.StartObj ( AnnotID );
      FOwner.FOwner.SaveToStream ( '/Type /Annot' );
      FOwner.FOwner.SaveToStream ( '/Subtype /Widget' );
      FOwner.FOwner.SaveToStream ( '/H /T' );
      FOwner.FOwner.SaveToStream ( '/Rect [' + IntToStr ( Round ( FLeft ) ) + ' ' + IntToStr ( FBottom ) +
        ' ' + IntToStr ( FRight ) + ' ' + IntToStr ( FTop ) + ']' );
      FOwner.FOwner.SaveToStream ( '/P ' + IntToStr ( FOwner.PageID ) + ' 0 R' );
      if FChecked then
        FOwner.FOwner.SaveToStream ( '/V /Yes /AS /Yes' )
      else
        FOwner.FOwner.SaveToStream ( '/V /Off /AS /Off' );
      FOwner.FOwner.SaveToStream ( '/T (' + EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, Name ) ) + ')' );
      FOwner.FOwner.SaveToStream ( '/FT /Btn' );
      FOwner.FOwner.SaveToStream ( '/F ' + IntToStr ( CalcFlags ) );
      i := 0;
      if FReadOnly then
        i := i or 1;
      if FRequired then
        i := i or 2;
      FOwner.FOwner.SaveToStream ( '/Ff ' + IntToStr ( i ) );
      FOwner.FOwner.SaveToStream ( '/AP <</N << /Yes ' + IntToStr ( FCheck.PageID ) + ' 0 R ', False );
      FOwner.FOwner.SaveToStream ( '/Off  ' + IntToStr ( FUnCheck.PageID ) + ' 0 R >> ', False );
      FOwner.FOwner.SaveToStream ( '>>' );
      FOwner.FOwner.SaveToStream ( CalcActions + '>>' );
      FOwner.FOwner.CloseHObj;
      FOwner.FOwner.CloseObj;
    finally
      FUnCheck.Free;
    end;
  finally
    FCheck.Free;
  end;
end;

{ TPDFEdit }

constructor TPDFEdit.Create ( AOwner: TPDFPage; AName: string );
begin
  inherited Create ( AOwner, AName );
  FBorderColor := clBlack;
  FColor := clWhite;
  FShowBorder := True;
  FMultiline := False;
  FIsPassword := False;
  FMaxLength := 0;
end;

procedure TPDFEdit.Paint;
var
  x, y, z: array [ 0..3 ] of Byte;
  i: Integer;
  s: string;
begin
  i := ColorToRGB ( FColor );
  Move ( i, z [ 0 ], 4 );
  i := ColorToRGB ( FBorderColor );
  Move ( i, y [ 0 ], 4 );
  I := ColorToRGB ( FFont.Color );
  Move ( i, x [ 0 ], 4 );
  with FShow do
  begin
    Width := abs ( FRight - FLeft );
    Height := abs ( FBottom - FTop );
    FEmulationEnabled := False;
    SetLineWidth ( 1 );
    SetRGBColorFill ( z [ 0 ] / 255, z [ 1 ] / 255, z [ 2 ] / 255 );
    SetRGBColorStroke ( y [ 0 ] / 255, y [ 1 ] / 255, y [ 2 ] / 255 );
    Rectangle ( 0, 0, Width, Height );
    if FShowBorder then
      FillAndStroke
    else
      Fill;
    AppendAction ( '/Tx BMC' );
    BeginText;
    SetRGBColorFill ( x [ 0 ] / 255, X [ 1 ] / 255, X [ 2 ] / 255 );
    SetActiveFont ( FFont.Name, FFont.Style, FFont.Size, 0 );
    SetCurrentFont ( False );
    FCurrentFont.SetAllANSI;
    if FText <> '' then
      if not FIsPassword then
        TextBox ( Rect ( 2, 2, Width - 2, Height - 2 ), FText, FJustification, vjCenter )
      else
      begin
        s := '';
        for i := 1 to Length ( FText ) do
          s := s + '*';
        TextBox ( Rect ( 2, 2, Width - 2, Height - 2 ), s, FJustification, vjCenter )
      end;
    FFN := FCurrentFont.AliasName;
    EndText;
    AppendAction ( 'EMC' );
  end;
end;

procedure TPDFEdit.Save;
var
  i, j: Integer;
begin
  FShow := TPDFPage.Create ( FOwner.FOwner );
  try
    FShow.FIsForm := True;
    FShow.FRemoveCR := True;
    Paint;
    for i := 0 to Length ( FShow.FLinkedFont ) - 1 do
      if FOwner.FOwner.FAcroForm.FFonts.IndexOf ( FShow.FLinkedFont [ i ] ) = -1 then
        FOwner.FOwner.FAcroForm.FFonts.Add ( FShow.FLinkedFont [ i ] );
    FShow.Save;
    FOwner.FOwner.StartObj ( AnnotID );
    FOwner.FOwner.SaveToStream ( '/Type /Annot' );
    FOwner.FOwner.SaveToStream ( '/Subtype /Widget' );
    FOwner.FOwner.SaveToStream ( '/Rect [' + IntToStr ( Round ( FLeft ) ) + ' ' + IntToStr ( FBottom ) +
      ' ' + IntToStr ( FRight ) + ' ' + IntToStr ( FTop ) + ']' );
    FOwner.FOwner.SaveToStream ( '/FT /Tx' );
    FOwner.FOwner.SaveToStream ( '/F ' + IntToStr ( CalcFlags ) );
    FOwner.FOwner.SaveToStream ( '/P ' + IntToStr ( FOwner.PageID ) + ' 0 R' );
    FOwner.FOwner.SaveToStream ( '/T (' + EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, Name ) ) + ')' );
    i := 0;
    if FReadOnly then
      i := i or 1;
    if FRequired then
      i := i or 2;
    if FMultiline then
    begin
      j := 1 shl 12;
      i := i or j;
    end;
    if FIsPassword then
    begin
      j := 1 shl 13;
      i := i or j;
    end;
    FOwner.FOwner.SaveToStream ( '/Ff ' + IntToStr ( i ) );
    case FJustification of
      hjCenter: FOwner.FOwner.SaveToStream ( '/Q 1' );
      hjRight: FOwner.FOwner.SaveToStream ( '/Q 2' );
    end;
    if FText <> '' then
      FOwner.FOwner.SaveToStream ( '/V (' + EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, FText ) ) + ')' );
    if FText <> '' then
      FOwner.FOwner.SaveToStream ( '/DV (' + EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, FText ) ) + ')' );
    if FMaxLength <> 0 then
      FOwner.FOwner.SaveToStream ( '/MaxLen ' + IntToStr ( FMaxLength ) );
    FOwner.FOwner.SaveToStream ( '/DA (' + EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled,
      FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, '/' + FFN + ' ' + IntToStr ( FFont.Size ) ) ) + ' Tf ' +
      FormatFloat ( GetRValue ( FFont.Color ) / 255 ) + ' ' +
      FormatFloat ( GetGValue ( FFont.Color ) / 255 ) + ' ' + FormatFloat ( GetBValue ( FFont.Color ) / 255 ) + ' rg)' );
    FOwner.FOwner.SaveToStream ( '/AP <</N ' + IntToStr ( FShow.PageID ) + ' 0 R >> ' );
    FOwner.FOwner.SaveToStream ( CalcActions + '>>' );
    FOwner.FOwner.CloseHObj;
    FOwner.FOwner.CloseObj;
  finally
    FShow.Free;
  end;
end;

procedure TPDFEdit.SetMaxLength ( const Value: Integer );
begin
  if Value < 0 then
    FMaxLength := 0
  else
    FMaxLength := Value;
end;

{ TPDFComboBox }

constructor TPDFComboBox.Create ( AOwner: TPDFPage; AName: string );
begin
  inherited Create ( AOwner, AName );
  FItems := TStringList.Create;
  FEditEnabled := True;
  FBorderColor := clBlack;
  FColor := clWhite;
end;

destructor TPDFComboBox.Destroy;
begin
  inherited;
  FItems.Free;
end;

procedure TPDFComboBox.Paint;
var
  x, y, z: array [ 0..3 ] of Byte;
  i: Integer;
begin
  i := ColorToRGB ( FColor );
  Move ( i, z [ 0 ], 4 );
  i := ColorToRGB ( FBorderColor );
  Move ( i, y [ 0 ], 4 );
  I := ColorToRGB ( FFont.Color );
  Move ( i, x [ 0 ], 4 );
  with FShow do
  begin
    Width := abs ( FRight - FLeft );
    Height := abs ( FBottom - FTop );
    FEmulationEnabled := False;
    SetLineWidth ( 1 );
    SetRGBColorFill ( z [ 0 ] / 255, z [ 1 ] / 255, z [ 2 ] / 255 );
    SetRGBColorStroke ( y [ 0 ] / 255, y [ 1 ] / 255, y [ 2 ] / 255 );
    Rectangle ( 0, 0, Width, Height );
    FillAndStroke;
    NewPath;
    Rectangle ( 0, 0, Width, Height );
    Clip;
    NewPath;
    AppendAction ( '/Tx BMC' );
    BeginText;
    SetRGBColorFill ( x [ 0 ] / 255, X [ 1 ] / 255, X [ 2 ] / 255 );
    SetActiveFont ( FFont.Name, FFont.Style, FFont.Size, 0 );
    SetCurrentFont ( False );
    FCurrentFont.SetAllANSI;
    FFN := FCurrentFont.AliasName;
    if FText <> '' then
      TextBox ( Rect ( 2, 2, Width - 2, Height - 2 ), FText, hjLeft, vjCenter );
    EndText;
    AppendAction ( 'EMC' );
  end;
end;

procedure TPDFComboBox.Save;
var
  i, j: Integer;
begin
  FShow := TPDFPage.Create ( FOwner.FOwner );
  try
    FShow.FIsForm := True;
    FShow.FRemoveCR := True;
    Paint;
    for i := 0 to Length ( FShow.FLinkedFont ) - 1 do
      if FOwner.FOwner.FAcroForm.FFonts.IndexOf ( FShow.FLinkedFont [ i ] ) = -1 then
        FOwner.FOwner.FAcroForm.FFonts.Add ( FShow.FLinkedFont [ i ] );
    FShow.Save;
    FOwner.FOwner.StartObj ( AnnotID );
    FOwner.FOwner.SaveToStream ( '/Type /Annot' );
    FOwner.FOwner.SaveToStream ( '/Subtype /Widget' );
    FOwner.FOwner.SaveToStream ( '/Rect [' + IntToStr ( Round ( FLeft ) ) + ' ' + IntToStr ( FBottom ) +
      ' ' + IntToStr ( FRight ) + ' ' + IntToStr ( FTop ) + ']' );
    FOwner.FOwner.SaveToStream ( '/FT /Ch' );
    FOwner.FOwner.SaveToStream ( '/F ' + IntToStr ( CalcFlags ) );
    FOwner.FOwner.SaveToStream ( '/P ' + IntToStr ( FOwner.PageID ) + ' 0 R' );
    FOwner.FOwner.SaveToStream ( '/T (' + EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, Name ) ) + ')' );
    i := 0;
    if FReadOnly then
      i := i or 1;
    if FRequired then
      i := i or 2;
    j := 1 shl 17;
    i := i or j;
    if FEditEnabled then
    begin
      j := 1 shl 18;
      i := i or j;
    end;
    FOwner.FOwner.SaveToStream ( '/Ff ' + IntToStr ( i ) );
    FOwner.FOwner.SaveToStream ( '/Opt [', False );
    for i := 0 to Items.Count - 1 do
      FOwner.FOwner.SaveToStream ( '(' + EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, Items [ i ] ) ) + ')' );
    FOwner.FOwner.SaveToStream ( ']' );
    FOwner.FOwner.SaveToStream ( '/F 4' );
    if FText <> '' then
      FOwner.FOwner.SaveToStream ( '/V (' + EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, FText ) ) + ')' );
    if FText <> '' then
      FOwner.FOwner.SaveToStream ( '/DV (' + EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, FText ) ) + ')' );
    FOwner.FOwner.SaveToStream ( '/DA (' + EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled,
      FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, '/' + FFN + ' ' + IntToStr ( FFont.Size ) ) ) + ' Tf ' +
      FormatFloat ( GetRValue ( FFont.Color ) / 255 ) + ' ' +
      FormatFloat ( GetGValue ( FFont.Color ) / 255 ) + ' ' + FormatFloat ( GetBValue ( FFont.Color ) / 255 ) + ' rg)' );
    FOwner.FOwner.SaveToStream ( '/AP <</N ' + IntToStr ( FShow.PageID ) + ' 0 R >> ' );
    FOwner.FOwner.SaveToStream ( CalcActions + '>>' );
    FOwner.FOwner.CloseHObj;
    FOwner.FOwner.CloseObj;
  finally
    FShow.Free;
  end;
end;

{ TPDFRadioButton }

constructor TPDFRadioButton.Create ( AOwner: TPDFPage; AName: string );
var
  I: Integer;
  F: Boolean;
begin
  inherited Create ( AOwner, AName );
  FExportValue := '';
  FColor := clWhite;
  F := False;
  for I := 0 to FOwner.FOwner.FRadioGroups.Count - 1 do
    if UpperCase ( AName ) = UpperCase ( TPDFRadioGroup ( FOwner.FOwner.FRadioGroups [ I ] ).FName ) then
    begin
      FRG := TPDFRadioGroup ( FOwner.FOwner.FRadioGroups [ I ] );
      F := True;
      Break;
    end;
  if not F then
    FRG := FOwner.FOwner.CreateRadioGroup ( AName );
  FRG.FButtons.Add ( Self );
  ExportValue := '';
end;

procedure TPDFRadioButton.Paint;
var
  x, y, z: array [ 0..3 ] of Byte;
  i: Integer;
begin
  I := ColorToRGB ( FFont.Color );
  Move ( i, x [ 0 ], 4 );
  i := ColorToRGB ( FColor );
  Move ( i, z [ 0 ], 4 );
  i := ColorToRGB ( FBorderColor );
  Move ( i, y [ 0 ], 4 );
  with FCheck do
  begin
    Width := abs ( FRight - FLeft );
    Height := abs ( FBottom - FTop );
    SetLineWidth ( 1 );
    SetRGBColorFill ( z [ 0 ] / 255, z [ 1 ] / 255, z [ 2 ] / 255 );
    SetRGBColorStroke ( y [ 0 ] / 255, y [ 1 ] / 255, y [ 2 ] / 255 );
    Circle ( Width / 2, Height / 2, Height / 2 - 0.5 );
    FillAndStroke;
    SetRGBColorFill ( x [ 0 ] / 255, X [ 1 ] / 255, X [ 2 ] / 255 );
    Circle ( Width / 2, Height / 2, Height / 4 - 0.5 );
    Fill;
    BeginText;
    SetActiveFont ( 'ZapfDingbats', [ ], 10 );
    SetCurrentFont ( False );
    FFN := FCurrentFont.AliasName;
    EndText;
  end;
  with FUncheck do
  begin
    Width := abs ( FRight - FLeft );
    Height := abs ( FBottom - FTop );
    SetLineWidth ( 1 );
    SetRGBColorFill ( z [ 0 ] / 255, z [ 1 ] / 255, z [ 2 ] / 255 );
    SetRGBColorStroke ( y [ 0 ] / 255, y [ 1 ] / 255, y [ 2 ] / 255 );
    Circle ( Width / 2, Height / 2, Height / 2 - 0.5 );
    FillAndStroke;
  end;
end;

procedure TPDFRadioButton.Save;
var
  i: Integer;
begin
  FCheck := TPDFPage.Create ( FOwner.FOwner );
  try
    FCheck.FIsForm := True;
    FUnCheck := TPDFPage.Create ( FOwner.FOwner );
    try
      FUnCheck.FIsForm := True;
      Paint;
      FCheck.Save;
      FUnCheck.Save;
      for i := 0 to Length ( FCheck.FLinkedFont ) - 1 do
        if FOwner.FOwner.FAcroForm.FFonts.IndexOf ( FCheck.FLinkedFont [ i ] ) = -1 then
          FOwner.FOwner.FAcroForm.FFonts.Add ( FCheck.FLinkedFont [ i ] );
      FOwner.FOwner.StartObj ( AnnotID );
      FOwner.FOwner.SaveToStream ( '/Type /Annot' );
      FOwner.FOwner.SaveToStream ( '/Subtype /Widget' );
      FOwner.FOwner.SaveToStream ( '/Rect [' + IntToStr ( Round ( FLeft ) ) + ' ' + IntToStr ( FBottom ) +
        ' ' + IntToStr ( FRight ) + ' ' + IntToStr ( FTop ) + ']' );
      FOwner.FOwner.SaveToStream ( '/P ' + IntToStr ( FOwner.PageID ) + ' 0 R' );
      if FChecked then
        FOwner.FOwner.SaveToStream ( '/AS /' + FExportValue )
      else
        FOwner.FOwner.SaveToStream ( '/AS /Off' );
      FOwner.FOwner.SaveToStream ( '/MK <</CA (' + EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, 'l' ) ) + ') ', False );
      FOwner.FOwner.SaveToStream ( '/AC (' + EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, '?' ) ) + ')/RC (' +
        EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled, FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, '?' ) ) + ')', False );
      FOwner.FOwner.SaveToStream ( '/BC [' + FormatFloat ( GetRValue ( FBorderColor ) / 255 ) + ' ' +
        FormatFloat ( GetGValue ( FBorderColor ) / 255 ) + ' ' + FormatFloat ( GetBValue ( FBorderColor ) / 255 ) + ' ]', False );
      FOwner.FOwner.SaveToStream ( '/BG [' + FormatFloat ( GetRValue ( FColor ) / 255 ) + ' ' +
        FormatFloat ( GetGValue ( FColor ) / 255 ) + ' ' + FormatFloat ( GetBValue ( FColor ) / 255 ) + ' ]', False );
      FOwner.FOwner.SaveToStream ( '>>' );
      FOwner.FOwner.SaveToStream ( '/DA (' + EscapeSpecialChar ( EnCodeString ( FOwner.FOwner.FProtectionEnabled,
        FOwner.FOwner.FKey, FOwner.FOwner.FPKL, AnnotID, '/' + FFN + ' 0' ) ) + ' Tf ' +
        FormatFloat ( GetRValue ( FFont.Color ) / 255 ) + ' ' +
        FormatFloat ( GetGValue ( FFont.Color ) / 255 ) + ' ' + FormatFloat ( GetBValue ( FFont.Color ) / 255 ) + ' rg)' );
      FOwner.FOwner.SaveToStream ( '/F ' + IntToStr ( CalcFlags ) );
      FOwner.FOwner.SaveToStream ( '/Parent ' + IntToStr ( FRG.GroupID ) + ' 0 R' );
      FOwner.FOwner.SaveToStream ( '/AP <</N << /' + FExportValue + ' ' + IntToStr ( FCheck.PageID ) + ' 0 R ', False );
      FOwner.FOwner.SaveToStream ( '/Off  ' + IntToStr ( FUnCheck.PageID ) + ' 0 R >> ', False );
      FOwner.FOwner.SaveToStream ( '/D << /' + FName + ' ' + IntToStr ( FCheck.PageID ) + ' 0 R ', False );
      FOwner.FOwner.SaveToStream ( '/Off  ' + IntToStr ( FUnCheck.PageID ) + ' 0 R >> ', False );
      FOwner.FOwner.SaveToStream ( '>>' );
      FOwner.FOwner.SaveToStream ( '/H /T' );
      FOwner.FOwner.SaveToStream ( CalcActions + '>>' );
      FOwner.FOwner.CloseHObj;
      FOwner.FOwner.CloseObj;
    finally
      FUnCheck.Free;
    end;
  finally
    FCheck.Free;
  end;
end;

procedure TPDFRadioButton.SetExportValue ( const Value: string );
var
  I: Integer;
  WS: string;
begin
  WS := ReplStr ( Value, ' ', '_' );
  if WS = '' then
    if FExportValue <> '' then
      raise TPDFException.Create ( SCannotSetEmptyExportValue )
    else
      FExportValue := FName + IntToStr ( FRG.FButtons.Count )
  else
    for I := 0 to FRG.FButtons.Count - 1 do
      if UpperCase ( TPDFRadioButton ( FRG.FButtons [ I ] ).FName ) = UpperCase ( WS ) then
        raise TPDFException.Create ( SExportValuePresent );
  FExportValue := WS;
end;

{ TPDFRadioGroup }

constructor TPDFRadioGroup.Create ( AOwner: TPDFDocument; Name: string );
begin
  FOwner := AOwner;
  FName := Name;
  FButtons := TPDFControls.Create;
end;

destructor TPDFRadioGroup.Destroy;
begin
  FButtons.Free;
  inherited;
end;

procedure TPDFRadioGroup.Save;
var
  I: Integer;
begin
  FOwner.FAcroForm.FRadioGroups.Add ( Self );
  FOwner.StartObj ( GroupID );
  FOwner.SaveToStream ( '/FT /Btn' );
  FOwner.SaveToStream ( '/T (' + EscapeSpecialChar ( EnCodeString ( FOwner.FProtectionEnabled, FOwner.FKey, FOwner.FPKL, GroupID, FName ) ) + ')' );
  for I := 0 to FButtons.Count - 1 do
    if TPDFRadioButton ( FButtons [ I ] ).FChecked then
    begin
      FOwner.SaveToStream ( '/V /' + TPDFRadioButton ( FButtons [ I ] ).FExportValue );
      FOwner.SaveToStream ( '/DV /' + TPDFRadioButton ( FButtons [ I ] ).FExportValue );
      Break;
    end;
  FOwner.SaveToStream ( '/Kids [', False );
  for I := 0 to FButtons.Count - 1 do
    FOwner.SaveToStream ( IntToStr ( TPDFRadioButton ( FButtons [ I ] ).AnnotID ) + ' 0 R ', False );
  FOwner.SaveToStream ( ']' );
  I := 0;
  if TPDFRadioButton ( FButtons [ 0 ] ).FReadOnly then
    i := i or 1;
  if TPDFRadioButton ( FButtons [ 0 ] ).FRequired then
    i := i or 2;
  if FButtons.Count <> 1 then
    I := I or ( 1 shl 14 );
  I := I or ( 1 shl 15 );
  FOwner.SaveToStream ( '/Ff ' + IntToStr ( I ) );
  FOwner.CloseHObj;
  FOwner.CloseObj;
end;

{ TPDFGoToRemoteAction }

procedure TPDFGoToRemoteAction.Save;
var
  S: string;
begin
  if FInNewWindow then
    S := 'true'
  else
    S := 'false';
  FOwner.StartObj ( ActionID );
  FOwner.SaveToStream ( '/S /GoToR /D [' + IntToStr ( FPageIndex ) +
    ' /FitB] /F <</Type /Filespec /F (' + EscapeSpecialChar ( EnCodeString ( FOwner.FProtectionEnabled,
    FOwner.FKey, FOwner.FPKL, ActionID, PrepareFileSpec ( FDocument ) ) ) + ') >>' +
    '/NewWindow ' + s );
  if FNext <> nil then
    FOwner.SaveToStream ( '/Next ' + IntToStr ( FNext.ActionID ) + ' 0 R' );
  FOwner.CloseHObj;
  FOwner.CloseObj;
end;

procedure TPDFGoToRemoteAction.SetDocument ( const Value: string );
begin
  FDocument := Value;
end;

procedure TPDFGoToRemoteAction.SetInNewWindow ( const Value: Boolean );
begin
  FInNewWindow := Value;
end;

procedure TPDFGoToRemoteAction.SetPageIndex ( const Value: Integer );
begin
  FPageIndex := Value;
end;

{ TPDFNamedDestinationAction }

procedure TPDFNamedDestinationAction.Save;
var
  S: string;
begin
  FOwner.StartObj ( ActionID );
  if FDocument = '' then
  begin
    FOwner.SaveToStream ( '/S /GoTo /D (' +
      EscapeSpecialChar ( EnCodeString ( FOwner.FProtectionEnabled, FOwner.FKey, FOwner.FPKL, ActionID, FDestination ) )
      + ')' );
  end
  else
  begin
    if FNewWindow then
      S := 'true'
    else
      S := 'false';
    FOwner.SaveToStream ( '/S /GoToR /D (' + EscapeSpecialChar ( EnCodeString ( FOwner.FProtectionEnabled,
      FOwner.FKey, FOwner.FPKL, ActionID, FDestination ) ) +
      ') /F <</Type /Filespec /F (' + EscapeSpecialChar ( EnCodeString ( FOwner.FProtectionEnabled,
      FOwner.FKey, FOwner.FPKL, ActionID, PrepareFileSpec ( FDocument ) ) ) + ') >>' +
      '/NewWindow ' + s );
  end;
  if FNext <> nil then
    FOwner.SaveToStream ( '/Next ' + IntToStr ( FNext.ActionID ) + ' 0 R' );
  FOwner.CloseHObj;
  FOwner.CloseObj;
end;

procedure TPDFNamedDestinationAction.SetDestination ( const Value: string );
begin
  FDestination := Value;
end;

procedure TPDFNamedDestinationAction.SetDocument ( const Value: string );
begin
  FDocument := Value;
end;

procedure TPDFNamedDestinationAction.SetNewWindow ( const Value: Boolean );
begin
  FNewWindow := Value;
end;



{$IFDEF LLPDFEVAL}

procedure InitTrial;
begin
  RC4Init ( KeyData, @ComponentName [ 1 ], Length ( ComponentName ) );
  SetLength ( s1, SizeOf ( sopp1 ) );
  RC4Reset ( KeyData );
  RC4Crypt ( KeyData, @sopp1 [ 1 ], @s1 [ 1 ], SizeOf ( sopp1 ) );
  SetLength ( s2, SizeOf ( sopp2 ) );
  RC4Reset ( KeyData );
  RC4Crypt ( KeyData, @sopp2 [ 1 ], @s2 [ 1 ], SizeOf ( sopp2 ) );
  SetLength ( s3, SizeOf ( sopp3 ) );
  RC4Reset ( KeyData );
  RC4Crypt ( KeyData, @sopp3 [ 1 ], @s3 [ 1 ], SizeOf ( sopp3 ) );
  RC4Burn ( KeyData );
end;
{$ENDIF}


initialization
  begin
{$IFDEF LLPDFEVAL}
    InitTrial;
{$ENDIF}
  end;
end.

