
{*****************************************}
{                                         }
{          Report Machine v3.0            }
{              Include file               }
{                                         }
{*****************************************}

{$DEFINE USE_INTERNAL_JVCL}
//{$DEFINE JVCLCTLS}

{$IFDEF JVCLCTLS}
  {$UNDEF USE_INTERNAL_JVCL}
{$ENDIF}

{$IFDEF USE_INTERNAL_JVCL}
  {$I rm_jvcl.inc}
{$ELSE}
  {$I jvcl.inc}
{$ENDIF}

//{$DEFINE TrialVersion}
{$DEFINE RM_ENTVersion}
//{$DEFINE RM_WEBREPORT}
//{$DEFINE RM_COMVERSION}

{$DEFINE llPDFLib} // Use llPDFLib to Export PDF File
//{$DEFINE USE_IMAGEEN}  // Use ImageEn to Export multi page Tiff File
//{$DEFINE TntUnicode} // Use TntUnicode, Support Unicode
{$DEFINE USE_TABSET}

//-------------------- Use SynEdit -------------------------------------
//{$DEFINE_USE_INTERNAL_SYNEDIT}
//{$DEFINE USE_OUTSIDE_SYNEDIT}

//-------------------- Raize ---------------------------------------------------
//{$DEFINE Raize}

//-------------------- FlatStyle -----------------------------------------------
//如果使用FlatStyle
//{$DEFINE FlatStyle}

//-------------------- psoft Barcode library------------------------------------
{$DEFINE USE_EANBARCODE}

{$B-} {- Complete Boolean Evaluation }
{$R-} {- Range-Checking }
{$V-} {- Var-String Checking }
{$T-} {- Typed @ operator }
{$X+} {- Extended syntax }
{$P+} {- Open string params }
{$J+} {- Writeable structured consts }
{$H+} {- Use long strings by default }

{$IFDEF BCB}
{$OBJEXPORTALL on}
{$ENDIF}

//------------------- GIF images -----------------------------------
// GIF images - need RXLib. Adds approximately
// If you want to show GIF images in "Picture" object, uncomment
// the following line:
{$DEFINE RXGIF}

//------------------- InfoPower -----------------------------------
// If you want to TwwDBRichEdit, Print TwwDBGrid Use TRMFormReport, uncomment
// the following line:
//{$DEFINE InfoPower}

//-------------------- Use RichEdit 2.0 ---------------------------------
{$DEFINE USE_RICHEDIT_VER_20}

//------------------- TeeChart component ----------------------------
//{$DEFINE TeeChart}
//{$DEFINE TeeChart40}

//--------------------Decision Grid----------------------------------
//{$DEFINE DecisionGrid}

//------------------- JPEG images -----------------------------------
// JPEG images - only available in Delphi 3 and above. Adds approximately
// 100Kb to your EXE.
// If you want to show jpeg images in "Picture" object, uncomment
// the following line:
{$DEFINE JPEG}

//------------------- "Open Picture" dialog -------------------------
// Only available in Delphi 3 and above. Adds approximately 60Kb to your EXE.
// If you want to use standard "Open picture" dialog in picture editor,
// uncomment the following line:
{$DEFINE OPENPICTUREDLG}

//------------------- EHLib 1.54 or Above -----------------------------------
// If you want to Print TDBGridEh Use TRMFormReport, uncomment
// the following line:
//{$DEFINE EHLib}
//{$DEFINE EHLib20}

//------------------ Use TurboPower --------------------------------
{$DEFINE TurboPower}

//-------------------- Zlib ---------------------------------------
//{$DEFINE Zlib}

//-----------------Data Manager -----------------------------
{$DEFINE DM_ADO} //Support ADO
//{$DEFINE DM_BDE} //Support BDE
//{$DEFINE DM_IBX} //Support InterBase Express
{$DEFINE DM_DBX}
//{$DEFINE DM_MIDAS}
//{$DEFINE DM_UNIDAC}
//-------------------------------------------------------------------
{$IFNDEF COMPILER5_UP}
{$UNDEF DM_ADO}
{$UNDEF DM_IBX}
{$ENDIF}

{$IFNDEF COMPILER7_UP}
{$UNDEF DM_DBX}
{$ENDIF}

{$IFNDEF COMPILER4_UP}
{$UNDEF USE_TB2K}
{$ENDIF}

{$IFDEF Raize }
{$UNDEF FlatStyle}
{$UNDEF JVCLCTLS}
{$ENDIF}

{$IFDEF TrialVersion}
{$UNDEF DM_IBX}
{$UNDEF DM_DBX}
{$UNDEF DM_MIDAS}
{$ENDIF}

{$IFNDEF JVCLCTLS}
  {$UNDEF RXGIF}
{$ENDIF}

{$IFNDEF USE_TB2K}
  {$UNDEF USE_TB2K_TBX}
{$ENDIF}

{$IFDEF RM_WEBREPORT}
  {$DEFINE RM_COMVERSION}
{$ENDIF}

{$IFDEF RM_COMVERSION}
  {$DEFINE RM_ENTVersion}
  {$DEFINE llPDFLib}
{$ENDIF}

{$IFDEF RM_COMVERSION}
  {$DEFINE TntUnicode}
{$ENDIF}

{$IFDEF COMPILER12_UP}
  {$UNDEF TntUnicode}
{$ENDIF}

{$IFNDEF COMPILER7_UP}
  {$UNDEF USE_TABSET}
{$ENDIF}


{$IFDEF COMPILER12_UP}
  {$DEFINE USE_INTERNAL_SYNEDIT}
{$ENDIF}

{$IFDEF USE_INTERNAL_SYNEDIT}
  {$DEFINE USE_SYNEDIT}
{$ENDIF}

{$IFDEF USE_OUTSIDE_SYNEDIT}
  {$DEFINE USE_SYNEDIT}
{$ENDIF}
