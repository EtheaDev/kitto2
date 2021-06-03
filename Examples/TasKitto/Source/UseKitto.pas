unit UseKitto;

{$I Kitto.Defines.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  DBXFirebird,
  EF.DB.ADO,
  EF.DB.DBX,
  {$ENDIF}
  EF.DB.FD,
  // Kitto.AccessControl.DB,
  Kitto.Auth.DB,
  // Kitto.Auth.DBServer,
  // Kitto.Auth.OSDB,
  // Kitto.Auth.TextFile,
  {$IFDEF MSWINDOWS}
  Kitto.Ext.ADOTools, //For Excel import/export
  Kitto.Ext.DebenuQuickPDFTools, //For PDF Merge
  Kitto.Ext.XSLTools, //For XSL Transformation
  {$ENDIF}
  Kitto.Ext.FOPTools, //For FOP Engine
  // Kitto.Localization.dxgettext, //Commented to enable per-session localization
  Kitto.Metadata.ModelImplementation,
  Kitto.Metadata.ViewBuilders,
  Kitto.Ext.CalendarPanel,
  Kitto.Ext.All
  ;

implementation

initialization
{$WARN SYMBOL_PLATFORM OFF}
  // check memory leaks at the end of the app
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
{$WARN SYMBOL_PLATFORM ON}

end.
