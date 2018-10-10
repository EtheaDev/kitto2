unit UseKitto;

{$I Kitto.Defines.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  // EF.DB.ADO,
  EF.DB.DBX,
  DBXFirebird,
  {$ENDIF}
  EF.DB.FD,
  // Kitto.AccessControl.DB,
  // Kitto.Auth.DB,
  // Kitto.Auth.DBServer,
  // Kitto.Auth.OSDB,
  Kitto.Auth.TextFile,
  {$IFDEF MSWINDOWS}
  Kitto.Ext.ADOTools, //For Excel export
  {$ENDIF}
  // Kitto.Localization.dxgettext, //Commented to enable per-session localization
  Kitto.Metadata.ModelImplementation,
  Kitto.Metadata.ViewBuilders,
  Kitto.Ext.All
  ;

implementation

end.
