unit UseKitto;

{$I Kitto.Defines.inc}

interface

uses
  EF.DB.ADO,
  EF.DB.DBX,
  // Kitto.AccessControl.DB,
  Kitto.Auth.DB,
  // Kitto.Auth.DBServer,
  // Kitto.Auth.OSDB,
  // Kitto.Auth.TextFile,
  Kitto.Ext.ADO.Tools, //For Excel export
  Kitto.Ext.DebenuQuickPDF.Tools, //For PDF Merge
  Kitto.Ext.FOP.Tools, //For FOP Engine
  Kitto.Localization.dxgettext,
  Kitto.Metadata.ModelImplementation,
  Kitto.Metadata.ViewBuilders,
  Kitto.Ext.All
  ;

implementation

end.
