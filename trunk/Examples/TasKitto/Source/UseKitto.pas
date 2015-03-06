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
  Kitto.Ext.ADOTools, //For Excel export
  Kitto.Ext.DebenuQuickPDFTools, //For PDF Merge
  Kitto.Ext.FOPTools, //For FOP Engine
  Kitto.Ext.XSLTools, //For XSL Transformation
  Kitto.Localization.dxgettext,
  Kitto.Metadata.ModelImplementation,
  Kitto.Metadata.ViewBuilders,
  Kitto.Ext.All
  ;

implementation

end.
