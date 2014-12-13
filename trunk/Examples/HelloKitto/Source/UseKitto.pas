unit UseKitto;

interface

uses
  EF.DB.ADO,
  EF.DB.DBX,
{$IFDEF D20+}
  EF.DB.FD, //FireDac support
  FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLMeta, //FireDac support for MS-SQL
  FireDAC.Phys.IBBase, FireDAC.Phys.FB, //FireDac support for Firebird
{$ENDIF}
  // Kitto.AccessControl.DB,
  Kitto.Auth.DB,
  // Kitto.Auth.DBServer,
  // Kitto.Auth.OSDB,
  // Kitto.Auth.TextFile,
  Kitto.Ext.ADO.Tools, //For Excel export
  Kitto.Ext.DebenuQuickPDF.Tools, //For PDF Merge
  Kitto.Ext.FOP.Tools, //For FOP Engine
  Kitto.Localization.dxgettext,
  Kitto.Ext.TilePanel,
  Kitto.Metadata.ModelImplementation,
  Kitto.Metadata.ViewBuilders,
  Kitto.Ext.All
  ;

implementation

end.
