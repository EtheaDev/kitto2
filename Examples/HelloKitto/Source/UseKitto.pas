unit UseKitto;

interface

uses
  DBXMSSQL,
  DBXFirebird,
  EF.DB.ADO,
  EF.DB.DBX,
{$IFDEF D20+}
  EF.DB.FD, //FireDac support
  FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLMeta, //FireDac support for MS-SQL
  FireDAC.Phys.IBBase, FireDAC.Phys.FB, //FireDac support for Firebird
{$ENDIF}
  // Kitto.AccessControl.DB,
  //Kitto.Auth.DB,
  // Kitto.Auth.DBServer,
  // Kitto.Auth.OSDB,
  // Kitto.Auth.TextFile,
  Kitto.Ext.ADOTools, //For Excel/Import export
  Kitto.Ext.DebenuQuickPDFTools, //For PDF Merge
  Kitto.Ext.FOPTools, //For FOP Engine
  //Kitto.Ext.ReportBuilderTools, //Tool for Reportbuilder
  Kitto.Localization.dxgettext,
  Kitto.Ext.TilePanel,
  Kitto.Metadata.ModelImplementation,
  Kitto.Metadata.ViewBuilders,
  Kitto.Ext.All
  ;

implementation

end.
