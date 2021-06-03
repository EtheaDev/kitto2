{-------------------------------------------------------------------------------
   Copyright 2012-2021 Ethea S.r.l.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-------------------------------------------------------------------------------}
unit KIDE.UseKitto;

interface

uses
  EF.DB.ADO,
  EF.DB.DBX,
  EF.DB.FD, //FireDac support
  FireDAC.Phys,
  FireDAC.Phys.Intf,
  FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLMeta, //FireDac support for MS-SQL
  FireDAC.Phys.IBMeta, FireDAC.Phys.IBBase, FireDAC.Phys.IBDef, FireDAC.Phys.IB, FireDAC.Phys.FB, //FireDac support for Interbase and Firebird
  Kitto.AccessControl.DB,
  Kitto.Auth.DB,
  Kitto.Auth.DBServer,
  Kitto.Auth.OSDB,
  Kitto.Auth.TextFile,
  Kitto.Ext.IndyTools, //For indy tools
  Kitto.Ext.DataTool, //For export tools
  Kitto.Ext.ADOTools, //For Excel export
  Kitto.Ext.XSLTools, //For XML export
  Kitto.Ext.DebenuQuickPDFTools, //For PDF Merge
  Kitto.Ext.FOPTools, //For FOP Engine
  Kitto.Ext.SQLTool, //For SQL Tool
  Kitto.Localization.dxgettext,
  Kitto.Ext.TilePanel,
  Kitto.Metadata.ModelImplementation,
  Kitto.Metadata.ViewBuilders,
  Kitto.Ext.All
  ;

implementation

end.
