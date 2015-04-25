{-------------------------------------------------------------------------------
   Copyright 2013 Ethea S.r.l.

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

unit Kitto.Ext.SQLTool;

interface

uses
  SysUtils, Classes, Data.DB,
  Kitto.Metadata.DataView,
  Kitto.Ext.Base, Kitto.Ext.DataTool;

type
  ///	<summary>
  ///  Base class to execute a SQL Command
  /// </summary>
  TKExtDataExecSQLCmdToolController = class(TKExtDataToolController)
  strict private
    function GetSQLCommandText: string;
    function GetDatabaseName: string;
  strict protected
    procedure ExecuteTool; override;
    procedure AssignParamValue(const AParam: TParam;var AValue: Variant);
  public
    class function GetDefaultImageName: string; override;
  published
    property SQLCommandText: string read GetSQLCommandText;
    property DatabaseName: string read GetDatabaseName;
  end;

  ///	<summary>
  ///  Base class to execute a Stored Procedure
  /// </summary>
(*
  TKExtDataExecSPToolController = class(TKExtDataToolController)
  strict private
    function GetStoredProcName: string;
  strict
  private
    function GetDatabaseName: string; protected
    procedure ExecuteTool; override;
    procedure AssignParamValue(const AParam: TParam;var AValue: Variant);
  public
    class function GetDefaultImageName: string; override;
  published
    property StoredProcName: string read GetStoredProcName;
    property DatabaseName: string read GetDatabaseName;
  end;
*)

implementation

uses
  StrUtils,
  EF.Tree, EF.DB, EF.StrUtils, EF.SysUtils, EF.Localization, EF.Macros,
  Kitto.DatabaseRouter, Kitto.Config, Kitto.Ext.Session, Kitto.Ext.Controller;

{ TKExtDataExecSQLCmdToolController }

procedure TKExtDataExecSQLCmdToolController.AssignParamValue(const AParam: TParam; var AValue: Variant);
var
  LNode: TEFNode;
  LRecord: TKViewTableRecord;

  function ExpandExpression(const AExpression: string): string;
  begin
    if Assigned(LRecord) then
    begin
      Result := LRecord.ExpandFieldJSONValues(AExpression, True);
      Result := TEFMacroExpansionEngine.Instance.Expand(Result);
    end
    else
      Result := TEFMacroExpansionEngine.Instance.Expand(Result);
  end;

begin
  LRecord := ServerRecord;

  //By default Param Values are assigned by the Parameters node values:
  LNode := Config.FindNode('Parameters/'+AParam.Name);
  if Assigned(LNode) then
  begin
    AValue := ExpandExpression(LNode.AsString);
  end;
end;

procedure TKExtDataExecSQLCmdToolController.ExecuteTool;
var
  LCommandText: string;
  LProcedure: TEFDBCommand;
  LDBConnection: TEFDBConnection;
  I: Integer;
  LParam: TParam;
  LParamNode: TEFNode;
  LSuccessNode: TEFNode;
  LFailureNode: TEFNode;
  LSuccess: Boolean;
  LSuccessMessage, LErrorMessage: string;
  LExpandedValue: string;

  function ExpandExpression(const AExpression: string): string;
  begin
    Result := AExpression;
    if Assigned(ServerRecord) then
      Result := ServerRecord.ExpandFieldJSONValues(Result, True);
    Result := TEFMacroExpansionEngine.Instance.Expand(Result);
  end;

begin
  inherited;
  LDBConnection := TKConfig.Instance.DBConnections[DatabaseName];
  LCommandText := Config.GetString('SQLCommandText');
  Assert(LCommandText <> '','SQLCommandText is mandatory');
  LProcedure := LDBConnection.CreateDBCommand;
  LProcedure.CommandText := LCommandText;
  //Assign Input param values
  for I := 0 to LProcedure.Params.Count -1 do
  begin
    LParam := LProcedure.Params[I];
    LParamNode := Config.FindNode('InputParams/'+LParam.Name);
    if Assigned(LParamNode) then
    begin
      LExpandedValue := ExpandExpression(LParamNode.GetString('Value'));
      if not SameText(LExpandedValue, LParamNode.AsString) then
        LParam.AsString := LExpandedValue
      else
        LParamNode.AssignToParam(LParam);
    end;
  end;

  //Execute Command
  ExecuteInTransaction(
    procedure
    var
      I: Integer;

      function ExpandParamNode(const AExpression: string): string;
      var
        I: Integer;
        LParam: TParam;
      begin
        Result := AExpression;
        for I := 0 to LProcedure.Params.Count - 1 do
        begin
          LParam := LProcedure.Params[I];
          Result := ReplaceText(Result, '{' + LParam.Name + '}', LParam.AsString);
        end;
      end;

    begin
      LProcedure.Execute;

      //Read output parameters
      LSuccess := True;
      for I := 0 to LProcedure.Params.Count -1 do
      begin
        LParam := LProcedure.Params[I];
        LParamNode := Config.FindNode('OutputParams/'+LParam.Name);
        if Assigned(LParamNode) then
        begin
          LParamNode.Value := LParam.Value;
          LSuccessNode := LParamNode.FindNode('SuccessValue');
          LFailureNode := LParamNode.FindNode('FailureValue');
          if Assigned(LFailureNode) then
            LSuccess := LFailureNode.Value <> LParam.Value
          else if Assigned(LSuccessNode) then
            LSuccess := LSuccessNode.Value = LParam.Value;
        end;
      end;

      if LSuccess then
      begin
        LSuccessMessage := ExpandParamNode(Config.GetString('SuccessMessageTemplate',
          Format(_('Command %s executed succesfully!'), [DisplayLabel])));
        Session.Flash(LSuccessMessage);
      end
      else
      begin
        LErrorMessage := ExpandParamNode(Config.GetString('ErrorMessageTemplate',
          Format(_('Error executing command %s!'), [DisplayLabel])));
        raise Exception.Create(LErrorMessage);
      end;
    end);
end;

function TKExtDataExecSQLCmdToolController.GetSQLCommandText: string;
begin
  Result := Config.GetExpandedString('SQLCommandText');
end;

function TKExtDataExecSQLCmdToolController.GetDatabaseName: string;
begin
  Result := Config.GetString('DatabaseName', ViewTable.DatabaseName);
end;

class function TKExtDataExecSQLCmdToolController.GetDefaultImageName: string;
begin
  Result := 'exec_sqlcommand';
end;

{ TKExtDataExecSPToolController }
(*
procedure TKExtDataExecSPToolController.AssignParamValue(const AParam: TParam; var AValue: Variant);
var
  LNode: TEFNode;
  LRecord: TKViewTableRecord;

  function ExpandExpression(const AExpression: string): string;
  begin
    if Assigned(LRecord) then
    begin
      Result := LRecord.ExpandFieldJSONValues(AExpression, True);
      Result := TEFMacroExpansionEngine.Instance.Expand(Result);
    end
    else
      Result := TEFMacroExpansionEngine.Instance.Expand(Result);
  end;

begin
  LRecord := ServerRecord;

  //By default Param Values are assigned by the Parameters node values:
  LNode := Config.FindNode('Parameters/'+AParam.Name);
  if Assigned(LNode) then
  begin
    AValue := ExpandExpression(LNode.AsString);
  end;
end;

procedure TKExtDataExecSPToolController.ExecuteTool;
var
  LCommandText, LStoredProcName: string;
  LProcedure: TEFDBCommand;
  LDBConnection: TEFDBConnection;
  I: Integer;
  LParam: TParam;
  AValue: Variant;
begin
  inherited;
  LDBConnection := TKConfig.Instance.DBConnections[DatabaseName];
  LStoredProcName := Config.GetString('StoredProcName');
  Assert(LStoredProcName <> '','StoredProcName is mandatory');
  LProcedure := LDBConnection.CreateDBCommand;
  LCommandText := 'execute '+LStoredProcName;
  LProcedure.CommandText := LCommandText;
  for I := 0 to LProcedure.Params.Count -1 do
  begin
    LParam := LProcedure.Params[I];
    if LParam.ParamType in [ptInput, ptInputOutput] then
      AssignParamValue(LParam, AValue);
  end;
  if LProcedure.Execute <> 0 then
    raise Exception.CreateFmt('Error executing %s', [ExtractFileName(StoredProcName)]);
end;

function TKExtDataExecSPToolController.GetStoredProcName: string;
begin
  Result := Config.GetExpandedString('StoredProcName');
end;

function TKExtDataExecSPToolController.GetDatabaseName: string;
begin
  Result := Config.GetString('DatabaseName', ViewTable.DatabaseName);
end;

class function TKExtDataExecSPToolController.GetDefaultImageName: string;
begin
  Result := 'exec_storedproc';
end;
*)

initialization
//  TKExtControllerRegistry.Instance.RegisterClass('ExecuteStoredProc', TKExtDataExecSPToolController);
  TKExtControllerRegistry.Instance.RegisterClass('ExecuteSQLCommand', TKExtDataExecSQLCmdToolController);

finalization
//  TKExtControllerRegistry.Instance.UnregisterClass('ExecuteStoredProc');
  TKExtControllerRegistry.Instance.UnregisterClass('ExecuteSQLCommand');

end.
