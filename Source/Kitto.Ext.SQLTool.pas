{-------------------------------------------------------------------------------
   Copyright 2012-2018 Ethea S.r.l.

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
  SysUtils
  , Classes
  , Data.DB
  , Kitto.Metadata.DataView
  , Kitto.Ext.Base
  , Kitto.Ext.DataTool
  ;

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
  //published
    property SQLCommandText: string read GetSQLCommandText;
    property DatabaseName: string read GetDatabaseName;
  end;

implementation

uses
  StrUtils
  , EF.Tree
  , EF.DB
  , EF.StrUtils
  , EF.Sys
  , EF.Localization
  , EF.Macros
  , Kitto.DatabaseRouter
  , Kitto.Config
  , Kitto.Web.Application
  , Kitto.Ext.Controller;

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
  LDBCommand: TEFDBCommand;
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

  function ExpandParamNode(const AExpression: string): string;
  var
    I: Integer;
    LParam: TParam;
  begin
    Result := AExpression;
    for I := 0 to LDBCommand.Params.Count - 1 do
    begin
      LParam := LDBCommand.Params[I];
      Result := ReplaceText(Result, '{' + LParam.Name + '}', LParam.AsString);
    end;
  end;

begin
  inherited;
  LDBConnection := TKConfig.Instance.CreateDBConnection(DatabaseName);
  try
    LDBConnection.StartTransaction;
    try
      LCommandText := Config.GetString('SQLCommandText');
      Assert(LCommandText <> '','SQLCommandText is mandatory');

      LDBCommand := LDBConnection.CreateDBCommand;
      try
        LDBCommand.CommandText := LCommandText;
        // Assign input param values
        for I := 0 to LDBCommand.Params.Count - 1 do
        begin
          LParam := LDBCommand.Params[I];
          LParamNode := Config.FindNode('InputParams/' + LParam.Name);
          if Assigned(LParamNode) then
          begin
            LExpandedValue := ExpandExpression(LParamNode.GetString('Value'));
            if not SameText(LExpandedValue, LParamNode.AsString) then
              LParam.AsString := LExpandedValue
            else
              LParamNode.AssignToParam(LParam);
          end;
        end;

        LDBCommand.Execute;

        // Read output parameters
        LSuccess := True;
        for I := 0 to LDBCommand.Params.Count - 1 do
        begin
          LParam := LDBCommand.Params[I];
          LParamNode := Config.FindNode('OutputParams/' + LParam.Name);
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
          TKWebApplication.Current.Toast(LSuccessMessage);
        end
        else
        begin
          LErrorMessage := ExpandParamNode(Config.GetString('ErrorMessageTemplate',
            Format(_('Error executing command %s!'), [DisplayLabel])));
          raise Exception.Create(LErrorMessage);
        end;
        LDBConnection.CommitTransaction;
      finally
        FreeAndNil(LDBCommand);
      end;
    except
      LDBConnection.RollbackTransaction;
      raise;
    end;
  finally
    FreeAndNil(LDBConnection);
  end;
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

initialization
  TKExtControllerRegistry.Instance.RegisterClass('ExecuteSQLCommand', TKExtDataExecSQLCmdToolController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('ExecuteSQLCommand');

end.
