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
///	<summary>
///	  The view validator should flag:
///	  <list type="bullet">
///	    <item>views without controllers</item>
///	    <item>missing required parameters (requires a means to describe view
///	    metadata, currently not existing)</item>
///	    <item>misplaced nodes (useful to spot typos or when incompatible
///	    changes are done in Kitto)</item>
///	  </list>
///	</summary>
unit KIDE.ViewValidator;

interface

uses
  Kitto.Metadata.Models, Kitto.Metadata.Views, Kitto.Metadata.DataView,
  KIDE.TreeValidator;

type
  TViewValidator = class(TTreeValidator)
  private
    FViews: TKViews;
    FModels: TKModels;
    procedure ValidateView(const AView: TKView);
    procedure ValidateDataView(const ADataView: TKDataView);
    procedure ValidateDataViewFilters(const ADataView: TKDataView);
    procedure ValidateViewTable(const AViewTable: TKViewTable);
    procedure ValidateViewField(const AViewField: TKViewField);
    procedure ValidateFilters(const AViewTable: TKViewTable);
    procedure ValidateGrouping(const AViewTable: TKViewTable);
  protected
    procedure InternalExecute; override;
  public
    procedure ValidateViews(ASingleView: TKView);
  end;

implementation

uses
  SysUtils, StrUtils,
  EF.Tree,
  KIDE.Project;

{ TViewValidator }

procedure TViewValidator.InternalExecute;
begin
  inherited;
  ValidateViews(nil);
end;

procedure TViewValidator.ValidateView(const AView: TKView);
begin
  Assert(Assigned(AView));

  LogIndent;
  try
    Log(Format('Validating view %s...', [AView.PersistentName]));

    ValidateTree(AView);

    if AView.ControllerType = '' then
      LogWarning('Missing Controller. Most (but not all) views need an explicitly specified controller.');

    if AView is TKDataView then
      ValidateDataView(TKDataView(AView));
  finally
    LogOutdent;
  end;
end;

procedure TViewValidator.ValidateViewTable(const AViewTable: TKViewTable);
var
  I: Integer;
  LFieldNode: TEFNode;
begin
  Assert(Assigned(AViewTable));
  Assert(Assigned(FModels));

  LogIndent;
  try
    Log(Format('Validating view table %s...', [AViewTable.ModelName]));

    if AViewTable.ModelName = '' then
      LogError('Missing Model.')
    else if FModels.FindModel(AViewTable.ModelName) = nil then
      LogError(Format('Model %s not found.', [AViewTable.ModelName]));

    ValidateGrouping(AViewTable);

    ValidateFilters(AViewTable);

    for I := 0 to AViewTable.FieldCount - 1 do
      ValidateViewField(AViewTable.Fields[I]);
    for I := 0 to AViewTable.DetailTableCount - 1 do
      ValidateViewTable(AViewTable.DetailTables[I]);
  finally
    LogOutdent;
  end;
end;

procedure TViewValidator.ValidateGrouping(const AViewTable: TKViewTable);
var
  LNode: TEFNode;
  LFieldName: string;
begin
  Assert(Assigned(AViewTable));

  LNode := AViewTable.FindNode('Controller/Grouping');
  if Assigned(LNode) then
  begin
    LFieldName := LNode.GetString('FieldName');
    if LFieldName = '' then
      LogError('Missing Controller/Grouping/FieldName.')
    else if AViewTable.FindField(LFieldName) = nil then
      LogError(Format('Controller/Grouping/FieldName %s not found in view table.', [LFieldName]));
  end;
end;

procedure TViewValidator.ValidateFilters(const AViewTable: TKViewTable);
var
  LNode: TEFNode;
begin
  Assert(Assigned(AViewTable));

  LNode := AViewTable.FindNode('Controller/Filters');
  if Assigned(LNode) then
    LogError('Upgrade needed: Filters node should be moved from <ViewTable>/Controller to Controller.');
end;

procedure TViewValidator.ValidateDataViewFilters(const ADataView: TKDataView);
var
  LNode: TEFNode;
  I: Integer;
  LFilterNode: TEFNode;
  J: Integer;
  LSubItems: TEFNode;
begin
  LNode := ADataView.FindNode('Controller/Filters');
  if Assigned(LNode) then
  begin
    LNode := LNode.FindChild('Items');
    if not Assigned(LNode) then
      LogError('Missing Controller/Filters/Items.')
    else
    begin
      for I := 0 to LNode.ChildCount - 1 do
      begin
        LFilterNode := LNode.Children[I];

        if SameText(LFilterNode.AsString, 'FreeSearch') then
        begin
          if LFilterNode.GetString('ExpressionTemplate') = '' then
            LogError('Missing Controller/Filters/Items/FreeSearch/ExpressionTemplate.');
        end
        else if SameText(LFilterNode.AsString, 'DynaList') then
        begin
          if LFilterNode.GetString('ExpressionTemplate') = '' then
            LogError('Missing Controller/Filters/Items/DynaList/ExpressionTemplate.');
          if LFilterNode.GetString('CommandText') = '' then
            LogError('Missing Controller/Filters/Items/DynaList/CommandText.');
        end
        else if SameText(LFilterNode.AsString, 'List') or SameText(LFilterNode.AsString, 'ButtonList') then
        begin
          LSubItems := LFilterNode.FindNode('Items');
          for J := 0 to LSubItems.ChildCount - 1 do
          begin
            if LSubItems.Children[I].GetString('Expression') = '' then
              LogError(Format('Missing Controller/Filters/Items/ButtonList/Items[%d]/Expression.', [J]));
          end;
        end;
      end;
    end;
  end;
end;

procedure TViewValidator.ValidateDataView(const ADataView: TKDataView);
begin
  Assert(Assigned(ADataView));

  if Assigned(ADataView.FindNode('Controller/List/Layout')) then
    LogError('Upgrade needed: Controller/List/Layout nodes must be changed to Controller/Grid/Layout.');

  ValidateDataViewFilters(ADataView);
  ValidateViewTable(ADataView.MainTable);
end;

procedure TViewValidator.ValidateViewField(const AViewField: TKViewField);
var
  LFieldMessage: string;
  LModelField: TKModelField;
begin
  Assert(Assigned(AViewField));

  LogIndent;
  try
    LFieldMessage := Format('Validating field "%s"', [AViewField.FieldName]);
    LModelField := AViewField.FindModelField;
    if LModelField = nil then
      LogError(Format('%s: Model field "%s" not found.', [LFieldMessage, AViewField.FieldName]))
    else if LModelField.FieldName <> AViewField.FieldName then
      LogError(Format('%s: Case mismatch in Model FieldName "%s".', [LFieldMessage, LModelField.FieldName]));
  finally
    LogOutdent;
  end;
end;

procedure TViewValidator.ValidateViews(ASingleView: TKView);
var
  I: Integer;
  LView: TKView;
begin
  inherited;
  if not Assigned(ASingleView) then
    Log('Validating views...');
  FViews := TProject.CurrentProject.Config.Views;
  FModels := FViews.Models;

  for I := 0 to FViews.ViewCount - 1 do
  begin
    LView := FViews.Views[I];
    if not Assigned(ASingleView) or SameText(ASingleView.PersistentName, LView.PersistentName) then
      ValidateView(LView);
  end;

  if ErrorCount > 0 then
    LogWarning('View validation complete. Errors were detected.')
  else
    LogInfo('View validation complete. No errors detected.');
end;

end.
