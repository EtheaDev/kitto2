///	<summary>
///	  The view validator should flag:
///	  <list type="bullet">
///	    <item>views without controllers</item>
///	    <item>missing required parameters (requires a means to describe view
///	    metadata, currently not existing)</item>
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
    procedure ValidateViewTable(const AViewTable: TKViewTable);
    procedure ValidateViewField(const AViewField: TKViewField);
    procedure ValidateFilters(const AViewTable: TKViewTable);
    procedure ValidateGrouping(const AViewTable: TKViewTable);
  protected
    procedure InternalExecute; override;
  end;

implementation

uses
  SysUtils,
  EF.Tree,
  KIDE.Project;

{ TViewValidator }

procedure TViewValidator.InternalExecute;
var
  I: Integer;
begin
  inherited;
  Log('Validating views...');
  FModels := TProject.CurrentProject.Config.Models;
  FViews := TProject.CurrentProject.Config.Views;

  for I := 0 to FViews.ViewCount - 1 do
    ValidateView(FViews.Views[I]);

  if ErrorCount > 0 then
    LogWarning('View validation complete. Errors were detected.')
  else
    LogInfo('View validation complete. No errors detected.');
end;

procedure TViewValidator.ValidateView(const AView: TKView);
begin
  Assert(Assigned(AView));

  LogIndent;
  try
    Log(Format('Validating view %s...', [AView.PersistentName]));

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
  I: Integer;
  LFilterNode: TEFNode;
  J: Integer;
  LSubItems: TEFNode;
begin
  Assert(Assigned(AViewTable));

  LNode := AViewTable.FindNode('Controller/Filters');
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

  ValidateViewTable(ADataView.MainTable);
end;

procedure TViewValidator.ValidateViewField(const AViewField: TKViewField);
begin
  Assert(Assigned(AViewField));

  LogIndent;
  try
    Log(Format('Validating field %s...', [AViewField.FieldName]));
    if AViewField.FindModelField = nil then
      LogError(Format('Model field %s not found.', [AViewField.FieldName]));
  finally
    LogOutdent;
  end;
end;

end.
