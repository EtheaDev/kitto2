{-------------------------------------------------------------------------------
   Copyright 2012 Ethea S.r.l.

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

unit Kitto.Metadata.ViewBuilders;

{$I Kitto.Defines.inc}

interface

uses
  EF.Tree,
  Kitto.Metadata.Models, Kitto.Metadata.Views, Kitto.Metadata.DataView;

type
  TKAutoViewBuilderBase = class(TKViewBuilder)
  private
    function BuildSearchString(const AFields: TKViewFields): string;
    procedure AddFields(const AViewTable: TKViewTable; const AModel: TKModel);
    procedure AddDetailTables(const AViewTable: TKViewTable;
      const AModel: TKModel);
  protected
    function GetControllerType: string; virtual; abstract;
    function GetModel: TKModel;
  public
    function BuildView(const AViews: TKViews;
      const APersistentName: string = '';
      const ANode: TEFNode = nil): TKView; override;
  end;

  TKAutoListViewBuilder = class(TKAutoViewBuilderBase)
  protected
    function GetControllerType: string; override;
  end;

  TKAutoFormViewBuilder = class(TKAutoViewBuilderBase)
  protected
    function GetControllerType: string; override;
  end;

implementation

uses
  SysUtils,
  EF.Localization,
  Kitto.Config;

{ TKAutoViewBuilderBase }

procedure TKAutoViewBuilderBase.AddFields(const AViewTable: TKViewTable;
  const AModel: TKModel);
var
  LFields: TKViewFields;
  I: Integer;
  LField: TKModelField;
  LViewField: TKViewField;
begin
  Assert(Assigned(AViewTable));
  Assert(Assigned(AModel));

  LFields := AViewTable.AddChild(TKViewFields.Create('Fields')) as TKViewFields;
  for I := 0 to AModel.FieldCount - 1 do
  begin
    LField := AModel.Fields[I];

    LViewField := TKViewField.Create;
    try
      if LField.IsReference and not Assigned(LField.ReferencedModel.FindDetailReferenceByField(LField)) then
      begin
        // Add caption field of referenced model.
        LViewField.Rename(LField.ReferencedModel.ModelName + '.' +
          LField.ReferencedModel.CaptionField.FieldName);
        // AsString is the alias name.
        LViewField.AsString := LField.ReferencedModel.ModelName;
      end
      else
      begin
        // Add plain field.
        LViewField.Rename(LField.FieldName);
      end;
      LFields.AddChild(LViewField);
    except
      FreeAndNil(LViewField);
      raise;
    end;
  end;
end;

procedure TKAutoViewBuilderBase.AddDetailTables(const AViewTable: TKViewTable;
  const AModel: TKModel);
var
  I: Integer;
  LDetailTable: TKViewTable;
begin
  Assert(Assigned(AViewTable));
  Assert(Assigned(AModel));

  for I := 0 to AModel.DetailReferenceCount - 1 do
  begin
    LDetailTable := TKViewTable.Create;
    try
      LDetailTable.SetString('Model', AModel.DetailReferences[I].DetailModel.ModelName);
      AViewTable.AddDetailTable(LDetailTable);
      AddFields(LDetailTable, LDetailTable.Model);
      AddDetailTables(LDetailTable, LDetailTable.Model);
    except
      FreeAndNil(LDetailTable);
    end;
  end;
end;

function TKAutoViewBuilderBase.BuildView(const AViews: TKViews;
  const APersistentName: string; const ANode: TEFNode): TKView;
var
  LMainTable: TKViewTable;
  //LMainTableController: TEFNode;
  LModel: TKModel;
  LFilters: TEFNode;
  LFilterItems: TEFNode;
  LSearchItem: TEFNode;
  LSourceControllerNode: TEFNode;
  LSourceMainTableControllerNode: TEFNode;
  LControllerNode: TEFNode;
begin
  inherited;
  LModel := GetModel;
  Result := TKDataView.Create;
  try
    Result.SetString('Type', 'Data');
    if APersistentName <> '' then
    begin
      Result.PersistentName := APersistentName;
      AViews.AddObject(Result);
    end
    else if ANode <> nil then
      AViews.AddNonpersistentObject(Result, ANode);
    LSourceControllerNode := FindNode('Controller');
    if Assigned(LSourceControllerNode) then
      Result.AddChild(TEFNode.Clone(LSourceControllerNode));
    //Result.SetString('DisplayLabel', LModel.PluralDisplayLabel);
    LControllerNode := Result.SetString('Controller', GetControllerType);

    LMainTable := Result.AddChild(TKViewTable.Create('MainTable')) as TKViewTable;
    LMainTable.SetString('Model', LModel.ModelName);
    AddFields(LMainTable, LModel);
    AddDetailTables(LMainTable, LModel);

    LFilters := LControllerNode.AddChild('Filters');
    LFilters.SetString('DisplayLabel', _(Format('Search %s', [LModel.PluralDisplayLabel])));
    LFilterItems := LFilters.AddChild('Items');
    LSearchItem := LFilterItems.AddChild('FreeSearch', _('Free Search'));
    LSearchItem.SetString('ExpressionTemplate', BuildSearchString(
      LMainTable.ChildByName('Fields') as TKViewFields));

    LSourceMainTableControllerNode := FindNode('MainTable/Controller');
    if Assigned(LSourceMainTableControllerNode) then
      {LMainTableController := }LMainTable.AddChild(TEFNode.Clone(LSourceMainTableControllerNode))
    else
      {LMainTableController := }LMainTable.AddChild('Controller');
  except
    if APersistentName <> '' then
      AViews.DeleteObject(Result)
    else if ANode <> nil then
      AViews.DeleteNonpersistentObject(ANode);
    FreeAndNil(Result);
    raise;
  end;
end;

function TKAutoViewBuilderBase.BuildSearchString(const AFields: TKViewFields): string;
var
  I: Integer;
begin
  Assert(Assigned(AFields));

  Result := '';
  for I := 0 to AFields.FieldCount - 1 do
  begin
    if AFields[I].IsVisible and AFields[I].DataType.IsText then
    begin
      if Result = '' then
        Result := '(' + AFields[I].QualifiedDBName + ' like ''%{value}%'')'
      else
        Result := Result + ' or (' + AFields[I].QualifiedDBName + ' like ''%{value}%'')';
    end;
  end;
end;

function TKAutoViewBuilderBase.GetModel: TKModel;
begin
  Assert(Assigned(Views));
  Assert(Assigned(Views.Models));

  Result := Views.Models.ModelByName(GetString('Model'));
end;

{ TKAutoListViewBuilder }

function TKAutoListViewBuilder.GetControllerType: string;
begin
  Result := 'List';
end;

{ TKAutoFormViewBuilder }

function TKAutoFormViewBuilder.GetControllerType: string;
begin
  Result := 'Form';
end;

initialization
  TKViewBuilderRegistry.Instance.RegisterClass('AutoList', TKAutoListViewBuilder);
  TKViewBuilderRegistry.Instance.RegisterClass('AutoForm', TKAutoFormViewBuilder);

finalization
  TKViewBuilderRegistry.Instance.UnregisterClass('AutoList');
  TKViewBuilderRegistry.Instance.UnregisterClass('AutoForm');

end.
