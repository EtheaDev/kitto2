unit Kitto.Metadata.ViewBuilders;

{$I Kitto.Defines.inc}

interface

uses
  Kitto.Metadata.Models, Kitto.Metadata.Views;

type
  TKAutoViewBuilderBase = class(TKViewBuilder)
  protected
    function GetControllerType: string; virtual; abstract;
    function GetModel: TKModel;
  public
    function BuildView: TKView; override;
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
  EF.Tree,
  Kitto.Config, Kitto.Metadata.DataView;

{ TKAutoViewBuilderBase }

function TKAutoViewBuilderBase.BuildView: TKView;
var
  LMainTable: TKViewTable;
  LMainTableController: TEFNode;
begin
  GetModel;
  Result := TKDataView.Create;
  try
    Result.AddChild(TEFNode.Clone(FindNode('Controller')));
    //Result.SetString('DisplayLabel', LModel.PluralDisplayLabel);
    Result.SetString('Controller', GetControllerType);

    LMainTable := Result.AddChild(TKViewTable.Create('MainTable')) as TKViewTable;
    LMainTable.SetString('Model', GetString('Model'));
    LMainTable.AddChild(TKViewFields.Create('Fields'));
    { TODO :
add all model fields, plus a lookup to the default field of each
reference that is not to a master record. The default field is
configured, or is the first non-key field by default. }
    LMainTableController := LMainTable.AddChild(TEFNode.Clone(FindNode('MainTable/Controller')));
{ TODO :
don't set this when the controller has learned to query the model
for cardinality estimate. }
    LMainTableController.SetBoolean('PagingTools', True);
{ TODO : create default filtering options based on model metadata }
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TKAutoViewBuilderBase.GetModel: TKModel;
begin
  Result := TKConfig.Instance.Models.ModelByName(GetString('Model'));
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
