unit Ext.Data;

interface

uses
  Classes, StrUtils, Kitto.Ext, Ext.Base, Ext.Util;

type
  TExtDataDataReader = class;
  TExtDataField = class;
  TExtDataNode = class;
  TExtDataStore = class;
  TExtDataTree = class;
  TExtDataJsonReader = class;

  TExtDataProxy = class(TExtUtilObservable)
  private
    FApi: TExtObject;
    FReader: TExtDataDataReader;
    procedure SetReader(const AValue: TExtDataDataReader);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    function SetApi(const AApi: string; const AUrl: string): TExtExpression; overload;
    property Reader: TExtDataDataReader read FReader write SetReader;
  end;

  TExtDataAjaxProxy = class(TExtDataProxy)
  private
    FUrl: string;
    procedure SetUrl(const AValue: string);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    property Url: string read FUrl write SetUrl;
  end;

  TExtDataDataReader = class(TExtObject)
  private
    FFields: TExtObjectArray;
  protected
    function GetObjectNamePrefix: string; override;
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    property Fields: TExtObjectArray read FFields;
  end;

  TExtDataField = class(TExtObject)
  private
    FName: string;
    FAllowBlank: Boolean;
    FUseNull: Boolean;
    FMapping: string;
    FType: string;
    procedure SetAllowBlank(const AValue: Boolean);
    procedure SetMapping(AValue: string);
    procedure _SetName(const AValue: string);
    procedure SetType(const AValue: string);
    procedure SetUseNull(const AValue: Boolean);
  protected
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    property AllowBlank: Boolean read FAllowBlank write SetAllowBlank;
    property Mapping: string read FMapping write SetMapping;
    property Name: string read FName write _SetName;
    property &Type: string read FType write SetType;
    property UseNull: Boolean read FUseNull write SetUseNull;
  end;

  TExtDataNode = class(TExtUtilObservable)
  private
    FChildren: TExtObjectArray;
    FLeaf: Boolean;
    FId: string;
    procedure _SetId(const AValue: string);
    procedure SetLeaf(const AValue: Boolean);
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
    function SetId(const AId: string): TExtExpression;
    property Id: string read FId write _SetId;
    property Leaf: Boolean read FLeaf write SetLeaf;
    property Children: TExtObjectArray read FChildren;
  end;

  TExtDataStore = class(TExtUtilObservable)
  private
    FAutoLoad: Boolean;
    FRemoteSort: Boolean;
    FGroupField: string;
    FTotalLength: Integer;
    FStoreId: string;
    FProxy: TExtDataProxy;
    procedure SetAutoLoad(const AValue: Boolean);
    procedure SetProxy(const AValue: TExtDataProxy);
    procedure SetRemoteSort(const AValue: Boolean);
    procedure SetStoreId(AValue: string);
    procedure SetTotalLength(const AValue: Integer);
    procedure SetGroupField(const AValue: string);
  protected
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    function Load(const AOptions: TExtObject): TExtExpression;
    procedure RemoveAll(const ASilent: Boolean = False);
    property AutoLoad: Boolean read FAutoLoad write SetAutoLoad;
    property GroupField: string read FGroupField write SetGroupField;
    property Proxy: TExtDataProxy read FProxy write SetProxy;
    property RemoteSort: Boolean read FRemoteSort write SetRemoteSort;
    property StoreId: string read FStoreId write SetStoreId;
    property TotalLength: Integer  read FTotalLength write SetTotalLength;
  end;

  TExtDataTree = class(TExtUtilObservable)
  public
    class function JSClassName: string; override;
  end;

  TExtDataJsonReader = class(TExtDataDataReader)
  private
    FRootProperty: string;
    FSuccessProperty: string;
    FMessageProperty: string;
    FTotalProperty: string;
    procedure SetRootProperty(const AValue: string);
    procedure SetSuccessProperty(const AValue: string);
    procedure SetTotalProperty(const AValue: string);
    procedure SetMessageProperty(const AValue: string);
  public
    class function JSClassName: string; override;
    property RootProperty: string read FRootProperty write SetRootProperty;
    property SuccessProperty: string read FSuccessProperty write SetSuccessProperty;
    property MessageProperty: string read FMessageProperty write SetMessageProperty;
    property TotalProperty: string read FTotalProperty write SetTotalProperty;
  end;

  TExtDataXmlStore = class(TExtDataStore)
  protected
    procedure InitDefaults; override;
  public
    class function JSClassName: string; override;
  end;

implementation

uses
  Math;

procedure TExtDataDataReader.InitDefaults;
begin
  inherited;
  FFields := CreateConfigArray('fields');
end;

class function TExtDataDataReader.JSClassName: string;
begin
  Result := 'Ext.data.DataReader';
end;

function TExtDataDataReader.GetObjectNamePrefix: string;
begin
  Result := 'reader';
end;

procedure TExtDataField.SetAllowBlank(const AValue: Boolean);
begin
  FAllowBlank := SetConfigItem('allowBlank', AValue);
end;

procedure TExtDataField.SetMapping(AValue: string);
begin
  FMapping := SetConfigItem('mapping', AValue);
end;

procedure TExtDataField._SetName(const AValue: string);
begin
  FName := SetConfigItem('name', AValue);
end;

procedure TExtDataField.SetType(const AValue: string);
begin
  FType := SetConfigItem('type', AValue);
end;

procedure TExtDataField.SetUseNull(const AValue: Boolean);
begin
  FUseNull := SetConfigItem('useNull', AValue);
end;

function TExtDataField.GetObjectNamePrefix: string;
begin
  Result := 'dfld';
end;

class function TExtDataField.JSClassName: string;
begin
  Result := 'Ext.data.Field';
end;

procedure TExtDataNode._SetId(const AValue: string);
begin
  FId := SetConfigItem('id', 'setId', AValue);
end;

procedure TExtDataNode.SetLeaf(const AValue: Boolean);
begin
  FLeaf := SetConfigItem('leaf', AValue);
end;

class function TExtDataNode.JSClassName: string;
begin
  Result := 'Ext.data.Node';
end;

procedure TExtDataNode.InitDefaults;
begin
  inherited;
  FChildren := CreateConfigArray('children');
end;

function TExtDataNode.SetId(const AId: string): TExtExpression;
begin
  FId := AId;
  Result := CallMethod('setId')
    .AddParam(AId)
    .AsExpression;
end;

procedure TExtDataStore.SetAutoLoad(const AValue: Boolean);
begin
  FAutoLoad := SetConfigItem('autoLoad', AValue);
end;

procedure TExtDataStore.SetProxy(const AValue: TExtDataProxy);
begin
  FProxy := AValue;
  SetConfigItem('proxy', AValue);
end;

procedure TExtDataStore.SetRemoteSort(const AValue: Boolean);
begin
  FRemoteSort := SetConfigItem('remoteSort', AValue);
end;

procedure TExtDataStore.SetTotalLength(const AValue: Integer);
begin
  FTotalLength := SetProperty('totalLength', AValue);
end;

procedure TExtDataStore.SetGroupField(const AValue: string);
begin
  FGroupField := SetConfigItem('groupField', AValue);
end;

procedure TExtDataStore.SetStoreId(AValue: string);
begin
  FStoreId := SetProperty('storeId', AValue);
end;

class function TExtDataStore.JSClassName: string;
begin
  Result := 'Ext.data.Store';
end;

function TExtDataStore.GetObjectNamePrefix: string;
begin
  Result := 'store';
end;

function TExtDataStore.Load(const AOptions: TExtObject): TExtExpression;
begin
  Result := CallMethod('load')
    .AddParam(AOptions)
    .AsExpression;
end;

procedure TExtDataStore.RemoveAll(const ASilent: Boolean);
begin
  CallMethod('removeAll')
    .AddParam(ASilent)
    .AsExpression;
end;

class function TExtDataTree.JSClassName: string;
begin
  Result := 'Ext.data.Tree';
end;

procedure TExtDataProxy.SetReader(const AValue: TExtDataDataReader);
begin
  FReader.Free;
  FReader := TExtDataDataReader(SetConfigItem('reader', AValue));
end;

class function TExtDataProxy.JSClassName: string;
begin
  Result := 'Ext.data.proxy.Proxy';
end;

procedure TExtDataProxy.InitDefaults;
begin
  inherited;
  FApi := TExtObject.CreateInternal(Self, 'api');
end;

function TExtDataProxy.SetApi(const AApi: string; const AUrl: string): TExtExpression;
begin
  Result := CallMethod('setApi')
    .AddParam(AApi)
    .AddParam(AUrl)
    .AsExpression;
end;

procedure TExtDataJsonReader.SetRootProperty(const AValue: string);
begin
  FRootProperty := SetConfigItem('rootProperty', AValue);
end;

procedure TExtDataJsonReader.SetSuccessProperty(const AValue: string);
begin
  FSuccessProperty := SetConfigItem('successProperty', AValue);
end;

procedure TExtDataJsonReader.SetTotalProperty(const AValue: string);
begin
  FTotalProperty := SetConfigItem('totalProperty', AValue);
end;

procedure TExtDataJsonReader.SetMessageProperty(const AValue: string);
begin
  FMessageProperty := SetConfigItem('messageProperty', AValue);
end;

class function TExtDataJsonReader.JSClassName: string;
begin
  Result := 'Ext.data.JsonReader';
end;

class function TExtDataXmlStore.JSClassName: string;
begin
  Result := 'Ext.data.XmlStore';
end;

procedure TExtDataXmlStore.InitDefaults;
begin
  inherited;
end;

procedure TExtDataAjaxProxy.SetUrl(const AValue: string);
begin
  FUrl := SetConfigItem('url', AValue);
end;

class function TExtDataAjaxProxy.JSClassName: string;
begin
  Result := 'Ext.data.proxy.Ajax';
end;

procedure TExtDataAjaxProxy.InitDefaults;
begin
  inherited;
end;

end.
