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

unit Kitto.Metadata.Views;

{$I Kitto.Defines.inc}

interface

uses
  Types, Classes, Generics.Collections,
  EF.Classes, EF.Types, EF.Tree,
  Kitto.Metadata, Kitto.Metadata.Models, Kitto.Store, Kitto.Rules;

type
  TKViews = class;

  TKView = class(TKMetadata)
  strict private
    function GetControllerType: string;
    function GetCatalog: TKViews;
  strict protected
    const DEFAULT_IMAGE_NAME = 'default_view';
    function GetDisplayLabel: string; virtual;
    function GetImageName: string; virtual;
    class function GetClassNameForResourceURI: string; override;
  public
    property Catalog: TKViews read GetCatalog;

    property DisplayLabel: string read GetDisplayLabel;
    property ImageName: string read GetImageName;

    property ControllerType: string read GetControllerType;
  end;

  TKViewList = class(TList<TKView>)
  public
    procedure AddViewNamesToStrings(const AStrings: TStrings);
  end;

  TKLayouts = class;

  TKLayout = class(TKMetadata)
  private
    FLayouts: TKLayouts;
  end;

  ///	<summary>
  ///	  A catalog of views.
  ///	</summary>
  TKViews = class(TKMetadataCatalog)
  private
    FLayouts: TKLayouts;
    FModels: TKModels;
    function GetLayouts: TKLayouts;
    function BuildView(const ANode: TEFNode;
      const AViewBuilderName: string): TKView;
    function GetView(I: Integer): TKView;
    function GetViewCount: Integer;
  protected
    function GetObjectClassType: TKMetadataClass; override;
    procedure SetPath(const AValue: string); override;
  public
    constructor Create(const AModels: TKModels);
    destructor Destroy; override;
  public
    property ViewCount: Integer read GetViewCount;
    property Views[I: Integer]: TKView read GetView; default;
    function ViewByName(const AName: string): TKView;
    function FindView(const AName: string): TKView;

    function ViewByNode(const ANode: TEFNode): TKView;
    function FindViewByNode(const ANode: TEFNode): TKView;

    property Models: TKModels read FModels;
    property Layouts: TKLayouts read GetLayouts;
    procedure Open; override;
    procedure Close; override;
  end;

  ///	<summary>
  ///	  A catalog of layouts. Internally used by the catalog of views.
  ///	</summary>
  TKLayouts = class(TKMetadataCatalog)
  private
    function GetLayout(I: Integer): TKLayout;
    function GetLayoutCount: Integer;
  protected
    procedure AfterCreateObject(const AObject: TKMetadata); override;
    function GetObjectClassType: TKMetadataClass; override;
  public
    property LayoutCount: Integer read GetLayoutCount;
    property Layouts[I: Integer]: TKLayout read GetLayout; default;
    function LayoutByName(const AName: string): TKLayout;
    function FindLayout(const AName: string): TKLayout;
  end;

  ///	<summary>
  ///	  A view that executes an action.
  ///	</summary>
  TKActionView = class(TKView)

  end;

  ///	<summary>The type of nodes in a tree view.</summary>
  TKTreeViewNode = class(TEFNode)
  private
    function GetTreeViewNodeCount: Integer;
    function GetTreeViewNode(I: Integer): TKTreeViewNode;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    property TreeViewNodeCount: Integer read GetTreeViewNodeCount;
    property TreeViewNodes[I: Integer]: TKTreeViewNode read GetTreeViewNode;
  end;

  ///	<summary>A node in a tree view that is a folder (i.e. contains other
  ///	nodes and doesn't represent a view).</summary>
  TKTreeViewFolder = class(TKTreeViewNode)
  private
    function GetIsInitiallyCollapsed: Boolean;
  public
    property IsInitiallyCollapsed: Boolean read GetIsInitiallyCollapsed;
  end;

  ///	<summary>
  ///	  A view that is a tree of views. Contains views and folders, which
  ///  in turn contain views.
  ///	</summary>
  TKTreeView = class(TKView)
  private
    function GetTreeViewNode(I: Integer): TKTreeViewNode;
    function GetTreeViewNodeCount: Integer;
  protected
    function GetChildClass(const AName: string): TEFNodeClass; override;
  public
    property TreeViewNodeCount: Integer read GetTreeViewNodeCount;
    property TreeViewNodes[I: Integer]: TKTreeViewNode read GetTreeViewNode;
  end;

  TKViewBuilder = class(TKMetadata)
  strict private
    FViews: TKViews;
  private
    FPersistentName: string;
    FNode: TEFNode;
  strict protected
    property Views: TKViews read FViews;
    property PersistentName: string read FPersistentName;
  public
    function BuildView(const AViews: TKViews;
      const APersistentName: string = '';
      const ANode: TEFNode = nil): TKView; virtual;
  end;

  TKViewBuilderClass = class of TKViewBuilder;

  TKViewBuilderRegistry = class(TEFRegistry)
  private
    class var FInstance: TKViewBuilderRegistry;
    class function GetInstance: TKViewBuilderRegistry; static;
  protected
    class destructor Destroy;
  public
    class property Instance: TKViewBuilderRegistry read GetInstance;
    function GetClass(const AId: string): TKViewBuilderClass;
  end;

  TKViewBuilderFactory = class(TEFFactory)
  private
    class var FInstance: TKViewBuilderFactory;
    class function GetInstance: TKViewBuilderFactory; static;
  protected
    function DoCreateObject(const AClass: TClass): TObject; override;
  public
    class destructor Destroy;
  public
    class property Instance: TKViewBuilderFactory read GetInstance;

    function CreateObject(const AId: string): TKViewBuilder; reintroduce;
  end;

implementation

uses
  SysUtils, StrUtils, Variants, TypInfo,
  EF.DB, EF.StrUtils,
  Kitto.Types, Kitto.Config, Kitto.SQL;

{ TKViews }

procedure TKViews.Close;
begin
  inherited;
  if Assigned(FLayouts) then
    FLayouts.Close;
end;

constructor TKViews.Create(const AModels: TKModels);
begin
  inherited Create;
  FModels := AModels;
end;

destructor TKViews.Destroy;
begin
  FreeAndNil(FLayouts);
  inherited;
end;

function TKViews.FindView(const AName: string): TKView;
begin
  Result := FindObject(AName) as TKView;
end;

function TKViews.FindViewByNode(const ANode: TEFNode): TKView;
var
  LWords: TStringDynArray;
begin
  if Assigned(ANode) then
  begin
    Result := FindNonpersistentObject(ANode) as TKView;
    if not Assigned(Result) then
    begin
      LWords := Split(ANode.AsExpandedString);
      if Length(LWords) >= 2 then
      begin
        // Two words: the first one is the verb.
        if SameText(LWords[0], 'Build') then
        begin
          Result := BuildView(ANode, LWords[1]);
          Exit;
        end;
      end;
    end;
  end;
  Result := FindObjectByNode(ANode) as TKView;
end;

function TKViews.BuildView(const ANode: TEFNode; const AViewBuilderName: string): TKView;
var
  LViewBuilder: TKViewBuilder;
begin
  Assert(Assigned(ANode));
  Assert(AViewBuilderName <> '');

  LViewBuilder := TKViewBuilderFactory.Instance.CreateObject(AViewBuilderName);
  try
    LViewBuilder.Assign(ANode);
    Result := LViewBuilder.BuildView(Self, '', ANode);
    AfterCreateObject(Result);
  finally
    FreeAndNil(LViewBuilder);
  end;
end;

function TKViews.GetLayouts: TKLayouts;
begin
  if not Assigned(FLayouts) then
    FLayouts := TKLayouts.Create;
  Result := FLayouts;
end;

function TKViews.GetObjectClassType: TKMetadataClass;
begin
  Result := TKView;
end;

function TKViews.GetView(I: Integer): TKView;
begin
  Result := Objects[I] as TKView;
end;

function TKViews.GetViewCount: Integer;
begin
  Result := ObjectCount;
end;

procedure TKViews.Open;
begin
  inherited;
  Layouts.Open;
end;

procedure TKViews.SetPath(const AValue: string);
begin
  inherited;
  Layouts.Path := IncludeTrailingPathDelimiter(AValue) + 'Layouts';
end;

function TKViews.ViewByName(const AName: string): TKView;
begin
  Result := ObjectByName(AName) as TKView;
end;

function TKViews.ViewByNode(const ANode: TEFNode): TKView;
begin
  Result := FindViewByNode(ANode);
  if not Assigned(Result) then
    if Assigned(ANode) then
      ObjectNotFound(ANode.Name + ':' + ANode.AsString)
    else
      ObjectNotFound('<nil>');
end;

{ TKLayouts }

procedure TKLayouts.AfterCreateObject(const AObject: TKMetadata);
begin
  inherited;
  (AObject as TKLayout).FLayouts := Self;
end;

function TKLayouts.FindLayout(const AName: string): TKLayout;
begin
  Result := FindObject(AName) as TKLayout;
end;

function TKLayouts.GetLayout(I: Integer): TKLayout;
begin
  Result := Objects[I] as TKLayout;
end;

function TKLayouts.GetLayoutCount: Integer;
begin
  Result := ObjectCount;
end;

function TKLayouts.GetObjectClassType: TKMetadataClass;
begin
  Result := TKLayout;
end;

function TKLayouts.LayoutByName(const AName: string): TKLayout;
begin
  Result := ObjectByName(AName) as TKLayout;
end;

{ TKView }

class function TKView.GetClassNameForResourceURI: string;
begin
  // We want all derived classes to be identified as views.
  Result := 'View';
end;

function TKView.GetCatalog: TKViews;
begin
  Result := inherited Catalog as TKViews;
end;

function TKView.GetControllerType: string;
begin
  Result := GetExpandedString('Controller');
end;

function TKView.GetDisplayLabel: string;
begin
  Result := GetString('DisplayLabel');
end;

function TKView.GetImageName: string;
begin
  Result := GetString('ImageName');
  if Result = '' then
    Result := DEFAULT_IMAGE_NAME;
end;

{ TKTreeViewNode }

function TKTreeViewNode.GetChildClass(const AName: string): TEFNodeClass;
begin
  if SameText(AName, 'Folder') then
    Result := TKTreeViewFolder
  else if SameText(AName, 'View') then
    Result := TKTreeViewNode
  else
    Result := inherited GetChildClass(AName);
end;

function TKTreeViewNode.GetTreeViewNode(I: Integer): TKTreeViewNode;
begin
  Result := GetChild<TKTreeViewNode>(I);
end;

function TKTreeViewNode.GetTreeViewNodeCount: Integer;
begin
  Result := GetChildCount<TKTreeViewNode>;
end;

{ TKTreeView }

function TKTreeView.GetChildClass(const AName: string): TEFNodeClass;
begin
  if SameText(AName, 'Folder') then
    Result := TKTreeViewFolder
  else if SameText(AName, 'View') then
    Result := TKTreeViewNode
  else
    Result := inherited GetChildClass(AName);
end;

function TKTreeView.GetTreeViewNode(I: Integer): TKTreeViewNode;
begin
  Result := GetChild<TKTreeViewNode>(I);
end;

function TKTreeView.GetTreeViewNodeCount: Integer;
begin
  Result := GetChildCount<TKTreeViewNode>;
end;

{ TKViewBuilderRegistry }

class destructor TKViewBuilderRegistry.Destroy;
begin
  FreeAndNil(FInstance);
end;

function TKViewBuilderRegistry.GetClass(const AId: string): TKViewBuilderClass;
begin
  Result := TKViewBuilderClass(inherited GetClass(AId));
end;

class function TKViewBuilderRegistry.GetInstance: TKViewBuilderRegistry;
begin
  if FInstance = nil then
    FInstance := TKViewBuilderRegistry.Create;
  Result := FInstance;
end;

{ TKViewBuilderFactory }

function TKViewBuilderFactory.CreateObject(const AId: string): TKViewBuilder;
begin
  Result := inherited CreateObject(AId) as TKViewBuilder;
end;

class destructor TKViewBuilderFactory.Destroy;
begin
  FreeAndNil(FInstance);
end;

function TKViewBuilderFactory.DoCreateObject(const AClass: TClass): TObject;
begin
  // Must use the virtual constructor in TEFTree.
  Result := TKViewBuilderClass(AClass).Create;
end;

class function TKViewBuilderFactory.GetInstance: TKViewBuilderFactory;
begin
  if FInstance = nil then
    FInstance := TKViewBuilderFactory.Create(TKViewBuilderRegistry.Instance);
  Result := FInstance;
end;

{ TKViewBuilder }

function TKViewBuilder.BuildView(const AViews: TKViews;
  const APersistentName: string; const ANode: TEFNode): TKView;
begin
  Assert(Assigned(AViews));
  Assert(Assigned(AViews.Models));

  FViews := AViews;
  FPersistentName := APersistentName;
  FNode := ANode;
  Result := nil;
end;

{ TKViewList }

procedure TKViewList.AddViewNamesToStrings(const AStrings: TStrings);
var
  LView: TKView;
begin
  for LView in Self do
    AStrings.Add(LView.PersistentName);
end;

{ TKTreeViewFolder }

function TKTreeViewFolder.GetIsInitiallyCollapsed: Boolean;
begin
  Result := GetBoolean('IsInitiallyCollapsed', False);
end;

initialization
  TKMetadataRegistry.Instance.RegisterClass('Tree', TKTreeView);

finalization
  TKMetadataRegistry.Instance.UnregisterClass('Tree');

end.
