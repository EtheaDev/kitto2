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
unit KIDE.DesignMetadata;

interface

uses
  EF.Tree,
  Kitto.Metadata,
  Kitto.Metadata.Models,
  Kitto.Metadata.Views, Kitto.Metadata.DataView;

type
  TEFTreeDesignMetadata = class helper for TEFTree
  public
    function CanAddChild: Boolean;

    ///	<summary>Returns True for nodes that should be treated as leaf in the
    ///	designer. Such nodes have no subnodes, but rather have their contents
    ///	displayed by a custom separate panel.</summary>
    function IsTreeDesignLeaf: Boolean;
  end;

  TEFNodeDesignMetadata = class helper for TEFNode
  public
    ///	<summary>Returns the node's image index to be used in
    ///	designers.</summary>
    function GetDesignImageIndex: Integer;

    ///	<summary>Returns True for nodes that should be treated as leaf in the
    ///	designer. Such nodes have no subnodes, but rather have their contents
    ///	displayed by a custom separate panel.</summary>
    function IsDesignLeaf: Boolean;

    function CanEditName: Boolean;

    function CanEditValue: Boolean;

    function CanDelete: Boolean;
  end;

///	<summary>Returns the ViewTable of a specific form or grid layout.
/// AWindowControllerType can be 'Form' or 'Grid'</summary>
function GetViewTableOfLayout(const ALayout: TKLayout;
  const AWindowControllerType: string): TKViewTable;

implementation

uses
  SysUtils, StrUtils,
  EF.Classes, EF.StrUtils,
  KIDE.Project, KIDE.Utils,
  KIDE.MainDataModuleUnit;

function GetViewTableOfLayout(const ALayout: TKLayout;
  const AWindowControllerType: string): TKViewTable;
var
  I, J: Integer;
  LView: TKDataView;
  LViews: TKViews;
  LLayoutPersistentName, LLayoutName: string;
  LViewTable: TKViewTable;

  function IsViewTableGridLayout(AVieTable: TKViewTable): Boolean;
  var
    LLayoutNode: TEFNode;
  begin
    LLayoutNode := AVieTable.FindNode('Controller/'+AWindowControllerType+'/Layout');
    Result := Assigned(LLayoutNode) and SameText(LLayoutNode.AsString, LLayoutPersistentName);
  end;

begin
  LLayoutPersistentName := ALayout.PersistentName;
  Result := nil;
  LView := nil;
  LViews := TProject.CurrentProject.Config.Views;
  for I := 0 to LViews.ViewCount -1 do
  begin
    if LViews.Views[I] is TKDataView then
      LView := LViews.Views[I] as TKDataView
    else
      Continue;
    if Assigned(LView) then
      LLayoutName := LView.PersistentName+'_'+AWindowControllerType
    else
      LLayoutName := '';
    if SameText(LLayoutName, LLayoutPersistentName) and (LView is TKDataView) then
    begin
      Result := LView.MainTable;
      break;
    end
    else
    begin
      LViewTable := LView.MainTable;
      if IsViewTableGridLayout(LViewTable) then
        Result := LViewTable
      else
      begin
        for J := 0 to LView.MainTable.DetailTableCount -1 do
        begin
          LViewTable := LView.MainTable.DetailTables[J];
          if IsViewTableGridLayout(LViewTable) then
          begin
            Result := LViewTable;
            break;
          end;
        end;
      end;
    end;
  end;
end;

{ TEFTreeDesignMetadata }

function TEFTreeDesignMetadata.CanAddChild: Boolean;
begin
  Result := True;
end;

function TEFTreeDesignMetadata.IsTreeDesignLeaf: Boolean;
begin
  Result := False;

  if InheritsFrom(TKModelField) or InheritsFrom(TKViewField) then
    Result := True
  else if (Root is TKDataView) and StrMatches(GetPath, 'Controller/Filters/Items/?*') then
    Result := True
  else if (Root is TEFPersistentTree) and StrMatches(GetPath, 'Databases/?*') then
    Result := True
  else if ChildCount = 0 then
    Result := True;
end;

{ TEFNodeDesignMetadata }

function TEFNodeDesignMetadata.CanDelete: Boolean;
begin
  Result := Assigned(Parent);
end;

function TEFNodeDesignMetadata.CanEditName: Boolean;
begin
  Result := True;
//    InheritsFrom(TKModelField)
//    or InheritsFrom(TKViewField)
//    or (ChildCount = 0)
//    or ((Root is TEFComponentConfig) and (Parent is TEFNode) and SameText(TEFNode(Parent).Name, 'Databases'));
end;

function TEFNodeDesignMetadata.CanEditValue: Boolean;
begin
  Result := IsDesignLeaf
    or (((Root is TKView) or (Root is TEFNode)) and SameText(Name, 'Controller'))
    or (((Root is TKDataView) or (Root is TEFNode)) and StrMatches(GetPath, '*/Controller/Grouping/ShowCount'));
end;

function TEFNodeDesignMetadata.GetDesignImageIndex: Integer;
begin
  if InheritsFrom(TKModel) then
    Result := MODEL_PICTURE
  else if InheritsFrom(TKModelField) then
  begin
    if TKModelField(Self).IsReference then
      Result := FIELD_REF_PICTURE
    else if TKModelField(Self).IsKey then
      Result := FIELD_PK_PICTURE
    else
      Result := FIELD_PICTURE;
  end
  else if InheritsFrom(TKViewField) then
  begin
    if TKViewField(Self).IsReference then
      Result := FIELD_REF_PICTURE
    else
      Result := FIELD_PICTURE;
  end
  else if InheritsFrom(TKRule) then
    Result := BULB_PICTURE
  else if InheritsFrom(TKView) then
    Result := TProject.CurrentProject.GetViewImageIndex(TKView(Self))
  else if InheritsFrom(TKLayout) then
    Result := TProject.CurrentProject.GetLayoutImageIndex(TKLayout(Self))
  else if (Root is TEFPersistentTree) and StrMatches(GetPath, 'Databases/?*') then
    Result := DATABASE_PICTURE
  else if (Root is TEFPersistentTree) and SameText(Name, 'Databases') then
    Result := DATABASE_PICTURE
  else if (Root is TEFPersistentTree) and SameText(Name, 'Auth') then
    Result := AUTH_PICTURE
  else if (Root is TEFPersistentTree) and SameText(Name, 'AccessControl') then
    Result := UAC_PICTURE
  else if (Root is TEFPersistentTree) and SameText(Name, 'Ext') then
    Result := EXT_PICTURE
  else if (Root is TEFPersistentTree) and SameText(Name, 'FastCGI') then
    Result := FASTCGI_PICTURE
  else if InheritsFrom(TEFNode) and EndsText('Label', TEFNode(Self).Name) then
    Result := LABEL_PICTURE
  else if IsSubViewNode(TEFNode(Self)) then
    Result := VIEW_PICTURE
  else if (ChildCount > 0) and (TEFNode(Self).AsString = '') then
    Result := FOLDER_PICTURE
  else if InheritsFrom(TEFNode) and (TEFNode(Self).DataType is TEFStringDataType) then
    Result := STRING_FIELD_PICTURE
  else if InheritsFrom(TEFNode) and (TEFNode(Self).DataType is TEFMemoDataType) then
    Result := MEMO_FIELD_PICTURE
  else if InheritsFrom(TEFNode) and (TEFNode(Self).DataType is TEFDateDataType) then
    Result := DATA_FIELD_PICTURE
  else if InheritsFrom(TEFNode) and (TEFNode(Self).DataType is TEFDateTimeDataType) then
    Result := DATETIME_FIELD
  else if InheritsFrom(TEFNode) and (TEFNode(Self).DataType is TEFTimeDataType) then
    Result := TIME_FIELD_PICTURE
  else if InheritsFrom(TEFNode) and (TEFNode(Self).DataType is TEFBooleanDataType) then
    Result := BOOLEAN_FIELD_PICTURE
  else if InheritsFrom(TEFNode) and (TEFNode(Self).DataType is TEFIntegerDataType) then
    Result := INTEGER_FIELD_PICTURE
  else if InheritsFrom(TEFNode) and (TEFNode(Self).DataType is TEFNumericDataTypeBase) then
    Result := NUMERIC_FIELD_PICTURE
  else
    Result := GENERIC_PICTURE;
end;

function TEFNodeDesignMetadata.IsDesignLeaf: Boolean;
begin
  Result := IsTreeDesignLeaf;

  if (Root is TEFPersistentTree) and SameText(Name, 'Auth') then
    Result := True
  else if (Root is TEFPersistentTree) and SameText(Name, 'AccessControl') then
    Result := True
  else if (Root is TEFPersistentTree) and SameText(Name, 'Ext') then
    Result := True
  else if (Root is TEFPersistentTree) and SameText(Name, 'FastCGI') then
    Result := True;
end;

end.
