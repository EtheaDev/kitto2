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

///	<summary>
///	  This units defines routines that link EF.Tree with RTTI.
///	</summary>
unit EF.Rtti;

interface

uses
  EF.Tree;

///	<summary>Copies published properties in AObject to ANode. Creates subnodes
///	for nested objects.</summary>
///	<remarks>Some non-streamable types, such as events, are not
///	copied.</remarks>
procedure ObjectToNode(const AObject: TObject; const ANode: TEFTree);

///	<summary>Copies property values from ANode to AObject and
///	subobjects.</summary>
///	<remarks>Some non-streamable types, such as events, are not
///	supported.</remarks>
procedure NodeToObject(const ANode: TEFTree; const AObject: TObject);

implementation

uses
  Rtti, TypInfo;

procedure ObjectPropertiesToNode(const AParentNode: TEFTree; const AInstance: TObject;
  const ARttiContext: TRttiContext);
var
  LProperty: TRttiProperty;
begin
  Assert(Assigned(AParentNode));

  if AInstance <> nil then
  begin
    for LProperty in ARttiContext.GetType(AInstance.ClassType).GetProperties do
    begin
      if LProperty.IsReadable and (LProperty.Visibility = mvPublished) then
      begin
        case LProperty.PropertyType.TypeKind of
          TTypeKind.tkClass:
          begin
            ObjectPropertiesToNode(AParentNode.GetNode(LProperty.Name, True),
              LProperty.GetValue(AInstance).AsObject, ARttiContext);
          end;
          TTypeKind.tkInteger, TTypeKind.tkInt64:
            AParentNode.SetInteger(LProperty.Name, LProperty.GetValue(AInstance).AsInteger);
          TTypeKind.tkChar, TTypeKind.tkString, TTypeKind.tkUString, TTypeKind.tkWChar, TTypeKind.tkLString, TTypeKind.tkWString:
            AParentNode.SetString(LProperty.Name, LProperty.GetValue(AInstance).AsString);
          TTypeKind.tkFloat:
            AParentNode.SetFloat(LProperty.Name, LProperty.GetValue(AInstance).AsExtended);
          TTypeKind.tkVariant:
            AParentNode.SetValue(LProperty.Name, LProperty.GetValue(AInstance).AsVariant);
          TTypeKind.tkEnumeration:
            AParentNode.SetString(LProperty.Name, GetEnumProp(AInstance, LProperty.Name));
          TTypeKind.tkSet:
            AParentNode.SetString(LProperty.Name, GetSetProp(AInstance, LProperty.Name, True));
        end;
      end;
    end;
  end;
end;

procedure ObjectToNode(const AObject: TObject; const ANode: TEFTree);
var
  LContext: TRTTIContext;
begin
  Assert(Assigned(AObject));
  Assert(Assigned(ANode));

  LContext := TRTTIContext.Create;
  ObjectPropertiesToNode(ANode, AObject, LContext);
end;

procedure NodeToObjectProperties(const AParentNode: TEFTree; const AInstance: TObject;
  const ARttiContext: TRttiContext);
var
  LProperty: TRttiProperty;
begin
  if (AParentNode <> nil) and (AInstance <> nil) then
  begin
    for LProperty in ARttiContext.GetType(AInstance.ClassType).GetProperties do
    begin
      if LProperty.IsWritable and (LProperty.Visibility = mvPublished) then
      begin
        case LProperty.PropertyType.TypeKind of
          TTypeKind.tkClass:
          begin
            NodeToObjectProperties(AParentNode.FindNode(LProperty.Name),
              LProperty.GetValue(AInstance).AsObject, ARttiContext);
          end;
          TTypeKind.tkInteger, TTypeKind.tkInt64:
            LProperty.SetValue(AInstance, AParentNode.GetInteger(LProperty.Name));
          TTypeKind.tkChar, TTypeKind.tkString, TTypeKind.tkUString, TTypeKind.tkWChar, TTypeKind.tkLString, TTypeKind.tkWString:
            LProperty.SetValue(AInstance, AParentNode.GetString(LProperty.Name));
          TTypeKind.tkFloat:
            LProperty.SetValue(AInstance, AParentNode.GetFloat(LProperty.Name));
          TTypeKind.tkVariant:
            LProperty.SetValue(AInstance, TValue.FromVariant(AParentNode.GetValue(LProperty.Name)));
          TTypeKind.tkEnumeration:
            SetEnumProp(AInstance, LProperty.Name, AParentNode.GetString(LProperty.Name));
          TTypeKind.tkSet:
            SetSetProp(AInstance, LProperty.Name, AParentNode.GetString(LProperty.Name));
        end;
      end;
    end;
  end;
end;

procedure NodeToObject(const ANode: TEFTree; const AObject: TObject);
var
  LContext: TRTTIContext;
begin
  Assert(Assigned(AObject));

  if Assigned(ANode) then
  begin
    LContext := TRTTIContext.Create;
    NodeToObjectProperties(ANode, AObject, LContext);
  end;
end;

end.
