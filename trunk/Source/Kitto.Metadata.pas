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

unit Kitto.Metadata;

{$I Kitto.Defines.inc}

interface

uses
  Classes, Generics.Collections,
  EF.Types, EF.Tree, EF.YAML;

type
  TKMetadataCatalog = class;

  TKMetadata = class(TEFTree)
  private
    FCatalog: TKMetadataCatalog;
  strict private
    FPersistentName: string;
    function GetIsPersistent: Boolean;
    function GetPersistentFileName: string;
  strict protected
    class function GetClassNameForResourceURI: string; virtual;
  public
    procedure Assign(const ASource: TEFTree); override;

    property Catalog: TKMetadataCatalog read FCatalog;

    property PersistentName: string read FPersistentName write FPersistentName;

    property IsPersistent: Boolean read GetIsPersistent;

    ///	<summary>Returns a string URI that uniquely identifies the object, to
    ///	be used for access control.</summary>
    function GetResourceURI: string; virtual;

    ///	<summary>Returns true if access is granted to the resource representing
    ///	the metadata object in the specified mode. This is a shortcut to
    ///	calling TKConfig.Instance.IsAccessGranted (possibly multiple times for
    ///	cascading, and "or"ing the results).</summary>
    function IsAccessGranted(const AMode: string): Boolean; virtual;

    ///	<summary>Returns the full path name of the persistent file.</summary>
    property PersistentFileName: string read GetPersistentFileName;
  end;

  TKMetadataClass = class of TKMetadata;

  TKMetadataItem = class(TEFNode)
  strict protected
    class function GetClassNameForResourceURI: string; virtual;
  public
    ///	<summary>Returns a string URI that uniquely identifies the object, to
    ///	be used for access control.</summary>
    function GetResourceURI: string; virtual;

    ///	<summary>Returns true if access is granted to the resource representing
    ///	the metadata object in the specified mode. This is a shortcut to
    ///	calling TKConfig.Instance.IsAccessGranted (possibly multiple times for
    ///	cascading, and "or"ing the results).</summary>
    function IsAccessGranted(const AMode: string): Boolean; virtual;
  end;

  TKMetadataCatalog = class
  private
    FPath: string;
    FIndex: TStringList;
    FReader: TEFYAMLReader;
    FWriter: TEFYAMLWriter;
    FDisposedObjects: TList<TKMetadata>;
    FNonpersistentObjects: TDictionary<TEFNode, TKMetadata>;
    procedure LoadIndex;
    function GetObjectCount: Integer;
    function GetReader: TEFYAMLReader;
    function LoadObject(const AName: string): TKMetadata;
    function GetWriter: TEFYAMLWriter;
    function GetObject(I: Integer): TKMetadata;
    function ObjectExists(const AName: string): Boolean;
    procedure DuplicateObjectError(const AName: string);
    procedure AfterAddObject(const AObject: TKMetadata);
    function FixObjectClassType(const AObject: TKMetadata): TKMetadata;
  protected
    procedure ObjectNotFound(const AName: string);
    // Delete file and free object.
    procedure DisposeObject(const AObject: TKMetadata);
    property Reader: TEFYAMLReader read GetReader;
    property Writer: TEFYAMLWriter read GetWriter;
    procedure AfterCreateObject(const AObject: TKMetadata); virtual;
    procedure SetPath(const AValue: string); virtual;
    function GetObjectClassType: TKMetadataClass; virtual; abstract;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function FindNonpersistentObject(const ANode: TEFNode): TKMetadata;
    procedure AddNonpersistentObject(const AObject: TKMetadata; const ANode: TEFNode);
    procedure DeleteNonpersistentObject(const ANode: TEFNode);
  public
    property Path: string read FPath write SetPath;

    ///	<summary>Returns the full file name for the specified persistent
    ///	name.</summary>
    function GetFullFileName(const AName: string): string;

    procedure Open; virtual;
    procedure Close; virtual;

    property ObjectCount: Integer read GetObjectCount;
    property Objects[I: Integer]: TKMetadata read GetObject;
    function FindObject(const AName: string): TKMetadata;
    type TPredicate = reference to function (const AObject: TKMetadata): Boolean;
    function FindObjectByPredicate(const APredicate: TPredicate): TKMetadata;

    function ObjectByName(const AName: string): TKMetadata;

    function ObjectByNode(const ANode: TEFNode): TKMetadata;
    function FindObjectByNode(const ANode: TEFNode): TKMetadata;

    procedure AddObject(const AObject: TKMetadata);
    procedure DeleteObject(const AObject: TKMetadata);

    ///	<summary>
    ///	  Marks the object as disposed and removes it from the index (but
    ///	  doesn't free it). The file is then deleted when SaveAll is called.
    ///	</summary>
    ///	<param name="AObject">
    ///	  Object to be disposed.
    ///	</param>
    procedure MarkObjectAsDisposed(const AObject: TKMetadata);

    ///	<summary>
    ///	  Saves all modified objects in the catalog and disposes all objects
    ///	  marked for disposition.
    ///	</summary>
    procedure SaveAll;

    procedure SaveObject(const AObject: TKMetadata);
  end;

  TKMetadataRegistry = class(TEFRegistry)
  private
    class var FInstance: TKMetadataRegistry;
    class function GetInstance: TKMetadataRegistry; static;
  protected
    procedure BeforeRegisterClass(const AId: string; const AClass: TClass);
      override;
  public
    class property Instance: TKMetadataRegistry read GetInstance;
    class destructor Destroy;
    function GetClass(const AId: string): TKMetadataClass;
  end;

implementation

uses
  Windows, SysUtils,
  EF.Localization, EF.StrUtils,
  Kitto.Types, Kitto.Config;

{ TKMetadataCatalog<T> }

procedure TKMetadataCatalog.AfterConstruction;
begin
  inherited;
  FIndex := TStringList.Create;
  FIndex.Sorted := True;
  Findex.Duplicates := dupError;
  Findex.CaseSensitive := False;
  FDisposedObjects := TList<TKMetadata>.Create;
  FNonpersistentObjects := TDictionary<TEFNode, TKMetadata>.Create;
end;

procedure TKMetadataCatalog.AfterCreateObject(const AObject: TKMetadata);
begin
  AObject.FCatalog := Self;
end;

procedure TKMetadataCatalog.AfterAddObject(const AObject: TKMetadata);
begin
  AObject.FCatalog := Self;
end;

procedure TKMetadataCatalog.Close;
var
  I: Integer;
  LObject: TKMetadata;
begin
  for LObject in FNonpersistentObjects.Values do
    LObject.Free;
  FNonpersistentObjects.Clear;

  for I := FDisposedObjects.Count - 1 downto 0 do
  begin
    FDisposedObjects[I].Free;
    FDisposedObjects.Delete(I);
  end;
  FDisposedObjects.Clear;

  for I := FIndex.Count - 1 downto 0 do
  begin
    FIndex.Objects[I].Free;
    FIndex.Delete(I);
  end;
  FIndex.Clear;
end;

procedure TKMetadataCatalog.DisposeObject(const AObject: TKMetadata);
var
  LFileName: string;
begin
  Assert(Assigned(AObject));

  if AObject.PersistentName <> '' then
  begin
    LFileName := GetFullFileName(AObject.PersistentName);
    if FileExists(LFileName) then
      DeleteFile(LFileName);
  end;
  AObject.Free;
end;

procedure TKMetadataCatalog.DuplicateObjectError(const AName: string);
begin
  raise EKError.CreateFmt(_('Duplicate object %s.'), [AName]);
end;

procedure TKMetadataCatalog.DeleteNonpersistentObject(const ANode: TEFNode);
begin
  FNonpersistentObjects.Remove(ANode);
end;

procedure TKMetadataCatalog.DeleteObject(const AObject: TKMetadata);
begin
  if ObjectExists(AObject.PersistentName) then
    FIndex.Delete(FIndex.IndexOf(AObject.PersistentName));
end;

destructor TKMetadataCatalog.Destroy;
begin
  Close;
  FreeAndNil(FIndex);
  FreeAndNil(FDisposedObjects);
  FreeAndNil(FNonpersistentObjects);
  FreeAndNil(FReader);
  FreeAndNil(FWriter);
  inherited;
end;

procedure TKMetadataCatalog.MarkObjectAsDisposed(const AObject: TKMetadata);
var
  LIndex: Integer;
begin
  FDisposedObjects.Add(AObject);
  if FIndex.Find(AObject.PersistentName, LIndex) then
  begin
    Assert(TKMetadata(FIndex.Objects[LIndex]) = AObject);
    FIndex.Delete(LIndex);
  end;
end;

function TKMetadataCatalog.GetFullFileName(const AName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FPath) + AName + '.yaml';
end;

function TKMetadataCatalog.GetObject(I: Integer): TKMetadata;
begin
  Result := nil;
  if (I >= 0) and (I < FIndex.Count) then
  begin
    Result := FIndex.Objects[I] as TKMetadata;
    if Result = nil then
    begin
      Result := LoadObject(FIndex[I]);
      FIndex.Objects[I] := Result;
    end;
  end;
  if Result = nil then
    ObjectNotFound(IntToStr(I));
end;

function TKMetadataCatalog.GetObjectCount: Integer;
begin
  Result := FIndex.Count;
end;

function TKMetadataCatalog.GetReader: TEFYAMLReader;
begin
  if not Assigned(FReader) then
    FReader := TEFYAMLReader.Create;
  Result := FReader;
end;

function TKMetadataCatalog.GetWriter: TEFYAMLWriter;
begin
  if not Assigned(FWriter) then
    FWriter := TEFYAMLWriter.Create;
  Result := FWriter;
end;

{$WARN SYMBOL_PLATFORM OFF}
procedure TKMetadataCatalog.LoadIndex;
var
  LResult: Integer;
  LSearchRec: TSearchRec;
begin
  LResult := FindFirst(IncludeTrailingPathDelimiter(FPath) + '*.yaml', faNormal, LSearchRec);
  while LResult = 0 do
  begin
    FIndex.AddObject(ChangeFileExt(LSearchRec.Name, ''), nil);
    LResult := FindNext(LSearchRec);
  end;
  FindClose(LSearchRec);
end;
{$WARN SYMBOL_PLATFORM ON}

function TKMetadataCatalog.FindNonpersistentObject(
  const ANode: TEFNode): TKMetadata;
begin
  if FNonpersistentObjects.ContainsKey(ANode) then
    Result := FNonpersistentObjects[ANode]
  else
    Result := nil;
end;

function TKMetadataCatalog.FindObject(const AName: string): TKMetadata;
var
  LIndex: Integer;
begin
  if FIndex.Find(AName, LIndex) then
  begin
    Result := FIndex.Objects[LIndex] as TKMetadata;
    if Result = nil then
    begin
      Result := LoadObject(AName);
      FIndex.Objects[LIndex] := Result;
    end;
  end
  else
    Result := nil;
end;

function TKMetadataCatalog.FindObjectByPredicate(
  const APredicate: TPredicate): TKMetadata;
var
  I: Integer;
begin
  Assert(Assigned(APredicate));

  for I := 0 to FIndex.Count - 1 do
  begin
    Result := FIndex.Objects[I] as TKMetadata;
    if Result = nil then
    begin
      Result := LoadObject(FIndex[I]);
      FIndex.Objects[I] := Result;
    end;
    if APredicate(Result) then
      Exit;
  end;
  Result := nil;
end;

function TKMetadataCatalog.ObjectByName(const AName: string): TKMetadata;
begin
  Result := FindObject(AName);
  if Result = nil then
    ObjectNotFound(AName);
end;

function TKMetadataCatalog.ObjectByNode(const ANode: TEFNode): TKMetadata;
begin
  Result := FindObjectByNode(ANode);
  if Result = nil then
    if Assigned(ANode) then
      ObjectNotFound(ANode.Name + ':' + ANode.AsString)
    else
      ObjectNotFound('<nil>');
end;

function TKMetadataCatalog.FindObjectByNode(const ANode: TEFNode): TKMetadata;
var
  LObjectName: string;
begin
  Result := nil;
  if Assigned(ANode) then
  begin
    Result := FindNonpersistentObject(ANode);
    if not Assigned(Result) then
    begin
      LObjectName := ANode.AsExpandedString;
      if LObjectName <> '' then
        Result := ObjectByName(LObjectName)
      else if ANode.ChildCount > 0 then
      begin
        Result := GetObjectClassType.Create;
        try
          Result.Assign(ANode);
          // Change object type according to the declaration, if present.
          Result := FixObjectClassType(Result);
          AddNonpersistentObject(Result, ANode);
        except
          FreeAndNil(Result);
          raise;
        end;
      end;
    end;
  end;
end;

procedure TKMetadataCatalog.ObjectNotFound(const AName: string);
begin
  raise EKError.CreateFmt(_('Object %s not found.'), [AName]);
end;

function TKMetadataCatalog.ObjectExists(const AName: string): Boolean;
begin
  Result := FIndex.IndexOf(AName) >= 0;
end;

function TKMetadataCatalog.LoadObject(const AName: string): TKMetadata;
var
  LFileName: string;
begin
  Result := nil;
  LFileName := GetFullFileName(AName);
  if FileExists(LFileName) then
  begin
    Result := GetObjectClassType.Create;
    Result.PersistentName := AName;
    Reader.LoadTreeFromFile(Result, LFileName);
    // Change object type according to the declaration, if present.
    Result := FixObjectClassType(Result);
    AfterCreateObject(Result);
  end;
  if Result = nil then
    ObjectNotFound(AName);
end;

function TKMetadataCatalog.FixObjectClassType(const AObject: TKMetadata): TKMetadata;
var
  LDeclaredClassName: string;
  LDeclaredClassType: TKMetadataClass;
begin
  Assert(Assigned(AObject));

  // Change object type according to the declaration, if present.
  LDeclaredClassName := AObject.GetString('Type');
  if LDeclaredClassName <> '' then
  begin
    LDeclaredClassType := TKMetadataRegistry.Instance.GetClass(LDeclaredClassName);
    if LDeclaredClassType <> AObject.ClassType then
    begin
      Result := LDeclaredClassType.Clone(AObject);
      AObject.Free;
    end
    else
      Result := AObject;
  end
  else
    Result := AObject;
end;

procedure TKMetadataCatalog.AddNonpersistentObject(const AObject: TKMetadata;
  const ANode: TEFNode);
begin
  FNonpersistentObjects.Add(ANode, AObject);
  AfterAddObject(AObject);
end;

procedure TKMetadataCatalog.AddObject(const AObject: TKMetadata);
begin
  Assert(Assigned(AObject));
  Assert(AObject.PersistentName <> '');

  if ObjectExists(AObject.PersistentName) then
    DuplicateObjectError(AObject.PersistentName);
  FIndex.AddObject(AObject.PersistentName, AObject);
  AfterAddObject(AObject);
end;

procedure TKMetadataCatalog.SetPath(const AValue: string);
begin
  FPath := AValue;
end;

procedure TKMetadataCatalog.Open;
begin
  Close;
  LoadIndex;
end;

procedure TKMetadataCatalog.SaveAll;
var
  I: Integer;
begin
  for I := 0 to FIndex.Count - 1 do
  begin
    if Assigned(FIndex.Objects[I]) then
      SaveObject(FIndex.Objects[I] as TKMetadata);
  end;

  for I := FDisposedObjects.Count - 1 downto 0 do
  begin
    DisposeObject(FDisposedObjects[I]);
    FDisposedObjects.Delete(I);
  end;
end;

procedure TKMetadataCatalog.SaveObject(const AObject: TKMetadata);
begin
  Assert(Assigned(AObject));
  Assert(AObject.PersistentName <> '');

  Writer.SaveTreeToFile(AObject, GetFullFileName(AObject.PersistentName));
end;

{ TKMetadataRegistry }

procedure TKMetadataRegistry.BeforeRegisterClass(const AId: string;
  const AClass: TClass);
begin
  if not AClass.InheritsFrom(TKMetadata) then
    raise EKError.CreateFmt('Cannot regisater class %s (Id %s). Class is not a TKMetadata subclass.', [AClass.ClassName, AId]);
  inherited;
end;

class destructor TKMetadataRegistry.Destroy;
begin
  FreeAndNil(FInstance);
end;

function TKMetadataRegistry.GetClass(const AId: string): TKMetadataClass;
begin
  Result := TKMetadataClass(inherited GetClass(AId));
end;

class function TKMetadataRegistry.GetInstance: TKMetadataRegistry;
begin
  if FInstance = nil then
    FInstance := TKMetadataRegistry.Create;
  Result := TKMetadataRegistry(FInstance);
end;

{ TKMetadata }

procedure TKMetadata.Assign(const ASource: TEFTree);
begin
  inherited;
  if Assigned(ASource) and (ASource is TKMetadata) then
    FPersistentName := TKMetadata(ASource).PersistentName;
end;

function TKMetadata.GetIsPersistent: Boolean;
begin
  Result := PersistentName <> '';
end;

function TKMetadata.GetPersistentFileName: string;
begin
  Assert(Assigned(FCatalog));

  Result := FCatalog.GetFullFileName(PersistentName);
end;

function TKMetadata.GetResourceURI: string;
begin
  if PersistentName = '' then
    Result := ''
  else
    Result := 'metadata://' + GetClassNameForResourceURI + '/' + PersistentName;
end;

function TKMetadata.IsAccessGranted(const AMode: string): Boolean;
begin
  Result := TKConfig.Instance.IsAccessGranted(GetResourceURI, AMode);
end;

class function TKMetadata.GetClassNameForResourceURI: string;
begin
  Result := StripPrefix(ClassName, 'TK');
end;

{ TKMetadataItem }

class function TKMetadataItem.GetClassNameForResourceURI: string;
begin
  Result := StripPrefix(ClassName, 'TK');
end;

function TKMetadataItem.GetResourceURI: string;
begin
  if Parent is TKMetadata then
    Result := TKMetadata(Parent).GetResourceURI + '/' + GetClassNameForResourceURI
  else
    Result := 'metadata://' + GetClassNameForResourceURI;
end;

function TKMetadataItem.IsAccessGranted(const AMode: string): Boolean;
begin
  Result := TKConfig.Instance.IsAccessGranted(GetResourceURI, AMode);
end;

end.

