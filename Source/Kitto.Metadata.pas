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
  Classes, Generics.Collections, Types,
  EF.Types, EF.Tree, EF.YAML, EF.ObserverIntf;

type
  TKMetadataCatalog = class;

  /// <summary>
  ///  A metadata object is a tree object (often persistent) that is
  ///  managed by a catalog. There are catalogs for models, views,
  ///  layouts.
  /// </summary>
  TKMetadata = class(TEFPersistentTree)
  private
    FCatalog: TKMetadataCatalog;
    function BuildURI(const AName: string): string;
  strict protected
    class function GetClassNameForResourceURI: string; virtual;
    function GetPersistentFileName: string; override;
  public
    const SYS_PREFIX = 'Sys$';

    property Catalog: TKMetadataCatalog read FCatalog;

    /// <summary>
    ///  Returns a string URI that uniquely identifies the object, to
    ///  be used for access control or other purposes.
    /// </summary>
    function GetResourceURI: string; virtual;

    /// <summary>
    ///  Returns a string URI that (non necessarily uniquely) identifies the
    ///  object for access control purposes. By default, returns the same value
    ///  as GetResourceURI.
    /// </summary>
    function GetACURI: string; virtual;

    /// <summary>
    ///  Returns true if access is granted to the resource representing
    ///  the metadata object in the specified mode. This is a shortcut to
    ///  calling TKConfig.Instance.IsAccessGranted (possibly multiple times for
    ///  cascading, and "or"ing the results).
    /// </summary>
    function IsAccessGranted(const AMode: string): Boolean; virtual;

    /// <summary>
    ///  Calls IsAccessGranted and raises an exception if the method returns False.
    ///  This is a shortcut to calling TKConfig.Instance.CheckAccessGranted. (possibly multiple times for
    /// </summary>
    procedure CheckAccessGranted(const AMode: string);

    /// <summary>
    ///  Makes the current object a copy of the specified tree.
    /// </summary>
    procedure Assign(const ASource: TEFTree); override;
  end;

  TKMetadataClass = class of TKMetadata;

  TKMetadataItem = class(TEFNode)
  strict protected
    class function GetClassNameForResourceURI: string; virtual;
  public
    /// <summary>
    ///  Returns a string URI that uniquely identifies the object, to
    ///  be used for access control and other purposes.
    /// </summary>
    function GetResourceURI: string; virtual;

    /// <summary>
    ///  Returns a string URI that (non necessarily uniquely) identifies the
    ///  object for access control purposes. By default, returns the same value
    ///  as GetResourceURI.
    /// </summary>
    function GetACURI: string; virtual;

    /// <summary>
    ///  Returns True if access is granted to the resource representing
    ///  the metadata object in the specified mode. This is a shortcut to
    ///  calling TKConfig.Instance.IsAccessGranted (possibly multiple times for
    ///  cascading, and "or"ing the results).
    /// </summary>
    function IsAccessGranted(const AMode: string): Boolean; virtual;

    /// <summary>
    ///  Calls IsAccessGranted and raises an exception if the method returns False.
    ///  This is a shortcut to calling TKConfig.Instance.CheckAccessGranted. (possibly multiple times for
    /// </summary>
    procedure CheckAccessGranted(const AMode: string);
  end;

  TKMetadataRegistry = class;

  TKMetadataCatalog = class(TEFSubjectAndObserver)
  strict private
    FPath: string;
    FIndex: TStringList;
    FReader: TEFYAMLReader;
    FWriter: TEFYAMLWriter;
    FDisposedObjects: TList<TKMetadata>;
    FNonpersistentObjects: TDictionary<TEFNode, TKMetadata>;
    FDynamicObjects: TDictionary<string, TKMetadata>;
    procedure RefreshIndex;
    function GetObjectCount: Integer;
    function GetReader: TEFYAMLReader;
    function LoadObject(const AName: string): TKMetadata;
    function GetWriter: TEFYAMLWriter;
    function GetObject(I: Integer): TKMetadata;
    function ObjectExists(const AName: string): Boolean;
    procedure DuplicateObjectError(const AName: string);
    procedure AfterAddObject(const AObject: TKMetadata);
    function FixObjectClassType(const AObject: TKMetadata): TKMetadata;
    procedure CreateIndex;

    /// <summary>Frees and deletes all persistent objects that don't exist on
    /// disk anymore.</summary>
    procedure Purge;
    procedure ObjectAdded(const AFileName: string);
    procedure ObjectRemoved(const AFileName: string);
    procedure ObjectDisposed(const AFileName: string);
  strict protected
    procedure ObjectNotFound(const AName: string);
    // Delete file and free object.
    procedure DisposeObject(const AObject: TKMetadata);
    property Reader: TEFYAMLReader read GetReader;
    property Writer: TEFYAMLWriter read GetWriter;
    procedure AfterCreateObject(const AObject: TKMetadata); virtual;
    procedure SetPath(const AValue: string); virtual;
    function GetObjectClassType: TKMetadataClass; virtual; abstract;
    function GetDefaultObjectTypeName: string; virtual;
    function GetMetadataRegistry: TKMetadataRegistry; virtual; abstract;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function FindNonpersistentObject(const ANode: TEFNode): TKMetadata;
    procedure AddNonpersistentObject(const AObject: TKMetadata; const ANode: TEFNode);
    procedure DeleteNonpersistentObject(const ANode: TEFNode);

    function FindDynamicObject(const AName: string): TKMetadata;
    procedure AddDynamicObject(const AObject: TKMetadata; const AName: string);
    procedure DeleteDynamicObject(const AName: string);
  public
    property Path: string read FPath write SetPath;

    /// <summary>Returns the full file name for the specified persistent
    /// name.</summary>
    function GetFullFileName(const AName: string): string;

    /// <summary>Closes and reopens the catalog, rebuilds the index and
    /// invalidates all objects.</summary>
    /// <remarks>Upon calling this method, any pointer to an object loaded by
    /// the catalog is no longer valid.</remarks>
    procedure Open; virtual;

    /// <summary>Returns true if the catalog is open.</summary>
    /// <remarks>Currently returns False if the catalog is open but
    /// empty.</remarks>
    function IsOpen: Boolean;

    /// <summary>Rebuilds the index preserving any objects already created. An
    /// object that is no longer part of the rebuilt index, though (for example
    /// when a file is deleted from the outside), is destroyed
    /// anyway.</summary>
    /// <remarks>If the catalog is not open, calling this method raises an
    /// exception.</remarks>
    procedure Refresh; virtual;

    /// <summary>Closes the catalog and frees all objects.</summary>
    procedure Close; virtual;

    property ObjectCount: Integer read GetObjectCount;
    property Objects[I: Integer]: TKMetadata read GetObject;
    function FindObject(const AName: string): TKMetadata; overload;
    function FindObject(const ANames: TStringDynArray): TKMetadata; overload;
    type TPredicate = reference to function (const AObject: TKMetadata): Boolean;
    function FindObjectByPredicate(const APredicate: TPredicate): TKMetadata;

    function ObjectByName(const AName: string): TKMetadata; overload;
    function ObjectByName(const ANames: TStringDynArray): TKMetadata; overload;

    function ObjectByNode(const ANode: TEFNode): TKMetadata;
    function FindObjectByNode(const ANode: TEFNode): TKMetadata;

    procedure AddObject(const AObject: TKMetadata);
    procedure RemoveObject(const AObject: TKMetadata);
    procedure DeleteObject(const AIndex: Integer);

    /// <summary>
    ///   Marks the object as disposed and removes it from the index (but
    ///   doesn't free it). The file is then deleted when SaveAll is called.
    /// </summary>
    /// <param name="AObject">
    ///   Object to be disposed.
    /// </param>
    procedure MarkObjectAsDisposed(const AObject: TKMetadata);

    /// <summary>
    ///   Saves all modified objects in the catalog and disposes all objects
    ///   marked for disposition.
    /// </summary>
    procedure SaveAll;

    procedure SaveObject(const AObject: TKMetadata);
  end;

  TKMetadataRegistry = class(TEFRegistry)
  public
    /// <summary>
    ///   Returns a reference to the class identified by Id1; falls back to Id2
    ///   if the first one is not found. Looks for system classes as well.
    /// </summary>
    /// <param name="AId1">
    ///   Id of the primary class requested.
    /// </param>
    /// <param name="AId2">
    ///   Id of the fallback class.
    /// </param>
    /// <remarks>
    ///   The search sequence is:<br />Id1 user class<br />Id2 user class
    ///   <br />Id1 system class<br />Id2 system class<br />Error
    /// </remarks>
    function GetClass(const AId1, AId2: string): TKMetadataClass;

    /// <summary>
    ///   Like GetClass, but returns nil instead of raiseing errors if no class
    ///   found.
    /// </summary>
    function FindClass(const AId1, AId2: string): TKMetadataClass;
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
  FDisposedObjects := TList<TKMetadata>.Create;
  FNonpersistentObjects := TDictionary<TEFNode, TKMetadata>.Create;
  FDynamicObjects := TDictionary<string, TKMetadata>.Create;
end;

procedure TKMetadataCatalog.CreateIndex;
begin
  FIndex := TStringList.Create;
  FIndex.Sorted := True;
  Findex.Duplicates := dupError;
  Findex.CaseSensitive := False;
end;

procedure TKMetadataCatalog.AfterCreateObject(const AObject: TKMetadata);
begin
  AObject.FCatalog := Self;
end;

procedure TKMetadataCatalog.AfterAddObject(const AObject: TKMetadata);
begin
  AObject.FCatalog := Self;
  ObjectAdded(AObject.PersistentFileName);
end;

procedure TKMetadataCatalog.ObjectAdded(const AFileName: string);
begin
  NotifyObservers('ObjectAdded' + #9 + AFileName);
end;

procedure TKMetadataCatalog.ObjectRemoved(const AFileName: string);
begin
  NotifyObservers('ObjectRemoved' + #9 + AFileName);
end;

procedure TKMetadataCatalog.Close;
var
  I: Integer;
  LObject: TKMetadata;
begin
  Assert(IsOpen);

  for LObject in FNonpersistentObjects.Values do
    LObject.Free;
  FNonpersistentObjects.Clear;

  for LObject in FDynamicObjects.Values do
    LObject.Free;
  FDynamicObjects.Clear;

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
  FreeAndNil(FIndex);
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
      SysUtils.DeleteFile(LFileName);
  end;
  AObject.Free;
end;

procedure TKMetadataCatalog.DuplicateObjectError(const AName: string);
begin
  raise EKError.CreateFmt(_('Duplicate object %s.'), [AName]);
end;

procedure TKMetadataCatalog.DeleteDynamicObject(const AName: string);
begin
  FDynamicObjects.Remove(AName);
end;

procedure TKMetadataCatalog.DeleteNonpersistentObject(const ANode: TEFNode);
begin
  FNonpersistentObjects.Remove(ANode);
end;

procedure TKMetadataCatalog.DeleteObject(const AIndex: Integer);
var
  LFileName: string;
begin
  LFileName := Objects[AIndex].PersistentFileName;
  FIndex.Objects[AIndex].Free;
  FIndex.Delete(AIndex);
  ObjectRemoved(LFileName);
end;

procedure TKMetadataCatalog.RemoveObject(const AObject: TKMetadata);
var
  LFileName: string;
begin
  Assert(IsOpen);

  if ObjectExists(AObject.PersistentName) then
  begin
    LFileName := AObject.PersistentFileName;
    FIndex.Delete(FIndex.IndexOf(AObject.PersistentName));
    ObjectRemoved(LFileName);
  end;
end;

destructor TKMetadataCatalog.Destroy;
begin
  if IsOpen then
    Close;
  FreeAndNil(FDisposedObjects);
  FreeAndNil(FNonpersistentObjects);
  FreeAndNil(FDynamicObjects);
  FreeAndNil(FReader);
  FreeAndNil(FWriter);
  inherited;
end;

procedure TKMetadataCatalog.MarkObjectAsDisposed(const AObject: TKMetadata);
var
  LIndex: Integer;
begin
  Assert(IsOpen);

  FDisposedObjects.Add(AObject);
  if FIndex.Find(AObject.PersistentName, LIndex) then
  begin
    Assert(TKMetadata(FIndex.Objects[LIndex]) = AObject);
    FIndex.Delete(LIndex);
    ObjectDisposed(AObject.PersistentFileName);
  end;
end;

function TKMetadataCatalog.GetDefaultObjectTypeName: string;
begin
  Result := '';
end;

function TKMetadataCatalog.GetFullFileName(const AName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FPath) + AName + '.yaml';
end;

function TKMetadataCatalog.GetObject(I: Integer): TKMetadata;
begin
  Assert(IsOpen);

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
  Assert(IsOpen);

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

function TKMetadataCatalog.IsOpen: Boolean;
begin
  Result := Assigned(FIndex);
end;

{$WARN SYMBOL_PLATFORM OFF}
procedure TKMetadataCatalog.Refresh;
begin
  Assert(IsOpen);

  RefreshIndex;
end;

procedure TKMetadataCatalog.RefreshIndex;
var
  LResult: Integer;
  LSearchRec: TSearchRec;
  LBaseName: string;
  LIndex: Integer;
begin
  Assert(IsOpen);

  // Add new files and update existing files.
  LResult := SysUtils.FindFirst(IncludeTrailingPathDelimiter(FPath) + '*.yaml', faNormal, LSearchRec);
  while LResult = 0 do
  begin
    LBaseName := ChangeFileExt(LSearchRec.Name, '');
    if FIndex.Find(LBaseName, LIndex) then
      Objects[LIndex].LoadFromYamlFile(GetFullFileName(LBaseName))
    else
    begin
      FIndex.AddObject(LBaseName, nil);
      ObjectAdded(LBaseName);
    end;
    LResult := SysUtils.FindNext(LSearchRec);
  end;
  SysUtils.FindClose(LSearchRec);
  // Delete no longer existing files.
  Purge;
end;
{$WARN SYMBOL_PLATFORM ON}

procedure TKMetadataCatalog.Purge;
var
  I: Integer;
begin
  Assert(IsOpen);

  for I := ObjectCount - 1 downto 0 do
  begin
    if not FileExists(Objects[I].PersistentFileName) then
      DeleteObject(I);
  end;
end;

function TKMetadataCatalog.FindDynamicObject(const AName: string): TKMetadata;
begin
  if FDynamicObjects.ContainsKey(AName) then
    Result := FDynamicObjects[AName]
  else
    Result := nil;
end;

function TKMetadataCatalog.FindNonpersistentObject(const ANode: TEFNode): TKMetadata;
begin
  if FNonpersistentObjects.ContainsKey(ANode) then
    Result := FNonpersistentObjects[ANode]
  else
    Result := nil;
end;

function TKMetadataCatalog.FindObject(const AName: string): TKMetadata;
var
  LIndex: Integer;
  LClass: TKMetadataClass;
begin
  Assert(IsOpen);

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
  begin
    Result := FindDynamicObject(AName);
    if not Assigned(Result) then
    begin
      LClass := GetMetadataRegistry.FindClass(AName, '');
      if Assigned(LClass) then
      begin
        Result := LClass.Create;
        Result.PersistentName := AName;
        AddDynamicObject(Result, AName);
      end;
    end;
  end;
end;

function TKMetadataCatalog.FindObjectByPredicate(
  const APredicate: TPredicate): TKMetadata;
var
  I: Integer;
begin
  Assert(IsOpen);
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

function TKMetadataCatalog.ObjectByName(const ANames: TStringDynArray): TKMetadata;
begin
  Result := FindObject(ANames);
  if Result = nil then
    ObjectNotFound(string.Join('|', ANames));
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

procedure TKMetadataCatalog.ObjectDisposed(const AFileName: string);
begin
  NotifyObservers('ObjectDisposed' + #9 + AFileName);
end;

function TKMetadataCatalog.FindObject(const ANames: TStringDynArray): TKMetadata;
var
  AName: string;
begin
  Result := nil;
  for AName in ANames do
  begin
    Result := FindObject(AName);
    if Assigned(Result) then
      Break;
  end;
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
  Assert(IsOpen);

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
    Result.AfterLoad;
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
  LDeclaredClassName := AObject.GetString('Type', GetDefaultObjectTypeName);
  LDeclaredClassType := GetMetadataRegistry.FindClass(LDeclaredClassName, AObject.PersistentName);
  if Assigned(LDeclaredClassType) and (LDeclaredClassType <> AObject.ClassType) then
  begin
    Result := LDeclaredClassType.Clone(AObject);
    AObject.Free;
  end
  else
    Result := AObject;
end;

procedure TKMetadataCatalog.AddDynamicObject(const AObject: TKMetadata;
  const AName: string);
begin
  FDynamicObjects.Add(AName, AObject);
  AfterAddObject(AObject);
end;

procedure TKMetadataCatalog.AddNonpersistentObject(const AObject: TKMetadata;
  const ANode: TEFNode);
begin
  FNonpersistentObjects.Add(ANode, AObject);
  AfterAddObject(AObject);
end;

procedure TKMetadataCatalog.AddObject(const AObject: TKMetadata);
begin
  Assert(IsOpen);
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
  Assert(not IsOpen);

  CreateIndex;
  RefreshIndex;
end;

procedure TKMetadataCatalog.SaveAll;
var
  I: Integer;
begin
  Assert(IsOpen);

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

function TKMetadataRegistry.FindClass(const AId1, AId2: string): TKMetadataClass;

  function FindClassIfNotEmpty(const AId: string): TKMetadataClass;
  begin
    if AId <> '' then
      Result := TKMetadataClass(inherited FindClass(AId))
    else
      Result := nil;
  end;

begin
  Result := FindClassIfNotEmpty(AId1);
  if not Assigned(Result) then
  begin
    Result := FindClassIfNotEmpty(AId2);
    if not Assigned(Result) then
    begin
      Result := FindClassIfNotEmpty(TKMetadata.SYS_PREFIX + AId1);
      if not Assigned(Result) then
        Result := FindClassIfNotEmpty(TKMetadata.SYS_PREFIX + AId2);
    end;
  end;
end;

function TKMetadataRegistry.GetClass(const AId1, AId2: string): TKMetadataClass;

  function FindClassIfNotEmpty(const AId: string): TKMetadataClass;
  begin
    if AId <> '' then
      Result := TKMetadataClass(inherited FindClass(AId))
    else
      Result := nil;
  end;

begin
  Result := FindClass(AId1, AId2);
  if not Assigned(Result) then
  begin
    if AId1 <> '' then
      ClassNotFound(AId1)
    else
      ClassNotFound(AId2);
  end;
end;

{ TKMetadata }

function TKMetadata.GetPersistentFileName: string;
begin
  Assert(Assigned(FCatalog));

  Result := FCatalog.GetFullFileName(PersistentName);
end;

function TKMetadata.GetResourceURI: string;
var
  LName: string;
begin
  LName := GetString('ResourceName');
  if LName = '' then
    LName := PersistentName;
  if LName = '' then
    Result := ''
  else
    Result := BuildURI(LName);
end;

function TKMetadata.IsAccessGranted(const AMode: string): Boolean;
begin
  Result := TKConfig.Instance.IsAccessGranted(GetACURI, AMode);
end;

procedure TKMetadata.Assign(const ASource: TEFTree);
begin
  inherited;
  if Assigned(ASource) and (ASource is TKMetadata) then
    FCatalog := TKMetadata(ASource).Catalog;
end;

procedure TKMetadata.CheckAccessGranted(const AMode: string);
begin
  TKConfig.Instance.CheckAccessGranted(GetACURI, AMode);
end;

function TKMetadata.GetACURI: string;
var
  LName: string;
begin
  LName := GetString('ACName');
  if LName = '' then
    LName := GetString('ResourceName');
  if LName = '' then
    LName := PersistentName;
  if LName = '' then
    Result := ''
  else
    Result := BuildURI(LName);
end;

function TKMetadata.BuildURI(const AName: string): string;
begin
  Result := 'metadata://' + GetClassNameForResourceURI + '/' + AName;
end;


class function TKMetadata.GetClassNameForResourceURI: string;
begin
  Result := StripPrefix(ClassName, 'TK');
end;

{ TKMetadataItem }

procedure TKMetadataItem.CheckAccessGranted(const AMode: string);
begin
  TKConfig.Instance.CheckAccessGranted(GetACURI, AMode);
end;

function TKMetadataItem.GetACURI: string;
begin
  Result := GetResourceURI;
end;

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
  Result := TKConfig.Instance.IsAccessGranted(GetACURI, AMode);
end;

end.

