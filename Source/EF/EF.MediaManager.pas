{
  This unit defines a class that can provide media items (mostly images
  to be used in menus and buttons) identified each by an unique name,
  fetching them from different sources, such as disk files and resources in
  the main executable file or in external library files.
  
  An index stored as an external text file locates each media item by its name
  in a media library. A media library is a collection of media items, and it
  can be a directory, a DLL, the main executable file, etc. Each media library
  is loaded on first access and then kept in memory until shutdown or
  explicit unload.
}
unit EF.MediaManager;

interface

uses
  Windows, Classes, Contnrs, Graphics, ImgList;

type
  {
    Encapsulates the on-disk index that locates all media items. The media index
    is a text file with one line for each item, in the format:
    <media item name>=<library specification>.
  }
  TEFMediaIndex = class
  private
    FIndex: TStringList;
    {
      Adds the data in AFileName to the index contents. Raises an exception in
      case of duplicates.
    }
    procedure AddFromFile(const AFileName: string);
  public
    constructor Create;
    destructor Destroy; override;
    function GetLibraryNameByMediaItemName(const AMediaItemName: string): string;
  end;

  TEFMediaManager = class;

 {
    A media library is an abstract set of media items. Concrete sets may be
    files in a folder, resources in a module, etc.
    It is identified by a name and can provide media items in several ways
    (see the public interface).
  }
  TEFMediaLibrary = class
  private
    FName: string;
  protected
    {
      Implements GetBitmap.
    }
    function InternalGetBitmap(const ABitmapName: string; const AOutputBitmap: TBitmap): Boolean; virtual;
    {
      Implements GetIcon.
    }
    function InternalGetIcon(const AIconName: string; const AOutputIcon: TIcon): Boolean; virtual;
    {
      Implements GetImageList.
    }
    function InternalGetImageList(const ABitmapNames: array of string;
      const AOutputImageList: TCustomImageList): Boolean;
    {
      Implements AddBitmapToImageList.
    }
    function InternalAddBitmapToImageList(const ABitmapName: string;
      const AOutputImageList: TCustomImageList): Boolean; virtual;
    {
      Implements HasMediaItem.
    }
    function InternalHasMediaItem(const AMediaItemName: string): Boolean; virtual;
    {
      Implements GetResource.
    }
    function InternalGetResource(const AResourceName: string;
      const AResourceType: PChar; out AResourceHandle: THandle): Boolean; virtual;
    {
      Implements GetMediaItemData.
    }
    function InternalGetMediaItemData(const AMediaItemName: string;
      const AOutputStream: TStream): Boolean; virtual;
  public
    constructor Create(const AName: string); virtual;
    {
      Copies the bitmap with the specified name to AOutputBitmap and returns
      True. If the bitmap cannot be found, returns False.
    }
    function GetBitmap(const ABitmapName: string; const AOutputBitmap: TBitmap): Boolean;
    {
      Copies the icon with the specified name to AOutputIcon and returns
      True. If the icon cannot be found, returns False.
    }
    function GetIcon(const AIconName: string; const AOutputIcon: TIcon): Boolean;
    {
      Adds all specified bitmaps to AOutputImageList and returns True. If
      a bitmap cannot be added, breaks out and returns False.
    }
    function GetImageList(const ABitmapNames: array of string;
      const AOutputImageList: TCustomImageList): Boolean;
    {
      Adds the bitmap with the specified name to AOutputImageList and returns
      True. If the bitmap cannot be added, returns False.
    }
    function AddBitmapToImageList(const ABitmapName: string;
      const AOutputImageList: TCustomImageList): Boolean;
    {
      If the library contains a resource with the given name, returns the
      library handle which can be used to load the resource autonomously by
      the caller. Not all library kinds support resources (actually only
      resource libraries do).
    }
    function GetResource(const AResourceName: string; const AResourceType: PChar;
      out AResourceHandle: THandle): Boolean;
    {
      Writes the requested media item's data to a stream and returns True.
      If the item is not found, doesn't write anything and returns False.
    }
    function GetMediaItemData(const AMediaItemName: string;
      const AOutputStream: TStream): Boolean;
    {
      Uniquely identifies the library. May be a file name.
    }
    property Name: string read FName;
    {
      Returns True if the library has the requested media item, False otherwise.
      It is called in cases when an index is not available or is incomplete.
    }
    function HasMediaItem(const AMediaItemName: string): Boolean;
  end;

  {
    Base class for media libraries that fetch media items from module resources.
    Currently only supports bitmaps and 'AVI' resources.
  }
  TEFResourceMediaLibrary = class(TEFMediaLibrary)
  private
    FModuleHandle: THandle;
  protected
    function InternalGetResource(const AResourceName: string;
      const AResourceType: PChar; out AResourceHandle: THandle): Boolean; override;
    function InternalGetMediaItemData(const AMediaItemName: string;
      const AOutputStream: TStream): Boolean; override;
    property ModuleHandle: THandle read FModuleHandle;
    function AcquireModuleHandle: THandle; virtual; abstract;
    procedure ReleaseModuleHandle(const AHandle: THandle); virtual;
    function InternalAddBitmapToImageList(const ABitmapName: string;
      const AOutputImageList: TCustomImageList): Boolean; override;
    function InternalGetBitmap(const ABitmapName: string;
      const AOutputBitmap: TBitmap): Boolean; override;
    function InternalGetIcon(const AIconName: string; const AOutputIcon: TIcon): Boolean; override;
    function InternalHasMediaItem(const AMediaItemName: string): Boolean; override;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;
  end;

  {
    A media library whose media items are resources stored in a dynamically
    loaded DLL file.
  }
  TEFDLLMediaLibrary = class(TEFResourceMediaLibrary)
  protected
    function AcquireModuleHandle: THandle; override;
    procedure ReleaseModuleHandle(const AHandle: THandle); override;
  end;

  {
    A media library whose media items are resources stored in the main
    application file.
  }
  TEFBuiltinMediaLibrary = class(TEFResourceMediaLibrary)
  protected
    function AcquireModuleHandle: THandle; override;
  end;

  {
    A media library that fetches media items from a file system directory
    whose path is specified in Name. Item names are transformed into file
    names by prepending the path and changing the last underscore to a dot.
    Example:
    some_media_item_bmp -> C:\Some\Path\Media\some_media_item.bmp
    This is done because, by convention, a media item name cannot contain
    the dot character (as not all media library types support it).
  }
  TEFDirectoryMediaLibrary = class(TEFMediaLibrary)
  private
    {
      Builds and returns the file name for the specified media item.
    }
    function GetMediaItemFileName(const AMediaItemName: string): string;
  protected
    function InternalAddBitmapToImageList(const ABitmapName: string;
      const AOutputImageList: TCustomImageList): Boolean; override;
    function InternalGetBitmap(const ABitmapName: string;
      const AOutputBitmap: TBitmap): Boolean; override;
    function InternalGetIcon(const AIconName: string; const AOutputIcon: TIcon): Boolean; override;
    function InternalHasMediaItem(const AMediaItemName: string): Boolean; override;
    function InternalGetMediaItemData(const AMediaItemName: string;
      const AOutputStream: TStream): Boolean; override;
  end;

  {
    A library that doesn't contain anything.
    Used as a null object to avoid "if Assigned()" kind of code.
  }
  TEFNullMediaLibrary = class(TEFMediaLibrary)
  end;

  {
    Manages the index and a set of media libraries, loading each at startup.
    Forwards each media item request to the appropriate library according
    to the index. Items not listed in the index are looked for sequentially in
    all media libraries (see TEFDirectoryMediaLibrary for naming rules).
  }
  TEFMediaManager = class(TComponent)
  private
    FMediaPaths: string;
    FLibraries: TObjectList;
    FNullLibrary: TEFNullMediaLibrary;
    FMediaIndex: TEFMediaIndex;
    function GetLibraryIndexByMediaItemName(const AMediaItemName: string): Integer;
    function GetMediaIndex: TEFMediaIndex;
    procedure SetMediaPaths(const AValue: string);
    {
      Creates all default libraries. Default libraries are
      directory and dll libraries concerning the paths in FMediaPaths, plus
      the 'Builtin' resource library.
    }
    procedure CreateDefaultLibraries;
    {
      Makes sure the default libraries are creates. Calls CreateDefaultLibraries
      if the list of libraries is empty.
    }
    procedure EnsureDefaultLibraries;
    property MediaIndex: TEFMediaIndex read GetMediaIndex;
    procedure GetMediaIndexNames(const APath: string; const AFileNameList: TStrings);
    function GetLibrary(const AIndex: Integer): TEFMediaLibrary;
    function GetLibraryCount: Integer;
    // Factory method; creates a library whose type and name are determined
    // by analyzing the given specification, according to the following
    // information:
    // 'Builtin' creates an instance of TEFBuiltinMediaLibrary named 'Builtin'.
    // A valid and existing directory name creates an instance of
    // TEFFolderMediaLibrary named after the directory name.
    // A valid and existing file name creates an instance of TEFDLLMediaLibrary
    // named after the full file name.
    function CreateLibrary(const ALibrarySpecification: string): TEFMediaLibrary;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {
      Access to the currently loaded libraries.
    }
    property Libraries[const AIndex: Integer]: TEFMediaLibrary read GetLibrary;
    {
      Count of currently loaded libraries.
    }
    property LibraryCount: Integer read GetLibraryCount;
    {
      Adds a custom library and takes ownership of it. The caller may
      remove the library when no longer needed, which will also free it.
      The method returns the position in the list at which the library was
      added.
    }
    function AddLibrary(const ALibrary: TEFMediaLibrary): Integer;
    {
      Removes the specified library, if found in the media manager, and frees it.
      Returns the position in the list at which the library was found before
      removal, or False if it wasn't found.
    }
    function RemoveLibrary(const ALibrary: TEFMediaLibrary): Integer;
    {
      Removes the library at the specified position in the list of libraries,
      and frees it.
    }
    procedure DeleteLibrary(const AIndex: Integer);
    {
      Unloads all loaded libraries freeing up the memory occupied by them.
      Each library will be reloaded upon next access.
    }
    procedure ClearLibraries;
    {
      Copies the bitmap with the specified name to AOutputBitmap and returns
      True. If the media item cannot be found, returns False.
    }
    function GetBitmap(const ABitmapName: string;
      const AOutputBitmap: TBitmap): Boolean;
    {
      Copies the icon with the specified name to AOutputIcon and returns
      True. If the media item cannot be found, returns False.
    }
    function GetIcon(const AIconName: string;
      const AOutputIcon: TIcon): Boolean;
    {
      Adds all specified bitmaps to AOutputImageList and returns True. If
      a bitmap cannot be added, breaks out and returns False.
    }
    function GetImageList(const ABitmapNames: array of string;
      const AOutputImageList: TCustomImageList): Boolean;
    // If the resource is found, then the handle of the resource library
    // that contains it is returned in AResourceHandle. This allows the caller
    // to use the name + handle to load and process the resource autonomously.
    // This call is only supprted by resource media libraries.
    function GetResource(const AResourceName: string;
      const AResourceType: PChar; out AResourceHandle: THandle): Boolean; deprecated;
    {
      Writes the requested media item's data to a stream and returns True.
      If the item is not found, doesn't write anything and returns False.
    }
    function GetMediaItemData(const AMediaItemName: string;
      const AOutputStream: TStream): Boolean;
  published
    {
      A list of folders containing media libraries and files. Defaults to a
      subfolder called Media in the application's directory.
    }
    property MediaPaths: string read FMediaPaths write SetMediaPaths;
  end;

  {
    Utility class that can be used by applications to associate image list
    indexes to media item names. Adds a locate-by-name capability to
    the image list class.
    Caution: once you pass an image list to an instance of this class, you
    should NOT add or delete images or otherwise work with it from the outside.
  }
  TEFImageIndexMap = class
  private
    FItemNames: TStringList;
    FImageList: TCustomImageList;
    FMediaManager: TEFMediaManager;
  public
    constructor Create(const AMediaManager: TEFMediaManager;
      const AImageList: TCustomImageList);
    destructor Destroy; override;
    {
      At the first call acquires the image from the media manager,
      adds it to the image list and returns the index. Then it just returns
      the image index.
    }
    function GetImageIndex(const AImageName: string): Integer;
    {
      Gives access to the image list this map is referring to.
    }
    property ImageList: TCustomImageList read FImageList;
    {
      Clears both its internal list of image indexes and the associated
      image list.
    }
    procedure Clear;
  end;

{
  Gives access to the singleton media manager instance.
}
function MediaManager: TEFMediaManager;

implementation

uses
  SysUtils, Controls,
  EF.SysUtils, EF.StrUtils;

var
  _EFMediaManager: TEFMediaManager;

resourcestring
  SMediaNotFound = 'Media item "%s" not found.';
  SMediaLibraryNotFound = 'Media library "%s" not found.';

const
  DEFAULT_LIBRARY_INDEX = 0;

function MediaManager: TEFMediaManager;
begin
  if not Assigned(_EFMediaManager) then
    _EFMediaManager := TEFMediaManager.Create(nil);
  Result := _EFMediaManager;
end;

function GetModulePath(const ASubfolder: string = ''): string;
var
  Buffer: array[0..MAX_PATH] of char;
begin
  if GetModuleFileName(HInstance, Buffer, SizeOf(Buffer)) > 0 then
    Result := ExtractFilePath(Buffer)
  else
    Result := '';

  if ASubfolder <> '' then
    if Result = '' then
      Result := ASubfolder
    else
      Result := IncludeTrailingPathDelimiter(Result) + ASubfolder;

  if Pos(' ', Result) > 0 then
    Result := '"' + Result + '"';
end;

procedure BitmapToIcon(const ASourceBitmap: TBitmap;
  const ADestinationIcon: TIcon);
begin
  with TImageList.CreateSize(ASourceBitmap.Width, ASourceBitmap.Height) do
  begin
    try
      AllocBy := 1;
      // By convention, set the lower left corner color as the
      // transparent color.
      AddMasked(ASourceBitmap,
        ASourceBitmap.Canvas.Pixels[0, ASourceBitmap.Height - 1]);
      GetIcon(0, ADestinationIcon);
    finally
      Free;
    end;
  end;
end;

{ TEFMediaManager }

constructor TEFMediaManager.Create(AOwner: TComponent);
begin
  inherited;
  FLibraries := TObjectList.Create(True);
  FNullLibrary := TEFNullMediaLibrary.Create('');
  MediaPaths := GetModulePath('Media');
end;

function TEFMediaManager.CreateLibrary(const ALibrarySpecification: string): TEFMediaLibrary;
var
  LLibrarySpecification: string;
begin
  LLibrarySpecification := ALibrarySpecification;
  if LLibrarySpecification = 'Builtin' then
    Result := TEFBuiltinMediaLibrary.Create(LLibrarySpecification)
  else
  begin
    if DirectoryExists(LLibrarySpecification) then
      Result := TEFDirectoryMediaLibrary.Create(LLibrarySpecification)
    else if FileExists(LLibrarySpecification) then
      Result := TEFDLLMediaLibrary.Create(LLibrarySpecification)
    else
      raise Exception.CreateFmt(SMediaLibraryNotFound, [LLibrarySpecification]);
  end;
end;

procedure TEFMediaManager.DeleteLibrary(const AIndex: Integer);
begin
  FLibraries.Delete(AIndex);
end;

destructor TEFMediaManager.Destroy;
begin
  ClearLibraries;
  FreeAndNil(FLibraries);
  FreeAndNil(FMediaIndex);
  FreeAndNil(FNullLibrary);
  inherited;
end;

procedure TEFMediaManager.EnsureDefaultLibraries;
begin
  if LibraryCount = 0 then
    CreateDefaultLibraries;
end;

function TEFMediaManager.AddLibrary(const ALibrary: TEFMediaLibrary): Integer;
begin
  Result := FLibraries.Add(ALibrary);
end;

procedure TEFMediaManager.ClearLibraries;
begin
  FLibraries.Clear;
end;

function TEFMediaManager.GetBitmap(const ABitmapName: string;
  const AOutputBitmap: TBitmap): Boolean;
begin
  Assert(ABitmapName <> '');
  Assert(Assigned(AOutputBitmap));

  Result := Libraries[GetLibraryIndexByMediaItemName(ABitmapName)].GetBitmap(ABitmapName, AOutputBitmap);
end;

function TEFMediaManager.GetIcon(const AIconName: string;
  const AOutputIcon: TIcon): Boolean;
begin
  Assert(AIconName <> '');
  Assert(Assigned(AOutputIcon));
  
  Result := Libraries[GetLibraryIndexByMediaItemName(AIconName)].GetIcon(AIconName, AOutputIcon);
end;

function TEFMediaManager.GetImageList(const ABitmapNames: array of string;
  const AOutputImageList: TCustomImageList): Boolean;
var
  LBitmapNameIndex: Integer;
begin
  Assert(Length(ABitmapNames) > 0);
  Assert(Assigned(AOutputImageList));

  Result := False;
  for LBitmapNameIndex := Low(ABitmapNames) to High(ABitmapNames) do
  begin
    Result := Libraries[GetLibraryIndexByMediaItemName(
      ABitmapNames[LBitmapNameIndex])].AddBitmapToImageList(
      ABitmapNames[LBitmapNameIndex], AOutputImageList);
    if not Result then
      Exit;
  end;
end;

function TEFMediaManager.GetResource(const AResourceName: string;
  const AResourceType: PChar; out AResourceHandle: THandle): Boolean;
begin
  Assert(AResourceName <> '');
  Assert(Assigned(AResourceType));
  
  Result := Libraries[GetLibraryIndexByMediaItemName(AResourceName)].GetResource(AResourceName, AResourceType, AResourceHandle);
end;

function TEFMediaManager.RemoveLibrary(const ALibrary: TEFMediaLibrary): Integer;
begin
  Result := FLibraries.Remove(ALibrary);
end;

function TEFMediaManager.GetMediaItemData(const AMediaItemName: string;
  const AOutputStream: TStream): Boolean;
begin
  Assert(AMediaItemName <> '');
  Assert(Assigned(AOutputStream));

  Result := Libraries[GetLibraryIndexByMediaItemName(AMediaItemName)].GetMediaItemData(
    AMediaItemName, AOutputStream);
end;

function TEFMediaManager.GetLibrary(const AIndex: Integer): TEFMediaLibrary;
begin
  if AIndex = -1 then
    Result := FNullLibrary
  else
    Result := FLibraries[AIndex] as TEFMediaLibrary;
end;

function TEFMediaManager.GetLibraryCount: Integer;
begin
  Result := FLibraries.Count;
end;

function TEFMediaManager.GetLibraryIndexByMediaItemName(const AMediaItemName: string): Integer;
var
  LLibraryName: string;
  LLibraryIndex: Integer;
begin
  EnsureDefaultLibraries;  

  LLibraryName := MediaIndex.GetLibraryNameByMediaItemName(AMediaItemName);
  if LLibraryName = '' then
  begin
    // Entry not found in the index. Scan all loaded libraries to see if any of
    // them has it. Return the index of the first found library, or -1 if no
    // library is found.
    for LLibraryIndex := 0 to LibraryCount - 1 do
    begin
      if Libraries[LLibraryIndex].HasMediaItem(AMediaItemName) then
      begin
        Result := LLibraryIndex;
        Exit;
      end;
    end;
    Result := -1;
    Exit;
  end
  else
  begin
    for LLibraryIndex := 0 to LibraryCount - 1 do
    begin
      if Libraries[LLibraryIndex].Name = LLibraryName then
      begin
        Result := LLibraryIndex;
        Exit;
      end;
    end;
  end;
  // Library not in list: try to load it. An exception is raised if the library
  // cannot be loaded.
  Result := FLibraries.Add(CreateLibrary(LLibraryName));
end;

function TEFMediaManager.GetMediaIndex: TEFMediaIndex;
var
  LPathList: TStrings;
  LPathIndex: Integer;
  LIndexFileNames: TStrings;
  LFileNameIndex: Integer;
begin
  if not Assigned(FMediaIndex) then
  begin
    FMediaIndex := TEFMediaIndex.Create;
    try
      LPathList := TStringList.Create;
      try
        LPathList.Delimiter := ';';
        LPathList.DelimitedText := FMediaPaths;
        for LPathIndex := 0 to LPathList.Count - 1 do
        begin
          LIndexFileNames := TStringList.Create;
          try
            GetMediaIndexNames(LPathList[LPathIndex], LIndexFileNames);
            for LFileNameIndex := 0 to LIndexFileNames.Count - 1 do
              if FileExists(LIndexFileNames[LFileNameIndex]) then
                FMediaIndex.AddFromFile(LIndexFileNames[LFileNameIndex]);
          finally
            FreeAndNil(LIndexFileNames);
          end;
        end;
      finally
        LPathList.Free;
      end;
    except
      FreeAndNil(FMediaIndex);
      raise;
    end;
  end;
  Result := FMediaIndex;
end;

procedure TEFMediaManager.GetMediaIndexNames(const APath: string;
  const AFileNameList: TStrings);
begin
  Assert(Assigned(AFileNameList));

  FindAllFiles('idx', APath, AFileNameList);
end;

procedure TEFMediaManager.SetMediaPaths(const AValue: string);
begin
  if FMediaPaths <> AValue then
  begin
    FMediaPaths := AValue;
    ClearLibraries;
  end;
end;

procedure TEFMediaManager.CreateDefaultLibraries;
var
  LPathList: TStrings;
  LLibraryPath: string;
  LFileNames: TStrings;
  LFileNameIndex: Integer;
  LLibraryIndex: Integer;
begin
  FLibraries.Add(CreateLibrary('Builtin'));
  LPathList := TStringList.Create;
  try
    LPathList.Delimiter := ';';
    LPathList.DelimitedText := FMediaPaths;
    for LLibraryIndex := 0 to LPathList.Count - 1 do
    begin
      LLibraryPath := IncludeTrailingPathDelimiter(LPathList[LLibraryIndex]);
      if DirectoryExists(LLibraryPath) then
      begin
        FLibraries.Add(CreateLibrary(LLibraryPath));
        // A dll library for each dll or package in that folder.
        LFileNames := TStringList.Create;
        try
          // Bpl packages no longer supported as it's too easy to load
          // the rtl from a different Delphi version and wreck the
          // debugger. Resource dlls created with any version of Delphi
          // (or other compilers) pose no problems instead.
          FindAllFiles('dll', LLibraryPath, LFileNames, False);
          for LFileNameIndex := 0 to LFileNames.Count - 1 do
            FLibraries.Add(CreateLibrary(LFileNames[LFileNameIndex]));
        finally
          FreeAndNil(LFileNames);
        end;
      end;
    end;
  finally
    FreeAndNil(LPathList);
  end;
end;

{ TEFMediaIndex }

constructor TEFMediaIndex.Create;
begin
  inherited Create;
  FIndex := TStringList.Create;
  FIndex.Sorted := True;
  FIndex.Duplicates := dupError;
end;

destructor TEFMediaIndex.Destroy;
begin
  FreeAndNil(FIndex);
  inherited;
end;

function TEFMediaIndex.GetLibraryNameByMediaItemName(const AMediaItemName: string): string;
var
  LItemIndex: Integer;
begin
  LItemIndex := FIndex.IndexOfName(AMediaItemName);
  if LItemIndex >= 0 then
    Result := FIndex.ValueFromIndex[LItemIndex]
  else
    Result := '';
end;

procedure TEFMediaIndex.AddFromFile(const AFileName: string);
var
  LStrings: TStrings;
begin
  LStrings := TStringList.Create;
  try
    LStrings.LoadFromFile(AFileName);
    FIndex.AddStrings(LStrings);
  finally
    LStrings.Free;
  end;
end;

{ TEFImageIndexMap }

procedure TEFImageIndexMap.Clear;
begin
  Assert(Assigned(FImageList));
  FItemNames.Clear;
  FImageList.Clear;
end;

constructor TEFImageIndexMap.Create(const AMediaManager: TEFMediaManager;
  const AImageList: TCustomImageList);
begin
  inherited Create;
  Assert(Assigned(AMediaManager));
  Assert(Assigned(AImageList));
  FMediaManager := AMediaManager;
  FImageList := AImageList;
  FImageList.Clear;
  FItemNames := TStringList.Create;
  FItemNames.Sorted := True;
  FItemNames.Duplicates := dupError;
end;

destructor TEFImageIndexMap.Destroy;
begin
  FItemNames.Free;
  inherited;
end;

function TEFImageIndexMap.GetImageIndex(const AImageName: string): Integer;
var
  LNameIndex: Integer;
begin
  if AImageName = '' then
    Result := -1
  else if FItemNames.Find(AImageName, LNameIndex) then
    Result := Integer(FItemNames.Objects[LNameIndex])
  else
  begin
    if FMediaManager.GetImageList([AImageName], FImageList) then
    begin
      Result := FImageList.Count - 1;
      FItemNames.AddObject(AImageName, TObject(Result));
    end
    else
      Result := -1;
  end;
end;

{ TEFMediaLibrary }

function TEFMediaLibrary.AddBitmapToImageList(const ABitmapName: string;
  const AOutputImageList: TCustomImageList): Boolean;
begin
  Result := InternalAddBitmapToImageList(ABitmapName, AOutputImageList);
end;

constructor TEFMediaLibrary.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

function TEFMediaLibrary.GetBitmap(const ABitmapName: string;
  const AOutputBitmap: TBitmap): Boolean;
begin
  Result := InternalGetBitmap(ABitmapName, AOutputBitmap);
end;

function TEFMediaLibrary.GetIcon(const AIconName: string;
  const AOutputIcon: TIcon): Boolean;
begin
  Result := InternalGetIcon(AIconName, AOutputIcon);
end;

function TEFMediaLibrary.GetImageList(const ABitmapNames: array of string;
  const AOutputImageList: TCustomImageList): Boolean;
begin
  Result := InternalGetImageList(ABitmapNames, AOutputImageList);
end;

function TEFMediaLibrary.GetMediaItemData(const AMediaItemName: string;
  const AOutputStream: TStream): Boolean;
begin
  Result := InternalGetMediaItemData(AMediaItemName, AOutputStream);
end;

function TEFMediaLibrary.GetResource(const AResourceName: string;
  const AResourceType: PChar; out AResourceHandle: THandle): Boolean;
begin
  Result := InternalGetResource(AResourceName, AResourceType, AResourceHandle);
end;

function TEFMediaLibrary.HasMediaItem(const AMediaItemName: string): Boolean;
begin
  Result := InternalHasMediaItem(AMediaItemName);
end;

function TEFMediaLibrary.InternalAddBitmapToImageList(const ABitmapName: string;
  const AOutputImageList: TCustomImageList): Boolean;
begin
  Result := False;
end;

function TEFMediaLibrary.InternalGetBitmap(const ABitmapName: string;
  const AOutputBitmap: TBitmap): Boolean;
begin
  Result := False;
end;

function TEFMediaLibrary.InternalGetIcon(const AIconName: string;
  const AOutputIcon: TIcon): Boolean;
begin
  Result := False;
end;

function TEFMediaLibrary.InternalGetImageList(const ABitmapNames: array of string;
  const AOutputImageList: TCustomImageList): Boolean;
var
  LBitmapNameIndex: Integer;
begin
  Result := False;
  if Assigned(AOutputImageList) then begin
    for LBitmapNameIndex := Low(ABitmapNames) to High(ABitmapNames) do begin
      Result := AddBitmapToImageList(ABitmapNames[LBitmapNameIndex], AOutputImageList);
      if not Result then
        Exit;
    end;
  end;
end;

function TEFMediaLibrary.InternalGetMediaItemData(const AMediaItemName: string;
  const AOutputStream: TStream): Boolean;
begin
  Result := False;
end;

function TEFMediaLibrary.InternalGetResource(const AResourceName: string;
  const AResourceType: PChar; out AResourceHandle: THandle): Boolean;
begin
  AResourceHandle := 0;
  Result := False;
end;

function TEFMediaLibrary.InternalHasMediaItem(const AMediaItemName: string): Boolean;
begin
  Result := False;
end;

{ TEFResourceMediaLibrary }

function TEFResourceMediaLibrary.InternalGetBitmap(const ABitmapName: string;
  const AOutputBitmap: TBitmap): Boolean;
begin
  Result := False;
  if Assigned(AOutputBitmap) then
  begin
    AOutputBitmap.LoadFromResourceName(ModuleHandle, AnsiUpperCase(ABitmapName));
    Result := True;
  end;
end;

function TEFResourceMediaLibrary.InternalGetIcon(const AIconName: string;
  const AOutputIcon: TIcon): Boolean;
var
  LStream: TResourceStream;
  LIconName: string;
  LBitmap: TBitmap;
begin
  Result := False;
  if Assigned(AOutputIcon) then
  begin
    LIconName := AnsiUpperCase(AIconName);
    if FindResource(ModuleHandle, PChar(LIconName), RT_ICON) <> 0 then
    begin
      LStream := TResourceStream.Create(ModuleHandle, LIconName, RT_ICON);
      try
        AOutputIcon.LoadFromStream(LStream);
        Result := True;
      finally
        FreeAndNil(LStream);
      end;
    end
    else
    begin
      if FindResource(ModuleHandle, PChar(LIconName), RT_BITMAP) <> 0 then
      begin
        LBitmap := TBitmap.Create;
        try
          LBitmap.LoadFromResourceName(ModuleHandle, LIconName);
          BitmapToIcon(LBitmap, AOutputIcon);
          Result := True;
        finally
          LBitmap.Free;
        end;
      end;
    end;
  end;
end;

function TEFResourceMediaLibrary.InternalGetMediaItemData(const AMediaItemName: string;
  const AOutputStream: TStream): Boolean;
var
  LResStream: TStream;

  function GetResType: PChar;
  var
    LFormat: string;
    LUnderscorePos: Integer;
  begin
    LUnderscorePos := RightPos('_', AMediaItemName);
    if LUnderscorePos > 0 then
      LFormat := Copy(AMediaItemName, LUnderscorePos + 1, MaxInt)
    else
      LFormat := '';

    if SameText(LFormat, 'BMP') then
      Result := RT_BITMAP
    else if SameText(LFormat, 'ICO') then
      Result := RT_ICON
    else
      Result := PChar(UpperCase(LFormat)); // covers AVI plus any custom type.
  end;

begin
  LResStream := TResourceStream.Create(ModuleHandle, AMediaItemName, GetResType);
  try
    AOutputStream.CopyFrom(LResStream, 0);
    Result := True;
  finally
    LResStream.Free;
  end;
end;

function TEFResourceMediaLibrary.InternalGetResource(
  const AResourceName: string; const AResourceType: PChar;
  out AResourceHandle: THandle): Boolean;
begin
  if FindResource(ModuleHandle, PChar(AResourceName), AResourceType) <> 0 then
  begin
    AResourceHandle := ModuleHandle;
    Result := True;
  end
  else
  begin
    AResourceHandle := 0;
    Result := False;
  end;
end;

function TEFResourceMediaLibrary.InternalHasMediaItem(
  const AMediaItemName: string): Boolean;
begin
  Result := (FindResource(ModuleHandle, PChar(AMediaItemName), RT_BITMAP) <> 0)
    or (FindResource(ModuleHandle, PChar(AMediaItemName), RT_ICON) <> 0)
    or (FindResource(ModuleHandle, PChar(AMediaItemName), PChar('AVI')) <> 0);
end;

function TEFResourceMediaLibrary.InternalAddBitmapToImageList(const ABitmapName: string;
  const AOutputImageList: TCustomImageList): Boolean;
var
  LTempBitmap: TBitmap;
  LTransparentColour: TColor;
begin
  LTempBitmap := TBitmap.Create;
  try
    LTempBitmap.LoadFromResourceName(ModuleHandle, AnsiUpperCase(ABitmapName));
    LTransparentColour := LTempBitmap.Canvas.Pixels[0, LTempBitmap.Height - 1];
    Result := AOutputImageList.AddMasked(LTempBitmap, LTransparentColour) >= 0;
  finally
    LTempBitmap.Free;
  end;
end;

constructor TEFResourceMediaLibrary.Create(const AName: string);
begin
  inherited Create(AName);
  FModuleHandle := AcquireModuleHandle;
  if FModuleHandle = 0 then
    raise Exception.CreateFmt(SMediaLibraryNotFound, [AName]);
end;

destructor TEFResourceMediaLibrary.Destroy;
begin
  ReleaseModuleHandle(FModuleHandle);
  inherited;
end;

procedure TEFResourceMediaLibrary.ReleaseModuleHandle(const AHandle: THandle);
begin
end;

{ TEFBuiltinMediaLibrary }

function TEFBuiltinMediaLibrary.AcquireModuleHandle: THandle;
begin
  Result := MainInstance;
end;

{ TEFDLLMediaLibrary }

function TEFDLLMediaLibrary.AcquireModuleHandle: THandle;
begin
  Result := LoadLibrary(PChar(Name));
end;

procedure TEFDLLMediaLibrary.ReleaseModuleHandle(const AHandle: THandle);
begin
  inherited;
  if ModuleHandle <> 0 then
    FreeLibrary(ModuleHandle);
end;

{ TEFDirectoryMediaLibrary }

function TEFDirectoryMediaLibrary.GetMediaItemFileName(
  const AMediaItemName: string): string;
var
  LUnderscorePos: Integer;
begin
  Result := AMediaItemName;
  // This is done because not all media libraries support dots in the media
  // item name, so a generic media item name is specified with only underscores
  // and not dots, allowing it to be found in any library. But in such cases we
  // need to convert the last underscore to a dot to match the physical file
  // name. If the media item name already has a dot in it, then it means it is
  // not generic (it is a file name with extension already) so we don't process
  // it.
  if Pos('.', Result) = 0 then
  begin
    LUnderscorePos := RightPos('_', AMediaItemName);
    if LUnderscorePos > 0 then
      Result[LUnderscorePos] := '.';
  end;
  Result := Name + Result;
end;

function TEFDirectoryMediaLibrary.InternalAddBitmapToImageList(
  const ABitmapName: string; const AOutputImageList: TCustomImageList): Boolean;
var
  LTempBitmap: TBitmap;
  LBitmapFileName: string;
  LTransparentColour: TColor;
begin
  LBitmapFileName := GetMediaItemFileName(ABitmapName);
  if FileExists(LBitmapFileName) then
  begin
    LTempBitmap := TBitmap.Create;
    try
      LTempBitmap.LoadFromFile(LBitmapFileName);
      LTransparentColour := LTempBitmap.Canvas.Pixels[0, LTempBitmap.Height - 1];
      Result := AOutputImageList.AddMasked(LTempBitmap, LTransparentColour) >= 0;
    finally
      LTempBitmap.Free;
    end;
  end
  else
    Result := False;
end;

function TEFDirectoryMediaLibrary.InternalGetBitmap(const ABitmapName: string;
  const AOutputBitmap: TBitmap): Boolean;
begin
  AOutputBitmap.LoadFromFile(GetMediaItemFileName(ABitmapName));
  Result := True;
end;

function TEFDirectoryMediaLibrary.InternalGetIcon(const AIconName: string;
  const AOutputIcon: TIcon): Boolean;
var
  LIconFileName: string;
  LBitmap: TBitmap;
begin
  Result := False;
  if Assigned(AOutputIcon) then
  begin
    LIconFileName := GetMediaItemFileName(AIconName);
    if FileExists(LIconFileName) then
    begin
      if AnsiUpperCase(ExtractFileExt(LIconFileName)) = '.BMP' then
      begin
        LBitmap := TBitmap.Create;
        try
          LBitmap.LoadFromFile(LIconFileName);
          BitmapToIcon(LBitmap, AOutputIcon);
          Result := True;
        finally
          LBitmap.Free;
        end;
      end
      else
      begin
        AOutputIcon.LoadFromFile(LIconFileName);
        Result := True;
      end;
    end;
  end;
end;

function TEFDirectoryMediaLibrary.InternalGetMediaItemData(const AMediaItemName: string;
  const AOutputStream: TStream): Boolean;
var
  LFileStream: TStream;
begin
  LFileStream := TFileStream.Create(GetMediaItemFileName(AMediaItemName), fmOpenRead or fmShareDenyWrite);
  try
    AOutputStream.CopyFrom(LFileStream, 0);
    Result := True;
  finally
    LFileStream.Free;
  end;
end;

function TEFDirectoryMediaLibrary.InternalHasMediaItem(
  const AMediaItemName: string): Boolean;
begin
  Result := FileExists(GetMediaItemFileName(AMediaItemName));
end;

initialization

finalization
  FreeAndNil(_EFMediaManager);

end.
