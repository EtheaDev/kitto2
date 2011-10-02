unit Kitto.LocalStorage;

{$I Kitto.Defines.inc}

interface

uses
  Classes,
  EF.Macros, EF.Classes;

type
  {
    Allows to store simple data items (use its Config sub-object to
    manage them) and complex data items (use LoadItem and SaveItem) in a
    local folder specified in BasePath.

    Data is stored in a Kitto-user-specific subfolder. By default, the base path
    is a Windows-account-specific folder.

    This object stores all user-customizable data in a Kitto application.
    It is accessed through the LocalStorage singleton instance.
  }
  TKLocalStorage = class(TEFComponent)
  private
    FBasePath: string;
    function GetConfigFileName: string;
    function GetItemFileName(const AItemName: string): string;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure SetBasePath(const AValue: string);
  protected
    { TODO : Restore an automatic way to save config on change. }
    //procedure ConfigurationChanged; override;
  public
    {
      Sets the base path under which all data files are stored:

      %APPDATA%\%APP_TITLE%\UserData\<AUserId>\

      The macros are expanded using the provided macro expansion engine.
      If AUserId is empty, the the string '_AllUsers' is used instead.
      This method *must* be called before using the class.
    }
    procedure SetDefaultBasePath(const AUserId: string;
      AMacroExpansionEngine: TEFMacroExpansionEngine);
    {
      Base path under which all data files are stored. By default, it is empty.
      Set a value through SetDefaultBasePath before using the class.

      Under this folder, which is automatically created if it dosn't exist,
      you'll find a file called KittoLocalStorage.conf containing all simple
      data items (data items of simple types, such as numbers and strings),
      plus a file for each complex data item.
    }
    property BasePath: string read FBasePath write SetBasePath;
    {
      Writes the data for a given item to a stream provided by the caller and
      returns True. If no data is available, returns False.
    }
    function LoadItem(const AItemName: string; const ADestinationStream: TStream): Boolean;
    {
      Reads the data for a given item from a stream provided by the caller and
      writes it to the local storage.
    }
    procedure SaveItem(const AItemName: string; const ASourceStream: TStream);
    {
      Returns True is a given item's data is present in the local storage,
      False otherwise.
    }
    function ItemExists(const AItemName: string): Boolean;
    {
      Deletes a given item's data from the local storage and return True.
      If no data was found to delete, returns False.
    }
    function DeleteItem(const AItemName: string): Boolean;
  end;

{
  Singleton TKLocalStorage object.
}
function LocalStorage: TKLocalStorage;

implementation

uses
  SysUtils,
  EF.SysUtils;

var
  _LocalStorage: TKLocalStorage;

function LocalStorage: TKLocalStorage;
begin
  if not Assigned(_LocalStorage) then
    _LocalStorage := TKLocalStorage.Create;
  Result := _LocalStorage;
end;

{ TKLocalStorage }

//procedure TKLocalStorage.ConfigurationChanged;
//begin
//  inherited;
//  SaveConfig;
//end;

function TKLocalStorage.GetConfigFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(BasePath) + 'KittoLocalStorage.yaml';
end;

function TKLocalStorage.GetItemFileName(const AItemName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(BasePath) + AItemName + '.local';
end;

procedure TKLocalStorage.LoadConfig;
var
  LFileName: string;
begin
  LFileName := GetConfigFileName;
{ TODO : Restore config I/O if needed. }
//  if FileExists(LFileName) then
//    Configuration.LoadFromFile(LFileName)
//  else
//    Configuration.Clear;
end;

procedure TKLocalStorage.SaveConfig;
var
  LFileName: string;
begin
  LFileName := GetConfigFileName;
  ForceDirectories(ExtractFilePath(LFileName));
{ TODO : Restore config I/O if needed. }
  //Configuration.SaveToFile(LFileName);
end;

function TKLocalStorage.LoadItem(const AItemName: string;
  const ADestinationStream: TStream): Boolean;
var
  LFileStream: TFileStream;
  LFileName: string;
begin
  Assert(AItemName <> '');
  Assert(Assigned(ADestinationStream));

  Result := False;
  LFileName := GetItemFileName(AItemName);
  if FileExists(LFileName) then
  begin
    LFileStream := TFileStream.Create(LFileName, fmOpenRead or fmShareDenyWrite);
    try
      Result := LFileStream.Size <> 0;
      if Result then
        ADestinationStream.CopyFrom(LFileStream, 0);
    finally
      LFileStream.Free;
    end;
  end;
end;

procedure TKLocalStorage.SaveItem(const AItemName: string;
  const ASourceStream: TStream);
var
  LFileStream: TFileStream;
  LFileName: string;
begin
  Assert(AItemName <> '');
  Assert(Assigned(ASourceStream));

  if ASourceStream.Size <> 0 then
  begin
    LFileName := GetItemFileName(AItemName);
    ForceDirectories(ExtractFilePath(LFileName));
    LFileStream := TFileStream.Create(LFileName, fmCreate or fmShareExclusive);
    try
      LFileStream.CopyFrom(ASourceStream, 0);
    finally
      LFileStream.Free;
    end;
  end;
end;

function TKLocalStorage.ItemExists(const AItemName: string): Boolean;
begin
  Result := FileExists(GetItemFileName(AItemName));
end;

function TKLocalStorage.DeleteItem(const AItemName: string): Boolean;
begin
  EF.SysUtils.DeleteFile(GetItemFileName(AItemName));
  Result := True;
end;

procedure TKLocalStorage.SetBasePath(const AValue: string);
begin
  if FBasePath <> AValue then
  begin
    FBasePath := AValue;
    LoadConfig;
  end;
end;

procedure TKLocalStorage.SetDefaultBasePath(const AUserId: string;
  AMacroExpansionEngine: TEFMacroExpansionEngine);
var
  LUserId: string;
begin
  LUserId := AUserId;
  if LUserId = '' then
    LUserId := '_AllUsers';
  BasePath := AMacroExpansionEngine.Expand('%APPDATA%\%APP_TITLE%\UserData\' + LUserId);
end;

initialization

finalization
  FreeAndNil(_LocalStorage);

end.
