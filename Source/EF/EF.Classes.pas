unit EF.Classes;

{$I EF.Defines.inc}

interface

uses
  Types, SysUtils, Classes, DB, Generics.Collections,
  EF.Intf, EF.Types, EF.Macros, EF.Tree, EF.ObserverIntf;

type
  TEFComponent = class(TEFSubjectAndObserver)
  private
    FOnLog: TEFLogEvent;
    FLogLevel: Integer;
    FObservers: TInterfaceList;
    FConfig: TEFNode;
    function GetConfig: TEFNode;
  public
    const DEFAULT_LOG_LEVEL = 1;
  protected
    {
      Override this method to enable Config auto-load upon first request.
      The default implementation returns '', which disables auto-load.
    }
    function GetConfigFileName: string; virtual;
    {
      Implements GetClassId.
    }
    class function InternalGetClassId: string; virtual;
    {
      Implements GetId.
    }
    function InternalGetId: string; virtual;
    {
      Fires OnLog.
    }
    procedure DoLog(const AString: string; const ALogLevel: Integer = DEFAULT_LOG_LEVEL); overload;
    {
      Calls DoLog for each line in AStrings, pre-pending ALinePrefix to
      each line.
    }
    procedure DoLog(const AStrings: TStrings; const ALinePrefix: string = '';
      const ALogLevel: Integer = DEFAULT_LOG_LEVEL); overload;
    {
      Checks that a proprerty has an assigned/valid value, and raises a
      generic exception otherwise. ACondition should evaluate to True if the
      property is assigned/valid, and to False otherwise. In this case, an
      exception with APropertyName in the message text is raised.
    }
    procedure CheckProperty(const ACondition: Boolean; const APropertyName: string);
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    {
      Returns an identification string based on the class name. It can be used
      by a registry to hold string identifiers for different classes.
      By default it returns the class name without any 'T' or 'TEF' prefix.
    }
    class function GetClassId: string;
    {
      Returns an Id for the object. By default it returns the same as GetClassId.
    }
    function GetId: string;
    {
      From IEFInterface.
    }
    function AsObject: TObject;
    {
      Fired each time the object needs to log information about its work.
    }
    property OnLog: TEFLogEvent read FOnLog write FOnLog;
    {
      Sets the logging level for this component. Only messages that
      have a level lower than or equal to this setting will be logged.
    }
    property LogLevel: Integer read FLogLevel write FLogLevel default DEFAULT_LOG_LEVEL;

    property Config: TEFNode read GetConfig;
  end;

  TEFComponentClass = class of TEFComponent;

implementation

uses
  StrUtils, TypInfo,
  EF.SysUtils, EF.StrUtils, EF.Localization, EF.YAML;

{ TEFComponent }

procedure TEFComponent.AfterConstruction;
begin
  inherited;
  FLogLevel := DEFAULT_LOG_LEVEL;
  FObservers := TInterfaceList.Create;
end;

function TEFComponent.AsObject: TObject;
begin
  Result := Self;
end;

procedure TEFComponent.CheckProperty(const ACondition: Boolean;
  const APropertyName: string);
begin
  if not ACondition then
    raise EEFError.CreateFmt(_('Unspecified property "%s".'), [APropertyName]);
end;

destructor TEFComponent.Destroy;
begin
  FreeAndNil(FConfig);
  FreeAndNil(FObservers);
  inherited;
end;

procedure TEFComponent.DoLog(const AString: string; const ALogLevel: Integer = DEFAULT_LOG_LEVEL);
begin
  if (FLogLevel >= ALogLevel) and Assigned(FOnLog) then
    FOnLog(Self, '[' + GetId + '] ' + AString);
end;

procedure TEFComponent.DoLog(const AStrings: TStrings; const ALinePrefix: string = '';
  const ALogLevel: Integer = DEFAULT_LOG_LEVEL);
var
  LLogLineIndex: Integer;
begin
  Assert(Assigned(AStrings));

  for LLogLineIndex := 0 to AStrings.Count - 1 do
    DoLog(ALinePrefix + AStrings[LLogLineIndex], ALogLevel);
end;

class function TEFComponent.GetClassId: string;
begin
  Result := InternalGetClassId;
end;

function TEFComponent.GetConfig: TEFNode;
var
  LConfigFileName: string;
begin
  if not Assigned(FConfig) then
  begin
    LConfigFileName := GetConfigFileName;
    if LConfigFileName <> '' then
      FConfig := TEFTreeFactory.LoadFromFile<TEFNode>(LConfigFileName)
    else
      FConfig := TEFNode.Create;
  end;
  Result := FConfig;
end;

function TEFComponent.GetConfigFileName: string;
begin
  Result := '';
end;

function TEFComponent.GetId: string;
begin
  Result := InternalGetId;
end;

class function TEFComponent.InternalGetClassId: string;
begin
  // Strips standard prefix from the class name.
  // If the class name doesn't start with TEF, then let's at least
  // remove the T.
  Result := StripPrefixAndSuffix(ClassName, 'T', '');
  Result := StripPrefixAndSuffix(Result, 'EF', '');
end;

function TEFComponent.InternalGetId: string;
begin
  Result := GetClassId;
end;

end.
