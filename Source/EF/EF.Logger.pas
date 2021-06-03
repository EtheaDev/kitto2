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

///	<summary>
///	 Basic logging services.
///	</summary>
unit EF.Logger;

{$I EF.Defines.inc}

interface

uses
  SysUtils
  , Types
  , Classes
  , EF.ObserverIntf
  , EF.Classes
  , EF.Tree
  , EF.Macros
  ;

type
  TEFLogger = class(TEFComponent)
  private
    FLogLevel: Integer;
    FMacroExpansionEngine: TEFMacroExpansionEngine;
    class var
      FInstance: TEFLogger;
    procedure SetLogLevelFromConfig(const ALogLevelNode: TEFNode);
  public
    class constructor Create;
    class destructor Destroy;
    procedure AfterConstruction; override;
    property MacroExpansionEngine: TEFMacroExpansionEngine read FMacroExpansionEngine;
  public
    const LOG_LOW = 1;
    const LOG_MEDIUM = 2;
    const LOG_HIGH = 3;
    const LOG_DETAILED = 4;
    const LOG_DEBUG = 5;

    const DEFAULT_LOG_LEVEL = LOG_LOW;

    procedure Configure(const AConfig: TEFTree; const AMacroExpansionEngine: TEFMacroExpansionEngine);

    property LogLevel: Integer read FLogLevel write FLogLevel;

    procedure Log(const AString: string; const ALogLevel: Integer = DEFAULT_LOG_LEVEL);

    procedure LogLow(const AString: string); inline;
    procedure LogMedium(const AString: string); inline;
    procedure LogHigh(const AString: string); inline;
    procedure LogDetailed(const AString: string); inline;
    procedure LogDebug(const AString: string); inline;

    procedure LogFmt(const AString: string; const AParams: array of const;
      const ALogLevel: Integer = DEFAULT_LOG_LEVEL);

    class property Instance: TEFLogger read FInstance;
  end;

  TEFLogEndpoint = class(TEFSubjectAndObserver)
  strict private
    FIsEnabled: Boolean;
  strict protected
    function GetConfigPath: string; virtual;
    procedure Configure(const AConfig: TEFComponentConfig; const AMacroExpansionEngine: TEFMacroExpansionEngine); virtual;
    procedure DoLog(const AString: string); virtual; abstract;
    property IsEnabled: Boolean read FIsEnabled;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); override;
  end;

implementation

{ TEFLogger }

procedure TEFLogger.AfterConstruction;
begin
  inherited;
  FLogLevel := DEFAULT_LOG_LEVEL;
end;

class constructor TEFLogger.Create;
begin
  FInstance := TEFLogger.Create;
end;

class destructor TEFLogger.Destroy;
begin
  FreeAndNil(FInstance);
end;

procedure TEFLogger.Log(const AString: string; const ALogLevel: Integer);
begin
  if FLogLevel >= ALogLevel then
    NotifyObservers(AString);
end;

procedure TEFLogger.LogDebug(const AString: string);
begin
  Log(AString, LOG_DEBUG);
end;

procedure TEFLogger.LogDetailed(const AString: string);
begin
  Log(AString, LOG_DETAILED);
end;

procedure TEFLogger.LogFmt(const AString: string; const AParams: array of const;
  const ALogLevel: Integer);
begin
  Log(Format(AString, AParams), ALogLevel);
end;

procedure TEFLogger.LogHigh(const AString: string);
begin
  Log(AString, LOG_HIGH);
end;

procedure TEFLogger.LogLow(const AString: string);
begin
  Log(AString, LOG_LOW);
end;

procedure TEFLogger.LogMedium(const AString: string);
begin
  Log(AString, LOG_MEDIUM);
end;

procedure TEFLogger.SetLogLevelFromConfig(const ALogLevelNode: TEFNode);
var
  LValue: string;
begin
  if Assigned(ALogLevelNode) then
  begin
    LValue := ALogLevelNode.AsString.ToLower;
    if LValue <> '' then
    begin
      if LValue = 'low' then
        LogLevel := LOG_LOW
      else if LValue = 'medium' then
        LogLevel := LOG_MEDIUM
      else if LValue = 'high' then
        LogLevel := LOG_HIGH
      else if LValue = 'detailed' then
        LogLevel := LOG_DETAILED
      else if LValue = 'debug' then
        LogLevel := LOG_DEBUG
      else
        LogLevel := ALogLevelNode.AsInteger;
    end;
  end;
end;

procedure TEFLogger.Configure(const AConfig: TEFTree; const AMacroExpansionEngine: TEFMacroExpansionEngine);
begin
  if Assigned(AConfig) then
    SetLogLevelFromConfig(AConfig.FindNode('Level'));
  try
    Config.Assign(AConfig);
    FMacroExpansionEngine := AMacroExpansionEngine;
    NotifyObservers('{ConfigChanged}');
  finally
    Config.Clear;
    FMacroExpansionEngine := nil;
  end;
end;

{ TEFLogEndpoint }

procedure TEFLogEndpoint.AfterConstruction;
begin
  inherited;
  TEFLogger.Instance.AttachObserver(Self);
end;

procedure TEFLogEndpoint.Configure(const AConfig: TEFComponentConfig; const AMacroExpansionEngine: TEFMacroExpansionEngine);
begin
  FIsEnabled := False;
  if Assigned(AConfig) then
    FIsEnabled := AConfig.GetBoolean(GetConfigPath + 'IsEnabled', FIsEnabled);
end;

function TEFLogEndpoint.GetConfigPath: string;
begin
  Result := '';
end;

destructor TEFLogEndpoint.Destroy;
begin
  TEFLogger.Instance.DetachObserver(Self);
  inherited;
end;

procedure TEFLogEndpoint.UpdateObserver(const ASubject: IEFSubject; const AContext: string);
begin
  inherited;
  if SameText(AContext, '{ConfigChanged}') then
    Configure(TEFLogger(ASubject.AsObject).Config, TEFLogger(ASubject.AsObject).MacroExpansionEngine)
  else
  begin
    //prevent logging of password
    if pos('PASSWORD', UpperCase(AContext)) = 0 then
      DoLog(AContext);
  end;
end;

end.
