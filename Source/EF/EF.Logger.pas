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
///	  Basic logging services.
///	</summary>
unit EF.Logger;

{$I EF.Defines.inc}

interface

uses
  SysUtils, Classes, SyncObjs,
  EF.ObserverIntf, EF.Tree, EF.Macros;

type
  TEFLogger = class(TEFSubjectAndObserver)
  private
    FCriticalSection: TCriticalSection;
    FLogLevel: Integer;
    FConfig: TEFNode;
    FMacroExpansionEngine: TEFMacroExpansionEngine;
  class var
    FInstance: TEFLogger;
  protected
    procedure EnterCS;
    procedure LeaveCS;
    procedure Sync(AProc: TProc);
  public
    class constructor Create;
    class destructor Destroy;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    const LOG_LOW = 1;
    const LOG_MEDIUM = 2;
    const LOG_HIGH = 3;
    const LOG_DETAILED = 4;
    const LOG_DEBUG = 5;

    const DEFAULT_LOG_LEVEL = LOG_LOW;

    procedure Configure(const AConfig: TEFNode; const AMacroExpansionEngine: TEFMacroExpansionEngine);

    property LogLevel: Integer read FLogLevel write FLogLevel;

    procedure Log(const AString: string; const ALogLevel: Integer = DEFAULT_LOG_LEVEL);
    procedure LogStrings(const ATitle: string; const AStrings: TStrings; const ALogLevel: Integer = DEFAULT_LOG_LEVEL);
    procedure LogFmt(const AString: string; const AParams: array of const;
      const ALogLevel: Integer = DEFAULT_LOG_LEVEL);

    class property Instance: TEFLogger read FInstance;
  end;

  TEFLogEndpoint = class(TEFSubjectAndObserver)
  private
    FIsEnabled: Boolean;
  protected
    function GetConfigPath: string; virtual;
    procedure Configure(const AConfig: TEFNode; const AMacroExpansionEngine: TEFMacroExpansionEngine); virtual;
    procedure DoLog(const AString: string); virtual; abstract;
    property IsEnabled: Boolean read FIsEnabled;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure UpdateObserver(const ASubject: IEFSubject;
      const AContext: string = ''); override;
  end;

implementation

{ TEFLogger }

procedure TEFLogger.AfterConstruction;
begin
  inherited;
  FCriticalSection := TCriticalSection.Create;
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

destructor TEFLogger.Destroy;
begin
  FreeAndNil(FCriticalSection);
  inherited;
end;

procedure TEFLogger.EnterCS;
begin
  FCriticalSection.Enter;
end;

procedure TEFLogger.LeaveCS;
begin
  FCriticalSection.Leave;
end;

procedure TEFLogger.Log(const AString: string; const ALogLevel: Integer);
begin
  Sync(
    procedure
    begin
      if FLogLevel >= ALogLevel then
        NotifyObservers(AString);
    end);
end;

procedure TEFLogger.LogFmt(const AString: string; const AParams: array of const;
  const ALogLevel: Integer);
begin
  Log(Format(AString, AParams), ALogLevel);
end;

procedure TEFLogger.LogStrings(const ATitle: string;
  const AStrings: TStrings; const ALogLevel: Integer);
begin
  Assert(Assigned(AStrings));

  Log(ATitle, ALogLevel);
  Log(AStrings.Text, ALogLevel);
end;

procedure TEFLogger.Configure(const AConfig: TEFNode;
  const AMacroExpansionEngine: TEFMacroExpansionEngine);
begin
  if Assigned(AConfig) then
    LogLevel := AConfig.GetInteger('Level', LogLevel);
  try
    FConfig := AConfig;
    FMacroExpansionEngine := AMacroExpansionEngine;
    NotifyObservers('{ConfigChanged}');
  finally
    FConfig := nil;
    FMacroExpansionEngine := nil;
  end;
end;

procedure TEFLogger.Sync(AProc: TProc);
begin
  EnterCS;
  try
    AProc;
  finally
    LeaveCS;
  end;
end;

{ TEFLogEndpoint }

procedure TEFLogEndpoint.AfterConstruction;
begin
  inherited;
  TEFLogger.Instance.AttachObserver(Self);
end;

procedure TEFLogEndpoint.Configure(const AConfig: TEFNode;
  const AMacroExpansionEngine: TEFMacroExpansionEngine);
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

procedure TEFLogEndpoint.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
  inherited;
  if SameText(AContext, '{ConfigChanged}') then
    Configure(TEFLogger(ASubject.AsObject).FConfig, TEFLogger(ASubject.AsObject).FMacroExpansionEngine)
  else
    DoLog(AContext);
end;

end.
