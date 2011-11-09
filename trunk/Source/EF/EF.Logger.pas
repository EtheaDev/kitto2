{-------------------------------------------------------------------------------
   Copyright 2011 Ethea S.r.l.

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
  EF.Streams;

type
  TEFLogger = class
  private
  class var
    FLogStream: TEFTextStream;
    FCriticalSection: TCriticalSection;
    FLogFileName: string;
    FLogLevel: Integer;

    class function GetLogStream: TEFTextStream; static;
    class procedure SetLogFileName(const AValue: string); static;
  protected
    class procedure EnterCS;
    class procedure LeaveCS;
    class property LogStream: TEFTextStream read GetLogStream;
    class procedure Sync(AProc: TProc);
  public
    class constructor Create;
    class destructor Destroy;
  public
    const LOG_LOW = 1;
    const LOG_MEDIUM = 2;
    const LOG_HIGH = 3;
    const LOG_DETAILED = 4;
    const LOG_DEBUG = 5;

    const DEFAULT_LOG_LEVEL = LOG_LOW;
    class property LogFileName: string read FLogFileName write SetLogFileName;
    class property LogLevel: Integer read FLogLevel write FLogLevel;

    class procedure Log(const AString: string; const ALogLevel: Integer = DEFAULT_LOG_LEVEL);
    class procedure LogStrings(const ATitle: string; const AStrings: TStrings; const ALogLevel: Integer = DEFAULT_LOG_LEVEL);
    class procedure LogFmt(const AString: string; const AParams: array of const;
      const ALogLevel: Integer = DEFAULT_LOG_LEVEL);
  end;

implementation

{$IFDEF D15+}
uses
  CodeSiteLogging;
{$ENDIF}

{ TEFLogger }

class constructor TEFLogger.Create;
begin
  FCriticalSection := TCriticalSection.Create;
  FLogFileName := ChangeFileExt(ParamStr(0), '.log');
  FLogLevel := DEFAULT_LOG_LEVEL;
end;

class destructor TEFLogger.Destroy;
begin
  FreeAndNil(FCriticalSection);
end;

class procedure TEFLogger.EnterCS;
begin
  FCriticalSection.Enter;
end;

class function TEFLogger.GetLogStream: TEFTextStream;
var
  LCreateFlag: Integer;
begin
  if not Assigned(FLogStream) then
  begin
    if FileExists(FLogFileName) then
      LCreateFlag := 0
    else
    begin
      LCreateFlag := fmCreate;
      ForceDirectories(ExtractFilePath(FLogFileName));
    end;
    FLogStream := TEFTextStream.Create(TFileStream.Create(FLogFileName, LCreateFlag or fmOpenWrite or fmShareDenyWrite));
    FLogStream.Seek(0, soFromEnd);
  end;
  Result := FLogStream;
end;

class procedure TEFLogger.LeaveCS;
begin
  FCriticalSection.Leave;
end;

class procedure TEFLogger.Log(const AString: string; const ALogLevel: Integer);
begin
  Sync(
    procedure
    begin
      if FLogLevel >= ALogLevel then
      begin
        {$IFDEF D15+}
        CodeSite.Send(AString);
        {$ENDIF}
        if FLogFileName <> '' then
          LogStream.WriteLn(FormatDateTime('[yyyy-mm-dd hh:nn:ss.zzz] ', Now()) +  AString);
      end;
    end);
end;

class procedure TEFLogger.LogFmt(const AString: string; const AParams: array of const;
  const ALogLevel: Integer);
begin
  Log(Format(AString, AParams), ALogLevel);
end;

class procedure TEFLogger.LogStrings(const ATitle: string;
  const AStrings: TStrings; const ALogLevel: Integer);
begin
  Assert(Assigned(AStrings));

  Log(ATitle, ALogLevel);
  Log(AStrings.Text, ALogLevel);
end;

class procedure TEFLogger.SetLogFileName(const AValue: string);
begin
  Sync(
    procedure
    begin
      if AValue <> FLogFileName then
      begin
        FLogFileName := AValue;
        FreeAndNil(FLogStream);
      end;
    end);
end;

class procedure TEFLogger.Sync(AProc: TProc);
begin
  EnterCS;
  try
    AProc;
  finally
    LeaveCS;
  end;
end;

end.
