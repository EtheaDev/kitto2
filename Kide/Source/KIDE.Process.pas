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
unit KIDE.Process;

interface

type
  TProcess = class
  strict private
    FErrorCount: Integer;
    FLogIndent: Integer;
  strict
  private
    function GetLogIndentString: string; protected
    procedure LogIndent;
    procedure LogOutdent;
    procedure Log(const AString: string);
    procedure LogError(const AString: string);
    procedure LogWarning(const AString: string);
    procedure LogInfo(const AString: string);
    property ErrorCount: Integer read FErrorCount;
    procedure InternalExecute; virtual; abstract;
  public
    procedure Execute;
  end;

implementation

uses
  SysUtils, StrUtils,
  EF.Logger,
  KIDE.EFHelpers;

{ TProcess }

procedure TProcess.Execute;
begin
  FErrorCount := 0;
  FLogIndent := 0;
  try
    InternalExecute;
  except
    on E: Exception do
    begin
      LogError('Blocking error.');
      LogError(E.Message);
    end;
  end;
end;

function TProcess.GetLogIndentString: string;
begin
  Result := DupeString(' ', FLogIndent);
end;

procedure TProcess.Log(const AString: string);
begin
  TEFLogger.Instance.Log(GetLogIndentString + AString);
end;

procedure TProcess.LogError(const AString: string);
begin
  Inc(FErrorCount);
  TEFLogger.Instance.LogError(GetLogIndentString + AString);
end;

procedure TProcess.LogIndent;
begin
  Inc(FLogIndent, 2);
end;

procedure TProcess.LogInfo(const AString: string);
begin
  TEFLogger.Instance.LogInfo(GetLogIndentString + AString);
end;

procedure TProcess.LogOutdent;
begin
  Dec(FLogIndent, 2);
end;

procedure TProcess.LogWarning(const AString: string);
begin
  TEFLogger.Instance.LogWarning(GetLogIndentString + AString);
end;

end.
