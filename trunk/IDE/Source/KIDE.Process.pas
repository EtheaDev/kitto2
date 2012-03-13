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
