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
///	  This unit defines classes and routines to help with macro-expansion in
///	  strings. The macro-expansion engine can be extended by defining and
///	  registering custom expander classes.
///	</summary>
unit EF.Macros;

{$I EF.Defines.inc}

interface

uses
  Generics.Collections,
  Classes, Contnrs;
  
type
  ///	<summary>
  ///	  Abstract macro expander class. Descendants are able to expand a certain
  ///	  set of macros in a given string. This class is used with
  ///	  TEFMacroExpansionEngine but it can be used on its own as well.
  ///	</summary>
  TEFMacroExpander = class
  protected
    ///	<summary>
    ///	  Utility method: replaces all occurrences of AMacroName in AString with
    ///	  AMacroValue, and returns the resulting string. Should be used by all
    ///	  inherited classes to expand macros. Implements support for control
    ///	  sequences (such as %Q, that encapsulates QuotedStr()).
    ///	</summary>
    function ExpandMacros(const AString, AMacroName, AMacroValue: string): string;

    ///	<summary>
    ///   Implements Expand. The default implementation just returns the input
    ///   argument unchanged.
    ///	</summary>
    function InternalExpand(const AString: string): string; virtual;
  public
    constructor Create; virtual;

    ///	<summary>
    ///	  Expands all supported macros found in a given string, and returns the
    ///	  string with macros expanded. Calls the virtual protected method
    ///	  InternalExpand to do the job.
    ///	</summary>
    function Expand(const AString: string): string;
  end;
  TEFMacroExpanderClass = class of TEFMacroExpander;

  TEFMacroExpansionEngine = class;

  TEFGetMacroExpansionEngine = reference to function: TEFMacroExpansionEngine;

  ///	<summary>
  ///	  The engine is able to expand an open-ended set of macros in a given
  ///	  string. Use AddExpander calls to add support for more macros, and call
  ///	  Expand to trigger macro expansion. Alternatively, you can use a
  ///	  ready-made singleton engine which supports all registered expanders
  ///	  out-of-the-box. Macro expansion engines can be chained: each engine
  ///	  calls the previous engine in the chain, if available, when invoked.
  ///	</summary>
  TEFMacroExpansionEngine = class
  private
    FExpanders: TObjectList<TEFMacroExpander>;
    FPrevious: TEFMacroExpansionEngine;
    class var FInstance: TEFMacroExpansionEngine;
    class var FOnGetInstance: TEFGetMacroExpansionEngine;
    function CallExpanders(const AString: string): string;
    class function GetInstance: TEFMacroExpansionEngine; static;
  protected
    class destructor Destroy;
  public
    constructor Create(const APrevious: TEFMacroExpansionEngine = nil);
    destructor Destroy; override;
  public
    class property Instance: TEFMacroExpansionEngine read GetInstance;

    class property OnGetInstance: TEFGetMacroExpansionEngine
      read FOnGetInstance write FOnGetInstance;

    ///	<summary>
    ///	  Expands all recognized macros in AString and returns the resulting
    ///	  string. The result depends on the set of macro expanders used. Add a
    ///	  new macro expander by calling AddExpander before calling Expand. If
    ///	  no macro expanders are used, the result is the same as the input
    ///	  string.
    ///	</summary>
    function Expand(const AString: string): string;

    ///	<summary>
    ///	  Adds an expander to the list used by the Expand method. Acquires
    ///	  ownership of AExpander, which means it will be destroyed when
    ///	  RemoveExpanders or ClearExpanders are called, or when the current
    ///	  object is itself destroyed.
    ///	</summary>
    procedure AddExpander(const AExpander: TEFMacroExpander);

    ///	<summary>
    ///	  Removes a previously added expander from the list used by the Expand
    ///	  method. Returns True if the specified object was found and removed,
    ///	  and False otherwise.
    ///	</summary>
    ///	<remarks>
    ///	  The removed object is NOT freed automatically.
    ///	</remarks>
    function RemoveExpander(const AExpander: TEFMacroExpander): Boolean;

    ///	<summary>
    ///	  Returns the index of a given expander in the list, or -1 if it is not
    ///	  found. Use this method to check whether an expander is part of an
    ///	  expansion engine.
    ///	</summary>
    function IndexOfExpander(const AExpander: TEFMacroExpander): Integer;

    ///	<summary>
    ///	  Removes from the list and frees all expanders of the given class.
    ///	</summary>
    procedure RemoveExpanders(const AExpanderClass: TEFMacroExpanderClass);

    ///	<summary>
    ///	  Removes all expanders from the list and destroys them.
    ///	</summary>
    procedure ClearExpanders;
  end;

  {
    A macro expander that can expand some path-related macros.
    Supported macros (case sensitive):
    @table(
    @row(
    @cell(%APP_PATH%)@cell(ExtractFilePath(ParamStr(0)))
    )@row(
    @cell(%APP_NAME%)@cell(ParamStr(0))
    )@row(
    @cell(%APP_BASENAME%)@cell(ExtractFileName(ParamStr(0)))
    )@row(
    @cell(%APP_BASENAME%)@cell(RemoveFileExt(ExtractFileName(ParamStr(0))))
    )@row(
    @cell(%WIN_DIR%)@cell(GetWindowsDirectory (including final \))
    )@row(
    @cell(%SYS_DIR%)@cell(GetSystemDirectory (including final \))
    ))
  }
  TEFPathMacroExpander = class(TEFMacroExpander)
  protected
    function InternalExpand(const AString: string): string; override;
  end;

  {
    A macro expander that can expand some system macros.
    Supported macros (case sensitive):
    @table(
      @row(
        @cell(%DATE%)@cell(DateToStr(Date)))
      @row(
        @cell(%YESTERDAY%)@cell(DateToStr(Date - 1)))
      @row(
        @cell(%TOMORROW%)@cell(DateToStr(Date + 1)))
      @row(
        @cell(%TIME%)@cell(TimeToStr(Now)))
      @row(
        @cell(%DATETIME%)@cell(DateTimeToStr(Now)))
      @row(
        @cell(%PROCESS_ID%)@cell(An integer value that is the current process' Id))
      @row(
        @cell(%THREAD_ID%)@cell(An integer value that is the current thread's Id))
    )
  }
  TEFSysMacroExpander = class(TEFMacroExpander)
  protected
    function InternalExpand(const AString: string): string; override;
  end;

  {
    A macro expander that expands all environment variables.
    See EF.SysUtils.ExpandEnvironmentVariables for details on
    format and case sensitivity.
  }
  TEFEnvironmentVariableMacroExpander = class(TEFMacroExpander)
  protected
    function InternalExpand(const AString: string): string; override;
  end;

  {
    Expands parameters passed on the command line.
    Command line parameters must be passed in the format@br
      @bold(/<parameter name> <parameter value>)@br
    or@br
      @bold(-<parameter name> <parameter value>)@br
    The macro format is@br
      @bold(%cmd:<parameter name>%)
    @seealso(EFSysUtils.GetCmdLineParamValue).
  }
  TEFCmdLineParamMacroExpander = class(TEFMacroExpander)
  private
    FParams: TStringList;
    procedure ReadCmdLineParams;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  protected
    function InternalExpand(const AString: string): string; override;
  end;

  {
    Expands the current date and/or time formatted in several ways.
    Supported macros (case sensitive):
    @table(
      @row(
        @cell(%YYYYMMDD_DATE%)@cell(The current date in YYYYMMDD format))
      @row(
        @cell(%YYYYMMDD_YESTERDAY%)@cell(The current date minus 1 day in YYYYMMDD format))
      @row(
        @cell(%YYYYMMDD_TOMORROW%)@cell(The current date plus 1 day in YYYYMMDD format))
      @row(
        @cell(%YYMMDD_DATE%)@cell(The current date in YYMMDD format))
      @row(
        @cell(%YYMMDD_YESTERDAY%)@cell(The current date minus 1 day in YYMMDD format))
      @row(
        @cell(%YYMMDD_TOMORROW%)@cell(The current date plus 1 day in YYMMDD format))
      @row(
        @cell(%MMDD_DATE%)@cell(The current date in MMDD format))
      @row(
        @cell(%DD_DATE%)@cell(The current day of the month))
      @row(
        @cell(%MM_DATE%)@cell(The current month of the year))
      @row(
        @cell(%YYYY_DATE%)@cell(The current year))
      @row(
        @cell(%WEEKDAYNAME_SHORT%)@cell(The short name of the current day of the week))
      @row(
        @cell(%WEEKDAYNAME_LONG%)@cell(The name of the current day of the week))
      @row(
        @cell(%WEEKDAYNR%)@cell(The number of the current day of the week (1 = sunday)))
      @row(
        @cell(%ISOWEEKDAYNR%)@cell(The number of the current day of the week (1 = monday)))
      @row(
        @cell(%HHMMSS_TIME%)@cell(The current time in HHMMSS format))
      @row(
        @cell(%HHMM_TIME%)@cell(The current time in HHMM format))
      @row(
        @cell(%HH_TIME%)@cell(The current hour))
      @row(
        @cell(%MM_TIME%)@cell(The current minute))
      @row(
        @cell(%SS_TIME%)@cell(The current second))
    )
  }
  TEFDateTimeStrMacroExpander = class(TEFMacroExpander)
  protected
    function InternalExpand(const AString: string): string; override;
  end;

  {
    This macro expander includes the content of a specified (text) file
    into the string while it's being expanded.
    The macro format is as follows:
    @table(
    @row(
    @cell(%FILE(<filename>[,<param1>[...]]))@cell(
      The contents of <filename>. If <filename> is not a fully qualified
      filename, then the value of the DefaultPath property of the expander
      is pre-prended to it before trying to load it. If parameters are passed
      after the file name, then they are used to substitute parameter
      placeholders in the file contents. Placeholders are in the form
      %%P1%%, %%P2%% and so on.

      If a parameter (or the file name) includes a space or a comma, the entire
      parameter should be enclosed in ". Space and comma are both valid
      parameter separators. If a parameter includes the " character, then the
      character should be doubled (as well as the entire parameter be enclosed
      between " characters).

      If the file name is not specified or the file doesn't exist, then the
      macro expands to ''. If a parameter doesn't exist, it expands to ''.)
    ))
  }
  TEFFileMacroExpander = class(TEFMacroExpander)
  private
    FDefaultPath: string;
    function ExpandParams(const AString: string;
      const AParams: TStrings): string;
  protected
    function InternalExpand(const AString: string): string; override;
  public
    {
      The value of this property is pre-pended to any relative file name
      before loading the file contents as part of the expansion process.
      Leave this property empty to have the expander use the current directory
      as default path instead.
    }
    property DefaultPath: string read FDefaultPath write FDefaultPath;
  end;

  {
    A macro expander that generates GUIDs.
    Supported macros (case sensitive):
    @table(
      @row(
        @cell(%GUID%)@cell(CreateGuidStr()))
      @row(
        @cell(%COMPACT_GUID%)@cell(CreateCompactGuidStr()))
    )
  }
  TEFGUIDMacroExpander = class(TEFMacroExpander)
  protected
    function InternalExpand(const AString: string): string; override;
  end;

///	<summary>Creates and adds instances of all standard macro expanders to the
///	specified macro expansion engine, which acquires ownership of
///	them.</summary>
procedure AddStandardMacroExpanders(const AMacroExpansionEngine: TEFMacroExpansionEngine);

implementation

uses
  Windows, SysUtils, DateUtils, StrUtils,
  EF.StrUtils, EF.SysUtils;

procedure AddStandardMacroExpanders(const AMacroExpansionEngine: TEFMacroExpansionEngine);
begin
  Assert(Assigned(AMacroExpansionEngine));

  AMacroExpansionEngine.AddExpander(TEFPathMacroExpander.Create);
  AMacroExpansionEngine.AddExpander(TEFSysMacroExpander.Create);
  AMacroExpansionEngine.AddExpander(TEFEnvironmentVariableMacroExpander.Create);
  AMacroExpansionEngine.AddExpander(TEFCmdLineParamMacroExpander.Create);
  AMacroExpansionEngine.AddExpander(TEFDateTimeStrMacroExpander.Create);
  AMacroExpansionEngine.AddExpander(TEFGUIDMacroExpander.Create);
end;

{ TEFMacroExpander }

function TEFMacroExpander.ExpandMacros(const AString, AMacroName,
  AMacroValue: string): string;
begin
  // First try control sequences, then standard, as the latter format is part
  // of the former (almost all standard macros begin with '%'.
  Result := StringReplace(AString, '%Q' + AMacroName, QuotedStr(AMacroValue),
    [rfReplaceAll, rfIgnoreCase]);
  // Standard.
  Result := StringReplace(Result, AMacroName, AMacroValue,
    [rfReplaceAll, rfIgnoreCase]);
end;

function TEFMacroExpander.InternalExpand(const AString: string): string;
begin
  Result := AString;
end;

constructor TEFMacroExpander.Create;
begin
end;

function TEFMacroExpander.Expand(const AString: string): string;
begin
  Result := InternalExpand(AString);
end;

{ TEFMacroExpansionManager }

constructor TEFMacroExpansionEngine.Create(const APrevious: TEFMacroExpansionEngine = nil);
begin
  inherited Create;
  FPrevious := APrevious;
  FExpanders := TObjectList<TEFMacroExpander>.Create(True);
end;

class destructor TEFMacroExpansionEngine.Destroy;
begin
  FreeAndNil(FInstance);
end;

destructor TEFMacroExpansionEngine.Destroy;
begin
  ClearExpanders;
  FreeAndNil(FExpanders);
  inherited;
end;

procedure TEFMacroExpansionEngine.AddExpander(const AExpander: TEFMacroExpander);
begin
  Assert(Assigned(AExpander));
  
  FExpanders.Add(AExpander);
end;

function TEFMacroExpansionEngine.RemoveExpander(const AExpander: TEFMacroExpander): Boolean;
begin
  if Assigned(AExpander) then
    Result := Assigned(FExpanders.Extract(AExpander))
  else
    Result := False;
end;

function TEFMacroExpansionEngine.IndexOfExpander(const AExpander: TEFMacroExpander): Integer;
begin
  Assert(Assigned(AExpander));

  Result := FExpanders.IndexOf(AExpander);
end;

procedure TEFMacroExpansionEngine.RemoveExpanders(const AExpanderClass: TEFMacroExpanderClass);
var
  I: Integer;
begin
  Assert(Assigned(AExpanderClass));

  for I := FExpanders.Count - 1 downto 0 do
    if FExpanders[I] is AExpanderClass then
      FExpanders.Delete(I);
end;

procedure TEFMacroExpansionEngine.ClearExpanders;
begin
  FExpanders.Clear;
end;

function TEFMacroExpansionEngine.Expand(const AString: string): string;
var
  LText: string;
begin
  // Keep iterating until all macros in all included files are expanded.
  // This also allows to support macros in macros, at a performance cost.
  LText := AString;
  Result := CallExpanders(LText);
  while LText <> Result do
  begin
    LText := Result;
    Result := CallExpanders(LText);
  end;
end;

class function TEFMacroExpansionEngine.GetInstance: TEFMacroExpansionEngine;
begin
  Result := nil;
  if Assigned(FOnGetInstance) then
    Result := FOnGetInstance();
  if not Assigned(Result) then
  begin
    if not Assigned(FInstance) then
    begin
      FInstance := TEFMacroExpansionEngine.Create;
      AddStandardMacroExpanders(FInstance);
    end;
    Result := FInstance;
  end;
end;

function TEFMacroExpansionEngine.CallExpanders(const AString: string): string;
var
  I: Integer;
begin
  if Assigned(FPrevious) then
    Result := FPrevious.Expand(AString)
  else
    Result := AString;
  for I := 0 to FExpanders.Count - 1 do
    Result := TEFMacroExpander(FExpanders[I]).Expand(Result);
end;

{ TEFPathMacroExpander }

function TEFPathMacroExpander.InternalExpand(const AString: string): string;
begin
  Result := inherited InternalExpand(AString);
  Result := ExpandMacros(Result, '%APP_PATH%', ExtractFilePath(ParamStr(0)));
  Result := ExpandMacros(Result, '%APP_NAME%', ParamStr(0));
  Result := ExpandMacros(Result, '%APP_FILENAME%', ExtractFileName(ParamStr(0)));
  Result := ExpandMacros(Result, '%APP_BASENAME%', ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
  Result := ExpandMacros(Result, '%WIN_DIR%', IncludeTrailingPathDelimiter(SafeGetWindowsDirectory));
  Result := ExpandMacros(Result, '%SYS_DIR%', IncludeTrailingPathDelimiter(SafeGetSystemDirectory));
end;

{ TEFSysMacroExpander }

function TEFSysMacroExpander.InternalExpand(const AString: string): string;
begin
  Result := inherited InternalExpand(AString);
  Result := ExpandMacros(Result, '%DATE%', DateToStr(Date));
  Result := ExpandMacros(Result, '%YESTERDAY%', DateToStr(Date - 1));
  Result := ExpandMacros(Result, '%TOMORROW%', DateToStr(Date + 1));
  Result := ExpandMacros(Result, '%TIME%', TimeToStr(Now));
  Result := ExpandMacros(Result, '%DATETIME%', DateTimeToStr(Now));
  Result := ExpandMacros(Result, '%PROCESS_ID%', IntToStr(GetCurrentProcessId));
  Result := ExpandMacros(Result, '%THREAD_ID%', IntToStr(GetCurrentThreadId));
end;

{ TEFEnvironmentVariableMacroExpander }

function TEFEnvironmentVariableMacroExpander.InternalExpand(const AString: string): string;
begin
  Result := inherited InternalExpand(AString);
  Result := ExpandEnvironmentVariables(Result);
end;

{ TEFCmdLineParamMacroExpander }

procedure TEFCmdLineParamMacroExpander.AfterConstruction;
begin
  inherited;
  FParams := TStringList.Create;
  ReadCmdLineParams;
end;

destructor TEFCmdLineParamMacroExpander.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

procedure TEFCmdLineParamMacroExpander.ReadCmdLineParams;
var
  LParamIndex: Integer;
begin
  FParams.Clear;
  // Skip the last parameter, as it could never have one after itself.
  for LParamIndex := 1 to ParamCount - 1 do
  begin
    if (ParamStr(LParamIndex) <> '')
        and ((ParamStr(LParamIndex)[1] = '/')
        or (ParamStr(LParamIndex)[1] = '-')) then
      FParams.Add(Copy(ParamStr(LParamIndex), 2, MaxInt) + '=' + ParamStr(LParamIndex + 1));
  end;
end;

function TEFCmdLineParamMacroExpander.InternalExpand(
  const AString: string): string;
var
  LParamIndex: Integer;
begin
  Result := inherited InternalExpand(AString);
  for LParamIndex := 0 to FParams.Count - 1 do
    Result := ExpandMacros(Result, '%Cmd:' + FParams.Names[LParamIndex] + '%',
      FParams.ValueFromIndex[LParamIndex]);
end;

{ TEFDateTimeStrMacroExpander }

function TEFDateTimeStrMacroExpander.InternalExpand(
  const AString: string): string;
begin
  Result := inherited InternalExpand(AString);
  Result := ExpandMacros(Result, '%YYYYMMDD_DATE%', FormatDateTime('yyyymmdd', Date));
  Result := ExpandMacros(Result, '%YYYYMMDD_YESTERDAY%', FormatDateTime('yyyymmdd', Date - 1));
  Result := ExpandMacros(Result, '%YYYYMMDD_TOMORROW%', FormatDateTime('yyyymmdd', Date + 1));
  Result := ExpandMacros(Result, '%YYMMDD_DATE%', FormatDateTime('yymmdd', Date));
  Result := ExpandMacros(Result, '%YYMMDD_YESTERDAY%', FormatDateTime('yymmdd', Date - 1));
  Result := ExpandMacros(Result, '%YYMMDD_TOMORROW%', FormatDateTime('yymmdd', Date + 1));
  Result := ExpandMacros(Result, '%MMDD_DATE%', FormatDateTime('mmdd', Date));
  Result := ExpandMacros(Result, '%DD_DATE%', FormatDateTime('dd', Date));
  Result := ExpandMacros(Result, '%MM_DATE%', FormatDateTime('mm', Date));
  Result := ExpandMacros(Result, '%YYYY_DATE%', FormatDateTime('yyyy', Date));
  Result := ExpandMacros(Result, '%WEEKDAYNAME_SHORT%', FormatDateTime('ddd', Date));
  Result := ExpandMacros(Result, '%WEEKDAYNAME_LONG%', FormatDateTime('dddd', Date));
  Result := ExpandMacros(Result, '%WEEKDAYNR%', IntToStr(DayOfWeek(Date)));
  Result := ExpandMacros(Result, '%ISOWEEKDAYNR%', IntToStr(DayOfTheWeek(Date)));
  Result := ExpandMacros(Result, '%HHMMSS_TIME%', FormatDateTime('hhnnss', Now));
  Result := ExpandMacros(Result, '%HHMM_TIME%', FormatDateTime('hhnn', Now));
  Result := ExpandMacros(Result, '%HH_TIME%', FormatDateTime('hh', Now));
  Result := ExpandMacros(Result, '%MM_TIME%', FormatDateTime('nn', Now));
  Result := ExpandMacros(Result, '%SS_TIME%', FormatDateTime('ss', Now));
end;

{ TEFFileMacroExpander }

function TEFFileMacroExpander.ExpandParams(const AString: string; const AParams: TStrings): string;
const
  PARAM_HEAD = '%%P';
  PARAM_TAIL = '%%';
var
  LPosHead: Integer;
  LPosTail: Integer;
  LParamIndex: Integer;
  LParamValue: string;
begin
  Assert(Assigned(AParams));

  Result := AString;
  LPosHead := Pos(PARAM_HEAD, Result);
  if LPosHead > 0 then
  begin
    LPosTail := PosEx(PARAM_TAIL, Result, LPosHead + 1);
    if LPosTail > 0 then
    begin
      LParamIndex := StrToIntDef(
        Copy(Result, LPosHead + Length(PARAM_HEAD),
          LPosTail - (LPosHead + Length(PARAM_HEAD))), -1);
      if (LParamIndex >= 0) and (LParamIndex < AParams.Count) then
        LParamValue := AParams[LParamIndex]
      else
        LParamValue := '';

      Result := Copy(Result, 1, LPosHead - 1) + LParamValue
        + ExpandParams(Copy(Result, LPosTail + Length(PARAM_TAIL), MaxInt), AParams);
    end;
  end;
end;

function TEFFileMacroExpander.InternalExpand(const AString: string): string;
const
  MACRO_HEAD = '%FILE(';
  MACRO_TAIL = ')%';
var
  LPosHead: Integer;
  LPosTail: Integer;
  LFileName: string;
  LFileStream: TFileStream;
  LStringStream: TStringStream;
  LFileContents: string;
  LParams: TStrings;
begin
  Result := inherited InternalExpand(AString);

  LPosHead := Pos(MACRO_HEAD, Result);
  if LPosHead > 0 then
  begin
    LPosTail := PosEx(MACRO_TAIL, Result, LPosHead + 1);
    if LPosTail > 0 then
    begin
      LParams := TStringList.Create;
      try
        LParams.CommaText := Copy(Result, LPosHead + Length(MACRO_HEAD),
          LPosTail - (LPosHead + Length(MACRO_HEAD)));

        if LParams.Count = 0 then
          LFileContents := ''
        else
        begin
          LFileName := LParams[0];
          if not IsAbsolutePath(LFileName) then
            if FDefaultPath <> '' then
              LFileName := FDefaultPath + LFileName
            else
              LFileName := IncludeTrailingPathDelimiter(GetCurrentDir) + LFileName;

          if FileExists(LFileName) then
          begin
            LFileStream := TFileStream.Create(LFileName, fmOpenRead or fmShareDenyWrite);
            try
              LStringStream := TStringStream.Create('');
              try
                LStringStream.CopyFrom(LFileStream, 0);
                LFileContents := LStringStream.DataString;
              finally
                LStringStream.Free;
              end;
            finally
              LFileStream.Free;
            end;
          end
          else
            LFileContents := '';
        end;

        LFileContents := ExpandParams(LFileContents, LParams);

        Result := Copy(Result, 1, LPosHead - 1) + LFileContents
          + InternalExpand(Copy(Result, LPosTail + Length(MACRO_TAIL), MaxInt));
      finally
        LParams.Free;
      end;
    end;
  end;
end;

{ TEFGUIDMacroExpander }

function TEFGUIDMacroExpander.InternalExpand(const AString: string): string;
begin
  Result := inherited InternalExpand(AString);
  Result := ExpandMacros(Result, '%GUID%', CreateGuidStr);
  Result := ExpandMacros(Result, '%COMPACT_GUID%', CreateCompactGuidStr);
end;

end.
