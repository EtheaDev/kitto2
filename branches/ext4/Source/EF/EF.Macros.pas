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
  SysUtils, Generics.Collections, Classes, Contnrs;
  
type
  ///	<summary>
  ///	  Abstract macro expander class. Descendants are able to expand a certain
  ///	  set of macros in a given string. This class is used with
  ///	  TEFMacroExpansionEngine but it can be used on its own as well.
  ///	</summary>
  TEFMacroExpander = class
  strict protected
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
  strict private
    FExpanders: TObjectList<TEFMacroExpander>;
    FPrevious: TEFMacroExpansionEngine;
    class var FInstance: TEFMacroExpansionEngine;
    class var FOnGetInstance: TEFGetMacroExpansionEngine;
    function CallExpanders(const AString: string): string;
    class function GetInstance: TEFMacroExpansionEngine; static;
  strict protected
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

  ///	<summary>
  ///	  <para>A macro expander that can expand some path-related macros.</para>
  ///	  <para>Supported macros (case sensitive):</para>
  ///	  <list type="table">
  ///	    <listheader>
  ///	      <term>Name</term>
  ///	      <description>Expands to</description>
  ///	    </listheader>
  ///	    <item>
  ///	      <term>%APP_PATH%</term>
  ///	      <description>ExtractFilePath(ParamStr(0))</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%APP_NAME%</term>
  ///	      <description>ParamStr(0)</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%APP_FILENAME%</term>
  ///	      <description>ExtractFileName(ParamStr(0))</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%APP_BASENAME%</term>
  ///	      <description>
  ///	      RemoveFileExt(ExtractFileName(ParamStr(0)))</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%WIN_DIR%</term>
  ///	      <description>GetWindowsDirectory (including final
  ///	      backslash)</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%SYS_DIR%</term>
  ///	      <description>GetSystemDirectory (including final
  ///	      backslash)</description>
  ///	    </item>
  ///	  </list>
  ///	</summary>
  TEFPathMacroExpander = class(TEFMacroExpander)
  strict protected
    function InternalExpand(const AString: string): string; override;
  end;

  ///	<summary>
  ///	  <para>A macro expander that can expand some system macros.</para>
  ///	  <para>Supported macros (case sensitive):</para>
  ///	  <list type="table">
  ///	    <listheader>
  ///	      <term>Name</term>
  ///	      <description>Expands to</description>
  ///	    </listheader>
  ///	    <item>
  ///	      <term>%DATE%</term>
  ///	      <description>DateToStr(Date)</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%YESTERDAY%</term>
  ///	      <description>DateToStr(Date - 1)</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%TOMORROW%</term>
  ///	      <description>DateToStr(Date + 1)</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%TIME%</term>
  ///	      <description>TimeToStr(Now)</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%PROCESS_ID%</term>
  ///	      <description>An integer value that is the current process'
  ///	      Id</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%THREAD_ID%</term>
  ///	      <description>An integer value that is the current thread's
  ///	      Id</description>
  ///	    </item>
  ///	  </list>
  ///	</summary>
  TEFSysMacroExpander = class(TEFMacroExpander)
  strict protected
    function InternalExpand(const AString: string): string; override;
  end;

  ///	<summary>A macro expander that expands all environment variables. See
  ///	EF.SysUtils.ExpandEnvironmentVariables for details on format and case
  ///	sensitivity.</summary>
  ///	<example>%COMPUTERNAME% expands to the current computer's name.</example>
  ///	<seealso cref="EF.SysUtils.ExpandEnvironmentVariables"></seealso>
  TEFEnvironmentVariableMacroExpander = class(TEFMacroExpander)
  strict protected
    function InternalExpand(const AString: string): string; override;
  end;

  ///	<summary>Expands named parameters passed on the command line. A named
  ///	command line parameter is prefixed with / or - characters, it has a name,
  ///	a space and a value.</summary>
  ///	<example>
  ///	  <para>Given the following command line:</para>
  ///	  <para><c>app.exe /p1 value -p2 value2</c></para>
  ///	  <para>the sequence '%Cmd:p1%' expands to 'value'.</para>
  ///	</example>
  ///	<seealso cref="EF.SysUtils.GetCmdLineParamValue"></seealso>
  TEFCmdLineParamMacroExpander = class(TEFMacroExpander)
  strict private
    FParams: TStringList;
    procedure ReadCmdLineParams;
  strict protected
    function InternalExpand(const AString: string): string; override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

  ///	<summary>
  ///	  <para>Expands the current date and/or time formatted in several
  ///	  ways.</para>
  ///	  <para>Supported macros (case sensitive):</para>
  ///	  <list type="table">
  ///	    <listheader>
  ///	      <term>Name</term>
  ///	      <description>Expands to</description>
  ///	    </listheader>
  ///	    <item>
  ///	      <term>%YYYYMMDD_DATE%</term>
  ///	      <description>The current date (Date) in YYYYMMDD
  ///	      format</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%YYYYMMDD_YESTERDAY%</term>
  ///	      <description>Date - 1 day in YYYYMMDD format</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%YYYYMMDD_TOMORROW%</term>
  ///	      <description>Date + 1 day in YYYYMMDD format</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%YYMMDD_DATE%</term>
  ///	      <description>Date in YYMMDD format</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%YYMMDD_YESTERDAY%</term>
  ///	      <description>Date - 1 day in YYMMDD format</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%YYMMDD_TOMORROW%</term>
  ///	      <description>Date + 1 day in YYMMDD format</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%MMDD_DATE%</term>
  ///	      <description>The current date in MMDD format</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%DD_DATE%</term>
  ///	      <description>The current day of the month</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%MM_DATE%</term>
  ///	      <description>The current month of the year</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%YYYY_DATE%</term>
  ///	      <description>The current year</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%WEEKDAYNAME_SHORT%</term>
  ///	      <description>The short name of the current day of the
  ///	      week</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%WEEKDAYNAME_LONG%</term>
  ///	      <description>The name of the current day of the week</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%WEEKDAYNR%</term>
  ///	      <description>The number of the current day of the week (1 =
  ///	      sunday)</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%ISOWEEKDAYNR%</term>
  ///	      <description>The number of the current day of the week (1 =
  ///	      monday)</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%HHMMSS_TIME%</term>
  ///	      <description>The current time in HHMMSS format</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%HHMM_TIME%</term>
  ///	      <description>The current time in HHMM format</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%HH_TIME%</term>
  ///	      <description>The current hour</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%MM_TIME%</term>
  ///	      <description>The current minute</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%SS_TIME%</term>
  ///	      <description>The current second</description>
  ///	    </item>
  ///	  </list>
  ///	</summary>
  TEFDateTimeStrMacroExpander = class(TEFMacroExpander)
  strict protected
    function InternalExpand(const AString: string): string; override;
  end;

  ///	<summary>
  ///	  <para>A macro expander that generates GUIDs.</para>
  ///	  <para>Supported macros (case sensitive):</para>
  ///	  <list type="table">
  ///	    <listheader>
  ///	      <term>Name</term>
  ///	      <description>Expands to</description>
  ///	    </listheader>
  ///	    <item>
  ///	      <term>%GUID%</term>
  ///	      <description>CreateGuidStr()</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%COMPACT_GUID%</term>
  ///	      <description>CreateCompactGuidStr()</description>
  ///	    </item>
  ///	  </list>
  ///	</summary>
  ///	<seealso cref="CreateGuidStr"></seealso>
  ///	<seealso cref="CreateCompactGuidStr"></seealso>
  TEFGUIDMacroExpander = class(TEFMacroExpander)
  strict protected
    function InternalExpand(const AString: string): string; override;
  end;

  ///	<summary>
  ///	  <para>A macro expander that expands symbols to characters.</para>
  ///	  <para>Supported macros (case sensitive):</para>
  ///	  <list type="table">
  ///	    <listheader>
  ///	      <term>Name</term>
  ///	      <description>Expands to</description>
  ///	    </listheader>
  ///	    <item>
  ///	      <term>%TAB%</term>
  ///	      <description>The Tab (#9) character</description>
  ///	    </item>
  ///	    <item>
  ///	      <term>%SPACE%</term>
  ///	      <description>The space character</description>
  ///	    </item>
  ///	  </list>
  ///	</summary>
  ///	<seealso cref="CreateGuidStr"></seealso>
  ///	<seealso cref="CreateCompactGuidStr"></seealso>
  TEFEntityMacroExpander = class(TEFMacroExpander)
  strict protected
    function InternalExpand(const AString: string): string; override;
  end;

  TStringArray = TArray<string>;

  ///	<summary>A base class for macro expanders that support parameterized
  ///	macros, in the form %NAME([param1[,param2[,...]])%.</summary>
  TEFParameterizedMacroExpanderBase = class abstract(TEFMacroExpander)
  strict protected
    ///	<summary>Returns a list of names of macros to look for.</summary>
    function GetMacroNames: TArray<string>; virtual; abstract;

    ///	<summary>Expands the specified parameterized macro and returns the
    ///	expanded value; the default implementation returns an empty string.
    ///	Derived classes should expand supported macros (returned by
    ///	GetMacroNames) and call inherited for any unsupported case.</summary>
    function ExpandParameterizedMacro(const AMacroName: string; const AParams: TArray<string>): string; virtual;

    function InternalExpand(const AString: string): string; override;
  end;

  ///	<summary>
  ///	  <para>The %FILE()% macro expands to the contents of a specified (text)
  ///	  file.</para>
  ///	  <para>The macro format is as follows:</para>
  ///	  <list type="table">
  ///	    <listheader>
  ///	      <term>Format</term>
  ///	      <description>Description</description>
  ///	    </listheader>
  ///	    <item>
  ///	      <term>(%FILE(&lt;filename&gt;[,&lt;param1&gt;[...]]))%</term>
  ///	      <description>The contents of &lt;filename&gt;. If &lt;filename&gt;
  ///	      is not a fully qualified filename, then the value of the
  ///	      DefaultPath property of the expander is pre-prended to it before
  ///	      trying to load it. If parameters are passed after the file name,
  ///	      then they are used to substitute parameter placeholders in the file
  ///	      contents. Placeholders are in the form %%P1%%, %%P2%% and so
  ///	      on.</description>
  ///	    </item>
  ///	  </list>
  ///	</summary>
  ///	<remarks>
  ///	  <para>Any macros in the file name or contents are expanded as
  ///	  well.</para>
  ///	  <para>If a parameter (or the file name) includes a space or a comma,
  ///	  the entire parameter should be enclosed in ". Space and comma are both
  ///	  valid parameter separators. If a parameter includes the " character,
  ///	  then the character should be doubled (as well as the entire parameter
  ///	  be enclosed between " characters).</para>
  ///	  <para>If the file name is not specified or the file doesn't exist, then
  ///	  the macro expands to ''. If a parameter doesn't exist, it expands to
  ///	  ''.</para>
  ///	</remarks>
  TEFFileMacroExpander = class(TEFParameterizedMacroExpanderBase)
  strict private
    FDefaultPath: string;
    function ExpandParams(const AString: string;
      const AParams: TArray<string>): string;
  strict protected
    function GetMacroNames: TArray<string>; override;
    function ExpandParameterizedMacro(const AMacroName: string;
      const AParams: TArray<string>): string; override;
  public
    {
      The value of this property is pre-pended to any relative file name
      before loading the file contents as part of the expansion process.
      Leave this property empty to have the expander use the current directory
      as default path instead.
    }
    property DefaultPath: string read FDefaultPath write FDefaultPath;
  end;

///	<summary>Creates and adds instances of all standard macro expanders to the
///	specified macro expansion engine, which acquires ownership of
///	them.</summary>
procedure AddStandardMacroExpanders(const AMacroExpansionEngine: TEFMacroExpansionEngine);

implementation

uses
  Windows, DateUtils, StrUtils, Types,
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
  AMacroExpansionEngine.AddExpander(TEFEntityMacroExpander.Create);
  AMacroExpansionEngine.AddExpander(TEFFileMacroExpander.Create);
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

function TEFFileMacroExpander.ExpandParameterizedMacro(const AMacroName: string;
  const AParams: TArray<string>): string;
var
  LFileName: string;
  I: Integer;
  LOtherParams: TArray<string>;
begin
  if SameText(AMacroName, 'FILE') then
  begin
    if Length(AParams) > 0 then
    begin
      LFileName := AParams[0];
      if not IsAbsolutePath(LFileName) then
        if FDefaultPath <> '' then
          LFileName := IncludeTrailingPathDelimiter(FDefaultPath) + LFileName
        else
          LFileName := IncludeTrailingPathDelimiter(GetCurrentDir) + LFileName;

      if FileExists(LFileName) then
      begin
        Result := TEFMacroExpansionEngine.Instance.Expand(TextFileToString(LFileName));
        if Length(AParams) > 1 then
        begin
          SetLength(LOtherParams, Length(AParams) - 1);
          for I := 1 to High(AParams) do
            LOtherParams[I - 1] := AParams[I];
          Result := ExpandParams(Result, LOtherParams);
        end;
      end
      else
        Result := '';
    end
    else
      Result := '';
  end
  else
    Result := inherited ExpandParameterizedMacro(AMAcroName, AParams);
end;

function TEFFileMacroExpander.ExpandParams(const AString: string; const AParams: TArray<string>): string;
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
      if (LParamIndex >= 0) and (LParamIndex < Length(AParams)) then
        LParamValue := AParams[LParamIndex]
      else
        LParamValue := '';

      Result := Copy(Result, 1, LPosHead - 1) + LParamValue
        + ExpandParams(Copy(Result, LPosTail + Length(PARAM_TAIL), MaxInt), AParams);
    end;
  end;
end;

function TEFFileMacroExpander.GetMacroNames: System.TArray<System.string>;
begin
  SetLength(Result, 1);
  Result[0] := 'FILE';
end;

{ TEFGUIDMacroExpander }

function TEFGUIDMacroExpander.InternalExpand(const AString: string): string;
begin
  Result := inherited InternalExpand(AString);
  Result := ExpandMacros(Result, '%GUID%', CreateGuidStr);
  Result := ExpandMacros(Result, '%COMPACT_GUID%', CreateCompactGuidStr);
end;

{ TEFEntityMacroExpander }

function TEFEntityMacroExpander.InternalExpand(const AString: string): string;
begin
  Result := inherited InternalExpand(AString);

  Result := ExpandMacros(Result, '%TAB%', #9);
  Result := ExpandMacros(Result, '%SPACE', ' ');
end;

{ TEFParameterizedMacroExpanderBase }

function TEFParameterizedMacroExpanderBase.InternalExpand(
  const AString: string): string;
const
  MACRO_TAIL = ')%';
var
  LMacroNames: TArray<string>;
  LMacroName: string;
  LMacroHead: string;
  LPosHead: Integer;
  LPosTail: Integer;
  LParams: TStrings;
  LParamsArray: TArray<string>;
  I: Integer;
begin
  Result := inherited InternalExpand(AString);

  LMacroNames := GetMacroNames;
  for LMacroName in LMacroNames do
  begin
    LMacroHead := '%' + LMacroName + '(';

    LPosHead := Pos(LMacroHead, Result);
    if LPosHead > 0 then
    begin
      LPosTail := PosEx(MACRO_TAIL, Result, LPosHead + 1);
      if LPosTail > 0 then
      begin
        LParams := TStringList.Create;
        try
          LParams.CommaText := Copy(Result, LPosHead + Length(LMacroHead),
            LPosTail - (LPosHead + Length(LMacroHead)));
          LParamsArray := LParams.ToStringArray;
          for I := 0 to High(LParamsArray) do
            LParamsArray[I] := TEFMacroExpansionEngine.Instance.Expand(LParamsArray[I]);
          Result := Copy(Result, 1, LPosHead - 1) + ExpandParameterizedMacro(LMacroName, LParamsArray)
            + InternalExpand(Copy(Result, LPosTail + Length(MACRO_TAIL), MaxInt));
        finally
          LParams.Free;
        end;
      end;
    end;
  end;
end;

function TEFParameterizedMacroExpanderBase.ExpandParameterizedMacro(
  const AMacroName: string; const AParams: TArray<string>): string;
begin
  Result := '';
end;

end.
