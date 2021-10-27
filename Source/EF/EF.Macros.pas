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

/// <summary>
///   This unit defines classes and routines to help with macro-expansion in
///   strings. The macro-expansion engine can be extended by defining and
///   registering custom expander classes.
/// </summary>
unit EF.Macros;

{$I EF.Defines.inc}

interface

uses
  SysUtils
  , Generics.Collections
  , Classes
  ;

type
  TEFMacroExpansionEngine = class;

  /// <summary>
  ///  Abstract macro expander class. Descendants are able to expand a certain
  ///  set of macros in a given string. This class is used with
  ///  TEFMacroExpansionEngine but it can be used on its own as well.
  /// </summary>
  TEFMacroExpander = class
  private
    FOwner: TEFMacroExpansionEngine;
  strict protected
    /// <summary>
    ///   Utility method: replaces all occurrences of AMacroName in AString with
    ///   AMacroValue. Should be used by all inherited classes to expand macros.
    ///   Implements support for control sequences (such as %Q, that encapsulates QuotedStr()).
    /// </summary>
    procedure ExpandMacros(var AString: string; const AMacroName, AMacroValue: string); inline;

    /// <summary>
    ///  Implements Expand. The default implementation doesn't change
    ///  the input argument except for translating parts of it that are
    ///  enclosed between '_(' and ')'.
    /// </summary>
    procedure InternalExpand(var AString: string); virtual;

    function GetFormatSettings: TFormatSettings;
  public
    constructor Create; virtual;

    /// <summary>
    ///   Expands all supported macros found in a given string, and returns the
    ///   string with macros expanded. Calls the virtual protected method
    ///   InternalExpand to do the job.
    /// </summary>
    procedure Expand(var AString: string); inline;
  end;
  TEFMacroExpanderClass = class of TEFMacroExpander;

  TEFGetMacroExpansionEngine = reference to function: TEFMacroExpansionEngine;

  /// <summary>
  ///   The engine is able to expand an open-ended set of macros in a given
  ///   string. Use AddExpander calls to add support for more macros, and call
  ///   Expand to trigger macro expansion. Alternatively, you can use a
  ///   ready-made singleton engine which supports all registered expanders
  ///   out-of-the-box. Macro expansion engines can be chained: each engine
  ///   calls the previous engine in the chain, if available, when invoked.
  /// </summary>
  TEFMacroExpansionEngine = class
  strict private
    FExpanders: TObjectList<TEFMacroExpander>;
    FPrevious: TEFMacroExpansionEngine;
    FOnGetFormatSettings: TFunc<TFormatSettings>;
    class var FInstance: TEFMacroExpansionEngine;
    class var FOnGetInstance: TEFGetMacroExpansionEngine;
    class threadvar FDisableCount: Integer;
    procedure CallExpanders(var AString: string);
    class function GetInstance: TEFMacroExpansionEngine; static;
  private
    function GetFormatSettings: TFormatSettings;
    class procedure SetOnGetInstance(const Value: TEFGetMacroExpansionEngine); static;
  strict protected
    class destructor Destroy;
  public
    constructor Create(const APrevious: TEFMacroExpansionEngine = nil);
    destructor Destroy; override;
  public
    class property Instance: TEFMacroExpansionEngine read GetInstance;

    class property OnGetInstance: TEFGetMacroExpansionEngine read FOnGetInstance write SetOnGetInstance;

    /// <summary>
    ///   Expands all recognized macros in AString and returns the resulting
    ///   string. The result depends on the set of macro expanders used. Add a
    ///   new macro expander by calling AddExpander before calling Expand. If
    ///   no macro expanders are used, the result is the same as the input
    ///   string.
    /// </summary>
    procedure Expand(var AString: string);

    /// <summary>
    ///   Adds an expander to the list used by the Expand method. Acquires
    ///   ownership of AExpander, which means it will be destroyed when
    ///   RemoveExpanders or ClearExpanders are called, or when the current
    ///   object is itself destroyed.
    /// </summary>
    procedure AddExpander(const AExpander: TEFMacroExpander);

    /// <summary>
    ///   Removes a previously added expander from the list used by the Expand
    ///   method. Returns True if the specified object was found and removed,
    ///   and False otherwise.
    /// </summary>
    /// <remarks>
    ///   The removed object is NOT freed automatically.
    /// </remarks>
    function RemoveExpander(const AExpander: TEFMacroExpander): Boolean;

    /// <summary>
    ///   Returns the index of a given expander in the list, or -1 if it is not
    ///   found. Use this method to check whether an expander is part of an
    ///   expansion engine.
    /// </summary>
    function IndexOfExpander(const AExpander: TEFMacroExpander): Integer;

    /// <summary>
    ///   Removes from the list and frees all expanders of the given class.
    /// </summary>
    procedure RemoveExpanders(const AExpanderClass: TEFMacroExpanderClass);

    /// <summary>
    ///  Removes all expanders from the list and destroys them.
    /// </summary>
    procedure ClearExpanders;

    /// <summary>
    ///  Function that returns settings for format-sensitive macros such as %DATE%.
    ///  If not specified, global FormatSettings variable is used.
    /// </summary>
    property OnGetFormatSettings: TFunc<TFormatSettings> read FOnGetFormatSettings write FOnGetFormatSettings;

    /// <summary>
    ///  Disables macro expansion (meaning that strings are returned untouched)
    ///  for the current thread. Calls can be nested. Each call to Disable must
    ///  be matched by a call to Enable in the same thread.
    /// </summary>
    class procedure DisableForCurrentThread;

    /// <summary>
    ///  Re-enables macro expansion for the current thread after a call to Disable.
    ///  Calls can be nested. Each call to Disable must be matched by a call to Enable
    ///  in the same thread.
    /// </summary>
    class procedure EnableForCurrentThread;
  end;

  /// <summary>
  ///   <para>A macro expander that can expand some path-related macros.</para>
  ///   <para>Supported macros (case sensitive):</para>
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Name</term>
  ///       <description>Expands to</description>
  ///     </listheader>
  ///     <item>
  ///       <term>%APP_PATH%</term>
  ///       <description>ExtractFilePath(ParamStr(0))</description>
  ///     </item>
  ///     <item>
  ///       <term>%APP_NAME%</term>
  ///       <description>ParamStr(0)</description>
  ///     </item>
  ///     <item>
  ///       <term>%APP_FILENAME%</term>
  ///       <description>ExtractFileName(ParamStr(0))</description>
  ///     </item>
  ///     <item>
  ///       <term>%APP_BASENAME%</term>
  ///       <description>
  ///       RemoveFileExt(ExtractFileName(ParamStr(0)))</description>
  ///     </item>
  ///     <item>
  ///       <term>%WIN_DIR%</term>
  ///       <description>GetWindowsDirectory (including final
  ///       backslash)</description>
  ///     </item>
  ///     <item>
  ///       <term>%SYS_DIR%</term>
  ///       <description>GetSystemDirectory (including final
  ///       backslash)</description>
  ///     </item>
  ///   </list>
  /// </summary>
  TEFPathMacroExpander = class(TEFMacroExpander)
  strict protected
    procedure InternalExpand(var AString: string); override;
  end;

  /// <summary>
  ///   <para>A macro expander that can expand some system macros.</para>
  ///   <para>Supported macros (case sensitive):</para>
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Name</term>
  ///       <description>Expands to</description>
  ///     </listheader>
  ///     <item>
  ///       <term>%DATE%</term>
  ///       <description>DateToStr(Date)</description>
  ///     </item>
  ///     <item>
  ///       <term>%YESTERDAY%</term>
  ///       <description>DateToStr(Date - 1)</description>
  ///     </item>
  ///     <item>
  ///       <term>%TOMORROW%</term>
  ///       <description>DateToStr(Date + 1)</description>
  ///     </item>
  ///     <item>
  ///       <term>%TIME%</term>
  ///       <description>TimeToStr(Now)</description>
  ///     </item>
  ///     <item>
  ///       <term>%DATETIME%</term>
  ///       <description>DateTimeToStr(Now)</description>
  ///     </item>
  ///     <item>
  ///       <term>%PROCESS_ID%</term>
  ///       <description>An integer value that is the current process'
  ///       Id</description>
  ///     </item>
  ///     <item>
  ///       <term>%THREAD_ID%</term>
  ///       <description>An integer value that is the current thread's
  ///       Id</description>
  ///     </item>
  ///   </list>
  /// </summary>
  TEFSysMacroExpander = class(TEFMacroExpander)
  strict protected
    procedure InternalExpand(var AString: string); override;
  end;

  /// <summary>
  ///  A macro expander that expands all environment variables. See
  ///  EF.Sys.ExpandEnvironmentVariables for details on format and case
  ///  sensitivity.
  /// </summary>
  /// <example>%COMPUTERNAME% expands to the current computer's name.</example>
  /// <seealso cref="EF.Sys.ExpandEnvironmentVariables"></seealso>
  TEFEnvironmentVariableMacroExpander = class(TEFMacroExpander)
  strict protected
    procedure InternalExpand(var AString: string); override;
  end;

  /// <summary>Expands named parameters passed on the command line. A named
  /// command line parameter is prefixed with / or - characters, it has a name,
  /// a space and a value.</summary>
  /// <example>
  ///   <para>Given the following command line:</para>
  ///   <para><c>app.exe /p1 value -p2 value2</c></para>
  ///   <para>the sequence '%Cmd:p1%' expands to 'value'.</para>
  /// </example>
  /// <seealso cref="EF.Sys.GetCmdLineParamValue"></seealso>
  TEFCmdLineParamMacroExpander = class(TEFMacroExpander)
  strict private
    FParams: TStringList;
    procedure ReadCmdLineParams;
  strict protected
    procedure InternalExpand(var AString: string); override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   <para>Expands the current date and/or time formatted in several
  ///   ways.</para>
  ///   <para>Supported macros (case sensitive):</para>
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Name</term>
  ///       <description>Expands to</description>
  ///     </listheader>
  ///     <item>
  ///       <term>%YYYYMMDD_DATE%</term>
  ///       <description>The current date (Date) in YYYYMMDD
  ///       format</description>
  ///     </item>
  ///     <item>
  ///       <term>%YYYYMMDD_YESTERDAY%</term>
  ///       <description>Date - 1 day in YYYYMMDD format</description>
  ///     </item>
  ///     <item>
  ///       <term>%YYYYMMDD_TOMORROW%</term>
  ///       <description>Date + 1 day in YYYYMMDD format</description>
  ///     </item>
  ///     <item>
  ///       <term>%YYMMDD_DATE%</term>
  ///       <description>Date in YYMMDD format</description>
  ///     </item>
  ///     <item>
  ///       <term>%YYMMDD_YESTERDAY%</term>
  ///       <description>Date - 1 day in YYMMDD format</description>
  ///     </item>
  ///     <item>
  ///       <term>%YYMMDD_TOMORROW%</term>
  ///       <description>Date + 1 day in YYMMDD format</description>
  ///     </item>
  ///     <item>
  ///       <term>%MMDD_DATE%</term>
  ///       <description>The current date in MMDD format</description>
  ///     </item>
  ///     <item>
  ///       <term>%DD_DATE%</term>
  ///       <description>The current day of the month</description>
  ///     </item>
  ///     <item>
  ///       <term>%MM_DATE%</term>
  ///       <description>The current month of the year</description>
  ///     </item>
  ///     <item>
  ///       <term>%YYYY_DATE%</term>
  ///       <description>The current year</description>
  ///     </item>
  ///     <item>
  ///       <term>%WEEKDAYNAME_SHORT%</term>
  ///       <description>The short name of the current day of the
  ///       week</description>
  ///     </item>
  ///     <item>
  ///       <term>%WEEKDAYNAME_LONG%</term>
  ///       <description>The name of the current day of the week</description>
  ///     </item>
  ///     <item>
  ///       <term>%WEEKDAYNR%</term>
  ///       <description>The number of the current day of the week (1 =
  ///       sunday)</description>
  ///     </item>
  ///     <item>
  ///       <term>%ISOWEEKDAYNR%</term>
  ///       <description>The number of the current day of the week (1 =
  ///       monday)</description>
  ///     </item>
  ///     <item>
  ///       <term>%HHMMSS_TIME%</term>
  ///       <description>The current time in HHMMSS format</description>
  ///     </item>
  ///     <item>
  ///       <term>%HHMM_TIME%</term>
  ///       <description>The current time in HHMM format</description>
  ///     </item>
  ///     <item>
  ///       <term>%HH_TIME%</term>
  ///       <description>The current hour</description>
  ///     </item>
  ///     <item>
  ///       <term>%MM_TIME%</term>
  ///       <description>The current minute</description>
  ///     </item>
  ///     <item>
  ///       <term>%SS_TIME%</term>
  ///       <description>The current second</description>
  ///     </item>
  ///   </list>
  /// </summary>
  TEFDateTimeStrMacroExpander = class(TEFMacroExpander)
  strict protected
    procedure InternalExpand(var AString: string); override;
  end;

  /// <summary>
  ///   <para>A macro expander that generates GUIDs.</para>
  ///   <para>Supported macros (case sensitive):</para>
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Name</term>
  ///       <description>Expands to</description>
  ///     </listheader>
  ///     <item>
  ///       <term>%GUID%</term>
  ///       <description>CreateGuidStr()</description>
  ///     </item>
  ///     <item>
  ///       <term>%COMPACT_GUID%</term>
  ///       <description>CreateCompactGuidStr()</description>
  ///     </item>
  ///   </list>
  /// </summary>
  /// <seealso cref="CreateGuidStr"></seealso>
  /// <seealso cref="CreateCompactGuidStr"></seealso>
  TEFGUIDMacroExpander = class(TEFMacroExpander)
  strict protected
    procedure InternalExpand(var AString: string); override;
  end;

  /// <summary>
  ///   <para>A macro expander that expands symbols to characters.</para>
  ///   <para>Supported macros (case sensitive):</para>
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Name</term>
  ///       <description>Expands to</description>
  ///     </listheader>
  ///     <item>
  ///       <term>%TAB%</term>
  ///       <description>The Tab (#9) character</description>
  ///     </item>
  ///     <item>
  ///       <term>%SPACE%</term>
  ///       <description>The space character</description>
  ///     </item>
  ///   </list>
  /// </summary>
  /// <seealso cref="CreateGuidStr"></seealso>
  /// <seealso cref="CreateCompactGuidStr"></seealso>
  TEFEntityMacroExpander = class(TEFMacroExpander)
  strict protected
    procedure InternalExpand(var AString: string); override;
  end;

  TStringArray = TArray<string>;

  /// <summary>A base class for macro expanders that support parameterized
  /// macros, in the form %NAME([param1[,param2[,...]])%.</summary>
  TEFParameterizedMacroExpanderBase = class abstract(TEFMacroExpander)
  strict protected
    /// <summary>Returns a list of names of macros to look for.</summary>
    function GetMacroNames: TArray<string>; virtual; abstract;

    /// <summary>Expands the specified parameterized macro and returns the
    /// expanded value; the default implementation returns an empty string.
    /// Derived classes should expand supported macros (returned by
    /// GetMacroNames) and call inherited for any unsupported case.</summary>
    function ExpandParameterizedMacro(const AMacroName: string; const AParams: TArray<string>): string; virtual;

    procedure InternalExpand(var AString: string); override;
  end;

  /// <summary>
  ///   <para>The %FILE()% macro expands to the contents of a specified (text)
  ///   file.</para>
  ///   <para>The macro format is as follows:</para>
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Format</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>(%FILE(&lt;filename&gt;[,&lt;param1&gt;[...]]))%</term>
  ///       <description>The contents of &lt;filename&gt;. If &lt;filename&gt;
  ///       is not a fully qualified filename, then the value of the
  ///       DefaultPath property of the expander is pre-prended to it before
  ///       trying to load it. If parameters are passed after the file name,
  ///       then they are used to substitute parameter placeholders in the file
  ///       contents. Placeholders are in the form %%P1%%, %%P2%% and so
  ///       on.</description>
  ///     </item>
  ///   </list>
  /// </summary>
  /// <remarks>
  ///   <para>Any macros in the file name or contents are expanded as
  ///   well.</para>
  ///   <para>If a parameter (or the file name) includes a space or a comma,
  ///   the entire parameter should be enclosed in ". Space and comma are both
  ///   valid parameter separators. If a parameter includes the " character,
  ///   then the character should be doubled (as well as the entire parameter
  ///   be enclosed between " characters).</para>
  ///   <para>If the file name is not specified or the file doesn't exist, then
  ///   the macro expands to ''. If a parameter doesn't exist, it expands to
  ///   ''.</para>
  /// </remarks>
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

  /// <summary>
  ///   <para>The %FILENAME_TO_URL()% macro converts the server filename
  ///   as an url file name.</para>
  /// </summary>
  TEFFileNameToUrlMacroExpander = class(TEFParameterizedMacroExpanderBase)
  strict private
    function ExpandParam(const AFileName: string): string;
  strict protected
    function GetMacroNames: TArray<string>; override;
    function ExpandParameterizedMacro(const AMacroName: string;
      const AParams: TArray<string>): string; override;
  end;

/// <summary>Creates and adds instances of all standard macro expanders to the
/// specified macro expansion engine, which acquires ownership of
/// them.</summary>
procedure AddStandardMacroExpanders(const AMacroExpansionEngine: TEFMacroExpansionEngine);

implementation

uses
  DateUtils
  {$IFDEF MSWINDOWS}
  , Windows
  {$ENDIF}
  , StrUtils
  , Types
  , EF.Localization
  , EF.StrUtils
  , EF.Sys
  {$IFDEF MSWINDOWS}
  , EF.Sys.Windows
  {$ENDIF}
  ;

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
  AMacroExpansionEngine.AddExpander(TEFFileNameToUrlMacroExpander.Create);
end;

{ TEFMacroExpander }

procedure TEFMacroExpander.ExpandMacros(var AString: string; const AMacroName, AMacroValue: string);
begin
  // First try control sequences, then standard, as the latter format is part
  // of the former (almost all standard macros begin with '%'.
  ReplaceAllCaseSensitive(AString, '%Q' + AMacroName, QuotedStr(AMacroValue));
  // Standard.
  ReplaceAllCaseSensitive(AString, AMacroName, AMacroValue);
end;

function TEFMacroExpander.GetFormatSettings: TFormatSettings;
begin
  if Assigned(FOwner) then
    Result := FOwner.GetFormatSettings
  else
    Result :=  FormatSettings;
end;

procedure TEFMacroExpander.InternalExpand(var AString: string);
var
  LLocalizableStartPos, LLocalizableEndPos: integer;
  LLocalizableString: string;
  LLocalizableStringLength: Integer;
  LLocalizedString: string;
begin
  // If parts of the string are enclosed between '_(' and ')', extract and translate them.
  // e.g. <p>_(User: %Auth:UserName%)</p>
  // returns: <p>_(Utente: %Auth:UserName%)</p>
  // Fully-enclosed localizable strings are taken care of elsewhere.
  LLocalizableStartPos := Pos('_(', AString);
  while LLocalizableStartPos > 1 do
  begin
    LLocalizableString := Copy(AString, LLocalizableStartPos + 2, MaxInt);
    LLocalizableEndPos := Pos(')', LLocalizableString);
    if LLocalizableEndPos > 1 then
    begin
      Delete(LLocalizableString, LLocalizableEndPos, MaxInt);
      LLocalizableStringLength := Length(LLocalizableString);
      LLocalizedString := _(LLocalizableString);
      // Remove localizable string and localization directive.
      Delete(AString, LLocalizableStartPos, LLocalizableStringLength + 3);
      // Replace with localized string.
      Insert(LLocalizedString, AString, LLocalizableStartPos);
    end;
    LLocalizableStartPos := Pos('_(', AString);
  end;
end;

constructor TEFMacroExpander.Create;
begin
end;

procedure TEFMacroExpander.Expand(var AString: string);
begin
  InternalExpand(AString);
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

class procedure TEFMacroExpansionEngine.DisableForCurrentThread;
begin
  Inc(FDisableCount);
end;

procedure TEFMacroExpansionEngine.AddExpander(const AExpander: TEFMacroExpander);
begin
  Assert(Assigned(AExpander));
  Assert(AExpander.FOwner = nil);
  
  FExpanders.Add(AExpander);
  AExpander.FOwner := Self;
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

class procedure TEFMacroExpansionEngine.SetOnGetInstance(const Value: TEFGetMacroExpansionEngine);
begin
  FreeAndNil(FInstance);
  FOnGetInstance := Value;
end;

procedure TEFMacroExpansionEngine.ClearExpanders;
begin
  FExpanders.Clear;
end;

class procedure TEFMacroExpansionEngine.EnableForCurrentThread;
begin
  Dec(FDisableCount);
end;

procedure TEFMacroExpansionEngine.Expand(var AString: string);
var
  LPreviousString: string;
begin
  if FDisableCount > 0 then
    Exit;

  // Keep iterating until all macros in all included files are expanded.
  // This also allows to support macros in macros, at a performance cost.
  LPreviousString := AString;
  CallExpanders(AString);
  while LPreviousString <> AString do
  begin
    LPreviousString := AString;
    CallExpanders(AString);
  end;
end;

function TEFMacroExpansionEngine.GetFormatSettings: TFormatSettings;
begin
  if Assigned(FOnGetFormatSettings) then
    Result := FOnGetFormatSettings
  else
    Result := FormatSettings;
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

procedure TEFMacroExpansionEngine.CallExpanders(var AString: string);
var
  I: Integer;
begin
  if Assigned(FPrevious) then
    FPrevious.Expand(AString);
  for I := 0 to FExpanders.Count - 1 do
    TEFMacroExpander(FExpanders[I]).Expand(AString);
end;

{ TEFPathMacroExpander }

procedure TEFPathMacroExpander.InternalExpand(var AString: string);
var
  LMajorVersion, LMinorVersion, LRelease, LBuild : integer;
begin
  inherited InternalExpand(AString);
  if AString.Contains('%APP_PATH%') then
    ExpandMacros(AString, '%APP_PATH%', ExtractFilePath(ParamStr(0)));
  if AString.Contains('%APP_NAME%') then
    ExpandMacros(AString, '%APP_NAME%', ParamStr(0));
  if AString.Contains('%APP_FILENAME%') then
    ExpandMacros(AString, '%APP_FILENAME%', ExtractFileName(ParamStr(0)));
  if AString.Contains('%APP_BASENAME%') then
    ExpandMacros(AString, '%APP_BASENAME%', ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
  {$IFDEF MSWINDOWS}
  if AString.Contains('%WIN_DIR%') then
    ExpandMacros(AString, '%WIN_DIR%', IncludeTrailingPathDelimiter(SafeGetWindowsDirectory));
  if AString.Contains('%SYS_DIR%') then
    ExpandMacros(AString, '%SYS_DIR%', IncludeTrailingPathDelimiter(SafeGetSystemDirectory));
  if AString.Contains('%APP_VERSION') then
  begin
    GetVerInfo(ParamStr(0), LMajorVersion, LMinorVersion, LRelease, LBuild);
    ExpandMacros(AString, '%APP_VERSION%', Format('%d.%d', [LMajorVersion, LMinorVersion]));
    ExpandMacros(AString, '%APP_VERSION_FULL%', Format('%d.%d.%d', [LMajorVersion, LMinorVersion, LRelease]));
  end;
  {$ENDIF}
end;

{ TEFSysMacroExpander }

procedure TEFSysMacroExpander.InternalExpand(var AString: string);
var
  LFormatSettings: TFormatSettings;
begin
  inherited InternalExpand(AString);
  LFormatSettings := GetFormatSettings;
  if AString.Contains('%DATE%') then
    ExpandMacros(AString, '%DATE%', FormatDateTime(LFormatSettings.ShortDateFormat, Date));
  if AString.Contains('%YESTERDAY%') then
    ExpandMacros(AString, '%YESTERDAY%', FormatDateTime(LFormatSettings.ShortDateFormat, Date - 1));
  if AString.Contains('%TOMORROW%') then
    ExpandMacros(AString, '%TOMORROW%', FormatDateTime(LFormatSettings.ShortDateFormat, Date + 1));
  if AString.Contains('%TIME%') then
    ExpandMacros(AString, '%TIME%', FormatDateTime(LFormatSettings.ShortTimeFormat, Now));
  if AString.Contains('%DATETIME%') then
    ExpandMacros(AString, '%DATETIME%', FormatDateTime(LFormatSettings.ShortDateFormat, Now)+' '+ FormatDateTime(LFormatSettings.ShortTimeFormat, Now));
  {$IFDEF MSWINDOWS}
  if AString.Contains('%PROCESS_ID%') then
    ExpandMacros(AString, '%PROCESS_ID%', IntToStr(GetCurrentProcessId));
  if AString.Contains('%THREAD_ID%') then
    ExpandMacros(AString, '%THREAD_ID%', IntToStr(GetCurrentThreadId));
  {$ENDIF}
end;

{ TEFEnvironmentVariableMacroExpander }

procedure TEFEnvironmentVariableMacroExpander.InternalExpand(var AString: string);
begin
  inherited InternalExpand(AString);
  { TODO : Implement for linux? }
  ExpandEnvironmentVariables(AString);
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

procedure TEFCmdLineParamMacroExpander.InternalExpand(var AString: string);
var
  LParamIndex: Integer;
begin
  inherited InternalExpand(AString);
  for LParamIndex := 0 to FParams.Count - 1 do
    ExpandMacros(AString, '%Cmd:' + FParams.Names[LParamIndex] + '%',
      FParams.ValueFromIndex[LParamIndex]);
end;

{ TEFDateTimeStrMacroExpander }

procedure TEFDateTimeStrMacroExpander.InternalExpand(var AString: string);
begin
  inherited InternalExpand(AString);
  if AString.Contains('%YYYYMMDD_DATE%') then
    ExpandMacros(AString, '%YYYYMMDD_DATE%', FormatDateTime('yyyymmdd', Date));
  if AString.Contains('%YYYYMMDD_YESTERDAY%') then
    ExpandMacros(AString, '%YYYYMMDD_YESTERDAY%', FormatDateTime('yyyymmdd', Date - 1));
  if AString.Contains('%YYYYMMDD_TOMORROW%') then
    ExpandMacros(AString, '%YYYYMMDD_TOMORROW%', FormatDateTime('yyyymmdd', Date + 1));
  if AString.Contains('%YYMMDD_DATE%') then
    ExpandMacros(AString, '%YYMMDD_DATE%', FormatDateTime('yymmdd', Date));
  if AString.Contains('%YYMMDD_YESTERDAY%') then
    ExpandMacros(AString, '%YYMMDD_YESTERDAY%', FormatDateTime('yymmdd', Date - 1));
  if AString.Contains('%YYMMDD_TOMORROW%') then
    ExpandMacros(AString, '%YYMMDD_TOMORROW%', FormatDateTime('yymmdd', Date + 1));
  if AString.Contains('%MMDD_DATE%') then
    ExpandMacros(AString, '%MMDD_DATE%', FormatDateTime('mmdd', Date));
  if AString.Contains('%DD_DATE%') then
    ExpandMacros(AString, '%DD_DATE%', FormatDateTime('dd', Date));
  if AString.Contains('%MM_DATE%') then
    ExpandMacros(AString, '%MM_DATE%', FormatDateTime('mm', Date));
  if AString.Contains('%YYYY_DATE%') then
    ExpandMacros(AString, '%YYYY_DATE%', FormatDateTime('yyyy', Date));
  if AString.Contains('%WEEKDAYNAME_SHORT%') then
    ExpandMacros(AString, '%WEEKDAYNAME_SHORT%', FormatDateTime('ddd', Date));
  if AString.Contains('%WEEKDAYNAME_LONG%') then
    ExpandMacros(AString, '%WEEKDAYNAME_LONG%', FormatDateTime('dddd', Date));
  if AString.Contains('%WEEKDAYNR%') then
    ExpandMacros(AString, '%WEEKDAYNR%', IntToStr(DayOfWeek(Date)));
  if AString.Contains('%ISOWEEKDAYNR%') then
    ExpandMacros(AString, '%ISOWEEKDAYNR%', IntToStr(DayOfTheWeek(Date)));
  if AString.Contains('%HHMMSS_TIME%') then
    ExpandMacros(AString, '%HHMMSS_TIME%', FormatDateTime('hhnnss', Now));
  if AString.Contains('%HHMM_TIME%') then
    ExpandMacros(AString, '%HHMM_TIME%', FormatDateTime('hhnn', Now));
  if AString.Contains('%HH_TIME%') then
    ExpandMacros(AString, '%HH_TIME%', FormatDateTime('hh', Now));
  if AString.Contains('%MM_TIME%') then
    ExpandMacros(AString, '%MM_TIME%', FormatDateTime('nn', Now));
  if AString.Contains('%SS_TIME%') then
    ExpandMacros(AString, '%SS_TIME%', FormatDateTime('ss', Now));
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
        Result := TextFileToString(LFileName);
        TEFMacroExpansionEngine.Instance.Expand(Result);
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

procedure TEFGUIDMacroExpander.InternalExpand(var AString: string);
begin
  inherited InternalExpand(AString);
  if AString.Contains('GUID%') then
  begin
    ExpandMacros(AString, '%GUID%', CreateGuidStr);
    ExpandMacros(AString, '%COMPACT_GUID%', CreateCompactGuidStr);
  end;
end;

{ TEFEntityMacroExpander }

procedure TEFEntityMacroExpander.InternalExpand(var AString: string);
begin
  inherited InternalExpand(AString);
  ExpandMacros(AString, '%TAB%', #9);
  ExpandMacros(AString, '%SPACE', ' ');
end;

{ TEFParameterizedMacroExpanderBase }

procedure TEFParameterizedMacroExpanderBase.InternalExpand(var AString: string);
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
  LRest: string;
begin
  inherited InternalExpand(AString);

  LMacroNames := GetMacroNames;
  for LMacroName in LMacroNames do
  begin
    LMacroHead := '%' + LMacroName + '(';

    LPosHead := Pos(LMacroHead, AString);
    if LPosHead > 0 then
    begin
      LPosTail := PosEx(MACRO_TAIL, AString, LPosHead + 1);
      if LPosTail > 0 then
      begin
        LParams := TStringList.Create;
        try
          LParams.CommaText := Copy(AString, LPosHead + Length(LMacroHead),
            LPosTail - (LPosHead + Length(LMacroHead)));
          LParamsArray := LParams.ToStringArray;
          for I := 0 to High(LParamsArray) do
            TEFMacroExpansionEngine.Instance.Expand(LParamsArray[I]);
          LRest := Copy(AString, LPosTail + Length(MACRO_TAIL), MaxInt);
          InternalExpand(LRest);
          { TODO : Can be optimized by using Insert/Delete instead of concatenation/assignment. }
          AString := Copy(AString, 1, LPosHead - 1) + ExpandParameterizedMacro(LMacroName, LParamsArray) + LRest;
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

{ TEFFileNameToUrlMacroExpander }

function TEFFileNameToUrlMacroExpander.ExpandParam(const AFileName: string): string;
begin
  Assert(AFileName <> '');
  Result := 'file:///'+ReplaceStr(AFileName, '\', '/');
end;

function TEFFileNameToUrlMacroExpander.ExpandParameterizedMacro(
  const AMacroName: string; const AParams: TArray<string>): string;
var
  LFileName: string;
begin
  if SameText(AMacroName, 'FILENAME_TO_URL') then
  begin
    if Length(AParams) > 0 then
    begin
      LFileName := AParams[0];
      Result := ExpandParam(LFileName);
    end
    else
      Result := '';
  end
  else
    Result := inherited ExpandParameterizedMacro(AMAcroName, AParams);
end;

function TEFFileNameToUrlMacroExpander.GetMacroNames: TArray<string>;
begin
  SetLength(Result, 1);
  Result[0] := 'FILENAME_TO_URL';
end;

end.
