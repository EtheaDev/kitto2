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
unit KIDE.HelpViewer;

interface

uses
  Classes;

procedure ShowKittoWiki(const AHelpContext: string);
procedure ShowKideHelp(const AHelpContext: Integer);

implementation

uses
  HelpIntfs, SysUtils, Windows, ShellAPI;

type
  TKideHelpViewer = class(TInterfacedObject, ICustomHelpViewer)
  private
    FViewerID: Integer;
    { fields used to handle popup configuration }
    FTopCenter: TPoint;
    FForeground: TColorRef;
    FBackground: TColorRef;
    FMargins: TRect;
    FFontDesc: string;
    FPopupResourceHandle: HWND;
    FPopupResourceID: LongInt;
    FPopupText: string;
    FInitialized: Boolean;
    FHelpManager: IHelpManager;
    procedure ValidateHelpViewer;
{$IF DEFINED(CLR)}
  strict protected
    procedure Finalize; override;
{$IFEND}
  public
    constructor Create;
    destructor Destroy; override;
    { internal support functions }
    function GetHelpFile(const Name: string): string;
    procedure InternalShutDown;
    procedure SynchTopic(Handle: HWND; const HelpFileName: string);
{$IF DEFINED(CLR)}
    procedure LookupALink(Handle: HWND; const HelpFileName: string;
      LinkPtr: HH_AkLink); overload;
    procedure LookupALink(Handle: HWND; const HelpFileName: string;
      LinkPtr: DWORD); overload;
    procedure LookupKeyword(Handle: HWND; const HelpFileName: string;
      LinkPtr: HH_AKLink); overload;
    procedure LookupKeyword(Handle: HWND; const HelpFileName: string;
      LinkPtr: DWORD); overload;
    procedure DisplayTextPopup(Handle: HWND; Data: HH_Popup); overload;
    procedure DisplayTextPopup(Handle: HWND; Data: LongInt); overload;
{$ELSE}
    procedure LookupALink(Handle: HWND; const HelpFileName: string;
      LinkPtr: PHH_AkLink);
    procedure LookupKeyword(Handle: HWND; const HelpFileName: string;
      LinkPtr: PHH_AKLink);
    procedure DisplayTextPopup(Handle: HWND; Data: PHH_Popup);
{$IFEND}
    { ICustomHelpViewer }
    function GetViewerName : string;
    function UnderstandsKeyword(const HelpString: string): Integer;
    function GetHelpStrings(const HelpString: string): TStringList;
    function CanShowTableOfContents: Boolean;
    procedure ShowTableOfContents;
    procedure ShowHelp(const HelpString: string);
    procedure NotifyID(const ViewerID: Integer);
    procedure SoftShutDown;
    procedure ShutDown;
    { IExtendedHelpViewer }
    function UnderstandsTopic(const Topic: string): Boolean;
    procedure DisplayTopic(const Topic: string); overload;
    function UnderstandsContext(const ContextID: Integer;
      const HelpFileName: string): Boolean;
    procedure DisplayHelpByContext(const ContextId: Integer;
      const HelpFileName: string);
    { ISpecialHtmlHelpViewer }
    function CallHtmlHelp(Handle: HWND; HelpFileName: string;
      Command: Integer; Data: LongInt): HWND;
    { IExtendedHelpViewer2 }
    procedure DisplaySearch(const Topic: string);
    procedure DisplayIndex(const Topic: string);
    procedure DisplayTopic(const Topic, Anchor: string); overload;
    { IPopupHelp }
    procedure SetupPopupWindow(TopCenter: TPoint; Foreground, Background: DWord;
      Margins: TRect; const FontDesc: string);
    procedure SetupPopupSource(Handle: HWND; ID: LongInt; const Text: string);
    procedure Popup(KeepWindowSetup: Boolean; KeepSourceSetup: Boolean);
    procedure ClearSetup;
    { ISpecialWinHelpViewer }
    function CallWinHelp(Handle: THandle; const HelpFileName: string;
      Command: Word; Data: NativeUInt) : Boolean;
    { properties }
    property ViewerID : Integer read FViewerID;
    property HelpManager : IHelpManager read FHelpManager write FHelpManager;
    property TopCenter: TPoint read FTopCenter write FTopCenter;
    property Foreground: TColorRef read FForeground write FForeground;
    property Background: TColorRef read FBackground write FBackground;
    property Margins: TRect read FMargins write FMargins;
    property FontDesc: string read FFontDesc write FFontDesc;
    property PopupResourceHandle: HWND read FPopupResourceHandle write FPopupResourceHandle;
    property PopupResourceId: LongInt read FPopupResourceId write FPopupResourceId;
    property PopupText: string read FPopupText write FPopupText;
  end;

var
  KideHelpViewer: TKideHelpViewer;
  HelpViewerIntf: ICustomHelpViewer;
{$IF DEFINED(CLR)}
  FInitializedCookie: DWORD_PTR;
{$ELSE}
  FInitializedCookie: LongInt;
{$IFEND}

procedure ShowKittoWiki(const AHelpContext: string);
begin
  KideHelpViewer.ShowHelp(AHelpContext);
end;

procedure ShowKideHelp(const AHelpContext: Integer);
begin
  KideHelpViewer.DisplayHelpByContext(AHelpContext, '');
end;

function KideHelpHandle(Handle: HWND; HelpFileName: string; Command: Integer; Data: LongInt): HWND;
begin
  Result := 0;
end;

procedure TKideHelpViewer.ValidateHelpViewer;
begin
  if not FInitialized then
  begin
{$IF DEFINED(CLR)}
    KideHelpHandle(0, '', HH_INITIALIZE, FInitializedCookie);
{$ELSE}
    KideHelpHandle(0, '', HH_INITIALIZE, DWORD_PTR(@FInitializedCookie));
{$IFEND}
    FInitialized := True;
  end;
end;

{ ICustomHelpViewer. }

{ GetViewerName returns a string that the Help Manager can use to identify
  this Viewer in a UI element asking users to choose among Viewers. }
function TKideHelpViewer.GetViewerName: string;
begin
  Result := 'KIDE2';
end;

{ UnderstandsKeyword is a querying function that the Help Manager calls to
  determine if the Viewer provide helps on a particular keyword string. }
function TKideHelpViewer.UnderstandsKeyword(const HelpString: string): Integer;
var
  HelpFile: string;
begin
  HelpFile := GetHelpFile('');
  Result := 1;
end;

{ GetHelpStrings is used by the Help Manager to display a list of keyword
  matches from which an application's user can select one. It assumes
  that the String List is properly allocated, so this function should
  never return nil. }

function TKideHelpViewer.GetHelpStrings(const HelpString: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Add(GetViewerName + ': ' + HelpString);
end;

{ CanShowTableOfContents is a querying function that the Help Manager
  calls to determine if the Viewer supports tables of contents. HtmlHelp does. }

function TKideHelpViewer.CanShowTableOfContents: Boolean;
begin
  Result := True;
end;

{ ShowTableOfContents is a command function that the Help Manager uses
  to direct the Viewer to display a table of contents. It is never
  called without being preceded by a call to CanShowTableOfContents. }
procedure TKideHelpViewer.ShowTableOfContents;
var
  FileName : string;
  Handle: HWND;
begin
  ValidateHelpViewer;
  FileName := GetHelpFile('');
  Handle := HelpManager.GetHandle;
  SynchTopic(Handle, FileName);
  KideHelpHandle(Handle, FileName, HH_DISPLAY_TOPIC, 0);
end;

procedure TKideHelpViewer.ShowHelp(const HelpString: string);
var
  Link: string;
begin
  ValidateHelpViewer;
  Link := GetHelpFile('')+HelpString;
  ShellExecute(0, 'open' , PChar(Link), nil, nil, SW_SHOW );
end;

{ NotifyID is called by the Help Manager after a successful registration
  to provide the Help Viewer with a cookie which uniquely identifies the
  Viewer to the Manager, and can be used in communications between the two. }

procedure TKideHelpViewer.NotifyId(const ViewerId: Integer);
begin
  FViewerID := ViewerID;
end;

{ SoftShutDown is called by the help manager to ask the viewer to
  terminate any externally spawned subsystem without shutting itself down. }
procedure TKideHelpViewer.SoftShutDown;
begin
  if FInitialized then
    KideHelpHandle(0, '', HH_CLOSE_ALL, 0);
end;

procedure TKideHelpViewer.ShutDown;
begin
  SoftShutDown;
  if FInitialized then
  begin
{$IF DEFINED(CLR)}
    KideHelpHandle(0, '', HH_UNINITIALIZE, FInitializedCookie);
{$ELSE}
    KideHelpHandle(0, '', HH_UNINITIALIZE, DWORD_PTR(@FInitializedCookie));
{$IFEND}
    FInitialized := false;
    FInitializedCookie := 0;
  end;
  if Assigned(FHelpManager) then HelpManager := nil;
end;

{ IExtendedHelpViewer }

{ UnderstandsTopic is called by the Help Manager to ask if the Viewer
  is capable of displaying a topic-based help query for a given topic.
  It's default behavior is to say 'yes'. }

function TKideHelpViewer.UnderstandsTopic(const Topic: string): Boolean;
var
  HelpFile: string;
begin;
  HelpFile := GetHelpFile('');
  Result := True;
end;

{ DisplayTopic is called by the Help Manager if a Help Viewer claims
  in its response to UnderstandsTopic to be able to provide Topic-based
  help for a particular keyword. }

procedure TKideHelpViewer.DisplayTopic(const Topic: string);
const
  InvokeSep = '::/';
  InvokeSuf = '.htm';
var
  HelpFile: string;
  InvocationString: string;
begin
  ValidateHelpViewer;
  HelpFile := GetHelpFile('');
  InvocationString := HelpFile + InvokeSep + Topic + InvokeSuf;
  KideHelpHandle(HelpManager.GetHandle, InvocationString, HH_DISPLAY_TOPIC, 0);
end;

{ UnderstandsContext is a querying function called by the Help Manager
  to determine if an Extended Help Viewer is capable of providing
  help for a particular context-ID. Like all querying functions in
  this file, the default behavior is to say 'yes' unless overridden by
  a Tester. }

function TKideHelpViewer.UnderstandsContext(const ContextId: Integer;
                                            const HelpFileName: string): Boolean;
begin
  Result := True;
end;

{ DisplayHelpByContext is used by the Help Manager to request that a
  Help Viewer display help for a particular Context-ID. It is only
  invoked after a successful call to CanShowContext. }

procedure TKideHelpViewer.DisplayHelpByContext(const ContextId: Integer;
  const HelpFileName: string);
var
  Handle: HWND;
  FileName: string;
  Link: string;
begin
  ValidateHelpViewer;
  if ContextId = 100 then
    Link := 'Index'
  else
    Link := 'hs'+IntToStr(ContextId);

  Link := Format('http://www.ethea.it/docs/kitto/en/Kide2/Help/%s.htm',
    [Link]);
  ShellExecute(0, 'open' , PChar(Link), nil, nil, SW_SHOW );
end;


{ ISpecialWinHelpViewer }

{ This function reveals a design flaw in the D6/7 help system. :(
  ISpecialWinHelpViewer.CallWinHelp is intended to allow third-party
  help systems to process WinHelp messages which were not generalizable
  by the system designer. In this case, the expected behavior would be
  to convert WinHelp messages into HtmlHelp messages and forward them
  along.

  However, the same necessity that compels developers to be able to send
  non-generalizable winhelp messages also compels the ability to send
  non-generalizable htmlhelp messages. There is no mechanism in the
  existing architecture to do that.

  The function signature for WinHelp() and HtmlHelp() are sufficiently
  similar, however, to allow the function to multiplex. Depending on the
  answer provided by an implementation of IHtmlHelpTester, the function
  will either convert messages (under the assumption that they are
  WinHelp messages) or pass them through (under the assumption that they
  are HtmlHelp messages).

  The need for this should be resolved in the next revision of the RTL, but
  this function will continue to behave that way for purposes of backwards
  compatability. }

function TKideHelpViewer.CallWinHelp(Handle: THandle; const HelpFileName: string; Command: Word; Data: NativeUInt): Boolean;
var
  Converted : Boolean;
  FileName: string;
begin
  ValidateHelpViewer;
  Result := false;
  FileName := GetHelpFile(HelpFileName);
  if FileName <> '' then
  begin
    Converted := False;

    if not Converted then
      // Before giving up, lets try to translate some basic
      // WinHelp commands to their HtmlHelp equivalents
      case Command of
        HELP_CONTEXT: Command := HH_HELP_CONTEXT;
        HELP_QUIT: Command := HH_CLOSE_ALL;
        HELP_INDEX: Command := HH_DISPLAY_INDEX;
        HELP_HELPONHELP: Command := HH_DISPLAY_INDEX;
        HELP_FINDER: Command := HH_DISPLAY_TOC;
      else
        Exit;
      end;

    CallHtmlHelp(Handle, FileName, Command, Data);
    Result := True;
  end;
end;

{ ISpecialHtmlHelpViewer }

function TKideHelpViewer.CallHtmlHelp(Handle: HWND; HelpFileName: string;
  Command: Integer; Data: LongInt): HWND;
begin
  ValidateHelpViewer;
  Result := 0;
  case Command of
    HH_CLOSE_ALL:
    begin
      SoftShutDown;
    end;
    HH_DISPLAY_TEXT_POPUP:
    begin
{$IF DEFINED(CLR)}
      DisplayTextPopup(Handle, Data);
{$ELSE}
      DisplayTextPopup(Handle, PHH_Popup(Data));
{$IFEND}
    end;
    HH_HELP_CONTEXT:
    begin
      DisplayHelpByContext(Data, HelpFileName);
    end;
    HH_ALINK_LOOKUP:
    begin
{$IF DEFINED(CLR)}
      LookupALink(Handle, HelpFileName, Data);
{$ELSE}
      LookupALink(Handle, HelpFileName, PHH_AkLink(Data));
{$IFEND}
    end;
    { DisplayIndex, DisplaySearch, DisplayToc }
  else
    begin
      SynchTopic(Handle, HelpFileName);
      Result := KideHelpHandle(Handle, HelpFileName, Command, Data);
    end;
  end;
end;

{ IExtendedHelpViewer2 }

procedure TKideHelpViewer.DisplayIndex(const Topic: string);
var
  HelpFile: string;
begin
  ValidateHelpViewer;
  HelpFile := GetHelpFile('');
  KideHelpHandle(HelpManager.GetHandle, HelpFile, HH_DISPLAY_INDEX, StrToInt(Topic));
end;

procedure TKideHelpViewer.DisplaySearch(const Topic: string);
var
  HelpFile: string;
  Query: HH_FTS_QUERY;
begin
  ValidateHelpViewer;
  HelpFile := GetHelpFile('');

{$IF DEFINED(CLR)}
  Query.cbStruct := Marshal.SizeOf(TypeOf(Query));
  Query.pszSearchQuery := Topic;
{$ELSE}
  FillChar(Query, SizeOf(Query), 0);
  Query.cbStruct := SizeOf(Query);
  Query.pszSearchQuery := PChar(Topic);
{$IFEND}
  Query.fUniCodeStrings := True;
  Query.iProximity := -1;
  Query.fStemmedSearch := False;
  Query.fTitleOnly := False;
  Query.fExecute := True;
  Query.pszWindow := nil;

  KideHelpHandle(HelpManager.GetHandle, HelpFile, HH_DISPLAY_SEARCH, 0);
end;

procedure TKideHelpViewer.DisplayTopic(const Topic, Anchor: string);
const
  InvokeSep = '::/';
  InvokeSuf = '.htm';
  AnchorSep = '#';
var
  HelpFile: string;
  InvocationString: string;
begin
  ValidateHelpViewer;
  HelpFile := GetHelpFile('');
  InvocationString := HelpFile + InvokeSep + Topic + InvokeSuf + AnchorSep + Anchor;
  KideHelpHandle(HelpManager.GetHandle, InvocationString, HH_DISPLAY_TOPIC, 0);
end;

{ IPopupHelp }

procedure TKideHelpViewer.SetupPopupWindow(TopCenter: TPoint;
                                           Foreground, Background: DWord; 
                                           Margins: TRect; 
                                           const FontDesc: string);
begin
  Self.TopCenter := TopCenter;
  Self.Foreground := Foreground;
  Self.Background := Background;
  Self.Margins := Margins;
  Self.FontDesc := FontDesc;
end;

procedure TKideHelpViewer.SetupPopupSource(Handle: HWND; ID: LongInt;
                                           const Text: string);
begin
 Self.PopupResourceHandle := Handle;
 Self.PopupResourceId := ID;
 Self.PopupText := Text;
end;

procedure TKideHelpViewer.Popup(KeepWindowSetup: Boolean;
                                KeepSourceSetup: Boolean);
begin
  { execute popup call }
  if not KeepWindowSetup then
  begin
  end;
  if not KeepSourceSetup then
  begin
  end;
end;

procedure TKideHelpViewer.ClearSetup;
begin

end;

{==========================================================================}

procedure TKideHelpViewer.SynchTopic(Handle: HWND; const HelpFileName: string);
begin
  KideHelpHandle(Handle, HelpFileName, HH_DISPLAY_TOPIC, 0);
end;

{$IF DEFINED(CLR)}
procedure TKideHelpViewer.LookupALink(Handle: HWND;
  const HelpFileName: string; LinkPtr: HH_AkLink);
begin
  KideHelpHandle(Handle, HelpFileName, HH_ALINK_LOOKUP, LinkPtr);
end;

procedure TKideHelpViewer.LookupALink(Handle: HWND;
  const HelpFileName: string; LinkPtr: DWORD);
begin
  KideHelpHandle(Handle, HelpFileName, HH_ALINK_LOOKUP, LinkPtr);
end;

procedure TKideHelpViewer.LookupKeyword(Handle: HWND;
  const HelpFileName: string; LinkPtr: HH_AkLink);
begin
  KideHelpHandle(Handle, HelpFileName, HH_KEYWORD_LOOKUP, LinkPtr);
end;

procedure TKideHelpViewer.LookupKeyword(Handle: HWND;
  const HelpFileName: string; LinkPtr: DWORD);
begin
  KideHelpHandle(Handle, HelpFileName, HH_KEYWORD_LOOKUP, LinkPtr);
end;

procedure TKideHelpViewer.DisplayTextPopup(Handle: HWND; Data: HH_Popup);
var
  FileName: string;
begin
  KideHelpHandle(Handle, FileName, HH_DISPLAY_TEXT_POPUP, Data);
end;

procedure TKideHelpViewer.DisplayTextPopup(Handle: HWND; Data: LongInt);
var
  FileName: string;
begin
  KideHelpHandle(Handle, FileName, HH_DISPLAY_TEXT_POPUP, Data);
end;
{$ELSE}
procedure TKideHelpViewer.LookupALink(Handle: HWND;
  const HelpFileName: string; LinkPtr: PHH_AkLink);
begin
  KideHelpHandle(Handle, HelpFileName, HH_ALINK_LOOKUP, DWORD_PTR(LinkPtr));
end;

procedure TKideHelpViewer.LookupKeyword(Handle: HWND;
  const HelpFileName: string; LinkPtr: PHH_AkLink);
begin
  KideHelpHandle(Handle, HelpFileName, HH_KEYWORD_LOOKUP, DWORD_PTR(LinkPtr));
end;

procedure TKideHelpViewer.DisplayTextPopup(Handle: HWND; Data: PHH_Popup);
var
  FileName: string;
begin
  KideHelpHandle(Handle, FileName, HH_DISPLAY_TEXT_POPUP, DWORD_PTR(Data));
end;
{$IFEND}

constructor TKideHelpViewer.Create;
begin
  inherited Create;
  HelpViewerIntf := Self;
  FInitialized := False;
  FInitializedCookie := 0;
  ClearSetup;
end;

destructor TKideHelpViewer.Destroy;
begin
  KideHelpViewer := nil;
  inherited Destroy;
end;

function TKideHelpViewer.GetHelpFile(const Name: string): string;
var
  FileName: string;
begin
  Result := '';
  if (Name = '') and Assigned(FHelpManager) then
    FileName := HelpManager.GetHelpFile
  else
    FileName := Name;
  if FileName = '' then
    FileName := 'https://github.com/EtheaDev/kitto2/wiki/';

  Result := FileName;
end;

procedure TKideHelpViewer.InternalShutDown;
begin
  SoftShutDown;
  if Assigned(FHelpManager) then
  begin
    HelpManager.Release(ViewerID);
    HelpManager := nil;
  end;
end;

{$IF DEFINED(CLR)}
procedure TKideHelpViewer.Finalize;
begin
  InternalShutDown;
end;
{$IFEND}

initialization
  KideHelpViewer := TKideHelpViewer.Create;
  RegisterViewer(HelpViewerIntf, KideHelpViewer.FHelpManager);

{$IF NOT DEFINED(CLR)}
finalization
  if Assigned(KideHelpViewer.HelpManager) then
    KideHelpViewer.InternalShutDown;
  HelpViewerIntf := nil;
{$IFEND}
  
end.
