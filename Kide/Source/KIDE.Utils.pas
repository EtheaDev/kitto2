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
unit KIDE.Utils;

interface

uses
  Classes, Controls, Dialogs, Graphics, ExtCtrls,
  SynEdit, SynEditHighlighter, SynEditOptionsDialog,
  Ext.Base, EF.Tree, Kitto.Web.Application;

type
  TExtraInfo = (eiOriginalFileName, eiProductName, eiInternalName, eiProductVersion, eiFileVersion, eiFileDescription, eiCompanyName, eiLegalCopyright, eiLegalTradeMarks, eiComments, eiCompanyURL, eiProductURL, eiEmailSupport);

const
  AExtraInfoStr: array[TExtraInfo] of string = ('OriginalFileName', 'ProductName', 'InternalName', 'ProductVersion', 'FileVersion', 'FileDescription', 'CompanyName', 'LegalCopyright', 'LegalTradeMarks', 'Comments', 'CompanyURL', 'ProductURL', 'Email-Support');

function ExtractLocaleNameFromFileName(const AFileName: string): string;

procedure NotImplemented;

function GetKIDEVersion: string;

procedure ShowImage(const AImageName: string; AImage: TImage);
procedure ShowViewImage(const AViewName: string; AImage: TImage);

function CreateSynEditor(AContainer: TComponent;
  AParent: TWinControl;
  AName: string;
  ASynHighlighter: TSynCustomHighlighter;
  AFontSize: Integer = 0;
  OnChange: TNotifyEvent = nil) : TSynEdit;

function GetHighLighterSettingsFileName(AEditor: TSynEdit): string;

procedure SynEditorRestoreOptions(AEditor: TSynEdit);

procedure GetVerInfo( const FileName : string;
  var MajorVersion, MinorVersion, Release, Build : integer;
  ExtraInfo : TStringList = nil);

function GetConfigMetadataFileName(const ANode: TEFTree): string;

/// <summary>Returns path for Special Window Folders like user documents.
/// ACSIDL is a constant defined in ShlObj like CSIDL_PERSONAL.</summary>
function GetSpecialFolder(const ACSIDL: Integer) : string;

function GetMetadataNodeFileName(const ANode: TEFTree): string;
function IsLoginViewNode(const ANode: TEFTree): Boolean;
function IsSubViewNode(const ANode: TEFNode): Boolean;
function IsDatabaseNode(const ANode: TEFNode): Boolean;
function GetControllerClass(const AControllerNode: TEFNode): TClass;
function GetSubControllerClass(ANode: TEFNode): TClass;
function GetRegionName(const ABorderPanelControllerClassName: string;
  const ARegion: string): string;
function GetControllerClassName(const AControllerNode: TEFNode): string;

implementation

uses
  SysUtils, StrUtils, Windows, System.UITypes, TypInfo,
  Themes, ShlObj,
  EF.Rtti, EF.Classes, EF.DB,
  KIDE.Project, KIDE.MRUOptions, KIDE.Config,
  KIDE.LoginWindowControllerDesignerFrameUnit,
  Kitto.Metadata.Views, Kitto.Ext.List, Kitto.Ext.Base, Kitto.JS.Controller,
  EF.Macros, EF.StrUtils, EF.Localization;

function GetHighLighterSettingsFileName(AEditor: TSynEdit): string;
begin
  Result := '%APPDATA%\KIDE\'+
    TStyleManager.ActiveStyle.Name + '_' + AEditor.Highlighter.LanguageName + '.ini';
  TEFMacroExpansionEngine.Instance.Expand(Result);
end;

procedure UpdateSynEditorStyle(AEditor: TSynEdit);
var
  LStyle: TCustomStyleServices;
begin
  LStyle := TStyleManager.ActiveStyle;
  if not LStyle.IsSystemStyle then
  begin
    AEditor.Color  := LStyle.GetStyleColor(scEdit);
    with AEditor.Gutter do
    begin
      GradientEndColor   := LStyle.GetSystemColor(clBtnFace);
      GradientStartColor := LStyle.GetSystemColor(clWindow);
      Font.Color         := LStyle.GetSystemColor(clWindowText);
      BorderColor        := LStyle.GetStyleColor(scSplitter);
    end;
    AEditor.ActiveLineColor :=LStyle.GetSystemColor(clHighlight);
  end;
end;

procedure SynEditorRestoreOptions(AEditor: TSynEdit);
var
  LOptions: TSynEditorOptionsContainer;
  LHighLighterSettingsFileName: string;
begin
  LOptions := TSynEditorOptionsContainer.Create(nil);
  try
    LOptions.Assign(AEditor);
    NodeToObject(TMRUOptions.Instance.FindNode('CodeEditor/Options/Yaml'), LOptions);
    AEditor.Assign(LOptions);
    LHighLighterSettingsFileName := GetHighLighterSettingsFileName(AEditor);
    if FileExists(LHighLighterSettingsFileName) then
      AEditor.Highlighter.LoadFromFile(LHighLighterSettingsFileName);
    //Styles compatibility
    UpdateSynEditorStyle(AEditor);
  finally
    FreeAndNil(LOptions);
  end;
end;

function ExtractLocaleNameFromFileName(const AFileName: string): string;
var
  P: Integer;
  LLastBackslash: Integer;
begin
  P := Pos('\LC_MESSAGES\', AFileName);
  if P > 0 then
  begin
    Result := Copy(AFileName, 1, P - 1);
    LLastBackslash := RightPos('\', Result);
    Result := Copy(Result, LLastBackslash + 1, MaxInt);
  end
  else
    Result := AFileName;
end;

procedure NotImplemented;
begin
  MessageDlg(_('Not yet implemented.'), mtInformation, [mbOK], 0);
end;

function GetBuildDateTimeAsString: string;
var
  LDateTime: TDateTime;
begin
  if FileAge(ParamStr(0), LDateTime) then
    Result := DateTimeToStr(LDateTime)
  else
    Result := _('unknown datetime');
end;

function GetKIDEVersion: string;
var
  LMajorVersion, LMinorVersion, LRelease, LBuild: Integer;
begin
  GetVerInfo(ParamStr(0), LMajorVersion, LMinorVersion, LRelease, LBuild);
  Result := Format('%d.%d.%d',[LMajorVersion, LMinorVersion, LRelease]);
end;

procedure ShowImage(const AImageName: string; AImage: TImage);
var
  LFileName: string;
  LApplication: string;
begin
  Assert(Assigned(TProject.CurrentProject.Application));
  if AImageName = '' then
    AImage.Picture.Bitmap := nil
  else
  begin
    LFileName := AImageName;
    TEFMacroExpansionEngine.Instance.Expand(LFileName);
    TProject.CurrentProject.Application.FindImagePathName(LFileName);
    if FileExists(LFileName) then
      AImage.Picture.LoadFromFile(LFileName)
    else
      AImage.Picture.Bitmap := nil;
  end;
end;

procedure ShowViewImage(const AViewName: string; AImage: TImage);
var
  LView: TKView;
  LViewName: string;
begin
  if AViewName = '' then
    AImage.Picture.Bitmap := nil
  else
  begin
    LViewName := AViewName;
    TEFMacroExpansionEngine.Instance.Expand(LViewName);
    LView := TProject.CurrentProject.Config.Views.FindView(LViewName);
    if Assigned(LView) then
      ShowImage(LView.ImageName, AImage)
    else
      AImage.Picture.Bitmap := nil;
  end;
end;

function CreateSynEditor(AContainer: TComponent;
  AParent: TWinControl;
  AName: string;
  ASynHighlighter: TSynCustomHighlighter;
  AFontSize: Integer = 0;
  OnChange: TNotifyEvent = nil) : TSynEdit;
begin
  Result := TSynEdit.Create(AContainer);
  Try
    Result.Name := AName;
    Result.Text := '';
    Result.Align := alClient;
    Result.Parent := AParent;
    Result.ActiveLineColor := clInfoBk;
    Result.Font.Name := 'Consolas';
    Result.TabOrder := 1;
    Result.Gutter.Visible := True;
    Result.Gutter.Font.Name := 'Consolas';
    Result.Highlighter := ASynHighlighter;
    Result.Options := [eoAutoIndent, eoDragDropEditing, eoEnhanceHomeKey, eoEnhanceEndKey,
      eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs,
      eoTabIndent, eoTabsToSpaces, eoTrimTrailingSpaces];
    Result.TabWidth := 2;
    if AFontSize <> 0 then
      Result.Gutter.Font.Size := AFontSize
    else
      Result.Gutter.Font.Size := 10;
    SynEditorRestoreOptions(Result);
    if AFontSize <> 0 then
      Result.Font.Size := AFontSize;
    Result.WantTabs := True; //Force always use of tab key into editor
    Result.OnChange := OnChange;
  Except
    Result.Free;
    raise;
  End;
end;

procedure GetVerInfo( const FileName : string;
  var MajorVersion, MinorVersion, Release, Build : integer;
  ExtraInfo : TStringList = nil);
type
  cArray   = Array[1..$3FFF] of Char;
  TLangInf = Array[1..2]     of Word;      // Language and charset identifiers

var
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
  i : TExtraInfo;
  VerInf : Record
    Case Integer of
         0: (p  : Pointer);
         1: (pc : pChar);
         2: (li : ^TLangInf);
    end;
  Len: DWORD;
  SubBlock: String;

begin
  MajorVersion := 0;
  MinorVersion := 0;
  Release := 0;
  Build := 0;

  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize > 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
      begin
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
        begin
          MajorVersion := HIWORD(FI.dwFileVersionMS);
          MinorVersion := LOWORD(FI.dwFileVersionMS);
          Release := HIWORD(FI.dwFileVersionLS);
          Build := LOWORD(FI.dwFileVersionLS);
        end;
        if Assigned(ExtraInfo) then
        begin
          if VerQueryValue(VerBuf, PChar('\VarFileInfo\Translation'), VerInf.p, Len) then
            SubBlock := Format('StringFileInfo\%s%s\', [IntToHex(VerInf.li^[1],4), IntToHex(VerInf.li^[2],4)])
          else
            SubBlock := 'StringFileInfo\040904E4\';  // Language and charset not found, try defaults
          for i := low(TExtraInfo) to High(TExtraInfo) do
          if VerQueryValue(VerBuf, PChar(SubBlock + AExtraInfoStr[i]), VerInf.p, Len) then
            ExtraInfo.Add(VerInf.pc)
          else
            ExtraInfo.Add('');
        end;
      end;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

function GetConfigMetadataFileName(const ANode: TEFTree): string;
var
  LNode: TEFNode;
  LCOntrollerClass: TClass;
  LEFDBAdapter: TEFDBAdapter;
  LParentFileName, LSuffixName: string;
begin
  Result := '';
  if ANode is TEFPersistentTree then
    Result := TEFPersistentTree(ANode).ClassName
  else if ANode is TEFNode then
  begin
    LNode := TEFNode(ANode);
    Result := LNode.ClassName;
    if SameText(Result, 'TKModelField') then
    begin
      if Assigned(LNode.Parent) and (LNode.Parent is TEFNode) and
        SameText(TEfNode(LNode.Parent).Name, 'fields') then
        Result := 'TKModelReferencedField';
    end
    else if SameText(Result, 'TEFNode') then
    begin
      if SameText('Controller', RightStr(LNode.Name, 10)) then
      begin
        //Controller Node: use the class node
        LControllerClass := GetControllerClass(LNode);
        if Assigned(LControllerClass) then
        begin
          Result := LCOntrollerClass.ClassName;
          Exit;
        end;
      end
      else if IsDatabaseNode(LNode) then
      begin
        LEFDBAdapter := TEFDBAdapterRegistry.Instance.DBAdapters[LNode.AsString];
        Result := LEFDBAdapter.ClassName;
        Exit;
      end
      else
      begin
        LControllerClass := GetSubControllerClass(LNode);
        if Assigned(LControllerClass) and LControllerClass.InheritsFrom(TKExtToolController) then
        begin
          Result := 'TKViewTable_Controller_ToolViews_'+
            StringReplace(LControllerClass.ClassName,'Controller','',[]);
          Exit;
        end;
      end;
      LParentFileName := GetConfigMetadataFileName(LNode.Parent);
      if SameText('TKLayout', Copy(LParentFileName,1,8)) then
      begin
        //For Layout nested elements, only last element is used
        Result := 'TKLayout'+'_'+LNode.Name;
        Exit;
      end;
      if SameText('TKExtLoginWindow', LParentFileName) then
      begin
        if SameText('BorderPanel', LNode.Name) then
        begin
          //A BorderPanel node of Login Window is a BorderPanelController
          Result := 'TKExtBorderPanelController';
          Exit;
        end;
        if SameText('FormPanel', LNode.Name) then
        begin
          //A FormPanel node of Login Window is a BorderPanelController
          Result := 'TKExtFormPanel';
          Exit;
        end;
      end;
      if IsSubViewNode(LNode) then
      begin
        //For a SubView element, uses the generic TKView template
        Result := 'TKView';
        Exit;
      end;
      Result := LParentFileName+'_'+LNode.Name;
    end
    else if SameText(Result, 'TKTreeViewNode') then
    begin
      if MatchText(LNode.AsString, ['Build_AutoList', 'Build_AutoForm']) then
        Result := 'TKViewBuilder'
      else
        Result := 'TKView';
    end;
  end;
end;

function GetSpecialFolder(const ACSIDL: Integer) : string;
var
  RecPath : PWideChar;
begin
  RecPath := StrAlloc(MAX_PATH);
    try
    FillChar(RecPath^, MAX_PATH, 0);
    if SHGetSpecialFolderPath(0, RecPath, ACSIDL, false)
      then result := RecPath
      else result := '';
    finally
      StrDispose(RecPath);
    end;
end;

function GetMetadataNodeFileName(const ANode: TEFTree): string;
begin
  Result := TKideConfig.Instance.MetadataTemplatePath + GetConfigMetaDataFileName(ANode)+'.yaml';
end;

function IsSubViewNode(const ANode: TEFNode): Boolean;
begin
  Result := False;
  if MatchText(ANode.Name, ['SubView', 'CenterView', 'WestView', 'NorthView', 'EastView', 'SouthView']) then
    Result := (ANode.Parent is TEFNode) and SameText(TEFNode(ANode.Parent).Name, 'Controller') //subview or Subnodes of TreeView
  else if SameText(ANode.Name, 'View') then
    Result := Assigned(ANode.FindNode('Controller')) or //View Node of TreeView with embedded Controller
    (ANode.Parent is TEFNode) and SameText(TEFNode(ANode.Parent).Name, 'Folder') or //View Node of Folder of Tree
    (ANode.Parent is TEFNode) and SameText(TEFNode(ANode.Parent).Name, 'View'); //View Node of View of Tree
end;

function IsLoginViewNode(const ANode: TEFTree): Boolean;
var
  LoginNodeValue: string;
begin
  Result := False;
  if (ANode is TKView) then
  begin
    LoginNodeValue := TProject.CurrentProject.Config.Config.GetString('Login');
    Result := SameText(TKView(ANode).PersistentName, 'Login') or
      SameText(TKView(ANode).PersistentName, LoginNodeValue);
  end
  else if (ANode is TEFNode) then
    Result := SameText(TEFNode(ANode).Name, 'Login') and (TEFNode(ANode).Parent is TEFComponentConfig);
end;

function IsDatabaseNode(const ANode: TEFNode): Boolean;
begin
  Result := (ANode.Parent is TEFNode) and SameText(TEFNode(ANode.Parent).Name, 'Databases') and
    MatchText(ANode.Value, ['ADO', 'FD', 'DBX']);
end;

function GetRegionName(const ABorderPanelControllerClassName: string;
  const ARegion: string): string;
begin
  Result := StripPrefix(ARegion, 'rg');
end;

function GetControllerClassName(const AControllerNode: TEFNode): string;
var
  LClass: TClass;
  LNode: TEFNode;
begin
  Assert(Assigned(AControllerNode));
  if AControllerNode is TEFNode then
  begin
    LNode := TEFNode(AControllerNode);
    Result := (LNode.AsString);
    if Result = '' then
    begin
      //Controller Nodes without value: use default
      if SameText(LNode.Name, 'CenterController') and (LNode.AsString = '') then
        Result := 'GridPanel'
      else if Assigned(LNode.Parent) and (LNode.Parent is TEFNode) then
      begin
        LClass := GetControllerClass(TEFNode(LNode.Parent));
        if Assigned(LClass) and LClass.InheritsFrom(TKExtListPanelController) then
          Result := 'GridPanel'
        else if Assigned(LNode.Parent) and IsLoginViewNode(LNode.Parent) then
          Result := 'Login';
      end
      else if Assigned(LNode.Parent) and IsLoginViewNode(LNode.Parent) then
        Result := 'Login';
    end;
  end;
end;

function GetSubControllerClass(ANode: TEFNode): TClass;
var
  LControllerNode: TEFNode;
  LControllerClass: TClass;
begin
  Result := nil;
  LControllerNode := ANode.FindNode('Controller');
  if Assigned(LControllerNode) then
  begin
    LControllerClass := TJSControllerRegistry.Instance.FindClass(LControllerNode.AsString);
    if Assigned(LControllerClass) then
      Result := LControllerClass;
  end;
end;

function GetControllerClass(const AControllerNode: TEFNode): TClass;
var
  LControllerClassName: string;
begin
  Assert(Assigned(AControllerNode));
  LControllerClassName := GetControllerClassName(AControllerNode);
  if LControllerClassName <> '' then
    Result := TJSControllerRegistry.Instance.FindClass(LControllerClassName)
  else
    Result := nil;
end;

end.

