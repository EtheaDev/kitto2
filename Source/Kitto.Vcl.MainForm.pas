{-------------------------------------------------------------------------------
   Copyright 2012-2018 Ethea S.r.l.

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

unit Kitto.Vcl.MainForm;

{$I Kitto.Defines.inc}

interface

uses
  {$IF RTLVersion >= 23.0}Vcl.Themes, Vcl.Styles,{$IFEND}
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, IdCustomHTTPServer,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin, Generics.Collections,
  Vcl.ActnList, Kitto.Config, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.ImgList, EF.Logger,
  Actions, Vcl.Tabs, Vcl.Grids, System.ImageList, Kitto.Types, Kitto.Web.Server,
  Kitto.Web.Application, Kitto.Web.Session, Kitto.Web.Engine;

type
  TKMainFormLogEndpoint = class(TEFLogEndpoint)
  private
    FOnLog: TKLogEvent;
  protected
    procedure DoLog(const AString: string); override;
  public
    property OnLog: TKLogEvent read FOnLog write FOnLog;
  end;

  TKMainForm = class(TForm)
    ActionList: TActionList;
    StartAction: TAction;
    StopAction: TAction;
    PageControl: TPageControl;
    HomeTabSheet: TTabSheet;
    SessionCountLabel: TLabel;
    RestartAction: TAction;
    ConfigFileNameComboBox: TComboBox;
    ConfigLinkLabel: TLabel;
    StartSpeedButton: TSpeedButton;
    StopSpeedButton: TSpeedButton;
    ImageList: TImageList;
    LogMemo: TMemo;
    ControlPanel: TPanel;
    AppTitleLabel: TLabel;
    OpenConfigDialog: TOpenDialog;
    SpeedButton1: TSpeedButton;
    HomeURLLabel: TLabel;
    AppIcon: TImage;
    MainTabSet: TTabSet;
    SessionPanel: TPanel;
    SessionToolPanel: TPanel;
    RefreshButton: TButton;
    SessionListView: TListView;
    SessionListRefreshTimer: TTimer;
    procedure StartActionUpdate(Sender: TObject);
    procedure StopActionUpdate(Sender: TObject);
    procedure StartActionExecute(Sender: TObject);
    procedure StopActionExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RestartActionUpdate(Sender: TObject);
    procedure RestartActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ConfigFileNameComboBoxChange(Sender: TObject);
    procedure ConfigLinkLabelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HomeURLLabelClick(Sender: TObject);
    procedure MainTabSetChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
    procedure RefreshButtonClick(Sender: TObject);
    procedure SessionListViewEdited(Sender: TObject; Item: TListItem; var S: string);
    procedure SessionListViewInfoTip(Sender: TObject; Item: TListItem; var InfoTip: string);
    procedure SessionListRefreshTimerTimer(Sender: TObject);
  private
    FServer: TKWebServer;
    FApplication: TKWebApplication;
    FRestart: Boolean;
    FLogEndPoint: TKMainFormLogEndpoint;
    procedure ShowTabGUI(const AIndex: Integer);
    procedure UpdateSessionInfo;
    procedure SessionListUpdateHandler(AEngine: TKWebEngine;
      ASession: TKWebSession);
    procedure RecreateServer;
    const
      TAB_LOG = 0;
      TAB_SESSIONS = 1;
    function IsStarted: Boolean;
    procedure FillConfigFileNameCombo;
    procedure SetConfig(const AFileName: string);
    procedure SelectConfigFile;
    procedure DisplayHomeURL(const AHomeURL: string);
    function HasConfigFileName: Boolean;
    procedure DoLog(const AString: string);
  end;

var
  KMainForm: TKMainForm;

implementation

{$R *.dfm}

uses
  Math
  , StrUtils
  , DateUtils
  , EF.Sys
  , EF.Sys.Windows
  , EF.Shell
  , EF.Localization
  ;

{ TKMainForm }

procedure TKMainForm.RefreshButtonClick(Sender: TObject);
begin
  UpdateSessionInfo;
end;

procedure TKMainForm.ConfigLinkLabelClick(Sender: TObject);
begin
  SelectConfigFile;
end;

procedure TKMainForm.SelectConfigFile;
begin
  OpenConfigDialog.InitialDir := TKConfig.AppHomePath;
  if OpenConfigDialog.Execute then
  begin
    // The Home is the parent directory of the Metadata directory.
    TKConfig.AppHomePath := ExtractFilePath(OpenConfigDialog.FileName) + '..';
    Caption := TKConfig.AppHomePath;
    FillConfigFileNameCombo;
    SetConfig(ExtractFileName(OpenConfigDialog.FileName));
  end;
end;

procedure TKMainForm.SessionListRefreshTimerTimer(Sender: TObject);
begin
  UpdateSessionInfo;
end;

procedure TKMainForm.SessionListViewEdited(Sender: TObject; Item: TListItem;
  var S: string);
begin
  if TObject(Item.Data) is TKWebSession then
    TKWebSession(Item.Data).DisplayName := S;
end;

procedure TKMainForm.SessionListViewInfoTip(Sender: TObject; Item: TListItem; var InfoTip: string);
var
  LSession: TKWebSession;
begin
  if Assigned(Item) and  (TObject(Item.Data) is TKWebSession) then
  begin
    LSession := TKWebSession(Item.Data);
    InfoTip :=
      'User Agent: ' + LSession.LastRequestInfo.UserAgent + sLineBreak +
      'Client Address: ' + LSession.LastRequestInfo.ClientAddress + sLineBreak +
      'Last Request: ' + DateTimeToStr(LSession.LastRequestInfo.DateTime);
  end;
end;

procedure TKMainForm.DoLog(const AString: string);
begin
  LogMemo.Lines.Add(AString);
end;

procedure TKMainForm.StopActionExecute(Sender: TObject);
begin
  if IsStarted then
  begin
    DoLog(_('Stopping listener...'));
    FServer.Active := False;
    DoLog(_('Listener stopped'));
    HomeURLLabel.Visible := False;
    while IsStarted do
      Vcl.Forms.Application.ProcessMessages;
    if FRestart then
    begin
      FRestart := False;
      StartAction.Execute;
    end;
  end;
  UpdateSessionInfo;
end;

procedure TKMainForm.StopActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := IsStarted;
end;

procedure TKMainForm.MainTabSetChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  ShowTabGUI(NewTab);
  UpdateSessionInfo;
  SessionListRefreshTimer.Enabled := MainTabSet.TabIndex = TAB_SESSIONS;
end;

procedure TKMainForm.ShowTabGUI(const AIndex: Integer);
begin
  case AIndex of
    TAB_LOG:
      LogMemo.BringToFront;

    TAB_SESSIONS:
    begin
      UpdateSessionInfo;
      SessionPanel.BringToFront;
    end;
  end;
end;

procedure TKMainForm.RestartActionExecute(Sender: TObject);
begin
  FRestart := True;
  StopAction.Execute;
end;

procedure TKMainForm.RestartActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := IsStarted;
end;

procedure TKMainForm.UpdateSessionInfo;

  procedure UpdateCount(const ACount: Integer);
  begin
    SessionCountLabel.Caption := Format('Active Sessions: %d', [ACount]);
  end;

  procedure AddItem(const ACaption: string; const ASession: TKWebSession = nil);
  var
    LItem: TListItem;
  begin
    LItem := SessionListView.Items.Add;
    LItem.Caption := ACaption;
    if Assigned(ASession) then
    begin
      LItem.Data := ASession;
      // Start Time.
      LItem.SubItems.Add(IfThen(DateOf(ASession.CreationDateTime) = Date,
        TimeToStr(TimeOf(ASession.CreationDateTime)), DateTimeToStr(ASession.CreationDateTime)));
      // Last Req.
      if ASession.LastRequestInfo.DateTime = 0 then
        LItem.SubItems.Add('-')
      else
        LItem.SubItems.Add(IfThen(DateOf(ASession.LastRequestInfo.DateTime) = Date,
          TimeToStr(TimeOf(ASession.LastRequestInfo.DateTime)), DateTimeToStr(ASession.LastRequestInfo.DateTime)));
      // User.
      LItem.SubItems.Add(ASession.AuthData.GetString('UserName'));
      // Origin.
      LItem.SubItems.Add(ASession.LastRequestInfo.ClientAddress);
      // User Agent.
      LItem.SubItems.Add(ASession.LastRequestInfo.UserAgent);
    end;
  end;

var
  LSessions: TArray<TKWebSession>;
  LSession: TKWebSession;
begin
  SessionListView.Clear;
  if IsStarted then
  begin
    LSessions := FServer.Engine.GetSessions;

    UpdateCount(Length(LSessions));

    if Length(LSessions) = 0 then
      AddItem(_('None'))
    else
    begin
      for LSession in LSessions do
        AddItem(LSession.DisplayName, LSession);
    end;
  end
  else
  begin
    AddItem(_('Inactive'));
    UpdateCount(0);
  end;
end;

function TKMainForm.HasConfigFileName: Boolean;
begin
  Result := ConfigFileNameComboBox.Text <> '';
end;

procedure TKMainForm.HomeURLLabelClick(Sender: TObject);
begin
  OpenDocument(HomeURLLabel.Caption);
end;

function TKMainForm.IsStarted: Boolean;
begin
  Result := Assigned(FServer) and FServer.Active;
end;

procedure TKMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopAction.Execute;
end;

procedure TKMainForm.SessionListUpdateHandler(AEngine: TKWebEngine; ASession: TKWebSession);
begin
  UpdateSessionInfo;
end;

procedure TKMainForm.FormCreate(Sender: TObject);
var
  LDefaultConfig: string;
begin
{$IF RTLVersion >= 23.0}
  TStyleManager.TrySetStyle('Aqua Light Slate');
{$IFEND}
  //Read command line param -config
  LDefaultConfig := ChangeFileExt(GetCmdLineParamValue('Config', TKConfig.BaseConfigFileName),'.yaml');
  if LDefaultConfig <> '' then
    TKConfig.BaseConfigFileName := LDefaultConfig;

  RecreateServer;

  FLogEndPoint := TKMainFormLogEndpoint.Create;
  FLogEndPoint.OnLog := DoLog;

  UpdateSessionInfo;
end;

procedure TKMainForm.FormDestroy(Sender: TObject);
begin
  FServer.Active := False;
  SessionListView.Clear;
  FreeAndNil(FServer);
  FreeAndNil(FLogEndPoint);
end;

procedure TKMainForm.FormShow(Sender: TObject);
begin
  ShowTabGUI(TAB_LOG);
  Caption := TKConfig.AppHomePath;
  DoLog(Format(_('Build date: %s'), [DateTimeToStr(GetFileDateTime(ParamStr(0)))]));
  FillConfigFileNameCombo;
  if HasConfigFileName then
    StartAction.Execute
  else
    SelectConfigFile;
end;

procedure TKMainForm.ConfigFileNameComboBoxChange(Sender: TObject);
begin
  SetConfig(ConfigFileNameComboBox.Text);
end;

procedure TKMainForm.SetConfig(const AFileName: string);
var
  LWasStarted: Boolean;
  LAppIconFileName: string;
begin
  LWasStarted := IsStarted;
  if LWasStarted then
    StopAction.Execute;
  ConfigFileNameComboBox.ItemIndex := ConfigFileNameComboBox.Items.IndexOf(AFileName);
  TKConfig.BaseConfigFileName := AFileName;
  FApplication.ReloadConfig;
  AppTitleLabel.Caption := Format(_('Application: %s'), [_(FApplication.Config.AppTitle)]);
  LAppIconFileName := FApplication.FindResourcePathName(FApplication.Config.AppIcon + '.png');
  if LAppIconFileName <> '' then
    AppIcon.Picture.LoadFromFile(LAppIconFileName)
  else
    AppIcon.Picture.Bitmap := nil;
  StartAction.Update;
  if LWasStarted then
    StartAction.Execute;
end;

procedure TKMainForm.DisplayHomeURL(const AHomeURL: string);
begin
  DoLog(Format(_('Home URL: %s'), [AHomeURL]));
  HomeURLLabel.Caption := AHomeURL;
  HomeURLLabel.Visible := True;
end;

procedure TKMainForm.FillConfigFileNameCombo;
var
  LConfigIndex: Integer;
  LConfigFileName: string;
begin
  FindAllFiles('yaml', TKConfig.GetMetadataPath, ConfigFileNameComboBox.Items, False, False);
  if ConfigFileNameComboBox.Items.Count > 0 then
  begin
    LConfigFileName := TKConfig.BaseConfigFileName;
    LConfigIndex := ConfigFileNameComboBox.Items.IndexOf(LConfigFileName);
    if LConfigIndex <> -1 then
    begin
      ConfigFileNameComboBox.ItemIndex := LConfigIndex;
      ConfigFileNameComboBoxChange(ConfigFileNameComboBox);
    end
    else
    begin
      ConfigFileNameComboBox.ItemIndex := 0;
      ConfigFileNameComboBoxChange(ConfigFileNameComboBox);
    end;
  end;
end;

procedure TKMainForm.StartActionExecute(Sender: TObject);
begin
  RecreateServer;
  FServer.Active := True;
  SessionCountLabel.Visible := True;
  DoLog(_('Listener started'));
  DisplayHomeURL(FApplication.GetHomeURL(FServer.DefaultPort));
  UpdateSessionInfo;
end;

procedure TKMainForm.StartActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := HasConfigFileName and not IsStarted;
end;

procedure TKMainForm.RecreateServer;
begin
  FreeAndNil(FServer);

  FServer := TKWebServer.Create(nil);
  FServer.Engine.OnSessionStart := SessionListUpdateHandler;
  FServer.Engine.OnSessionEnd := SessionListUpdateHandler;
  FApplication := FServer.Engine.AddRoute(TKWebApplication.Create) as TKWebApplication;
  FServer.Setup(FApplication.Config);
end;

{ TKMainFormLogEndpoint }

procedure TKMainFormLogEndpoint.DoLog(const AString: string);
begin
  if Assigned(FOnLog) then
    FOnLog(AString);
end;

end.
