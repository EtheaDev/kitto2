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

unit Kitto.Ext.MainFormUnit;

{$I Kitto.Defines.inc}

interface

uses
  {$IF RTLVersion >= 23.0}Themes, Styles,{$IFEND}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ComCtrls, ToolWin, Kitto.Ext.Application,
  ActnList, Kitto.Config, StdCtrls, Buttons, ExtCtrls, ImgList, EF.Logger;

type
  TKExtLogEvent = procedure (const AString: string) of object;

  TKExtMainFormLogEndpoint = class(TEFLogEndpoint)
  private
    FOnLog: TKExtLogEvent;
  protected
    procedure DoLog(const AString: string); override;
  public
    property OnLog: TKExtLogEvent read FOnLog write FOnLog;
  end;

  TKExtMainForm = class(TForm)
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
    procedure StartActionUpdate(Sender: TObject);
    procedure StopActionUpdate(Sender: TObject);
    procedure StartActionExecute(Sender: TObject);
    procedure StopActionExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure RestartActionUpdate(Sender: TObject);
    procedure RestartActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ConfigFileNameComboBoxChange(Sender: TObject);
    procedure ConfigLinkLabelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HomeURLLabelClick(Sender: TObject);
  private
    FAppThread: TKExtAppThread;
    FRestart: Boolean;
    FLogEndPoint: TKExtMainFormLogEndpoint;
    function IsStarted: Boolean;
    function GetAppThread: TKExtAppThread;
    procedure AppThreadTerminated(Sender: TObject);
    procedure UpdateSessionCountlabel;
    function GetSessionCount: Integer;
    procedure FillConfigFileNameCombo;
    procedure SetConfig(const AFileName: string);
    procedure SelectConfigFile;
    procedure DisplayHomeURL(const AHomeURL: string);
    property AppThread: TKExtAppThread read GetAppThread;
    function HasConfigFileName: Boolean;
    procedure DoLog(const AString: string);
  end;

var
  KExtMainForm: TKExtMainForm;

implementation

{$R *.dfm}

uses
  Math,
  EF.SysUtils, EF.Shell, EF.Localization,
  FCGIApp;

procedure TKExtMainForm.AppThreadTerminated(Sender: TObject);
begin
  FAppThread := nil;
  DoLog(_('Listener stopped'));
  SessionCountLabel.Visible := False;
end;

procedure TKExtMainForm.ConfigLinkLabelClick(Sender: TObject);
begin
  SelectConfigFile;
end;

procedure TKExtMainForm.SelectConfigFile;
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

procedure TKExtMainForm.DoLog(const AString: string);
begin
  LogMemo.Lines.Add(AString);
end;

procedure TKExtMainForm.StopActionExecute(Sender: TObject);
begin
  if IsStarted then
  begin
    DoLog(_('Stopping listener...'));
    FAppThread.Terminate;
    while IsStarted do
      Forms.Application.ProcessMessages;
    if FRestart then
    begin
      FRestart := False;
      StartAction.Execute;
    end;
  end;
end;

procedure TKExtMainForm.StopActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := IsStarted;
end;

procedure TKExtMainForm.RestartActionExecute(Sender: TObject);
begin
  FRestart := True;
  StopAction.Execute;
end;

procedure TKExtMainForm.RestartActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := IsStarted;
end;

procedure TKExtMainForm.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  UpdateSessionCountlabel;
end;

procedure TKExtMainForm.UpdateSessionCountlabel;
begin
  SessionCountLabel.Caption := Format(_('Active Sessions: %d'), [GetSessionCount]);
end;

function TKExtMainForm.GetSessionCount: Integer;
begin
  if Assigned(FCGIApp.Application) then
    Result := Max(0, FCGIApp.Application.ThreadsCount)
  else
    Result := 0;
end;

function TKExtMainForm.HasConfigFileName: Boolean;
begin
  Result := ConfigFileNameComboBox.Text <> '';
end;

procedure TKExtMainForm.HomeURLLabelClick(Sender: TObject);
begin
  OpenDocument(HomeURLLabel.Caption);
end;

function TKExtMainForm.IsStarted: Boolean;
begin
  Result := Assigned(FAppThread);
end;

procedure TKExtMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopAction.Execute;
  Sleep(100); // Apparently avoids a finalization problem in DBXCommon.
end;

procedure TKExtMainForm.FormCreate(Sender: TObject);
begin
  FLogEndPoint := TKExtMainFormLogEndpoint.Create;
  FLogEndPoint.OnLog := DoLog;
end;

procedure TKExtMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FLogEndPoint);
end;

procedure TKExtMainForm.FormShow(Sender: TObject);
begin
  Caption := TKConfig.AppHomePath;
  DoLog(Format(_('Build date: %s'), [DateTimeToStr(GetFileDateTime(ParamStr(0)))]));
  FillConfigFileNameCombo;
  if HasConfigFileName then
    StartAction.Execute
  else
    SelectConfigFile;
end;

procedure TKExtMainForm.ConfigFileNameComboBoxChange(Sender: TObject);
begin
  SetConfig(ConfigFileNameComboBox.Text);
end;

procedure TKExtMainForm.SetConfig(const AFileName: string);
var
  LConfig: TKConfig;
  LWasStarted: Boolean;
begin
  LWasStarted := IsStarted;
  if LWasStarted then
    StopAction.Execute;
  ConfigFileNameComboBox.ItemIndex := ConfigFileNameComboBox.Items.IndexOf(AFileName);
  TKConfig.BaseConfigFileName := AFileName;
  LConfig := TKConfig.Create;
  try
    AppTitleLabel.Caption := Format(_('Application: %s'), [_(LConfig.AppTitle)]);
  finally
    FreeAndNil(LConfig);
  end;
  StartAction.Update;
  if LWasStarted then
    StartAction.Execute;
end;

procedure TKExtMainForm.DisplayHomeURL(const AHomeURL: string);
begin
  DoLog(Format(_('Home URL: %s'), [AHomeURL]));
  HomeURLLabel.Caption := AHomeURL;
  HomeURLLabel.Visible := True;
end;

procedure TKExtMainForm.FillConfigFileNameCombo;
begin
  FindAllFiles('yaml', TKConfig.GetMetadataPath, ConfigFileNameComboBox.Items, False, False);
  if ConfigFileNameComboBox.Items.Count > 0 then
  begin
    ConfigFileNameComboBox.ItemIndex := 0;
    ConfigFileNameComboBoxChange(ConfigFileNameComboBox);
  end;
end;

function TKExtMainForm.GetAppThread: TKExtAppThread;
begin
  if not Assigned(FAppThread) then
  begin
    FAppThread := TKExtAppThread.Create(True);
    FAppThread.FreeOnTerminate := True;
    FAppThread.OnTerminate := AppThreadTerminated;
    FAppThread.Configure;
  end;
  Result := FAppThread;
end;

procedure TKExtMainForm.StartActionExecute(Sender: TObject);
var
  LConfig: TKConfig;
begin
  AppThread.Start;
  SessionCountLabel.Visible := True;
  DoLog(_('Listener started'));
  LConfig := TKConfig.Create;
  try
    DisplayHomeURL(Format('http://localhost/kitto/%d', [LConfig.Config.GetInteger('FastCGI/TCPPort', 2014)]));
  finally
    FreeAndNil(LConfig);
  end;
end;

procedure TKExtMainForm.StartActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := HasConfigFileName and not IsStarted;
end;

{ TKExtMainFormLogEndpoint }

procedure TKExtMainFormLogEndpoint.DoLog(const AString: string);
begin
  if Assigned(FOnLog) then
    FOnLog(AString);
end;

{$IF RTLVersion >= 23.0}
initialization
  TStyleManager.TrySetStyle('Aqua Light Slate');
{$IFEND}

end.
