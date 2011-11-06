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

unit Kitto.Ext.MainFormUnit;

{$I Kitto.Defines.inc}

interface

uses
  {$IF RTLVersion >= 23.0}Themes, Styles,{$IFEND}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ComCtrls, ToolWin, Kitto.Ext.Application,
  ActnList, Kitto.Config, StdCtrls, Buttons, ExtCtrls, ImgList;

type
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
  private
    FKAppThread: TKExtAppThread;
    FRestart: Boolean;
    function IsStarted: Boolean;
    function GetKAppThread: TKExtAppThread;
    procedure KAppThreadTerminated(Sender: TObject);
    procedure UpdateSessionCountlabel;
    function GetSessionCount: Integer;
    procedure FillConfigFileNameCombo;
    procedure SetConfig(const AFileName: string);
    procedure SelectConfigFile;
    property KAppThread: TKExtAppThread read GetKAppThread;
    function HasConfigFileName: Boolean;
    procedure Log(const AString: string);
  end;

var
  KExtMainForm: TKExtMainForm;

implementation

{$R *.dfm}

uses
  Math,
  EF.SysUtils, EF.Localization,
  FCGIApp;

procedure TKExtMainForm.KAppThreadTerminated(Sender: TObject);
begin
  FKAppThread := nil;
  Log(_('Stopped'));
  SessionCountLabel.Visible := False;
  if FRestart then
  begin
    FRestart := False;
    StartAction.Execute;
  end;
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

procedure TKExtMainForm.Log(const AString: string);
begin
  LogMemo.Lines.Add(AString);
end;

procedure TKExtMainForm.StopActionExecute(Sender: TObject);
begin
  if IsStarted then
  begin
    Log(_('Stopping...'));
    FKAppThread.Terminate;
    while IsStarted do
      Forms.Application.ProcessMessages;
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

function TKExtMainForm.IsStarted: Boolean;
begin
  Result := Assigned(FKAppThread);
end;

procedure TKExtMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopAction.Execute;
  { TODO : needed otherwise OnTerminate is not called. Should be done in a different way. }
  Sleep(500);
end;

procedure TKExtMainForm.FormShow(Sender: TObject);
begin
  Caption := TKConfig.AppHomePath;
  Log(Format(_('Build date: %s'), [DateTimeToStr(GetFileDateTime(ParamStr(0)))]));
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
    AppTitleLabel.Caption := Format(_('Application: %s'), [LConfig.AppTitle]);
  finally
    FreeAndNil(LConfig);
  end;
{ TODO : fix the restart issue }
//  if LWasStarted then
//    StartAction.Execute;
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

function TKExtMainForm.GetKAppThread: TKExtAppThread;
var
  LConfig: TKConfig;
begin
  if not Assigned(FKAppThread) then
  begin
    FKAppThread := TKExtAppThread.Create(True);
    FKAppThread.OnTerminate := KAppThreadTerminated;
    LConfig := TKConfig.Create;
    try
      FKAppThread.AppTitle := LConfig.AppTitle;
      FKAppThread.TCPPort := LConfig.Config.GetInteger('TCPPort', 2014);
      Log(Format('TCPPort: %d', [FKAppThread.TCPPort]));
      FKAppThread.SessionTimeout := LConfig.Config.GetInteger('SessionTimeout', 30);
      Log(Format('SessionTimeout: %d', [FKAppThread.SessionTimeout]));
      FKAppThread.FreeOnTerminate := True;
    finally
      FreeAndNil(LConfig);
    end;
  end;
  Result := FKAppThread;
end;

procedure TKExtMainForm.StartActionExecute(Sender: TObject);
begin
  KAppThread.Start;
  SessionCountLabel.Visible := True;
  Log(_('Started'));
end;

procedure TKExtMainForm.StartActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := HasConfigFileName and not IsStarted;
end;

{$IF RTLVersion >= 23.0}
initialization
  TStyleManager.TrySetStyle('Aqua Light Slate');
{$IFEND}

end.
