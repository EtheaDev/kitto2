unit Kitto.Ext.MainFormUnit;

interface

uses
  {$IF RTLVersion >= 23.0}Themes, Styles,{$IFEND}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ComCtrls, ToolWin, Kitto.Ext.Application,
  ActnList, Kitto.Config, StdCtrls;

type
  TKExtMainForm = class(TForm)
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ActionList: TActionList;
    StartAction: TAction;
    StopAction: TAction;
    StatusBar: TStatusBar;
    PageControl: TPageControl;
    LogTabSheet: TTabSheet;
    MonitorTabSheet: TTabSheet;
    LogMemo: TMemo;
    SessionCountLabel: TLabel;
    ToolButton3: TToolButton;
    RestartAction: TAction;
    ConfigFileNameComboBox: TComboBox;
    Label1: TLabel;
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
  private
    FKAppThread: TKExtAppThread;
    FRestart: Boolean;
    function IsStarted: Boolean;
    function GetKAppThread: TKExtAppThread;
    procedure KAppThreadTerminated(Sender: TObject);
    procedure UpdateSessionCountlabel;
    function GetSessionCount: Integer;
    procedure FillConfigFileNameCombo;
    property KAppThread: TKExtAppThread read GetKAppThread;
  end;

var
  KExtMainForm: TKExtMainForm;

implementation

{$R *.dfm}

uses
  Math,
  EF.SysUtils,
  FCGIApp;

procedure TKExtMainForm.KAppThreadTerminated(Sender: TObject);
begin
  FKAppThread := nil;
  StatusBar.SimpleText := 'Stopped';
  if FRestart then
  begin
    FRestart := False;
    StartAction.Execute;
  end;
end;

procedure TKExtMainForm.StopActionExecute(Sender: TObject);
begin
  if IsStarted then
  begin
    StatusBar.SimpleText := 'Stopping...';
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
  SessionCountLabel.Caption := Format('Active Sessions: %d', [GetSessionCount]);
end;

function TKExtMainForm.GetSessionCount: Integer;
begin
  if Assigned(FCGIApp.Application) then
    Result := Max(0, FCGIApp.Application.ThreadsCount)
  else
    Result := 0;
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
  FillConfigFileNameCombo;
  StartAction.Execute;
end;

procedure TKExtMainForm.ConfigFileNameComboBoxChange(Sender: TObject);
var
  LConfig: TKConfig;
  LWasStarted: Boolean;
begin
  LWasStarted := IsStarted;
  if LWasStarted then
    StopAction.Execute;
  TKConfig.BaseConfigFileName := ConfigFileNameComboBox.Text;
  LConfig := TKConfig.Create;
  try
    Caption := Format('%s - %s', [LConfig.AppTitle, DateTimeToStr(GetFileDateTime(ParamStr(0)))]);
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
    ConfigFileNameComboBox.ItemIndex := 0;
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
      FKAppThread.SessionTimeout := LConfig.Config.GetInteger('SessionTimeout', 30);
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
  StatusBar.SimpleText := 'Started';
end;

procedure TKExtMainForm.StartActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not IsStarted;
end;

{$IF RTLVersion >= 23.0}
initialization
  TStyleManager.TrySetStyle('Aqua Light Slate');
{$IFEND}

end.
