unit Ext.Ux;

interface

uses
  StrUtils, Kitto.Ext, Ext.Base, Ext.Util, Ext.Menu;

type
  TExtUxStatusBar = class(TExtToolbar)
  private
    FAutoClear: Integer; // 5000
    FBusyIconCls: string; // 'x-status-busy'
    FBusyText: string; // 'Loading...'
    FCls: string; // 'x-statusbar'
    FDefaultIconCls: string;
    FDefaultText: string;
    FIconCls: string;
    FStatusAlign: string;
    FText: string;
    procedure SetAutoClear(const AValue: Integer);
    procedure SetBusyIconCls(const AValue: string);
    procedure SetBusyText(const AValue: string);
    procedure SetCls(const AValue: string);
    procedure SetDefaultIconCls(const AValue: string);
    procedure SetDefaultText(const AValue: string);
    procedure SetIconCls(const AValue: string);
    procedure SetStatusAlign(const AValue: string);
    procedure _SetText(const AValue: string);
  protected
    procedure InitDefaults; override;
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
    function ClearStatus(const AConfig: TExtObject = nil): TExtExpression;
    function GetText: TExtExpression;
    function SetIcon(const AIconCls: string = ''): TExtExpression;
    function SetStatus(const AConfig: TExtObject = nil): TExtExpression; overload;
    function SetStatus(const AConfig: string): TExtExpression; overload;
    function SetText(const AText: string): TExtExpression;
    function ShowBusy(const AConfig: TExtObject = nil): TExtExpression; overload;
    function ShowBusy(const AConfig: string): TExtExpression; overload;
    property AutoClear: Integer read FAutoClear write SetAutoClear;
    property BusyIconCls: string read FBusyIconCls write SetBusyIconCls;
    property BusyText: string read FBusyText write SetBusyText;
    property Cls: string read FCls write SetCls;
    property DefaultIconCls: string read FDefaultIconCls write SetDefaultIconCls;
    property DefaultText: string read FDefaultText write SetDefaultText;
    property IconCls: string read FIconCls write SetIconCls;
    property StatusAlign: string read FStatusAlign write SetStatusAlign;
    property Text: string read FText write _SetText;
  end;

implementation

procedure TExtUxStatusBar.SetAutoClear(const AValue: Integer);
begin
  FAutoClear := SetConfigItem('autoClear', AValue);
end;

procedure TExtUxStatusBar.SetBusyIconCls(const AValue: string);
begin
  FBusyIconCls := AValue;
  SetConfigItem('busyIconCls', AValue);
end;

procedure TExtUxStatusBar.SetBusyText(const AValue: string);
begin
  FBusyText := AValue;
  SetConfigItem('busyText', AValue);
end;

procedure TExtUxStatusBar.SetCls(const AValue: string);
begin
  FCls := AValue;
  SetConfigItem('cls', AValue);
end;

procedure TExtUxStatusBar.SetDefaultIconCls(const AValue: string);
begin
  FDefaultIconCls := AValue;
  SetConfigItem('defaultIconCls', AValue);
end;

procedure TExtUxStatusBar.SetDefaultText(const AValue: string);
begin
  FDefaultText := AValue;
  SetConfigItem('defaultText', AValue);
end;

procedure TExtUxStatusBar.SetIconCls(const AValue: string);
begin
  FIconCls := AValue;
  SetConfigItem('iconCls', AValue);
end;

procedure TExtUxStatusBar.SetStatusAlign(const AValue: string);
begin
  FStatusAlign := AValue;
  SetConfigItem('statusAlign', AValue);
end;

procedure TExtUxStatusBar._SetText(const AValue: string);
begin
  FText := AValue;
  SetConfigItem('text', 'setText', AValue);
end;

class function TExtUxStatusBar.JSClassName: string;
begin
  Result := 'Ext.ux.statusbar.StatusBar';
end;

procedure TExtUxStatusBar.InitDefaults;
begin
  inherited;
  FAutoClear := 5000;
  FBusyIconCls := 'x-status-busy';
  FBusyText := 'Loading...';
  FCls := 'x-statusbar';
end;

function TExtUxStatusBar.ClearStatus(const AConfig: TExtObject = nil): TExtExpression;
begin
  Result := CallMethod('clearStatus')
    .AddParam(AConfig)
    .AsExpression;
end;

function TExtUxStatusBar.GetObjectNamePrefix: string;
begin
  Result := 'statusbar';
end;

function TExtUxStatusBar.GetText: TExtExpression;
begin
  Result := CallMethod('getText')
    .AsExpression;
end;

function TExtUxStatusBar.SetIcon(const AIconCls: string): TExtExpression;
begin
  Result := CallMethod('setIcon')
    .AddParam(AIconCls)
    .AsExpression;
end;

function TExtUxStatusBar.SetStatus(const AConfig: TExtObject): TExtExpression;
begin
  Result := CallMethod('setStatus')
    .AddParam(AConfig)
    .AsExpression;
end;

function TExtUxStatusBar.SetStatus(const AConfig: string): TExtExpression;
begin
  Result := CallMethod('setStatus')
    .AddParam(AConfig)
    .AsExpression;
end;

function TExtUxStatusBar.SetText(const AText: string): TExtExpression;
begin
  Result := CallMethod('setText')
    .AddParam(AText)
    .AsExpression;
end;

function TExtUxStatusBar.ShowBusy(const AConfig: TExtObject): TExtExpression;
begin
  Result := CallMethod('showBusy')
    .AddParam(AConfig)
    .AsExpression;
end;

function TExtUxStatusBar.ShowBusy(const AConfig: string): TExtExpression;
begin
  Result := CallMethod('showBusy')
    .AddParam(AConfig)
    .AsExpression;
end;

end.
