unit Kitto.Ext.StatusBar;

{$I Kitto.Defines.inc}

interface

uses
  Ext,
  Kitto.Ext.Base, Kitto.Metadata.Views;

type
  TKExtDefaultStatusBar = class(TKExtStatusBar)
  public
    procedure ClearStatus; override;
  end;

  TKExtStatusBarController = class(TKExtPanelControllerBase)
  private
    FStatusBar: TKExtDefaultStatusBar;
  protected
    procedure InitDefaults; override;
    procedure DoDisplay; override;
  end;

implementation

uses
  ExtPascal,
  EF.Tree,
  Kitto.Ext.Controller, Kitto.Environment, Kitto.Ext.Session;

{ TKExtStatusBarController }

procedure TKExtStatusBarController.DoDisplay;
begin
  inherited;
  FStatusBar.DefaultText := View.GetExpandedString('Controller/Text');
  FStatusBar.DefaultIconCls := Session.SetViewIconStyle(View, '', 'sb_', 'padding-left: 25px !important;');
end;

procedure TKExtStatusBarController.InitDefaults;
begin
  inherited;
  Layout := lyFit;
  AutoHeight := True;
  Border := False;

  FStatusBar := TKExtDefaultStatusBar.AddTo(Items);
end;

{ TKExtDefaultStatusBar }

procedure TKExtDefaultStatusBar.ClearStatus;
begin
  inherited;
  SetText(DefaultText);
  SetIcon(DefaultIconCls);
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('StatusBar', TKExtStatusBarController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('StatusBar');

end.
