unit Kitto.Ext.StatusBar;

{$I Kitto.Defines.inc}

interface

uses
  Ext, ExtUx,
  Kitto.Ext.Base, Kitto.Metadata.Views;

type
  TKExtStatusBarController = class(TKExtPanelControllerBase)
  private
    FStatusBar: TExtUxStatusBar;
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
  FStatusBar.Text := View.GetExpandedString('Controller/Text');
  FStatusBar.IconCls := Session.SetViewIconStyle(View, '', 'sb_', 'padding-left: 25px !important;');
end;

procedure TKExtStatusBarController.InitDefaults;
begin
  inherited;
  Layout := lyFit;
  AutoHeight := True;
  Border := False;

  FStatusBar := TExtUxStatusBar.AddTo(Items);
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('StatusBar', TKExtStatusBarController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('StatusBar');

end.
