///	<summary>This unit defines a wait form to be used when long operations take
///	place, and a default instance of it, accessible through global
///	functions.</summary>
unit KIDE.WaitFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, KIDE.BaseFormUnit;

type
  {
    A wait form to be used when long operations take place.
  }
  TWaitForm = class(TBaseForm)
    WaitMessageLabel: TLabel;
    Bevel: TBevel;
    procedure FormDeactivate(Sender: TObject);
  private
    FClosing: Boolean;
    function GetWaitMessage: string;
    procedure SetWaitMessage(const AValue: string);
    procedure SetOptimalWidth;
    procedure CenterForm;
  public
    ///	<summary>Message shown to the user while the form is visible. Setting
    ///	this property causes the form to resize accordingly.</summary>
    property WaitMessage: string read GetWaitMessage write SetWaitMessage;
  end;

///	<summary>Shows the default instance with the specificed message, or a
///	default generic message.</summary>
procedure ShowDefaultWaitForm(const AWaitMessage: string = '';
  const AOwnerForm: TComponent = nil);

///	<summary>Hides and destroys the default wait form, if it's
///	visible.</summary>
procedure HideDefaultWaitForm;

implementation

{$R *.dfm}

uses
  Consts,
  EF.SysUtils, EF.Localization;

var
  _DefaultWaitForm: TWaitForm;

procedure ShowDefaultWaitForm(const AWaitMessage: string = '';
  const AOwnerForm: TComponent = nil);
begin
  HideDefaultWaitForm;
  _DefaultWaitForm := TWaitForm.Create(AOwnerForm);
  try
    if AWaitMessage = '' then
      _DefaultWaitForm.WaitMessage := _('Operation in progress. Please wait...')
    else
      _DefaultWaitForm.WaitMessage := AWaitMessage;
    _DefaultWaitForm.Show;
    _DefaultWaitForm.Update;
  except
    FreeAndNil(_DefaultWaitForm);
    raise;
  end;
end;

procedure HideDefaultWaitForm;
begin
  if Assigned(_DefaultWaitForm) then
  begin
    _DefaultWaitForm.FClosing := True;
    _DefaultWaitForm.Close;
    // Make sure the form is freed immediately, otherwise an exception raised
    // while the wait form is active doesn't pop up the application message box.
    FreeAndNil(_DefaultWaitForm);
  end;
end;

{ TWaitForm }

procedure TWaitForm.FormDeactivate(Sender: TObject);
begin
  if not FClosing then
    SetFocus;
end;

function TWaitForm.GetWaitMessage: string;
begin
  Result := WaitMessageLabel.Caption;
end;

procedure TWaitForm.SetWaitMessage(const AValue: string);
begin
  WaitMessageLabel.Caption := AValue;
  SetOptimalWidth;
  CenterForm;
  Update;
end;

procedure TWaitForm.SetOptimalWidth;
const
  SPACING = 30;
begin
  ClientWidth := WaitMessageLabel.Width + SPACING + SPACING;
  WaitMessageLabel.Left := SPACING;
end;

procedure TWaitForm.CenterForm;
begin
  Left := (Screen.WorkAreaWidth div 2) - (Width div 2);
end;

end.
