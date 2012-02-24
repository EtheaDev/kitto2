unit KIDE.BaseWizardFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ActnList, ExtCtrls,
  KIDE.BaseFormUnit;

type
  TBaseWizardForm = class(TBaseForm)
    PageControl: TPageControl;
    BackButton: TButton;
    ForwardButton: TButton;
    ActionList: TActionList;
    BackAction: TAction;
    ForwardAction: TAction;
    ButtonPanel: TPanel;
    TitlePanel: TPanel;
    procedure BackActionUpdate(Sender: TObject);
    procedure ForwardActionUpdate(Sender: TObject);
    procedure BackActionExecute(Sender: TObject);
    procedure ForwardActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetPageTitle: string;
    procedure SetPageTitle(const AValue: string);
    function GetPageIndex: Integer;
  protected
    {
      Called when the form is shown.
    }
    procedure InitWizard; virtual;
    {
      Called when the user is about to leave page ACurrentPageIndex and going
      to page ANewPageIndex.
    }
    procedure BeforeLeavePage(const ACurrentPageIndex, ANewPageIndex: Integer;
      const AGoingForward: Boolean); virtual;
    {
      Called when the user is about to enter page ANewPageIndex and coming
      from page ACurrentPageIndex.
    }
    procedure BeforeEnterPage(const ANewPageIndex, ACurrentPageIndex: Integer;
      const AGoingForward: Boolean); virtual;
    {
      Called when the user has left page AOldPageIndex to go to page ACurrentPageIndex.
    }
    procedure AfterLeavePage(const AOldPageIndex, ACurrentPageIndex: Integer;
      const AGoingForward: Boolean); virtual;
    {
      Called when the user has entered page ACurrentPageIndex coming from
      page AOldPageIndex.
    }
    procedure AfterEnterPage(const ACurrentPageIndex, AOldPageIndex: Integer;
      const AGoingForward: Boolean); virtual;
    {
      Called to decide whether to enable the Back button or not. By default, it
      is enabled unless on the first page (in which case it is also invisible).
    }
    function CanGoBack: Boolean; virtual;
    {
      Called to decide whether to enable the Forward button or not. By default,
      it is always enabled (when on the last page, it will close the form).
    }
    function CanGoForward: Boolean; virtual;

    property PageTitle: string read GetPageTitle write SetPageTitle;

    property PageIndex: Integer read GetPageIndex;

    procedure FinishWizard; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  EF.Localization;

function TBaseWizardForm.CanGoBack: Boolean;
begin
  Result := PageControl.ActivePageIndex > 0;
end;

function TBaseWizardForm.CanGoForward: Boolean;
begin
  Result := True;
end;

procedure TBaseWizardForm.BackActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := CanGoBack;
end;

procedure TBaseWizardForm.AfterEnterPage(const ACurrentPageIndex, AOldPageIndex: Integer;
  const AGoingForward: Boolean);
begin
  BackAction.Visible := ACurrentPageIndex > 0;
end;

procedure TBaseWizardForm.AfterLeavePage(const AOldPageIndex, ACurrentPageIndex: Integer;
  const AGoingForward: Boolean);
begin
end;

procedure TBaseWizardForm.BeforeEnterPage(const ANewPageIndex, ACurrentPageIndex: Integer;
  const AGoingForward: Boolean);
begin
end;

procedure TBaseWizardForm.BeforeLeavePage(const ACurrentPageIndex, ANewPageIndex: Integer;
  const AGoingForward: Boolean);
begin
end;

constructor TBaseWizardForm.Create(AOwner: TComponent);
begin
  inherited;
  ForwardAction.Caption := _('Next >>');
  BackAction.Caption := _('<< Previous');
end;

procedure TBaseWizardForm.BackActionExecute(Sender: TObject);
begin
  BeforeLeavePage(PageControl.ActivePageIndex, PageControl.ActivePageIndex - 1, False);
  BeforeEnterPage(PageControl.ActivePageIndex - 1, PageControl.ActivePageIndex, False);
  PageControl.SelectNextPage(False, False);
  AfterLeavePage(PageControl.ActivePageIndex + 1, PageControl.ActivePageIndex, False);
  AfterEnterPage(PageControl.ActivePageIndex, PageControl.ActivePageIndex + 1, False);
end;

procedure TBaseWizardForm.ForwardActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := CanGoForward;
  if PageControl.ActivePageIndex = PageControl.PageCount - 1 then
    (Sender as TAction).Caption := _('Finish')
  else
    (Sender as TAction).Caption := _('Next >>');
end;

function TBaseWizardForm.GetPageIndex: Integer;
begin
  Result := PageControl.ActivePageIndex;
end;

function TBaseWizardForm.GetPageTitle: string;
begin
  Result := TitlePanel.Caption;
end;

procedure TBaseWizardForm.ForwardActionExecute(Sender: TObject);
begin
  if PageControl.ActivePageIndex = PageControl.PageCount - 1 then
  begin
    FinishWizard;
    ModalResult := mrOk;
  end
  else
  begin
    BeforeLeavePage(PageControl.ActivePageIndex, PageControl.ActivePageIndex + 1, True);
    BeforeEnterPage(PageControl.ActivePageIndex + 1, PageControl.ActivePageIndex, True);
    PageControl.SelectNextPage(True, False);
    AfterLeavePage(PageControl.ActivePageIndex - 1, PageControl.ActivePageIndex, True);
    AfterEnterPage(PageControl.ActivePageIndex, PageControl.ActivePageIndex - 1, True);
  end;
end;

procedure TBaseWizardForm.FinishWizard;
begin
end;

procedure TBaseWizardForm.FormShow(Sender: TObject);
begin
  InitWizard;
end;

procedure TBaseWizardForm.InitWizard;
begin
  BeforeEnterPage(0, -1, True);
  PageControl.ActivePageIndex := 0;
  AfterEnterPage(0, -1, True);
end;

procedure TBaseWizardForm.SetPageTitle(const AValue: string);
begin
  TitlePanel.Caption := AValue;
end;

end.
