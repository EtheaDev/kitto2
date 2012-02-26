unit KIDE.SplashFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls;

type
  // Used to disable styles and paint the labels always white.
  TLabel = class(Vcl.StdCtrls.TLabel);

  TSplashForm = class(TForm)
    LogoImage: TImage;
    Label2: TLabel;
    VersionLabel: TLabel;
    procedure FormShow(Sender: TObject);
    procedure LogoImageClick(Sender: TObject);
  private
    FIsModal: Boolean;
  public
    class procedure ShowAbout;
  end;

var
  SplashForm: TSplashForm;

implementation

uses
  KIDE.Utils;

{$R *.dfm}

procedure TSplashForm.FormShow(Sender: TObject);
begin
  VersionLabel.Caption := GetKIDEVersion;
end;

procedure TSplashForm.LogoImageClick(Sender: TObject);
begin
  if FIsModal then
    Close;
end;

class procedure TSplashForm.ShowAbout;
begin
  with TSplashForm.Create(Application) do
  begin
    try
      FIsModal := True;
      ShowModal;
    finally
      Free;
    end;
  end;
end;

end.
