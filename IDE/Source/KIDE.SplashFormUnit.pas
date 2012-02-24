unit KIDE.SplashFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls;

type
  TSplashForm = class(TForm)
    LogoImage: TImage;
    Label1: TLabel;
    Label2: TLabel;
    VersionLabel: TLabel;
    procedure FormShow(Sender: TObject);
  private
  public
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

end.
