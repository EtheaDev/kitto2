unit KIDE.ImageViewerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.BaseFrameUnit, Vcl.ExtCtrls, System.Actions,
  Vcl.ActnList, Vcl.StdCtrls,
  Vcl.Imaging.jpeg, Vcl.Imaging.pngimage, Vcl.Imaging.GIFImg;

type
  TImageViewerFrame = class(TBaseFrame)
    ViewerImage: TImage;
    TopPanel: TPanel;
    StretchCheckBox: TCheckBox;
    ProportionalCheckBox: TCheckBox;
    procedure StretchCheckBoxClick(Sender: TObject);
    procedure ProportionalCheckBoxClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure LoadFromFile(const AFileName: string);
  end;

var
  ImageViewerFrame: TImageViewerFrame;

implementation

{$R *.dfm}

procedure TImageViewerFrame.ProportionalCheckBoxClick(Sender: TObject);
begin
  inherited;
  ViewerImage.Proportional := ProportionalCheckBox.Checked;
end;

procedure TImageViewerFrame.StretchCheckBoxClick(Sender: TObject);
begin
  inherited;
  ViewerImage.Stretch := StretchCheckBox.Checked;
end;

procedure TImageViewerFrame.LoadFromFile(const AFileName: string);
begin
  ViewerImage.Picture.LoadFromFile(AFileName);
end;

end.
