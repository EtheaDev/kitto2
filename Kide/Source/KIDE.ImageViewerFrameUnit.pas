{-------------------------------------------------------------------------------
   Copyright 2012-2021 Ethea S.r.l.

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
