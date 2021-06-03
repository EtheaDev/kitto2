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
    CopyrightLabel: TLabel;
    VersionLabel: TLabel;
    procedure FormShow(Sender: TObject);
    procedure LogoImageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    FIsModal: Boolean;
    FPlaySound: Boolean;
    procedure PlaySoundResource;
  public
    class procedure ShowAbout;
  end;

var
  SplashForm: TSplashForm;

implementation

uses
  MMSystem,
  EF.Localization,
  KIDE.Utils;

{$R *.dfm}

procedure TSplashForm.FormCreate(Sender: TObject);
begin
  FPlaySound := FindCmdLineSwitch('sound');
end;

procedure TSplashForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Chr(27) then
    Close;
end;

procedure TSplashForm.FormShow(Sender: TObject);
begin
  if FPlaySound then
    PlaySoundResource;
  VersionLabel.Caption := Format('%s: %s',[_('Version'),GetKIDEVersion]);
end;

procedure TSplashForm.PlaySoundResource;
var
  LFindHandle, LResHandle: THandle;
  LSound: PChar;
begin
  LFindHandle := FindResource(HInstance, 'Splash', 'WAVE');
  if LFindHandle <> 0 then
  begin
    LResHandle := LoadResource(HInstance, LFindHandle);
    if LResHandle <> 0 then
    begin
      LSound := LockResource(LResHandle);
      if Assigned(LSound) then
        sndPlaySound(LSound, SND_ASYNC or SND_MEMORY);
      UnlockResource(LResHandle);
    end;
    FreeResource(LFindHandle) ;
  end;
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
      //Play sound always when showing About form
      FPlaySound := True;
      ShowModal;
    finally
      Free;
    end;
  end;
end;

end.
