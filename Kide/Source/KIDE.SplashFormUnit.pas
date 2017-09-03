{*******************************************************************}
{                                                                   }
{   Kide2 Editor: GUI for Kitto2                                    }
{                                                                   }
{   Copyright (c) 2012-2017 Ethea S.r.l.                            }
{   ALL RIGHTS RESERVED / TUTTI I DIRITTI RISERVATI                 }
{                                                                   }
{*******************************************************************}
{                                                                   }
{   The entire contents of this file is protected by                }
{   International Copyright Laws. Unauthorized reproduction,        }
{   reverse-engineering, and distribution of all or any portion of  }
{   the code contained in this file is strictly prohibited and may  }
{   result in severe civil and criminal penalties and will be       }
{   prosecuted to the maximum extent possible under the law.        }
{                                                                   }
{   RESTRICTIONS                                                    }
{                                                                   }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED      }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE        }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE       }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT  }
{   AND PERMISSION FROM ETHEA S.R.L.                                }
{                                                                   }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON       }
{   ADDITIONAL RESTRICTIONS.                                        }
{                                                                   }
{*******************************************************************}
{                                                                   }
{   Il contenuto di questo file è protetto dalle leggi              }
{   internazionali sul Copyright. Sono vietate la riproduzione, il  }
{   reverse-engineering e la distribuzione non autorizzate di tutto }
{   o parte del codice contenuto in questo file. Ogni infrazione    }
{   sarà perseguita civilmente e penalmente a termini di legge.     }
{                                                                   }
{   RESTRIZIONI                                                     }
{                                                                   }
{   SONO VIETATE, SENZA IL CONSENSO SCRITTO DA PARTE DI             }
{   ETHEA S.R.L., LA COPIA, LA VENDITA, LA DISTRIBUZIONE E IL       }
{   TRASFERIMENTO A TERZI, A QUALUNQUE TITOLO, DEL CODICE SORGENTE  }
{   CONTENUTO IN QUESTO FILE E ALTRI FILE AD ESSO COLLEGATI.        }
{                                                                   }
{   SI FACCIA RIFERIMENTO ALLA LICENZA D'USO PER INFORMAZIONI SU    }
{   EVENTUALI RESTRIZIONI ULTERIORI.                                }
{                                                                   }
{*******************************************************************}
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
