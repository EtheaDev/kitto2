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
  EF.Sys, EF.Localization;

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
