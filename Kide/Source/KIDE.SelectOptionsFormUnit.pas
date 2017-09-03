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
unit KIDE.SelectOptionsFormUnit;

interface

uses
  SysUtils, StdCtrls, ExtCtrls, Classes, Controls, Forms, Graphics;

type
  TSelectOptionForm = class(TForm)
    paBottom: TPanel;
    paButtons: TPanel;
    bbOK: TButton;
    bbCancel: TButton;
    gb: TRadioGroup;
    IconsRadioGroup: TRadioGroup;
    procedure FormShow(Sender: TObject);
  private
  public
    Constructor CreateSelectOption(AOwner : TComponent; ANewFont : TFont;
       AOptions : TStringList; ATitle : string; AColumns: Integer = 1;
       ANewWidth : Integer = 0; ANewHeight : Integer = 0;
       ASelectedIndex: Integer =0; AIconsStyle : Integer = 0);
  end;

  function KideSelectOption(AOptions : string; AFont : TFont;
    ATitle : string; const AHelpContext: Integer;
    var ASelectedIndex : Integer; var AIconsStyle: Integer;
    AColumns: Integer = 1; AMinWidth: Integer = 0; AMinHeigth: Integer = 0) : Boolean;

implementation

{$R *.DFM}

uses
  Math;

function KideSelectOption(AOptions : string; AFont : TFont;
  ATitle : string; const AHelpContext: Integer;
  var ASelectedIndex : Integer; var AIconsStyle: Integer;
  AColumns: Integer = 1; AMinWidth: Integer = 0; AMinHeigth: Integer = 0) : Boolean;
var
  FSelection : TSelectOptionForm;
  LList : TStringList;
  LHeight : Integer;
  LWidth : Integer;
  LMinWidth, LMinHeight: Integer;
  I : Integer;
begin
  LList := nil;
  FSelection := nil;
  Try
    LList := TStringList.Create;
    LList.Text := AOptions;
    LMinHeight := 180 + (20 * LList.Count) div AColumns;
    LHeight := Max(LMinHeight, AMinHeigth);
    LWidth := (7 * length(ATitle));
    for I := 0 to LList.Count - 1 do
      LWidth := max(LWidth, (7 * length(LList.strings[I])));
    FSelection := TSelectOptionForm.CreateSelectOption(
      nil, AFont, LList, ATitle, AColumns, LWidth, LHeight, ASelectedIndex, AIconsStyle);
    LMinWidth := Max(FSelection.paButtons.Width+20, AMinWidth);
    FSelection.Width := max(FSelection.Width, LMinWidth);
    FSelection.HelpContext := AHelpContext;
    Result := FSelection.ShowModal = mrOK;
    ASelectedIndex := FSelection.gb.ItemIndex;
    AIconsStyle := FSelection.IconsRadioGroup.ItemIndex;
  Finally
    LList.Free;
    FSelection.Free;
  End;
end;

procedure TSelectOptionForm.FormShow(Sender: TObject);
begin
  if gb.CanFocus then
    gb.SetFocus;
end;

constructor TSelectOptionForm.CreateSelectOption( AOwner : TComponent;
  ANewFont : TFont; AOptions : TStringList;
  ATitle : string; AColumns: Integer = 1;
  ANewWidth : Integer = 0; ANewHeight : Integer = 0;
  ASelectedIndex: Integer =0; AIconsStyle : Integer = 0);
begin
  inherited Create(AOwner);
  if ANewFont <> nil then
    Font := ANewFont;
  if ANewWidth <> 0 then
    Width := ANewWidth;
  if ANewHeight <> 0 then
    Height := ANewHeight;
  gb.Items.Text := AOptions.Text;
  gb.ItemIndex := ASelectedIndex;
  IconsRadioGroup.ItemIndex := AIconsStyle;
  gb.Columns := AColumns;
  self.Caption := ATitle;
end;

end.
