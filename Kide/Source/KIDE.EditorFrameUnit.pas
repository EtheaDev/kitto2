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
unit KIDE.EditorFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.BaseFrameUnit, KIDE.Editor, EF.Tree;

type
  TEditorFrame = class {abstract}(TBaseFrame, IEditor)
  strict protected
    procedure Save; virtual;
    procedure Reload; virtual;
    function EditorMatchesSpec(const ASpec: string): Boolean; virtual; abstract;
    function EditorSuits(const ASpec: string; const AParams: TEFNode): Boolean; virtual; abstract;
    procedure RefreshEditor; virtual;
    function GetEditorProperty(const AName: string): Variant; virtual;
    function IsChanged: Boolean; virtual;
  public
    procedure DisplayEmbedded(const AParent: TWinControl); virtual;
    procedure CloseEditor(const AForce: Boolean); virtual;

    procedure InitEditor(const AParams: TEFNode); virtual;
    function IsEditorActionEnabled(const AAction: TEditorAction): Boolean; virtual;
    procedure ExecuteEditorAction(const AAction: TEditorAction);

    function AsObject: TObject;

    function GetSpec: string; virtual; abstract;
  end;

implementation

{$R *.dfm}

uses
  System.UITypes,
  EF.Localization;

{ TEditorFrame }

function TEditorFrame.AsObject: TObject;
begin
  Result := Self;
end;

procedure TEditorFrame.CloseEditor(const AForce: Boolean);
begin
  if not AForce and IsChanged then
  begin
    case MessageDlg('If you close this page, all pending changes will be lost. Do you want to save the changes before closing?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: Save;
      mrCancel: Abort;
    end;
  end;
end;

procedure TEditorFrame.DisplayEmbedded(const AParent: TWinControl);
begin
  Assert(Assigned(AParent));

  Parent := AParent;
  Align := alClient;
end;

procedure TEditorFrame.ExecuteEditorAction(const AAction: TEditorAction);
begin
  case AAction of
    eaSave: Save;
    eaReload: Reload;
  end;
end;

function TEditorFrame.GetEditorProperty(const AName: string): Variant;
begin
  Result := '';
end;

procedure TEditorFrame.InitEditor(const AParams: TEFNode);
begin
  Assert(Assigned(AParams));
end;

function TEditorFrame.IsChanged: Boolean;
begin
  Result := False;
end;

function TEditorFrame.IsEditorActionEnabled(const AAction: TEditorAction): Boolean;
begin
  Result := True;
end;

procedure TEditorFrame.RefreshEditor;
begin
end;

procedure TEditorFrame.Save;
begin
end;

procedure TEditorFrame.Reload;
begin
  if MessageDlg(_('Your changes will be lost! Are you sure you want to undo/reload content?'),
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Abort;
end;

end.
