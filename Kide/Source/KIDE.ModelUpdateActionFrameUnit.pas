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
unit KIDE.ModelUpdateActionFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.ModelCreator,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TModelUpdateActionFrame = class(TFrame)
    Label1: TLabel;
  strict private
    FModelUpdateAction: TModelUpdateAction;
  strict protected
    procedure SetModelUpdateAction(const AValue: TModelUpdateAction); virtual;
  public
    property ModelUpdateAction: TModelUpdateAction read FModelUpdateAction
      write SetModelUpdateAction;

    procedure SaveToAction; virtual;
  end;
  TModelUpdateActionFrameClass = class of TModelUpdateActionFrame;

function CreateModelUpdateActionFrame(const AAction: TModelUpdateAction;
  const AContainer: TWinControl): TModelUpdateActionFrame;

implementation

{$R *.dfm}

uses
  KIDE.TableInfoModelUpdateActionFrameUnit,
  KIDE.DetailReferenceUpdateActionFrameUnit,
  KIDE.ModelFieldUpdateActionFrameUnit,
  KIDE.ReferenceFieldUpdateActionFrameUnit;

function CreateModelUpdateActionFrame(const AAction: TModelUpdateAction;
  const AContainer: TWinControl): TModelUpdateActionFrame;
var
  LClass: TModelUpdateActionFrameClass;
begin
  Assert(Assigned(AAction));
  Assert(Assigned(AContainer));

  // Factory.
  if AAction is TTableInfoModelUpdateAction then
    LClass := TTableInfoModelUpdateActionFrame
  else if AAction is TDetailReferenceUpdateAction then
    LClass := TDetailReferenceUpdateActionFrame
  else if (AAction is TAddField) or (AAction is TModifyField) then
    LClass := TModelFieldUpdateActionFrame
  else if (AAction is TAddReferenceField) or (AAction is TModifyReferenceField) then
    LClass := TReferenceFieldUpdateActionFrame
  else
    LClass := TModelUpdateActionFrame;

  Result := LClass.Create(AContainer);
  try
    Result.Parent := AContainer;
    Result.Align := alClient;
    Result.ModelUpdateAction := AAction;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TModelUpdateActionFrame }

procedure TModelUpdateActionFrame.SaveToAction;
begin
end;

procedure TModelUpdateActionFrame.SetModelUpdateAction(const AValue: TModelUpdateAction);
begin
  FModelUpdateAction := AValue;
end;

end.
