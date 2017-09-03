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
unit KIDE.SimpleNodeDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Tabs,
  EF.Tree,
  KIDE.EFTreeFrameUnit, System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin,
  KIDE.EditNodeBaseFrameUnit, Vcl.StdCtrls, KIDE.NodeDesignerFrameUnit, KIDE.BaseFrameUnit,
  KIDE.CodeEditorFrameUnit, Vcl.Samples.Spin;

type
  TSimpleNodeDesignerFrame = class(TEditNodeBaseFrame)
    KeyPanel: TPanel;
    KeyEdit: TLabeledEdit;
    CheckBoxPanel: TPanel;
    CheckBoxValue: TCheckBox;
    IntegerPanel: TPanel;
    IntegerValue: TSpinEdit;
    IntegerValueLabel: TLabel;
    TextPanel: TPanel;
    ValueLabel: TLabel;
    ValueEdit: TMemo;
  private
  strict protected
  protected
    procedure UpdateDesignPanel(const AForce: Boolean); override;
    procedure DesignPanelToEditNode; override;
  public
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

{ TDefaultNodeDesignerFrame }

procedure TSimpleNodeDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  TEFNode(EditNode).Name := KeyEdit.Text;
  if TextPanel.Visible then
    TEFNode(EditNode).Value := ValueEdit.Text
  else if CheckBoxPanel.Visible then
    TEFNode(EditNode).AsBoolean := CheckBoxValue.Checked
  else if IntegerPanel.Visible then
    TEFNode(EditNode).AsInteger := IntegerValue.Value;
end;

procedure TSimpleNodeDesignerFrame.Init(const ANode: TEFTree);
var
  LNodeDataType: TEFDataType;
begin
  inherited;
  LNodeDataType := TEFNode(ANode).DataType;
  KeyEdit.Enabled := TEFNode(ANode).ChildCount = 0;
  CheckBoxPanel.Visible := LNodeDataType.InheritsFrom(TEFBooleanDataType);
  IntegerPanel.Visible := LNodeDataType.InheritsFrom(TEFIntegerDataType);
  TextPanel.Visible := not (CheckBoxPanel.Visible or IntegerPanel.Visible);
end;

class function TSimpleNodeDesignerFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := (ANode is TEFNode) and (TEFNode(ANode).ChildCount = 0);
end;

procedure TSimpleNodeDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
  KeyEdit.Text := TEFNode(EditNode).Name;
  if TextPanel.Visible then
    ValueEdit.Text := TEFNode(EditNode).AsString
  else if CheckBoxPanel.Visible then
    CheckBoxValue.Checked := TEFNode(EditNode).AsBoolean
  else if IntegerPanel.Visible then
    IntegerValue.Value := TEFNode(EditNode).AsInteger;
end;
(*
initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TSimpleNodeDesignerFrame.GetClassId, TSimpleNodeDesignerFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TSimpleNodeDesignerFrame.GetClassId);
*)
end.
