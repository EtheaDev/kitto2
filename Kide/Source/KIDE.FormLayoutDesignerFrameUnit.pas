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
unit KIDE.FormLayoutDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.TreeDesignerFrameUnit, Vcl.Samples.Spin,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls,
  EF.Tree, KIDE.NodeDesignerFrameUnit, KIDE.PairsValuesFrameUnit,
  SynEdit, SynHighlighterHtml, Vcl.Buttons;

type
  TFormLayoutDesignerFrame = class(TTreeDesignerFrame)
    EditorPageControl: TPageControl;
    OptionsTabSheet: TTabSheet;
    MemoWidthLabel: TLabel;
    _MemoWidth: TSpinEdit;
    MaxFieldWidthLabel: TLabel;
    _MaxFieldWidth: TSpinEdit;
    _MinFieldWidth: TSpinEdit;
    MinFieldWidthLabel: TLabel;
    MsgTargetComboBox: TComboBox;
    MsgTargetLabel: TLabel;
    _RequiredLabelTemplate: TLabeledEdit;
    _LabelWidth: TSpinEdit;
    LabelWidthLabel: TLabel;
    LabelAlignComboBox: TComboBox;
    LabelAlignLabel: TLabel;
    _LabelSeparator: TLabeledEdit;
    UpdateLayoutAction: TAction;
    ToolButton1: TToolButton;
    UpdateLayoutToolButton: TToolButton;
    procedure UpdateLayoutActionExecute(Sender: TObject);
  private
    //procedure Preview;
  protected
    class function SuitsTree(const ATree: TEFTree): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  EF.Classes, KIDE.Utils, KIDE.Project,
  Kitto.Ext.Editors, Kitto.Ext.Form, Kitto.Metadata.Views, Kitto.Metadata.DataView,
  KIDE.DesignMetadata, KIDE.PreviewLayoutForm;

{ TLayoutDesignerFrame }

procedure TFormLayoutDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupIntegerNode('MemoWidth', LAYOUT_MEMOWIDTH);
  CleanupIntegerNode('MaxFieldWidth', LAYOUT_MAXFIELDWIDTH);
  CleanupIntegerNode('LabelWidth', FORM_LABELWIDTH);
  CleanupIntegerNode('MinFieldWidth', LAYOUT_MINFIELDWIDTH);
  CleanupTextNode('MsgTarget', LAYOUT_MSGTARGET);
  CleanupTextNode('RequiredLabelTemplate', LAYOUT_REQUIREDLABELTEMPLATE);
  CleanupTextNode('LabelAlign', 'Top');
  CleanupTextNode('LabelSeparator', ':');
end;

constructor TFormLayoutDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TFormLayoutDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  EditNode.SetString('MsgTarget', MsgTargetComboBox.Text);
  EditNode.SetString('LabelAlign', LabelAlignComboBox.Text);
end;

(*
procedure TFormLayoutDesignerFrame.Preview;
var
  LView: TKDataView;
  LViewTable: TKViewTable;
  LLayout: TKLayout;
  LControllerNode: TEFNode;
begin
  inherited;
  LLayout := EditNode as TKLayout;
  LView := GetViewOfLayout(LLayout);
  if LView <> nil then
  begin
    LViewTable := LView.MainTable;
    LControllerNode := LViewTable.FindNode('Controller');
    Assert(Assigned(LViewTable));
    Assert(Assigned(LView));
    Assert(Assigned(LControllerNode));
    if Assigned(LViewTable) then
      ShowLayout(TKDataView(LView), LViewTable, LLayout, LControllerNode);
  end;
end;
*)

procedure TFormLayoutDesignerFrame.UpdateLayoutActionExecute(Sender: TObject);
var
  LViewTable: TKViewTable;
  LLayout: TKLayout;
  I: Integer;
  LField: TKViewField;
  LNode: TEFNode;
begin
  inherited;
  LLayout := EditNode as TKLayout;
  LViewTable := GetViewTableOfLayout(LLayout, 'Form');
  if LViewTable <> nil then
  begin
    for I := 0 to LViewTable.FieldCount -1 do
    begin
      LField := LViewTable.Fields[I];
      if not LField.IsVisible then
        Continue;
      LNode := LLayout.FindChildByNameAndValue('Field', LField.AliasedName, True);
      if not Assigned(LNode) then
        LLayout.AddChild('Field', LField.AliasedName);
    end;
    Apply;
    UpdateDesigner;
  end;
end;

class function TFormLayoutDesignerFrame.SuitsTree(const ATree: TEFTree): Boolean;
begin
  Result := (ATree is TKLayout) and TKLayout(ATree).IsFormLayout;
end;

procedure TFormLayoutDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LMsgTarget, LLabelAlign: string;
begin
  inherited;
  _MemoWidth.Value := EditNode.GetInteger('MemoWidth', LAYOUT_MEMOWIDTH);
  _MaxFieldWidth.Value := EditNode.GetInteger('MaxFieldWidth', LAYOUT_MAXFIELDWIDTH);
  _MinFieldWidth.Value := EditNode.GetInteger('MinFieldWidth', LAYOUT_MINFIELDWIDTH);
  _RequiredLabelTemplate.Text :=  EditNode.GetString('RequiredLabelTemplate', LAYOUT_REQUIREDLABELTEMPLATE);
  _LabelSeparator.Text := EditNode.GetString('LabelSeparator', ':');
  _LabelWidth.Value := EditNode.GetInteger('LabelWidth', 120);

  LMsgTarget := EditNode.GetString('MsgTarget', LAYOUT_MSGTARGET);
  MsgTargetComboBox.ItemIndex := MsgTargetComboBox.Items.IndexOf(LMsgTarget);

  LLabelAlign := EditNode.GetString('LabelAlign', 'Top');
  LabelAlignComboBox.ItemIndex := LabelAlignComboBox.Items.IndexOf(LLabelAlign);
end;

initialization
  TTreeDesignerFrameRegistry.Instance.RegisterClass(TFormLayoutDesignerFrame.GetClassId, TFormLayoutDesignerFrame);

finalization
  TTreeDesignerFrameRegistry.Instance.UnregisterClass(TFormLayoutDesignerFrame.GetClassId);

end.
