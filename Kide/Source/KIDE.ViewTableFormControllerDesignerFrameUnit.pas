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
unit KIDE.ViewTableFormControllerDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit,
  EF.Tree, Vcl.Samples.Spin, Vcl.StdActns,
  KIDE.MainDataModuleUnit, KIDE.FormControllerButtonDesignerFrameUnit;

type
  TViewTableFormControllerDesignerFrame = class(TEditNodeBaseFrame)
    FormGroupBox: TGroupBox;
    ButtonScaleComboBox: TComboBox;
    ButtonScaleLabel: TLabel;
    ItemsTabControl: TTabControl;
    _KeepOpenAfterOperation: TCheckBox;
    procedure ItemsTabControlChange(Sender: TObject);
  private
    FormControllerButtonDesignerFrame: TFormControllerButtonDesignerFrame;
    function BuildButtonPages: Integer;
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure CleanupDefaultsToEditNode; override;
    procedure DesignPanelToEditNode; override;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    class function IsViewTableFormControllerNode(const ANode: TEFNode): Boolean;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  StrUtils,
  EF.Macros,
  Kitto.Ext.Controller, Kitto.Ext.Base,
  Kitto.Ext.Form,
  KIDE.Project, KIDE.Config, KIDE.MainTableControllerDesignerFrameUnit;

{ TViewTableFormControllerDesignerFrame }

function TViewTableFormControllerDesignerFrame.BuildButtonPages: Integer;
var
  I: Integer;
  LNodeName: string;
begin
  Result := 0;
  ItemsTabControl.Tabs.Clear;
  for I := 0 to EditNode.ChildCount -1 do
  begin
    LNodeName := EditNode.Children[I].Name;
    if MatchText(LNodeName, ['CloneButton', 'ConfirmButton', 'CancelButton', 'CloseButton']) then
    begin
      ItemsTabControl.Tabs.Add(LNodeName);
      FormControllerButtonDesignerFrame := EmbedEditNodeFrame(ItemsTabControl, TFormControllerButtonDesignerFrame,
        EditNode.Children[I]) as TFormControllerButtonDesignerFrame;
    end;
    Inc(Result);
  end;
  if Result = 0 then
  begin
    EmbedEditNodeFrame(ItemsTabControl, TFormControllerButtonDesignerFrame, nil);
    FormControllerButtonDesignerFrame := nil;
  end;
end;

procedure TViewTableFormControllerDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('ButtonScale', 'medium');
  CleanupOrphanNode('ConfirmButton');
  CleanupOrphanNode('CancelButton');
  CleanupOrphanNode('CloseButton');
  CleanupBooleanNode('KeepOpenAfterOperation');
  BuildButtonPages;
end;

constructor TViewTableFormControllerDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
  TKideConfig.Instance.Config.GetNode('ButtonScales').GetChildValues(ButtonScaleComboBox.Items);
end;

procedure TViewTableFormControllerDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  EditNode.SetString('ButtonScale' ,ButtonScaleComboBox.Text);
end;

procedure TViewTableFormControllerDesignerFrame.Init(const ANode: TEFTree);
begin
  inherited;
  BuildButtonPages;
end;

class function TViewTableFormControllerDesignerFrame.IsViewTableFormControllerNode(
  const ANode: TEFNode): Boolean;
begin
  Result := SameText(ANode.Name, 'FormController') and (ANode.Parent is TEFNode) and
    TMainTableControllerDesignerFrame.IsViewTableControllerNode(TEFNode(ANode.Parent));
end;

procedure TViewTableFormControllerDesignerFrame.ItemsTabControlChange(
  Sender: TObject);
var
  LNodeName: string;
  LNode: TEFNode;
begin
  inherited;
  LNodeName := ItemsTabControl.Tabs[ItemsTabControl.TabIndex];
  LNode := EditNode.FindNode(LNodeName);
  if Assigned(LNode) then
  begin
    FormControllerButtonDesignerFrame.Init(LNode);
    FormControllerButtonDesignerFrame.UpdateDesignPanel;
  end
  else
    FormControllerButtonDesignerFrame := nil;
end;

class function TViewTableFormControllerDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
begin
  Assert(Assigned(ANode));
  Result := IsViewTableFormControllerNode(ANode);
end;

procedure TViewTableFormControllerDesignerFrame.UpdateDesignPanel(
  const AForce: Boolean);
var
  LButtonScale: string;
begin
  inherited;
  Assert(Assigned(EditNode));
  LButtonScale := EditNode.GetString('ButtonScale', 'medium');
  ButtonScaleComboBox.ItemIndex := ButtonScaleComboBox.Items.IndexOf(LButtonScale);
  if BuildButtonPages > 0 then
    ItemsTabControlChange(ItemsTabControl);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TViewTableFormControllerDesignerFrame.GetClassId, TViewTableFormControllerDesignerFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TViewTableFormControllerDesignerFrame.GetClassId);

end.
