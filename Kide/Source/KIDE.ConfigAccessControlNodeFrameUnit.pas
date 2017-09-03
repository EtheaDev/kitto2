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
unit KIDE.ConfigAccessControlNodeFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, Vcl.StdCtrls, KIDE.BaseFrameUnit, KIDE.PairsValuesFrameUnit,
  EF.Tree, Vcl.ComCtrls, SynEdit, SynHighlighterHtml, SynHighlighterSQL;

type
  TConfigAccessControlNodeFrame = class(TEditNodeBaseFrame)
    ACPageControl: TPageControl;
    ReadPermissionsCommandTextTabSheet: TTabSheet;
    ReadPermissionsCommandTextPanel: TPanel;
    ReadRolesCommandTextTabSheet: TTabSheet;
    ReadRolesCommandTextPanel: TPanel;
    ACGroupBox: TGroupBox;
    ACComboBox: TComboBox;
    procedure ACComboBoxChange(Sender: TObject);
  private
    FReadPermissionsCommandTextEdit: TSynEdit;
    FReadRolesCommandTextEdit: TSynEdit;
    FSynSQLSyn: TSynSQLSyn;
  protected
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean); override;
    procedure DesignPanelToEditNode; override;
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
  public
    procedure Init(const ANode: TEFTree); override;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Config,
  EF.Classes, KIDE.Utils, Kitto.AccessControl.DB;

{ TConfigDatabasesNodeFrame }

procedure TConfigAccessControlNodeFrame.ACComboBoxChange(Sender: TObject);
begin
  inherited;
  if SameText(ACComboBox.Text,'DB') then
  begin
    FReadPermissionsCommandTextEdit.Text := EditNode.GetString('ReadPermissionsCommandText',
      DEFAULT_READPERMISSIONCOMMANDTEXT);
    FReadRolesCommandTextEdit.Text := EditNode.GetString('ReadRolesCommandText',
      DEFAULT_READROLESCOMMANDTEXT);
  end
  else
  begin
    FReadPermissionsCommandTextEdit.Text := '';
    FReadRolesCommandTextEdit.Text := '';
  end;
  IsChanged := True;
end;

procedure TConfigAccessControlNodeFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('ReadPermissionsCommandText', DEFAULT_READPERMISSIONCOMMANDTEXT);
  CleanupTextNode('ReadPermissionsCommandText', '');
  CleanupTextNode('ReadRolesCommandText', DEFAULT_READROLESCOMMANDTEXT);
  CleanupTextNode('ReadRolesCommandText', '');
end;

constructor TConfigAccessControlNodeFrame.Create(AOwner: TComponent);
begin
  inherited;
  FSynSQLSyn := TSynSQLSyn.Create(Self);

  FReadPermissionsCommandTextEdit  := CreateSynEditor(Self, ReadPermissionsCommandTextPanel,
    '_ReadPermissionsCommandText', FSynSQLSyn, Font.Size, EditorChange);
  FReadPermissionsCommandTextEdit.Gutter.Visible := False;

  FReadRolesCommandTextEdit := CreateSynEditor(Self, ReadRolesCommandTextPanel,
    '_ReadRolesCommandText', FSynSQLSyn, Font.Size, EditorChange);
  FReadRolesCommandTextEdit.Gutter.Visible := False;

  TKideConfig.Instance.Config.GetNode('AccessControl/AccessControllers').GetChildValues(ACComboBox.Items);
end;

procedure TConfigAccessControlNodeFrame.DesignPanelToEditNode;
begin
  inherited;
  ACPageControl.ActivePageIndex := 0;

  if ACComboBox.Text <> '' then
    TEFNode(EditNode).Value := ACComboBox.Text
  else
    TEFNode(EditNode).Value := NODE_NULL_VALUE;
end;

procedure TConfigAccessControlNodeFrame.Init(const ANode: TEFTree);
begin
  inherited;
end;

class function TConfigAccessControlNodeFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := (ANode.Parent is TEFComponentConfig) and SameText(ANode.Name, 'AccessControl');
end;

procedure TConfigAccessControlNodeFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LACType: string;
begin
  inherited;
  ACPageControl.ActivePageIndex := 0;
  LACType := TEFNode(EditNode).AsString;
  ACComboBox.ItemIndex := ACComboBox.Items.IndexOf(LACType);
  ACComboBoxChange(ACComboBox);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TConfigAccessControlNodeFrame.GetClassId, TConfigAccessControlNodeFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TConfigAccessControlNodeFrame.GetClassId);

end.
