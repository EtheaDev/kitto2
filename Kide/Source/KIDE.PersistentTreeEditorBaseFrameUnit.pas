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
unit KIDE.PersistentTreeEditorBaseFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.ComCtrls,
  Vcl.ToolWin, Vcl.ExtCtrls,
  EF.Tree,
  KIDE.TreeEditorBaseFrameUnit, KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit,
  System.Actions;

type
  TPersistentTreeEditorBaseFrame = class {abstract}(TTreeEditorBaseFrame)
    procedure ValidateActionUpdate(Sender: TObject);
    procedure ValidateActionExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
  private
    function GetEditPersistentTree: TEFPersistentTree;
    function GetOriginalPersistentTree: TEFPersistentTree;
  strict protected
    function GetEditorProperty(const AName: string): Variant; override;
    procedure Save; override;
    procedure Reload; override;
    function GetTreeClass: TEFTreeClass; override;
    procedure RefreshEditor; override;
    procedure ValidateEditor; virtual; abstract;
  public
    property EditPersistentTree: TEFPersistentTree read GetEditPersistentTree;
    property OriginalPersistentTree: TEFPersistentTree read GetOriginalPersistentTree;
  end;

implementation

{$R *.dfm}

uses
  EF.YAML;

{ TPersistentTreeEditorBaseFrame }

procedure TPersistentTreeEditorBaseFrame.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  inherited;
  ValidateAction.Visible := True;
end;

function TPersistentTreeEditorBaseFrame.GetEditorProperty(
  const AName: string): Variant;
begin
  if SameText(AName, 'ShortTitle') then
    Result := ExtractFileName(OriginalPersistentTree.PersistentName)
  else
    Result := inherited GetEditorProperty(AName);
end;

function TPersistentTreeEditorBaseFrame.GetEditPersistentTree: TEFPersistentTree;
begin
  Result := EditObject as TEFPersistentTree;
end;

function TPersistentTreeEditorBaseFrame.GetOriginalPersistentTree: TEFPersistentTree;
begin
  Result := OriginalObject as TEFPersistentTree;
end;

function TPersistentTreeEditorBaseFrame.GetTreeClass: TEFTreeClass;
begin
  Result := TEFPersistentTree;
end;

procedure TPersistentTreeEditorBaseFrame.RefreshEditor;
begin
  OriginalPersistentTree.LoadFromYamlFile(OriginalPersistentTree.PersistentFileName);
  inherited;
end;

procedure TPersistentTreeEditorBaseFrame.Save;
begin
  inherited;
  TEFYAMLWriter.SaveTree(OriginalPersistentTree, OriginalPersistentTree.PersistentFileName);
end;

procedure TPersistentTreeEditorBaseFrame.ValidateActionExecute(Sender: TObject);
begin
  inherited;
  RefreshEditor;
  ValidateEditor;
end;

procedure TPersistentTreeEditorBaseFrame.ValidateActionUpdate(Sender: TObject);
begin
  inherited;
  ValidateAction.Enabled := (GetEditPersistentTree <> nil) and not IsChanged;
end;

procedure TPersistentTreeEditorBaseFrame.Reload;
begin
  inherited;
  RefreshEditor;
end;

end.
