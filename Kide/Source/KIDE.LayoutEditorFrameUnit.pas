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
unit KIDE.LayoutEditorFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.MetadataEditorBaseFrameUnit,
  Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, KIDE.BaseFrameUnit, Kitto.Metadata.Views,
  KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, EF.Tree, System.Actions;

type
  TLayoutEditorFrame = class(TMetadataEditorBaseFrame)
  private
    function GetEditLayout: TKLayout;
    property EditLayout: TKLayout read GetEditLayout;
  strict protected
    function GetTreeClass: TEFTreeClass; override;
    function GetEditorProperty(const AName: string): Variant; override;
    function GetPreviewCommand: string; override;
    procedure ValidateEditor; override;
  public
    procedure InitEditor(const AParams: TEFNode); override;
  end;

implementation

{$R *.dfm}

uses
  KITTO.Metadata.DataView,
  KIDE.Editor, KIDE.DesignMetadata, KIDE.LayoutValidator;

{ TLayoutEditorFrame }

function TLayoutEditorFrame.GetEditLayout: TKLayout;
begin
  Result := EditMetadata as TKLayout;
end;

function TLayoutEditorFrame.GetEditorProperty(const AName: string): Variant;
begin
  if SameText(AName, 'ImageIndex') then
    Result := 8
  else
    Result := inherited GetEditorProperty(AName);
end;

function TLayoutEditorFrame.GetPreviewCommand: string;
var
  LViewTable: TKViewTable;
  LLayoutName: string;
begin
  Result := inherited GetPreviewCommand;
  LLayoutName := Copy(EditLayout.PersistentName,length(LLayoutName)-4,5);
  if SameText(LLayoutName,'_Form') then
  begin
    LViewTable := GetViewTableOfLayout(EditLayout, LLayoutName);
    if Assigned(LViewTable) then
      Result := Format('view=%s&action=%s', [LViewTable.View.PersistentName, 'New']);
  end;
end;

function TLayoutEditorFrame.GetTreeClass: TEFTreeClass;
begin
  Result := TKLayout;
end;

procedure TLayoutEditorFrame.InitEditor(const AParams: TEFNode);
begin
  inherited;
end;

procedure TLayoutEditorFrame.ValidateEditor;
var
  LLayoutValidator: TLayoutValidator;
begin
  LLayoutValidator := TLayoutValidator.Create;
  try
    LLayoutValidator.ValidateLayouts(EditLayout);
  finally
    FreeAndNil(LLayoutValidator);
  end;
end;

initialization
  TEditorRegistry.Instance.RegisterClass(TLayoutEditorFrame.ClassName, TLayoutEditorFrame);

finalization
  TEditorRegistry.Instance.UnregisterClass(TLayoutEditorFrame.ClassName);

end.
