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
unit KIDE.FileImageBaseFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, Vcl.ExtCtrls,
  Vcl.ToolWin, Vcl.ComCtrls, Vcl.ActnList, EF.Tree, EF.YAML,
  KIDE.FileTree, KIDE.EditorFrameUnit, KIDE.BaseFrameUnit, KIDE.ImageViewerFrameUnit, KIDE.Editor,
  KIDE.TreeDesignerFrameUnit, KIDE.NodeDesignerFrameUnit, System.Actions;

type
  TFileImageBaseFrame = class {abstract}(TEditorFrame)
    ClientPanel: TPanel;
    ImageViewerFrame: TImageViewerFrame;
  strict private
    FFileNodeHandler: TFileNodeHandler;
  private
    procedure InitImageViewer;
  strict protected
    function EditorMatchesSpec(const ASpec: string): Boolean; override;
    function EditorSuits(const ASpec: string; const AParams: TEFNode): Boolean; override;
    function GetEditorProperty(const AName: string): Variant; override;
    procedure Reload; override;
    procedure RefreshEditor; override;
    property FileNodeHandler: TFileNodeHandler read FFileNodeHandler;
    function GetTreeClass: TEFTreeClass; virtual; abstract;
  public
    destructor Destroy; override;
    procedure InitEditor(const AParams: TEFNode); override;
    function GetSpec: string; override;
  end;

implementation

{$R *.dfm}

uses
  EF.StrUtils, EF.Sys.Windows,
  KIDE.MRUOptions, KIDE.DesignMetadata, KIDE.MainDataModuleUnit, KIDE.Project;

{ TFileEditorBaseFrame }

destructor TFileImageBaseFrame.Destroy;
begin
  inherited;
end;

procedure TFileImageBaseFrame.InitEditor(const AParams: TEFNode);
begin
  inherited;
  FFileNodeHandler := AParams.GetObject('Object') as TFileNodeHandler;
  Assert(Assigned(FFileNodeHandler));

  Assert(DirectoryExists(ExtractFilePath(FFileNodeHandler.FileName)));

  InitImageViewer;
end;

procedure TFileImageBaseFrame.InitImageViewer;
begin
  ImageViewerFrame.LoadFromFile(FFileNodeHandler.FileName);
end;

procedure TFileImageBaseFrame.Reload;
begin
  inherited;
  ImageViewerFrame.LoadFromFile(FileNodeHandler.FileName);
end;

procedure TFileImageBaseFrame.RefreshEditor;
begin
  inherited;
  InitImageViewer;
end;

function TFileImageBaseFrame.EditorMatchesSpec(const ASpec: string): Boolean;
begin
  Result := SameFileName(ASpec, FFileNodeHandler.FileName);
end;

function TFileImageBaseFrame.EditorSuits(const ASpec: string;
  const AParams: TEFNode): Boolean;
var
  LFileExt: string;
  LObject: TObject;
begin
  Result := False;
  LObject := AParams.GetObject('Object');
  if LObject.InheritsFrom(TFileNodeHandler) then
  begin
    LFileExt := ExtractFileExt(TFileNodeHandler(LObject).FileName);
    Result := MatchStr(LFileExt, ['.png','.jpg','.gif','.bmp']);
  end;
end;

function TFileImageBaseFrame.GetEditorProperty(const AName: string): Variant;
begin
  if SameText(AName, 'ShortTitle') then
    Result := ExtractFileName(FFileNodeHandler.FileName)
  else if SameText(AName, 'LongTitle') then
    Result := FFileNodeHandler.FileName
  else if SameText(AName, 'ImageIndex') then
    Result := GetFileImageIndex(ExtractFileFormat(FFileNodeHandler.FileName))
  else
    Result := inherited GetEditorProperty(AName);
end;

function TFileImageBaseFrame.GetSpec: string;
begin
  Result := FFileNodeHandler.FileName;
end;

initialization
  TEditorRegistry.Instance.RegisterClass(TFileImageBaseFrame.ClassName, TFileImageBaseFrame);

finalization
  TEditorRegistry.Instance.UnregisterClass(TFileImageBaseFrame.ClassName);

end.
