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
unit KIDE.FileEditorBaseFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, Vcl.ExtCtrls,
  Vcl.ToolWin, Vcl.ComCtrls, Vcl.ActnList, EF.Tree, EF.YAML,
  KIDE.FileTree, KIDE.EditorFrameUnit, KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, KIDE.Editor,
  KIDE.TreeDesignerFrameUnit, KIDE.NodeDesignerFrameUnit, System.Actions;

type
  TFileEditorBaseFrame = class {abstract}(TEditorFrame)
    ClientPanel: TPanel;
    CodeEditorFrame: TCodeEditorFrame;
  strict private
    FEditCode: string;
    FFileNodeHandler: TFileNodeHandler;
  private
    procedure UpdateCodeEditor(const AForce: Boolean);
    procedure InitEditCode;
  strict protected
    function EditorMatchesSpec(const ASpec: string): Boolean; override;
    function EditorSuits(const ASpec: string; const AParams: TEFNode): Boolean; override;
    function GetEditorProperty(const AName: string): Variant; override;
    procedure Save; override;
    procedure Reload; override;
    procedure RefreshEditor; override;
    property FileNodeHandler: TFileNodeHandler read FFileNodeHandler;
    property EditCode: string read FEditCode;
    function IsChanged: Boolean; override;
    function GetTreeClass: TEFTreeClass; virtual; abstract;
    function GetPreviewCommand: string; virtual;
  public
    destructor Destroy; override;
    procedure InitEditor(const AParams: TEFNode); override;
    function IsEditorActionEnabled(const AAction: TEditorAction): Boolean; override;
    function GetSpec: string; override;
  end;

implementation

{$R *.dfm}

uses
  EF.StrUtils, EF.Sys.Windows,
  KIDE.MRUOptions, KIDE.DesignMetadata, KIDE.MainDataModuleUnit, KIDE.Project;

{ TFileEditorBaseFrame }

destructor TFileEditorBaseFrame.Destroy;
begin
  inherited;
end;

procedure TFileEditorBaseFrame.InitEditor(const AParams: TEFNode);
begin
  inherited;
  FFileNodeHandler := AParams.GetObject('Object') as TFileNodeHandler;
  Assert(Assigned(FFileNodeHandler));

  Assert(DirectoryExists(ExtractFilePath(FFileNodeHandler.FileName)));

  InitEditCode;
  UpdateCodeEditor(True);
end;

procedure TFileEditorBaseFrame.InitEditCode;
begin
  CodeEditorFrame.LoadFromFile(FFileNodeHandler.FileName, TEncoding.UTF8);
  FEditCode := CodeEditorFrame.Code;
end;

function TFileEditorBaseFrame.IsChanged: Boolean;
begin
  Result := (FEditCode <> CodeEditorFrame.Code);
end;

function TFileEditorBaseFrame.IsEditorActionEnabled(
  const AAction: TEditorAction): Boolean;
begin
  if AAction in [eaSave, eaReload] then
    Result := IsChanged
  else
    Result := inherited IsEditorActionEnabled(AAction);
end;

procedure TFileEditorBaseFrame.Save;
begin
  inherited;
  CodeEditorFrame.SaveToFile(FileNodeHandler.FileName, TEncoding.UTF8);
  FEditCode := CodeEditorFrame.Code;
end;

procedure TFileEditorBaseFrame.Reload;
begin
  inherited;
  CodeEditorFrame.LoadFromFile(FileNodeHandler.FileName, TEncoding.UTF8);
  FEditCode := CodeEditorFrame.Code;
end;

procedure TFileEditorBaseFrame.RefreshEditor;
begin
  inherited;
  InitEditCode;
  UpdateCodeEditor(True);
end;

procedure TFileEditorBaseFrame.UpdateCodeEditor(const AForce: Boolean);
begin
  if AForce then
    CodeEditorFrame.RefreshCode(FEditCode)
  else
    CodeEditorFrame.Code := FEditCode;
end;

function TFileEditorBaseFrame.EditorMatchesSpec(const ASpec: string): Boolean;
begin
  Result := SameFileName(ASpec, FFileNodeHandler.FileName);
end;

function TFileEditorBaseFrame.EditorSuits(const ASpec: string;
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
    Result := MatchStr(LFileExt, ['.css', '.js', '.json', '.yaml', '.html']);
  end;
end;

function TFileEditorBaseFrame.GetEditorProperty(const AName: string): Variant;
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

function TFileEditorBaseFrame.GetPreviewCommand: string;
begin
  Result := '';
end;

function TFileEditorBaseFrame.GetSpec: string;
begin
  Result := FFileNodeHandler.FileName;
end;

initialization
  TEditorRegistry.Instance.RegisterClass(TFileEditorBaseFrame.ClassName, TFileEditorBaseFrame);

finalization
  TEditorRegistry.Instance.UnregisterClass(TFileEditorBaseFrame.ClassName);

end.
