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
unit KIDE.SubViewDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.TreeDesignerFrameUnit,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  EF.Tree, Kitto.Metadata.Views, Vcl.StdActns, KIDE.EditNodeBaseFrameUnit;

type
  TSubViewDesignerFrame = class(TEditNodeBaseFrame)
    VCPageControl: TPageControl;
    ViewTabSheet: TTabSheet;
    ControllerTabSheet: TTabSheet;
    ViewGroupBox: TGroupBox;
    ImageNameSpeedButton: TSpeedButton;
    ImageNameImage: TImage;
    ImageNameImageLarge: TImage;
    _DisplayLabel: TLabeledEdit;
    ImageNameEdit: TLabeledEdit;
    FileOpenAction: TFileOpen;
    ControllerLabel: TLabel;
    ControllerComboBox: TComboBox;
    ViewNameEdit: TLabeledEdit;
    ViewNameSpeedButton: TSpeedButton;
    ViewNameImage: TImage;
    ViewNameImageLarge: TImage;
    FileOpenViewAction: TFileOpen;
    MobileSettingsTabSheet: TTabSheet;
    procedure FileOpenActionAccept(Sender: TObject);
    procedure ImageNameEditChange(Sender: TObject);
    procedure ViewNameEditChange(Sender: TObject);
    procedure FileOpenViewActionAccept(Sender: TObject);
    procedure FileOpenViewActionBeforeExecute(Sender: TObject);
    procedure ControllerComboBoxChange(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure VCPageControlChange(Sender: TObject);
  strict private
    function CheckControllerClass: boolean;
  private
    function GetEditView: TKView;
    function IsHomeView: Boolean;
  protected
    class function SuitsTree(const ATree: TEFTree): Boolean; override;
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
    procedure CleanupDefaultsToEditNode; override;
    property EditView: TKView read GetEditView;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  StrUtils, EF.Macros,
  KIDE.Project, KIDE.Utils, KIDE.Config,
  Kitto.Ext.Controller, Kitto.Ext.Base,
  //View classes for this designer
  Kitto.Ext.ViewPort, Kitto.Ext.Window,
  KIDE.MobileSettingsDesignerFrameUnit;

{ TDownloadFileToolDesignerFrame }

procedure TSubViewDesignerFrame.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  inherited;
  ControllerTabSheet.TabVisible := CheckControllerClass;
end;

procedure TSubViewDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('ImageName');
  CleanupTextNode('DisplayLabel');
  CleanupOrphanNode('Controller');
  if IsHomeView then
    CleanupOrphanNode('MobileSettings');
end;

procedure TSubViewDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  VCPageControl.ActivePageIndex := 0;
  EditNode.SetString('Controller', ControllerComboBox.Text);
  if EditNode is TEFNode then
    TEFNode(EditNode).AsString := ViewNameEdit.Text;
  EditNode.SetString('ImageName', ImageNameEdit.Text);
end;

procedure TSubViewDesignerFrame.FileOpenActionAccept(Sender: TObject);
var
  LFileName: string;
begin
  inherited;
  LFileName := ExtractFileName(FileOpenAction.Dialog.FileName);
  if SameText(ExtractFileExt(LFileName), '.png') then
    LFileName := Copy(LFileName, 1, length(LFileName)-4);
  ImageNameEdit.Text := LFileName;
end;

procedure TSubViewDesignerFrame.FileOpenViewActionAccept(Sender: TObject);
var
  LFileName: string;
begin
  inherited;
  LFileName := ExtractFileName(FileOpenViewAction.Dialog.FileName);
  if SameText(ExtractFileExt(LFileName), '.yaml') then
    LFileName := Copy(LFileName, 1, length(LFileName)-5);
  ViewNameEdit.Text := LFileName;
end;

procedure TSubViewDesignerFrame.FileOpenViewActionBeforeExecute(
  Sender: TObject);
begin
  inherited;
  FileOpenAction.Dialog.InitialDir := TProject.CurrentProject.Config.Views.Path;
end;

function TSubViewDesignerFrame.GetEditView: TKView;
begin
  if EditNode is TKView then
    Result := TKView(EditNode)
  else
    Result := nil;
end;

class function TSubViewDesignerFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Assert(Assigned(ANode));
  Result := IsSubViewNode(ANode);
end;

class function TSubViewDesignerFrame.SuitsTree(const ATree: TEFTree): Boolean;
begin
  Result := ATree is TKView;
end;

function TSubViewDesignerFrame.CheckControllerClass: boolean;
var
  LControllerNode: TEFNode;
begin
  LControllerNode := EditNode.FindNode('Controller');
  Result := Assigned(LControllerNode);
end;

procedure TSubViewDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LControllerClassName: string;
  LControllerNode: TEFNode;
  LFrameClass: TEditNodeBaseFrameClass;
begin
  inherited;
  VCPageControl.ActivePageIndex := 0;
  LControllerNode := EditNode.FindNode('Controller');
  if Assigned(LControllerNode) then
  begin
    LControllerClassName := GetControllerClassName(LControllerNode);
    ControllerComboBox.Text := LControllerClassName;
    if LControllerNode.AsString = '' then
      ControllerComboBox.Enabled := False;
  end;
  ImageNameEdit.Text := EditNode.GetString('ImageName');
  if EditNode is TEFNode then
    ViewNameEdit.Text := TEFNode(EditNode).AsString;
end;

procedure TSubViewDesignerFrame.VCPageControlChange(Sender: TObject);
begin
  inherited;
  if VCPageControl.ActivePage = MobileSettingsTabSheet then
  begin
    EmbedEditNodeFrame(MobileSettingsTabSheet, TMobileSettingsDesginerFrame,
      EditNode.FindNode('MobileSettings', True), True);
  end;
end;

procedure TSubViewDesignerFrame.ViewNameEditChange(Sender: TObject);
begin
  inherited;
  ShowViewImage(ViewNameEdit.Text+'_large', ViewNameImageLarge);
  ShowViewImage(ViewNameEdit.Text, ViewNameImage);
  if ViewNameEdit.Text <> '' then
  begin
    ControllerComboBox.Text := '';
  end;
  IsChanged := True;
end;

procedure TSubViewDesignerFrame.ControllerComboBoxChange(Sender: TObject);
begin
  inherited;
  if ControllerComboBox.Text <> '' then
  begin
    ViewNameEdit.Text := '';
  end;
  IsChanged := True;
end;

constructor TSubViewDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
  TKideConfig.Instance.Config.GetNode('View/Controller').GetChildValues(ControllerComboBox.Items);

end;

procedure TSubViewDesignerFrame.ImageNameEditChange(Sender: TObject);
begin
  inherited;
  ShowImage(ImageNameEdit.Text+'_large', ImageNameImageLarge);
  ShowImage(ImageNameEdit.Text, ImageNameImage);
  IsChanged := True;
end;

procedure TSubViewDesignerFrame.Init(const ANode: TEFTree);
var
  LControllerNode: TEFNode;
  LFrameClass: TEditNodeBaseFrameClass;
begin
  inherited;
  LControllerNode := ANode.FindNode('Controller');
  if Assigned(LControllerNode) then
  begin
    LFrameClass := TEditNodeFrameFractory.Instance.GetEditNodeFrameClass(LControllerNode);
    if Assigned(LFrameClass) then
      EmbedEditNodeFrame(ControllerTabSheet, LFrameClass, LControllerNode);
    MobileSettingsTabSheet.TabVisible := IsHomeView;
  end;
  if EditNode is TEFNode then
  begin
    ViewTabSheet.Caption := TEFNode(EditNode).Name;
  end
  else
  begin
    ViewNameEdit.Visible := False;
    ViewNameSpeedButton.Visible := False;
    ViewNameImage.Visible := False;
    ViewNameImageLarge.Visible := False;
  end;
end;

function TSubViewDesignerFrame.IsHomeView: Boolean;
begin
  Result := MatchText(EditNode.GetString('Controller'), ['ViewPort', 'Window']);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TSubViewDesignerFrame.GetClassId, TSubViewDesignerFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TSubViewDesignerFrame.GetClassId);

end.
