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
unit KIDE.DataViewWizardFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.UITypes, System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.BaseWizardFormUnit, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ImgList,
  Kitto.Metadata.Models, Kitto.Metadata.Views, Kitto.Metadata.DataView,
  KIDE.Editor, System.ImageList;

type
  TDataViewTreeNode = class(TTreeNode)
  strict private
    FView: TKDataView;
  public
    property View: TKDataView read FView;
    procedure Initialize(const AView: TKDataView);
    procedure Rename(const ANewName: string);
    function CanRename: Boolean;
    function IsValid: Boolean;
  end;

  TDataViewWizardForm = class(TBaseWizardForm)
    SelectTabSheet: TTabSheet;
    SelectLeftPanel: TPanel;
    DataViewTreeView: TTreeView;
    SelectRightPanel: TPanel;
    ActionFramePanel: TPanel;
    SelectSplitter: TSplitter;
    TreeImages: TImageList;
    procedure DataViewTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DataViewTreeViewCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure DataViewTreeViewEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure DataViewTreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure DataViewTreeViewCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure DataViewTreeViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DataViewTreeViewChange(Sender: TObject; Node: TTreeNode);
  private
    FModelList: TKModelList;
    FViews: TKViews;
    FEditor: IEditor;
    procedure CreateDataViews;
    procedure CreateDataViewsFromModelList;
    procedure DisplayDataViews;
    procedure SaveDataViews;
    procedure ValidateDataViews;
    procedure UpdateObjectProperties(const ANode: TDataViewTreeNode);
  protected
    procedure InitWizard; override;
    procedure AfterEnterPage(const ACurrentPageIndex: Integer;
      const AOldPageIndex: Integer; const AGoingForward: Boolean); override;
    procedure BeforeLeavePage(const ACurrentPageIndex: Integer;
      const ANewPageIndex: Integer; const AGoingForward: Boolean); override;
    function CanGoForward: Boolean; override;
  public
    class procedure ShowDialog(const AModelList: TKModelList);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  StrUtils,
  EF.Localization, EF.Macros, EF.Sys.Windows, EF.Intf, EF.Tree,
  Kitto.Metadata.ViewBuilders,
  KIDE.Project;

const
  WIZPAGE_SELECT = 0;

{ TDataViewWizardForm }

procedure TDataViewWizardForm.ValidateDataViews;
var
  I: Integer;
  LView: TKDataView;
begin
  Assert(Assigned(TProject.CurrentProject));
  Assert(FViews.ViewCount > 0);

  for I := 0 to FViews.ViewCount - 1 do
  begin
    LView := TKDataView.Clone(FViews.Views[I]);
    if Assigned(TProject.CurrentProject.Config.Views.FindView(LView.PersistentName)) then
      raise Exception.CreateFmt(_('View %s has a duplicate name.'), [LView.PersistentName]);
  end;
end;

procedure TDataViewWizardForm.SaveDataViews;
var
  I: Integer;
  LView: TKDataView;
begin
  Assert(Assigned(TProject.CurrentProject));
  Assert(FViews.ViewCount > 0);

  for I := 0 to FViews.ViewCount - 1 do
  begin
    LView := TKDataView.Clone(FViews.Views[I]);
    TProject.CurrentProject.Config.Views.AddObject(LView);
    TProject.CurrentProject.Config.Views.SaveObject(LView);
  end;
end;

procedure TDataViewWizardForm.CreateDataViewsFromModelList;
var
  I: Integer;
  LViewBuilder: TKAutoListViewBuilder;
begin
  Assert(Assigned(FModelList));
  Assert(FModelList.Count > 0);

  for I := 0 to FModelList.Count - 1 do
  begin
    LViewBuilder := TKAutoListViewBuilder.Create;
    try
      LViewBuilder.SetString('Model', FModelList[I].ModelName);
      LViewBuilder.BuildView(FViews, FModelList[I].PluralModelName);
    except
      FreeAndNil(LViewBuilder);
      raise;
    end;
  end;
end;

procedure TDataViewWizardForm.CreateDataViews;
begin
  FViews.Open;
  if FViews.ViewCount > 0 then
  begin
    if MessageDlg(_('Unsaved views from an earlier session were found. If you want to restore them, click Yes. If you want to discard them and start anew, click No.'), mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    begin
      FViews.Close;
      DeleteTree(FViews.Path);
      FViews.Open;
      CreateDataViewsFromModelList;
    end;
  end
  else
    CreateDataViewsFromModelList;
end;

procedure TDataViewWizardForm.DisplayDataViews;
var
  I: Integer;
  LNode: TDataViewTreeNode;
  LView: TKDataView;
begin
  Assert(Assigned(FViews));

  DataViewTreeView.Items.Clear;

  DataViewTreeView.Items.BeginUpdate;
  try
    for I := 0 to FViews.ViewCount - 1 do
    begin
      LView := FViews[I] as TKDataView;
      LNode := DataViewTreeView.Items.AddChild(nil, LView.DisplayLabel) as TDataViewTreeNode;
      LNode.Initialize(LView);
    end;
    DataViewTreeView.FullExpand;
    if DataViewTreeView.Items.Count > 0 then
      DataViewTreeView.Select(DataViewTreeView.Items[0]);
  finally
    DataViewTreeView.Items.EndUpdate;
  end;
end;

procedure TDataViewWizardForm.AfterEnterPage(const ACurrentPageIndex,
  AOldPageIndex: Integer; const AGoingForward: Boolean);
begin
  inherited;
  if ACurrentPageIndex = WIZPAGE_SELECT then
  begin
    PageTitle := _('Data Views to be created');
    if AGoingForward then
    begin
      CreateDataViews;
      DisplayDataViews;
    end;
  end;
end;

procedure TDataViewWizardForm.BeforeLeavePage(const ACurrentPageIndex,
  ANewPageIndex: Integer; const AGoingForward: Boolean);
begin
  inherited;
  if (ACurrentPageIndex = WIZPAGE_SELECT) and AGoingForward then
  begin
    ValidateDataViews;
    if MessageDlg(Format(_('Data Views to be created: %d. Are you sure you want to continue?'), [FViews.ViewCount]), mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Abort;
    SaveDataViews;
  end;
end;

function TDataViewWizardForm.CanGoForward: Boolean;
begin
  if PageControl.ActivePageIndex = WIZPAGE_SELECT then
    Result := FViews.ViewCount > 0
  else
    Result := inherited CanGoForward;
end;

constructor TDataViewWizardForm.Create(AOwner: TComponent);

  function NormalizePath(const APath: string): string;
  begin
    Result := ReplaceStr(APath, DriveDelim, '$');
    Result := ReplaceStr(Result, PathDelim, '_');
  end;

begin
  inherited;
  Assert(Assigned(TProject.CurrentProject));

  FViews := TKViews.Create(TProject.CurrentProject.Config.Models);
  FViews.Path := TEFMacroExpansionEngine.Instance.Expand('%APPDATA%\KIDE\DataViewWizard\' + NormalizePath(TProject.CurrentProject.GetMRUKeyName));
  ForceDirectories(FViews.Path);
end;

procedure TDataViewWizardForm.DataViewTreeViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  inherited;
  UpdateObjectProperties(Node as TDataViewTreeNode);
end;

procedure TDataViewWizardForm.UpdateObjectProperties(const ANode: TDataViewTreeNode);
var
  LParams: TEFNode;
begin
  if Assigned(FEditor) then
  begin
    FEditor.ExecuteEditorAction(eaSave);
    FEditor.ExecuteEditorAction(eaClose);
  end;
  FreeAndNilEFIntf(FEditor);

  if Assigned(ANode) then
  begin
    LParams := TEFNode.Create;
    try
      LParams.SetObject('Object', ANode.View);
      LParams.SetString('ObjectFileName', ANode.View.PersistentFileName);
      LParams.SetString('MRURootKeyName', 'DataViewWizardTreeEditor');
      FEditor := TEditorFactory.Instance.CreateEditor(ANode.View.PersistentFileName, LParams, Self);
      try
        FEditor.InitEditor(LParams);
        FEditor.DisplayEmbedded(ActionFramePanel);
      except
        FreeAndNilEFintf(FEditor);
        raise;
      end;
    finally
      FreeAndNil(LParams);
    end;
  end;
end;

procedure TDataViewWizardForm.DataViewTreeViewCreateNodeClass(
  Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  inherited;
  NodeClass := TDataViewTreeNode;
end;

procedure TDataViewWizardForm.DataViewTreeViewCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  inherited;
  DefaultDraw := True;
  if State = [] then
  begin
    if not (Node as TDataViewTreeNode).IsValid then
      (Sender as TTreeView).Canvas.Font.Color := clRed;
  end;
end;

procedure TDataViewWizardForm.DataViewTreeViewEdited(Sender: TObject;
  Node: TTreeNode; var S: string);
begin
  inherited;
  (Node as TDataViewTreeNode).Rename(S);
  (Sender as TTreeView).Repaint;
end;

procedure TDataViewWizardForm.DataViewTreeViewEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
begin
  inherited;
  AllowEdit := (Node as TDataViewTreeNode).CanRename;
end;

procedure TDataViewWizardForm.DataViewTreeViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_F2) and (Shift = []) and Assigned((Sender as TTreeView).Selected) then
    (Sender as TTreeView).Selected.EditText;
end;

procedure TDataViewWizardForm.DataViewTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbRight then
    DataViewTreeView.Selected := DataViewTreeView.GetNodeAt(X, Y);
end;

destructor TDataViewWizardForm.Destroy;
begin
  FViews.Close;
  DeleteTree(FViews.Path);
  FreeAndNil(FViews);
  inherited;
end;

procedure TDataViewWizardForm.InitWizard;
begin
  inherited;
  Assert(Assigned(FModelList));
end;

class procedure TDataViewWizardForm.ShowDialog(const AModelList: TKModelList);
var
  LForm: TDataViewWizardForm;
begin
  LForm := TDataViewWizardForm.Create(Application);
  try
    LForm.FModelList := AModelList;
    LForm.ShowModal;
  finally
    FreeAndNil(LForm);
  end;
end;

{ TDataViewTreeNode }

function TDataViewTreeNode.CanRename: Boolean;
begin
  Result := True;
end;

procedure TDataViewTreeNode.Initialize(const AView: TKDataView);
begin
  Assert(Assigned(AView));

  FView := AView;
end;

function TDataViewTreeNode.IsValid: Boolean;
begin
  Assert(Assigned(FView));

  Result := True;

  if Assigned(TProject.CurrentProject.Config.Views.FindView(FView.PersistentName)) then
    Result := False;
end;

procedure TDataViewTreeNode.Rename(const ANewName: string);
begin
  Assert(Assigned(FView));

  FView.PersistentName := ANewName;
end;

end.
