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
unit KIDE.PreviewLayoutForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, Generics.Collections,
  System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.BaseFormUnit, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons, Vcl.ComCtrls, KIDE.Vcl.Editors,
  Kitto.Metadata.DataView, Kitto.Types, Kitto.Metadata.Views, EF.Tree;

type
  TPreviewLayoutForm = class(TBaseForm)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  strict private
    FEditItems: TVclEditItemList;
    FIsReadOnly: Boolean;
    FLabelAlign: TVclFormFormPanelLabelAlign;
    FFormPanel: TVclFormPanel;
    FMainPagePanel: TKideEditPage;
    FFocusField: TVclFormField;
    FTabPanel: TVclTabPanel;
    function FindLayout: TKLayout;
    function LayoutContainsPageBreaks: Boolean;
    function GetDetailStyle: string;
  private
    ControllerNode: TEFNode;
    View: TKDataView;
    ViewTable: TKViewTable;
    Layout: TKLayout;
    Operation: string;
    PopUpWidth: Integer;
    PopUpHeight: Integer;
    procedure InitFlags;
    procedure InitComponents;
    procedure CreateFormPanel;
    procedure CreateEditors;
    property Config: TEFNode read ControllerNode;
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure ShowLayout(AView: TKDataView; AViewTable: TKViewTable;
  ALayout: TKLayout; AControllerNode: TEFNode; AOperation: string = 'Edit');

implementation

{$R *.dfm}

uses
  System.Types, StrUtils, Math;

procedure ShowLayout(AView: TKDataView; AViewTable: TKViewTable;
  ALayout: TKLayout; AControllerNode: TEFNode; AOperation: string = 'Edit');
var
  LWidth, LHeight: Integer;
  LPopUpWindowNode: TEFNode;
  LPreviewLayoutForm: TPreviewLayoutForm;
begin
  LPreviewLayoutForm := TPreviewLayoutForm.Create(nil);
  try
    LPreviewLayoutForm.ControllerNode := AControllerNode;
    LPreviewLayoutForm.Layout := ALayout;
    LPreviewLayoutForm.View := AView;
    LPreviewLayoutForm.ViewTable := AViewTable;
    LPreviewLayoutForm.Operation := AOperation;
    LPopUpWindowNode := AControllerNode.FindNode('PopupWindow');
    if Assigned(LPopUpWindowNode) then
    begin
      LWidth := LPopUpWindowNode.GetInteger('Width',0);
      LHeight := LPopUpWindowNode.GetInteger('Height',0);
    end
    else
    begin
      //TODO: AutoSize
      LWidth := 800;
      LHeight := 600;
    end;
    LPreviewLayoutForm.PopUpWidth := LWidth;
    LPreviewLayoutForm.PopUpHeight := LHeight;
    LPreviewLayoutForm.InitComponents;
    LPreviewLayoutForm.ShowModal;
  finally
    LPreviewLayoutForm.Free;
  end;
end;

{ TPreviewLayoutForm }

constructor TPreviewLayoutForm.Create(AOwner: TComponent);
begin
  inherited;
  ;
end;

procedure TPreviewLayoutForm.InitFlags;
var
  LLabelAlignNode: TEFNode;
begin
  if Caption = '' then
    Caption := ViewTable.DisplayLabel;

  if MatchStr(Operation, ['Add', 'Dup']) then
    FIsReadOnly := ViewTable.GetBoolean('Controller/PreventAdding')
      or View.GetBoolean('IsReadOnly')
      or ViewTable.IsReadOnly
      or Config.GetBoolean('PreventAdding')
  else //Edit or View Mode
    FIsReadOnly := ViewTable.GetBoolean('Controller/PreventEditing')
      or View.GetBoolean('IsReadOnly')
      or ViewTable.IsReadOnly
      or Config.GetBoolean('PreventEditing');

  LLabelAlignNode := ViewTable.FindNode('Controller/FormController/LabelAlign');
  if FindLayout <> nil then
    FLabelAlign := laTop
  else if Assigned(LLabelAlignNode) then
    FLabelAlign := OptionAsLabelAlign(LLabelAlignNode)
  else
    FLabelAlign := laRight; //Default to right
end;

function TPreviewLayoutForm.LayoutContainsPageBreaks: Boolean;
begin
  Result := False;
  if Assigned(Layout) then
  begin
    Result := Assigned(Layout.FindChildByPredicate(
      function (const ANode: TEFNode): Boolean
      begin
        Result := SameText(ANode.Name, 'PageBreak');
      end));
  end
end;

procedure TPreviewLayoutForm.CreateEditors;
var
  LLayoutProcessor: TVclLayoutProcessor;
begin
  FreeAndNil(FEditItems);
  FEditItems := TVclEditItemList.Create;
  LLayoutProcessor := TVclLayoutProcessor.Create;
  try
    LLayoutProcessor.ViewTable := ViewTable;
    LLayoutProcessor.FormPanel := FFormPanel;
    LLayoutProcessor.MainEditPage := FMainPagePanel;
    LLayoutProcessor.TabPanel := FTabPanel;
    LLayoutProcessor.OnNewEditItem :=
      procedure (AEditItem: IVclEditItem)
      begin
        FEditItems.Add(AEditItem.AsWinControl);
      end;
    LLayoutProcessor.ForceReadOnly := FIsReadOnly;
    if MatchStr(Operation, ['Add', 'Dup']) then
      LLayoutProcessor.Operation := eoInsert
    else
      LLayoutProcessor.Operation := eoUpdate;
    LLayoutProcessor.CreateEditors(FindLayout);
    FFocusField := LLayoutProcessor.FocusField;
  finally
    FreeAndNil(LLayoutProcessor);
  end;

  FEditItems.AllEditors(
    procedure (AEditor: IVclEditor)
    var
      LFormField: TVclFormField;
    begin
      LFormField := AEditor.AsVclFormField;
      if Assigned(LFormField) then
        LFormField.AsVclFormField.RefreshValue;
    end);

  FEditItems.AllEditItems(
    procedure (AEditor: IVclEditItem)
    begin
      AEditor.RefreshValue;
    end);
    
(*
  // Set button handlers (editors are needed by GetConfirmJSCode).
  if Assigned(FConfirmButton) then
    FConfirmButton.Handler := JSFunction(GetConfirmJSCode(ConfirmChanges));
  if Assigned(FEditButton) then
    FEditButton.Handler := JSFunction(GetConfirmJSCode(SwitchToEditMode));
  if Assigned(FCloneButton) then
    FCloneButton.Handler := JSFunction(GetConfirmJSCode(ConfirmChangesAndClone));
*)
end;

procedure TPreviewLayoutForm.CreateFormPanel;
begin
  FFormPanel := TVclFormPanel.CreateAndAddTo(Self, Self);
  FFormPanel.Align := alClient;
  //FFormPanel.Border := False;
  //FFormPanel.Header := False;
  //FFormPanel.Layout := lyFit; // Vital to avoid detail grids with zero height!
  FFormPanel.AutoScroll := False;
  FFormPanel.LabelWidth := 120;
  //FFormPanel.MonitorValid := True;
  //FFormPanel.Cls := 'x-panel-mc'; // Sets correct theme background color.
  FFormPanel.LabelAlign := FLabelAlign;
  FTabPanel := TVclTabPanel.CreateAndAddTo(Self, FFormPanel);
  //FTabPanel.Border := False;
  //FTabPanel.AutoScroll := False;
  //FTabPanel.BodyStyle := 'background:none'; // Respects parent's background color.
  //FTabPanel.DeferredRender := False;
  if ((ViewTable.DetailTableCount > 0) and SameText(GetDetailStyle, 'Tabs')) or LayoutContainsPageBreaks then
  begin
    FMainPagePanel := TKideEditPage.CreateAndAddTo(FTabPanel);
    FMainPagePanel.TabVisible := True;
    FMainPagePanel.Caption := ViewTable.DisplayLabel;
    FMainPagePanel.EditPanel := FFormPanel;
    FTabPanel.ActivePageIndex := 0;
  end
  else
  begin
    FMainPagePanel := TKideEditPage.CreateAndAddTo(FTabPanel);
    FMainPagePanel.EditPanel := FFormPanel;
    //FMainPagePanel.TabVisible := False;
    FMainPagePanel.Caption := ViewTable.DisplayLabel;
  end;
end;

function TPreviewLayoutForm.FindLayout: TKLayout;
begin
  Result := Layout;
end;

procedure TPreviewLayoutForm.FormCreate(Sender: TObject);
begin
  inherited;
  Font.name := 'Tahoma';
  Font.Height := -12;
  Font.color := clRed;
end;

procedure TPreviewLayoutForm.FormShow(Sender: TObject);
begin
  inherited;
  ClientWidth := PopUpWidth;
  ClientHeight := PopUpHeight;
end;

function TPreviewLayoutForm.GetDetailStyle: string;
begin
  Result := 'Tabs';
end;

procedure TPreviewLayoutForm.InitComponents;
begin
  InitFlags;
  CreateFormPanel;
  //CreateButtons;
  CreateEditors;
end;

end.
