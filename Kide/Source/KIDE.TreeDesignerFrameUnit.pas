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
unit KIDE.TreeDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Tabs, Vcl.ExtCtrls,
  Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin,
  KIDE.BaseFrameUnit, KIDE.EditNodeBaseFrameUnit, KIDE.CodeEditorFrameUnit,
  EF.Types, EF.Tree, Vcl.StdCtrls;

type
  TTreeDesignerFrame = class(TEditNodeBaseFrame)
    TabSet: TTabSet;
    CodeEditorFrame: TCodeEditorFrame;
    ApplyAction: TAction;
    CancelAction: TAction;
    ToolBar: TToolBar;
    ApplyActionToolButton: TToolButton;
    CancelActionToolButton: TToolButton;
    PathPanel: TStaticText;
    procedure TabSetChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
    procedure ApplyActionExecute(Sender: TObject);
    procedure CancelActionExecute(Sender: TObject);
    procedure ApplyActionUpdate(Sender: TObject);
    procedure CancelActionUpdate(Sender: TObject);
  strict private
    procedure UpdateCodeEditor(const AForce: Boolean);
    procedure CodeEditorApply(Sender: TObject);
    procedure CodeEditorCancel(Sender: TObject);
  private
    FApplying: Boolean;
    FOnApply: TNotifyEvent;
  strict protected
    const
      TAB_DESIGN = 0;
      TAB_CODE = 1;

    ///	<summary>
    ///	  <para>Returns the corresponding physical tab index for each logical
    ///	  tab index.</para>
    ///	  <para>The default implementation returns the input value
    ///	  unchanged.</para>
    ///	</summary>
    ///	<returns>Descendants that remove tabs or change the tab order must
    ///	override this method to provide the needed translations of standard tab
    ///	indexes.</returns>
    function GetPhysicalTabIndex(const ALogicalTabIndex: Integer): Integer; virtual;

    procedure ActivateTab(const ALogicalTabIndex: Integer);

    procedure UpdatePanelVisibility(const APhysicalTabIndex: Integer);

    procedure SetInitialTab; virtual;

    procedure UpdateDesignPanel(const AForce: Boolean); override;

  protected

  public
    constructor Create(AOwner: TComponent); override;
    procedure Init(const ANode: TEFTree); override;

    procedure GUIToEditNode; override;

    ///	<summary>
    ///   Applies the changes to the node. Returns True if any
    ///	  changes were actually written to the node (in which case it fires OnApply
    ///	  as well). Also called internally (handle OnApply to be
    ///	  notified).
    /// </summary>
    function Apply: Boolean;

    ///	<summary>
    ///   Discards any pending changes in the GUI.
    /// </summary>
    procedure Cancel;

    ///	<summary>Fired by apply when changes are actually applied to the
    ///	node.</summary>
    property OnApply: TNotifyEvent read FOnApply write FOnApply;

    ///	<summary>Re-reads data from the original node, prompting the user if
    ///	there are any pending changes.</summary>
    procedure UpdateDesigner;

  end;

  TTreeDesignerFrameClass = class of TTreeDesignerFrame;

  TTreeDesignerFrameRegistry = class(TEFRegistry)
  private
    class var FInstance: TTreeDesignerFrameRegistry;
    class function GetInstance: TTreeDesignerFrameRegistry; static;
  public
    class destructor Destroy;
    class property Instance: TTreeDesignerFrameRegistry read GetInstance;

    procedure RegisterClass(const AId: string; const AClass: TTreeDesignerFrameClass);
    property Classes;
  end;

  ///	<summary>
  ///	  Queries the registry to create a specific designer frame for each node.
  //    It is friend to TTreeDesignerFrameRegistry.
  ///	</summary>
  TTreeDesignerFrameFractory = class
  private
    class var FInstance: TTreeDesignerFrameFractory;
    class function GetInstance: TTreeDesignerFrameFractory; static;
  public
    class destructor Destroy;
    class property Instance: TTreeDesignerFrameFractory read GetInstance;

    ///	<summary>
    ///   Creates a designer frame suitable for the specified Tree.
    /// </summary>
    function CreateDesignerFrame(const ANode: TEFTree; const AOwner: TComponent): TTreeDesignerFrame;
  end;

implementation

{$R *.dfm}

uses
  TypInfo,
  EF.YAML,
  KIDE.NodeDesignerFrameUnit, KIDE.DefaultTreeDesignerFrameUnit;

{ TTreeDesignerFrameRegistry }

class destructor TTreeDesignerFrameRegistry.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TTreeDesignerFrameRegistry.GetInstance: TTreeDesignerFrameRegistry;
begin
  if FInstance = nil then
    FInstance := TTreeDesignerFrameRegistry.Create;
  Result := FInstance;
end;

procedure TTreeDesignerFrameRegistry.RegisterClass(const AId: string;
  const AClass: TTreeDesignerFrameClass);
begin
  inherited RegisterClass(AId, AClass);
end;

{ TTreeDesignerFrameFractory }

function TTreeDesignerFrameFractory.CreateDesignerFrame(const ANode: TEFTree;
  const AOwner: TComponent): TTreeDesignerFrame;
var
  I: Integer;
  LClasses: TArray<TClass>;
  LClass, LClassToCreate: TTreeDesignerFrameClass;

begin
  Assert(Assigned(ANode));

  LClasses := TTreeDesignerFrameRegistry.Instance.Classes.Values.ToArray;
  LClassToCreate := nil;
  for I := Low(LClasses) to High(LClasses) do
  begin
    LClass := TTreeDesignerFrameClass(LClasses[I]);
    if LClass.SuitsTree(ANode) then
    begin
      if (LClassToCreate <> nil) then
      begin
        if (LClass.InheritsFrom(LClassToCreate)) then
          LClassToCreate := LClass;
      end
      else
        LClassToCreate := LClass;
    end;
  end;
  if LClassToCreate <> nil then
    Result := LClassToCreate.Create(AOwner)
  else
    Result := TDefaultTreeDesignerFrame.Create(AOwner); // default designer frame.
  Try
    Result.Init(ANode);
    Result.UpdateDesigner;
  Except
    Result.Free;
    raise;
  End;
end;

class destructor TTreeDesignerFrameFractory.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TTreeDesignerFrameFractory.GetInstance: TTreeDesignerFrameFractory;
begin
  if FInstance = nil then
    FInstance := TTreeDesignerFrameFractory.Create;
  Result := FInstance;
end;

{ TTreeDesignerFrame }

constructor TTreeDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
  CodeEditorFrame.Align := alClient;
  CodeEditorFrame.OnApply := CodeEditorApply;
  CodeEditorFrame.OnCancel := CodeEditorCancel;
end;

procedure TTreeDesignerFrame.Init(const ANode: TEFTree);
begin
  inherited Init(ANode);

  PathPanel.Font.Style := PathPanel.Font.Style + [fsBold];
  PathPanel.Caption := ANode.GetPath;
  PathPanel.Visible := PathPanel.Caption <> '';

  SetInitialTab;
  UpdatePanelVisibility(TabSet.TabIndex);
  { TODO : remember initial tab in MRU for each node designer type }
end;

procedure TTreeDesignerFrame.SetInitialTab;
begin
  ActivateTab(TAB_DESIGN);
end;

procedure TTreeDesignerFrame.UpdatePanelVisibility(const APhysicalTabIndex: Integer);
begin
  DesignPanel.Visible := APhysicalTabIndex = GetPhysicalTabIndex(TAB_DESIGN);
  CodeEditorFrame.Visible := APhysicalTabIndex = GetPhysicalTabIndex(TAB_CODE);
end;

procedure TTreeDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  inherited;
end;

procedure TTreeDesignerFrame.UpdateDesigner;
begin
  ChangesDisabled := True;
  try
    Assert(Assigned(EditNode));
    UpdateDesignPanel(False);
    UpdateCodeEditor(False);
  finally
    ChangesDisabled := False;
  end;
end;

procedure TTreeDesignerFrame.UpdateCodeEditor(const AForce: Boolean);
begin
  if AForce then
    CodeEditorFrame.RefreshCode(EditNode.AsYamlString)
  else
    CodeEditorFrame.Code := EditNode.AsYamlString;
end;

function TTreeDesignerFrame.Apply: Boolean;
begin
  Result := False;
  if not FApplying then
  begin
    FApplying := True;
    try
      ChangesDisabled := True;
      try
        if Assigned(FOnApply) then
          FOnApply(Self);
        Result := True;
      finally
        ChangesDisabled := False;
      end;
    finally
      FApplying := False;
    end;
  end;
end;

procedure TTreeDesignerFrame.ApplyActionExecute(Sender: TObject);
begin
  inherited;
  GUIToEditNode;
  Apply;
end;

procedure TTreeDesignerFrame.ApplyActionUpdate(Sender: TObject);
begin
  inherited;
  ApplyAction.Enabled := IsChanged;
end;

procedure TTreeDesignerFrame.TabSetChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  inherited;
  Apply;
  UpdatePanelVisibility(NewTab);
  AllowChange := True;
end;

function TTreeDesignerFrame.GetPhysicalTabIndex(
  const ALogicalTabIndex: Integer): Integer;
begin
  Result := ALogicalTabIndex;
end;

procedure TTreeDesignerFrame.GUIToEditNode;
begin
  if DesignPanel.Visible then
  begin
    inherited;
    UpdateCodeEditor(False);
  end
  else if CodeEditorFrame.Visible then
  begin
    if EditNode.AsYamlString <> CodeEditorFrame.Code then
    begin
      EditNode.AsYamlString := CodeEditorFrame.Code;
      CodeEditorFrame.RefreshCode(EditNode.AsYamlString);
      UpdateDesignPanel(False);
    end;
  end;
end;

procedure TTreeDesignerFrame.ActivateTab(const ALogicalTabIndex: Integer);
begin
  TabSet.TabIndex := GetPhysicalTabIndex(ALogicalTabIndex);
end;

procedure TTreeDesignerFrame.Cancel;
begin
  ChangesDisabled := True;
  try
    if IsChanged then
    begin
      CleanupDefaultsToEditNode;
      UpdateDesignPanel(True);
      UpdateCodeEditor(True);
    end;
  finally
    ChangesDisabled := False;
  end;
  IsChanged := False;
end;

procedure TTreeDesignerFrame.CancelActionExecute(Sender: TObject);
begin
  inherited;
  Cancel;
end;

procedure TTreeDesignerFrame.CancelActionUpdate(Sender: TObject);
begin
  inherited;
  CancelAction.Enabled := IsChanged;
end;

procedure TTreeDesignerFrame.CodeEditorApply(Sender: TObject);
begin
  GUIToEditNode;
  Apply;
end;

procedure TTreeDesignerFrame.CodeEditorCancel(Sender: TObject);
begin
  CodeEditorFrame.RefreshCode(EditNode.AsYamlString);
end;

end.
