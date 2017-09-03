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
unit KIDE.NodeDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.BaseFrameUnit, Vcl.Tabs,
  Vcl.ExtCtrls, EF.Types, EF.Tree, System.Actions,
  Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin,
  KIDE.CodeEditorFrameUnit, KIDE.TreeDesignerFrameUnit, Vcl.StdCtrls,
  KIDE.EditNodeBaseFrameUnit;

type
  TNodeDesignerFrame = class(TTreeDesignerFrame)
  strict private
    FEditNodeFrame: TEditNodeBaseFrame;
  private
    class function GetEditNodeFrameClass(ANode: TEFNode): TEditNodeBaseFrameClass;
    function GetEditNode: TEFNode;
    procedure SetEditNodeFrame(const Value: TEditNodeBaseFrame);
  protected
    ///	<summary>
    ///   Returns True if the class is suitable for designing the
    ///	  specified node.
    /// </summary>
    class function SuitsTree(const ATree: TEFTree): Boolean; override;
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
  public
    procedure Init(const ANode: TEFTree); override;
    constructor Create(AOwner: TComponent); override;
    property EditNode: TEFNode read GetEditNode;
    property EditNodeFrame: TEditNodeBaseFrame read FEditNodeFrame write SetEditNodeFrame;
  end;

  TNodeDesignerFrameClass = class of TNodeDesignerFrame;

  TNodeDesignerFrameRegistry = class(TEFRegistry)
  private
    class var FInstance: TNodeDesignerFrameRegistry;
    class function GetInstance: TNodeDesignerFrameRegistry; static;
  public
    class destructor Destroy;
    class property Instance: TNodeDesignerFrameRegistry read GetInstance;

    procedure RegisterClass(const AId: string; const AClass: TNodeDesignerFrameClass);
    property Classes;
  end;

  ///	<summary>
  ///	  Queries the registry to create a specific designer frame for each node.
  //    It is friend to TNodeDesignerFrameRegistry.
  ///	</summary>
  TNodeDesignerFrameFractory = class
  private
    class var FInstance: TNodeDesignerFrameFractory;
    class function GetInstance: TNodeDesignerFrameFractory; static;
  public
    class destructor Destroy;
    class property Instance: TNodeDesignerFrameFractory read GetInstance;

    ///	<summary>
    ///   Creates a designer frame suitable for the specified node.
    /// </summary>
    function CreateDesignerFrame(const ANode: TEFNode; const AOwner: TComponent): TNodeDesignerFrame; overload;
  end;

implementation

{$R *.dfm}

uses
  TypInfo,
  EF.YAML, EF.Classes,
  KIDE.DefaultNodeDesignerFrameUnit;

{ TNodeDesignerFrameRegistry }

class destructor TNodeDesignerFrameRegistry.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TNodeDesignerFrameRegistry.GetInstance: TNodeDesignerFrameRegistry;
begin
  if FInstance = nil then
    FInstance := TNodeDesignerFrameRegistry.Create;
  Result := FInstance;
end;

procedure TNodeDesignerFrameRegistry.RegisterClass(const AId: string;
  const AClass: TNodeDesignerFrameClass);
begin
  inherited RegisterClass(AId, AClass);
end;

{ TNodeDesignerFrameFractory }

function TNodeDesignerFrameFractory.CreateDesignerFrame(const ANode: TEFNode;
  const AOwner: TComponent): TNodeDesignerFrame;
var
  I: Integer;
  LClasses: TArray<TClass>;
  LClass, LClassToCreate: TNodeDesignerFrameClass;

begin
  Assert(Assigned(ANode));

  LClasses := TNodeDesignerFrameRegistry.Instance.Classes.Values.ToArray;
  LClassToCreate := nil;
  for I := Low(LClasses) to High(LClasses) do
  begin
    LClass := TNodeDesignerFrameClass(LClasses[I]);
    if LClass.SuitsNode(ANode) then
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
  begin
    Result := LClassToCreate.Create(AOwner);
  end
  else
    Result := TDefaultNodeDesignerFrame.Create(AOwner); // default designer frame.
  Try
    Result.Init(ANode);
    Result.UpdateDesigner;
  Except
    Result.Free;
    raise;
  End;
end;

class destructor TNodeDesignerFrameFractory.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TNodeDesignerFrameFractory.GetInstance: TNodeDesignerFrameFractory;
begin
  if FInstance = nil then
    FInstance := TNodeDesignerFrameFractory.Create;
  Result := FInstance;
end;

{ TNodeDesignerFrame }

class function TNodeDesignerFrame.SuitsTree(const ATree: TEFTree): Boolean;
begin
  if Assigned(ATree) and (ATree is TEFNode) then
    Result := SuitsNode(TEFNode(ATree))
  else
    Result := False;
end;

constructor TNodeDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
  ;
end;

function TNodeDesignerFrame.GetEditNode: TEFNode;
begin
  Result := inherited EditNode as TEFNode;
end;

class function TNodeDesignerFrame.GetEditNodeFrameClass(ANode: TEFNode): TEditNodeBaseFrameClass;
begin
  Result := TEditNodeFrameFractory.Instance.GetEditNodeFrameClass(ANode);
end;

procedure TNodeDesignerFrame.Init(const ANode: TEFTree);
var
  LNodeFrameClass: TEditNodeBaseFrameClass;
begin
  if (ANode is TEFNode) and not Assigned(FEditNodeFrame) then
  begin
    LNodeFrameClass := GetEditNodeFrameClass(TEFNode(ANode));
    if Assigned(LNodeFrameClass) then
    begin
      FEditNodeFrame := LNodeFrameClass.Create(Self);
      FEditNodeFrame.Parent := DesignPanel;
      FEditNodeFrame.Align := alClient;
      FEditNodeFrame.Init(TEFNode(ANode));
    end;
  end
  else
    FEditNodeFrame.Init(TEFNode(ANode));
  inherited;
end;

procedure TNodeDesignerFrame.SetEditNodeFrame(const Value: TEditNodeBaseFrame);
begin
  FEditNodeFrame := Value;
end;

class function TNodeDesignerFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := Assigned(GetEditNodeFrameClass(ANode));
end;

initialization
  TNodeDesignerFrameRegistry.Instance.RegisterClass(TNodeDesignerFrame.GetClassId, TNodeDesignerFrame);

finalization
  TNodeDesignerFrameRegistry.Instance.UnregisterClass(TNodeDesignerFrame.GetClassId);

end.
