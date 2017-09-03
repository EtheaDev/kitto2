unit KIDE.PairsValuesDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.NodeDesignerFrameUnit,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.Grids,
  EF.Tree, EF.Types, KIDE.PairsValuesFrameUnit, KIDE.EditNodeBaseFrameUnit;

type
  TPairsValuesDesignerFrame = class(TNodeDesignerFrame)
    PairsValuesFrame: TPairsValuesFrame;
  private
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode(const AReset: Boolean = False); override;
  public
  end;

implementation

{$R *.dfm}

uses
  Kitto.Metadata.Models, Kitto.Metadata.DataView;

{ TPairsValuesDesignerFrame }

procedure TPairsValuesDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  ;
end;

procedure TPairsValuesDesignerFrame.DesignPanelToEditNode(const AReset: Boolean = False);
begin
  inherited;
  //PairsValuesFrame.DesignPanelToEditNode(AReset);
end;

class function TPairsValuesDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
var
  LParentNode: TEFNode;
begin
  Result := False;
  if Assigned(ANode.Parent) and (ANode.Parent is TEFNode) then
  begin
    LParentNode := TEFNode(ANode.Parent);
    Result := ((LParentNode is TKModelField) and SameText(ANode.Name, 'AllowedValues')) or
      ((LParentNode is TKViewField) and (SameText(ANode.Name, 'AllowedValues') or SameText(ANode.Name, 'Colors')));
  end;
end;

procedure TPairsValuesDesignerFrame.UpdateDesignPanel(const AForce: Boolean = False);
begin
  inherited;
//  PairsValuesFrame.UpdateDesignPanel(AForce);
end;

initialization
  TNodeDesignerFrameRegistry.Instance.RegisterClass(TPairsValuesDesignerFrame.GetClassId, TPairsValuesDesignerFrame);

finalization
  TNodeDesignerFrameRegistry.Instance.UnregisterClass(TPairsValuesDesignerFrame.GetClassId);

end.
