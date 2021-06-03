{-------------------------------------------------------------------------------
   Copyright 2012-2021 Ethea S.r.l.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-------------------------------------------------------------------------------}
unit KIDE.HtmlPanelControllerDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.EditNodeBaseFrameUnit,
  Vcl.ExtCtrls, Vcl.Tabs, Vcl.Samples.Spin,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Buttons,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, KIDE.PanelControllerDesignerFrameUnit,
  EF.Tree, KIDE.ControllerDesignerFrameUnit,
  SynEdit, SynHighlighterHtml;

type
  THtmlPanelControllerDesignerFrame = class(TPanelControllerDesignerFrame)
    HtmlPanelControllerGroupBox: TGroupBox;
    HtmlLabel: TLabel;
    _FileName: TLabeledEdit;
    HtmlPanel: TPanel;
  strict private
    FEdit: TSynEdit;
    FSynHTMLSyn: TSynHTMLSyn;
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
  protected
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  EF.Macros,
  Kitto.Ext.Base, Kitto.Ext.HtmlPanel,
  KIDE.Utils;

{ TDownloadFileToolDesignerFrame }

procedure THtmlPanelControllerDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('FileName');
  CleanupTextNode('Html');
end;

constructor THtmlPanelControllerDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
  FSynHTMLSyn := TSynHTMLSyn.Create(Self);
  FEdit := CreateSynEditor(Self, HtmlPanel, '_Html', FSynHTMLSyn, Font.Size, EditorChange);
  FEdit.Gutter.Visible := False;
end;

class function THtmlPanelControllerDesignerFrame.SuitsNode(
  const ANode: TEFNode): Boolean;
var
  LControllerClass: TClass;
begin
  Assert(Assigned(ANode));
  LControllerClass := GetControllerClass(ANode);
  Result := Assigned(LControllerClass) and
    LControllerClass.InheritsFrom(TKExtHtmlPanelController);
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(THtmlPanelControllerDesignerFrame.GetClassId, THtmlPanelControllerDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(THtmlPanelControllerDesignerFrame.GetClassId);

end.
