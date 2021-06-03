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
unit KIDE.EditorFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.BaseFrameUnit, KIDE.Editor, EF.Tree;

type
  TEditorFrame = class {abstract}(TBaseFrame, IEditor)
  strict protected
    procedure Save; virtual;
    procedure Reload; virtual;
    function EditorMatchesSpec(const ASpec: string): Boolean; virtual; abstract;
    function EditorSuits(const ASpec: string; const AParams: TEFNode): Boolean; virtual; abstract;
    procedure RefreshEditor; virtual;
    function GetEditorProperty(const AName: string): Variant; virtual;
    function IsChanged: Boolean; virtual;
  public
    procedure DisplayEmbedded(const AParent: TWinControl); virtual;
    procedure CloseEditor(const AForce: Boolean); virtual;

    procedure InitEditor(const AParams: TEFNode); virtual;
    function IsEditorActionEnabled(const AAction: TEditorAction): Boolean; virtual;
    procedure ExecuteEditorAction(const AAction: TEditorAction);

    function AsObject: TObject;

    function GetSpec: string; virtual; abstract;
  end;

implementation

{$R *.dfm}

uses
  System.UITypes,
  EF.Localization;

{ TEditorFrame }

function TEditorFrame.AsObject: TObject;
begin
  Result := Self;
end;

procedure TEditorFrame.CloseEditor(const AForce: Boolean);
begin
  if not AForce and IsChanged then
  begin
    case MessageDlg('If you close this page, all pending changes will be lost. Do you want to save the changes before closing?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: Save;
      mrCancel: Abort;
    end;
  end;
end;

procedure TEditorFrame.DisplayEmbedded(const AParent: TWinControl);
begin
  Assert(Assigned(AParent));

  Parent := AParent;
  Align := alClient;
end;

procedure TEditorFrame.ExecuteEditorAction(const AAction: TEditorAction);
begin
  case AAction of
    eaSave: Save;
    eaReload: Reload;
  end;
end;

function TEditorFrame.GetEditorProperty(const AName: string): Variant;
begin
  Result := '';
end;

procedure TEditorFrame.InitEditor(const AParams: TEFNode);
begin
  Assert(Assigned(AParams));
end;

function TEditorFrame.IsChanged: Boolean;
begin
  Result := False;
end;

function TEditorFrame.IsEditorActionEnabled(const AAction: TEditorAction): Boolean;
begin
  Result := True;
end;

procedure TEditorFrame.RefreshEditor;
begin
end;

procedure TEditorFrame.Save;
begin
end;

procedure TEditorFrame.Reload;
begin
  if MessageDlg(_('Your changes will be lost! Are you sure you want to undo/reload content?'),
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Abort;
end;

end.
