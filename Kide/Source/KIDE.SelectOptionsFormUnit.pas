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
unit KIDE.SelectOptionsFormUnit;

interface

uses
  SysUtils, StdCtrls, ExtCtrls, Classes, Controls, Forms, Graphics;

type
  TSelectOptionForm = class(TForm)
    paBottom: TPanel;
    paButtons: TPanel;
    bbOK: TButton;
    bbCancel: TButton;
    gb: TRadioGroup;
    IconsRadioGroup: TRadioGroup;
    procedure FormShow(Sender: TObject);
  private
  public
    Constructor CreateSelectOption(AOwner : TComponent; ANewFont : TFont;
       AOptions : TStringList; ATitle : string; AColumns: Integer = 1;
       ANewWidth : Integer = 0; ANewHeight : Integer = 0;
       ASelectedIndex: Integer =0; AIconsStyle : Integer = 0);
  end;

  function KideSelectOption(AOptions : string; AFont : TFont;
    ATitle : string; const AHelpContext: Integer;
    var ASelectedIndex : Integer; var AIconsStyle: Integer;
    AColumns: Integer = 1; AMinWidth: Integer = 0; AMinHeigth: Integer = 0) : Boolean;

implementation

{$R *.DFM}

uses
  Math;

function KideSelectOption(AOptions : string; AFont : TFont;
  ATitle : string; const AHelpContext: Integer;
  var ASelectedIndex : Integer; var AIconsStyle: Integer;
  AColumns: Integer = 1; AMinWidth: Integer = 0; AMinHeigth: Integer = 0) : Boolean;
var
  FSelection : TSelectOptionForm;
  LList : TStringList;
  LHeight : Integer;
  LWidth : Integer;
  LMinWidth, LMinHeight: Integer;
  I : Integer;
begin
  LList := nil;
  FSelection := nil;
  Try
    LList := TStringList.Create;
    LList.Text := AOptions;
    LMinHeight := 180 + (20 * LList.Count) div AColumns;
    LHeight := Max(LMinHeight, AMinHeigth);
    LWidth := (7 * length(ATitle));
    for I := 0 to LList.Count - 1 do
      LWidth := max(LWidth, (7 * length(LList.strings[I])));
    FSelection := TSelectOptionForm.CreateSelectOption(
      nil, AFont, LList, ATitle, AColumns, LWidth, LHeight, ASelectedIndex, AIconsStyle);
    LMinWidth := Max(FSelection.paButtons.Width+20, AMinWidth);
    FSelection.Width := max(FSelection.Width, LMinWidth);
    FSelection.HelpContext := AHelpContext;
    Result := FSelection.ShowModal = mrOK;
    ASelectedIndex := FSelection.gb.ItemIndex;
    AIconsStyle := FSelection.IconsRadioGroup.ItemIndex;
  Finally
    LList.Free;
    FSelection.Free;
  End;
end;

procedure TSelectOptionForm.FormShow(Sender: TObject);
begin
  if gb.CanFocus then
    gb.SetFocus;
end;

constructor TSelectOptionForm.CreateSelectOption( AOwner : TComponent;
  ANewFont : TFont; AOptions : TStringList;
  ATitle : string; AColumns: Integer = 1;
  ANewWidth : Integer = 0; ANewHeight : Integer = 0;
  ASelectedIndex: Integer =0; AIconsStyle : Integer = 0);
begin
  inherited Create(AOwner);
  if ANewFont <> nil then
    Font := ANewFont;
  if ANewWidth <> 0 then
    Width := ANewWidth;
  if ANewHeight <> 0 then
    Height := ANewHeight;
  gb.Items.Text := AOptions.Text;
  gb.ItemIndex := ASelectedIndex;
  IconsRadioGroup.ItemIndex := AIconsStyle;
  gb.Columns := AColumns;
  self.Caption := ATitle;
end;

end.
