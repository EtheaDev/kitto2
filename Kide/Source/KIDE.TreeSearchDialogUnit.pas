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
unit KIDE.TreeSearchDialogUnit;

{$I SynEdit.inc}

interface

uses
  SysUtils, Classes, Controls, Forms,
  StdCtrls, ExtCtrls;

type
  TTreeSearchDialog = class(TForm)
    SearchForLabel: TLabel;
    FSearchText: TComboBox;
    FSearchOptions: TGroupBox;
    FSearchCaseSensitive: TCheckBox;
    FSearchWholeWords: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    FNodeOptions: TGroupBox;
    FSearchFromFocusedNode: TCheckBox;
    FSearchOnlyKeys: TCheckBox;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    function GetSearchCaseSensitive: boolean;
    function GetSearchFromFocusedNode: boolean;
    function GetSearchText: string;
    function GetSearchTextHistory: string;
    function GetSearchWholeWords: boolean;
    function GetSearchOnlyKeys: boolean;
    procedure SetSearchCaseSensitive(Value: boolean);
    procedure SetSearchFromFocusedNode(Value: boolean);
    procedure SetSearchText(Value: string);
    procedure SetSearchTextHistory(Value: string);
    procedure SetSearchWholeWords(Value: boolean);
    procedure SetSearchOnlyKeys(Value: boolean);
  public
    property SearchCaseSensitive: boolean read GetSearchCaseSensitive
      write SetSearchCaseSensitive;
    property SearchFromFocusedNode: boolean read GetSearchFromFocusedNode
      write SetSearchFromFocusedNode;
    property SearchText: string read GetSearchText write SetSearchText;
    property SearchTextHistory: string read GetSearchTextHistory
      write SetSearchTextHistory;
    property SearchWholeWords: boolean read GetSearchWholeWords
      write SetSearchWholeWords;
    property SearchOnlyKeys: boolean read GetSearchOnlyKeys
      write SetSearchOnlyKeys;
  end;

implementation

{$R *.DFM}

{ TTextSearchDialog }

function TTreeSearchDialog.GetSearchCaseSensitive: boolean;
begin
  Result := FSearchCaseSensitive.Checked;
end;

function TTreeSearchDialog.GetSearchFromFocusedNode: boolean;
begin
  Result := FSearchFromFocusedNode.Checked;
end;

function TTreeSearchDialog.GetSearchText: string;
begin
  Result := FSearchText.Text;
end;

function TTreeSearchDialog.GetSearchTextHistory: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FSearchText.Items.Count - 1 do
  begin
    if i >= 10 then
      break;
    if i > 0 then
      Result := Result + #13#10;
    Result := Result + FSearchText.Items[i];
  end;
end;

function TTreeSearchDialog.GetSearchWholeWords: boolean;
begin
  Result := FSearchWholeWords.Checked;
end;

function TTreeSearchDialog.GetSearchOnlyKeys: boolean;
begin
  Result := FSearchOnlyKeys.Checked;
end;

procedure TTreeSearchDialog.SetSearchCaseSensitive(Value: boolean);
begin
  FSearchCaseSensitive.Checked := Value;
end;

procedure TTreeSearchDialog.SetSearchFromFocusedNode(Value: boolean);
begin
  FSearchFromFocusedNode.Checked := Value;
end;

procedure TTreeSearchDialog.SetSearchText(Value: string);
begin
  FSearchText.Text := Value;
end;

procedure TTreeSearchDialog.SetSearchTextHistory(Value: string);
begin
  FSearchText.Items.Text := Value;
end;

procedure TTreeSearchDialog.SetSearchWholeWords(Value: boolean);
begin
  FSearchWholeWords.Checked := Value;
end;

procedure TTreeSearchDialog.SetSearchOnlyKeys(Value: boolean);
begin
  FSearchOnlyKeys.Checked := Value;
end;

{ event handlers }

procedure TTreeSearchDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  s: string;
  i: Integer;
begin
  if ModalResult = mrOK then
  begin
    s := FSearchText.Text;
    if s <> '' then
    begin
      i := FSearchText.Items.IndexOf(s);
      if i > -1 then
      begin
        FSearchText.Items.Delete(i);
        FSearchText.Items.Insert(0, s);
        FSearchText.Text := s;
      end
      else
        FSearchText.Items.Insert(0, s);
    end;
  end;
end;

end.
