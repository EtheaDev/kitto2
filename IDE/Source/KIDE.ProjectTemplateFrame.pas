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
unit KIDE.ProjectTemplateFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.BaseFrame, Vcl.ComCtrls,
  Vcl.ActnList, Vcl.ImgList, System.Actions, System.ImageList;

type
  TProjectTemplateFrame = class(TBaseFrame)
    ImageList: TImageList;
    ActionList: TActionList;
    ListView: TListView;
    procedure ListViewDblClick(Sender: TObject);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FOnDblClick: TNotifyEvent;
    FOnChange: TNotifyEvent;
    function GetCurrentTemplateName: string;
    procedure DoChange;
    procedure DoDblClick;
  public
    property CurrentTemplateName: string read GetCurrentTemplateName;
    procedure UpdateList(const ADefaultTemplateName: string);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
  end;

implementation

{$R *.dfm}

uses
  Types
  , StrUtils
  , EF.Sys.Windows
  , KIDE.Project
  , KIDE.ProjectTemplate
  , KIDE.Config
  ;

{ TProjectTemplateFrame }

function TProjectTemplateFrame.GetCurrentTemplateName: string;
begin
  if Assigned(ListView.Selected) then
    Result := ListView.Selected.Caption
  else
    Result := '';
end;

procedure TProjectTemplateFrame.ListViewDblClick(Sender: TObject);
begin
  inherited;
  DoDblClick;
end;

procedure TProjectTemplateFrame.ListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  inherited;
  DoChange;
end;

procedure TProjectTemplateFrame.UpdateList(const ADefaultTemplateName: string);
var
  LItem: TListItem;
  LTemplateName: string;
begin
  ListView.Clear;

  for LTemplateName in TProjectTemplate.InstalledTemplates do
  begin
    LItem := ListView.Items.Add;
    LItem.Caption := LTemplateName;
    { TODO : Use a different image for each template. }
    LItem.ImageIndex := 0;
  end;

  if ListView.Items.Count > 0 then
  begin
    ListView.Selected := ListView.FindCaption(0,
      IfThen(ADefaultTemplateName <> '', ADefaultTemplateName, 'Empty'),
        False, True, False);
    if ListView.Selected = nil then
      ListView.Selected := ListView.Items[0];
    ListView.ItemFocused := ListView.Selected;
    ListView.SetFocus;
  end;
  DoChange;
end;

procedure TProjectTemplateFrame.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TProjectTemplateFrame.DoDblClick;
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

end.
