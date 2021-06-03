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
unit KIDE.BaseForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TBaseForm = class(TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  strict protected
    procedure StorePositionAndSize;
    procedure RestorePositionAndSize;
    function GetMRURootKeyName: string; virtual;
  public
    procedure CreateParams(var Params: TCreateParams); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.MRUOptions;

procedure TBaseForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
end;

procedure TBaseForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StorePositionAndSize;
end;

procedure TBaseForm.StorePositionAndSize;
begin
  TMRUOptions.Instance.SetInteger(GetMRURootKeyName + '/WindowState', Ord(WindowState));
  if WindowState = wsNormal then
  begin
    TMRUOptions.Instance.SetInteger(GetMRURootKeyName + '/Left', Left);
    TMRUOptions.Instance.SetInteger(GetMRURootKeyName + '/Top', Top);
    TMRUOptions.Instance.SetInteger(GetMRURootKeyName + '/Width', Width);
    TMRUOptions.Instance.SetInteger(GetMRURootKeyName + '/Height', Height);
  end;
  TMRUOptions.Instance.Save;
end;

procedure TBaseForm.FormShow(Sender: TObject);
begin
  RestorePositionAndSize;
end;

procedure TBaseForm.RestorePositionAndSize;
var
  LWindowState: TWindowState;
begin
  LWindowState := TWindowState(TMRUOptions.Instance.GetInteger(GetMRURootKeyName + '/WindowState', Ord(WindowState)));
  if LWindowState in [wsNormal, wsMaximized] then
    WindowState := LWindowState
  else
    WindowState := wsNormal;
  if WindowState = wsNormal then
  begin
    Left := TMRUOptions.Instance.GetInteger(GetMRURootKeyName + '/Left', Left);
    Top := TMRUOptions.Instance.GetInteger(GetMRURootKeyName + '/Top', Top);
    Width := TMRUOptions.Instance.GetInteger(GetMRURootKeyName + '/Width', Width);
    Height := TMRUOptions.Instance.GetInteger(GetMRURootKeyName + '/Height', Height);
  end;
end;

function TBaseForm.GetMRURootKeyName: string;
begin
  Result := Name;
end;

end.
