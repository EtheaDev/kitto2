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
unit KIDE.DbGrid;

interface

uses
  Windows, Messages, Types,
  SysUtils, Classes, DB, ExtCtrls, StdCtrls,
  Controls, DBCtrls, DBGrids, Grids;

type
  TDbGrid = class(DBGrids.TDBGrid)
  private
    FCursorIsDefault: Boolean;
    FCheckBoxedFields: string;
    function isCheckBoxedColumn(Column : TColumn) : boolean;
    procedure ToggleBooleanField;
    function GetCheckBounds(Rect : TRect; Alignment : TAlignment) : TRect;
    function isMouseOverCheck(X, Y: Integer): boolean;
    function CanEditCell(X, Y: integer): boolean;
    procedure SetCheckBoxedFields(const Value: string);
    function isCheckBoxedField(Field: TField): boolean;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer;
      Column: TColumn; State: TGridDrawState); override;
    function CanEditModify: Boolean; override;
    procedure DblClick; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CellClick(Column: TColumn); override;
    function CanEditShow: Boolean; override;
  published
    property CheckBoxedFields: string read FCheckBoxedFields write SetCheckBoxedFields;
  end;

implementation

uses
  Variants;

{ TDbGrid }

constructor TDbGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TDbGrid.isCheckBoxedColumn(Column: TColumn): boolean;
begin
  Result := isCheckBoxedField(Column.Field);
end;

function TDbGrid.isCheckBoxedField(Field : TField): boolean;
const
  SEP = ';';
begin
  Result := (Field <> nil) and
    ((Field.DataType = ftBoolean) or
    ((FCheckBoxedFields<>'') and (Pos(SEP+UpperCase(Field.FieldName)+SEP,UpperCase(SEP+FCheckBoxedFields+SEP)) > 0)));
end;

procedure TDbGrid.CellClick(Column: TColumn);
begin
  inherited;
end;

function TDbGrid.CanEditShow: Boolean;
begin
  Result := inherited CanEditShow;
  if (Columns.Count > 0) and isCheckBoxedColumn(Columns[SelectedIndex]) then
  begin
    HideEditor;
    GetEditText(Col,Row);
  end;
end;

destructor TDbGrid.Destroy;
begin
  inherited;
end;

procedure TDbGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and (Shift = [ssLeft]) then
  begin
    if CanEditCell(X,Y) and isMouseOverCheck(X,Y) then
      ToggleBooleanField;
  end;
end;

function TDbGrid.GetCheckBounds(Rect : TRect; Alignment : TAlignment) : TRect;
var
  Check_Size : integer;
begin
  Check_Size := Rect.Bottom-Rect.Top-1;
  case Alignment of
    taLeftJustify : Result.Left := Rect.Left;
    taRightJustify : Result.Left := Rect.Right - Check_Size;
    taCenter : Result.Left := Rect.Left + ((Rect.Right-Rect.Left+1) div 2) - 8;
  end;
  Result.Right := Result.Left+Check_Size;
  Result.Top := Rect.Top + ((Rect.Bottom-Rect.Top+1) div 2) - (Check_Size div 2);
  Result.Bottom := Result.Top + Check_Size;
end;

procedure TDbGrid.ToggleBooleanField;
var
  Field : TField;
begin
  Field := SelectedField;
  if inherited CanEditModify then
    Field.AsBoolean := not Field.AsBoolean;
end;

procedure TDbGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
const
   CtrlState: array[Boolean] of integer = (DFCS_BUTTONCHECK, DFCS_BUTTONCHECK or DFCS_CHECKED);
var
   OutRect : TRect;
begin
  inherited;
  OutRect := GetCheckBounds(Rect, Column.Alignment);
  if isCheckBoxedColumn(Column) then
  begin
    if not (gdSelected in State) then
      Canvas.FillRect(Rect);
    if (VarIsNull(Column.Field.Value)) then
      DrawFrameControl(Canvas.Handle, OutRect, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_INACTIVE)
    else
      DrawFrameControl(Canvas.Handle, OutRect, DFC_BUTTON, CtrlState[Column.Field.AsBoolean]);
  end
  else
    inherited;
end;

procedure TDbGrid.KeyPress(var Key: Char);
begin
  //Toggle boolean field pressing space
  if (Key = ' ') and isCheckBoxedColumn(Columns[SelectedIndex]) then
    ToggleBooleanField;
  inherited;
end;

function TDbGrid.CanEditModify: Boolean;
begin
  if isCheckBoxedColumn(Columns[SelectedIndex]) then
    Result := False
  else
    Result := inherited CanEditModify;
end;

procedure TDbGrid.DblClick;
begin
  inherited;
  if (dgEditing in Options) and
     isCheckBoxedColumn(Columns[SelectedIndex]) then
    ToggleBooleanField;
end;

function TDbGrid.isMouseOverCheck(X, Y: Integer) : boolean;
var
  Rect : TRect;
  OutRect : TRect;
  Cell: TGridCoord;
  ColIndex : integer;
  RowIndex : integer;
begin
  Result := False;
  //Check if entering checkbox area
  Cell := MouseCoord(X, Y);
  ColIndex := Cell.X-Indicatoroffset;
  RowIndex := Cell.Y-Ord(dgtitles in Options);
  if (ColIndex >= 0) and (RowIndex >= 0) then
  begin
    if isCheckBoxedColumn(Columns[ColIndex]) then
    begin
      //Retrieve checkbox dimensions
      Rect := CellRect(Cell.X, Cell.Y);
      OutRect := GetCheckBounds(Rect,Columns[ColIndex].Alignment);
      //Check mouse-over
      if (X > OutRect.Left) and (Y > OutRect.Top) and
        (X < OutRect.Right) and (Y < OutRect.Bottom) then
      begin
        Result := True;
      end;
    end;
  end;
end;

function TDbGrid.CanEditCell(X, Y: integer) : boolean;
var
  Cell: TGridCoord;
  Column: TColumn;
  ColNum: Integer;
begin
  Result := False;
  Cell := MouseCoord(X, Y);
  if (Cell.X <> -1) and (Cell.Y <> -1) then
  begin
    ColNum := RawToDataColumn(Cell.X);
    if ColNum < 0 then
      Exit;
    Column := Columns[ColNum];
    if (dgediting in Options) and not ReadOnly and Datalink.Active and not Datalink.Readonly then
    begin
      if (DataLink.Editing and (Cell.Y = Row)) or
        (DataLink.DataSource.AutoEdit) then
      begin
        with Column do
        if (not ReadOnly) and Assigned(Field) and Field.CanModify
          and (not (Field.DataType in ftNonTextTypes) or Assigned(Field.OnSetText)) then
        begin
          Result := True;
        end;
      end;
    end;
  end;
end;

procedure TDbGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Shift=[]) then
  begin
    if (isMouseOverCheck(X,Y) and CanEditCell(X,Y)) then
    begin
      if Cursor = crDefault then
          FCursorIsDefault := True;
      Cursor := crHandPoint;
    end
    else if FCursorIsDefault then
      Cursor := crDefault;
  end;
end;

procedure TDbGrid.SetCheckBoxedFields(const Value: string);
begin
  if FCheckBoxedFields <> Value then
    FCheckBoxedFields := Value;
end;

end.
