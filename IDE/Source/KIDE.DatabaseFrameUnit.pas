unit KIDE.DatabaseFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, Vcl.ComCtrls,
  Kitto.Config, Vcl.ActnList, Vcl.Menus;

type
  TDatabaseFrame = class(TFrame)
    DBListView: TListView;
    ImageList: TImageList;
    ActionList: TActionList;
    PopupMenu: TPopupMenu;
    AddDBAction: TAction;
    EditDBAction: TAction;
    DeleteDBAction: TAction;
    NewDatabaseConnection1: TMenuItem;
    EditDatabaseConnection1: TMenuItem;
    DeleteDatabaseConnection1: TMenuItem;
    procedure AddDBActionUpdate(Sender: TObject);
    procedure EditDBActionUpdate(Sender: TObject);
    procedure DeleteDBActionUpdate(Sender: TObject);
    procedure AddDBActionExecute(Sender: TObject);
    procedure EditDBActionExecute(Sender: TObject);
    procedure DeleteDBActionExecute(Sender: TObject);
    procedure DBListViewDblClick(Sender: TObject);
    procedure DBListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FOnChange: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    function GetCurrentDBConnectionName: string;
    procedure DoChange;
    procedure DoDblClick;
  public
    property CurrentDBConnectionName: string read GetCurrentDBConnectionName;
    procedure UpdateDBList(const AConfig: TKConfig;
      const ADefaultDBName: string);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
  end;

implementation

{$R *.dfm}

uses
  Types, StrUtils,
  KIDE.Utils;

{ TDatabaseFrame }

procedure TDatabaseFrame.AddDBActionExecute(Sender: TObject);
begin
  NotImplemented;
end;

procedure TDatabaseFrame.AddDBActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not Assigned(DBListView.Selected);
end;

procedure TDatabaseFrame.DBListViewDblClick(Sender: TObject);
begin
  DoDblClick;
end;

procedure TDatabaseFrame.DBListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  DoChange;
end;

procedure TDatabaseFrame.DeleteDBActionExecute(Sender: TObject);
begin
  NotImplemented;
end;

procedure TDatabaseFrame.DeleteDBActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(DBListView.Selected);
end;

procedure TDatabaseFrame.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDatabaseFrame.DoDblClick;
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

procedure TDatabaseFrame.EditDBActionExecute(Sender: TObject);
begin
  NotImplemented;
end;

procedure TDatabaseFrame.EditDBActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(DBListView.Selected);
end;

function TDatabaseFrame.GetCurrentDBConnectionName: string;
begin
  if Assigned(DBListView.Selected) then
    Result := DBListView.Selected.Caption
  else
    Result := '';
end;

procedure TDatabaseFrame.UpdateDBList(const AConfig: TKConfig;
  const ADefaultDBName: string);
var
  LDBNames: TStringDynArray;
  I: Integer;
  LItem: TListItem;
begin
  Assert(Assigned(AConfig));

  LDBNames := AConfig.DBConnectionNames;
  DBListView.Clear;
  for I := Low(LDBNames) to High(LDBNames) do
  begin
    LItem := DBListView.Items.Add;
    LItem.Caption := LDBNames[I];
    { TODO : Use a different image for each database type/use default images for
      default names such as Main/Development/Production/Test. }
    LItem.ImageIndex := 0;
  end;
  if DBListView.Items.Count > 0 then
  begin
    DBListView.Selected := DBListView.FindCaption(0,
      IfThen(ADefaultDBName <> '', ADefaultDBName, TKConfig.MAIN_DB_NAME),
      False, True, False);
    if DBListView.Selected = nil then
      DBListView.Selected := DBListView.Items[0];
    DBListView.ItemFocused := DBListView.Selected;
    DBListView.SetFocus;
  end;
  DoChange;
end;

end.
