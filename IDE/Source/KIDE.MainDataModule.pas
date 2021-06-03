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
unit KIDE.MainDataModule;

interface

uses
  SysUtils
  , Classes
{$IFDEF MADEXCEPT}
  , madExcept
{$ENDIF}
  , ImgList
  , Controls
  , ImageList
  ;

const
  FOLDER_PICTURE = 0;
  CONFIG_PICTURE = 3;
  VIEW_PICTURE  = 4;
  MODEL_PICTURE = 5;
  MODEL_WIZARD_PICTURE = 7;
  LAYOUT_PICTURE  = 8;
  LANGUAGES_PICTURE = 10;
  EDIT_FILE = 11;
  VALIDATE_PICTURE = 15;
  NEW_MODEL_PICTURE = 50;
  DATA_WIZARD_PICTURE = 22;
  NEW_VIEW_PICTURE = 51;
  NEW_LAYOUT_PICTURE = 52;
  FIELD_PICTURE  = 30;
  FIELD_PK_PICTURE = 87;
  FIELD_REF_PICTURE = 78;
  DESIGN_PICTURE = 25;
  GENERIC_PICTURE  = 26;
  LABEL_PICTURE  = 34;
  MEMO_FIELD_PICTURE  = 35;
  DATA_FIELD_PICTURE  = 36;
  DATETIME_FIELD = 36;
  TIME_FIELD_PICTURE  = 37;
  STRING_FIELD_PICTURE  = 38;
  INTEGER_FIELD_PICTURE  = 39;
  BOOLEAN_FIELD_PICTURE  = 40;
  NUMERIC_FIELD_PICTURE  = 41;
  DATABASE_PICTURE  = 42;
  ADD_CHILD = 43;
  DELETE_NODE = 44;
  NEW_CONFIG_PICTURE = 49;
  FASTCGI_PICTURE = 12;
  EDIT_STYLE = 62;
  EDIT_SCRIPT = 63;
  FILE_TXT = 18;
  FILE_HTML = 64;
  FILE_IMAGE = 20;
  FILE_UNKNOWN = 21;
  FILE_SCRIPT = 65;
  COLOR_PALETTE = 57;
  IMAGE_UNDO = 66;
  KIDE_ICON = 68;
  HOME_VIEW = 69;
  FORM_VIEW = 70;
  LIST_VIEW = 71;
  TREE_VIEW = 72;
  LAYOUT_FORM = 73;
  LAYOUT_GRID = 74;
  KITTO_ICON = 75;
  HELP_PICTURE = 76;
  WIKI_PICTURE = 77;
  BULB_PICTURE = 79;
  DELETE_CONFIG_PICTURE = 45;
  DELETE_MODEL_PICTURE = 46;
  DELETE_VIEW_PICTURE = 47;
  DELETE_LAYOUT_PICTURE = 48;
  IMAGE_BRICK = 80;
  ARROW_LEFT_PICTURE = 81;
  ARROW_RIGHT_PICTURE = 82;
  AUTH_PICTURE  = 83;
  UAC_PICTURE  = 84;
  EXT_PICTURE  = 85;

type
  TIconsStyle = (it16Color, it18Black, it24Black);

  TMainDataModule = class(TDataModule)
    ToolbarImages: TImageList;
    Images18Black: TImageList;
    Images24Black: TImageList;
    Images: TImageList;
    Images16Color: TImageList;
    ToolbarImages16Color: TImageList;
    ToolbarImages24Black: TImageList;
    ToolbarImages18Black: TImageList;
    procedure DataModuleCreate(Sender: TObject);
  private
    FIconsStyle: TIconsStyle;
    procedure SetIconsStyle(AIconsStyle: TIconsStyle);
    procedure UpdateIconsStyle;
  public
{$IFDEF MADEXCEPT}
    procedure ShowMadExcept(const exceptIntf: IMEException;
      var handled: boolean);
{$ENDIF}
    property IconsStyle: TIconsStyle read FIconsStyle write SetIconsStyle;

  end;

var
  MainDataModule: TMainDataModule;

function GetFileImageIndex(const AExt: string): Integer;

implementation

{$R *.dfm}

uses
  System.StrUtils, KIDE.MRUOptions;

{$IFDEF MADEXCEPT}
const
  EDATABASEERRORDESC = 'Error accessing database';
  EFILERERROR = 'Error in input/output file operation';
  EGENERICERROR = 'Error';
  EACCESSVIOLDESC = 'Unexpected fatal error in application';
  ERR_ACCES_VIOL_DESC = 'Unexpected error.'+sLineBreak+sLineBreak+'%s'+sLineBreak+sLineBreak+
                        'It is reccomended to exit and reexecute the program.'+sLineBreak+
                        'If this error persists, please contact our technical support.';

function GetErrorClassNameDesc(const ExceptionClassName : string;
  IsAccessViolation: boolean) : string;
begin
  Result := '';
  if pos('Database', ExceptionClassName) > 0 then
    Result := EDATABASEERRORDESC
  else if pos('FilerError', ExceptionClassName) > 0 then
    Result := EFILERERROR
  else if IsAccessViolation then
    Result := EACCESSVIOLDESC
  else
    Result := EGENERICERROR;
end;

procedure TMainDataModule.ShowMadExcept(const exceptIntf: IMEException;
  var handled: boolean);
var
  LClassDesc, LClassName, LErrorDesc, LErrorMsg: string;
  LTerminateVisible: boolean;
  LErrorIcon, LHelpContext: integer;
  LUnespectedError: boolean;
begin
  //Event-handler per mad Exception: modifica il comportamento di default.
  //Se l'errore è un Access Violation mostra subito il bug report
  //altrimenti mostra l'impostazione predefinita nei settings di madExcept
  LClassDesc := GetErrorClassNameDesc(exceptIntf.ExceptClass,
    LUnespectedError);
  LClassName := exceptIntf.ExceptClass;
  LErrorMsg := exceptIntf.ExceptMessage;

  //Titolo della form di errore
  exceptIntf.TitleBar := LClassDesc;

  LUnespectedError :=
    (exceptIntf.ExceptionRecord.ExceptionCode = $C0000094) or
    (exceptIntf.ExceptionRecord.ExceptionCode = $C000008C) or
    (exceptIntf.ExceptionRecord.ExceptionCode = $C0000095) or
    (exceptIntf.ExceptionRecord.ExceptionCode = $C000008F) or
    (exceptIntf.ExceptionRecord.ExceptionCode = $C0000090) or
    (exceptIntf.ExceptionRecord.ExceptionCode = $C0000092) or
    (exceptIntf.ExceptionRecord.ExceptionCode = $C000008E) or
    (exceptIntf.ExceptionRecord.ExceptionCode = $C0000091) or
    (exceptIntf.ExceptionRecord.ExceptionCode = $C0000093) or
    (exceptIntf.ExceptionRecord.ExceptionCode = $C000008D) or
    (exceptIntf.ExceptionRecord.ExceptionCode = $C0000005) or
    (exceptIntf.ExceptionRecord.ExceptionCode = $C0000096) or
    (exceptIntf.ExceptionRecord.ExceptionCode = $C000013A) or
    (exceptIntf.ExceptionRecord.ExceptionCode = $C00000FD);

  if LUnespectedError then
  begin
    exceptIntf.ExceptMsg := Format(ERR_ACCES_VIOL_DESC, [LErrorMsg]);
    exceptIntf.RestartBtnVisible := True;
    exceptIntf.CloseBtnVisible := True;
    exceptIntf.ShowBtnVisible := True;
  end
  else
  begin
    //Messaggio di errore originale
    exceptIntf.ExceptMsg :=  LErrorMsg;
    //Nasconde i pulsanti restart, close e show bug report
    exceptIntf.RestartBtnVisible := False;
    exceptIntf.CloseBtnVisible := False;
    exceptIntf.ShowBtnVisible := False;
    //Focus su pulsante "continua"
    exceptIntf.FocusedButton := bContinueApplication;
  end;
end;
{$ENDIF}

procedure TMainDataModule.DataModuleCreate(Sender: TObject);
begin
  FIconsStyle := TIconsStyle(TMRUOptions.Instance.GetInteger('IconsStyle', Ord(it18Black)));
  UpdateIconsStyle;
end;

procedure TMainDataModule.SetIconsStyle(AIconsStyle: TIconsStyle);
begin
  if FIconsStyle <> AIconsStyle then
  begin
    FIconsStyle := AIconsStyle;
    UpdateIconsStyle;
    TMRUOptions.Instance.SetInteger('IconsStyle', Ord(FIconsStyle));
  end;
end;

procedure TMainDataModule.UpdateIconsStyle;
begin
  case FIconsStyle of
    it16Color:
    begin
      Images.Assign(Images16Color);
      ToolbarImages.Assign(ToolbarImages16Color);
    end;
    it18Black:
    begin
      Images.Assign(Images18Black);
      ToolbarImages.Assign(ToolbarImages18Black);
    end;
    it24Black:
    begin
      Images.Assign(Images24Black);
      ToolbarImages.Assign(ToolbarImages24Black);
    end;
  end;
end;

function GetFileImageIndex(const AExt: string): Integer;
begin
  if MatchText(AExt, ['js']) then
    Result := FILE_SCRIPT
  else if MatchText(AExt, ['htm', 'html']) then
    Result := FILE_HTML
  else if MatchText(AExt, ['jpg', 'png', 'gif']) then
    Result := FILE_IMAGE
  else if MatchText(AExt, ['css']) then
    Result := COLOR_PALETTE
  else
    Result := FILE_UNKNOWN;
end;

end.
