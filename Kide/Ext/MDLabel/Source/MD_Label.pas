//////////////////////////////////////////////////////////////////////////////////
//     This source code is provided 'as-is', without any express or implied     //
//     warranty. In no event will Infintuary be held liable for any damages     //
//     arising from the use of this software.                                   //
//                                                                              //
//     Infintuary does not warrant, that the source code will be free from      //
//     defects in design or workmanship or that operation of the source code    //
//     will be error-free. No implied or statutory warranty of merchantability  //
//     or fitness for a particular purpose shall apply. The entire risk of      //      
//     quality and performance is with the user of this source code.            //
//                                                                              //
//     Permission is granted to anyone to use this software for any purpose,    //
//     including commercial applications, and to alter it and redistribute it   //
//     freely, subject to the following restrictions:                           //
//                                                                              //
//     1. The origin of this source code must not be misrepresented; you must   //
//        not claim that you wrote the original source code.                    //
//                                                                              //
//     2. Altered source versions must be plainly marked as such, and must not  //
//        be misrepresented as being the original source code.                  //
//                                                                              //
//     3. This notice may not be removed or altered from any source             //
//        distribution.                                                         //
//////////////////////////////////////////////////////////////////////////////////

unit MD_Label;

{$IFDEF FPC}
{$MODE delphi}
{$ELSE}
{$DEFINE WINDOWS}
{$ENDIF}

interface

uses
  {$IFDEF WINDOWS}Windows, Messages, {$ENDIF}
  {$IFDEF FPC} LMessages,{$ENDIF}
  SysUtils, Classes, Controls, Graphics;

type
  TOnPaintBackground = procedure (Sender: TObject; Canvas: TCanvas; Width: Integer; Height: Integer) of object;
  TOnLinkClicked = procedure (Sender: TObject; LinkIndex: Integer; LinkText: WideString) of object;

  TMDLabel = class (TCustomControl)
  private
{$IFNDEF UNICODE}
    FCaptionW: WideString;
    FCaptionUTF8: UTF8String;
{$ENDIF}

    FLinkFontNormal: TFont;
    FLinkFontHover: TFont;

    FAutoSizeWidth: Boolean;
    FAutoSizeHeight: Boolean;
    FTextAlignment: TAlignment;
    FCompressSpaces: Boolean;
    FTabWidth: Integer;
    FInitialized: Boolean;
    FTextHeight: Integer;
    FMaxWidth: Integer;

    FParsingText: Boolean;
    FBuildingLines: Boolean;
    FRebuildLines: Boolean;

    FOnHeightChanged: TNotifyEvent;
    FOnWidthChanged: TNotifyEvent;
    FOnPaintBackground: TOnPaintBackground;
    FOnLinkClicked: TOnLinkClicked;

    FLines: TList;
    FWords: TList;
    FLinkRCs: TList;

    FMouseDownMove: Boolean;
    FMouseDownIndex: Integer;
    FMouseWasDown: Boolean;

{$IFNDEF UNICODE}
    procedure SetCaptionW(Value: WideString);
    procedure SetCaptionUTF8(Value: UTF8String);
{$ENDIF}
    procedure SetAutoSizeWidth(Value: Boolean);
    procedure SetAutoSizeHeight(Value: Boolean);
    procedure SetTextAlignment(Value: TAlignment);
    procedure SetCompressSpaces(Value: Boolean);
    procedure SetTabWidth(Value: Integer);
    procedure SetMaxWidth(Value: Integer);
    procedure WMEraseBkgnd(var Msg: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure DoFontChange(Sender: TObject);
    function TextHeightW(ws: WideString): Integer;
    function TextWidthW(ws: WideString): Integer;
    procedure TextSizeW(ws: WideString; out RC: TRect);
    procedure SetFont(Index: Integer; Value: TFont);
    procedure SetHeight(Value: Integer);
    function IsMouseOverLink(LinkID: Integer): Boolean;
    function GetLinkText(LinkID: Integer): WideString;
    procedure ParseText;
    procedure BuildLines;
    procedure TextToWords;
  protected
    procedure Paint; override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(var {%H-}Msg: TMessage); message CM_MOUSELEAVE;
{$IFDEF UNICODE}
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Align;
    property AutoSizeHeight: Boolean read FAutoSizeHeight write SetAutoSizeHeight;
    property AutoSizeWidth: Boolean read FAutoSizeWidth write SetAutoSizeWidth;
{$IFNDEF UNICODE}
    property Caption: WideString read FCaptionW write SetCaptionW stored False;
    property CaptionUTF8: UTF8String read FCaptionUTF8 write SetCaptionUTF8;
{$ELSE}
    property Caption;
{$ENDIF}
    property Color;
    property CompressSpaces: Boolean read FCompressSpaces write SetCompressSpaces;
    property Height;
    property Left;
    property Font;
    property LinkFontNormal: TFont index 1 read FLinkFontNormal write SetFont;
    property LinkFontHover: TFont index 2 read FLinkFontHover write SetFont;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth;
    property ParentColor;
    property ParentFont;
    property TabWidth: Integer read FTabWidth write SetTabWidth;
    property TextAlignment: TAlignment read FTextAlignment write SetTextAlignment;
    property TextHeight: Integer read FTextHeight;
    property Top;
    property Visible;
    property Width;

    property OnClick;
    property OnDblClick;
    property OnHeightChanged: TNotifyEvent read FOnHeightChanged write FOnHeightChanged;
    property OnLinkClicked: TOnLinkClicked read FOnLinkClicked write FOnLinkClicked;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaintBackground: TOnPaintBackground read FOnPaintBackground write FOnPaintBackground;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnWidthChanged: TNotifyEvent read FOnWidthChanged write FOnWidthChanged;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MDR', [TMDLabel]);
end;

type
  TToken = record
    Kind: Integer;
    Text: WideString;
    Value: Integer;
  end;

  TWordInfo = class
  protected
    FText: WideString;
    FRect: TRect;
    FFontStyle: TFontStyles;
    FLinkID: Integer;
    FFontColor: TColor;
    FBackColor: TColor;
    FSize: Integer;
    FWordHeight: Integer;
    FWordWidth: Integer;
  public
    constructor Create(AText: WideString; AFontStyle: TFontStyles; AFontColor: TColor; AFontSize: Integer; ABackColor: TColor; ALinkID: Integer);
    procedure SetWidth(XPos: Integer; TextRC: TRect);
    procedure AdjustWidth(XPos: Integer);
    procedure SetLineHeight(LineTop, LineHeight: Integer);
    procedure SetXOffset(Offset: Integer);
    procedure Add(ws: WideString);
  end;

  TSpaceInfo = class (TWordInfo)
  end;

  TDefaultFormatInfo = class (TWordInfo)
  public
    constructor Create;
  end;

  TBreakInfo = class (TDefaultFormatInfo)
  end;

  TTabInfo = class (TDefaultFormatInfo)
  public
    FTabPos: Integer;
    FTabBreak: Boolean;
    constructor Create(TabPos: Integer; TabBreak: Boolean);
  end;

  TDSPInfo = class (TWordInfo)
  end;

  TLineInfo = class
    FWords: TList;
    FLineWidth: Integer;
    FLineHeight: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

  TLinkRect = class
    FRect: TRect;
    FMouseOver: Boolean;
    FLinkID: Integer;
    procedure SetData(ARect: TRect; ALinkID: Integer);
    constructor Create(ALinkID: Integer);
  end;

constructor TWordInfo.Create(AText: WideString; AFontStyle: TFontStyles; AFontColor: TColor; AFontSize: Integer; ABackColor: TColor; ALinkID: Integer);
begin
  FText := AText;
  FRect := Rect(0, 0, 0, 0);
  FFontStyle := AFontStyle;
  FLinkID := ALinkID;
  FFontColor := AFontColor;
  FSize := AFontSize;
  FBackColor := ABackColor;
end;

procedure TWordInfo.SetWidth(XPos: Integer; TextRC: TRect);
begin
  FWordWidth := TextRC.Right - TextRC.Left;
  FRect.Left := XPos;
  FRect.Right := XPos + FWordWidth;
  FRect.Top := 0;
  FRect.Bottom := TextRC.Bottom;
  FWordHeight := TextRC.Bottom;
end;

procedure TWordInfo.AdjustWidth(XPos: Integer);
begin
  FWordWidth := FRect.Right - FRect.Left;
  FRect.Left := XPos;
  FRect.Right := XPos + FWordWidth;
  FRect.Top := 0;
end;

procedure TWordInfo.SetLineHeight(LineTop, LineHeight: Integer);
begin
  FRect.Bottom := LineTop + LineHeight;
  FRect.Top := FRect.Bottom - FWordHeight;
end;

procedure TWordInfo.SetXOffset(Offset: Integer);
begin
  FRect.Left := FRect.Left + Offset;
  FRect.Right := FRect.Left + FWordWidth;
end;

procedure TWordInfo.Add(ws: WideString);
begin
  FText := FText + ws;
end;

constructor TDefaultFormatInfo.Create;
begin
  inherited Create('', [], clBlack, 0, clNone, 0);
end;

constructor TTabInfo.Create(TabPos: Integer; TabBreak: Boolean);
begin
  inherited Create;
  FTabPos := TabPos;
  FTabBreak := TabBreak;
end;

constructor TLineInfo.Create;
begin
  FWords := TList.Create;
  FLineWidth := 0;
  FLineHeight := 0;
end;

destructor TLineInfo.Destroy;
begin
  FWords.Free;
  inherited;
end;

constructor TLinkRect.Create(ALinkID: Integer);
begin
  FRect := Rect(0, 0, 0, 0);
  FMouseOver := False;
  FLinkID := ALinkID;
end;

procedure TLinkRect.SetData(ARect: TRect; ALinkID: Integer);
begin
  FRect := ARect;
  FLinkID := ALinkID;
end;

function GetSpaces(Count: Integer): WideString;
var
  i: Integer;
begin
  result := '';
  for i := 1 to Count do result := result + ' ';
end;

{$IFNDEF UNICODE}
type
  TCharSet = set of char;

function CharInSet(c: char; TheSet: TCharSet): boolean;
begin
  result := (c in TheSet);
end;

function UTF8ToWideString(s8: UTF8String): WideString;
begin
  result := UTF8Decode(s8);
end;
{$ENDIF}

const
  TOKEN_UNKNOWN  =  0;
  TOKEN_TEXT     =  1;
  TOKEN_B_ON     =  2;
  TOKEN_B_OFF    =  3;
  TOKEN_I_ON     =  4;
  TOKEN_I_OFF    =  5;
  TOKEN_U_ON     =  6;
  TOKEN_U_OFF    =  7;
  TOKEN_S_ON     =  8;
  TOKEN_S_OFF    =  9;
  TOKEN_FC_ON    = 10;
  TOKEN_FC_OFF   = 11;
  TOKEN_A_ON     = 12;
  TOKEN_A_OFF    = 13;
  TOKEN_SPACE    = 14;
  TOKEN_BREAK    = 15;
  TOKEN_TAB      = 16;
  TOKEN_TABF     = 17;
  TOKEN_DSP      = 18;
  TOKEN_FS_ON    = 19;
  TOKEN_FS_OFF   = 20;
  TOKEN_BC_ON    = 21;
  TOKEN_BC_OFF   = 22;

function HexStringToBuffer(const HexString: string): TBytes;

  function HexDigitToByte(HexDigit: Char): Byte;
  begin
    case HexDigit of
      '0'..'9': result := Byte(HexDigit) - Byte('0');
      'A'..'F': result := Byte(HexDigit) - Byte('A') + 10;
      else result := 0;
    end;
  end;

const
  HexChars = ['0'..'9', 'A'..'F', 'a'..'f'];
var
  i: Integer;
  sHex: string;
begin
  sHex := '';
  for i := 1 to length(HexString) do if (CharInSet(HexString[i], HexChars)) then sHex := sHex + UpCase(HexString[i]);
  if length(sHex) mod 2 <> 0 then sHex := '0' + sHex;
  SetLength(result, (length(sHex) div 2));

  for i := 0 to length(result) - 1 do begin
    result[i] := (HexDigitToByte(sHex[2 * i + 1]) * 16) + HexDigitToByte(sHex[2 * i + 2]);
  end;
end;

function HexStringToColor(HexString: string; var Color: Integer): Boolean;
var
  Buffer: TBytes;
begin
  result := False;
  Color := 0;
  if length(HexString) > 6 then Exit;
  if length(HexString) < 1 then Exit;
  Buffer := HexStringToBuffer(HexString);

  Color := Integer(Buffer[length(Buffer) - 1]);
  if length(Buffer) > 1 then Color := Color or Integer(Buffer[length(Buffer) - 2]) shl 8;
  if length(Buffer) > 2 then Color := Color or Integer(Buffer[length(Buffer) - 3]) shl 16;
  result := True;
end;

function DecStringToColor(DecString: string; var Color: Integer): Boolean;
begin
  result := False;
  Color := 0;
  if length(DecString) > 8 then Exit;
  result := TryStrToInt(DecString, Color);
  Color := Color and $FFFFFF;
end;

function NameStringToColor(NameString: string; var Color: Integer): Boolean;
begin
  result := True;
  NameString := LowerCase(NameString);
  if NameString = 'clblack' then Color := clBlack
  else if NameString = 'clmaroon' then Color := clMaroon
  else if NameString = 'clgreen' then Color := clGreen
  else if NameString = 'clolive' then Color := clOlive
  else if NameString = 'clnavy' then Color := clNavy
  else if NameString = 'clpurple' then Color := clPurple
  else if NameString = 'clteal' then Color := clTeal
  else if NameString = 'clgray' then Color := clGray
  else if NameString = 'clsilver' then Color := clSilver
  else if NameString = 'clred' then Color := clRed
  else if NameString = 'cllime' then Color := clLime
  else if NameString = 'clyellow' then Color := clYellow
  else if NameString = 'clblue' then Color := clBlue
  else if NameString = 'clfuchsia' then Color := clFuchsia
  else if NameString = 'clmagenta' then Color := clFuchsia
  else if NameString = 'claqua' then Color := clAqua
  else if NameString = 'clltgray' then Color := clLtGray
  else if NameString = 'cldkgray' then Color := clDkGray
  else if NameString = 'clwhite' then Color := clWhite
  else if NameString = 'clmoneygreen' then Color := clMoneyGreen
  else if NameString = 'clskyblue' then Color := clSkyBlue
  else if NameString = 'clcream' then Color := clCream
  else if NameString = 'clmedgray' then Color := clMedGray
  else if NameString = 'clbrown' then Color := $17335C
  else if NameString = 'clorange' then Color := $008CFF
  else if NameString = 'clpink' then Color := $9314FF
  else result := False;
end;

function EntityToString(ws: WideString): WideString;
begin
  result := ws;
  case ws[1] of
    'a':
      if ws = 'amp' then result := '&';
    'b':
      if ws = 'bull' then result := UTF8ToWideString(AnsiString(#$e2#$80#$a2));
    'c':
      if ws = 'cent' then result := UTF8ToWideString(AnsiString(#$C2#$A2))
      else if ws = 'copy' then result := UTF8ToWideString(AnsiString(#$C2#$A9));
    'e':
      if ws = 'euro' then result := '€';
    'g':
      if ws = 'gt' then result := '>';
    'l':
      if ws = 'lt' then result := '<';
    'n':
      if ws = 'nbsp' then result := ' '
      else if ws = 'ndash' then result := UTF8ToWideString(AnsiString(#$e2#$80#$93));
    'r':
      if ws = 'reg' then result := UTF8ToWideString(AnsiString(#$C2#$AE));
    't':
      if ws = 'trade' then result := UTF8ToWideString(AnsiString(#$e2#$84#$a2));
  end;
end;

function ReplaceHTMLEntities(ws: WideString): WideString;
var
  p, i, j: Integer;
  c, c2: WideChar;
  Entity: WideString;
begin
  p := pos('&', ws);
  if p < 1 then result := ws
  else begin
    result := copy(ws, 1, p - 1);
    i := p;
    while i <= length(ws) do begin
      c := ws[i];
      if c = '&' then begin
        j := i + 1;
        while j <= length(ws) do begin
          c2 := ws[j];
          if c2 = ';' then begin
            Entity := copy(ws, i + 1, j - i - 1);
            if Entity <> '' then result := result + EntityToString(LowerCase(Entity));
            i := j;
            Break;
          end
          else if c2 = '&' then begin
            result := result + copy(ws, i, j - i);
            i := j - 1;
            Break;
          end;
          inc(j);
        end;
        if j > length(ws) then begin
          result := result + copy(ws, i, length(ws));
          Exit;
        end;
      end
      else result := result + c;
      inc(i);
    end;
  end;
end;

function GetToken(Line: WideString; var Index: Integer; out Token: TToken): Boolean;
type
  TIntType = (ctUnknown, ctHex, ctDec, ctName);
const
  WhiteSpace = [' ', #9];
  LineBreak = [#10, #13];
  Space = WhiteSpace + LineBreak;
  Digits = ['0'..'9'];
  HexDigits = ['0'..'9', 'a'..'f', 'A'..'F'];
  Letters = ['a'..'z', 'A'..'Z'];

  ST_START              =  0;
  ST_TAG_START          =  1;
  ST_TAG_BOLD           =  2;
  ST_TAG_ITALIC         =  3;
  ST_TAG_UNDERLINE      =  4;
  ST_TAG_STRIKEOUT      =  5;
  ST_TAG_COLOR          =  6;
  ST_COL_HEX            =  7;
  ST_COL_DEC            =  8;
  ST_COL_END            =  9;
  ST_COL_NAME           = 10;
  ST_COLOR              = 11;
  ST_COLOR_END          = 12;
  ST_ENDTAG             = 15;
  ST_ENDTAG_BOLD        = 16;
  ST_ENDTAG_ITALIC      = 17;
  ST_ENDTAG_UNDERLINE   = 18;
  ST_ENDTAG_STRIKEOUT   = 19;
  ST_ENDTAG_COLOR       = 20;
  ST_TAG_LINK           = 21;
  ST_ENDTAG_LINK        = 22;
  ST_SPACE              = 23;
  ST_TAG_BREAK          = 24;
  ST_TAG_TAB            = 25;
  ST_TAB                = 26;
  ST_TABF               = 27;
  ST_TAB_DEC            = 28;
  ST_TAB_HEX            = 29;
  ST_TAB2_END           = 30;
  ST_TAB_END            = 31;
  ST_TAG_DSP            = 36;
  ST_TAG_DSP2           = 37;
  ST_TAG_DSP3           = 38;
  ST_DSP_END            = 39;
  ST_FONT_SIZE          = 40;
  ST_SIZE_DEC           = 41;
  ST_SIZE_END2          = 42;
  ST_SIZE_END           = 43;
  ST_TAG_FONT           = 44;
  ST_TAG_FONT_SIZE      = 45;
  ST_ENDTAG_FONT        = 47;
  ST_ENDTAG_FONT_SIZE   = 48;
  ST_ENDTAG_FONT_COLOR  = 49;
  ST_ENDTAG_BACK_COLOR  = 50;

var
  c: Char;
  State: Integer;
  StartIndex: Integer;
  IntType: TIntType;
  IntValue: string;
  BoolValue: Boolean;
begin
  Token.Kind := TOKEN_UNKNOWN;
  Token.Text := '';
  Token.Value := 0;
  IntValue := '';
  IntType := ctUnknown;
  BoolValue := false;

  State := ST_START;
  StartIndex := Index;
  c := #0;
  while True do begin
    if Index <= length(Line) then begin
      if Integer(Line[Index]) < 256 then c := Char(Line[Index])
      else c := 'a'; // use dummy character for all wide characters
    end
    else begin
      result := (Token.Kind <> TOKEN_UNKNOWN);
      if result then begin
        Token.Text := (copy(Line, StartIndex, Index - StartIndex));
        if Token.Kind = TOKEN_TEXT then Token.Text := ReplaceHTMLEntities(Token.Text);
      end;
      Exit;
    end;
    case State of
      ST_START:
        begin
          if c = '<' then begin
            if Token.Kind = TOKEN_TEXT then begin
              Token.Text := ReplaceHTMLEntities(copy(Line, StartIndex, Index - StartIndex));
              result := True;
              Exit;
            end
            else State := ST_TAG_START;
          end
          else if CharInSet(c, Space) then begin
            if Token.Kind = TOKEN_TEXT then begin
              Token.Text := ReplaceHTMLEntities(copy(Line, StartIndex, Index - StartIndex));
              result := True;
              Exit;
            end
            else begin
              State := ST_SPACE;
              Token.Kind := TOKEN_SPACE;
              if CharInSet(c, WhiteSpace) then Token.Text := c
              else if (c = #10) then Token.Text := ' '
              else Token.Text := '';
            end;
          end
          else begin
            Token.Kind := TOKEN_TEXT;
          end
        end;
      ST_SPACE:
        begin
          if CharInSet(c, Space) then begin
            if CharInSet(c, WhiteSpace) then Token.Text := Token.Text + c
            else if (c = #10) then Token.Text := Token.Text + ' ';
          end
          else begin
            result := True;
            Exit;
          end;
        end;
      ST_TAG_START:
        if CharInSet(c, Space) then State := ST_TAG_START
        else if CharInSet(c, ['B', 'b']) then State := ST_TAG_BOLD
        else if CharInSet(c, ['F', 'f']) then State := ST_TAG_FONT
        else if CharInSet(c, ['I', 'i']) then State := ST_TAG_ITALIC
        else if CharInSet(c, ['U', 'u']) then State := ST_TAG_UNDERLINE
        else if CharInSet(c, ['S', 's']) then State := ST_TAG_STRIKEOUT
        else if CharInSet(c, ['C', 'c']) then State := ST_TAG_COLOR
        else if CharInSet(c, ['A', 'a']) then State := ST_TAG_LINK
        else if CharInSet(c, ['T', 't']) then State := ST_TAG_TAB
        else if CharInSet(c, ['D', 'd']) then State := ST_TAG_DSP
        else if c = '/' then State := ST_ENDTAG
        else if c <> '<' then begin
          Token.Kind := TOKEN_TEXT;
          State := ST_START;
        end;
      ST_TAG_BOLD:
        if c = '>' then begin
          Token.Kind := TOKEN_B_ON;
          inc(Index);
          result := True;
          Exit;
        end
        else if CharInSet(c, ['R', 'r']) then State := ST_TAG_BREAK
        else if CharInSet(c, ['C', 'c']) then begin
          State := ST_TAG_COLOR;
          BoolValue := True;
        end
        else  State := ST_START;
      ST_TAG_BREAK:
        if c = '>' then begin
          Token.Kind := TOKEN_BREAK;
          inc(Index);
          result := True;
          Exit;
        end
        else  State := ST_START;
      ST_TAG_LINK:
        if c = '>' then begin
          Token.Kind := TOKEN_A_ON;
          inc(Index);
          result := True;
          Exit;
        end
        else  State := ST_START;
      ST_TAG_ITALIC:
        if c = '>' then begin
          Token.Kind := TOKEN_I_ON;
          inc(Index);
          result := True;
          Exit;
        end
        else  State := ST_START;
      ST_TAG_UNDERLINE:
        if c = '>' then begin
          Token.Kind := TOKEN_U_ON;
          inc(Index);
          result := True;
          Exit;
        end
        else  State := ST_START;
      ST_TAG_STRIKEOUT:
        if c = '>' then begin
          Token.Kind := TOKEN_S_ON;
          inc(Index);
          result := True;
          Exit;
        end
        else  State := ST_START;
      ST_TAG_FONT:
        if CharInSet(c, ['S', 's']) then State := ST_TAG_FONT_SIZE
        else if CharInSet(c, ['C', 'c']) then State := ST_TAG_COLOR
        else State := ST_START;
      ST_TAG_FONT_SIZE:
        if c = ':' then State := ST_FONT_SIZE
        else State := ST_START;
      ST_FONT_SIZE:
        if CharInSet(c, Digits) then begin
          State := ST_SIZE_DEC;
          IntValue := IntValue + c;
        end
        else  State := ST_START;
      ST_SIZE_DEC:
        if CharInSet(c, Digits) then IntValue := IntValue + c
        else if CharInSet(c, Space) then State := ST_SIZE_END2
        else if c = '>' then State := ST_SIZE_END
        else State := ST_START;
      ST_SIZE_END2:
        if c = '>' then State := ST_SIZE_END
        else  State := ST_START;
      ST_SIZE_END:
        begin
          Token.Kind := TOKEN_TEXT;
          Result := True;
          if length(IntValue) <= 3 then begin
            if TryStrToInt(IntValue, Token.Value) then Token.Kind := TOKEN_FS_ON;
          end;
          Exit;
        end;
      ST_TAG_TAB:
        if c = ':' then State := ST_TAB
        else if c = 'f' then begin
          BoolValue := True;
          State := ST_TABF;
        end
        else if c = '>' then State := ST_TAB_END
        else  State := ST_START;
      ST_TABF:
        if c = ':' then State := ST_TAB
        else  State := ST_START;
      ST_TAB:
        if c = '$' then begin
          State := ST_TAB_HEX;
        end
        else if CharInSet(c, Digits) then begin
          State := ST_TAB_DEC;
          IntValue := IntValue + c;
        end
        else  State := ST_START;
      ST_TAB_HEX:
        if CharInSet(c, HexDigits) then IntValue := IntValue + c
        else if CharInSet(c, Space) then State := ST_TAB2_END
        else if c = '>' then State := ST_TAB_END
        else State := ST_START;
      ST_TAB_DEC:
        if CharInSet(c, Digits) then IntValue := IntValue + c
        else if CharInSet(c, Space) then State := ST_TAB2_END
        else if c = '>' then State := ST_TAB_END
        else State := ST_START;
      ST_TAB2_END:
        if c = '>' then State := ST_TAB_END
        else  State := ST_START;
      ST_TAB_END:
        begin
          Token.Kind := TOKEN_TEXT;
          Result := True;
          if IntValue = '' then Token.Kind := TOKEN_TAB
          else if length(IntValue) <= 4 then begin
            if TryStrToInt(IntValue, Token.Value) then begin
              if BoolValue then Token.Kind := TOKEN_TABF
              else Token.Kind := TOKEN_TAB;
            end;
          end;
          Exit;
        end;
      ST_TAG_DSP:
        if CharInSet(c, ['s', 'S']) then State := ST_TAG_DSP2
        else State := ST_START;
      ST_TAG_DSP2:
        if CharInSet(c, ['p', 'P']) then State := ST_TAG_DSP3
        else State := ST_START;
      ST_TAG_DSP3:
        if c = '>' then State := ST_DSP_END
        else  State := ST_START;
      ST_DSP_END:
        begin
          Token.Kind := TOKEN_DSP;
          Result := True;
          Exit;
        end;
      ST_TAG_COLOR:
        if c = ':' then State := ST_COLOR
        else  State := ST_START;
      ST_COLOR:
        if c = '$' then begin
          State := ST_COL_HEX;
          IntType := ctHex;
        end
        else if CharInSet(c, Digits) then begin
          State := ST_COL_DEC;
          IntValue := IntValue + c;
          IntType := ctDec;
        end
        else if CharInSet(c, ['C', 'c']) then begin
          State := ST_COL_NAME;
          IntValue := IntValue + c;
          IntType := ctName;
        end
        else  State := ST_START;
      ST_COL_HEX:
        if CharInSet(c, HexDigits) then IntValue := IntValue + c
        else if CharInSet(c, Space) then State := ST_COL_END
        else if c = '>' then State := ST_COLOR_END
        else State := ST_START;
      ST_COL_DEC:
        if CharInSet(c, Digits) then IntValue := IntValue + c
        else if CharInSet(c, Space) then State := ST_COL_END
        else if c = '>' then State := ST_COLOR_END
        else State := ST_START;
      ST_COL_NAME:
        if CharInSet(c, Letters) then IntValue := IntValue + c
        else if CharInSet(c, Space) then State := ST_COL_END
        else if c = '>' then State := ST_COLOR_END
        else State := ST_START;
      ST_COL_END:
        if c = '>' then State := ST_COLOR_END
        else  State := ST_START;
      ST_COLOR_END:
        begin
          Token.Kind := TOKEN_TEXT;
          Result := True;
          if BoolValue then begin
            case IntType of
              ctHex: if HexStringToColor(IntValue, Token.Value) then Token.Kind := TOKEN_BC_ON;
              ctDec: if DecStringToColor(IntValue, Token.Value) then Token.Kind := TOKEN_BC_ON;
              ctName: if NameStringToColor(IntValue, Token.Value) then Token.Kind := TOKEN_BC_ON;
            end;
          end
          else begin
            case IntType of
              ctHex: if HexStringToColor(IntValue, Token.Value) then Token.Kind := TOKEN_FC_ON;
              ctDec: if DecStringToColor(IntValue, Token.Value) then Token.Kind := TOKEN_FC_ON;
              ctName: if NameStringToColor(IntValue, Token.Value) then Token.Kind := TOKEN_FC_ON;
            end;
          end;
          Exit;
        end;
      ST_ENDTAG:
        if CharInSet(c, Space) then State := ST_ENDTAG
        else if CharInSet(c, ['B', 'b']) then State := ST_ENDTAG_BOLD
        else if CharInSet(c, ['I', 'i']) then State := ST_ENDTAG_ITALIC
        else if CharInSet(c, ['U', 'u']) then State := ST_ENDTAG_UNDERLINE
        else if CharInSet(c, ['S', 's']) then State := ST_ENDTAG_STRIKEOUT
        else if CharInSet(c, ['C', 'c']) then State := ST_ENDTAG_FONT_COLOR
        else if CharInSet(c, ['A', 'a']) then State := ST_ENDTAG_LINK
        else if CharInSet(c, ['F', 'f']) then State := ST_ENDTAG_FONT
        else begin
          Token.Kind := TOKEN_TEXT;
          State := ST_START;
        end;
      ST_ENDTAG_BOLD:
        if c = '>' then begin
          Token.Kind := TOKEN_B_OFF;
          inc(Index);
          result := True;
          Exit;
        end
        else if CharInSet(c, ['C', 'c']) then State := ST_ENDTAG_BACK_COLOR
        else  State := ST_START;
      ST_ENDTAG_LINK:
        if c = '>' then begin
          Token.Kind := TOKEN_A_OFF;
          inc(Index);
          result := True;
          Exit;
        end
        else  State := ST_START;
      ST_ENDTAG_FONT:
        if CharInSet(c, ['S', 's']) then State := ST_ENDTAG_FONT_SIZE
        else if CharInSet(c, ['C', 'c']) then State := ST_ENDTAG_FONT_COLOR
        else State := ST_START;
      ST_ENDTAG_FONT_SIZE:
        if c = '>' then begin
          Token.Kind := TOKEN_FS_OFF;
          inc(Index);
          result := True;
          Exit;
        end
        else  State := ST_START;
      ST_ENDTAG_ITALIC:
        if c = '>' then begin
          Token.Kind := TOKEN_I_OFF;
          inc(Index);
          result := True;
          Exit;
        end
        else  State := ST_START;
      ST_ENDTAG_UNDERLINE:
        if c = '>' then begin
          Token.Kind := TOKEN_U_OFF;
          inc(Index);
          result := True;
          Exit;
        end
        else  State := ST_START;
      ST_ENDTAG_STRIKEOUT:
        if c = '>' then begin
          Token.Kind := TOKEN_S_OFF;
          inc(Index);
          result := True;
          Exit;
        end
        else  State := ST_START;
      ST_ENDTAG_FONT_COLOR:
        if c = '>' then begin
          Token.Kind := TOKEN_FC_OFF;
          inc(Index);
          result := True;
          Exit;
        end
        else  State := ST_START;
      ST_ENDTAG_BACK_COLOR:
        if c = '>' then begin
          Token.Kind := TOKEN_BC_OFF;
          inc(Index);
          result := True;
          Exit;
        end
        else  State := ST_START;
    end;
    inc(Index);
  end;
end;

procedure TMDLabel.TextToWords;
var
  Token: TToken;
  i, Index: Integer;
  ws: WideString;
  CurrentStyle: TFontStyles;
  FontColorStack: TList;
  BackColorStack: TList;
  SizeStack: TList;
  LinkID, CurLinkID: Integer;
  InLink: Boolean;
  FontSize: Integer;
  FontColor: TColor;
  BackColor: TColor;
begin
  if (csLoading in ComponentState) or not FInitialized then exit;

  for i := 0 to FWords.Count - 1 do if FWords[i] <> nil then TWordInfo(FWords[i]).Free;
  FWords.Clear;
{$IFNDEF UNICODE}
  ws := FCaptionW;
{$ELSE}
  ws := inherited Caption;
{$ENDIF}
  if ws = '' then Exit;

  FontColorStack := TList.Create;
  BackColorStack := TList.Create;
  SizeStack := TList.Create;
  try
    FontColorStack.Add(Pointer(Font.Color));
    BackColorStack.Add(Pointer(clNone));
    SizeStack.Add(Pointer(Font.Size));
    CurLinkID := 0;
    InLink := False;
    CurrentStyle := Font.Style;

    Index := 1;
    while GetToken(ws, Index, Token) do begin
      if InLink then LinkID := CurLinkID                            
      else LinkID := 0;
      FontSize := Integer(SizeStack[SizeStack.Count - 1]);
      FontColor := TColor(FontColorStack[FontColorStack.Count - 1]);
      BackColor := TColor(BackColorStack[BackColorStack.Count - 1]);
      case Token.Kind of
        TOKEN_TEXT: FWords.Add(TWordInfo.Create(Token.Text, CurrentStyle, FontColor, FontSize, BackColor, LinkID));
        TOKEN_A_ON: begin inc(CurLinkID); InLink := True; end;
        TOKEN_A_OFF: InLink := False;
        TOKEN_B_ON: CurrentStyle := CurrentStyle + [fsBold];
        TOKEN_B_OFF: CurrentStyle := CurrentStyle - [fsBold];
        TOKEN_I_ON: CurrentStyle := CurrentStyle + [fsItalic];
        TOKEN_I_OFF: CurrentStyle := CurrentStyle - [fsItalic];
        TOKEN_U_ON: CurrentStyle := CurrentStyle + [fsUnderline];
        TOKEN_U_OFF: CurrentStyle := CurrentStyle - [fsUnderline];
        TOKEN_S_ON: CurrentStyle := CurrentStyle + [fsStrikeOut];
        TOKEN_S_OFF: CurrentStyle := CurrentStyle - [fsStrikeOut];
        TOKEN_FC_ON: FontColorStack.Add(Pointer(Token.Value));
        TOKEN_FC_OFF: if FontColorStack.Count > 1 then FontColorStack.Delete(FontColorStack.Count - 1);
        TOKEN_BC_ON: BackColorStack.Add(Pointer(Token.Value));
        TOKEN_BC_OFF: if BackColorStack.Count > 1 then BackColorStack.Delete(BackColorStack.Count - 1);
        TOKEN_SPACE: FWords.Add(TSpaceInfo.Create(Token.Text, CurrentStyle, FontColor, FontSize, BackColor, LinkID));
        TOKEN_BREAK: FWords.Add(TBreakInfo.Create);
        TOKEN_TAB: FWords.Add(TTabInfo.Create(Token.Value, false));
        TOKEN_TABF: FWords.Add(TTabInfo.Create(Token.Value, true));
        TOKEN_FS_ON: SizeStack.Add(Pointer(Token.Value));
        TOKEN_FS_OFF: if SizeStack.Count > 1 then SizeStack.Delete(SizeStack.Count - 1);
        TOKEN_DSP: FWords.Add(TDSPInfo.Create(' ', CurrentStyle, FontColor, FontSize, BackColor, LinkID));
      end;
    end;
  finally
    FontColorStack.Free;
    BackColorStack.Free;
    SizeStack.Free;
  end;
end;

procedure TMDLabel.ParseText;
var
  i: Integer;
begin
  if (csLoading in ComponentState) or not FInitialized then exit;

  if FParsingText then Exit;
  FParsingText := True;
  try
    for i := 0 to FLinkRCs.Count - 1 do if FLinkRCs[i] <> nil then TLinkRect(FLinkRCs[i]).Free;
    FLinkRCs.Clear;

    TextToWords;
    for i := 0 to FWords.Count - 1 do begin
      if TWordInfo(FWords[i]).FLinkID > 0 then begin
        FLinkRCs.Add(TLinkRect.Create(TWordInfo(FWords[i]).FLinkID));
      end;
    end;
    BuildLines;
  finally
    FParsingText := False;
  end;
end;

procedure TMDLabel.BuildLines;
var
  i, j: Integer;
  ws: WideString;
  LineTop: Integer;
  WordInfo: TWordInfo;
  LineInfo: TLineInfo;
  tmpWordInfo: TWordInfo;
  LineWidth: Integer;
  MaxLineWidth: Integer;
  TabSpaces: WideString;
  TextRC: TRect;
  DefaultLineHeight: Integer;
  Offset: Integer;
  TabPos: Integer;
  TabBreak: Integer;
  tmpWords: TList;
  WordLeft, WordLen: Integer;
const
  _TABWIDTH = 50;
begin
  if (csLoading in ComponentState) or not FInitialized then exit;
  FBuildingLines := True;
  try
    for i := 0 to FLines.Count - 1 do if FLines[i] <> nil then TLineInfo(FLines[i]).Free;
    FLines.Clear;

    LineInfo := TLineInfo.Create;
    FLines.Add(LineInfo);
    LineWidth := 0;
    TabBreak := 0;
    TabSpaces := GetSpaces(FTabWidth);

    Canvas.Font.Assign(Font);
    DefaultLineHeight := TextHeightW(' ');

    for i := 0 to FWords.Count - 1 do begin
      WordInfo := TWordInfo(FWords[i]);
      if (WordInfo is TSpaceInfo) or (WordInfo is TDSPInfo) then begin
        if FCompressSpaces then begin
          if LineInfo.FWords.Count = 0 then ws := '' // ignore leading spaces
          else ws := ' ';
        end
        else begin
          ws := '';
          for j := 1 to length(WordInfo.FText) do begin
            if WordInfo.FText[j] = ' ' then ws := ws + ' '
            else if WordInfo.FText[i] = #9 then ws := ws + TabSpaces;
          end;
        end;
      end
      else ws := WordInfo.FText;

      if WordInfo is TBreakInfo then begin
        LineInfo.FLineWidth := LineWidth;
        LineInfo := TLineInfo.Create;
        FLines.Add(LineInfo);
        LineWidth := 0;
        TabBreak := 0;
      end
      else if WordInfo is TTabInfo then begin
        if TTabInfo(WordInfo).FTabPos = 0 then TabPos := ((LineWidth div _TABWIDTH) + 1) * _TABWIDTH
        else begin
          TabPos := TTabInfo(WordInfo).FTabPos;
          if TTabInfo(WordInfo).FTabBreak then TabBreak := TabPos;
        end;

        if LineWidth < TabPos then begin
          if (TabPos <= Width) or FAutoSizeWidth then begin
            WordInfo.SetWidth(LineWidth, Rect(0, 0, TabPos - LineWidth, 0));
            LineWidth := TabPos;
            LineInfo.FWords.Add(WordInfo);
          end;
        end;
      end
      else if ws <> '' then begin
        if WordInfo.FLinkID = 0 then begin
          Canvas.Font.Assign(Font);
          Canvas.Font.Color := WordInfo.FFontColor;
          Canvas.Font.Style := WordInfo.FFontStyle;
          Canvas.Font.Size := WordInfo.FSize;
        end
        else begin
          if IsMouseOverLink(WordInfo.FLinkID) then Canvas.Font.Assign(FLinkFontHover)
          else Canvas.Font.Assign(FLinkFontNormal);
        end;

        if WordInfo is TDSPInfo then TextSizeW('0', TextRC)
        else TextSizeW(ws, TextRC);
        WordInfo.SetWidth(LineWidth, TextRC);
        if (WordInfo.FRect.Right <= Width) or (FAutoSizeWidth and ((WordInfo.FRect.Right <= FMaxWidth) or (FMaxWidth = 0))) then begin
          LineInfo.FWords.Add(WordInfo);
          LineWidth := WordInfo.FRect.Right;
        end
        else begin // word wrap
          tmpWords := TList.Create;
          try
            while LineInfo.FWords.Count > 0 do begin
              tmpWordInfo := TWordInfo(LineInfo.FWords[LineInfo.FWords.Count - 1]);
              if not ((tmpWordInfo is TSpaceInfo) or (tmpWordInfo is TTabInfo) or (tmpWordInfo is TDSPInfo)) then begin
                tmpWords.Insert(0, tmpWordInfo);
                LineInfo.FWords.Delete(LineInfo.FWords.Count - 1);
              end
              else Break;
            end;

            while LineInfo.FWords.Count > 0 do begin
              tmpWordInfo := TWordInfo(LineInfo.FWords[LineInfo.FWords.Count - 1]);
              if (tmpWordInfo is TSpaceInfo) or (tmpWordInfo is TTabInfo) or (tmpWordInfo is TDSPInfo) then begin
                LineInfo.FWords.Delete(LineInfo.FWords.Count - 1);
              end
              else Break;
            end;

            if (trim(ws) <> '') then tmpWords.Add(WordInfo);

            if LineInfo.FWords.Count > 0 then begin
              LineInfo.FLineWidth := TWordInfo(LineInfo.FWords[LineInfo.FWords.Count - 1]).FRect.Right;
              if (tmpWords.Count > 0) then begin
                LineInfo := TLineInfo.Create;
                FLines.Add(LineInfo);
              end;
            end
            else LineInfo.FLineWidth := 0;

            if (tmpWords.Count > 0) then begin
              if (LineInfo.FWords.Count = 0) then LineWidth := TabBreak
              else LineWidth := 0;
              for j := 0 to tmpWords.Count - 1 do begin
                WordInfo := TWordInfo(tmpWords[j]);
                LineInfo.FWords.Add(WordInfo);
                WordInfo.AdjustWidth(LineWidth);
                LineWidth := WordInfo.FRect.Right;
              end;

              if FAutoSizeWidth and (FMaxWidth <> 0) and (LineWidth > FMaxWidth) then begin
                WordInfo := TWordInfo(LineInfo.FWords[LineInfo.FWords.Count - 1]);
                if WordInfo <> nil then begin
                  LineWidth := WordInfo.FRect.Left;
                  ws := WordInfo.FText;
                  WordLeft := WordInfo.FRect.Left;
                  WordLen := length(ws);
                  while WordLeft + TextWidthW(ws + '...') > FMaxWidth do begin
                    dec(WordLen);
                    ws := copy (ws, 1, WordLen);
                  end;
                  WordInfo.FText := ws + '...';
                  TextSizeW(WordInfo.FText, TextRC);
                  WordInfo.SetWidth(LineWidth, TextRC);
                  LineWidth := WordInfo.FRect.Right;
                end;
              end;
            end;
          finally
            tmpWords.Free;
          end;
        end;
      end;
    end;
    LineInfo.FLineWidth := LineWidth;

    MaxLineWidth := 0;
    LineTop := 0;
    for i := 0 to FLines.Count - 1 do begin
      LineInfo := TLineInfo(FLines[i]);
      if LineInfo.FLineWidth > MaxLineWidth then MaxLineWidth := LineInfo.FLineWidth;

      LineInfo.FLineHeight := DefaultLineHeight;
      for j := 0 to LineInfo.FWords.Count - 1 do begin
        if TWordInfo(LineInfo.FWords[j]).FWordHeight > LineInfo.FLineHeight then LineInfo.FLineHeight := TWordInfo(LineInfo.FWords[j]).FWordHeight;
      end;
      for j := 0 to LineInfo.FWords.Count - 1 do begin
        TWordInfo(LineInfo.FWords[j]).SetLineHeight(LineTop, LineInfo.FLineHeight);
      end;
      LineTop := LineTop + LineInfo.FLineHeight;
    end;

    if FAutoSizeWidth and (MaxLineWidth <> Width) then begin
      if (MaxLineWidth > FMaxWidth) and (FMaxWidth <> 0) then MaxLineWidth := FMaxWidth;
      Width := MaxLineWidth;
      if Assigned(FOnWidthChanged) then FOnWidthChanged(self);
    end;
    SetHeight(LineTop);

    if FAutoSizeHeight and (LineTop <> Height) then begin
      Height := LineTop;
      FTextHeight := LineTop;
      if Assigned(FOnHeightChanged) then FOnHeightChanged(self);
    end;

    for i := 0 to FLines.Count - 1 do begin
      LineInfo := TLineInfo(FLines[i]);
      case FTextAlignment of
        taRightJustify: Offset := Width - LineInfo.FLineWidth;
        taCenter: Offset := (Width - LineInfo.FLineWidth) div 2;
        else Offset := 0;
      end;
      if Offset <> 0 then begin
        for j := 0 to LineInfo.FWords.Count - 1 do begin
          TWordInfo(LineInfo.FWords[j]).SetXOffset(Offset);
        end;
      end;
    end;
  finally
    FBuildingLines := False;
  end;
end;

constructor TMDLabel.Create(AOwner: TComponent);
begin
  FInitialized := False; // required for runtime creation of MDLabel
  inherited;
  ControlStyle := [csOpaque, csCaptureMouse, csClickEvents, csSetCaption];

  FLinkFontNormal := TFont.Create;
  FLinkFontNormal.Assign(Font);
  FLinkFontNormal.Color := clBlue;
  FLinkFontNormal.Style := [];

  FLinkFontHover := TFont.Create;
  FLinkFontHover.Assign(Font);
  FLinkFontHover.Color := clRed;
  FLinkFontHover.Style := [fsUnderline];

  Width := 100;
  Height := 13;
  Cursor := crArrow;
  TabStop := False;
  DoubleBuffered := True;
  FTextHeight := 0;

  FAutoSizeWidth := True;
  FAutoSizeHeight := True;
  FTextAlignment := taLeftJustify;
  FCompressSpaces := False;
  FTabWidth := 8;
  FParsingText := False;
  FBuildingLines := False;
  FRebuildLines := False;
  FMaxWidth := 0;

  FLinkFontNormal.OnChange := DoFontChange;
  FLinkFontHover.OnChange := DoFontChange;

  FOnLinkClicked := nil;
  FOnPaintBackground := nil;
  FOnHeightChanged := nil;
  FOnWidthChanged := nil;

  FLines := TList.Create;
  FWords := TList.Create;
  FLinkRCs := TList.Create;

  FMouseDownMove := False;
  FMouseWasDown := False;
  FMouseDownIndex := - 1;
  FInitialized := True;
end;

procedure TMDLabel.CreateWnd;
begin
  inherited CreateWnd;
{$IFNDEF UNICODE}
  if (inherited Caption <> '') and (FCaptionUTF8 = '') then CaptionUTF8 := inherited Caption;
{$ENDIF}
end;

procedure TMDLabel.Loaded;
begin
  inherited;
  ParseText;
end;

destructor TMDLabel.Destroy;
var
  i: Integer;
begin
  FLinkFontNormal.Free;
  FLinkFontHover.Free;

  for i := 0 to FLines.Count - 1 do if FLines[i] <> nil then TLineInfo(FLines[i]).Free;
  FLines.Free;
  for i := 0 to FWords.Count - 1 do if FWords[i] <> nil then TWordInfo(FWords[i]).Free;
  FWords.Free;
  for i := 0 to FLinkRCs.Count - 1 do if FLinkRCs[i] <> nil then TLinkRect(FLinkRCs[i]).Free;
  FLinkRCs.Free;
  inherited;
end;

procedure TMDLabel.WMEraseBkgnd(var Msg: TWmEraseBkgnd);
begin
  Msg.Result := 1;
end;

{$IFNDEF UNICODE}
procedure TMDLabel.SetCaptionW(Value: WideString);
begin
  if FCaptionW = Value then Exit;
  FCaptionW := Value;
  FCaptionUTF8 := UTF8Encode(Value);
  ParseText;
  Invalidate;
end;

procedure TMDLabel.SetCaptionUTF8(Value: UTF8String);
begin
  if FCaptionUTF8 = Value then Exit;
  FCaptionUTF8 := Value;
  FCaptionW := UTF8Decode(FCaptionUTF8);
  if (FCaptionW = '') and (FCaptionUTF8 <> '') then FCaptionW := 'invalid UTF8';
  ParseText;
  Invalidate;
end;

{$ELSE}
procedure TMDLabel.CMTextChanged(var Message: TMessage);
begin
  inherited;
  ParseText;
  Invalidate;
end;
{$ENDIF}

procedure TMDLabel.SetFont(Index: Integer; Value: TFont);
begin
  case Index of
    1: FLinkFontNormal.Assign(Value);
    2: FLinkFontHover.Assign(Value);
  end;
end;

procedure TMDLabel.SetHeight(Value: Integer);
begin
  if FAutoSizeHeight then Exit;
  if Height = Value then Exit;
  Height := Value;
  if Assigned(FOnHeightChanged) then FOnHeightChanged(self);
end;

procedure TMDLabel.SetAutoSizeWidth(Value: Boolean);
begin
  if FAutoSizeWidth = Value then Exit;
  FAutoSizeWidth := Value;
  BuildLines;
  Invalidate;
end;

procedure TMDLabel.SetAutoSizeHeight(Value: Boolean);
begin
  if FAutoSizeHeight = Value then Exit;
  FAutoSizeHeight := Value;
  BuildLines;
  Invalidate;
end;

procedure TMDLabel.SetCompressSpaces(Value: Boolean);
begin
  if FCompressSpaces = Value then Exit;
  FCompressSpaces := Value;
  BuildLines;
  Invalidate;
end;

procedure TMDLabel.SetTextAlignment(Value: TAlignment);
begin
  if FTextAlignment = Value then Exit;
  FTextAlignment := Value;
  BuildLines;
  Invalidate;
end;

procedure TMDLabel.TextSizeW(ws: WideString; out RC: TRect);
begin
  RC := Rect(0, 0, 0, 0);
  DrawTextW(Canvas.Handle, PWideChar(ws), length(ws), RC, DT_CALCRECT or DT_NOPREFIX or DT_LEFT or DT_SINGLELINE);
end;

function TMDLabel.TextHeightW(ws: WideString): Integer;
var
  RC: TRect;
begin
  RC := Rect(0, 0, 0, 0);
  DrawTextW(Canvas.Handle, PWideChar(ws), length(ws), RC, DT_CALCRECT or DT_NOPREFIX or DT_LEFT or DT_SINGLELINE);
  result := RC.Bottom - RC.Top;
end;

function TMDLabel.TextWidthW(ws: WideString): Integer;
var
  RC: TRect;
begin
  RC := Rect(0, 0, 0, 0);
  DrawTextW(Canvas.Handle, PWideChar(ws), length(ws), RC, DT_CALCRECT or DT_NOPREFIX or DT_LEFT or DT_SINGLELINE);
  result := RC.Right - RC.Left;
end;

procedure TMDLabel.SetTabWidth(Value: Integer);
begin
  if FTabWidth = Value then Exit;
  FTabWidth := Value;
  BuildLines;
  Invalidate;
end;

procedure TMDLabel.SetMaxWidth(Value: Integer);
begin
  if FMaxWidth = Value then Exit;
  FMaxWidth := Value;
  BuildLines;
  Invalidate;
end;

procedure TMDLabel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  RC: TRect;
  i: Integer;
  LinkRect: TLinkRect;
  LinkText: WideString;
begin
  try
    if Button <> mbLeft then Exit;
    if (Y < 0) or (Y > Height) then Exit;

    for i := 0 to FLinkRCs.Count - 1 do begin
      LinkRect := TLinkRect(FLinkRCs[i]);
      RC := LinkRect.FRect;
      if (X >= RC.Left) and (X <= RC.Right) and (Y >= RC.Top) and (Y <= RC.Bottom) then begin
        if FMouseDownIndex = i then begin
          FMouseWasDown := False;
          LinkText := GetLinkText(LinkRect.FLinkID);
          if Assigned(FOnLinkClicked) then FOnLinkClicked(self, LinkRect.FLinkID, LinkText);
          Exit;
        end;
      end;
    end;
  finally
    FMouseDownIndex := - 1;
    inherited;
  end;
end;

procedure TMDLabel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  RC: TRect;
  i: Integer;
  LinkRect: TLinkRect;
begin
  FMouseWasDown := True;
  FMouseDownIndex := - 1;
  try
    if (Y < 0) or (Y > Height) then Exit;
    if Button <> mbLeft then Exit;

    for i := 0 to FLinkRCs.Count - 1 do begin
      LinkRect := TLinkRect(FLinkRCs[i]);
      RC := LinkRect.FRect;
      if (X >= RC.Left) and (X <= RC.Right) and (Y >= RC.Top) and (Y <= RC.Bottom) then FMouseDownIndex := i;
    end;
  finally
    inherited;
  end;
end;

procedure TMDLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Msg: TMessage;
  RC: TRect;
  i: Integer;
  LinkRect: TLinkRect;
  CursorHand: Boolean;
  TrackMouseEv: TTrackMouseEvent;
begin
  try
    TrackMouseEv.cbSize := sizeof(TrackMouseEv);
    TrackMouseEv.hwndTrack := Handle;
    TrackMouseEv.dwFlags := TME_LEAVE;
    TrackMouseEvent(TrackMouseEv);

    FMouseDownMove := FMouseWasDown;
    if (Y > Height) or (Y < 0) or (X > Width) or (X < 0) then begin
      MouseLeave(Msg{%H-});
      Exit;
    end;

    CursorHand := False;
    for i := 0 to FLinkRCs.Count - 1 do begin
      LinkRect := TLinkRect(FLinkRCs[i]);
      RC := LinkRect.FRect;

      if (X >= RC.Left) and (X <= RC.Right) and (Y > RC.Top) and (Y < RC.Bottom) then begin
        CursorHand := True;
        if not LinkRect.FMouseOver then begin
          LinkRect.FMouseOver := True;
          BuildLines;
          Invalidate;
        end;
      end
      else begin
        if LinkRect.FMouseOver then begin
          LinkRect.FMouseOver := False;
          BuildLines;
          Invalidate;
        end;
      end;
    end;

    if CursorHand then begin
      if Cursor <> crHandPoint then Cursor := crHandPoint;
    end
    else if Cursor <> crArrow then Cursor := crArrow;

  finally
    inherited MouseMove(Shift, X, Y);
  end;
end;

procedure TMDLabel.MouseLeave(var Msg: TMessage);
var
  i: Integer;
  LinkRect: TLinkRect;
begin
  FMouseDownMove := False;
  FMouseWasDown := False;

  for i := 0 to FLinkRCs.Count - 1 do begin
    LinkRect := TLinkRect(FLinkRCs[i]);
    if LinkRect.FMouseOver then begin
      LinkRect.FMouseOver := False;
      Cursor := crArrow;
      BuildLines;
      Invalidate;
    end;
  end;
end;

function TMDLabel.IsMouseOverLink(LinkID: Integer): Boolean;
var
  i: Integer;
  LinkRect: TLinkRect;
begin
  result := False;
  for i := 0 to FLinkRCs.Count - 1 do begin
    LinkRect := TLinkRect(FLinkRCs[i]);
    if LinkRect.FLinkID = LinkID then begin
      if LinkRect.FMouseOver then begin
        result := True;
        Exit;
      end;
    end;
  end;
end;

function TMDLabel.GetLinkText(LinkID: Integer): WideString;
var
  i: Integer;
begin
  result := '';
  for i := 0 to FWords.Count - 1 do begin
    if TWordInfo(FWords[i]).FLinkID = LinkID then begin
      result := result + TWordInfo(FWords[i]).FText;
    end;
  end;
end;

procedure TMDLabel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  OldWidth: Integer;
begin
  OldWidth := Width;
  inherited;
  if (csLoading in ComponentState) or not FInitialized then exit;
  if FBuildingLines then begin
    FRebuildLines := True;
    exit;
  end;
  if OldWidth <> AWidth then begin
    BuildLines;
    Invalidate;
    if Assigned(FOnWidthChanged) then FOnWidthChanged(self);
  end;
end;

procedure TMDLabel.CMFontChanged(var Message: TMessage); 
begin
  DoFontChange(nil);
end;

procedure TMDLabel.DoFontChange(Sender: TObject);
begin
  ParseText;
  Invalidate;
end;

procedure TMDLabel.Paint;
var
  ws: WideString;
  i, Line: Integer;
  WordInfo: TWordInfo;
  LineInfo: TLineInfo;
  LinkIndex: Integer;
begin
  if not (csDesigning in ComponentState) and not Visible then Exit;
  if FParsingText or FBuildingLines then exit;
  if FRebuildLines then begin
    FRebuildLines := False;
    BuildLines;
  end;

  if Assigned(FOnPaintBackground) then begin
    FOnPaintBackground(self, self.Canvas, Width, Height);
  end
  else begin
    Canvas.Pen.Width := 0;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Color;
    Canvas.Pen.Color := Canvas.Brush.Color;
    Canvas.Rectangle(ClientRect);
    Canvas.Brush.Style := bsClear;
  end;

  LinkIndex := 0;
  for Line := 0 to FLines.Count - 1 do begin
    LineInfo := TLineInfo(FLines[Line]);

    for i := 0 to LineInfo.FWords.Count - 1 do begin
      WordInfo := TWordInfo(LineInfo.FWords[i]);
      ws := WordInfo.FText;
      if ws <> '' then begin
        if WordInfo.FBackColor = clNone then Canvas.Brush.Style := bsClear
        else begin
          Canvas.Brush.Color := WordInfo.FBackColor;
          Canvas.Brush.Style := bsSolid; //  required for Lazarus
        end;
        if WordInfo.FLinkID = 0 then begin
          Canvas.Font.Assign(self.Font);
          Canvas.Font.Color := WordInfo.FFontColor;
          Canvas.Font.Style := WordInfo.FFontStyle;
          Canvas.Font.Size := WordInfo.FSize;
        end
        else begin
          if IsMouseOverLink(WordInfo.FLinkID) then Canvas.Font.Assign(FLinkFontHover)
          else Canvas.Font.Assign(FLinkFontNormal);
        end;

        if WordInfo.FLinkID > 0 then begin
          TLinkRect(FLinkRCs[LinkIndex]).SetData(WordInfo.FRect, WordInfo.FLinkID);
          inc(LinkIndex);
        end;
        DrawTextW(Canvas.Handle, PWideChar(ws), length(ws), WordInfo.FRect, DT_NOPREFIX or DT_LEFT or DT_SINGLELINE);
      end;
    end;
  end;
end;

end.


