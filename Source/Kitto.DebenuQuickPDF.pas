{-------------------------------------------------------------------------------
   Copyright 2019 Ethea S.r.l.

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

unit Kitto.DebenuQuickPDF;

interface

uses
  DB,
  //DebenuPDFLibraryLite1112_TypeLibrary, old library 11.12
  DebenuPDFLibraryLite1114_TLB,
  SysUtils, Classes, System.UITypes,
  Kitto.Metadata.DataView, EF.Tree;

type
  EMergePDFError = class(Exception) end;

  //Layout structure
  TMergePDFLayout = class helper for TEFPersistentTree
  strict private
    function GetAuthor: string;
    function GetCreationDate: TDateTime;
    function GetCreator: string;
    function GetSubject: string;
    function GetTitle: string;
    function GetItems: TEFNode;
    function GetNodeValue(const ANodeName: string): string;
  private
    function GetKeywords: string;
  public
    property Items: TEFNode read GetItems;
    property Author: string read GetAuthor;
    property Title: string read GetTitle;
    property Subject: string read GetSubject;
    property Creator: string read GetCreator;
    property Keywords: string read GetKeywords;
    property CreationDate: TDateTime read GetCreationDate;
  end;

  //Debenu Quick PDF Font definition
  TPDFStandardFont = (
    sfCourier,                 //0
    sfCourierBold,             //1
    sfCourierBoldOblique,      //2
    sfCourierOblique,          //3
    sfHelvetica,               //4
    sfHelveticaBold,           //5
    sfHelveticaBoldOblique,    //6
    sfHelveticaOblique,        //7
    sfTimesRoman,              //8
    sfTimesBold,               //9
    sfTimesItalic,             //10
    sfTimesBoldItalic,         //11
    sfSymbol,                  //12
    sfZapfDingbats);           //13

  TPDFTextAlign = (
    taLeft,                    //0
    taCenter,                  //1
    taRight,                   //2
    taJustified,               //3
    taForcejustified,          //4
    taLastlinejustified);      //5

  TPDFVertAlign = (
    vaCenter,                  //0
    vaTop,                     //1
    vaBottom,                  //2
    vaCenterNoWrap,            //3
    vaTopNoWrap,               //4
    vaBottomNoWrap);           //5

  TKMergePDFEngine = class(TComponent)
  strict private
    FRecord: TKViewTableRecord;
    FPDFDoc: TPDFLibrary;
    FDefaultFontColor: TAlphaColor;
    FDefaultFontSize: Double;
    FDefaultFontNumber: Integer;
    function GetPDFDoc: TPDFLibrary;
    procedure CheckDebenuPDFError(DebenuReturnCode: Integer;
      const FullFileName: string = '');
    function LoadLayout(const ALayoutFileName: string): TEFPersistentTree;
    property PDFDoc: TPDFLibrary read GetPDFDoc;
    

    procedure AddImage(const AImageFileName: string; ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetupFont(const AFontSize: Double; const AFontColor: TAlphaColor;
      const AFontNumber: integer);
    procedure AddText(const AText: string; const AFontSize: Double; const AFontColor: TAlphaColor;
      const AFontNumber, AXPos, AYPos, AAlign: Integer);
    procedure DrawQRCode(const AText: string; AXPos, AYPos: Integer; ASize: Double; ARotation: Integer);
    procedure SetInformation(const ATitle, AAuthor, ASubject, ACreator, AKeyWords: string; ACreationDate: TDateTime);
    procedure AddTextBox(const AText: string; const AFontSize: Double; const AFontColor: TAlphaColor;
      const AFontNumber, ALeft, ATop, AWidth, AHeight: Integer; const AAlign, AVertAlign: Integer);
    procedure GetFontAttributes(AParentNode: TEFTree; const NodeName: string;
      out AFontNumber: Integer; out AFontSize: double; out AColor: TAlphaColor);
    function ExpandExpression(const AExpression: string): string;
  public
    procedure AfterConstruction; override;
    procedure MergePDF(const AFileName: string;
      const ALayoutFileName: string;
      const ABaseFileName: string;
      const ARecord: TKViewTableRecord);
  end;

implementation

uses
  Windows,
  System.Math , System.TypInfo, System.UIConsts,
  EF.Classes, EF.StrUtils, EF.Localization, EF.DB, EF.Macros,
  Kitto.Web.Application, Kitto.Metadata.Models, Kitto.Config;

{ TKMergePDFEngine }

procedure TKMergePDFEngine.AddImage(const AImageFileName: string; ALeft, ATop,
  AWidth, AHeight: Integer);
begin
  if FileExists(AImageFileName) then
  begin
    CheckDebenuPDFError(FPDFDoc.AddImageFromFile(AImageFileName, 0),AImageFileName);
    CheckDebenuPDFError(FPDFDoc.DrawImage(ALeft, ATop, AWidth, AHeight));
  end;
end;

procedure TKMergePDFEngine.AddText(const AText: string; const AFontSize: Double;
  const AFontColor: TAlphaColor; const AFontNumber, AXPos, AYPos, AAlign: Integer);
var
  LAlign: Integer;
begin
  LAlign := IfThen(AAlign <> -1, AAlign, 0);
  CheckDebenuPDFError(FPDFDoc.SetTextAlign(LAlign));
  SetupFont(AFontSize, AFontColor, AFontNumber);
  //Draw Text into PDF
  CheckDebenuPDFError(FPDFDoc.DrawText(AXPos, AYPos, AText));
end;

procedure TKMergePDFEngine.AddTextBox(const AText: string;
  const AFontSize: Double; const AFontColor: TAlphaColor; const AFontNumber,
  ALeft, ATop, AWidth, AHeight, AAlign, AVertAlign: Integer);
var
  LAlign: Integer;
  LVertAlign: Integer;
begin
  SetupFont(AFontSize, AFontColor, AFontNumber);
  LAlign := IfThen(AAlign <> -1, AAlign, 0);
  LVertAlign := IfThen(AVertAlign <> -1, AVertAlign, 0);
  CheckDebenuPDFError(FPDFDoc.SetTextAlign(LAlign));
  CheckDebenuPDFError(FPDFDoc.DrawTextBox(ALeft, ATop, AWidth, AHeight, AText, LVertAlign));
end;

procedure TKMergePDFEngine.AfterConstruction;
begin
  inherited;

end;

procedure TKMergePDFEngine.CheckDebenuPDFError(DebenuReturnCode: Integer;
  const FullFileName: string);
var
  ErrorMsg: string;
  DebenuErrorCode: Integer;
begin
  if DebenuReturnCode <> 0 then
    Exit
  else
    DebenuErrorCode := PDFDoc.LastErrorCode;

  case DebenuErrorCode of
    101: ErrorMsg := _('The Strength parameter passed to the Encrypt function was invalid');
    102: ErrorMsg := _('The Permissions parameter passed to the Encrypt function was invalid.');
    103: ErrorMsg := _('The Encrypt function was used on a document that was already encrypted');
    104: ErrorMsg := _('The Encrypt function failed for an unknown reason');
    201: ErrorMsg := _('The SetInformation function failed because the document is encrypted');
    202: ErrorMsg := _('The Key parameter passed to the SetInformation function was out of range');
    301: ErrorMsg := _('An invalid combination of barcode and option was sent to the DrawBarcode function');
    302: ErrorMsg := _('Non-numeric characters were sent to DrawBarcode using EAN-13');
    303: ErrorMsg := _('The EAN-13 barcode has an invalid checksum character');
    401: ErrorMsg := _('Could not open input file');
    402: ErrorMsg := _('Output file already exists and could not be deleted');
    403: ErrorMsg := _('Could not open output file');
    404: ErrorMsg := _('Invalid password');
    405: ErrorMsg := _('Document is not encrypted');
    406: ErrorMsg := _('Document is already encrypted');
    407: ErrorMsg := _('Invalid encryption strength');
    408: ErrorMsg := _('Invalid permissions');
    409: ErrorMsg := _('Invalid file structure, file is damaged');
    410: ErrorMsg := _('One of the input files is encrypted');
    411: ErrorMsg := _('File not found');
    412: ErrorMsg := _('Invalid page range list');
    501: ErrorMsg := _('The specified FileHandle was invalid');
    999: ErrorMsg := _('The function could not be used because the library is not unlocked');
  else
    ErrorMsg := _( 'Error unknown of Debenu Quick PDF Library (DebenuPDFLibraryLite1114.dll)');
  end;
  if FullFileName <> '' then
    ErrorMsg := Format(_('Error on file "%s": %s' ), [FullFileName, ErrorMsg]);
  raise EMergePDFError.Create(ErrorMsg);

end;

procedure TKMergePDFEngine.DrawQRCode(const AText: string; AXPos,
  AYPos: Integer; ASize: Double; ARotation: Integer);
var
  LDrawOptions: integer;
begin
  if (ARotation >= 0) and (ARotation < 90) then
    LDrawOptions := 0
  else if (ARotation >= 90) and (ARotation < 180) then
    LDrawOptions := 1
  else if (ARotation >= 180) and (ARotation < 270) then
    LDrawOptions := 2
  else
    LDrawOptions := 3;
  //Draw QRCode into PDF
  CheckDebenuPDFError(FPDFDoc.DrawQRCode(AXPos, AYPos, ASize, AText, 0, LDrawOptions));
end;

function TKMergePDFEngine.ExpandExpression(const AExpression: string): string;
begin
  Result := AExpression;
  FRecord.ExpandExpression(Result);
  TEFMacroExpansionEngine.Instance.Expand(Result);
end;

procedure TKMergePDFEngine.GetFontAttributes(AParentNode: TEFTree;
  const NodeName: string; out AFontNumber: Integer; out AFontSize: double;
  out AColor: TAlphaColor);
var
  LFontName, LColorName: string;
  LFontNode: TEFNode;
begin
  LFontNode := AParentNode.FindNode(NodeName);
  if Assigned(LFontNode) then
  begin
    LFontName := LFontNode.AsString;
    AFontNumber := GetEnumValue(TypeInfo(TPDFStandardFont),'sf'+LFontName);
    AFontSize := LFontNode.GetFloat('Size',-1);
    LColorName := LFontNode.GetString('Color');
    if LColorName <> '' then
      AColor := StringToAlphaColor(LColorName)
    else
      AColor := FDefaultFontColor;
  end
  else
  begin
    AFontNumber := FDefaultFontNumber;
    AFontSize := FDefaultFontSize;
    AColor := FDefaultFontColor;
  end;
end;

function TKMergePDFEngine.GetPDFDoc: TPDFLibrary;
begin
  if not Assigned(FPDFDoc) then
  begin
    FPDFDoc := TPDFLibrary.Create(nil);
    try
      FPDFDoc.Connect;
    except
      On E: Exception do
        raise EMergePDFError.CreateFmt(_('Error using DebenuPDFLibraryLite1114.dll library: %s' ),[E.Message]);
    end;
  end;
  Result := FPDFDoc;
end;

function TKMergePDFEngine.LoadLayout(
  const ALayoutFileName: string): TEFPersistentTree;
begin
  Assert(FileExists(ALayoutFileName), 'Layout file at node Layout not found');
  Result := TEFTreeFactory.LoadFromFile<TEFComponentConfig>(ALayoutFileName);
end;

procedure TKMergePDFEngine.MergePDF(const AFileName, ALayoutFileName,
  ABaseFileName: string; const ARecord: TKViewTableRecord);
var
  LExpression, LImageFileName, LText: string;
  LNode, LItems: TEFNode;
  I, LLeft, LTop, LWidth, LHeight, LXPos, LYPos, LFontNumber, LRotation: Integer;
  LFontSize : Double;
  LFontColor: TAlphaColor;
  LVertAlign, LAlign: Integer;
  LLayoutFileName: TEFPersistentTree;

begin
  FRecord := ARecord;
  LLayoutFileName := LoadLayout(ALayoutFileName);

  CheckDebenuPDFError(PDFDoc.LoadFromFile(ABaseFileName,''),ABaseFileName);

  //Initialize default font informations
  FDefaultFontNumber := 0;
  FDefaultFontSize := 10;
  FDefaultFontColor := claBlack;
  GetFontAttributes(LLayoutFileName.Root, 'DefaultFont', FDefaultFontNumber, FDefaultFontSize, FDefaultFontColor);

  //Add document informations to PDF
  SetInformation(
    ExpandExpression(LLayoutFileName.Title),
    ExpandExpression(LLayoutFileName.Author),
    ExpandExpression(LLayoutFileName.Subject),
    ExpandExpression(LLayoutFileName.Creator),
    ExpandExpression(LLayoutFileName.Keywords),
    LLayoutFileName.CreationDate);

  //Images node
  LItems := LLayoutFileName.Items;
  if Assigned(LItems) then
  begin
    for I := 0 to LItems.ChildCount - 1 do
    begin
      LNode := LItems.Children[I];
      if SameText(LNode.Name,'Image') then
      begin
        //image properties
        LImageFileName := LNode.GetString('FileName');
        LImageFileName := ExpandExpression(LImageFileName);
        LLeft := LNode.GetInteger('Left');
        //fa in modo che il top parte da 0
        LTop := Trunc( PDFDoc.PageHeight) - LNode.GetInteger('Top');
        LWidth := LNode.GetInteger('Width');
        LHeight := LNode.GetInteger('Height');

        //Add an image
        LImageFileName := TKWebApplication.Current.FindResourcePathName(LImageFileName);
        AddImage(LImageFileName,LLeft,LTop,LWidth,LHeight);
      end
      else if SameText(LNode.Name,'Text') then
      begin
        LExpression := LNode.GetString('Expression');
        LText := ExpandExpression(LExpression);
        LTop := LNode.GetInteger('Top', -1);
        LLeft := LNode.GetInteger('Left', -1);
        //se assegnato Top eLeft vengono assegnati Xpos e Ypos
        if (LTop = -1) and (LLeft = -1) then
        begin
          LXPos := LNode.GetInteger('XPos');
          LYPos := LNode.GetInteger('YPos');
        end
        else
        begin
          LXPos := LLeft;
          LYPos := Trunc( PDFDoc.PageHeight) - LTop;
        end;

        GetFontAttributes(LNode, 'Font', LFontNumber, LFontSize, LFontColor);

        LAlign := GetEnumValue(TypeInfo(TPDFTextAlign),'ta'+LNode.GetString('Align'));
        //Add a Text
        AddText(LText, LFontSize, LFontColor, LFontNumber, LXPos, LYPos, LAlign);
      end
      else if SameText(LNode.Name,'QRCode') then
      begin
        LExpression := LNode.GetString('Expression');
        LText := ExpandExpression(LExpression);
        LTop := LNode.GetInteger('Top', -1);
        LLeft := LNode.GetInteger('Left', -1);
        //se assegnato Top eLeft vengono assegnati Xpos e Ypos
        if (LTop = -1) and (LLeft = -1) then
        begin
          LXPos := LNode.GetInteger('XPos');
          LYPos := LNode.GetInteger('YPos');
        end
        else
        begin
          LXPos := LLeft;
          LYPos := Trunc( PDFDoc.PageHeight) - LTop;
        end;
        LWidth := LNode.GetInteger('Size', 10);
        LRotation := LNode.GetInteger('Rotation');
        //Add a QRCode
        DrawQRCode(LText, LXPos, LYPos, LWidth, LRotation);
      end
      else if SameText(LNode.Name,'TextBox') then
      begin
        //TextBox properties
        LExpression := LNode.GetString('Expression');
        LText := ExpandExpression(LExpression);
        LLeft := LNode.GetInteger('Left');
        //fa in modo che il top parte da 0
        LTop := Trunc( PDFDoc.PageHeight) - LNode.GetInteger('Top');
        LWidth := LNode.GetInteger('Width');
        LHeight := LNode.GetInteger('Height');

        GetFontAttributes(LNode, 'Font', LFontNumber, LFontSize, LFontColor);
        LVertAlign := GetEnumValue(TypeInfo(TPDFVertAlign),'va'+LNode.GetString('VertAlign'));
        LAlign := GetEnumValue(TypeInfo(TPDFTextAlign),'ta'+LNode.GetString('Align'));
        //Add a TextBox
        AddTextBox(LText, LFontSize, LFontColor, LFontNumber, LLeft, LTop, LWidth, LHeight, LAlign, LVertAlign);
      end;
    end;
  end;

  //Salva il file
  CheckDebenuPDFError(FPDFDoc.SaveToFile(AFileName),AFileName);
end;

procedure TKMergePDFEngine.SetInformation(const ATitle, AAuthor, ASubject,
  ACreator, AKeyWords: string; ACreationDate: TDateTime);
var
  LDate: string;
  LSysTime: SystemTime;
  LTimeZone: TTimeZoneInformation;
  LTimeZoneGap: string;

  procedure AddInformation(AKey: Integer; const AText: string);
  begin
    if AText <> '' then
      CheckDebenuPDFError(FPDFDoc.SetInformation(AKey, AText));
  end;
begin
  //Add Informations
  AddInformation(1, AAuthor);
  AddInformation(2, ATitle);
  AddInformation(3, ASubject);
  AddInformation(4, AKeywords);
  AddInformation(5, ACreator);
  if ACreationDate <> 0 then
  begin
    GetSystemTime(LSysTime);
    GetTimeZoneInformation(LTimeZone);
    LTimeZoneGap := PadLeft(IntToStr(LTimeZone.Bias div -60),2);
    LDate := 'D:'+FormatDateTime('yyyymmddhhmmss', ACreationDate)+'Z'+LTimeZoneGap+'''00''';
    AddInformation(7, LDate);
  end;
end;

procedure TKMergePDFEngine.SetupFont(const AFontSize: Double;
  const AFontColor: TAlphaColor; const AFontNumber: integer);
var
  LFontId: Integer;
  LColorR, LColorG, LColorB: Byte;
begin
  if AFontNumber <> -1 then
  begin
    LFontId := FPDFDoc.AddStandardFont(AFontNumber);
    FPDFDoc.SelectFont(LFontId);
  end;
  if AFontSize > 0 then
    CheckDebenuPDFError(FPDFDoc.SetTextSize(AFontSize));
  LColorR := TAlphaColorRec(AFontColor).R;
  LColorG := TAlphaColorRec(AFontColor).G;
  LColorB := TAlphaColorRec(AFontColor).B;
  CheckDebenuPDFError(FPDFDoc.SetTextColor(LColorR, LColorG, LColorB));

end;

{ TMergePDFLayout }

function TMergePDFLayout.GetAuthor: string;
begin
  Result := GetNodeValue('Author');
end;

function TMergePDFLayout.GetCreationDate: TDateTime;
var
  LDateStr: string;
begin
  LDateStr := GetNodeValue('CreationDate');
  try
    Result := StrToDateTime(LDateStr, TKConfig.Instance.UserFormatSettings);
  except
    Result := 0;
  end;
end;

function TMergePDFLayout.GetCreator: string;
begin
  Result := GetNodeValue('Creator');
end;

function TMergePDFLayout.GetItems: TEFNode;
begin
  Result := FindNode('Items');
end;

function TMergePDFLayout.GetKeywords: string;
begin
  Result := GetNodeValue('Keywords');
end;

function TMergePDFLayout.GetNodeValue(const ANodeName: string): string;
var
  LNode: TEFNode;
begin
  LNode := FindNode('Information');
  if Assigned(LNode) then
    Result := LNode.GetExpandedString(ANodeName)
  else
    Result := '';
end;

function TMergePDFLayout.GetSubject: string;
begin
  Result := GetNodeValue('Subject');
end;

function TMergePDFLayout.GetTitle: string;
begin
  Result := GetNodeValue('Title');
end;

end.
