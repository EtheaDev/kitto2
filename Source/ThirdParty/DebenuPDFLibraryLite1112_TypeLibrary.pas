unit DebenuPDFLibraryLite1112_TypeLibrary;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 52393 $
// File generated on 13/11/2014 18.32.55 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Program Files (x86)\Debenu\PDF Library\Lite\DebenuPDFLibraryLite1112.dll (1)
// LIBID: {419E6874-9452-4870-94FA-CF30EDB51EA6}
// LCID: 0
// Helpfile: 
// HelpString: Debenu Quick PDF Library (Lite Edition) 11.12
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// SYS_KIND: SYS_WIN32
// Errors:
//   Error creating palette bitmap of (TPDFLibrary) : Server C:\PROGRA~2\Debenu\PDFLIB~1\Lite\DEBENU~3.DLL contains no icons
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows, System.Classes, System.Variants, System.Win.StdVCL, Vcl.Graphics, Vcl.OleServer, Winapi.ActiveX;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  DebenuPDFLibraryLite1112MajorVersion = 11;
  DebenuPDFLibraryLite1112MinorVersion = 12;

  LIBID_DebenuPDFLibraryLite1112: TGUID = '{419E6874-9452-4870-94FA-CF30EDB51EA6}';

  IID_IPDFLibrary: TGUID = '{0D585812-AAED-4680-829D-E447A7593E20}';
  CLASS_PDFLibrary: TGUID = '{3A0BE429-08CE-4183-8A8D-9CB0B00CB5A3}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IPDFLibrary = interface;
  IPDFLibraryDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  PDFLibrary = IPDFLibrary;


// *********************************************************************//
// Interface: IPDFLibrary
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0D585812-AAED-4680-829D-E447A7593E20}
// *********************************************************************//
  IPDFLibrary = interface(IDispatch)
    ['{0D585812-AAED-4680-829D-E447A7593E20}']
    function AddImageFromFile(const FileName: WideString; Options: Integer): Integer; safecall;
    function AddLinkToWeb(Left: Double; Top: Double; Width: Double; Height: Double; 
                          const Link: WideString; Options: Integer): Integer; safecall;
    function AddStandardFont(StandardFontID: Integer): Integer; safecall;
    function DocumentCount: Integer; safecall;
    function DrawQRCode(Left: Double; Top: Double; SymbolSize: Double; const Text: WideString; 
                        EncodeOptions: Integer; DrawOptions: Integer): Integer; safecall;
    function DrawImage(Left: Double; Top: Double; Width: Double; Height: Double): Integer; safecall;
    function DrawText(XPos: Double; YPos: Double; const Text: WideString): Integer; safecall;
    function DrawTextBox(Left: Double; Top: Double; Width: Double; Height: Double; 
                         const Text: WideString; Options: Integer): Integer; safecall;
    function FindImages: Integer; safecall;
    function GetInformation(Key: Integer): WideString; safecall;
    function GetPageBox(BoxType: Integer; Dimension: Integer): Double; safecall;
    function HasFontResources: Integer; safecall;
    function ImageCount: Integer; safecall;
    function ImageHeight: Integer; safecall;
    function ImageWidth: Integer; safecall;
    function IsLinearized: Integer; safecall;
    function LastErrorCode: Integer; safecall;
    function LoadFromFile(const FileName: WideString; const Password: WideString): Integer; safecall;
    function MergeDocument(DocumentID: Integer): Integer; safecall;
    function NewDocument: Integer; safecall;
    function NewPage: Integer; safecall;
    function NormalizePage(NormalizeOptions: Integer): Integer; safecall;
    function PageCount: Integer; safecall;
    function PageHeight: Double; safecall;
    function PageRotation: Integer; safecall;
    function PageWidth: Double; safecall;
    function RemoveDocument(DocumentID: Integer): Integer; safecall;
    function RotatePage(PageRotation: Integer): Integer; safecall;
    function SaveToFile(const FileName: WideString): Integer; safecall;
    function SecurityInfo(SecurityItem: Integer): Integer; safecall;
    function SelectDocument(DocumentID: Integer): Integer; safecall;
    function SelectedDocument: Integer; safecall;
    function SelectFont(FontID: Integer): Integer; safecall;
    function SelectImage(ImageID: Integer): Integer; safecall;
    function SelectPage(PageNumber: Integer): Integer; safecall;
    function SetBaseURL(const NewBaseURL: WideString): Integer; safecall;
    function SetInformation(Key: Integer; const NewValue: WideString): Integer; safecall;
    function SetMeasurementUnits(MeasurementUnits: Integer): Integer; safecall;
    function SetOrigin(Origin: Integer): Integer; safecall;
    function SetPageBox(BoxType: Integer; Left: Double; Top: Double; Width: Double; Height: Double): Integer; safecall;
    function SetPageDimensions(NewPageWidth: Double; NewPageHeight: Double): Integer; safecall;
    function SetPageLayout(NewPageLayout: Integer): Integer; safecall;
    function SetPageMode(NewPageMode: Integer): Integer; safecall;
    function SetPageSize(const PaperName: WideString): Integer; safecall;
    function SetTextAlign(TextAlign: Integer): Integer; safecall;
    function SetTextColor(Red: Double; Green: Double; Blue: Double): Integer; safecall;
    function SetTextSize(TextSize: Double): Integer; safecall;
    function SetTextUnderline(Underline: Integer): Integer; safecall;
  end;

// *********************************************************************//
// DispIntf:  IPDFLibraryDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0D585812-AAED-4680-829D-E447A7593E20}
// *********************************************************************//
  IPDFLibraryDisp = dispinterface
    ['{0D585812-AAED-4680-829D-E447A7593E20}']
    function AddImageFromFile(const FileName: WideString; Options: Integer): Integer; dispid 72876032;
    function AddLinkToWeb(Left: Double; Top: Double; Width: Double; Height: Double; 
                          const Link: WideString; Options: Integer): Integer; dispid 72876033;
    function AddStandardFont(StandardFontID: Integer): Integer; dispid 72876034;
    function DocumentCount: Integer; dispid 72876035;
    function DrawQRCode(Left: Double; Top: Double; SymbolSize: Double; const Text: WideString; 
                        EncodeOptions: Integer; DrawOptions: Integer): Integer; dispid 72876036;
    function DrawImage(Left: Double; Top: Double; Width: Double; Height: Double): Integer; dispid 72876037;
    function DrawText(XPos: Double; YPos: Double; const Text: WideString): Integer; dispid 72876038;
    function DrawTextBox(Left: Double; Top: Double; Width: Double; Height: Double; 
                         const Text: WideString; Options: Integer): Integer; dispid 72876039;
    function FindImages: Integer; dispid 72876040;
    function GetInformation(Key: Integer): WideString; dispid 72876041;
    function GetPageBox(BoxType: Integer; Dimension: Integer): Double; dispid 72876042;
    function HasFontResources: Integer; dispid 72876043;
    function ImageCount: Integer; dispid 72876044;
    function ImageHeight: Integer; dispid 72876045;
    function ImageWidth: Integer; dispid 72876046;
    function IsLinearized: Integer; dispid 72876047;
    function LastErrorCode: Integer; dispid 72876048;
    function LoadFromFile(const FileName: WideString; const Password: WideString): Integer; dispid 72876049;
    function MergeDocument(DocumentID: Integer): Integer; dispid 72876050;
    function NewDocument: Integer; dispid 72876051;
    function NewPage: Integer; dispid 72876052;
    function NormalizePage(NormalizeOptions: Integer): Integer; dispid 72876053;
    function PageCount: Integer; dispid 72876054;
    function PageHeight: Double; dispid 72876055;
    function PageRotation: Integer; dispid 72876056;
    function PageWidth: Double; dispid 72876057;
    function RemoveDocument(DocumentID: Integer): Integer; dispid 72876058;
    function RotatePage(PageRotation: Integer): Integer; dispid 72876059;
    function SaveToFile(const FileName: WideString): Integer; dispid 72876060;
    function SecurityInfo(SecurityItem: Integer): Integer; dispid 72876061;
    function SelectDocument(DocumentID: Integer): Integer; dispid 72876062;
    function SelectedDocument: Integer; dispid 72876063;
    function SelectFont(FontID: Integer): Integer; dispid 72876064;
    function SelectImage(ImageID: Integer): Integer; dispid 72876065;
    function SelectPage(PageNumber: Integer): Integer; dispid 72876066;
    function SetBaseURL(const NewBaseURL: WideString): Integer; dispid 72876067;
    function SetInformation(Key: Integer; const NewValue: WideString): Integer; dispid 72876068;
    function SetMeasurementUnits(MeasurementUnits: Integer): Integer; dispid 72876069;
    function SetOrigin(Origin: Integer): Integer; dispid 72876070;
    function SetPageBox(BoxType: Integer; Left: Double; Top: Double; Width: Double; Height: Double): Integer; dispid 72876071;
    function SetPageDimensions(NewPageWidth: Double; NewPageHeight: Double): Integer; dispid 72876072;
    function SetPageLayout(NewPageLayout: Integer): Integer; dispid 72876073;
    function SetPageMode(NewPageMode: Integer): Integer; dispid 72876074;
    function SetPageSize(const PaperName: WideString): Integer; dispid 72876075;
    function SetTextAlign(TextAlign: Integer): Integer; dispid 72876076;
    function SetTextColor(Red: Double; Green: Double; Blue: Double): Integer; dispid 72876077;
    function SetTextSize(TextSize: Double): Integer; dispid 72876078;
    function SetTextUnderline(Underline: Integer): Integer; dispid 72876079;
  end;

// *********************************************************************//
// The Class CoPDFLibrary provides a Create and CreateRemote method to          
// create instances of the default interface IPDFLibrary exposed by              
// the CoClass PDFLibrary. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoPDFLibrary = class
    class function Create: IPDFLibrary;
    class function CreateRemote(const MachineName: string): IPDFLibrary;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TPDFLibrary
// Help String      : DebenuPDFLibraryLite1112.PDFLibrary Object
// Default Interface: IPDFLibrary
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TPDFLibrary = class(TOleServer)
  private
    FIntf: IPDFLibrary;
    function GetDefaultInterface: IPDFLibrary;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IPDFLibrary);
    procedure Disconnect; override;
    function AddImageFromFile(const FileName: WideString; Options: Integer): Integer;
    function AddLinkToWeb(Left: Double; Top: Double; Width: Double; Height: Double; 
                          const Link: WideString; Options: Integer): Integer;
    function AddStandardFont(StandardFontID: Integer): Integer;
    function DocumentCount: Integer;
    function DrawQRCode(Left: Double; Top: Double; SymbolSize: Double; const Text: WideString; 
                        EncodeOptions: Integer; DrawOptions: Integer): Integer;
    function DrawImage(Left: Double; Top: Double; Width: Double; Height: Double): Integer;
    function DrawText(XPos: Double; YPos: Double; const Text: WideString): Integer;
    function DrawTextBox(Left: Double; Top: Double; Width: Double; Height: Double; 
                         const Text: WideString; Options: Integer): Integer;
    function FindImages: Integer;
    function GetInformation(Key: Integer): WideString;
    function GetPageBox(BoxType: Integer; Dimension: Integer): Double;
    function HasFontResources: Integer;
    function ImageCount: Integer;
    function ImageHeight: Integer;
    function ImageWidth: Integer;
    function IsLinearized: Integer;
    function LastErrorCode: Integer;
    function LoadFromFile(const FileName: WideString; const Password: WideString): Integer;
    function MergeDocument(DocumentID: Integer): Integer;
    function NewDocument: Integer;
    function NewPage: Integer;
    function NormalizePage(NormalizeOptions: Integer): Integer;
    function PageCount: Integer;
    function PageHeight: Double;
    function PageRotation: Integer;
    function PageWidth: Double;
    function RemoveDocument(DocumentID: Integer): Integer;
    function RotatePage(PageRotation: Integer): Integer;
    function SaveToFile(const FileName: WideString): Integer;
    function SecurityInfo(SecurityItem: Integer): Integer;
    function SelectDocument(DocumentID: Integer): Integer;
    function SelectedDocument: Integer;
    function SelectFont(FontID: Integer): Integer;
    function SelectImage(ImageID: Integer): Integer;
    function SelectPage(PageNumber: Integer): Integer;
    function SetBaseURL(const NewBaseURL: WideString): Integer;
    function SetInformation(Key: Integer; const NewValue: WideString): Integer;
    function SetMeasurementUnits(MeasurementUnits: Integer): Integer;
    function SetOrigin(Origin: Integer): Integer;
    function SetPageBox(BoxType: Integer; Left: Double; Top: Double; Width: Double; Height: Double): Integer;
    function SetPageDimensions(NewPageWidth: Double; NewPageHeight: Double): Integer;
    function SetPageLayout(NewPageLayout: Integer): Integer;
    function SetPageMode(NewPageMode: Integer): Integer;
    function SetPageSize(const PaperName: WideString): Integer;
    function SetTextAlign(TextAlign: Integer): Integer;
    function SetTextColor(Red: Double; Green: Double; Blue: Double): Integer;
    function SetTextSize(TextSize: Double): Integer;
    function SetTextUnderline(Underline: Integer): Integer;
    property DefaultInterface: IPDFLibrary read GetDefaultInterface;
  published
  end;

procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses System.Win.ComObj;

class function CoPDFLibrary.Create: IPDFLibrary;
begin
  Result := CreateComObject(CLASS_PDFLibrary) as IPDFLibrary;
end;

class function CoPDFLibrary.CreateRemote(const MachineName: string): IPDFLibrary;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_PDFLibrary) as IPDFLibrary;
end;

procedure TPDFLibrary.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{3A0BE429-08CE-4183-8A8D-9CB0B00CB5A3}';
    IntfIID:   '{0D585812-AAED-4680-829D-E447A7593E20}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TPDFLibrary.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IPDFLibrary;
  end;
end;

procedure TPDFLibrary.ConnectTo(svrIntf: IPDFLibrary);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TPDFLibrary.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TPDFLibrary.GetDefaultInterface: IPDFLibrary;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TPDFLibrary.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TPDFLibrary.Destroy;
begin
  inherited Destroy;
end;

function TPDFLibrary.AddImageFromFile(const FileName: WideString; Options: Integer): Integer;
begin
  Result := DefaultInterface.AddImageFromFile(FileName, Options);
end;

function TPDFLibrary.AddLinkToWeb(Left: Double; Top: Double; Width: Double; Height: Double; 
                                  const Link: WideString; Options: Integer): Integer;
begin
  Result := DefaultInterface.AddLinkToWeb(Left, Top, Width, Height, Link, Options);
end;

function TPDFLibrary.AddStandardFont(StandardFontID: Integer): Integer;
begin
  Result := DefaultInterface.AddStandardFont(StandardFontID);
end;

function TPDFLibrary.DocumentCount: Integer;
begin
  Result := DefaultInterface.DocumentCount;
end;

function TPDFLibrary.DrawQRCode(Left: Double; Top: Double; SymbolSize: Double; 
                                const Text: WideString; EncodeOptions: Integer; DrawOptions: Integer): Integer;
begin
  Result := DefaultInterface.DrawQRCode(Left, Top, SymbolSize, Text, EncodeOptions, DrawOptions);
end;

function TPDFLibrary.DrawImage(Left: Double; Top: Double; Width: Double; Height: Double): Integer;
begin
  Result := DefaultInterface.DrawImage(Left, Top, Width, Height);
end;

function TPDFLibrary.DrawText(XPos: Double; YPos: Double; const Text: WideString): Integer;
begin
  Result := DefaultInterface.DrawText(XPos, YPos, Text);
end;

function TPDFLibrary.DrawTextBox(Left: Double; Top: Double; Width: Double; Height: Double; 
                                 const Text: WideString; Options: Integer): Integer;
begin
  Result := DefaultInterface.DrawTextBox(Left, Top, Width, Height, Text, Options);
end;

function TPDFLibrary.FindImages: Integer;
begin
  Result := DefaultInterface.FindImages;
end;

function TPDFLibrary.GetInformation(Key: Integer): WideString;
begin
  Result := DefaultInterface.GetInformation(Key);
end;

function TPDFLibrary.GetPageBox(BoxType: Integer; Dimension: Integer): Double;
begin
  Result := DefaultInterface.GetPageBox(BoxType, Dimension);
end;

function TPDFLibrary.HasFontResources: Integer;
begin
  Result := DefaultInterface.HasFontResources;
end;

function TPDFLibrary.ImageCount: Integer;
begin
  Result := DefaultInterface.ImageCount;
end;

function TPDFLibrary.ImageHeight: Integer;
begin
  Result := DefaultInterface.ImageHeight;
end;

function TPDFLibrary.ImageWidth: Integer;
begin
  Result := DefaultInterface.ImageWidth;
end;

function TPDFLibrary.IsLinearized: Integer;
begin
  Result := DefaultInterface.IsLinearized;
end;

function TPDFLibrary.LastErrorCode: Integer;
begin
  Result := DefaultInterface.LastErrorCode;
end;

function TPDFLibrary.LoadFromFile(const FileName: WideString; const Password: WideString): Integer;
begin
  Result := DefaultInterface.LoadFromFile(FileName, Password);
end;

function TPDFLibrary.MergeDocument(DocumentID: Integer): Integer;
begin
  Result := DefaultInterface.MergeDocument(DocumentID);
end;

function TPDFLibrary.NewDocument: Integer;
begin
  Result := DefaultInterface.NewDocument;
end;

function TPDFLibrary.NewPage: Integer;
begin
  Result := DefaultInterface.NewPage;
end;

function TPDFLibrary.NormalizePage(NormalizeOptions: Integer): Integer;
begin
  Result := DefaultInterface.NormalizePage(NormalizeOptions);
end;

function TPDFLibrary.PageCount: Integer;
begin
  Result := DefaultInterface.PageCount;
end;

function TPDFLibrary.PageHeight: Double;
begin
  Result := DefaultInterface.PageHeight;
end;

function TPDFLibrary.PageRotation: Integer;
begin
  Result := DefaultInterface.PageRotation;
end;

function TPDFLibrary.PageWidth: Double;
begin
  Result := DefaultInterface.PageWidth;
end;

function TPDFLibrary.RemoveDocument(DocumentID: Integer): Integer;
begin
  Result := DefaultInterface.RemoveDocument(DocumentID);
end;

function TPDFLibrary.RotatePage(PageRotation: Integer): Integer;
begin
  Result := DefaultInterface.RotatePage(PageRotation);
end;

function TPDFLibrary.SaveToFile(const FileName: WideString): Integer;
begin
  Result := DefaultInterface.SaveToFile(FileName);
end;

function TPDFLibrary.SecurityInfo(SecurityItem: Integer): Integer;
begin
  Result := DefaultInterface.SecurityInfo(SecurityItem);
end;

function TPDFLibrary.SelectDocument(DocumentID: Integer): Integer;
begin
  Result := DefaultInterface.SelectDocument(DocumentID);
end;

function TPDFLibrary.SelectedDocument: Integer;
begin
  Result := DefaultInterface.SelectedDocument;
end;

function TPDFLibrary.SelectFont(FontID: Integer): Integer;
begin
  Result := DefaultInterface.SelectFont(FontID);
end;

function TPDFLibrary.SelectImage(ImageID: Integer): Integer;
begin
  Result := DefaultInterface.SelectImage(ImageID);
end;

function TPDFLibrary.SelectPage(PageNumber: Integer): Integer;
begin
  Result := DefaultInterface.SelectPage(PageNumber);
end;

function TPDFLibrary.SetBaseURL(const NewBaseURL: WideString): Integer;
begin
  Result := DefaultInterface.SetBaseURL(NewBaseURL);
end;

function TPDFLibrary.SetInformation(Key: Integer; const NewValue: WideString): Integer;
begin
  Result := DefaultInterface.SetInformation(Key, NewValue);
end;

function TPDFLibrary.SetMeasurementUnits(MeasurementUnits: Integer): Integer;
begin
  Result := DefaultInterface.SetMeasurementUnits(MeasurementUnits);
end;

function TPDFLibrary.SetOrigin(Origin: Integer): Integer;
begin
  Result := DefaultInterface.SetOrigin(Origin);
end;

function TPDFLibrary.SetPageBox(BoxType: Integer; Left: Double; Top: Double; Width: Double; 
                                Height: Double): Integer;
begin
  Result := DefaultInterface.SetPageBox(BoxType, Left, Top, Width, Height);
end;

function TPDFLibrary.SetPageDimensions(NewPageWidth: Double; NewPageHeight: Double): Integer;
begin
  Result := DefaultInterface.SetPageDimensions(NewPageWidth, NewPageHeight);
end;

function TPDFLibrary.SetPageLayout(NewPageLayout: Integer): Integer;
begin
  Result := DefaultInterface.SetPageLayout(NewPageLayout);
end;

function TPDFLibrary.SetPageMode(NewPageMode: Integer): Integer;
begin
  Result := DefaultInterface.SetPageMode(NewPageMode);
end;

function TPDFLibrary.SetPageSize(const PaperName: WideString): Integer;
begin
  Result := DefaultInterface.SetPageSize(PaperName);
end;

function TPDFLibrary.SetTextAlign(TextAlign: Integer): Integer;
begin
  Result := DefaultInterface.SetTextAlign(TextAlign);
end;

function TPDFLibrary.SetTextColor(Red: Double; Green: Double; Blue: Double): Integer;
begin
  Result := DefaultInterface.SetTextColor(Red, Green, Blue);
end;

function TPDFLibrary.SetTextSize(TextSize: Double): Integer;
begin
  Result := DefaultInterface.SetTextSize(TextSize);
end;

function TPDFLibrary.SetTextUnderline(Underline: Integer): Integer;
begin
  Result := DefaultInterface.SetTextUnderline(Underline);
end;

procedure Register;
begin
  RegisterComponents(dtlServerPage, [TPDFLibrary]);
end;

end.
