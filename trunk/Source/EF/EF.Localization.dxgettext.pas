///	<summary>
///	  This unit defines a localization interface to the open source translation
///	  tool dxgettext (GNU gettext for Delphi). It plugs into Ethea Foundation's
///	  localization architecture to allow EF code to be localized by means of
///	  dxgettext.
///	</summary>
unit EF.Localization.dxgettext;

{$I EF.Defines.inc}

interface

uses
  Classes,
  EF.Intf, EF.Localization;
  
type
  {
    Localization tool that wraps dxgettext.
  }
  TEFdxgettextLocalizationTool = class(TEFNoRefCountObject, IInterface,
    IEFInterface, IEFLocalizationTool)
  public
    // IEFInterface
    function AsObject: TObject;
    // IEFLocalizationTool
    // Note: this tool doesn't make use of Id strings.
    function TranslateString(const AString: string;
      const AIdString: string = ''): string;
    procedure TranslateComponent(const AComponent: TComponent);
    procedure ForceLanguage(const ALanguageId: string);
    function GetCurrentLanguageId: string;
    procedure AfterConstruction; override;
  end;

implementation

uses
  gnugettext;
  
{ TEFdxgettextLocalizationTool }

procedure TEFdxgettextLocalizationTool.AfterConstruction;
begin
  inherited;
end;

function TEFdxgettextLocalizationTool.AsObject: TObject;
begin
  Result := Self;
end;

procedure TEFdxgettextLocalizationTool.ForceLanguage(const ALanguageId: string);
begin
  gnugettext.UseLanguage(ALanguageId);
end;

function TEFdxgettextLocalizationTool.GetCurrentLanguageId: string;
begin
  Result := gnugettext.GetCurrentLanguage;
end;

procedure TEFdxgettextLocalizationTool.TranslateComponent(const AComponent: TComponent);
begin
  gnugettext.TranslateComponent(AComponent);
end;

function TEFdxgettextLocalizationTool.TranslateString(const AString,
  AIdString: string): string;
begin
  Result := gnugettext.gettext(AString);
end;

initialization
  EFLocalizationToolRegistry.RegisterTool(TEFdxgettextLocalizationTool.Create);

finalization
  EFLocalizationToolRegistry.UnregisterTool;
  
end.
