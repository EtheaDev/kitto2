{-------------------------------------------------------------------------------
   Copyright 2012 Ethea S.r.l.

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

///	<summary>
///	  This unit defines a Kitto-specific localization interface to the open
///	  source translation tool dxgettext (GNU gettext for Delphi). It plugs into
///	  Ethea Foundation's localization architecture to allow Kitto applications
///	  to be localized by means of dxgettext.
///	</summary>
unit Kitto.Localization.dxgettext;

{$I Kitto.Defines.inc}

interface

uses
  Classes,
  EF.Intf, EF.Localization;

type
  ///	<summary>
  ///	  Localization tool that wraps dxgettext with Kitto-specific behaviour.
  ///	</summary>
  TKdxgettextLocalizationTool = class(TEFNoRefCountObject, IInterface,
    IEFInterface, IEFLocalizationTool)
  private
    const KITTO_TEXT_DOMAIN = 'Kitto';
  private
    FTextDomainBound: Boolean;
    procedure EnsureTextDomainBound;
  public
    function AsObject: TObject;
    function TranslateString(const AString: string;
      const AIdString: string = ''): string;
    procedure TranslateComponent(const AComponent: TComponent);
    procedure ForceLanguage(const ALanguageId: string);
    function GetCurrentLanguageId: string;
    procedure AfterConstruction; override;
  end;

implementation

uses
  gnugettext,
  Kitto.Config;

{ TKdxgettextLocalizationTool }

procedure TKdxgettextLocalizationTool.AfterConstruction;
begin
  inherited;
  FTextDomainBound := False;
end;

function TKdxgettextLocalizationTool.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKdxgettextLocalizationTool.ForceLanguage(const ALanguageId: string);
begin
  gnugettext.UseLanguage(ALanguageId);
end;

function TKdxgettextLocalizationTool.GetCurrentLanguageId: string;
begin
  Result := gnugettext.GetCurrentLanguage;
end;

procedure TKdxgettextLocalizationTool.TranslateComponent(const AComponent: TComponent);
begin
  EnsureTextDomainBound;
  gnugettext.TranslateComponent(AComponent, KITTO_TEXT_DOMAIN);
  gnugettext.TranslateComponent(AComponent, 'default');
end;

function TKdxgettextLocalizationTool.TranslateString(const AString,
  AIdString: string): string;
begin
  EnsureTextDomainBound;
  // Look in the Kitto text domain first, then in the application domain.
  Result := gnugettext.dgettext(KITTO_TEXT_DOMAIN, AString);
  if Result = AString then
    Result := gnugettext.dgettext('default', AString);
end;

procedure TKdxgettextLocalizationTool.EnsureTextDomainBound;
begin
  if not FTextDomainBound then
  begin
    gnugettext.bindtextdomain(KITTO_TEXT_DOMAIN, TKConfig.Instance.SystemHomePath + 'locale');
    FTextDomainBound := True;
  end;
end;

initialization
  TEFLocalizationToolRegistry.RegisterTool(TKdxgettextLocalizationTool.Create);

finalization
  TEFLocalizationToolRegistry.UnregisterTool;

end.
