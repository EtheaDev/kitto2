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
  gnugettext,
  EF.Intf, EF.Localization;
  
type
  ///	<summary>
  ///	  EF localization tool that wraps dxgettext.
  ///	</summary>
  ///	<remarks>
  ///	  This tool doesn't make use of Id strings.
  ///	</remarks>
  TEFdxgettextLocalizationTool = class(TEFNoRefCountObject, IInterface,
    IEFInterface, IEFLocalizationTool)
  private
    FInstance: TGnuGettextInstance;
  public
    function AsObject: TObject;
    function TranslateString(const AString: string;
      const AIdString: string = ''): string;
    procedure TranslateComponent(const AComponent: TComponent);
    procedure ForceLanguage(const ALanguageId: string);
    function GetCurrentLanguageId: string;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TEFdxgettextLocalizationTool }

procedure TEFdxgettextLocalizationTool.AfterConstruction;
begin
  inherited;
  FInstance := TGnuGettextInstance.Create;
end;

function TEFdxgettextLocalizationTool.AsObject: TObject;
begin
  Result := Self;
end;

destructor TEFdxgettextLocalizationTool.Destroy;
begin
  FreeAndNil(FInstance);
  inherited;
end;

procedure TEFdxgettextLocalizationTool.ForceLanguage(const ALanguageId: string);
begin
  FInstance.UseLanguage(ALanguageId);
end;

function TEFdxgettextLocalizationTool.GetCurrentLanguageId: string;
begin
  Result := FInstance.GetCurrentLanguage;
end;

procedure TEFdxgettextLocalizationTool.TranslateComponent(const AComponent: TComponent);
begin
  FInstance.TranslateComponent(AComponent);
end;

function TEFdxgettextLocalizationTool.TranslateString(const AString,
  AIdString: string): string;
begin
  Result := FInstance.gettext(AString);
end;

initialization
  TEFLocalizationToolRegistry.RegisterTool(TEFdxgettextLocalizationTool.Create);

finalization
  TEFLocalizationToolRegistry.UnregisterTool;
  
end.
