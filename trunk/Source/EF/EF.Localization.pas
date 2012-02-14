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
///	  This unit defines an interface for localization libraries to plug into.
///	  All the units in EF send their strings to this stub, which in turn
///	  directs them to whatever localization tool is used to localize an
///	  application.@br To plug a localization tool, you need to implement
///	  IEFLocalizationTool in a class of your choice and register it by calling
///	  TEFLocalizationToolRegistry.RegisterLocalizationTool. This is usually done
///	  in a unit's initialization section.
///	</summary>
unit EF.Localization;

{$I EF.Defines.inc}

interface

uses
  Classes,
  EF.Intf;

type
  ///	<summary>
  ///	  The localization tool interface.
  ///	</summary>
  IEFLocalizationTool = interface(IEFInterface)
    ['{68D1ED40-99EE-44B4-BEBE-0E07700E4C61}']

    ///	<summary>Translates a string in the default language to a string in the
    ///	current language. Some tools file strings giving an Id to each of them.
    ///	A particular tool might require you to pass AIdString, an opaque string
    ///	that uniquely identifies the string to translate.</summary>
    function TranslateString(const AString: string; const AIdString: string = ''): string;

    ///	<summary>
    ///	  Translates all the string properties in Component and all the
    ///	  components it owns from the default language to the current language.
    ///	  It is usually called for each dfm-based form that is created.
    ///	</summary>
    procedure TranslateComponent(const AComponent: TComponent);

    ///	<summary>
    ///	  Forces the use of a particular language instead of the system default.
    ///	</summary>
    procedure ForceLanguage(const ALanguageId: string);

    ///	<summary>
    ///	  Returns the Id of the currently used language.
    ///	</summary>
    function GetCurrentLanguageId: string;
  end;

  ///	<summary>
  ///	  Keeps track of the currently active localization tool.
  ///	</summary>
  TEFLocalizationToolRegistry = class
  private
    class var FCurrentTool: IEFLocalizationTool;
    class function GetCurrentTool: IEFLocalizationTool; static;
  protected
    class destructor Destroy;
  public

    ///	<summary>
    ///	  Accesses the current tool. Use the EFLocalizationTool global function
    ///	  as a shortcut.
    ///	</summary>
    class property CurrentTool: IEFLocalizationTool read GetCurrentTool;

    ///	<summary>
    ///	  Sets the current tool to ATool. Pass nil to set the current tool back
    ///	  to the default tool.
    ///	</summary>
    class procedure RegisterTool(const ATool: IEFLocalizationTool);

    ///	<summary>
    ///	  Same as RegisterTool(nil);
    ///	</summary>
    class procedure UnregisterTool;
  end;

  ///	<summary>
  ///	  This class implements the default localization tool, which does nothing.
  ///	</summary>
  TEFNullLocalizationTool = class(TEFNoRefCountObject, IInterface,
    IEFInterface, IEFLocalizationTool)
  public
    function AsObject: TObject;
    function TranslateString(const AString: string;
      const AIdString: string = ''): string;
    procedure TranslateComponent(const AComponent: TComponent);
    procedure ForceLanguage(const ALanguageId: string);
    function GetCurrentLanguageId: string;
  end;

///	<summary>Shortcut for
///	TEFLocalizationToolRegistry.CurrentTool.TranslateString.</summary>
///	<param name="AString">String to translate. If the string is enclosed by
///	'_(' and ')', these characters are stripped.</param>
function TranslateString(const AString: string; const AIdString: string = ''): string;

///	<summary>Shortcut for
///	TEFLocalizationToolRegistry.CurrentTool.TranslateString.</summary>
///	<param name="AString">String to translate. If the string is enclosed by
///	'_(' and ')', these characters are stripped.</param>
function _(const AString: string; const AIdString: string = ''): string;

implementation

uses
  SysUtils,
  EF.StrUtils;

function EFLocalizationTool: IEFLocalizationTool;
begin
  Result := TEFLocalizationToolRegistry.CurrentTool;
end;

function TranslateString(const AString: string; const AIdString: string = ''): string;
begin
  if AString = '' then
    Result := ''
  else
    Result := EFLocalizationTool.TranslateString(StripPrefixAndSuffix(AString, '_(', ')'), AIdString);
end;

function _(const AString: string; const AIdString: string = ''): string;
begin
  Result := TranslateString(AString, AIdString);
end;

{ TEFNullLocalizationTool }

function TEFNullLocalizationTool.AsObject: TObject;
begin
  Result := Self;
end;

procedure TEFNullLocalizationTool.ForceLanguage(const ALanguageId: string);
begin
end;

function TEFNullLocalizationTool.GetCurrentLanguageId: string;
begin
  Result := '';
end;

procedure TEFNullLocalizationTool.TranslateComponent(const AComponent: TComponent);
begin
end;

function TEFNullLocalizationTool.TranslateString(const AString: string;
  const AIdString: string = ''): string;
begin
  Result := AString;
end;

{ TEFLocalizationToolRegistry }

class destructor TEFLocalizationToolRegistry.Destroy;
begin
  FreeAndNilEFIntf(FCurrentTool);
end;

class function TEFLocalizationToolRegistry.GetCurrentTool: IEFLocalizationTool;
begin
  Result := FCurrentTool;
end;

class procedure TEFLocalizationToolRegistry.RegisterTool(
  const ATool: IEFLocalizationTool);
begin
  FreeAndNilEFIntf(FCurrentTool);
  FCurrentTool := ATool;
end;

class procedure TEFLocalizationToolRegistry.UnregisterTool;
begin
  RegisterTool(nil);
end;

initialization
  TEFLocalizationToolRegistry.RegisterTool(TEFNullLocalizationTool.Create);

finalization
  TEFLocalizationToolRegistry.UnregisterTool;

end.
