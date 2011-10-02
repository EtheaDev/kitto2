{*******************************************************************}
{                                                                   }
{   Ethea Foundation                                                }
{   Localization Stub                                               }
{                                                                   }
{   Copyright (c) 2006-2010 Ethea Srl                               }
{   ALL RIGHTS RESERVED / TUTTI I DIRITTI RISERVATI                 }
{                                                                   }
{*******************************************************************}
{                                                                   }
{   The entire contents of this file is protected by                }
{   International Copyright Laws. Unauthorized reproduction,        }
{   reverse-engineering, and distribution of all or any portion of  }
{   the code contained in this file is strictly prohibited and may  }
{   result in severe civil and criminal penalties and will be       }
{   prosecuted to the maximum extent possible under the law.        }
{                                                                   }
{   RESTRICTIONS                                                    }
{                                                                   }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED      }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE        }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE       }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT  }
{   AND PERMISSION FROM ETHEA S.R.L.                                }
{                                                                   }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON       }
{   ADDITIONAL RESTRICTIONS.                                        }
{                                                                   }
{*******************************************************************}
{                                                                   }
{   Il contenuto di questo file è protetto dalle leggi              }
{   internazionali sul Copyright. Sono vietate la riproduzione, il  }
{   reverse-engineering e la distribuzione non autorizzate di tutto }
{   o parte del codice contenuto in questo file. Ogni infrazione    }
{   sarà perseguita civilmente e penalmente a termini di legge.     }
{                                                                   }
{   RESTRIZIONI                                                     }
{                                                                   }
{   SONO VIETATE, SENZA IL CONSENSO SCRITTO DA PARTE DI             }
{   ETHEA S.R.L., LA COPIA, LA VENDITA, LA DISTRIBUZIONE E IL       }
{   TRASFERIMENTO A TERZI, A QUALUNQUE TITOLO, DEL CODICE SORGENTE  }
{   CONTENUTO IN QUESTO FILE E ALTRI FILE AD ESSO COLLEGATI.        }
{                                                                   }
{   SI FACCIA RIFERIMENTO ALLA LICENZA D'USO PER INFORMAZIONI SU    }
{   EVENTUALI RESTRIZIONI ULTERIORI.                                }
{                                                                   }
{*******************************************************************} 

{
  This unit defines an interface for localization libraries to plug into.
  All the units in Ethea Foundations send their strings to this stub, which
  in turn directs them to whatever localization tool is used to localize an
  application.@br

  To plug a localization tool, you need to implement IEFLocalizationTool in a
  class of your choice and register it by calling
  LocalizationToolRegistry.RegisterLocalizationTool. This is usually done in
  a unit's initialization section.
}
unit EF.Localization;

interface

uses
  Classes,
  EF.Intf;

type
  {
    The localization tool interface.
  }
  IEFLocalizationTool = interface(IEFInterface)
    ['{68D1ED40-99EE-44B4-BEBE-0E07700E4C61}']
    {
      Translates a string in the default language to a string in the current
      language.
      Some tools file strings giving an Id to each of them. A particular tool
      might require you to pass AIdString, an opaque string that uniquely
      identifies the string to translate.
    }
    function TranslateString(const AString: string; const AIdString: string = ''): string;
    {
      Translates all the string properties in Component and all the components
      it owns from the default language to the current language. It is usually
      called for each dfm-based form that is created.
    }
    procedure TranslateComponent(const AComponent: TComponent);
    {
      Forces the use of a particular language instead of the system default.
    }
    procedure ForceLanguage(const ALanguageId: string);
    {
      Returns the Id of the currently used language.
    }
    function GetCurrentLanguageId: string;
  end;

  {
    Keeps track of the currently active localization tool.
  }
  TEFLocalizationToolRegistry = class
  private
    FCurrentTool: IEFLocalizationTool;
    function GetCurrentTool: IEFLocalizationTool;
  public
    destructor Destroy; override;
    {
      Accesses the current tool. Use the EFLocalizationTool global function
      as a shortcut.
    }
    property CurrentTool: IEFLocalizationTool read GetCurrentTool;
    {
      Sets the current tool to ATool. Pass nil to set the current tool back
      to the default tool.
    }
    procedure RegisterTool(const ATool: IEFLocalizationTool);
    {
      Same as @code(RegisterTool(nil);).
    }
    procedure UnregisterTool;
  end;

  {
    This class implements the default tool, which does nothing.
  }
  TEFNullLocalizationTool = class(TEFNoRefCountObject, IInterface,
    IEFInterface, IEFLocalizationTool)
  public
    {
      Implements IEFInterface.AsObject.
    }
    function AsObject: TObject;
    {
      Implements IEFLocalizationTool.TranslateString.
    }
    function TranslateString(const AString: string;
      const AIdString: string = ''): string;
    {
      Implements IEFLocalizationTool.TranslateComponent.
    }
    procedure TranslateComponent(const AComponent: TComponent);
    {
      Implements IEFLocalizationTool.ForceLanguage.
    }
    procedure ForceLanguage(const ALanguageId: string);
    {
      Implements IEFLocalizationTool.GetCurrentLanguageId.
    }
    function GetCurrentLanguageId: string;
  end;

{
  Singleton access to the localization tool registry. Used by localization
  tools.
}
function EFLocalizationToolRegistry: TEFLocalizationToolRegistry;

{
  Singleton access to the current localization tool. Used by localization tool
  users.
}
function EFLocalizationTool: IEFLocalizationTool;

{
  Shortcut for EFLocalizationTool.TranslateString.
}
function TranslateString(const AString: string; const AIdString: string = ''): string;

{
  Shortcut for EFLocalizationTool.TranslateString.
}
function _(const AString: string; const AIdString: string = ''): string;

implementation

uses
  SysUtils;

var
  _EFLocalizationToolRegistry: TEFLocalizationToolRegistry;

function EFLocalizationToolRegistry: TEFLocalizationToolRegistry;
begin
  if not Assigned(_EFLocalizationToolRegistry) then
    _EFLocalizationToolRegistry := TEFLocalizationToolRegistry.Create;
  Result := _EFLocalizationToolRegistry;
end;

function EFLocalizationTool: IEFLocalizationTool;
begin
  Result := EFLocalizationToolRegistry.CurrentTool;
end;

function TranslateString(const AString: string; const AIdString: string = ''): string;
begin
  if AString = '' then
    Result := ''
  else
    Result := EFLocalizationTool.TranslateString(AString, AIdString);
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

destructor TEFLocalizationToolRegistry.Destroy;
begin
  FreeAndNilEFIntf(FCurrentTool);
  inherited;
end;

function TEFLocalizationToolRegistry.GetCurrentTool: IEFLocalizationTool;
begin
  Result := FCurrentTool;
end;

procedure TEFLocalizationToolRegistry.RegisterTool(
  const ATool: IEFLocalizationTool);
begin
  FreeAndNilEFIntf(FCurrentTool);
  FCurrentTool := ATool;
end;

procedure TEFLocalizationToolRegistry.UnregisterTool;
begin
  RegisterTool(nil);
end;

initialization
  EFLocalizationToolRegistry.RegisterTool(TEFNullLocalizationTool.Create);

finalization
  EFLocalizationToolRegistry.UnregisterTool;
  
  FreeAndNil(_EFLocalizationToolRegistry);

end.
