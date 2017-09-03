{*******************************************************************}
{                                                                   }
{   Kide2 Editor: GUI for Kitto2                                    }
{                                                                   }
{   Copyright (c) 2012-2017 Ethea S.r.l.                            }
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
unit KIDE.MRUOptions;

interface

uses
  Classes,
  EF.Tree;

type
  TStringsHelper = class helper for TStrings
  public
    function IndexOfValue(const AValue: string): Integer;
  end;

  TMRUOptions = class(TEFTree)
  private
    FFileName: string;
    const MRU_SIZE = 20;
    class var FInstance: TMRUOptions;
    class function GetInstance: TMRUOptions; static;
    class destructor Destroy;
  public
    class property Instance: TMRUOptions read GetInstance;

    procedure StoreString(const AKey, AValue: string);
    procedure StoreInteger(const AKey: string; const AValue: Integer);
    procedure StoreBoolean(const AKey: string; const AValue: Boolean);
    procedure StoreMRUItem(const AKey, AValue: string);
    procedure StoreNode(const APath: string; const ANode: TEFNode);

    procedure Save;
  end;

implementation

uses
  SysUtils, Types,
  EF.Macros, EF.YAML;

{ TKIMRUOptions }

class destructor TMRUOptions.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TMRUOptions.GetInstance: TMRUOptions;
var
  LFileName: string;
begin
  if not Assigned(FInstance) then
  begin
    LFileName := TEFMacroExpansionEngine.Instance.Expand('%APPDATA%\KIDE\MRU.yaml');
    if FileExists(LFileName) then
      FInstance := TEFTreeFactory.LoadFromFile<TMRUOptions>(LFileName)
    else
      FInstance := TMRUOptions.Create;
    FInstance.FFileName := LFileName;
  end;
  Result := FInstance;
end;

procedure TMRUOptions.Save;
var
  LWriter: TEFYAMLWriter;
begin
  ForceDirectories(ExtractFilePath(FFileName));
  LWriter := TEFYAMLWriter.Create;
  try
    LWriter.SaveTreeToFile(Self, FFileName);
  finally
    FreeAndNil(LWriter);
  end;
end;

procedure TMRUOptions.StoreBoolean(const AKey: string; const AValue: Boolean);
begin
  SetBoolean(AKey, AValue);
  Save;
end;

procedure TMRUOptions.StoreInteger(const AKey: string; const AValue: Integer);
begin
  SetInteger(AKey, AValue);
  Save;
end;

procedure TMRUOptions.StoreMRUItem(const AKey, AValue: string);
var
  LItems: TStrings;
  LItemIndex: Integer;
begin
  LItems := TStringList.Create;
  try
    GetChildrenAsStrings(AKey, LItems);
    LItemIndex := LItems.IndexOfValue(AValue);
    if LItemIndex >= 0 then
      LItems.Delete(LItemIndex);

    while LItems.Count >= MRU_SIZE do
      LItems.Delete(LItems.Count - 1);

    LItems.Insert(0, 'Item=' + AValue);

    SetChildrenAsStrings(AKey, LItems);
    Save;
  finally
    FreeAndNil(LItems);
  end;
end;

procedure TMRUOptions.StoreNode(const APath: string; const ANode: TEFNode);
begin
  GetNode(APath, True).AddChild(TEFNode.Clone(ANode));
  Save;
end;

procedure TMRUOptions.StoreString(const AKey, AValue: string);
begin
  SetString(AKey, AValue);
  Save;
end;

{ TStringsHelper }

function TStringsHelper.IndexOfValue(const AValue: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if CompareStrings(ValueFromIndex[Result], AValue) = 0 then
      Exit;
  Result := -1;
end;

end.
