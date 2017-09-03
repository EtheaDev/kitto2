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
unit KIDE.MetadataHelpers;

interface

uses
  EF.DB,
  Kitto.Metadata.Models, Kitto.Metadata.DataView;

type
  TKModelHelper = class helper for TKModel
  public
    procedure SetModelName(const AModelName: string);
    procedure AddDetailReference(const ADetailReference: TKModelDetailReference);
    procedure DeleteDetailReference(const ADetailReference: TKModelDetailReference);
    procedure AddField(const AField: TKModelField);
    procedure DeleteField(const AField: TKModelField);
  end;

  TKModelFieldHelper = class helper for TKModelField
  private
  public
    procedure SetFieldSpec(const ADataType: string; const ASize, AScale: Integer;
      const AIsRequired, AIsKey: Boolean; const AReferencedModel: string);

    procedure SetIsKey(const AValue: Boolean);
    function EqualsColumnInfo(const AColumnInfo: TEFDBColumnInfo): Boolean;
    function EqualsForeignKeyInfo(const AForeignKeyInfo: TEFDBForeignKeyInfo): Boolean;
    procedure AddField(const AField: TKModelField);
    procedure DeleteField(const AField: TKModelField);
  end;

  TKModelDetailReferencesHelper = class helper for TKModelDetailReferences
  public
    procedure AddDetailReference(const ADetailReference: TKModelDetailReference);
    procedure DeleteDetailReference(const ADetailReference: TKModelDetailReference);
  end;

  TKModelDetailReferenceHelper = class helper for TKModelDetailReference
  public
    function EqualsForeignKeyInfo(const AForeignKeyInfo: TEFDBForeignKeyInfo): Boolean;
  end;

implementation

uses
  Types, SysUtils;

{ TKModelHelper }

procedure TKModelHelper.AddDetailReference(
  const ADetailReference: TKModelDetailReference);
begin
  Assert(Assigned(ADetailReference));

  GetDetailReferences.AddDetailReference(ADetailReference);
end;

procedure TKModelHelper.AddField(const AField: TKModelField);
begin
  Assert(Assigned(AField));

  GetFields.AddChild(AField);
end;

procedure TKModelHelper.DeleteDetailReference(
  const ADetailReference: TKModelDetailReference);
begin
  Assert(Assigned(ADetailReference));

  GetDetailReferences.DeleteDetailReference(ADetailReference);
end;

procedure TKModelHelper.DeleteField(const AField: TKModelField);
begin
  Assert(Assigned(AField));

  GetFields.RemoveChild(AField);
end;

procedure TKModelHelper.SetModelName(const AModelName: string);
begin
  SetString('ModelName', AModelName);
  PersistentName := AModelName;
end;

{ TKModelDetailReferencesHelper }

procedure TKModelDetailReferencesHelper.AddDetailReference(
  const ADetailReference: TKModelDetailReference);
begin
  Assert(Assigned(ADetailReference));

  AddChild(ADetailReference);
end;

procedure TKModelDetailReferencesHelper.DeleteDetailReference(
  const ADetailReference: TKModelDetailReference);
begin
  Assert(Assigned(ADetailReference));

  RemoveChild(ADetailReference);
end;

{ TKModelFieldHelper }

procedure TKModelFieldHelper.SetIsKey(const AValue: Boolean);
var
  LDataType: string;
  LSize, LScale: Integer;
  LIsRequired: Boolean;
  LIsKey: Boolean;
  LReferencedModel: string;
begin
  GetFieldSpec(LDataType, LSize, LScale, LIsRequired, LIsKey, LReferencedModel);
  SetFieldSpec(LDataType, LSize, LScale, LIsRequired, AValue, LReferencedModel);
end;

procedure TKModelFieldHelper.AddField(const AField: TKModelField);
begin
  Assert(Assigned(AField));

  GetFields.AddChild(AField);
end;

procedure TKModelFieldHelper.DeleteField(const AField: TKModelField);
begin
  Assert(Assigned(AField));

  GetFields.RemoveChild(AField);
end;

function TKModelFieldHelper.EqualsColumnInfo(
  const AColumnInfo: TEFDBColumnInfo): Boolean;
begin
  Result := False;
  if Assigned(AColumnInfo) then
    Result := SameText(DBColumnName, AColumnInfo.Name)
      and (DataType = AColumnInfo.DataType)
      and (Size = AColumnInfo.Size)
      and (IsRequired = AColumnInfo.IsRequired);
end;

function TKModelFieldHelper.EqualsForeignKeyInfo(
  const AForeignKeyInfo: TEFDBForeignKeyInfo): Boolean;

  function EqualsColumnNames: Boolean;
  var
    I: Integer;
  begin
    Result := FieldCount = AForeignKeyInfo.ColumnCount;
    if Result then
    begin
      for I := 0 to FieldCount - 1 do
      begin
        if AForeignKeyInfo.ColumnNames.IndexOf(Fields[I].DBColumnName) < 0 then
        begin
          Result := False;
          Break;
        end;
      end;
    end;
  end;

begin
  Result := False;
  if Assigned(AForeignKeyInfo) then
    Result := SameText(DBColumnName, AForeignKeyInfo.Name) and EqualsColumnNames;
end;

procedure TKModelFieldHelper.SetFieldSpec(const ADataType: string;
  const ASize, AScale: Integer; const AIsRequired: Boolean; const AIsKey: Boolean;
  const AReferencedModel: string);
var
  LSpec: string;
begin
  LSpec := ADataType;
  if AReferencedModel <> '' then
    LSpec := LSpec + '(' + AReferencedModel + ')'
  else if ASize <> 0 then
  begin
    LSpec := LSpec + '(' + IntToStr(ASize);
    if AScale <> 0 then
      LSpec := LSpec + ', ' + IntToStr(AScale);
    LSpec := LSpec + ')';
  end;
  if AIsRequired then
    LSpec := LSpec + ' not null';
  if AIsKey then
    LSpec := LSpec + ' primary key';
  AsString := LSpec;
end;

{ TKModelDetailReferenceHelper }

function TKModelDetailReferenceHelper.EqualsForeignKeyInfo(
  const AForeignKeyInfo: TEFDBForeignKeyInfo): Boolean;
begin
  Result := False;
  if Assigned(AForeignKeyInfo) then
    Result := SameText(DBForeignKeyName, AForeignKeyInfo.Name);
end;

end.
