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
///	<summary>
///	  The model validator should flag:
///	  <list type="bullet">
///	    <item>references (and detail references) to unexisting
///	    models/fields</item>
///	    <item>fields of a sizable data type with no size</item>
///	  </list>
///	</summary>
unit KIDE.ModelValidator;

interface

uses
  Kitto.Metadata.Models,
  KIDE.TreeValidator;

type
  TModelValidator = class(TTreeValidator)
  strict private
    FModels: TKModels;
    procedure ValidateModelField(const AField: TKModelField);
    procedure ValidateModel(const AModel: TKModel);
    procedure ValidateDetailReference(
      const ADetailReference: TKModelDetailReference);
  protected
    procedure InternalExecute; override;
  public
    procedure ValidateModels(ASingleModel: TKModel);
  end;

implementation

uses
  SysUtils,
  EF.Logger,
  KIDE.Project, KIDE.EFHelpers, KIDE.MetadataHelpers;

{ TModelValidator }

procedure TModelValidator.InternalExecute;
begin
  inherited;
  ValidateModels(nil);
end;

procedure TModelValidator.ValidateModel(const AModel: TKModel);
var
  I, J: Integer;
  LFoundKey: Boolean;
  LModelField, LReferencedField: TKModelField;

  procedure CheckDupField(LField: TKModelField);
  var
    LDupField: TKModelField;
  begin
    LDupField := AModel.FieldByName(LField.FieldName);
    if LDupField <> LField then
      LogError(Format('Field "%s" is duplicated!', [LField.FieldName]));
  end;

begin
  Assert(Assigned(AModel));
  Assert(Assigned(FModels));

  LogIndent;
  try
    Log(Format('Validating model %s...', [AModel.ModelName]));

    ValidateTree(AModel);

    LFoundKey := False;
    //Check for primary keys
    for I := 0 to AModel.FieldCount - 1 do
    begin
      LModelField := AModel.Fields[I];
      if LModelField.IsKey then
        LFoundKey := True;
    end;
    if not LFoundKey then
      LogError('No primary key set! Kitto cannot work properly without a primary key.');

    //Check for primary dup fields
    for I := 0 to AModel.FieldCount - 1 do
    begin
      LModelField := AModel.Fields[I];
      CheckDupField(LModelField);
      if LModelField.FieldCount > 0 then
      begin
        for J := 0 to LModelField.FieldCount - 1 do
        begin
          LReferencedField := LModelField.Fields[J];
          CheckDupField(LReferencedField);
        end;
      end;
    end;

    for I := 0 to AModel.FieldCount - 1 do
      ValidateModelField(AModel.Fields[I]);
    for I := 0 to AModel.DetailReferenceCount - 1 do
      ValidateDetailReference(AModel.DetailReferences[I]);
  finally
    LogOutdent;
  end;
end;

procedure TModelValidator.ValidateModelField(const AField: TKModelField);
var
  LFieldMessage: string;
begin
  Assert(Assigned(AField));
  Assert(Assigned(FModels));

  LogIndent;
  try
    LFieldMessage := Format('Validating field "%s"', [AField.FieldName]);
    if AField.DataType.HasSize and (AField.Size = 0) then
      LogError(Format('%s: Data type %s cannot have zero size.', [LFieldMessage, AField.DataType.GetTypeName]));

    if not AField.DataType.HasSize and (AField.Size <> 0) then
      LogError(Format('%s: Data type %s cannot have a size.', [LFieldMessage, AField.DataType.GetTypeName]));

    if AField.DataType is TKReferenceDataType then
    begin
      if AField.ReferencedModel = nil then
        LogError(Format('%s: Referenced model %s not found.', [LFieldMessage, AField.ReferencedModelName]));
    end;
  finally
    LogOutdent;
  end;
end;

procedure TModelValidator.ValidateModels(ASingleModel: TKModel);
var
  I: Integer;
  LModel: TKModel;
begin
  inherited;
  if not Assigned(ASingleModel) then
    Log('Validating models...');
  FModels := TProject.CurrentProject.Config.Models;

  for I := 0 to FModels.ModelCount - 1 do
  begin
    LModel := FModels.Models[I];
    if not Assigned(ASingleModel) or SameText(ASingleModel.ModelName, LModel.ModelName) then
      ValidateModel(LModel);
  end;

  if ErrorCount > 0 then
    LogWarning('Model validation complete. Errors were detected.')
  else
    LogInfo('Model validation complete. No errors detected.');
end;

procedure TModelValidator.ValidateDetailReference(const ADetailReference: TKModelDetailReference);
begin
  Assert(Assigned(ADetailReference));
  Assert(Assigned(FModels));

  LogIndent;
  try
    Log(Format('Validating detail reference %s...', [ADetailReference.DetailReferenceName]));

    if FModels.FindModel(ADetailReference.DetailModelName) = nil then
      LogError(Format('Detail model %s not found.', [ADetailReference.DetailModelName]));
  finally
    LogOutdent;
  end;
end;

end.
