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
    procedure ValidateModel(const AModel: TKModel);
  strict
  private
    procedure ValidateModelField(const AField: TKModelField);
  private
    procedure ValidateDetailReference(
      const ADetailReference: TKModelDetailReference); protected
    procedure InternalExecute; override;
  end;

implementation

uses
  SysUtils,
  EF.Logger,
  KIDE.Project, KIDE.EFHelpers, KIDE.MetadataHelpers;

{ TModelValidator }

procedure TModelValidator.InternalExecute;
var
  I: Integer;
begin
  inherited;
  Log('Validating models...');
  FModels := TProject.CurrentProject.Config.Models;

  for I := 0 to FModels.ModelCount - 1 do
    ValidateModel(FModels.Models[I]);

  if ErrorCount > 0 then
    LogWarning('Model validation complete. Errors were detected.')
  else
    LogInfo('Model validation complete. No errors detected.');
end;

procedure TModelValidator.ValidateModel(const AModel: TKModel);
var
  I: Integer;
  LFoundKey: Boolean;
begin
  Assert(Assigned(AModel));
  Assert(Assigned(FModels));

  LogIndent;
  try
    Log(Format('Validating model %s...', [AModel.ModelName]));

    LFoundKey := False;
    for I := 0 to AModel.FieldCount - 1 do
    begin
      if AModel.Fields[I].IsKey then
      begin
        LFoundKey := True;
        Break;
      end;
    end;
    if not LFoundKey then
      LogWarning('No primary key set. Model will not be updatable and record-level operations will not work correctly.');

    for I := 0 to AModel.FieldCount - 1 do
      ValidateModelField(AModel.Fields[I]);
    for I := 0 to AModel.DetailReferenceCount - 1 do
      ValidateDetailReference(AModel.DetailReferences[I]);
  finally
    LogOutdent;
  end;
end;

procedure TModelValidator.ValidateModelField(const AField: TKModelField);
begin
  Assert(Assigned(AField));
  Assert(Assigned(FModels));

  LogIndent;
  try
    Log(Format('Validating field %s...', [AField.FieldName]));
    if AField.DataType.HasSize and (AField.Size = 0) then
      LogError(Format('Data type %s cannot have zero size.', [AField.DataType.GetTypeName]));

    if not AField.DataType.HasSize and (AField.Size <> 0) then
      LogError(Format('Data type %s cannot have a size.', [AField.DataType.GetTypeName]));

    if AField.DataType is TKReferenceDataType then
    begin
      if AField.ReferencedModel = nil then
        LogError(Format('Referenced model %s not found.', [AField.ReferencedModelName]));
    end;
  finally
    LogOutdent;
  end;
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
