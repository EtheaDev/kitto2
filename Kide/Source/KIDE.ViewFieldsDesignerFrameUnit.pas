{-------------------------------------------------------------------------------
   Copyright 2012-2021 Ethea S.r.l.

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
unit KIDE.ViewFieldsDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.MetadataItemDesignerFrameUnit,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, Vcl.Tabs, EF.Tree,
  Vcl.StdCtrls, System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.Samples.Spin, Vcl.DBGrids,
  Kitto.Metadata.DataView, Kitto.Metadata.Models,
  KIDE.PairsValuesFrameUnit, KIDE.EditNodeBaseFrameUnit, Data.DB, Datasnap.DBClient, Vcl.Grids,
  KIDE.DbGrid, Vcl.DBCtrls;

type
  TViewFieldsDesignerFrame = class(TMetadataItemDesignerFrame)
    FieldsDataSet: TClientDataSet;
    FieldName: TWideStringField;
    FieldsDBGrid: TDBGrid;
    FieldsDataSource: TDataSource;
    AliasName: TWideStringField;
    Used: TBooleanField;
    PhysicalName: TWideStringField;
    ReferencedModelName: TWideStringField;
    TopPanel: TPanel;
    CheckPanel: TPanel;
    UseModelFieldsCheckBox: TCheckBox;
    ToolBar: TToolBar;
    AddRowActionToolButton: TToolButton;
    DeleteRowActionToolButton: TToolButton;
    MoveDownAction: TAction;
    MoveUpAction: TAction;
    procedure BooleanFieldGetText(Sender: TField; var Text: string; DisplayText: Boolean);
    procedure UseModelFieldsCheckBoxClick(Sender: TObject);
    procedure MoveDownActionUpdate(Sender: TObject);
    procedure MoveUpActionUpdate(Sender: TObject);
    procedure MoveActionExecute(Sender: TObject);
    procedure FieldsDataSourceStateChange(Sender: TObject);
  private
    procedure MoveRecord(const ARecno: Integer);
    function EditViewFields: TKViewFields;
    procedure BuildDataSetFields;
    procedure SaveDataSet;
  protected
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure DesignPanelToEditNode; override;
  public
    procedure Init(const ANode: TEFTree); override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
  end;

implementation

uses
  KIDE.Project, KIDE.NodeDesignerFrameUnit, Kitto.Metadata.ModelImplementation;

{$R *.dfm}

{ TViewFieldsDesignerFrame }

procedure TViewFieldsDesignerFrame.BooleanFieldGetText(Sender: TField; var Text: string;
  DisplayText: Boolean);
begin
  inherited;
  if Sender.AsBoolean then
    Text := '1'
  else
    Text := '0';
end;

procedure TViewFieldsDesignerFrame.CleanupDefaultsToEditNode;
begin
  ;
end;

procedure TViewFieldsDesignerFrame.DesignPanelToEditNode;
var
  LNodeName: string;
  LFieldNode: TKViewField;
  LNewField: TKViewField;
begin
  if UseModelFieldsCheckBox.Checked then
  begin
    EditNode.ClearChildren;
  end
  else
  begin
    SaveDataSet;
    //Update or insert View Fields
    FieldsDataSet.DisableControls;
    try
      FieldsDataSet.First;
      while not FieldsDataSet.Eof do
      begin
        LNodeName := FieldName.AsString;
        //Check existing Field Node to read attributes
        LFieldNode := EditNode.FindNode(LNodeName) as TKViewField;
        if Used.AsBoolean then
        begin
          if Assigned(LFieldNode) then
          begin
            LNewField := TKViewField.Clone(LFieldNode);
            LNewField.Value := AliasName.AsString;
            LFieldNode.Delete;
          end
          else
            LNewField := TKViewField.Create(LNodeName, AliasName.AsString);
          EditNode.AddChild(LNewField);
        end
        else if Assigned(LFieldNode) then
          LFieldNode.Delete;
        FieldsDataSet.Next;
      end;
    finally
      FieldsDataSet.First;
      FieldsDataSet.EnableControls;
    end;
  end;
end;

function TViewFieldsDesignerFrame.EditViewFields: TKViewFields;
begin
  Result := EditNode as TKViewFields;
end;

procedure TViewFieldsDesignerFrame.FieldsDataSourceStateChange(Sender: TObject);
begin
  inherited;
  if FieldsDataSource.State in dsEditModes then
    IsChanged := True;
end;

procedure TViewFieldsDesignerFrame.Init(const ANode: TEFTree);
begin
  if FieldsDataSet.Active then
    FieldsDataSet.Close;
  FieldsDataSet.CreateDataSet;
  inherited;
end;

procedure TViewFieldsDesignerFrame.MoveActionExecute(Sender: TObject);
begin
  inherited;
  FieldsDataSet.DisableControls;
  try
    if Sender = MoveDownAction then
      MoveRecord(FieldsDataSet.RecNo + 1)
    else
      MoveRecord(FieldsDataSet.RecNo - 1);
  finally
    FieldsDataSet.EnableControls;
  end;
end;

procedure TViewFieldsDesignerFrame.MoveDownActionUpdate(Sender: TObject);
begin
  inherited;
  MoveDownAction.Enabled := not UseModelFieldsCheckBox.Checked and
    (FieldsDataSet.RecNo <> FieldsDataSet.RecordCount);
end;

procedure TViewFieldsDesignerFrame.MoveRecord(const ARecno: Integer);
var
  LOldRecNo: Integer;
  LFieldName: string;
  LPhysicalName: string;
  LAliasName: string;
  LReferenceModelName: string;
  LUsed: Boolean;

  procedure SaveRecord;
    var
      LOldUsed: Boolean;

    procedure UpdateField(var LValue: string; Field: TWideStringField);
    var
      LOldValue: string;
    begin
      LOldValue := Field.Value;
      Field.Value := LValue;
      LValue := LOldValue;
    end;

  begin
    FieldsDataSet.Edit;
    try
      UpdateField(LFieldName, FieldName);
      UpdateField(LPhysicalName, PhysicalName);
      UpdateField(LAliasName, AliasName);
      UpdateField(LReferenceModelName, ReferencedModelName);
      LOldUsed := Used.Value;
      Used.Value := LUsed;
      LUsed := LOldUsed;
      FieldsDataSet.Post;
    except
      FieldsDataSet.Cancel;
    end;
  end;

begin
  LFieldName := FieldName.AsString;
  LPhysicalName := PhysicalName.AsString;
  LAliasName := AliasName.AsString;
  LReferenceModelName := ReferencedModelName.AsString;
  LUsed := Used.AsBoolean;
  LOldRecNo := FieldsDataSet.RecNo;
  FieldsDataSet.RecNo := ARecno;
  SaveRecord;
  FieldsDataSet.RecNo := LOldRecNo;
  SaveRecord;
  FieldsDataSet.RecNo := ARecno;
end;

procedure TViewFieldsDesignerFrame.MoveUpActionUpdate(Sender: TObject);
begin
  inherited;
  MoveUpAction.Enabled := not UseModelFieldsCheckBox.Checked and
    (FieldsDataSet.RecNo <> 1);
end;

procedure TViewFieldsDesignerFrame.SaveDataSet;
begin
  if FieldsDataSet.State in dsEditModes then
    FieldsDataSet.Post;
end;

class function TViewFieldsDesignerFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Assert(Assigned(ANode));
  Result := ANode is TKViewFields;
end;

procedure TViewFieldsDesignerFrame.BuildDataSetFields;
var
  LDefault: Boolean;
  LViewTable: TKViewTable;
  LViewField: TKViewField;
  LModel, LReferencedModel: TKModel;
  LModelField, LReferencedModelField: TKModelField;
  LFieldName, LPhysicalName, LReferencedModelName: string;
  I, J: Integer;
  LViewFieldNode: TEFNode;
  LAliasedName: string;

  procedure AddFieldRecord(
    const AFieldName, APhysicalName, AAliasName, AReferenceModelName: string;
    const AUsed: Boolean);
  begin
    FieldsDataSet.AppendRecord([
      AFieldName, //FieldName
      APhysicalName, //PhysicalName
      AAliasName, //AliasName
      AReferenceModelName, //ReferencedModelName
      AUsed//Used
      ]);
  end;

begin
  FieldsDataSet.DisableControls;
  try
    FieldsDataSet.EmptyDataSet;
    LViewTable := EditViewFields.Table;
    if LViewTable.HasModelName then
    begin
      LModel := LViewTable.Model;
      LDefault := UseModelFieldsCheckBox.Checked;
      if LDefault then
      begin
        try
          //By default the view uses model fields:
          //accessing FieldCount materialize default fields
          for I := 0 to LViewTable.FieldCount -1 do
          begin
            LViewField := LViewTable.Fields[I];
            LAliasedName := LViewField.AliasedName;
            LReferencedModel := LViewField.ModelField.ReferencedModel;
            if LViewField.IsReference then
            begin
              LFieldName := LViewField.FieldName;
              LPhysicalName := LReferencedModel.DBTableName+'.'+LReferencedModel.CaptionField.PhysicalName;
              LReferencedModelName := LReferencedModel.ModelName;
              if SameText(LAliasedName, LFieldName) then
                LAliasedName := '';
            end
            else
            begin
              LFieldName := LViewField.FieldName;
              LPhysicalName := LViewField.ModelField.PhysicalName;
              if Assigned(LViewField.ReferenceField) then
                LReferencedModelName := LViewField.ReferenceField.FieldName
              else
                LReferencedModelName := '';
              if SameText(LAliasedName, LFieldName) then
                LAliasedName := '';
            end;
            FieldsDataSet.AppendRecord([
              LFieldName,
              LPhysicalName,
              LAliasedName,
              LReferencedModelName,
              True//Used
              ]);
          end;
        finally
          //we need to delete materialized fields
          LViewTable.FindNode('Fields').ClearChildren;
        end;
      end
      else
      begin
        //Populate dataset with all fields present in model and referenced models
        for I := 0 to LModel.FieldCount -1 do
        begin
          LModelField := LModel.Fields[I];
          if LModelField.IsReference then
          begin
            LReferencedModel := LModelField.ReferencedModel;
            for J := 0 to LReferencedModel.FieldCount -1 do
            begin
              //Use all Fields of referenced model
              LReferencedModelField := LReferencedModel.Fields[J];
              LFieldName := LModelField.FieldName+'.'+LReferencedModelField.FieldName;
              LPhysicalName := LReferencedModel.DBTableName+'.'+LReferencedModelField.FieldName;

              //If referenced field correspond to captionfield use the fieldname of reference field
              if SameText(LReferencedModelField.FieldName, LReferencedModel.CaptionField.FieldName) then
              begin
                LFieldName := LModelField.FieldName;
                LAliasedName := '';
              end;

              //The Aliased Name defined into the view has precedence
              LViewFieldNode := EditViewFields.FindNode(LFieldName);
              if Assigned(LViewFieldNode) then
                LAliasedName := LViewFieldNode.AsString
              else
                LAliasedName := LModelField.FieldName+'_'+LReferencedModelField.FieldName;

              AddFieldRecord(
                LFieldName,
                LPhysicalName,
                LAliasedName,
                LReferencedModel.ModelName,
                Assigned(LViewFieldNode));
            end;
          end
          else
          begin
            LFieldName := LModelField.FieldName;
            LPhysicalName := LModelField.PhysicalName;
            LViewFieldNode := EditViewFields.FindNode(LFieldName);
            if Assigned(LViewFieldNode) then
              LAliasedName := LViewFieldNode.AsString
            else
              LAliasedName := '';
            AddFieldRecord(LFieldName, LPhysicalName, LAliasedName, '',
              Assigned(LViewFieldNode));
          end;
        end;
        //Move Fields in correct order
        for I := 0 to EditNode.ChildCount -1 do
        begin
          LViewFieldNode := EditNode.Children[I];
          if FieldsDataSet.Locate('FieldName', LViewFieldNode.Name, [loCaseInsensitive]) then
            MoveRecord(I+1);
        end;
      end;
    end;
  finally
    FieldsDataSet.First;
    FieldsDataSet.EnableControls;
  end;
end;

procedure TViewFieldsDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
begin
  UseModelFieldsCheckBox.Checked := EditViewFields.ChildCount = 0;
  UseModelFieldsCheckBox.Enabled := EditViewFields.Table.HasModelName;
  BuildDataSetFields;
end;

procedure TViewFieldsDesignerFrame.UseModelFieldsCheckBoxClick(Sender: TObject);
begin
  inherited;
  SaveDataSet;
  if UseModelFieldsCheckBox.Checked then
  begin
    FieldsDBGrid.Options := FieldsDBGrid.Options - [dgEditing, dgAlwaysShowEditor];
    FieldsDBGrid.Columns[0].Visible := False;
  end
  else
  begin
    FieldsDBGrid.Options := FieldsDBGrid.Options + [dgEditing, dgAlwaysShowEditor];
    FieldsDBGrid.Columns[0].Visible := True;
  end;
  BuildDataSetFields;
  IsChanged := True;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TViewFieldsDesignerFrame.GetClassId, TViewFieldsDesignerFrame);

finalization
  if Assigned(TEditNodeFrameRegistry.Instance) then
    TEditNodeFrameRegistry.Instance.UnregisterClass(TViewFieldsDesignerFrame.GetClassId);

end.
