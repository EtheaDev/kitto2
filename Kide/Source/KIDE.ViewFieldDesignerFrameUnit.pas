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
unit KIDE.ViewFieldDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.MetadataItemDesignerFrameUnit,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, Vcl.Tabs, EF.Tree,
  Vcl.StdCtrls, System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.Samples.Spin,
  Kitto.Metadata.DataView, Kitto.Metadata.Models, KIDE.ModelFieldDesignerFrameUnit,
  KIDE.PairsValuesFrameUnit, KIDE.EditNodeBaseFrameUnit;

type
  TViewFieldDesignerFrame = class(TModelFieldDesignerFrame)
  private
    function GetEditViewField: TKViewField;
  protected
    function GetAllowedValuesNode(const ACreateIfMissing: Boolean = False): TEFNode; override;
    function GetAutoAddFieldsNode(const ACreateIfMissing: Boolean = False): TEFNode; override;
    function GetRulesNode(const ACreateIfMissing: Boolean = False): TEFNode; override;
    function GetEditModelField: TKModelField; override;
    class function SuitsNode(const ANode: TEFNode): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean = False); override;
    procedure DesignPanelToEditNode; override;
    property EditViewField: TKViewField read GetEditViewField;
    property EditModelField: TKModelField read GetEditModelField;
  public
  end;

implementation

uses
  KIDE.Project, KIDE.NodeDesignerFrameUnit, Kitto.Metadata.ModelImplementation;

{$R *.dfm}

{ TModelFieldDesignerFrame }

procedure TViewFieldDesignerFrame.CleanupDefaultsToEditNode;
var
  LDefaultValue: Variant;
  LNode: TEFNode;
begin
  if Assigned(EditModelField) then
  begin
    //View Field values identical to Model Field values are cleared
    CleanupTextNode('PhysicalName', EditModelField.PhysicalName);
    CleanupTextNode('DisplayLabel', EditModelField.DisplayLabel);
    CleanupBooleanNode('IsVisible', EditModelField.IsVisible);
    CleanupBooleanNode('IsReadOnly', EditModelField.IsReadOnly);
    CleanupBooleanNode('IsRequired', EditModelField.IsRequired);
    CleanupBooleanNode('IsGenerated', EditModelField.IsGenerated);
    CleanupTextNode('Hint', EditModelField.Hint);
    CleanupIntegerNode('DisplayWidth', EditModelField.DisplayWidth);
    CleanupTextNode('DisplayFormat', EditModelField.DisplayFormat);
    CleanupTextNode('EditFormat', EditModelField.EditFormat);
    LDefaultValue := EvalExpression(EditModelField.GetValue('DefaultValue'));
    CleanupTextNode('DefaultValue', VarToStr(LDefaultValue));
    CleanupTextNode('DefaultFilterConnector', EditModelField.DefaultFilterConnector);
    CleanupTextNode('DefaultFilter', EditModelField.DefaultFilter);
    CleanupTextNode('LookupFilter', EditModelField.LookupFilter);
    CleanupBooleanNode('EmptyAsNull', EditModelField.EmptyAsNull);
    CleanupBooleanNode('CanUpdate', EditModelField.CanUpdate);
    CleanupBooleanNode('CanInsert', EditModelField.CanInsert);
    CleanupTextNode('FileNameField', EditModelField.FileNameField);
    CleanupTextNode('Expression', EditModelField.Expression);
    CleanupIntegerNode('AutoCompleteMinChars', EditModelField.AutoCompleteMinChars);
    if EditModelField.GetChildrenAsStrings('AllowedValues') = EditViewField.GetChildrenAsStrings('AllowedValues') then
      EditViewField.DeleteNode('AllowedValues')
    else
      CleanupOrphanNode('AllowedValues');
    LNode := EditNode.FindNode('IsPicture/Thumbnail/Width');
    if Assigned(LNode) and (EditModelField.GetInteger('IsPicture/Thumbnail/Width')=LNode.AsInteger) then
      EditNode.DeleteNode('IsPicture/Thumbnail/Width');
    LNode := EditNode.FindNode('IsPicture/Thumbnail/Height');
    if Assigned(LNode) and (EditModelField.GetInteger('IsPicture/Thumbnail/Height')=LNode.AsInteger) then
      EditNode.DeleteNode('IsPicture/Thumbnail/Height');
    CleanupBooleanNode('IsPicture', EditModelField.GetBoolean('IsPicture'));
    CleanupTextNode('MaxUploadSize', EditModelField.GetString('MaxUploadSize'));
    CleanupBooleanNode('IsComputed', EditModelField.GetBoolean('IsComputed'));
    CleanupBooleanNode('IsPassword', EditModelField.GetBoolean('IsPassword'));
  end;
end;

procedure TViewFieldDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  EditorPageControl.ActivePageIndex := 0;
  EditViewField.Name := FieldNameEdit.Text;
end;

function TViewFieldDesignerFrame.GetAllowedValuesNode(
  const ACreateIfMissing: Boolean = False): TEFNode;
begin
  Assert(Assigned(EditNode));
  Result := EditNode.FindNode('AllowedValues', ACreateIfMissing);
  if Result = nil then
    Result := EditModelField.FindNode('AllowedValues', False);
end;

function TViewFieldDesignerFrame.GetAutoAddFieldsNode(
  const ACreateIfMissing: Boolean): TEFNode;
begin
  Assert(Assigned(EditNode));
  Result := EditNode.FindNode('AutoAddFields', ACreateIfMissing);
  if Result = nil then
    Result := EditModelField.FindNode('AutoAddFields', False);
end;

function TViewFieldDesignerFrame.GetEditModelField: TKModelField;
begin
  if Assigned(EditViewField) and (EditViewField.HasModelField) then
    Result := EditViewField.ModelField
  else
    Result := nil;
end;

function TViewFieldDesignerFrame.GetEditViewField: TKViewField;
begin
  Result := EditNode as TKViewField;
end;

function TViewFieldDesignerFrame.GetRulesNode(
  const ACreateIfMissing: Boolean): TEFNode;
begin
  Assert(Assigned(EditNode));
  Result := EditNode.FindNode('Rules', ACreateIfMissing);
  if Result = nil then
    Result := EditModelField.FindNode('Rules', False);
end;

class function TViewFieldDesignerFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Assert(Assigned(ANode));
  Result := (ANode is TKViewField) and TKViewField(ANode).HasModelField;
end;

procedure TViewFieldDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LDataTypeStr: string;
begin
  inherited;
  EditorPageControl.ActivePageIndex := 0;
  _IsVisible.Checked := EditNode.GetBoolean('IsVisible', True);
  FieldNameEdit.Text := EditViewField.Name;
  _PhysicalName.Visible := False;

  LDataTypeStr := EditViewField.DataType.GetTypeName;
  TypeComboBox.Enabled := False;

  FieldSizeEdit.Text := IntToStr(EditViewField.Size);
  FieldSizeEdit.Enabled := False;

  DecimalPrecisionEdit.Text := IntToStr(EditViewField.DecimalPrecision);
  DecimalPrecisionEdit.Enabled := False;

  PrimaryKeyCheckBox.Checked := EditViewField.IsKey;
  PrimaryKeyCheckBox.Enabled := False;

  RequiredCheckBox.Checked := EditViewField.IsRequired;

  if Assigned(EditViewField.ModelField) then
    ReferenceModelEditComboBox.Text := EditViewField.ModelField.ReferencedModelName;
end;

initialization
  TEditNodeFrameRegistry.Instance.RegisterClass(TViewFieldDesignerFrame.GetClassId, TViewFieldDesignerFrame);

finalization
  TEditNodeFrameRegistry.Instance.UnregisterClass(TViewFieldDesignerFrame.GetClassId);

end.
