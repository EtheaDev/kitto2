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
unit KIDE.EditNodeBaseFrameUnit;

interface

uses
  Winapi.Windows, System.SysUtils, System.Variants, System.Classes, Generics.Collections,
  KIDE.BaseFrameUnit,
  EF.Tree, EF.Types,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Controls, System.Actions, Vcl.ActnList, Vcl.Samples.Spin;


type
  TEditNodeBaseFrame = class;

  TEditNodeBaseFrameClass = class of TEditNodeBaseFrame;

  TEditNodeBaseFrame = class(TBaseFrame)
    ClientPanel: TPanel;
    DesignPanel: TPanel;
    ActionList: TActionList;
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FrameEnter(Sender: TObject);
    procedure EditorChange(Sender: TObject);
  strict private
    FChangesDisabled: Integer;
    FIsChanged: Boolean;
    FEditNode: TEFTree;
    FEmbeddedEditors: TDictionary<TEFNode, TEditNodeBaseFrame>;
    procedure AutoUpdateEditNode; virtual;
    procedure SetIsChanged(const Value: Boolean);
    function GetIsChanged: Boolean;
    function GetChangesDisabled: Boolean;
    procedure SetChangesDisabled(const Value: Boolean);
  strict protected
    ///	<summary>
    ///   Notify that EditNode is changed
    /// </summary>
    property ChangesDisabled: Boolean read GetChangesDisabled write SetChangesDisabled;
    ///	<summary>
    ///   Notify that EditNode is changed
    /// </summary>
    property IsChanged: Boolean read GetIsChanged write SetIsChanged;

    ///	<summary>
    ///   Clears node that have default values
    /// </summary>
    procedure CleanupDefaultsToEditNode; virtual;
    ///	<summary>
    ///   Delete a text child node of EditNode if value is equivalent to ADefaultValue
    /// </summary>
    procedure CleanupTextNode(const ANodeName: string; ADefaultValue: string = '');
    ///	<summary>
    ///   Delete a boolean child node of EditNode if value is equivalent to ADefaultValue
    /// </summary>
    procedure CleanupBooleanNode(const ANodeName: string; ADefaultValue: Boolean = False);
    ///	<summary>
    ///   Delete an integer child node of EditNode if value is equivalent to ADefaultValue
    /// </summary>
    procedure CleanupIntegerNode(const ANodeName: string; ADefaultValue: Integer = 0);
    ///	<summary>
    ///   Delete a node with no child nodes
    /// </summary>
    procedure CleanupOrphanNode(const ANodeName: string);

  protected
    ///	<summary>
    ///   Returns True if the class is suitable for designing the
    ///	  specified Tree or Node.
    /// </summary>
    class function SuitsTree(const ATree: TEFTree): Boolean; virtual;

    ///	<summary>
    ///   Returns True if the class is suitable for designing the
    ///	  specified Tree or Node.
    /// </summary>
    class function SuitsNode(const ANode: TEFNode): Boolean; virtual;

    ///	<summary>
    ///   Copies data from the EditNode to the GUI.
    /// </summary>
    procedure UpdateDesignPanel(const AForce: Boolean); virtual;

    ///	<summary>
    ///   Copies data from the GUI to EditTree. Also performs late
    ///	  validation and raises an exception if GUI data is invalid.
    /// </summary>
    procedure DesignPanelToEditNode; virtual;

    ///	<summary>
    ///   Embed a frame into a WinControl and initialize it.
    /// </summary>
    function EmbedEditNodeFrame(const AParent: TWinControl;
      const AEditNodeClass: TEditNodeBaseFrameClass; const AEditNode: TEFTree;
      const AImmediateRefresh: Boolean = False; const AAlign: TAlign = alClient): TEditNodeBaseFrame;

    ///	<summary>
    ///   Called from ActionListUpdate to change Components properties
    /// </summary>
    procedure UpdateEditComponents; virtual;

    ///	<summary>
    ///   Returns the ControllerClass if the node is a Controller
    /// </summary>
    class function GetControllerClass(const ANode: TEFNode): TClass;

    ///	<summary>
    ///   Set the Node to Edit with the designer
    /// </summary>
    procedure SetEditNode(const ANode: TEFTree); virtual;

    ///	<summary>
    ///   Delete a node and check if exists an embedded editor for it to free
    /// </summary>
    procedure DeleteNode(const ANode: TEFNode); virtual;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    class function GetClassId: string;

    ///	<summary>Initialize Frame with the Node.</summary>
    procedure Init(const ANode: TEFTree); virtual;

    ///	<summary>Re-reads data from the original node, prompting the user if
    ///	there are any pending changes.</summary>
    procedure EditNodeToGUI; virtual;

    ///	<summary>Save GUI components values into EditNode.</summary>
    procedure GUIToEditNode; virtual;

    ///	<summary>
    ///  The node being edited. It is a reference to the original node.
    /// </summary>
    property EditNode: TEFTree read FEditNode;
  end;

  TEditNodeFrameRegistry = class(TEFRegistry)
  private
    class var FInstance: TEditNodeFrameRegistry;
    class function GetInstance: TEditNodeFrameRegistry; static;
  public
    class destructor Destroy;

    class property Instance: TEditNodeFrameRegistry read GetInstance;

    procedure RegisterClass(const AId: string; const AClass: TEditNodeBaseFrameClass);
    property Classes;
  end;

  ///	<summary>
  ///	  Queries the registry to create a specific designer frame for each node.
  //    It is friend to TNodeDesignerFrameRegistry.
  ///	</summary>
  TEditNodeFrameFactory = class
  private
    class var FInstance: TEditNodeFrameFactory;
    class function GetInstance: TEditNodeFrameFactory; static;
  public
    class destructor Destroy;
    class property Instance: TEditNodeFrameFactory read GetInstance;

    ///	<summary>
    ///   Returns the EditNodeFrame Class suitable for the specified node.
    /// </summary>
    function GetEditNodeFrameClass(const ANode: TEFTree): TEditNodeBaseFrameClass;
  end;

implementation

{$R *.dfm}

uses
  Forms, Dialogs, TypInfo,
  Ext.Base, Kitto.Ext.List, Kitto.Ext.Base,
  EF.Classes, KIDE.Utils;

{ TEditNodeFrameRegistry }

class destructor TEditNodeFrameRegistry.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TEditNodeFrameRegistry.GetInstance: TEditNodeFrameRegistry;
begin
  if (FInstance = nil)and(not Application.Terminated) then
    FInstance := TEditNodeFrameRegistry.Create;
  Result := FInstance;
end;

procedure TEditNodeFrameRegistry.RegisterClass(const AId: string;
  const AClass: TEditNodeBaseFrameClass);
begin
  inherited RegisterClass(AId, AClass);
end;

{ TNodeDesignerFrameFactory }

function TEditNodeFrameFactory.GetEditNodeFrameClass(const ANode: TEFTree): TEditNodeBaseFrameClass;
var
  I: Integer;
  LClasses: TArray<TClass>;
  LClass, LClassToCreate: TEditNodeBaseFrameClass;
begin
  Assert(Assigned(ANode));

  LClasses := TEditNodeFrameRegistry.Instance.Classes.Values.ToArray;
  LClassToCreate := nil;
  for I := Low(LClasses) to High(LClasses) do
  begin
    LClass := TEditNodeBaseFrameClass(LClasses[I]);
    if ((ANode is TEFNode) and LClass.SuitsNode(TEFNode(ANode)) or
      ((ANode is TEFTree) and LClass.SuitsTree(ANode))) then
    begin
      if (LClassToCreate <> nil) then
      begin
        if (LClass.InheritsFrom(LClassToCreate)) then
          LClassToCreate := LClass;
      end
      else
        LClassToCreate := LClass;
    end;
  end;
  if LClassToCreate <> nil then
    Result := LClassToCreate
  else
    Result := nil;
end;

class destructor TEditNodeFrameFactory.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TEditNodeFrameFactory.GetInstance: TEditNodeFrameFactory;
begin
  if FInstance = nil then
    FInstance := TEditNodeFrameFactory.Create;
  Result := FInstance;
end;

{ TEditNodeBaseFrame }

procedure TEditNodeBaseFrame.AfterConstruction;
begin
  inherited;
  FEmbeddedEditors := TDictionary<TEFNode, TEditNodeBaseFrame>.Create;
end;

procedure TEditNodeBaseFrame.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FEmbeddedEditors);
  FEditNode := nil;
end;

procedure TEditNodeBaseFrame.Init(const ANode: TEFTree);
begin
  Assert(Assigned(ANode));
  FEditNode := ANode;
  DesignPanel.ShowHint := True;
  if EditNode is TEFNode then
    Hint := TEFNode(EditNode).GetPath
  else if EditNode is TEFPersistentTree then
    Hint := TEFPersistentTree(EditNode).PersistentName;
end;

procedure TEditNodeBaseFrame.SetEditNode(const ANode: TEFTree);
begin
  FEditNode := ANode;
end;

function TEditNodeBaseFrame.GetChangesDisabled: Boolean;
begin
  Result := FChangesDisabled > 0;
end;

procedure TEditNodeBaseFrame.SetChangesDisabled(const Value: Boolean);
begin
  if Value then
    Inc(FChangesDisabled)
  else
    Dec(FChangesDisabled);
end;

procedure TEditNodeBaseFrame.SetIsChanged(const Value: Boolean);
var
  LComponent: TComponent;
  I: Integer;
begin
  if not ChangesDisabled then
  begin
    if (Owner is TEditNodeBaseFrame) then
      TEditNodeBaseFrame(Owner).IsChanged := Value
    else
      FIsChanged := Value;
  end;
end;

class function TEditNodeBaseFrame.SuitsNode(const ANode: TEFNode): Boolean;
begin
  Result := False;
end;

class function TEditNodeBaseFrame.SuitsTree(const ATree: TEFTree): Boolean;
begin
  Result := False;
end;

procedure TEditNodeBaseFrame.EditNodeToGUI;
begin
  Assert(Assigned(FEditNode));
  UpdateDesignPanel(False);
end;

procedure TEditNodeBaseFrame.EditorChange(Sender: TObject);
begin
  IsChanged := True;
end;

procedure TEditNodeBaseFrame.ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  inherited;
  UpdateEditComponents;
end;

procedure TEditNodeBaseFrame.AutoUpdateEditNode;
var
  I: Integer;
  LComponent: TComponent;
  LNodeName: string;
  LChildNode: TEFNode;
  LTextValue: string;
  LStringList: TStrings;
  LDateTimeValue: TDateTime;
  LBooleanValue: Boolean;
  LIntegerValue: Integer;
  LFloatValue: double;
begin
  //Automatically update Nodes Values
  //only for components that name starts with underscore
  for I := 0 to ComponentCount -1 do
  begin
    LComponent := Components[I];
    if Copy(LComponent.Name,1,1) = '_' then
    begin
      LNodeName := Copy(LComponent.Name,2,MaxInt);
      LNodeName := StringReplace(LNodeName,'_','/',[rfReplaceAll]);
      LChildNode := EditNode.FindNode(LNodeName, True);
      if Assigned(LChildNode) then
      begin
        if IsPublishedProp(LComponent, 'Lines') then
        begin
          //TMemo component
          LStringList := GetObjectProp(LComponent, 'Lines') as TStrings;
          LChildNode.AsString := LStringList.Text;
        end
        else if IsPublishedProp(LComponent, 'Date') then
        begin
          //TDateTimePicker component
          LDateTimeValue := GetFloatProp(LComponent, 'Date');
          LChildNode.AsDate := LDateTimeValue;
        end
        else if IsPublishedProp(LComponent, 'Checked') then
        begin
          //TCheckbox component for booleans
          LBooleanValue := Boolean(GetOrdProp(LComponent, 'Checked'));
          LChildNode.AsBoolean := LBooleanValue;
        end
        else if IsPublishedProp(LComponent, 'Value') then
        begin
          //TSpinEdit component for Integers
          LIntegerValue := GetOrdProp(LComponent, 'Value');
          LChildNode.AsInteger := LIntegerValue;
        end
        else if IsPublishedProp(LComponent, 'Text') then
        begin
          //Can be any type of data: use typed node
          LTextValue := GetStrProp(LComponent, 'Text');
          if LChildNode.DataType is TEFDateDataType then
          begin
            TryStrToDate(LTextValue, LDateTimeValue);
            LChildNode.AsDateTime := LDateTimeValue;
          end
          else if LChildNode.DataType is TEFDateTimeDataType then
          begin
            TryStrToDateTime(LTextValue, LDateTimeValue);
            LChildNode.AsDateTime := LDateTimeValue;
          end
          else if LChildNode.DataType is TEFTimeDataType then
          begin
            TryStrToTime(LTextValue, LDateTimeValue);
            LChildNode.AsDateTime := LDateTimeValue;
          end
          else if LChildNode.DataType is TEFBooleanDataType then
          begin
            TryStrToBool(LTextValue, LBooleanValue);
            LChildNode.AsBoolean := LBooleanValue;
          end
          else if LChildNode.DataType is TEFIntegerDataType then
          begin
            TryStrToInt(LTextValue, LIntegerValue);
            LChildNode.AsInteger := LIntegerValue;
          end
          else if LChildNode.DataType is TEFNumericDataTypeBase then
          begin
            TryStrToFloat(LTextValue, LFloatValue);
            LChildNode.AsFloat := LFloatValue;
          end
          else
            LChildNode.AsString := LTextValue;
        end;
      end;
    end;
  end;
end;

procedure TEditNodeBaseFrame.DeleteNode(const ANode: TEFNode);
var
  I: Integer;
  LComponent: TComponent;
  LEmbeddedEditor: TEditNodeBaseFrame;
begin
  if FEmbeddedEditors.ContainsKey(ANode) then
  begin
    LEmbeddedEditor := FEmbeddedEditors[ANode];
    FEmbeddedEditors.Remove(ANode);
    LEmbeddedEditor.Free;
  end;
  ANode.Delete;
end;

procedure TEditNodeBaseFrame.DesignPanelToEditNode;
var
  I: Integer;
  LComponent: TComponent;
begin
  AutoUpdateEditNode;
  //Automatically call GUIToEditNode of every TEditNodeBaseFrame hosted by the editor
  for I := 0 to ComponentCount -1 do
  begin
    LComponent := Components[I];
    if LComponent is TEditNodeBaseFrame then
      TEditNodeBaseFrame(LComponent).DesignPanelToEditNode;
  end;
end;

class function TEditNodeBaseFrame.GetClassId: string;
begin
  Result := ClassName;
end;

class function TEditNodeBaseFrame.GetControllerClass(const ANode: TEFNode): TClass;
begin
  Result := KIDE.Utils.GetControllerClass(ANode);
end;

function TEditNodeBaseFrame.GetIsChanged: Boolean;
begin
  if (Owner is TEditNodeBaseFrame) then
    Result := TEditNodeBaseFrame(Owner).IsChanged
  else
    Result := FIsChanged;
end;

procedure TEditNodeBaseFrame.GUIToEditNode;
begin
  if IsChanged then
  begin
    DesignPanelToEditNode;
    CleanupDefaultsToEditNode;
    IsChanged := False;
  end;
end;

procedure TEditNodeBaseFrame.UpdateDesignPanel(const AForce: Boolean);
var
  I: Integer;
  LComponent: TComponent;
  LNodeName, LNodeCaption: string;
  LChildNode: TEFNode;

  procedure GetTextOrMemoValue(const AValue: string);
  var
    LStringList: TStrings;
  begin
    if IsPublishedProp(LComponent, 'Text') then
      SetStrProp(LComponent, 'Text', AValue)
    else if IsPublishedProp(LComponent, 'Lines') then
    begin
      LStringList := GetObjectProp(LComponent, 'Lines') as TStrings;
      LStringList.Text := AValue;
    end;
  end;

  procedure GetDateValue(const AValue: TDateTime);
  begin
    if IsPublishedProp(LComponent, 'Date') then
      SetFloatProp(LComponent, 'Date', AValue);
  end;

  procedure GetDateTimeValue(const AValue: TDateTime);
  begin
    if IsPublishedProp(LComponent, 'Date') and IsPublishedProp(LComponent, 'Time') then
    begin
      SetFloatProp(LComponent, 'Date', Trunc(AValue));
      SetFloatProp(LComponent, 'Time', AValue - Trunc(AValue));
    end;
  end;

  procedure GetTimeValue(const AValue: TDateTime);
  begin
    if IsPublishedProp(LComponent, 'Time') then
      SetFloatProp(LComponent, 'Time', AValue);
  end;

  procedure GetBooleanValue(const AValue: Boolean);
  begin
    if IsPublishedProp(LComponent, 'Checked') then
      SetOrdProp(LComponent, 'Checked', Ord(AValue))
  end;

  procedure GetIntegerValue(const AValue: Integer);
  begin
    if IsPublishedProp(LComponent, 'Value') then
      SetOrdProp(LComponent, 'Value', AValue)
    else if IsPublishedProp(LComponent, 'Text') then
      SetStrProp(LComponent, 'Text', IntToStr(AValue));
  end;

  procedure GetFloatValue(const AValue: Double);
  begin
    if IsPublishedProp(LComponent, 'Text') then
      SetStrProp(LComponent, 'Text', FloatToStr(AValue));
  end;

begin
  if not (Owner is TEditNodeBaseFrame) and IsChanged then
  begin
    if EditNode is TEFPersistentTree then
      LNodeCaption := ExtractFileName(TEFPersistentTree(EditNode).PersistentName)
    else
      LNodeCaption := EditNode.GetPath;
    if (MessageDlg(Format('Pending changes in designer for: "%s". Discard?',
      [LNodeCaption]), mtWarning, [mbYes, mbNo, mbCancel], 0) <> mrYes) then
      Abort;
  end;

  //Automatically update components content
  //only if component name starts with underscore
  for I := 0 to ComponentCount -1 do
  begin
    LComponent := Components[I];
    if Copy(LComponent.Name,1,1) = '_' then
    begin
      LNodeName := Copy(LComponent.Name,2,MaxInt);
      LNodeName := StringReplace(LNodeName,'_','/',[rfReplaceAll]);
      LChildNode := EditNode.FindNode(LNodeName, False);
      if Assigned(LChildNode) then
      begin
        if LChildNode.DataType is TEFStringDataType then
          GetTextOrMemoValue(LChildNode.AsString)
        else if LChildNode.DataType is TEFMemoDataType then
          GetTextOrMemoValue(LChildNode.AsString)
        else if LChildNode.DataType is TEFDateDataType then
          GetDateValue(LChildNode.AsFloat)
        else if LChildNode.DataType is TEFDateTimeDataType then
          GetDateTimeValue(LChildNode.AsFloat)
        else if LChildNode.DataType is TEFTimeDataType then
          GetTimeValue(LChildNode.AsFloat)
        else if LChildNode.DataType is TEFBooleanDataType then
          GetBooleanValue(LChildNode.AsBoolean)
        else if LChildNode.DataType is TEFIntegerDataType then
          GetIntegerValue(LChildNode.AsInteger)
        else if LChildNode.DataType is TEFNumericDataTypeBase then
          GetFloatValue(LChildNode.AsFloat)
        else
          GetTextOrMemoValue(LChildNode.AsString);
      end
      else
        GetTextOrMemoValue('');
    end
    else if LComponent is TEditNodeBaseFrame then
      TEditNodeBaseFrame(LComponent).UpdateDesignPanel(AForce);

    //Attach EditorChange event to track changes
    if (LComponent is TLabeledEdit) and not Assigned(TLabeledEdit(LComponent).OnChange) then
      TLabeledEdit(LComponent).OnChange := EditorChange
    else if (LComponent is TMemo) and not Assigned(TMemo(LComponent).OnChange) then
      TMemo(LComponent).OnChange := EditorChange
    else if (LComponent is TComboBox) and not Assigned(TComboBox(LComponent).OnChange) then
      TComboBox(LComponent).OnChange := EditorChange
    else if (LComponent is TSpinEdit) and not Assigned(TSpinEdit(LComponent).OnChange) then
      TSpinEdit(LComponent).OnChange := EditorChange
    else if (LComponent is TCheckBox) and not Assigned(TCheckBox(LComponent).OnClick) then
      TCheckBox(LComponent).OnClick := EditorChange
    else if (LComponent is TLabeledEdit) and not Assigned(TLabeledEdit(LComponent).OnChange) then
      TLabeledEdit(LComponent).OnChange := EditorChange;
  end;
end;

procedure TEditNodeBaseFrame.CleanupBooleanNode(const ANodeName: string;
  ADefaultValue: Boolean = False);
var
  LChildNode: TEFNode;
begin
  Assert(Assigned(EditNode));
  LChildNode := EditNode.FindNode(ANodeName);
  if Assigned(LChildNode) then
  begin
    if (LChildNode.AsString = '') or (LChildNode.AsBoolean = ADefaultValue) then
      DeleteNode(LChildNode);
  end;
end;

procedure TEditNodeBaseFrame.CleanupIntegerNode(const ANodeName: string;
  ADefaultValue: Integer = 0);
var
  LChildNode: TEFNode;
begin
  Assert(Assigned(EditNode));
  LChildNode := EditNode.FindNode(ANodeName);
  if Assigned(LChildNode) and (LChildNode.AsInteger = ADefaultValue) then
    DeleteNode(LChildNode);
end;

procedure TEditNodeBaseFrame.CleanupOrphanNode(const ANodeName: string);
var
  LChildNode: TEFNode;
begin
  Assert(Assigned(EditNode));
  LChildNode := EditNode.FindNode(ANodeName);
  if Assigned(LChildNode) and (LChildNode.AsString = '') and (LChildNode.ChildCount = 0) then
  begin
    DeleteNode(LChildNode);
  end;
end;

procedure TEditNodeBaseFrame.CleanupTextNode(const ANodeName: string;
  ADefaultValue: string = '');
var
  LChildNode: TEFNode;
begin
  Assert(Assigned(EditNode));
  LChildNode := EditNode.FindNode(ANodeName);
  if Assigned(LChildNode) and
    (SameText(LChildNode.AsString, ADefaultValue) or (LChildNode.AsString = '')) then
    DeleteNode(LChildNode);
end;

procedure TEditNodeBaseFrame.CleanupDefaultsToEditNode;
var
  I: Integer;
  LComponent: TComponent;
begin
  //Automatically call CleanupDefaultsToEditNode of every TEditNodeBaseFrame hosted by the editor
  for I := ComponentCount -1 downto 0 do
  begin
    LComponent := Components[I];
    if LComponent is TEditNodeBaseFrame then
      TEditNodeBaseFrame(LComponent).CleanupDefaultsToEditNode;
  end;
end;

function TEditNodeBaseFrame.EmbedEditNodeFrame(const AParent: TWinControl;
  const AEditNodeClass: TEditNodeBaseFrameClass; const AEditNode: TEFTree;
  const AImmediateRefresh: Boolean = False; const AAlign: TAlign = alClient): TEditNodeBaseFrame;
var
  LComponentName: string;
begin
  ChangesDisabled := True;
  try
    LComponentName := AParent.Name+'_'+AEditNodeClass.ClassName;
    Result := FindComponent(LComponentName) as TEditNodeBaseFrame;
    if Assigned(Result) then
    begin
      //Frame already created
      if not Assigned(AEditNode) then
        Result.Free
      else
        Result.Init(AEditNode);
    end
    else
    begin
      if Assigned(AEditNode) then
      begin
        Result := AEditNodeClass.Create(self);
        Result.Parent := AParent;
        Result.Align := AAlign;
        Result.Name := LComponentName;
        Result.Init(AEditNode);
        if AEditNode is TEFNode then
          FEmbeddedEditors.Add(TEFNode(AEditNode), Result);
      end;
    end;
  finally
    ChangesDisabled := False;
  end;
  if Assigned(Result) and AImmediateRefresh then
    Result.UpdateDesignPanel(False);
end;

procedure TEditNodeBaseFrame.FrameEnter(Sender: TObject);
begin
  inherited;
  if HelpKeyword <> '' then
    Parent.HelpKeyword := HelpKeyword;
end;

procedure TEditNodeBaseFrame.UpdateEditComponents;
var
  I: Integer;
  LComponent: TComponent;
begin
  //Automatically calls UpdateEditComponents of Embedded Frames
  for I := 0 to ComponentCount -1 do
  begin
    LComponent := Components[I];
    if LComponent is TEditNodeBaseFrame then
      TEditNodeBaseFrame(LComponent).UpdateEditComponents;
  end;
end;

end.
