inherited ViewFieldsDesignerFrame: TViewFieldsDesignerFrame
  Width = 691
  Height = 409
  HelpKeyword = 'Models#model-fields-and-their-properties'
  ExplicitWidth = 691
  ExplicitHeight = 409
  inherited ClientPanel: TPanel
    Width = 691
    Height = 409
    ExplicitWidth = 691
    ExplicitHeight = 409
    inherited DesignPanel: TPanel
      Width = 691
      Height = 409
      ExplicitWidth = 691
      ExplicitHeight = 409
      object FieldsDBGrid: TDBGrid
        Left = 0
        Top = 32
        Width = 691
        Height = 377
        Align = alClient
        DataSource = FieldsDataSource
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        Columns = <
          item
            Alignment = taCenter
            Expanded = False
            FieldName = 'Used'
            Title.Alignment = taCenter
            Width = 37
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'FieldName'
            ReadOnly = True
            Width = 117
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'PhysicalName'
            ReadOnly = True
            Width = 157
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'ReferencedModelName'
            ReadOnly = True
            Width = 100
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'AliasName'
            Width = 169
            Visible = True
          end>
      end
      object TopPanel: TPanel
        Left = 0
        Top = 0
        Width = 691
        Height = 32
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object CheckPanel: TPanel
          Left = 0
          Top = 0
          Width = 193
          Height = 32
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 0
          ExplicitHeight = 34
          object UseModelFieldsCheckBox: TCheckBox
            Left = 5
            Top = 1
            Width = 189
            Height = 22
            Caption = 'Use model fields (reset View fields)'
            TabOrder = 0
            OnClick = UseModelFieldsCheckBoxClick
          end
        end
        object ToolBar: TToolBar
          Left = 193
          Top = 0
          Width = 498
          Align = alClient
          AutoSize = True
          BorderWidth = 1
          ButtonHeight = 26
          ButtonWidth = 26
          Images = MainDataModule.Images
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          ExplicitHeight = 34
          object AddRowActionToolButton: TToolButton
            Left = 0
            Top = 0
            Action = MoveDownAction
          end
          object DeleteRowActionToolButton: TToolButton
            Left = 26
            Top = 0
            Action = MoveUpAction
          end
        end
      end
    end
  end
  inherited ActionList: TActionList
    Left = 296
    Top = 328
    object MoveDownAction: TAction
      Category = 'Edit'
      Caption = 'Move Down'
      Hint = 'Move down current line'
      ImageIndex = 53
      OnExecute = MoveActionExecute
      OnUpdate = MoveDownActionUpdate
    end
    object MoveUpAction: TAction
      Category = 'Edit'
      Caption = 'Move Up'
      Hint = 'Move up current line'
      ImageIndex = 54
      OnExecute = MoveActionExecute
      OnUpdate = MoveUpActionUpdate
    end
  end
  object FieldsDataSet: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 448
    Top = 176
    object FieldName: TWideStringField
      DisplayLabel = 'Field Name'
      FieldName = 'FieldName'
      Size = 1000
    end
    object PhysicalName: TWideStringField
      DisplayLabel = 'Physical Name'
      FieldName = 'PhysicalName'
      Size = 1000
    end
    object AliasName: TWideStringField
      DisplayLabel = 'Alias Name'
      FieldName = 'AliasName'
      Size = 1000
    end
    object ReferencedModelName: TWideStringField
      DisplayLabel = 'Referenced Model'
      FieldName = 'ReferencedModelName'
      Size = 1000
    end
    object Used: TBooleanField
      DisplayLabel = 'Use it'
      FieldName = 'Used'
      OnGetText = BooleanFieldGetText
    end
  end
  object FieldsDataSource: TDataSource
    DataSet = FieldsDataSet
    OnStateChange = FieldsDataSourceStateChange
    Left = 256
    Top = 264
  end
end
