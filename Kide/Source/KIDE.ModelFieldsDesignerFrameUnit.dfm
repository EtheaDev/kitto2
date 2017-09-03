inherited ModelFieldsDesignerFrame: TModelFieldsDesignerFrame
  Width = 1139
  Height = 561
  HelpKeyword = 'Models#model-fields-and-their-properties'
  ExplicitWidth = 1139
  ExplicitHeight = 561
  inherited ClientPanel: TPanel
    Width = 1139
    Height = 561
    ExplicitWidth = 1139
    ExplicitHeight = 561
    inherited DesignPanel: TPanel
      Width = 1139
      Height = 561
      Padding.Top = 30
      ExplicitWidth = 1139
      ExplicitHeight = 561
      object FieldsLabel: TLabel
        Left = 10
        Top = 9
        Width = 27
        Height = 13
        Caption = 'Fields'
      end
      object FieldsDBGrid: TDBGrid
        Left = 0
        Top = 30
        Width = 1139
        Height = 531
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
            Expanded = False
            FieldName = 'FieldName'
            Width = 100
            Visible = True
          end
          item
            Expanded = False
            FieldName = '_PhysicalName'
            Width = 100
            Visible = True
          end
          item
            Expanded = False
            FieldName = '_DisplayLabel'
            Width = 138
            Visible = True
          end
          item
            Expanded = False
            FieldName = '_DefaultValue'
            Width = 100
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'TypeComboBox'
            Width = 100
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'ReferenceModelEditComboBox'
            Width = 100
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'FieldSizeEdit'
            Width = 38
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'DecimalPrecisionEdit'
            Width = 39
            Visible = True
          end
          item
            Alignment = taCenter
            Expanded = False
            FieldName = 'PrimaryKeyCheckBox'
            Width = 70
            Visible = True
          end
          item
            Alignment = taCenter
            Expanded = False
            FieldName = 'RequiredCheckBox'
            Width = 70
            Visible = True
          end
          item
            Alignment = taCenter
            Expanded = False
            FieldName = '_IsVisible'
            Width = 70
            Visible = True
          end
          item
            Alignment = taCenter
            Expanded = False
            FieldName = '_IsReadOnly'
            Width = 70
            Visible = True
          end
          item
            Alignment = taCenter
            Expanded = False
            FieldName = '_EmptyAsNull'
            Width = 70
            Visible = True
          end
          item
            Expanded = False
            FieldName = '_DisplayWidth'
            Width = 70
            Visible = True
          end
          item
            Expanded = False
            FieldName = '_DisplayFormat'
            Width = 70
            Visible = True
          end
          item
            Expanded = False
            FieldName = '_EditFormat'
            Width = 70
            Visible = True
          end
          item
            Alignment = taCenter
            Expanded = False
            FieldName = '_IsPicture'
            Width = 70
            Visible = True
          end
          item
            Expanded = False
            FieldName = '_FileNameField'
            Width = 100
            Visible = True
          end
          item
            Expanded = False
            FieldName = '_IsPassword'
            Width = 70
            Visible = True
          end>
      end
      object DBNavigator: TDBNavigator
        Left = 48
        Top = 5
        Width = 110
        Height = 21
        DataSource = FieldsDataSource
        VisibleButtons = [nbPrior, nbNext, nbDelete, nbPost, nbCancel]
        Flat = True
        TabOrder = 1
      end
    end
  end
  inherited ActionList: TActionList
    Left = 296
    Top = 328
  end
  object FieldsDataSet: TClientDataSet
    Aggregates = <>
    Params = <>
    BeforeCancel = FieldsDataSetBeforeCancel
    Left = 448
    Top = 176
    object FieldNameEdit: TStringField
      FieldName = 'FieldName'
      Size = 1000
    end
    object _PhysicalName: TStringField
      DisplayLabel = 'PhysicalName'
      FieldName = '_PhysicalName'
      Size = 1000
    end
    object _DefaultValue: TStringField
      DisplayLabel = 'DefaultValue'
      FieldName = '_DefaultValue'
      Size = 1000
    end
    object TypeComboBox: TStringField
      DisplayLabel = 'Type'
      FieldName = 'TypeComboBox'
      Size = 1000
    end
    object ReferenceModelEditComboBox: TStringField
      DisplayLabel = 'ReferenceModel'
      FieldName = 'ReferenceModelEditComboBox'
      Size = 1000
    end
    object _FileNameField: TStringField
      DisplayLabel = 'FileNameField'
      FieldName = '_FileNameField'
      Size = 1000
    end
    object FieldSizeEdit: TIntegerField
      DisplayLabel = 'Size'
      FieldName = 'FieldSizeEdit'
    end
    object DecimalPrecisionEdit: TIntegerField
      DisplayLabel = 'DecimalPrecision'
      FieldName = 'DecimalPrecisionEdit'
    end
    object PrimaryKeyCheckBox: TBooleanField
      DisplayLabel = 'IsKey'
      FieldName = 'PrimaryKeyCheckBox'
      OnGetText = BooleanFieldGetText
    end
    object RequiredCheckBox: TBooleanField
      DisplayLabel = 'IsRequired'
      FieldName = 'RequiredCheckBox'
      OnGetText = BooleanFieldGetText
    end
    object _IsVisible: TBooleanField
      DisplayLabel = 'IsVisible'
      FieldName = '_IsVisible'
      OnGetText = BooleanFieldGetText
    end
    object _IsReadOnly: TBooleanField
      DisplayLabel = 'IsReadOnly'
      FieldName = '_IsReadOnly'
      OnGetText = BooleanFieldGetText
    end
    object _EmptyAsNull: TBooleanField
      DisplayLabel = 'EmptyAsNull'
      FieldName = '_EmptyAsNull'
      OnGetText = BooleanFieldGetText
    end
    object _DisplayWidth: TIntegerField
      DisplayLabel = 'DisplayWidth'
      FieldName = '_DisplayWidth'
    end
    object _DisplayFormat: TStringField
      DisplayLabel = 'DisplayFormat'
      FieldName = '_DisplayFormat'
      Size = 1000
    end
    object _EditFormat: TStringField
      DisplayLabel = 'EditFormat'
      FieldName = '_EditFormat'
      Size = 1000
    end
    object _IsPicture: TBooleanField
      DisplayLabel = 'IsPicture'
      FieldName = '_IsPicture'
      OnGetText = BooleanFieldGetText
    end
    object _CanUpdate: TBooleanField
      DisplayLabel = 'CanUpdate'
      FieldName = '_CanUpdate'
      OnGetText = BooleanFieldGetText
    end
    object _CanInsert: TBooleanField
      DisplayLabel = 'CanInsert'
      FieldName = '_CanInsert'
      OnGetText = BooleanFieldGetText
    end
    object _DisplayLabel: TStringField
      DisplayLabel = 'DisplayLabel'
      FieldName = '_DisplayLabel'
      Size = 1000
    end
    object _DefaultFilter: TStringField
      DisplayLabel = 'DefaultFilter'
      FieldName = '_DefaultFilter'
      Size = 1000
    end
    object _DefaultFilterConnector: TStringField
      FieldName = '_DefaultFilterConnector'
      Size = 1000
    end
    object _IsPicture_Thumbnail_Width: TIntegerField
      FieldName = '_IsPicture_Thumbnail_Width'
    end
    object _IsPicture_Thumbnail_Height: TIntegerField
      FieldName = '_IsPicture_Thumbnail_Height'
    end
    object _Expression: TMemoField
      DisplayLabel = 'Expression'
      FieldName = '_Expression'
      BlobType = ftMemo
    end
    object _IsComputed: TBooleanField
      DisplayLabel = 'IsComputed'
      FieldName = '_IsComputed'
      OnGetText = BooleanFieldGetText
    end
    object _IsGenerated: TBooleanField
      DisplayLabel = 'IsGenerated'
      FieldName = '_IsGenerated'
      OnGetText = BooleanFieldGetText
    end
    object _MaxUploadSize: TStringField
      DisplayLabel = 'MaxUploadSize'
      FieldName = '_MaxUploadSize'
      Size = 100
    end
    object _IsPassword: TBooleanField
      DisplayLabel = 'IsPassword'
      FieldName = '_IsPassword'
    end
  end
  object FieldsDataSource: TDataSource
    DataSet = FieldsDataSet
    OnStateChange = FieldsDataSourceStateChange
    Left = 256
    Top = 264
  end
end
