inherited ModelFieldDesignerFrame: TModelFieldDesignerFrame
  Width = 492
  Height = 561
  HelpKeyword = 'Models#model-fields-and-their-properties'
  ExplicitWidth = 492
  ExplicitHeight = 561
  inherited ClientPanel: TPanel
    Width = 492
    Height = 561
    ExplicitWidth = 492
    ExplicitHeight = 561
    inherited DesignPanel: TPanel
      Width = 492
      Height = 561
      ExplicitWidth = 492
      ExplicitHeight = 561
      object EditorPageControl: TPageControl
        Left = 0
        Top = 0
        Width = 492
        Height = 561
        ActivePage = FieldPropertiesTabSheet
        Align = alClient
        TabOrder = 0
        OnChange = EditorPageControlChange
        object FieldPropertiesTabSheet: TTabSheet
          Caption = 'Field Properties'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object FieldPropertiesScrollBox: TScrollBox
            Left = 0
            Top = 0
            Width = 484
            Height = 533
            Align = alClient
            BorderStyle = bsNone
            TabOrder = 0
            object FieldPropertiesPanel: TPanel
              Left = 0
              Top = 0
              Width = 484
              Height = 442
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 0
              DesignSize = (
                484
                442)
              object TypeLabel: TLabel
                Left = 3
                Top = 39
                Width = 24
                Height = 13
                Caption = 'Type'
              end
              object FieldSizeLabel: TLabel
                Left = 149
                Top = 39
                Width = 19
                Height = 13
                Caption = 'Size'
              end
              object DecimalPrecisionLabel: TLabel
                Left = 223
                Top = 39
                Width = 81
                Height = 13
                Caption = 'Decimal precision'
              end
              object DisplayWidthLabel: TLabel
                Left = 4
                Top = 172
                Width = 62
                Height = 13
                Caption = 'DisplayWidth'
              end
              object Thumbnail_WidthLabel: TLabel
                Left = 79
                Top = 251
                Width = 80
                Height = 13
                Caption = 'Thumbnail/Width'
              end
              object Thumbnail_HeightLabel: TLabel
                Left = 168
                Top = 251
                Width = 83
                Height = 13
                Caption = 'Thumbnail/Height'
              end
              object FieldNameEdit: TLabeledEdit
                Left = 4
                Top = 18
                Width = 140
                Height = 21
                EditLabel.Width = 49
                EditLabel.Height = 13
                EditLabel.Caption = 'FieldName'
                TabOrder = 0
              end
              object TypeComboBox: TComboBox
                Left = 4
                Top = 56
                Width = 140
                Height = 21
                Style = csDropDownList
                TabOrder = 3
                OnChange = TypeComboBoxChange
              end
              object FieldSizeEdit: TSpinEdit
                Left = 148
                Top = 56
                Width = 69
                Height = 22
                MaxValue = 0
                MinValue = 0
                TabOrder = 5
                Value = 0
              end
              object PrimaryKeyCheckBox: TCheckBox
                Left = 4
                Top = 121
                Width = 69
                Height = 17
                Caption = 'IsKey'
                TabOrder = 10
              end
              object RequiredCheckBox: TCheckBox
                Left = 72
                Top = 121
                Width = 80
                Height = 17
                Caption = 'IsRequired'
                TabOrder = 11
              end
              object _PhysicalName: TLabeledEdit
                Left = 148
                Top = 18
                Width = 188
                Height = 21
                EditLabel.Width = 65
                EditLabel.Height = 13
                EditLabel.Caption = 'PhysicalName'
                TabOrder = 1
              end
              object _DisplayLabel: TLabeledEdit
                Left = 4
                Top = 96
                Width = 168
                Height = 21
                EditLabel.Width = 59
                EditLabel.Height = 13
                EditLabel.Caption = 'DisplayLabel'
                TabOrder = 8
              end
              object _IsVisible: TCheckBox
                Left = 4
                Top = 144
                Width = 69
                Height = 17
                Caption = 'IsVisible'
                TabOrder = 15
              end
              object _IsReadOnly: TCheckBox
                Left = 72
                Top = 144
                Width = 80
                Height = 17
                Caption = 'IsReadOnly'
                TabOrder = 16
              end
              object _IsGenerated: TCheckBox
                Left = 158
                Top = 144
                Width = 80
                Height = 17
                Caption = 'IsGenerated'
                TabOrder = 17
              end
              object DecimalPrecisionEdit: TSpinEdit
                Left = 223
                Top = 56
                Width = 78
                Height = 22
                MaxValue = 0
                MinValue = 0
                TabOrder = 6
                Value = 0
              end
              object _Hint: TLabeledEdit
                Left = 178
                Top = 96
                Width = 291
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                EditLabel.Width = 19
                EditLabel.Height = 13
                EditLabel.Caption = 'Hint'
                TabOrder = 9
              end
              object _DisplayWidth: TSpinEdit
                Left = 4
                Top = 188
                Width = 73
                Height = 22
                MaxValue = 0
                MinValue = 0
                TabOrder = 20
                Value = 0
              end
              object _DisplayFormat: TLabeledEdit
                Left = 83
                Top = 188
                Width = 128
                Height = 21
                EditLabel.Width = 68
                EditLabel.Height = 13
                EditLabel.Caption = 'DisplayFormat'
                TabOrder = 21
              end
              object _EditFormat: TLabeledEdit
                Left = 217
                Top = 188
                Width = 170
                Height = 21
                EditLabel.Width = 52
                EditLabel.Height = 13
                EditLabel.Caption = 'EditFormat'
                TabOrder = 22
              end
              object _DefaultValue: TLabeledEdit
                Left = 342
                Top = 18
                Width = 127
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                EditLabel.Width = 61
                EditLabel.Height = 13
                EditLabel.Caption = 'DefaultValue'
                TabOrder = 2
              end
              object _DefaultFilterConnector: TLabeledEdit
                Left = 4
                Top = 228
                Width = 140
                Height = 21
                EditLabel.Width = 109
                EditLabel.Height = 13
                EditLabel.Caption = 'DefaultFilterConnector'
                TabOrder = 23
              end
              object _DefaultFilter: TLabeledEdit
                Left = 148
                Top = 228
                Width = 239
                Height = 21
                EditLabel.Width = 59
                EditLabel.Height = 13
                EditLabel.Caption = 'DefaultFilter'
                TabOrder = 24
              end
              object _EmptyAsNull: TCheckBox
                Left = 248
                Top = 144
                Width = 86
                Height = 17
                Caption = 'EmptyAsNull'
                TabOrder = 18
              end
              object _CanUpdate: TCheckBox
                Left = 158
                Top = 121
                Width = 80
                Height = 17
                Caption = 'CanUpdate'
                TabOrder = 12
              end
              object _CanInsert: TCheckBox
                Left = 248
                Top = 121
                Width = 86
                Height = 17
                Caption = 'CanInsert'
                TabOrder = 13
              end
              object _FileNameField: TLabeledEdit
                Left = 223
                Top = 56
                Width = 140
                Height = 21
                EditLabel.Width = 71
                EditLabel.Height = 13
                EditLabel.Caption = 'File Name Field'
                TabOrder = 7
              end
              object _IsPicture: TCheckBox
                Left = 6
                Top = 269
                Width = 69
                Height = 17
                Caption = 'IsPicture'
                TabOrder = 25
              end
              object _IsPicture_Thumbnail_Width: TSpinEdit
                Left = 79
                Top = 267
                Width = 69
                Height = 22
                Increment = 10
                MaxValue = 0
                MinValue = 0
                TabOrder = 26
                Value = 0
              end
              object _IsPicture_Thumbnail_Height: TSpinEdit
                Left = 169
                Top = 267
                Width = 78
                Height = 22
                Increment = 10
                MaxValue = 0
                MinValue = 0
                TabOrder = 27
                Value = 0
              end
              object _MaxUploadSize: TLabeledEdit
                Left = 257
                Top = 267
                Width = 130
                Height = 21
                EditLabel.Width = 72
                EditLabel.Height = 13
                EditLabel.Caption = 'MaxUploadSize'
                TabOrder = 28
              end
              object _IsComputed: TCheckBox
                Left = 342
                Top = 121
                Width = 89
                Height = 17
                Caption = 'IsComputed'
                TabOrder = 14
              end
              object ExpressionGroupBox: TGroupBox
                Left = 6
                Top = 294
                Width = 463
                Height = 128
                Anchors = [akLeft, akTop, akRight]
                Caption = 'Expression'
                TabOrder = 29
              end
              object ReferenceModelEdit: TLabeledEdit
                Left = 150
                Top = 56
                Width = 186
                Height = 21
                Cursor = crHandPoint
                EditLabel.Width = 87
                EditLabel.Height = 13
                EditLabel.Caption = 'Referenced Model'
                ReadOnly = True
                TabOrder = 4
                OnClick = ReferenceModelEditClick
              end
              object _IsPassword: TCheckBox
                Left = 342
                Top = 144
                Width = 89
                Height = 17
                Caption = 'IsPassword'
                TabOrder = 19
              end
            end
            object FieldPropertiesScrollPanel: TPanel
              Left = 0
              Top = 442
              Width = 462
              Height = 91
              Align = alLeft
              BevelOuter = bvNone
              TabOrder = 1
            end
          end
        end
        object ReferenceTabSheet: TTabSheet
          Caption = 'Reference attributes'
          ImageIndex = 3
          OnResize = ReferenceTabSheetResize
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object FieldsGroupBox: TGroupBox
            Left = 0
            Top = 41
            Width = 484
            Height = 112
            Align = alTop
            Caption = 'Fields'
            TabOrder = 0
          end
          object AutoAddFieldsGroupBox: TGroupBox
            Left = 0
            Top = 273
            Width = 484
            Height = 260
            Align = alClient
            Caption = 'AutoAddFields'
            TabOrder = 1
          end
          object ReferenceModelPanel: TPanel
            Left = 0
            Top = 0
            Width = 484
            Height = 41
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 2
            DesignSize = (
              484
              41)
            object ReferenceModelLabel: TLabel
              Left = 2
              Top = 1
              Width = 87
              Height = 13
              Caption = 'Referenced Model'
            end
            object AutoCompleteMinCharsLabel: TLabel
              Left = 365
              Top = 0
              Width = 112
              Height = 13
              Anchors = [akTop, akRight]
              Caption = 'AutoCompleteMinChars'
              ExplicitLeft = 364
            end
            object ReferenceModelEditComboBox: TComboBox
              Left = 2
              Top = 16
              Width = 357
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
            end
            object _AutoCompleteMinChars: TSpinEdit
              Left = 365
              Top = 16
              Width = 112
              Height = 22
              Anchors = [akTop, akRight]
              MaxValue = 0
              MinValue = 0
              TabOrder = 1
              Value = 0
            end
          end
          object LookupFilterGroupBox: TGroupBox
            Left = 0
            Top = 153
            Width = 484
            Height = 120
            Align = alTop
            Caption = 'LookupFilter'
            TabOrder = 3
          end
        end
        object AllowedValuesTabSheet: TTabSheet
          Caption = 'Allowed Values'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
        object RulesTabSheet: TTabSheet
          Caption = 'Rules'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
      end
    end
  end
  inherited ActionList: TActionList
    Left = 296
    Top = 328
  end
end
