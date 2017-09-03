inherited ViewTableDesignerFrame: TViewTableDesignerFrame
  Width = 404
  HelpKeyword = 'DataView'
  ExplicitWidth = 404
  inherited ClientPanel: TPanel
    Width = 404
    ExplicitWidth = 404
    inherited DesignPanel: TPanel
      Width = 404
      ExplicitWidth = 404
      object ViewTablePageControl: TPageControl
        Left = 0
        Top = 0
        Width = 404
        Height = 299
        ActivePage = ViewTableTabSheet
        Align = alClient
        TabOrder = 0
        OnChange = ViewTablePageControlChange
        object ViewTableTabSheet: TTabSheet
          Caption = 'ViewTable'
          DesignSize = (
            396
            271)
          object ModelLabel: TLabel
            Left = 5
            Top = -2
            Width = 28
            Height = 13
            Caption = 'Model'
          end
          object ImageNameSpeedButton: TSpeedButton
            Left = 303
            Top = 13
            Width = 24
            Height = 23
            Action = FileOpenAction
          end
          object ImageNameImage: TImage
            Left = 332
            Top = 19
            Width = 16
            Height = 16
            Hint = 'Image icon'
            Proportional = True
            Stretch = True
          end
          object ImageNameImageLarge: TImage
            Left = 356
            Top = 3
            Width = 32
            Height = 32
            Hint = 'Image icon large'
            Proportional = True
            Stretch = True
          end
          object DisplayLabelEdit: TLabeledEdit
            Left = 5
            Top = 56
            Width = 156
            Height = 21
            EditLabel.Width = 59
            EditLabel.Height = 13
            EditLabel.Caption = 'DisplayLabel'
            TabOrder = 2
          end
          object PluralDisplayLabelEdit: TLabeledEdit
            Left = 164
            Top = 56
            Width = 224
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 85
            EditLabel.Height = 13
            EditLabel.Caption = 'PluralDisplayLabel'
            TabOrder = 3
          end
          object DefaultSortingEdit: TLabeledEdit
            Left = 5
            Top = 95
            Width = 383
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 69
            EditLabel.Height = 13
            EditLabel.Caption = 'DefaultSorting'
            TabOrder = 4
          end
          object DefaultFilterEdit: TLabeledEdit
            Left = 5
            Top = 134
            Width = 383
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 59
            EditLabel.Height = 13
            EditLabel.Caption = 'DefaultFilter'
            TabOrder = 5
          end
          object ModelComboBox: TComboBox
            Left = 5
            Top = 14
            Width = 156
            Height = 21
            TabOrder = 0
            Text = 'ModelComboBox'
            OnChange = ModelComboBoxChange
          end
          object _ImageName: TLabeledEdit
            Left = 167
            Top = 14
            Width = 136
            Height = 21
            EditLabel.Width = 57
            EditLabel.Height = 13
            EditLabel.Caption = 'ImageName'
            TabOrder = 1
            OnChange = _ImageNameChange
          end
          object IsReadOnlyCheckBox: TCheckBox
            Left = 5
            Top = 162
            Width = 97
            Height = 17
            Caption = 'IsReadOnly'
            TabOrder = 6
          end
          object _DatabaseRouter: TLabeledEdit
            Left = 5
            Top = 199
            Width = 156
            Height = 21
            EditLabel.Width = 79
            EditLabel.Height = 13
            EditLabel.Caption = 'DatabaseRouter'
            TabOrder = 8
          end
          object _DatabaseRouter_DatabaseName: TLabeledEdit
            Left = 167
            Top = 199
            Width = 221
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 156
            EditLabel.Height = 13
            EditLabel.Caption = 'DatabaseRouter/DatabaseName'
            TabOrder = 9
          end
          object IsLargeCheckBox: TCheckBox
            Left = 108
            Top = 162
            Width = 87
            Height = 17
            Caption = 'IsLarge'
            TabOrder = 7
          end
        end
        object FieldsTabSheet: TTabSheet
          Caption = 'Fields'
          ImageIndex = 3
        end
        object DetailTablesTabSheet: TTabSheet
          Caption = 'DetailTables'
          ImageIndex = 1
        end
        object RulesTabSheet: TTabSheet
          Caption = 'Rules'
          ImageIndex = 2
        end
      end
    end
  end
  inherited ActionList: TActionList
    object FileOpenAction: TFileOpen
      Category = 'File'
      Dialog.DefaultExt = '*.png'
      Dialog.Filter = 
        'png image files|*.png|gif image files|*.gif|jpeg image files|*.j' +
        'pg;*.jpeg|bitmap image files|*.bmp|All files|*.*'
      Hint = 'Open|Opens an existing file'
      ImageIndex = 0
      ShortCut = 16463
      OnAccept = FileOpenActionAccept
    end
  end
end
