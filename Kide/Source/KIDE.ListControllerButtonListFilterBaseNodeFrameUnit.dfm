inherited ListControllerButtonListFilterBaseNodeFrame: TListControllerButtonListFilterBaseNodeFrame
  Width = 451
  Height = 335
  ExplicitWidth = 451
  ExplicitHeight = 335
  inherited ClientPanel: TPanel
    Width = 451
    Height = 335
    ExplicitWidth = 451
    ExplicitHeight = 335
    inherited DesignPanel: TPanel
      Width = 451
      Height = 335
      ExplicitWidth = 451
      ExplicitHeight = 335
      object ListFilterGroupBox: TGroupBox
        Left = 0
        Top = 0
        Width = 451
        Height = 99
        Align = alTop
        Caption = 'ButtonListFilterBase'
        TabOrder = 0
        object ButtonScaleLabel: TLabel
          Left = 120
          Top = 58
          Width = 57
          Height = 13
          Caption = 'ButtonScale'
        end
        object LabelEdit: TLabeledEdit
          Left = 6
          Top = 30
          Width = 210
          Height = 21
          EditLabel.Width = 25
          EditLabel.Height = 13
          EditLabel.Caption = 'Label'
          TabOrder = 0
        end
        object _IsSingleSelect: TCheckBox
          Left = 232
          Top = 32
          Width = 97
          Height = 17
          Caption = 'IsSingleSelect'
          TabOrder = 1
        end
        object ConnectorRadioGroup: TRadioGroup
          Left = 6
          Top = 57
          Width = 106
          Height = 37
          Caption = 'Connector'
          Columns = 2
          Items.Strings = (
            'or'
            'and')
          TabOrder = 2
        end
        object ButtonScaleComboBox: TComboBox
          Left = 118
          Top = 75
          Width = 195
          Height = 21
          TabOrder = 3
        end
      end
    end
  end
end
