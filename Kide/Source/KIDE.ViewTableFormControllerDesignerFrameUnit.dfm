inherited ViewTableFormControllerDesignerFrame: TViewTableFormControllerDesignerFrame
  Width = 592
  ExplicitWidth = 592
  inherited ClientPanel: TPanel
    Width = 592
    ExplicitWidth = 592
    inherited DesignPanel: TPanel
      Width = 592
      ExplicitWidth = 592
      object FormGroupBox: TGroupBox
        Left = 0
        Top = 0
        Width = 592
        Height = 57
        Align = alTop
        Caption = 'FormController'
        TabOrder = 0
        object ButtonScaleLabel: TLabel
          Left = 6
          Top = 14
          Width = 57
          Height = 13
          Caption = 'ButtonScale'
        end
        object ButtonScaleComboBox: TComboBox
          Left = 6
          Top = 30
          Width = 195
          Height = 21
          TabOrder = 0
        end
        object _KeepOpenAfterOperation: TCheckBox
          Left = 224
          Top = 32
          Width = 161
          Height = 17
          Caption = 'KeepOpenAfterOperation'
          TabOrder = 1
        end
      end
      object ItemsTabControl: TTabControl
        Left = 0
        Top = 57
        Width = 592
        Height = 242
        Align = alClient
        TabOrder = 1
        Tabs.Strings = (
          'Item')
        TabIndex = 0
        OnChange = ItemsTabControlChange
      end
    end
  end
end
