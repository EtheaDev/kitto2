inherited ListControllerButtonListFilterItemFrame: TListControllerButtonListFilterItemFrame
  inherited ClientPanel: TPanel
    inherited DesignPanel: TPanel
      Padding.Top = 50
      object LabelEdit: TLabeledEdit
        Left = 6
        Top = 23
        Width = 210
        Height = 21
        EditLabel.Width = 25
        EditLabel.Height = 13
        EditLabel.Caption = 'Label'
        TabOrder = 0
      end
      object _IsDefault: TCheckBox
        Left = 240
        Top = 25
        Width = 97
        Height = 17
        Caption = 'IsDefault'
        TabOrder = 1
      end
      object ExpressionPanel: TPanel
        Left = 0
        Top = 50
        Width = 589
        Height = 249
        Align = alClient
        BevelOuter = bvNone
        Padding.Top = 16
        TabOrder = 2
        object ExpressionLabel: TLabel
          Left = 6
          Top = 0
          Width = 52
          Height = 13
          Caption = 'Expression'
        end
      end
    end
  end
end
