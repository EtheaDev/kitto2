inherited ListControllerDynaButtonListFilterNodeFrame: TListControllerDynaButtonListFilterNodeFrame
  inherited ClientPanel: TPanel
    inherited DesignPanel: TPanel
      inherited ListFilterGroupBox: TGroupBox
        Height = 145
        Caption = 'DynaButtonList'
        ExplicitHeight = 145
        object _ExpressionTemplate: TLabeledEdit
          Left = 6
          Top = 112
          Width = 435
          Height = 21
          EditLabel.Width = 96
          EditLabel.Height = 13
          EditLabel.Caption = 'ExpressionTemplate'
          TabOrder = 4
        end
      end
      object CommandTextPanel: TPanel
        Left = 0
        Top = 145
        Width = 451
        Height = 190
        Align = alClient
        BevelOuter = bvNone
        Padding.Top = 16
        TabOrder = 1
        object CommandTextLabel: TLabel
          Left = 6
          Top = 0
          Width = 69
          Height = 13
          Caption = 'CommandText'
        end
      end
    end
  end
end
