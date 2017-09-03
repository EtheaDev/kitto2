inherited ListControllerFilterSpacerFrame: TListControllerFilterSpacerFrame
  inherited ClientPanel: TPanel
    inherited DesignPanel: TPanel
      Padding.Top = 50
      object WidthLabel: TLabel
        Left = 8
        Top = 4
        Width = 28
        Height = 13
        Caption = 'Width'
      end
      object _Width: TSpinEdit
        Left = 8
        Top = 19
        Width = 93
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
    end
  end
end
