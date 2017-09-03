inherited MobileSettingsDesginerFrame: TMobileSettingsDesginerFrame
  inherited ClientPanel: TPanel
    inherited DesignPanel: TPanel
      object ViewPortContentGroupBox: TGroupBox
        Left = 0
        Top = 0
        Width = 589
        Height = 57
        Align = alTop
        Caption = 'ViewportContent'
        TabOrder = 0
        ExplicitTop = 8
        object WidthLabel: TLabel
          Left = 8
          Top = 16
          Width = 28
          Height = 13
          Caption = 'Width'
        end
        object _ViewportContent_Width: TSpinEdit
          Left = 8
          Top = 31
          Width = 54
          Height = 22
          Increment = 10
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 0
        end
        object user_scalableCheckBox: TCheckBox
          Left = 76
          Top = 33
          Width = 117
          Height = 17
          Caption = 'user-scalable'
          TabOrder = 1
        end
      end
    end
  end
end
