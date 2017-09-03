inherited SimpleNodeDesignerFrame: TSimpleNodeDesignerFrame
  Width = 364
  ExplicitWidth = 364
  inherited ClientPanel: TPanel
    Width = 364
    ExplicitWidth = 364
    inherited DesignPanel: TPanel
      Width = 364
      ExplicitWidth = 364
      object KeyPanel: TPanel
        Left = 0
        Top = 0
        Width = 364
        Height = 45
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          364
          45)
        object KeyEdit: TLabeledEdit
          Left = 6
          Top = 18
          Width = 352
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          EditLabel.Width = 18
          EditLabel.Height = 13
          EditLabel.Caption = 'Key'
          TabOrder = 0
        end
      end
      object CheckBoxPanel: TPanel
        Left = 0
        Top = 45
        Width = 364
        Height = 28
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object CheckBoxValue: TCheckBox
          Left = 6
          Top = 5
          Width = 97
          Height = 17
          Caption = 'Value (Boolean)'
          TabOrder = 0
        end
      end
      object IntegerPanel: TPanel
        Left = 0
        Top = 73
        Width = 364
        Height = 34
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
        object IntegerValueLabel: TLabel
          Left = 6
          Top = 8
          Width = 73
          Height = 13
          Caption = 'Value (Integer)'
        end
        object IntegerValue: TSpinEdit
          Left = 85
          Top = 6
          Width = 68
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 0
        end
      end
      object TextPanel: TPanel
        Left = 0
        Top = 107
        Width = 364
        Height = 192
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 3
        ExplicitHeight = 182
        object ValueLabel: TLabel
          Left = 6
          Top = 5
          Width = 26
          Height = 13
          Caption = 'Value'
        end
        object ValueEdit: TMemo
          AlignWithMargins = True
          Left = 6
          Top = 20
          Width = 352
          Height = 166
          Margins.Left = 6
          Margins.Top = 20
          Margins.Right = 6
          Margins.Bottom = 6
          Align = alClient
          TabOrder = 0
          ExplicitLeft = 3
          ExplicitTop = 40
          ExplicitWidth = 358
          ExplicitHeight = 139
        end
      end
    end
  end
  inherited ActionList: TActionList
    Left = 232
    Top = 32
  end
end
