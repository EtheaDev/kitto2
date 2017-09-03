inherited PanelControllerDesignerFrame: TPanelControllerDesignerFrame
  inherited ClientPanel: TPanel
    inherited DesignPanel: TPanel
      inherited ControllerGroupBox: TGroupBox
        object PanelControllerGroupBox: TGroupBox
          Left = 2
          Top = 15
          Width = 451
          Height = 57
          Align = alTop
          Caption = 'PanelController'
          TabOrder = 0
          DesignSize = (
            451
            57)
          object WidthLabel: TLabel
            Left = 8
            Top = 16
            Width = 28
            Height = 13
            Caption = 'Width'
          end
          object HeightLabel: TLabel
            Left = 69
            Top = 16
            Width = 31
            Height = 13
            Caption = 'Height'
          end
          object _Width: TSpinEdit
            Left = 8
            Top = 31
            Width = 54
            Height = 22
            Increment = 10
            MaxValue = 0
            MinValue = 0
            TabOrder = 0
            Value = 0
            OnKeyPress = IntegerKeyPress
          end
          object _Height: TSpinEdit
            Left = 70
            Top = 31
            Width = 54
            Height = 22
            Increment = 10
            MaxValue = 0
            MinValue = 0
            TabOrder = 1
            Value = 0
            OnKeyPress = IntegerKeyPress
          end
          object _Split: TCheckBox
            Left = 134
            Top = 15
            Width = 59
            Height = 17
            Caption = 'Split'
            TabOrder = 2
          end
          object _Border: TCheckBox
            Left = 134
            Top = 37
            Width = 59
            Height = 14
            Caption = 'Border'
            TabOrder = 3
          end
          object _Collapsible: TCheckBox
            Left = 194
            Top = 15
            Width = 72
            Height = 17
            Caption = 'Collapsible'
            TabOrder = 4
          end
          object _Header: TCheckBox
            Left = 194
            Top = 35
            Width = 72
            Height = 17
            Caption = 'Header'
            TabOrder = 5
          end
          object _Title: TLabeledEdit
            Left = 272
            Top = 33
            Width = 172
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 20
            EditLabel.Height = 13
            EditLabel.Caption = 'Title'
            TabOrder = 6
          end
        end
      end
    end
  end
end
