inherited WindowControllerDesignerFrame: TWindowControllerDesignerFrame
  HelpKeyword = 'Window'
  inherited ClientPanel: TPanel
    inherited DesignPanel: TPanel
      inherited ControllerGroupBox: TGroupBox
        object WindowControllerGroupBox: TGroupBox
          Left = 2
          Top = 15
          Width = 451
          Height = 61
          Align = alTop
          Caption = 'Window'
          TabOrder = 1
          object WidthLabel: TLabel
            Left = 4
            Top = 14
            Width = 28
            Height = 13
            Caption = 'Width'
          end
          object HeightLabel: TLabel
            Left = 65
            Top = 14
            Width = 31
            Height = 13
            Caption = 'Height'
          end
          object _Width: TSpinEdit
            Left = 4
            Top = 29
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
            Left = 66
            Top = 29
            Width = 54
            Height = 22
            Increment = 10
            MaxValue = 0
            MinValue = 0
            TabOrder = 1
            Value = 0
            OnKeyPress = IntegerKeyPress
          end
          object _ResizeHandles: TLabeledEdit
            Left = 128
            Top = 29
            Width = 77
            Height = 21
            EditLabel.Width = 69
            EditLabel.Height = 13
            EditLabel.Caption = 'ResizeHandles'
            TabOrder = 2
          end
          object _Maximizable: TCheckBox
            Left = 211
            Top = 32
            Width = 86
            Height = 17
            Caption = 'Maximizable'
            TabOrder = 3
          end
          object _Movable: TCheckBox
            Left = 308
            Top = 32
            Width = 97
            Height = 17
            Caption = 'Movable'
            TabOrder = 4
          end
        end
        object SubViewGroupBox: TGroupBox
          Left = 2
          Top = 76
          Width = 451
          Height = 240
          Align = alClient
          Caption = 'SubView'
          TabOrder = 0
        end
      end
    end
  end
end
