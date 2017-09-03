inherited ConfigDefaultsNodeFrame: TConfigDefaultsNodeFrame
  Width = 655
  Height = 548
  HelpKeyword = 'Config_Auth'
  ExplicitWidth = 655
  ExplicitHeight = 548
  inherited ClientPanel: TPanel
    Width = 655
    Height = 548
    ExplicitWidth = 655
    ExplicitHeight = 548
    inherited DesignPanel: TPanel
      Width = 655
      Height = 548
      ExplicitWidth = 655
      ExplicitHeight = 548
      object AuthScrollBox: TScrollBox
        Left = 0
        Top = 0
        Width = 655
        Height = 121
        Align = alTop
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        TabOrder = 0
        object AuthAutoScrollPanel: TPanel
          Left = 0
          Top = 126
          Width = 305
          Height = 0
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 2
        end
        object GridPanel: TGroupBox
          Left = 0
          Top = 0
          Width = 638
          Height = 65
          Align = alTop
          Caption = 'Grid'
          TabOrder = 0
          object PageRecordCountLabel: TLabel
            Left = 5
            Top = 18
            Width = 87
            Height = 13
            Caption = 'PageRecordCount'
          end
          object ThemeLabel: TLabel
            Left = 98
            Top = 18
            Width = 65
            Height = 13
            Caption = 'DefaultAction'
          end
          object _Grid_PageRecordCount: TSpinEdit
            Left = 5
            Top = 33
            Width = 87
            Height = 22
            ParentCustomHint = False
            Increment = 10
            MaxValue = 0
            MinValue = 0
            TabOrder = 0
            Value = 0
          end
          object DefaultActionComboBox: TComboBox
            Left = 98
            Top = 33
            Width = 121
            Height = 21
            Style = csDropDownList
            DropDownCount = 20
            TabOrder = 1
            OnChange = DefaultActionComboBoxChange
            Items.Strings = (
              'Edit'
              'View'
              'Add')
          end
        end
        object WindowGroupBox: TGroupBox
          Left = 0
          Top = 65
          Width = 638
          Height = 61
          Align = alTop
          Caption = 'Window'
          TabOrder = 1
          object WidthLabel: TLabel
            Left = 5
            Top = 13
            Width = 28
            Height = 13
            Caption = 'Width'
          end
          object HeightLabel: TLabel
            Left = 98
            Top = 13
            Width = 31
            Height = 13
            Caption = 'Height'
          end
          object _Window_Height: TSpinEdit
            Left = 98
            Top = 29
            Width = 87
            Height = 22
            Increment = 10
            MaxValue = 0
            MinValue = 0
            TabOrder = 1
            Value = 0
          end
          object _Window_Width: TSpinEdit
            Left = 5
            Top = 29
            Width = 87
            Height = 22
            Increment = 10
            MaxValue = 0
            MinValue = 0
            TabOrder = 0
            Value = 0
          end
        end
      end
    end
  end
end
