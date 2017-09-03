inherited ListControllerListFilterBaseNodeFrame: TListControllerListFilterBaseNodeFrame
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
        Caption = 'ListFilterBase'
        TabOrder = 0
        object WidthLabel: TLabel
          Left = 5
          Top = 57
          Width = 28
          Height = 13
          Caption = 'Width'
        end
        object ListWidthLabel: TLabel
          Left = 78
          Top = 57
          Width = 44
          Height = 13
          Caption = 'ListWidth'
        end
        object AutoCompleteMinCharsLabel: TLabel
          Left = 150
          Top = 57
          Width = 112
          Height = 13
          Caption = 'AutoCompleteMinChars'
        end
        object _Width: TSpinEdit
          Left = 6
          Top = 72
          Width = 66
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
        object _ListWidth: TSpinEdit
          Left = 78
          Top = 72
          Width = 66
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 0
        end
        object _AutoCompleteMinChars: TSpinEdit
          Left = 150
          Top = 72
          Width = 66
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 3
          Value = 0
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
      end
    end
  end
end
