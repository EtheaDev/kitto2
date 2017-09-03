inherited ListControllerFiltersNodeFrame: TListControllerFiltersNodeFrame
  Width = 451
  ExplicitWidth = 451
  inherited ClientPanel: TPanel
    Width = 451
    ExplicitWidth = 451
    inherited DesignPanel: TPanel
      Width = 451
      ExplicitWidth = 451
      object FilterPageControl: TPageControl
        Left = 0
        Top = 0
        Width = 451
        Height = 299
        ActivePage = FilterPanelTabSheet
        Align = alClient
        TabOrder = 0
        object FilterPanelTabSheet: TTabSheet
          Caption = 'FilterPanel'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object ColumnWidthLabel: TLabel
            Left = 3
            Top = 41
            Width = 63
            Height = 13
            Caption = 'ColumnWidth'
          end
          object LabelWidthLabel: TLabel
            Left = 76
            Top = 41
            Width = 53
            Height = 13
            Caption = 'LabelWidth'
          end
          object LabelAlignLabel: TLabel
            Left = 148
            Top = 41
            Width = 48
            Height = 13
            Caption = 'LabelAlign'
          end
          object _ColumnWidth: TSpinEdit
            Left = 4
            Top = 56
            Width = 66
            Height = 22
            MaxValue = 0
            MinValue = 0
            TabOrder = 2
            Value = 0
          end
          object _LabelWidth: TSpinEdit
            Left = 76
            Top = 56
            Width = 66
            Height = 22
            MaxValue = 0
            MinValue = 0
            TabOrder = 3
            Value = 0
          end
          object LabelAlignComboBox: TComboBox
            Left = 148
            Top = 56
            Width = 77
            Height = 21
            TabOrder = 4
          end
          object _DisplayLabel: TLabeledEdit
            Left = 3
            Top = 16
            Width = 222
            Height = 21
            EditLabel.Width = 59
            EditLabel.Height = 13
            EditLabel.Caption = 'DisplayLabel'
            TabOrder = 0
          end
          object ConnectorRadioGroup: TRadioGroup
            Left = 231
            Top = 0
            Width = 106
            Height = 37
            Caption = 'Connector'
            Columns = 2
            Items.Strings = (
              'or'
              'and')
            TabOrder = 1
          end
        end
        object ItemsTabSheet: TTabSheet
          Caption = 'Items'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
      end
    end
  end
end
