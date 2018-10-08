inherited ExportTextToolDesignerFrame: TExportTextToolDesignerFrame
  Height = 412
  HelpKeyword = 'ExportTextTool'
  ExplicitHeight = 412
  inherited ClientPanel: TPanel
    Height = 412
    ExplicitHeight = 412
    inherited DesignPanel: TPanel
      Height = 412
      ExplicitHeight = 412
      inherited ControllerGroupBox: TGroupBox
        Height = 412
        ExplicitHeight = 384
        object ExportTextToolGroupBox: TGroupBox
          Left = 2
          Top = 273
          Width = 454
          Height = 64
          Align = alTop
          Caption = 'ExportTextTool'
          TabOrder = 3
          ExplicitLeft = 0
          ExplicitTop = 258
          ExplicitWidth = 450
          object _IncludeHeader: TCheckBox
            Left = 11
            Top = 16
            Width = 97
            Height = 17
            Caption = 'IncludeHeader'
            TabOrder = 0
          end
          object _FixedLength: TCheckBox
            Left = 11
            Top = 35
            Width = 97
            Height = 17
            Caption = 'FixedLength'
            TabOrder = 1
          end
          object _Delimiter: TLabeledEdit
            Left = 122
            Top = 29
            Width = 31
            Height = 21
            EditLabel.Width = 41
            EditLabel.Height = 13
            EditLabel.Caption = 'Delimiter'
            TabOrder = 2
          end
          object _QuoteChar: TLabeledEdit
            Left = 178
            Top = 29
            Width = 31
            Height = 21
            EditLabel.Width = 53
            EditLabel.Height = 13
            EditLabel.Caption = 'QuoteChar'
            TabOrder = 3
          end
        end
      end
    end
  end
end
