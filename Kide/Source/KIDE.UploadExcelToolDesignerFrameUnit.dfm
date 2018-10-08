inherited UploadExcelToolDesignerFrame: TUploadExcelToolDesignerFrame
  Height = 412
  HelpKeyword = 'ExportExcelTool'
  ExplicitHeight = 412
  inherited ClientPanel: TPanel
    Height = 412
    ExplicitHeight = 412
    inherited DesignPanel: TPanel
      Height = 412
      ExplicitHeight = 412
      inherited ControllerGroupBox: TGroupBox
        Height = 412
        ExplicitHeight = 412
        inherited UploadToolGroupBox: TGroupBox
          inherited _AcceptedWildcards: TLabeledEdit
            EditLabel.ExplicitWidth = 49
          end
        end
        object UploadExcelToolGroupBox: TGroupBox
          Left = 2
          Top = 235
          Width = 454
          Height = 166
          Align = alTop
          Caption = 'UploadExcelTool'
          TabOrder = 3
          object _ExcelRangeName: TLabeledEdit
            Left = 6
            Top = 32
            Width = 190
            Height = 21
            EditLabel.Width = 83
            EditLabel.Height = 13
            EditLabel.Caption = 'ExcelRangeName'
            TabOrder = 0
          end
        end
      end
    end
  end
end
