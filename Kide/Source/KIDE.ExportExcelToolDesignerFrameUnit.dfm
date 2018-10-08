inherited ExportExcelToolDesignerFrame: TExportExcelToolDesignerFrame
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
        ExplicitHeight = 320
        object ExportExcelToolGroupBox: TGroupBox
          Left = 2
          Top = 273
          Width = 454
          Height = 64
          Align = alTop
          Caption = 'ExportExcelTool'
          TabOrder = 3
          ExplicitLeft = 3
          ExplicitTop = 339
          ExplicitWidth = 450
          DesignSize = (
            454
            64)
          object _TemplateFileName: TLabeledEdit
            Left = 3
            Top = 32
            Width = 247
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 87
            EditLabel.Height = 13
            EditLabel.Caption = 'TemplateFileName'
            TabOrder = 0
          end
          object _ExcelRangeName: TLabeledEdit
            Left = 256
            Top = 32
            Width = 190
            Height = 21
            Anchors = [akTop, akRight]
            EditLabel.Width = 83
            EditLabel.Height = 13
            EditLabel.Caption = 'ExcelRangeName'
            TabOrder = 1
          end
        end
      end
    end
  end
end
