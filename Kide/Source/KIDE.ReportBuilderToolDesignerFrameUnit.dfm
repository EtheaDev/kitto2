inherited ReportBuilderToolDesignerFrame: TReportBuilderToolDesignerFrame
  HelpKeyword = 'ReportBuilderTool'
  inherited ClientPanel: TPanel
    inherited DesignPanel: TPanel
      inherited ControllerGroupBox: TGroupBox
        ExplicitLeft = 4
        ExplicitTop = 24
        ExplicitWidth = 0
        ExplicitHeight = 0
        inherited ToolGroupBox: TGroupBox
          ExplicitWidth = 450
        end
        inherited DownloadToolGroupBox: TGroupBox
          ExplicitLeft = 0
          ExplicitTop = 120
          ExplicitWidth = 450
        end
        object ReportBuilderlToolGroupBox: TGroupBox
          Left = 2
          Top = 273
          Width = 454
          Height = 64
          Align = alTop
          Caption = 'ReportBuilderTool'
          TabOrder = 3
          ExplicitLeft = 0
          ExplicitTop = 302
          ExplicitWidth = 450
          DesignSize = (
            454
            64)
          object _TemplateFileName: TLabeledEdit
            Left = 6
            Top = 32
            Width = 319
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 87
            EditLabel.Height = 13
            EditLabel.Caption = 'TemplateFileName'
            TabOrder = 0
          end
          object _Design: TCheckBox
            Left = 331
            Top = 35
            Width = 97
            Height = 17
            Anchors = [akTop, akRight]
            Caption = 'Design'
            TabOrder = 1
          end
        end
      end
    end
  end
end
