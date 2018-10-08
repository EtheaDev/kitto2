inherited MergePDFToolDesignerFrame: TMergePDFToolDesignerFrame
  Height = 434
  HelpKeyword = 'MergePDFTool'
  ExplicitHeight = 434
  inherited ClientPanel: TPanel
    Height = 434
    ExplicitHeight = 412
    inherited DesignPanel: TPanel
      Height = 434
      ExplicitHeight = 412
      inherited ControllerGroupBox: TGroupBox
        Height = 434
        ExplicitLeft = 4
        ExplicitTop = 24
        ExplicitHeight = 288
        inherited DownloadToolGroupBox: TGroupBox
          ExplicitLeft = 0
          ExplicitTop = 164
        end
        object MergePDFToolGroupBox: TGroupBox
          Left = 2
          Top = 273
          Width = 454
          Height = 96
          Align = alTop
          Caption = 'MergePDFTool'
          TabOrder = 3
          ExplicitLeft = 3
          ExplicitTop = 358
          ExplicitWidth = 450
          DesignSize = (
            454
            96)
          object _BaseFileName: TLabeledEdit
            Left = 6
            Top = 32
            Width = 440
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 66
            EditLabel.Height = 13
            EditLabel.Caption = 'BaseFileName'
            TabOrder = 0
          end
          object _LayoutFileName: TLabeledEdit
            Left = 6
            Top = 70
            Width = 440
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 76
            EditLabel.Height = 13
            EditLabel.Caption = 'LayoutFileName'
            TabOrder = 1
          end
        end
      end
    end
  end
end
