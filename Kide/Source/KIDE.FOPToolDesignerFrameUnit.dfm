inherited FOPToolDesignerFrame: TFOPToolDesignerFrame
  Height = 412
  HelpKeyword = 'FOPTool'
  ExplicitHeight = 412
  inherited ClientPanel: TPanel
    Height = 412
    ExplicitHeight = 412
    inherited DesignPanel: TPanel
      Height = 412
      ExplicitHeight = 412
      inherited ControllerGroupBox: TGroupBox
        Height = 412
        ExplicitLeft = 4
        ExplicitTop = 24
        ExplicitHeight = 325
        inherited DownloadToolGroupBox: TGroupBox
          ExplicitLeft = 0
          ExplicitTop = 164
        end
        object FOPToolGroupBox: TGroupBox
          Left = 2
          Top = 273
          Width = 454
          Height = 59
          Align = alTop
          Caption = 'FOPTool'
          TabOrder = 3
          ExplicitLeft = 0
          ExplicitTop = 352
          ExplicitWidth = 450
          DesignSize = (
            454
            59)
          object _TransformFileName: TLabeledEdit
            Left = 6
            Top = 32
            Width = 440
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 92
            EditLabel.Height = 13
            EditLabel.Caption = 'TransformFileName'
            TabOrder = 0
          end
        end
      end
    end
  end
end
