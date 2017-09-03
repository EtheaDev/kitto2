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
          ExplicitLeft = 4
          ExplicitTop = 24
          ExplicitHeight = 325
          inherited DownloadToolGroupBox: TGroupBox
            ExplicitLeft = 0
            ExplicitTop = 164
          end
          object FOPToolGroupBox: TGroupBox
            Left = 0
            Top = 302
            Width = 450
            Height = 59
            Align = alTop
            Caption = 'FOPTool'
            TabOrder = 4
            ExplicitTop = 352
            DesignSize = (
              450
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
